clc;

[T,epsMatrix,sigMatrix] = readData();
%% TODO Change the nominal strain to the modified Green strain tensor
% this will be multiplied with coefficents
% the only thing I'm not sure if NE12, NE13 are needed to be divided by 2
% Another confusion is that when converting into Green Strain tensor
% should we use C or B. Because their theoretical manual said B
%%
% [E11,E22,E33,E12,E13,E23]=modifyStrain(T.eps11,T.eps22,T.eps33,T.eps12,T.eps13,T.eps23);
% epsMatrix = [E11,E22,E33,E12,E13,E23];


[C,E]=calcStep(epsMatrix,T);
Cmod = C; Emod = E;
Cmod(4:6,1:3) = 0; Cmod(1:3,4:6) = 0;
Emod(4:6,1:3) = 0; Emod(1:3,4:6) = 0;
Cmod = shuffleElements(Cmod);
Emod = shuffleElements(Emod);
Emod = diag(diag(Emod));
% ft = fittype('a*x+b*x^3');
% f12 = fit( eps12, sig12, ft,'StartPoint',[0,0] );
% f13 = fit( eps13, sig13, ft,'StartPoint',[0,0] );
% f23 = fit( eps23, sig23, ft,'StartPoint',[0,0] );
% Cmod(4:6,:) = 0; Emod(4:6,:) = 0;
% Cmod(4,4) = f12.a; Emod(4,4) = f12.b;
% Cmod(5,5) = f13.a; Emod(5,5) = f13.b;
% Cmod(6,6) = f23.a; Emod(6,6) = f23.b;



% Cmod(6,1:3) = C(6,1:3); Cmod(1:3,6) = C(6,1:3);
% Emod(6,1:3) = E(6,1:3); Emod(1:3,6) = E(6,1:3);
Emod = (Emod + Emod')/2;
Cmod = (Cmod + Cmod')/2;

figure(2)
plotExp(Cmod,Emod,T,epsMatrix)

writematrix(Cmod,'CnonLinAnsioProp.csv');
writematrix(Emod,'EnonLinAnsioProp.csv');

J=cost(Cmod,Emod, T)

%% Useful functions
function C=shuffleElements(C)
    C(4,5) = C(5,4);
    C(4,6) = C(6,4);
    C(5,6) = C(6,5);
    C(1,2) = C(2,1);
    C(1,3) = C(3,1);
    C(2,3) = C(3,2);
end
function [E11bar,E22bar,E33bar,E12bar,E13bar,E23bar]=modifyStrain2(eps11,eps22,eps33,eps12,eps13,eps23)
    E11bar = zeros(size(eps11));
    E22bar = zeros(size(eps11));
    E33bar = zeros(size(eps11));
    E12bar = zeros(size(eps11));
    E13bar = zeros(size(eps11));
    E23bar = zeros(size(eps11));
    for k=1:length(eps11)
        eps_matrix = [eps11(k),eps12(k),eps13(k) ...
            ;eps12(k),eps22(k),eps23(k); ...
            eps13(k),eps23(k),eps33(k)];
        J = sqrt(det(eps_matrix*2+eye(3)));
        xpow = J^(-2/3);
        xpowplus = J^(2/3);
        E11bar(k) = real(xpow * (eps11(k) - 1/2 * (xpowplus - 1)));
        E22bar(k) = real(xpow * (eps22(k) - 1/2 * (xpowplus - 1)));
        E33bar(k) = real(xpow * (eps33(k) - 1/2 * (xpowplus - 1)));
        E12bar(k) = real(xpow * eps12(k)) ;
        E13bar(k) = real(xpow * eps13(k)) ;
        E23bar(k) = real(xpow * eps23(k)) ;        
    end
end


function [E11,E22,E33,E12,E13,E23]=modifyStrain(eps11,eps22,eps33,eps12,eps13,eps23)
    E11 = zeros(size(eps11));
    E22 = zeros(size(eps11));
    E33 = zeros(size(eps11));
    E12 = zeros(size(eps11));
    E13 = zeros(size(eps11));
    E23 = zeros(size(eps11));
    for k=1:length(eps11)
        eps_matrix = [eps11(k),eps12(k),eps13(k) ...
            ;eps12(k),eps22(k),eps23(k); ...
            eps13(k),eps23(k),eps33(k)];
        J = sqrt(det(eps_matrix*2+eye(3)));
        xpow = exp(log(J)*2/3);
        E11(k) = real(xpow * eps11(k) + 1/2 * (xpow - 1));
        E22(k) = real(xpow * eps22(k) + 1/2 * (xpow - 1));
        E33(k) = real(xpow * eps33(k) + 1/2 * (xpow - 1));
        E12(k) = real(xpow * eps12(k)) ;
        E13(k) = real(xpow * eps13(k)) ;
        E23(k) = real(xpow * eps23(k)) ;        
    end
end
function [C,E]=calcStep(epsMatrix,T)
    fun = @(c,eps) c(1)*eps(:,1) + c(2) * eps(:,2) + ...
                           c(3)*eps(:,3) + c(4) * eps(:,4) + ...
                           c(5)*eps(:,5) + c(6) * eps(:,6) + ...
                           c(7)*eps(:,1).^3 + c(8) * eps(:,2).^3 + ...
                           c(9)*eps(:,3).^3 + c(10) * eps(:,4).^3 + ...
                           c(11)*eps(:,5).^3 + c(12) * eps(:,6).^3  ...
                                                                ;
    X = epsMatrix;
    
    sig_trees.sig11=fitnlm(X,T.sig11,fun, rand(1,12));

    sig_trees.sig22 = fitnlm(X,T.sig22,fun, rand(1,12));
    sig_trees.sig33 = fitnlm(X,T.sig33,fun, rand(1,12));
    sig_trees.sig12 = fitnlm(X,T.sig12,fun, rand(1,12));
    sig_trees.sig13 = fitnlm(X,T.sig13,fun, rand(1,12));
    sig_trees.sig23 = fitnlm(X,T.sig23,fun, rand(1,12));
    coeffs1 = sig_trees.sig11.Coefficients.Estimate';
    coeffs2 = sig_trees.sig22.Coefficients.Estimate';
    coeffs3 = sig_trees.sig33.Coefficients.Estimate';
    coeffs4 = sig_trees.sig12.Coefficients.Estimate';
    coeffs5 = sig_trees.sig13.Coefficients.Estimate';
    coeffs6 = sig_trees.sig23.Coefficients.Estimate';
    
    
    C = zeros(6,6);
    E = zeros(6,6);
    C(1,:) = coeffs1(1:6); E(1,:) = coeffs1(7:12);
    C(2,:) = coeffs2(1:6); E(2,:) = coeffs2(7:12);
    C(3,:) = coeffs3(1:6); E(3,:) = coeffs3(7:12);
    C(4,:) = coeffs4(1:6); E(4,:) = coeffs4(7:12);
    C(5,:) = coeffs5(1:6); E(5,:) = coeffs5(7:12);
    C(6,:) = coeffs6(1:6); E(6,:) = coeffs6(7:12);
    
end




function J=cost(C,E, T)
   
    epsMatrix = [T.eps11,T.eps22,T.eps33,T.eps12,T.eps13,T.eps23];
    coeffs = [C,E];
    sig11 = predictStress(coeffs(1,:),epsMatrix);
    sig22 = predictStress(coeffs(2,:),epsMatrix);
    sig33 = predictStress(coeffs(3,:),epsMatrix);
    sig12 = predictStress(coeffs(4,:),epsMatrix);
    sig13 = predictStress(coeffs(5,:),epsMatrix);
    sig23 = predictStress(coeffs(6,:),epsMatrix);
    J = (sig11 - T.sig11).^2 + ...
        (sig22 - T.sig22).^2 + ...
        (sig33 - T.sig33).^2 + ...
        (sig12 - T.sig12).^2 + ...
        (sig13 - T.sig13).^2 + ...
        (sig23 - T.sig23).^2 ;
    J = sum(J,'all');
end


function newCoeffs=recoverTransformedCoefs(coeffs,diagElementIndex)

    C1 = coeffs(1);
    C2 = coeffs(2);
    C3 = coeffs(3);
    C4 = coeffs(4);
    C5 = coeffs(5);
    C6 = coeffs(6);
    if diagElementIndex == 1
        temp = C1;
        C2 = temp-C2^2;
        C3 = temp-C3^2;
        C4 = temp-C4^2;
        C5 = temp-C5^2;
        C6 = temp-C6^2;
    elseif diagElementIndex == 2
        temp = C2;
        C1 = temp-C1^2;
        C3 = temp-C3^2;
        C4 = temp-C4^2;
        C5 = temp-C5^2;
        C6 = temp-C6^2;
    elseif diagElementIndex == 3
        temp = C3;
        C1 = temp-C1^2;
        C2 = temp-C2^2;
        C4 = temp-C4^2;
        C5 = temp-C5^2;
        C6 = temp-C6^2;
    elseif diagElementIndex == 4
        temp = C4;
        C1 = temp-C1^2;
        C2 = temp-C2^2;
        C3 = temp-C3^2;
        C5 = temp-C5^2;
        C6 = temp-C6^2;
    elseif diagElementIndex == 5
        temp = C5;
        C1 = temp-C1^2;
        C2 = temp-C2^2;
        C3 = temp-C3^2;
        C4 = temp-C4^2;
        C6 = temp-C6^2;
    elseif diagElementIndex == 6
        temp = C6;
        C1 = temp-C1^2;
        C2 = temp-C2^2;
        C3 = temp-C3^2;
        C4 = temp-C4^2;
        C5 = temp-C5^2;
    end
    E1 = coeffs(7);
    E2 = coeffs(8);
    E3 = coeffs(9);
    E4 = coeffs(10);
    E5 = coeffs(11);
    E6 = coeffs(12);
    newCoeffs = [C1,C2,C3,C4,C5,C6,E1,E2,E3,E4,E5,E6];
end