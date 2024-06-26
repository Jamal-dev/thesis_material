% x_min = [2.447228102	10.4389943	-5.438142207	-276.1781545	-274.8033369	-64.44578116	1.11411983	7.920265497	0.264411347];
% eps_ne = get_eps_matrix(1,-0.2929,-0.2929,0,0,0);
% params = zeros(1,9);
% params(1) = 9.82946180372075;
params = [10	9.82946180372075	0	0	0	0	0	0	0	0];
% eps_ne = get_eps_matrix(1,-0.2929,-0.2929,0,0,0);
eps_ne = get_eps_matrix(1,0.5509,0.5509,0,0,0);
ai = [1,0,0];
[cauchy_dev_stress,tableComp]=pipeLine(params,eps_ne,ai);
F_abq = [2	1.473	1.473	8.51E-18	-4.40E-16	-0.4854	-5.06E-16	-2.47E-16	0.4854];
F_abq = abq_defGradMat(F_abq);
Bbar_abq = F_abq * F_abq';
J_abq = det(F_abq);
Cbar_abq = F_abq' * F_abq;
function T=checkDev(params,eps_ne,ai)
      FirstInvariant=   3.09174310343583      ;
 dUdI1=   9.82946000000000      ;
 SecondInvariant=   3.08192929955025      ;
 dUdI2=  0.000000000000000E+000 ;
 ThirdInvariant=   4.81083741755879      ;
 dUdIJ=  0.762167483511758      ;
 FourthInvariant=   1.40360919486468      ;
 dUdI4=  0.000000000000000E+000 ;
 FifthInvariant=   1.97011877190869      ;
 dUdI5=  0.000000000000000E+000 ;
     
     D = 10;
     [B,Bbar] = get_B_mat(eps_ne);
     
     [I1bar,I2bar,I4alpha,I5alpha,Fbar,Cbar,J]=getPseduInvariantsFROM_B(Bbar,ai,B);
     pred.dUdI1 = diffUdI1(params,I1bar,I2bar,I4alpha,I5alpha);
     pred.dUdI2 = diffUdI2(params,I1bar,I2bar,I4alpha,I5alpha);
     pred.dUdJ = 2/D * (J-1);
     pred.dUdI4 = diffUdI4alpha(params,I1bar,I2bar,I4alpha,I5alpha);
     pred.dUdI5 = diffUdI5alpha(params,I1bar,I2bar,I4alpha,I5alpha);
     T = table(zeros(10,1),zeros(10,1),zeros(10,1), ...
         'VariableNames',{'Pred','Abaqus','Diff'},'RowNames', ...
            {'I1','I2','J','I4','I5','dUdI1','dUdI2','dUdJ','dUdI4','dUdI5'});
     T(1,1:2) = {I1bar,FirstInvariant};
     T(2,1:2) = {I2bar,SecondInvariant};
     T(3,1:2) = {J,ThirdInvariant};
     T(4,1:2) = {I4alpha,FourthInvariant};
     T(5,1:2) = {I5alpha,FifthInvariant};
     T(6,1:2) = {pred.dUdI1,dUdI1};
     T(7,1:2) = {pred.dUdI2,dUdI2};
     T(8,1:2) = {pred.dUdJ,dUdIJ};
     T(9,1:2) = {pred.dUdI4,dUdI4};
     T(10,1:2) = {pred.dUdI5,dUdI5};
     T.Diff = abs(T.Pred-T.Abaqus);

end

function [cauchy_dev,T]=pipeLine(params,eps_ne,ai)
    
    [B,Bbar] = get_B_mat(eps_ne);
    ai = [1;0;0];
    [I1bar,I2bar,I4alpha,I5alpha,Fbar,Cbar,J]=getPseduInvariantsFROM_B(Bbar,ai,B);
    a_alpha = Fbar * ai;
    a_dash_alpha = Bbar * ai;
    fiber_dyad_I4 = getDyad(a_alpha,a_alpha);
    fiber_dyad_I5 = dyadWithB(a_alpha,Bbar,a_alpha);
    dev_fiber_dyadI4 = deviatoric(fiber_dyad_I4);
    dev_fiber_dyadI5 = deviatoric(fiber_dyad_I5);
    dU.dI1 = diffUdI1(params,I1bar,I2bar,J,I4alpha,I5alpha);
    dU.dI2 = diffUdI2(params,I1bar,I2bar,J,I4alpha,I5alpha) ;
    dU.dJ = diffUdI3(params,I1bar,I2bar,J,I4alpha,I5alpha) ;
    dU.dI4 = diffUdI4alpha(params,I1bar,I2bar,J,I4alpha,I5alpha);
    dU.dI5 = diffUdI5alpha(params,I1bar,I2bar,J,I4alpha,I5alpha);
    vol_comp = dU.dJ;
    dev_Bbar = deviatoric(Bbar);
    comp1 = (dU.dI1 + I1bar * dU.dI2) * dev_Bbar - dU.dI2 * (dev_Bbar*dev_Bbar);
    

    cauchy_stress = 2/J * (dU.dI1 + dU.dI2 * I1bar) * dev_Bbar ...
                    -2/J * dU.dI2 * (dev_Bbar*dev_Bbar) ...
                    + vol_comp * eye(3) ...
                    + 2*dU.dI4 * dev_fiber_dyadI4 ...
                    + 2*dU.dI5 * dev_fiber_dyadI5;
    
    cauchy_dev = 2/J* deviatoric(comp1) +  2*dU.dI4 * dev_fiber_dyadI4 + ...
        2*dU.dI5 * dev_fiber_dyadI5;
    T=checkDev(params,eps_ne,ai);
    
end

function mat = getDyad(vec1,vec2)
    mat = vec1 * vec2';
end
function [B,Bbar] = get_B_mat(eps_nominal)
    B = (eps_nominal + eye(3)).^2;
    detJ = sqrt(det(B));
    Bbar = detJ^(-2/3) * B;
end

function [I1bar,I2bar,I4alpha,I5alpha,Fbar,Cbar,J]=getPseduInvariantsFROM_B(Bbar,ai,B)
    Fbar = chol(Bbar);
    J = sqrt(det(B));
    % Bbar = Fbar  * Fbar';
    Cbar = Fbar'  * Fbar;
    I1bar = trace(Bbar);
    I2bar = 1/2*(I1bar^2 - trace(Bbar*Bbar));
    I4alpha = dotProd(Cbar,ai);
    I5alpha = dotProd(Cbar*Cbar,ai);
end