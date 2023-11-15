clear all;
clc;
syms I_1 I_2 I_4alpha I_5alpha theta phi lambda_1 lambda_2 lambda_3
syms J D
assume(I_1,'real'); assume(I_2,'real'); assume(I_4alpha,'real');
assume(I_5alpha,'real');
assume(J,'real');
assume(D,'real');
 assume(theta,'real'); assume(phi,'real'); assume(lambda_1,'real');
 assume(lambda_2,'real');
  assume(lambda_3,'real');


% F = diag([lambda_1, lambda_2, lambda_3]);
% 
% J = 1;    % for incompressible materials
% Fbar = F; % for incompressible materials
% 
% I1 = trace(Fbar);
% 
% Bbar = Fbar * Fbar';
% 
% dev_Bbar = dev(Bbar);
% 
% ai = [cos(phi); sin(phi)*sin(theta); sin(phi)*cos(theta)];
% fiber_direction = Fbar * ai;
% 
% I4alpha = dot(fiber_direction,fiber_direction);
% 
% fiber_dyad = fiber_direction * fiber_direction';
% 
% dev_fiber_dyad = dev(fiber_dyad);

syms params [1 10]
[derv_U_I1,derv_U_I4] =psi(I_1,I_2,I_4alpha,I_5alpha,J,params);
clear all;

function [derv_U_I1,derv_U_I4] =psi(I1,I2,I4,I5,J,params)
    U = 1/params(1) * (J-1)        + ...
        params(2) * (I1-3) + ...
        params(3) * (I1-3)^2 +  ...
        params(4) * (I1-3)^3 +  ...
        params(5) * (I4-1) + ...
        params(6) * (I4-1)^2 + ...
        params(7) * (I4-1)^3 + ...
        params(8) * (I5-1) + ...
        params(9) * (I5-1)^2 + ...
        params(10) * (I5-1)^3  ...
        ;
    % Ealpha = params(4)*(I1-3) + (1-3*params(4))*(I4-1);
    % U = params(1) * (I1-3) + ...
    %     params(2)/params(3) * ...
    %     (exp(params(3)*Ealpha^2)-1);

    
    derv_U_I1 = diff(U,I1);
    derv_U_I2 = diff(U,I2);
    derv_U_I3 = diff(U,J);
    derv_U_I4 = diff(U,I4);
    derv_U_I5 = diff(U,I5);
    matlabFunction(derv_U_I1,"File","diffUdI1","Vars",{params, I1,I2,J, I4,I5});
    matlabFunction(derv_U_I2,"File","diffUdI2","Vars",{params, I1,I2,J, I4,I5});
    matlabFunction(derv_U_I3,"File","diffUdI3","Vars",{params, I1,I2,J, I4,I5});
    matlabFunction(derv_U_I4,"File","diffUdI4alpha","Vars",{params, I1,I2,J, I4,I5});
    matlabFunction(derv_U_I5,"File","diffUdI5alpha","Vars",{params, I1,I2,J, I4,I5});
end


function res = dev(tensor)
    I = eye(3);
    res = tensor - 1/3 * trace(tensor) * I;
end
