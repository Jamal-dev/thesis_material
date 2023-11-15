function dUdJ=caldUdJ(eps_vec,dUdE,F)
    if nargin == 2
       eps_mat=col2Mat(eps_vec);
       [~,F] = getFfromGreenLangrangeStrain(eps_mat);
       J = det(F);
    else
        J = det(F);
    end
    term1 = 2/3 * 1/J;
    dE11Dj = term1 * (eps_vec(1)+1/2);
    dE22Dj = term1 * (eps_vec(2)+1/2);
    dE33Dj = term1 * (eps_vec(3)+1/2);
    dE12Dj = term1 * (eps_vec(4));
    dE13Dj = term1 * (eps_vec(5));
    dE23Dj = term1 * (eps_vec(6));
    dUdJ = dUdE(1) * dE11Dj + ...
           dUdE(2) * dE22Dj + ...
           dUdE(3) * dE33Dj + ...
           2* (dUdE(4) * dE12Dj + ...
               dUdE(5) * dE13Dj + ...
               dUdE(6) * dE23Dj );
    dUdJ = - dUdJ;
end