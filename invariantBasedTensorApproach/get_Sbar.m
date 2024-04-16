function [sigma,Sbar,S_iso,S] = get_Sbar(F,a0,params)
    C = F' * F;
    J = det(F);
    Fbar = J^(-1/3)*F;
    Cbar = Fbar' * Fbar;
    
    a0xa0 = a0 * a0';
    inv_C = inv(C);
    [I1,I2,~,I4,I5]=get5Invariants(Cbar,a0);
    I3 = J;
    dPsi.dI1 = diffUdI1(params,I1,I2,I3,I4,I5);
    dPsi.dI2 = diffUdI2(params,I1,I2,I3,I4,I5);
    dPsi.dI3 = diffUdI3(params,I1,I2,I3,I4,I5);
    dPsi.dI4 = diffUdI4(params,I1,I2,I3,I4,I5);
    dPsi.dI5 = diffUdI5(params,I1,I2,I3,I4,I5);

    gamaBar1 = 2 * (dPsi.dI1 + I1 * dPsi.dI2);
    gamaBar2 = - 2 * dPsi.dI2;

    gamaBar4 = 2 * dPsi.dI4;
    gamaBar5 = 2 * dPsi.dI5;
    Sbar = gamaBar1 * eye(3) + ...
           gamaBar2 * Cbar + ...
           gamaBar4 * a0xa0 + ...
           gamaBar5 * dyadWithB(a0,Cbar,a0) ;
    Svol = J * dPsi.dI3 * inv_C;
    % Projection = get_4th_I - 1/3 * get_dyad(inv_C,C);
    Projection = get_4th_symmetricI - 1/3 * get_dyad(inv_C,C);
    S_iso = J^(-2/3) * double_dotProd(Projection,Sbar);
    S = Svol +  S_iso;
    P = F*S;
    sigma = (1/J) * F * S * F';
end