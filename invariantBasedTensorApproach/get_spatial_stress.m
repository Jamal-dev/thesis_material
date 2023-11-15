function [sigma,sigmaBar,sigmaIso,sigmaVol] = get_spatial_stress(F,a0,params)
    J = det(F);
    Fbar = J^(-1/3)*F;
    Cbar = Fbar' * Fbar;
    C = F' * F;
    Bbar = Fbar * Fbar';
    aBar = Fbar * a0;
    aBarxaBar = aBar * aBar';
    [I1,I2,I3,I4,I5]=get5Invariants(Cbar,a0);
    dPsi.dI1 = diffUdI1(params,I1,I2,I3,I4,I5);
    dPsi.dI2 = diffUdI2(params,I1,I2,I3,I4,I5);
    dPsi.dI3 = diffUdI3(params,I1,I2,I3,I4,I5);
    dPsi.dI4 = diffUdI4(params,I1,I2,I3,I4,I5);
    dPsi.dI5 = diffUdI5(params,I1,I2,I3,I4,I5);

    gamaBar1 = 2 * (dPsi.dI1 + I1 * dPsi.dI2);
    gamaBar2 = - 2 * dPsi.dI2;

    gamaBar4 = 2 * dPsi.dI4;
    gamaBar5 = 2 * dPsi.dI5;

    sigmaBar = gamaBar1 * Bbar + ...
           gamaBar2 * Bbar*Bbar + ...
           gamaBar4 * aBarxaBar + ...
           gamaBar5 * dyadWithB(aBar,Bbar,aBar) ;
    sigmaIso = deviatoric(sigmaBar);
    p = dPsi.dI3;
    sigmaVol = p * eye(3);
    sigma = sigmaIso + sigmaVol;
    
end