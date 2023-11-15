function sigma=dcauchyDParam(params,I1bar, ...
                                I2bar,I3, ...
                                I4bar,I5bar, ...
                                Cbar,inv_C, ...
                                C, ...
                                F, ...
                                a0xa0,Base5, ...
                                dUdI1_dparam, ...
                                dUdI2_dparam, ...
                                dUdI3_dparam, ...
                                dUdI4_dparam, ...
                                dUdI5_dparam)
    dPsi.dI1 = dUdI1_dparam(params,I1bar,I2bar,I3,I4bar,I5bar);
    dPsi.dI2 = dUdI2_dparam(params,I1bar,I2bar,I3,I4bar,I5bar);
    dPsi.dI3 = dUdI3_dparam(params,I1bar,I2bar,I3,I4bar,I5bar);
    dPsi.dI4 = dUdI4_dparam(params,I1bar,I2bar,I3,I4bar,I5bar);
    dPsi.dI5 = dUdI5_dparam(params,I1bar,I2bar,I3,I4bar,I5bar);

    gamaBar1 = 2 * (dPsi.dI1 + I1bar * dPsi.dI2);
    gamaBar2 = - 2 * dPsi.dI2;

    gamaBar4 = 2 * dPsi.dI4;
    gamaBar5 = 2 * dPsi.dI5;
    Sbar = gamaBar1 * eye(3) + ...
           gamaBar2 * Cbar + ...
           gamaBar4 * a0xa0 + ...
           gamaBar5 * Base5 ;
    J = I3;
    Svol = J * dPsi.dI3 * inv_C;
    % Projection = get_4th_I - 1/3 * get_dyad(inv_C,C);
    Projection = get_4th_symmetricI - 1/3 * get_dyad(inv_C,C);
    S_iso = J^(-2/3) * double_dotProd(Projection,Sbar);
    S = Svol +  S_iso;
    sigma = (1/J) * F * S * F';

end