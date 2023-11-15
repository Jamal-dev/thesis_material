function sigma=cauchyStress(params,I1bar, ...
                                I2bar,I3bar, ...
                                I4bar,I5bar, ...
                                Cbar,inv_C, ...
                                C, ...
                                F, ...
                                a0xa0,Base5)
    dPsi.dI1 = diffUdI1(params,I1bar,I2bar,I3bar,I4bar,I5bar);
    dPsi.dI2 = diffUdI2(params,I1bar,I2bar,I3bar,I4bar,I5bar);
    dPsi.dI3 = diffUdI3(params,I1bar,I2bar,I3bar,I4bar,I5bar);
    dPsi.dI4 = diffUdI4(params,I1bar,I2bar,I3bar,I4bar,I5bar);
    dPsi.dI5 = diffUdI5(params,I1bar,I2bar,I3bar,I4bar,I5bar);

    gamaBar1 = 2 * (dPsi.dI1 + I1bar * dPsi.dI2);
    gamaBar2 = - 2 * dPsi.dI2;

    gamaBar4 = 2 * dPsi.dI4;
    gamaBar5 = 2 * dPsi.dI5;
    Sbar = gamaBar1 * eye(3) + ...
           gamaBar2 * Cbar + ...
           gamaBar4 * a0xa0 + ...
           gamaBar5 * Base5 ;
    J = I3bar;
    Svol = J * dPsi.dI3 * inv_C;
    % Projection = get_4th_I - 1/3 * get_dyad(inv_C,C);
    Projection = get_4th_symmetricI - 1/3 * get_dyad(inv_C,C);
    S_iso = J^(-2/3) * double_dotProd(Projection,Sbar);
    S = Svol +  S_iso;
    sigma = (1/J) * F * S * F';

end