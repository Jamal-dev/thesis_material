function [spatial_Cij]=get_stiffness_spatial(F,a0,params)
    J = det(F);
    Fbar = J^(-1/3)*F;
    Cbar = Fbar' * Fbar;
    C = F' * F;
    inv_C = inv(C);
    B = F * F';
    Bbar = Fbar * Fbar';
    aBar = Fbar * a0;
    aBarxaBar = aBar * aBar';
    IxI = get_dyad(eye(3),eye(3));

    [I1,I2,I3,I4,I5]=get5Invariants(Cbar,a0);
    dPsi.dI1 = diffUdI1(params,I1,I2,I3,I4,I5);
    dPsi.dI2 = diffUdI2(params,I1,I2,I3,I4,I5);
    dPsi.dI3 = diffUdI3(params,I1,I2,I3,I4,I5);
    dPsi.dI4 = diffUdI4(params,I1,I2,I3,I4,I5);
    dPsi.dI5 = diffUdI5(params,I1,I2,I3,I4,I5);

    ddPsi.dI1dI1 = diffUdI1dI1(params,I1,I2,I3,I4,I5);
    ddPsi.dI1dI2 = diffUdI1dI2(params,I1,I2,I3,I4,I5);
    ddPsi.dI2dI2 = diffUdI2dI2(params,I1,I2,I3,I4,I5);

    ddPsi.dI1dI4 = diffUdI1dI4(params,I1,I2,I3,I4,I5);
    ddPsi.dI1dI5 = diffUdI1dI5(params,I1,I2,I3,I4,I5);

    ddPsi.dI2dI4 = diffUdI2dI4(params,I1,I2,I3,I4,I5);
    ddPsi.dI2dI5 = diffUdI2dI5(params,I1,I2,I3,I4,I5);

    ddPsi.dI4dI4 = diffUdI4dI4(params,I1,I2,I3,I4,I5);
    ddPsi.dI5dI5 = diffUdI5dI5(params,I1,I2,I3,I4,I5);
    ddPsi.dI4dI5 = diffUdI4dI5(params,I1,I2,I3,I4,I5);

    delta1 = 4 * (ddPsi.dI1dI1 +  ...
                    2*I1 * ddPsi.dI1dI2 + ...
                    2*dPsi.dI2 + ...
                    I1^2 * ddPsi.dI2dI2);
    delta2 = -4 * (ddPsi.dI1dI2 + I1 * ddPsi.dI2dI2);
    delta3 = 4 * ddPsi.dI2dI2;
    delta4 = -4 * dPsi.dI2;

    delta5 = 4 * (ddPsi.dI1dI4 + I1 * ddPsi.dI2dI4);
    delta6 = -4 * ddPsi.dI2dI4;
    delta7 = 4 * ddPsi.dI4dI4;

    delta8 = 4 * (ddPsi.dI1dI5 + I1 * ddPsi.dI2dI5);
    delta9 = -4 * ddPsi.dI2dI5;

    delta10 = 4 * ddPsi.dI5dI5;

    delta11 = -2 * dPsi.dI4;

    delta12 = 4 * dPsi.dI5;

    delta13 = -2 * dPsi.dI5;

    C_ijkl_BAR_spatial = delta1 * get_dyad(Bbar,Bbar) + ...
                   delta2 * (get_dyad(Bbar,Bbar*Bbar)+get_dyad(Bbar*Bbar,Bbar)) ...
                   + delta3 * get_dyad(Bbar*Bbar,Bbar*Bbar) ...
                   + delta4 * get_dyad(Bbar,Bbar) ...
                   + delta5 * (get_dyad(aBarxaBar,Bbar)+get_dyad(Bbar,aBarxaBar)) ...
                   + delta6 * (get_dyad(aBarxaBar,Bbar*Bbar)+get_dyad(Bbar*Bbar,aBarxaBar)) ...
                   + delta7 * get_dyad(aBarxaBar,aBarxaBar) ...
                   + delta8 * (get_dyad(get_dyad(Bbar*aBar,aBar),Bbar) ...
                                +get_dyad(get_dyad(aBar,Bbar*aBar),Bbar) ...
                                +get_dyad(Bbar,get_dyad(Bbar*aBar,aBar)) ...
                                +get_dyad(Bbar,get_dyad(aBar,Bbar*aBar)) ...
                                ) ...
                   + delta9 * (get_dyad(get_dyad(Bbar*aBar,aBar),Bbar*Bbar) ...
                                +get_dyad(get_dyad(aBar,Bbar*aBar),Bbar*Bbar) ...
                                +get_dyad(Bbar*Bbar,get_dyad(Bbar*aBar,aBar)) ...
                                +get_dyad(Bbar*Bbar,get_dyad(aBar,Bbar*aBar)) ...
                                ) ...
                   + delta10 * (get_dyad(get_dyad(Bbar*aBar,aBar),get_dyad(Bbar*aBar,aBar)) ...
                                +get_dyad(get_dyad(aBar,Bbar*aBar),get_dyad(aBar,Bbar*aBar)) ...
                                +get_dyad(get_dyad(aBar,Bbar*aBar),get_dyad(Bbar*aBar,aBar)) ...
                                +get_dyad(get_dyad(Bbar*aBar,aBar),get_dyad(aBar,Bbar*aBar)) ...
                                ) ...
                   + delta11 * Base11(Bbar,aBar) ...
                   + delta12 * Base12(Bbar,aBar) ...
                   + delta13 * Base13(Bbar,aBar) ...
                   ;
    [sigma,sigmaBar,sigmaIso,sigmaVol] = get_spatial_stress(F,a0,params);
    tauBar = sigmaBar * J;
    tauIso = sigmaIso * J;
    Proj_I = get_4th_I - 1/3 * get_dyad(eye(3),eye(3));
    spatial_c_ijklISO = 1/J * ...
                              ( double_dotProd(Proj_I, ...
                                            double_dotProd(C_ijkl_BAR_spatial,Proj_I) ...
                                              ) ...
                                +2/3 * trace(tauBar) * Proj_I ...
                                -2/3 * (get_dyad(eye(3),tauIso) + ...
                                        get_dyad(tauIso,eye(3)) ) ...
                                    );
    p = dPsi.dI3;
    p_tilda = p;
    spatial_c_ijklVOL = p_tilda * IxI - 2* p *  get_4th_I;

    spatial_c_ijkl = spatial_c_ijklVOL + spatial_c_ijklISO;
    
    spatial_Cij=convert4thOrder2Mat(spatial_c_ijkl);
    mu = params(2)*2;
    C_ijkl_iso_NeoHok = 2*mu*(get_4th_I/3*I1 ...
                        -I1/9*get_dyad(eye(3),eye(3)) ...
                        -1/3*get_dyad(eye(3),deviatoric(Bbar)) ...
                        -1/3*get_dyad(deviatoric(Bbar),eye(3)) ...
                        ); 
    C_ij_iso_NeoHok = convert4thOrder2Mat(C_ijkl_iso_NeoHok);
end


function C=Base11(Bbar,aBar)
    invBbar = inv(Bbar);
    C = zeros(3,3,3,3);
    Bbar_aBar = Bbar * aBar;
    aBar_invBbar = single_dotProd(aBar,invBbar);
    for i=1:3
        for j=1:3
            for k=1:3
                for l=1:3
                    
                    C(i,j,k,l) = C(i,j,k,l) + ...
                                Bbar_aBar(i) ...
                                  *(aBar_invBbar(k) * krnDel(j,l) +...
                                        aBar(l)*invBbar(j,k) ...
                                        );
                    
                end
            end
        end
    end
end

function C=Base12(Bbar,aBar)
    C = zeros(3,3,3,3);
    Bbar_aBar = Bbar * aBar; 
    for i=1:3
        for j=1:3
            for k=1:3
                for l=1:3
                    
                    C(i,j,k,l) = C(i,j,k,l) + ...
                                Bbar(i,k)*aBar(l)*aBar(j) ...
                                  + Bbar_aBar(i)*krnDel(j,k)*aBar(l);
                    
                end
            end
        end
    end
end

function C=Base13(Bbar,aBar)
    invBbar = inv(Bbar);
    C = zeros(3,3,3,3);
    Bbar_aBar = Bbar * aBar;
    Bbar_Bbar_aBar = Bbar * Bbar * aBar;
    aBar_invBbar = single_dotProd(aBar,invBbar);
    for i=1:3
        for j=1:3
            for k=1:3
                for l=1:3
                    
                    C(i,j,k,l) = C(i,j,k,l) + ...
                                Bbar_Bbar_aBar(i) * aBar_invBbar(k) * krnDel(j,l) ...
                                + Bbar_Bbar_aBar(i) * invBbar(j,k) * aBar(l) ...
                                + Bbar_aBar(i) * aBar(k) * krnDel(j,l) ...
                                + Bbar_aBar(i) * invBbar(j,k) * Bbar_aBar(l);
                    
                end
            end
        end
    end
end


