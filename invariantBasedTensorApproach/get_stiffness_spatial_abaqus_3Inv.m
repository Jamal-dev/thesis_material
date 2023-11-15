function [spatial_Cij]=get_stiffness_spatial_abaqus_3Inv(F,a0,params)
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
    
    delta1 = 2 * (dPsi.dI1 + I1 * dPsi.dI2);
    delta2 = -2 * dPsi.dI2;
    delta3 = 4 * (ddPsi.dI1dI1 +  ...
                    2*I1 * ddPsi.dI1dI2 + ...
                    dPsi.dI2 + ...
                    I1^2 * ddPsi.dI2dI2);
    delta4 = -4 * (ddPsi.dI1dI2 + I1 * ddPsi.dI2dI2);
    delta5 = 4 * ddPsi.dI2dI2;
    delta6 = -4/3 * (dPsi.dI1+ ...
                    2*I1*dPsi.dI2 + ...
                    I1*ddPsi.dI2dI2 + ...
                    (I1^2 + 2*I2)*ddPsi.dI1dI2 + ...
                    2*I1*I2*ddPsi.dI2dI2);
    delta7 = 4/3 * (2*dPsi.dI2+ ...
                    I1*ddPsi.dI1dI2 + ...
                    2*I2*ddPsi.dI2dI2);
    H1=get_H1(Bbar);
    H2=get_H2(Bbar);

    spatial_c_ijkl = 1/J * (delta1 * H1 + ...
                            delta2 * H2 + ...
                            delta3 * get_dyad(Bbar,Bbar) + ...
                            delta4 * (get_dyad(Bbar*Bbar,Bbar) ...
                                      +get_dyad(Bbar,Bbar*Bbar)) +...
                            delta5 * get_dyad(Bbar*Bbar,Bbar*Bbar) + ...
                            delta6 * (get_dyad(eye(3),Bbar)+get_dyad(Bbar,eye(3))) ...
                            + delta7 * (get_dyad(eye(3),Bbar*Bbar)+get_dyad(Bbar*Bbar,eye(3))) ...
                            );
    spatial_Cij=convert4thOrder2Mat(spatial_c_ijkl);

    
end

function H1=get_H1(Bbar,H1)
    if nargin == 1
        H1 = zeros(3,3,3,3);
    end
    for i=1:3
        for j=1:3
            for l=1:3
                for k=1:3
                    H1(i,j,k,l) = H1(i,j,k,l) +0.5*( ...
                                    krnDel(i,k)*Bbar(j,l) + ...
                                    krnDel(j,l)*Bbar(i,k) + ...
                                    krnDel(i,l)*Bbar(j,k) + ...
                                    krnDel(j,k)*Bbar(i,l)) ;
                end
            end
        end
    end

end

function H2=get_H2(Bbar)
    if nargin == 1
        H2 = zeros(3,3,3,3);
    end
    Bbar_Bbar = Bbar * Bbar; 
    for i=1:3
        for j=1:3
            for l=1:3
                for k=1:3
                    H2(i,j,k,l) = H2(i,j,k,l) +0.5*( ...
                                    krnDel(i,k)*Bbar_Bbar(j,l) + ...
                                    krnDel(j,l)*Bbar_Bbar(i,k) + ...
                                    krnDel(i,l)*Bbar_Bbar(j,k) + ...
                                    krnDel(j,k)*Bbar_Bbar(i,l))+ ...
                                    Bbar(i,k)*Bbar(j,l)+ ...
                                    Bbar(i,l)*Bbar(j,k);
                end
            end
        end
    end

end