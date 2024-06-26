function [spatial_cij, ...
            ref_Cij, ...
            ref_CijklBar, ...
            ref_Cijkl_iso, ...
            ref_Cijkl_vol, ...
            ref_Cijkl]=get_stiffness(F,a0,params)
    J = det(F);
    Fbar = J^(-1/3)*F;
    Cbar = Fbar' * Fbar;
    C = F' * F;
    inv_C = inv(C);
    [I1,I2,~,I4,I5]=get5Invariants(Cbar,a0);
    I3 = J;
    
    dPsi.dI1 = diffUdI1(params,I1,I2,I3,I4,I5);
    dPsi.dI2 = diffUdI2(params,I1,I2,I3,I4,I5);
    dPsi.dI3 = diffUdI3(params,I1,I2,I3,I4,I5);
    dPsi.dI4 = diffUdI4(params,I1,I2,I3,I4,I5);
    dPsi.dI5 = diffUdI5(params,I1,I2,I3,I4,I5);

    ddPsi.dI1dI1 = diffUdI1dI1(params,I1,I2,I3,I4,I5);
    ddPsi.dI1dI2 = diffUdI1dI2(params,I1,I2,I3,I4,I5);
    ddPsi.dI2dI2 = diffUdI2dI2(params,I1,I2,I3,I4,I5);

    ddPsi.dI3dI3 = diffUdI3dI3(params,I1,I2,I3,I4,I5);

    ddPsi.dI1dI4 = diffUdI1dI4(params,I1,I2,I3,I4,I5);
    ddPsi.dI1dI5 = diffUdI1dI5(params,I1,I2,I3,I4,I5);

    ddPsi.dI2dI4 = diffUdI2dI4(params,I1,I2,I3,I4,I5);
    ddPsi.dI2dI5 = diffUdI2dI5(params,I1,I2,I3,I4,I5);

    ddPsi.dI4dI4 = diffUdI4dI4(params,I1,I2,I3,I4,I5);
    ddPsi.dI5dI5 = diffUdI5dI5(params,I1,I2,I3,I4,I5);
    ddPsi.dI4dI5 = diffUdI4dI5(params,I1,I2,I3,I4,I5);

    delta1 = 4 * (ddPsi.dI1dI1 +  ...
                    2*I1 * ddPsi.dI1dI2 + ...
                    dPsi.dI2 + ...
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

    delta11 = 4 * ddPsi.dI4dI5;

    delta12 = 4 * dPsi.dI5;

    a0xa0 = get_dyad(a0,a0);
    IxI = get_dyad(eye(3),eye(3));
    dI5dC = dyadWithB(a0,Cbar,a0);
    ddI5ddC = get_dyad(a0,single_dotProd(get_4th_symmetricI,a0)) + ...
              get_dyad(single_dotProd(a0,get_4th_symmetricI),a0); 

    ref_CijklBar = delta1 * IxI + ...
            delta2 * (get_dyad(eye(3),Cbar) + get_dyad(Cbar,eye(3))) + ...
            delta3 * get_dyad(Cbar,Cbar) + ...
            delta4 * get_4th_symmetricI + ...
            delta5 * (get_dyad(eye(3),a0xa0) + get_dyad(a0xa0,eye(3))) + ...
            delta6 * (get_dyad(Cbar,a0xa0) + get_dyad(a0xa0,Cbar)) + ...
            delta7 * get_dyad(a0xa0,a0xa0) + ...
            delta8 * (get_dyad(eye(3),dI5dC) + get_dyad(dI5dC,eye(3))) + ...
            delta9 * (get_dyad(Cbar,dI5dC) + get_dyad(dI5dC,Cbar)) + ...
            delta10 * (get_dyad(dI5dC,dI5dC) ) + ...
            delta11 * (get_dyad(a0xa0,dI5dC) + get_dyad(dI5dC,a0xa0)) + ...
            delta12 * ddI5ddC;
    % ref_CijklBar = J^(-4/3) * ref_CijklBar;

    [sigma,Sbar,S_iso,~] = get_Sbar(F,a0,params);
    % In one paper it was suggested get_4th_I to be symmetric
    % Proj =       get_4th_I - 1/3 * get_dyad(inv_C,C);
    % Proj_trans = get_4th_I - 1/3 * get_dyad(C    ,inv_C);
    Proj =       get_4th_symmetricI - 1/3 * get_dyad(inv_C,C);
    Proj_trans = get_4th_symmetricI - 1/3 * get_dyad(C    ,inv_C);
    Proj_tilda = sym_dyad_prod(inv_C,inv_C) - 1/3 * get_dyad(inv_C,inv_C);
    Tr_Sbar = double_dotProd(J^(-2/3)*Sbar,C);
    ref_Cijkl_iso = double_dotProd(Proj,double_dotProd(ref_CijklBar,Proj_trans)) ...
                + 2/3 * Tr_Sbar * Proj_tilda ...
                -2/3 * (get_dyad(inv_C,S_iso)+get_dyad(S_iso,inv_C));
    p = dPsi.dI3;
    p_tilda = p + J * ddPsi.dI3dI3;
    
    ref_Cijkl_vol = J * p_tilda * get_dyad(inv_C,inv_C) ...
                - 2 * J * p *  sym_dyad_prod(inv_C,inv_C);
    ref_Cijkl = ref_Cijkl_vol + ref_Cijkl_iso;
    ref_Cij=convert4thOrder2Mat(ref_Cijkl);
    
    spatial_Cijkl=push_forward_4thOrderTensor(ref_Cijkl,F,sigma);
    c_abaqus_dash = get_abaqus_c_dash_tensor(sigma);
    c_ijkl_spatial_abaqus = spatial_Cijkl + c_abaqus_dash;
    spatial_cij=convert4thOrder2Mat(c_ijkl_spatial_abaqus);


    % spatial_c_ijklBAR = push_forward_4thOrderTensor(ref_CijklBar,F,sigma);
    % Proj_I = get_4th_I - 1/3 * get_dyad(eye(3),eye(3));
    % sigma_iso = sigma - p*eye(3);
    % sigmaBar = deviatoric(sigma_iso);
    % tauIso = J * sigma_iso;
    % tauBar = J * sigmaBar;
    % spatial_c_ijklISO = 1/J * ...
    %                           ( double_dotProd(Proj_I, ...
    %                                         double_dotProd(spatial_c_ijklBAR,Proj_I) ...
    %                                           ) ...
    %                             +2/3 * trace(tauBar) * Proj_I ...
    %                             -2/3 * (get_dyad(eye(3),tauIso) + ...
    %                                     get_dyad(tauIso,eye(3)) ) ...
    %                                 );
    % spatial_c_ijklVOL = p_tilda * IxI - 2* p *  get_4th_I;
    % 
    % spatial_c_ijkl = spatial_c_ijklVOL + spatial_c_ijklISO;
    % 
    % spatial_Cij=convert4thOrder2Mat(spatial_c_ijkl);
            


end