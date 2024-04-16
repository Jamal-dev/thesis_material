function sigma = predict_stress(b,Cten2,kappa)
    %% $$\sigma =  -p * eye(3) + \frac{2}{J} \mathbf{b} \cdot \frac{\partial
    % \Psi}{\partial \mathbf{b}}$$
    %---------------------------------------------------
    J = sqrt(det(b));
    dUdJ = kappa*2*(J-1);
    % ddUdJdJ = kappa*2;
    p = dUdJ;
    % p_tilda = p + J * ddUdJdJ;

    bbar = J^(-2/3) * b;
    bbar_lin = [  bbar(1,1);  bbar(2,2);  bbar(3,3); ...
                2*bbar(1,2);2*bbar(1,3);2*bbar(2,3)];
    dPsidbbar_lin = Cten2*bbar_lin;
    dPsidbbar = [dPsidbbar_lin(1),dPsidbbar_lin(4),dPsidbbar_lin(5); ...
                 dPsidbbar_lin(4),dPsidbbar_lin(2),dPsidbbar_lin(6); ...
                 dPsidbbar_lin(5),dPsidbbar_lin(6),dPsidbbar_lin(3) ...
                 ];
    sigmaBar = 2/J * bbar * dPsidbbar;
    sigma = p*eye(3) + deviatoric(sigmaBar);
end