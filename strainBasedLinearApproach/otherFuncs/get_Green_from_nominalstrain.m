function [eps_G_lin,F,b]=get_Green_from_nominalstrain(eps_nominal)
    % eps_nominal = sqrt(F * F.T) -1= v-1
    eps_nom = [eps_nominal(1)  , eps_nominal(4), eps_nominal(5); ...
               eps_nominal(4), eps_nominal(2)  , eps_nominal(6); ...
               eps_nominal(5), eps_nominal(6), eps_nominal(3)  ; ];
    v = eps_nom + eye(3);
    b = v*v;
    
    % detJ = det(v);
    [R, ~, ~] = poldecomp(v);
    F = v * inv(R);
    % F = chol(b)';
    C = F'*F;
    eps_G = 1/2 * (C-eye(3));
    eps_G_lin = [eps_G(1,1), eps_G(2,2), eps_G(3,3), eps_G(1,2),eps_G(1,3),eps_G(2,3)];
    % Fbar = detJ^(-1/3) * F;
    % Bbar = detJ^(-2/3) * b;
    
end
