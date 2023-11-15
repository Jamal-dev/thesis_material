function [F,B,eps_nom]=get_F_from_nominalstrain(eps_nominal)
    % eps_nominal = sqrt(F * F.T) -1
    eps_nom = [eps_nominal(1)  , eps_nominal(4), eps_nominal(5); ...
               eps_nominal(4), eps_nominal(2)  , eps_nominal(6); ...
               eps_nominal(5), eps_nominal(6), eps_nominal(3)  ; ];
    eps = eps_nom + eye(3);
    B = eps.^2;
    F = chol(B)';
    
end