function [F,Fbar,b,Bbar,eps_nom]=get_F_from_nominalstrain(eps_nominal)
    % eps_nominal = sqrt(F * F.T) -1= v-1
    eps_nom = [eps_nominal(1)  , eps_nominal(4), eps_nominal(5); ...
               eps_nominal(4), eps_nominal(2)  , eps_nominal(6); ...
               eps_nominal(5), eps_nominal(6), eps_nominal(3)  ; ];
    v = eps_nom + eye(3);
    b = v*v;
    
    detJ = det(v);
    F = chol(b)';
    Fbar = detJ^(-1/3) * F;
    Bbar = detJ^(-2/3) * b;
    
end