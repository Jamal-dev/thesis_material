function [Fbar,F] = getFfromGreenLangrangeStrain(eps)
    C = eps*2 +eye(3);
    detJ  = sqrt(det(C));
    Cbar = detJ^(-2/3) * C;
    Fbar = chol(Cbar);
    F = detJ^(1/3) * Fbar;
    
end