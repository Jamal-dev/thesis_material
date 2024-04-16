function [E11,E22,E33,E12,E13,E23]=modifyStrain(eps11,eps22,eps33,eps12,eps13,eps23)
    E11 = zeros(size(eps11));
    E22 = zeros(size(eps11));
    E33 = zeros(size(eps11));
    E12 = zeros(size(eps11));
    E13 = zeros(size(eps11));
    E23 = zeros(size(eps11));
    for k=1:length(eps11)
        eps_matrix = [eps11(k),eps12(k),eps13(k) ...
            ;eps12(k),eps22(k),eps23(k); ...
            eps13(k),eps23(k),eps33(k)];
        J = sqrt(det(eps_matrix*2+eye(3)));
        xpow = J^(2/3);
        E11(k) = real(xpow * eps11(k) + 1/2 * (xpow - 1));
        E22(k) = real(xpow * eps22(k) + 1/2 * (xpow - 1));
        E33(k) = real(xpow * eps33(k) + 1/2 * (xpow - 1));
        E12(k) = real(xpow * eps12(k)) ;
        E13(k) = real(xpow * eps13(k)) ;
        E23(k) = real(xpow * eps23(k)) ;        
    end
end
