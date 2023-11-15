function eps=get_eps_matrix(eps11,eps22,eps33,eps12,eps13,eps23)
    if nargin ==1
        eps22 = eps11(2);
        eps33 = eps11(3);
        eps12 = eps11(4);
        eps13 = eps11(5);
        eps23 = eps11(6);
        eps11 = eps11(1);
    end
    eps = [eps11, eps12, eps13;
           eps12, eps22, eps23;
           eps13, eps23, eps33];
    

end