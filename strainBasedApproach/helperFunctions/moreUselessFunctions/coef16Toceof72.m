function flat=coef16Toceof72(coef)
    coefs = zeros(6,12);
    coefs(1,1:3)= coef(1:3);
    coefs(2,2:3)= coef(4:5);
    coefs(3,3)  = coef(6);

    coefs(4,4:5)= coef(7:8);
    coefs(5,5)= coef(9);
    coefs(6,6)  = coef(10);

    coefs(1,7) = coef(11);
    coefs(2,8) = coef(12);
    coefs(3,9) = coef(13);
    coefs(4,10) = coef(14);
    coefs(5,11) = coef(15);
    coefs(6,12) = coef(16);
    R=decomposeAndSym(coefs);
    flat = reshape(R',1,[]);
end