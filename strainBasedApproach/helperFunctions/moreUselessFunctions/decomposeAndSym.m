function R=decomposeAndSym(coefs)
    A = coefs(1:6,1:6);
    B = coefs(1:6,7:12);
    A = createLowerPart(A);
    B = createLowerPart(B);
    R = [A,B];
end