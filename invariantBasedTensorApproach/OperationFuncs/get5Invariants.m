function [I1,I2,I3,I4,I5]=get5Invariants(U,a0)
    [I1,I2,I3] = get3Invariants(U);
    dyad = a0 * a0';
    I4 = doubleDotProd(dyad,U);
    I5 = doubleDotProd(dyad,U*U);

end