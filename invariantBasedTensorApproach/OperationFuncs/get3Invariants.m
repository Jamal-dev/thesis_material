function [I,II,III]=get3Invariants(U)
    I = trace(U);
    II = 1/2*(I^2 - trace(U*U));
    III = det(U);
end