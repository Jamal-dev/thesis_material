function B = deviatoric(A)
    B = A - trace(A) * eye(3);
end