function flagAoneDim = checkIf1Dim(A)
    if ndims(A) == 2 && (size(A,1) == 1 || size(A,2) ==1)
        % A is one dimensional
        flagAoneDim = 1;
    else
        flagAoneDim = 0;
    end
end