function [ndimA,ndimB]=get_dims(A,B)
    flagAoneDim = checkIf1Dim(A);
    flagBoneDim = checkIf1Dim(B);
    if flagAoneDim==0 &&  flagBoneDim ==1
        
        ndimA = ndims(A);
        ndimB = 1;
    elseif flagAoneDim==1 &&  flagBoneDim ==0
        
        ndimA = 1;
        ndimB = ndims(B);
    elseif flagAoneDim==1 &&  flagBoneDim ==1
        
        ndimA = 1;
        ndimB = 1;
    else
        
        ndimA = ndims(A);
        ndimB = ndims(B);
    end
end