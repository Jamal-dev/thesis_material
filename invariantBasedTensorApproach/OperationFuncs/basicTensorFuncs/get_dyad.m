function C=get_dyad(A,B)
    flagAoneDim = checkIf1Dim(A);
    flagBoneDim = checkIf1Dim(B);
    if flagAoneDim==0 &&  flagBoneDim ==1
        C = zeros([size(A),3]);
        ndimA = ndims(A);
        ndimB = 1;
    elseif flagAoneDim==1 &&  flagBoneDim ==0
        C = zeros([3,size(B)]);
        ndimA = 1;
        ndimB = ndims(B);
    elseif flagAoneDim==1 &&  flagBoneDim ==1
        C = zeros([3,3]);
        ndimA = 1;
        ndimB = 1;
    else
        if isa(A,'sym')
            C = sym('dyadProd', [size(A),size(B)]);
        else
            C = zeros([size(A),size(B)]);
        end
        ndimA = ndims(A);
        ndimB = ndims(B);
    end

    if ndimA==1 && ndimB ==2
        % three loops
        for i=1:3
            for j=1:3
                for k=1:3
                    C(i,j,k) = C(i,j,k) + A(i) * B(j,k);
                end
            end
        end
    elseif ndimA==2 && ndimB ==1
        % three loops
        for i=1:3
            for j=1:3
                for k=1:3
                    C(i,j,k) = C(i,j,k) + A(i,j) * B(k);
                end
            end
        end
    elseif ndimA==1 && ndimB ==1
        % two loops
        for i=1:3
            for j=1:3
                C(i,j) = C(i,j) + A(i) * B(j);
            end
        end
    elseif ndimA==2 && ndimB ==2
        % four loops
        for i=1:3
            for j=1:3
                for k=1:3
                    for l=1:3
                        C(i,j,k,l) = C(i,j,k,l) + A(i,j) * B(k,l);
                    end
                end
            end
        end
    elseif ndimA==1 && ndimB ==3
        % four loops
        for i=1:3
            for j=1:3
                for k=1:3
                    for l=1:3
                        C(i,j,k,l) = C(i,j,k,l) + A(i) * B(j,k,l);
                    end
                end
            end
        end
    elseif ndimA==3 && ndimB ==1
        % four loops
        for i=1:3
            for j=1:3
                for k=1:3
                    for l=1:3
                        C(i,j,k,l) = C(i,j,k,l) + A(i,j,k) * B(l);
                    end
                end
            end
        end
    else
        error(['dyad prod is not implemented for ndimA=' num2str(ndimA) ', and for ndimB=' num2str(ndimB)]);
    
    end


end

