function R=createLowerPart(A)
    
    R=A;
    for i=1:size(A,1)
        for j=1:size(A,2)
            if j<i
                R(i,j) = A(j,i);
            end
        end
    end

end