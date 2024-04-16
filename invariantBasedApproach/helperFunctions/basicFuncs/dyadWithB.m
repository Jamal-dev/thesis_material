function R=dyadWithB(left,B,right)
    %% This will do the following dyadic operation
    % $$ R= left \otimes B \cdot right + left \cdot B \otimes right $$
    % where left and right vectors and B is the matrix
    R =zeros(size(B));
    for i=1:size(B,1)
        for j=1:size(B,2)
            for k=1:length(left)
                R(i,j) = R(i,j) + ...
                            left(i) * B(j,k) *right(k) + ...
                            right(j) * B(i,k) *left(k)  ...
                            ;
            end
        end
    end

    
end