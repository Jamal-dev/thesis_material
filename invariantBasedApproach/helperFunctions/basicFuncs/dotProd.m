function sum = dotProd(mat,vec)
    sum = 0;
    for i = 1:size(mat,1)
        for j=1:size(mat,2)
            sum = sum + vec(i) * mat(i,j) * vec(j);
        end
    end
end