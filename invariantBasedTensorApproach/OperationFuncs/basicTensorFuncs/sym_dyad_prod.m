function C=sym_dyad_prod(A,B)
    [ndimA,ndimB]=get_dims(A,B);
    size_C = 3 * ones(1,ndimA+ndimB);
    C = zeros(size_C);
    if ndimA ==2 && ndimB ==2
        for i=1:3
            for j=1:3
                for k=1:3
                    for l=1:3
                        C(i,j,k,l) = C(i,j,k,l) + ...
                                    0.5*(A(i,k)*B(j,l)+A(i,l)*B(j,k));
                    end
                end
            end
        end
    else
        error(['sym_dyad_product is not implemented for ndimA=' num2str(ndimA) ', and for ndimB=' num2str(ndimB)]);
    end

end