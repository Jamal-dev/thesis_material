function C=single_dotProd(A,B)
    [ndimA,ndimB]=get_dims(A,B);

    ndimC = max([ndimA,ndimB])-min([ndimA,ndimB]);
    if ndimC < 0
        error(['Single dot product is not possible for ndimA=' num2str(ndimA) ', and for ndimB=' num2str(ndimB)]);
        
    else
        if ndimA == 1 && ndimB == 4
            C = zeros(3,3,3);
            for i=1:3
                for j=1:3
                    for k=1:3
                        for a=1:3
                            C(i,j,k) = C(i,j,k) + A(a) * B(a,i,j,k);
                        end
                    end
                end
            end
        elseif ndimA == 4 && ndimB == 1
            C = zeros(3,3,3);
            for i=1:3
                for j=1:3
                    for k=1:3
                        for a=1:3
                            C(i,j,k) = C(i,j,k) +  A(i,j,k,a) * B(a);
                        end
                    end
                end
            end
        elseif ndimA == 2 && ndimB == 4
            C = zeros(3,3,3,3);
            for i=1:3
                for j=1:3
                    for k=1:3
                        for l=1:3
                            for a=1:3
                                C(i,j,k,l) = C(i,j,k,l) +  A(i,a) * B(a,j,k,l);
                            end
                        end
                    end
                end
            end
        elseif ndimA == 4 && ndimB == 2
            C = zeros(3,3,3,3);
            for i=1:3
                for j=1:3
                    for k=1:3
                        for l=1:3
                            for a=1:3
                                C(i,j,k,l) = C(i,j,k,l) +  A(i,j,k,a) * B(a,l);
                            end
                        end
                    end
                end
            end
        elseif ndimA == 1 && ndimB == 2
            C = zeros(3,1);
            for i=1:3
                for k=1:3
                    C(i) = C(i) + A(k) * B(k,i);
                end
            end
        elseif ndimA == 2 && ndimB == 1
            C = zeros(3,1);
            for i=1:3
                for k=1:3
                    C(i) = C(i) +  B(i,k) * A(k);
                end
            end
        elseif ndimA == 2 && ndimB == 2
            C = zeros(3,3);
            for i=1:3
                for j=1:3
                    for k=1:3
                        C(i,j)=C(i,j)+A(i,k)*B(k,j);
                    end
                end
            end
        else
            error(['Single dot product is not implemented for ndimA=' num2str(ndimA) ', and for ndimB=' num2str(ndimB)]);
        end
    end
end



