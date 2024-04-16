function C=double_dotProd(A,B)
    [ndimA,ndimB]=get_dims(A,B);

    ndimC = max([ndimA,ndimB])-min([ndimA,ndimB]);
    if ndimC < 0
        error(['Double dot product is not possible for ndimA=' num2str(ndimA) ', and for ndimB=' num2str(ndimB)]);
        
    else
        % Here we should deal with different cases
        if ndimA == 4 && ndimB == 2
            if isa(A,'sym')
                C = sym('DoubleDotProd', [3,3]);
            else
                C = zeros([3,3]);
            end
            for i =1:3
                for j=1:3
                    for k=1:3
                        for l=1:3
                            C(i,j) = C(i,j) + A(i,j,k,l) * B(k,l);
                        end
                    end
                end
            end
        elseif ndimA == 2 && ndimB == 4
            if isa(A,'sym')
                C = sym('DoubleDotProd', [3,3]);
            else
                C = zeros([3,3]);
            end
            for i =1:3
                for j=1:3
                    for k=1:3
                        for l=1:3
                            C(i,j) = C(i,j) + B(k,l) * A(k,l,i,j) ;
                        end
                    end
                end
            end
        elseif ndimA==2 && ndimB ==2
            
            C = 0;
            for i=1:3
                for j=1:3
                    C = C + A(i,j) * B(i,j);
                end
            end
        elseif ndimA==4 && ndimB == 4
            if isa(A,'sym')
                C = sym('DoubleDotProd', [3,3,3,3]);
            else
                C = zeros([3,3,3,3]);
            end

            for i=1:3
                for j=1:3
                    for k=1:3
                        for l=1:3
                            for m=1:3
                                for n=1:3
                                    C(i,j,k,l) = C(i,j,k,l) + ...
                                                A(i,j,m,n)*B(m,n,k,l);
                                end
                            end
                        end
                    end
                end
            end
        else
            error(['Double dot product is not implemented for ndimA=' num2str(ndimA) ', and for ndimB=' num2str(ndimB)])
        end

    end


end

