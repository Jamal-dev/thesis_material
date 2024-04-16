function I = get_4th_I
    I = zeros(3,3,3,3);
    for i=1:3
        for j=1:3
            for k=1:3
                for l=1:3
                    I(i,j,k,l) = I(i,j,k,l) + ...
                                 krnDel(i,k) * krnDel(j,l)  ...
                                    ;
                end
            end
        end
    end
    
end