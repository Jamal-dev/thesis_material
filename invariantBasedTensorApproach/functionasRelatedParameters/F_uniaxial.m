function [F_uniaxial] = F_uniaxial(strain,dir)
    if nargin == 1
        dir =1;
    end
    second = 1/sqrt(1+strain);
    if dir ==1
        F_uniaxial = [...
            [1+strain, 0.0, 0.0];...
            [0.0, second, 0.0];...
            [0.0, 0.0, second]];
    elseif dir ==2
        F_uniaxial = [...
            [second, 0.0, 0.0];...
            [0.0, 1+strain, 0.0];...
            [0.0, 0.0, second]];
    elseif dir ==3
        F_uniaxial = [...
            [second, 0.0, 0.0];...
            [0.0, second, 0.0];...
            [0.0, 0.0, 1+strain]];
    else
        error('Direction can 1,2, or 3')
    end
end