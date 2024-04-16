function F_pureshear = F_pureshear(alpha,dir)
    if dir ==1
        % rotation about z axis
        F_pureshear = [1.0, alpha, 0.0;alpha, 1.0, 0.0;0.0, 0.0, 1/(1-alpha^2)];
    elseif dir ==2
        % rotation about y axis
        F_pureshear = [1.0, 0.0, alpha;0.0, 1/(1-alpha^2), 0.0;alpha, 0.0, 1.0];
    elseif dir ==3
        % rotation about x axis
        F_pureshear = [1/(1-alpha^2), 0.0, 0.0;0.0, 1.0, alpha;0.0, alpha, 1.0];
    end
end