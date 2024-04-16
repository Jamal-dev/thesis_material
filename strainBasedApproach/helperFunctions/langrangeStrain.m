function E=langrangeStrain(eps_nominal)
    % Langrange strain on the reference configuration
    [F,B,~]=get_F_from_nominalstrain(eps_nominal);
    C = F' * F;
    E = 1/2*(C-eye(3));
end