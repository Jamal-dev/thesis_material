function [deviatoric] = deviatoric(tensor)
    I = eye(3);
    I_1 = trace(tensor);
    deviatoric = tensor-1/3*I*I_1;
end