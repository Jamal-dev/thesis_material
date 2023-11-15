function [C,E]=coef72ToCEmat(coef)
    R=reshape(coef,12,[])';
    C = R(1:6,1:6);
    E = R(1:6,7:12);
end