function mat=abq_defGradMat(vec)
    mat = zeros(3,3);
    mat(1,1) = vec(1);
    mat(2,2) = vec(2);
    mat(3,3) = vec(3);
    mat(1,2) = vec(4);
    mat(1,3) = vec(5);
    mat(2,3) = vec(6);
    mat(2,1) = vec(7);
    mat(3,1) = vec(8);
    mat(3,2) = vec(9);
end