function R=inv_mat3(A)
    R = zeros(3,3);
    idetT = 1/det(A);
    R(1,1)=+idetT*(A(2,2)*A(3,3)-A(2,3)*A(3,2));
    R(2,1)=-idetT*(A(2,1)*A(3,3)-A(2,3)*A(3,1));
    R(3,1)=+idetT*(A(2,1)*A(3,2)-A(2,2)*A(3,1));
    R(1,2)=-idetT*(A(1,2)*A(3,3)-A(1,3)*A(3,2));
    R(2,2)=+idetT*(A(1,1)*A(3,3)-A(1,3)*A(3,1));
    R(2,3)=-idetT*(A(1,1)*A(2,3)-A(1,3)*A(2,1));
    R(1,3)=+idetT*(A(1,2)*A(2,3)-A(1,3)*A(2,2));
    R(3,2)=-idetT*(A(1,1)*A(3,2)-A(1,2)*A(3,1));
    R(3,3)=+idetT*(A(1,1)*A(2,2)-A(1,2)*A(2,1));
end