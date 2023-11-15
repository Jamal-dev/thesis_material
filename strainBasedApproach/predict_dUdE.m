function dUdE_i=predict_dUdE(coeffs,eps)

    C1 = coeffs(1);
    C2 = coeffs(2);
    C3 = coeffs(3);
    C4 = coeffs(4);
    C5 = coeffs(5);
    C6 = coeffs(6);
    
    E1 = coeffs(7);
    E2 = coeffs(8);
    E3 = coeffs(9);
    E4 = coeffs(10);
    E5 = coeffs(11);
    E6 = coeffs(12);
    
    eps11 = eps(:,1);
    eps22 = eps(:,2);
    eps33 = eps(:,3);
    eps12 = eps(:,4);
    eps13 = eps(:,5);
    eps23 = eps(:,6);
    A11 = eps11.*eps11;
    A22 = eps22.*eps22;
    A33 = eps33.*eps33;
    A12 = eps12.*eps12;
    A13 = eps13.*eps13;
    A23 = eps23.*eps23;
    F1 = E1 * eps11;
    F2 = E2 * eps22;
    F3 = E3 * eps33;
    F4 = E4 * eps12;
    F5 = E5 * eps13;
    F6 = E6 * eps23;
    
    dUdE_i = C1 * eps11 +  C2 * eps22 +  C3 * eps33  +  ...
        2*(C4 * eps12 + C5 * eps13  +  C6 * eps23)  + ...
        F1 .* A11 ...
          +  F2 .* A22  +  F3 .* A33  +  ...
          2*(F4 .* A12 +F5 .* A13  +  F6 .* A23)   ;
    
end
