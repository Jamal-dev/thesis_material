function derv_U_dI4_dI4 = diffUdI4dI4(in1,I_1,I_2,J,I_4alpha,I_5alpha)
%DIFFUDI4DI4
%    DERV_U_DI4_DI4 = DIFFUDI4DI4(IN1,I_1,I_2,J,I_4ALPHA,I_5ALPHA)

%    This function was generated by the Symbolic Math Toolbox version 8.7.
%    12-Oct-2023 20:10:50

params9 = in1(:,9);
params10 = in1(:,10);
params11 = in1(:,11);
params21 = in1(:,21);
params23 = in1(:,23);
params25 = in1(:,25);
t2 = I_4alpha-1.0;
derv_U_dI4_dI4 = params9.*2.0+params10.*t2.*6.0+params11.*t2.^2.*1.2e+1+params21.*(I_2-3.0).^2.*2.0+params23.*(I_1-3.0).^2.*2.0+params25.*(I_5alpha-1.0).^2.*2.0;
