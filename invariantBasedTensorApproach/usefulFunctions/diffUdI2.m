function derv_U_I2 = diffUdI2(in1,I_1,I_2,J,I_4alpha,I_5alpha)
%DIFFUDI2
%    DERV_U_I2 = DIFFUDI2(IN1,I_1,I_2,J,I_4ALPHA,I_5ALPHA)

%    This function was generated by the Symbolic Math Toolbox version 8.7.
%    12-Oct-2023 20:10:51

params5 = in1(:,5);
params6 = in1(:,6);
params7 = in1(:,7);
params18 = in1(:,18);
params19 = in1(:,19);
params20 = in1(:,20);
params21 = in1(:,21);
t2 = I_2.*2.0;
t3 = I_4alpha-1.0;
t4 = I_5alpha-1.0;
t5 = t2-6.0;
derv_U_I2 = params5+params6.*t5+params18.*t4+params20.*t3+params7.*(I_2-3.0).^2.*3.0+params19.*t4.^2.*t5+params21.*t3.^2.*t5;
