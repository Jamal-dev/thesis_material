function derv_U_I1 = diffUdI1(in1,I_1,I_2,J,I_4alpha,I_5alpha)
%DIFFUDI1
%    DERV_U_I1 = DIFFUDI1(IN1,I_1,I_2,J,I_4ALPHA,I_5ALPHA)

%    This function was generated by the Symbolic Math Toolbox version 8.7.
%    12-Oct-2023 20:10:51

params2 = in1(:,2);
params3 = in1(:,3);
params4 = in1(:,4);
params16 = in1(:,16);
params17 = in1(:,17);
params22 = in1(:,22);
params23 = in1(:,23);
t2 = I_1.*2.0;
t3 = I_4alpha-1.0;
t4 = I_5alpha-1.0;
t5 = t2-6.0;
derv_U_I1 = params2+params3.*t5+params16.*t4+params22.*t3+params4.*(I_1-3.0).^2.*3.0+params17.*t4.^2.*t5+params23.*t3.^2.*t5;
