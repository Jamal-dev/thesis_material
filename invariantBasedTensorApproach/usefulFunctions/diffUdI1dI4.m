function derv_U_dI1_dI4 = diffUdI1dI4(in1,I_1,I_2,J,I_4alpha,I_5alpha)
%DIFFUDI1DI4
%    DERV_U_DI1_DI4 = DIFFUDI1DI4(IN1,I_1,I_2,J,I_4ALPHA,I_5ALPHA)

%    This function was generated by the Symbolic Math Toolbox version 8.7.
%    12-Oct-2023 20:10:50

params22 = in1(:,22);
params23 = in1(:,23);
derv_U_dI1_dI4 = params22+params23.*(I_1.*2.0-6.0).*(I_4alpha.*2.0-2.0);
