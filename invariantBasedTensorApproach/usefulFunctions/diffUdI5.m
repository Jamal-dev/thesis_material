function derv_U_I5 = diffUdI5(in1,I_1,I_2,J,I_4alpha,I_5alpha)
%DIFFUDI5
%    DERV_U_I5 = DIFFUDI5(IN1,I_1,I_2,J,I_4ALPHA,I_5ALPHA)

%    This function was generated by the Symbolic Math Toolbox version 8.7.
%    12-Oct-2023 20:10:52

params12 = in1(:,12);
params13 = in1(:,13);
params14 = in1(:,14);
params15 = in1(:,15);
params16 = in1(:,16);
params17 = in1(:,17);
params18 = in1(:,18);
params19 = in1(:,19);
params24 = in1(:,24);
params25 = in1(:,25);
t2 = I_5alpha.*2.0;
t3 = I_1-3.0;
t4 = I_2-3.0;
t5 = I_4alpha-1.0;
t6 = I_5alpha-1.0;
t7 = t2-2.0;
derv_U_I5 = params12+params16.*t3+params13.*t7+params18.*t4+params24.*t5+params14.*t6.^2.*3.0+params15.*t6.^3.*4.0+params17.*t3.^2.*t7+params19.*t4.^2.*t7+params25.*t5.^2.*t7;
