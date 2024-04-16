function stressList=predStressByDU(coef,eps)
    coef1 = coef(1:12);
    dUdE_1=predict_dUdE(coef1,eps);
    coef2 = coef(13:24);
    dUdE_2=predict_dUdE(coef2,eps);
    coef3 = coef(25:36);
    dUdE_3=predict_dUdE(coef3,eps);
    coef4 = coef(37:48);
    dUdE_12=predict_dUdE(coef4,eps);
    coef5 = coef(49:60);
    dUdE_13=predict_dUdE(coef5,eps);
    coef6 = coef(61:72);
    dUdE_23=predict_dUdE(coef6,eps);
    stressList = zeros(size(eps,1),6);
    for k=1:size(eps,1)
        eps_row = eps(k,:);
        eps_mat = col2Mat(eps_row);
        [Fbar,F] = getFfromGreenLangrangeStrain(eps_mat);
        dUdE = [dUdE_1(k),dUdE_2(k),dUdE_3(k), ...
                dUdE_12(k),dUdE_13(k),dUdE_23(k)];
        dUdE = col2Mat(dUdE);
        DeVsigma =  deviatoric(F * dUdE * F');
        DeVsigma = lang2col(DeVsigma);
        stressList(k,:) = DeVsigma;
    end
    
    
end