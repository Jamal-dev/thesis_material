function stressList=predStressMonoModel(coef,eps)
    flat=coef16Toceof72(coef);
    stressList=predStressByDU(flat,eps);


end


