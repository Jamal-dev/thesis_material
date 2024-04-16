function res=objective(coefs,eps,sigExp)
    stressList=predStressMonoModel(coefs,eps);
    xrange = 1:170;
    sig11=stressList(xrange,1);
    sigExp11 = sigExp(xrange,1);
    xrange = xrange(end)+1:255;
    sig22=stressList(xrange,2);
    sigExp22 = sigExp(xrange,2);
    xrange = xrange(end)+1:425;
    sig33=stressList(xrange,3);
    sigExp33 = sigExp(xrange,3);
    xrange = xrange(end)+1:595;
    sig12=stressList(xrange,4);
    sigExp12 = sigExp(xrange,4);
    xrange = xrange(end)+1:748;
    sig13=stressList(xrange,5);
    sigExp13 = sigExp(xrange,5);
    xrange = xrange(end)+1:918;
    sig23=stressList(xrange,6);
    sigExp23 = sigExp(xrange,6);
    w = [3 3 3 1 1 5];
    res = sum(w(1) * (sig11-sigExp11).^2) + ...
          sum(w(2) * (sig22-sigExp22).^2) + ...
          sum(w(3) * (sig33-sigExp33).^2) + ...
          sum(w(4) * (sig12-sigExp12).^2) + ...
          sum(w(5) * (sig13-sigExp13).^2) + ...
          sum(w(6) * (sig23-sigExp23).^2) ;
end