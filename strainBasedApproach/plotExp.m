function plotExp(C,E,T,epsMatrix)
    fontsize = 14;
    ylimit=170;
    xrange = 1:1*ylimit;
    eps = epsMatrix(xrange,1);
    coeffs = [C,E];
    sig = predictStress(coeffs(1,:),epsMatrix(xrange,:));
    sig_exp = T.sig11(xrange);
    subplot(3,2,1)
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{11}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{11}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,2)
    xrange = xrange(end)+1:255;
    eps = epsMatrix(xrange,2);
    sig = predictStress(coeffs(2,:),epsMatrix(xrange,:));
    sig_exp = T.sig22(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{22}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{22}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,3)
    xrange = xrange(end)+1:425;
    eps = epsMatrix(xrange,3);
    sig = predictStress(coeffs(3,:),epsMatrix(xrange,:));
    sig_exp = T.sig33(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{33}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{33}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,4)
    xrange = xrange(end)+1:595;
    eps = epsMatrix(xrange,4);
    sig = predictStress(coeffs(4,:),epsMatrix(xrange,:));
    sig_exp = T.sig12(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{12}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{12}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,5)
    xrange = xrange(end)+1:748;
    eps = epsMatrix(xrange,5);
    sig = predictStress(coeffs(5,:),epsMatrix(xrange,:));
    sig_exp = T.sig13(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{13}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{13}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,6)
    xrange = xrange(end)+1:918;
    eps = epsMatrix(xrange,6);
    sig = predictStress(coeffs(6,:),epsMatrix(xrange,:));
    sig_exp = T.sig23(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    
    xlabel('$\varepsilon_{23}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{23}$','interpreter','latex','FontSize',fontsize)
    
end