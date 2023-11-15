function plotExpBycoefs72(coeffs,sigMatrix,epsMatrix)
    fontsize = 14;
    ylimit=170;
    xrange = 1:1*ylimit;
    eps = epsMatrix(xrange,1);
    sig = predStressByDU(coeffs,epsMatrix(xrange,:));
    sig_exp = sigMatrix(xrange,1);
    subplot(3,2,1)
    plot(eps,sig(:,1),eps,sig_exp,'.')
    xlabel('$\varepsilon_{11}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{11}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,2)
    xrange = xrange(end)+1:255;
    eps = epsMatrix(xrange,2);
    sig = predStressByDU(coeffs,epsMatrix(xrange,:));
    sig_exp = sigMatrix(xrange,2);
    plot(eps,sig(:,2),eps,sig_exp,'.')
    xlabel('$\varepsilon_{22}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{22}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,3)
    xrange = xrange(end)+1:425;
    eps = epsMatrix(xrange,3);
    sig = predStressByDU(coeffs,epsMatrix(xrange,:));
    sig_exp = sigMatrix(xrange,3);
    plot(eps,sig(:,3),eps,sig_exp,'.')
    xlabel('$\varepsilon_{33}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{33}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,4)
    xrange = xrange(end)+1:595;
    eps = epsMatrix(xrange,4);
    sig = predStressByDU(coeffs,epsMatrix(xrange,:));
    sig_exp = sigMatrix(xrange,4);
    plot(eps,sig(:,4),eps,sig_exp,'.')
    xlabel('$\varepsilon_{12}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{12}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,5)
    xrange = xrange(end)+1:748;
    eps = epsMatrix(xrange,5);
    sig = predStressByDU(coeffs,epsMatrix(xrange,:));
    sig_exp = sigMatrix(xrange,5);
    plot(eps,sig(:,5),eps,sig_exp,'.')
    xlabel('$\varepsilon_{13}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{13}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,6)
    xrange = xrange(end)+1:918;
    eps = epsMatrix(xrange,6);
    sig = predStressByDU(coeffs,epsMatrix(xrange,:));
    sig_exp = sigMatrix(xrange,6);
    plot(eps,sig(:,6),eps,sig_exp,'.')
    
    xlabel('$\varepsilon_{23}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{23}$','interpreter','latex','FontSize',fontsize)
    
end