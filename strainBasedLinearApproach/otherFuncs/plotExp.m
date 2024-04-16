function plotExp(C,T,kappa,b_list)
    
    figure;
    km = size(b_list,1);
    predSigma = zeros(km,6);
    b_lin = zeros(km,6);
    for k=1:km
        b = arrange(b_list(k,:,:));
        sigma = predict_stress(b,C,kappa);
        predSigma(k,:)=[sigma(1,1),sigma(2,2),sigma(3,3), ...
                        sigma(1,2),sigma(1,3),sigma(2,3)];
        b_lin(k,:)=[b(1,1),b(2,2),b(3,3), ...
                    b(1,2),b(1,3),b(2,3)];
    end
    
    fontsize = 14;
    
    ylimit=170;
    xrange = 1:1*ylimit;
    eps = b_lin(xrange,1);
    
    sig = predSigma(xrange,1);
    sig_exp = T.sig11(xrange);
    subplot(3,2,1)
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{11}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{11}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,2)
    xrange = xrange(end)+1:255;
    eps = b_lin(xrange,2);
    sig = predSigma(xrange,2);
    sig_exp = T.sig22(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{22}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{22}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,3)
    xrange = xrange(end)+1:425;
    eps = b_lin(xrange,3);
    sig = predSigma(xrange,3);
    sig_exp = T.sig33(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{33}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{33}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,4)
    xrange = xrange(end)+1:595;
    eps = b_lin(xrange,4);
    sig = predSigma(xrange,4);
    sig_exp = T.sig12(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{12}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{12}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,5)
    xrange = xrange(end)+1:748;
    eps = b_lin(xrange,5);
    sig = predSigma(xrange,5);
    sig_exp = T.sig13(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{13}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{13}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,6)
    xrange = xrange(end)+1:918;
    eps = b_lin(xrange,6);
    sig = predSigma(xrange,6);
    sig_exp = T.sig23(xrange);
    plot(eps,sig,eps,sig_exp,'.')
    
    xlabel('$\varepsilon_{23}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{23}$','interpreter','latex','FontSize',fontsize)
    
end
