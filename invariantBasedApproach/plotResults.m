

function plotResults(params,dev_stressList_exp,epsMatrix)
    %% Predicted stress section
    sig_dev_list_pred = zeros(length(epsMatrix),6);
    ai = [1;0;0];
    [I1barList,I2barList, ...
            I4barList,I5barList, ...
            BbarList,dev_fiber_dyadList_I4, ...
            dev_fiber_dyadList_I5,JList]=get_requiredValues(epsMatrix,ai);
    for k=1:length(I1barList)
        I1bar = I1barList(k);
        I2bar = I2barList(k);
        I4alpha = I4barList(k);
        I5alpha = I5barList(k);
        J = JList(k);
        Bbar = arrange(BbarList(k,:,:));
        
        dev_fiber_dyad_I4 = arrange(dev_fiber_dyadList_I4(k,:,:));
        dev_fiber_dyad_I5 = arrange(dev_fiber_dyadList_I5(k,:,:));
        sig_dev = cauchyStress(params, ...
                                I1bar,I2bar,I4alpha,I5alpha, ...
                                Bbar, ...
                                dev_fiber_dyad_I4,dev_fiber_dyad_I5, ...
                                J);
        sig_dev_vec= [sig_dev(1,1),sig_dev(2,2),sig_dev(3,3), ...
                      sig_dev(1,2),sig_dev(1,3),sig_dev(2,3)];
        sig_dev_list_pred(k,:) = sig_dev_vec;
    end

    %%
    fontsize = 14;
    ylimit=170;
    xrange = 1:1*ylimit;
    eps = epsMatrix(xrange,1);
    sig = sig_dev_list_pred(xrange,1);
    sig_exp = dev_stressList_exp(xrange,1);
    subplot(3,2,1)
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{11}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{11}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,2)
    xrange = xrange(end)+1:255;
    eps = epsMatrix(xrange,2);
    sig = sig_dev_list_pred(xrange,2);
    sig_exp = dev_stressList_exp(xrange,2);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{22}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{22}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,3)
    xrange = xrange(end)+1:425;
    eps = epsMatrix(xrange,3);
    sig = sig_dev_list_pred(xrange,3);
    sig_exp = dev_stressList_exp(xrange,3);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{33}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{33}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,4)
    xrange = xrange(end)+1:595;
    eps = epsMatrix(xrange,4);
    sig = sig_dev_list_pred(xrange,4);
    sig_exp = dev_stressList_exp(xrange,4);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{12}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{12}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,5)
    xrange = xrange(end)+1:748;
    eps = epsMatrix(xrange,5);
    sig = sig_dev_list_pred(xrange,5);
    sig_exp = dev_stressList_exp(xrange,5);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{13}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{13}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,6)
    xrange = xrange(end)+1:918;
    eps = epsMatrix(xrange,6);
    sig = sig_dev_list_pred(xrange,6);
    sig_exp = dev_stressList_exp(xrange,6);
    plot(eps,sig,eps,sig_exp,'.')
    
    xlabel('$\varepsilon_{23}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{23}$','interpreter','latex','FontSize',fontsize)
end





