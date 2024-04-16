

function plotResults(params,stressList_exp,epsMatrix)
    %% Predicted stress section
    sig_list_pred = zeros(length(epsMatrix),6);
    ai = [1;0;0];
    [I1barList,I2barList, ...
            I3barList, ...
            I4barList,I5barList, ...
            CbarList,a0xa0List, ...
            Base5List, ...
            CList, ...
            inv_CList, ...
            FbarList, ...
            FList ...
            ] ...
            =get_requiredValues(epsMatrix,ai);
    for k=1:length(I1barList)
        I1bar = I1barList(k);
        I2bar = I2barList(k);
        I3bar = I3barList(k);
        I4bar = I4barList(k);
        I5bar = I5barList(k);
        
        Cbar = arrange(CbarList(k,:,:));
        a0xa0 = arrange(a0xa0List(k,:,:));
        Base5 = arrange(Base5List(k,:,:));
        C = arrange(CList(k,:,:));
        inv_C = arrange(inv_CList(k,:,:));
        Fbar = arrange(FbarList(k,:,:));
        F = arrange(FList(k,:,:));
        % sigma = get_Sbar(F,ai,params);
        sigma=cauchyStress(params,I1bar, ...
                                I2bar,I3bar, ...
                                I4bar,I5bar, ...
                                Cbar,inv_C, ...
                                C, ...
                                F, ...
                                a0xa0,Base5);
        
        
        sig_vec= [sigma(1,1),sigma(2,2),sigma(3,3), ...
                      sigma(1,2),sigma(1,3),sigma(2,3)];
        sig_list_pred(k,:) = sig_vec;
    end

    %%
    fontsize = 14;
    ylimit=170;
    xrange = 1:1*ylimit;
    eps = epsMatrix(xrange,1);
    sig = sig_list_pred(xrange,1);
    sig_exp = stressList_exp(xrange,1);
    subplot(3,2,1)
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{11}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{11}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,2)
    xrange = xrange(end)+1:255;
    eps = epsMatrix(xrange,2);
    sig = sig_list_pred(xrange,2);
    sig_exp = stressList_exp(xrange,2);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{22}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{22}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,3)
    xrange = xrange(end)+1:425;
    eps = epsMatrix(xrange,3);
    sig = sig_list_pred(xrange,3);
    sig_exp = stressList_exp(xrange,3);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{33}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{33}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,4)
    xrange = xrange(end)+1:595;
    eps = epsMatrix(xrange,4);
    sig = sig_list_pred(xrange,4);
    sig_exp = stressList_exp(xrange,4);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{12}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{12}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,5)
    xrange = xrange(end)+1:748;
    eps = epsMatrix(xrange,5);
    sig = sig_list_pred(xrange,5);
    sig_exp = stressList_exp(xrange,5);
    plot(eps,sig,eps,sig_exp,'.')
    xlabel('$\varepsilon_{13}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{13}$','interpreter','latex','FontSize',fontsize)
    
    subplot(3,2,6)
    xrange = xrange(end)+1:918;
    eps = epsMatrix(xrange,6);
    sig = sig_list_pred(xrange,6);
    sig_exp = stressList_exp(xrange,6);
    plot(eps,sig,eps,sig_exp,'.')
    
    xlabel('$\varepsilon_{23}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{23}$','interpreter','latex','FontSize',fontsize)
end





