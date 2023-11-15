function error = objective(params,I1barList, ...
                I2barList, ...
                I3barList, ...
                I4barList, ...
                I5barList,CbarList, ...
                inv_CList, ...
                FList, ...
                CList, ...
                a0xa0List, ...
                Base5List,stressList_exp)
    km = length(I1barList);
    predStressList = zeros(km,6);
    for k=1:km
        I1bar = I1barList(k);
        I2bar = I2barList(k);
        I3bar = I3barList(k);
        I4bar = I4barList(k);
        I5bar = I5barList(k);
        
        Cbar = arrange(CbarList(k,:,:));
        inv_C = arrange(inv_CList(k,:,:));
        C = arrange(CList(k,:,:));
        F = arrange(FList(k,:,:));
        
        a0xa0 = arrange(a0xa0List(k,:,:));
        Base5 = arrange(Base5List(k,:,:));
        sig_predicted = cauchyStress(params,I1bar,I2bar,I3bar,I4bar,I5bar,Cbar ...
                                ,inv_C,C,F,a0xa0,Base5);
                            
        sig_vec_pred= [sig_predicted(1,1),sig_predicted(2,2),sig_predicted(3,3), ...
                      sig_predicted(1,2),sig_predicted(1,3),sig_predicted(2,3)];
        predStressList(k,:) = sig_vec_pred;
        
        
    end

    error  = errorAsPerData(predStressList,stressList_exp);
end

function res=errorAsPerData(pred_stress,exp_stress)
    
    xrange = 1:170;
    sig11=pred_stress(xrange,1);
    sigExp11 = exp_stress(xrange,1);
    xrange = xrange(end)+1:255;
    sig22=pred_stress(xrange,2);
    sigExp22 = exp_stress(xrange,2);
    xrange = xrange(end)+1:425;
    sig33=pred_stress(xrange,3);
    sigExp33 = exp_stress(xrange,3);
    xrange = xrange(end)+1:595;
    sig12=pred_stress(xrange,4);
    sigExp12 = exp_stress(xrange,4);
    xrange = xrange(end)+1:748;
    sig13=pred_stress(xrange,5);
    sigExp13 = exp_stress(xrange,5);
    xrange = xrange(end)+1:918;
    sig23=pred_stress(xrange,6);
    sigExp23 = exp_stress(xrange,6);
    w = [1 1 1 1 1 1];
    res = sum(w(1) * (sig11-sigExp11).^2) + ...
          sum(w(2) * (sig22-sigExp22).^2) + ...
          sum(w(3) * (sig33-sigExp33).^2) + ...
          sum(w(4) * (sig12-sigExp12).^2) + ...
          sum(w(5) * (sig13-sigExp13).^2) + ...
          sum(w(6) * (sig23-sigExp23).^2) ;
end
