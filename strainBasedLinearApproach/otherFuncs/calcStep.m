function [C,error]=calcStep(epsMatrix,T,F_list)
    ylimit=170;
    xrange = 1:ylimit;
%     xrange = 1:1*ylimit;
    
    mdl1=fitlm(epsMatrix(xrange,:),T.sig11(xrange),'intercept',false); 
    % xrange = 1:length(epsMatrix);
    xrange = xrange(end)+1:255;
    mdl2=fitlm(epsMatrix(xrange,:),T.sig22(xrange),'intercept',false);
    
    xrange = xrange(end)+1:425;
    mdl3=fitlm(epsMatrix(xrange,:),T.sig33(xrange),'intercept',false);
    xrange = xrange(end)+1:595;
    mdl4=fitlm(epsMatrix(xrange,:),T.sig12(xrange),'intercept',false);
    xrange = xrange(end)+1:748;
    mdl5=fitlm(epsMatrix(xrange,:),T.sig13(xrange),'intercept',false);
    xrange = xrange(end)+1:918;
    mdl6=fitlm(epsMatrix(xrange,:),T.sig23(xrange),'intercept',false);
    C = zeros(6,6);
    C(1,:) = mdl1.Coefficients.Estimate';
    C(2,:) = mdl2.Coefficients.Estimate';
    C(3,:) = mdl3.Coefficients.Estimate';
    C(4,:) = mdl4.Coefficients.Estimate';
    C(5,:) = mdl5.Coefficients.Estimate';
    C(6,:) = mdl6.Coefficients.Estimate';
    % C(1:3,4:6) = C(4:6,1:3);
    % C(4,5) = C(5,4);
    % C(4,6) = C(6,4);
    % C(5,6) = C(6,5);
    % C(1,2) = C(2,1);
    % C(1,3) = C(3,1);
    % C(2,3) = C(3,2);
    error = mdl1.MSE + mdl2.MSE + mdl3.MSE ...
        + mdl4.MSE + mdl5.MSE + mdl6.MSE;
end
