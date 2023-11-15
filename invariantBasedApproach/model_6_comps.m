%% load data
clc;
[T,epsMatrix,sigDevMatrix,sigMatrix]=readData();
    
%%
ai = [1;0;0];
sprev = rng(0,'v5uniform');
[I1barList,I2barList, ...
            I3barList, ...
            I4barList,I5barList, ...
            BbarList,dev_fiber_dyadList_I4, ...
            dev_fiber_dyadList_I5, ...
            JList, ...
            FbarList, ...
            FList] ...
            =get_requiredValues(epsMatrix,ai);
params_0 = rand(1,11);
error = objective(params_0,I1barList,I2barList,I3barList,I4barList,I5barList...
                ,BbarList,dev_fiber_dyadList_I4, ...
                dev_fiber_dyadList_I5,sigDevMatrix, ...
                JList);
%% Particle swarm
lb = [0.0,-inf,-inf,-inf];
ub = [inf,inf,inf,inf];
objFun = @(params)objective(params,I1barList,I2barList,I4barList,I5barList...
                ,BbarList,dev_fiber_dyadList_I4, ...
                dev_fiber_dyadList_I5,sigDevMatrix, ...
                JList);

% Set up the options for the PSO algorithm
% ,'Display', 'iter'
options = optimoptions('particleswarm','SwarmSize',100 ...
    ,'MaxIterations',10000000, ...
    'MaxStallIterations',5000, ...
    'PlotFcn','pswplotbestf', ...
    'HybridFcn','fmincon');

% Run the PSO algorithm
num_variables = 11;
[x_min, f_min,exitflag,output] = particleswarm(objFun, num_variables, lb, ub, options  ) % 1 variable
writematrix(x_min,['current_x_min_' num2str(num_variables) '.csv'])
%% fmincon
% options = optimoptions('fmincon', 'Display', 'iter', ...
%     'StepTolerance', 0, 'ConstraintTolerance', 0, ...
%     'MaxFunctionEvaluations', 100000, ...
%     'MaxIterations',1e6);
% [optimalParams, fval] = fmincon(objFun, initialParams, [], [], [], [], lb, ub, [], options);
%%



function error = objective(params,I1barList, ...
                I2barList, ...
                I3barList, ...
                I4barList, ...
                I5barList,BbarList, ...
                dev_fiber_dyadList_I4, ...
                dev_fiber_dyadList_I5,dev_stressList_exp, ...
                JList)
    km = length(I1barList);
    predDevStressList = zeros(km,6);
    for k=1:km
        I1bar = I1barList(k);
        I2bar = I2barList(k);
        I3bar = I3barList(k);
        I4alpha = I4barList(k);
        I5alpha = I5barList(k);
        J = JList(k);
        Bbar = arrange(BbarList(k,:,:));
        
        dev_fiber_dyad_I4 = arrange(dev_fiber_dyadList_I4(k,:,:));
        dev_fiber_dyad_I5 = arrange(dev_fiber_dyadList_I5(k,:,:));
        sig_dev_predicted = cauchyStress(params,I1bar,I2bar,I3bar,I4alpha,I5alpha,Bbar ...
                                ,dev_fiber_dyad_I4,dev_fiber_dyad_I5,J);
                            
        sig_dev_vec_pred= [sig_dev_predicted(1,1),sig_dev_predicted(2,2),sig_dev_predicted(3,3), ...
                      sig_dev_predicted(1,2),sig_dev_predicted(1,3),sig_dev_predicted(2,3)];
        predDevStressList(k,:) = sig_dev_vec_pred;
        
        
    end

    error  = errorAsPerData(predDevStressList,dev_stressList_exp);
end

function res=errorAsPerData(pred_dev_stress,exp_dev_stress)
    
    xrange = 1:170;
    sig11=pred_dev_stress(xrange,1);
    sigExp11 = exp_dev_stress(xrange,1);
    xrange = xrange(end)+1:255;
    sig22=pred_dev_stress(xrange,2);
    sigExp22 = exp_dev_stress(xrange,2);
    xrange = xrange(end)+1:425;
    sig33=pred_dev_stress(xrange,3);
    sigExp33 = exp_dev_stress(xrange,3);
    xrange = xrange(end)+1:595;
    sig12=pred_dev_stress(xrange,4);
    sigExp12 = exp_dev_stress(xrange,4);
    xrange = xrange(end)+1:748;
    sig13=pred_dev_stress(xrange,5);
    sigExp13 = exp_dev_stress(xrange,5);
    xrange = xrange(end)+1:918;
    sig23=pred_dev_stress(xrange,6);
    sigExp23 = exp_dev_stress(xrange,6);
    w = [3 3 3 1 1 5];
    res = sum(w(1) * (sig11-sigExp11).^2) + ...
          sum(w(2) * (sig22-sigExp22).^2) + ...
          sum(w(3) * (sig33-sigExp33).^2) + ...
          sum(w(4) * (sig12-sigExp12).^2) + ...
          sum(w(5) * (sig13-sigExp13).^2) + ...
          sum(w(6) * (sig23-sigExp23).^2) ;
end



















