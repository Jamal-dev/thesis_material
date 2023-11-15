%First define the variables
[T,epsMatrix,sigDevMatrix,sigMatrix]=readData();
    
%%
ai = [1;0;0];
sprev = rng(0,'v5uniform');
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
%%
obj = @(params)gradCalculation(params,I1barList, ...
                I2barList, ...
                I3barList, ...
                I4barList, ...
                I5barList,CbarList, ...
                inv_CList, ...
                FList, ...
                CList, ...
                a0xa0List, ...
                Base5List,sigMatrix);
load("theta.mat");
%%
% theta = params;
learning_rate = 1e-4;
num_iterations = 5e6;
print_every = 50;
[theta, J_history,theta_best] = adam_optimizer(theta,obj  ...
                                , learning_rate, ...
                                num_iterations, ...
                                print_every);