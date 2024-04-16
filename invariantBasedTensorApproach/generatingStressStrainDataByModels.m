%% Prepare X and y
T = readData();

X = T(:,1:6);
yPred = T(:,7:12);
X = X{:,:};
yPred = yPred{:,:};
X=eps_nom_2_vSquared(X);

%%
%                            
%                            c(13)*eps(:,1).^3 + c(14) * eps(:,2).^3 + ...
%                            c(15)*eps(:,3).^3 + c(16) * eps(:,4).^3 + ...
%                            c(17)*eps(:,5).^3 + c(18) * eps(:,6).^3  ...
fun = @(c,eps) c(1)*eps(:,1) + c(2) * eps(:,2) + ...
                           c(3)*eps(:,3) + c(4) * eps(:,4) + ...
                           c(5)*eps(:,5) + c(6) * eps(:,6) + ...
                           c(7)*eps(:,1).^2 + c(8) * eps(:,2).^2 + ...
                           c(9)*eps(:,3).^2 + c(10) * eps(:,4).^2 +...
                           c(11)*eps(:,5).^2 + c(12) * eps(:,6).^2  ...
                                                                ;
num_consts = 12;
sig_nlmFuncs.sig11 = fitnlm(X,yPred(:,1),fun, rand(1,num_consts));
sig_nlmFuncs.sig22 = fitnlm(X,yPred(:,2),fun, rand(1,num_consts));
sig_nlmFuncs.sig33 = fitnlm(X,yPred(:,3),fun, rand(1,num_consts));
sig_nlmFuncs.sig12 = fitnlm(X,yPred(:,4),fun, rand(1,num_consts));
sig_nlmFuncs.sig13 = fitnlm(X,yPred(:,5),fun, rand(1,num_consts));
sig_nlmFuncs.sig23 = fitnlm(X,yPred(:,6),fun, rand(1,num_consts));
%%
[Bbar_list,sig_list_nonLinModel]=genData(sig_nlmFuncs, ...
                                                            @F_uniaxial,@F_pureshear);

plot_results(Bbar_list, sig_list_nonLinModel)
%%
function v2=eps_nom_2_vSquared(X)
    v2 = zeros(size(X));
    for k=1:size(X,1)
        eps = X(k,:);
        eps=[eps(1),eps(4),eps(5); ...
             eps(4),eps(2),eps(6); ...
             eps(5),eps(6),eps(3); ];
        vsq = eps + eye(3);
        v2(k,:)=[vsq(1,1),vsq(2,2),vsq(3,3),...
                 vsq(1,2),vsq(1,3),vsq(2,3)];
    end
end

function [Bbar_list,sig_list_nonLinModel]=genData(sig_nlmFuncs, ...
                                                            F_uniaxial,F_pureshear)
    % for 11
    max_strain = 0.8;
    num =200;
    uni_strain = linspace(0,2*sqrt(3)-1,num);
    [Bbar_list1,sig_list1] ...
                            =generateStressStrainData(sig_nlmFuncs,...
                            F_uniaxial,1,uni_strain);
    [Bbar_list2,sig_list2] ...
                            =generateStressStrainData(sig_nlmFuncs,...
                            F_uniaxial,2,uni_strain);
    [Bbar_list3,sig_list3] ...
                            =generateStressStrainData(sig_nlmFuncs,...
                            F_uniaxial,3,uni_strain);
    uni_strain = linspace(0,1.0,num);
    [Bbar_list4,sig_list4] ...
                            =generateStressStrainData(sig_nlmFuncs,...
                            F_pureshear,1,uni_strain);
    [Bbar_list5,sig_list5] ...
                            =generateStressStrainData(sig_nlmFuncs,...
                            F_pureshear,2,uni_strain);
    [Bbar_list6,sig_list6] ...
                            =generateStressStrainData(sig_nlmFuncs,...
                            F_pureshear,3,uni_strain);
    sig_list_nonLinModel = [sig_list1;sig_list2;sig_list3; ...
                sig_list4;sig_list5;sig_list6];
    Bbar_list = [Bbar_list1;Bbar_list2;Bbar_list3; ...
                    Bbar_list4;Bbar_list5;Bbar_list6];

            
end

function [Bbar_list,sig_list]=generateStressStrainData(sig_tree,...
                            def_grad_type,dir,uni_strain)
    
    sig_list = zeros(length(uni_strain),6);
    Bbar_list = zeros(length(uni_strain),6);
    for k = 1:length(uni_strain)
        uni_st = uni_strain(k);
        Fbar = def_grad_type(uni_st,dir);
        Bbar = Fbar*Fbar';
        eps_linear_comp = [Bbar(1,1),Bbar(2,2),Bbar(3,3), ...
                           Bbar(1,2),Bbar(1,3),Bbar(2,3)];
        Bbar_list(k,:) = eps_linear_comp;
        sig11=predict(sig_tree.sig11,eps_linear_comp);
        sig22=predict(sig_tree.sig22,eps_linear_comp);
        sig33=predict(sig_tree.sig33,eps_linear_comp);
        sig12=predict(sig_tree.sig12,eps_linear_comp);
        sig13=predict(sig_tree.sig13,eps_linear_comp);
        sig23=predict(sig_tree.sig23,eps_linear_comp);
        sig = [sig11,sig22,sig33,sig12,sig13,sig23];
        sig_list(k,:) =sig; 

    end
end




function plot_results(Bbar_list, sig)
    i_max = 200;
    fontsize = 14;
    subplot(2,3,1);
    plot(Bbar_list(1:i_max,1),sig(1:i_max,1),'LineWidth',2);
    xlabel('$\varepsilon_{11}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{11}$','interpreter','latex','FontSize',fontsize)

    subplot(2,3,2);
    plot(Bbar_list(i_max+1:2*i_max,2),sig(i_max+1:2*i_max,2),'LineWidth',2);
    xlabel('$\varepsilon_{22}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{22}$','interpreter','latex','FontSize',fontsize)

    subplot(2,3,3);
    plot(Bbar_list(2*i_max+1:3*i_max,3),sig(2*i_max+1:3*i_max,3),'LineWidth',2);
    xlabel('$\varepsilon_{33}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{33}$','interpreter','latex','FontSize',fontsize)

    subplot(2,3,4);
    plot(Bbar_list(3*i_max+1:4*i_max,4),sig(3*i_max+1:4*i_max,4),'LineWidth',2);
    xlabel('$\varepsilon_{12}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{12}$','interpreter','latex','FontSize',fontsize)

    subplot(2,3,5);
    plot(Bbar_list(4*i_max+1:5*i_max,5),sig(4*i_max+1:5*i_max,5),'LineWidth',2);
    xlabel('$\varepsilon_{13}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{13}$','interpreter','latex','FontSize',fontsize)

    subplot(2,3,6);
    plot(Bbar_list(5*i_max+1:6*i_max,6),sig(5*i_max+1:6*i_max,6),'LineWidth',2);
    xlabel('$\varepsilon_{23}$','interpreter','latex','FontSize',fontsize)
    ylabel('$\sigma_{23}$','interpreter','latex','FontSize',fontsize)

end








