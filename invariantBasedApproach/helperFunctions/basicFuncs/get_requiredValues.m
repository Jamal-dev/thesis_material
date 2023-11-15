function [I1barList,I2barList, ...
            I3barList, ...
            I4barList,I5barList, ...
            BbarList,dev_fiber_dyadList_I4, ...
            dev_fiber_dyadList_I5, ...
            JList, ...
            FbarList, ...
            FList ...
            ] ...
            =get_requiredValues(epsMatrix,ai)
    
        
    km = size(epsMatrix,1);
    I1barList = zeros(km,1);
    I2barList = zeros(km,1);
    I3barList = zeros(km,1);
    I4barList = zeros(km,1);
    I5barList = zeros(km,1);
    JList = zeros(km,1);
    BbarList = zeros(km,3,3);
    FbarList = zeros(km,3,3);
    FList = zeros(km,3,3);
    dev_fiber_dyadList_I4 = zeros(km,3,3);
    dev_fiber_dyadList_I5 = zeros(km,3,3);
    for k=1:km
        eps11 = epsMatrix(k,1);
        eps22 = epsMatrix(k,2);
        eps33 = epsMatrix(k,3);
        eps12 = epsMatrix(k,4);
        eps13 = epsMatrix(k,5);
        eps23 = epsMatrix(k,6);
        eps_nominal=get_eps_matrix(eps11,eps22,eps33,eps12,eps13,eps23);
   
        [F,Fbar,B,Bbar,~]=get_F_from_nominalstrain(eps_nominal);
        J = det(F);
        [I1bar,I2bar,I3bar,I4alpha,I5alpha]=getPseduInvariants(Fbar,Bbar,ai);
        a_alpha = Fbar * ai;
        a_dash_alpha = Bbar * ai;
        fiber_dyad_I4 = get_dyad(a_alpha,a_alpha);
        fiber_dyad_I5 = dyadWithB(a_alpha,B,a_alpha);
        % fiber_dyad_I5 = (a_alpha) * (a_dash_alpha') + ...
        %                 (a_dash_alpha) * (a_alpha') + ...
        %                 (a_alpha) * (a_dash_alpha') + ...
        %                 (a_dash_alpha) * (a_alpha');
        dev_fiber_dyad_I4 = deviatoric(fiber_dyad_I4);
        dev_fiber_dyad_I5 = deviatoric(fiber_dyad_I5);
        I1barList(k) = I1bar;
        I2barList(k) = I2bar;
        I3barList(k) = I3bar;
        I4barList(k) = I4alpha;
        I5barList(k) = I5alpha;
        JList(k)     = J;
        BbarList(k,:,:) = Bbar;
        FbarList(k,:,:) = Fbar;
        FList(k,:,:) = F;
        dev_fiber_dyadList_I4(k,:,:) = dev_fiber_dyad_I4;
        dev_fiber_dyadList_I5(k,:,:) = dev_fiber_dyad_I5;
    end
    
end