function [I1barList,I2barList, ...
            I3barList, ...
            I4barList,I5barList, ...
            CbarList,a0xa0List, ...
            Base5List, ...
            CList, ...
            inv_CList, ...
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
    CList = zeros(km,3,3);
    CbarList = zeros(km,3,3);
    inv_CList = zeros(km,3,3);
    FbarList = zeros(km,3,3);
    FList = zeros(km,3,3);
    a0xa0List = zeros(km,3,3);
    Base5List = zeros(km,3,3);
    for k=1:km
        eps11 = epsMatrix(k,1);
        eps22 = epsMatrix(k,2);
        eps33 = epsMatrix(k,3);
        eps12 = epsMatrix(k,4);
        eps13 = epsMatrix(k,5);
        eps23 = epsMatrix(k,6);
   
        [F,Fbar,~,~,~]=get_F_from_nominalstrain([eps11,eps22,eps33,eps12,eps13,eps23]);
        J = det(F);
        a0xa0 = get_dyad(ai,ai);
        a0=ai;
        I3bar = J;
        C = F' * F;
        inv_C = inv(C);
        Cbar = Fbar'*Fbar;
        Cbar_squared = Cbar * Cbar;
        I1bar = trace(Cbar);
        I2bar = 0.5*(I1bar*I1bar - trace(Cbar_squared));
        I4alpha = double_dotProd(Cbar,a0xa0);
        I5alpha = double_dotProd(Cbar_squared,a0xa0);
        
        fiber_dyad_I4 = a0xa0;
        fiber_dyad_I5 = dyadWithB(a0,Cbar,a0);
        
        I1barList(k) = I1bar;
        I2barList(k) = I2bar;
        I3barList(k) = I3bar;
        I4barList(k) = I4alpha;
        I5barList(k) = I5alpha;
        JList(k)     = J;
        CbarList(k,:,:) = Cbar;
        CList(k,:,:) = C;
        inv_CList(k,:,:) = inv_C;
        FbarList(k,:,:) = Fbar;
        FList(k,:,:) = F;
        a0xa0List(k,:,:) = fiber_dyad_I4;
        Base5List(k,:,:) = fiber_dyad_I5;
    end
    
end