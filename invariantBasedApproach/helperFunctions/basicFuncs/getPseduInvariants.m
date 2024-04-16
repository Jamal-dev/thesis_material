function [I1bar,I2bar,I3bar,I4alpha,I5alpha]=getPseduInvariants(Fbar,Bbar,ai)
    Cbar = Fbar'  * Fbar;
    I1bar = trace(Bbar);
    I2bar = 1/2*(I1bar^2 - trace(Bbar*Bbar));
    I3bar = det(Fbar);
    I4alpha = dotProd(Cbar,ai);
    I5alpha = dotProd(Cbar*Cbar,ai);
end