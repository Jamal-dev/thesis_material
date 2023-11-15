digits(11);
C10 = 9.82946180372075;
% C10 = 30;
D1 = 10;
params = [D1	C10	0	0	0	0	0	0	0	0];
params(3:end)=ones(1,8);

% params = ones(1,10);
% params = [ 0.8147    0.9058    0.1270    0.9134    0.6324    0.0975    0.2785    0.5469    0.9575    0.9649];
eps_ne = [1,0.5509,0.5509,0,0,0];

a0=[1;0;0];
[F,Fbar,B,Bbar,~]=get_F_from_nominalstrain(eps_ne);
DFGRD1=zeros(3,3);
 DFGRD1(           1 ,           1 )=   2.00000000000000      ;
 DFGRD1(           1 ,           2 )= -1.313635198909592E-016 ;
 DFGRD1(           1 ,           3 )= -5.988581965670684E-016 ;
 DFGRD1(           2 ,           1 )= -1.501655550909744E-016 ;
 DFGRD1(           2 ,           2 )=  0.629834290342414      ;
 DFGRD1(           2 ,           3 )= -0.321418670469538      ;
 DFGRD1(           3 ,           1 )= -5.723644363279481E-017 ;
 DFGRD1(           3 ,           2 )=  0.321418670469538      ;
 DFGRD1(           3 ,           3 )=  0.629834290342413      ;
 F1(           1 ,           1 )=   1.99999999999999      ;
 F1(           1 ,           2 )= -1.963210740133394E-015 ;
 F1(           1 ,           3 )=  1.542659100147195E-015 ;
 F1(           2 ,           1 )=  3.501353863448537E-015 ;
 F1(           2 ,           2 )=   1.75486446416961      ;
 F1(           2 ,           3 )= -0.274189570239687      ;
 F1(           3 ,           1 )= -1.292658077130050E-014 ;
 F1(           3 ,           2 )=  0.274189570239715      ;
 F1(           3 ,           3 )=   1.75486446416963      ;
F = F1;
J = det(F);
Fbar = J^(-1/3) * DFGRD1;
Bbar = Fbar * Fbar';
stress_neoHok = 1/params(1) * eye(3) + 2*params(2)/J * (Bbar - trace(Bbar)*eye(3)/3)
[sigma,Sbar,S] = get_Sbar(F,a0,params);
[Cij_spatial,Cij_ref,CijklBar,Cijkl_iso,Cijkl_vol,Cijkl]=get_stiffness(F,a0,params);
[spatial_Cij]=get_stiffness_spatial(F,a0,params);

Cij_abaqus = abaqus_Cij_neoHOK(Bbar, D1,C10,J);

function DDSDDE=abaqus_Cij_neoHOK(Bbar, D1,C10,J)
    BBAR = [Bbar(1,1),Bbar(2,2),Bbar(3,3), ...
            Bbar(1,2),Bbar(1,3),Bbar(2,3)];
    DDSDDE = zeros(6,6);
    TRBBAR=(BBAR(1)+BBAR(2)+BBAR(3))/3;
    EG=2*C10/J;
    EK=2/D1*(2*J-1);
    EG23=EG*2/3;
    DDSDDE(1, 1)= EG23*(BBAR(1)+TRBBAR)+EK;
    DDSDDE(2, 2)= EG23*(BBAR(2)+TRBBAR)+EK;
    DDSDDE(3, 3)= EG23*(BBAR(3)+TRBBAR)+EK;
    DDSDDE(1, 2)=-EG23*(BBAR(1)+BBAR(2)-TRBBAR)+EK;
    DDSDDE(1, 3)=-EG23*(BBAR(1)+BBAR(3)-TRBBAR)+EK;
    DDSDDE(2, 3)=-EG23*(BBAR(2)+BBAR(3)-TRBBAR)+EK;
    DDSDDE(1, 4)= EG23*BBAR(4)/2;
    DDSDDE(2, 4)= EG23*BBAR(4)/2;
    DDSDDE(3, 4)=-EG23*BBAR(4);
    DDSDDE(4, 4)= EG*(BBAR(1)+BBAR(2))/2;
      
    DDSDDE(1, 5)= EG23*BBAR(5)/2;
    DDSDDE(2, 5)=-EG23*BBAR(5);
    DDSDDE(3, 5)= EG23*BBAR(5)/2;
    DDSDDE(1, 6)=-EG23*BBAR(6);
    DDSDDE(2, 6)= EG23*BBAR(6)/2;
    DDSDDE(3, 6)= EG23*BBAR(6)/2;
    DDSDDE(5, 5)= EG*(BBAR(1)+BBAR(3))/2;
    DDSDDE(6, 6)= EG*(BBAR(2)+BBAR(3))/2;
    DDSDDE(4,5)= EG*BBAR(6)/2;
    DDSDDE(4,6)= EG*BBAR(5)/2;
    DDSDDE(5,6)= EG*BBAR(4)/2;
    % Other symmetric part
    for i=1:6
        for j=1:6
            if j<i
                DDSDDE(i,j) = DDSDDE(j,i);
            end
        end
    end
end