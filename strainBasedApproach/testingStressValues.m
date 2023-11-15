eps_nominal = [1,-0.2956,-0.2902,2.071e-11,-6.098e-11,2.768e-4];
[F,B,eps_nom]=get_F_from_nominalstrain(eps_nominal);
E_lang=langrangeStrain(eps_nominal);
Vec=lang2col(E_lang)';
 
sigma=stressValues(Cmod,Emod,F,Vec)
sigma1=get_stress(Cmod,Emod)

function sigma=get_stress(Cmod,Emod)
    E11=   1.50000143369644     ;
     E22= -0.251888011947330     ;
     E33= -0.248097801635101     ;
     E12=  1.741198272029676E-010;
     E13= -6.108858914302027E-010;
     E23= -5.842502699918411E-006;
    vec = [E11,E22,E33,E12,E13,E23]';
    eps=col2Mat(vec);
    [Fbar,F] = getFfromGreenLangrangeStrain(eps);
    sigma=stressValues(Cmod,Emod,F,vec);
end

% function E=langrangeStrain(F)
%     C = F' * F;
%     E = 1/2*(C-eye(3));
% end

function [Fbar,F] = getFfromGreenLangrangeStrain(eps)
    C = eps*2 +eye(3);
    detJ  = sqrt(det(C));
    Cbar = detJ^(-2/3) * C;
    Fbar = chol(Cbar);
    % if take transpose then it will become Bbar
    Fbar = Fbar';
    F = detJ^(1/3) * Fbar;
    
end

function sigma=stressValues(C,E,F,Vec)
    dUdE = C * Vec + E * Vec.^3;
    dUdE(4:6) = 2 * dUdE(4:6);
    dUdE = col2Mat(dUdE);
    DeVsigma =  deviatoric(F * dUdE * F');
    p_c = caldUdJ(Vec,lang2col(dUdE),F);
    J = det(F);
    sigma = 1/J * DeVsigma - p_c*eye(3);
    disp('two')
end

function R=get_twoDotProds(dUdE,F)
    R = zeros(size(dUdE));
    for i=1:3
        for j=1:3
            for k=1:3
                for l=1:3
                    R(i,j) = R(i,j) + F(i,k) * dUdE(k,l) * F(j,l);
                end
            end
        end
    end
end

function mat=col2Mat(vec)
    mat = [vec(1),vec(4),vec(5);...
           vec(4),vec(2),vec(6);...
           vec(5),vec(6),vec(3)];
end