function [T,eps_nominal_Matrix, ...
            sigDevMatrix, ...
            sigMatrix, ...
            eps11,eps22,eps33, ...
            eps12,eps13,eps23, ...
            sig11,sig22,sig33, ...
            sig12,sig13,sig23] ...
                    =readData()
    filepath=fullfile('D:','Leibniz_University','thesis','materialModelTestingMatlab','avgStrainStressInfoNEPureShearLoading.csv');
    % filepath=fullfile('/mnt','sda1','Leibniz_University','thesis','materialModelTestingMatlab','avgStrainStressInfoNEPureShearLoading.csv');

    columNames = {'eps11','eps22','eps33','eps12','eps13','eps23', ...
        'sig11','sig22','sig33','sig12','sig13','sig23'};
    opt = detectImportOptions(filepath);
    opt.VariableNames = columNames;
    T = readtable(filepath,opt);
    eps_nominal_Matrix = [T.eps11,T.eps22,T.eps33,T.eps12,T.eps13,T.eps23];
    sigDevMatrix = zeros(length(T.sig11),6);
    for k = 1:length(T.sig11)
        temp = [T.sig11(k), T.sig12(k), T.sig13(k); ...
                T.sig12(k), T.sig22(k), T.sig23(k); ...
                T.sig13(k), T.sig23(k), T.sig33(k)];
        devS = deviatoric(temp);
        sigDevMatrix(k,:) = [devS(1,1),devS(2,2),devS(3,3), ...
                          devS(1,2),devS(1,3),devS(2,3)  ];
    end
    sigMatrix = [T.sig11,T.sig22,T.sig33,T.sig12,T.sig13,T.sig23];
    xrange = 1:170;
    eps11= T.eps11(xrange); sig11=T.sig11(xrange);
    xrange = xrange(end)+1:255;
    eps22= T.eps22(xrange); sig22=T.sig22(xrange);
    xrange = xrange(end)+1:425;
    eps33= T.eps33(xrange); sig33=T.sig33(xrange);
    xrange = xrange(end)+1:595;
    eps12= T.eps12(xrange); sig12=T.sig12(xrange);
    xrange = xrange(end)+1:748;
    eps13= T.eps13(xrange); sig13=T.sig13(xrange);
    xrange = xrange(end)+1:918;
    eps23= T.eps23(xrange); sig23=T.sig23(xrange);
end