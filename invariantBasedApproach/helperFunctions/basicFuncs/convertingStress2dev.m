function dev_sig = convertingStress2dev(sig)
    km = size(sig,1);
    dev_sig = zeros(km,6);
    for k =1:km
        s = sig(k,:);
        sig_mat = get_eps_matrix(s);
        sig_dev = deviatoric(sig_mat);
        dev_sig(k,:) = [sig_dev(1,1),sig_dev(2,2),sig_dev(3,3), ...
                        sig_dev(1,2),sig_dev(1,3),sig_dev(2,3)];
    end
end