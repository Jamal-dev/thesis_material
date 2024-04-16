function P = get_4th_Projection
    IxI = get_dyad(eye(3),eye(3));
    P = get_4th_I - IxI/3;
end