# Online Python compiler (interpreter) to run Python online.
# Write Python 3 code in this online editor and run it.
import numpy as np
def matmul(A='A',B='B',dim=3,Atrans=False,Btrans=False):
    R = np.empty((dim,dim))
    R = R.tolist()
    for i in range(dim):
        for j in range(dim):
            R[i][j] = ''
    inital_spacing = '     *    '
    for i in range(1,dim+1):
        for j in range(1,dim+1):
            for k in range(dim):
                if k<dim-1:
                    if k>0:
                        if Atrans==True and Btrans == False:
                            R[i-1][j-1] += f'{inital_spacing}{A}({k+1},{i}) * {B}({k+1},{j}) + \n'
                        elif Atrans == False and Btrans == True:
                            R[i-1][j-1] += f'{inital_spacing}{A}({i},{k+1}) * {B}({j},{k+1}) + \n'
                        elif Atrans == True and Btrans == True:
                            R[i-1][j-1] += f'{inital_spacing}{A}({k+1},{i}) * {B}({j},{k+1}) + \n'
                        elif Atrans == False and Btrans == False:
                            R[i-1][j-1] += f'{inital_spacing}{A}({i},{k+1}) * {B}({k+1},{j}) + \n'
                    else:
                        if Atrans==True and Btrans == False:
                            R[i-1][j-1] += f'{A}({k+1},{i}) * {B}({k+1},{j}) + \n'
                        elif Atrans == False and Btrans == True:
                            R[i-1][j-1] += f'{A}({i},{k+1}) * {B}({j},{k+1}) + \n'
                        elif Atrans == True and Btrans == True:
                            R[i-1][j-1] += f'{A}({k+1},{i}) * {B}({j},{k+1}) + \n'
                        elif Atrans == False and Btrans == False:
                            R[i-1][j-1] += f'{A}({i},{k+1}) * {B}({k+1},{j}) + \n'
                else:
                    if Atrans==True and Btrans == False:
                        R[i-1][j-1] += f'{inital_spacing}{A}({k+1},{i}) * {B}({k+1},{j}) '
                    elif Atrans == False and Btrans == True:
                        R[i-1][j-1] += f'{inital_spacing}{A}({i},{k+1}) * {B}({j},{k+1}) '
                    elif Atrans == True and Btrans == True:
                        R[i-1][j-1] += f'{inital_spacing}{A}({k+1},{i}) * {B}({j},{k+1}) '
                    elif Atrans == False and Btrans == False:
                        R[i-1][j-1] += f'{inital_spacing}{A}({i},{k+1}) * {B}({k+1},{j}) '
    E = ['' for i in range(2*dim)]
    E[0] = R[0][0]
    E[1] = R[1][1]
    E[2] = R[2][2]
    E[3] = R[0][1]
    E[4] = R[0][2]
    E[5] = R[1][2]
    return E
                
E = matmul(A='DISTGR',B='DISTGR',dim=3,Atrans=True,Btrans=False)
for i,e in enumerate(E):
    print(f'      CBAR({i+1}) = {e}')
print("Hello world")