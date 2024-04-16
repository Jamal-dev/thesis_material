      include 'ttb/ttb_library.f'

      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,JSTEP,KINC)
     
      ! ABAQUS UMAT:    Nearly-Incompressible Neo-Hookean Material
      !                 Example for usage of Tensor Toolbox
      ! capability:     3D analysis, axisymmetric?
      ! Formulation:    Total Lagrange with push forward for Abaqus
      ! Andreas Dutzler, 2018-07-22, Graz University of Technology

      
     
      use Tensor
      !implicit none
      INCLUDE 'ABA_PARAM.INC'
      

      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3),
     4 JSTEP(4)
      
      PARAMETER(ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, FOUR=4.D0)
      
      type(Tensor2) :: F1
      real(kind=8)  :: J,kappa,C10
      
      type(Tensor2) :: C1,invC1,S1,Eye, Cbar, Sbar, S_iso
      type(Tensor4) :: C4, P4, CijklBar, Cijkl, Cijkl_iso, Cijkl_vol
      type(Tensor4) :: Proj_tilda
      
      ! material parameters
      C10=PROPS(2)
      D1 =PROPS(1)
      kappa = ONE/D1
      
      
      
      PRINT *, 'C10=', C10, ';'
      PRINT *, 'kappa=', kappa, ';'
      
      Eye = identity2(Eye)
      F1 = dfgrd1(1:3,1:3)
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'F1(',K1,',',K2,')=', F1%ab(K1, K2), ';'
        END DO
      END DO
      J = det(F1)
      PRINT *, 'J=', J, ';'
      ! right cauchy-green deformation tensor and it's inverse
      C1 = transpose(F1)*F1
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'C1(',K1,',',K2,')=', C1%ab(K1, K2), ';'
        END DO
      END DO
      invC1 = inv(C1) ! faster method: invC1 = inv(C1,J**2)
      dUdJ = kappa*(J-1)*TWO
      ddUdJdJ = kappa*TWO
      dUdI1bar = C10
      dUdI2bar = ZERO
      ! double derivatives
      ddUdI1bardI1bar = ZERO
      ddUdI1bardI2bar = ZERO
      ddUdI2bardI2bar = ZERO
      invariance_I1 = tr(C1)
      
      Cbar = J**(-2./3.)*C1
      invariance_I1Bar = tr(Cbar)
      gama1Bar = TWO * (dUdI1bar + invariance_I1Bar * dUdI2bar)
      gama2Bar = -TWO * dUdI2bar
      Sbar = gama1Bar * Eye + gama2Bar * Cbar
      p = dUdJ
      p_tilda = p + J * ddUdJdJ
      P4 = identity4(Eye)-1./3.*(invC1.dya.C1)
      S_iso = J**(-2./3.)*(P4**Sbar)
      S1 = J * p * invC1 + S_iso
      
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'invC1(',K1,',',K2,')=', invC1%ab(K1, K2), ';'
        END DO
      END DO
      ! pk2 stress
      !S1 = 2.*C10*J**(-2./3.)*dev(C1)*invC1 + kappa*(J-1)*J*invC1
      
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'S1(',K1,',',K2,')=', S1%ab(K1, K2), ';'
        END DO
      END DO
      
      ! push forward to cauchy stress used in abaqus
      S1 = piola(F1,S1)/J
      
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'sigma(',K1,',',K2,')=', S1%ab(K1, K2), ';'
        END DO
      END DO
      deltaBar1 = FOUR * (ddUdI1bardI1bar+
     *      TWO*invariance_I1Bar*ddUdI1bardI2bar +
     *      dUdI2bar +
     *      invariance_I1Bar*invariance_I1Bar*ddUdI2bardI2bar)
      deltaBar2 = -FOUR * (ddUdI1bardI2bar +
     *      invariance_I1Bar*ddUdI2bardI2bar)
      deltaBar3 = FOUR * ddUdI2bardI2bar 
      deltaBar4 = -FOUR * dUdI2bar 
      CijklBar = deltaBar1 * (Eye.dya.Eye) + deltaBar2 *( (Eye.dya.Cbar)
     *      + (Cbar.dya.Eye)) + deltaBar3 * (Cbar.dya.Cbar)
     *      + deltaBar4 * identity4(Eye)
      CijklBar = J**(-4./3.)*CijklBar
      Proj_tilda = identity4(invC1) - 1./3.*(invC1.dya.invC1)
      trace_withC = tr(SBar*CBar)
      
      Cijkl_iso = P4**CijklBar**transpose(P4) + 2./3.*
     *       trace_withC*Proj_tilda - 2./3.*(
     *       (invC1.dya.S_iso) + (S_iso.dya.invC1))
      Cijkl_vol = (J * p_tilda) * ((invC1.dya.invC1))-(2.*J*p)
     *          *(identity4(invC1))
      
      Cijkl = Cijkl_iso + Cijkl_vol
      
      
      ! material elasticity tensor
    !  C4 = 2.*C10 * J**(-2./3.) * 2./3. *
    ! *  ( tr(C1) * (invC1.cdya.invC1)
    ! *    - (Eye.dya.invC1) - (invC1.dya.Eye)
    ! *    + tr(C1)/3. * (invC1.dya.invC1) )
    ! *  + (kappa*(J-1)*J+kappa*J**2) * (invC1.dya.invC1)
    ! *  - 2.*kappa*(J-1)*J* (invC1.cdya.invC1)
    !  
    !  DO K1=1, 3
    !    DO K2=1, 3
    !          DO K3=1, 3
    !                DO K4=1, 3
    !                    PRINT *, 'C4_ref(',K1,',',K2,',',K3,',',K4,')=', 
    ! *                              C4%abcd(K1, K2,K3,K4), ';'          		
    !                END DO
    !          END DO
    !    END DO
    !  END DO
      

      
      
     
      ! push forward to jaumann tangent of cauchy stress for abaqus
      C4 = piola(F1,Cijkl)/J + (S1.cdya.Eye)+(Eye.cdya.S1)
      
     
      
      DO K1=1, 3
        DO K2=1, 3
              DO K3=1, 3
                    do K4=1, 3
                        PRINT *, 'C4_spat(',K1,',',K2,',',K3,',',K4,')=', 
     *                              C4%abcd(K1, K2,K3,K4), ';'          		
                    end do
              END DO
        END DO
      END DO
     
      ! output as array
      PRINT *, 'ntens=', ntens, ';'
      STRESS(1:ntens)         = asabqarray(voigt(S1),ntens)
      DDSDDE(1:ntens,1:ntens) = asabqarray(voigt(C4),ntens,ntens)
      DO K1=1, 6
          PRINT *, 'STRESS(',K1,')=', STRESS(K1), ';'
      END DO
      
      DO K1=1, 6
        DO K2=1, 6
          PRINT *, 'DDSDDE(',K1,',',K2,')=', DDSDDE(K1, K2), ';'
        END DO
      END DO
      
      return
      end