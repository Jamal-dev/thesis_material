      include 'ttb/ttb_library.f'      
      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,STRAN,DSTRAN,
     2 TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,MATERL,NDI,NSHR,NTENS,
     3 NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,CELENT,
     4 DFGRD0,DFGRD1,NOEL,NPT,KSLAY,KSPT,KSTEP,KINC)
C
      use Tensor
      !implicit none
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*8 MATERL
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),DFGRD0(3,3),DFGRD1(3,3),
     3 TIME(2),PREDEF(1),DPRED(1),PROPS(NPROPS),COORDS(3),DROT(3,3)
C

C    LOCAL ARRAYS
C ----------------------------------------------------------------
C    BBAR   - DEVIATORIC RIGHT CAUCHY-GREEN TENSOR
C    F_BAR - DEVIATORIC DEFORMATION GRADIENT (DISTORTION TENSOR)
C ----------------------------------------------------------------
C
      
      real(kind=8)  :: J, dUdI4Bar, I4Bar, I5Bar, I1Bar,I3bar, I2Bar
      real(kind=8)  :: dUdI1Bar,dUdI2Bar, dUdI5Bar, term
      real(kind=8)  :: ddUdI1BardI1Bar,ddUdI2BardI2Bar, ddUdI5BardI5Bar
      real(kind=8)  :: ddUdI1BardI2Bar,ddUdI1BardI4Bar, ddUdI1BardI5Bar
      real(kind=8)  :: ddUdI2BardI4Bar,ddUdI2BardI5Bar, ddUdI4BardI5Bar
      real(kind=8)  :: deltaBar1,deltaBar2, deltaBar3, ddUdI4BardI4Bar
      real(kind=8)  :: deltaBar4,deltaBar5, deltaBar6
      real(kind=8)  :: deltaBar7,deltaBar8, deltaBar9
      real(kind=8)  :: deltaBar10,deltaBar11, deltaBar12
      real(kind=8)  :: gama1Bar,gama2Bar, gama4Bar, gama5Bar
      real(kind=8)  :: p, p_tilda, dUdJ, ddUdJdJ,trace_withC
      real(kind=8)  :: ZERO, ONE, TWO, THREE, FOUR, SIX
      real(kind=8)  :: ONE_THIRD, TWO_THIRD, FOUR_THIRD, HALF
      real(kind=8)  :: a1,a2,props(4),props(5),props(6),props(7),props(8),props(9),props(10),props(11),props(12),props(13),dInv
      type(Tensor1) :: A0, Cbar_A0
      type(Tensor2) :: Fbar, S_vol
      type(Tensor2) :: C1Tensor,invC1,S1,Eye, Cbar, Sbar, S_iso
      type(Tensor2) :: a0xa0, a0xCa0, F1
      type(Tensor4) :: C4, P4, CijklBar, Cijkl, Cijkl_iso, Cijkl_vol
      type(Tensor4) :: Proj_tilda
      type(Tensor4) :: Base5, Base6, Base7, Base8, Base9, Base10
      type(Tensor4) :: Base11, Base12, I4_identity
      Integer       :: K1,K2,K3,K4,K5
C
      PARAMETER(ZERO=0.D0, ONE=1.D0, 
     * TWO=2.D0, 
     * THREE=3.D0, 
     * FOUR=4.D0,
     * SIX=6.D0,
     * ONE_THIRD=1.D0/3.D0,
     * TWO_THIRD=2.D0/3.D0,
     * FOUR_THIRD=4.D0/3.D0,
     * HALF=1.D0/2.D0)
C
C ----------------------------------------------------------------
C    UMAT FOR COMPRESSIBLE NEO-HOOKEAN HYPERELASTICITY
C    CANNOT BE USED FOR PLANE STRESS
C ----------------------------------------------------------------
C    U = 1/params(1) * (J-1)^2        + ...
C        params(2) * (I1-3) + ...
C        params(3) * (I1-3)^2 +  ...
C        params(4) * (I1-3)^3 +  ...
C        params(5) * (I2-3) + ...
C        params(6) * (I2-3)^2 +  ...
C        params(7) * (I2-3)^3 +  ...
C        params(8) * (I4-1) + ...
C        params(9) * (I4-1)^2 + ...
C        params(10) * (I4-1)^3 + ...
C        params(11) * (I4-1)^4 + ...
C        params(12) * (I5-1) + ...
C        params(13) * (I5-1)^2 + ...
C        params(14) * (I5-1)^3 + ...
C        params(15) * (I5-1)^4 + ...
C        params(16) * (I5-1)* (I1-3) + ...
C        params(17) * (I5-1)^2* (I1-3)^2 + ...
C        params(18) * (I4-1)* (I2-3) + ...
C        params(19) * (I4-1)^2* (I2-3)^2  ...

C ----------------------------------------------------------------
C
C    ELASTIC PROPERTIES
C
C Read material properties
      
      
      dInv = one / props(1)

C
C    JACOBIAN AND DISTORTION TENSOR
C
      Eye = identity2(Eye)
      F1 = DFGRD1(1:3,1:3)
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'F1(',K1,',',K2,')=', F1%ab(K1, K2), ';'
        END DO
      END DO
      Fbar = J**(-ONE_THIRD)*F1
      J = det(F1)
      C1Tensor = transpose(F1)*F1
      Cbar = J**(-TWO_THIRD)*C1Tensor
      ! Assign fiber direction as 1
      A0%a(1) = ONE
      A0%a(2) = ZERO
      A0%a(3) = ZERO
      ! dyad of a0
      a0xa0 = A0.dya.A0
      Cbar_A0 = Cbar*a0
      a0xCa0 = (A0.dya.Cbar_A0) + (Cbar_A0.dya.A0)
      invC1 = inv(C1Tensor) ! faster method: invC1 = inv(C1Tensor,J**2)
      
      
      

C
C    CALCULATE invariances
C
      
      I1Bar=tr(Cbar)
      I2Bar= HALF * (I1Bar * I1Bar - tr(Cbar*Cbar))
      I3Bar = J
      I4Bar = Cbar**(a0xa0)
      I5Bar = (Cbar*Cbar)**(a0xa0)
      PRINT *, 'I1Bar=', I1Bar, ';'
      PRINT *, 'I2Bar=', I2Bar, ';'
      PRINT *, 'I3Bar=', I3Bar, ';'
      PRINT *, 'I4Bar=', I4Bar, ';'
      PRINT *, 'I5Bar=', I5Bar, ';'
C
C    CALCULATE derivatives of invariances
C    Model     U = 1/params(1) * (J-1)^2        + ...
C        params(2) * (I1-3) + ...
C        params(3) * (I1-3)^2 +  ...
C        params(4) * (I1-3)^3 +  ...
C        params(5) * (I2-3) + ...
C        params(6) * (I2-3)^2 +  ...
C        params(7) * (I2-3)^3 +  ...
C        params(8) * (I4-1) + ...
C        params(9) * (I4-1)^2 + ...
C        params(10) * (I4-1)^3 + ...
C        params(11) * (I4-1)^4 + ...
C        params(12) * (I5-1) + ...
C        params(13) * (I5-1)^2 + ...
C        params(14) * (I5-1)^3 + ...
C        params(15) * (I5-1)^4 + ...
C        params(16) * (I5-1)* (I1-3) + ...
C        params(17) * (I5-1)^2* (I1-3)^2 + ...
C        params(18) * (I4-1)* (I2-3) + ...
C        params(19) * (I4-1)^2* (I2-3)^2  ...

      !PRINT *, 'props(2)=', props(2), ';'
      !PRINT *, 'props(3)=', props(3), ';'
      !PRINT *, 'props(4)=', props(4), ';'
      !PRINT *, 'props(5)=', props(5), ';'
      !PRINT *, 'props(6)=', props(6), ';'
      !PRINT *, 'props(7)=', props(7), ';'
      !PRINT *, 'props(8)=', props(8), ';'
      !PRINT *, 'props(9)=', props(9), ';'
      !PRINT *, 'props(10)=', props(10), ';'
      !PRINT *, 'props(11)=', props(11), ';'
      !PRINT *, 'props(12)=', props(12), ';'
      !PRINT *, 'props(13)=', props(13), ';'
      term = (I1Bar-THREE)
      dUdI1Bar = props(2) + TWO*props(3)*term + THREE*props(4)*term*term
      ddUdI1BardI1Bar = TWO*props(3) + SIX*props(4)*term
      term = (I2Bar-THREE)
      dUdI2Bar = props(5) + TWO*props(6)*term + THREE*props(7)*term*term
      ddUdI2BardI2Bar = TWO*props(6) + SIX*props(7)*term
      term = (I4Bar-ONE)
      dUdI4Bar = props(8) + TWO*props(9)*term + THREE*props(10)*term*term
      ddUdI4BardI4Bar = TWO*props(9) + SIX*props(10)*term
      term = (I5Bar-ONE)
      dUdI5Bar = props(11) + TWO*props(12)*term + THREE*props(13)*term*term
      ddUdI5BardI5Bar = TWO*props(12) + SIX*props(13)*term
      ! mixed remaining derivatives
      ddUdI1BardI2Bar = ZERO
      ddUdI1BardI4Bar = ZERO
      ddUdI1BardI5Bar = ZERO
      ddUdI2BardI4Bar = ZERO
      ddUdI2BardI5Bar = ZERO
      ddUdI4BardI5Bar = ZERO
      ! derivatives w.r.t I3bar
      dUdJ = dInv*(J-ONE)*TWO
      ddUdJdJ = dInv*TWO
      
C
C    CALCULATE THE STRESS
C
      gama1Bar = TWO * (dUdI1bar + I1Bar * dUdI2bar)
      gama2Bar = -TWO * dUdI2bar
      gama4Bar = TWO * dUdI4bar
      gama5Bar = TWO * dUdI5bar
      
      !PRINT *, 'gama1Bar=', gama1Bar, ';'
      !PRINT *, 'gama2Bar=', gama2Bar, ';'
      !PRINT *, 'gama4Bar=', gama4Bar, ';'
      !PRINT *, 'gama5Bar=', gama5Bar, ';'
      
      Sbar = gama1Bar * Eye + gama2Bar * Cbar + 
     *       gama4Bar * a0xa0 + gama5Bar * a0xCa0
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'C(',K1,',',K2,')=', C1Tensor%ab(K1, K2), ';'
        END DO
      END DO
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'inv_C1(',K1,',',K2,')=', invC1%ab(K1, K2), ';'
        END DO
      END DO
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'SBAR(',K1,',',K2,')=', Sbar%ab(K1, K2), ';'
        END DO
      END DO
      p = dUdJ
      p_tilda = p + J * ddUdJdJ
      P4 = identity4(Eye)-ONE_THIRD*(invC1.dya.C1Tensor)
      S_iso = J**(-TWO_THIRD)*((P4**Sbar))
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'S_iso(',K1,',',K2,')=', S_iso%ab(K1, K2), ';'
        END DO
      END DO
      ! second Piola Kirchhoff stress tensor
      S_vol = J * p * invC1
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'S_vol(',K1,',',K2,')=', S_vol%ab(K1, K2), ';'
        END DO
      END DO
      S1 = S_vol + S_iso
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'S2(',K1,',',K2,')=', S1%ab(K1, K2), ';'
        END DO
      END DO
      ! push forward to cauchy stress used in abaqus
      S1 = piola(F1,S1)/J
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'sigma(',K1,',',K2,')=', S1%ab(K1, K2), ';'
        END DO
      END DO
      
C
C    CALCULATE THE STIFFNESS
C
      deltaBar1 = FOUR * (ddUdI1bardI1bar+
     *      TWO*I1Bar*ddUdI1bardI2bar +
     *      dUdI2bar +
     *      I1Bar*I1Bar*ddUdI2bardI2bar)
      deltaBar2 = -FOUR * (ddUdI1bardI2bar +
     *      I1Bar*ddUdI2bardI2bar)
      deltaBar3 = FOUR * ddUdI2bardI2bar 
      deltaBar4 = -FOUR * dUdI2bar
      deltaBar5 = FOUR * (ddUdI1bardI4bar+
     *      I1Bar*ddUdI2bardI4bar )
      deltaBar6 = -FOUR * (ddUdI2bardI4bar )
      deltaBar7 = FOUR * ddUdI4bardI4bar
      deltaBar8 = FOUR * (ddUdI1bardI5bar+
     *      I1Bar*ddUdI2bardI5bar )
      deltaBar9 = -FOUR * (ddUdI2bardI5bar )
      deltaBar10 = FOUR * ddUdI5bardI5bar
      deltaBar11 = FOUR * ddUdI4bardI5bar
      deltaBar12 = FOUR * dUdI5bar
      
      !PRINT *, 'deltaBar1=', deltaBar1, ';'
      !PRINT *, 'deltaBar2=', deltaBar2, ';'
      !PRINT *, 'deltaBar3=', deltaBar3, ';'
      !PRINT *, 'deltaBar4=', deltaBar4, ';'
      !PRINT *, 'deltaBar5=', deltaBar5, ';'
      !PRINT *, 'deltaBar6=', deltaBar6, ';'
      !PRINT *, 'deltaBar7=', deltaBar7, ';'
      !PRINT *, 'deltaBar8=', deltaBar8, ';'
      !PRINT *, 'deltaBar9=', deltaBar9, ';'
      !PRINT *, 'deltaBar10=', deltaBar10, ';'
      !PRINT *, 'deltaBar11=', deltaBar11, ';'
      !PRINT *, 'deltaBar12=', deltaBar12, ';'
      
      Base5 = (Eye.dya.a0xa0) + (a0xa0.dya.Eye)
      Base6 = (Cbar.dya.a0xa0) + (a0xa0.dya.Cbar)
      Base7 = (a0xa0.dya.a0xa0)
      Base8 = (Eye.dya.a0xCa0) + (a0xCa0.dya.Eye)
      Base9 = (Cbar.dya.a0xCa0) + (a0xCa0.dya.Cbar)
      Base10 = (a0xCa0.dya.a0xCa0)
      Base11 = (a0xa0.dya.a0xCa0) + (a0xCa0.dya.a0xa0)
      ! ddI5dCdC
      I4_identity = identity4(Eye)
      Base12 = 0.
      DO K1 = 1,3
        DO K2 = 1,3
          DO K3 = 1,3
            DO K4 = 1,3
                  DO K5 = 1,3
              Base12%abcd(K1,K2,K3,K4) = 
     *             Base12%abcd(K1,K2,K3,K4) + 
     *              A0%a(K1) * I4_identity%abcd(K2,K3,K4,K5) 
     *               *  A0%a(K5) +
     *              A0%a(K4) * I4_identity%abcd(K1,K2,K3,K5) 
     *               *  A0%a(K5) 
                  ENDDO
            ENDDO
           ENDDO
        ENDDO
      ENDDO
      
      
      
      CijklBar = deltaBar1 * (Eye.dya.Eye) + deltaBar2 *( (Eye.dya.Cbar)
     *      + (Cbar.dya.Eye)) + deltaBar3 * (Cbar.dya.Cbar)
     *      + deltaBar4 * identity4(Eye)
     *       + deltaBar5 * (Base5) + deltaBar6 * (Base6)
     *       + deltaBar7 * (Base7) + deltaBar8 * (Base8)
     *       + deltaBar9 * (Base9) + deltaBar10 * (Base10)
     *       + deltaBar11 * (Base11) + deltaBar12 * (Base12)
      
      !DO K1=1, 3
      !  DO K2=1, 3
      !        DO K3=1, 3
      !              DO K4=1, 3
      !                  PRINT *, 'CijklBar(',K1,',',K2,',',K3,',',K4,')=', 
     *!                              CijklBar%abcd(K1, K2,K3,K4), ';'          		
      !              END DO
      !        END DO
      !  END DO
      !END DO
      
      ! Cijkl part
      CijklBar = J**(-FOUR_THIRD)*CijklBar
      Proj_tilda = identity4(invC1) - ONE_THIRD*(invC1.dya.invC1)
      trace_withC = tr(SBar*CBar)
      PRINT *, 'trace_withC=', trace_withC, ';'
      
      Cijkl_iso = P4**CijklBar**transpose(P4) + TWO_THIRD*
     *       trace_withC*Proj_tilda - TWO_THIRD*(
     *       (invC1.dya.S_iso) + (S_iso.dya.invC1))
      !DO K1=1, 3
      !  DO K2=1, 3
      !        DO K3=1, 3
      !              DO K4=1, 3
      !                  PRINT *, 'Cijkl_iso(',K1,',',K2,',',K3,',',K4,')=', 
     *!                              Cijkl_iso%abcd(K1, K2,K3,K4), ';'          		
      !              END DO
      !        END DO
      !  END DO
      !END DO
      Cijkl_vol = (J * p_tilda) * ((invC1.dya.invC1))-(TWO*J*p)
     *          *(identity4(invC1))
      
      Cijkl = Cijkl_iso + Cijkl_vol
      
      !DO K1=1, 3
      !  DO K2=1, 3
      !        DO K3=1, 3
      !              DO K4=1, 3
      !                  PRINT *, 'Cijkl(',K1,',',K2,',',K3,',',K4,')=', 
     *!                              Cijkl%abcd(K1, K2,K3,K4), ';'          		
      !              END DO
      !        END DO
      !  END DO
      !END DO
      
      
      

      
      
     
      ! push forward to jaumann tangent of cauchy stress for abaqus
      C4 = piola(F1,Cijkl)/J + (S1.cdya.Eye)+(Eye.cdya.S1)
      
      !DO K1=1, 3
      !  DO K2=1, 3
      !        DO K3=1, 3
      !              DO K4=1, 3
      !                  PRINT *, 'C4_spatial(',K1,',',K2,',',K3,',',K4,')=', 
     *!                              C4%abcd(K1, K2,K3,K4), ';'          		
      !              END DO
      !        END DO
      !  END DO
      !END DO
      
     
      
      
     
      ! output as array
      
      STRESS(1:ntens)         = asabqarray(voigt(S1),ntens)
      DDSDDE(1:ntens,1:ntens) = asabqarray(voigt(C4),ntens,ntens)
      
      DO K1=1,6
            DO K2=1,6
                  PRINT *, 'DDSDDE(',K1,',',K2,')=', 
     *                              DDSDDE(K1, K2), ';'
            END DO
      END DO
      
      
C
      RETURN
      END