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
      real(kind=8)  :: a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3,dInv
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
C    PROPS(1) - D1
C    PROPS(2) - C01
C    PROPS(3) - D1
C ----------------------------------------------------------------
C
C    ELASTIC PROPERTIES
C
C Read material properties
      
      
      dInv = one / props(1)
      a1=props(2)
      a2=props(3)
      a3=props(4)
      ! properties for I2
      b1=zero
      b2=zero
      b3=zero
      ! properties for I4
      c1=props(5)
      c2=props(6)
      c3=props(7)
C     Properties for the I5
      d1=props(8)
      d2=props(9)
      d3=props(10)

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
C    Model U^iso= a1 *(I1Bar-3) + a2 *(I1Bar-3)^2 + a3**(I1Bar-3)^3 + b1*(I2Bar-3) + b2*(I2Bar-3)^2 + b3*(I2Bar-3)^3 
C      + c1*(I4Bar-1) + c2*(I4Bar-1)^2 + c3*(I4Bar-1)^3
C      + d1*(I5Bar-1) + d2*(I5Bar-1)^2 + d3*(I5Bar-1)^3
      !PRINT *, 'a1=', a1, ';'
      !PRINT *, 'a2=', a2, ';'
      !PRINT *, 'a3=', a3, ';'
      !PRINT *, 'b1=', b1, ';'
      !PRINT *, 'b2=', b2, ';'
      !PRINT *, 'b3=', b3, ';'
      !PRINT *, 'c1=', c1, ';'
      !PRINT *, 'c2=', c2, ';'
      !PRINT *, 'c3=', c3, ';'
      !PRINT *, 'd1=', d1, ';'
      !PRINT *, 'd2=', d2, ';'
      !PRINT *, 'd3=', d3, ';'
      term = (I1Bar-THREE)
      dUdI1Bar = a1 + TWO*a2*term + THREE*a3*term*term
      ddUdI1BardI1Bar = TWO*a2 + SIX*a3*term
      term = (I2Bar-THREE)
      dUdI2Bar = b1 + TWO*b2*term + THREE*b3*term*term
      ddUdI2BardI2Bar = TWO*b2 + SIX*b3*term
      term = (I4Bar-ONE)
      dUdI4Bar = c1 + TWO*c2*term + THREE*c3*term*term
      ddUdI4BardI4Bar = TWO*c2 + SIX*c3*term
      term = (I5Bar-ONE)
      dUdI5Bar = d1 + TWO*d2*term + THREE*d3*term*term
      ddUdI5BardI5Bar = TWO*d2 + SIX*d3*term
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