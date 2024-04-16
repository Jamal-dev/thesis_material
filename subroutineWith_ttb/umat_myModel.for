      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,STRAN,DSTRAN,
     2 TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,MATERL,NDI,NSHR,NTENS,
     3 NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,CELENT,
     4 DFGRD0,DFGRD1,NOEL,NPT,KSLAY,KSPT,KSTEP,KINC)
C
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
C    DISTGR - DEVIATORIC DEFORMATION GRADIENT (DISTORTION TENSOR)
C ----------------------------------------------------------------
C
      DIMENSION BBAR(6),DISTGR(3,3),CBAR(6)
      DIMENSION BBAR_BBAR(3)
C
      PARAMETER(ZERO=0.D0, ONE=1.D0, 
     * TWO=2.D0, 
     * THREE=3.D0, 
     * FOUR=4.D0, 
     * HALF=1.D0/2.D0)
C
C ----------------------------------------------------------------
C    UMAT FOR COMPRESSIBLE NEO-HOOKEAN HYPERELASTICITY
C    CANNOT BE USED FOR PLANE STRESS
C ----------------------------------------------------------------
C    PROPS(1) - C10
C    PROPS(2) - C01
C    PROPS(3) - D1
C ----------------------------------------------------------------
C
C    ELASTIC PROPERTIES
C
C Read material properties
      d=props(1)
      dInv = one / d
      a1=props(2)
      a2=props(3)
      a3=props(4)
      b1=zero
      b2=zero
      b3=zero
      c1=props(5)
      c2=props(6)
      c3=props(7)
C   Making c4 to c6 zero for now
      c4=zero
      c5=zero
      c6=zero
C     Properties for the I5
      d1=props(8)
      d2=props(9)
      d3=props(10)
C     Making the d4 to d6 also zero
      d4=zero
      d5=zero
      d6=zero
C
C    JACOBIAN AND DISTORTION TENSOR
C
      J_DET_1=DFGRD1(1, 1)*DFGRD1(2, 2)*DFGRD1(3, 3)
     1   -DFGRD1(1, 2)*DFGRD1(2, 1)*DFGRD1(3, 3)
      IF(NSHR.EQ.3) THEN
        J_DET_1=J_DET_1+DFGRD1(1, 2)*DFGRD1(2, 3)*DFGRD1(3, 1)
     1         +DFGRD1(1, 3)*DFGRD1(3, 2)*DFGRD1(2, 1)
     2         -DFGRD1(1, 3)*DFGRD1(3,1)*DFGRD1(2, 2)
     3         -DFGRD1(2, 3)*DFGRD1(3, 2)*DFGRD1(1, 1)
      END IF
      J_DET_0=DFGRD0(1, 1)*DFGRD0(2, 2)*DFGRD0(3, 3)
     1   -DFGRD0(1, 2)*DFGRD0(2, 1)*DFGRD0(3, 3)
      IF(NSHR.EQ.3) THEN
        J_DET_0=J_DET_0+DFGRD0(1, 2)*DFGRD0(2, 3)*DFGRD0(3, 1)
     1         +DFGRD0(1, 3)*DFGRD0(3, 2)*DFGRD0(2, 1)
     2         -DFGRD0(1, 3)*DFGRD0(3,1)*DFGRD0(2, 2)
     3         -DFGRD0(2, 3)*DFGRD0(3, 2)*DFGRD0(1, 1)
      END IF
      SCALE=J_DET_1**(-ONE/THREE)
      SCALE_0=J_DET_0**(-ONE/THREE)
      DO K1=1, 3
        DO K2=1, 3
          DISTGR(K2, K1)=SCALE*DFGRD1(K2, K1)
        END DO
      END DO
      DO K1=1, 3
        DO K2=1, 3
          DISTGR_0(K2, K1)=SCALE_0*DFGRD0(K2, K1)
        END DO
      END DO
C
C    CALCULATE LEFT CAUCHY-GREEN TENSOR
C
      BBAR(1)=DISTGR(1, 1)**2+DISTGR(1, 2)**2+DISTGR(1, 3)**2
      BBAR(2)=DISTGR(2, 1)**2+DISTGR(2, 2)**2+DISTGR(2, 3)**2
      BBAR(3)=DISTGR(3, 3)**2+DISTGR(3, 1)**2+DISTGR(3, 2)**2
      BBAR(4)=DISTGR(1, 1)*DISTGR(2, 1)+DISTGR(1, 2)*DISTGR(2, 2)
     1       +DISTGR(1, 3)*DISTGR(2, 3)
      IF(NSHR.EQ.3) THEN
        BBAR(5)=DISTGR(1, 1)*DISTGR(3, 1)+DISTGR(1, 2)*DISTGR(3, 2)
     1         +DISTGR(1, 3)*DISTGR(3, 3)
        BBAR(6)=DISTGR(2, 1)*DISTGR(3, 1)+DISTGR(2, 2)*DISTGR(3, 2)
     1         +DISTGR(2, 3)*DISTGR(3, 3)
      END IF
      CBAR(1) = DISTGR(1,1) * DISTGR(1,1) + 
     *    DISTGR(2,1) * DISTGR(2,1) + 
     *    DISTGR(3,1) * DISTGR(3,1) 
      CBAR(2) = DISTGR(1,2) * DISTGR(1,2) + 
     *    DISTGR(2,2) * DISTGR(2,2) + 
     *    DISTGR(3,2) * DISTGR(3,2) 
      CBAR(3) = DISTGR(1,3) * DISTGR(1,3) + 
     *    DISTGR(2,3) * DISTGR(2,3) + 
     *    DISTGR(3,3) * DISTGR(3,3) 
      CBAR(4) = DISTGR(1,1) * DISTGR(1,2) + 
     *    DISTGR(2,1) * DISTGR(2,2) + 
     *    DISTGR(3,1) * DISTGR(3,2) 
      IF(NSHR.EQ.3) THEN    
          CBAR(5) = DISTGR(1,1) * DISTGR(1,3) + 
     *    DISTGR(2,1) * DISTGR(2,3) + 
     *    DISTGR(3,1) * DISTGR(3,3) 
          CBAR(6) = DISTGR(1,2) * DISTGR(1,3) + 
     *    DISTGR(2,2) * DISTGR(2,3) + 
     *    DISTGR(3,2) * DISTGR(3,3)
      END IF
      BBAR_BBAR(1) = BBAR(1,1) * BBAR(1,1) + 
     *    BBAR(1,2) * BBAR(2,1) + 
     *    BBAR(1,3) * BBAR(3,1) 
      BBAR_BBAR(2) = BBAR(2,1) * BBAR(1,2) + 
     *    BBAR(2,2) * BBAR(2,2) + 
     *    BBAR(2,3) * BBAR(3,2) 
      BBAR_BBAR(3) = BBAR(3,1) * BBAR(1,3) + 
     *    BBAR(3,2) * BBAR(2,3) + 
     *    BBAR(3,3) * BBAR(3,3)
      TRBBAR_BBAR=(BBAR_BBAR(1)+BBAR_BBAR(2)+BBAR_BBAR(3))/THREE
      I1Bar=(BBAR(1)+BBAR(2)+BBAR(3))/THREE
      I2Bar= HALF * (I1Bar * I1Bar - TRBBAR_BBAR)
      I3Bar = J_DET_1
      
C
C    CALCULATE THE STRESS
C
      TRBBAR=(BBAR(1)+BBAR(2)+BBAR(3))/THREE
      EG=TWO*C10/J_DET_1
      EK=TWO/D1*(TWO*J_DET_1-ONE)
      PR=TWO/D1*(J_DET_1-ONE)
      DO K1=1,NDI
        STRESS(K1)=EG*(BBAR(K1)-TRBBAR)+PR
      END DO
      DO K1=NDI+1,NDI+NSHR
        STRESS(K1)=EG*BBAR(K1)
      END DO
C
C    CALCULATE THE STIFFNESS
C
      EG23=EG*TWO/THREE
      DDSDDE(1, 1)= EG23*(BBAR(1)+TRBBAR)+EK
      DDSDDE(2, 2)= EG23*(BBAR(2)+TRBBAR)+EK
      DDSDDE(3, 3)= EG23*(BBAR(3)+TRBBAR)+EK
      DDSDDE(1, 2)=-EG23*(BBAR(1)+BBAR(2)-TRBBAR)+EK
      DDSDDE(1, 3)=-EG23*(BBAR(1)+BBAR(3)-TRBBAR)+EK
      DDSDDE(2, 3)=-EG23*(BBAR(2)+BBAR(3)-TRBBAR)+EK
      DDSDDE(1, 4)= EG23*BBAR(4)/TWO
      DDSDDE(2, 4)= EG23*BBAR(4)/TWO
      DDSDDE(3, 4)=-EG23*BBAR(4)
      DDSDDE(4, 4)= EG*(BBAR(1)+BBAR(2))/TWO
      IF(NSHR.EQ.3) THEN
        DDSDDE(1, 5)= EG23*BBAR(5)/TWO
        DDSDDE(2, 5)=-EG23*BBAR(5)
        DDSDDE(3, 5)= EG23*BBAR(5)/TWO
        DDSDDE(1, 6)=-EG23*BBAR(6)
        DDSDDE(2, 6)= EG23*BBAR(6)/TWO
        DDSDDE(3, 6)= EG23*BBAR(6)/TWO
        DDSDDE(5, 5)= EG*(BBAR(1)+BBAR(3))/TWO
        DDSDDE(6, 6)= EG*(BBAR(2)+BBAR(3))/TWO
        DDSDDE(4,5)= EG*BBAR(6)/TWO
        DDSDDE(4,6)= EG*BBAR(5)/TWO
        DDSDDE(5,6)= EG*BBAR(4)/TWO
      END IF
      DO K1=1, NTENS
        DO K2=1, K1-1
          DDSDDE(K1, K2)=DDSDDE(K2, K1)
        END DO
      END DO
C
      RETURN
      END