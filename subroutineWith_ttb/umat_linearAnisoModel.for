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
      DIMENSION BBAR(6),DISTGR(3,3), B_LIN(6), B(6),CDASH(6,6)
C
      PARAMETER(ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, FOUR=4.D0)
C
C ----------------------------------------------------------------
C    UMAT FOR COMPRESSIBLE NEO-HOOKEAN HYPERELASTICITY
C    CANNOT BE USED FOR PLANE STRESS
C ----------------------------------------------------------------
C    PROPS(1) - C10
C    
C    PROPS(2) - D1
C ----------------------------------------------------------------
C
C    ELASTIC PROPERTIES
C
      C10=PROPS(2)
      D1 =PROPS(1)
C
C    JACOBIAN AND DISTORTION TENSOR
C
      DET=DFGRD1(1, 1)*DFGRD1(2, 2)*DFGRD1(3, 3)
     1   -DFGRD1(1, 2)*DFGRD1(2, 1)*DFGRD1(3, 3)
      IF(NSHR.EQ.3) THEN
        DET=DET+DFGRD1(1, 2)*DFGRD1(2, 3)*DFGRD1(3, 1)
     1         +DFGRD1(1, 3)*DFGRD1(3, 2)*DFGRD1(2, 1)
     2         -DFGRD1(1, 3)*DFGRD1(3,1)*DFGRD1(2, 2)
     3         -DFGRD1(2, 3)*DFGRD1(3, 2)*DFGRD1(1, 1)
      END IF
      PRINT *, '======================================================'
      PRINT *, 'J=', DET, ';'
      
      SCALE=DET**(-ONE/THREE)
      DO K1=1, 3
        DO K2=1, 3
          DISTGR(K2, K1)=SCALE*DFGRD1(K2, K1)
        END DO
      END DO
      PRINT *, '-----------------------------------------------------'
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'DFGRD1(',K1,',',K2,')=', DFGRD1(K1, K2), ';'
        END DO
      END DO
      PRINT *, '-----------------------------------------------------'
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'DFGRD0(',K1,',',K2,')=', DFGRD0(K1, K2), ';'
        END DO
      END DO
      PRINT *, '-----------------------------------------------------'
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'DISTGR(',K1,',',K2,')=', DISTGR(K1, K2), ';'
        END DO
      END DO
      PRINT *, '-----------------------------------------------------'
      DO K1=1, 3
        DO K2=1, 3
          PRINT *, 'DROT(',K1,',',K2,')=', DROT(K1, K2), ';'
        END DO
      END DO
      PRINT *, '-----------------------------------------------------'
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
      
      B(1)=DFGRD1(1, 1)**2+DFGRD1(1, 2)**2+DFGRD1(1, 3)**2
      B(2)=DFGRD1(2, 1)**2+DFGRD1(2, 2)**2+DFGRD1(2, 3)**2
      B(3)=DFGRD1(3, 3)**2+DFGRD1(3, 1)**2+DFGRD1(3, 2)**2
      B(4)=DFGRD1(1, 1)*DFGRD1(2, 1)+DFGRD1(1, 2)*DFGRD1(2, 2)
     1       +DFGRD1(1, 3)*DFGRD1(2, 3)
      IF(NSHR.EQ.3) THEN
        B(5)=DFGRD1(1, 1)*DFGRD1(3, 1)+DFGRD1(1, 2)*DFGRD1(3, 2)
     1         +DFGRD1(1, 3)*DFGRD1(3, 3)
        B(6)=DFGRD1(2, 1)*DFGRD1(3, 1)+DFGRD1(2, 2)*DFGRD1(3, 2)
     1         +DFGRD1(2, 3)*DFGRD1(3, 3)
      END IF
      
      B_LIN(1)=DFGRD1(1, 1)**2+DFGRD1(1, 2)**2+DFGRD1(1, 3)**2
      B_LIN(2)=DFGRD1(2, 1)**2+DFGRD1(2, 2)**2+DFGRD1(2, 3)**2
      B_LIN(3)=DFGRD1(3, 3)**2+DFGRD1(3, 1)**2+DFGRD1(3, 2)**2
      B_LIN(4)=TWO*(DFGRD1(1, 1)*DFGRD1(2, 1)+DFGRD1(1, 2)*DFGRD1(2, 2)
     1       +DFGRD1(1, 3)*DFGRD1(2, 3))
      IF(NSHR.EQ.3) THEN
        B_LIN(5)=TWO*(DFGRD1(1, 1)*DFGRD1(3, 1)+DFGRD1(1, 2)*DFGRD1(3, 2)
     1         +DFGRD1(1, 3)*DFGRD1(3, 3))
        B_LIN(6)=TWO*(DFGRD1(2, 1)*DFGRD1(3, 1)+DFGRD1(2, 2)*DFGRD1(3, 2)
     1         +DFGRD1(2, 3)*DFGRD1(3, 3))
      END IF
      
      !DO K1=1, 6
      !  B_LIN(K1)=B_LIN(K1)*SCALE
      !END DO
C
C    CALCULATE THE Stifness
C
      DDSDDE(1, 1)=141.245550
      DDSDDE(1, 2)=-39.733521
      DDSDDE(1, 3)=-30.480133
      DDSDDE(1, 4)=0.000000
      DDSDDE(1, 5)=0.000000
      DDSDDE(1, 6)=0.000000
      DDSDDE(2, 1)=-39.733521
      DDSDDE(2, 2)=21.460797
      DDSDDE(2, 3)=34.661271
      DDSDDE(2, 4)=0.000000
      DDSDDE(2, 5)=0.000000
      DDSDDE(2, 6)=0.000000
      DDSDDE(3, 1)=-30.480133
      DDSDDE(3, 2)=34.661271
      DDSDDE(3, 3)=11.189723
      DDSDDE(3, 4)=0.000000
      DDSDDE(3, 5)=0.000000
      DDSDDE(3, 6)=0.000000
      DDSDDE(4, 1)=0.000000
      DDSDDE(4, 2)=0.000000
      DDSDDE(4, 3)=0.000000
      DDSDDE(4, 4)=34.798703
      DDSDDE(4, 5)=0.450730
      DDSDDE(4, 6)=20.606863
      DDSDDE(5, 1)=0.000000
      DDSDDE(5, 2)=0.000000
      DDSDDE(5, 3)=0.000000
      DDSDDE(5, 4)=0.450730
      DDSDDE(5, 5)=-55.606091
      DDSDDE(5, 6)=0.614338
      DDSDDE(6, 1)=0.000000
      DDSDDE(6, 2)=0.000000
      DDSDDE(6, 3)=0.000000
      DDSDDE(6, 4)=20.606863
      DDSDDE(6, 5)=0.614338
      DDSDDE(6, 6)=-21.978790
C
C    CALCULATE THE STRESS
C
      STRESS(1) = DDSDDE(1, 1)*B_LIN(1)+DDSDDE(1, 2)*B_LIN(2)
     * +DDSDDE(1, 3)*B_LIN(3)+DDSDDE(1, 4)*B_LIN(4)
     * +DDSDDE(1, 5)*B_LIN(5)+DDSDDE(1, 6)*B_LIN(6)
      STRESS(2) = DDSDDE(2, 1)*B_LIN(1)+DDSDDE(2, 2)*B_LIN(2)
     * +DDSDDE(2, 3)*B_LIN(3)+DDSDDE(2, 4)*B_LIN(4)
     * +DDSDDE(2, 5)*B_LIN(5)+DDSDDE(2, 6)*B_LIN(6)
      STRESS(3) = DDSDDE(3, 1)*B_LIN(1)+DDSDDE(3, 2)*B_LIN(2)
     * +DDSDDE(3,3)*B_LIN(3)+DDSDDE(3, 4)*B_LIN(4)
     * +DDSDDE(3, 5)*B_LIN(5)+DDSDDE(3, 6)*B_LIN(6)
      STRESS(4) = DDSDDE(4, 1)*B_LIN(1)+DDSDDE(4, 2)*B_LIN(2)
     * +DDSDDE(4, 3)*B_LIN(3)+DDSDDE(4, 4)*B_LIN(4)
     * +DDSDDE(4, 5)*B_LIN(5)+DDSDDE(4, 6)*B_LIN(6)
      STRESS(5) = DDSDDE(5, 1)*B_LIN(1)+DDSDDE(5, 2)*B_LIN(2)
     * +DDSDDE(5, 3)*B_LIN(3)+DDSDDE(5, 4)*B_LIN(4)
     * +DDSDDE(5, 5)*B_LIN(5)+DDSDDE(5, 6)*B_LIN(6)
      STRESS(6) = DDSDDE(6, 1)*B_LIN(1)+DDSDDE(6, 2)*B_LIN(2)
     * +DDSDDE(6, 3)*B_LIN(3)+DDSDDE(6, 4)*B_LIN(4)
     * +DDSDDE(6, 5)*B_LIN(5)+DDSDDE(6, 6)*B_LIN(6)
      print *,'STRESS(1)=', STRESS(1)
      print *,'STRESS(2)=', STRESS(2)
      print *,'STRESS(3)=', STRESS(3)
      print *,'STRESS(4)=', STRESS(4)
      print *,'STRESS(5)=', STRESS(5)
      print *,'STRESS(6)=', STRESS(6)
      
C    Calculate Cdash
      Cdash(1,1)=TWO*STRESS(1)
      Cdash(1,2)=Zero
      Cdash(1,3)=Zero
      Cdash(1,4)=STRESS(4)
      Cdash(1,5)=STRESS(5)
      Cdash(1,6)=Zero
      Cdash(2,1)=Zero
      Cdash(2,2)=TWO*STRESS(2)
      Cdash(2,3)=Zero
      Cdash(2,4)=STRESS(4)
      Cdash(2,5)=Zero
      Cdash(2,6)=STRESS(6)
      Cdash(3,1)=Zero
      Cdash(3,2)=Zero
      Cdash(3,3)=TWO*STRESS(3)
      Cdash(3,4)=Zero
      Cdash(3,5)=STRESS(5)
      Cdash(3,6)=STRESS(6)
      Cdash(4,1)=STRESS(4)
      Cdash(4,2)=STRESS(4)
      Cdash(4,3)=Zero
      Cdash(4,4)=half*(STRESS(1)+STRESS(2))
      Cdash(4,5)=half*STRESS(6)
      Cdash(4,6)=half*STRESS(5)
      Cdash(5,1)=STRESS(5)
      Cdash(5,2)=Zero
      Cdash(5,3)=STRESS(5)
      Cdash(5,4)=half*STRESS(6)
      Cdash(5,5)=half*(STRESS(1)+STRESS(3))
      Cdash(5,6)=half*STRESS(4)
      Cdash(6,1)=Zero
      Cdash(6,2)=STRESS(6)
      Cdash(6,3)=STRESS(6)
      Cdash(6,4)=half*STRESS(5)
      Cdash(6,5)=half*STRESS(4)
      Cdash(6,6)=half*(STRESS(2)+STRESS(3))
      
      DO K1=1, 6
        DO K2=1, 6
          DDSDDE(K1, K2)=DDSDDE(K1, K2)+Cdash(K1, K2)
        END DO
      END DO
C
      RETURN
      END