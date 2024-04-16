!  abaqusSubroutine.f90 
!
!  FUNCTIONS:
!  abaqusSubroutine - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: abaqusSubroutine
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

      subroutine uanisohyper_strain(ebar, aj, ua, du1, du2,du3,
     1 temp, noel, cmname,incmpFlag, ihybFlag, ndi, nshr, ntens,
     2 numStatev, statev, numFieldv, fieldv, fieldvInc,
     3 numProps, props)
C
       INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
C
      dimension ebar(ntens), ua(2), du1(ntens+1)
      dimension du2((ntens+1)*(ntens+2)/2)
      dimension du3((ntens+1)*(ntens+2)/2)
      dimension statev(numStatev), fieldv(numFieldv)
      dimension fieldvInc(numFieldv), props(numProps)
C
      parameter ( half = 0.5d0,
     $ one = 1.d0,
     $ two = 2.d0,
     $ third = 1.d0/3.d0,
     $ twothds = 2.d0/3.d0,
     $ four = 4.d0 )
C
*
* Nonlinear anisotropic function by Jamal
*
      C1111=props(1)
      C1122=props(2)
      C1133=props(3)
      C1112=props(4)
      C1113=props(5)
      C1123=props(6)
      C2222=props(7)
      C2233=props(8)
      C2212=props(9)
      C2213=props(10)
      C2223=props(11)
      C3333=props(12)
      C3312=props(13)
      C3313=props(14)
      C3323=props(15)
      C1212=props(16)
      C1213=props(17)
      C1223=props(18)
      C1313=props(19)
      C1323=props(20)
      C2323=props(21)
C
      C2211=C1122
      C3311=C1133
      C3322=C2233
      C1211=C1112
      C1222=C2212
      C1233=C3312
      C1311=C1113
      C1322=C2213
      C1333=C3313
      C1312=C1213
      C2311=C1123
      C2322=C2223
      C2333=C3323
      C2312=C1223
      C2313=C1323
    
! Rest of the 45 components

      C3213 = C2313
      C3131 = C1313
      C2221 = C2212
      C2232 = C2223
      C3222 = C2322
      C2332 = C2323
      C3112 = C1312
      C2121 = C1212
      C2133 = C1233
      C3212 = C2312
      C3232 = C2323
      C3321 = C3312
      C2132 = C1223
      C1221 = C1212
      C3111 = C1311
      C3233 = C2333
      C3113 = C1313
      C2112 = C1212
      C1332 = C1323
      C1232 = C1223
      C3223 = C2323
      C2321 = C2312
      C3121 = C1312
      C2331 = C2313
      C2113 = C1213
      C1121 = C1112
      C1231 = C1213
      C3332 = C3323
      C1321 = C1312
      C2231 = C2213
      C2111 = C1211
      C3211 = C2311
      C1131 = C1113
      C3123 = C1323
      C2123 = C1223
      C2131 = C1213
      C3331 = C3313
      C3132 = C1323
      C3133 = C1333
      C3122 = C1322
      C1132 = C1123
      C2122 = C1222
      C1331 = C1313
      C3221 = C2312
      C3231 = C2313
C

      

C
      xpow = exp ( log(aj) * twothds )
      detuInv = one / aj
C ebar is the modified Green-Lagrange strain tensor
C E11, E22 is the Green-Lagrange strain tensor

      E11 = xpow * ebar(1) + half * ( xpow - one )
      E22 = xpow * ebar(2) + half * ( xpow - one )
      E33 = xpow * ebar(3) + half * ( xpow - one )
      E12 = xpow * ebar(4)
      E13 = xpow * ebar(5)
      E23 = xpow * ebar(6)
      
! rest of 3 components
      E21 = E12
      E31 = E13
      E32 = E23
C
      term1 = twothds * detuInv
      dE11Dj = term1 * ( E11 + half )
      dE22Dj = term1 * ( E22 + half )
      dE33Dj = term1 * ( E33 + half )
      dE12Dj = term1 * E12
      dE13Dj = term1 * E13
      dE23Dj = term1 * E23
C
      term2 = - third * detuInv
      d2E11DjDj = term2 * dE11Dj
      d2E22DjDj = term2 * dE22Dj
      d2E33DjDj = term2 * dE33Dj
      d2E12DjDj = term2 * dE12Dj
      d2E13DjDj = term2 * dE13Dj
      d2E23DjDj = term2 * dE23Dj
C
      d2UdE11dE11 = C1111 
      d2UdE11dE22 = C1122 
      d2UdE11dE33 = C1133 
C
      d2UdE11dE12 = C1112 
      d2UdE11dE13 = C1113 
      d2UdE11dE23 = C1123 
C
C
      d2UdE22dE11 = C2211 
      d2UdE22dE22 = C2222 
      d2UdE22dE33 = C2233 
C
      d2UdE22dE12 = C2212 
      d2UdE22dE13 = C2213 
      d2UdE22dE23 = C2223 
C
      d2UdE33dE11 = C3311 
      d2UdE33dE22 = C3322 
      d2UdE33dE33 = C3333 
C
      d2UdE33dE12 = C3312 
      d2UdE33dE13 = C3313 
      d2UdE33dE23 = C3323 
C
C
      d2UdE12dE11 = C1211 
      d2UdE12dE22 = C1222 
      d2UdE12dE33 = C1233 
C
      d2UdE12dE12 = C1212 
      d2UdE12dE13 = C1213 
      d2UdE12dE23 = C1223 
C
      d2UdE13dE11 = C1311 
      d2UdE13dE22 = C1322 
      d2UdE13dE33 = C1333 
C
      d2UdE13dE12 = C1312 
      d2UdE13dE13 = C1313 
      d2UdE13dE23 = C1323 
C
      d2UdE23dE11 = C2311 
      d2UdE23dE22 = C2322 
      d2UdE23dE33 = C2333 
C
      d2UdE23dE12 = C2312 
      d2UdE23dE13 = C2313 
      d2UdE23dE23 = C2323 


!   Now the stiffness part
      dUdE11 = C1111 * E11
     * + C1122 * E22
     * + C1133 * E33
     * + C1112 * E12 * two
     * + C1113 * E13 * two
     * + C1123 * E23 * two
    

C
      dUdE22 = C2211 * E11
     * + C2222 * E22
     * + C2233 * E33
     * + C2212 * E12 * two
     * + C2213 * E13 * two
     * + C2223 * E23 * two
     
C
      dUdE33 = C3311 * E11
     * + C3322 * E22
     * + C3333 * E33
     * + C3312 * E12 * two
     * + C3313 * E13 * two
     * + C3323 * E23 * two

C
      dUdE12 = C1211 * E11
     * + C1222 * E22
     * + C1233 * E33
     * + C1212 * E12 * two
     * + C1213 * E13 * two
     * + C1223 * E23 * two
C
      dUdE22 = C1311 * E11
     * + C1322 * E22
     * + C1333 * E33
     * + C1312 * E12 * two
     * + C1313 * E13 * two
     * + C1323 * E23 * two
C
      dUdE23 = C2311 * E11
     * + C2322 * E22
     * + C2333 * E33
     * + C2312 * E12 * two
     * + C2313 * E13 * two
     * + C2323 * E23 * two
!   Now we describe the strain energy
      U = half * E11 * C1111 * E11 
     * + half * E11 * C1122 * E22 
     * + half * E11 * C1133 * E33 
     * + half * E11 * C1112 * E12 
     * + half * E11 * C1113 * E13 
     * + half * E11 * C1123 * E23 
     * + half * E11 * C1121 * E21 
     * + half * E11 * C1131 * E31 
     * + half * E11 * C1132 * E32 
     * + half * E22 * C2211 * E11 
     * + half * E22 * C2222 * E22 
     * + half * E22 * C2233 * E33 
     * + half * E22 * C2212 * E12 
     * + half * E22 * C2213 * E13 
     * + half * E22 * C2223 * E23 
     * + half * E22 * C2221 * E21 
     * + half * E22 * C2231 * E31 
     * + half * E22 * C2232 * E32 
     * + half * E33 * C3311 * E11 
     * + half * E33 * C3322 * E22 
     * + half * E33 * C3333 * E33 
     * + half * E33 * C3312 * E12 
     * + half * E33 * C3313 * E13 
     * + half * E33 * C3323 * E23 
     * + half * E33 * C3321 * E21 
     * + half * E33 * C3331 * E31 
     * + half * E33 * C3332 * E32 
     * + half * E12 * C1211 * E11 
     * + half * E12 * C1222 * E22 
     * + half * E12 * C1233 * E33 
     * + half * E12 * C1212 * E12 
     * + half * E12 * C1213 * E13 
     * + half * E12 * C1223 * E23 
     * + half * E12 * C1221 * E21 
     * + half * E12 * C1231 * E31 
     * + half * E12 * C1232 * E32 
     * + half * E13 * C1311 * E11 
     * + half * E13 * C1322 * E22 
     * + half * E13 * C1333 * E33 
     * + half * E13 * C1312 * E12 
     * + half * E13 * C1313 * E13 
     * + half * E13 * C1323 * E23 
     * + half * E13 * C1321 * E21 
     * + half * E13 * C1331 * E31 
     * + half * E13 * C1332 * E32 
     * + half * E23 * C2311 * E11 
     * + half * E23 * C2322 * E22 
     * + half * E23 * C2333 * E33 
     * + half * E23 * C2312 * E12 
     * + half * E23 * C2313 * E13 
     * + half * E23 * C2323 * E23 
     * + half * E23 * C2321 * E21 
     * + half * E23 * C2331 * E31 
     * + half * E23 * C2332 * E32 
     * + half * E21 * C2111 * E11 
     * + half * E21 * C2122 * E22 
     * + half * E21 * C2133 * E33 
     * + half * E21 * C2112 * E12 
     * + half * E21 * C2113 * E13 
     * + half * E21 * C2123 * E23 
     * + half * E21 * C2121 * E21 
     * + half * E21 * C2131 * E31 
     * + half * E21 * C2132 * E32 
     * + half * E31 * C3111 * E11 
     * + half * E31 * C3122 * E22 
     * + half * E31 * C3133 * E33 
     * + half * E31 * C3112 * E12 
     * + half * E31 * C3113 * E13 
     * + half * E31 * C3123 * E23 
     * + half * E31 * C3121 * E21 
     * + half * E31 * C3131 * E31 
     * + half * E31 * C3132 * E32 
     * + half * E32 * C3211 * E11 
     * + half * E32 * C3222 * E22 
     * + half * E32 * C3233 * E33 
     * + half * E32 * C3212 * E12 
     * + half * E32 * C3213 * E13 
     * + half * E32 * C3223 * E23 
     * + half * E32 * C3221 * E21 
     * + half * E32 * C3231 * E31 
     * + half * E32 * C3232 * E32 
C
      ua(2) = U
      ua(1) = ua(2)
C
      du1(1) = xpow * dUdE11
      du1(2) = xpow * dUdE22
      du1(3) = xpow * dUdE33
      du1(4) = xpow * dUdE12
      du1(5) = xpow * dUdE13
      du1(6) = xpow * dUdE23
      du1(7) = dUdE11*dE11Dj + dUdE22*dE22Dj + dUdE33*dE33Dj
     * + two * ( dUdE12*dE12Dj
     *          +dUdE13*dE13Dj
     *          +dUdE23*dE23Dj )


C
      xpow2 = xpow * xpow
C
      du2(indx(1,1)) = xpow2 * d2UdE11dE11
      du2(indx(1,2)) = xpow2 * d2UdE11dE22
      du2(indx(2,2)) = xpow2 * d2UdE22dE22
      du2(indx(1,3)) = xpow2 * d2UdE11dE33
      du2(indx(2,3)) = xpow2 * d2UdE22dE33
      du2(indx(3,3)) = xpow2 * d2UdE33dE33
C
      du2(indx(1,4)) = xpow2 * d2UdE11dE12
      du2(indx(2,4)) = xpow2 * d2UdE22dE12
      du2(indx(3,4)) = xpow2 * d2UdE33dE12
C
      du2(indx(4,4)) = xpow2 * d2UdE12dE12
C
      du2(indx(1,5)) = xpow2 * d2UdE11dE13
      du2(indx(2,5)) = xpow2 * d2UdE22dE13
      du2(indx(3,5)) = xpow2 * d2UdE33dE13
      du2(indx(4,5)) = xpow2 * d2UdE12dE13
C
      du2(indx(5,5)) = xpow2 * d2UdE13dE13
C
      du2(indx(1,6)) = xpow2 * d2UdE11dE23
      du2(indx(2,6)) = xpow2 * d2UdE22dE23
      du2(indx(3,6)) = xpow2 * d2UdE33dE23
      du2(indx(4,6)) = xpow2 * d2UdE12dE23
      du2(indx(5,6)) = xpow2 * d2UdE13dE23
C
      du2(indx(6,6)) = xpow2 * d2UdE23dE23
! w.r.t J part 
      du2(indx(1,7)) = xpow * ( term1 * dUdE11
     * + d2UdE11dE11 * dE11Dj
     * + d2UdE11dE22 * dE22Dj
     * + d2UdE11dE33 * dE33Dj )
      du2(indx(2,7)) = xpow * ( term1 * dUdE22
     * + d2UdE22dE11 * dE11Dj
     * + d2UdE22dE22 * dE22Dj
     * + d2UdE22dE33 * dE33Dj )
      du2(indx(3,7)) = xpow * ( term1 * dUdE33
     * + d2UdE33dE11 * dE11Dj
     * + d2UdE33dE22 * dE22Dj
     * + d2UdE33dE33 * dE33Dj )
      du2(indx(4,7)) = xpow * ( term1 * dUdE12
     * + two * d2UdE12dE12 * dE12Dj )
      du2(indx(5,7)) = xpow * ( term1 * dUdE13
     * + two * d2UdE13dE13 * dE23Dj )
      du2(indx(6,7)) = xpow * ( term1 * dUdE23
     * + two * d2UdE23dE23 * dE13Dj )
      du2(indx(7,7))= dUdE11*d2E11DjDj
     * +dUdE22*d2E22DjDj
     * +dUdE33*d2E33DjDj
     * + two*( dUdE12*d2E12DjDj
     * +dUdE13*d2E13DjDj
     * +dUdE23*d2E23DjDj)
     * + d2UdE11dE11 * dE11Dj * dE11Dj
     * + d2UdE22dE22 * dE22Dj * dE22Dj
     * + d2UdE33dE33 * dE33Dj * dE33Dj
     * + two * ( d2UdE11dE22 * dE11Dj * dE22Dj
     * +d2UdE11dE33 * dE11Dj * dE33Dj
     * +d2UdE22dE33 * dE22Dj * dE33Dj )
     * + four * ( d2UdE12dE12 * dE12Dj * dE12Dj
     * +d2UdE13dE13 * dE13Dj * dE13Dj
     * +d2UdE23dE23 * dE23Dj * dE23Dj )
*
      return
      end


*
* Maps index from Square to Triangular storage
* of symmetric matrix
*
      integer function indx( i, j )
*
      include 'aba_param.inc'
*
      ii = min(i,j)
      jj = max(i,j)
*
      indx = ii + jj*(jj-1)/2
*
      return
      end


