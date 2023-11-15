!  abaqusSubroutine.f77
!
!  FUNCTIONS:
!  abaqusSubroutine - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: abaqusSubroutine
!
!  PURPOSE:  Ansiotropic invariant based subroutine.
!  Author:   Jamal Ahmed Bhatti
!
!****************************************************************************
      subroutine uanisohyper_inv (aInv, ua, zeta, nFibers, nInv,
     * ui1, ui2, ui3, temp, noel,
     * cmname, incmpFlag, ihybFlag,
     * numStatev, statev,
     * numFieldv, fieldv, fieldvInc,
     * numProps, props)
c
      include 'aba_param.inc'
c
      character *80 cmname
      dimension aInv(nInv), ua(2), zeta(nFibers*(nFibers-1)/2)
      dimension ui1(nInv), ui2(nInv*(nInv+1)/2)
      dimension ui3(nInv*(nInv+1)/2), statev(numStatev)
      dimension fieldv(numFieldv), fieldvInc(numFieldv)
      dimension props(numProps)
C
      parameter ( zero = 0.d0,
     * one = 1.d0,
     * two = 2.d0,
     * three = 3.d0,
     * four = 4.d0,
     * five = 5.d0,
     * six = 6.d0 )
      !PRINT *, 'Number of fibers=', nFibers
      !PRINT *, 'Zeta1=', zeta(1)
      !PRINT *, 'Zeta2=', zeta(2)
      !PRINT *, 'Number of Inv=', nInv
C
C Kaliske-Schmidtt energy function (3D)
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
C Compute Udev and 1st and 2nd derivatives w.r.t invariants
C -I1
      bi1 = aInv(1)
      PRINT *, 'FirstInvariant=', bi1, ';'
      term = bi1-three
      ua(2) = a1*term + a2*term**2 + a3*term**3
      ui1(1) = a1 + two*a2*term + three*a3*term**2
      PRINT *, 'dUdI1=', ui1(1), ';'
      ui2(indx(1,1)) = two*a2 + three*two*a3*term
C -I2
      bi2 = aInv(2)
      PRINT *, 'SecondInvariant=', bi2, ';'
      term = bi2-three
      ua(2) = ua(2) + b1*term + b2*term**2 + b3*term**3
      ui1(2) = b1 + two*b2*term + three*b3*term**2
      PRINT *, 'dUdI2=', ui1(2), ';'
      ui2(indx(2,2)) = two*b2 + three*two*b3*term
C - I3 (=J)
      bi3 = aInv(3)
      PRINT *, 'ThirdInvariant=', bi3, ';'
      term = bi3-one
      ui1(3) = two*dInv*term
      PRINT *, 'dUdIJ=', ui1(3), ';'
      ui2(indx(3,3)) = two*dInv
C - I4(11)
      nI411 = indxInv4(1,1)
      bi411 = aInv(nI411)
      PRINT *, 'FourthInvariant=', bi411, ';'
      term = bi411-one
      !term = bi411-three
      ua(2) = ua(2)
     * + c1*term
     * + c2*term**2 + c3*term**3 + c4*term**4
     * + c5*term**5 + c6*term**6
      ui1(nI411) =
     * c1
     * + two*c2*term
     * + three*c3*term**2
     * + four*c4*term**3
     * + five*c5*term**4
     * + six*c6*term**5
      PRINT *, 'dUdI4=', ui1(nI411), ';'
      ui2(indx(nI411,nI411)) =
     * two*c2
     * + three*two*c3*term
     * + four*three*c4*term**2
     * + five*four*c5*term**3
     * + six*five*c6*term**4
C - I5(11)
      nI511 = indxInv5(1,1)
      bi511 = aInv(nI511)
      PRINT *, 'FifthInvariant=', bi511, ';'
      term = bi511-one
      !term = bi511-three
      ua(2) = ua(2)
     * + d1*term
     * + d2*term**2 + d3*term**3 + d4*term**4
     * + d5*term**5 + d6*term**6
      ui1(nI511) =
     * d1 
     * + two*d2*term
     * + three*d3*term**2
     * + four*d4*term**3
     * + five*d5*term**4
     * + six*d6*term**5
      PRINT *, 'dUdI5=', ui1(nI511), ';'
      ui2(indx(nI511,nI511)) =
     * two*d2
     * + three*two*d3*term
     * + four*three*d4*term**2
     * + five*four*d5*term**3
     * + six*five*d6*term**4

C
c Add volumetric energy
c
      term = aInv(3) - one
      volumetric_energy = dInv*term*term
      PRINT *, 'volumetric_energy=', volumetric_energy, ';'
      ua(1) = ua(2) + volumetric_energy
C
      return
      end
C
C Maps index from Square to Triangular storage of symmetric matrix
      integer function indx( i, j )
C
      include 'aba_param.inc'
C
      ii = min(i,j)
      jj = max(i,j)
C
      indx = ii + jj*(jj-1)/2
C
      return
      end
C
C
C Generate enumeration of Anisotropic Pseudo Invariants of type 4 
      integer function indxInv4( i, j )
C
      include 'aba_param.inc'
C
      ii = min(i,j)
      jj = max(i,j)
C
      indxInv4 = 4 + jj*(jj-1) + 2*(ii-1)
C
      return
      end
C
C
C Generate enumeration of Anisotropic Pseudo Invariants oftype 5 
      integer function indxInv5( i, j )
C
      include 'aba_param.inc'
C
      ii = min(i,j)
      jj = max(i,j)
C
      indxInv5 = 5 + jj*(jj-1) + 2*(ii-1)
C
      return
      end

