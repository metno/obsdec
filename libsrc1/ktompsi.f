      SUBROUTINE KTOMPSI(IA)
C
C**** *KTOMPSI*
C
C
C     PURPOSE.
C     --------
C
C         CONVERTS KNOTS TO METRES PER SECOND, ROUNDING
C         TO NEAREST METRE.
C
C         INPUT    :  IA WIND SPEED IN KNOTS (INTEGER)
C
C         OUTPUT   :  IA WIND SPEED IN M/S (INTEGER)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *KTOMPSI(IA)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
C     ------------------------------------------------------------------
C
C*          1.   CONVERT WIND IN KNOTS TO METER PER SECOND .
C                -------------------------------------------
 100  CONTINUE
C
C
cpsjan08      IA=INT(0.5148 * IA +0.5)
cps It is more than 30 years since some countries (like UK) used the UK or
cps Admiralty nautical mile of 1853.184 m. Now all countries use 1852 m, so
cps 1 knot = 1 nautical mile/hour = 1852m/3600s = 0.5144 m/s  
      IA=INT(0.5144 * IA + 0.5)
      
      RETURN
      END
