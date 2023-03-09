      SUBROUTINE MARDSEN(LAT,LONG,M,IERR)
C**** *MARDSEN*
C
C
C     PURPOSE.
C     --------
C
C         CHECK THE LAT&LONG AGAINST MARDSEN SQUARE
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *MARDSEN(LAT,LONG,M,IERR)*
C
C          INPUT     : LATITUDE  IN HUNDREDTH'S OF DEGREE
C                      LONGITUDE IN HUNDREDTH'S OF DEGREE
C              M     - MARDSEN SQUARE VALUE GIVEN IN REPORT
C
C          OUTPUT   : IERR  - ERROR INDICATOR, = 1 if error
C
C
C
C
C     METHOD.
C     -------
C
C          Recalculate Mardsen square (MMM) from LAT and LONG and compare with M.
C
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C     A. HOLOPAINEN  JUNE -84
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      implicit none
      integer LAT,LONG,M,IERR
      integer lolo,laa,loo,mmm,lo
C
C
C
C     ------------------------------------------------------------------
C
C*          1.   CHECK POSITION.
C                ---------------
      IERR = 0
      LOLO = LONG
C
C     DON'T CHECK IF POSITION IS ON THE LINE BETWEEN TWO (OR FOUR) SQUARES
C
      LAA = 1000*(LAT/1000)
      LOO = 1000*(LONG/1000)
C
      IF (LAA .EQ. LAT) RETURN
      IF (LOO .EQ. LONG) RETURN
C
      LAT = LAT /10
      LONG= LONG/10
C
      IF (LAT .GE. 0) THEN
         LO = IABS(LONG/100)+1
         IF (LOLO .GE. 0) LO = 37 - LO
         IF (LAT .LT. 800) THEN
            MMM = (LAT/100)*36 + LO
         ELSE
            MMM = 900 + LO
         END IF
      END IF
C
      IF (LAT .LT. 0) THEN
         LO = IABS( LONG/100 )
         IF (LOLO .GE. 0) LO = 35 - LO
         MMM = 300 + IABS( LAT/100 )*36 +LO
      END IF
C
      IF (MMM .NE. M) IERR = 1
C
      RETURN
      END
