      SUBROUTINE SEASON (RLAT,RLONG,MONTH)
C
C**** *SEASON*
C
C       PURPOSE.
C      ----------
C
C           DETERMINE SEASON
C
C       INTERFACE.
C      ------------
C
C           CALL SEASON (RLAT,RLONG,MONTH)
C
C        INPUT
C         RLAT   -  LATITUDE VALUE FOR REPORT
C         RLONG  -  LONGITUDE VALUE FOR REPORT
C         MONTH  -  MONTH FOR REPORT
C
C        OUTPUT
C         ISEASN (CONST) - SEASON (1=WINTER,2=SUMMER)
C
C       METHOD.
C      ---------
C
C        SUMMER IS ASSUMED IF NORTH OF EQUATOR, APRIL TO SEPTEMBER OR
C                             SOUTH OF EQUATOR, OCTOBER TO MARCH,
C           OTHERWISE WINTER.
C
C       EXTERNALS.
C      ------------
C
C        NONE
C
C       REFERENCE.
C      ------------
C
C        NONE
C
C       AUTHOR.
C      ---------
C
C        B. NORRIS,  ECMWF,  JANUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C        NONE
C
      INCLUDE 'const.f'
C
      ISEASN=1
      IF(RLAT.GT.0.0.AND.MONTH.GE.4.AND.MONTH.LE.9) ISEASN=2
      IF(RLAT.LT.0.0.AND.(MONTH.GE.10.OR.MONTH.LE.3)) ISEASN=2
C
      RETURN
      END
