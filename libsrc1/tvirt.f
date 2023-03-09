      SUBROUTINE TVIRT (PP,TT,TD,TV)
C
C**** *TVIRT*
C
C     PURPOSE.
C     --------
C
C        CALCULATE VIRTUAL TEMPERATURE
C
C     INTERFACE.
C     ----------
C
C      CALL TVIRT (PP,TT,TD,TV)
C
C     INPUT
C            PP  -  PRESSURE (PA)
C            TT  -  TEMPERATURE  (DEG K)
C            TD  -  DEW-POINT TEMPERATURE  (DEG K)
C     OUTPUT
C            TV  -  VIRTUAL TEMPERATURE  (DEG K)
C
C     METHOD.
C     -------
C
C         CALCULATE VIRTUAL TEMPERATURE GIVEN PRESSURE, TEMPERATURE
C         AND DEWPOINT.
C
C     EXTERNALS.
C     ----------
C
C        NONE
C
C     REFERENCE.
C     ----------
C
C        NONE
C
C     AUTHOR.
C     -------
C
C        B. NORRIS,  ECMWF,  DECEMBER 1989.
C
C     MODIFICATIONS.
C     --------------
C
C        NONE
C
C
C    ------------------------------------------------------------------
C
C                     1.  PREPARATION
C                     ---------------
C
  100 CONTINUE
      DATA CO/231.82582/
      DATA EL/5418.118466/
      DATA TO/3.660322E-3/
C
C    -------------------------------------------------------------------
C
C                        2.  CALCULATION
C                       -----------------
C
  200 CONTINUE
      TV=TT*(1.+CO*EXP(EL*(TO-1./TD))/PP)
C
C    --------------------------------------------------------------------
C
C                        3.  EXIT
C                        --------
C
  300 CONTINUE
      RETURN
      END
