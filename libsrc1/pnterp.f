      SUBROUTINE PNTERP (ILEV,NLEV,IPP,POS)
C
C**** *PNTERP*
C
C       PURPOSE.
C      ----------
C
C           INTERPOLATE POSITION OF PRESSURE OR HEIGHT
C               WITHIN TABLE OF UPPER AIR LEVELS
C
C       INTERFACE.
C      ------------
C
C         CALL PNTERP (ILEV,NLEV,IPP,POS)
C
C        INPUT
C         ILEV   -  TABLE OF STANDARD PRESSURE OR HEIGHT LEVELS
C         NLEV   -  DIMENSION OF ILEV
C         IPP    -  PRESSURE LEVEL
C
C        OUTPUT
C         IERR   -  ERROR CODE
C         POS    -  INTERPOLATED POSITION WITHIN TABLE
C
C       METHOD.
C      ---------
C
C           IF VALUES OF ILEV INCREASE, HEIGHT ASSUMED, LINEAR IN Z
C           IF VALUES OF ILEV DECREASE, PRESSURE ASSUMED, LINEAR IN LOGP
C
C       EXTERNALS.
C      -----------
C
C         NONE
C
C       REFERENCE.
C      ------------
C
C         NONE
C
C       AUTHOR.
C      ---------
C
C         B. NORRIS,  ECMWF,  JANUARY 1989.
C
C       MODIFICATIONS.A
C      ----------------
C
C         NONE
C
      DIMENSION ILEV(NLEV)
      LOGICAL LDOWN
      DATA EPS/0.000001/
C
C    -------------------------------------------------------------------
C
C                       1.  SETUP
C                     ------------
C
  100 CONTINUE
      IERR=0
      LDOWN=.FALSE.
      IF(ILEV(1).GT.ILEV(NLEV)) LDOWN=.TRUE.
C
C    -------------------------------------------------------------------
C
C           2.  CHECK WHETHER VALUE IS OUTSIDE TABLE LIMITS
C         --------------------------------------------------
C
  200 CONTINUE
      IF(LDOWN) THEN
           IF(IPP.GE.ILEV(1)) THEN
                POS=1.0+EPS
                GO TO 400
           ENDIF
           IF(IPP.LE.ILEV(NLEV)) THEN
                POS=FLOAT(NLEV)-EPS
                GO TO 400
           ENDIF
      ELSE
           IF(IPP.LE.ILEV(1)) THEN
                POS=1.0+EPS
                GO TO 400
           ENDIF
           IF(IPP.GE.ILEV(NLEV)) THEN
                POS=FLOAT(NLEV)-EPS
                GO TO 400
           ENDIF
      ENDIF
C
C    ---------------------------------------------------------------------------------------
C
C          3.   VALUE IS BETWEEN MAX AND MIN TABLE VALUES
C         ------------------------------------------------
C
  300 CONTINUE
      DO 312 JLEV=2,NLEV-1
C
C         3.1   CHECK IF VALUE  = TABLE VALUE
C
      IF(IPP.EQ.ILEV(JLEV)) THEN
           POS=FLOAT(JLEV)
           GO TO 400
      ENDIF
  312 CONTINUE
C
C         3.2   VALUE IS BETWEEN 2 TABLE VALUES
C
      DO 322 JLEV=1,NLEV-1
      IF(LDOWN) THEN
           IF(IPP.LE.ILEV(JLEV).AND.IPP.GE.ILEV(JLEV+1)) THEN
                JPOS=JLEV
                GO TO 324
           ENDIF
      ELSE
           IF(IPP.GE.ILEV(JLEV).AND.IPP.LE.ILEV(JLEV+1)) THEN
                JPOS=JLEV
                GO TO 324
           ENDIF
      ENDIF
  322 CONTINUE
  324 CONTINUE
C
C           3.3   INTERPOLATE
C
      IF(LDOWN) THEN
           POS=FLOAT(JPOS)+
     X         (ALOG(FLOAT(ILEV(JPOS)))-ALOG(FLOAT(IPP)))/
     X         (ALOG(FLOAT(ILEV(JPOS)))-ALOG(FLOAT(ILEV(JPOS+1))))
      ELSE
           POS=FLOAT(JPOS)+
     X  (FLOAT(ILEV(JPOS))-FLOAT(IPP))/
     X  (FLOAT(ILEV(JPOS))-FLOAT(ILEV(JPOS+1)))
      ENDIF
C
C    --------------------------------------------------------------------
C
C                   4.   EXIT
C                  -----------
C
  400 CONTINUE
      RETURN
      END
