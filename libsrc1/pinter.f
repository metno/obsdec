      SUBROUTINE PINTER (IERR,UPZ,PP,POS)
C
C**** *PINTER*
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
C         CALL PINTER (IERR,UPZ,PP,POS)
C
C        INPUT
C         UPZ    -  TABLE OF STANDARD PRESSURE OR HEIGHT LEVELS
C         PP     -  PRESSURE OR HEIGHT LEVEL
C
C        OUTPUT
C         IERR   -  ERROR CODE
C         POS    -  INTERPOLATED POSITION WITHIN TABLE
C
C       METHOD.
C      ---------
C
C           IF VALUES OF UPZ INCREASE, HEIGHT ASSUMED, LINEAR IN Z
C           IF VALUES OF UPZ DECREASE, PRESSURE ASSUMED, LINEAR IN LOGP
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
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
      INCLUDE 'paramq.f'
      DIMENSION UPZ(NLEV)
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
      IF(UPZ(1).GT.UPZ(NLEV)) LDOWN=.TRUE.
C
C    -------------------------------------------------------------------
C
C           2.  CHECK WHETHER VALUE IS OUTSIDE TABLE LIMITS
C         --------------------------------------------------
C
  200 CONTINUE
      IF(LDOWN) THEN
           IF(PP.GT.UPZ(1).OR.PP.LT.UPZ(NLEV)) THEN
                IERR=2101
                GO TO 400
           ENDIF
      ELSE
           IF(PP.LT.UPZ(1).OR.PP.GT.UPZ(NLEV)) THEN
                IERR=2101
                GO TO 400
           ENDIF
      ENDIF
C
C    -------------------------------------------------------------------------
C
C          3.   VALUE IS BETWEEN MAX AND MIN TABLE VALUES
C         ------------------------------------------------
C
  300 CONTINUE
      DO 312 JLEV=1,NLEV-1
C
C         3.1   CHECK IF VALUE  = TABLE VALUE
C
      IF(ABS(PP-UPZ(JLEV)).LT.EPS) THEN
           POS=FLOAT(JLEV)+EPS
           GO TO 400
      ENDIF
  312 CONTINUE
C
C         3.2   VALUE IS BETWEEN 2 TABLE VALUES
C
      DO 322 JLEV=1,NLEV-1
      IF(LDOWN) THEN
           IF(PP.LE.UPZ(JLEV).AND.PP.GE.UPZ(JLEV+1)) THEN
                JPOS=JLEV
                GO TO 324
           ENDIF
      ELSE
           IF(PP.GE.UPZ(JLEV).AND.PP.LE.UPZ(JLEV+1)) THEN
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
           POS=FLOAT(JPOS)+(ALOG(UPZ(JPOS))-ALOG(PP))/
     X         (ALOG(UPZ(JPOS))-ALOG(UPZ(JPOS+1))) +EPS
      ELSE
           POS=FLOAT(JPOS)+(UPZ(JPOS)-PP)/(UPZ(JPOS)-UPZ(JPOS+1))+EPS
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
