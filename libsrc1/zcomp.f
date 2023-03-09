      SUBROUTINE ZCOMP (IERR,NST,PP,TV,IVS,Z,NNZ,ZN,ZSTN,
     X                  CALLSN,IY,IM,ID,IH,IN)
C
C**** *ZCOMP*
C
C     PURPOSE.
C     --------
C
C        COMPARE STANDARD LEVEL HEIGHTS WITH HEIGHTS RECOMPUTED
C         FROM SIGNIFICANT TEMPERATURE LEVELS
C
C     INTERFACE.
C     ----------
C
C      CALL ZCOMP (IERR,NST,PP,TV,IVS,Z,NNZ,ZN,ZSTN,
C                  CALLSN,IY,IM,ID,IH,IN)
C
C     INPUT
C            NST -  NUMBER OF TEMPERATURE LEVELS
C            PP  -  PRESSURE (PA)
C            TV  -  VIRTUAL TEMPERATURE  (DEG K)
C            IVS -  VERTICAL SIGNIFICANCE
C            Z   -  HEIGHT (M)
C            NNZ -  FLAG POSITIONS FOR HEIGHT
C            ZSTN-  HEIGHT OF STATION
C            CALLSN  -  STATION NUMBER OR CALL SIGN
C            IY,IM,ID,IH,IN - YEAR,MONTH,DAY,HOUR,MINUTE OF REPORT
C     OUTPUT
C            IERR-  ERROR CODE
C            ZN  -  COMPUTED HEIGHT (M)
C
C     METHOD.
C     -------
C
C         LOOK FOR SURFACE LEVEL;
C         FOR ALL OTHER LEVELS, CALCULATE HEIGHT FROM NEXT LOWER
C         SIGNIFICANT LEVEL. IF STANDARD LEVEL, COMPARE OBSERVED
C         HEIGHT WITH RECOMPUTED HEIGHT.
C         CALCULATION OF HEIGHTS TERMINATES IF VIRTUAL TEMPERATURE
C         IS MISSING, OR LAYER IS TOO DEEP, OR PART B OR PART D
C         IS MISSING.
C
C     EXTERNALS.
C     ----------
C
C        NONE
C
C     REFERENCE.
C     ----------
C
C        GUIDE ON THE GLOBAL DATA PROCESSING SYSTEM WMO N305 1982
C           CHAPTER 6, QUALITY CONTROL PROCEDURES
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
      INCLUDE 'paramq.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
      DIMENSION PP(*),TV(*),IVS(*),Z(*),NNZ(*),ZN(*)
      CHARACTER*(*) CALLSN
      LOGICAL LB,LD
C
C    ------------------------------------------------------------------
C
C                     1.  INITIALIZATION
C                    --------------------
C
  100 CONTINUE
      IERR=0
      LB=.FALSE.
      LD=.FALSE.
C
C          1.1  CHECK THERE IS MORE THAN 1 LEVEL;
C                LOOK FOR SURFACE LEVEL
C
      IF(NST.LE.1) GO TO 300
      DO 112 ILEV=1,NST
cpssep03      IF((IVS(ILEV).AND.64).EQ.64) THEN
      IF(IAND(IVS(ILEV),64).EQ.64) THEN
           NLSIG=ILEV
           ZN(NLSIG)=ZSTN
           GO TO 114
      ENDIF
  112 CONTINUE
      GO TO 300
  114 CONTINUE
      IF(NLSIG.EQ.NST) GO TO 300
      ILOW=NLSIG+1
C
C           1.2  LOOK FOR PART B AND PART D
C
      DO 122 ILEV=1,NST
cpssep03      IF((IVS(ILEV).AND. 4).EQ.4.AND.
      IF(IAND(IVS(ILEV), 4).EQ.4.AND.
     X   IAND(IVS(ILEV),64).EQ.0.AND.
     X   IAND(IVS(ILEV),16).EQ.0.AND.
     X   PP(ILEV).GE.10000.0)  LB=.TRUE.
cpssep03      IF((IVS(ILEV).AND. 4).EQ.4.AND.
      IF(IAND(IVS(ILEV), 4).EQ.4.AND.
     X   IAND(IVS(ILEV),16).EQ.0.AND.
     X   PP(ILEV).LT.10000.0)  LD=.TRUE.
  122 CONTINUE
      IF(.NOT.LB) GO TO 300
C
C    -------------------------------------------------------------------
C
C                  2.  CALCULATE HEIGHTS
C                 -----------------------
C
  200 CONTINUE
      DO 245 ILEV=ILOW,NST
C
C          2.01  CHECK THAT PART D EXISTS
C
      IF(.NOT.LD.AND.PP(ILEV).LT.10000.0) GO TO 300
C
C          2.1  CHECK THAT VIRTUAL TEMPERATURE EXISTS
C
      IF(TV(NLSIG).EQ.RMISS.OR.TV(ILEV).EQ.RMISS) GO TO 300
C
C          2.2  CHECK DEPTH OF LAYER
C
      DP=PP(NLSIG)-PP(ILEV)
      DLIM=PP(NLSIG)*0.3+2000.0
      IF(DP.GT.DLIM) GO TO 300
C
C         2.3  CALCULATE HEIGHT FOR LEVEL
C
      DZ=(R/RG)*((TV(NLSIG)+TV(ILEV))/2.0)*ALOG(PP(NLSIG)/PP(ILEV))
      ZN(ILEV)=ZN(NLSIG)+DZ
cpssep03      IF((IVS(ILEV).AND.4).EQ.4) NLSIG=ILEV
      IF(IAND(IVS(ILEV),4).EQ.4) NLSIG=ILEV
C
C         2.4  COMPARE OBSERVED HEIGHT WITH RECOMPUTED HEIGHT
C
C     WRITE (*,'(1H ,''PP;Z;ZN;TV: '',4F12.2)')
C    X         PP(ILEV),Z(ILEV),ZN(ILEV),TV(ILEV)
      IF(Z(ILEV).NE.RMISS) THEN
           TOL=20.0
           IF(ZN(ILEV).GE.6000.0) TOL=30.0
           IF(ZN(ILEV).GE.15000.0) TOL=40.0
           IF(ABS(Z(ILEV)-ZN(ILEV)).GT.TOL) THEN
                CALL SETGRS (NNZ(ILEV),2)
C                WRITE (*,'(1H ,'' A COMPARED TO B     '',
C     X               A,2X,I4,4I2.2,3(A,F10.2))')
C     X               CALLSN,IY,IM,ID,IH,IN,
C     X               ' PP:',PP(ILEV),', ZA:',Z(ILEV),', ZB:',ZN(ILEV)
           ELSE
                CALL SETINT (IPASS,NNZ(ILEV),IEND)
           ENDIF
      ENDIF
  245 CONTINUE
C
C    --------------------------------------------------------------------
C
C                        3.  EXIT
C                        --------
C
  300 CONTINUE
      RETURN
      END
