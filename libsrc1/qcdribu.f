      SUBROUTINE QCDRIBU (IERR)
C
C**** *QCDRIBU*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR DRIBUS
C
C       INTERFACE.
C      ------------
C
C        CALL QCDRIBU (IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - DRIBU REPORTS
C         L11  (COMWT)  -  LOGICAL FOR SURFACE REPORT
C         L12  (COMWT)  -  LOGICAL FOR SUB-SURFACE REPORT
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - DRIBU REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        CHECK PARAMETER VALUES AGAINST GROSS LIMITS;
C        CHECK CONSISTENCY OF WIND SPEED AND DIRECTION;
C        CHECK CONSISTENCY OF PRESSURE TENDENCY
C
C       EXTERNALS.
C      ------------
C
C        CDDFF  -  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C        CSLT   -  GROSS LIMIT CHECK FOR SURFACE PARAMETERS
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        SETINT -  UPDATE CONFIDENCE FLAGS FOR INTERNAL CONSISTENCY
C        SEASON -  DETERMINE RELEVANT SEASON
C        UPFLAG -  ADD CONFIDENCE FLAGS TO REPORT
C        POSCHK -  COMPARE POSITION WITH PREVIOUS POSITIONS
C
C       REFERENCE.
C      ------------
C
C        GUIDE ON THE GLOBAL DATA PROCESSING SYSTEM WMO N305 1982
C          CHAPTER 6,  QUALITY CONTROL PROCEDURES
C
C       AUTHOR.
C      ---------
C
C         B. NORRIS,  ECMWF,  JULY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
      IMPLICIT LOGICAL (L,O,G)
      INCLUDE 'parameter.f'
      INCLUDE 'paramq.f'
      INCLUDE 'paramsh.f'
      INCLUDE 'comwt.f'
      INCLUDE 'const.f'
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
crr      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
      CHARACTER*38  CFNAME
      CHARACTER*9   CIDENT
      CHARACTER*9   CUNIQ(NUNIQ)
C
      LOGICAL LANSEA
      LOGICAL LSHIP
      PARAMETER (IUNIQ=12)
C    ---------------------------------------------------------------------------
C
C                      1.  SETUP
C                    --------------
C
  100 CONTINUE
      IF(IERR.NE.0) RETURN
      MISS=NINT(RMISS)
C
      LANSEA=.TRUE.
      LSHIP=.TRUE.
C
C          CFNAME = NAME OF TIME SERIES FILE
      CFNAME=cstream(msys)//'/'//'drau_shipposition'
C
C
C
C          1.01  CALL SIGNS BEGINNING WITH THE FOLLOWING
C                     ARE NOT UNIQUE
C
      DATA (CUNIQ(K),K=1,IUNIQ)/
     X      'BOAT     ',
     X      'BUOY     ',
     X      'PLAT     ',
     X      'RIGG     ',
     X      'SHIP     ',
     X      'XXX      ',
     X      '???      ',
     X      '///      ',
     X      'BBXX     ',
     X      'ZSWAV    ',
     X      'CG2      ',
     X      'CG3      '/
C
C          1.1  POSITIONS OF PARAMETERS WITHIN HEADER
C
      IF(L11) THEN
C               SURFACE REPORT
           NIY=5
           NIM=6
           NID=7
           NIH=8
           NIN=9
           NRLAT=10
           NRLONG=11
      ENDIF
      IF(L12) THEN
C               SUB-SURFACE REPORT
           NIY=3
           NIM=4
           NID=5
           NIH=6
           NIN=7
           NRLAT=8
           NRLONG=9
      ENDIF
C
C
C          1.2  POSITIONS OF METEOROLOGICAL PARAMETERS
C
      IF(L11) THEN
           NPMSL=12
           NPPP=14
           NA=15
           NDD=16
           NFF=17
           NTT=18
           NTW=32
      ENDIF
      IOFF=0
C
C          1.3  LOOP FOR REPORTS WITHIN ARRAY 'VALUES'
C
      DO 405 J=1,NSUB
      IERR=0
C
C          1.4  EXTRACT HEADER VALUES
C
      IY=NINT(VALUES(NIY,J))
      IM=NINT(VALUES(NIM,J))
      ID=NINT(VALUES(NID,J))
      IH=NINT(VALUES(NIH,J))
      IN=NINT(VALUES(NIN,J))
      RLAT =VALUES(NRLAT,J)
      RLONG=VALUES(NRLONG,J)
C
C          1.5   SET UP TABLES
C
      CALL DERITA (IERR,M)
      IF(IERR.NE.0) GO TO 500
C
C          1.6   DETERMINE SEASON
C
      CALL SEASON (RLAT,RLONG,IM)
C
C          1.7   EXTRACT PARAMETER VALUES
C
      IF(.NOT.L11) GO TO 400
      PMSL=VALUES(NPMSL,J)
      PPP=VALUES(NPPP,J)
      IA=NINT(VALUES(NA,J))
      DD=VALUES(NDD,J)
      FF=VALUES(NFF,J)
      TT=VALUES(NTT,J)
      TW=VALUES(NTW,J)
C
C    ----------------------------------------------------------------------
C
C                 2.  GROSS LIMIT CHECKS
C                -------------------------
C
  200 CONTINUE
C
C             2.1   PRESSURE CHARACTERISTIC
C
      IF(IA.NE.MISS.AND.IA.NE.15) THEN
           IF(IA.LT.0.OR.IA.GT.8) CALL SETGRS (NA,1)
      ENDIF
C
C             2.2   SURFACE WIND SPEED
C
      IF(FF.NE.RMISS) THEN
           CALL CSLT (IERR,FF,'FF',NFF,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''DRIBU CSLT ERROR:'',I5,
     X                '' FF:'',F10.2)') IERR,FF
      ENDIF
C
C             2.3   MEAN SEA LEVEL PRESSURE
C
      IF(PMSL.NE.RMISS) THEN
           CALL CSLT (IERR,PMSL,'PPP',NPMSL,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''DRIBU CSLT ERROR:'',I5,
     X                '' PMSL:'',F10.2)') IERR,PMSL
      ENDIF
C
C             2.4   3 HOUR PRESSURE TENDENCY
C
      IF(IA.GE.0.AND.IA.LE.8.AND.PPP.NE.RMISS) THEN
           CALL CSLT (IERR,PPP,'PP',NPPP,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''DRIBU CSLT ERROR:'',I5,
     X                '' PPP:'',F10.2)') IERR,PPP
      ENDIF
C
C             2.5   SURFACE AIR TEMPERATURE
C
      IF(TT.NE.RMISS) THEN
           CALL CSLT (IERR,TT,'TT',NTT,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''DRIBU CSLT ERROR:'',I5,
     X                '' TT:'',F10.2)') IERR,TT
      ENDIF
C
C             2.6   SEA SURFACE TEMPERATURE
C
      IF(TW.NE.RMISS) THEN
           CALL CSLT (IERR,TW,'TW',NTW,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''DRIBU CSLT ERROR:'',I5,
     X                '' TW:'',F10.2)') IERR,TW
      ENDIF
C
C             2.7   POSITION CHECK (TIME CONTINUITY)
C
C
      CALL POSCHK (CFNAME,CIDENT,IUNIQ,CUNIQ,NTYPE,NSBTYPE,
     X    IY,IM,ID,IH,IN,RLAT,RLONG,LANSEA,LSHIP,IFL,IERR)
      IF(IERR.NE.0) THEN
          WRITE (*,'(1H ,'' POSCHK ERROR:'',I5)') IERR
      ELSE
          CALL SETABS (IFL,NRLAT,NRLONG,IEND)
      ENDIF
C
C   ----------------------------------------------------------------------
C
C                  3.  INTERNAL CONSISTENCY CHECKS
C                ------------------------------------
C
  300 CONTINUE
C
C             3.1   CONSISTENCY OF WIND SPEED AND DIRECTION
C
      CALL CDDFF (DD,FF,NDD,NFF)
C
C             3.2   CONSISTENCY OF PRESSURE TENDENCY
C
      IF(IA.NE.MISS.AND.IA.NE.15.AND.PPP.NE.RMISS) THEN
           ICONF=IPASS
           IF(PPP.EQ.0.0.AND.IA.NE.0.AND.IA.NE.4.AND.IA.NE.5)
     X     ICONF=IFAIL
           CALL SETINT (ICONF,NA,NPPP,IEND)
           ICONF=IPASS
           IF(IA.EQ.4.AND.PPP.NE.0.0)
     X     ICONF=IFAIL
           CALL SETINT (ICONF,NA,NPPP,IEND)
      ENDIF
C
C    --------------------------------------------------------------------
C
C                 4. ADD FLAGS TO BUFR RECORD
C                -------------------------------
C
  400 CONTINUE
      CALL UPFLAG (IERR,J,VALUES,JP22,JP1,M,IOFF)
      IF(IERR.NE.0) GO TO 500
C
  405 CONTINUE
C
C    --------------------------------------------------------------------
C
C                   5.  EXIT
C                  -----------
C
  500 CONTINUE
      RETURN
      END
