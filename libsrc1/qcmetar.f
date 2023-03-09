      SUBROUTINE QCMETAR (KODE,KPRES,IERR)
C
C**** *QCMETAR*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR METARS
C
C       INTERFACE.
C      ------------
C
C        CALL QCMETAR (KODE,KPRES,IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - SYNOP REPORTS
C         KODE   -  INDICATOR FOR SYNOP OR SHIP
C         KPRES  -  INDICATOR FOR HIGH OR LOW LEVEL REPORT
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - SYNOP REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        CHECK PARAMETER VALUES AGAINST GROSS LIMITS;
C ???    CHECK REDUCTION OF STATION LEVEL PRESSURE TO STANDARD LEVEL;
C        CHECK CONSISTENCY OF WIND SPEED AND DIRECTION;
C        CHECK INTERNAL CONSISTENCY OF COMMON PARAMETERS.
C
C       EXTERNALS.
C      ------------
C
C        CDDFF  -  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C        CSLT   -  GROSS LIMIT CHECK FOR SURFACE PARAMETERS
C        CUAT   -  GROSS LIMIT CHECK FOR UPPER AIR PARAMETERS
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        DPLIM  -  OBTAIN LIMITS FOR PRESSURE GIVEN HEIGHT
C        PREDUC -  CHECK PRESSURE REDUCTION TO STANDARD LEVEL
C        SEASON -  DETERMINE RELEVANT SEASON
C        SETINT -  SET CONFIDENCE FLAGS FOR INTERNAL CONSISTENCY CHECK
C        SETGRS -  UPDATE CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C        SETSF  -  SET CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C        SETABS -  SET CONFIDENCE FLAGS TO PARTICULAR VALUE
C        POSCHK -  COMPARE POSITION WITH PREVIOUS POSITIONS
C        UPFLAG -  ADD CONFIDENCE FLAGS TO REPORT
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
C         B. NORRIS,  ECMWF,  MARCH 1989.
C
C       MODIFICATIONS.
C      ----------------
C 
C         B. NORRIS, 14/03/90, ADD SHIP POSITION CHECK
C
      IMPLICIT LOGICAL (L,O,G)
      INCLUDE 'parameter.f'
      INCLUDE 'paramq.f'
      INCLUDE 'paramsh.f'
      PARAMETER (IUNIQ=12)
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
      DIMENSION PLIM(NMM)
      LOGICAL LPRES,LAND,LANSEA,LSHIP
      CHARACTER*38  CFNAME
      CHARACTER*9   CIDENT
      CHARACTER*9   CUNIQ(NUNIQ)
C
C    ---------------------------------------------------------------------------
C
C                      1.  SETUP
C                    --------------
C
  100 CONTINUE
      IF(IERR.NE.0) RETURN
      MISS=NINT(RMISS)
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
           NIY=4
           NIM=5
           NID=6
           NIH=7
           NIN=8
           NRLAT=9
           NRLONG=10
           NZSTN=11
C
C          1.2  POSITIONS OF METEOROLOGICAL PARAMETERS
C
ccccaug97           NPSTN=12
ccccaug97           NPMSL=13
ccccaug97           NDD=14
ccccaug97           NFF=15
ccccaug97           NFFMAX=16
ccccaug97           NDD1=17
ccccaug97           NDD2=18
ccccaug97           NTT=25
ccccaug97           NTD=26
ccccaug97           NVV=27
ccccaug97           NVV2=29
           NPQNH=12
           NDD=13
           NFF=14
           NFFMAX=15
           NDD1=16
           NDD2=17
           NTT=20
           NTD=21
           NVV=25
           NVV2=27
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
      ZSTN=0.0
      IF(NZSTN.NE.IMISS) ZSTN=VALUES(NZSTN,J)
      ZMSL=0.0
C
C          1.5   SET UP TABLES
C
      CALL DERITA (IERR,M)
      IF(IERR.NE.0) GO TO 500
C
C          1.6   DETERMINE SEASON
C
      CALL SEASON (RLAT,RLONG,IM)
      PP2=RMISS
      Z2=RMISS
C
C
C              2.  BASIC REPORT
C            --------------------
C
  200 CONTINUE
C
C         2.1  EXTRACT PARAMETER VALUES
C
caug97PSTN=VALUES(NPSTN,J)
      PQNH=VALUES(NPQNH,J)
      PMSL=RMISS
      DD=VALUES(NDD,J)
cpsjun99 Let cddff take care of this!      IF(DD.GT.360)DD=RMISS
      FF=VALUES(NFF,J)
      FFMAX=VALUES(NFFMAX,J)
      DD1=VALUES(NDD1,J)
      DD2=VALUES(NDD2,J)
      TT=VALUES(NTT,J)
      TD=VALUES(NTD,J)
      VV=VALUES(NVV,J)
      VV2=VALUES(NVV2,J)
      IWW=RMISS
C
C          2.2  GROSS LIMIT CHECKS
C
C
C                   STATION LEVEL PRESSURE (QNH)
C
caug97      IF(PSTN.NE.RMISS.AND.ZSTN.NE.RMISS) THEN
      IF(PQNH.NE.RMISS.AND.ZSTN.NE.RMISS) THEN
           CALL DPLIM (ZSTN,RLAT,RLONG,PLIM)
caug97         CALL SETSF (PSTN,NPSTN,PLIM)
           CALL SETSF (PQNH,NPQNH,PLIM)
      ENDIF
C
C                   SURFACE WIND SPEED
C     ff
      IF(FF.NE.RMISS) THEN
           CALL CSLT (IERR,FF,'FF',NFF,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' FF:'',F10.2)') IERR,FF
      ENDIF
C
C
C     Gust
      IF(FFMAX.NE.RMISS) THEN
           CALL CSLT (IERR,FFMAX,'FF',NFFMAX,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' FFMAX:'',F10.2)') IERR,FFMAX
      ENDIF
C
C                   SURFACE AIR TEMPERATURE
C
      IF(TT.NE.RMISS) THEN
           CALL CSLT (IERR,TT,'TT',NTT,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' TT:'',F10.2)') IERR,TT
      ENDIF
C
C                   SURFACE DEW POINT TEMPERATURE
C
      IF(TD.NE.RMISS) THEN
           CALL CSLT (IERR,TD,'TD',NTD,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' TD:'',F10.2)') IERR,TD
      ENDIF
C
C          2.3  INTERNAL CONSISTENCY CHECKS
C
C                WIND SPEED AND DIRECTION
C
      CALL CDDFF (DD,FF,NDD,NFF)
C
C     Gust > ff
      IF(FF.NE.RMISS.AND.FFMAX.NE.RMISS) THEN
         ICONF=IFAIL
         IF(FF.LE.FFMAX) ICONF=IPASS
cpsmai02 g77 complains if functions are not called with the expected number of arguments
         CALL SETINT (ICONF,NFF,NFFMAX,IEND,0,0,0,0,0,0,0)

      ENDIF
C
C                VAIABLE WIND DIRECTION
C
      IF(DD.NE.RMISS)THEN
C
C          dd1 > dd > dd2
           IF(DD1.NE.RMISS.AND.DD2.NE.RMISS) THEN
              ICONF=IFAIL
              IF(DD1.LE.DD.AND.DD2.GE.DD) ICONF=IPASS
              CALL SETINT (ICONF,NDD,NDD1,NDD2,IEND,0,0,0,0,0,0)
C           
           ELSE
C          dd1 >  dd2
C
              ICONF=IFAIL
              IF(DD1.LT.DD2) ICONF=IPASS
              CALL SETINT (ICONF,NDD1,NDD2,IEND,0,0,0,0,0,0,0)
C           
           ENDIF
      ENDIF
C                DEW POINT DEPRESSION
C
      IF(TT.NE.RMISS.AND.TD.NE.RMISS) THEN
           ICONF=IFAIL
           IF(.NOT.LAND) THEN
                IF(TT-TD.GE.-1.0.AND.TT-TD.LE.30.0) ICONF=IPASS
           ELSE
                IF(TT-TD.GE.0.0.AND.TT-TD.LE.50.0) ICONF=IPASS
           ENDIF
           CALL SETINT (ICONF,NTT,NTD,IEND,0,0,0,0,0,0,0)
C
C             DEW POINT DEPRESSION AND PRESENT WEATHER
C
           IF(IWW.NE.MISS) THEN
                ICONF=IPASS
                IF(TT-TD.GT.5.0.AND.((IWW.GE.40.AND.IWW.LE.49).OR.
     X                                IWW.EQ.120.OR.
     X                               (IWW.GE.130.AND.IWW.LE.135)))
     X          ICONF=IFAIL
                CALL SETINT (ICONF,NTT,NTD,NWW,IEND,0,0,0,0,0,0)
           ENDIF
      ENDIF
C
C               TEMPERATURE AND PRESENT WEATHER
C
      IF(TT.NE.RMISS.AND.IWW.NE.MISS) THEN
           ICONF=IPASS
           IF(TT.GT.5.0+RABS.AND.((IWW.GE.68.AND.IWW.LE.79).OR.
     X                            (IWW.GE.83.AND.IWW.LE.88).OR.
     X                             IWW.EQ.111.OR.IWW.EQ.124.OR.
     X                            (IWW.GE.145.AND.IWW.LE.146).OR.
     X                            (IWW.GE.167.AND.IWW.LE.168).OR.
     X                            (IWW.GE.170.AND.IWW.LE.176).OR.
     X                            (IWW.GE.185.AND.IWW.LE.187)))
     X     ICONF=IFAIL
           CALL SETINT (ICONF,NTT,NWW,IEND,0,0,0,0,0,0,0)
           ICONF=IPASS
           IF(TT.LT.-2.0+RABS.AND.((IWW.GE.50.AND.IWW.LE.55).OR.
     X                             (IWW.GE.58.AND.IWW.LE.65).OR.
     X                             (IWW.GE.68.AND.IWW.LE.69).OR.
     X                             (IWW.GE.80.AND.IWW.LE.82).OR.
     X                              IWW.EQ.123.OR.
     X                             (IWW.GE.143.AND.IWW.LE.144).OR.
     X                             (IWW.GE.151.AND.IWW.LE.153).OR.
     X                             (IWW.GE.157.AND.IWW.LE.158).OR.
     X                             (IWW.GE.161.AND.IWW.LE.163).OR.
     X                             (IWW.GE.167.AND.IWW.LE.168).OR.
     X                             (IWW.GE.181.AND.IWW.LE.184)))
     X     ICONF=IFAIL
           CALL SETINT (ICONF,NTT,NWW,IEND,0,0,0,0,0,0,0)
      ENDIF
C
C
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
