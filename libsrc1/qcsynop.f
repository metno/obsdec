      SUBROUTINE QCSYNOP (KSEC,KODE,KPRES,IERR)
C
C**** *QCSYNOP*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR SYNOPS AND SHIPS
C
C       INTERFACE.
C      ------------
C
C        CALL QCSYNOP (KSEC,KODE,KPRES,IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - SYNOP REPORTS
C         KSEC   -  SECTION OF SYNOP
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
C        COMPARE POSITION WITH PREVIOUS POSITION IF SHIP;
C        CHECK PARAMETER VALUES AGAINST GROSS LIMITS;
C        CHECK REDUCTION OF STATION LEVEL PRESSURE TO STANDARD LEVEL;
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
      LANSEA=.TRUE.
      LSHIP=.TRUE.
C          CFNAME = NAME OF TIME SERIES FILE
CRR      CFNAME=cstream(msys)//'/'//'SHIPPOSITION'
      CFNAME=cstream(msys)//'/'//'syno_shipposition'
ccc      print*,' qcsynop - cfname= ',cfname
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
      IF(KODE.LT.20) THEN
C               LAND
           NIY=4
           NIM=5
           NID=6
           NIH=7
           NIN=8
           NRLAT=9
           NRLONG=10
           NZSTN=11
           NTW=IMISS
           LAND=.TRUE.
      ELSE
C               SEA
           NIY=5
           NIM=6
           NID=7
           NIH=8
           NIN=9
           NRLAT=10
           NRLONG=11
           NZSTN=IMISS
           LAND=.FALSE.
      ENDIF
C
C          1.2  POSITIONS OF METEOROLOGICAL PARAMETERS
C
      IF(KSEC.EQ.1) THEN
C               BASIC REPORT
           IF(KPRES.LE.1.OR.(.NOT.LAND)) THEN
C                    LOW LEVEL OR SHIP
                NPSTN=12
                NPMSL=13
                NPSTD=IMISS
                NZ=IMISS
                NPPP=14
                NA=15
                NDD=16
                NFF=17
                NTT=18
                NTD=19
                NVV=21
                NWW=22
                NTC=25
                NNH=27
                NH=28
                NCL=29
                NCM=30
                NCH=31
                IF(.NOT.LAND) NTW=32
           ELSE
C                    HIGH LEVEL
                NPSTN=12
                NPMSL=IMISS
                NPSTD=13
                NZ=14
                NPPP=15
                NA=16
                NDD=17
                NFF=18
                NTT=19
                NTD=20
                NVV=22
                NWW=23
                NTC=26
                NNH=28
                NH=29
                NCL=30
                NCM=31
                NCH=32
           ENDIF
      ELSEIF(KSEC.EQ.2) THEN
C               EXTENDED REPORT
           IF(.NOT.LAND) THEN
C                    SHIP
                NDS=2
                NVS=3
                NTMIN=12
                NTMAX=13
CLiL
                NGUST=30
                NFFMAX=31
                NTGMIN=IMISS
CLiL
           ELSE
C                    LAND
                NDS=IMISS
                NVS=IMISS
CLiL
                NGUST=12
                NFFMAX=13
                NTGMIN=15
                NTMIN=16
                NTMAX=17
CLiL
CLi                NTMIN=13
CLi                NTMAX=14
CCC
C
           ENDIF
      ELSE
           WRITE (*,'(1H ,''SYNOP TYPE NOT RECOGNIZED:'',I5)') KSEC
           GO TO 500
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
C          1.7   SHIP POSITION
C
      IF(.NOT.LAND) THEN
           CALL POSCHK (CFNAME,CIDENT,IUNIQ,CUNIQ,NTYPE,NSBTYPE,
     X        IY,IM,ID,IH,IN,RLAT,RLONG,LANSEA,LSHIP,IFL,IERR)
           IF(IERR.NE.0) THEN
                WRITE (*,'(1H ,'' POSCHK ERROR:'',I5)') IERR
           ELSE
                CALL SETABS (IFL,NRLAT,NRLONG,IEND)
           ENDIF
      ENDIF
C
C    --------------------------------------------------------------------
C
C              2.  BASIC REPORT
C            --------------------
C
  200 CONTINUE
      IF(KSEC.NE.1) GO TO 300
C
C         2.1  EXTRACT PARAMETER VALUES
C
      PSTN=VALUES(NPSTN,J)
      PMSL=RMISS
      PSTD=RMISS
      Z=RMISS
      IF(NPMSL.NE.IMISS) PMSL=VALUES(NPMSL,J)
      IF(NPSTD.NE.IMISS) PSTD=VALUES(NPSTD,J)
      IF(NZ.NE.IMISS) Z=VALUES(NZ,J)/9.8
      PPP=VALUES(NPPP,J)
      IA=NINT(VALUES(NA,J))
      DD=VALUES(NDD,J)
      FF=VALUES(NFF,J)
      TT=VALUES(NTT,J)
      TD=VALUES(NTD,J)
      VV=VALUES(NVV,J)
      IWW=NINT(VALUES(NWW,J))
      TC=VALUES(NTC,J)
      INH=NINT(VALUES(NNH,J))
      H=VALUES(NH,J)
      ICL=NINT(VALUES(NCL,J))
      ICM=NINT(VALUES(NCM,J))
      ICH=NINT(VALUES(NCH,J))
      TW=RMISS
      IF(NTW.NE.IMISS) TW=VALUES(NTW,J)
C
C          2.2  GROSS LIMIT CHECKS
C
C
C                   STATION LEVEL PRESSURE
C
      IF(PSTN.NE.RMISS.AND.ZSTN.NE.RMISS) THEN
           CALL DPLIM (ZSTN,RLAT,RLONG,PLIM)
           CALL SETSF (PSTN,NPSTN,PLIM)
      ENDIF
C
C                   GEOPOTENTIAL
C
      IF(PSTD.NE.RMISS.AND.Z.NE.RMISS) THEN
           CALL CUAT (IERR,Z,'Z',NZ,RLAT,RLONG,PSTD,PP2,Z2,
     X                                       NPSTD,IMISS,IMISS)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CUAT ERROR:'',I5,
     X            ''PP:'',F10.2,'' Z:'',F10.2)') IERR,PSTD,Z
      ENDIF
C
C                   TOTAL CLOUD AMOUNT
C
      IF(TC.NE.RMISS) THEN
           IF(TC.LT.0.0.OR.TC.GT.109.0) CALL SETGRS (NTC,1)
clilaug97           IF(TC.LT.0.0.OR.TC.GT.100.0) CALL SETGRS (NTC,1)
      ENDIF
C
C                   LOWEST CLOUD AMOUNT
C
      IF(INH.NE.MISS) THEN
           IF(INH.LT.0.OR.INH.GT.9) CALL SETGRS (NNH,1)
      ENDIF
C
C                   PRESSURE CHARACTERISTIC
C
      IF(IA.NE.MISS.AND.IA.NE.15) THEN
           IF(IA.LT.0.OR.IA.GT.8) CALL SETGRS (NA,1)
      ENDIF
C
C                   SURFACE WIND SPEED
C
      IF(FF.NE.RMISS) THEN
           CALL CSLT (IERR,FF,'FF',NFF,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' FF:'',F10.2)') IERR,FF
      ENDIF
C
C                   MEAN SEA LEVEL PRESSURE
C
      IF(PMSL.NE.RMISS) THEN
           CALL CSLT (IERR,PMSL,'PPP',NPMSL,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' PMSL:'',F10.2)') IERR,PMSL
      ENDIF
C
C               3 HOUR PRESSURE TENDENCY
C
      IF(IA.GE.0.AND.IA.LE.8.AND.PPP.NE.RMISS) THEN
           CALL CSLT (IERR,PPP,'PP',NPPP,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' PPP:'',F10.2)') IERR,PPP
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
C                   SEA SURFACE TEMPERATURE
C
      IF(TW.NE.RMISS) THEN
           CALL CSLT (IERR,TW,'TW',NTW,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' TW:'',F10.2)') IERR,TW
      ENDIF
C
C          2.3  INTERNAL CONSISTENCY CHECKS
C
C                 REDUCTION OF STATION LEVEL PRESSURE
C                   TO STANDARD LEVEL
C
      IF(ZSTN.NE.RMISS.AND.PSTN.NE.RMISS.AND.
     X   PMSL.NE.RMISS.AND.TT.NE.RMISS) THEN
C                  PRESSURE REDUCED TO MSL
           LPRES=.FALSE.
           CALL PREDUC (ZMSL,ZSTN,PMSL,PSTN,TT,LPRES,
     X               IMISS,NZSTN,NPMSL,NPSTN,NTT)
      ENDIF
      IF(Z.NE.RMISS.AND.ZSTN.NE.RMISS.AND.PSTN.NE.RMISS.AND.
     X   PSTD.NE.RMISS.AND.TT.NE.RMISS) THEN
C                  GEOPOTENTIAL OF STANDARD PRESSURE LEVEL
           LPRES=.TRUE.
           CALL PREDUC (Z,ZSTN,PSTD,PSTN,TT,LPRES,
     X               NZ,NZSTN,NPSTD,NPSTN,NTT)
      ENDIF
C
C                WIND SPEED AND DIRECTION
C
      CALL CDDFF (DD,FF,NDD,NFF)
C
C                DEW POINT DEPRESSION
C
      IF(TT.NE.RMISS.AND.TD.NE.RMISS) THEN
           ICONF=IFAIL
           IF(.NOT.LAND) THEN
                IF(TT-TD.GE.-1.0.AND.TT-TD.LE.30.0) ICONF=IPASS
           ELSE
                IF(TT-TD.GE.0.0.AND.TT-TD.LE.50.0) ICONF=IPASS
           ENDIF
           CALL SETINT (ICONF,NTT,NTD,IEND)
C
C             DEW POINT DEPRESSION AND PRESENT WEATHER
C
           IF(IWW.NE.MISS) THEN
                ICONF=IPASS
                IF(TT-TD.GT.5.0.AND.((IWW.GE.40.AND.IWW.LE.49).OR.
     X                                IWW.EQ.120.OR.
     X                               (IWW.GE.130.AND.IWW.LE.135)))
     X          ICONF=IFAIL
                CALL SETINT (ICONF,NTT,NTD,NWW,IEND)
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
           CALL SETINT (ICONF,NTT,NWW,IEND)
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
           CALL SETINT (ICONF,NTT,NWW,IEND)
      ENDIF
C
C                  PRESSURE TENDENCY
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
C                 VISIBILITY AND PRESENT WEATHER
C
      IF(VV.NE.RMISS.AND.IWW.NE.MISS) THEN
           ICONF=IPASS
           IF(VV.LT.1000.0.AND.((IWW.GE.0.AND.IWW.LE.3).OR.
     X                          (IWW.GE.5.AND.IWW.LE.29).OR.
     X                           IWW.EQ.40.OR.
     X                          (IWW.GE.50.AND.IWW.LE.53).OR.
     X                          (IWW.GE.60.AND.IWW.LE.72).OR.
     X                          (IWW.GE.79.AND.IWW.LE.83).OR.
     X                          (IWW.GE.100.AND.IWW.LE.104).OR.
     X                          (IWW.GE.106.AND.IWW.LE.119).OR.
     X                          (IWW.GE.121.AND.IWW.LE.123).OR.
     X                          (IWW.GE.125.AND.IWW.LE.126).OR.
     X                          (IWW.GE.160.AND.IWW.LE.168).OR.
     X                          (IWW.GE.181.AND.IWW.LE.182)))
     X     ICONF=IFAIL
           CALL SETINT (ICONF,NVV,NWW,IEND)
           ICONF=IPASS
           IF(VV.GE.1000.0.AND.((IWW.GE.41.AND.IWW.LE.49).OR.
     X                           IWW.EQ.120.OR.
     X                          (IWW.GE.130.AND.IWW.LE.135)))
     X     ICONF=IFAIL
           CALL SETINT (ICONF,NVV,NWW,IEND)
      ENDIF
C
C                 CLOUD COVER AND PRESENT WEATHER
C
      IF(IWW.NE.MISS) THEN
           IF(TC.NE.RMISS) THEN
                ICONF=IPASS
                IF(TC.GE.0.0.AND.TC.LE.109.0.AND.
clilaug97                IF(TC.GE.0.0.AND.TC.LE.100.0.AND.
     X                     (IWW.EQ.43.OR.IWW.EQ.45.OR.
     X                      IWW.EQ.47.OR.IWW.EQ.49))  ICONF=IFAIL
                CALL SETINT (ICONF,NTC,NWW,IEND)
                ICONF=IPASS
                IF(TC.EQ.0.0.AND.(IWW.EQ.3.OR.
     X                           (IWW.GE.14.AND.IWW.LE.17).OR.
     X                           (IWW.GE.50.AND.IWW.LE.75).OR.
     X                           (IWW.GE.77.AND.IWW.LE.99).OR.
     X                            IWW.EQ.103.OR.
     X                           (IWW.GE.121.AND.IWW.LE.126).OR.
     X                           (IWW.GE.140.AND.IWW.LE.199)))
     X          ICONF=IFAIL
                CALL SETINT (ICONF,NTC,NWW,IEND)
           ENDIF
           IF(INH.NE.MISS) THEN
                ICONF=IPASS
                IF(INH.NE.9.AND.
     X                     (IWW.EQ.43.OR.IWW.EQ.45.OR.
     X                      IWW.EQ.47.OR.IWW.EQ.49))  ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NWW,IEND)
           ENDIF
      ENDIF
C
C                  CLOUD COVER AND CLOUD TYPE AND CLOUD HEIGHT
C
      IF(TC.NE.RMISS.AND.INH.NE.MISS.AND.ICH.NE.MISS) THEN
           ICONF=IPASS
C  TOTAL CLOUD > 0, LOWEST LAYER = 0, NO HIGH CLOUD
           IF(TC.GT.0.0.AND.INH.EQ.0.AND.ICH.EQ.10) ICONF=IFAIL
           CALL SETINT (ICONF,NTC,NNH,NCH,IEND)
           IF(ICM.NE.MISS) THEN
                ICONF=IPASS
C  TOTAL CLOUD = 100, LOWEST LAYER NOT COMPLETE,
C  MEDIUM AND HIGH CLOUD INVISIBLE
                IF(TC.EQ.100.0.AND.INH.LT.8.AND.ICM.EQ.61.AND.ICH.EQ.60)
     X          ICONF=IFAIL
                CALL SETINT (ICONF,NTC,NNH,NCM,NCH,IEND)
           ENDIF
      ENDIF
      IF(INH.NE.MISS) THEN
C  LOWEST LAYER INVISIBLE BUT CLOUD INFORMATION REPORTED
           IF(ICL.NE.MISS) THEN
                ICONF=IPASS
                IF(INH.EQ.9.AND.ICL.GE.30.AND.ICL.LE.39) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NCL,IEND)
           ENDIF
           IF(ICM.NE.MISS) THEN
                ICONF=IPASS
                IF(INH.EQ.9.AND.ICM.GE.20.AND.ICM.LE.29) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NCM,IEND)
           ENDIF
           IF(ICH.NE.MISS) THEN
                ICONF=IPASS
                IF(INH.EQ.9.AND.ICH.GE.10.AND.ICH.LE.19) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NCH,IEND)
           ENDIF
           IF(H.NE.RMISS) THEN
                ICONF=IPASS
                IF(INH.EQ.9.AND.H.GE.0.0) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NH,IEND)
           ENDIF
C  LOWEST LAYER = 0, BUT LOW AND/OR MEDIUM CLOUD REPORTED
           IF(ICL.NE.MISS) THEN
                ICONF=IPASS
                IF(INH.EQ.0.AND.ICL.GE.31.AND.ICL.LE.39) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NCL,IEND)
           ENDIF
           IF(ICM.NE.MISS) THEN
                ICONF=IPASS
                IF(INH.EQ.0.AND.ICM.GE.21.AND.ICM.LE.29) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NCM,IEND)
           ENDIF
           IF(H.NE.RMISS) THEN
                ICONF=IPASS
CLIL                IF(INH.EQ.0.AND.H.LT.2500.0) ICONF=IFAIL
c If no low clouds (Nh eq 0.0), then heigth of base should be above 2500m .
                IF(INH.EQ.0.0)THEN
                   IF(H.EQ.0.0)THEN
c                     no cloudbase.
                      ICONF=IPASS
                   ELSEIF(H.LT.2500.0)THEN
                      ICONF=IFAIL
                   ENDIF
                ENDIF
cLIL 
                CALL SETINT (ICONF,NNH,NH,IEND)
           ENDIF
      ENDIF
      IF(TC.NE.RMISS) THEN
C  TOTAL CLOUD = 0, BUT CLOUD TYPES OR HEIGHTS REPORTED
           IF(ICL.NE.MISS) THEN
                ICONF=IPASS
                IF(TC.EQ.0.0.AND.ICL.GE.31.AND.ICL.LE.39) ICONF=IFAIL
                CALL SETINT (ICONF,NTC,NCL,IEND)
           ENDIF
           IF(ICM.NE.MISS) THEN
                ICONF=IPASS
                IF(TC.EQ.0.0.AND.ICM.GE.21.AND.ICM.LE.29) ICONF=IFAIL
                CALL SETINT (ICONF,NTC,NCM,IEND)
           ENDIF
           IF(ICH.NE.MISS) THEN
                ICONF=IPASS
                IF(TC.EQ.0.0.AND.ICH.GE.11.AND.ICH.LE.19) ICONF=IFAIL
                CALL SETINT (ICONF,NTC,NCH,IEND)
           ENDIF
           IF(H.NE.RMISS) THEN
                ICONF=IPASS
CLIL                IF(TC.EQ.0.0.AND.H.LT.2500.0) ICONF=IFAIL
c If Total cloudcover (N) eq 0.0, then heigth of base should be 0.0.
                IF(TC.EQ.0.0)THEN
                   IF(H.EQ.0.0)THEN
c                     no cloudbase.
                      ICONF=IPASS
                   ELSEIF(H.LT.2500.0)THEN
                      ICONF=IFAIL
                   ENDIF
                ENDIF
cLIL 
                CALL SETINT (ICONF,NTC,NH,IEND)
           ENDIF
      ENDIF
      IF(TC.NE.RMISS.AND.ICL.NE.MISS.AND.ICM.NE.MISS.AND.ICH.NE.MISS)
     X      THEN
            ICONF=IPASS
C  TOTAL CLOUD > 0, BUT NO CLOUD TYPES REPORTED
            IF(TC.GT.0.0.AND.ICL.EQ.30.AND.ICM.EQ.20.AND.
     X         ICH.EQ.10) ICONF=IFAIL
            CALL SETINT (ICONF,NTC,NCL,NCM,NCH,IEND)
      ENDIF
      IF(INH.NE.MISS) THEN
           IF(ICL.NE.MISS) THEN
                IF(ICM.NE.MISS) THEN
                     ICONF=IPASS
C  LOWEST LAYER EXISTS BUT NO LOW OR MEDIUM CLOUD
                     IF(INH.NE.0.AND.ICL.EQ.30.AND.ICM.EQ.20)
     X                    ICONF=IFAIL
                     CALL SETINT (ICONF,NNH,NCL,NCM,IEND)
                     ICONF=IPASS
C  COMPLETE LOWEST LAYER BUT MEDIUM CLOUD NOT INVISIBLE
                     IF(INH.EQ.8.AND.ICL.GE.31.AND.ICL.LE.39.AND.
     X                   ICM.NE.61) ICONF=IFAIL
                     CALL SETINT (ICONF,NNH,NCL,NCM,IEND)
                ENDIF
                ICONF=IPASS
C  LOWEST LAYER NOT COMPLETE BUT LOW CLOUD INVISIBLE
                IF(INH.NE.9.AND.ICL.EQ.62) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NCL,IEND)
           ENDIF
           IF(ICH.NE.MISS) THEN
                ICONF=IPASS
C  COMPLETE LOWEST LAYER BUT HIGH CLOUD NOT INVISIBLE
                IF(INH.EQ.8.AND.ICH.NE.60) ICONF=IFAIL
                CALL SETINT (ICONF,NNH,NCH,IEND)
           ENDIF
      ENDIF
C
C               CONSISTENCY OF CLOUD TYPES
C
      IF(ICH.NE.MISS) THEN
           IF(ICM.NE.MISS) THEN
                ICONF=IPASS
C  MEDIUM CLOUD INVISIBLE BUT HIGH CLOUD NOT INVISIBLE
                IF(ICM.EQ.61.AND.ICH.NE.60) ICONF=IFAIL
                CALL SETINT (ICONF,NCM,NCH,IEND)
                IF(ICL.NE.MISS) THEN
                     ICONF=IPASS
C  LOW CLOUD INVISIBLE BUT MEDIUM AND HIGH CLOUD NOT INVISIBLE
                     IF(ICL.EQ.62.AND.ICM.NE.61.AND.ICH.NE.60)
     X               ICONF=IFAIL
                     CALL SETINT (ICONF,NCL,NCM,NCH,IEND)
                ENDIF
           ENDIF
      ENDIF
C
C              CLOUD HEIGHT AND VISIBILITY
C
      IF(H.NE.RMISS.AND.VV.NE.RMISS) THEN
           ICONF=IPASS
cLIL           IF(H.LT.100.0.AND.VV.GT.20000.0) ICONF=IFAIL
c If VV gt 20 km, then no low clouds or fog.
           IF(VV.GT.20000.0)THEN
              IF(H.EQ.0.0)THEN
c                no clouds.
                 ICONF=IPASS
              ELSEIF(H.LT.100.0)THEN
c                low clouds.
                 ICONF=IFAIL
              ENDIF
           ENDIF
cLIL 
           CALL SETINT (ICONF,NH,NVV,IEND)
      ENDIF
      GO TO 400
C
C    --------------------------------------------------------------------
C
C                3.  EXTENDED REPORT
C               ---------------------
C
  300 CONTINUE
      IF(KSEC.NE.2) GO TO 400
C
C           3.1  EXTRACT PARAMETER VALUES
C
      DS=RMISS
      VS=RMISS
      IF(NDS.NE.IMISS) DS=VALUES(NDS,J)
      IF(NVS.NE.IMISS) VS=VALUES(NVS,J)
      TMIN=VALUES(NTMIN,J)
      TMAX=VALUES(NTMAX,J)
CLiL
      RGUST=VALUES(NGUST,J)
      FFMAX=VALUES(NFFMAX,J)
      TGRO=RMISS
      IF(NTGMIN.NE.IMISS) TGRO=VALUES(NTGMIN,J)
CLiL
C
C           3.2  GROSS LIMIT CHECKS
C
CLiL
C                   GUST
C
      IF(RGUST.NE.RMISS) THEN
           CALL CSLT (IERR,RGUST,'FF',NGUST,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' GUST:'',F10.2)') IERR,GUST
      ENDIF
C
C
C                   SURFACE MAX 10 MIN WIND SPEED
C
      IF(FFMAX.NE.RMISS) THEN
           CALL CSLT (IERR,FFMAX,'FF',NFFMAX,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' FMAX:'',F10.2)') IERR,FFMAX
      ENDIF
C
C
C                   MINIMUM GROUND TEMPERATURE
C
      IF(TGRO.NE.RMISS) THEN
           CALL CSLT (IERR,TGRO,'TT',NTGMIN,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' TGRO:'',F10.2)') IERR,TMIN
      ENDIF
C
CLiL
C
C                   MINIMUM AIR TEMPERATURE
C
      IF(TMIN.NE.RMISS) THEN
           CALL CSLT (IERR,TMIN,'TT',NTMIN,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' TMIN:'',F10.2)') IERR,TMIN
      ENDIF
C
C                   MAXIMUM AIR TEMPERATURE
C
      IF(TMAX.NE.RMISS) THEN
           CALL CSLT (IERR,TMAX,'TT',NTMAX,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SYNOP CSLT ERROR:'',I5,
     X                '' TMAX:'',F10.2)') IERR,TMAX
      ENDIF
C
C            3.3  INTERNAL CONSISTENCY CHECKS
C
C
CLiL
C
C     10 min mean LE Gust
      IF(RGUST.NE.RMISS.AND.FFMAX.NE.RMISS) THEN
           ICONF=IFAIL
           IF(FFMAX.LE.RGUST) ICONF=IPASS
           CALL SETINT (ICONF,NGUST,NFFMAX,IEND)
      ENDIF
C
C
C     Tmin LE Tmax
      IF(TMAX.NE.RMISS.AND.TMIN.NE.RMISS) THEN
           ICONF=IFAIL
           IF(TMIN.LE.TMAX) ICONF=IPASS
           CALL SETINT (ICONF,NTMIN,NTMAX,IEND)
      ENDIF
C
C     Checking against TT and FF is not possible as they are in
C     the basic report.
CLiL
C
C                  SHIP SPEED AND DIRECTION
C
      CALL CDDFF (DS,VS,NDS,NVS)
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
