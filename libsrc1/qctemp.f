      SUBROUTINE QCTEMP (KODE,IERR)
C
C**** *QCTEMP*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR TEMPS
C
C       INTERFACE.
C      ------------
C
C        CALL QCTEMP (KODE,IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - TEMP REPORTS
C         KODE   -  INDICATOR FOR TYPE OF TEMP
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - TEMP REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        COMPARE POSITION WITH PREVIOUS POSITION IF SHIP;
C        CHECK SURFACE LEVEL AGAINST LIMITS;
C        CHECK UPPER AIR LEVELS AGAINST LIMITS;
C        CHECK CONSISTENCY OF WIND SPEED AND DIRECTION;
C        CHECK FOR UNREASONABLE INVERSIONS OR LAPSE RATES;
C        CHECK CONSECUTIVE STANDARD LEVELS FOR EXCESSIVE WIND SHEAR;
C        CHECK HEIGHTS OF STANDARD LEVELS WITH THOSE RECOMPUTED
C            FROM SIGNIFICANT LEVELS;
C        HYDROSTATIC CHECK OF STANDARD LEVELS;
C        INSERT COMPUTED SIGNIFICANT LEVEL HEIGHTS IN BUFR MESSAGE
C
C       EXTERNALS.
C      ------------
C
C        CDDFF  -  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C        CUAT   -  GROSS LIMIT CHECK FOR UPPER AIR PARAMETERS
C        CSLT   -  GROSS LIMIT CHECK FOR SURFACE PARAMETERS
C        DPLIM  -  OBTAIN LIMITS FOR PRESSURE GIVEN HEIGHT
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        POSCHK -  COMPARE POSITION WITH PREVIOUS POSITIONS
C        INSHIZ -  DETERMINE SHIP PLATFORM HEIGHT
C        SEASON -  DETERMINE RELEVANT SEASON
C        SETSF  -  UPDATE CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C        SETGRS -  SET CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C        SETABS -  SET CONFIDENCE FLAGS TO PARTICULAR VALUE
C        LAPS   -  CHECK FOR UNREASONABLE INVERSIONS OR LAPSE RATES
C        WSHEAR -  CHECK WIND SHEAR BETWEEN STANDARD LEVELS
C        TVIRT  -  CALCULATE VIRTUAL TEMPERATURE
C        ZCOMP  -  RECOMPUTE AND CHECK STANDARD LEVEL HEIGHTS
C        HYDRO  -  HYDROSTATIC CHECK OF STANDARD LEVELS
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
C         B. NORRIS,  ECMWF,  FEBRUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C         B. NORRIS, 89/12/21, RECOMPUTE STANDARD LEVEL HEIGHTS
C         B. NORRIS, 90/02/14, INSERT COMPUTED SIGNIFICANT LEVEL
C                               HEIGHTS IN VALUES ARRAY
C         B. NORRIS, 90/03/26, LAPSE RATE CHECK
C         B. NORRIS, 90/03/27, HYDROSTATIC CHECK
C         B. NORRIS, 91/01/21, INSERT SHIP PLATFORM HEIGHT IN SURF LEVEL
C         B. NORRIS, 92/06/26, ADD SHIP POSITION CHECK
C         M. DRAGOSAVAC 93/04/06 READ IN BIAS CORRECTION TABLES ONLY ONCE
C
      IMPLICIT LOGICAL (L,O,G)
      INCLUDE 'parameter.f'
      INCLUDE 'paramq.f'
      INCLUDE 'paramsh.f'
      PARAMETER (IUNIQ=12)
      PARAMETER (JTEMP=100)
      INCLUDE 'comwt.f'
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
      DIMENSION PLIM(NMM)
      DIMENSION STDPP(NLEV),STDDD(NLEV),STDFF(NLEV)
      DIMENSION ISTDDD(NLEV),ISTDFF(NLEV)
      DIMENSION SPP(JTEMP),STT(JTEMP),STD(JTEMP)
      DIMENSION IVSS(JTEMP),SZ(JTEMP),STV(JTEMP),SZN(JTEMP)
      DIMENSION ISPP(JTEMP),ISTT(JTEMP),ISTD(JTEMP),ISZ(JTEMP)
      CHARACTER*38 CFNAME
      CHARACTER*9 CIDENT
      CHARACTER*9 CUNIQ(NUNIQ)
      LOGICAL LAND
      LOGICAL LANSEA
      LOGICAL LSHIP
      LOGICAL LBIAS
C
      INCLUDE 'parabias.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
crr      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
      save nsobc,nstbc,LBIAS
      save YSTIDBC,YDUMMYBC,YSTYPBC,YSNAMBC,ILEVBC,RCORBC,SZNBC,LBC
C
C
C    ---------------------------------------------------------------------------
C
C                      1.  SETUP
C                    --------------
C
  100 CONTINUE
C
C          1.1  DATA INITIALIZATION
C
C       POSITIONS OF PARAMETERS WITHIN LEVEL
      DATA NPP/1/,NVS/2/,NZ/3/,NTT/4/,NTD/5/,NDD/6/,NFF/7/
C       CRITICAL CONFIDENCE VALUE
      DATA ICRIT/65/
      LANSEA=.TRUE.
      LSHIP=.TRUE.
C          CFNAME = NAME OF TIME SERIES FILE
CRR      CFNAME=cstream(msys)//'/'//'SHIPTEMPPOSI'
      CFNAME=cstream(msys)//'/'//'temp_shipposition'
C
C          1.11  CALL SIGNS BEGINNING WITH THE FOLLOWING
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
C
C          1.2  POSITIONS OF PARAMETERS WITHIN HEADER
C
      IF(IERR.NE.0) RETURN

      IF(IERR.NE.0) THEN
          WRITE (*,'(1H ,'' BIASCOR ERROR:'',I5)') IERR
          RETURN
      ENDIF
      LAND=.FALSE.
      IF(KODE.EQ.35) THEN
           NIY=5
           NIM=6
           NID=7
           NIH=8
           NIN=9
           NRLAT=10
           NRLONG=11
           NALT=12
           LAND=.TRUE.
           NILEV=20
      ELSEIF(KODE.EQ.36.OR.KODE.EQ.135.OR.KODE.EQ.137) THEN
           NIY=6
           NIM=7
           NID=8
           NIH=9
           NIN=10
           NRLAT=11
           NRLONG=12
CRR info : nalt for mobile obs. is undefined.
           NILEV=21
      ELSE
           WRITE (*,'(1H ,''TEMP TYPE NOT RECOGNIZED:'',I5)') KODE
           GO TO 400
      ENDIF
      ILVSIZ=7
      IOFF=0
      NSTD=0
      NST=0
C
C          1.3  LOOP FOR REPORTS WITHIN ARRAY 'VALUES'
C
      DO 305 J=1,NSUB
      IERR=0
C
C
C          1.4  EXTRACT HEADER VALUES
C
      NUMLEV=NINT(VALUES(NILEV,J))
      IF(LAND) THEN
           ZSTN=VALUES(NALT,J)
      ELSE
           CALL INSHIZ (IERR,CIDENT,ZSTN)
           IF(IERR.NE.0) GO TO 400
      ENDIF
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
      IF(IERR.NE.0) GO TO 400
C
C          1.6   DETERMINE SEASON
C
      CALL SEASON (RLAT,RLONG,IM)
      PP2=RMISS
      Z2=RMISS
C
C          1.65  SHIP POSITION
C
      IF(KODE.EQ.36) THEN
           CALL POSCHK (CFNAME,CIDENT,IUNIQ,CUNIQ,NTYPE,NSBTYPE,
     X        IY,IM,ID,IH,IN,RLAT,RLONG,LANSEA,LSHIP,IFL,IERR)
           IF(IERR.NE.0) THEN
                WRITE (*,'(1H ,'' POSCHK ERROR:'',I5)') IERR
           ELSE
cpsmai02 g77 complains if functions are not called with the expected number of arguments
                CALL SETABS (IFL,NRLAT,NRLONG,IEND,0,0,0,0,0,0,0)
           ENDIF
      ENDIF
C
C          1.7   LOOP FOR LEVELS
C
      DO 245 ILEV=1,NUMLEV
      IBASE=(ILEV-1)*ILVSIZ+NILEV
      NNPP=IBASE+NPP
      NNZ=IBASE+NZ
      NNTT=IBASE+NTT
      NNTD=IBASE+NTD
      NNDD=IBASE+NDD
      NNFF=IBASE+NFF
      NNVS=IBASE+NVS
C
C          1.8  EXTRACT PARAMETER VALUES
C
      PP=VALUES(NNPP,J)
      Z =VALUES(NNZ ,J)
      IF(Z.NE.RMISS) Z=Z/9.8        ! CHANGED  10/11/89.
      TT=VALUES(NNTT,J)
      TD=VALUES(NNTD,J)
      DD=VALUES(NNDD,J)
      FF=VALUES(NNFF,J)
      IVS=NINT(VALUES(NNVS,J))
C
C    --------------------------------------------------------------------
C
C                2.  CHECKING OF PARAMETER VALUES
C             -------------------------------------
C
  200 CONTINUE
C
C            2.1   EXTRACT PARTICULAR LEVELS
C
C         2.11  EXTRACT STANDARD LEVEL WINDS
C
      IF(PP.NE.RMISS.AND.DD.NE.RMISS.AND.FF.NE.RMISS.AND.
cpsmai02     X  ((IVS.AND.32).EQ.32).AND.NSTD.LT.NLEV) THEN
     X  (iand(IVS,32).EQ.32).AND.NSTD.LT.NLEV) THEN
           NSTD=NSTD+1
           STDPP(NSTD)=PP
           STDDD(NSTD)=DD
           STDFF(NSTD)=FF
           ISTDDD(NSTD)=NNDD
           ISTDFF(NSTD)=NNFF
      ENDIF
C
C            2.12  EXTRACT TEMPERATURE LEVELS
C
      IF(PP.NE.RMISS.AND.TT.NE.RMISS.AND.NST.LT.JTEMP) THEN
           NST=NST+1
           SPP(NST) = PP
           STT(NST) = TT
           STD(NST) = TD
           IVSS(NST) = IVS
           SZ(NST) = Z
           SZN(NST) = RMISS
           ISPP(NST) = NNPP
           ISTT(NST) = NNTT
           ISTD(NST) = NNTD
           ISZ(NST) = NNZ
      ENDIF
C
C            2.2  CHECK SURFACE LEVEL
C
cpsmai02      IF((IVS.AND.64).EQ.64) THEN
      IF(iand(IVS,64).EQ.64) THEN
C
C          2.21  SURFACE PRESSURE
C
           IF(PP.NE.RMISS.AND.ZSTN.NE.RMISS) THEN
                CALL DPLIM (ZSTN,RLAT,RLONG,PLIM)
                CALL SETSF (PP,NNPP,PLIM)
           ENDIF
C
C          2.22  SURFACE TEMPERATURE
C
           IF(TT.NE.RMISS) THEN
                CALL CSLT (IERR,TT,'TT',NNTT,RLAT,RLONG)
                IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP CSLT ERROR:'',I5,
     X                '' TT:'',F10.2)') IERR,TT
           ENDIF
C
C          2.23  SURFACE DEWPOINT
C
           IF(TD.NE.RMISS) THEN
                CALL CSLT (IERR,TD,'TD',NNTD,RLAT,RLONG)
                IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP CSLT ERROR:'',I5,
     X                '' TD:'',F10.2)') IERR,TD
           ENDIF
C
C          2.24  SURFACE WIND SPEED
C
           IF(FF.NE.RMISS) THEN
                CALL CSLT (IERR,FF,'FF',NNFF,RLAT,RLONG)
                IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP CSLT ERROR:'',I5,
     X                '' FF:'',F10.2)') IERR,FF
           ENDIF
           GO TO 235
      ENDIF
C
C         2.3  UPPER AIR LEVEL
C
C              2.31  GROSS LIMIT CHECK FOR PRESSURE
C
      IF(PP.NE.RMISS) THEN
           IF(PP.LT.0.0.OR.PP.GT.108000.0) CALL SETGRS (NNPP,1)
      ENDIF
C
C              2.32  CHECK HEIGHT DEPENDING ON PRESSURE
C
      IF(PP.NE.RMISS.AND.Z.NE.RMISS) THEN
                CALL CUAT (IERR,Z,'Z',NNZ,RLAT,RLONG,PP,PP2,Z2,
     X                                           NNPP,IMISS,IMISS)
                IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP CUAT ERROR:'',I5,
     X            '' PP:'',F10.2,'' Z:'',F10.2)') IERR,PP,Z
      ENDIF
C
C              2.33  CHECK TEMPERATURE DEPENDING ON HEIGHT OR PRESSURE
C
      IF(PP.NE.RMISS.OR.Z.NE.RMISS) THEN
           IF(PP.NE.RMISS) THEN
                PAR1=PP
                NPAR1=NNPP
                PAR3=RMISS
                NPAR3=IMISS
           ELSE
                PAR1=RMISS
                NPAR1=IMISS
                PAR3=Z
                NPAR3=NNZ
           ENDIF
           IF(TT.NE.RMISS) THEN
                CALL CUAT (IERR,TT,'TT',NNTT,RLAT,RLONG,PAR1,PP2,PAR3,
     X                                          NPAR1,IMISS,NPAR3)
                IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP CUAT ERROR:'',I5,
     X   '' Z:'',F10.2,'' PP:'',F10.2,'' TT:'',F10.2)') IERR,Z,PP,TT
           ENDIF
C
C              2.34  CHECK WIND SPEED DEPENDING ON HEIGHT OR PRESSURE
C
           IF(FF.NE.RMISS) THEN
                CALL CUAT (IERR,FF,'FF',NNFF,RLAT,RLONG,PAR1,PP2,PAR3,
     X                                          NPAR1,IMISS,NPAR3)
                IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP CUAT ERROR:'',I5,
     X   '' Z:'',F10.2,'' PP:'',F10.2,'' FF:'',F10.2)') IERR,Z,PP,FF
           ENDIF
      ENDIF
  235 CONTINUE
C
C             2.4  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C
      CALL CDDFF (DD,FF,NNDD,NNFF)
C
C             2.41  CHECK DEWPOINT DEPRESSION
C
      IF(TT.NE.RMISS.AND.TD.NE.RMISS) THEN
           DEFCIT=TT-TD
           IF(DEFCIT.LT.-1.0.OR.DEFCIT.GT.50.0) THEN
                CALL SETGRS (NNTT,3)
                CALL SETGRS (NNTD,3)
           ENDIF
      ENDIF
  245 CONTINUE
C
C             2.5  CHECK INVERSION AND LAPSE RATE
C
      CALL LAPS (IERR,NST,SPP,STT,IVSS,ISPP,ISTT,RLAT,RLONG,
     X           CIDENT,IY,IM,ID,IH,IN)
      IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP LAPS ERROR:'',I5)') IERR
C
C             2.6  DEWPOINT IS OBTAINED AS DEPRESSION FROM TEMPERATURE
C
      IF(NST.GT.0) THEN
           DO 265 ILEV=1,NST
           IF(STD(ILEV).EQ.RMISS) GO TO 265
           IFTD=MIN(IFLAG(1,ISTT(ILEV)),IFLAG(1,ISTD(ILEV)))
           CALL SETABS (IFTD,ISTD(ILEV),IEND,0,0,0,0,0,0,0,0)
  265      CONTINUE
      ENDIF
C
C             2.7  CHECK WIND SHEAR BETWEEN STANDARD LEVELS
C
      CALL WSHEAR (IERR,NSTD,STDPP,STDDD,STDFF,ISTDDD,ISTDFF)
      IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP WSHEAR ERROR:'',I5)') IERR
C
C             2.71  CALCULATE VIRTUAL TEMPERATURE
C                    IF TD MISSING, TVIRT = TT
C
      DO 2715 ILEV=1,NST
      STV(ILEV)=RMISS
      IF(IFLAG(1,ISPP(ILEV)).GE.ICRIT.AND.
     X   IFLAG(1,ISTT(ILEV)).GE.ICRIT) THEN
           STV(ILEV)=STT(ILEV)
           IF(STD(ILEV).NE.RMISS.AND.IFLAG(1,ISTD(ILEV)).GE.ICRIT)
     X           CALL TVIRT (SPP(ILEV),STT(ILEV),STD(ILEV),STV(ILEV))
      ENDIF
 2715 CONTINUE
C
C             2.8  CHECK HEIGHTS OF STANDARD LEVELS
C
C         2.81  CHECK THAT STATION HEIGHT IS MEANINGFUL
C
      IF(ZSTN.EQ.RMISS) GO TO 288
crr 20.03.2002 
crr The following statement causes the program to core dump. NALT is undefined
crr  and the value of IFLAG(1,NALT) is therefore unpredictable. 
crr Logically the compiler should not evaluate IFLAG(1,NALT) if LAND is 'false'
crr  but it does so.   
crr 20.03.2002      IF(LAND.AND.IFLAG(1,NALT).LT.ICRIT) GO TO 288
      IF(LAND) then
         IF(IFLAG(1,NALT).LT.ICRIT) GO TO 288
      endif
C
C         2.82  CALCULATE STANDARD LEVEL HEIGHTS AND COMPARE
C
      CALL ZCOMP (IERR,NST,SPP,STV,IVSS,SZ,ISZ,SZN,ZSTN,
     X            CIDENT,IY,IM,ID,IH,IN)
      IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP ZCOMP ERROR:'',I5)') IERR
  288 CONTINUE
C
C         2.9   HYDROSTATIC CHECK OF STANDARD LEVELS
C
      CALL HYDRO (IERR,NST,SPP,STV,IVSS,SZ,ISZ,ISTT,ISTD,
     X            CIDENT,IY,IM,ID,IH,IN)
      IF(IERR.NE.0) WRITE (*,'(1H ,''TEMP HYDRO ERROR:'',I5)') IERR
C
C    --------------------------------------------------------------------
C
C                 3. ADD FLAGS AND SUBSTITUTIONS TO BUFR RECORD
C                ------------------------------------------------
C
  300 CONTINUE
C
C
C           3.01  ADD FLAGS
C
      CALL UPFLAG (IERR,J,VALUES,JP22,JP1,M,IOFF)
      IF(IERR.NE.0) GO TO 400
C
C           3.02  INSERT DERIVED HEIGHTS
C
      DO 302 ILEV=1,NST
      IF(SZ(ILEV).EQ.RMISS.AND.SZN(ILEV).NE.RMISS.AND.
cpsmai02     X  ((IVSS(ILEV).AND.4).EQ.4.OR.(IVSS(ILEV).AND.64).EQ.64)) THEN
     X  (iand(IVSS(ILEV),4).EQ.4.OR.iand(IVSS(ILEV),64).EQ.64)) THEN
C          VALUES(2*M+ISZ(ILEV),J)=SZN(ILEV)*9.8
C*****
C*****      REMOVE NEXT 2 LINES WHEN BUFR CAN HANDLE SUBSTITUTIONS
C*****
          VALUES(ISZ(ILEV),J)=SZN(ILEV)*9.8
          VALUES(M+ISZ(ILEV),J)=70.0
      ENDIF
  302 CONTINUE
C
C          DO THE BIAS CORRECTION
C          READ BIAS CORRECTION FILES
C
      LBC=.TRUE.
C
      IF(.NOT.LBIAS) THEN
      CALL RBCFILES (IERR,YSTIDBC,YSTYPBC,NSTBC,YSNAMBC,RCORBC,NSOBC)
      LBIAS=.TRUE.
      END IF
C
C          1.7   LOOP FOR LEVELS
C
      NST=0
      DO 303 ILEV=1,NUMLEV
      IBASE=(ILEV-1)*ILVSIZ+NILEV
      NNPP=IBASE+NPP
      NNZ=IBASE+NZ
c      NNTT=IBASE+NTT
c      NNTD=IBASE+NTD
c      NNDD=IBASE+NDD
c      NNFF=IBASE+NFF
c      NNVS=IBASE+NVS
c
C          1.8  EXTRACT PARAMETER VALUES
C
      PP=VALUES(NNPP,J)
      Z =VALUES(NNZ ,J)
      IF(Z.NE.RMISS) Z=Z/9.8        ! CHANGED  10/11/89.
c      TT=VALUES(NNTT,J)
c      TD=VALUES(NNTD,J)
c      DD=VALUES(NNDD,J)
c      FF=VALUES(NNFF,J)
c      IVS=NINT(VALUES(NNVS,J))
C
C            2.12  EXTRACT TEMPERATURE LEVELS
C
      IF(PP.NE.RMISS.AND.Z.NE.RMISS.AND.NST.LT.JTEMP) THEN
           NST=NST+1
           SPP(NST) = PP
c           STT(NST) = TT
c           STD(NST) = TD
c           IVSS(NST) = IVS
           SZ(NST) = Z
           SZNBC(NST) = RMISS
           ISPP(NST) = NNPP
c           ISTT(NST) = NNTT
c           ISTD(NST) = NNTD
           ISZ(NST) = NNZ
      ENDIF
C

  303 CONTINUE
C
C        2.10   BIASCORRECTION
C
      CALL BIASCOR (LBC,NST,SPP,SZ,SZNBC,
     X              YSTIDBC,YSTYPBC,NSTBC,YSNAMBC,RCORBC,NSOBC, 
     X              CIDENT,IM,ID,IH,RLAT,RLONG)
C
      DO 304 ILEV=1,NST
      IF( LBC.AND.SZNBC(ILEV).NE.RMISS) THEN
C     X  .AND.((IVSS(ILEV).AND.4).EQ.4.OR.(IVSS(ILEV).AND.64).EQ.64)) THEN
          VALUES(2*M+ISZ(ILEV),J)=SZNBC(ILEV)*9.8
      ENDIF
  304 CONTINUE
C END OF BIASCOR
C
C
  305 CONTINUE
C
C    --------------------------------------------------------------------
C
C                   4.  EXIT
C                  -----------
C
  400 CONTINUE
      RETURN
      END
