      SUBROUTINE QCPILOT (KODE,IERR)
C
C**** *QCPILOT*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR PILOTS
C
C       INTERFACE.
C      ------------
C
C        CALL QCPILOT (KODE,IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - PILOT REPORTS
C         KODE   -  INDICATOR FOR TYPE OF PILOT
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - PILOT REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        CHECK SURFACE LEVEL AGAINST LIMITS;
C        CHECK UPPER AIR LEVELS AGAINST LIMITS;
C        CHECK CONSISTENCY OF WIND SPEED AND DIRECTION;
C        CHECK CONSECUTIVE STANDARD LEVELS FOR EXCESSIVE WIND SHEAR
C
C       EXTERNALS.
C      ------------
C
C        CDDFF  -  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C        CUAT   -  GROSS LIMIT CHECK FOR UPPER AIR PARAMETERS
C        CSLT   -  GROSS LIMIT CHECK FOR SURFACE PARAMETERS
C        DPLIM  -  OBTAIN LIMITS FOR PRESSURE GIVEN HEIGHT
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        SEASON -  DETERMINE RELEVANT SEASON
C        SETSF  -  UPDATE CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C        SETGRS -  SET CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C        WSHEAR -  CHECK WIND SHEAR BETWEEN STANDARD LEVELS
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
C         NONE
C
      INCLUDE 'parameter.f'
      INCLUDE 'paramq.f'
      INCLUDE 'comwt.f'
      INCLUDE 'const.f'
      DIMENSION PLIM(NMM)
      DIMENSION STDPP(NLEV),STDDD(NLEV),STDFF(NLEV)
      DIMENSION ISTDDD(NLEV),ISTDFF(NLEV)
      LOGICAL LAND
C
C    ---------------------------------------------------------------------------
C
C                      1.  SETUP
C                    --------------
C
  100 CONTINUE
C
C          1.1  POSITIONS OF PARAMETERS WITHIN LEVEL
C
      DATA NPP/1/,NVS/2/,NZ/3/,NDD/4/,NFF/5/
C
C          1.2  POSITIONS OF PARAMETERS WITHIN HEADER
C
      IF(IERR.NE.0) RETURN
      LAND=.FALSE.
      IF(KODE.EQ.32) THEN
           NMONTH=6
           NRLAT=10
           NRLONG=11
           NALT=12
           LAND=.TRUE.
      ELSEIF(KODE.EQ.33) THEN
           NMONTH=7
           NRLAT=11
           NRLONG=12
      ELSE
           WRITE (*,'(1H ,''PILOT TYPE NOT RECOGNIZED:'',I5)') KODE
           GO TO 400
      ENDIF
      NILEV=13
      ILVSIZ=5
      IOFF=0
      NSTD=0
C
C          1.3  LOOP FOR REPORTS WITHIN ARRAY 'VALUES'
C
      DO 305 J=1,NSUB
      IERR=0
C
C          1.4  EXTRACT HEADER VALUES
C
      NUMLEV=NINT(VALUES(NILEV,J))
      ZSTN=0.0
      IF(LAND) ZSTN=VALUES(NALT,J)
      MONTH=NINT(VALUES(NMONTH,J))
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
      CALL SEASON (RLAT,RLONG,MONTH)
      Z2=RMISS
      PP2=RMISS
C
C          1.7   LOOP FOR LEVELS
C
      DO 245 ILEV=1,NUMLEV
      IBASE=(ILEV-1)*ILVSIZ+NILEV
      NNPP=IBASE+NPP
      NNZ=IBASE+NZ
      NNDD=IBASE+NDD
      NNFF=IBASE+NFF
      NNVS=IBASE+NVS
C
C          1.8  EXTRACT PARAMETER VALUES
C
      PP=VALUES(NNPP,J)
      Z =VALUES(NNZ ,J)
      IF(Z.NE.999999.) Z=Z/9.8        ! CHANGED  10/11/89.
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
C            2.1  EXTRACT STANDARD LEVELS
C
      IF(PP.NE.RMISS.AND.DD.NE.RMISS.AND.FF.NE.RMISS.AND.
cpssep03     X  ((IVS.AND.32).EQ.32).AND.NSTD.LT.NLEV) THEN
     X  (IAND(IVS,32).EQ.32).AND.NSTD.LT.NLEV) THEN
           NSTD=NSTD+1
           STDPP(NSTD)=PP
           STDDD(NSTD)=DD
           STDFF(NSTD)=FF
           ISTDDD(NSTD)=NNDD
           ISTDFF(NSTD)=NNFF
      ENDIF
C
C            2.2  CHECK SURFACE LEVEL
C
cpssep03      IF((IVS.AND.64).EQ.64) THEN
      IF(IAND(IVS,64).EQ.64) THEN
C
C          2.21  SURFACE PRESSURE
C
           IF(PP.NE.RMISS.AND.ZSTN.NE.RMISS) THEN
                CALL DPLIM (ZSTN,RLAT,RLONG,PLIM)
                CALL SETSF (PP,NNPP,PLIM)
           ENDIF
C
C          2.22  SURFACE WIND SPEED
C
           IF(FF.NE.RMISS) THEN
                CALL CSLT (IERR,FF,'FF',NNFF,RLAT,RLONG)
                IF(IERR.NE.0) WRITE (*,'(1H ,''PILOT CSLT ERROR:'',I5,
     X                '' FF:'',F10.2)') IERR,FF
           ENDIF
           GO TO 235
      ENDIF
C
C         2.3  UPPER AIR LEVEL
C
C              2.31  GROSS LIMIT FOR PRESSURE
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
                IF(IERR.NE.0) WRITE (*,'(1H ,''PILOT CUAT ERROR:'',I5,
     X           '' PP:'',F10.2,'' Z:'',F10.2)') IERR,PP,Z
      ENDIF
C
C              2.33  CHECK WIND SPEED DEPENDING ON PRESSURE OR HEIGHT
C
      IF((PP.NE.RMISS.OR.Z.NE.RMISS).AND.FF.NE.RMISS) THEN
           IF(Z.NE.RMISS) THEN
                PAR1=RMISS
                NPAR1=IMISS
                PAR3=Z
                NPAR3=NNZ
           ELSE
                PAR1=PP
                NPAR1=NNPP
                PAR3=RMISS
                NPAR3=IMISS
           ENDIF
                CALL CUAT (IERR,FF,'FF',NNFF,RLAT,RLONG,PAR1,PP2,PAR3,
     X                                          NPAR1,IMISS,NPAR3)
                IF(IERR.NE.0) WRITE (*,'(1H ,''PILOT CUAT ERROR:'',I5,
     X   '' Z:'',F10.2,'' PP:'',F10.2,'' FF:'',F10.2)') IERR,Z,PP,FF
      ENDIF
  235 CONTINUE
C
C             2.4  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C
      CALL CDDFF (DD,FF,NNDD,NNFF)
  245 CONTINUE
C
C             2.5  CHECK WIND SHEAR BETWEEN STANDARD LEVELS
C
      CALL WSHEAR (IERR,NSTD,STDPP,STDDD,STDFF,ISTDDD,ISTDFF)
      IF(IERR.NE.0) WRITE (*,'(1H ,''PILOT WSHEAR ERROR:'',I5)') IERR
C
C    --------------------------------------------------------------------
C
C                 3. ADD FLAGS TO BUFR RECORD
C                -------------------------------
C
  300 CONTINUE
      CALL UPFLAG (IERR,J,VALUES,JP22,JP1,M,IOFF)
      IF(IERR.NE.0) GO TO 400
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
