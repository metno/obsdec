      SUBROUTINE QCSATOB (KSEC,IERR)
C
C**** *QCSATOB*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR SATOBS
C
C       INTERFACE.
C      ------------
C
C        CALL QCSATOB (KSEC,IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - SATOB REPORTS
C         KSEC   -  SECTION OF SATOB
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - SATOB REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        CHECK SURFACE LEVEL AGAINST LIMITS;
C        CHECK UPPER AIR LEVELS AGAINST LIMITS;
C        CHECK CONSISTENCY OF WIND SPEED AND DIRECTION.
C
C       EXTERNALS.
C      ------------
C
C        CDDFF  -  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C        CUAT   -  GROSS LIMIT CHECK FOR UPPER AIR PARAMETERS
C        CSLT   -  GROSS LIMIT CHECK FOR SURFACE PARAMETERS
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        SEASON -  DETERMINE RELEVANT SEASON
C        SETGRS -  UPDATE CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
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
      INCLUDE 'paramts.f'
      INCLUDE 'paramq.f'
      INCLUDE 'comwt.f'
      INCLUDE 'const.f'
C
C    ---------------------------------------------------------------------------
C
C                      1.  SETUP
C                    --------------
C
  100 CONTINUE
      IF(IERR.NE.0) RETURN
C
C          1.1  POSITIONS OF PARAMETERS WITHIN HEADER
C
      DATA NMONTH/4/,NRLAT/9/,NRLONG/10/
C
C          1.2  POSITIONS OF PARAMETERS WITHIN LEVEL
C
      NPP=IMISS
      NTT=IMISS
      NDD=IMISS
      NFF=IMISS
      NTS=IMISS
      NTC=IMISS
      IF(KSEC.EQ.2) THEN
           NPP=12
           NTT=13
           NDD=14
           NFF=15
      ELSEIF(KSEC.EQ.3) THEN
           NPP=12
           NDD=13
           NFF=14
      ELSEIF(KSEC.EQ.4) THEN
           NTS=12
      ELSEIF(KSEC.EQ.5) THEN
           NPP=12
           NTC=13
           NTT=14
      ELSE
           WRITE (*,'(1H ,''SATOB TYPE NOT RECOGNIZED:'',I5)') KSEC
           GO TO 400
      ENDIF
      IOFF=0
C
C          1.3  LOOP FOR REPORTS WITHIN ARRAY 'VALUES'
C
      DO 305 J=1,NSUB
      IERR=0
C
C          1.4  EXTRACT HEADER VALUES
C
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
      PP2=RMISS
      Z2=RMISS
C
C          1.7  EXTRACT PARAMETER VALUES
C
      IF(NPP.NE.IMISS) PP=VALUES(NPP,J)
      IF(NTT.NE.IMISS) TT=VALUES(NTT,J)
      IF(NDD.NE.IMISS) DD=VALUES(NDD,J)
      IF(NFF.NE.IMISS) FF=VALUES(NFF,J)
      IF(NTS.NE.IMISS) TS=VALUES(NTS,J)
      IF(NTC.NE.IMISS) TC=VALUES(NTC,J)
C
C    --------------------------------------------------------------------
C
C                2.  CHECKING OF PARAMETER VALUES
C             -------------------------------------
C
  200 CONTINUE
C
C          2.1  PRESSURE
C
      IF(NPP.NE.IMISS.AND.PP.NE.RMISS) THEN
           IF(PP.LT.0.0.OR.PP.GT.108000.0) CALL SETGRS (NPP,1)
      ENDIF
C
C          2.2  CLOUD AMOUNT
C
      IF(NTC.NE.IMISS.AND.TC.NE.RMISS) THEN
           IF(TC.LT.0.0.OR.TC.GT.100.0) CALL SETGRS (NTC,1)
      ENDIF
C
C          2.3  SURFACE TEMPERATURE
C
      IF(NTS.NE.IMISS.AND.TS.NE.RMISS) THEN
           CALL CSLT (IERR,TS,'TT',NTS,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''SATOB CSLT ERROR:'',I5,
     X                '' TS:'',F10.2)') IERR,TS
      ENDIF
C
C          2.4  CHECK TEMPERATURE DEPENDING ON PRESSURE
C
      IF(NPP.NE.IMISS.AND.PP.NE.RMISS) THEN
           IF(NTT.NE.IMISS.AND.TT.NE.RMISS) THEN
                CALL CUAT (IERR,TT,'TT',NTT,RLAT,RLONG,PP,PP2,Z2,
     X                                           NPP,IMISS,IMISS)
                IF(IERR.NE.0) WRITE (*,'(1H ,''SATOB CUAT ERROR:'',I5,
     X        '' PP:'',F10.2,'' TT:'',F10.2)') IERR,PP,TT
           ENDIF
C
C          2.5  CHECK WIND SPEED DEPENDING ON PRESSURE
C
           IF(NFF.NE.IMISS.AND.FF.NE.RMISS) THEN
                CALL CUAT (IERR,FF,'FF',NFF,RLAT,RLONG,PP,PP2,Z2,
     X                                           NPP,IMISS,IMISS)
                IF(IERR.NE.0) WRITE (*,'(1H ,''SATOB CUAT ERROR:'',I5,
     X        '' PP:'',F10.2,'' FF:'',F10.2)') IERR,PP,FF
           ENDIF
      ENDIF
C
C          2.6  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C
      IF (NDD.NE.IMISS.AND.NFF.NE.IMISS) CALL CDDFF (DD,FF,NDD,NFF)
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
