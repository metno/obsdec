      SUBROUTINE QCTOVS (K,IERR)
C
C**** *QCTOVS*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR TOVS
C
C       INTERFACE.
C      ------------
C
C        CALL QCTOVS (K,IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - TOVS REPORTS
C         K      -  INDICATOR FOR TYPE OF TOVS
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - TOVS REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        DETERMINE WHETHER LAYER MEAN TEMPERATURE OR PRECIPITABLE
C          WATER IS REPORTED;
C        CHECK UPPER AIR LEVELS AGAINST LIMITS.
C
C       EXTERNALS.
C      ------------
C
C        CUAT   -  GROSS LIMIT CHECK FOR UPPER AIR PARAMETERS
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        SEASON -  DETERMINE RELEVANT SEASON
C        SETGRS -  SET CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
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
C         B. NORRIS,  ECMWF,  JUNE 1989.
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
      LOGICAL LTEMP
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
      DATA NPP1/1/,NPP2/2/,NTM/5/,NWL/5/
C
C          1.2  POSITIONS OF PARAMETERS WITHIN HEADER
C
      IF(IERR.NE.0) RETURN
      IOFF=0
      NMONTH=5
      NRLAT=9
      NRLONG=10
C
C          1.3  SET STARTING POSITION OF LEVELS AND LENGTH OF LEVELS
C
      NILEV=18
      ILVSIZ=5
      NUMLEV=(M-NILEV)/ILVSIZ
C
C          1.4  DETERMINE WHETHER LAYER MEAN TEMPERATURE OR
C                    PRECIPITABLE WATER IS REPORTED
C
      IF(K.EQ.0.OR.K.EQ.1) THEN
           LTEMP=.TRUE.
      ELSEIF(K.EQ.2) THEN
           LTEMP=.FALSE.
      ELSE
           WRITE (*,'(1H ,''TOVS TYPE NOT RECOGNIZED:'',I5)') K
           GO TO 400
      ENDIF
C
C          1.5  LOOP FOR REPORTS WITHIN ARRAY 'VALUES'
C
      DO 305 J=1,NSUB
      IERR=0
C
C          1.6  EXTRACT HEADER VALUES
C
      MONTH=NINT(VALUES(NMONTH,J))
      RLAT =VALUES(NRLAT,J)
      RLONG=VALUES(NRLONG,J)
C
C          1.7   SET UP TABLES
C
      CALL DERITA (IERR,M)
      IF(IERR.NE.0) GO TO 400
C
C          1.8   DETERMINE SEASON
C
      CALL SEASON (RLAT,RLONG,MONTH)
      Z2=RMISS
C
C          1.9   LOOP FOR LEVELS
C
      DO 245 ILEV=1,NUMLEV
      IBASE=(ILEV-1)*ILVSIZ+NILEV
      NNPP1=IBASE+NPP1
      NNPP2=IBASE+NPP2
      IF(LTEMP) THEN
           NNTM=IBASE+NTM
      ELSE
           NNWL=IBASE+NWL
      ENDIF
C
C          1.10  EXTRACT PARAMETER VALUES
C
      PP1=VALUES(NNPP1,J)
      PP2=VALUES(NNPP2,J)
      IF(LTEMP) THEN
           TM=VALUES(NNTM,J)
      ELSE
           WL=VALUES(NNWL,J)
      ENDIF
C
C    --------------------------------------------------------------------
C
C                2.  CHECKING OF PARAMETER VALUES
C             -------------------------------------
C
  200 CONTINUE
C
C         2.1  UPPER AIR LEVEL
C
C              2.11  GROSS LIMIT FOR PRESSURE
C
      IF(PP1.NE.RMISS.AND.PP2.NE.RMISS) THEN
           IF(PP1.LT.0.0.OR.PP1.GT.108000.0) CALL SETGRS (NNPP1,1)
           IF(PP2.LT.0.0.OR.PP2.GT.108000.0) CALL SETGRS (NNPP2,1)
           IF(LTEMP) THEN
C
C              2.12  CHECK LAYER MEAN TEMPERATURE DEPENDING ON PRESSURE
C
                IF(TM.NE.RMISS) THEN
                     CALL CUAT (IERR,TM,'TM',NNTM,RLAT,RLONG,PP1,PP2,Z2,
     X                                           NNPP1,NNPP2,IMISS)
                     IF(IERR.NE.0) WRITE (*,'(1H ,''TOVS CUAT ERROR:'',
     X                   I5,'' PP1:'',F10.2,'' PP2:'',F10.2,
     X                   '' TM:'',F10.2)') IERR,PP1,PP2,TM
                ENDIF
           ELSE
C
C              2.13  CHECK PRECIPITABLE WATER DEPENDING ON PRESSURE
C
                IF(WL.NE.RMISS) THEN
                     CALL CUAT (IERR,WL,'WL',NNWL,RLAT,RLONG,PP1,PP2,Z2,
     X                                           NNPP1,NNPP2,IMISS)
                     IF(IERR.NE.0) WRITE (*,'(1H ,''TOVS CUAT ERROR:'',
     X                   I5,'' PP1:'',F10.2,'' PP2:'',F10.2,
     X                   '' WL:'',F10.2)') IERR,PP1,PP2,WL
                ENDIF
           ENDIF
      ENDIF
  245 CONTINUE
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
