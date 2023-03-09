      SUBROUTINE QCAIREP (IERR)
C
C**** *QCAIREP*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR AIREPS
C
C       INTERFACE.
C      ------------
C
C        CALL QCAIREP (IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - AIREP REPORTS
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - AIREP REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        CHECK FLIGHT LEVEL AGAINST LIMITS, THEN CHECK TEMPERATURE
C          AND WIND SPEED FOR PARTICULAR HEIGHT;
C        CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C
C       EXTERNALS.
C      ------------
C
C        CDDFF  -  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C        CUAT   -  GROSS LIMIT CHECK FOR UPPER AIR PARAMETERS
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        SEASON -  DETERMINE RELEVANT SEASON
C        SETGRS -  UPDATE CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
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
C         B. NORRIS,  ECMWF,  JANUARY 1989.
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
      PARAMETER (IUNIQ=5)
C
C    ---------------------------------------------------------------------------
C
C                      1.  SETUP
C                    --------------
C
  100 CONTINUE
c
      IF(IERR.NE.0) RETURN
      IOFF=0
      MISS=NINT(RMISS)
C
      LANSEA=.FALSE.
      LSHIP=.FALSE.
C
C          CFNAME = NAME OF TIME SERIES FILE
C
crr      CFNAME=cstream(msys)//'/'//'AIREPOSITION'
      CFNAME=cstream(msys)//'/'//'arep_shipposition'
C
C          1.0  CALL SIGNS BEGINNING WITH THE FOLLOWING
C                     ARE NOT UNIQUE
C
      DATA (CUNIQ(K),K=1,IUNIQ)/
     X      'XXX      ',
     X      '???      ',
     X      '///      ',
     X      'XX999    ',
     X      'XXX99    '/
C
C
C          1.1  POSITIONS OF PARAMETERS WITHIN REPORT
C
      DATA NIY/3/,NIM/4/,NID/5/,NIH/6/,NIN/7/,NRLAT/8/,NRLONG/9/
      DATA NZ/11/,NTT/12/,NDD/13/,NFF/14/
C
C
C          1.2  LOOP FOR REPORTS WITHIN ARRAY 'VALUES'
C
      DO 305 J=1,NSUB
      IERR=0
C
C          1.3  EXTRACT PARAMETER VALUES
C
      IY=NINT(VALUES(NIY,J))
      IM=NINT(VALUES(NIM,J))
      ID=NINT(VALUES(NID,J))
      IH=NINT(VALUES(NIH,J))
      IN=NINT(VALUES(NIN,J))
      RLAT =VALUES(NRLAT,J)
      RLONG=VALUES(NRLONG,J)
      Z    =VALUES(NZ,J)
      TT   =VALUES(NTT,J)
      DD   =VALUES(NDD,J)
      FF   =VALUES(NFF,J)
C
C          1.4   SET UP TABLES
C
      CALL DERITA (IERR,M)
      IF (IERR.NE.0) GO TO 400
C
C          1.5   DETERMINE SEASON
C
      CALL SEASON (RLAT,RLONG,IM)
      PP1=RMISS
      PP2=RMISS
C
C    --------------------------------------------------------------------
C
C                2.  CHECKING OF PARAMETER VALUES
C             -------------------------------------
C
  200 CONTINUE
      IF(Z.NE.RMISS) THEN
C
C            2.1  CHECK VALUE OF HEIGHT AGAINST LIMITS
C
           IF(Z.LT.10.0.OR.Z.GT.25000.0) THEN
               CALL SETGRS (NZ,2)
C
C            2.2  IF Z IS BAD, FLAG TT,DD AND FF ALSO
C
               CALL SETGRS (NTT,3)
               CALL SETGRS (NDD,3)
               CALL SETGRS (NFF,3)
           ENDIF
C
           IF(TT.NE.RMISS) THEN
C
C             2.3  CHECK TEMPERATURE DEPENDING ON HEIGHT
C
                CALL CUAT (IERR,TT,'TT',NTT,RLAT,RLONG,PP1,PP2,Z,
     X                                           IMISS,IMISS,NZ)
                IF(IERR.NE.0) WRITE (*,'(1H ,''AIREP CUAT ERROR:'',I5,
     X                '' Z:'',F10.2,'' TT:'',F10.2)') IERR,Z,TT
           ENDIF
C
           IF(FF.NE.RMISS) THEN
C
C              2.4  CHECK WIND SPEED DEPENDING ON HEIGHT
C
                CALL CUAT (IERR,FF,'FF',NFF,RLAT,RLONG,PP1,PP2,Z,
     X                                           IMISS,IMISS,NZ)
                IF(IERR.NE.0) WRITE (*,'(1H ,''AIREP CUAT ERROR:'',I5,
     X               '' Z:'',F10.2,'' FF:'',F10.2)') IERR,Z,FF
           ENDIF
      ENDIF
C
C             2.5  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C
      CALL CDDFF (DD,FF,NDD,NFF)
C
C
C
C             2.6   POSITION CHECK (TIME CONTINUITY)
C
C
      CALL POSCHK (CFNAME,CIDENT,IUNIQ,CUNIQ,NTYPE,NSBTYPE,
     X    IY,IM,ID,IH,IN,RLAT,RLONG,LANSEA,LSHIP,IFL,IERR)
      IF(IERR.NE.0) THEN
          WRITE (*,'(1H ,'' POSPCHK ERROR:'',I5)') IERR
      ELSE
          CALL SETABS (IFL,NRLAT,NRLONG,IEND)
      ENDIF
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
