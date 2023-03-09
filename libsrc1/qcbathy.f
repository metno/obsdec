      SUBROUTINE QCBATHY (IERR)
C
C**** *QCBATHY*
C
C       PURPOSE.
C      ----------
C
C          QUALITY CONTROL FOR BATHYS
C
C       INTERFACE.
C      ------------
C
C        CALL QCBATHY (IERR)
C
C        INPUT
C         ARRAY VALUES (COMWT) - BATHY REPORTS
C         L21  (COMWT)  -  LOGICAL FOR SURFACE REPORT
C         L22  (COMWT)  -  LOGICAL FOR SUB-SURFACE REPORT
C
C        OUTPUT
C         ARRAY VALUES (COMWT) - BATHY REPORTS WITH CONFIDENCE FLAGS
C                                               ADDED
C         IERR   -  ERROR CODE
C
C       METHOD.
C      ---------
C
C        CHECK PARAMETER VALUES AGAINST GROSS LIMITS;
C        CHECK CONSISTENCY OF WIND SPEED AND DIRECTION;
C
C       EXTERNALS.
C      ------------
C
C        CDDFF  -  CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C        CSLT   -  GROSS LIMIT CHECK FOR SURFACE PARAMETERS
C        DERITA -  INITIALISE TABLES AND CONFIDENCE FLAGS
C        SEASON -  DETERMINE RELEVANT SEASON
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
      MISS=NINT(RMISS)
C
C          1.1  POSITIONS OF PARAMETERS WITHIN HEADER
C
      IF(L21) THEN
           NMONTH=6
           NRLAT=10
           NRLONG=11
      ENDIF
      IF(L22) THEN
           NMONTH=3
           NRLAT=7
           NRLONG=8
      ENDIF
C
C          1.2  POSITIONS OF METEOROLOGICAL PARAMETERS
C
      IF(L21) THEN
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
      MONTH=NINT(VALUES(NMONTH,J))
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
      CALL SEASON (RLAT,RLONG,MONTH)
C
C          1.7   EXTRACT PARAMETER VALUES
C
      IF(.NOT.L21) GO TO 400
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
C             2.1   SURFACE WIND SPEED
C
      IF(FF.NE.RMISS) THEN
           CALL CSLT (IERR,FF,'FF',NFF,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''BATHY CSLT ERROR:'',I5,
     X                '' FF:'',F10.2)') IERR,FF
      ENDIF
C
C             2.2   SURFACE AIR TEMPERATURE
C
      IF(TT.NE.RMISS) THEN
           CALL CSLT (IERR,TT,'TT',NTT,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''BATHY CSLT ERROR:'',I5,
     X                '' TT:'',F10.2)') IERR,TT
      ENDIF
C
C             2.3   SEA SURFACE TEMPERATURE
C
      IF(TW.NE.RMISS) THEN
           CALL CSLT (IERR,TW,'TW',NTW,RLAT,RLONG)
           IF(IERR.NE.0) WRITE (*,'(1H ,''BATHY CSLT ERROR:'',I5,
     X                '' TW:'',F10.2)') IERR,TW
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
