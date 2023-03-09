      SUBROUTINE CSLT (IERR,P,YP,IP,RLAT,RLONG)
C
C**** *CSLT*
C
C      PURPOSE.
C     ---------
C
C             CHECK SURFACE PARAMETER FOR GROSS LIMIT CHECK
C
C
C      INTERFACE.
C     ------------
C
C         CALL CSLT (IERR,P,YP,IP,RLAT,RLONG)
C
C      INPUT
C        P      -  VALUE OF PARAMETER
C        YP     -  NAME OF PARAMETER
C        IP     -  POSITION OF CONFIDENCE FLAG FOR PARAMETER
C        RLAT   -  LATITUDE VALUE FOR REPORT
C        RLONG  -  LONGITUDE VALUE FOR REPORT
C
C      OUTPUT
C        IERR   -  ERROR CODE
C        IFLAG (CONF)  -  CONFIDENCE FLAG FOR PARAMETER UPDATED
C
C      METHOD.
C     ---------
C
C        GET GROSS LIMITS FROM CORRECT TABLE
C        UPDATE CONFIDENCE ACCORDINGLY
C
C      EXTERNALS.
C     ------------
C
C         GETLIM  -  GET GROSS LIMITS FROM TABLE
C         SETSF   -  UPDATE CONFIDENCE FLAG DEPENDING UPON SEVERITY
C                      OF FAILURE
C
C      REFERENCE.
C     ------------
C
C         GUIDE ON THE GLOBAL DATA PROCSEEING SYSTEM WMO N305 1982
C           CHAPTER 6, QUALITY CONTROL PROCEDURES
C
C      AUTHOR.
C     ---------
C
C         B. NORRIS,  ECMWF,  JANUARY 1989.
C
C      MODIFICATIONS.
C     -----------------
C
C         NONE
C
      INCLUDE 'paramq.f'
      INCLUDE 'const.f'
      INCLUDE 'ctsf.f'
C
      DIMENSION VLIM(NMM)
      CHARACTER*(*) YP
      IERR=0
C
C    --------------------------------------------------------------------
C
C                 1.  GET GROSS LIMITS FROM APPROPRIATE TABLE
C                -----------------------------------------------
C
  100 CONTINUE
C
C        1.1  WIND SPEED
C
      IF(YP.EQ.'FF') THEN
           CALL GETLIM (RLAT,RLONG,SFF,VLIM)
C
C        1.2  TEMPERATURE - CONVERT TO DEG K
C
      ELSEIF(YP.EQ.'TT') THEN
           CALL GETLIM (RLAT,RLONG,STT,VLIM)
           DO 122 I=1,NMM
           VLIM(I)=VLIM(I)+RABS
  122      CONTINUE
C
C         1.3  DEW POINT - CONVERT TO DEG K
C
      ELSEIF(YP.EQ.'TD') THEN
           CALL GETLIM (RLAT,RLONG,STD,VLIM)
           DO 132 I=1,NMM
           VLIM(I)=VLIM(I)+RABS
  132      CONTINUE
C
C         1.4  STATION LEVEL PRESSURE
C
      ELSEIF(YP.EQ.'PS') THEN
           CALL GETLIM (RLAT,RLONG,SPS,VLIM)
C
C         1.5  MEAN SEA LEVEL PRESSURE
C
      ELSEIF(YP.EQ.'PPP') THEN
           CALL GETLIM (RLAT,RLONG,SPPP,VLIM)
C
C         1.6  PRESSURE TENDENCY
C
      ELSEIF(YP.EQ.'PP') THEN
           CALL GETLIM (RLAT,RLONG,SPP,VLIM)
C
C         1.7  SEA SURFACE TEMPERATURE - CONVERT TO DEG K
C
      ELSEIF(YP.EQ.'TW') THEN
           CALL GETLIM (RLAT,RLONG,STW,VLIM)
           DO 172 I=1,NMM
           VLIM(I)=VLIM(I)+RABS
  172      CONTINUE
      ELSE
C
C        1.8  PARAMETER NOT CATERED FOR
C
           IERR=2001
           GO TO 300
      ENDIF
C
C    -------------------------------------------------------------------
C
C              2.  SET APPROPRIATE FLAGS
C             ---------------------------
C
  200 CONTINUE
      CALL SETSF (P,IP,VLIM)
C
C
C    ------------------------------------------------------------------
C
C                   3.    EXIT
C                 --------------
C
  300 CONTINUE
      RETURN
      END
