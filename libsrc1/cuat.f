      SUBROUTINE CUAT (IERR,P,YP,IP,RLAT,RLONG,PP1,PP2,Z,
     X                                        IPP1,IPP2,IZ)
C
C**** *CUAT*
C
C      PURPOSE.
C     ----------
C
C             CHECK UPPER AIR PARAMETER FOR GROSS LIMIT CHECK
C
C      INTERFACE.
C     ------------
C
C         CALL CUAT (IERR,P,YP,IP,RLAT,RLONG,PP1,PP2,Z,IPP1,IPP2,IZ)
C
C        INPUT
C         P      -  VALUE OF PARAMETER
C         YP     -  NAME OF PARAMETER
C         IP     -  POSITION OF CONFIDENCE FLAG FOR PARAMETER
C         RLAT   -  LATITUDE VALUE FOR REPORT
C         RLONG  -  LONGITUDE VALUE FOR REPORT
C         PP1    -  PRESSURE LEVEL OR BASE OF LAYER
C         PP2    -  TOP OF LAYER
C         Z      -  HEIGHT LEVEL
C         IPP1   -  POSITION OF CONFIDENCE FLAG FOR PP1
C         IPP2   -  POSITION OF CONFIDENCE FLAG FOR PP2
C         IZ     -  POSITION OF CONFIDENCE FLAG FOR Z
C
C        OUTPUT
C         IERR   -  ERROR CODE
C         IFLAG (CONF)  -  CONFIDENCE FLAGS UPDATED
C
C      METHOD.
C     ---------
C
C         GET GROSS LIMITS FROM CORRECT TABLE
C         UPDATE CONFIDENCE FLAGS ACCORDINGLY
C
C       EXTERNALS.
C      ------------
C
C         GUALIM - GET GROSS LIMITS FROM TABLE
C         SETGRS - UPDATE CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C         SETINT - UPDATE CONFIDENCE FLAGS FOR INT CONSISTENCY CHECK
C
C       REFERENCE.
C      ------------
C
C         GUIDE ON THE GLOBAL DATA PROCESSING SYSTEM WMO N305 1982
C           CHAPTER 6, QUALITY CONTROL PROCEDURES
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
      INCLUDE 'paramq.f'
      INCLUDE 'const.f'
      INCLUDE 'ctua.f'
      INCLUDE 'conf.f'
C
      DIMENSION VLIM(NMM)
      CHARACTER*(*) YP
      LOGICAL LAYER,LSUM
      IERR=0
C
C    --------------------------------------------------------------------
C
C            1.  GET GROSS LIMITS FROM APPROPRIATE TABLE
C           ----------------------------------------------
C
  100 CONTINUE
C
C         1.1  GEOPOTENTIAL
C
      IF(YP.EQ.'Z') THEN
           LAYER=.FALSE.
           CALL GUALIM (IERR,UPZ,UZ,RLAT,RLONG,PP1,PP2,Z,
     X                  LAYER,LSUM,VLIM)
C
C           1.2  TEMPERATURE - CONVERT TO DEG K
C
      ELSEIF(YP.EQ.'TT') THEN
           LAYER=.FALSE.
           CALL GUALIM (IERR,UPZ,UTT,RLAT,RLONG,PP1,PP2,Z,
     X                  LAYER,LSUM,VLIM)
           DO 122 I=1,NMM
           VLIM(I)=VLIM(I)+RABS
  122      CONTINUE
C
C           1.3  WIND SPEED
C
      ELSEIF(YP.EQ.'FF') THEN
           LAYER=.FALSE.
           CALL GUALIM (IERR,UPZ,UFF,RLAT,RLONG,PP1,PP2,Z,
     X                  LAYER,LSUM,VLIM)
C
C           1.4  LAYER MEAN TEMPERATURE - CONVERT TO DEG K
C
      ELSEIF(YP.EQ.'TM') THEN
           LAYER=.TRUE.
           LSUM=.FALSE.
           CALL GUALIM (IERR,UPZ,UTT,RLAT,RLONG,PP1,PP2,Z,
     X                  LAYER,LSUM,VLIM)
           DO 142 I=1,NMM
           VLIM(I)=VLIM(I)+RABS
  142      CONTINUE
C
C           1.5  THICKNESS
C
      ELSEIF(YP.EQ.'TL') THEN
           LAYER=.TRUE.
           LSUM=.TRUE.
           CALL GUALIM (IERR,UPZ,UTL,RLAT,RLONG,PP1,PP2,Z,
     X                  LAYER,LSUM,VLIM)
C
C           1.6  PRECIPITABLE WATER
C
      ELSEIF(YP.EQ.'WL') THEN
           LAYER=.TRUE.
           LSUM=.TRUE.
           CALL GUALIM (IERR,UPZ,UWL,RLAT,RLONG,PP1,PP2,Z,
     X                  LAYER,LSUM,VLIM)
      ELSE
C
C          1.7   PARAMETER NOT CATERED FOR
C
           IERR=2401
      ENDIF
      IF(IERR.NE.0) GO TO 300
C
C    --------------------------------------------------------------------
C
C                2.  SET APPROPRIATE FLAGS
C               ---------------------------
C
  200 CONTINUE
      IF(P.LT.VLIM(1)) THEN
C
C          2.1  P<MIN2
C
           CALL SETINT (ICBO(2,2),IPP1,IPP2,IZ,IEND)
           CALL SETGRS (IP,2)
      ELSEIF(P.GE.VLIM(1).AND.P.LT.VLIM(2)) THEN
C
C          2.2  MIN2<P<MIN1
C
           CALL SETINT (ICBO(3,2),IPP1,IPP2,IZ,IEND)
           CALL SETGRS (IP,3)
      ELSEIF(P.GE.VLIM(2).AND.P.LE.VLIM(3)) THEN
C
C          2.3  MIN1<P<MAX1
C
           CALL SETINT (ICBO(4,2),IPP1,IPP2,IZ,IEND)
           CALL SETGRS (IP,4)
      ELSEIF(P.GT.VLIM(3).AND.P.LE.VLIM(4)) THEN
C
C          2.4  MAX1<P<MAX2
C
           CALL SETINT (ICBO(3,2),IPP1,IPP2,IZ,IEND)
           CALL SETGRS (IP,3)
      ELSEIF(P.GT.VLIM(4)) THEN
C
C          2.5  P>MAX2
C
           CALL SETINT (ICBO(2,2),IPP1,IPP2,IZ,IEND)
           CALL SETGRS (IP,2)
      ELSE
      ENDIF
C
C    ----------------------------------------------------------------------
C
C                  3.   EXIT
C                 ------------
C
  300 CONTINUE
      RETURN
      END
