      SUBROUTINE SETSF (P,IP,VLIM)
C
C**** *SETSF*
C
C      PURPOSE.
C     ---------
C
C           SET CONFIDENCE FLAG FOR GROSS LIMIT CHECK DEPENDING
C                 UPON SEVERITY OF FAILURE
C
C
C      INTERFACE.
C     ------------
C
C         CALL SETSF (P,IP,VLIM)
C
C      INPUT
C        P      -  VALUE OF PARAMETER
C        IP     -  POSITION OF CONFIDENCE FLAG FOR PARAMETER
C        VLIM   -  GROSS LIMITS
C
C      OUTPUT
C        IFLAG (CONF)  -  CONFIDENCE FLAG FOR PARAMETER UPDATED
C
C      METHOD.
C     ---------
C
C        DETERMINE WHERE PARAMETER VALUE FALLS WITHIN GROSS LIMITS
C        UPDATE CONFIDENCE ACCORDINGLY
C
C      EXTERNALS.
C     ------------
C
C         SETGRS  -  UPDATE CONFIDENCE FLAG FOR GROSS LIMIT CHECK
C
C      REFERENCE.
C     ------------
C
C         GUIDE ON THE GLOBAL DATA PROCESSING SYSTEM WMO N305 1982
C           CHAPTER 6, QUALITY CONTROL PROCEDURES
C
C      AUTHOR.
C     ---------
C
C         B. NORRIS,  ECMWF,  FEBRUARY 1989.
C
C      MODIFICATIONS.
C     -----------------
C
C         NONE
C
      INCLUDE 'paramq.f'
C
      DIMENSION VLIM(NMM)
C
C    -------------------------------------------------------------------
C
C              1.  SET APPROPRIATE FLAGS
C             ---------------------------
C
  100 CONTINUE
      IF(P.LT.VLIM(1)) THEN
C
C          1.1  P<MIN2
C
           CALL SETGRS (IP,2)
      ELSEIF(P.GE.VLIM(1).AND.P.LT.VLIM(2)) THEN
C
C          1.2  MIN2<P<MIN1
C
           CALL SETGRS (IP,3)
      ELSEIF(P.GE.VLIM(2).AND.P.LE.VLIM(3)) THEN
C
C          1.3  MIN1<P<MAX1
C
           CALL SETGRS (IP,4)
      ELSEIF(P.GT.VLIM(3).AND.P.LE.VLIM(4)) THEN
C
C          1.4  MAX1<P<MAX2
C
           CALL SETGRS (IP,3)
      ELSEIF(P.GT.VLIM(4)) THEN
C
C          1.5  P>MAX2
C
           CALL SETGRS (IP,2)
      ELSE
      ENDIF
C
C    ------------------------------------------------------------------
C
C                   2.    EXIT
C                 --------------
C
  200 CONTINUE
      RETURN
      END
