      SUBROUTINE ISTIME(CTIME,ITM,IRET)
C
C**** *ISTIME*
C
C
C     PURPOSE.
C     --------
C         CALCULATE TIME IN MINUTES SINCE 1/1/1978,
C         GIVEN INPUT AS DD-MMM-YYYY HH:MM:SS.CC
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ISTIME(CTIME,ITM,IRET)*
C
C          INPUT :  CTIME (ASCII TIME)
C
C          OUTPUT:  ITM  ( TIME IN MINUTES SINCE 1/1/1978.)
C                   IRET ( RETURN CODE)            
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       21/10/89.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER*(*) CTIME
      CHARACTER*3 CM(12)
C
      DIMENSION IDM(13)
C
      DATA IDM/0,31,28,31,30,31,30,31,31,30,31,30,31/  
      DATA CM /'Jan','Feb','Mar','Apr','May','Jun',
     1         'Jul','Aug','Sep','Oct','Nov','Dec'/
C
C     ------------------------------------------------------------------
C*          1.   SET MONTH.
C                ----------
 100  CONTINUE
C
      idays=0
c
      DO 101 I=1,12
       IF(CTIME(4:6).EQ.CM(I)) THEN
          IM=I
          IM=IM+1
          GO TO  110
       END IF
 101  CONTINUE 
C  
C
C*          1.1  SET MINUTES.
C                ------------
 110  CONTINUE
C
      READ (CTIME,'(I2,5X,I4,1X,I2,1X,I2)') ID,IY,IH,IMIN
C
      DO 111 I=1978,IY-1
       IDAYS=IDAYS+365
       IF(MOD(I,4).EQ.0) IDAYS=IDAYS+1
 111  CONTINUE
C
      DO 112 I=1,IM-1
       IDAYS=IDAYS+IDM(I) 
       IF(I.EQ.3) THEN
          IF(MOD(IY,4).EQ.0) IDAYS=IDAYS+1
       END IF
 112  CONTINUE
C
      IDAYS=IDAYS+ID-1
C
      ITM=IDAYS*1440+IH*60+IMIN
C
      RETURN
      END 
