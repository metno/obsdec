      SUBROUTINE PRTBULL ( I,M )
C
C**** *PRTBULL*
C
C
C     PURPOSE.
C     --------
C         PRINTS BULLETIN IN ARRAY 'KCHAR'
C
C         INPUT      : BULLETIN IN 'KCHAR' .
C                      I - PRINT STARTS AT CHARACTER I
C                      M - PRINT ENDS AT CHARACTER M .
C
C         OUTPUT     : BULLETIN IS PRINTED . ARRAY 'KCHAR' AND POINTERS
C                      UNCHANGED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTBULL(I,M)*
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
C         *CALL* *NEXTPRT(I,J)*
C         *CALL* *NEXTEND(I,J)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
CRR      DIMENSION LINE(80)
      CHARACTER*1 ILINE(80)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT BULLETIN.
C                ---------------
 100  CONTINUE
C
CTEST
      PRINT*,' PRTBULL - start of subr. I= ',I,' M= ',M
CTEST
      IP = I
      J = M
C
C*          1.1  SET OUTPUT LINE TO ALL SPACES .
C                -------------------------------
 110  CONTINUE
C
      K = 80
      DO 111 N=1,K
CRR             LINE(N) = 32
             ILINE(N) = CHAR(32)
  111 CONTINUE
C
C     LOCATE START AND END OF NEXT LINE OF CHARACTERS ( IF ANY ).
C
      CALL NEXTPRT ( IP,J )
      IF ( IP.GE.J ) RETURN
      JP = IP
      CALL NEXTEND ( JP,J )
      K = JP - IP
      IF(K.GT.80) K=80
C
C     INSERT IN OUTPUT LINE AND SUPPRESS PARITY BIT.
C
      DO 112 N =1,K
CRR             LINE(N) = KCHAR(IP).AND.127
             ILINE(N) = CHAR(KCHAR(IP))
             IP = IP + 1
  112 CONTINUE
C
      WRITE ( *,9900) (ILINE(N),N=1,K)
C
C     GET NEXT LINE
C
      GO TO 110
C
 9900 FORMAT (1H ,80A1)
C
      END
