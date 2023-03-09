      SUBROUTINE PRTBULL ( IS,IE )
C
C**** *PRTBULL*
C
C
C     PURPOSE.
C     --------
C         PRINTS BULLETIN IN ARRAY 'KCHAR'
C
C         INPUT      : BULLETIN IN 'KCHAR' .
C                      IS - PRINT STARTS AT CHARACTER IS
C                      IE - PRINT ENDS AT CHARACTER IE .
C
C         OUTPUT     : BULLETIN IS PRINTED . ARRAY 'KCHAR' AND POINTERS
C                      UNCHANGED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTBULL(IS,IE)*
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
      implicit none
      integer is,ie
      integer ip,jp,n1,line_length

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'   ! kchar
C
      CHARACTER*1 CLINE(80)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT BULLETIN.
C                ---------------
C
CTEST
      PRINT*,' PRTBULL - start of subr. IS= ',IS,' IE= ',IE
CTEST
      IP = IS
 110  CONTINUE
C
C     Set output line to all spaces .
      DO N1=1,80
         CLINE(N1) = CHAR(32)
      END DO
C
C     LOCATE START AND END OF NEXT LINE OF CHARACTERS (IF ANY).
C
      CALL NEXTPRT ( IP,IE )
      IF ( IP.GE.IE ) RETURN
      JP = IP
      CALL NEXTEND ( JP,IE )
      LINE_LENGTH = JP - IP
      IF (LINE_LENGTH.GT.80) LINE_LENGTH=80
C
C     INSERT OUTPUT LINE
C
      DO N1=1,LINE_LENGTH
         CLINE(N1) = CHAR(KCHAR(IP))
         IP = IP + 1
      END DO
C
      WRITE (*,9900) (CLINE(N1),N1=1,LINE_LENGTH)
C
C     GET NEXT LINE
C
      GO TO 110
C
 9900 FORMAT (1H ,80A1)
C
      END
