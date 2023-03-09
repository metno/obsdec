      SUBROUTINE NEXTVAL ( I,N,K )
C
C**** *NEXTVAL*
C
C
C     PURPOSE.
C     --------
C
C         LOCATE THE FIRST WORD CONTAINING THE VALUE 'N' IN
C         ARRAY 'KCHAR' BETWEEN WORD 'I' AND WORD 'K' .
C
C         INPUT    : 'KCHAR' CONTAINS ONE BULLETIN , ONE CHARACTER PER
C                    WORD.
C
C         OUTPUT   : I = REQUIRED LOCATION . I > K MEANS VALUE NOT FOUND.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTVAL(I,N,K)*
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
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
C     ------------------------------------------------------------------
C
C*          1.   EXTRACT VALUE.
C                --------------
 100  CONTINUE
C
      I = IABS(I)
      J = I
      DO 101 I=J,K
cpssep03         KAR = KCHAR(I) .AND. 127
         KAR = IAND(KCHAR(I),127)
         IF ( KAR .EQ. N ) RETURN
  101 CONTINUE
C
      RETURN
      END
