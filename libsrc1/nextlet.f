      SUBROUTINE NEXTLET ( I,K )
C
C**** *NEXTLET*
C
C
C     PURPOSE.
C     --------
C
C         LOCATE FIRST WORD CONTAINING A LETTER IN ARRAY
C         'KCHAR' BETWEEN WORD 'I' AND WORD 'K' .
C
C         INPUT     : BULLETIN IN 'KCHAR' , 1 CHARACTER PER WORD.
C
C         OUTPUT    : I = REQUIRED LOCATION . I > K MEANS NO LETTER FOUND.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTLET(I,K)*
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
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
C     ------------------------------------------------------------------
C
C*          1.   FIND POINTER TO NEXT LETTER.
C                ----------------------------
 100  CONTINUE
C
C
      I = IABS(I)
      J = I
      DO 110 I=J,K
cpssep03         KAR = KCHAR(I) .AND. 127
         KAR = IAND(KCHAR(I),127)
         IF ( KAR .GE. 65 .AND. KAR .LE. 90 ) RETURN
  110 CONTINUE
C
      RETURN
      END
