      SUBROUTINE NEXTSEP ( I,J )
C
C**** *NEXTSEP*
C
C
C     PURPOSE.
C     --------
C         LOCATE THE NEXT GROUP BY FINDING THE NEXT
C         CHARACTER WHICH IS NOT 'CR' OR 'LF' OR 'SPACE'.
C                        'CR' OR 'LF' OR 'SPACE'
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR' .
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR' .
C
C         OUTPUT    : I - POSITION OF NEXT 'CR' OR 'LF' OR 'SPACE' CHARACTER
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTSEP(I,J)*
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
C*          1.   SCAN BULLETIN.
C                --------------
 100  CONTINUE
C
C     'CR' = 13 , 'LF' = 10 , 'SPACE' = 32.
C
      I=IABS(I)
      K = I
      DO 101 I=K,J
cpssep03         KAR = KCHAR(I) .AND. 127
         KAR = IAND(KCHAR(I),127)
         IF(KAR .EQ. 13 .OR. KAR .EQ. 10 .OR. KAR .EQ. 32) RETURN
  101 CONTINUE
C
      RETURN
      END
