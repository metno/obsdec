      SUBROUTINE NEXSEP2 ( I,J,*)
C
C**** *NEXSEP2*
C
C
C     PURPOSE.
C     --------
C
C         LOCATE THE NEXT GROUP BY FINDING THE NEXT
C         CHARACTER WHICH IS NOT 'CR' OR 'LF' OR 'SPACE'.
C         'CR' OR 'LF' OR 'SPACE'
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR' .
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR' .
C
C         OUTPUT    : I - POSITION OF NEXT 'CR' OR 'LF' OR 'SPACE' CHARACTER
C                         IF NO CHARACTER FOUND THE CONTROL RETURN TO
C                         ALTERNATIVE RETURN POINT *
cps This (that the control return to return point * if no character
cps is found) is the only difference between nexsep2.f and nextsep.f
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXSEP2(I,J,*)*
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
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
C     ------------------------------------------------------------------
C*          1.  SCAN BULLETIN.                     .
C                -------------
 100  CONTINUE
C
C
C     'CR' = 13 , 'LF' = 10 , 'SPACE' = 32.
C
      I=IABS(I)
      K = I
      DO 101 I=K,J
         IF(I .GE. J) RETURN 1
cpssep03         KAR = KCHAR(I) .AND. 127
         KAR = IAND(KCHAR(I),127)
         IF(KAR .EQ. 13 .OR. KAR .EQ. 10 .OR. KAR .EQ. 32) RETURN
  101 CONTINUE
C
      RETURN 1
      END
