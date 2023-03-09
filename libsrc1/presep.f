      SUBROUTINE PRESEP ( I,J,*)
C
C**** *PRESEP*
C
C
C     PURPOSE.
C     --------
C
C         LOCATE THE PREVIOUS GROUP BY FINDING THE NEXT
C         CHARACTER WHICH IS 'CR' OR 'LF' OR 'SPACE'.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRESEP(I,J,*)
C
C                 INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR' .
C                             J - SCAN ENDS AT WORD 'J' OF 'KCHAR' .
C
C                 OUTPUT    : I - POSITION OF PREVIOUS 'CR' OR 'LF' OR 'S
C                                 IF NO CHARACTER FOUND THE CONTROL RETUR
C                                 ALTERNATIVE RETURN POINT *
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
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
C     ------------------------------------------------------------------
C
C*          1.   LOCATE THE PREVIOUS GROUP BY FINDING THE NEXT
C                ---------------------------------------------
C                CHARACTER WHICH IS 'CR' OR 'LF' OR 'SPACE'.
C                --------------------------------------------
 100  CONTINUE
C
C
C     'CR' = 13 , 'LF' = 10 , 'SPACE' = 32.
C
      I=IABS(I)
      K = I
      DO 101 I=K,J,-1
         IF(I .LE. J) RETURN 1
cpssep03         KAR = KCHAR(I) .AND. 127
         KAR = IAND(KCHAR(I),127)
         IF(KAR .EQ. 13 .OR. KAR .EQ. 10 .OR. KAR .EQ. 32) RETURN
  101 CONTINUE
C
      RETURN 1
      END
