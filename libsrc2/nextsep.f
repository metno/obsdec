      SUBROUTINE NEXTSEP ( I,J )
C
C     PURPOSE.
C     --------
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT 'CR' OR 'LF' OR 'SPACE'
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTSEP(I,J)*
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR' .
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR' .
C
C         OUTPUT    : I - POSITION OF NEXT 'CR' OR 'LF' OR 'SPACE' CHARACTER
C                     I > J INDICATES NO CHARACTER FOUND.
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*        15/09/87.
C
C     MODIFICATIONS.
C     --------------
C
C          PS nov 2006 Returns I = J+1 if character(s) not found

      implicit none
      integer I,J
      integer k,kar

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'    ! kchar

C     ------------------------------------------------------------------
C*          1.   SCAN BULLETIN.
C                --------------

C     'CR' = 13 , 'LF' = 10 , 'SPACE' = 32.

      I = IABS(I)
      K = I
      DO I=K,J
         KAR = IAND(KCHAR(I),127)
         IF(KAR .EQ. 13 .OR. KAR .EQ. 10 .OR. KAR .EQ. 32) RETURN
      END DO

C     CHARACTER NOT FOUND
      I = J + 1

      RETURN
      END
