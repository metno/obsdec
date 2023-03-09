      SUBROUTINE NEXTEQ ( I,J )
C
C     PURPOSE.
C     --------
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT '=' CHARACTER.
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR'
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR'
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER.
C                     I > J INDICATES NO CHARACTER FOUND.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTEQ(I,J)*
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
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

C     '=' IS 61 .

      I = IABS(I)
      K = I
      DO I=K,J
         KAR = IAND(KCHAR(I),127)
         IF (KAR .EQ. 61) RETURN
      END DO

C     CHARACTER NOT FOUND
      I = J + 1

      RETURN
      END
