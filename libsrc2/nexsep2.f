      SUBROUTINE NEXSEP2 ( I,J,*)
C
C     PURPOSE.
C     --------
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT 'CR' OR 'LF' OR 'SPACE'
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXSEP2(I,J,*)*
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR'
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR'
C
C         OUTPUT    : I - POSITION OF NEXT 'CR' OR 'LF' OR 'SPACE' CHARACTER.
C                         IF CHARACTER NOT FOUND THE CONTROL
C                         RETURNS TO ALTERNATIVE RETURN POINT *
cps This (that the control returns to return point * if no seperator
cps is found) is the only difference between nexsep2.f and nextsep.f
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
C          NONE.

      implicit none
      integer I,J
      integer k,kar

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'    ! kchar

C     ------------------------------------------------------------------
C*          1.   SCAN BULLETIN.
C                --------------

C     'CR' = 13, 'LF' = 10, 'SPACE' = 32

      I = IABS(I)
      K = I
      DO I=K,J
         KAR = IAND(KCHAR(I),127)
         IF (KAR.EQ.13. OR. KAR.EQ.10 .OR. KAR.EQ.32) RETURN
      END DO

C     CHARACTER NOT FOUND
      RETURN 1

      END
