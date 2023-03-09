      SUBROUTINE NEXPRT2(I,J,*)
C
C     PURPOSE.
C     --------
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT CHARACTER WHICH
C         IS NOT 'SOH', 'CR', 'LF, 'SPACE' OR 'GS' or '='
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXPRT2(I,J,*)*
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR'
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR'
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER.
C                         IF CHARACTER NOT FOUND THE CONTROL
C                         RETURNS TO ALTERNATIVE RETURN POINT *
cps This (that the control returns to return point * if no seperator
cps is found) is the only difference between nexprt2.f and nextprt.f
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
C          PS Jan 2007 Removed '=' as character searched for

      implicit none
      integer I,J
      integer k,kar

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'    ! kchar

C     ------------------------------------------------------------------
C*          1.   SCAN BULLETIN.
C                --------------

C     'SOH' = 1, 'LF' = 10, 'CR' = 13, SPACE = 32, 'GS' = 29, '=' = 61

      I = IABS(I)
      K = I
      DO I=K,J
         KAR = IAND(KCHAR(I),127)
         IF (KAR.NE.1. AND .KAR.NE.10 .AND. KAR.NE.13 .and. kar.ne.61
     *        .AND .KAR.NE.32 .AND. KAR.NE.29) RETURN
      END DO

C     CHARACTER NOT FOUND
      RETURN 1

      END
