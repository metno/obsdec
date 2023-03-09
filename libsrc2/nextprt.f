      SUBROUTINE NEXTPRT ( I,J )
C
C     PURPOSE.
C     --------
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT CHARACTER WHICH
C         IS NOT 'SOH', 'CR', 'LF, 'SPACE' OR 'GS'
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTPRT(I,J)*
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR'
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR'
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER.
cps                   if I < 0 on input, I is set to IABS(I) initially     
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
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
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

C     'SOH' = 1, 'LF' = 10, 'CR' = 13, SPACE = 32, 'GS' = 29

      I = IABS(I)
      K = I
      DO I=K,J
         KAR = IAND(KCHAR(I),127)
         IF (KAR.NE.1. AND .KAR.NE.10 .AND. KAR.NE.13.
     *        AND .KAR.NE.32 .AND. KAR.NE.29) RETURN
      END DO

C     CHARACTER NOT FOUND
      I = J + 1

      RETURN
      END
