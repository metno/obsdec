      SUBROUTINE NEXPRT2(I,J,*)
C
C**** *NEXPRT2*
C
C
C     PURPOSE.
C     --------
C
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT CHARACTER WHICH
C         IS NOT 'SOH' , 'CR' , 'LF' , 'SPACE' OR 'GS' .
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER.
C                         IF CHARACTER NOT FOUND THE CONROL
C                         RETURNS TO ALTERNATIVE RETURN POINT *
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXPRT2(I,J,*)*
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
C
C*          1.   SCAN BULLETIN.                    .
C                --------------
 100  CONTINUE
C
C     'SOH' = 1 , 'LF' = 10 , 'CR' = 13 , SPACE = 32 , 'GS' = 29.
C
      I = IABS(I)
      K = I
      DO 101 I=K,J
         IF(I .GE. J) RETURN 1
cpssep03         KAR = KCHAR(I).AND.127
         KAR = IAND(KCHAR(I),127)
         IF ( KAR.NE.1.AND.KAR.NE.10.AND.KAR.NE.13.
     C            AND.KAR.NE.32.AND.KAR.NE.29) RETURN
  101 CONTINUE
C
      RETURN 1
      END
