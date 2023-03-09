      SUBROUTINE NEXTEQ ( I,J )
C
C**** *NEXTEQ*
C
C
C     PURPOSE.
C     --------
C
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT '=' CHARACTER.
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C         OUTPUT    : I - POSITION OF REQUIRED CHARACTER. I > J INDICATES
C                         CHARACTER NOT FOUND.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTEQ(I,J)*
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
C*          1.   SCANE BULLETIN FOR NEXT '=' SIGN.
C                ---------------------------------
 100  CONTINUE
C
C     '=' IS 61 .
C
      I = IABS(I)
      K = I
      DO 101 I=K,J
cpssep03         KAR = KCHAR(I) .AND. 127
         KAR = IAND(KCHAR(I),127)
         IF(KAR .EQ. 61) RETURN
  101 CONTINUE
C
      I=J
      RETURN
      END
