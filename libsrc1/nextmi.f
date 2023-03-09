      SUBROUTINE NEXTMI(I,J,II)
C
C**** *NEXTMI*
C
C
C     PURPOSE.
C     --------
C
C         TO FIND NEXT MIMIMJMJ GROUP IN THE BULLETIN.
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT GROUP OF
C         ('TTAA' OR 'TTBB' OR 'TTCC' OR 'TTDD' ETC.)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTMI(I,J,II)*
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C
C         OUTPUT    : II- POSITION OF THE FIRST CHARACTER
C                         IN REQUIRED GROUP
C                         IF CHARACTER NOT FOUND II = 99999
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
      DIMENSION MIMJ(26)
C
C
      DATA MIMJ /65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     &           80,81,82,83,84,85,86,87,88,89,90/
C     ------------------------------------------------------------------
C*          1.   FIND NEXT MIMIMJMJ GROUP.
C                -------------------------
 100  CONTINUE
C
Crr
crr   II=9999
      II=99999
      K =IABS(I)
      M =IABS(I)
C
 101  CONTINUE
C
      CALL NEXSEP2(M,J,*1000)
      CALL NEXPRT2(M,J,*1000)
C
      IF(M.GE.J) RETURN
C
      K1= KCHAR(M  )
      K2= KCHAR(M+1)
      K3= KCHAR(M+2)
      K4= KCHAR(M+3)
C
      DO 102 N=1,26
C
      IF(K1.EQ.MIMJ(N).AND.K2.EQ.MIMJ(N))
     &   THEN
C
            DO 103 NN=1,26
C
            IF(K3.EQ.MIMJ(NN).AND.K4.EQ.MIMJ(NN))
     &         THEN
                  CALL PRESEP(M,K,*1000)
                  CALL PREPRT(M,K,*1000)
                  II= M+1
                  RETURN
               END IF
C
 103        CONTINUE
C
         END IF
C
 102  CONTINUE
C
       GO TO 101
C
 1000 CONTINUE
C
      RETURN
      END
