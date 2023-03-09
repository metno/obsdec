      SUBROUTINE PRTKINT(IA,K,KL,MINDIC)
C
C**** *PRTKINT*
C
C
C     PURPOSE.
C     --------
C         PRINTS THE INTERMEDIATE FORMAT ARRAY (KINT)
C         OF DECODING DATA (PHASE II).
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTKINT(IA,K,J,MINDIC)*
C
C         INPUT     : IA     - THE 'KINT' ARRAY
C                     K      - PRINT STARTS AT WORD I.
C                     KL      - PRINT STOPS AT WORD J .
C                     MINDIC - MISSING VALUE INDICATOR
C
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
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
cpsjul07      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
      IMPLICIT LOGICAL(O,G,L), CHARACTER*8(C,H,Y)
C
      DIMENSION IA(1)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT ARRAY 'KINT'.
C                -------------------
 100  CONTINUE
C
C
      LODATA = .TRUE.
      LODOT = .TRUE.
C
C
      WRITE(*,10000)
10000 FORMAT(1H ,2X,'  DATA IN INTERMEDIATE FORMAT ( KINT ) ',/)
C
C
         DO 101 I=K,KL,10
         I2 = I+9
C
            DO 102 J=I,I2
            IF(IA(J) .EQ. MINDIC) GO TO 102
            LODATA = .FALSE.
102         CONTINUE
C
         IF(LODATA) THEN
C
C                      CHECK IF THERE ARE MORE DATA
C
                       DO 103 J=I2,KL
                       IF(IA(J) .EQ. MINDIC) GO TO 103
                       GO TO 104
103                    CONTINUE
                       RETURN
C
104                    CONTINUE
C
                       IF(LODOT) THEN
                                    WRITE(*,20000)
20000                               FORMAT(1H ,3X,'---',/1H ,3X,'---')
                                    LODOT = .FALSE.
                                 END IF
                       GO TO 101
                    END IF
C
         LODOT = .TRUE.
         LODATA = .TRUE.
C
         WRITE(*,30000) I,(IA(J),J=I,I2)
30000    FORMAT(1H ,2X,I4,2X,10(I10,1X))
C
C
101      CONTINUE
C
      RETURN
      END
