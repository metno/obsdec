      SUBROUTINE PRTKDEC(IA,K,J,MINDIC)
C
C**** *PRTKDEC*
C
C
C     PURPOSE.
C     --------
C         PRINTS THE DECODED FORMAT ARRAY (KDEC)
C         OF DECODING DATA (PHASE II).
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTKEDEC(IA,K,J,MINDIC)*
C
C         INPUT     : IA     - THE 'KDEC' ARRAY
C                      K      - PRINT STARTS AT WORD I.
C                      J      - PRINT STOPS AT WORD J .
C                      MINDIC - MISSING VALUE INDICATOR
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
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
cpsjul07      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
      IMPLICIT LOGICAL(O,G,L), CHARACTER*8(C,H,Y)
C
C
      DIMENSION IA(1)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT ARRAY 'KDEC'.
C                -------------------
 100  CONTINUE
C
C
C
      LODATA = .TRUE.
      LODOT = .TRUE.
C
      WRITE(*,10000)
10000 FORMAT(1H ,2X,'  DATA IN DECODED FORMAT ( KDEC ) ',/)
C
C
         DO 101 I=K,J,10
         I2 = I+9
C
            DO 102 JJ=I,I2
            IF(IA(JJ) .EQ. MINDIC) GO TO 102
            LODATA = .FALSE.
102         CONTINUE
C
         IF(LODATA) THEN
C
C                      CHECK IF THERE ARE MORE DATA
C
                       DO 103 JJ=I2,J
                       IF(IA(JJ) .EQ. MINDIC) GO TO 103
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
         IF(I .EQ. 1)  WRITE(*,30000) I,(IA(IK),IK=I,I2)
         IF(I .EQ. 11) WRITE(*,40000) I,(IA(IK),IK=I,I2)
         IF(I .GT. 20) WRITE(*,50000) I,(IA(IK),IK=I,I2)
30000    FORMAT(1H ,2X,I4,4X,6(I10,1X),6X,A4,1X,3(I10,1X))
40000    FORMAT(1H ,2X,I4,4X,2(I10,1X),O10,1X,I10,1X,O10,1X,2(I10,1X),
     C          2(I10,1X),I10)
50000    FORMAT(1H ,2X,I4,4X,10(I10,1X))
C
101      CONTINUE
C
C
C
C
C
      RETURN
      END
