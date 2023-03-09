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
C
C     MODIFICATIONS.
C     --------------
C
C
      implicit none

      integer ia,k,kl,mindic
      integer i,i2,j
      logical lodata,lodot,nomoredata

      DIMENSION IA(*)

C     ------------------------------------------------------------------
C
C*          1.   PRINT ARRAY 'KINT'.


      LODATA = .TRUE.           ! Print data (LODATA = 'LOgical DATA')
      LODOT  = .TRUE.           ! Print --- lines (LOgical DOT)
      NOMOREDATA = .FALSE.


      WRITE(*,10000)
10000 FORMAT(1H ,2X,'  DATA IN INTERMEDIATE FORMAT ( KINT ) ',/)


      DO I=K,KL,10
         I2 = I+9

         DO J=I,I2
            IF(IA(J) .EQ. MINDIC) CYCLE
            LODATA = .FALSE.
         END DO

         IF (LODATA) THEN
C     Check if there are more data
            DO J=I2,KL
               IF(IA(J) .EQ. MINDIC) CYCLE
               NOMOREDATA = .FALSE.
            END DO

            IF (NOMOREDATA) RETURN

            IF (LODOT) THEN
               WRITE(*,20000)
20000          FORMAT(1H ,3X,'---',/1H ,3X,'---')
               LODOT = .FALSE.
            END IF
            GO TO 101
         END IF

         LODOT = .TRUE.
         LODATA = .TRUE.

         WRITE(*,30000) I,(IA(J),J=I,I2)
30000    FORMAT(1H ,2X,I4,2X,10(I10,1X))

 101     CONTINUE
      END DO

      RETURN
      END
