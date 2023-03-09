      SUBROUTINE EXTGRP ( I,N1,N2,N3,N4,N5,N,IRET )
C
C**** *EXTGRP*
C
C
C     PURPOSE.
C     --------
C         CONVERT GROUP IN THE REPORT
C         TO INTEGERS IN ARRAY 'KINT' .
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXTGRP ( I,N1,N2,N3,N4,N5,N,IRET )
C
C                    I - CONVERSION OF GROUP STARTS IN WORD I
C                        OF ARRAY 'KCHAR' .
C
C                    N1 - NUMBER OF CHARACTERS CONVERTED TO FIRST INTEGER
C                    N2 -    "    "     "          "     " SECOND   "
C                    N3 -    "    "     "          "     "  THIRD   "
C                    N4 -    "    "     "          "     "  FOURTH  "
C                    N5 =    "    "     "          "     "  FIFTH   "
C
C
C                    N - INDEX TO ARRAY 'KINT' , WHERE THE INSERTION OF
C                        INTEGER VALUES STARTS.
C                        VALUES ARE INSERTED IN CONSECUTIVE WORDS.
C
C                    IRET = 0 , ALL CONVERSIONS PERFORMED OK.
C                         = N , ERROR FOUND IN N TH INTEGER CONVERSION.
C
C                    I - POINTS TO CHARACTER AFTER LAST ONE EXTRACTED ,
C                        UNLESS A 'SEPARATOR' (= 'SPACE' , 'LINE FEED',
C                        'CARRIAGE RETURN') IS ENCOUNTERED IN FIGURES
C                        BEING EXTRACTED . IN THIS CASE I IS MADE NEGATIVE
C                        WITH THE ABSOLUTE VALUE OF 'I' POINTING TO THE
C                        'SEPARATOR' FOUND. IN THIS CASE ALSO THE WHOLE
C                        GROUP IS DELETED FROM 'KINT'.
C
C                        INTEGER VALUES EXTRACTED PUT IN WORDS N,N+1 ETC
C                        OF ARRAY 'KINT' .
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
C         *CALL* *EXTINT(I,N,K)*
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
      DIMENSION NN(5)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT GROUP OF CHARACTERS.
C                ----------------------------
 100  CONTINUE
C
      NN(1) = N1
      NN(2) = N2
      NN(3) = N3
      NN(4) = N4
      NN(5) = N5
C
      IRET = 0
C
      K = N
C
C
C
C*          1.1  CONVERT GROUP .
C                ---------------
 110  CONTINUE
C
      DO 111 J=1,5
         IF ( NN(J).EQ.0 ) RETURN
         CALL EXTINT ( I,NN(J),K )
         IF ( I.LT.0 ) THEN
                            IRET = J
                               DO 50 JJ=1,5
                               IF(NN(JJ) .EQ. 0) RETURN
                               KINT(N+JJ-1) = MINDIC
50                             CONTINUE
                            RETURN
                       END IF
         K = K + 1
  111 CONTINUE
C
      RETURN
      END
