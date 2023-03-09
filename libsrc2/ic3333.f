      SUBROUTINE IC3333(IQUADR,LAT,LONG,MINDIC,LAT2,LONG2)
C**** *IC3333*
C 
C    NAME     :  IC3333
C
C    FUNCTION :  DECODE LATITUDE AND LONGITUDE GIVEN IN THE FORM
C                99LALALA QCL0L0L0L0
C
C    INPUT    :  IQUADR   THE QUADRANT OF THE GLOBE (QC)
C             :  LAT      LATITUDE IN TENTHS OF DEGREE
C             :  LONG     LONGITUDE IN TENTHS OF DEGREE
C             :  MINDIC   MISSING DATA VALUE
C
C
C    OUTPUT   :  LAT2   LATITUDE IN HUNDREDTHS OF DEGREE
C                       SOUTHERN LATITUDE NEGATIVE
C             :  LONG2: LONGITUDE IN HUNDREDTHS OF DEGREE
C                       WESTERN LONGITUDE NEGATIVE
C
C                LAT2 AND LONG2 ARE SET TO MISSING DATA VALUE IF
C                ANY ERRORS FOUND IN LAT,LONG OR QUADRANT
C
      IMPLICIT NONE
      INTEGER IQUADR,LAT,LONG,MINDIC,LAT2,LONG2
      INTEGER LATSIGN(4),LONSIGN(4),IQ
C
C     The arrays latsign and lonsign are used to determine if
C     lat. and long. are negative or positive
      DATA LATSIGN/ 1,-1,-1, 1/
      DATA LONSIGN/ 1, 1,-1,-1/
C
C     Set LAT2 and LONG2 to missing data value
C
      LAT2=MINDIC
      LONG2=MINDIC
C
C     Check that the quadrant is correct
C
      IF(IQUADR .NE. 1 .AND. IQUADR .NE. 3 .AND. IQUADR .NE.
     1     5 .AND. IQUADR .NE. 7) RETURN
C
C     Check that the latitude and longitude are sensible
C
      IF(LAT .LT. 0 .OR. LAT .GT. 900) RETURN
      IF(LONG .LT. 0 .OR. LONG .GT. 1800) RETURN
C
      IQ=(IQUADR+1)/2
C
      LAT2=10*LAT*LATSIGN(IQ)
      LONG2=10*LONG*LONSIGN(IQ)
C
      RETURN
      END
