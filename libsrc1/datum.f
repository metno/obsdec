      SUBROUTINE DATUM(IDD,IMM,IYY)
C
C**** *DATUM*
C
C
C     PURPOSE.
C     --------
C         DEFINE PROPER MONTH AND YEAR IF DAY IS DEFINED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DATUM(IDD,IMM,IYY)*
C
C                       IDD - DAY
C                       IMM - MONTH
C                       IYY - YEAR
cpsapr98   IYY changed to 4 characters instead of 2
C
C     METHOD.
C     -------
C
Cps this line is nonsence IF CURRENT MONTH IS JANUARY YEAR BECOMS PREVIOUS ONE.
C          IF IDD IS LESS OR EQUALL THAN CURRENT DAY IT IS FROM CURRENT MONTH
C          AND YEAR.
cps        Unless the day-difference is more than 16; then IDD is considered
cps        to be from previous month. If this is december: YEAR becomes
cps        previous year.
C          IF IDD IS GREATER THAN CURRENT DAY DAY IS CONSIDERED TO BE FROM
C          FUTURE.
C
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *ASCTIM(YDTIME)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER*3 YMONTH(12)
      character*23 ydtime
C
      DATA YMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     1            'Aug','Sep','Oct','Nov','Dec'/
C
C     ------------------------------------------------------------------
C
C*          1.   GET DATE FROM THE SYSTEM.
C                -------------------------
 100  CONTINUE
C
      CALL ASCTIM(YDTIME)
cpsapr98      READ(YDTIME(10:11),'(I2)') IYEAR
      READ(YDTIME(8:11),'(I4)') IYEAR
      READ(YDTIME(1:2),'(I2)') IDAY
C
      DO 101 I=1,12
       IF(YDTIME(4:6).EQ.YMONTH(I)) IMONTH=I
 101  CONTINUE
C
C*          1.1  DEFINE MONTH AND YEAR
C                ---------------------
 110  CONTINUE
C
      idiff=iday-idd
cc      print*,'idiff=iday-idd ',imonth,iday,idd,idiff
c same day
      if(idiff.eq.0) then
         IMM=IMONTH
         IYY=IYEAR
         RETURN
      END IF
c change of month
      if(idiff.lt.0.and.iabs(idiff).ge.16) then
        IF(IMONTH.EQ.1) THEN
            IMM=12
            IYY=IYEAR-1
            RETURN
         END IF
         IMM=IMONTH-1
         IYY=IYEAR
         RETURN
      END IF
c obs. day less than current day by more than 1.
c Test for too old date done in subr. isfile
      if(idiff.gt.0.and.iabs(idiff).le.27) then
         IMM=IMONTH
         IYY=IYEAR
         RETURN
      END IF
c obs. day greater than current day by 1 i.e.date for future. 
c Test on no. of minutes in the future done in subr. isfile
      if(idiff.lt.0.and.iabs(idiff).le.1) then
         IMM=IMONTH
         IYY=IYEAR
         RETURN
      END IF
c obs. day greater than current day and change of month i.e.date for future. 
c Test on no. of minutes in the future done in subr. isfile
c Also for obs. day greater than current day by more than 1.
         IF(IMONTH.EQ.12) THEN
            IMM=1
            IYY=IYEAR+1
            RETURN
         END IF
         IMM=IMONTH+1
         IYY=IYEAR
C
      RETURN
      END
