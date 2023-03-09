      SUBROUTINE DATUM(idd,month,year)
C
C**** *DATUM*
C
C
C     PURPOSE.
C     --------
C         DEFINE PROPER MONTH AND YEAR IF DAY IS DEFINED.
C
C     INTERFACE.
C     ----------
C
C         *CALL* *DATUM(IDD,month,year)*
C
C     Input:       IDD - day of observation
C     Output:      month
C                  year (4 digits)
C
C     AUTHOR.
C     -------
C      
C     P.SANNES 2009-05-15
C     The original datum.f from ECMWF has been completely rewritten.
C      
C     METHOD.
C     -------
C
C     If IDD equals current day, then month and year of today is
C     returned. If IDD equals day of tomorrow, then month and year of
C     tomorrow is returned. If IDD is less than current day, then month
C     and year of today is returned. Else month and year of previous
C     month is returned.

      integer idd,month,year
      integer yyyy,mm,dd    ! date of today
      integer nyyyy,nmm,ndd ! date of next day (tomorrow)
      CHARACTER*10 time
      CHARACTER*8 date

      CALL DATE_AND_TIME(date,time) ! Intrinsic fortran90 function,
                                    ! returns current date and time
      READ(date(1:4),'(I4)') yyyy
      READ(date(5:6),'(I2)') mm
      READ(date(7:8),'(I2)') dd

      IF (idd.EQ.dd) THEN ! idd supposed to be today
         year = yyyy
         month = mm
      ELSE
C     Get date of tomorrow
         nyyyy = yyyy
         nmm = mm
         ndd = dd
         CALL inc_date(nyyyy,nmm,ndd)

         IF (idd.EQ.ndd) THEN   ! idd supposed to be tomorrow
            year = nyyyy
            month = nmm
         ELSE IF (idd.LT.dd) THEN ! idd supposed to be earlier this month
            year = yyyy
            month = mm
         ELSE IF (idd.GT.dd) THEN ! idd supposed to be previous month
            IF (mm.EQ.1) THEN
               year = yyyy - 1
               month = 12
            ELSE
               year = yyyy
               month = mm - 1
            END IF
         END IF
      END IF

      RETURN
      END SUBROUTINE DATUM

      SUBROUTINE inc_date(yyyy,mm,dd)
C     Inrease date by one day.
C     Modeled from the shell routine inc_date in funcs.sh, used in sms.
C     Note: it is not tested whether date is a valid date!
      IMPLICIT NONE
      INTEGER yyyy,mm,dd
      LOGICAL is_leap_year    ! Function

      IF (dd.EQ.28 .AND. mm.EQ.2) THEN
         IF (is_leap_year(yyyy)) THEN
            dd = 29
         ELSE
            dd = 1
            mm = 3
         END IF
      ELSE IF (dd.EQ.29 .AND. mm.EQ.2) THEN
            dd = 1
            mm = 3
      ELSE IF (dd.EQ.30 .AND. (mm.EQ.4.OR.mm.EQ.6
     +        .OR.mm.EQ.9.OR.mm.EQ.11)) THEN
         dd = 1
         mm = mm + 1
      ELSE IF (dd.EQ.31 .AND. (mm.EQ.1.OR.mm.EQ.5.OR.mm.EQ.7
     +     .OR.mm.EQ.8.OR.mm.EQ.10)) THEN
         dd = 1
         mm = mm + 1
      ELSE If (dd.EQ.31 .AND. mm.EQ.12) THEN
         dd = 1
         mm = 1
         yyyy = yyyy + 1
      ELSE
         dd = dd + 1
      END IF

      RETURN   
      END SUBROUTINE inc_date

      LOGICAL FUNCTION is_leap_year(yyyy)
C     Returns TRUE if yyyy is a leap year, else returns FALSE
      IMPLICIT NONE
      INTEGER yyyy

      IF (MOD(yyyy,400).EQ.0) THEN
         is_leap_year = .TRUE.
      ELSE IF (MOD(yyyy,4).EQ.0 .AND. MOD(yyyy,100).NE.0) THEN
         is_leap_year = .TRUE.
      ELSE
         is_leap_year = .FALSE.
      END IF

      RETURN
      END FUNCTION is_leap_year
      
      
