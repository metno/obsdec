      SUBROUTINE ERRSTA (IPART,IMARK,IFIRST,NUMBER)
C
C**** *ERRSTA*
C
C
C     PURPOSE.
C     --------
C
C         COUNTS THE NUMBER OF ERRORS IN THE REPORT
C         COUNTS THE NUMBER OF ERRORS IN THE DECODING JOB
C         MARKS THE ERROR BIT TO KDEC
C         ADDS ?-MARK TO KCHAR AT THE ERRONEUS GROUP
C
C         INPUT     : IPART    - INDICATOR OF NOER
C                                IN TEMPS  3 = A, 4 = B, 5 = C, 6 = D,
C                                IN PILOS  7 = A, 8 = B, 9 = C, 10 = D,
C                                2 = AIRCRAFT, 22 = DRIBU, 23 = BATHY, 24 = TESAC
C                     IMARK    = 1 IF ? IS TO BE ADDED TO ERRONEUS GROUP
C                              = 2 IF ? IS TO BE ADDED TO END OF ERRONEUS GROUP
C                     IFIRST   = O IF FIRST DECODING ATTEMPT OF THE REPORT
C                              = 1 OTHERWISE
C                     NUMBER   - NUMBER OF ERRORS IN THE REPORT SO FAR
C
C         OUTPUT    : NUMBER   - NEW NUMBER OF ERRORS
C                     NOER     - NUMBER OF ERRONEUS REPORT (INCREASED BY 1
C                                IF THE FIRST ERROR IN THE REPORT)
C                     KDEC(20) - ERROR BITS
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ERRSTA(IPART,IMARK,IFIRST,NUMBER)*
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     AUTHOR.
C     -------
C
C          A.HOLOPAINEN  NOV.83
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      implicit none
      integer IPART,IMARK,IFIRST,NUMBER
      integer kerbit,npt

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'   ! kdec,kerr
      INCLUDE 'comindx.f'   ! ipt,ieq,imi
      INCLUDE 'comstat.f'   ! numrerr,noer

C     ------------------------------------------------------------------

      IF(NUMBER .GE. 0) NUMBER = NUMBER + 1
      IF ( IFIRST.EQ.0 ) THEN
            IF(NUMBER .EQ. 1)
     *          NUMRERR(IPART)=NUMRERR(IPART) + 1
            NOER(IPART,KERR)=NOER(IPART,KERR) + 1
            KERBIT = IAND(ISHFT(KDEC(20),1-KERR),1)
            IF(KERBIT .EQ. 0) KDEC(20) = KDEC(20) + 2**(KERR-1)
         END IF

      IPT = IABS(IPT)
      IF(IMARK .EQ. 1) KCHAR(IPT) = IOR(KCHAR(IPT),128)
      IF(IMARK .EQ. 2) THEN
         NPT = IPT
         CALL NEXSEP2(NPT,IEQ,*200)
         CALL PREPRT(NPT,IMI,*200)
         KCHAR(NPT) = IOR(KCHAR(NPT),128)
      END IF

200   CONTINUE

      RETURN
      END
