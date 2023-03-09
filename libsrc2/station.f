      SUBROUTINE STATION ( IERR )
C
C     PURPOSE.
C     --------
C         READ IN STATION LIST AND MAKE LIST OF IMPORTANT STATIONS.
C         ( WMO VOLUME A - LIST OF OBSERVING STATIONS)
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *STATION(IERR)*
C
C          OUTPUT: IERR  - ERROR INDICATOR, > 0 if file handling error
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *IMPSTAT*
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

      implicit none
      integer ierr
      integer ndx,ios

      INCLUDE 'combuff.f'  ! iparams,ipoints
      INCLUDE 'comdir.f'   ! yppdat

C     ------------------------------------------------------------------
C*          1.   READ IN STATION LIST.
C                ---------------------
      IERR = 0
      NDX  = 0

C     Find end of character string. 
      NDX = INDEX(YPPDAT(1:80),' ') - 1

      OPEN(UNIT=4,IOSTAT=IOS,ERR=300,
     1     FILE=YPPDAT(1:NDX)//'station_amend.dat',
cpsoct10     2     READONLY,
     2     ACTION='READ',
     3     STATUS='OLD',
     4     FORM='UNFORMATTED')

      READ(4,IOSTAT=IOS,ERR=400) IPARAMS,IPOINTS

      CLOSE(4,IOSTAT=IOS,ERR=500)

C*           2.  FIND IMPORTANT STATIONS.
C                ------------------------
      CALL IMPSTAT

      RETURN

C     ------------------------------------------------------------------

 300  CONTINUE
      WRITE(*,9901) IOS
      IERR = 1
      RETURN

 400  CONTINUE
      WRITE(*,9902) IOS
      IERR = 2
      RETURN

 500  CONTINUE
      WRITE(*,9903) IOS
      IERR = 3
      RETURN

 9901 FORMAT(1H ,' ERROR DURING OPENING STATION FILE
     *     (station_amend.dat), IOS=',I5)
 9902 FORMAT(1H ,' ERROR DURING READING FROM STATION FILE
     *     (station_amend.dat), IOS=',I5)
 9903 FORMAT(1H ,' ERROR DURING CLOSING STATION FILE
     *     (station_amend.dat), IOS=',I5)

      END
