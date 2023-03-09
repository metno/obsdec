      SUBROUTINE STDATM(IERR)
C     Read in std_atm file into arrays SH and P
C     Returns IERR > 0 if file handling error

      IMPLICIT NONE

      INCLUDE 'comstd.f'  ! sh,p
      INCLUDE 'comdir.f'  ! yppdat

      INTEGER IERR,NDX,IOS,I1

      IERR = 0

C     Find end of character string. 
      NDX=INDEX(YPPDAT(1:80),' ') - 1

      OPEN(8,IOSTAT=IOS,ERR=1010,
     1     FILE=YPPDAT(1:NDX)//'std_atm.dat',
     2     FORM='FORMATTED',
     3     STATUS='OLD')

      DO I1=1,512
         READ(UNIT=8,FMT=7777,ERR=1020,END=1100,IOSTAT=IOS) SH(I1),P(I1)
 7777    FORMAT(1X,F5.0,2X,F8.2)
      END DO
      GOTO 1100

 1010 CONTINUE
      WRITE(*,*) 'Error opening std_atm file'
      WRITE(*,*) 'Iostat=',IOS
      IERR=1
      RETURN

 1020 CONTINUE
      WRITE(*,*) 'Error during read std_atm file'
      WRITE(*,*) 'Iostat=',IOS
      IERR=1
      RETURN

 1100 CLOSE(8)
      print*,'std_atm: sh(1,11,111)=',sh(1),sh(11),sh(111)
      RETURN
      END
