      SUBROUTINE SHIPHEIGHTS(IERR)
C     Read in ship_height file into array SHGT
C     Returns IERR > 0 if file handling error

      IMPLICIT NONE

      INCLUDE 'comsh.f'   ! ship decks heigts: SHGT
      INCLUDE 'comshc.f'  ! ship names: YSNAME
      INCLUDE 'comdir.f'  ! yppdat

      INTEGER IERR,NDX,IOS,I1
      CHARACTER*80 DUMMY

      IERR = 0

C     Find end of character string. 
      NDX=INDEX(YPPDAT(1:80),' ') - 1

      OPEN(88,IOSTAT=IOS,ERR=1010,
     1     FILE=YPPDAT(1:NDX)//'ship_height.dat',
     2     FORM='FORMATTED',
     3     STATUS='OLD')

C     Skip first line ('MAXIMUM   99  SHIPS ALLOWED')
      READ(88,'(A9,F3.0)',ERR=1020,END=1100) DUMMY

      I1=0
 1000 CONTINUE
      I1=I1+1
      READ(88,'(A9,F3.0)',ERR=1020,END=1100) YSNAME(I1),SHGT(I1)
      GO TO 1000

 1010 CONTINUE
      WRITE(*,*) 'Error opening ship_height file'
      WRITE(*,*) 'Iostat=',IOS
      IERR=1
      RETURN

 1020 CONTINUE
      WRITE(*,*) 'Error during read ship_height file'
      WRITE(*,*) 'Iostat=',IOS
      IERR=1
      RETURN

 1100 CLOSE(88)
      RETURN
      END
