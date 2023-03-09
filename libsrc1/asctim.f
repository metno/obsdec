      SUBROUTINE ASCTIM(CDATETIME)
C
C     This subroutine returns ascii time in form
C 
C     dd-mmm-yyyy hh:mm:ss.cc
Cps e.g. 08-Jul-2010 11:41:24.44
C     
      CHARACTER*23 CDATETIME
      CHARACTER*8  CTIME
cpsjul10 No longer used      CHARACTER*9  CDATE
      CHARACTER*2  CC
crr
      CHARACTER*24 CORINT
C
      SAVE ICC
c
crr      CALL TIME(CTIME)
cpsjul10      CALL DATE(CDATE)
crr
cps FDATE returns e.g.  Thu Jul  8 11:52:10 2010
      CALL FDATE(CORINT)
      if(corint(9:9).eq.' ') corint(9:9)='0'
crr
C
      IF(ICC.GE.99) ICC=0
C
      ICC=ICC+1
      WRITE(CC,'(I2.2)') ICC
C
crr      CDATETIME=CDATE(1:7)//'19'//CDATE(8:9)//' '//CTIME//'.'//CC
      CDATETIME=CORINT(9:10)//'-'//CORINT(5:7)//'-'//CORINT(21:24)//' '
     *  //CORINT(12:19)//'.'//CC
C
      RETURN
      END
