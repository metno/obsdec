      SUBROUTINE GETNEXT(IERR )
C
C**** *GETNEXT*
C
C
C     PURPOSE.
C     --------
C         GET NEXT MESSAGE FROM MDB.
cps       Updates timestamp.dat and writes bulletine to STDOUT
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *GETNEXT( IERR )*
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
      implicit none
      integer ierr
      integer ios,ileng,iret,nryear,nrmonth,i1
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'  ! ilen
      INCLUDE 'comstat.f'  ! numbull
      INCLUDE 'comwt.f'    ! nsub
      INCLUDE 'commdbc.f'  ! ctstamp,cstream
      INCLUDE 'commsys.f'  ! msys,msystop
      INCLUDE 'comkey.f'   ! nrday,nrhour,nrmin,nrsec
C
c
      CHARACTER*15000 YCHAR
      CHARACTER*23 YTIME
      CHARACTER*14 CTIME
      CHARACTER*78 CSTR
c
      CSTR = '                   '//cstream(msys)(1:4)//' decoding'
C
C     ------------------------------------------------------------------
C*          1.   RESET ERROR INDICATOR AND NSUB
C                -----------------------------
      IERR=0
      NSUB=0
C
C           1.0  WRITE LAST TIME STAMP PROCESSED
C                -------------------------------.
C
      CALL ASCTIM(YTIME)        ! present time in format dd-mmm-yyyy hh:mm:ss.cc
      WRITE(17,9906,IOSTAT=IOS,ERR=300)
     1     CSTR,CSTREAM(MSYS),CTSTAMP,YTIME
C
      REWIND(17)
C
C           1.1  GET NEXT MESSAGE.
C                -----------------
 110  CONTINUE
      CALL GETBULL(YCHAR,ILENG,IRET)
cps      return code 7 means no more data, or error in system read
cps      return code 8 means too long bulletin: skip to next bulletin
      IF (IRET.EQ.7) THEN
         IERR=7
         RETURN
      ELSEIF (IRET.EQ.8) THEN
         GOTO 110
      END IF
C
      ILEN=ILENG
      NRSEC=00
cps      nryear and nrmonth are never used
      READ(CTSTAMP,'(I4,4I2)') nryear,nrmonth,NRDAY,NRHOUR,NRMIN
C
      WRITE(*,*) 'Current time stamp ',CTSTAMP(1:12)
C
      NUMBULL=NUMBULL+1
C
C           1.2  PRINT BULLETIN NUMBER.
C                ----------------------
C
      WRITE(*,9902) NUMBULL
CTEST**********************************************
c     stop after reading specified number of records
      IF(MSYSTOP.GT.0) THEN
         IF(NUMBULL.GT.MSYSTOP) then
            IERR=7
            RETURN
         END IF
      ENDIF
CTEST**********************************************

C
C           1.4  TRANSFER BULLETIN TO 'KCHAR' ONE CHARACTER PER WORD.
C                ----------------------------------------------------
C                AND SUPPRESS PARITY BIT.
C                ------------------------
      DO I1=1,ILEN
         KCHAR(I1)=IAND(ICHAR(YCHAR(I1:I1)),127)
      END DO
C
C     Write bulletine to STDOUT
      WRITE(*,*) YCHAR(1:ILENG)
C
      RETURN
C
C     -----------------------------------------------------------------
C
 9902 FORMAT(1H ,' ** BULLETIN NUMBER = ',I9)
 9906 FORMAT(1H ,A,
     1     //1H ,'stream             ',A,
     1      /1H ,'time stamp         ',a,
     1     //1H ,'last time updated  ',a  )
 9907 FORMAT(1H ,' ERROR WRITING TIME STAMP, IOS = ',I6)
C
 300  CONTINUE
      IERR=1
      WRITE(*,9907) ' Error writing time stamp.'
      RETURN

      END
