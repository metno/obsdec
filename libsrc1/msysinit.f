      SUBROUTINE MSYSINIT(IERR)
C
C     PURPOSE.
C     --------
C         OPEN CORRECT MSYSn FILE AND xxxx.timestamp.dat FILE
c         INITIALIZE cstream
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *MSYSINIT(IERR)*
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
C     call pbopen (libemos)
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          R. R.
C
C
C     MODIFICATIONS.
C     --------------
C
C     Sep 2003 P.S. 'msysdsi=open(cf(1:ndx),ioflag,iomode)'
C     replaced by pbopen. Cleaning up.
C     Jun 2003 P.S. 'grid' -> 'amte'
C
C
      INCLUDE 'commdb.f'
      INCLUDE 'commdbc.f'
CRR common for directories
      INCLUDE 'comdir.f'
C
      CHARACTER*80 CF
      CHARACTER*6  CDATFILE(40)
      CHARACTER*12  CTSTAMP
      CHARACTER*4  cstream(40),cfstamp(40)
      CHARACTER*23  current
      CHARACTER*24  corint
      CHARACTER*1  space
C  -see subroutine mdbinit
cpsjun03 Changed 'grid' into 'amte' for cstream and cfstamp (msys4)
      DATA cstream/'sys1','syno','temp',
     1              'amte','arep','drau',
     2              'sato','meta','syno',
     3            2*'    ','sate',1*'    ',
     4              'pilo','bath','tesa',24*'    '/
C
      DATA CDATFILE/'msys1/','msys2/','msys3/',
     1              'msys4/','msys5/','msys6/',
     2              'msys7/','msys8/','msys9/',
     3            2*'      ','msys7/',1*'      ',
     4              'msys3/','msys1/','msys1/',24*'      '/
C
      DATA cfstamp/'sys1','syno','temp',
     1              'amte','arep','drau',
     2              'sato','meta','sys9',
     3            2*'    ','sate',1*'    ',
     4              'pilo','bath','tesa',24*'    '/
C
      ierr = 0
      space=char(32)
      ndx=0
C
C  - Find end of character string. Open MSYSn file.
      ndx=index(ymsys(1:80),space) - 1
      cf=YMSYS(1:ndx)//CDATFILE(msys)//'data'//ctstamp(9:12)
      print*,' msys data file: ',cf
c
c     remove trailing spaces in file name
      ndx=index(cf(1:80),space) - 1
c
c   - We have to open msys file for binary read (pbread2, in getbull.f))
      call pbopen(msysdsi,cf(1:ndx),'r',ierr)
      if(ierr.ne.0) then
         print*,'Error opening file ',cf(1:ndx)
         return
      end if
C
C  - Open timestamp file.
      cf=cfstamp(msys)//'.timestamp.dat'
      print*,' timestamp file: ',cf
      OPEN(UNIT=17,IOSTAT=IOS,
     1                      FILE=cf,
     2                      FORM='FORMATTED',
     4                      RECL=80,
     6                      STATUS='OLD'              )
      if(ios.ne.0) then
         print*,'Open error ',ios,' on file ',CF
         ierr=1
         return
      end if

      RETURN
      END
