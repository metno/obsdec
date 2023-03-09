      SUBROUTINE MSYSINIT ( IERR )
C
C     PURPOSE.
C     --------
C         OPEN CORRECT MSYSn FILE AND xxxx.timestamp.dat FILE
C         AND bufr.dat. INITIALIZE cstream
C         
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *MSYSINIT(IERR)*
C
C         OUTPUT: IERR  - ERROR INDICATOR, > 0 IF ERROR OPENING FILE
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *PBOPEN* (libemos)
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
C                    replaced by pbopen. Cleaning up.
C     Jun 2003 P.S. 'grid' -> 'amte'
C     Jul 2006 P.S. Opens bufr.dat, added 'comwrite.f'
C     Feb 2011 P.S. bufr.dat opened in subdirectory cstream(msys)
C     Aug 2012 P.S. msys1 -> msys6 i cdatfile for bath and tesa
C     

      implicit none
      integer ierr
      integer ndx,ios,iret
      character space
      character*80 cf
      character*6  cdatfile(40)
      character*4  cfstamp(40)

      include 'commsys.f'  ! msys,msysdsi
      include 'commdbc.f'  ! cstream,ctstamp
      include 'comdir.f'   ! ymsys
      include 'comwrite.f' ! sunit

cpsjun03 Changed 'grid' into 'amte' for cstream and cfstamp (msys4)
      DATA cstream/'sys1','syno','temp',
     1              'amte','arep','drau',
     2              'sato','meta','syno',
     3            2*'    ','sate',1*'    ',
     4              'pilo','bath','tesa',24*'    '/

      DATA cdatfile/'msys1/','msys2/','msys3/',
     1              'msys4/','msys5/','msys6/',
     2              'msys7/','msys8/','msys9/',
     3            2*'      ','msys7/',1*'      ',
     4              'msys3/','msys6/','msys6/',24*'      '/

      DATA cfstamp/'sys1','syno','temp',
     1              'amte','arep','drau',
     2              'sato','meta','sys9',
     3            2*'    ','sate',1*'    ',
     4              'pilo','bath','tesa',24*'    '/

      ierr = 0
      space = char(32)

C     Find end of character string. Open MSYSn file.
      ndx = index(ymsys(1:80),space) - 1
      cf = YMSYS(1:ndx)//CDATFILE(msys)//'data'//ctstamp(9:12)
      write(*,*) ' msys data file: ',cf

C     Remove trailing spaces in file name
      ndx=index(cf(1:80),space) - 1

C     We have to open msys file for binary read (pbread2, in getbull.f))
      call pbopen(msysdsi,cf(1:ndx),'r',iret)
      if (iret.ne.0) then
         write(*,*) 'Error opening file ',cf(1:ndx)
         ierr = 1
         return
      end if

C     Open timestamp file.
      cf = cfstamp(msys)//'.timestamp.dat'
      write(*,*) ' timestamp file: ',cf
      OPEN(UNIT=17,IOSTAT=IOS,
     1     FILE=cf,
     2     FORM='FORMATTED',
     4     RECL=80,
     6     STATUS='OLD'              )
      if (ios.ne.0) then
         write(*,*) 'Open error ',ios,' on file ',cf
         ierr = 2
         return
      end if

C     Open file for appending of BUFR messages
      call pbopen(sunit,cstream(msys) // '/bufr.dat','a',iret)
      if (iret .ne. 0) then 
         write(*,*) 'pbopen failed for bufr.dat'
         ierr = 3
         return
      end if

      RETURN
      END
