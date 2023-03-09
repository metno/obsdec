      SUBROUTINE SETTAB( IERR )
C
C**** *SETTAB*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO SET *BUFR *TABLES *B.
cps i.e: reads table B into variables in /COMTAB/ and /COMTABC/
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SETTAB(IERR)*
C
C     METHOD.
C      -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
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
C          M. D. DRAGOSAVAC    *ECMWF*       17/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
cpsmai02 removed
c      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER CTABBEN*64,CTABBU*24
cpsmai02 
      character spc
C
      INCLUDE 'parameter.f'
      INCLUDE 'comtab.f'
      INCLUDE 'comtabc.f'
CNH common for directories
      INCLUDE 'comdir.f'
C
C
C     ------------------------------------------------------------------
C*          1.    READ IN *BUFR *TABLE *B.
C                 ------------------------
 100  CONTINUE
C
      ierr=0
      spc=char(32)
      ndx=0
C
C  - Find end of character string. 
      ndx=index(ydatdef(1:80),spc) - 1
crr 990807 version 7
crr 950317 version 3
      OPEN(UNIT=9,IOSTAT=IOS,ERR=200,
crr     1     FILE='BUFR_TABLES:B000980201.DAT',
crr     1     FILE=YDATDEF(1:ndx)//'B000980301',
     1     FILE=YDATDEF(1:ndx)//'B0000980701',
     1     FORM='UNFORMATTED',
cpsjul07     1     READONLY,
     1     ACTION='READ',
     1     ACCESS='SEQUENTIAL',
     1     STATUS='OLD')
C
      READ(9,IOSTAT=IOS,ERR=300) NTABBTR,CTABBEN,CTABBU,NTABBS,
     1                           NTABBRV,NTABBDW
C
      CLOSE(9)
C
      RETURN
C     ------------------------------------------------------------------
 200  CONTINUE
C
cpsjun11      PRINT*,'Open error ',ios,' on file BUFR_TABLES:B0000980701'
      PRINT*,'Open error ',ios,' on file ',YDATDEF(1:ndx),'B0000980701'
      ierr=1
      return
C     -----------------------------------------------------------------
 300  CONTINUE
C
cpsjun11      PRINT*,'Read error ',ios,' on file BUFR_TABLES:B0000980701'
      PRINT*,'Read error ',ios,' on file ',YDATDEF(1:ndx),'B0000980701'
      ierr=1
      return
C     ------------------------------------------------------------------
C
      END
