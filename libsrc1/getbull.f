      SUBROUTINE GETBULL(YCHAR,ILENG,IRET)
CTEST
C**** *GETBULL*
C
C
C     PURPOSE.
C     --------
C         READ NEXT BULLETIN from msysx/dataxxxx
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *GETBULL(YCHAR,ILENG,IRET)*
C
C     METHOD.
C     -------
C
c     Expects bulletin to start with 'ZCZC', end with 'NNNN'.
c     Starting in msys file where we left in last call to getbull.f:
c     Skips characters until 'ZCZC' is met, then stores chsoh as YCHAR(1). 
c     Rest of bulletin until 'NNNN' (this included) is stored as 
c     YCHAR(2:ILENG).
c     ILENG is on return therefore length of bulletin (chsoh, NNNN, ^M 
c     and new line characters included). 
c     IRET=0 if returning normally after having read a bulletin,
c         =7 if eof is read (this will require a separate call if last
c                            bulletin ends in 'NNNN')
c            or if error in system read
c         =8 if bulletin is too long ( > nbyte characters)
C
C     EXTERNALS.
C     ----------
C
C     call pbread2 (libemos)
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
c     12.09.2003 P.S. Replaced 'nbyter=read(msysdsi,in,nbyte)' with pbread2
c     (from pbio in libemos.a) and removed call on errsns (to compile on Linux)
c     30.11.2000 P.S. Increased max number of characters accepted in bulletin
c     from 4096 to 15000.
c     10.11.1999 P.S. Rewritten. Most important changes: 
c     - Use SAVE for variables in,ipek,nbyter,eof.
c     Earlier version was based on assumption that Fortran variables are 
c     static, which does not seem to be the case for the SGI compiler.
c     If in earlier version the dimension of IN was lowered to the same or 
c     almost the same size as nbyte (actually: less than nbyte + 1018), IN 
c     sometimes contained garbage, i.e. got overwritten, when getbull.f 
c     was called for a second or later times.
c     - If 'NNNN' happens to get divided between two reads, this is now
c     detected and no bulletine is missed in such cases (earlier version
c     only checked for divided 'ZCZC')
c     - Stops storing in YCHAR when bulletin is longer than nbyte characters, 
c     but continues to read until 'NNNN' is found. IRET=8 is returned in this
c     case, expecting calling program nix_xxxx to reject this bulletin.      
C
c
c     msysdsi(file descriptor (actually C-pointer) for msys file):
      INCLUDE 'commdb.f'
C
      PARAMETER (nbyte=15000)
c     This is the (max) number of bytes we try to read into IN in each read
c     (also the maximal allowed size of a bulletine)
c
      INTEGER inn(nbyte/4)       ! assuming integer in pbio (libemos) is integer*4
      CHARACTER*15000 in,ychar   ! 15000=nbyte 
      EQUIVALENCE (inn,in)
 
      CHARACTER*3 inx
      CHARACTER*1 chsoh   
cpsjul07      PARAMETER (chsoh='1'O) ! CHaracter: Start Of Header (=^A when viewed in emacs)
      PARAMETER (chsoh=ACHAR(1)) ! CHaracter: Start Of Header (=^A when viewed in emacs)
c
      INTEGER eof
c
      DATA ipek/0/
      DATA nbyter/0/
      DATA eof/0/
c ipek-   =index of last byte read in IN when 'NNNN' was encountered, else 0
c nbyter- =number of bytes read from msys data file
c eof-    =1 when end of file is met (detected by reading nbyter < nbyte)
c
      SAVE inn,in,ipek,nbyter,eof
c     According to the SGI Fortran manual, initializing ipek, nbyter and eof
c     in DATA-statements means an implicit SAVE of these variables. But
c     prefer to make the SAVE explicit to make clear that we want these
c     variables to retain their values from previous run (and to avoid future 
c     surprises if shifting to another compiler!)
C
      iextra=0
      ISTART=0
      IFOUND=0
      ILENG=0
c iextra- for checking for ZCZC when bulletin continues over a read.
c ISTART- =1 when start of bulletin is found.
c IFOUND- =1 when end of bulletin is found.
c ILENG-  length in chars. of bulletin.

C  -
C  - Start from where you left off
20    CONTINUE
c
      if(ipek.eq.0) then
         if(eof.eq.1) GO TO 400
         i=0
         call pbread2(msysdsi,inn,nbyte,nbyter)
c     On success a non-negative integer is returned in nbyter indicating the number of
c     bytes actually read. (0 if we were at end-of-file when pbread2 is called).
c     Otherwise, a -2 is returned.
         if(nbyter.eq.-2) then
            print*,'getbull - Error in pbread2 of msys file'
            GO TO 600
         endif
         if(nbyter.ne.nbyte) eof=1
      else
         i=ipek
      endif
25    I=I+1
c Check if ZCZC or NNNN spans over into next read. 
c If there are only 3 bytes left to check, save them in array inx and 
c set iextra=1. After next read test for ZCZC and NNNN in array inx 
c and beginning of array IN. Remember to reset iextra=0 after that test!
      if(i+2.eq.nbyter) then
         if(iextra.eq.0) then
            inx(1:3)=in(i:i+2)
         endif
         iextra=1
         ipek=0
      endif
c 
C
C  - Check if found bytes ZCZC
C
c if bulletin continues over a read i.e.iextra=1 means that the last 3 chars 
c have been saved in inx, and i=1 means that we have done a new read.
c 
      if(iextra.eq.1.and.i.eq.1) then
         if(inx(1:3).eq.'ZCZ'.and.in(1:1).eq.'C') then
            i=-2
            ISTART=1
         elseif(inx(2:3).eq.'ZC'.and.in(1:2).eq.'ZC') then 
            i=-1
            ISTART=1
         elseif(inx(3:3).eq.'Z'.and.in(1:3).eq.'CZC') then 
            i=0
            ISTART=1
         endif
      endif
c
      if(in(i:i+3).eq.'ZCZC') then
         ISTART=1
      endif
c
C  - If found ZCZC store character IN(i) in YCHAR, but let first character 
c     (corresponding to just have found 'ZCZC') be chsoh. Don't store
c     anything if ileng > nbyte
c
c     i:     where in IN we are
c     ileng: where in YCHAR we are
c
      IF(ISTART.EQ.1) THEN
         ILENG = ILENG+1
         if(ileng.eq.1) then
            YCHAR(1:1) = chsoh
            i=i+3
         elseif(ileng.le.nbyte) then
            YCHAR(ILENG:ILENG) = IN(i:i)
         endif
      ENDIF
C
C  - Check for bytes NNNN, provided ZCZC has already been found
c First check for the case where bulletin continues over a read.
c iextra=1 means that the last 3 chars have been saved in inx 
c and i=1 means that we have done a new read.
c 
      if(istart.eq.0) GO TO 150
      if(iextra.eq.1.and.i.eq.1) then
         if(inx(1:3).eq.'NNN'.and.in(1:1).eq.'N') then
            IFOUND=1
         elseif(inx(2:3).eq.'NN'.and.in(1:2).eq.'NN') then 
            i=2
            ILENG = ILENG+1
            YCHAR(ILENG:ILENG)='N'
            IFOUND=1
         elseif(inx(3:3).eq.'N'.and.in(1:3).eq.'NNN') then 
            i=3
            ILENG = ILENG+2
            YCHAR(ILENG:ILENG+1)='NN'
            IFOUND=1
         endif
         iextra=0
      endif
      if(i.ge.4) then
         if(in(i-3:i).eq.'NNNN') then
            IFOUND=1
         endif
      endif

      if(IFOUND.eq.1) then
         if(i.ge.nbyter) then
            ipek=0
         else
            ipek=i
         endif
         GO TO 200
      endif
c
 150  CONTINUE
      if(i.lt.nbyter) GO TO 25
      if(i.ge.nbyter) then
         ipek=0
         GO TO 20
      endif
C
C
 200  CONTINUE
      if(ileng.gt.nbyte) GO TO 500
      if(ifound.eq.1) GO TO 300
      GO TO 20
C
 300  CONTINUE
      IRET=0
      RETURN

 400  CONTINUE
      IRET=7
      RETURN

 500  CONTINUE
      print*,' WARNING in getbull.f: bulletin too long, ILENG=',ILENG
      IRET=8
      RETURN

 600  CONTINUE
      print*,' GETBULL: READ ERROR. '
      IRET=7
      RETURN
      END
