      PROGRAM DBTDEQC
C
C**** *DBTDEQC*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING
C         BATHY,DRIBU AND TESAC DATA.
C
cpssep06/aug11  Rewritten:
c  - Use BUFREN i libbufr for BUFR encoding
c  - No quality control (no call for qcdribu/qcbathy/qctesac,
c    and no confidence values produced)
c  - No section 2
c  - No index file (.key) created
c  - Bufr messages written sequentially with no garbage between to file bufr.dat
c    and/or bufr.dat_ocea (expected to be processed by bufrupdate next).
c  - No need for byte swapping, as everything written is big endian
c  - IMPLICIT NONE used everywhere
c  - INCLUDE files now contains declaration of variables also
c  - libdec.a stripped considerably  
c  - No .lfpw files produced
C
C**   INTERFACE.
C     ----------
C
c              msysdsi        YMSYS(1:ndx)//CDATFILE(msys)//'dataXXxx.dat' (usually msysdsi=4, opened in msysinit.f)
c              sunit          cstream(msys)/bufr.dat (opened in msysinit.f)
c              sunit_ocea     cstream(msys)/bufr.dat_ocea (opened only if oceanographic report)
c              11             cstream(msys)//'/'//cstream(msys)//'_error_file.dat'
c              17             cfstamp(msys)//'.timestamp.dat' (opened in msysinit.f)
c              77             ...dir.dat  (argument to program)
c              93             cstream(msys)//'stop.dat' (an empty file will be created if not present)
C
C     METHOD.
C     -------
C
C          NONE.
C
C     EXTERNALS.
C
C     ----------
C
C        *CALL* *INITVAR( IERR )*
C        *CALL* *PROCRFB( IERR )*
C        *CALL* *PROCHDR( IERR )*
C        *CALL* *PROCTXT( IERR )*
C        *CALL* *PROCT1S( IERR )*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      implicit none
      integer narg,ios,ierr,iunit,j,ipt,k
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comdir.f'
      INCLUDE 'commdbc.f'    ! ctstamp,cstream
      INCLUDE 'commsys.f'
      include 'comwrite.f'   ! num_records

      CHARACTER dir_file*80
      CHARACTER*15 cstop
c
      CHARACTER*2   cmsys
      CHARACTER*3   cmsystop
c..functions:
      integer       iargc
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES AND CONSTANTS.
C                ----------------------------------
 100  CONTINUE
C
crr
C   Read arguments: cmsys cmsystop
C
      narg=iargc()
      msystop=0
c
      if(narg.lt.3.) then
             print*,''
             print*,' Usage: drau dir_file cmsys timestamp <cmsystop>'
             print*,'                                      (optional)'
             print*,' where  dir_file  - specification of directories'
             print*,'        cmsys     - msys file number '
             print*,'                    6=drau, 15=bath, 16=tesac'
cpsokt98             print*,'        ctstamp   - mmddhhmm of msys file '
             print*,'        ctstamp   - yyyymmddhhmm of msys file '
             print*,'        cmsystop   - no. of records to read '
             print*,''
             goto 1020
      else
         call getarg(1,dir_file)
         call getarg(2,cmsys)
         read(cmsys(1:2),'(i2)') msys
         call getarg(3,ctstamp)
      endif
      if(narg.eq.4) then
         call getarg(4,cmsystop)
         read(cmsystop(1:3),'(i3)') msystop
      endif
c
      open(unit=77,iostat=ios,err=2001,
     1     FILE=dir_file,
     2     form='formatted',
     3     status='unknown' )
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) ymsys
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) yppdat
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) ydatdef
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) ytest
      print*,'ymsys',ymsys
c
      CALL INITVAR( IERR )
      IF(IERR.NE.0) CALL exit(2)
C
C           1.1  INITIALIZE MDB ACCESS.
C                ----------------------
 110  CONTINUE
C
      CALL MSYSINIT(IERR )
      IF(IERR.NE.0) THEN
                       PRINT*,'MSYSINIT: ERROR=',IERR
                       CALL exit(2)
                    END IF 
C
      IUNIT=93
C
      print*,' IUNIT ',IUNIT
      open(unit=iunit,iostat=ios,err=1001,
     1     FILE=cstream(msys)//'.stop.dat',
     2     form='formatted',
     3     status='unknown' )
C
C
C     ------------------------------------------------------------------
C*          2.   READ IN BULLETIN.
C                -----------------
 200  CONTINUE
C
      READ(IUNIT,'(a)',iostat=ios,err=1002) cstop
      REWIND(IUNIT)
C
      IF(cstop.eq.'stop_processing') GO TO 1000
C
      CALL GETNEXT(IERR )
C
      IF(IERR.NE.0) THEN
         IF(IERR.EQ.7) THEN
CRR         IERR=7 originally for timeout(i.e.no data to read). Here it is 
CRR         changed to mean that we have read all data (i.e. gone round msys file).
            PRINT*,'END OF MSYS FILE'
            IERR=0
            GO TO 1000
         END IF
         PRINT*,'GETNEXT: ERROR=',IERR
         CALL exit(2)
      END IF 
C     ------------------------------------------------------------------
C*          3.   FORMAT BULLETIN.
C                ----------------
 300  CONTINUE
C
      CALL PROCRFB( IERR, 'S', ' ' )
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' DBTDEQC - after PROCRFB, IERR= ',IERR
         GO TO 200
      END IF
C     ------------------------------------------------------------------
C*          4.   DECODE BULLETIN HEADER.
C                -----------------------
 400  CONTINUE
C
      CALL PROCHDR( IERR )
CTEST
      IF(KERR.NE.0)
     1 WRITE(*,*) ' DBTDEQC - after PROCHDR, KERR= ',KERR
CTEST
      j=ipt+10
      k=ipt-20
C
CLIL:
c     print*,'KDEC(14)',KDEC(14)
c      print*,'IPT,KCHAR',IPT,(KCHAR(I),i=ipt,j)
CLIL
      IF(KERR.NE.0) GO TO 200
C     ------------------------------------------------------------------
C*          5.   CHECK TEXT OF BULLETIN.
C                -----------------------
 500  CONTINUE
C
      CALL PROCTXT( IERR )
CTEST
      IF(KERR.NE.0)
     1 WRITE(*,*) ' DBTDEQC - after PROCTXT, KERR= ',KERR
CTEST
      IF(KERR.NE.0) GO TO 200
C
C     ------------------------------------------------------------------
C*          6.   CALL APPROPRIATE ROUTINE TO DECODE OCEANOGRAPHIC DATA.
C                ---------------------------------------------------
 600  CONTINUE
C
      CALL PROCT1S(cmsys, IERR)
CTEST
      IF(IERR.NE.0)
     1 WRITE(*,*) ' DBTDEQC - after PROCT1S, IERR= ',IERR
CTEST
C
C
      GO TO 200
C
C     ------------------------------------------------------------------
C
 1001 continue
c
      print*,' Open error on ',cstream(msys),'.stop.dat'
      go to 1000
c
 1002 continue
c
      print*,' Read error on ',cstream(msys),'.stop.dat'
      go to 1000
C
 2001 continue
c
      print*,' Open error on dir.dat'
      go to 1000
c
 2002 continue
c
      print*,' Read error on dir.dat'
c
 1000 CONTINUE
c
      print*,'number of reports written ',num_records
c
 1020 continue
      END
      SUBROUTINE PROCT1S (cmsys, IERR)
C
C**** *PROCT1S*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING BATHY,
C         TESAC AND DRIBU.
C
C         DATA ( BULLETINS WITH 'T1' OF 'S' )
C
C         INPUT    : IERR IS NOT USED ON INPUT.
C
C                    IT2  = 1-26 CORRESPONDING TO 'T2' OF A-Z.
C
C         OUTPUT   : IERR IS UNALTERED UNLESS A FATAL ERROR OCCURRS ,
C                     WHEN IT IS SET TO 1.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCT1S(IERR)*
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
C         *CALL* *BULLSO(IERR)*
C         *CALL* *BULLSS(IERR)*
C
      implicit none
      character*2   cmsys
      integer ierr
C
C
      INCLUDE 'parameter.f'     ! To be able to include comwork.f
      INCLUDE 'comwork.f'       ! IT2
C
C     ------------------------------------------------------------------
C*          1.  CALL APPROPRIATE ROUTINE, 'IT2' CONTAINS AN INTEGER IN THE
C                ---------------------------------------------------------
C               RANGE 1-26, CORRESPONDING TO 'T2' OF ABBREVIATED HEADER.
C                -------------------------------------------------------

      IF (IT2.EQ.19 .AND. cmsys.EQ.'6 ') THEN 
C     SS - FM18 BUOY. Decode this only when called as drau2bufr
         CALL BULLSS( IERR )
      ELSE IF (IT2.EQ.15 .AND. (cmsys.EQ.'15' .OR. cmsys.EQ.'16')) THEN
C     SO - FM63 (BATHY)/FM64 (TESAC). Decode this only when called as
C     bath2bufr or tesa2bufr
         CALL BULLSO( IERR )
      END IF

      RETURN
      END
      SUBROUTINE BULLSS ( IERR )
C
C**** *BULLSS*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF INDIVIDUAL
C         REPORTS FROM BULLETINS OF 'DRIBU '  REPORTS.
C
C         INPUT    : BULLETIN IN CCITT 5 CHARACTER, 1 CHARACTER PER
C                    WORD IN 'KCHAR'.
C
C                    POINTERS TO BEGINNING AND END OF 'STARTING LINE',
C                    'MIMIMJMJ LINE' AND END OF BULLETIN.
C
C                    IERR NOT USED
C
C         OUTPUT   : DECODED REPORTS WRITTEN TO FILE AND ERROR REPORTS
C                    TO ERROR FILE.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLSS(IERR)*
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *NEXTMI (I,J,K,KDEC(4))*
C         *CALL* *NEXTEQ (I,J)*
C         *CALL* *SAVREP (IHEAD,IERR) *
C         *CALL* *ZZINT (IHEAD,IERR)*
C         *CALL* *ZZDEC (IHEAD,IERR)*
C         *CALL* *NEXPRT (I,J,)*
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
      integer ihead,k,i,len,iipt,npt,misseq

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'

C*          1.1 SET FLAGS AND WORKING POINTERS.
C               -------------------------------

C     WORKING POINTER SET TO POINT TO FIRST LETTER OF MIMIMJMJ

      IPT = IMI

C     CLEAR FLAG WHICH INDICATES BULLETIN HEADER ALREADY WRITTEN
C     TO ERROR FILE

      IHEAD = 0

C*           1.2  SET DATA REPORT TYPE INDICATORS IN DECODED
C                 ------------------------------------------
C                 REPORT HEADER AND PART OF REPORT INDICATOR.
C                 ------------------------------------------
 101  CONTINUE

      K= IPT + 10
C     CHECK IF VALID MIMI OF 'MIMIMJMJ' (ZZYY)

      DO I=IPT,K
         IF (KCHAR( I ) .EQ. 90 .AND. KCHAR(I+1) .EQ. 90 .AND.
     &        KCHAR(I+2) .EQ. 89 .AND. KCHAR(I+3) .EQ. 89) THEN
            KDEC(4)=  165
            IPT    =  I
            GO TO 200
         END IF
      END DO

C*         1.3 INVALID 'MIMI'. WRITE REPORT TO ERROR FILE
C              ------------------------------------------

      KCHAR(IPT)=IOR(KCHAR(IPT),128)
      IEQ=IPT
      CALL NEXTEQ (IEQ,IGS)
      IF(IEQ.GT.IGS) RETURN

C     See if report too short
      LEN = IEQ-IPT
      IF(LEN.LT.17) GO TO 300

      KDEC(4)= 165
      CALL SAVREP(IHEAD,IERR)
      GO TO 300

C*          2.3 LOCATE END OF REPORT
C               --------------------
 200  CONTINUE

      IEQ = IPT
      MISSEQ = 0
      CALL NEXTEQ(IEQ,IGS)
      IF (IEQ.GT.IGS) THEN
         WRITE(*,*) ' = sign missing at the end of report '
         MISSEQ = 1 
         IEQ = IGS
      ENDIF

C     Check if '=' sign is missing, trying to find the next ZZYY
      CALL NEXTMI(IPT,IEQ,IIPT,KDEC(4))
      IF (IIPT.NE.9999) THEN
         IF (IIPT.LT.IEQ) THEN
            IF (MISSEQ.EQ.0) 
     *           WRITE(*,*) ' = sign missing at the end of report '
            IEQ = IIPT
         END IF
      END IF

C     See if report too short
      LEN = IEQ-IPT
      IF (LEN.LT.17) GO TO 300

C*           2. DECODE DRIBU REPORT.
C               --------------------
C     Convert to intermediate (integer) format

cps If YY and MM are in wrong order in group YYMMJ, KERR is put to 2 somewhere
cps ZZINT, but I haven't been able to locate where in ZZINT
      CALL ZZINT(IHEAD,IERR)

      IF (KERR.NE.0 .OR. IERR.NE.0) THEN
         WRITE(*,*) ' BULLSS - after call zzint, kerr,ierr',KERR,IERR
      ELSE
C     Convert intermediate (integer) format to decoded form
C     and write to file
         CALL ZZDEC(IHEAD,IERR)
         IF (KERR.NE.0 .OR. IERR.NE.0) THEN
            WRITE(*,*) ' BULLSS - after call zzdec, kerr,ierr',KERR,IERR
         END IF
      END IF

C*             3. LOCATE START OF NEXT REPORT
C                 ----------------------------
 300  CONTINUE

      IPT = IEQ + 1
      CALL NEXTPRT(IPT,IGS)

C*            3.1  CHECK FOR END OF BULLETIN
C                  -------------------------

      IF (IPT.GE.IGS) RETURN  ! This shouldn't be possible for correct 
                              ! formatted bulletines, but might happen if
                              ! '=' is missing at end of report

C     Search the beginning of next report
      NPT = IEQ
      CALL NEXTLET(NPT,IGS)
      IF(NPT.GE.IGS) RETURN
      CALL NEXTEND(NPT,IGS)
      IF(NPT.GE.IGS) RETURN

C     Go to next report
      GO TO 101

      END
      SUBROUTINE ZZINT (IHEAD,IERR)
C
C**** *ZZINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT DRIBU REPORTS  FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE (INTEGER) FORMAT
C
C         INPUT    : REPORT IN KCHAR(IPT) - KCHAR(IEQ),
C                    IN CCITT 5, 1 CHARACTER PER WORD.
C
C                    IHEAD = 0  INDICATES BULLETIN HEADER NOT ALREADY
C                               WRITTEN TO ERROR FILE
C                          = 1  MEANS HEADER ALREADY WRITTEN TO ERROR FILE
C
C                    IERR       NOT USED ON INPUT
C
C          OUTPUT   : REPORTS IN INTEGER FORMAT IN ARRAY 'KINT' IN DESIRED
C                     FORMAT (SEE SEPARATE DOCUMENTATION).
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ZZINT(IHEAD,IERR)*
C
C     METHOD.
C     -------
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING IS PROCEEDED
C         UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON FATAL ERROR HAS BEEN DETECTED THE ROUTINE TRIES
C         TO FIND THE BEGINNING OF CORRECT DATA. IF CORRECT DATA ARE
C         FOUND THE DECODING THE NUMBER OF NON-FATAL ERRORS IS COUNTED
C         AND IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE DATA
C         AFTER THE FIRST ERROR ARE REMOVED (THIS IS DONE TO ENSURE
C         THAT TOTALLY CORRUPTED DATA ARE NOT USED E.G. WRONG PART
C         OF DRIBU).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C         NUMERR = NUMBER OF NON-FATAL ERRORS
C         IREMPOS= THE POINTER TO FIRST ERROR IN 'KINT'.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *NEXTFIG(I,J)*
C         *CALL* *EXTGRP (I, , , , , , )*
C         *CALL* *ERRSTA (I, , , , , , )*
C         *CALL* *NEXSEP2(I,J,*)*
C         *CALL* *NEXPRT2(I,J,*)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C
      implicit none
      integer ihead,ierr
      integer keep,ifirst,numerr,irempos,nil,len,i,iret,ilo,ila
      integer ival,ipos
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comindx.f'
      include 'comdrau.f'   ! LaLaLaLaLa
C
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR. KEEP POINTER TO FIRST
C              ------------------------------------------------------
C              CHARACTER OF REPORT.
C              --------------------
 100  CONTINUE
C
C     POINTER TO FIRST CHARACTER
C
      KEEP=IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST= 0
C
C     ERROR INDICATOR
C
 101  KERR   = 0
      NUMERR = 0
      IREMPOS= 10
C
C
C*        1.1 CHECK FOR 'NIL' REPORT.
C             -----------------------
 110  CONTINUE
C
      NIL= 0
C
      LEN=IEQ-IPT
      IF(LEN.LE.30)
     &   THEN
            KERR=1
            NIL =1
            CALL ERRSTA(22,0,IFIRST,NUMERR)
         END IF
C
C*            2. SET AREA FOR INTERMEDIATE (INTEGER) REPORT
C                ------------------------------------------
C                TO MISSING DATA VALUE .
C                -----------------------
 200  CONTINUE
C
      DO I=1,jp15
         KINT(I)=MINDIC
      END DO
C
C
C*            3.  SECTION 0 - IDENTIFICATION AND POSITION DATA;
C                 ---------------------------------------------
C                 BUOY IDENTIFIECATION,TIME,POSITION.
C                 -----------------------------------
 300  CONTINUE
C
C     BUOY IDENTIFIER. A1BWNBNBNB
C     ---------------------------
C
      CALL NEXTFIG(IPT,IEQ)
      IF(IPT.GT.IEQ) GO TO 700
C
      CALL EXTGRP (IPT,1,1,3,0,0,1,IRET)
      IF(IRET.NE.0) THEN
         KERR=5
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=1
      END IF
C
C     YYMMJ - GROUP
C
      CALL NEXSEP2 (IPT,IEQ,*700)
      CALL NEXPRT2 (IPT,IEQ,*700)
C
      CALL EXTGRP(IPT,2,2,1,0,0,4,IRET)
      IF(IRET.NE.0) THEN
         KERR=2
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=4
         GO TO 700
      END IF
C
C     GG gg iw
C     -- -- -
      CALL NEXSEP2 (IPT,IEQ,*700)
      CALL NEXPRT2 (IPT,IEQ,*700)
C
      CALL EXTGRP(IPT,2,2,1,0,0,7,IRET)
      IF(IRET.NE.0) THEN
         KERR=3
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         GO TO 700
      END IF
C
      IF(KINT(4).LT.1.OR.KINT(1).GT.31) THEN
         IRET=1
         KINT(4)=MINDIC
      END IF
C
      IF(KINT(5).LT.1.OR.KINT(5).GT.12) THEN
         IRET=2
         KINT(5)=MINDIC
      END IF
C
      IF(KINT(6).LT.0.OR.KINT(6).GT.9) THEN
         IRET=3
         KINT(6)=MINDIC
      END IF
C 
      IF(KINT(7).LT.0.OR.KINT(7).GT.24) THEN
         IRET=2
         KINT(7)=MINDIC
      END IF
      IF(KINT(8).LT.0.OR.KINT(8).GT.59) THEN
         IRET=2
         KINT(8)=MINDIC
      END IF
C
      IF(IRET.NE.0) THEN
C
C        CHECK IF ALREADY KNOWN TO BE 'NIL' - REPORT
C
         IF(NIL.EQ.1) THEN
            KERR=1
            RETURN
         END IF
C
         KERR=2
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         RETURN
C
      END IF
C
C     QCLALALALALA - GROUP
C     - - - - -
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
      if(kchar(ipt+6).eq.32) then
         if(kchar(ipt+5).eq.47) kchar(ipt+5)=48
         if(kchar(ipt+4).eq.47) kchar(ipt+4)=48
         LaLaLaLaLa = .true.
         ILO=6
         ILA=5
      elseif(kchar(ipt+5).eq.32) then
         LaLaLaLaLa = .false.
         ILO=5
         ILA=4
      end if
 
      CALL EXTGRP (IPT,1,ILA,0,0,0,10,IRET)
      IF(IRET.NE.0) THEN
         KERR=4
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         GO TO 700
      END IF
C
C     LOLOLOLOLOLO - GROUP
C     - - - - -
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
      if(kchar(ipt+6).eq.32) then
         if(kchar(ipt+5).eq.47) kchar(ipt+5)=48
         if(kchar(ipt+4).eq.47) kchar(ipt+4)=48
      end if
C
      CALL EXTGRP (IPT,ILO,0,0,0,0,12,IRET)
      IF(IRET.NE.0) THEN
         KERR=5
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         GO TO 700
      END IF
C
C     Ql - quality control indicator for position
C     Qt - quality control indicator for time
C     QA - Location Quality class
Crrsept00     6QlQt// 
C     6QlQtQA/ 
C     -------
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
C     CHECK IF NEXT GROUP QUALITY CONTROL GROUP
C     -----------------------------------------
Crrsept00       IF(KCHAR(IPT).EQ.54.AND.KCHAR(IPT+3).EQ.47.AND.
Crrsept00      1   KCHAR(IPT+4).EQ.47) THEN
      IF(KCHAR(IPT).EQ.54.AND.KCHAR(IPT+4).EQ.47) THEN
         CALL EXTGRP (IPT,1,1,1,1,0,13,IRET)
         IF(IRET.NE.0) THEN
            KERR=5
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            GO TO 700
         END IF
C
         CALL NEXSEP2(IPT,IEQ,*700)
         CALL NEXPRT2(IPT,IEQ,*700)
C
      END IF 
C
C        CHECK IF IT IS BEGINING OF SECTION 1
C
      IF(KCHAR(IPT).EQ.49.AND.KCHAR(IPT+1).EQ.49.AND.
     1      KCHAR(IPT+2).EQ.49) THEN
            CALL EXTGRP (IPT,3,1,1,0,0,17,IRET)
            IF(IRET.NE.0) THEN
               KERR=5
               CALL ERRSTA(22,1,IFIRST,NUMERR)
               IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
               GO TO 700
            END IF
       ELSE
          GO TO 311
       END IF
C
C-----------------------------------------------------------------------
C*          3.1 SECTION 1 - SURFACE PARAMETERS.
C               -------------------------------
 310  CONTINUE
C
C     CHECK IF THE NEXT GROUP IS BEGINING OF SECTION 2
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
 311  CONTINUE
C
      IF(KCHAR(IPT  ) .EQ. 50 .AND.
     &   KCHAR(IPT+1) .EQ. 50 .AND.
     &   KCHAR(IPT+2) .EQ. 50 ) then
C           BEGINING OF THE SECTION 2 FOUND
         GO TO 400
      END IF
C
      IF(KCHAR(IPT  ) .EQ. 51 .AND.
     &   KCHAR(IPT+1) .EQ. 51 .AND.
     &   KCHAR(IPT+2) .EQ. 51 ) then
C           BEGINING OF THE SECTION 3 FOUND
            GO TO 420
         END IF
C
      IF(KCHAR(IPT  ) .EQ. 52 .AND.
     &   KCHAR(IPT+1) .EQ. 52 .AND.
     &   KCHAR(IPT+2) .EQ. 52) then
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 500
         END IF
C     -----------------------------------------------------------------
C     THERE ARE SOME USEFULL INFORMATION IN SECTION 1
C
      CALL EXTVAL(IPT,1,IVAL)
C
      IF(IVAL .EQ. 0) THEN
C
C        WIND DIRECTION AND SPEED
C
         CALL EXTGRP (IPT,1,2,2,0,0,20,IRET)
         IF(IRET.NE.0) THEN
            KERR=9
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=20
            GO TO 700
         END IF
         GO TO 310
      END IF
C
      IF(IVAL .EQ. 1) THEN
C
C       AIR TEMPERATURE FOUND
C
        CALL EXTGRP (IPT,1,1,3,0,0,23,IRET) 
        IF(IRET.NE.0) THEN
           KERR=10
           CALL ERRSTA(22,1,IFIRST,NUMERR)
           IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
           IF(NUMERR.EQ.1) IREMPOS=23
           GO TO 700
        END IF
        GO TO 310
      END IF
C
      IF(IVAL .EQ. 2) THEN
C
C       2SNTDTDTD
C
        CALL EXTGRP (IPT,1,1,3,0,0,26,IRET)
        IF(IRET.NE.0) THEN
           KERR=10
           CALL ERRSTA(22,1,IFIRST,NUMERR)
           IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
           IF(NUMERR.EQ.1) IREMPOS=26
           GO TO 700
        END IF
        GO TO 310
      END IF
C
      IF(IVAL .EQ. 3) THEN
C
C       3P0P0P0P0 STATION LEVEL PRESSURE
C
        CALL EXTGRP (IPT,1,4,0,0,0,29,IRET)
        IF(IRET.NE.0) THEN
           KERR=10
           CALL ERRSTA(22,1,IFIRST,NUMERR)
           IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
           IF(NUMERR.EQ.1) IREMPOS=29
           GO TO 700
        END IF
        GO TO 310
      END IF
C
      IF(IVAL .EQ. 4) THEN
C
C        MEAN SEA LEVEL PRESSURE FOUND
C
         CALL EXTGRP (IPT,1,4,0,0,0,31,IRET)
         IF(IRET.NE.0) THEN
            KERR=7
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=31
            GO TO 700
         END IF
         GO TO 310
      END IF
C
      IF(IVAL .EQ. 5) THEN
C
C        PRESSURE TENDENCY  FOUND
C
         CALL EXTGRP (IPT,1,1,3,0,0,33,IRET)
         IF(IRET.NE.0) THEN
            KERR=11
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=33
            GO TO 700
         END IF
         GO TO 310
      END IF
C
      GO TO 310
C     ------------------------------------------------------------------
C*          4. SECTION 2.
C              ----------
 400  CONTINUE
C
      CALL EXTGRP (IPT,3,1,1,0,0,36,IRET)
      IF(IRET.NE.0) THEN
         KERR=8
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=36
         GO TO 700
      END IF
C
 410  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
      IF(KCHAR(IPT  ) .EQ. 51 .AND.
     &   KCHAR(IPT+1) .EQ. 51 .AND.
     &   KCHAR(IPT+2) .EQ. 51 ) then
C           BEGINING OF THE SECTION 3 FOUND
            GO TO 420
         END IF
C
      IF(KCHAR(IPT  ) .EQ. 52 .AND.
     &   KCHAR(IPT+1) .EQ. 52 .AND.
     &   KCHAR(IPT+2) .EQ. 52) then 
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 500
         END IF
C     -----------------------------------------------------------------
C     THERE ARE SOME USEFULL INFORMATION IN SECTION 2
C
      CALL EXTVAL(IPT,1,IVAL)
C
C
      IF(IVAL .EQ. 0) THEN
C
C        SEA SURFACE TEMPERATURE FOUND
C
         CALL EXTGRP (IPT,1,1,3,0,0,39,IRET)
         IF(IRET.NE.0) THEN
            KERR=8
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=39
            GO TO 700
         END IF
         GO TO 410
      END IF
C
      IF(IVAL .EQ. 1) THEN
C
C        PERIOD AND HEIGTH OF WAVES.
C
         CALL EXTGRP (IPT,1,2,2,0,0,42,IRET)
         IF(IRET.NE.0) THEN
            KERR=9
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=42
            GO TO 700
         END IF
         GO TO 410
      END IF
C
      IF(IVAL.EQ.2.AND.KCHAR(IPT+1).EQ.48) THEN
C
C        PERIOD OF WAVES.
C
         CALL EXTGRP (IPT,2,3,0,0,0,45,IRET)
         IF(IRET.NE.0) THEN
            KERR=10
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=45
            GO TO 700
         END IF
         GO TO 410
      END IF
C
      IF(IVAL.EQ.2.AND.KCHAR(IPT+1).EQ.49) THEN
C
C        HEIGHT OF WAVES.
C
         CALL EXTGRP (IPT,2,3,0,0,0,47,IRET)
         IF(IRET.NE.0) THEN
            KERR=11
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=47
            GO TO 700
         END IF
         GO TO 410
      END IF
C
      GO TO 410
C-----------------------------------------------------------------------
C*          4.2 SECTION 3 - TEMPERATURES,SALINITY AT SIGNIFICANT DEPTHS.
C              ---------------------------------------------------------
 420  CONTINUE
C
      CALL EXTGRP (IPT,3,1,1,0,0,49,IRET)
      IF(IRET.NE.0) THEN
         KERR=12
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=49
         GO TO 700
      END IF
C
 421  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
      IF(KCHAR(IPT  ) .EQ. 52 .AND.
     &   KCHAR(IPT+1) .EQ. 52 .AND.
     &   KCHAR(IPT+2) .EQ. 52) then
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 500
         END IF
C
C     CHECK IF SEA T OR CURRENT INFORMATION FOLLOW
C
      IF(KCHAR(IPT  ) .EQ.56 .AND.
     &   KCHAR(IPT+1) .EQ.56       ) GO TO 430
      IF(KCHAR(IPT  ) .EQ.54 .AND.
     &   KCHAR(IPT+1) .EQ.54       ) GO TO 440
C
      GO TO 421
C
C     -----------------------------------------------------------------
 430  CONTINUE
C
      CALL EXTGRP (IPT,4,1,0,0,0,52,IRET)     ! 8887k2
      IF(IRET.NE.0) THEN
         KERR=14
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=52
         GO TO 700
      END IF
C
C-----------------------------------------------------------------------
      IPOS=48  
C
 431  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
      IF(KCHAR(IPT  ) .EQ. 52 .AND.
     &   KCHAR(IPT+1) .EQ. 52 .AND.
     &   KCHAR(IPT+2) .EQ. 52) then
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 500
         END IF
C
C     CHECK IF CURRENT INFORMATION FOLLOWS.
C
      IF(KCHAR(IPT  ) .EQ.54 .AND.
     &   KCHAR(IPT+1) .EQ.54      ) GO TO 440
C     -----------------------------------------------------------------
C     TEMPERATURES AT SIGNIFICANT DEPTHS FOLLOW
C
      CALL EXTVAL(IPT,1,IVAL)
C
      IF(IVAL.EQ. 2 ) THEN     ! znznznzn
cpsokt18 KINT allows 58 levels at most, kdec 53. But KKDEC stops at 49 levels
         IF(IPOS .GT. 48 + 6*49) THEN
            KERR=49
            RETURN
         END IF
cps
         IPOS=IPOS+6
         CALL EXTGRP (IPT,1,4,0,0,0,IPOS,IRET)
         IF(IRET.NE.0) THEN
            KERR=14
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=IPOS
            GO TO 700
         END IF
         GO TO 431
      END IF
C
      IF(IVAL.EQ. 3 ) THEN     ! TnTnTnTn
cpssep02 Some buoys report last digit in TnTnTnTn as '/'. Convert this to '0'. 
cps      Could alternatively extracted 3 digits instead of 4 and then tested
cps      for this in ZZDEC, but this hack is simpler...
         if(kchar(ipt+4).eq.47) kchar(ipt+4)=48
         CALL EXTGRP (IPT,1,4,0,0,0,IPOS+2,IRET)
         IF(IRET.NE.0) THEN
            KERR=14
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=IPOS+2
            GO TO 700
         END IF
         GO TO 431
      END IF
C
      IF(IVAL.EQ. 4 ) THEN     ! SnSnSnSn
         CALL EXTGRP (IPT,1,4,0,0,0,IPOS+4,IRET)
         IF(IRET.NE.0) THEN
            KERR=14
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=IPOS+4
            GO TO 700
         END IF
      END IF
C
      GO TO 431
C-----------------------------------------------------------------------
 440  CONTINUE
C
C     66K69K3
C
      CALL EXTGRP (IPT,2,1,1,1,0,401,IRET)
      IF(IRET.NE.0) THEN
         KERR=14
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=401
         GO TO 700
      END IF
C
      IPOS=405
C
 441  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
      IF(KCHAR(IPT  ) .EQ. 52 .AND.
     &   KCHAR(IPT+1) .EQ. 52 .AND.
     &   KCHAR(IPT+2) .EQ. 52 .AND.
     &   KCHAR(IPT+3) .LE. 32      )
     &   THEN
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 500
         END IF
C
C     -----------------------------------------------------------------
C     DEPTH, DIRECTION AND SPEED OF CURRENT.
C
      CALL EXTVAL(IPT,1,IVAL)
C
      IF(IVAL.EQ. 2 ) THEN
         CALL EXTGRP (IPT,1,4,0,0,0,IPOS,IRET)
         IF(IRET.NE.0) THEN
            KERR=14
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=IPOS
            GO TO 700
         END IF
      END IF
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
      IF(KCHAR(IPT  ) .EQ. 52 .AND.
     &   KCHAR(IPT+1) .EQ. 52 .AND.
     &   KCHAR(IPT+2) .EQ. 52 ) then 
C        BEGINING OF THE SECTION 4 FOUND
         GO TO 500
      END IF
C
      CALL EXTGRP (IPT,2,3,0,0,0,IPOS+2,IRET)
      IF(IRET.NE.0) THEN
         KERR=14
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=IPOS+2
         GO TO 700
      END IF
C
      IPOS=IPOS+4
      GO TO 441
C     -----------------------------------------------------------------
C*          5. QUALITY CONTROL INFORMATION.
C              ----------------------------
cps Section 4, which starts by 444. From nov 1 2001 9 groups, each one
cps starting with integer 1-9 are possible here, with two additional 
cps groups possibly following group 2 depending on value of QL. We
cps decode some of these groups into KINT and KDEC (see kint.drau), 
cps but none of these data seem to be further decoded into bufr.
 500  CONTINUE
C
      CALL EXTGRP (IPT,3,0,0,0,0,701,IRET)
      IF(IRET.NE.0) THEN
         KERR=15
         CALL ERRSTA(22,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
         IF(NUMERR.EQ.1) IREMPOS=701
         GO TO 700
      END IF
C
 510  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*700)
      CALL NEXPRT2(IPT,IEQ,*700)
C
C     THERE ARE SOME INFORMATION ABOUT TECHNICAL AND QUALITY FEATURES
C
      CALL EXTVAL(IPT,1,IVAL)
C
      IF(IVAL.EQ.1) THEN
C        1QPQ2QTWQ4 Quality of the pressure measurement, housekeeping parameter,
C                   water-surface temperature, air temperature
         CALL EXTGRP (IPT,1,1,1,1,1,702,IRET)
         IF(IRET.NE.0) THEN
            KERR=16
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=702
            GO TO 700
         END IF
         GO TO 510
      END IF
C
      IF(IVAL.EQ.2) THEN
C        2QNQLQA/ Quality of the buoy satellite transmission, 
C                 quality of location, location quality class
         CALL EXTGRP (IPT,1,1,1,1,0,707,IRET)
         IF(IRET.NE.0) THEN
            KERR=17
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=707
            GO TO 700
         END IF
C
         CALL NEXSEP2(IPT,IEQ,*700)
         CALL NEXPRT2(IPT,IEQ,*700)
C
         IF(KINT(709).EQ.1) THEN
C
C           SKIP YYMMJ GGgg/ GROUPS
Cps skipping of GGgg/ is done by ' CALL NEXSEP2' right after '510 continue'
C
            CALL NEXSEP2(IPT,IEQ,*700)
            CALL NEXPRT2(IPT,IEQ,*700)
C
            GO TO 510
         ENDIF
C
         IF(KINT(709).EQ.2) THEN
Cps         QL=2. QcLaLaLaLaLa LoLoLoLoLoLo should follow
C           QcLaLaLaLaLa Latitude of second possible solution
            CALL EXTGRP (IPT,1,4,0,0,0,711,IRET)
            IF(IRET.NE.0) THEN
               KERR=19
               CALL ERRSTA(22,1,IFIRST,NUMERR)
               IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
               IF(NUMERR.EQ.1) IREMPOS=711
               GO TO 700
            END IF
C
            CALL NEXSEP2(IPT,IEQ,*700)
            CALL NEXPRT2(IPT,IEQ,*700)
C
C           LoLoLoLoLoLo Longitude of second possible solution
            CALL EXTGRP (IPT,5,0,0,0,0,713,IRET)
            IF(IRET.NE.0) THEN
               KERR=20
               CALL ERRSTA(22,1,IFIRST,NUMERR)
               IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
               IF(NUMERR.EQ.1) IREMPOS=713
               GO TO 700
            END IF
C
            GO TO 510
C
         END IF
      END IF
C
      IF(IVAL.EQ.3) THEN
C        3 ZhZhZhZh Hydrostatic pressure of lower end of cable, in kPa
         CALL EXTGRP (IPT,1,4,0,0,0,725,IRET)
         IF(IRET.NE.0) THEN
            KERR=22
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1)  IREMPOS=725
            GO TO 700
         END IF
      END IF
c
      IF(IVAL.EQ.4) THEN
C        4ZcZcZcZc  lenght of cable in m termistor string
         CALL EXTGRP (IPT,1,4,0,0,0,727,IRET)
         IF(IRET.NE.0) THEN
            KERR=22
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1)  IREMPOS=727
            GO TO 700
         END IF
      END IF
C
      IF(IVAL.EQ.5) THEN
C        5BtBtXtXt Type of buoy, type of drogue
         CALL EXTGRP (IPT,1,2,2,0,0,729,IRET)
         IF(IRET.NE.0) THEN
            KERR=22
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1)  IREMPOS=729
            GO TO 700
         END IF
      END IF
C
      IF(IVAL.EQ.6) THEN
C        6AhAhAhAn  Anemometer height, type of anemometer
         CALL EXTGRP (IPT,1,3,1,0,0,732,IRET)
         IF(IRET.NE.0) THEN
            KERR=22
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1)  IREMPOS=732
            GO TO 700
         END IF
      END IF
C
      IF(IVAL.EQ.7) THEN
C           7VBVBdBdB Drifting speed in cm/s, drift direction  
Cpssept00 Moved this part from inside 'IF(IVAL.EQ.2) THEN' loop
Cps       Old version incorrectly placed the group following GGgg/ in
Cps       kint(714-716), even if this group did not start with 7.
Cps       Note that we now permit 7VBVBDBDB to be decoded even if
Cps       QL <> 1. We regard reports with this discrepancy with FM 18 BUOY 
Cps       to be a possible problem not for decoding, but for quality 
Cps       control; we decode what we find!
         CALL EXTGRP (IPT,1,2,2,0,0,714,IRET)
         IF(IRET.NE.0) THEN
            KERR=18
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=714
            GO TO 700
         END IF
      END IF
C
      IF(IVAL.EQ.8) THEN
C        8ViViViVi Information on the engineering status of the buoy
         CALL EXTGRP (IPT,1,4,0,0,0,717,IRET)
         IF(IRET.NE.0) THEN
            KERR=21
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=717
            GO TO 700
         END IF
C
         GO TO 510
C
      END IF
C
      IF(IVAL.EQ.9) THEN
C        9/ZdZdZd Length of the cable at which the drogue is attached, in metres
         CALL EXTGRP (IPT,1,1,3,0,0,719,IRET)
         IF(IRET.NE.0) THEN
            KERR=22
            CALL ERRSTA(22,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(22,KERR)=NOER(22,KERR)+1
            IF(NUMERR.EQ.1)  IREMPOS=719
            GO TO 700
         END IF
      END IF
C
      GO TO 510
C     -----------------------------------------------------------------
C*            7. ERROR CHECKING AND HANDLING .
C                -----------------------------
 700  CONTINUE
C
C     RETURN IF NO ERRORS IN REPORT
CTEST
CTEST      CALL PRTKINT(KINT,1,3000,MINDIC)
C
      IF(KERR.EQ.0) RETURN
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF(IFIRST.EQ.0) THEN
         IPT=KEEP
         CALL FIXSS
         IFIRST=1
         NUMRERR(22)=NUMRERR(22) + 1
         GO TO 101
      ELSE
         CALL SAVREP(IHEAD,IERR)
C     
C     CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
         DO 701 I=KEEP,IGS
            KCHAR(I)=IAND(KCHAR(I),127)
 701     CONTINUE
C     
C     ONLY REPORTS WITH ERROR IN DATE/TIME
C     OR LAT/LONG ARE NOT PROCESSED ANY FURTHER
C     
C     IF THERE ARE MORE THEN 5 NON-FATAL ERRORS
C     IN REPORT IT IS ONLY PROCESSED UNTIL THE
C     FIRST ERROR
C     
         IF(NUMERR.GT.5) THEN
            DO I=IREMPOS,jp15
               KINT(I)=MINDIC
            END DO
         END IF
C     
         IF(KERR.GT.6) KERR=0
      END IF
C     
 703  CONTINUE
C
C      CALL PRTKINT(KINT,1,3000,MINDIC)
C
      RETURN
      END
      SUBROUTINE ZZDEC(IHEAD,IERR)
C
C**** *ZZDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE DRIBU REPORT  FROM INTERMEDIATE
C         FORMAT TO FULL DECODED FORMAT
C
C         INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'
C
C         OUTPUT   : REPORT WRITTEN INTO DECODED REPORT FILE 04
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ZZDEC(IHEAD,IERR)*
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
C          CALL QLALOD 
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C
      implicit none
      integer ihead,ierr
      integer i,ib1,ib2,ib3,ipkt,ipkd,kdec13,kdec22
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
      INCLUDE 'comkeyc.f'
      include 'comdrau.f'    ! L11-L33,NTL,NCL,LaLaLaLaLa
C
C     ------------------------------------------------------------------
C*          1.  INITIALIZE CONSTANTS AND VARIABLES.
C                ----------------------------------
 100  CONTINUE
C
C     CLEAR THE ERROR INDICATOR
C
      KERR=0
C
C     SET DECODED REPORT TO MISSING DATA VALUE
cps   kdec(4) is set in bullso, no kdec is set in zzint
C
      DO I=1,3
         KDEC(I)=MINDIC
      END DO
      DO I=5,jp11
         KDEC(I)=MINDIC
      END DO
C
C     CLEAR 'NIL' REPORT INDICATOR
C
      KDEC(20)=0
C
C---------------------------------------------------------------------
C*           2. SET UP HEADER OF FULLY EXPANDED DRIBU FORMAT.
C               ---------------------------------------------
 200  CONTINUE
C
C     SET UP DAY, HOUR AND MINUTE
C
      KDEC(1)=KINT(4)
      KDEC(2)=KINT(7)
      KDEC(9)=KINT(8)
C
C     SET UP LATITUDE AND LONGITUDE
C
      if (LaLaLaLaLa) then
         CALL QLALOD(KINT(10),KINT(11),KINT(12),MINDIC,KDEC(5),KDEC(6))
      else
         CALL QLALOM(KINT(10),KINT(11),KINT(12),MINDIC,KDEC(5),KDEC(6))
      end if
C
      IF(KDEC(5).EQ.MINDIC.OR.KDEC(6).EQ.MINDIC) THEN
         IERR=5
         RETURN
      ENDIF
C
C     SET BUOY IDENTIFICATION
C
      IF(KINT(1).NE.MINDIC.AND.
     &   KINT(2).NE.MINDIC.AND.
     &   KINT(3).NE.MINDIC) THEN
         KINT(1)=KINT(1) + 48
         KINT(2)=KINT(2) + 48
         IB1=KINT(3)/100
         IB2=(KINT(3)-IB1*100)/10
         IB3=(KINT(3)-((KINT(3)/10)*10))
         IB1=IB1+48
         IB2=IB2+48
         IB3=IB3+48
C
         CIDENT=CHAR(KINT(1))//CHAR(KINT(2))//CHAR(IB1)
     1          //CHAR(IB2)//CHAR(IB3)//'    '
C
         READ(CIDENT,'(I5)') KDEC(7)
C
      ELSE
C
C        IDENTIFIER IS MISSING
C
         CIDENT='XXX      '               
         KDEC(7)=MINDIC
      END IF
C
C     SET ENGINEERING STATUS OF BUOY,INDICATOR FOR TYPE OF DROGUE
C     AND LENGTH OF CABLE AT WICH THE DROGUE IS ATTACHED
C
      IF(KINT(718).EQ.MINDIC) KINT(718)='1777'O
      IF(KINT(720).EQ.MINDIC) KINT(720)='1777'O
      IF(KINT(721).EQ.MINDIC) KINT(721)='1777'O
C
      kdec13 = IOR(ISHFT(KINT(718),16),ISHFT(KINT(720),8))
      KDEC(13)=IOR(kdec13,KINT(721))
C
C     SET UP : QUALITY OF PRESSURE MEASURMENTS
C              QUALITY OF SECOND WORD IN MESSAGE
C              QUALITY OF MEASURMENT OF WATER-SURFACE TEMPERATURE
C              QUALITY OF FOURTH WORD IN MESSAGE
C              QUALITY OF LOCATION
C
      IF(KINT(703).EQ.MINDIC) KINT(703)='37'O
      IF(KINT(704).EQ.MINDIC) KINT(704)='37'O
      IF(KINT(705).EQ.MINDIC) KINT(705)='37'O
      IF(KINT(706).EQ.MINDIC) KINT(706)='37'O
      IF(KINT(708).EQ.MINDIC) KINT(708)='37'O
      IF(KINT(709).EQ.MINDIC) KINT(709)='37'O
C
      kdec22 = IOR(ISHFT(KINT(703),25),ISHFT(KINT(704),20))
      kdec22 = IOR(kdec22,ISHFT(KINT(705),15))
      kdec22 = IOR(kdec22,ISHFT(KINT(706),10))
      kdec22 = IOR(kdec22,ISHFT(KINT(708), 5))
      KDEC(22) = IOR(kdec22,KINT(709))
C
C
C-----------------------------------------------------------------------
C*           3. MEAN SEA-LEVEL PRESSURE.
C               ------------------------
 300  CONTINUE
C
      IF(KINT(32).NE.MINDIC) THEN
         KDEC(25)=KINT(32)
         IF(KINT(32).LE.999) KDEC(25)=KINT(32)+10000
      END IF
C
C               STATIONLEVEL PRESSURE
C
      IF(KINT(30).NE.MINDIC) THEN
         KDEC(26)=KINT(30)
         IF(KINT(30).LE.999) KDEC(26)=KINT(30)+10000
      END IF
C     -----------------------------------------------------------------
C*             4. WIND DIRECTION AND SPEED.
C                 -------------------------
 400  CONTINUE
C
      IF(KINT(21).NE.MINDIC.AND.KINT(22).NE.MINDIC) THEN
         KDEC(27)=KINT(21)*10
cpsjun99         IF(KINT(21).EQ.99) KDEC(27)=0
cpsjun06 Next section rewritten to check for direction being unphysical,
cps      in which case MINDIC is inserted
         IF(KINT(21).EQ.99) then
cpsjun06 Changed back again: use 0 for variable wind in bufr
cps            KDEC(27)=510
            KDEC(27)=0
         else if(kint(21).gt.36) then
            kdec(27)=mindic
         else IF(KINT(21).EQ.0 ) THEN
cpsjun06 dd=0 means calm (so speed must be 0). If not, we regard it 
cps      as a typo for 360
            if(kint(22).gt.0 .and. kint(22).ne.mindic) then
               print*,'dd=0, with ff>0. Changed to 360'
               kdec(27) = 360
            end if
cps            KDEC(27)=0
cps            KDEC(28)=0
         END IF
C
         IF(KINT(9).EQ.0.OR.KINT(9).EQ.1) THEN
C
C           WIND SPEED IN (M/S)
C
crr  change accuracy of wind by multiplying by 10.
crr            KDEC(28)=KINT(22)
            KDEC(28)=KINT(22)*10
         END IF
C
         IF(KINT(9).EQ.3.OR.KINT(9).EQ.4) THEN
C           WIND SPEED IN KNOTS
crr            KDEC(28)=KINT(22)/2. + .5
            KDEC(28)=KINT(22)*10
            CALL KTOMPSI(KDEC(28))
         END IF
C
         IF(KINT(9).EQ.MINDIC) THEN
            KDEC(27)=MINDIC
            KDEC(28)=MINDIC
         END IF
      END IF
C     -----------------------------------------------------------------
C*          5.  AIR TEMPERATURE .
C               -----------------
 500  CONTINUE
C
      IF(KINT(24).NE.MINDIC.AND.KINT(25).NE.MINDIC) THEN
         IF(KINT(24).EQ.0) KDEC(29)=KINT(25)
         IF(KINT(24).EQ.1) KDEC(29)=-KINT(25)
      END IF
C
C               DEW POINT TEMPERATURE
C
      IF(KINT(27).NE.MINDIC.AND.KINT(28).NE.MINDIC) THEN
         IF(KINT(27).EQ.0) KDEC(30)=KINT(28)
         IF(KINT(27).EQ.1) KDEC(30)=-KINT(28)
C
C              RELATIVE HUMIDITY
C
         IF(KINT(27).EQ.9) KDEC(30)=KINT(28)
      END IF
C
C     ------------------------------------------------------------------
C*          6. SEA-SURFACE TEMPERATURE
C
 600  CONTINUE
C
      IF(KINT(40).NE.MINDIC.AND.KINT(41).NE.MINDIC) THEN
         IF(KINT(40).EQ.0) KDEC(31)=KINT(41)
         IF(KINT(40).EQ.1) KDEC(31)=-KINT(41)
      END IF
C
C     ------------------------------------------------------------------
C*    WAVE PERIOD
C
      IF(KINT(43).NE.MINDIC) THEN
         IF(KINT(43) .GE. 2**6 -1) THEN
            WRITE(*,*) 'PWAPWA too big (>63). Changed to missing'
         ELSE
            KDEC(34) = KINT(43)
         END IF
      END IF
C
C     ------------------------------------------------------------------
C     A- CODE
C
      KDEC(32)=KINT(34)
C     ------------------------------------------------------------------
C*           7. PRESSURE TENDENCY.
C               ------------------
 700  CONTINUE
C
      IF (KINT(35).NE.MINDIC) THEN
         KDEC(33)=KINT(35)
C     VALUE IS NEGATIVE IF CHARACTERISTIC ( A ) IS 5 - 8.
         IF (KDEC(32).GE.5.AND.KDEC(32).LE.8) KDEC(33) = -KDEC(33)
      END IF
C     ------------------------------------------------------------------
C*           8. DRIFT DIRECTION OF THE BUOY .
C               -----------------------------
 800  CONTINUE
C
      IF(KINT(716).NE.MINDIC) KDEC(34)=KINT(716)
C     -------------------------------------------------------------------
C*           9. DRIFTING SPEED ( CM/S)
C               ----------------------
cps Note that since we use descriptor 001013 in BUFR for this with scale=ref=0
cps and unit=m/s, 49 cm/s will be stored as 0 in BUFR, 50 cm/s as 1 (m/s).
 900  CONTINUE
C
      IF(KINT(715).NE.MINDIC) KDEC(35)=KINT(715)
C     ------------------------------------------------------------------
C*           
C
 1000 CONTINUE
C
C
C         
C     ------------------------------------------------------------------
C*           10. DEPTHS, TEMPERATURES, SALINITY, CURRENT.
C                ----------------------------------------
 1100 CONTINUE
C
cps ipkt=index in kint, ipkd=index in kdec (t and d are NOT abbr. for temperature and depth!)
      IPKT=55
      IPKD=41
      NTL=0
C
      DO I=1,49
         IF(KINT(IPKT  ).NE.MINDIC) THEN
            NTL=NTL+1
            KDEC(IPKD  )=KINT(IPKT) ! znznznzn
cpssep02 negative temperature is marked by adding 5000 in TnTnTnTn
            IF(KINT(IPKT+2).NE.MINDIC) THEN
               KDEC(IPKD+1)=KINT(IPKT+2) ! TnTnTnTn
               if(kint(ipkt+2).gt.5000) kdec(ipkd+1)=-kint(ipkt+2)+5000
            END IF
            IF(KINT(IPKT+4).NE.MINDIC) KDEC(IPKD+2)=KINT(IPKT+4) ! SnSnSnSn
         END IF
         IPKD=IPKD+3
         IPKT=IPKT+6
      END DO
C
      IPKT=406
      IPKD=201
      NCL=0
C
      DO I=1,49
         IF(KINT(IPKT  ).NE.MINDIC) THEN
            NCL=NCL+1
            KDEC(IPKD  )=KINT(IPKT) ! znznznzn
            IF(KINT(IPKT+1).NE.MINDIC) KDEC(IPKD+1)=KINT(IPKT+1) ! dndn
            IF(KINT(IPKT+2).NE.MINDIC) KDEC(IPKD+2)=KINT(IPKT+2) ! cncncn
         END IF
         IPKD=IPKD+3
         IPKT=IPKT+4
      END DO
C
C     -------------------------------------------------------------
C                3. Set up bufr expanded format for dribu data 
C                   ------------------------------------------

C      CALL PRTKINT(KINT,1,3000,MINDIC)
C      CALL PRTKDEC(KDEC,1,3000,MINDIC)
C
      CALL DRIBUEXP(IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ZZDEC - after DRIBUEXP, IERR=',IERR
         RETURN
      END IF

      IF (.NOT.L11.AND..NOT.L12.AND..NOT.L13) THEN
         WRITE(*,*) 'Nothing interesting to decode. Skipping report'
         RETURN
      END IF

C     -------------------------------------------------------------
C*              13. QUALITY CONTROL DRIBU REPORT.
C                   -----------------------------

      IF(.NOT.L11) GO TO 1500 
c$$$      IF(L11.AND.L12) THEN
c$$$         L12=.FALSE.
c$$$         CALL QCDRIBU( IERR )  ! If this is called, NSBTYPE must be defined
c$$$         L12=.TRUE.
c$$$      ELSE
c$$$         CALL QCDRIBU(IERR)
c$$$      END IF
C     -------------------------------------------------------------
C*              14. ENCODE BUFR MESSAGE.
C                   --------------------

cps   First make dribu surface report
c$$$      CALL BENCD( IERR )
      CALL SET_DELAYED_REPLICATION()     ! sets kdata and kdlen
      CALL SET_DESCRIPTORS()

      CALL BUFR_ENCODE()

c     Clear flag for surface report
      L11 = .FALSE.
C
C     -------------------------------------------------------------
C*              15. MOVE VALUEW INTO VALUES.
C                   ------------------------
 1500 CONTINUE
C
      IF(.NOT.L12.AND..NOT.L13) GO TO 1800 
cps Then make oceanographic report
      CALL QCCON( IERR )
C 
C     -------------------------------------------------------------
C*              16. QUALITY CONTROL DRIBU REPORT.
C                   -----------------------------
 1600 CONTINUE
C
c$$$      CALL QCDRIBU( IERR )
C     -------------------------------------------------------------
C*              17. ENCODE BUFR MESSAGE.
C                   --------------------
 1700 CONTINUE
C
c$$$      CALL BENCD( IERR )
      CALL SET_DELAYED_REPLICATION()     ! sets kdata and kdlen
      CALL SET_DESCRIPTORS()

      CALL BUFR_ENCODE()

c     Clear flags for oceanographic report
      L12 = .FALSE.
      L13 = .FALSE.
C
C------------------------------------------------------------------
 1800 CONTINUE
C 
      RETURN
      END
      SUBROUTINE FIXSS
C
C**** *FIXSS*
C
C
C     PURPOSE.
C     --------
C
C         TO FIX PROBLEM BULLETINS WITH T1T2=SS.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *FIXSS*
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
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C
      implicit none
      integer i
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
C     ------------------------------------------------------------------
C*          1. CLEAR PARITI BITS.
C              ------------------
 100  CONTINUE
C
      DO 101 I=IPT,IGS
         KCHAR(I)=IAND(KCHAR(I),127)
 101  CONTINUE
C
C*          2. CONVERT LETTERS TO FIGURES IF NEEDED.
C              -------------------------------------
 200  CONTINUE
C
      CALL REMEEE
C
      RETURN
      END
      SUBROUTINE QLALOM(IQUADR,ILAT,ILONG,MINDIC,ILAT2,ILONG2)
C
C**** *QLALOM*
C
C
C     PURPOSE.
C     --------
C
C         DECODE LATITUDE AND LONGITUDE GIVEN IN THE FORM
C         (QLALALALA) (LOLOLOLOLO) (WMO CODE TABLE 3333 FOR Q)
C
C         INPUT    :  IQUADR   THE QUADRANT OF THE GLOBE (QC)
C                  :  ILAT     LATITUDE IN DEGREES AND MINUTES
C                  :  ILONG    LONGITUDE IN DEGREES AND MINUTES
C                  :  MINDIC   MISSING DATA VALUE
C
C
C         OUTPUT   :  ILAT2  LATITUDE IN HUNDREDTHS OF DEGREE
C                            SOUTHERN LATITUDE NEGATIVE
C                  :  ILONG2:LONGITUDE IN HUNDREDTHS OF DEGREE
C                            WESTERN LONGITUDE NEGATIVE
C
C         ILAT2 AND ILONG2 ARE SET TO MISSING DATA VALUE IF
C         ANY ERRORS FOUND IN LAT,LONG OR QUADRANT
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *QLALOM(IQUADR,ILAT,ILONG,MINDIC,ILAT2,ILONG2)*
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
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C
      implicit none
      integer iquadr,ilat,ilong,mindic,ilat2,ilong2
      integer ilatsign(9),ilonsign(9),iladeg,ilamin,ilodeg,ilomin
C
      DATA ILATSIGN/1,0,-1,0,-1,0, 1,0,0/
      DATA ILONSIGN/1,0, 1,0,-1,0,-1,0,0/
C
C     ------------------------------------------------------------------
C*          1. SET LAT2 AND LONG2 TO MISSING DATA VALUE.
C              -----------------------------------------
 100  CONTINUE
C
      ILAT2=MINDIC
      ILONG2=MINDIC
C
C
C*          1.1 THE ARRAYS LATSIGN AND LONSIGN ARE USED TO DETERMINE IF
C               LAT. AND LONG. ARE NEGATIVE OR POSITIVE
 110  CONTINUE
C
C     CHECK THAT THE QUADRANT IS CORRECT
C
      IF( IQUADR.NE.1.AND.IQUADR.NE.3.AND.IQUADR.NE.5
     *     .AND.IQUADR.NE.7.AND.IQUADR.NE.9 ) RETURN
C
C*          1.2  CONVERT MINUTES TO DEGREES
C
      ILADEG = ILAT/100
      ILAMIN = ILAT - ILADEG*100
      ILAMIN = ILAMIN/6. + .5
      ILAT   = ILADEG*10 + ILAMIN
C
      ILODEG = ILONG/100
      ILOMIN = ILONG - ILODEG*100
      ILOMIN = ILOMIN/6. + .5
      ILONG  = ILODEG*10 + ILOMIN
C
C     CHECK THAT THE LATITUDE AND LONGITUDE ARE SENSIBLE
C
      IF(ILAT  .LT. 0 .OR. ILAT  .GT. 900) RETURN
      IF(ILONG .LT. 0 .OR. ILONG .GT. 1800) RETURN
C
      ILAT2=ILAT*ILATSIGN(IQUADR)*10
      ILONG2=ILONG*ILONSIGN(IQUADR)*10
C
      RETURN
      END
      SUBROUTINE QLALOD(IQUADR,ILAT,ILONG,MINDIC,ILAT2,ILONG2)
C
C**** *QLALOD*
C
C
C     PURPOSE.
C     --------
C
C         DECODE LATITUDE AND LONGITUDE GIVEN IN THE FORM
C         (QLALALALALA) (LOLOLOLOLOLO) (WMO CODE TABLE 3333 FOR Q)
C
C         INPUT    :  IQUADR   THE QUADRANT OF THE GLOBE (QC)
C                  :  ILAT     LATITUDE IN  THOUSANDTHS OF DEG.
C                  :  ILONG    LONGITUDE IN THOUSANDTHS OF DEG.
C                  :  MINDIC   MISSING DATA VALUE
C
C
C         OUTPUT   :  ILAT2  LATITUDE IN THOUSANDTHS OF DEGREE
C                            SOUTHERN LATITUDE NEGATIVE
C                  :  ILONG2:LONGITUDE IN THOUSANDTHS OF DEGREE
C                            WESTERN LONGITUDE NEGATIVE
C
C         ILAT2 AND ILONG2 ARE SET TO MISSING DATA VALUE IF
C         ANY ERRORS FOUND IN LAT,LONG OR QUADRANT
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *QLALOD(IQUADR,ILAT,ILONG,MINDIC,ILAT2,ILONG2)*
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
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C
      implicit none
      integer iquadr,ilat,ilong,mindic,ilat2,ilong2
      integer latsign,lonsign
C
C     ------------------------------------------------------------------
C*          1. SET LAT2 AND LONG2 TO MISSING DATA VALUE.
C              -----------------------------------------
 100  CONTINUE
C
      ILAT2=MINDIC
      ILONG2=MINDIC
C
      if(iquadr.eq.1) then
         latsign=1
         lonsign=1
      elseif(iquadr.eq.7) then
         latsign=1
         lonsign=-1
      elseif(iquadr.eq.3) then
         latsign=-1
         lonsign=1
      elseif(iquadr.eq.5) then
         latsign=-1
         lonsign=-1
      else
         return
      end if
C
C     CHECK THAT THE LATITUDE AND LONGITUDE ARE SENSIBLE
C
      IF(ILAT  .LT. 0 .OR. ILAT  .GT. 90000) RETURN
      IF(ILONG .LT. 0 .OR. ILONG .GT. 180000) RETURN
C
      ILAT2=ILAT*LATSIGN
      ILONG2=ILONG*LONSIGN
C
      RETURN
      END
      SUBROUTINE DRIBUEXP( IERR )                                       
C
C**** *DRIBUEXP*                                                        
C
C
C     PURPOSE.
c
C         SET UP BUFR EXPANDED FORMAT FOR DRIBU DATA.
c
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DRIBUEXP( IERR )*
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
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       NOV 1991.
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
      integer i,imonth,scale,ist,inc,iv

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'       ! values,valuew
      INCLUDE 'comident.f'
      INCLUDE 'comkeyc.f'
      INCLUDE 'comwtc.f'      ! cval
      include 'comdrau.f'     ! L11-L33,NTL,NCL,LaLaLaLaLa
      include 'comkey.f'      ! nyear,nmonth,nday,nhour,nminute
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES
C                --------------------

      NSUB=1
      N=NSUB
      L11=.FALSE.
      L12=.FALSE.
      L13=.FALSE.

cps valueS is used for surface report, valueW is used for oceanographic report
      DO I=1,JP22
         VALUES(I,NSUB)=999999.
         VALUEW(I,NSUB)=999999.
      END DO

      if(kint(21).ne.mindic.or.kint(22).ne.mindic.or.   ! dd,ff
     1     kint(25).ne.mindic.or.kint(28).ne.mindic.or. ! T,Td
     1     kint(30).ne.mindic.or.kint(32).ne.mindic.or. ! P0,P
     1     kint(34).ne.mindic.or.kint(35).ne.mindic.or. ! a,ppp
     1     kint(43).ne.mindic.or.kint(44).ne.mindic.or. ! PwaPwa,HwaHwa
     1     kint(46).ne.mindic.or.kint(48).ne.mindic.or. ! PwaPwaPwa,HwaHwaHwa
     1     kint(41).ne.mindic) then                     ! Tw
         L11=.TRUE.
      else
         GOTO 200
      endif

C*            2.1  SET UP SURFACE OBSERVATION.
C                  ---------------------------

      IF(KDEC( 7).NE.MINDIC) VALUES(1,NSUB)=FLOAT(KDEC(7))
                             VALUES(2,NSUB)=0.  ! Type of station, 0=automatic
      call get_year(kdec(1),kint(5),kint(6),NYEAR)
                             VALUES(3,NSUB)=FLOAT(NYEAR)         ! Year
      IF(KINT(5).NE.MINDIC)  VALUES(4,NSUB)=FLOAT(KINT(5))       ! Month
                             VALUES(5,NSUB)=FLOAT(KDEC(1))       ! Day
                             VALUES(6,NSUB)=FLOAT(KDEC(2))       ! Hour
                             VALUES(7,NSUB)=FLOAT(KDEC(9))       ! Minute
                             if (LaLaLaLaLa) then
                                SCALE=1000.
                             else
                                SCALE=100.
                             end if
                             VALUES(8,NSUB)=FLOAT(KDEC(5))/SCALE
                             VALUES(9,NSUB)=FLOAT(KDEC(6))/SCALE
      IF(KDEC(34).NE.MINDIC) VALUES(10,NSUB)=FLOAT(KDEC(34))     ! dBdB
      IF(KDEC(35).NE.MINDIC) VALUES(11,NSUB)=FLOAT(KDEC(35))     ! VBVB
      IF(KDEC(26).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(26))*10  ! P0P0P0P0
      IF(KDEC(33).NE.MINDIC) VALUES(13,NSUB)=FLOAT(KDEC(33))*10. ! ppp
      IF(KDEC(32).NE.MINDIC) VALUES(14,NSUB)=FLOAT(KDEC(32))     ! a
      IF(KDEC(27).NE.MINDIC) VALUES(15,NSUB)=FLOAT(KDEC(27))     ! dd
crr  changed accuracy of wind speed
      IF(KDEC(28).NE.MINDIC) VALUES(16,NSUB)=FLOAT(KDEC(28))/10. ! ff
      IF(KDEC(29).NE.MINDIC) VALUES(17,NSUB)=FLOAT(KDEC(29))/10.+273.15  ! TTT 
      IF(KDEC(30).NE.MINDIC.AND.KINT(27).EQ.9)
     *                       VALUES(18,NSUB)=FLOAT(KDEC(30))     ! UUU
      IF(KDEC(31).NE.MINDIC) VALUES(19,NSUB)=FLOAT(KDEC(31))/10.+273.15  ! TwTwTw
      IF(KDEC(25).NE.MINDIC) VALUES(20,NSUB)=FLOAT(KDEC(25))*10. ! PPPP
      IF(KDEC(30).NE.MINDIC.AND.KINT(27).NE.9)
     *                       VALUES(21,NSUB)=FLOAT(KDEC(30))/10.+273.15  ! TdTdTd
      IF(KDEC(34).NE.MINDIC) VALUES(22,NSUB)=FLOAT(KDEC(34))     ! PwaPwa
      IF(KINT(44).NE.MINDIC) VALUES(23,NSUB)=FLOAT(KINT(44))*.5  ! HwaHwa
      IF(KINT(46).NE.MINDIC) VALUES(22,NSUB)=FLOAT(KINT(46))*.1  ! PwaPwaPwa
      IF(KINT(48).NE.MINDIC) VALUES(23,NSUB)=FLOAT(KINT(48))*.1  ! HwaHwaHWa

      M=23

 200  CONTINUE

      IF (NTL.EQ.0.AND.NCL.EQ.0) GOTO 300

C*        DRIBU SUBSURFACE REPORT
C         -----------------------

      IF(KDEC( 7).NE.MINDIC) VALUEW(1,NSUB)=FLOAT(KDEC(7))  ! A1bwnbnbnb
      IF(KDEC(34).NE.MINDIC) VALUEW(2,NSUB)=FLOAT(KDEC(34)) ! dBdB
      IF(KDEC(35).NE.MINDIC) VALUEW(3,NSUB)=FLOAT(KDEC(35)) ! VBVB
                             VALUEW(4,NSUB)=0.
      call get_year(kdec(1),kint(5),kint(6),NYEAR)
                             VALUEW(5,NSUB)=FLOAT(NYEAR)
      IF(KINT(5).NE.MINDIC)  VALUEW(6,NSUB)=FLOAT(KINT(5))
                             VALUEW(7,NSUB)=FLOAT(KDEC(1))
                             VALUEW(8,NSUB)=FLOAT(KDEC(2))
                             VALUEW(9,NSUB)=FLOAT(KDEC(9))
                             IF (LaLaLaLaLa) THEN
                                SCALE=1000.
                             ELSE
                                SCALE=100.
                             END IF
                             VALUEW(10,NSUB)=FLOAT(KDEC(5))/SCALE
                             VALUEW(11,NSUB)=FLOAT(KDEC(6))/SCALE

      IF (NTL.GT.0) THEN
         L12=.TRUE.         !  DRIFTER SUBSURFACE Z,T,S REPORT
         VALUEW(12,NSUB)=0.                    ! '7' in 8887k2
         IF(KINT(53).NE.MINDIC) THEN
            VALUEW(13,NSUB)=FLOAT(KINT(53))    ! k2
         ELSE 
            VALUEW(13,NSUB)=7.                 ! missing value, BUFR table 002033
         ENDIF
         VALUEW(14,NSUB)=FLOAT(NTL)            ! Number of depths

         IST=41
         INC= 3
         IV =15

         DO I=1,NTL         ! SEA DEPTH,TEMPERATURES,SALINITY
            IF(KDEC(IST).NE.MINDIC) THEN
               VALUEW(IV,NSUB)=FLOAT(KDEC(IST)) ! znznznzn
               IF(KDEC(IST+1).NE.MINDIC) THEN
                  VALUEW(IV+1,NSUB)=FLOAT(KDEC(IST+1))/100.+273.15 ! TnTnTnTn
               END IF
               IF(KDEC(IST+2).NE.MINDIC) THEN
                  VALUEW(IV+2,NSUB)=FLOAT(KDEC(IST+2))/100.        ! SnSnSnSn
               END IF
               IV=IV+3
            END IF
            IST=IST+INC
         END DO
      END IF

      IF (NCL.GT.0) THEN
         L13=.TRUE.         !  DRIFTER SUBSURFACE Z,DD,CCC REPORT (CURRENT)

         IST=201
         INC= 3
         IF (NTL.EQ.0) IV=12

         IF(KINT(404).NE.MINDIC) VALUEW(IV,NSUB)=KINT(404) ! k3
         IV=IV+1
         VALUEW(IV,NSUB)=FLOAT(NCL) ! number of depths
         IV=IV+1

         DO I=1,NCL
            IF(KDEC(IST).NE.MINDIC) THEN
               VALUEW(IV,NSUB)=FLOAT(KDEC(IST)) ! znznznzn
               IF(KDEC(IST+1).NE.MINDIC) THEN
                  VALUEW(IV+1,NSUB)=FLOAT(KDEC(IST+1)*10) ! dndn
               END IF
               IF(KDEC(IST+2).NE.MINDIC) THEN
                  VALUEW(IV+2,NSUB)=FLOAT(KDEC(IST+2))/100. ! cncncn
               END IF
               IV=IV+3
            END IF
            IST=IST+INC
         END DO
      END IF

 300  CONTINUE

      MS=IV-1
C
cpsjan07 In nix_drau the following section 1 data is set in bencd
      CALL GET_YEAR(KDEC(1),KINT(5),KINT(6),NYEAR)
      IF(NYEAR.EQ.MINDIC) THEN
         WRITE(*,*) 'YEAR not found or cannot make sense.',
     *        ' Report rejected'
         IERR = 1
      ENDIF
      NMONTH =KINT(5)
      NDAY   =KDEC(1)
      NHOUR  =KDEC(2)
      NMINUTE=KDEC(9)

      RETURN
C
      END
      SUBROUTINE BATHYEXP( IERR )
C
C**** *BATHYEXP*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR BATHY DATA.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BATHYEXP( IERR )*
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
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C                                                                     
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C
C     MODIFICATIONS.
C     --------------
C
cpsjan07 Reordered kint and kdec: subsurface levels start at 100 (not 18 and 38),
cps      and packed consecutively in kdec (2 indices per level, not 5). 
cps      kint(338-361) shifted to 38-61. See kint.bath
C
C
      implicit none
      integer ierr
      integer i,imonth,scale,ist,inc,iv

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comident.f'
      INCLUDE 'comwtc.f'      ! cval
      include 'comdrau.f'     ! L11-L33,NTL,NCL.LaLaLaLaLa
      include 'comkey.f'      ! nyear,nmonth,nday,nhour,nminute
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES
C                --------------------

      NSUB=1
      N=NSUB
      L21=.FALSE.
      L22=.FALSE.

cps valueS is used for surface report, valueW is used for oceanographic report
      DO I=1,JP22
         VALUES(I,NSUB)=999999.
         VALUEW(I,NSUB)=999999.
      END DO

      IF(KDEC(26).NE.MINDIC.OR.KDEC(27).NE.MINDIC.OR.
     1     KDEC(28).NE.MINDIC) THEN   ! dd/ff/TnTnTn
         L21=.TRUE.
      ELSE
         GOTO 200
      END IF

C*            2.1  SET UP SURFACE OBSERVATION.
C                  ---------------------------

      VALUES(1,NSUB)=NSUB*1000+9
      CVAL(NSUB)=CHAR(IDENT(1))//CHAR(IDENT(2))//CHAR(IDENT(3))//
     1     CHAR(IDENT(4))//CHAR(IDENT(5))//CHAR(IDENT(6))//
     2     CHAR(IDENT(7))//CHAR(IDENT(8))//CHAR(IDENT(9))
      call get_year(kdec(1),kint(2),kint(3),NYEAR)
      VALUES(2,NSUB)=FLOAT(NYEAR)                          ! Year
      IF(KINT(2).NE.MINDIC)  VALUES(3,NSUB)=FLOAT(KINT(2)) ! Month
      VALUES(4,NSUB)=FLOAT(KDEC(1))                        ! Day
      VALUES(5,NSUB)=FLOAT(KDEC(2))                        ! Hour
      VALUES(6,NSUB)=FLOAT(KDEC(9))                        ! Minute
      IF (LaLaLaLaLa) THEN
         SCALE=1000.
      ELSE
         SCALE=100.
      END IF
      VALUES(7,NSUB)=FLOAT(KDEC(5))/SCALE                  ! Latitude
      VALUES(8,NSUB)=FLOAT(KDEC(6))/SCALE                  ! Longitude
      IF(KDEC(26).NE.MINDIC) VALUES(9,NSUB)=FLOAT(KDEC(26))! dd
crr  changed accuracy of wind speed
      IF(KDEC(27).NE.MINDIC) THEN
         VALUES(10,NSUB)=FLOAT(KDEC(27))/10.               ! ff
      END IF
      IF(KDEC(28).NE.MINDIC) THEN
         VALUES(11,NSUB)=FLOAT(KDEC(28))/10.+273.15        ! TTT
      END IF 
cps T0T0T0 (kdec(101)) is temperature "at surface". But note that in actual bathys 
cps z0z0 seems to vary between 00, 01 and 02. T0T0T0 is the only sea temperature
cps that is included in bathy surface report (it is also included in bathy 
cps oceanographic report, here together with z0z0). 
cps Note that sea temperature is calculated with two decimals below, while when
cps using rdbufr only one decimal is obtained (which is in fact more reasonable,
cps as in a bathy report all temperatures are given with one decimal only)
      IF(KDEC(101).NE.MINDIC) THEN
         VALUES(12,NSUB)=FLOAT(KDEC(101))/100.+273.15     ! T0T0T0 (TwTwTw)
      END IF

      M=32

 200  CONTINUE

      IF (NTL.EQ.0) GOTO 300

C*           3. BATHY SUB SURFACE TEMPERATURES.
C               -------------------------------
      L22=.TRUE.

C     First 8 elements are identical to surface report
      VALUEW(1,NSUB)=NSUB*1000+9
      CVAL(NSUB)=CHAR(IDENT(1))//CHAR(IDENT(2))//CHAR(IDENT(3))//
     1     CHAR(IDENT(4))//CHAR(IDENT(5))//CHAR(IDENT(6))//
     2     CHAR(IDENT(7))//CHAR(IDENT(8))//CHAR(IDENT(9))
      call get_year(kdec(1),kint(2),kint(3),NYEAR)
      VALUEW(2,NSUB)=FLOAT(NYEAR)                          ! Year
      IF(KINT(2).NE.MINDIC)  VALUEW(3,NSUB)=FLOAT(KINT(2)) ! Month
      VALUEW(4,NSUB)=FLOAT(KDEC(1))                        ! Day
      VALUEW(5,NSUB)=FLOAT(KDEC(2))                        ! Hour
      VALUEW(6,NSUB)=FLOAT(KDEC(9))                        ! Minute
      IF (LaLaLaLaLa) THEN
         SCALE=1000.
      ELSE
         SCALE=100.
      END IF
      VALUEW(7,NSUB)=FLOAT(KDEC(5))/SCALE                  ! Latitude
      VALUEW(8,NSUB)=FLOAT(KDEC(6))/SCALE                  ! Longitude
      VALUEW(9,NSUB)=FLOAT(KDEC(29))                       ! k1
      VALUEW(10,NSUB)=FLOAT(NTL)                           ! delayed replication

      IST=100
      INC=2
      IV=11

      DO I=1,NTL
         IF(KDEC(IST).NE.MINDIC) THEN
            VALUEW(IV,NSUB)=FLOAT(KDEC(IST))                    ! znzn
            IF(KDEC(IST+1).NE.MINDIC) THEN
               VALUEW(IV+1,NSUB)=FLOAT(KDEC(IST+1))/100.+273.15 ! TnTnTn
            END IF
            IV=IV+2
         END IF
         IST=IST+INC
      END DO

      IF(KINT(42).NE.MINDIC) THEN
         VALUEW(IV,NSUB)=FLOAT(KINT(42))                        ! k5
      END IF
      IV=IV+1
      IF(KDEC(36).NE.MINDIC) THEN
         VALUEW(IV,NSUB)=FLOAT(KDEC(36))                        ! DcDc
      END IF
      IV=IV+1
      IF(KDEC(37).NE.MINDIC) THEN
         VALUEW(IV,NSUB)=FLOAT(KDEC(37))/100.                   ! VcVc
      END IF

 300  CONTINUE

      MS=IV

cpssep06 In nix_drau the following section 1 data is set in bencb
      CALL GET_YEAR(KDEC(1),KINT(2),KINT(3),NYEAR)
      IF(NYEAR.EQ.MINDIC) THEN
         WRITE(*,*) 'YEAR not found. Report rejected'
         IERR = 1
      ENDIF
      NMONTH =KINT(2)
      NDAY   =KDEC(1)
      NHOUR  =KDEC(2)
      NMINUTE=KDEC(9)

      RETURN
      END
      SUBROUTINE TESACEXP( IERR )
C
C**** *TESACEXP*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR TESAC DATA.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *TESACEXP( IERR )*
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
C     REFERENCE.
C     ----------
C
C          NONE.
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
cpsjan07 Reordered kint and kdec to get space for more subsurface temperature
cps      levels. See kint.tesa

      implicit none
      integer ierr
      integer i,imonth,scale,ist,inc,iv,iv_ncl

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comident.f'
      INCLUDE 'comwtc.f'      ! cval
      include 'comdrau.f'     ! L11-L33,NTL,NCL,LaLaLaLaLa
      include 'comkey.f'      ! nyear,nmonth,nday,nhour,nminute
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES
C                ----------------------------------

      NSUB=1
      N=NSUB
      IV=12
      L31=.FALSE.
      L32=.FALSE.
      L33=.FALSE.

cps valueS is used for surface report, valueW is used for oceanographic report
      DO I=1,JP22
         VALUES(I,NSUB)=999999.
         VALUEW(I,NSUB)=999999.
      END DO

      IF(KDEC(26).NE.MINDIC.OR.KDEC(27).NE.MINDIC.OR.
     1     KDEC(28).NE.MINDIC) THEN   ! dd/ff/TnTnTn
         L31=.TRUE.
      ELSE
         GOTO 200
      END IF

C*            2.1  SET UP SURFACE OBSERVATION.
C                  ---------------------------

      VALUES(1,NSUB)=NSUB*1000+9
      CVAL(NSUB)=CHAR(IDENT(1))//CHAR(IDENT(2))//CHAR(IDENT(3))//
     1     CHAR(IDENT(4))//CHAR(IDENT(5))//CHAR(IDENT(6))//
     2     CHAR(IDENT(7))//CHAR(IDENT(8))//CHAR(IDENT(9))
      call get_year(kdec(1),kint(2),kint(3),NYEAR)
      VALUES(2,NSUB)=FLOAT(NYEAR)                          ! Year
      IF(KINT(2).NE.MINDIC)  VALUES(3,NSUB)=FLOAT(KINT(2)) ! Month
      VALUES(4,NSUB)=FLOAT(KDEC(1))                        ! Day
      VALUES(5,NSUB)=FLOAT(KDEC(2))                        ! Hour
      VALUES(6,NSUB)=FLOAT(KDEC(9))                        ! Minute
      IF (LaLaLaLaLa) THEN
         SCALE=1000.
      ELSE
         SCALE=100.
      END IF
      VALUES(7,NSUB)=FLOAT(KDEC(5))/SCALE                  ! Latitude
      VALUES(8,NSUB)=FLOAT(KDEC(6))/SCALE                  ! Longitude
      IF(KDEC(26).NE.MINDIC) VALUES(9,NSUB)=FLOAT(KDEC(26))! dd
crr  changed accuracy of wind speed
      IF(KDEC(27).NE.MINDIC) THEN
         VALUES(10,NSUB)=FLOAT(KDEC(27))/10.               ! ff
      END IF
      IF(KDEC(28).NE.MINDIC) THEN
         VALUES(11,NSUB)=FLOAT(KDEC(28))/10.+273.15        ! TTT
      END IF 

      M=32

 200  CONTINUE

      IF(NTL.EQ.0.AND.NCL.EQ.0) GOTO 300


C*           TESAC SUBSURFACE REPORT
C            -----------------------

      VALUEW(1,NSUB)=NSUB*1000+9
      CVAL(NSUB)=CHAR(IDENT(1))//CHAR(IDENT(2))//CHAR(IDENT(3))//
     1           CHAR(IDENT(4))//CHAR(IDENT(5))//CHAR(IDENT(6))//
     2           CHAR(IDENT(7))//CHAR(IDENT(8))//CHAR(IDENT(9))

      call get_year(kdec(1),kint(2),kint(3),NYEAR)
      VALUEW(2,NSUB)=FLOAT(NYEAR)
      IF(KINT(2).NE.MINDIC) VALUEW(3,NSUB)=FLOAT(KINT(2))
                            VALUEW(4,NSUB)=FLOAT(KDEC(1))
                            VALUEW(5,NSUB)=FLOAT(KDEC(2))
                            VALUEW(6,NSUB)=FLOAT(KDEC(9))
                            IF (LaLaLaLaLa) THEN
                               SCALE=1000.
                            ELSE
                               SCALE=100.
                            END IF
                            VALUEW(7,NSUB)=FLOAT(KDEC(5))/SCALE
                            VALUEW(8,NSUB)=FLOAT(KDEC(6))/SCALE

      IF (NTL.GT.0) THEN
         L32=.TRUE.         !  TESAC SUBSURFACE Z,T,S REPORT

         IF(KDEC(30).NE.MINDIC) VALUEW(9,NSUB)=FLOAT(KDEC(30)) ! k1
         IF(KINT(18).NE.MINDIC) VALUEW(10,NSUB)=FLOAT(KINT(18)) ! k2

         IST=38

         DO I=1,NTL   
            IF(KDEC(IST).NE.MINDIC) THEN
               VALUEW(IV,NSUB)=FLOAT(KDEC(IST))                    ! znznznzn
               IF(KDEC(IST+1).NE.MINDIC) THEN
                  VALUEW(IV+1,NSUB)=FLOAT(KDEC(IST+1))/100.+273.15 ! TnTnTnTn
               END IF
               IF(KDEC(IST+2).NE.MINDIC) THEN
                  VALUEW(IV+2,NSUB)=FLOAT(KDEC(IST+2))/100.        ! SnSnSnSn
               END IF
               IV=IV+3
            ELSE
               NTL=NTL-1
            END IF
            IST=IST+3
         END DO
         VALUEW(11,NSUB)=FLOAT(NTL)

      END IF

      IF(NCL.NE.0) THEN
         L33=.TRUE.             !  TESAC SUBSURFACE Z,DD,CCC REPORT (CURRENT)

         IF(KINT(5001).NE.MINDIC) VALUEW(IV,NSUB)=FLOAT(KINT(5001))

         IV_NCL=IV+1
         IV=IV+2
         IST=2000

         DO I=1,NCL
            IF(KDEC(IST).NE.MINDIC) THEN
               VALUEW(IV,NSUB)=FLOAT(KDEC(IST)) ! depth, znznznzn
               IF(KDEC(IST+1).NE.MINDIC) THEN
                  VALUEW(IV+1,NSUB)=FLOAT(KDEC(IST+1)) ! dndn
               END IF
               IF(KDEC(IST+2).NE.MINDIC) THEN
                  VALUEW(IV+2,NSUB)=FLOAT(KDEC(IST+2))/100. ! cncncn
               END IF
               IV=IV+3
            ELSE
               NCL=NCL-1
            END IF
            IST=IST+3
         END DO
         VALUEW(IV_NCL,NSUB)=FLOAT(NCL)
 
      END IF
       
 300  CONTINUE

      MS=IV
 
cpssep06 In nix_drau the following section 1 data is set in benct
      CALL GET_YEAR(KDEC(1),KINT(2),KINT(3),NYEAR)
      IF(NYEAR.EQ.MINDIC) THEN
         WRITE(*,*) 'YEAR not found. Report rejected'
         IERR = 1
      ENDIF
      NMONTH =KINT(2)
      NDAY   =KDEC(1)
      NHOUR  =KDEC(2)
      NMINUTE=KDEC(9)

      RETURN
      END

      subroutine set_descriptors
      implicit none
      include 'comdescr.f'   ! ktdlen,ktdlst
      include 'comdrau.f'    ! L11-L33,NTL,NCL
      integer i1
      
      do i1=1,70
         ktdlst(i1) = 0
      end do

      if (L11) then            ! dribu (buoy) surface report
         ktdlen = 6
         ktdlst(1) = 301055
         ktdlst(2) = 308006
         ktdlst(3) = 010051    ! PPPP
         ktdlst(4) = 012006    ! TdTdTd
         ktdlst(5) = 022011    ! PERIOD OF WAVES
         ktdlst(6) = 022021    ! HEIGHT OF WAVES
c$$$      else if (L12.and.L13) then
      else if (L12) then       ! dribu oceanographic Z,T(,S) report
         ktdlst(1) = 301035
         if (NTL.le.254) then
            ktdlen = 2
            ktdlst(2) = 306004
         else
            ktdlen = 8
            ktdlst(2) = 002032
            ktdlst(3) = 002033
            ktdlst(4) = 103000
            ktdlst(5) = 031002
            ktdlst(6) = 007062
            ktdlst(7) = 022043
            ktdlst(8) = 022062
         end if
         if (L13) then          ! dribu current
C     Don't check for NCL > 254, which is highly unlikely
            ktdlen = ktdlen + 1
            ktdlst(ktdlen) = 306005
         end if
      else if (L13) then       ! dribu (pure) current report
         ktdlen = 2
         ktdlst(1) = 301035
         ktdlst(2) = 306005
      else if (L21) then       ! bathy surface report
         ktdlen = 8
         ktdlst(1) = 001011
         ktdlst(2) = 301011
         ktdlst(3) = 301012
         ktdlst(4) = 301021
         ktdlst(5) = 011001
         ktdlst(6) = 011002
         ktdlst(7) = 012004
         ktdlst(8) = 022042
      else if (L22) then       ! bathy oceanographic
         if (NTL.le.254) then
            ktdlen = 1
            ktdlst(1) = 315001
         else
            ktdlen = 9
            ktdlst(1) = 001011
            ktdlst(2) = 301011
            ktdlst(3) = 301012
            ktdlst(4) = 301023
            ktdlst(5) = 002032
            ktdlst(6) = 102000
            ktdlst(7) = 031002
            ktdlst(8) = 007062
            ktdlst(9) = 022042
         end if    
      else if (L31) then        ! tesac surface report
         ktdlen = 8
         ktdlst(1) = 001011
         ktdlst(2) = 301011
         ktdlst(3) = 301012
         ktdlst(4) = 301021
         ktdlst(5) = 011001
         ktdlst(6) = 011002
         ktdlst(7) = 012004
         ktdlst(8) = 022042
      else if (L32) then        ! tesac oceanographic
         if (NTL.le.254) then
            ktdlen = 1
            ktdlst(1) = 315002
         else
            ktdlen = 11
            ktdlst(1) = 001011
            ktdlst(2) = 301011
            ktdlst(3) = 301012
            ktdlst(4) = 301023
            ktdlst(5) = 002032
            ktdlst(6) = 002033
            ktdlst(7) = 103000
            ktdlst(8) = 031002
            ktdlst(9) = 007062
            ktdlst(10) = 022043
            ktdlst(11) = 022062
         end if    
         if (L33) then          ! tesac current
            if (NCL.le.254) then
               ktdlen = ktdlen + 1
               ktdlst(ktdlen) = 306005
            else
               ktdlst(ktdlen+1) = 002031
               ktdlst(ktdlen+2) = 103000
               ktdlst(ktdlen+3) = 031002
               ktdlst(ktdlen+4) = 007062
               ktdlst(ktdlen+5) = 022004
               ktdlst(ktdlen+6) = 022031
               ktdlen = ktdlen + 6
            end if
         end if
      end if
c     We do not make surface bathy or tesac reports. Should we?
c     They are very rare, but maybe it is not much work required.

c     301055 expands to
c$$$     1  001005  BUOY/PLATFORM IDENTIFIER
c$$$     2  002001  TYPE OF STATION
c$$$     3  004001  YEAR
c$$$     4  004002  MONTH
c$$$     5  004003  DAY
c$$$     6  004004  HOUR
c$$$     7  004005  MINUTE
c$$$     8  005001  LATITUDE (HIGH ACCURACY)
c$$$     9  006001  LONGITUDE (HIGH ACCURACY)
c$$$    10  001012  DIRECTION OF MOTION OF MOVING OBSERVING PLATFORM
c$$$    11  001014  PLATFORM DRIFT SPEED (HIGH PRECISION)
c     308006 expands to
c$$$    12  010004  PRESSURE
c$$$    13  010061  3 HOUR PRESSURE CHANGE
c$$$    14  010063  CHARACTERISTIC OF PRESSURE TENDENCY
c$$$    15  011001  WIND DIRECTION
c$$$    16  011002  WIND SPEED
c$$$    17  012004  DRY BULB TEMPERATURE AT 2M
c$$$    18  013003  RELATIVE HUMIDITY
c$$$    19  022042  SEA/WATER TEMPERATURE
c     306004 expands to
c$$$    14  002032  INDICATOR FOR DIGITIZATION
c$$$    15  002033  METHOD OF SALINITY/DEPTH MEASUREMENT
ccc         103000  Delayed replication of 3 descriptors
c$$$    16  031001  DELAYED DESCRIPTOR REPLICATION FACTOR
c$$$    17  007062  DEPTH BELOW SEA/WATER SURFACE
c$$$    18  022043  SEA/WATER TEMPERATURE
c$$$    19  022062  SALINITY
c     315001 expands to
c$$$     1  001011  SHIP OR MOBILE LAND STATION IDENTIFIER
c$$$     2  004001  YEAR
c$$$     3  004002  MONTH
c$$$     4  004003  DAY
c$$$     5  004004  HOUR
c$$$     6  004005  MINUTE
c$$$     7  005002  LATITUDE (COARSE ACCURACY)
c$$$     8  006002  LONGITUDE (COARSE ACCURACY)
c$$$     9  002032  INDICATOR FOR DIGITIZATION
ccc         102000  Delayed replication of 2 descriptors
c$$$    10  031001  DELAYED DESCRIPTOR REPLICATION FACTOR
c$$$    11  007062  DEPTH BELOW SEA/WATER SURFACE
c$$$    12  022042  SEA/WATER TEMPERATURE
c     315002 expands to
c$$$     1  001011  SHIP OR MOBILE LAND STATION IDENTIFIER
c$$$     2  004001  YEAR
c$$$     3  004002  MONTH
c$$$     4  004003  DAY
c$$$     5  004004  HOUR
c$$$     6  004005  MINUTE
c$$$     7  005002  LATITUDE (COARSE ACCURACY)
c$$$     8  006002  LONGITUDE (COARSE ACCURACY)
c$$$     9  002032  INDICATOR FOR DIGITIZATION
c$$$    10  002033  METHOD OF SALINITY/DEPTH MEASUREMENT
ccc         102000  Delayed replication of 3 descriptors
c$$$    11  031001  DELAYED DESCRIPTOR REPLICATION FACTOR
c$$$    12  007062  DEPTH BELOW SEA/WATER SURFACE
c$$$    13  022043  SEA/WATER TEMPERATURE
c$$$    14  022062  SALINITY
c     306002 expands to
c$$$     1  002031  DURATION AND TIME OF CURRENT MEASUREMENT [CODE TABLE 2031] 0 0 5
c$$$        103000
c$$$     2  031001  DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 8
c$$$     3  007062  DEPTH BELOW SEA/WATER SURFACE [M] 1 0 17
c$$$     4  022004  DIRECTION OF CURRENT [DEGREE TRUE] 0 0 9
c$$$     5  022031  SPEED OF CURRENT [M/S] 2 0 13  

      end subroutine set_descriptors
C----------------------------------------------------------------------
      subroutine set_delayed_replication()
C     Set kdata and kdlen.
      implicit none

      include 'comrepl.f'    ! kdata,kdlen
      include 'comdrau.f'    ! L11-L33,NTL,NCL

      data kdlen /100/       ! Dimension of kdata array
      if (L11) then        ! surface report dribu, no replication
         kdlen = 0
      else if ((L12.and.L13) .or. (L32.and.L33)) then
         kdata(1) = NTL    ! number of subsurface temperature levels
         kdata(2) = NCL    ! number of subsurface current levels
         kdlen   = 2
      else if (L12.or.L22.or.L32) then  ! oceanographic subsurface Z,T(,S) report
         kdata(1) = NTL
         kdlen    = 1
      else if (L13.or.L33) then         ! oceanographic subsurface Z,DD,CCC report
         kdata(1) = NCL
         kdlen    = 1
      else if (L21.or.L31) then
         write(*,*) 'bathy/tesac surface report not implemented'
         call exit(2)
      else
         write(*,*) 'Internal error: No L11-L33 set.'
         call exit(2)
      end if

      end subroutine set_delayed_replication
C----------------------------------------------------------------------

      subroutine bufr_encode()
      implicit none
      include 'comdrau.f'      ! L11-L33
      integer ierr,i1,iiyear

      include 'parameter.f'  ! using jp1,jp2,jbufl
      include 'comwork.f'    ! kdec (and to be able to call prtkint/kdec/values)
      include 'comwt.f'      ! values,nsub

      include 'comwtc.f'     ! cval,cunit
      include 'combufren.f'  ! kbufl,kbuff
      include 'comdescr.f'   ! ktdlen,ktdlst
      include 'comrepl.f'    ! kdata,kdlen
      include 'comkey.f'     ! ncorr,ncorn(4),nyear,nmonth,nday,nhour,nminute

      real*8 new_values(jp22),rvind
      parameter (rvind=1.7E38)

      character*80 cvals(jp22)
      integer kelem,kvals
      integer ksec0(3),ksec1(40),ksec2(64),ksec3(4),ksec4(2)
      logical ocea      ! true if an oceanographic report is to be produced

C     BUXDES variables, for debugging
      INTEGER ntdexl
      INTEGER ntdexp(1000)
      CHARACTER*64 CNAMES(1000)
      CHARACTER*24 CUNITS(1000)

c$$$      print*,'KDATA,KDLEN=', kdata(1), kdata(2),kdlen
c$$$      CALL PRTKINT(KINT,1,100,MINDIC)
c$$$      CALL PRTKDEC(KDEC,1,100,MINDIC)
c$$$      CALL prtvalues(50)

c     bufr_encode is supposed called first for surface section if this exists,
c     then for oceanographic section if this exists, where L11/L21/L31 is
c     supposed reset to .false. after surface section has been processed.
      ocea = .false.
      if (.not. (L11.or.L21.or.L31)) ocea = .true.

      do i1=1,jp22
         if ( values(i1,1).eq.999999.) then
            new_values(i1) = rvind
         else
            new_values(i1) = values(i1,1)
         end if
      end do

      do i1=1,jp1
         cvals(i1) = cval(i1)
      end do

C     ktdlen and ktdlst was initialized in subroutine set_descriptors,
C     kdata and kdlen was set in set_delayed_replication

      kelem = jp22
      kvals = jp22

      ncorr = 0
      do i1=1,4
         if (ncorn(i1).gt.ncorr) then
            ncorr = ncorn(i1)
         end if
      end do

      ksec0(1) = 0   ! LENGTH OF SECTION 0, not used by bufren
      ksec0(2) = 0   ! TOTAL LENGTH OF BUFR MESSAGE, not used by bufren
      ksec0(3) = 4   ! BUFR EDITION NUMBER

      ksec1(1) = 22  ! LENGTH OF SECTION 1, must be set by user
      ksec1(2) = 4   ! BUFR EDITION NUMBER
      ksec1(3) = 88  ! ORIGINATING CENTRE
      ksec1(4) = ncorr ! UPDATE SEQUENCE NUMBER
      ksec1(5) = 0   ! FLAG (PRESENCE OF SECTION 2)
      if (L11 .or. L21 .or. L31) then
         ksec1(6) = 1           ! BUFR MESSAGE TYPE, surface
      else if (L12.or.L13.or.L22.or.L32.or.L33) then
         ksec1(6) = 31          ! BUFR MESSAGE TYPE, oceanographic
      else
         print*,'ERROR in bufr_encode: unknown subtype (no L11-L33 set)'
         call exit(2)
      end if
      if (L11) then
         ksec1(7) = 21          ! BUFR MESSAGE SUBTYPE, dribu surface
      else if (L12.or.L13) then
         ksec1(7) = 131         ! BUFR MESSAGE SUBTYPE, dribu oceanographic
      else if (L21) then
         ksec1(7) = 22          ! BUFR MESSAGE SUBTYPE, bathy surface
      else if (L22) then
         ksec1(7) = 132         ! BUFR MESSAGE SUBTYPE, bathy oceanographic
      else if (L31) then
         ksec1(7) = 23          ! BUFR MESSAGE SUBTYPE, tesac surface
      else if (L32 .or. L33) then
         ksec1(7) = 133         ! BUFR MESSAGE SUBTYPE, tesac oceanographic
      else
         print*,'ERROR in bufr_encode: unknown subtype (no L11-L33 set)'
         call exit(2)
      end if
      ksec1(8) = 0   ! VERSION NUMBER OF LOCAL TABLE
      iiyear=mod(nyear,100)
      if (iiyear.eq.0) iiyear = 100
      ksec1(9) = iiyear ! YEAR (2 digits)  
      ksec1(10) = nmonth
      ksec1(11) = nday
      ksec1(12) = nhour
      ksec1(13) = nminute
      ksec1(14) = 0  ! BUFR MASTER TABLE
      ksec1(15) = 14 ! VERSION NUMBER OF MASTER TABLE
      ksec1(16) = 0  ! ORIGINATING SUB-CENTRE
      if (L11 .or. L12 .or. L13) then
         ksec1(17) = 25 ! INTERNATIONAL DATA SUBCATEGORY (25 for BUOY, IOB)
      else if (L22 .or. L32 .or. L33) then
         ksec1(17) = 5 ! (BATHY, IOS)
      else if (L21 .or. L31) then ! Not yet implemented
         ksec1(17) = 255
      end if
      ksec1(18) = 0  ! SECOND

      ksec2(1) = 0   ! LENGTH OF SECTION 2, must be set by user 
      ksec3(1) = 0   ! LENGTH OF SECTION 3, not used by bufren
      ksec3(2) = 0   ! RESERVED
      ksec3(3) = 1   ! NUMBER OF DATA SUBSETS
      ksec3(4) = 128 ! FLAG (DATA TYPE/DATA COMPRESSION) here: data, no compression

      ksec4(1) = 0   ! LENGTH OF SECTION 4, not used by bufren
      ksec4(2) = 0   ! RESERVED

C     For debugging
c$$$      CALL BUXDES(1,ksec1,ktdlen,ktdlst,kdlen,kdata,kelem,
c$$$     +     ntdexl,ntdexp,cnames,cunits,kerr)
      
      call bufren(ksec0,ksec1,ksec2,ksec3,ksec4,
     +     ktdlen,ktdlst,kdlen,kdata,kelem,kvals,
     +     new_values,cvals,kbufl,kbuff,ierr)
      if (ierr.ne.0) then
         print*,'Error in BUFREN: ierr=',ierr
         call exit(2)
      end if

      call writebufr_drau(ocea)

C     Reset number of subsets to 0.
      nsub = 0

      return
      end subroutine bufr_encode
C----------------------------------------------------------------------

      subroutine writebufr_drau(ocea)
C     Writes files datadir/<obstype>/bufr.dat for surface part,
C     ...bufr.dat_ocea for oceanographic part
      implicit none
      include 'parameter.f'  ! jbufl, used in combufren.f
      include 'combufren.f'  ! kbufl,kbuff
      include 'comwrite.f'   ! num_records,sunit
      include 'commsys.f'    ! msys
      include 'commdbc.f'    ! cstream
      logical ocea           ! true if an oceanographic report is to be produced
      integer sunit_ocea,iret,ios
      logical opn
      character*80 cf

c     Open bufr file for appending  of BUFR messages if not already open
c     (bufr.dat was opened in msysinit.f)
      if (ocea) then
         cf = cstream(msys) // '/bufr.dat_ocea'
         inquire(file=cf,opened=opn,iostat=ios)
         if (ios.ne.0) then
            print*,'writebufr: INQUIRE failed, ios=',ios
            call exit(2)
         end if
         if (.not.opn) then
            call pbopen(sunit_ocea,cf,'a',iret)
            if ( iret .ne. 0 ) then 
               print*,'pbopen failed for ', cf
               call exit(2)
            end if
         end if
      end if

      if (ocea) then
C     kbufl is length of kbuff in words; pbwrite expects length in bytes.
C     Last 2 bytes written might be garbage (if length of BUFR message is
C     not a multiple of 4 bytes)
         call pbwrite(sunit_ocea,kbuff,4*kbufl,iret)
      else
         call pbwrite(sunit,kbuff,4*kbufl,iret)
      end if
      if (iret.lt.0) then
         print*,'Error in PBWRITE, returned ',iret
         call exit(2)
      end if

      num_records = num_records + 1
      end subroutine writebufr_drau
C----------------------------------------------------------------------

      SUBROUTINE GETNEXT(IERR )
C
C**** *GETNEXT*
C
C
C     PURPOSE.
C     --------
C         GET NEXT MESSAGE FROM MDB.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *GETNEXT( K,IERR )*
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
      integer nskip,ierr,istream,ios,ileng,iret,nryear,nrmonth,i
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comwt.f'
      INCLUDE 'commdbc.f'  ! ctstamp,cstream
      INCLUDE 'commsys.f'
      INCLUDE 'comkey.f'
C
c
      character*15000 ychar
      character*23 ytime
      CHARACTER*14 CTIME
      character*78 cstr
c
      data cstr/'                   drau decoding'/
C
      DATA NSKIP/0/
C     ------------------------------------------------------------------
C*          1.   RESET ERROR INDICATOR, WORD AND BIT POINTERS.
C                ---------------------------------------------
 100  CONTINUE
C
      IERR=0
c  nwpt here is not used - it is reset by sec0.f
      NSUB=0
      ISTREAM=msys
crr
      if(msys.eq.15) cstr(20:23)=cstream(msys)(1:4)
      if(msys.eq.16) cstr(20:23)=cstream(msys)(1:4)
C
C
c                write last time stamp processed.
c
         call asctim(ytime)    ! present time in format dd-mmm-yyyy hh:mm:ss.cc
         WRITE(17,9906,IOSTAT=IOS,ERR=410)
     1      CSTR,CSTREAM(msys),CTSTAMP,YTIME
CRR         WRITE(6,9906,IOSTAT=IOS,ERR=410)
CRR     1      CSTR,CSTREAM(msys),CTSTAMP,YTIME
C
         REWIND(17)
c
crr      end if
      go to 110
c
 410  continue
c
      print*,' Error writing time stamp.'
C
 9906 FORMAT(1h ,A,
     1     //1h ,'stream             ',A,
     1      /1h ,'time stamp         ',a,
     1     //1h ,'last time updated  ',a  )
c
      ierr=1
      return
C
C           1.1  GET NEXT MESSAGE.
C                -----------------
 110  CONTINUE
C
      CALL GETBULL(YCHAR,ILENG,IRET)
cps      return code 7 means no more data, or error in system read
cpsnov99 return code 8 means too long bulletin: skip to next bulletin
      IF(IRET.EQ.7) THEN
         IERR=7
         RETURN
      elseif(iret.eq.8) then
         goto 110
      END IF
C
crr      ILEN=ilenby
      ILEN=ILENG
crr      READ(CTSTAMP,'(6x,4i2)') NRDAY,NRHOUR,
crr     1                         NRMIN,NRSEC
      nrsec=00
cpsokt98      READ(CTSTAMP,'(4i2)') nrmonth,NRDAY,NRHOUR,NRMIN
      READ(CTSTAMP,'(i4,4i2)') nryear,nrmonth,NRDAY,NRHOUR,NRMIN
c
cpsokt98      print*,'Current time stamp ',ctstamp(1:8)
      print*,'Current time stamp ',ctstamp(1:12)
C
 115  CONTINUE
C
      NUMBULL=NUMBULL+1
C
C           1.2  PRINT BULLETIN NUMBER.
C                ----------------------
 120  CONTINUE
C
CTEST**********************************************
c stop after reading specified number of reords
        WRITE(*,9902) NUMBULL
ctestfile
cc        WRITE(99,9902) NUMBULL
      IF(MSYSTOP.GT.0) THEN
         IF(NUMBULL.GT.MSYSTOP) then
            IERR=7
            RETURN
         END IF
      ENDIF
CTEST**********************************************
C
C           1.3  SKIP BULLETIN IF NECESSARY.
C                ---------------------------
 130  CONTINUE
C
      IF(NUMBULL.EQ.NSKIP) GO TO 110
C
C
C           1.4  TRANSFER BULLETIN TO 'KCHAR' ONE CHARACTER PER WORD.
C                ----------------------------------------------------
C                AND SUPPRESS PARITY BIT.
C                ------------------------
 140  CONTINUE
C
      DO 141 I=1,ILEN
C
      KCHAR(I)=IAND(ICHAR(YCHAR(I:I)),127)
C
 141  CONTINUE
cc      print*,' getnext-kchar ',(kchar(i),i=1,10)
C
CTEST
      PRINT*,YCHAR(1:ILENG)
ctestfile
cc      WRITE(99,*)YCHAR(1:ILENG)
cc      PRINT*,' GETNEXT - returning '
CTEST
C
      RETURN
C
C     -----------------------------------------------------------------
 200  CONTINUE
C
      IERR=1
      WRITE(*,9901) IRET
      RETURN
C     -----------------------------------------------------------------
 300  CONTINUE
C
      IERR=1
      WRITE(*,9905) IOS
      RETURN
C
 310  CONTINUE
C
      IERR=1
      WRITE(*,9904) IOS
      RETURN
C     -----------------------------------------------------------------
 400  CONTINUE
C
 9901 FORMAT(1H ,' + + + ERROR GETTING NEXT MESSAGE FROM MDB + + +',I6)
 9902 FORMAT(1H ,' ** BULLETIN NUMBER = ',I9)
 9904 FORMAT(1H ,' ERROR WRITING STATUS FILE ',I6)
 9905 FORMAT(1H ,' ERROR OPENING STATUS FILE ',I6)
C
      END
      SUBROUTINE KKINT (IHEAD,IERR)
C
C**** *KKINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT TESAC REPORTS  FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE (INTEGER) FORMAT
cps IxIxIx,XrXr is not decoded to kint
C
C         INPUT    : REPORT IN KCHAR(IPT) - KCHAR(IEQ),
C                    IN CCITT 5, 1 CHARACTER PER WORD.
C
C                    IHEAD = 0  INDICATES BULLETIN HEADER NOT ALREADY
C                               WRITTEN TO ERROR FILE
C                          = 1  MEANS HEADER ALREADY WRITTEN TO ERROR FILE
C
C                    IERR       NOT USED ON INPUT
C
C          OUTPUT   : REPORTS IN INTEGER FORMAT IN ARRAY 'KINT' IN DESIRED
C                     FORMAT (SEE SEPARATE DOCUMENTATION).
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *KKINT(IHEAD,IERR)*
C
C     METHOD.
C     -------
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING IS PROCE
C         UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON FATAL ERROR HAS BEEN DETECTED THE ROUTINE TR
C         TO FIND THE BEGINNING OF CORRECT DATA. IF CORRECT DATA
C         FOUND THE DECODING THE NUMBER OF NON-FATAL ERRORS IS CO
C         AND IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE DAT
C         AFTER THE FIRST ERROR ARE REMOVED (THIS IS DONE TO ENSU
C         THAT TOTALLY CORRUPTED DATA ARE NOT USED E.G. WRONG PAR
C         OF TESAC).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C             NUMERR = NUMBER OF NON-FATAL ERRORS
C             IREMPOS= THE POINTER TO FIRST ERROR IN 'KINT'.
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *ERRSTA(      )*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C
      implicit none
      integer ihead,ierr
      integer keep,ifirst,numerr,irempos,nil,len,i,iret,ilo,ila
      integer ival,ipos,ir,ii,iii,iiii,nc
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      include 'comdrau.f'   ! LaLaLaLaLa
C
C     ------------------------------------------------------------------
C*          1.  CLEAR FLAGS AND ERROR INDICATOR. KEEP POINTER TO FIRST
C               ------------------------------------------------------
C               CHARACTER OF REPORT.
C               --------------------
 100  CONTINUE
C
C
C     POINTER TO FIRST CHARACTER
C
      KEEP=IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST= 0
C
C     ERROR INDICATOR
C
 101  KERR   = 0
      NUMERR = 0
      IREMPOS= 10
C
C
C*          1.1  CHECK FOR 'NIL' REPORT.
C                -----------------------
 110  CONTINUE
C
      NIL= 0
C
      LEN=IEQ-IPT
      IF(LEN.LE.30)
     &   THEN
            KERR=1
            NIL =1
            CALL ERRSTA(24,0,IFIRST,NUMERR)
         END IF
C
C     SET AREA FOR INTERMEDIATE (INTEGER) REPORT TO MISSING DATA VALUE
C
      DO I=1,jp15
         KINT(I)=MINDIC
      END DO
C
C
C*          1.2 CLEAR DATA FOR PREVIOUS REPORT FROM
C               -----------------------------------
C               DECODED REPORT HEADER AREA .
C               ----------------------------
 120  CONTINUE
C
      DO 121 I=1,3
         KDEC(I)=MINDIC
 121  CONTINUE
C
      DO 122 I=5,9
         KDEC(I)=MINDIC
 122  CONTINUE
C
      KDEC(13)=MINDIC
      KDEC(15)=IAND(KDEC(15),4)
      KDEC(16)=MINDIC
      KDEC(17)=MINDIC
C
      KDEC(20)=0
      KDEC(22)=MINDIC
      KDEC(23)=MINDIC
C
C     -----------------------------------------------------------------
C*          2. SECTION 1 - IDENTIFICATION AND POSITION DATA;
C              (WHENEVER AVAILABLE) SURFACE PRESSURE AND SEA SURFACE
C              TEMPERATURE; (OPTIONAL) WIND,AIR TEMPERATURE
C                 PRESSURE TENDENCY.
C
 200  CONTINUE
C
C     YYMMJ - GROUP
C
      CALL NEXTFIG(IPT,IEQ)
      IF(IPT .GT. IEQ) GO TO 600
C
      CALL EXTGRP(IPT,2,2,1,0,0,1,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=2
            CALL ERRSTA(24,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
            GO TO 600
         END IF
C
C     YY GG /
C     -- -- -
      CALL NEXSEP2 (IPT,IEQ,*600)
      CALL NEXPRT2 (IPT,IEQ,*600)
C
      CALL EXTGRP(IPT,2,2,1,0,0,4,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=3
            CALL ERRSTA(24,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
            GO TO 600
         END IF
C
      IF(KINT(1).LT.1.OR.KINT(1).GT.31)
     &   THEN
            IRET=1
            KINT(1)=MINDIC
         END IF
C
      IF(KINT(2).LT.1.OR.KINT(2).GT.12)
     &   THEN
            IRET=2
            KINT(2)=MINDIC
         END IF
C
      IF(KINT(3).LT.0.OR.KINT(3).GT.9) THEN
         IRET=3
         KINT(3)=MINDIC
      END IF
C 
      IF(KINT(4).LT.0.OR.KINT(4).GT.24)
     &   THEN
            IRET=2
            KINT(4)=MINDIC
         END IF
      IF(KINT(5).LT.0.OR.KINT(5).GT.59)
     &   THEN
            IRET=2
            KINT(4)=MINDIC
         END IF
C
      IF(IRET.NE.0)
     &   THEN
C
C           CHECK IF ALREADY KNOWN TO BE 'NIL' - REPORT
C
            IF(NIL.EQ.1)
     &         THEN
                  KERR=1
                  RETURN
               END IF
C
            KERR=2
            CALL ERRSTA(24,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
            RETURN
C
         END IF
C
C     QCLALALALA - GROUP
C     - - - - -
C
      CALL NEXSEP2 (IPT,IEQ,*600)
      CALL NEXPRT2(IPT,IEQ,*600)
C
      if(kchar(ipt+6).eq.32) then
         if(kchar(ipt+5).eq.47) kchar(ipt+5)=48
         if(kchar(ipt+4).eq.47) kchar(ipt+4)=48
         LaLaLaLaLa = .true.
         ILO=6
         ILA=5
      elseif(kchar(ipt+5).eq.32) then
         LaLaLaLaLa = .false.
         ILO=5
         ILA=4
      end if
c
      CALL EXTGRP (IPT,1,ILA,0,0,0,7,IRET)
      IF(IRET.NE.0) THEN
         KERR=4
         CALL ERRSTA(24,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
         GO TO 600
      END IF
C
C       LOLOLOLOLO - GROUP
C       - - - - -
C
        CALL NEXSEP2 (IPT,IEQ,*600)
        CALL NEXPRT2(IPT,IEQ,*600)
C
      if(kchar(ipt+6).eq.32) then
         if(kchar(ipt+5).eq.47) kchar(ipt+5)=48
         if(kchar(ipt+4).eq.47) kchar(ipt+4)=48
      end if
c
      CALL EXTGRP (IPT,ILO,0,0,0,0,9,IRET)
      IF(IRET.NE.0) THEN
         KERR=5
         CALL ERRSTA(24,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
         GO TO 600
      END IF
C-----------------------------------------------------------------------
 210  CONTINUE
C
C     CHECK IF THE NEXT GROUP IS BEGINING OF SECTION 2
C
      CALL NEXSEP2(IPT,IEQ,*600)
      CALL NEXPRT2(IPT,IEQ,*600)
C
C          888k1k2
C
      IF(KCHAR(IPT  ) .EQ. 56.AND.
     &   KCHAR(IPT+1) .EQ. 56.AND.
     &   KCHAR(IPT+2) .EQ. 56.AND.
     &   KCHAR(IPT+5) .LE. 32     )
     &   THEN
C           BEGINING OF THE SECTION 2 FOUND
            GO TO 300
         END IF
C
C     THERE ARE SOME USEFULL INFORMATION IN SECTION 1
C
      CALL EXTVAL(IPT,1,IVAL)
C
C
      IF(IVAL .EQ. 4)
     &   THEN
C           AIR TEMPERATURE FOUND
C
            CALL EXTGRP (IPT,1,1,3,0,0,13,IRET)
            IF(IRET.NE.0)
     &          THEN
                   KERR=7
                   CALL ERRSTA(24,1,IFIRST,NUMERR)
                   IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
                   IF(NUMERR.EQ.1) IREMPOS=13
                END IF
C
            GO TO 210
C
         ELSE
C
            IF(KINT(10).EQ.MINDIC)
     &         THEN
C                 WIND DIRECTION AND SPEED

C
                  CALL EXTGRP (IPT,1,2,2,0,0,10,IRET)
                  IF(IRET.NE.0)
     &               THEN
                        KERR=8
                        CALL ERRSTA(24,1,IFIRST,NUMERR)
                        IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
                        IF(NUMERR.EQ.1) IREMPOS=10
                     END IF
C
               END IF
C
         END IF
C
      GO TO 210
C-----------------------------------------------------------------------
C*          3. SECTION 2 - TEMPERATURES AND SALINITY AT EITHER SIGNOFICANT
C              OR SELECTED DEPTHS
C
 300  CONTINUE
C
cps   888k1k2
      CALL EXTGRP (IPT,3,1,1,0,0,16,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=9
            CALL ERRSTA(24,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=16
         END IF
C-----------------------------------------------------------------------
      IPOS=19
      IR  =0
c
c----
cps IxIxIxXrXr is always present in KKYY reports, not in KKXX reports
cps If not present, next 2 groups after 888.. will start with 2 and 3 
cps respectively. 
      iiii=ipt
      CALL NEXSEP2(iiii,IEQ,*500)
      CALL NEXPRT2(iiii,IEQ,*500)
      iii=iiii
      CALL NEXSEP2(iii,IEQ,*500)
      CALL NEXPRT2(iii,IEQ,*500)
      if(kchar(iiii).eq.50.and.kchar(iii).eq.51) then
      else
c
c        skipp IxIxIxXrXr  group  new code
c
        CALL NEXSEP2(IPT,IEQ,*500)
        CALL NEXPRT2(IPT,IEQ,*500)
      end if
C
 310  CONTINUE
C
      CALL NEXSEP2 (IPT,IEQ,*600)
      CALL NEXPRT2(IPT,IEQ,*600)
C
C             00000
C
      IF(KCHAR(IPT  ) .EQ. 48.AND.
     &   KCHAR(IPT+1) .EQ. 48.AND.
     &   KCHAR(IPT+2) .EQ. 48.AND.
     &   KCHAR(IPT+3) .EQ. 48.AND.
     &   KCHAR(IPT+4) .EQ. 48.AND.
     &   KCHAR(IPT+5) .LE. 32     )
     &   THEN
C           END OF SECTION 2 REACHED
            GO TO 320
         END IF
C
      IF(KCHAR(IPT ).GE.65.AND.KCHAR(IPT  ).LE.90)
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 311 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
 311        CONTINUE
C
            GO TO 600
C
         END IF
C
      IF(KCHAR(IPT  ).GE.48.AND.KCHAR(IPT  ).LE.57 .AND.
     &   KCHAR(IPT+1).GE.65.AND.KCHAR(IPT+1).LE.90       )
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 312 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
 312        CONTINUE
C
            GO TO 600
C
         END IF
C
cpssep02 Inserted to agree with mdb_buoy.f
      IF(KCHAR(IPT  ).EQ.57.AND.KCHAR(IPT+1).EQ.57.AND.
     1   KCHAR(IPT+2).EQ.57.AND.KCHAR(IPT+3).EQ.57.AND.
     2   KCHAR(IPT+4).EQ.57) THEN
C
         CALL NEXSEP2(IPT,IEQ,*600)
         CALL NEXPRT2(IPT,IEQ,*600)
C
C        PROBABLY SHIP'S CALL SIGN FOUND
C
         KPT=IPT
         CALL NEXTSEP(KPT,IEQ)
C
         NC=KPT-IPT
C
         DO 313 I=1,NC
C
         IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
         IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
 313     CONTINUE
C
         GO TO 600
C
      END IF
      if((ieq-ipt).eq.5) then
C
C        PROBABLY SHIP'S CALL SIGN FOUND and 99999 is missing
C
         KPT=IPT
         CALL NEXTSEP(KPT,IEQ)
C
         NC=KPT-IPT
C
         DO 314 I=1,NC
C
         IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
         IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
 314     CONTINUE
cps end insert
C
         GO TO 600
C
      END IF
C
C           66k6k4k3
C
      IF(KCHAR(IPT  ) .EQ. 54.AND.
     &   KCHAR(IPT+1) .EQ. 54.AND.
     &   KCHAR(IPT+5) .LE. 32     )
     &   THEN
C           BEGINING OF THE SECTION 3 FOUND
            GO TO 400
         END IF
C
C          55555 SECTION 4
C
      IF(KCHAR(IPT  ) .EQ. 53.AND.
     &   KCHAR(IPT+1) .EQ. 53.AND.
     &   KCHAR(IPT+2) .EQ. 53.AND.
     &   KCHAR(IPT+3) .EQ. 53.AND.
     &   KCHAR(IPT+4) .EQ. 53.AND.
     &   KCHAR(IPT+5) .LE. 32     )
     &   THEN
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 500
         END IF
C
C     TEMPERATURES AT SIGNIFICANT DEPTHS FOLLOW
C
      CALL EXTVAL(IPT,1,IVAL)
C
      IF(IVAL.EQ.2) THEN
cps         2znznznzn Depth
            CALL EXTGRP (IPT,1,4,0,0,0,IPOS,IRET)
            IF(IRET.NE.0)
     &         THEN
                  KERR=10
                  CALL ERRSTA(24,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=IPOS
            END IF
C
cpssep02 The following somewhat rewritten to agree with mdb_buoy.f
            IPOS=IPOS+2
c
            III=IPT
            CALL NEXSEP2 (III,IEQ,*600)
            CALL NEXPRT2(III,IEQ,*600)
c
            IF(KCHAR(III).NE.51) IPOS=IPOS+2
c
            GO TO 310
C
      END IF
C
      IF(IVAL.EQ.3) THEN
cps         3TnTnTnTn
            CALL EXTGRP (IPT,1,4,0,0,0,IPOS,IRET)
            IF(IRET.NE.0) THEN
               KERR=11
               CALL ERRSTA(24,1,IFIRST,NUMERR)
               IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
               IF(NUMERR.EQ.1) IREMPOS=IPOS
            END IF
C
C
            IPOS=IPOS+2
C
            III=IPT
            CALL NEXSEP2 (III,IEQ,*600)
            CALL NEXPRT2(III,IEQ,*600)
c
            IF(KCHAR(III).NE.52) THEN
               IPOS=IPOS+2
            end if
c
            GO TO 310
C
      END IF
C
      IF(IVAL.EQ.4) THEN
cps         4SnSnSnSn Salinity
            CALL EXTGRP (IPT,1,4,0,0,0,IPOS,IRET)
            IF(IRET.NE.0) THEN
                  KERR=12
                  CALL ERRSTA(24,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=IPOS
            END IF
C
            IPOS=IPOS+2
            GO TO 310
C
      ELSE
C
            IPOS=IPOS+2
C
            CALL NEXSEP2(IPT,IEQ,*600)
C
            GO TO 310
C
      END IF
C-----------------------------------------------------------------------
 320  CONTINUE
C
cps   00000
      CALL EXTGRP (IPT,5,0,0,0,0,4999,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=13
            CALL ERRSTA(24,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=4999
         END IF
C
      GO TO 310
C
C     -----------------------------------------------------------------
C*          4. SECTION 3 - CURRENT AT SELECTED AND/OR
C              SIGNIFICANT DEPTHS(OPTION)
C
 400  CONTINUE
C
cps   66k6k4k3 
      CALL EXTGRP (IPT,3,1,1,0,0,5000,IRET)
cps This means 66k6 is put into kint(5000). Note that kint(5001)
cps and kint(5002) is put directly into VALUESW in TESACEXP (no
cps KDEC defined for k4 and k3)
      IF(IRET.NE.0)
     &   THEN
            KERR=14
            CALL ERRSTA(24,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=5000
         END IF
C
      IPOS=5003
C
 410  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*600)
      CALL NEXPRT2(IPT,IEQ,*600)
C
      IF(KCHAR(IPT ).GE.65.AND.KCHAR(IPT  ).LE.90)
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 411 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I)
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I)
               END IF
C
 411        CONTINUE
C
            GO TO 600
C
         END IF
C
      IF(KCHAR(IPT  ).GE.48.AND.KCHAR(IPT  ).LE.57 .AND.
     &   KCHAR(IPT+1).GE.65.AND.KCHAR(IPT+1).LE.90       )
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 412 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
 412        CONTINUE
C
            GO TO 600
C
         END IF
cpssep02 Inserted to agree with mdb_buoy.f
      IF(KCHAR(IPT  ).EQ.57.AND.KCHAR(IPT+1).EQ.57.AND.
     1   KCHAR(IPT+2).EQ.57.AND.KCHAR(IPT+3).EQ.57.AND.
     2   KCHAR(IPT+4).EQ.57) THEN
C
cps 99999         
         CALL NEXSEP2(IPT,IEQ,*600)
         CALL NEXPRT2(IPT,IEQ,*600)
C
C        PROBABLY SHIP'S CALL SIGN FOUND
cps No, but A1bwnbnbnb
C
         KPT=IPT
         CALL NEXTSEP(KPT,IEQ)
C
         NC=KPT-IPT
C
         DO 413 I=1,NC
C
         IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
         IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
 413     CONTINUE
C
         GO TO 600
C
      END IF
      if((ieq-ipt).eq.5) then
C
C        PROBABLY SHIP'S CALL SIGN FOUND and 99999 is missing
C
         KPT=IPT
         CALL NEXTSEP(KPT,IEQ)
C
         NC=KPT-IPT
C
         DO 414 I=1,NC
C
         IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
         IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
cps end insert
C
 414     CONTINUE
C
         GO TO 600
C
      END IF
C
C             55555 SECTION 4
C
      IF(KCHAR(IPT  ) .EQ. 53.AND.
     &   KCHAR(IPT+1) .EQ. 53.AND.
     &   KCHAR(IPT+2) .EQ. 53.AND.
     &   KCHAR(IPT+3) .EQ. 53.AND.
     &   KCHAR(IPT+4) .EQ. 53.AND.
     &   KCHAR(IPT+5) .LE. 32     )
     &   THEN
C           BEGINING OF THE SECTION 4 (55555) FOUND
            GO TO 500
         END IF
C
C     THERE ARE SOME INFORMATION ABOUT CURRENT
C
      CALL EXTVAL(IPT,1,IVAL)
      IF(IVAL.EQ.2) THEN
cps         2znznznzn Depth         
            CALL EXTGRP (IPT,1,4,0,0,0,IPOS,IRET)
            IF(IRET.NE.0) THEN
                  KERR=15
                  CALL ERRSTA(24,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=IPOS
            END IF
C
            CALL NEXSEP2(IPT,IEQ,*600)
            CALL NEXPRT2(IPT,IEQ,*600)
c
            IPOS=IPOS+2
cps         dndncncncn Current direction and speed
            CALL EXTGRP (IPT,2,3,0,0,0,IPOS,IRET)
            IF(IRET.NE.0) THEN
                  KERR=16
                  CALL ERRSTA(24,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=IPOS
            END IF
 
            IPOS=IPOS+2
C
            GO TO 410
C
         END IF
C
C     ----------------------------------------------------------------
C*          5. SECTION 4 - TOTAL WATER DEPTH (OPTIONAL).
C              -----------------------------------------
 500  CONTINUE
C
cps   55555
      CALL EXTGRP (IPT,5,0,0,0,0,8000,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=17
            CALL ERRSTA(24,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=8000
         END IF
C
 510  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*600)
      CALL NEXPRT2(IPT,IEQ,*600)
C
      IF(KCHAR(IPT ).GE.65.AND.KCHAR(IPT  ).LE.90)
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 511 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I)
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I)
               END IF
C
 511        CONTINUE
C
            GO TO 600
C
         END IF
C
      IF(KCHAR(IPT  ).GE.48.AND.KCHAR(IPT  ).LE.57 .AND.
     &   KCHAR(IPT+1).GE.65.AND.KCHAR(IPT+1).LE.90       )
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 512 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(8003-1+I)=KCHAR(IPT-1+I) 
               END IF
C
 512        CONTINUE
C
            GO TO 600
C
         END IF
C
cpssep02 Inserted to agree with mdb_buoy.f
      IF(KCHAR(IPT  ).EQ.57.AND.KCHAR(IPT+1).EQ.57.AND.
     1   KCHAR(IPT+2).EQ.57.AND.KCHAR(IPT+3).EQ.57.AND.
     2   KCHAR(IPT+4).EQ.57) THEN
C
C         PROBABLY SHIP'S CALL SIGN FOUND
C
         CALL NEXSEP2(IPT,IEQ,*600)
         CALL NEXPRT2(IPT,IEQ,*600)
C
         KPT=IPT
         CALL NEXTSEP(KPT,IEQ)
C
         NC=KPT-IPT
C
         DO 513 I=1,NC
C
         IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
         IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
 513     CONTINUE
C
         GO TO 600
C
      END IF
      if((ieq-ipt).eq.5) then
C
C         PROBABLY SHIP'S CALL SIGN FOUND
C
         CALL NEXSEP2(IPT,IEQ,*600)
         CALL NEXPRT2(IPT,IEQ,*600)
C
         KPT=IPT
         CALL NEXTSEP(KPT,IEQ)
C
         NC=KPT-IPT
C
         DO 514 I=1,NC
C
         IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
         IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57) THEN
            KINT(8003-1+I)=KCHAR(IPT-1+I) 
         END IF
C
 514     CONTINUE
C
         GO TO 600
C
      END IF
cps end insert
C
      CALL EXTVAL(IPT,1,IVAL)
      IF(IVAL.EQ.1)
     &   THEN
cps         1ZdZdZdZd
            CALL EXTGRP (IPT,1,4,0,0,0,8001,IRET)
            IF(IRET.NE.0)
     &         THEN
                  KERR=18
                  CALL ERRSTA(24,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(24,KERR)=NOER(24,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=8001
                END IF
C
            GO TO 510

         ELSE
            CALL NEXSEP2(IPT,IEQ,*600)
C
            GO TO 510
C
         END IF
C-----------------------------------------------------------------------
 600  CONTINUE
C***
C*    ERROR CHECKING AND HANDLING
C****
C     RETURN IF NO ERRORS IN REPORT
C
      IF(KERR.EQ.0) RETURN
C
C     IF FIRST DECODING ATTEMPT ,TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF(IFIRST.EQ.0) THEN
                         IPT=KEEP
                         CALL FIXSO
                         IFIRST=1
                         NUMRERR(24)=NUMRERR(24)+1
                         GO TO 101
                      ELSE
                         CALL SAVREP(IHEAD,IERR)
C
C                        CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
                         DO 601 I=KEEP,IGS
                         KCHAR(I)=IAND(KCHAR(I),127)
 601                     CONTINUE
C
C                        ONLY REPORTS WITH ERROR IN DATE/TIME
C                        OR LAT/LONG ARE NOT PROCESSED ANY FURTHER
C
C                        IF THERE ARE MORE THEN 5 NON-FATAL ERRORS
C                        IN REPORT IT IS ONLY PROCESSED UNTIL THE
C                        FIRST ERROR
C
                         IF(NUMERR.GT.5) THEN
                            DO I=IREMPOS,jp15
                               KINT(I)=MINDIC
                            END DO
                         END IF
C
                         IF(KERR.GT.6) KERR=0
                      END IF
C
 610  CONTINUE
C
C      CALL PRTKINT(KINT,1,3000,MINDIC)
C
      RETURN
      END
      SUBROUTINE JJDEC(IHEAD,IERR)
C
C**** *JJDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE BATHY REPORT  FROM INTERMEDIATE
C         FORMAT TO FULL DECODED FORMAT
C
C         INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'
C
C         OUTPUT   : REPORT WRITTEN INTO DECODED REPORT FILE 04
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *JJDEC(IHEAD,IERR)*
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
C         *CALL* *QLALOM(                )*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
cpsjan07 Reordered kint and kdec: subsurface levels start at 100 (not 18 and 38),
cps      and packed consecutively in kdec (2 indices per level, not 5). 
cps      kint(338-361) shifted to 38-61. See kint.bath
C
C
      implicit none
      integer ihead,ierr
      integer i,ibl,ki,id,ii,ipkt,ipkd,izz

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comident.f' 
      INCLUDE 'comwt.f'
      include 'comdrau.f'    ! L11-L33,NTL,LaLaLaLaLa


C*    Clear error indicator.             .
      KERR=0
C
C*          1.1 Set unused header fields to missing data indicator
C               (altitude,wmo region and country,reserved fields).
C               --------------------------------------------------

      KDEC(07)=MINDIC
      KDEC(08)=MINDIC
      KDEC(13)=MINDIC
      KDEC(15)=IAND(KDEC(15),4)
      KDEC(16)=MINDIC
      KDEC(17)=MINDIC
      KDEC(21)=MINDIC
      KDEC(22)=MINDIC

C     Clear 'NIL' report indicator
      KDEC(20)=0
C
C     Set decoded report to missing data value
      DO I=25,jp11
         KDEC(I)=MINDIC
      END DO

C     ------------------------------------------------------------------
C*          2. Set up header of fully expanded bathy format.
C              ---------------------------------------------

C*    Set up day, hour and minute
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(4)
      KDEC(9)=KINT(5)

C*    Set up latitude and longitude
      if (LaLaLaLaLa) then
         CALL QLALOD(KINT(7),KINT(8),KINT(9),MINDIC,KDEC(5),KDEC(6))
      else
         CALL QLALOM(KINT(7),KINT(8),KINT(9),MINDIC,KDEC(5),KDEC(6))
      end if
      IF(KDEC(5).EQ.MINDIC.OR.KDEC(6).EQ.MINDIC) THEN
         IERR=5
         RETURN
      END IF

C     Set ship's call sign
      IBL=32
      IF(KINT(45).EQ.MINDIC.OR.
     &   KINT(46).EQ.MINDIC.OR.
     &   KINT(47).EQ.MINDIC    )
     &   THEN
            IDENT(1)=88
            IDENT(2)=88
            IDENT(3)=88
            IDENT(4)=IBL
            IDENT(5)=IBL
            IDENT(6)=IBL
            IDENT(7)=IBL
            IDENT(8)=IBL
            IDENT(9)=IBL
         ELSE
            IDENT(1)=KINT(45)
            IDENT(2)=KINT(46)
            IDENT(3)=KINT(47)
cpssep02 Rewritten this section to decode call signs up to 9 chars
cps      (in Bufr data descriptor 001011 is 72 bit wide, so allow 9 chars)
            ki=47
            id=3
            do ii=1,6
               if(kint(ki+ii).eq.mindic) then
                  ident(id+ii)=32
               else     
                  ident(id+ii)=32
                  if(kint(ki+ii).ge.48.and.kint(ki+ii).le.90) 
     &                 ident(id+ii)=kint(ki+ii)
               end if
            end do
         END IF

cpsjan07 k1 - Indicator for digitization
      CALL IC2032(KINT(17),KDEC(29))

C*    WIND DIRECTION AND SPEED

cpsjun06 Next section rewritten, but note that currently we make no
cps      surface report for tesac, so this part produce no output
      IF(KINT(11).NE.MINDIC.AND. KINT(12).NE.MINDIC) THEN
         IF(KINT(11).EQ.99) then
            KDEC(26)=0
         else if(kint(11).gt.36) then  ! unphysical value, set to missing
            kdec(26)=mindic
         else IF(KINT(11).EQ.0 ) THEN
cps      dd=0 means calm (so speed must be 0). If not, we regard it 
cps      as a typo for 360
            if(kint(12).gt.0 .and. kint(12).ne.mindic) then
               print*,'dd=0, with ff>0. Changed to 360'
               kdec(26) = 360
            end if
         else
            KDEC(26)=KINT(11)*10
         end if
crr  change accuracy of wind by multiplying by 10.
         IF(KINT(10).EQ.0.OR.KINT(10).EQ.2) THEN
C              WIND SPEED IN (M/S)
crr               KDEC(27)=KINT(12)
            KDEC(27)=KINT(12)*10
         END IF
C
         IF(KINT(10).EQ.1.OR.KINT(10).EQ.3) THEN
C              WIND SPEED IN KNOTS
crr               KDEC(27)=KINT(12)/2. + .5
            KDEC(27)=KINT(12)*10
            CALL KTOMPSI(KDEC(27))
         END IF
      END IF

C*    Air temperature
      IF(KINT(14).NE.MINDIC.AND.KINT(15).NE.MINDIC) THEN
         IF(KINT(14).EQ.0) THEN
            KDEC(28)=KINT(15)
         END IF
         IF(KINT(14).EQ.1) THEN
            KDEC(28)=-KINT(15)
         END IF
      END IF

C*    Total water depth in metres
      KDEC(32)=KINT(41)

C*    Surface current direction and speed
      IF(KINT(43).NE.MINDIC) KDEC(36)=KINT(43)*10
      IF(KINT(43).NE.MINDIC) THEN
         KDEC(37)=KINT(44)*10
         CALL KTOMPSI(KDEC(37))
      ENDIF

C*    DEPTHS AND TEMPERATURES

cps ipkt=index in kint, ipkd=index in kdec (t and d are NOT abbr. for temperature and depth!)
      IPKT=100
      IPKD=100
      IZZ=0    ! zz=depth in unit of 100 meters, to be added to each subsequent znzn
      NTL=0

      DO I=1,JP11/2  ! alternatively we could stop looping if kint(ipkt)=mindic
         IF (KINT(IPKT).NE.MINDIC .AND. KINT(IPKT+1).NE.MINDIC) THEN
            IF (KINT(IPKT).NE.999) THEN
               NTL=NTL+1
               KDEC(IPKD)=KINT(IPKT) + IZZ
               IF (KINT(IPKT+1).GT.500) THEN
cps negative temperatures marked by adding 500 to absolute value of temperature
                  KDEC(IPKD+1)=-(KINT(IPKT+1)-500)*10
               ELSE
                  KDEC(IPKD+1)=  KINT(IPKT+1)*10
               END IF
               IPKD=IPKD+2
               IF (IPKD+1.GT.JP11) EXIT
            ELSE
               IZZ=KINT(IPKT+1)*100
            END IF
            IPKT=IPKT+2
            IF (IPKT+1.GT.JP15) EXIT
         END IF
      END DO

C     -------------------------------------------------------------
C         3. Set up bufr expanded format for bathy data 
C            ------------------------------------------

      CALL BATHYEXP(IERR )
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' JJDEC - after BATHYEXP, IERR=',IERR
         RETURN
      END IF

      IF (.NOT.L21.AND..NOT.L22) THEN
         WRITE(*,*) 'Nothing interesting to decode. Skipping report'
         RETURN
      END IF

C     -------------------------------------------------------------
C           4. Quality control bathy basic surface report.
C              -------------------------------------------

C     Bathy surface report not implemented (never seen any anyway)
c$$$      IF(.NOT.L21) GO TO 600
      L21 = .FALSE.
      GO TO 600
c$$$      CALL QCBATHY(IERR)
C     -------------------------------------------------------------
C           5. Encode bufr message.
C              --------------------

C     First make bathy surface report
      CALL SET_DELAYED_REPLICATION()     ! sets kdata and kdlen
      CALL SET_DESCRIPTORS()

      CALL BUFR_ENCODE()

C     Clear flag for surface report
      L21 = .FALSE.

C     -------------------------------------------------------------
C           6. MOVE VALUEW INTO VALUES ARRAY.
C              ------------------------------
 600  CONTINUE
       IF(.NOT.L22) GO TO 900
C      Then make oceanographic report
       CALL QCCON( IERR )
C     -------------------------------------------------------------
C           7. Quality control bathy sub-surface data.
C              ---------------------------------------

c$$$      CALL QCBATHY(IERR)
C     -------------------------------------------------------------
C           8. Encode bufr message.
C              --------------------

      CALL SET_DELAYED_REPLICATION()     ! sets kdata and kdlen
      CALL SET_DESCRIPTORS()

      CALL BUFR_ENCODE()

C     Clear flags for oceanographic report
      L22 = .false.

 900  CONTINUE

      RETURN
      END
      SUBROUTINE KKDEC(IHEAD,IERR)
C
C**** *KKDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE TESAC REPORT  FROM INTERMEDIATE
C         FORMAT TO FULL DECODED FORMAT
cps k1,k2,k3,k4,k6,IxIxIx,XrXr is not decoded to kdec
C
C         INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'
C
C         OUTPUT   : REPORT WRITTEN INTO DECODED REPORT FILE 04
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *KKDEC(IHEAD,IERR)*
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
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
      integer ihead,ierr
      integer i,ibl,ki,id,ii,ipkt1,ipkt2,ipkd,izz

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comident.f'
      INCLUDE 'comwt.f'
      include 'comdrau.f'    ! L11-L33,NTL,NCL,LaLaLaLaLa

C*          1.1  SET UNUSED HEADER FIELDS TO MISSING DATA INDICATOR
C                (ALTITUDE,WMO REGION AND COUNTRY,RESERVED FIELDS)

      KDEC(07)=MINDIC
      KDEC(08)=MINDIC
      KDEC(13)=MINDIC
      KDEC(15)=IAND(KDEC(15),4)
      KDEC(16)=MINDIC
      KDEC(17)=MINDIC
      KDEC(21)=MINDIC
      KDEC(22)=MINDIC

C     Clear 'NIL' report indicator
      KDEC(20)=0

C     Set decoded report to missing data value
      DO I=25,JP11
         KDEC(I)=MINDIC
      END DO

C     -------------------------------------------------------------------
C*          2. SET UP HEADER OF FULLY EXPANDED TESAC FORMAT.
C              ---------------------------------------------

C*    Set up day, hour and minute
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(4)
      KDEC(9)=KINT(5)

C*    Set up latitude and longitude
      IF (LALALALALA) THEN
         CALL QLALOD(KINT(7),KINT(8),KINT(9),MINDIC,KDEC(5),KDEC(6))
      ELSE
         CALL QLALOM(KINT(7),KINT(8),KINT(9),MINDIC,KDEC(5),KDEC(6))
      END IF
      IF(KDEC(5).EQ.MINDIC.OR.KDEC(6).EQ.MINDIC) THEN
         IERR=5
         RETURN
      END IF

C*    Set ship's call sign
      IBL=32
C     Set to XXX (X=88) if missing or less than 3 chars
      IF(KINT(8003).EQ.MINDIC.OR.
     &   KINT(8004).EQ.MINDIC.OR.
     &   KINT(8005).EQ.MINDIC    )
     &   THEN
            IDENT(1)=88
            IDENT(2)=88
            IDENT(3)=88
            IDENT(4)=IBL
            IDENT(5)=IBL
            IDENT(6)=IBL
            IDENT(7)=IBL
            IDENT(8)=IBL
            IDENT(9)=IBL
         ELSE
            IDENT(1)=KINT(8003)
            IDENT(2)=KINT(8004)
            IDENT(3)=KINT(8005)
cpssep02 Rewritten this section to decode call signs up to 9 chars
cps      (Bufr data descriptor 001011 is 72 bit wide, so allow 9 chars)
            ki=8005
            id=3
            do ii=1,6
               if(kint(ki+ii).eq.mindic) then
                  ident(id+ii)=32
               else     
                  ident(id+ii)=32
                  if(kint(ki+ii).ge.48.and.kint(ki+ii).le.90) 
     &                 ident(id+ii)=kint(ki+ii)
               end if
            end do
         END IF

C
C*    SET IU, K1, K2, K3 AND K4

C     KINT( 10)  - IU
C     KINT( 17)  - K1
         CALL IC2032(KINT(17),KDEC(30))
C     KINT( 18)  - K2
C     KINT(5001)  - K4
C     KINT(5002)  - K3

C*    WIND DIRECTION AND SPEED

cpsjun06 Next section rewritten, but note that currently we make no
cps      surface report for tesac, so this part produce no output
      IF(KINT(11).NE.MINDIC.AND. KINT(12).NE.MINDIC) THEN
         IF(KINT(11).EQ.99) THEN
            KDEC(26)=0
         ELSE IF(KINT(11).GT.36) THEN  ! unphysical value, set to missing
            KDEC(26)=MINDIC
         ELSE IF(KINT(11).EQ.0 ) THEN
cps      dd=0 means calm (so speed must be 0). If not, we regard it 
cps      as a typo for 360
            IF(KINT(12).GT.0 .AND. KINT(12).NE.MINDIC) THEN
               PRINT*,'dd=0, with ff>0. Changed to 360'
               KDEC(26) = 360
            END IF
         ELSE
            KDEC(26)=KINT(11)*10
         END IF
crr  change accuracy of wind by multiplying by 10.
         IF(KINT(10).EQ.0.OR.KINT(10).EQ.2) THEN    ! WIND SPEED IN (M/S)
            KDEC(27)=KINT(12)*10
         END IF

         IF(KINT(10).EQ.1.OR.KINT(10).EQ.3) THEN    ! WIND SPEED IN KNOTS
            KDEC(27)=KINT(12)*10
            CALL KTOMPSI(KDEC(27))
         END IF
      END IF

C*    Air temperature
      IF(KINT(14).NE.MINDIC.AND.KINT(15).NE.MINDIC) THEN
         IF(KINT(14).EQ.0) THEN
            KDEC(28)=KINT(15)
         END IF
         IF(KINT(14).EQ.1) THEN
            KDEC(28)=-KINT(15)
         END IF
      END IF

C*    Total water depth in metres
      KDEC(32)=KINT(8002)

C*    TEMPERATURE, SALINITY, CURRENT DIRECTION AND SPEED

cps ipkt=index in kint, ipkd=index in kdec (t and d are NOT abbr. for temperature and depth!)
      IPKT1=20
      IPKT2=205
      IPKD =38
      NTL=0

cps There is room for (4499-19)/6 = 939 levels in kint,
cps (2000-38)/3 = 654 levels in kdec
      DO I=1,654
         IF(KINT(IPKT1).NE.MINDIC) THEN   ! znznznzn
            NTL=NTL+1
            KDEC(IPKD)=KINT(IPKT1)
         END IF
         IF(KINT(IPKT1+2).NE.MINDIC) THEN ! TnTnTnTn
            IF(KINT(IPKT1+2).GT.5000) THEN
               KDEC(IPKD+1)=-KINT(IPKT1+2)+5000
            ELSE
               KDEC(IPKD+1)= KINT(IPKT1+2)
            END IF
         END IF
         IF(KINT(IPKT1+4).NE.MINDIC) THEN ! SnSnSnSn  
            KDEC(IPKD+2)=KINT(IPKT1+4)
         END IF
         IPKT1=IPKT1 + 6
         IPKD =IPKD  + 3
      END DO

      IPKT2=5004
      IPKD =2000
      NCL=0

      DO I=1,654
         IF(KINT(IPKT2  ).NE.MINDIC) THEN ! znznznzn
            NCL=NCL+1
            KDEC(IPKD)=KINT(IPKT2 )
         END IF
         IF(KINT(IPKT2+1).NE.MINDIC.AND.KINT(IPKT2+2).NE.MINDIC) THEN ! current
            KDEC(IPKD+1)=KINT(IPKT2+1)*10 ! dndn
            KDEC(IPKD+2)=KINT(IPKT2+2)    ! cncncn
         END IF
         IPKT2=IPKT2 + 4
         IPKD =IPKD  + 3
      END DO

C     -------------------------------------------------------------
C           3. Set up bufr expanded format for tesac data 
C              ------------------------------------------

      CALL TESACEXP(IERR )
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' KKDEC - after TESACEXP, IERR=',IERR
         RETURN
      END IF

      IF (.NOT.L31.AND..NOT.L32.AND..NOT.L33) THEN
         WRITE(*,*) 'Nothing interesting to decode. Skipping report'
         RETURN
      END IF

C     -------------------------------------------------------------
C           4. Quality control tesac basic surface report.
C              -------------------------------------------

C     Tesac surface report not implemented (never seen any anyway)
c$$$      IF(.NOT.L31) GO TO 600
      L31 = .FALSE.
      GO TO 600
c$$$      CALL QCTESAC(IERR)
C     -------------------------------------------------------------
C           5. Encode bufr message.
C              --------------------

C     First make tesac surface report
      CALL SET_DELAYED_REPLICATION()     ! sets kdata and kdlen
      CALL SET_DESCRIPTORS()

      CALL BUFR_ENCODE()

C     Clear flag for surface report
      L31 = .FALSE.
C
C     -------------------------------------------------------------
C           6. Move valuew into values array.
C              ------------------------------
 600  CONTINUE
      IF(.NOT.L32) GO TO 900
C     Then make oceanographic report
      CALL QCCON( IERR )

C     -------------------------------------------------------------
C           7. Quality control tesac sub-surface report.
C              -------------------------------------------

c$$$      CALL QCTESAC(IERR)

C     -------------------------------------------------------------
C           8. Encode bufr message.
C              --------------------

      CALL SET_DELAYED_REPLICATION()     ! sets kdata and kdlen
      CALL SET_DESCRIPTORS()

      CALL BUFR_ENCODE()

C     Clear flags for oceanographic report
      L32 = .FALSE.
      L33 = .FALSE.

 900  CONTINUE

      RETURN
      END
      SUBROUTINE FIXSO
C
C**** *FIXSO*
C
C
C     PURPOSE.
C     --------
C         TRY TO FIX ERROR.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *FIXSO*
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
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
C
      implicit none
      integer i
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
C     ------------------------------------------------------------------
C*          1.  CLEAR PARITI BITS .
C               -------------------
 100  CONTINUE
C
      DO 101 I=IPT,IGS
         KCHAR(I)=IAND(KCHAR(I),127)
 101  CONTINUE
C
C           1.1 CONVERT LETTERS INTO FIGURES IF NEEDED.
C               ---------------------------------------
 110  CONTINUE
C
      CALL REMEEE
C
      RETURN
      END
      SUBROUTINE JJINT (IHEAD,IERR)
C
C**** *JJINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT BATHY REPORTS  FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE (INTEGER) FORMAT
C
C         INPUT    : REPORT IN KCHAR(IPT) - KCHAR(IEQ),
C                    IN CCITT 5, 1 CHARACTER PER WORD.
C
C                    IHEAD = 0  INDICATES BULLETIN HEADER NOT ALREADY
C                               WRITTEN TO ERROR FILE
C                          = 1  MEANS HEADER ALREADY WRITTEN TO ERROR FILE
C
C                    IERR       NOT USED ON INPUT
C
C         OUTPUT   : REPORTS IN INTEGER FORMAT IN ARRAY 'KINT' IN DESIRED
C                    FORMAT (SEE SEPARATE DOCUMENTATION).
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *JJINT(IHEAD,IERR)*
C
C     METHOD.
C     -------
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING IS PROCE
C         UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON FATAL ERROR HAS BEEN DETECTED THE ROUTINE TR
C         TO FIND THE BEGINNING OF CORRECT DATA. IF CORRECT DATA
C         FOUND THE DECODING THE NUMBER OF NON-FATAL ERRORS IS CO
C         AND IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE DAT
C         AFTER THE FIRST ERROR ARE REMOVED (THIS IS DONE TO ENSU
C         THAT TOTALLY CORRUPTED DATA ARE NOT USED E.G. WRONG PAR
C         OF BATHY).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C            NUMERR = NUMBER OF NON-FATAL ERRORS
C            IREMPOS= THE POINTER TO FIRST ERROR IN 'KINT'.
C
C
C     EXTERNALS.
C     ----------
C
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
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
cpsjan07 Reordered kint and kdec: subsurface levels start at index 100 (not 18
cps      and 38), and packed consecutively in kdec (2 indices per level, not 5). 
cps      kint(338-361) shifted to kint(38-61). See kint.bath
C
C
      implicit none
      integer ihead,ierr
      integer keep,ifirst,numerr,irempos,nil,len,i,iret,ilo,ila
      integer ival,ipos,nc,ixx
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      include 'comdrau.f'   ! LaLaLaLaLa
C
C     ------------------------------------------------------------------
C*          1.  CLEAR FLAGS AND ERROR INDICATOR. KEEP POINTER TO FIRST
C               ------------------------------------------------------
C               CHARACTER OF REPORT.
C               --------------------
 100  CONTINUE
C
C     POINTER TO FIRST CHARACTER
C
      KEEP=IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST= 0
C
C     ERROR INDICATOR
C
 101  KERR   = 0
      NUMERR = 0
      IREMPOS= 10
C
C
C*             1.1 CHECK FOR 'NIL' REPORT.
C                  -----------------------
 110  CONTINUE
C
      NIL= 0
C
      LEN=IEQ-IPT
      IF(LEN.LE.30)
     &   THEN
            KERR=1
            NIL =1
            CALL ERRSTA(23,0,IFIRST,NUMERR)
         END IF
C
C*            1.2  SET AREA FOR INTERMEDIATE (INTEGER) REPORT TO
C                  MISSING DATA VALUE
C
 120  CONTINUE
C
      DO I=1,jp15
         KINT(I)=MINDIC
      END DO
C
C
C*          1.3 CLEAR DATA FOR PREVIOUS REPORT FROM DECODED
C               REPORT HEADER AREA.
C
 130  CONTINUE
C
      DO 131 I=1,3
         KDEC(I)=MINDIC
 131  CONTINUE
C
      DO 132 I=5,9
         KDEC(I)=MINDIC
 132  CONTINUE
C
C
      KDEC(13)=MINDIC
      KDEC(16)=MINDIC
      KDEC(17)=MINDIC
C
      KDEC(20)=0
      KDEC(22)=MINDIC
      KDEC(23)=MINDIC
C     ------------------------------------------------------------------
C           2.  SECTION 1 - IDENTIFICATION AND POSITION DATA;
C               (WHENEVER AVAILABLE) SURFACE PRESSURE AND SEA SURFACE
C               TEMPERATURE; (OPTIONAL) WIND,AIR TEMPERATURE
C               PRESSURE TENDENCY.
C
 200  CONTINUE
C
C     YYMMJ - GROUP
C
      CALL NEXTFIG(IPT,IEQ)
      IF(IPT .GT. IEQ) GO TO 500
C
      CALL EXTGRP(IPT,2,2,1,0,0,1,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=2
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            GO TO 500
         END IF
C
C     YY GG /
C     -- -- -
      CALL NEXSEP2 (IPT,IEQ,*500)
      CALL NEXPRT2 (IPT,IEQ,*500)
C
      CALL EXTGRP(IPT,2,2,1,0,0,4,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=3
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            GO TO 500
         END IF
C
      IF(KINT(1).LT.1.OR.KINT(1).GT.31)
     &   THEN
            IRET=1
            KINT(1)=MINDIC
         END IF
      IF(KINT(2).LT.1.OR.KINT(2).GT.12)
     &   THEN
            IRET=2
            KINT(2)=MINDIC
         END IF
C
      IF(KINT(3).LT.0.OR.KINT(3).GT.9) THEN
         IRET=3
         KINT(3)=MINDIC
      END IF
C 
      IF(KINT(4).LT.0.OR.KINT(4).GT.24)
     &   THEN
            IRET=2
            KINT(4)=MINDIC
         END IF
      IF(KINT(5).LT.0.OR.KINT(5).GT.59)
     &   THEN
            IRET=2
            KINT(4)=MINDIC
         END IF
C
      IF(IRET.NE.0)
     &   THEN
C
C           CHECK IF ALREADY KNOWN TO BE 'NIL' - REPORT
C
            IF(NIL.EQ.1) THEN
               KERR=1
               RETURN
            END IF
C
            KERR=2
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            RETURN
C
         END IF
C
C     QCLALALALA - GROUP
C     - - - - -
C
      CALL NEXSEP2(IPT,IEQ,*500)
      CALL NEXPRT2(IPT,IEQ,*500)
C
      if(kchar(ipt+6).eq.32) then
         if(kchar(ipt+5).eq.47) kchar(ipt+5)=48
         if(kchar(ipt+4).eq.47) kchar(ipt+4)=48
         LaLaLaLaLa = .true.
         ILO=6
         ILA=5
      elseif(kchar(ipt+5).eq.32) then
         LaLaLaLaLa = .false.
         ILO=5
         ILA=4
      end if

      CALL EXTGRP (IPT,1,ILA,0,0,0,7,IRET)
      IF(IRET.NE.0) THEN
         KERR=4
         CALL ERRSTA(23,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
         GO TO 500
      END IF
C
C     LOLOLOLOLO - GROUP
C     - - - - -
C
      CALL NEXSEP2(IPT,IEQ,*500)
      CALL NEXPRT2(IPT,IEQ,*500)
C
      if(kchar(ipt+6).eq.32) then
         if(kchar(ipt+5).eq.47) kchar(ipt+5)=48
         if(kchar(ipt+4).eq.47) kchar(ipt+4)=48
      end if
c
      CALL EXTGRP (IPT,ILO,0,0,0,0,9,IRET)
      IF(IRET.NE.0) THEN
         KERR=5
         CALL ERRSTA(23,1,IFIRST,NUMERR)
         IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
         GO TO 500
      END IF
C-----------------------------------------------------------------------
 210  CONTINUE
C
C     CHECK IF THE NEXT GROUP IS BEGINING OF SECTION 2
C
      CALL NEXSEP2(IPT,IEQ,*500)
      CALL NEXPRT2(IPT,IEQ,*500)
C
      IF(KCHAR(IPT  ) .EQ. 56.AND.
     &   KCHAR(IPT+1) .EQ. 56.AND.
     &   KCHAR(IPT+2) .EQ. 56.AND.
     &   KCHAR(IPT+3) .EQ. 56.AND.
     &   KCHAR(IPT+5) .LE. 32       )
     &   THEN
C           BEGINING OF THE SECTION 2 FOUND
            GO TO 300
         END IF
C
C     THERE ARE SOME USEFULL INFORMATION IN SECTION 1
C
      CALL EXTVAL(IPT,1,IVAL)
C
C
      IF(IVAL .EQ. 4)
     &   THEN
C           AIR TEMPERATURE FOUND
C
            CALL EXTGRP (IPT,1,1,3,0,0,13,IRET)
            IF(IRET.NE.0)
     &          THEN
                   KERR=7
                   CALL ERRSTA(23,1,IFIRST,NUMERR)
                   IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
                   IF(NUMERR.EQ.1) IREMPOS=13
                END IF
C
            GO TO 210
C
         ELSE
            IF(KINT(10).EQ.MINDIC)
     &         THEN
C
C                 WIND DIRECTION AND SPEED
C
                  CALL EXTGRP (IPT,1,2,2,0,0,10,IRET)
                  IF(IRET.NE.0)
     &                THEN
                         KERR=9
                         CALL ERRSTA(23,1,IFIRST,NUMERR)
                         IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
                         IF(NUMERR.EQ.1) IREMPOS=10
                      END IF
C
               END IF
C
         END IF
C
      GO TO 210
C-----------------------------------------------------------------------
C*           3.  SECTION 2 - TEMPERATURES AT EITHER SIGNIFICANT
C                OR SELECTED DEPTHS
C
 300  CONTINUE
C
cps                    8888k1
      CALL EXTGRP (IPT,4,1,0,0,0,16,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=10
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=16
         END IF
C-----------------------------------------------------------------------
cpsaug01 Inserted extraction of IxIxIxXrXr group
cps      Presently this is not decoded to bufr
      CALL NEXSEP2(IPT,IEQ,*500)
      CALL NEXPRT2(IPT,IEQ,*500)
      CALL EXTGRP (IPT,3,2,0,0,0,60,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=10
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=60
         END IF
cps
      IPOS=100
C
 310  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*500)
      CALL NEXPRT2(IPT,IEQ,*500)
C
      IF(KCHAR(IPT  ) .EQ. 48.AND.
     &   KCHAR(IPT+1) .EQ. 48.AND.
     &   KCHAR(IPT+2) .EQ. 48.AND.
     &   KCHAR(IPT+3) .EQ. 48.AND.
     &   KCHAR(IPT+4) .EQ. 48.AND.
     &   KCHAR(IPT+5) .LE. 32       )
     &   THEN
C           END OF SECTION 2 REACHED
            GO TO 320
         END IF
C
      IF(KCHAR(IPT  ) .EQ. 54.AND.
     &   KCHAR(IPT+1) .EQ. 54.AND.
     &   KCHAR(IPT+2) .EQ. 54.AND.
     &   KCHAR(IPT+3) .EQ. 54.AND.
     &   KCHAR(IPT+4) .EQ. 54.AND.
     &   KCHAR(IPT+5) .LE. 32       )
     &   THEN
C           BEGINING OF THE SECTION 3 FOUND
            GO TO 400
         END IF
C
      IF(KCHAR(IPT  ) .EQ. 57.AND.
     &   KCHAR(IPT+1) .EQ. 57.AND.
     &   KCHAR(IPT+2) .EQ. 57.AND.
     &   KCHAR(IPT+3) .EQ. 57.AND.
     &   KCHAR(IPT+4) .EQ. 57.AND.
     &   KCHAR(IPT+5) .LE. 32       )
     &   THEN
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 450
         END IF
C
      IF(KCHAR(IPT ).GE.65.AND.KCHAR(IPT  ).LE.90)
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
cps NC is number of characters in call sign
            NC=KPT-IPT
C
            DO 311 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I) 
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I) 
               END IF
C
 311        CONTINUE
C
cpssep02 Inserted to agree with mdb_buoy.f
            do 3111 ixx=49,45+nc,-1
              kint(ixx)=32
 3111       continue
cps end insert
            GO TO 500
C
         END IF
C
      IF(KCHAR(IPT  ).GE.48.AND.KCHAR(IPT  ).LE.57 .AND.
     &   KCHAR(IPT+1).GE.65.AND.KCHAR(IPT+1).LE.90       )
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 312 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I)
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I)
               END IF
C
 312        CONTINUE
C
cpssep02 Inserted to agree with mdb_buoy.f
            do 3112 ixx=49,45+nc,-1
              kint(ixx)=32
 3112       continue
c end insert
            GO TO 500
C
         END IF
C
C
C*    TEMPERATURES AT SIGNIFICANT DEPTHS FOLLOW
C
      IF(KCHAR(IPT  ).EQ.57.AND.
     &   KCHAR(IPT+1).EQ.57.AND.
     &   KCHAR(IPT+2).EQ.57     )
     &   THEN
cps         999zz         
            CALL EXTGRP (IPT,3,2,0,0,0,IPOS,IRET)
            IF(IRET.NE.0)
     &         THEN
                  KERR=12
                  CALL ERRSTA(23,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=IPOS
               END IF
C
            IPOS=IPOS+2
            GO TO 310
C
         END IF
C
cps   zzTTT
      CALL EXTGRP (IPT,2,3,0,0,0,IPOS,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=12
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=IPOS
         END IF
C
      IPOS=IPOS+2
C
      GO TO 310
C-----------------------------------------------------------------------
 320  CONTINUE
C
cps   00000
      CALL EXTGRP (IPT,5,0,0,0,0,38,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=13
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=38
         END IF
C
      GO TO 310
C     -----------------------------------------------------------------
C*            4. SECTION 3 - TOTAL WATER DEPTH AND SURFACE
C                ------------------------------------------
C                CURRENT (OPTIONAL).
C                -------------------
 400  CONTINUE
C
cps   66666
      CALL EXTGRP (IPT,5,0,0,0,0,39,IRET)
      IF(IRET.NE.0)
     &   THEN
            KERR=14
            CALL ERRSTA(23,1,IFIRST,NUMERR)
            IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
            IF(NUMERR.EQ.1) IREMPOS=39
         END IF
C
 410  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*500)
      CALL NEXPRT2(IPT,IEQ,*500)
C
C
      IF(KCHAR(IPT  ) .EQ. 57.AND.
     &   KCHAR(IPT+1) .EQ. 57.AND.
     &   KCHAR(IPT+2) .EQ. 57.AND.
     &   KCHAR(IPT+3) .EQ. 57.AND.
     &   KCHAR(IPT+4) .EQ. 57.AND.
     &   KCHAR(IPT+5) .LE. 32       )
     &   THEN
C           BEGINING OF THE SECTION 4 FOUND
            GO TO 450
         END IF
C
      IF(KCHAR(IPT ).GE.65.AND.KCHAR(IPT  ).LE.90)
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 411 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I) 
               END IF
C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I) 
               END IF
C
 411        CONTINUE
C
cpssep02 Inserted to agree with mdb_buoy.f
            do 4111 ixx=49,45+nc,-1
               kint(ixx)=32
 4111       continue
cps end insert
            GO TO 500
C
         END IF
C
      IF(KCHAR(IPT  ).GE.48.AND.KCHAR(IPT  ).LE.57 .AND.
     &   KCHAR(IPT+1).GE.65.AND.KCHAR(IPT+1).LE.90       )
     &   THEN
C           PROBABLY SHIP'S CALL SIGN FOUND
C
            KPT=IPT
            CALL NEXTSEP(KPT,IEQ)
cpssep02 Inserted to agree with mdb_buoy.f
            if(kpt.gt.ieq) kpt=ieq
C
            NC=KPT-IPT
C
            DO 412 I=1,NC
C
            IF(KCHAR(IPT-1+I).GE.65.AND.KCHAR(IPT-1+I).LE.90)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I)
               END IF

C
            IF(KCHAR(IPT-1+I).GE.48.AND.KCHAR(IPT-1+I).LE.57)
     &         THEN
                  KINT(45-1+I)=KCHAR(IPT-1+I)
               END IF
C
 412        CONTINUE
C
cpssep02 Inserted to agree with mdb_buoy.f
            do 4112 ixx=49,45+nc,-1
               kint(ixx)=32
 4112       continue
cps end insert
            GO TO 500
C
         END IF
C
C
      CALL EXTVAL(IPT,1,IVAL)
cpssep02      IF(IVAL.EQ.1) THEN
cps   if ival=1 we cannot really know if this is start of 1ZdZdZdZd or
cps   start of k5DcDcVcVc with k5=1, unless 1ZdZdZdZd is already decoded.
cps   WMO could have avoided this ambiguity by replacing '1' with '6' 
cps   (or bigger), as the allowed values of k5 are 0-5
      IF(IVAL.EQ.1 .and. kint(40).ne.mindic) THEN
cps         1ZdZdZdZd Total water depth        
            CALL EXTGRP (IPT,1,4,0,0,0,40,IRET)
            IF(IRET.NE.0)
     &         THEN
                  KERR=15
                  CALL ERRSTA(23,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=40
                END IF
C
            GO TO 410

      ELSE
cps         k5DcDcVcVc Indicator for the method of current measurement, 
cps         surface current direction and speed (in tenths of knot)
            CALL EXTGRP (IPT,1,2,2,0,0,42,IRET)
            IF(IRET.NE.0)
     &         THEN
                  KERR=16
                  CALL ERRSTA(23,1,IFIRST,NUMERR)
                  IF(IFIRST.NE.0) NOER(23,KERR)=NOER(23,KERR)+1
                  IF(NUMERR.EQ.1) IREMPOS=42
                END IF
C
            GO TO 410
C
      END IF
C-----------------------------------------------------------------------
C*                4.5 SECTION 4 (A1BWNBNBNB)
 450  CONTINUE
C
      CALL NEXSEP2(IPT,IEQ,*500)
      CALL NEXPRT2(IPT,IEQ,*500)
C
      KINT(45)=KCHAR(IPT  )
      KINT(46)=KCHAR(IPT+1)
      KINT(47)=KCHAR(IPT+2)
      KINT(48)=KCHAR(IPT+3)
      KINT(49)=KCHAR(IPT+4)
      NC=5

C-----------------------------------------------------------------------
C*            5. ERROR CHECKING AND HANDLING .
C                -----------------------------
 500  CONTINUE
C
C     RETURN IF NO ERRORS IN REPORT
C
      IF(KERR.EQ.0) RETURN
C
C     IF FIRST DECODING ATTEMPT ,TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF(IFIRST.EQ.0) THEN
                         IPT=KEEP
                         CALL FIXSO
                         IFIRST=1
                         NUMRERR(23)=NUMRERR(23)+1
                         GO TO 101
                      ELSE
                         CALL SAVREP(IHEAD,IERR)
C
C                        CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
                         DO 501 I=KEEP,IGS
                         KCHAR(I)=IAND(KCHAR(I),127)
 501                     CONTINUE
C
C                        ONLY REPORTS WITH ERROR IN DATE/TIME
C                        OR LAT/LONG ARE NOT PROCESSED ANY FURTHER
C
C                        IF THERE ARE MORE THEN 5 NON-FATAL ERRORS
C                        IN REPORT IT IS ONLY PROCESSED UNTIL THE
C                        FIRST ERROR
C
                         IF(NUMERR.GT.5) THEN
                            DO I=IREMPOS,jp15
                               KINT(I)=MINDIC
                            END DO
                         END IF
C
                         IF(KERR.GT.6) KERR=0
                      END IF
C
 503  CONTINUE
C
C     CALL PRTKINT(KINT,1,3000,MINDIC)
C
      RETURN
      END
      SUBROUTINE BULLSO( IERR )
C
C**** *BULLSO*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF INDIVIDUAL
C         REPORTS FROM BULLETINS OF 'BATHY' AND 'TESAC' REPORTS.
C
C         INPUT    : BULLETIN IN CCITT 5 CHARACTER, 1 CHARACTER PER
C                    WORD IN 'KCHAR'.
C
C                    POINTERS TO BEGINNING AND END OF 'STARTING LINE',
C                    'MIMIMJMJ LINE' AND END OF BULLETIN.
C
C                    IERR NOT USED
C
C         OUTPUT   : DECODED REPORTS WRITTEN TO FILE AND ERROR REPORTS
C                    TO ERROR FILE.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLSO(IERR)*
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *PRTBULL(I,J)*
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
      integer ihead,k,i,len,iipt,npt,misseq

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      include 'commsys.f'

C
C*          1.1 SET FLAGS AND WORKING POINTERS.
C               -------------------------------

C     WORKING POINTER SET TO POINT TO FIRST LETTER OF MIMIMJMJ

      IPT = IMI

C     CLEAR FLAG WHICH INDICATES BULLETIN HEADER ALREADY WRITTEN
C     TO ERROR FILE

      IHEAD= 0

C*           1.2  SET DATA REPORT TYPE INDICATORS IN DECODED
C                 ------------------------------------------
C                 REPORT HEADER AND PART OF REPORT INDICATOR.
C                 ------------------------------------------

 100  CONTINUE

      K= IPT + 10
C     CHECK IF VALID MIMI OF 'MIMIMJMJ'.

      DO I=IPT,K
C     Check if it is 'BATHY' report (JJVV or JJYY)
         IF ((KCHAR( I ) .EQ. 74 .AND. KCHAR(I+1) .EQ. 74 .AND.
     &        KCHAR(I+2) .EQ. 86 .AND. KCHAR(I+3) .EQ. 86)
     &        .or. ( KCHAR( I ) .EQ. 74 .AND. KCHAR(I+1) .EQ. 74 .AND.
     &        KCHAR(I+2) .EQ. 89 .AND. KCHAR(I+3) .EQ. 89)) THEN
C     Return if it is tesac we want to decode
            IF (MSYS.EQ.16) RETURN
            KDEC(4)=  63
            IPT    =  I
            GO TO 200
         END IF
C     Check if it is 'TESAC' report (KKYY)
         IF (KCHAR( I ) .EQ. 75 .AND. KCHAR(I+1) .EQ. 75 .AND.
     &        KCHAR(I+2) .EQ. 89 .AND. KCHAR(I+3) .EQ. 89) THEN
C     Return if it is bathy we want to decode
            IF (MSYS.EQ.15) RETURN
            KDEC(4)=  64
            IPT    =  I
            GO TO 200
         END IF
C     Added check for waveob and trackob reports. In these cases
C     we just want to stop further processing, without writing 
C     anything to error file

C     Return if it is a 'WAVEOB' (MMXX) or 'TRACKOB' (NNXX) report
         IF (KCHAR( I ) .EQ. 77 .AND. KCHAR(I+1) .EQ. 77 .AND.
     &     KCHAR(I+2) .EQ. 88 .AND. KCHAR(I+3) .EQ. 88) THEN
            RETURN
         END IF
         IF (KCHAR( I ) .EQ. 78 .AND. KCHAR(I+1) .EQ. 78 .AND.
     &        KCHAR(I+2) .EQ. 88 .AND. KCHAR(I+3) .EQ. 88) THEN
            RETURN
         END IF
      END DO

C*         1.3 INVALID 'MIMI'. WRITE REPORT TO ERROR FILE
C              ------------------------------------------

      KCHAR(IPT)=IOR(KCHAR(IPT),128)
      IEQ=IPT
      CALL NEXTEQ (IEQ,IGS)
      IF(IEQ.GT.IGS) RETURN

C     See if report too short
      LEN = IEQ-IPT
      IF(LEN.LT.17) GO TO 300

      KDEC(4) = 63    ! In savrep it doesn't matter whether bathy or tesac...
      CALL SAVREP(IHEAD,IERR)
      GO TO 300

C*          2.3 LOCATE END OF REPORT
C               --------------------
 200  CONTINUE

      IEQ=IPT
      CALL NEXTEQ(IEQ,IGS)
      IF (IEQ.GT.IGS) THEN
         WRITE(*,*) ' = sign missing at the end of report '
         MISSEQ = 1 
         IEQ = IGS
      ENDIF

C     Check if '=' sign is missing, trying to find the next ZZYY
      CALL NEXTMI(IPT,IEQ,IIPT,KDEC(4))
      IF (IIPT.NE.9999) THEN
         IF (IIPT.LT.IEQ) THEN
            IF (MISSEQ.NE.1) 
     *           WRITE(*,*) ' = sign missing at the end of report '
            IEQ = IIPT
         END IF
      END IF

      IF (KDEC(4).EQ.63) THEN

C            2.4 DECODE BATHY REPORT.
C                --------------------

C     Convert to intermediate (integer) format
         CALL JJINT(IHEAD,IERR)
         IF (KERR.NE.0 .OR. IERR.NE.0) THEN
            WRITE(*,*) ' BULLSO - after call jjint, kerr,ierr',KERR,IERR
         ELSE
C     Convert intermediate (integer) format to decoded form
C     and write to file
            CALL JJDEC(IHEAD,IERR)
            IF (KERR.NE.0 .OR. IERR.NE.0) THEN
               WRITE(*,*) ' BULLSO - after call jjdec, kerr,ierr',
     *              KERR,IERR
            END IF
         END IF
      END IF

      IF (KDEC(4).EQ.64) THEN

C*                2.5  DECODE TESAC REPORT.
C                      --------------------

C     Convert to intermediate (integer) format
         CALL KKINT(IHEAD,IERR)
         IF (KERR.NE.0 .OR. IERR.NE.0) THEN
            WRITE(*,*) ' BULLSO - after call kkint, kerr,ierr',KERR,IERR
         ELSE
C     Convert intermediate (integer) format to decoded form
C     and write to file
            CALL KKDEC(IHEAD,IERR)
            IF (KERR.NE.0 .OR. IERR.NE.0) THEN
               WRITE(*,*) ' BULLSO - after call kkdec, kerr,ierr',
     *              KERR,IERR
            END IF
         END IF

      END IF
C      ----------------------------------------------------------------
C*            3.   LOCATE START OF NEXT REPORT.
C                  ----------------------------
 300  CONTINUE

      IPT = IEQ + 1
      CALL NEXTPRT(IPT,IGS)

C*            3.1  CHECK FOR END OF BULLETIN.
C                  --------------------------

      IF (IPT.GE.IGS) RETURN  ! This shouldn't be possible for correct 
                              ! formatted bulletines, but might happen if
                              ! '=' is missing at end of report

C     Search the beginning of next report
      NPT = IEQ
      CALL NEXTLET(NPT,IGS)
      IF(NPT.GE.IGS) RETURN
      CALL NEXTEND(NPT,IGS)
      IF(NPT.GE.IGS) RETURN

C     Go to next report
      GO TO 100

      END
      SUBROUTINE QCCON( IERR )
C
C**** *QCCON*
C
C
C     PURPOSE.
C     --------
C         MOVE VALUEW ARRAY INTO VALUES ARRAY FOR INPUT TO Q/C.
cps valueS is used for surface report,
cps valueW is used for oceanographic report
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *QCCON( IERR )*
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
C         NONE.
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
      integer i,j
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'
C
C     -----------------------------------------------------------------
C*          1. MOVE CONTENT OF VALUEW ARRAY INTO VALUES.
C              -----------------------------------------
 100  CONTINUE
C
      M = MS
C
      DO J=1,JP1
         DO I=1,JP22
            VALUES(I,J)=999999.
            VALUES(I,J)=VALUEW(I,J)
            VALUEW(I,J)=999999.
         END DO
      END DO
C
      RETURN
      END

      SUBROUTINE IC2032(INK1,OUTK1)
C****
C*
C*    NAME     : IC2032
C*
C*    FUNCTION :  DECODE THE INDICATOR FOR DIGITIZATION
C*                CODE TABLE 2262 IN SYNOP, TABLE 2032 IN BUFR
C*
C*    INPUT    :  INK1    - SYNOP CODE FIGURE FOR K1
C*
C*    OUTPUT   :  OUTK1   - BUFR CODE FIGURE FOR K1
C*
C*             OUTK1 IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INK1
C*
C*    P. SANNES  SEP.06
C*
C****
C
      IMPLICIT NONE
      INTEGER INK1,OUTK1
C
      IF (INK1.EQ.7) THEN
         OUTK1 = 0
      ELSE IF (INK1.EQ.8) THEN
         OUTK1 = 1
      ELSE
         OUTK1 = 3   ! Missing value in Bufr
      END IF
    
      RETURN
      END

      subroutine get_year(YY,MM,J,nyear)
C     Function:   Compute year (4 digits) from day, month and one digit year
C     Input:      YY,MM,J = day,month,year (one digit) from group YYMMJ
C     Output:     nyear (4 digits), set to mindic if not possible to compute
C     Calls:      datum
C     Background: More than 15 days old observations are not uncommon for
C                 drau/bathy/tesac. So utilize group YYMMJ as far as possible
C     Author:     P. Sannes dec 2006

      implicit none
      integer YY,MM,J,nyear,month,year,i
      include 'parameter.f' ! jpXX for arrays in comwork
      include 'comwork.f'   ! mindic

      nyear=mindic
      if (MM.ne.mindic .and. J.ne.mindic) then
         nyear=J+2000
         if(YY.ne.mindic) then
            call datum(YY,month,year)
            do i=1,9            ! Should work until 2099
               if (iabs(year-nyear).gt.1) then
                  nyear=nyear+10
               end if
            end do
            if (iabs(year-nyear).gt.1) nyear=mindic   ! we give up
         end if
      end if

      return
      end
