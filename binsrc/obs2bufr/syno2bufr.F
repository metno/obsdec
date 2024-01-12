        PROGRAM SYNO2BUFR 
C
C**** *SYNO2BUFR*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING
C         SYNOP DATA.
C
cpsaug06  Rewritten:
c  - Use BUFREN i libemos for BUFR encoding
c  - No quality control (no call for qcsyno, and no confidence values produced)
c  - No section 2
c  - No index file (.key) created
c  - Bufr messages written sequentially with no garbage between to file bufr.dat
c    (expected to be processed by updBufr next).
c  - No need for byte swapping, as everything written is big endian
c  - IMPLICIT NONE used everywhere
c  - INCLUDE files now contains declaration of variables also
c  - libdec.a stripped considerably 
c  - no read from std_atm.dat 
c  - RRR=990 decoded as 0 in Bufr
cpsmars09 Use templates 307079 (land) and 308009 (sea)
c  - RRR=990, R24R24R24R24=9999 decoded as -0.1 in Bufr
c  - sss=997,998 decoded as -0.01, -0.02
c  - 333 58(9)p24p24p24 decoded in new KDEC(140), no mix with 5appp      
cpsapr09 Added fxfx: 004025 011042 at end of both templates
      
C
C**   INTERFACE.
C     ----------
C
c              msysdsi        YMSYS(1:ndx)//CDATFILE(msys)//'dataXXxx.dat' (usually msysdsi=4, opened in msysinit.f,
                                                                           never closed explicitely. So why does this
                                                                           not interfere with the next file open?)
c              sunit          bufr.dat  (opened in msysinit.f)
c              4              YPPDAT(1:ndx)//'station_amend.dat' (opened in station.f)
c              11             cstream(msys)//'/'syno_error_file.dat'
c              17             cfstamp(msys)//'.timestamp.dat' (opened in msysinit.f)
c              55             cstream(msys)//'/'//'unknown_syno_station.dat'
c              77             ...dir.dat  (argument to program)
c              IU=90/91...    cstream(msys//'/'//CFNAME//'.rdb'
c              IUNIT=90/91... cstream(msys//'/'//CFNAME//'.key'
c              93             cstream(msys)//'stop.dat' (an empty file will be created if not present)
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
C        *CALL* *INITVAR( IERR )*
C        *CALL* *MSYSINIT( IERR )*
C        *CALL* *PROCRFB( IERR,T1,T1alt )*
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
      integer narg,ios,ierr,iunit,i
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comdir.f'
      INCLUDE 'commdbc.f'    ! ctstamp,cstream
      INCLUDE 'commsys.f'
      include 'comwrite.f'   ! num_records
c
      CHARACTER dir_file*80
      character*15 cstop
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
      if(narg.lt.1) then
         print*,''
         print*,' Usage: syno2bufr dir_file cmsys timestamp <cmsystop>'
         print*,'                                      (optional)'
         print*,' where  dir_file  - specification of directories'
         print*,'        cmsys     - msys file number (2 = syno)'
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
     1            FILE=dir_file,
     2     form='formatted',
     3     recl=80,
     3     status='unknown' )
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) ymsys
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) yppdat
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) ydatdef
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) ytest
       close(77)
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
cps      print*,' IUNIT ',IUNIT
      open(unit=iunit,iostat=ios,err=1001,
     1            FILE=cstream(msys)//'.stop.dat',
     2     form='formatted',
     3     status='unknown' )
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
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' SYNO2BUFR - after getnext, ierr ',IERR
CTEST
      IF(IERR.NE.0) THEN
         IF(IERR.EQ.7) THEN
CRR IERR=7 originally for timeout(i.e.no data to read). Here it is 
CRR changed to mean that we have read all data (i.e. gone round msys file).
            IERR=0
C
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
         PRINT*,' SYNO2BUFR - after PROCRFB, IERR= ',IERR
         GO TO 200
      END IF
C     ------------------------------------------------------------------
C*          4.   DECODE BULLETIN HEADER.
C                -----------------------
 400  CONTINUE
C
      CALL PROCHDR( IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' SYNO2BUFR - after PROCHDR, KERR= ',KERR
CTEST
      IF(KERR.NE.0) GO TO 200
C     ------------------------------------------------------------------
C*          5.   CHECK TEXT OF BULLETIN.
C                -----------------------
 500  CONTINUE
C
      CALL PROCTXT( IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' SYNO2BUFR - after PROCTXT, KERR= ',KERR
CTEST
      IF(KERR.NE.0) GO TO 200
C
C     ------------------------------------------------------------------
C*          6.   CALL APPROPRIATE ROUTINE TO DECODE SYNOP DATA.
C                ---------------------------------------------
 600  CONTINUE
C
      CALL PROCT1S( IERR )

CTEST
      IF(IERR.NE.0)
     1 PRINT*,' SYNO2BUFR - after PROCT1S, IERR= ',IERR
CTEST
C
C
      GO TO 200
C
C     ------------------------------------------------------------------
C
C
 1001 continue
c
      print*,' Open error on TEMP.stop.dat'
      go to 1000
c
 1002 continue
c
      print*,' Read error on TEMP.stop.dat'
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
      SUBROUTINE PROCT1S ( IERR )
C
C**** *PROCT1S*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING SURFASE
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
C         *CALL* *BULLSM(IERR)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C
      implicit none
      integer ierr
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
C     ------------------------------------------------------------------
C*          1.  CALL APPROPRIATE ROUTINE, 'IT2' CONTAINS AN INTEGER IN THE
C                ---------------------------------------------------------
C               RANGE 1-26, CORRESPONDING TO 'T2' OF ABBREVIATED HEADER.
C                -------------------------------------------------------
 100  CONTINUE
C
C
      GOTO ( 110,120,130,140,150,160,170,180,190,200,210,220,230,
     C       240,250,260,270,280,290,300,310,320,330,340,350,360) IT2
C
C     -----------------------------------------------------------------
C*              1.1  BULLETINS WITH 'TT' = 'SA'.
C                    ----------------------------------------------
 110  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.2  BULLETINS WITH 'TT' = 'SB'.
C                    ---------------------------
 120  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.3  BULLETINS WITH 'TT' = 'SC'.
C                    ---------------------------
  130 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.4  BULLETINS WITH 'TT' = 'SD'.
C                    ---------------------------
  140 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.5  BULLETINS WITH 'TT' = 'SE'.
C                    ----------------------------
  150 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.6  BULLETINS WITH 'TT' = 'SF'.
C                    ---------------------------
  160 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.7  BULLETINS WITH 'TT' = 'SG'.
C                    ----------------------------
  170 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.8  BULLETINS WITH 'TT' = 'SH'.
C                    ---------------------------
  180 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.9  BULLETINS WITH 'TT' = 'SI'. SYNOP INTERMED.HOURS.
C
  190 CONTINUE
      CALL BULLSI( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.0  BULLETINS WITH 'TT' = 'SJ'.
C                    ---------------------------
 200  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.1  BULLETINS WITH 'TT' = 'SK'.
C                    ---------------------------
 210  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.2  BULLETINS WITH 'TT' = 'SL'.
C                    ---------------------------
 220  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.3  BULLETINS WITH 'TT' = 'SM'. SYNOP MAIN HOURS.
C                    ---------------------------
 230  CONTINUE
      CALL BULLSM( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.4  BULLETINS WITH 'TT' = 'SN'. NON-STANDARD HOUR.
C                    ---------------------------
 240  CONTINUE
      CALL BULLSN( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.5  BULLETINS WITH 'TT' = 'SO'.
C                    ---------------------------
 250  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.6  BULLETINS WITH 'TT' = 'SP'.
C                    ---------------------------
 260  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.7  BULLETINS WITH 'TT' = 'SQ'.
C                    ---------------------------
 270  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.8  BULLETINS WITH 'TT' = 'SR'.
C                    ---------------------------
 280  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.9  BULLETINS WITH 'TT' = 'SS'.
C                    ---------------------------
 290  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              3.0  BULLETINS WITH 'TT' = 'ST'.
C                    ---------------------------
 300  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              3.1  BULLETINS WITH 'TT' = 'SU'.
C
 310  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.2  BULLETINS WITH 'TT' = 'SV'.
C
 320  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.3  BULLETINS WITH 'TT' = 'SW'.
C
 330  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.4  BULLETINS WITH 'TT' = 'SX'.
C
 340  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.5  BULLETINS WITH 'TT' = 'SY'.
C
 350  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.6  BULLETINS WITH 'TT' = 'SZ'.
C
 360  CONTINUE
      RETURN
C
      END
      SUBROUTINE BULLSM ( IERR )
C
C**** *BULLSM*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF INDIVIDUAL
C         REPORTS FROM BULLETINS OF SURFACE OBSERVATIONS.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLSM(IERR)*
C
C          INPUT      : BULLETIN IN CCITT 5 CHARACTERS , 1 CHARACTER PER
C                       WORD IN 'KCHAR' .
C
C                       POINTERS TO BEGINNING AND END OF 'STARTING LINE' ,
C                       'ABBREVIATED HEADING','MIMIMJ LINE' AND END OF
C                       BULLETIN .
C
C                       IERR NOT USED.
C
C          OUTPUT     : DECODED REPORTS WRITTEN TO FILE AND ERROR
C                       REPORTS TO ERROR FILE.
C
C                       IERR = 1 , IF ANY FILE HANDLING ERROR .
C
C     METHOD.
C     -------
C
C         THIS ROUTINE HAS 3 ENTRY POINTS. SYNOP AND SHIP
C         REPORTS FOR MAIN,INTERMEDIATE AND NON-STANDARD HOURS
C         ARE DECODED BY THIS ROUTINE.
C
C         THIS MAINTAINS SUBROUTINE NAMING CONVENTIONS.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *SMDEC(IHEAD,IERR)*
C         *CALL* *SMINT(IHEAD,IERR)*
C
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

      ENTRY BULLSI ( IERR )
      ENTRY BULLSN ( IERR )

      implicit none
      integer ierr
      integer isyygg,ihead,len,k1,k2,k,iret,i,iipt

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'    ! noer
C     ------------------------------------------------------------------

C*          1.   SET FLAGS AND WORKING POINTERS.
C                -------------------------------
C
C*    SHIP REPORTS IN OLD CODE ARE NOT HANDLED BY THIS ROUTINE SO
C     CHECK FOR MIMIMJMJ OF NNXX .
C
      IF ( KCHAR(IMI).EQ.78.AND.KCHAR(IMI+1).EQ.78) RETURN

cpsnov05 We are not yet able to handle synop mobil (OOXX)
cps      No natural place to put them in obs files (but we ought
cps      to be able to decode to Bufr!)
      IF ( KCHAR(IMI).EQ.79.AND.KCHAR(IMI+1).EQ.79) RETURN

C     WORKING POINTER SET TO POINT TO FIRST LETTER OF MIMIMJMJ
C     GROUP.
      IPT = IMI

C     CLEAR FLAG WHICH INDICATES YYGGIW GROUP PRESENT.
      ISYYGG = 0

C     CLEAR FLAG WHICH INDICATES BULLETIN HEADER ALREADY WRITTEN
C     TO ERROR FILE.
      IHEAD = 0

C*    HANDLE MIMIMJMJ LINE. SET DATA BASE REPORT TYPE INDICATORS
C     IN DECODED REPORT HEADER AND LOCATE START OF FIRST REPORT.

C     DATA BASE INDICATOR FOR LAND REPORTS IS 11 AND FOR SEA REPORTS
C     21. THESE ARE MODIFIED LATER IF REDUCED SHIP, BUOY OR
C     AUTOMATIC REPORT ENCOUNTERED.

C     DISTINGUISH BETWEEN LAND ( TT = AA ) AND SEA ( TT = BB ) STATIONS.
C     'A' = 65, 'B' = 66.

C     MIMIMJMJ OF A--- OR -A-- ACCEPTED AS LAND STATION BULLETIN.
C     THE LINE LENGTH IS CHECKED IN CASE OF MISSING MIMIMJ LINE IN
C     A BULLETIN OF SHIP REPORTS AND SHIP'S NAME INCLUDES -A OR A- .

      LEN = JMI - IMI
      IF (KCHAR(IMI).EQ.65.OR.(KCHAR(IMI+1).EQ.65.AND.LEN.LT.15)) THEN
C     Set data base code type indicator and YYGGIW flag.
         KDEC(4) = 11
         ISYYGG = 1

C     LOCATE YYGGIW GROUP - NEXT FIGURE.
         CALL NEXTFIG ( IPT,JMI )

      ELSE

C          IF MIMIMJ IS B--- OR -B-- BULLETIN IS OF SEA REPORTS

         IF ( KCHAR(IMI).EQ.66.OR.KCHAR(IMI+1).EQ.66 ) THEN

C     SET DATA BASE CODE FIGURE FOR REPORT TYPE
            KDEC(4) = 21

C                  LOCATE D---D GROUP . THIS SHOULD BE THE NEXT
C                  CHARACTER AND ON A NEW LINE, BUT THERE ARE SOME
C                  COMMON VARIATIONS.
C
C                  (1) A YYGGIW OR YYGG GROUP IS ADDED AFTER BBXX,
C                      ON THE SAME LINE.
C                  (2) THE REPORT CONTINUES ON THE SAME LINE AS BBXX.
C
C     SET K1 TO POINT TO THE NEXT 'SPACE' CHARACTER (32)

            K1 = IPT
            CALL NEXTVAL ( K1,32,IGS )

C     SET K2 TO POINT TO THE NEXT 'CR' OR 'LF' CHARACTER.
            K2 = IPT
            CALL NEXTEND ( K2,IGS )

C     IF A 'CR' CHARACTER IS ENCOUNTERED BEFORE 'SPACE'
C     D---D IS ON THE NEXT LINE.
            IF ( K2.LT.K1 ) THEN
               IPT = K2
            ELSE

C     IF THE REST OF BBXX LINE CONSISTS OF ONLY
C     5 FIGURES IT IS CONSIDERED TO BE YYGGIW,
C     AND D---D IS SOUGHT ON THE NEXT LINE.
C
               K = K2 - K1
               IF ( K.GT.6 ) THEN
                  IPT = K1
               ELSE
                  IPT = K2
               END IF
            END IF

C     IPT NOW POINTS TO THE CHARACTER BEFORE D---D GROUP.

            CALL NEXTPRT ( IPT,IGS )

         ELSE
C
C                  BULLETIN CANNOT BE IDENTIFIED FROM MIMIMJMJ , SO
C                  INSPECT  AA  OF ABBREVIATED HEADING.
C
C                  IF A1 IS V OR W AND A2 IS A,B,C,D,E,F,J OR X
C                  THEN BULLETIN IS OF SEA REPORTS.

            IF ( (KCHAR(IAH+2).EQ.86.OR.KCHAR(IAH+2).EQ.87).
     1           AND.(KCHAR(IAH+3).EQ.65.OR.KCHAR(IAH+3).
     2           EQ.66.OR.KCHAR(IAH+3).EQ.67.OR.KCHAR(IAH+3).
     3           EQ.68.OR.KCHAR(IAH+3).EQ.69.OR.KCHAR(IAH+3).
     4           EQ.70.OR.KCHAR(IAH+3).EQ.74.OR.KCHAR(IAH+3).
     5           EQ.88) )
     6           THEN            ! SEA STATION

C     SET DATA BASE CODE FIGURE
               KDEC(4) = 21

C     CHECK MIMIMJ LINE LENGTH TO DECIDE
C     WHETHER LINE IS MISSING OR MIMIMJMJ
C     IS CORRUPT.

               IF ( (JMI-IMI).LT.11 ) THEN

C     CORRUPT MIMIMJ
                  CALL NEXTEND ( IPT,IGS )
                  CALL NEXTPRT ( IPT,IGS )
               ELSE

C     MISSING MIMIMJMJ LINE
                  IPT = IMI
               END IF

            ELSE                ! LAND STATION BULLETIN
C
C     SET DATA BASE CODE FIGURE
               KDEC(4) = 11

C     CHECK MIMIMJMJ LINE LENGTH TO DECIDE
C     WHETHER GROUP IS CORRUPT OR LINE IS
C     MISSING.

               IF ( (JMI-IMI).LT.11 ) THEN

C     CORRUPT MIMIMJMJ
                  CALL NEXTEND (IPT,IGS)
                  IPT =IPT - 5

C     SET YYGGIW FLAG
                  ISYYGG = 1
               ELSE

C     MISSING LINE
                  IPT = IMI

C     SET WORDS FOR YY GG IW TO MISSING DATA VALUE.
                  KINT(1) = MINDIC
                  KINT(2) = MINDIC
                  KINT(3) = MINDIC

               END IF
            END IF
         END IF
      END IF


 101  CONTINUE     ! loop. We GOTO here after each report read in

C     IF LAND STATION REPORT EXTRACT 'YYGGIW' FROM 'MIMIMJMJ' LINE.

      IF ( KDEC(4).EQ.11.AND.ISYYGG.EQ.1 ) THEN
Cpsjan98  Last change of ITP was in NEXTFIG, where care has been
c         taken to remain on the same line. If ITP is changed
c         when calling NEXTPRT below, a line shift must have appeared. 
c         This means a 'AAXX^M^M' line, the 'YYGGIW' group is 
c         probably forgotten. The next group is most probably
c         the 'IIiii' group. But to be sure, the next group is read  
c         and compared with kdec(10,11), which is YY and GG extracted 
c         from abbreveated header.
         kpt = ipt
         CALL NEXTPRT ( IPT,IGS )
         if (ipt.ne.kpt) then
            call extgrp (ipt,2,2,1,0,0,1,iret)
            if (kint(1).eq.kdec(10).and.kint(2).eq.kdec(11)) then
               if (kdec(10).eq.mindic.or.kdec(11).eq. mindic) return
               call nextprt ( ipt,igs )
c              don't need to check for error in kint(1,2) values, as 
c              this was done in PROCHDR for kdec(10,11)
            else
c              regards 'YYGGIW' group as missing and resets ITP
               kint(1) = mindic
               kint(2) = mindic
               kint(3) = mindic
               ipt = kpt
            endif
            isyygg = 0
         else
Cpsjan98 
            CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET)
            CALL NEXTPRT ( IPT,IGS )
            ISYYGG = 0

C             CHECK VALUES OF YY,GG AND IW. IF ANY ERROR IS
C             FOUND DECODED VALUE IS CHANGED TO MISSING DATA VALUE
C             AND GROUP FLAGGED AS BEING IN ERROR. ERRORS IN THIS
C             GROUP ARE NOT FATAL AS YYGG FROM HEADING AND IW FROM
C             WMO STATIONS MASTER FILE ARE SUBSTITUTED LATER, IF
C             NECESSARY.

            I = 0
            IF (KINT(1).LT.1.OR.KINT(1).GT.31) THEN
               I = 1
               KINT(1) = MINDIC
            END IF

            IF ( KINT(2).LT.0.OR.KINT(2).GT.23) THEN
               I = 1
               KINT(2) = MINDIC
            END IF

            IF ( KINT(3).EQ.2.OR.KINT(3).LT.0.OR.KINT(3).GT.4) THEN
               I = 1
               KINT(3) = MINDIC
            END IF

            IF ( I.NE.0 ) THEN
               KCHAR(IPT-1) = IOR(KCHAR(IPT-1),128)
               NOER(1,3)=NOER(1,3)+1
            END IF
cpsjan98
         end if
cps
      END IF   ! end part special for land reports

C***
C*    LOCATE END OF REPORT ( = ) AND INCREMENT COUNTER.
C***
C
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
cpsjan07
      IF (IEQ.GE.IGS) THEN
         WRITE(*,*) ' = sign missing at the end of report '
         LEN = IGS - IPT
         IF (LEN.LE.5) RETURN   ! Nothing more interesting. If len > 5, however,
                                ! it might be that '=' has been forgotten
      END IF


C***
C*    DECODE REPORT .
C***

C     CONVERT REPORT TO INTERMEDIATE FORMAT.

      CALL SMINT( IHEAD,IERR )

C     "NIL" REPORTS (KERR=1) WILL NOT BE DECODED FOR
C     DATA MONITORING PURPOSES

      IF ( KERR.NE.0 ) THEN
         WRITE(*,*) ' BULLSM - after call smint, kerr,ierr',KERR,IERR
      ELSE
C        Convert intermediate to decoded format and write to file
         CALL SMDEC ( IHEAD,IERR )

         IF ( KERR.NE.0 ) THEN
            WRITE(*,*) ' BULLSM - after call smdec, kerr,ierr',KERR,IERR
         END IF
      END IF


C***
C*    LOCATE START OF NEXT REPORT.
C***

      IPT = IEQ + 1
      CALL NEXTPRT(IPT,IGS)   ! If end of report, IPT will point to NNNN,
                              ! else to start of new report
C***
C*    CHECK FOR END OF BULLETIN.
C***

      IF ( IPT.GT.IGS ) RETURN  ! This shouldn't be possible for correct 
                                ! formatted bulletines, but might happen if
                                ! '=' is missing at end of report
      IIPT = IPT
      CALL NEXTEND(IIPT,IGS)    ! GETBULL ends bulletine with NNNN (no CR or LF)
      IF (IIPT.GT.IGS) RETURN

C***
C*    RESET LAND OR SEA STATION RDB CODE FIGURE.
C***

      IF ( KDEC(4).GT.14 ) THEN
         KDEC(4) = 21
      ELSE
         KDEC(4) = 11
      END IF

C***
C*    IF LAND STATION REPORT IT IS POSSIBLE TO GET NEW 'MIMIMJMJ'
C*    AND 'YYGGIW' GROUPS , SO CHECK .
C***

      IF ( KDEC(4).EQ.11.AND.KCHAR(IPT).EQ.65 ) THEN

C     SET YYGGIW FLAG AND LOCATE START OF NEW
C     YYGGIW GROUP.
C
         ISYYGG = 1

c     Don't want IPT here to advance to a new line. Uses (after goto 101)
c     this fact to help deciding whether next group is YYGGIW group or not.
         kpt = ipt
         call nextend(kpt,igs)
         call nextfig(ipt,kpt)
         kpt = ipt
         call nextfig(kpt,igs)
         if (kpt.ge.igs) return
Cpsjan98
      END IF
      GO TO 101
C
C
      END
      SUBROUTINE SMDEC ( IHEAD,IERR )
C
C**** *SMDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE SURFACE REPORT FROM INTERMEDIATE FORMAT
C         TO DECODED SURFACE REPORT
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SMDEC(IHEAD,IERR)*
C
C          INPUT    :  REPORT IN INTERMEDIATE FORMAT IN KINT
C
C          OUTPUT   :  DECODED REPORT ON KDEC
C
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
C         *CALL* *IC3333 (     )*      FOR LATTITUDE AND LONGITUDE
C         *CALL* *IT5TODC(     )*      CONVERSION FROM CITT5 TO DISPLAY CODE
cps               not used

C         *CALL* *IC0877 (     )*      WIND DIRECTION AND SPEED
CLiL      *CALL* *IC0857 (     )*      MAX MEAN WIND SPEED
CLiL      *CALL* *ICSPSP1(     )*      SPECIAL PHENOMENA
C         *CALL* *IC4377 (     )*      HORIZONTAL VOISIBILITY
CLiL      *CALL* *IC3850 (     )*      SEATEMPERATURE
CLiL      *CALL* *IC3855 (     )*      WETBULB SEATEMPERATURE
C         *CALL* *IC3845 (     )*      TEMPERATURE
C         *CALL* *IC0264 (     )*      INDICATOR OF ISOBARIC SURFACE  (A3)
C         *CALL* *IC3590 (     )*      PRECIPITATION
C         *CALL* *ICTRTR (     )*      PERIOD FOR PRECIPITATION MEASUREMENT
C         *CALL* *IC1677 (     )*      HEIGHT OF BASE OF CLOUD  (HH)
C         *CALL* *IC0700 (     )*      SHIP'S DIRECTION
C         *CALL* *IC4451 (     )*      SHIP'S SPEED
C         *CALL* *ICPWPW (     )*      PERIOD OF WAVES
C         *CALL* *ICHWHW (     )*      HEIGHT OF WAVES
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C         *CALL*         (     )*
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      implicit none
      integer ihead,ierr
      integer i,k,kk,ihours,lowest,iret,ios,j,ikdec,indsnow,ih,imm
      integer kdummy
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'commdbc.f'  ! cstream
      INCLUDE 'commsys.f'
C
C     ------------------------------------------------------------------
C           1. IN THIS SUBROUTINE THE ONLY PARTS OF THE HEADER
C              DECODED ARE IDENTIFIER, THE LENGTH OF REPORT,
C              DAY OF THE MONTH AND TIME (HOUR & MIN) FROM REPORT
C              AND IN CASE OF SHIP OR BUOY
C              LAT, LONG, ALT=0
C
 100  CONTINUE
CTEST
CTEST      PRINT*,' SMDEC - at start '
CTEST
C
C
C     CLEAR ERROR INDICATOR.
C
      KERR = 0
      IERR = 0 
C
C*    SET DECODED REPORT TO MISSING DATA VALUE
C
      DO I=25,200
         KDEC(I)=MINDIC
      END DO
C
C
C
C*    CHECK IF SHIP OR BUOY
C
      IF(KDEC(4) .LT. 21 ) GO TO 101
CLiL Test:
CLiL      PRINT*,'KINT(9)',KINT(9)
CLiL      PRINT*,'KINT(10)',KINT(10)
CLiL      PRINT*,'KINT(11)',KINT(11)
CLiL      PRINT*,'KINT(12)',KINT(12)
CLiL      PRINT*,'KINT(13)',KINT(13)
CLiL
C
C
C
C*   LAT & LONG
C
C
      CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,KDEC(5),KDEC(6))
C
C
C
C
C*    ALTIT   FOR SEA STATION=0
C
C
      KDEC(8)=0
C
C
C
C
101   CONTINUE
C
C*     DAY AND TIME
C
C      CHECK IF DAY AND HOUR ARE MISSING. IN THAT CASE USE 
C      DATE FROM ABBREVIATED HEADER.
C
       KDEC(1)=KINT(1)
       KDEC(2)=KINT(2)
C
       IF(KINT(1).EQ.MINDIC) THEN
                                KDEC(1)=KDEC(10)
                                IF(KDEC(10).EQ.MINDIC) RETURN
                             END IF
C
       IF(KINT(2).EQ.MINDIC) THEN
                                KDEC(2)=KDEC(11)
                                IF(KDEC(11).EQ.MINDIC) RETURN
                             END IF
C
      KDEC(9)=0
C
C
C*    REPORT LENGTH
C
C
      KDEC(24)=120
C
C
C
C*    SECTION 1
C
C
C*    DD & FF   WIND DIRECTION AND SPEED
C
C
      K=MINDIC
      IF(KDEC(4) .GE. 21) GO TO 120
      K=KDEC(17)*1000 + KDEC(16)
120   CONTINUE
C
      if(kint(255).eq.0.and.kint(256).ne.mindic) then
         CALL IC0877(KINT(19),KINT(256),KINT(3),K,MINDIC,KDEC(25),
     1            KDEC(26))
      else
         CALL IC0877(KINT(19),KINT(20),KINT(3),K,MINDIC,KDEC(25),
     1            KDEC(26))
      end if
C
C*    VV   HORIZONTAL VISIBILITY
C
      CALL IC4377(KINT(17),MINDIC,KDEC(27))
C
C*    WW, W1, W2   PRESENT AND PAST WEATHER
C
C     Note: even if 7 group in section 1 is missing, WW,W1 and W2 might
C     still get values different from MINDIC

      IF (KINT(38).EQ.MINDIC) THEN
         IF (KINT(15).EQ.2 .OR. KINT(15).EQ.5) THEN
C     Manned/automatic station, no significant weather observed.
C     According to B/C 1.10.1.3 Note (1), this shall be coded in BUFR as
C     508 for present weather, as 10 for past weather (1) and (2)
            KDEC(28) = 508
            KDEC(29) = 10
            KDEC(30) = 10
         ELSE IF (KDEC(29).EQ.MINDIC .AND. KDEC(30).EQ.MINDIC) THEN
C     B/C 1.10.1.4: "If no observation was made (data not available),
C     code figure 509 shall be used for present weather and both past
C     weather (1) and past weather (2) shall be reported as missing"
            KDEC(28) = 509
         END IF
      END IF
         
      DO I=39,41
         IF (KINT(I).NE.MINDIC) KDEC(I-11)=KINT(I)
      END DO
C     
CLil 28.5.97   
C Automatic stations with Ix=7, reports ww,w1,w2 in table 4680 & 4531.
C These tables are found in the same bufr-table as 4677 & 4561. To reach
C correct value for ww, add 100 to table-value; for w1,w2, add 10.
      IF (KINT(15).EQ.7) THEN
         IF (KDEC(28).NE.MINDIC.AND.KDEC(28).LT.500)
     +        KDEC(28)=KDEC(28)+100
         IF (KDEC(29).NE.MINDIC.AND.KDEC(29).NE.10) KDEC(29)=KDEC(29)+10
         IF (KDEC(30).NE.MINDIC.AND.KDEC(30).NE.10) KDEC(30)=KDEC(30)+10
      END IF
C     
C
210   CONTINUE
C
C
C*    TTT   AIR TEMPERATURE IN TENTHS OF DEGREE CELSIUS
C
C
C
      CALL IC3845(KINT(23),KINT(22),KDEC(16),KDEC(17),MINDIC,KDEC(31))
C
C
C
C*    TDTDTD   DEW POINT TEMPERATURE IN TENTHS OF DEGREE
C
C
C
      IF(KINT(26) .EQ. MINDIC) GO TO 320
      CALL IC3845(KINT(26),KINT(25),KDEC(16),KDEC(17),MINDIC,KDEC(32))
C
C
C
C***  UUU RELATIVE HUMIDITY
C
C
C     IF (KINT(25) .EQ. 9) THEN DEW POINT TEMP IS NOT AVAILABLE
C     AND RELATIVE HUMIDITY IS MEASURED INSTEAD
C
      IF(KINT(25) .EQ. 9) KDEC(33)=KINT(26)
C
C     CHECK RANGE
C
      IF ( KDEC(33).GT.100 ) KDEC(33) = MINDIC
C
320   CONTINUE
C
C
C
C*    P0P0P0   PRESSURE AT STATION LEVEL IN TENTHS OF HECTOPASCAL
C
C
      IF(KINT(28) .EQ. MINDIC) GO TO 340
      KDEC(34)=KINT(28)
      KK = KINT(28) / 1000
      IF ( KK.EQ.0 ) KDEC(34) = KDEC(34) + 10000
340   CONTINUE
C
C
C      CHECK IF THERE IS SEA LEVEL PRESSURE OR GEOPOTENTIAL IN THIS
C      THIS REPORT (IF A3 (=KINT(30)) .NE. 0 AND .NE. 9, THEN GEOPOT
C
C
      IF(KINT(30).EQ.MINDIC.OR.KINT(31).EQ.MINDIC ) GO TO 370
      IF(KINT(30) .NE. 0 .AND. KINT(30) .NE. 9) GO TO 350
C
C
C
C*    PRESSURE AT SEA LEVEL IN TENTHS OF HECTOPASCAL
C
C
      KDEC(35)=KINT(31) + 1000*KINT(30)
      IF(KINT(30) .EQ. 0) KDEC(35)=KINT(31) + 10000
      GO TO 370
C
C
C
350   CONTINUE
C
C
C***  A3   INDICATOR OF STANDARD ISOBARIC SURFACE (CODE 264)
C
C
C
      CALL IC0264(KINT(30),MINDIC,KDEC(36))
C
C
C*    HHH   GEOPOTENTIAL AT AN AGREED ISOBARIC SURF, GIVEN BY A3
C           The thousands digit is omitted, so is added below
C
      KDEC(37)=KINT(31)
C
C     ADD 1000 IF 850 HPA LEVEL
C
      IF (KINT(30).EQ.8) KDEC(37)=KDEC(37) + 1000
C
C     ADD 2000 OR 3000 IF 700 HPA LEVEL
C
      IF (KINT(30).EQ.7) THEN
                             IF (KINT(31).LT.500)KDEC(37)=KDEC(37)+3000
                             IF (KINT(31).GE.500)KDEC(37)=KDEC(37)+2000
                         END IF
C
C     ADD 5000 IF 500 HPA LEVEL
C
      IF (KINT(30).EQ.5) KDEC(37) = KDEC(37) + 5000
C
C
C
370   CONTINUE

C*    A   CHARACTERISTIC OF PRESSURE TENDENCY DURING 3 HOURS

      IF(KINT(33) .EQ. MINDIC) GO TO 380
      KDEC(38) = KINT(33)
380   CONTINUE

C*    PPP   AMOUNT OF PRESSURE TENDENCY IN TENTHS OF HECTOPASCAL

      IF (KINT(34).NE.MINDIC) THEN
         KDEC(39) = KINT(34)
C        VALUE IS NEGATIVE IF CHARACTERISTIC ( A ) IS 5 - 8.
         IF (KDEC(38).GE.5.AND.KDEC(38).LE.8) KDEC(39) = -KDEC(39)
      END IF

C     ppp: 10 bits with scale -1, ref. value = -500 in bufr means that
C     values > 522 cannot be stored correctly (523 = missing value)
      IF (KDEC(39).NE. MINDIC .AND. KDEC(39).GT.522) THEN
         WRITE(*,*) 'ppp too big - set to missing ',KDEC(39),' dPa'
         KDEC(39) = MINDIC
      ELSE IF (KDEC(39).LT.-500) THEN
         WRITE(*,*) 'ppp lower than ref value - set to missing ',
     +        KDEC(39),' dPa'
         KDEC(39) = MINDIC
      END IF

C*     333 58(9)p24p24p24  24-HOUR PRESSURE CHANGE

      IF (KINT(235).NE.MINDIC) THEN
         KDEC(140) = KINT(235)
      ELSE IF(KINT(238).NE.MINDIC) THEN
         KDEC(140) = -KINT(238)
      END IF

C***  RRR   AMOUNT OF PRECIPITATION TENTHS OF MILLIMETRES

C*    Check if coded decimal for precipitation (national group)
C*    Legal values is from 0 to 9. If no value is coded, set to 0.

      CALL IC3590(KINT(36),KINT(170),KINT(14),MINDIC,KDEC(40),KDEC(136))
          
C     DURATION NOT DECODED IF RAINFALL VALUE IS MISSING.
      IF (KDEC(40).EQ.MINDIC) GO TO 415

C
C***  TR   DURATION OF PERIOD OF REFERENCE FOR PRECIP.(HOURS)
C
      IHOURS=KDEC(2)
cps   The test below is superfluous: kdec(2)=mindic is impossible
      IF(IHOURS .EQ. MINDIC) IHOURS=KDEC(11)
      IF(IHOURS .LT. 0 .OR. IHOURS .GT. 24) IHOURS=MINDIC
      IF(IHOURS .EQ. MINDIC) GO TO 415
C
      CALL ICTRTR (KDEC(16),KDEC(17),KINT(37),IHOURS,MINDIC,KDEC(41))
C
C
415   CONTINUE
C
C
C***  N   TOTAL CLOUD COVER
C
C
cpsokt97 Prints warning if N=/ or 0 or 9 and in spite of that cloudgroup  
c        8 is present in synop with at least one value different from /
cps
      IF(KINT(18) .EQ. MINDIC)THEN
	  if(kint(42).ne.mindic) then
             if(kint(43).ne.mindic .or. kint(44).ne.mindic .or.
     *            kint(45).ne.mindic .or. kint(46).ne.mindic) then
                Print*,'INFO: N=/, Group 8 should not be coded!.'
             endif
          endif
      ELSEIF(KINT(18) .EQ. 0)THEN
	  if(kint(42).ne.mindic) then
             if(kint(43).ne.mindic .or. kint(44).ne.mindic .or.
     *            kint(45).ne.mindic .or. kint(46).ne.mindic) then
                Print*,'INFO: N=0, Group 8 should not be coded!.'
             endif
          endif
      ELSEIF(KINT(18) .EQ. 9)THEN
	  if(kint(42).ne.mindic) then
             if(kint(43).ne.mindic .or. kint(44).ne.mindic .or.
     *            kint(43).ne.mindic .or. kint(44).ne.mindic) then
                Print*,'INFO: N=9, Group 8 should not be coded!.'
             endif
          endif
      ENDIF

      IF(KINT(18) .NE. MINDIC) THEN
         KDEC(42)=KINT(18)
      ENDIF
C
caug97-lil:
caug97 
caug97  In the following changes have been made to secure that all
caug97  cloudobservations are taken as reported in the synop, and
caug97  not changed to secure internal consistency.
cpsoct97 Completed this task.
c
cpsoct97 Removed some parts to increase readability of text:
cpsoct97 In these parts (which earlier (aug97) has been commented
cpsoct97 out) Nh,CL,CM,CN was put to 0 (for N=0) or MINDIC (N=9)
cpsoct97 Also transferred the warnings to section N TOTAL CLOUD COVER
C
C
C***  H   HEIGHT OF THE BASE OF LOWEST CLOUD
C
C
      IF(KINT(16) .EQ. MINDIC) THEN
         IF(KDEC(4).NE.23) KDEC(43)=16382
cpsoct97 This IF-test is somewhat mysterious, as removing it
c        has no effect on the final results of the program.
c        KDEC(4)=23 means reduced ship code, this value is
c        set if last digit in 1snTTT or 4PPPP group is /. According to
c        note (2) in 12.1.3.7 in the WMO Manual of Codes: "if 
c        the ship does not report cloud data, h should be coded 
c        with a solidus (/)". This is probably the reason behind
c        this test, but it is really unneccesary. For the same
c        reason all tests involving 16382 could have been deleted
c        if the if-test above was deleted, with no consequences.
c     16382 means missing, 16381 (below) means 0.0 m
c     I.e: Later on h (HL) in Bufr-code is set to 0.0
c     if KDEC(43) is 16381, to 999999. if KDEC(43)=16382 or MINDIC.
cpsaug13 Changed to h = 999999. (missing value) also for KDEC(43)=16381
c        since N=0 ought to mean no clouds; h=9 is then an error
cps
C
      ELSEIF(KINT(16) .EQ. 9. AND. KINT(18) .EQ. 0) THEN
         KDEC(43)=16381
C
      ELSE
cpsoct97         
         lowest=0
c     lowest is used to associate a more precise value with h when 
c     kint(16)=h=9, i.e. height of lowest cloud is more than 2500m.
c     Possible values of lowest: 0,1,2 corresponding to kdec(43)=
c     height=2600,3500,8000 metres. See subroutine IC1600.
cps
         IF(KINT(16) .EQ. 9) THEN
C
C     loop for different cloud types
C
            DO 425 I=46,44,-1
               IF(KINT(I) .EQ. MINDIC) GO TO 425
               IF (KINT(I).EQ.0) GO TO 425
               LOWEST=I-44
 425        CONTINUE
         ENDIF
C
         CALL IC1600(KINT(16),LOWEST,MINDIC,KDEC(43))
      ENDIF
C
C
C***  NH   TOTAL LOW CLOUDS
C
C
       IF(KINT(42) .EQ. MINDIC) GO TO 451
       KDEC(44)=KINT(43)
C
C
C
C***  CL, CM, CH   TYPE LOW MEDIUM AND HIGH CLOUDS
C
C
      DO 450 I=45,47
   	   KDEC(I)=KINT(I-1)
  	   IF(KINT(I-1) .EQ. MINDIC) KDEC(I)=14
  	   IF(KINT(42) .EQ. MINDIC) KDEC(I)=14

450   CONTINUE
C
451   CONTINUE
C
C*** 9GGgg
cpsokt99 We choose to ignore this group if hour or minute has an impossible
cps      value, or GG differ from termin with more than 30 minutes. In the
cps      last case: also writes an error message to error file.
cps      Note: check for impossible value has also been performed in SMINT, 
cps      with line in error written to error file by SAVREP.
      if(kint(47) .eq. 9) then
         call timediff(kint(48),kint(49),kdec(2),kdec(9),iret)
         if(iret .gt. 30) then
            OPEN(UNIT=11,IOSTAT=IOS,ERR=400,
     *           FILE=cstream(msys)//'/'//'syno_error_file.dat',
#ifdef FORTRAN90
     *           POSITION='APPEND',
#else
     *           ACCESS='APPEND',
#endif
     *           STATUS='UNKNOWN'   )

            if(kdec(4).lt.21) then
               write(11,4444) kint(4),kint(48),kint(49),kdec(2)
            else
               write(11,4445) char(kint(9)),char(kint(10)),
     *              char(kint(11)),char(kint(12)),
     *              char(kint(13)),kint(48),kint(49),kdec(2)
            endif
            close(11)
         endif
         if(iret .lt.0 .or. iret .gt. 30) then
            kint(47)=mindic
            kint(48)=mindic
            kint(49)=mindic
         endif
      endif
 4444 format(/,'Station ',i5,': 9GGgg= 9',2i2.2,' termin=',i2.2)
 4445 format(/,'Station ',5a,': 9GGgg= 9',2i2.2,' termin=',i2.2)
C
C***
C*
C*           SECTION 2
C*
C*    CHECK IF THERE IS SECTION 2 IN THIS REPORT
C***
C
      IF(KINT(50) .EQ. MINDIC) GO TO 690
C
C
C***  DS   DIRECTION OF THE SHIP
C
C
      CALL IC0700(KINT(51),MINDIC,KDEC(49))
C
C
C
C***  VS   SHIP"S SPEED
C
C
      CALL IC4451(KINT(52),MINDIC,KDEC(50))
C
C
C
C***  TWTWTW   SEA-SURFACE TEMPERATURE
C
C
CLiL
CHandling sea-temp in subroutine.
      IF(KINT(53) .NE. MINDIC)
     x    CALL IC3850(KINT(55),KINT(54),MINDIC,KDEC(51),KDEC(119))
C
ctest      print*,'Sea-temp:',KDEC(51),KDEC(119)
CLiL
C
C
C
C***  PWAPWA   PERIOD OF WAVES IN SECONDS
C
C
      CALL ICPWPW(KINT(57),MINDIC,KDEC(52))
C
C
C
C***  HWAHWA   HEIGHT OF WAVES IN DECIMETERS 
C
C
      CALL ICHWHW(KINT(58),MINDIC,KDEC(53))
C
C       CHECK IF HIGHT WITH .1 M REPORTED.
C
      IF(KINT(250).EQ.70) THEN
         IF(KINT(251).NE.MINDIC) KDEC(53)=KINT(251)
      END IF
C
C
C
C***  PWPW   PERIOD OF WIND WAVES IN SECONDS
C
C
      CALL ICPWPW(KINT(60),MINDIC,KDEC(54))
C
C
C
C
C***  HWHW   HEIGHT OF WIND WAVES IN DECIMETERS 
C
C
      CALL ICHWHW(KINT(61),MINDIC,KDEC(55))
C
C
C
C***  DW1DW1 & DW2DW2   DIRECTION FROM WHICH SWELL WAWES ARE COMING
C
C
      DO 560 I=56,57
      IF(KINT(I+7) .EQ. MINDIC) GO TO 560
      KDEC(I)=KINT(I+7)*10
CLil      IF(KINT(I+7) .EQ. 99) KDEC(I)=620
cpsapr99      IF(KINT(I+7) .EQ. 99) KDEC(I)=0
cpsjun06      IF(KINT(I+7) .EQ. 99) KDEC(I)=510
cps Changed back to old (Lillian) code
      IF(KINT(I+7) .EQ. 99) KDEC(I)=0
cps The allowed values for dw1dw1 (and dw2dw2) are 0-36 and 99, with
cps for instance 36 meaning direction between 355 and 4 degrees, while
cps 99 means 'Variable, or all directions, or waves confused, direction 
cps indeterminate', and dw1dw1=0 means 'no waves', in which case there
cps should be no 4Pw1Pw1Hw1Hw1 group.
560   CONTINUE
cpsjun06 If dwdw=0 and pwpw>0 or hwhw>0 this should have been
cps      coded as 360 degrees (dwdw=0 means calm)
      if (kint(63).eq.0 .and. ((kint(66).gt.0 .and. kint(66).ne.mindic)
     +     .or.(kint(67).gt.0 .and. kint(67).ne.mindic))) then
         write(*,*) 
     +        'dw1dw1=0, with pw1pw1>0 or hw1hw1>0. Changed to 360'
         kdec(56) = 360
      end if
      if (kint(64).eq.0 .and. ((kint(69).gt.0 .and. kint(69).ne.mindic)
     +     .or.(kint(70).gt.0 .and. kint(70).ne.mindic))) then
         write(*,*) 
     +        'dw2dw2=0, with pw2pw2>0 or hw2hw2>0. Changed to 360'
         kdec(57) = 360
      end if
C
C
C
C***  PW1PW1, HW1HW1    PERIOD AND HEIGHT OF
C***  PW2PW2, HW2HW2    SWELL WAVES
C
C
      CALL ICPWPW(KINT(66),MINDIC,KDEC(58))
      CALL ICHWHW(KINT(67),MINDIC,KDEC(59))
C
      CALL ICPWPW(KINT(69),MINDIC,KDEC(60))
      CALL ICHWHW(KINT(70),MINDIC,KDEC(61))
C
C
C
C
C***  IS     TYPE OF ICE
cpsapr98 C***  ESES   ICE THICKNESS IN DECIMETERS
C***  ESES   ICE THICKNESS IN CENTIMETERS
C***  RS     RATE OF ICING
C
C
      DO 620 I=72,74
      IF(KINT(I) .EQ. MINDIC) GO TO 620
      KDEC(I-10)=KINT(I)
620   CONTINUE
C
cpsapr98      IF(KDEC(63) .NE. MINDIC) KDEC(63)= (KDEC(63) + 5) / 10
C
C
C***  CI    CONCENTRATION OR ARRANGEMENT OF SEA ICE
C***  SI    STAGE OF DEVELOPMENT
C***  BI    LAND ICE
C***  DI    BEARING OF ICE
C***  ZI    TREND
C
C
      DO 650 I=76,80
      IF(KINT(I) .EQ. MINDIC) GO TO 650
cpsnov99
      if(i.eq.79) then
         call ic0700(kint(79),mindic,kdec(68))
      else
         KDEC(I-11)=KINT(I)
      endif
650   CONTINUE
C
Clil     IF(KDEC(68) .EQ. 99) KDEC(68)=620
      IF(KDEC(68) .EQ. 99) KDEC(68)=0
C
CLiL
CHandling wet-bulb sea-temp in subroutine. (april 1995)
      IF(KINT(252).ne.MINDIC)
     x    CALL IC3855(KINT(254),KINT(253),MINDIC,KDEC(120),KDEC(121))
C
690   CONTINUE
C
C***
C*
C*         SECTION 3
C*
C*     THIS IS THE REGIONAL PART AND THERE ARE DIFFERENCIES
C*     IN DIFFERENT REGIONS. REGION NUMBER IS KDEC(17).
C***
ctest      print*,' smdec sec.3 - kdec(17),kint(81) ',kdec(17),kint(81)
C
C***
C*      TEST IF THERE ARE ANY REGIONAL DATA
C***
Crr kdec(17) is WMO region number. This is equal to mindic for ships
Crr i.e. section 3 will not be decoded for ships. What about weather
Crr ships at a fixed position and following regional rules ?????????????????
C
CLiL Decode section 3 for ships also 20.4.95, go straight to SPSP
CLiL      IF(KDEC(17) .EQ. MINDIC) GO TO 930
      IF(KDEC(17) .EQ. MINDIC) GO TO 810
C
C
      IF(KINT(81) .EQ. MINDIC) GO TO 930
C
C***   TGTG   GROUND MIN TEMPERATURE IN TENTHS OF DEGREE
C             ONLY REGION I
C
C
C
C     THE FIRST GROUP IS 0TGTGRCRT AT 0600Z OR 0//RCRT
C     AT 0000 OR 1200. ONLY TGTG IS DECODED. TIME IS KDEC(11).
C
      IF(KDEC(11) .NE. 6) GO TO 700
      IF(KINT(83) .EQ. MINDIC) GO TO 700
      IF(KDEC(17) .NE. 1) GO TO 700
C
      KDEC(70)=KINT(83)*10
      IF(KDEC(70) .GT. 500) KDEC(70)=500-KDEC(70)
C
700   CONTINUE
C
cc      print*,' smdec - ground min.temp,kdec(70) ',kdec(70)
C
C***   TXTXTX   MAX AIR TEMPERATURE IN TENTHS OF DEGREE
C               THE PERIOD FOR MAX IS DIFFERENT FOR
C               DIFFERENT REGIONS
C
C
C     FOR REGION IV AT 1200Z THE MAX TEMP IS DEFINED FOR THE PERIOD
C     OF CALENDAR DAY, AND IT WILL NOT BE DECODED.
C     FOR REGION III DAYTIME TEMPERATURE IS USED, AND IT WILL
C     NOT BE DECODED EITHER.
C
      IF(KDEC(17) .EQ. 4 .AND. KDEC(11) .EQ. 12) GO TO 720
      IF(KDEC(17) .EQ. 3) GO TO 720
C
C
      IF(KINT(87) .EQ. MINDIC) GO TO 720
      CALL IC3845(KINT(89),KINT(88),KDEC(16),KDEC(17),MINDIC,KDEC(71))
C
C
C***   TX-PERIOD   LENGTH OF THE PERIOD FOR MAX TEMPERATURE (HOURS)
C                  IT VARIES FROM REGION TO REGION
C            REGION I   12 HOURS  (AT 1800)
C            REGION II  12 HOURS  (AT 1800)
C            REGION III DAYTIME TEMPERATURE  (MINDIC)
C            REGION IV  12 HOURS  (AT 0000 & 1800)
C                       24 HOURS  (AT 0600)
C                       CALENDAR DAY (AT 1200)   (MINDIC)
C            REGION V   24 HOURS  (AT 1200)
C            REGION VI  12 HOURS  (AT 1800)
C
cps NB KDEC(72) and KDEC(74) are never used! Period hard coded to 12 hours
      IF(KDEC(17) .EQ. 1 .OR. KDEC(17) .EQ. 2 .OR. KDEC(17) .EQ. 6)

     1                    THEN
                              KDEC(72)=12
                              GO TO 720
                          END IF
C
C
      IF(KDEC(17) .EQ. 4)
     1       THEN
                 IF(IHOURS .EQ. 12) GO TO 720
                 IF(IHOURS .EQ. 0 .OR. IHOURS .EQ. 18)
     1                    THEN
                              KDEC(72)=12
                              GO TO 720
                          END IF
                 IF(IHOURS .EQ. 6)
     1                    THEN
                              KDEC(72)=24
                              GO TO 720
                          END IF
             END IF
C
      IF(KDEC(17) .EQ. 5) KDEC(72)=24
C
720   CONTINUE
CTEST      print*,' smdec-3:max air temp,kdec(71),kdec(72)',kdec(71),kdec(72)
C
C
C***   TNTNTN   MIN AIR TEMPERATURE IN TENTHS OF DEGREE,
C               THE PERIOD FOR MIN VARIES FROM REGION TO REGION
C
C      FOR REGION III MIN TEMP IS DEFINED AT NIGHT TIME,
C      AND IT WILL NOT BE DECODED
C
      IF(KDEC(17) .EQ. 3) GO TO 740
C
C
      IF(KINT(90) .EQ. MINDIC) GO TO 740
      CALL IC3845(KINT(92),KINT(91),KDEC(16),KDEC(17),MINDIC,KDEC(73))
C
C
C
C***   TN-PERIOD   LENGHT OF THE PERIOD FOR MIN TEMPERATURE
C                  REGION I    12 HOURS  (AT 0600)
C                         II   12 HOURS  (AT 0600)
C                         III  NIGHT TIME  (MINDIC)
C                         IV   18 HOURS  (AT 0000)
C                         V    24 HOURS  (AT 0000)
C                         VI   12 HOURS  (AT 0600)
C
      IF(KDEC(17) .EQ. 1 .OR. KDEC(17) .EQ. 2 .OR.
     1   KDEC(17) .EQ. 6) THEN
                             KDEC(74)=12
                             GO TO 740
                          END IF
C
      IF(KDEC(17) .EQ. 4) THEN
                             IF(IHOURS .EQ. 0)
     1                             THEN
                                      KDEC(74)=18
                                      GO TO 740
                                   END IF
                             IF(IHOURS .EQ. 6 .OR. IHOURS .EQ.
     1                         18) THEN
                                      KDEC(74)=24
                                      GO TO 740
                                   END IF
                             IF(IHOURS .EQ. 12)
     1                             THEN
                                      KDEC(74)=12
                                      GO TO 740
                                   END IF
                          END IF
C
      IF(KDEC(17) .EQ. 5) KDEC(74)=24
C
740   CONTINUE
CTEST      print*,' smdec-3:min air temp,kdec(73),kdec(74)',kdec(73),kdec(74)
C
C***  RRR AMOUNT OF PRECIPITATION 1/10THS OF MM
C
ctest      print*,' smdec-kint(114),kint(14)',kint(114),kint(14)
C
C     Skip section 3 if iR=3 (RRRtR omitted) and precipitation has been
C     reported in section 1 (KINT(36)) but not in section 3 (KINT(114))
      IF (KINT(14).EQ.3 .AND. KINT(36).NE.MINDIC  
     +     .AND. KINT(114).EQ.MINDIC) GO TO 750
      
      CALL IC3590 (KINT(114),0,KINT(14),MINDIC,KDEC(102),KDEC(170))
C
C     DURATION NOT DECODED IF RAINFALL VALUE IS MISSING.
C
      IF (KDEC(102).EQ.MINDIC) GO TO 750
C
C
C*** DURATION OF RRR (tR)
         IHOURS = KDEC(2)
cps      The test below is superfluous: kdec(2)=mindic is impossible
         IF (IHOURS.EQ.MINDIC) IHOURS=KDEC(11)
         IF (IHOURS.LT.0.OR.IHOURS.GT.24) IHOURS=MINDIC
         IF (IHOURS.EQ.MINDIC) GO TO 750

      CALL ICTRTR (KDEC(16),KDEC(17),KINT(115),IHOURS,MINDIC,KDEC(103))
      
 750  CONTINUE

C*** R24R24R24

      IF (KINT(117).NE.MINDIC) THEN
         IF (KINT(117).EQ.9999) THEN ! Trace; coded as -0.1 in BUFR
            KDEC(101) = -1
         ELSE
            KDEC(101) = KINT(117)
         END IF
      END IF
C
C***   NS     AMOUNT OF CLOUD  (CODE FIGURE)
C      C      TYPE OF CLOUD
C      HSHS   HEIGHT OF BASE OF CLOUD LAYER
C
      DO I=75,84,3
         J=(I - 75)/3 + I
         IF(KINT(J+46) .EQ. MINDIC) GO TO 810
         KDEC(I)=KINT(J+47)
         KDEC(I+1)=KINT(J+48)
         CALL IC1677(KINT(J+49),MINDIC,KDEC(I+2))
      END DO
810   CONTINUE
C
C***  SPSPspsp  SPECIAL PHENOMENA       KINT(137-148)                     
C     (CODE FIGURES)                    KDEC(137-139)
      DO I=87,93,2
         J=(I-87)/2 + I
         IF(KINT(J+50) .EQ. MINDIC) GO TO 910

         CALL ICSPSP1(KINT(J+51),MINDIC,IKDEC)
         IF (IKDEC.EQ.137.OR.IKDEC.EQ.138) then
c     here only for SPSP=10,11 (see ICSPSP1)
            IF (KINT(J+52).EQ.99 .AND. KINT(258).NE.MINDIC) THEN
C     > 99 units => actual value of wind is in extra group. We are
C     not able to handle more than one such extra group in section 3
               KINT(J+52) = KINT(258)    
            END IF
            CALL IC0857(KINT(J+52),KINT(3),MINDIC,KDEC(IKDEC))
         ELSE IF (IKDEC.EQ.139) THEN
            CALL IC4077(KINT(J+52),MINDIC,KDEC(IKDEC))
         END IF
C     Not used in this program, but included to comply with kint.syno
         KDEC(I)=KINT(J+51)
         KDEC(I+1)=KINT(J+52)
      END DO
 910  CONTINUE
C
C
C***   E   STATE OF GROUND, NO SNOW OR ICE
C
      IF(KINT(93) .NE. MINDIC) KDEC(97)=KINT(94)
C
ctestc      print*,' smdec - state of dry ground,kint(93),kdec(97) ',
ctestc     *         kint(93),kdec(97)
C
C***   E'   STATE OF GROUND WITH SNOW OR ICE
C
C
      IF (KINT(98) .NE. MINDIC) THEN
         KDEC(98)=KINT(99)
         KDEC(99)=KINT(100)
         IF (KDEC(99) .EQ. 997) THEN
            KDEC(99)=-1
         ELSE IF (KDEC(99) .EQ. 998) THEN
            KDEC(99)=-2
         ELSE IF (KDEC(99) .EQ. 999) THEN
            KDEC(99)=MINDIC
         ENDIF
      END IF
C
C
C
C     FROM CHINA SNOW INFORMATION IS IN SPECIAL PHENOMENA. 9 SPSP SPSP
C     IN FORM 93 SPSPSP, WHERE SPSPSP = SNOW DEPTH IN CM.
C
      IF( KDEC(16) .EQ. 250 )
     C   THEN
            IF(KINT(137) .EQ. 9)
     C         THEN
                  INDSNOW = KINT(138) / 10
                  IF(INDSNOW .EQ. 3)
     C               THEN
                        KDEC(99) = 100 * (KINT(138) - 30) + KINT(139)
                        KDEC(98) = 1
                     END IF
               END IF
         END IF
C
C
C
C     IF E' IS MISSING AND E IS AVAILABLE IT MEANS THAT SNOWDEPTH=0
C
      IF((KDEC(98) .EQ. MINDIC) .AND. (KDEC(97) .NE. MINDIC))
     1    KDEC(99)=0

C     
C     J1J2J3J4J5 GROUP
C
C      EEE EVAPORATION/EVAPOTRANSPIRATION
C
      IF(KINT(181).NE.MINDIC) THEN
C        KG/M**2
         IF(KINT(182).NE.MINDIC) KDEC(110)=KINT(182)/10.
      END IF
C
C      SSS DURATION OF SUNSHINE 
C
      IF(KINT(189).NE.MINDIC) THEN
         IF(KINT(191).NE.MINDIC) THEN
            IH=KINT(191)/10
            IMM=(KINT(191)-IH*10)*6
            KDEC(111)=IH*60+IMM
         END IF
      END IF
C
C       NET RADIATION OVER 24 HOUR PERIOD
C
C          (OTHER RADIATION DATA ARE NOT PASSED)!!!!
C        
      IF(KINT(192).NE.MINDIC.AND.KINT(193).NE.MINDIC) THEN
         KDEC(112)=KINT(193)
      END IF
      IF(KINT(194).NE.MINDIC.AND.KINT(195).NE.MINDIC) THEN
         KDEC(112)=-KINT(195)
      END IF
C
     
 930  CONTINUE
CLiL.
C***
C*
C*         SECTION 5
C*
C*     THIS IS THE NATIONAL PART AND THERE ARE DIFFERENCIES
C*     IN DIFFERENT NATIONS.WMO COUNTRY NUMBER IN KDEC(16),
C*     SECTION 5 STARTS IN KINT(155)= 555.
C***
C*      TEST IF THERE ARE ANY NATIONAL DATA
C***
C
C
      IF (KINT(155).EQ.MINDIC) THEN 
         GO TO 950
      ENDIF

C**** GROUP 0 S Tz FXFX
      IF (KINT(156).EQ.MINDIC) GO TO 931

C*** S - STATE OF THE SEA
      IF (KINT(157).NE.MINDIC) THEN
         KDEC(130)=KINT(157)
      ENDIF

C*** Tz Time at which FXFX occurs.
      IF (KINT(158).NE.MINDIC) THEN
         KDEC(131)=KINT(158)
      ENDIF

C*** FXFX - Max of 10min-mean wind 
      CALL IC0857(KINT(159),KINT(3),MINDIC,KDEC(132))

 931  CONTINUE
C**** GROUP 1 SN TxTxTx
      IF (KINT(160).EQ.MINDIC) GO TO 932

C***   TXTXTX   MAX AIR TEMPERATURE IN TENTHS OF DEGREE
C               THE PERIOD FOR MAX WILL ALWAYS BE 12 HOURS IN SECTION 5.
C               IT IS ONLY CODED AT 06Z (NIGHT'S MAXTEMP). 
C*    Teste termin
      IF (KDEC(11).NE.6) GO TO 932
      CALL IC3845(KINT(162),KINT(161),KDEC(16),KDEC(17),MINDIC,KDEC(71))

C***   TX-PERIOD   LENGTH OF THE PERIOD FOR MAX TEMPERATURE (HOURS)
C                  IN SECTION 5 WILL ALWAYS BE 12.
      KDEC(72)=12

 932  CONTINUE
C**** GROUP 2 SN TnTnTn
      IF (KINT(163).EQ.MINDIC) GO TO 933

C***  TNTNTN   MIN AIR TEMPERATURE IN TENTHS OF DEGREE,
C               THE PERIOD FOR MIN WILL ALWAYS BE 12 HOURS IN SECTION 5.
C               IT IS ONLY CODED AT 18Z (DAY MINTEMP). 
C*    Teste termin
      IF (KDEC(11).NE.18) GO TO 933
      CALL IC3845(KINT(165),KINT(164),KDEC(16),KDEC(17),MINDIC,KDEC(73))

C***   TN-PERIOD   LENGHT OF THE PERIOD FOR MIN TEMPERATURE
C                  IS ALWAYS 12 HOURS IN SECTION 5.  
      KDEC(74)=12

 933  CONTINUE
C**** GROUP 3 SN TgTgTg
      IF (KINT(166).EQ.MINDIC) GO TO 934

C***   TGTGTG   MIN AIR TEMPERATURE JUST ABOVE THE GROUND.
C               (GROUND TEMPERATURE)
C               IT IS ONLY CODED AT 06Z (NIGHT'S MINTEMP). 
C*    Teste termin
      IF (KDEC(11).NE.6) GO TO 934
      CALL IC3845(KINT(168),KINT(167),KDEC(16),KDEC(17),MINDIC,KDEC(70))

 934  CONTINUE
C**** GROUP 4 RT WDWDWD
      IF (KINT(169).EQ.MINDIC) GO TO 950

C***  RT       TENTH OF MEASURED PRECIPITATION.
C              TO BE ADDED TO RRR.
C              THIS IS DONE IN SECTION 1, WHEN RRR IS CODED.
C***  WD WD WD  ADDITIONAL INFORMATION OM WEATHERCONDITIONS ON STATION.  
      IF(KINT(171) .EQ. MINDIC) GO TO 950
      KDEC(133)=KINT(171)

      IF(KINT(172) .EQ. MINDIC) GO TO 950
      KDEC(134)=KINT(172)

      IF(KINT(173) .EQ. MINDIC) GO TO 950
      KDEC(135)=KINT(173)

950   CONTINUE

c$$$       CALL PRTKINT(KINT,1,300,MINDIC) 
c$$$       CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC,kint(4))

      IF (KDEC(4).EQ.11.OR.KDEC(4).EQ.14) THEN
         CALL SYNEXP_LAND( IERR )
      ELSE
         CALL SYNEXP_SEA( IERR )
      END IF
      IF (IERR.NE.0) THEN
         WRITE(*,*)' After SYNEXP_LAND, IERR ',IERR
         RETURN
      END IF
c$$$      CALL BUFRENC( 1,IERR )
c$$$      CALL SYNEXP2( IERR )
c$$$      CALL QCSYNOP( 2,KDEC(4),KDEC(23),IERR )
c$$$      CALL BUFRENC( 2,IERR )

      call SET_DESCRIPTORS(kdec(4))

      CALL BUFR_ENCODE(kdec(4))
      RETURN

 400  CONTINUE
      WRITE(*,*) ' OPEN ERROR on syno_error_file.dat,  ios=',ios

      RETURN
      END
      SUBROUTINE SMINT (IHEAD,IERR)
C
C**** *SMINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT SURFACE REPORTS FROM CCITT. NO.5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SMINT(IHEAD,IERR)*
C
C          INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN  CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C                      IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                                WRITTEN TO ERROR FILE.
C                            = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                      IERR IS NOT USED ON INPUT.
C
C
C         OUTPUT     : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                      DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION )
C
C                      IERR = 1 IF ANY FILE HANDLING ERROR ENCOUNTERED.
C
C     EXTERNALS.
C     ----------
C
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C     MODIFICATIONS.
C     --------------
C
C

      implicit none
      integer ihead,ierr
      integer keep,ifirst,len,nil,i,id,idd,iwind,iret,ios,ibuoy,k,jpt
      integer j,ic1,ic2,ic3,ic4,ic5,n

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comdir.f'
      INCLUDE 'commdbc.f'  ! cstream,ctstamp
      INCLUDE 'commsys.f'

C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR.KEEP POINTER
C              TO FIRST CHARACTER OF REPORT.

CTEST      PRINT*,' SMINT - at start, KDEC(4) ', KDEC(4)

C     POINTER TO FIRST CHARACTER.
      KEEP = IPT

C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
      IFIRST = 0

 10   CONTINUE

C     ERROR INDICATOR.
      KERR = 0

C***
C*    CHECK FOR 'NIL' REPORT.
C***
      LEN = IEQ - IPT

C     RETURN IF REPORT SHORTER THAN 5 CHARS, FLAG ERROR = 2
C     SO IT WONT BE DECODED AT ALL (EVEN AS 'NIL')

      IF (LEN .LT. 5) THEN
         KERR = 2
         RETURN
      END IF

      NIL = 0

      IF (KDEC(4).LE.14.AND.LEN.LE.17) NIL = 1
      IF (KDEC(4).GE.21.AND.LEN.LE.28) NIL = 1

      IF (NIL.EQ.1) THEN
         KERR = 1
         NOER(1,1) = NOER(1,1) + 1
      END IF

C***
C*    SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C*    START IS IN WORD 4 AS YY GG IW FOR LAND STATIONS MAY ALREADY
C*    HAVE BEEN INSERTED .
C***
      DO I=4,300
         KINT(I) = MINDIC
      END DO

C     WORDS 9-13 SET TO CCITT.5 'XXX  ' ( 'X' = 88, 'SPACE' = 32 )
C     STATION/SHIP IDENTIFIER RETAINED IN THESE WORDS IN CCITT 5 CHARS.

      DO I=9,11
         KINT(I) = 88
      END DO

      KINT(12) = 32
      KINT(13) = 32
cpsjan24 allow for more than 5 letters in ship call sign
      KINT(109) = 32
      KINT(110) = 32
      KINT(111) = 32
      KINT(112) = 32
cpsjan24

C***
C*    CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C***

      DO I=1,3
         KDEC(I) = MINDIC
      END DO
      DO I=5,9
         KDEC(I) = MINDIC
      END DO

C     CLEAR FLAG FIELDS, RETAINING 'COR' FLAG.

      KDEC(13) = 0
      KDEC(15) = IAND(KDEC(15),4)
      DO I=16,17
         KDEC(I) = MINDIC
      END DO
      KDEC(20)=0
      DO I=22,23
         KDEC(I) = MINDIC
      END DO

C***
C*    CONVERT SECTION 0, WHICH IS DIFFERENT FOR LAND AND SEA
C*    STATIONS.
C***

      IF (KDEC(4).EQ.11 .OR. KDEC(4).EQ.14) THEN

C             LAND STATION
C             ------------

         KPT = IPT   ! Retain pointer to station identifier
         ID = 5      ! Number of characters in station identifier

         CALL EXTINT (IPT,5,4)       ! IIiii
         CALL LOCSTAT (IWIND,IRET)

         IF (IRET.EQ.1) THEN  ! Station number in range but no master file
                              ! entry, so report is ignored.
            KERR = 2
            NOER(1,2) = NOER(1,2)+1
C     If NIL report and no master file entry do NOT write to unknown file.
            IF (NIL.EQ.1) RETURN

C     Write IIiii with no master file entry to unknown file and stdout
            OPEN(UNIT=55,IOSTAT=IOS,ERR=710,
     1           FILE=cstream(msys)//'/'//'unknown_syno_station.dat',
#ifdef FORTRAN90
     2           POSITION='APPEND',
#else
     2           ACCESS='APPEND',
#endif
     4           FORM='FORMATTED',
     5           STATUS='UNKNOWN'        )

            WRITE (55,9901) ctstamp,KINT(4)
            CLOSE(55)

            WRITE (*,9901) ctstamp,KINT(4)
 9901       FORMAT (1H ,a12,' SYNO NO MASTER FILE ENTRY - ',I5)

            RETURN
 
 710        CONTINUE
            WRITE(*,*) ' OPEN ERROR on unknown_syno_station.dat,ios=',
     *           IOS
            RETURN
         ELSE IF (IRET.EQ.2) THEN  ! Corrupt number - fatal error.
            KPT = IABS(KPT)
            KCHAR(KPT+5) = IOR(KCHAR(KPT+5),128)
            KERR = 2
            IF (IFIRST.EQ.0) NOER(1,2) = NOER(1,2) + 1

C           Return if report too short to contain ident
            IF ((KPT+5) .GE. IEQ) RETURN
            GO TO 4000
         ELSE IF (IRET.EQ.3) THEN   ! Not a synop
            RETURN
         ELSE IF (IRET.EQ.4) THEN
C           Station is found in master file, but not marked as synop.
C           We choose to continue decoding. Would it be an idea to write
C           the message below to unknown_syno_station.dat also?
            WRITE(*,9902) KINT(4)
 9902       FORMAT (1H ,'Station ',I5,' found on station list,',
     *           ' but not correct type. Will still be decoded.')
         END IF


C     For "NIL" reports only the header will be decoded
         IF (NIL .EQ. 1) THEN
            KDEC(20) = 1
            GO TO 790
         END IF

C     If wind indicator IW is missing use the master file
C     indicator to replace IW.  Replacement IW indicates
C     wind measured and units knots or metres per second.
         IF ( KINT(3).EQ.MINDIC ) THEN
            IF ( IWIND.EQ.0 ) KINT(3)=4 ! knots
            IF ( IWIND.EQ.1 ) KINT(3)=1 ! metres per second
         END IF
      ELSE
C              SEA STATION
C              -----------

C     Retain pointer to first character in station identifier.
         KPT=IPT

C     Check whether buoy report or ship,rig or platform.
C     buoy reports have D---D of 5 figures.
         IBUOY = 0  ! Originally set to 0 (=buoy). Changed to 1 if not buoy,
                    ! changed to 2 if station identifier is missing.
         K = IPT
         CALL NEXTSEP(K,IEQ)

         IF (K-KPT .LT. 3) THEN   ! station identifier too short
            KERR = 2
            RETURN
         END IF

         IF ( K.NE.KPT + 4 ) THEN ! station identifier not 5 characters
            IBUOY = 1
         ELSE
            DO I=IPT,IPT+4
               IF ( KCHAR(I).LT.48.OR.KCHAR(I).GT.57 ) THEN
                  IBUOY=1         ! non-digit in station identifier
               END IF
            END DO
         END IF

C     If a 5 figure group is found check following group.
C     if the following group starts with a 9 the ship call
C     sign group is presumed missing.
c     i.e.: the 5 figure group we found is probably YYGGIw, not call sign

         CALL NEXTPRT (K,IEQ)
         IF (KCHAR(K).EQ.57) IBUOY = 2   ! call sign assumed missing

         IF ( IBUOY.NE.0 ) THEN

            KINT(4) = 0         ! ship,rig or platform. identifier no. set to 0

            IF (IBUOY.EQ.1) THEN
C              Move pointer past call sign . The first figure after start of the
C              next group is located as some ships use the full name (which can
C              consist of 2 words) instead of call sign.
               CALL NEXTVAL (IPT,32,IEQ)
               CALL NEXTFIG (IPT,IEQ)

               ID = IPT-KPT-1   ! Number of characters in call sign
            ELSE
               ID = 0           ! D---D missing , so set length of call sign to 0
            END IF
         ELSE
C     BUOY- A1 BW NBNBNB
            CALL EXTINT ( IPT,5,4 )
C     ALTER RDB REPORT TYPE FIGURE 
cps   why? This classifies station as automatic sea station
cps   even if ix says it is manual. Why override ix?
            KDEC(4) = 24
            ID = 5
         END IF

C                  YY GG IW
         CALL NEXTPRT ( IPT,IEQ )
C
C     IF FIRST OF FIGURE OF GROUP IS NOT 0-3 , YYGGIW
C     GROUP IS MISSING.

         IF (KCHAR(IPT).GE.48.AND.KCHAR(IPT).LE.51)
     C        CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
         JPT = IPT

C                  99 LALALA
         CALL NEXTPRT ( IPT,IEQ )
         CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )

C     CHECK VALIDITY OF LALALA
         IF (KINT(6).LT.0.OR.KINT(6).GT.900) THEN
            IPT = IABS(IPT)
            KCHAR(IPT)=IOR(KCHAR(IPT),128)
            KERR = 4
            IF (IFIRST.EQ.0) NOER(1,4) = NOER(1,4) + 1
            GO TO 4000
         END IF

C                  QC LOLOLOLO
         CALL NEXTPRT ( IPT,IEQ )
         CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )

C     Check validity of LOLOLOLO
         IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C        KINT(7).NE.5.AND.KINT(7).NE.7)  IRET=1
         IF (KINT(8).LT.0.OR.KINT(8).GT.1800) IRET=1
         IF (IRET.NE.0) THEN
            IPT = IABS(IPT)
            KCHAR(IPT) = IOR(KCHAR(IPT),128)
            KERR = 4
            IF (IFIRST.EQ.0) NOER(1,4) = NOER(1,4) + 1
            GO TO 4000
         END IF
      END IF
790   CONTINUE

C     Retain station identifier in character form (if any exists)
cpsjan24 expanded to allow for more than 5 characters in call sign
      IF (ID.NE.0) THEN
         IDD = ID
         IF (IDD.GT.5) IDD = 5
         J = KPT
         K = 9 + IDD -1
         DO I=9,K
C           If letter encountered for land station convert
C           it to figure (not for ship's call sign)
            IF (KDEC(4) .LE. 14) CALL LETFIG(KCHAR(J))
C           Reject report if station identifier doesn't consist solely
C           of letters and digits (and possibly space(s))
            IF (.NOT. ( (KCHAR(J).GE.48.AND.KCHAR(J).LE.57)  ! 0-9
     *              .OR.(KCHAR(J).GE.65.AND.KCHAR(J).LE.90)  ! A-Z
     *              .OR.(KCHAR(J).GE.97.AND.KCHAR(J).LE.122) ! a-z
     *              .OR.(KCHAR(J).EQ.32.) ))                 ! space
     *           THEN
               KERR = 2
               RETURN
            END IF
            KINT(I) = KCHAR(J)
            J = J + 1
         END DO

         IF (ID.GT.5 .AND. KDEC(4).GT.14) THEN
            IDD = ID - IDD
            IF (IDD.GT.4) IDD = 4
            J = KPT + 5
            K = 9 + IDD -1
            DO I=9,K
C           Reject report if station identifier doesn't consist solely
C           of letters and digits (and possibly space(s))
               IF (.NOT. ( (KCHAR(J).GE.48.AND.KCHAR(J).LE.57) ! 0-9
     *              .OR.(KCHAR(J).GE.65.AND.KCHAR(J).LE.90)    ! A-Z
     *              .OR.(KCHAR(J).GE.97.AND.KCHAR(J).LE.122)   ! a-z
     *              .OR.(KCHAR(J).EQ.32.) ))                   ! space
     *              THEN
                  KERR = 2
                  RETURN
               END IF
               KINT(100 + I) = KCHAR(J)
               J = J + 1
            END DO
         END IF
      END IF

      IF (NIL .EQ. 1) RETURN

C     Check values of YY GG IW for ship reports. If an error
C     is found in YY or GG the error is fatal as ship reports
C     frequently have different times from bulletin header times.
C     If there is an error in IW the wind cannot be decoded.
      IF (KDEC(4).GT.14) THEN
         IRET = 0
         IF (KINT(1).LT.1 .OR. KINT(1).GT.31) THEN
            IRET = 2
            KINT(1) = MINDIC
         END IF
         IF (KINT(2).LT.0 .OR. KINT(2).GT.23) THEN
            IRET = 2
            KINT(2) = MINDIC
         END IF
         IF (KINT(3).EQ.2.OR.KINT(3).LT.0.OR.KINT(3).GT.4) THEN
            IF (IRET.EQ.0) IRET = 1
            KINT(3) = MINDIC
         END IF

         IF (IRET.NE.0) THEN
            JPT = IABS(JPT)
            KCHAR(JPT) = IOR(KCHAR(JPT),128)
            IF (IFIRST.EQ.0) NOER(1,3) = NOER(1,3) + 1
            IF (IRET.EQ.2) THEN
               KERR = 3
               GO TO 4000
            END IF
         END IF
      END IF
C
C
C***
C*    CONVERT SECTION 1 .
C***
C
C     IR IX H VV
C
      CALL NEXTPRT ( IPT,IEQ )
      CALL EXTGRP ( IPT,1,1,1,2,0,14,IRET )
C
C     CHECK RANGES OF VALUES.
C
      IF (KINT(14).LT.0.OR.KINT(14).GT.4.AND.
     C                KINT(14).NE.MINDIC) IRET = 1
C
C     IX  1 - 7 OR /
C
      IF (KINT(15).LT.1.OR.KINT(15).GT.7.AND.
     C                KINT(15).NE.MINDIC) IRET = 1
C
      IF (IRET.NE.0 ) THEN
         IPT = IABS(IPT)
         KCHAR(IPT) = IOR(KCHAR(IPT),128)
         KERR = 5
         IF (IFIRST.EQ.0) NOER(1,5) = NOER(1,5) + 1
      END IF
C
C
C     IF AUTO REPORT ALTER RDB CODE FIGURE .
C
      IF ( KINT(15).GE.4.AND.KINT(15).LE.7)
     C                  THEN
                            IF (KDEC(4).EQ.11) KDEC(4) =14
                            IF (KDEC(4).EQ.21) KDEC(4) =24
                        END IF
C
C
C     N DD FF
C
      CALL NEXTPRT ( IPT,IEQ )
      CALL EXTGRP ( IPT,1,2,2,0,0,18,IRET )
C
C     CHECK RANGE OF VALUES.
C
      IF (KINT(19).LT.0) IRET = 1
      IF (KINT(19).GT.36.AND.KINT(19).LT.99) THEN
         IRET = 1
         KINT(19) = MINDIC
      ELSE IF (KINT(19).EQ.0.AND.KINT(20).NE.0) THEN
C     dd=0 means calm (so speed must be 0), dd=99 means variable
C     wind direction. For ddd in 355 - 4 degrees, dd=36
C     However: several stations report ddff=0001. In this case wind direction
C     is probably indeterminate (not calm, not 360), so change to missing
         IF (KINT(20).EQ.1) THEN
            WRITE(*,*) 'dd=0, with ff=1. Changed dd to missing'
            KINT(19) = MINDIC
         ELSE
            WRITE(*,*) 'dd=0, with ff>1. Changed to 360'
            KINT(19) = 36
         END if
C
C     IF GROUP IS  ///// , N//FF OR N////  IT IS ACCEPTED.
C
      else IF (KINT(19).EQ.MINDIC) then
         IRET = 0
      end if
C
C     check if ff = 99
C
      if(kint(20).eq.99) then
c
C        LOCATE NEXT GROUP .
C
         CALL NEXTPRT ( IPT,IEQ )
         IF (IPT.GE.IEQ) GO TO 3000
         if(kchar(ipt).eq.48.and.kchar(ipt+1).eq.48) then
            CALL EXTGRP ( IPT,2,3,0,0,0,255,IRET )
         end if
      end if
C
      IF (IRET.NE.0) THEN
                         IPT = IABS(IPT)
                         KCHAR(IPT) = IOR(KCHAR(IPT),128)
                         KERR = 6
                         IF (IFIRST.EQ.0) NOER(1,6) = NOER(1,6)+1
                     END IF
C
C
C
C     LOCATE NEXT GROUP .
C
      CALL NEXTPRT ( IPT,IEQ )
      IF (IPT.GE.IEQ) GO TO 3000
C
cclil 4.7.97
ccCheck if 00/// reported, if so, skip it:
      IF ( KCHAR(IPT).EQ.48 .and. KCHAR(IPT+1).EQ.48 )THEN

C        LOCATE NEXT GROUP .
C
         CALL NEXTSEP ( IPT,IEQ )
         CALL NEXTPRT ( IPT,IEQ )
         IF (IPT.GE.IEQ) GO TO 3000
C
      ENDIF
C
cclil end.
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 1 ( '1' = 49 ) .
C
      IF ( KCHAR(IPT).EQ.49 )
     C     THEN
C
C              1 SN TTT
C
C              SHIP REPORTS IN REDUCED FORM USE TT/ , SO
C              REPLACE / BY '0' .
C
C              MODIFY RDB CODE FIGURE AS WELL.
C
cps If temperature is missing in message, we don't want to classify the
cps synop as 'ship report in reduced form'
cpsmars05               IF ( KCHAR(IPT+4).EQ.47 )
               IF ( KCHAR(IPT+4).EQ.47 .and. KCHAR(IPT+2).ne.47 )
     C              THEN
                        KCHAR(IPT+4) = 48
                        IF(KDEC(4).GT.14) KDEC(4) = 23
                        print*,'TT/: ship report in reduced form'
                    END IF
C
               CALL EXTGRP( IPT,1,1,3,0,0,21,IRET )
C
               IF (KINT(22).LT.0.OR.KINT(22).GT.1) IRET = 1
C
C              IF SN  = / , GROUP IS TREATED AS 1////
C
               IF (KINT(22).EQ.MINDIC) IRET = 0
C
               IF ( IRET.NE.0 )
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT)=IOR(KCHAR(IPT),128)
                        KERR = 7
                        IF (IFIRST.EQ.0)  NOER(1,7) =NOER(1,7) + 1
                     END IF
C
                CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 2 ( '2' = 50 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION 2.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.50.AND.KCHAR(IPT+1).NE.50 )
     C     THEN
C
C              2 SN TDTDTD  OR   2 9 UUU
C
C              IF TDTDTD IS IN THE FORM TDTD/ , REPLACE / BY 0 .
C
               IF (KCHAR(IPT+4).EQ.47 ) KCHAR(IPT+4) = 48
C
               CALL EXTGRP ( IPT,1,1,3,0,0,24,IRET )
C
               IF (KINT(25).LT.0.OR.KINT(25).GT.9)IRET=1
               IF (KINT(25).GT.1.AND.KINT(25).LT.9) IRET = 1
C
C              IF SN = / , GROUP IS TREATED AS 2////
C
               IF (KINT(25).EQ.MINDIC) IRET = 0
C
               IF (IRET.NE.0)
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT) = IOR(KCHAR(IPT),128)
                        IF (IFIRST.EQ.0)  NOER(1,8) = NOER(1,8)+1
                        KERR = 8
                     END IF
               CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 3 ( '3' = 51 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION 3.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.51.AND.KCHAR(IPT+1).NE.51 )
     C     THEN
C
C              3 POPOPOPO
C
C              IF POPOPOPO IS OF THE FORM POPOPO/ , REPLACE / BY 0.
C
               IF (KCHAR(IPT+4).EQ.47) KCHAR(IPT+4) = 48
C
               CALL EXTGRP( IPT,1,4,0,0,0,27,IRET )
C
               IF (IRET.NE.0)
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT)=IOR(KCHAR(IPT),128)
                        KERR = 9
                        IF (IFIRST.EQ.0) NOER(1,9) = NOER(1,9)+1
                    END IF
C
               CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 4 ( '4' = 52 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION 4.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR (IPT).EQ.52.AND.KCHAR(IPT+1).NE.52 )
     C     THEN
C
C              4 P PPP    OR  4 A3 HHH
C
C              SHIP REPORTS IN REDUCED FORM USE PPP/ ,
C              SO REPLACE '/' BY '0' .
C
C              MODIFY RDB CODE FIGURE AS WELL.
C
cps If pressure is missing in message, we don't want to classify the
cps synop as 'ship report in reduced form'
cpsmar07               IF ( KCHAR(IPT+4).EQ.47 )
               IF ( KCHAR(IPT+4).EQ.47 .and. KCHAR(IPT+2).ne.47 )
     C              THEN
                        KCHAR(IPT+4) = 48
                        IF(KDEC(4).GT.14) KDEC(4) = 23
                        print*,'PPP/: ship report in reduced form'
                    END IF
C
               CALL EXTGRP( IPT,1,1,3,0,0,29,IRET )
C
               IF ( IRET.NE.0 )
     C              THEN
                        IPT = IABS(IPT)
                        KCHAR(IPT) = IOR(KCHAR(IPT),128)
                        IF (IFIRST.EQ.0) NOER(1,10)=NOER(1,10)+1
                        KERR = 10
                    END IF
C
               CALL NEXTPRT ( IPT,IEQ )
           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 5 ( '5' = 53 )
C     FIRST CHECK THAT IT IS NOT START OF SECTION  5.
C
      IF (IPT.GE.IEQ) GO TO 3000
C
CLiL check for CR too.
CLiL      IF ( KCHAR(IPT).EQ.53.AND.KCHAR(IPT+3).NE.32)
      IF( KCHAR(IPT).EQ.53.AND.
     C  ((KCHAR(IPT+3).NE.32).AND.(KCHAR(IPT+3).NE.13))) 
     C                    THEN
C                             5 A PPP
C
                              CALL EXTGRP ( IPT,1,1,3,0,0,32,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 6 ( '6' = 54 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.54 )
     C                    THEN
C
C                             6 RRR TR
C
                              CALL EXTGRP ( IPT,1,3,1,0,0,35,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 7 ( '7' = 55 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.55 )
     C                    THEN
C
C                             7 WW W1 W2
C
                              CALL EXTGRP ( IPT,1,2,1,1,0,38,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 8 . ( '8' = 56 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.56 )
     C                    THEN
C
C                             8 NH CL CM CH
C
                              CALL EXTGRP ( IPT,1,1,1,1,1,42,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 9 . ( '9' = 57 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.57 )
     C                    THEN
C     First check that it is not start of a section 9
      IF (KCHAR(IPT+1).EQ.57.AND.KCHAR(IPT+2).EQ.57.AND.
     *     (KCHAR(IPT+3).EQ.32.OR.
     *     KCHAR(IPT+3).EQ.13)) GOTO 3000
C
C                             9 GGgg
C
                              CALL EXTGRP ( IPT,1,2,2,0,0,47,IRET )
cpsokt99 Check GG and gg for impossible value or mindic (<0). 
cps      If mindic, then '9' is probably typo for another group.
                              IF ( IRET.NE.0 )
     C                             THEN
                                 IPT = IABS(IPT)
                              ENDIF
         if(kint(48).lt.0 .or. kint(48).ge.24) then
            KCHAR(IPT-4)=IOR(KCHAR(IPT-4),128)
            KERR=11
            IF (IFIRST.EQ.0) NOER(1,11)=NOER(1,11)+1
         endif
         if(kint(49).lt.0 .or. kint(49).ge.60) then
            KCHAR(IPT-2)=IOR(KCHAR(IPT-2),128)
            KERR=11
            IF (IFIRST.EQ.0) NOER(1,11)=NOER(1,11)+1
         endif
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C
C
C***
C*    CONVERT SECTION 2 .
C***
C
C     CONVERT 222 GROUP .( '2' = 50 )

  900 CONTINUE

      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).NE.50 .OR. KCHAR(IPT+1).NE.50 .OR.
     C    KCHAR(IPT+2).NE.50) THEN
C
C                            NOT SECTION 2
C
                             GO TO 1000
                         ELSE
C
C                            222 DS VS
C
                             CALL EXTGRP ( IPT,3,1,1,0,0,50,IRET )
                             CALL NEXTPRT ( IPT,IEQ )
C
C                            ALTER RDB CODE FIGURE IF ABBREVIATED
C                              REPORT ( DSVS = // )
C
                             IF ( KDEC(4).EQ.21.AND.KINT(51).EQ.MINDIC.
     C                           AND.KINT(52).EQ.MINDIC ) KDEC(4) =22
                         END IF
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 0 ( '0' = 48 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.48 )
     C                    THEN
C
C                              0 SN TWTWTW
C
                               CALL EXTGRP (IPT,1,1,3,0,0,53,IRET )
                               CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C    CONVERT IF GROUP IDENTIFYING FIGURE IS 1 ( '1' = 49 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.49 )
     C                    THEN
C
C                              1 PWAPWA HWAHWA
C
                               CALL EXTGRP( IPT,1,2,2,0,0,56,IRET )
                               CALL NEXTPRT ( IPT,IEQ )
                           END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 2 ( '2' = 50 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.50 )
     C                    THEN
C
C                             2 PWPW HWHW
C
                              CALL EXTGRP( IPT,1,2,2,0,0,59,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 3 ( '3' = 51 )
C     FIRST CHECK IF START OF SECTION 3 .
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF( KCHAR(IPT).EQ.51 .AND. .NOT.
     *     ( KCHAR(IPT+1).EQ.51.AND.KCHAR(IPT+2).EQ.51. AND.
     *     (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+3).EQ.13) ) )
     *     THEN
C
C     3 DW1DW1 DW2DW2
C
         CALL EXTGRP ( IPT,1,2,2,0,0,62,IRET)
         IF (IRET.NE.0) THEN
            IPT = IABS(IPT)
            KCHAR(IPT) = IOR(KCHAR(IPT),128)
            KERR = 12
            IF (IFIRST.EQ.0) NOER(1,12) = NOER(1,12)+1
         END IF
C     Convert to 'missing' if impossible value
         IF(KINT(63).GT.36.AND.KINT(63).LT.99)THEN
            KINT(63)=MINDIC
            IRET=1
         END IF
         IF(KINT(64).GT.36.AND.KINT(64).LT.99)THEN
            KINT(64)=MINDIC
            IRET=1
         END IF
      END IF
C
                               CALL NEXTPRT ( IPT,IEQ )
C
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 4 ( '4' = 52 )
C     FIRST CHECK IF START OF SECTION 4 .
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF( KCHAR(IPT).EQ.52 .AND. .NOT.
     *     ( KCHAR(IPT+1).EQ.52.AND.KCHAR(IPT+2).EQ.52. AND.
     *     (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+3).EQ.13) ) )
     *                    THEN
C
C                             4 PW1PW1 HW1HW1
C
                              CALL EXTGRP ( IPT,1,2,2,0,0,65,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 5 ( '5' = 53 )
C     FIRST CHECK IF START OF SECTION 5 .
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF( KCHAR(IPT).EQ.53 .AND. .NOT.
     *     ( KCHAR(IPT+1).EQ.53.AND.KCHAR(IPT+2).EQ.53. AND.
     *     (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+3).EQ.13) ) )
     *                    THEN
C
C                             5 PW2PW2 HW2HW2
C
                              CALL EXTGRP ( IPT,1,2,2,0,0,68,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 6 ( '6' = 54 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.54 )
     C                    THEN
C
C                             6 IS ESES RS
C
                              CALL EXTGRP ( IPT,1,1,2,1,0,71,IRET )
                              CALL NEXTPRT ( IPT,IEQ )
                          END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 70 ( '7' = 55 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.55.AND.KCHAR(IPT+1).EQ.48 ) THEN
C
C          70HwaHwaHwa
C
C
           CALL EXTGRP(IPT,2,3,0,0,0,250,IRET)
           CALL NEXTPRT ( IPT,IEQ )
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 8 ( '8' = 56 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.56) THEN
C
C          8swTbTbTb
C 
C
           CALL EXTGRP(IPT,1,1,3,0,0,252,IRET)
           CALL NEXTPRT ( IPT,IEQ )
      END IF
C
C     CHECK FOR 'ICE' INDICATOR.
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.73 )
     C                    THEN
C
C                             ICE , SO SET FLAG.
C
                              KINT(75) = 1
                              IPT =IPT + 3
                              CALL NEXTPRT( IPT,IEQ )
C
C                             CI SI BI DI ZI
C
                              CALL EXTGRP ( IPT,1,1,1,1,1,76,IRET )
                              CALL NEXTPRT (IPT,IEQ)
                          END IF
C
C***
C*    CONVERT SECTION 3.
C***
C
 1000 CONTINUE                     
      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).NE.51 .OR. KCHAR(IPT+1).NE.51 .OR.
     *    KCHAR(IPT+2).NE.51) THEN
         GO TO 2000             ! NOT SECTION 3
      ELSE
C
C     333 GROUP
         CALL EXTGRP ( IPT,3,0,0,0,0,81,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 0 ( '0' = 48 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.48 ) THEN
         IF ( KDEC(17).EQ.1 ) THEN
C     REGION 1
C     O  TGTG  RC  RT
            CALL EXTGRP(IPT,1,2,1,1,0,82,IRET)
            CALL NEXTPRT(IPT,IEQ)
         ELSE
C     REGION 4
C     0  CS  DL  DM  DH
            CALL EXTGRP(IPT,1,1,1,1,1,82,IRET)
            CALL NEXTPRT(IPT,IEQ)
         END IF
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 1 ( '1' = 49 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.49 ) THEN
C
C     1 SN TXTXTX
         CALL EXTGRP ( IPT,1,1,3,0,0,87,IRET )
         CALL NEXTPRT( IPT,IEQ )
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 2 ( '2' = 50 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.50 ) THEN
C
C     2 SN TNTNTN
         CALL EXTGRP ( IPT,1,1,3,0,0,90,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 3 ( '3' = 51 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.51 ) THEN
C
C     3 E SN TGTG
cps According to WMO manual vol. A this group is 3Ejjj, where
cps jjj='Supplementary information to be developed regionally
cps (see Volume II)'
         CALL EXTGRP ( IPT,1,1,1,2,0,93,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 4 ( '4' = 52 )
C     First check if start of section 4
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF( KCHAR(IPT).EQ.52.AND.
     C     ((KCHAR(IPT+3).NE.32).AND.(KCHAR(IPT+3).NE.13))) 
     C     THEN
C
C     4 E' SSS
         CALL EXTGRP (IPT,1,1,3,0,0,98,IRET)
         CALL NEXTPRT (IPT,IEQ)
      END IF
C
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 5 ( '5' = 53 )
C     First check if start of section 5
C
 1050 CONTINUE                  ! Loop. There may be several groups starting with 5
      IF (IPT.GE.IEQ) GO TO 3000
      IF( KCHAR(IPT).EQ.53.AND.
     *     ((KCHAR(IPT+3).NE.32).AND.(KCHAR(IPT+3).NE.13))) THEN
C
C        5  ?
C
         CALL EXTGRP (IPT,1,1,1,1,1,101,IRET)
C     If error in extracting 5'er group we stop decoding, because it
C     is too dangerous continuing (don't know if next group is a
C     j5j6j7j8j9 or not)
         IF (IRET.NE.0) THEN
            DO I=1,5
               KINT(100+I) = MINDIC
            ENDDO
            GOTO 4000
         ENDIF
         IC1=KINT(101)
         IC2=KINT(102)
         IC3=KINT(103)
         IC4=KINT(104)
         IC5=KINT(105)
         IPT = IPT - 5
C
C        IF ? IS 0,1,2,3: 5EEEIE (Evapotranspiration)
C
         IF(IC2.GE.0.AND.IC2.LE.3) THEN
            CALL EXTGRP(IPT,1,3,1,0,0,181,IRET)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050
C
C     54 g0 Sn dT (Temperature change data in period cov. by W1W2)
         ELSE IF (IC2.EQ.4) THEN
            CALL EXTGRP(IPT,1,1,1,1,1,184,IRET)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050
C
C     55 SSS  Sunshine past 24 hours (SSS might be ///)
         ELSE IF (IC2.EQ.5.AND.(IC3.EQ.MINDIC.OR.IC3.LT.3)) THEN
            CALL EXTGRP(IPT,1,1,3,0,0,189,IRET)
            CALL NEXTPRT (IPT,IEQ)
C
C     Radiation data (several groups possible) may follow
C
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.48) THEN ! 0
C
C     Positive net radiation during the preceding 24 H
               CALL EXTGRP(IPT,1,4,0,0,0,192,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.49) THEN ! 1
C
C     Negative net radiation during the preceding 24 H
               CALL EXTGRP(IPT,1,4,0,0,0,194,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.50) THEN ! 2
C
C     Global solar radiation during the preceding 24 H
               CALL EXTGRP(IPT,1,4,0,0,0,196,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.51) THEN ! 3
C
C     Diffused solar radiation during preceding 24 H
               CALL EXTGRP(IPT,1,4,0,0,0,198,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.52) THEN ! 4
C              First check if this is start of section 4 (444 )
               IF(KCHAR(IPT+1).EQ.52.AND.KCHAR(IPT+2).EQ.52.AND.
     *              (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+3).EQ.13))GOTO 2000
C
C     Downward long wave radiation during preceding 24 H
               CALL EXTGRP(IPT,1,4,0,0,0,200,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.53) THEN ! 5
C              First check if this is start of section 5 (555 )
               IF(KCHAR(IPT+1).EQ.53.AND.KCHAR(IPT+2).EQ.53.AND.
     *              (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+3).EQ.13))GOTO 2500
C              Then check for 553xx which we assume is 553SS, not j5F24F24F24F24
               IF(KCHAR(IPT+1).EQ.53.AND.KCHAR(IPT+2).EQ.51) GOTO 1050 
C
C     Upward long wave radiation during preceding 24 H
               CALL EXTGRP(IPT,1,4,0,0,0,202,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.54) THEN ! 6
C
C     Short wave radiation during preceding 24 H
               CALL EXTGRP(IPT,1,4,0,0,0,204,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF              ! end groups following 55SSS, SSS != 3xx
            GO TO 1050
            
C     55 3SS  Sunshine past hour
C
         ELSE IF (IC2.EQ.5.AND.IC3.EQ.3) THEN
C     If 55SSS is followed by 553xx, the last group might be
C     j5F24F24F24F24 or 553SS. We assume the latter.
C     Note that j5j6j7j8j9 group(s) might follow both 553 and 55[not 3],
C     but they are interpreted differently (hour values or 24 hour values)
            CALL EXTGRP(IPT,1,2,2,0,0,206,IRET)
            CALL NEXTPRT (IPT,IEQ)
C
C     Radiation data (several groups possible) may follow
C
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.48) THEN ! 0
C
C     Positive net radiation during the previous hour
               CALL EXTGRP(IPT,1,4,0,0,0,209,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.49) THEN ! 1
C
C     Negative net radiation during the previous hour
               CALL EXTGRP(IPT,1,4,0,0,0,211,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.50) THEN ! 2
C
C     Global solar radiation during the previous hour
               CALL EXTGRP(IPT,1,4,0,0,0,213,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.51) THEN ! 3
C
C     Diffused solar radiation during the previous hour 
               CALL EXTGRP(IPT,1,4,0,0,0,215,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.52) THEN ! 4
C     Check if this is start of section 4 (444 )
               IF(KCHAR(IPT+1).EQ.52.AND.KCHAR(IPT+2).EQ.52.AND.
     *              (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+3).EQ.13))GOTO 2000
C
C     Downward long wave radiation during the previous hour
               CALL EXTGRP(IPT,1,4,0,0,0,217,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.53) THEN ! 5
C     Check if this is start of section 5 (555 )
               IF(KCHAR(IPT+1).EQ.53.AND.KCHAR(IPT+2).EQ.53.AND.
     *              (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+3).EQ.13))GOTO 2500
C     Then check for 55Xxx with X < 3 which we assume is 55SSS (assume that
C     j5F24F24F24F24 starting with 55 is an impossible high value)
               IF(KCHAR(IPT+1).EQ.53.AND.KCHAR(IPT+2).LT.51) GOTO 1050
C
C     Upward long wave radiation during the previous hour
               CALL EXTGRP(IPT,1,4,0,0,0,219,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF
            IF (IPT.GE.IEQ) GO TO 3000
            IF(KCHAR(IPT).EQ.54) THEN ! 6
C     iR=0 or 2 means that 6RRRtR is included in section 3. In that case
C     we interpret 6.... as precipitation group (and just cross our
C     fingers and hope nobody will be so crude as to include both 6RRRtR
C     and j5j6j7j8j9 with j5 = 6)
               IF(KINT(14).EQ.0.OR.KINT(14).EQ.2) GOTO 1160 
C
C     Short wave radiation during the previous hour
               CALL EXTGRP(IPT,1,4,0,0,0,221,IRET)
               CALL NEXTPRT (IPT,IEQ)
            END IF              ! End groups following 553SS
            GO TO 1050

C              55407 4FFFF
         ELSE IF (IC2.EQ.5.AND.IC3.EQ.4.AND.IC4.EQ.0.AND.IC5.EQ.7) THEN
C           If group is 55407, next group is net short wave radiation during
C           the previous hour. Skip this group.
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ) ! ought to check that group starts with 4
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050
C              55408 4FFFF
         ELSE IF (IC2.EQ.5.AND.IC3.EQ.4.AND.IC4.EQ.0.AND.IC5.EQ.8) THEN
C           If group is 55408, next group is direct solar radiation radiation during
C           the previous hour. Skip this group.
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ) ! ought to check that group starts with 4
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050
C              55507 5F24F24F24F24
         ELSE IF (IC2.EQ.5.AND.IC3.EQ.5.AND.IC4.EQ.0.AND.IC5.EQ.7) THEN
C           If group is 55507, next group is net short wave radiation during
C           previous 24 hours. Skip this group.
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ) ! ought to check that group starts with 5
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050

C              55508 5F24F24F24F24
C     If group is 55508, next group is direct solar radiation radiation during
C     previous 24 hours. Skip this group.
         ELSE IF (IC2.EQ.5.AND.IC3.EQ.5.AND.IC4.EQ.0.AND.IC5.EQ.8) THEN
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ) ! ought to check that group starts with 5
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050
C
C              56 DL DM DH   DIRECTION ON CLOUD DRIFT
C
         ELSE IF (IC2.EQ.6) THEN
            CALL EXTGRP(IPT,1,1,1,1,1,223,IRET)
            CALL NEXTPRT (IPT,IEQ)
            IF (IPT.GE.IEQ) GO TO 3000
            GO TO 1050
C
C              57 C Da eC   DIRECTION AND ELEVATION OF CLOUD
C
         ELSE IF (IC2.EQ.7) THEN
            CALL EXTGRP(IPT,1,1,1,1,1,228,IRET)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050
C
C              58 OR 59  P24P24P24
C
         ELSE IF (IC2.EQ.8) THEN
            IPT = IPT
            CALL EXTGRP (IPT,1,1,3,0,0,233,IRET)
            CALL NEXTPRT (IPT,IEQ)
            GO TO 1050
         ELSE IF (IC2.EQ.9) THEN
            CALL EXTGRP (IPT,1,1,3,0,0,236,IRET)
            CALL NEXTPRT (IPT,IEQ)
            IF (IPT.GE.IEQ) GO TO 3000
            GO TO 1050
         END IF
C
      END IF

 1160 CONTINUE
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 6 ( '6' = 54 )
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.54 ) THEN            ! 6  RRR  TR
         CALL EXTGRP ( IPT,1,3,1,0,0,113,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF

C     CONVERT IF GROUP IDENTIFYING FIGURE IS 7 ( '7' = 55 )

      IF (IPT.GE.IEQ) GO TO 3000
      IF ( KCHAR(IPT).EQ.55 ) THEN            ! 7  R24R24R24R24
         CALL EXTGRP ( IPT,1,4,0,0,0,116,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF

C     CONVERT IF GROUP IDENTIFYING FIGURE IS 8 ( '8' = 56 )
C     CAN ACCEPT UP TO 4 SUCH GROUPS.

      N = 121
      DO I=1,4
         IF (IPT.GE.IEQ) GO TO 3000
         IF ( KCHAR(IPT).EQ.56 ) THEN         ! 8 NS C HSHS
            CALL EXTGRP (IPT,1,1,1,2,0,N,IRET)
            N = N + 4
            CALL NEXTPRT (IPT,IEQ)
         END IF
      END DO

 1100 CONTINUE
C     CONVERT IF GROUP IDENTIFYING FIGURE IS 9 ( '9' = 57 )
C     CAN BE UP TO 4 SUCH GROUPS. 
C     First check for '999 ' (Germany use this nonstandard section. Skip it)
      IF (KCHAR(IPT).EQ.57.AND.KCHAR(IPT+1).EQ.57.AND.
     *     KCHAR(IPT+2).EQ.57.AND.(KCHAR(IPT+3).EQ.32.OR.
     *     KCHAR(IPT+3).EQ.13)) GOTO 3000
cps   ?? can't find anything about 4 groups in WMO Manual of codes, and some
cps   lists regularly have 11 groups, so search for 15 groups (but decode only
cps   4 groups, as KINT does not have space for more than 4 groups).

      N = 137
      DO I=1,15
         IF (IPT.GE.IEQ) GO TO 3000
         IF ( KCHAR(IPT).EQ.57 ) THEN     ! 9  SPSP  SPSP
            IF (I.GT.4) THEN              ! Not space in KINT for more than 4 groups
               CALL NEXTSEP (IPT,IEQ)
               CALL NEXTPRT (IPT,IEQ)
               CYCLE
            ELSE
               CALL EXTGRP ( IPT,1,2,2,0,0,N,IRET )
C           Check if kint(n+1)=10,11,12,13 or 14, then kint(n+2) = ff
C           If ff is 99, read next group 00fff into kint(257) and kint(258)
               IF (KINT(N+1).GE.10.AND.KINT(N+1).LE.14) THEN
                  CALL NEXTPRT ( IPT,IEQ )
                  IF(KCHAR(IPT).EQ.48.AND.KCHAR(IPT+1).EQ.48) THEN
                     CALL EXTGRP ( IPT,2,3,0,0,0,257,IRET )
                  END IF
               END IF
               N = N + 3
               CALL NEXTPRT ( IPT,IEQ )
            END IF
         ELSE
            EXIT                          ! exit loop
         END IF
      END DO

C***
C*    CONVERT SECTION 4.
C***
C
 2000 CONTINUE

      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).NE.52 .OR. KCHAR(IPT+1).NE.52 .OR.
     *    KCHAR(IPT+2).NE.52) THEN   ! Not section 4
         GO TO 2500
      ELSE

C                  444 N' C' H'H' Ct

         CALL EXTINT (IPT,3,149)
         CALL NEXTPRT (IPT,IEQ)
C     The cloud groups in section 4 may repeat. But in KINT we have
C     space for only one group. To make it simple, we decode only last
C     of these groups (overwriting previous group(s)).
 2100    CONTINUE
         IF ((IPT.GE.IEQ)           .OR.
     *        (KCHAR(IPT).EQ.53.AND.KCHAR(IPT+1).EQ.53.AND.
     *        KCHAR(IPT+2).EQ.53.AND.(KCHAR(IPT+3).EQ.32.OR.
     *        KCHAR(IPT+3).EQ.13))  .OR.
     *        (KCHAR(IPT).EQ.57.AND.KCHAR(IPT+1).EQ.57.AND.
     *        KCHAR(IPT+2).EQ.57.AND.(KCHAR(IPT+3).EQ.32.OR.
     *        KCHAR(IPT+3).EQ.13))) THEN
            GOTO 2500
         ELSE
            CALL EXTGRP (IPT,1,1,2,1,0,150,IRET)
            CALL NEXTPRT ( IPT,IEQ )
         END IF
         GOTO 2100
      END IF

C***
C*    CONVERT SECTION 5 . NATIONAL GROUPS.
C***
 2500 CONTINUE
C     German stations use section 9 (999 ). Skip this.
      IF(KCHAR(IPT).EQ.57.AND.KCHAR(IPT+1).EQ.57.AND.
     *     KCHAR(IPT+2).EQ.57.AND.(KCHAR(IPT+3).EQ.32.OR.
     *     KCHAR(IPT+3).EQ.13)) GOTO 3000

      IF (IPT.GE.IEQ) GO TO 3000

CLiL Only to be converted if block=1, e.g. stationnumber 1xxx
CLiL or if shipcode starts with JW,JX,LA-LN. (Norwegian ships).
CLiL Reckons that only these send national group.
CLiL Test if ship or not ship (20.4.95)
C
      IF(KDEC(4).LT.21) THEN    ! N O T   SHIP
         IF((KINT(4)/1000).EQ.1) THEN
            GO TO 2505
         ENDIF
      ELSE                      ! SHIP
C     First letters of call-signal must be between LA and LN:
         IF(KINT(9).EQ.76)THEN
            IF((KINT(10).GE.65).OR.(KINT(10).LE.78))THEN
               GO TO 2505
            ENDIF
         ENDIF 
C     First letters of call-signal can also be JW or JX:
         IF(KINT(9).EQ.74)THEN
            IF((KINT(10).EQ.87).OR.(KINT(10).EQ.88))THEN
               GO TO 2505
            ENDIF
         ENDIF 
C     call-signal can also be OU2465:
C     Danish platform (A.P.Muller) lying between Ekofisk and Egersund 
C     which reports to VpV. 
C     Have to test kchar because there is only room for 5 chars in kint. 
         IF(kchar(kpt).EQ.79.AND.kchar(kpt+1).EQ.85.AND.
     *        kchar(kpt+2).EQ.50.AND.kchar(kpt+3).EQ.52.AND.
     *        kchar(kpt+4).EQ.54.AND.kchar(kpt+5).EQ.53)THEN
            GO TO 2505
         ENDIF 
      ENDIF

C      At this point we have a ship that is not norwegian( or 3xxxx ),
C      or we have a landstation that is not Norwegian.
C      We skip national sector:
      GO TO 3000

 2505 CONTINUE

      IF (KCHAR(IPT).NE.53 .OR. KCHAR(IPT+1).NE.53 .OR.
     *    KCHAR(IPT+2).NE.53) THEN    ! Not section 5
         GO TO 3000
      ELSE                            ! 555 GROUP
         CALL EXTGRP ( IPT,3,0,0,0,0,155,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF

C     Convert if group identifying figure is 0 ( '0' = 48 )
      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).EQ.48) THEN     ! 0 S TZ FXFX
         CALL EXTGRP (IPT,1,1,1,2,0,156,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF

C     Convert if group identifying figure is 1 ( '1' = 49 )
      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).EQ.49) THEN     ! 1 SN TxTxTx
         CALL EXTGRP ( IPT,1,1,3,0,0,160,IRET )
         CALL NEXTPRT( IPT,IEQ )
      END IF

C     Convert if group identifying figure is 2 ( '2' = 50 )
      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).EQ.50) THEN     ! 2 SN TnTnTn
         CALL EXTGRP ( IPT,1,1,3,0,0,163,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF

C     Convert if group identifying figure is 3 ( '3' = 51 )
      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).EQ.51) THEN     ! 3 SN TGTGTG
         CALL EXTGRP ( IPT,1,1,3,0,0,166,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF

C     Convert if group identifying figure is 4 ( '4' = 52 )
      IF (IPT.GE.IEQ) GO TO 3000
      IF (KCHAR(IPT).EQ.52) THEN     ! 4 RT WD WD WD
         CALL EXTGRP ( IPT,1,1,1,1,1,169,IRET )
         CALL NEXTPRT ( IPT,IEQ )
      END IF

C     Error if not section 5? and not end of report.
 3000 CONTINUE                     
C     Test for '555 ' and '999 '
      IF ((IPT.GE.IEQ)           .OR.
     *     (KCHAR(IPT).EQ.53.AND.KCHAR(IPT+1).EQ.53.AND.
     *     KCHAR(IPT+2).EQ.53.AND.(KCHAR(IPT+3).EQ.32.OR.
     *     KCHAR(IPT+3).EQ.13))  .OR.
     *     (KCHAR(IPT).EQ.57.AND.KCHAR(IPT+1).EQ.57.AND.
     *     KCHAR(IPT+2).EQ.57.AND.(KCHAR(IPT+3).EQ.32.OR.
     *     KCHAR(IPT+3).EQ.13))) THEN
         GOTO 4000
      ELSE
         KERR = 60
         KCHAR(IPT) = IOR(KCHAR(IPT),128)
         KCHAR(IEQ) = IOR(KCHAR(IEQ),128)
         IF (IFIRST.EQ.0) NOER(1,60) = NOER(1,60) + 1
      END IF
C
C
C***
C*    ERROR CHECKING AND HANDLING.
C***
C
C     RETURN IF NO ERROR IN REPORT.
C
 4000 CONTINUE
      IF ( KERR.EQ.0 ) RETURN
ctest      print*,'IPT,KERR,IFIRST ',IPT,KERR,IFIRST
C
C     IF FIRST DECODING ATTEMPT , TRY TO CORRECT ERROR AND DECODE
C     AGAIN.
C
      IF ( IFIRST.EQ.0 ) THEN
                        IPT = KEEP
                        CALL SAVREP( IHEAD,IERR )
c                       Parity-bits are cleared in FIXSM
                        CALL FIXSM
                        IFIRST = 1
                        GO TO 10
                    ELSE
C                       ONLY REPORTS WITH ERROR IN DATE/TIME OR
C                       LAT/LONG ARE NOT PROCESSED FURTHER.
C
                        IF (KERR.GT.4) KERR = 0
                        RETURN
                     END IF
C
      END
      SUBROUTINE FIXSM
C
C**** *FIXSM*
C
C
C     PURPOSE.
C     --------
C
C
C         *CALL* *FIXSM*
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
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C          PS Feb 2007. Do not remove any groups before 1snTTT
C
      implicit none
      integer i,icor,j,k,len,nskip
      logical ok
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
C     ------------------------------------------------------------------
C*          1.  CLEAR PARITY BITS.
C                -----------------

      DO I=IPT,IGS
         KCHAR(I) = IAND(KCHAR(I),127)
      END DO

      ICOR = 0

C     Check all groups in report. Only 5 figures (or '/') acceptable.
C     Or '222' or '333' or '444' or '555' or 'ICE' (then skips 17 char)
C     or 'ICI' (then skips 17 char)
C     Wrong groups becomes space (32).
      I=IPT        ! should normally point to station number or call sign

C     We do not dare to remove a group before group 1 in section 1 however,
C     so skip the expected number of such groups.
      IF (KDEC(4).EQ.11 .OR. KDEC(4).EQ.14) THEN         ! land report
         NSKIP = 3
      ELSE IF (KDEC(4).GE.21 .AND. KDEC(4).LE.24) THEN   ! ship report
         NSKIP = 6
      ENDIF
      DO J=1,NSKIP
         CALL NEXTSEP(I,IEQ)
         CALL NEXTPRT(I,IEQ)
      END DO
      
      DO 175 WHILE(I.LT.IEQ)

         ok=.false.
         if(kchar(i).eq.32.OR.kchar(i).eq.13.OR.kchar(i).eq.10)then
cps                     SP                CR                LF
c
C              Five digits or /:
               ok=.true.
               i=i+1
c
         elseif(kchar(i).ge.47.AND.kchar(i).le.57)then
c
            IF((kchar(i+1).ge.47.AND.kchar(i+1).le.57)
     c      .AND.(kchar(i+2).ge.47.AND.kchar(i+2).le.57)
     c      .AND.(kchar(i+3).ge.47.AND.kchar(i+3).le.57)
     c      .AND.(kchar(i+4).ge.47.AND.kchar(i+4).le.57))THEN
c
C              Five digits or /:
               ok=.true.
               i=i+5
c
            ELSEIF(kchar(i).eq.50.AND.kchar(i+1).eq.50.AND.
     c             kchar(i+2).eq.50)THEN
c
C              222 Maritime sector:
               ok=.true.
               i=i+3
c
            ELSEIF(kchar(i).eq.51.AND.kchar(i+1).eq.51.AND.
     c             kchar(i+2).eq.51)THEN
c
C              333 Regional sector:
               ok=.true.
               i=i+3
c
            ELSEIF(kchar(i).eq.52.AND.kchar(i+1).eq.52.AND.
     c             kchar(i+2).eq.52)THEN
c
C              444 Optional sector:
               ok=.true.
               i=i+3
c
            ELSEIF(kchar(i).eq.53.AND.kchar(i+1).eq.53.AND.
     c             kchar(i+2).eq.53)THEN
c
C              555 National sector:
               ok=.true.
               i=i+3
c
cpsoct97 Don't understand the reason for this test (tests for 'ICI ', why??)
            ELSEIF(kchar(i).eq.73.AND.kchar(i+1).eq.67.AND.
     c            kchar(i+2).eq.73.AND.kchar(i+3).eq.32)THEN
c
C              ICE:
               ok=.true.
               i=i+3
c
c
            ELSEIF(kchar(i).eq.73.AND.kchar(i+1).eq.67.AND.
     c             (kchar(i+2).eq.69.OR.kchar(i+3).eq.73))THEN
c
C              ICE EDGE or ICING??:
               ok=.true.
               i=i+20
               call nextsep(i,ieq)
c
            END IF
         end if

c         print*,'Report,i,ok=',i,ok
c         print*,'Report:',char(kchar(i)),char(kchar(i+1)),
c     c      char(kchar(i+2)),char(kchar(i+3)),char(kchar(i+4))
c
         if(.not.ok)then
           j=i
c           print*,'call nextsep(i)'
           call nextsep(i,ieq)
c
c
c           print*,'Report,ok,j,i,ieq=',ok,j,i,ieq
c           print*,'Report:',char(kchar(j)),char(kchar(j+1)),
c     c      char(kchar(j+2)),char(kchar(j+3)),char(kchar(j+4))
C     c     ,char(kchar(j+5)),char(kchar(j+6)),char(kchar(j+7))
c
           if(i.ge.ieq)i=ieq-1
cps        Sets all characters in the erronous field to spaces:   
           DO 170 k=j,i
              kchar(k)=32
 170       CONTINUE
c
           i=i+1
         end if
c
 175  CONTINUE
C
cps         HANDLE TYPING ERRORS CORRECTED BY THE 'E E E'
cps         PROCEDURE AS SPECIFIED IN GTS MANUAL.
      CALL REMEEE
C
C*    TEST IF SEA STATION REPORT.
C
      IF ( KDEC(4).GT.14 ) GO TO 5000
C
C*    CLEAR SPURIOUS SM CHARACTERS FROM KWBC REPORTS
C
C
      K = IPT + 10
      DO 200 I=IPT,K
         IF ( KCHAR(I).EQ.32.AND.KCHAR(I+1).EQ.83.AND.KCHAR(I+2).
     C      EQ.77.AND.KCHAR(I+3).EQ.32)
     C           THEN
C---                 WRITE (*,9901) ICOR
C---                 CALL PRTBULL (IPT,IEQ)
                     KCHAR(I+1) = 32
                     KCHAR(I+2) = 32
                     ICOR = 1
                 END IF
C
  200 CONTINUE
C
C
C*    SOME CENTRES USE '333' GROUP IN THE FORM '333//'
C
C
      DO 250 I=IPT,IEQ
         IF ( KCHAR(I).EQ.51.AND.KCHAR(I+1).EQ.51.AND.KCHAR(I+2).
     C      EQ.51.AND.KCHAR(I+3).EQ.47.AND.KCHAR(I+4).EQ.47)
     C           THEN
C---                 WRITE (*,9901) ICOR
C---                 CALL PRTBULL (IPT,IEQ)
                     KCHAR(I+3) = 32
                     KCHAR(I+4) = 32
                     ICOR = 1
                 END IF
  250 CONTINUE
C
C
C*    FIXUP COMMON ERRORS IN IIIII GROUP FROM LAND STATIONS.
C
C
C     REMOVE EXTRA CHARACTER PRECEDING IIIII IN REPORTS FROM
C     MXKF,AMMC,NZKL AND EESA.
C
      K = IPT+5
      DO 300 I=IPT,K
         IF ( KCHAR(I).GE.48.AND.KCHAR(I).LE.57) GO TO 350
         IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90) GO TO 350
         IF (KCHAR(I).EQ.32) GO TO 300
C--      WRITE (*,9901) ICOR
C--      CALL PRTBULL (IPT,IEQ)
         ICOR = 2
         KCHAR(I) = 32
  300 CONTINUE
  350 IF (ICOR.EQ.2) CALL NEXTPRT (IPT,IEQ)
C
C     ADD II OF 03 TO U.K. REPORTS FROM CENTRES OTHER THAN EGRR.
C
      K = IAH + 4
      CALL NEXTLET (K,JAH)
      IF (KCHAR(K).EQ.69.AND.KCHAR(K+1).EQ.71)
     C          THEN
                    IF (KCHAR(K+2).EQ.82.AND.KCHAR(K+3).EQ.82)
     C                THEN
C----                     WRITE (*,9901) ICOR
C----                     CALL PRTBULL (IPT,IEQ)
                          IPT = IPT - 2
                          KCHAR(IPT) = 48
                          KCHAR(IPT+1) = 51
                          ICOR = 3
                      END IF
                END IF
C
C
C     REMOVE ANY EXTRA SHORT GROUPS BEFORE IIIII
C
      K = IPT
      CALL NEXTVAL (K,32,IEQ)
      K = K - IPT
      IF (K.LE.3) THEN
C--                   WRITE (*,9901) ICOR
C--                   CALL PRTBULL (IPT,IEQ)
                      IPT = K+ IPT
                      CALL NEXTPRT (IPT,IEQ)
                      ICOR = 4
                  END IF
C
C
C***
C*    COMMON FORMAT ERRORS IN REPORTS FROM SOUTH AMERICA
C     AND AFRICA
C
C
      DO 400 K=IPT,IEQ
C
C          $,],:,*,V OR ? INSTEAD OF = AT END OF REPORT
C         IF ANY OF THESE CHARACTERS IS FOLLOWED BY LF IT IS
C         REPLACED BY = .
C
           IF (KCHAR(K).EQ.36.OR.KCHAR(K).EQ.93.OR.KCHAR(K)
     C        .EQ.58.OR.KCHAR(K).EQ.42.OR.KCHAR(K).EQ.86
     C        .OR.KCHAR(K).EQ.63.AND.KCHAR(K+1).EQ.13)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 61
                     ICOR = 5
                 END IF
C
C          = SIGN MISSING AT END OF NIL REPORTS
C
           IF (KCHAR(K).EQ.76.AND.KCHAR(K+1).EQ.13)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 61
                     ICOR = 6
                 END IF
C
C          - INSTEAD OF SPACE
C
           IF (KCHAR(K).EQ.45)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 32
                     ICOR = 7
                 END IF
C
C          $ INSTEAD OF =
C
           IF (KCHAR(K).EQ.36)
     C           THEN
C--                  WRITE (*,9901) ICOR
C--                  CALL PRTBULL (IPT,IEQ)
                     KCHAR(K) = 61
                     ICOR = 8
                 END IF
C
C
  400 CONTINUE
C
C     RESET POINTER TO END OF REPORT
C
      IEQ = IPT
      CALL NEXTEQ (IEQ,IGS)
C
C
C     IF NO END OF REPORT HAS BEEN FOUND INSERT = AT END OF
C     LINE . THIS ENSURES THAT AT LEAST SECTION 1 OF REPORTS IS
C     DECODED.
C
C     IF THE REPORT IS LONGER THAN 144 CHARACTERS = IS ALSO
C     PRESUMED MISSING.
C
      LEN = IEQ - IABS(IPT)
C
      IF (IEQ.GE.IGS.OR.LEN.GE.144)
     C      THEN
C--           WRITE (*,9901) ICOR
C--           CALL PRTBULL (IPT,IEQ)
              K = IPT
              CALL NEXTEND (K,IGS)
              IEQ = K
              KCHAR(K) = 61
              ICOR = 9
C
            END IF
C
C
C
C
 5000 CONTINUE
      IF (ICOR.EQ.0) RETURN
      IF (ICOR.EQ.3) RETURN
C--   CALL PRTBULL (IPT,IEQ)
C--   WRITE (*,9901)ICOR
C
 9901 FORMAT (1H ,'***************',I3,' *********************')
C
C
C
      RETURN
      END
      SUBROUTINE ICHWHW(IN,MINDIC,OUT)
C
C****
C*
C*    NAME     : ICHWHW
C*
C*    FUNCTION :  DECODE THE HEIGHT OF WAVES IN DECIMETERS.
C*
C*    INPUT    :  IN      - CODE FIGURE FOR THE HEIGHT
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUT     - DECODED HEIGHT
C*
C*             OUT IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN IN
C*
C****
C
      INTEGER OUT
C
C***   SET MISSING VALUE
C
      OUT=MINDIC
C
      IF(IN .EQ. MINDIC) RETURN
C
      OUT=IN*5
C
      RETURN
      END
      SUBROUTINE IC2700(ICODE,ICOVER)
C
C**** *IC2700*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 2700 INTO PERCENTAGE CLOUD COVERAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC2700(ICODE,ICOVER)*
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
      integer icode,icover
      integer ict(10),ipr(10),i
C
      DATA ICT/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9/
      DATA IPR/ 0,10,25,40,50,60,75,90,100,113/
C     N=9  (sky obscured...) in synop is stored as 113 % in BUFR
C
C
C     ------------------------------------------------------------------
C*          1.   CONVERT CLOUD COVERAGE IN OKTAS INTO PERCENTAGE.
C                ------------------------------------------------
 100  CONTINUE
C
      DO 101 I=1,10
C
      IF(ICODE.EQ.ICT(I)) THEN
                             ICOVER=IPR(I)
                             GO TO 200
                          END IF
 101  CONTINUE
C
      ICOVER=999999
C
 200  CONTINUE
C
      RETURN
      END 
      SUBROUTINE IC1855(ICODE,OUTCODE)
C
C     PURPOSE:  TO CONVERT CODE TABLE 1855 INTO BUFR TABLE 2002.
C
C     INTERFACE: *CALL* *IC1855(ICODE,OUTCODE)*
C
C     AUTHOR: P.SANNES 2010-05-10

      implicit none
      integer icode,outcode
      integer iw(4),ipr(4),i
      
      DATA IW/  0, 1, 3, 4/
      DATA IPR/ 0, 8, 4, 12/

      OUTCODE = 999999
      
      DO I=1,4 
         IF (ICODE.EQ.IW(I)) THEN
            OUTCODE=IPR(I)
         END IF
      END DO

      RETURN
      END 
      SUBROUTINE IC1751(ICODE,OUTCODE)
C
C**** *IC1751*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 1751 INTO BUFR TABLE 20033.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC1751(ICODE,OUTCODE)*
C

      implicit none
      integer icode,outcode
      integer ict(5),ipr(5),i
C
      DATA ICT/  1, 2, 3, 4, 5/
      DATA IPR/  8, 4,12, 2,10/
C
C     ------------------------------------------------------------------
C*          1.   CONVERT CAUSE OF ICE ACCRETION
C                ------------------------------
      OUTCODE = 999999
      
      DO I=1,5 
         IF (ICODE.EQ.ICT(I)) THEN
            OUTCODE=IPR(I)
         END IF
      END DO

      RETURN
      END 
      SUBROUTINE IC3551(ICODE,OUTCODE)
C
C**** *IC3551*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 3551 INTO BUFR TABLE 20032.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC3551(ICODE,OUTCODE)*
C

      implicit none
      integer icode,outcode
      integer ict(5),ipr(5),i
C
      DATA ICT/ 0, 1, 2, 3, 4/
      DATA IPR/ 0, 1, 2, 3, 4/
C
C     ------------------------------------------------------------------
C*          1.   CONVERT RATE OF ICE ACCRETION
C                -----------------------------

      OUTCODE = 999999
      DO I=1,5 
         IF (ICODE.EQ.ICT(I)) THEN
            ICODE = IPR(I)
         END IF
      END DO

      RETURN
      END 
      SUBROUTINE IC639(ICODE,OUTCODE)
C
C**** *IC639*
C
C
C     PURPOSE.
C     --------
C         TO CONVERT CODE TABLE 639 INTO BUFR TABLE 20034.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC639(ICODE,OUTCODE)*
C
C
      implicit none
      integer icode,outcode
      integer ict(10),ipr(10),i
C
      DATA ICT/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9/
      DATA IPR/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9/
C
C     ------------------------------------------------------------------
C*          1.   CONVERT SEA ICE CONCENTRATION
C                -----------------------------

      OUTCODE=999999
      DO I=1,10
         IF (ICODE.EQ.ICT(I)) THEN
            OUTCODE=IPR(I)
         END IF
      END DO

      RETURN
      END 
      SUBROUTINE ICPWPW(IN,MINDIC,OUT)
C
C****
C*
C*    NAME     : ICPWPW
C*
C*    FUNCTION :  DECODE THE PERIOD OF WAVES IN SEC
C*
C*    INPUT    :  IN      - CODE FIGURE FOR THE PERIOD
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUT     - THE DECODED PERIOD
C*
C*             OUT is set to missing value if IN=99
C*             (estimation of period impossible owing to confused sea)
C*             or if >= 63 (period greater than what can be stored in 6 bits)
C*             with a warning written to stdout in last case
C****

      implicit none
      integer in,mindic,out

      IF (IN.EQ.MINDIC .OR. IN.EQ.99) THEN
         OUT=MINDIC
      ELSE IF (IN.GE.63) THEN
         WRITE(*,*) 'Period of waves >= 63. Changed to missing'
         OUT=MINDIC
      ELSE
         OUT=IN
      END IF

      RETURN
      END
      SUBROUTINE ICTRTR (ICOUNT,IREG,KTR,IHOURS,MINDIC,ITR)
C****
C*
C*    NAME     : ICTRTR
C*
C*    FUNCTION :  DETERMINE DURATION OF RAINFALL . REGIONAL AND
C*                NATIONAL PRACTICES ARE HANDLED.
C*
C*    INPUT    :  IREG   : WMO REGION NUMBER
C*                ICOUNT : WMO COUNTRY NUMBER
C*                KTR    : CODE FIGURE FOR 'TR'
C*                IHOURS : REPORT TIME (HOURS)
C*                MINDIC : MISSING DATA VALUE
C*
C*    OUTPUT   :  ITR    :  MEASUREMENT PERIOD FOR RAINFALL (HOURS)
C*
C*                       ITR IS SET TO MISSING VALUE
C*                       IF ANY ERRORS IN IREG OR IHOURS
C*
C     PS MAR09: Don't trust the calculation of ITR when KTR is missing
C     for non-synoptic termins. Rewritten to return MINDIC in this case.
C***
C
      implicit none
      integer ICOUNT,IREG,KTR,IHOURS,MINDIC,ITR
      integer idurat(28),k,ind
C
      DATA IDURAT / 6, 24,  6, 12, 12, -9, 12, -9,
     *              6,  6, 24,  6,  6,  6,  6,  6,
     C             24,  6,  6,  6,  6, 12,  6, 12,
     C             24,  6, 12, 18 /
C
C
C
C***   SET MISSING VALUE
C
      ITR=MINDIC
C
C
      IF (IREG.LT.1.OR.IREG.GT.7) RETURN
      IF(IHOURS .LT. 0 .OR. IHOURS .GT. 24) RETURN
C
C***
C*    VALID 'TR' CODE FIGURE REPORTED.
C***
C
      IF (KTR.NE.MINDIC) THEN
         IF (KTR.LE.4) THEN
            ITR = KTR * 6
         ELSE IF (KTR.EQ.5) THEN
            ITR = 1
         ELSE IF (KTR.EQ.6) THEN
            ITR = 2
         ELSE IF (KTR.EQ.7) THEN
            ITR = 3
         ELSE IF (KTR.EQ.8) THEN
            ITR = 9
         ELSE IF (KTR.EQ.9) THEN
            ITR = 15
         END IF
         RETURN
      END IF
C
C***
C*    NO 'TR' FIGURE . GROUP MAY HAVE BEEN OMITTED BECAUSE RRR = 0
C*    OR BECAUSE NATIONAL PRACTICE IS TO CODE A / .
C***
C
C     ARRAY IDURAT IS USED TO DEFINE THE PERIOD FOR
C     DIFFERENT REGIONS AND DIFFERENT REPORT TIMES
C
C
C      REGION I     00 & 12 Z        6
C                      18 Z         12
C                      06 Z         24
C      REGION II    00 & 12 Z       12
C                   06 & 18 Z     MINDIC
C      REGION III   00&06&12 Z       6
C                      12 Z         24
C      REGION IV       ALL           6
C      REGION V        00 Z         24
C                   06&12&18 Z       6
C      REGION VI    00 & 12 Z        6
C                   06 & 18 Z       12
C
C

      K = IHOURS
C     Return missing value if non-synoptic hour
      IF (K.NE.0 .AND. K.NE.6 .AND. K.NE.12 .AND. K.NE.18) RETURN
C
C     IND IS THE INDEX TO DEFINE THE REGION AND REPORT TIME
C
      IND=(IREG-1)*4 + K/6 +1
C
C
C     SOME COUNTRIES IN REGION 2 HAVE NATIONAL PRACTICES.
C
      IF ( IREG.NE.2 ) THEN
         ITR = IDURAT(IND)
         RETURN
      END IF
C
C***
C*    REGION 2 NATIONAL PRACTICES.
C***
C
C     SAUDI ARABIA . 'TR' ALWAYS REPORTED AS / . PERIOD IS 12 HOURS.
C     COUNTRY NUMBER IS 020 ( REGION 2 ).
C
      IF ( ICOUNT.EQ.20 ) THEN
         ITR = 12
         RETURN
      END IF
C
C     CHINA . PERIOD IS ALWAYS 6 HOURS.
C     COUNTRY NUMBER IS 250 ( REGION 2 ).
C
      IF ( ICOUNT.EQ.250 ) THEN
         ITR = 6
         RETURN
      END IF
C
C     INDIA AND SRI LANKA . PERIOD IS FROM 0300.
C     COUNTRY NUMBERS FOR INDIA ARE 100 AND 110 AND FOR SRI LANKA 120.
C
      IF ( ICOUNT.GE.100.AND.ICOUNT.LE.120 ) THEN
         ITR = IHOURS - 3
         IF (ITR.LE.0) ITR = ITR + 24
         RETURN
      END IF
C
C***
C*    REGION 2 REGIONAL PRACTICE.
C***
C
      IF (IDURAT(IND) .EQ. -9) RETURN
      ITR = IDURAT(IND)
C
      RETURN
      END
      SUBROUTINE IC0264(INA3,MINDIC,OUTA3)
C****
C*
C*    NAME     : IC0264
C*
C*    FUNCTION :  DECODE THE INDICATOR OF STANDARD ISOBARIC
C*                SURFACE A3 IN HECTOPASCAL
C*
C*    INPUT    :  INA3    - CODE FIGURE FOR A3
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTA3   - DECODED A3
C*
C*             OUTA3 IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INA3
C*
C****
C
      implicit none
      integer INA3,MINDIC,OUTA3
C
C***   SET MISSING VALUE
C
      OUTA3=MINDIC
C
      IF(INA3 .EQ. MINDIC) RETURN
      IF(INA3 .LE. 0 .OR. INA3 .GE. 9) RETURN
C
      GO TO (100,900,1000,1000,500,900,700,850) INA3
C
100   OUTA3=1000
      RETURN
C
500   OUTA3=500
      RETURN
C
700   OUTA3=700
      RETURN
C
850   OUTA3=850
      RETURN
C
900   OUTA3=925
      RETURN
C
1000  RETURN
C
      END
      SUBROUTINE IC0700(INDD,MINDIC,OUTDD)
C
C****
C*
C*    NAME     : IC0700
C*
C*    FUNCTION :  DECODE THE DIRECTION FROM WHICH SURF. WIND IS
C*                BLOWING, OR THE DIRECTION OF THE SHIP (D,DS,...),
Cps               or true bearing of principal ice edge (Di, table 0739)
C*
C*    INPUT    :  INDD    - DIRECTION CODE FIGURE
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTDD  - DECODED DIRECTION IN DEGREES
C*
C*             OUTDD IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INDD
C*
C****
C
      implicit none
      integer INDD,MINDIC,OUTDD,idir(9)
C
C
      DATA IDIR/0,45,90,135,180,225,270,315,360/
C
C***   SET MISSING VALUE
C
      OUTDD=MINDIC
C
      IF(INDD .LT. 0 .OR. INDD .GT. 8) RETURN
C
      OUTDD=IDIR(INDD+1)
C
      RETURN
      END
      SUBROUTINE IC0777(IDD,ITEMP,MINDIC,IDEWPT)
C
C****
C*
C*    NAME     :  IC0777
C*
C*    FUNCTION :  DECODE DEW-POINT TEMPERATURE IN TENS OF DEGREE
C*
C*    INPUT    :  IDD     - DEW-POINT EPRESSION
C*             :  ITEMP   - TEMPERATURE
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  IDEWPT  - DECODED DEW-POINT TEMPERATURE
C*
C*             IDEWPT IS SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN IDD
C*
C*   A.HOLOPAINEN  JAN.83
C*
C****
C
C
C
C***   CHECK MISSING DATA INDICATOR
C
      implicit none
      integer IDD,ITEMP,MINDIC,IDEWPT
      integer idew

      IF(IDD .EQ. MINDIC) RETURN
C
      IF(ITEMP .EQ. MINDIC) RETURN
C
C     CHECK THE RANGE OF IDD
C
      IF(IDD .LT. 0 .OR. IDD .GT. 99) RETURN
C
      IF(IDD .GT. 51 .AND. IDD .LE. 55) RETURN
C
C
      IDEW=IDD
      IF(IDD .GE. 56) IDEW=10 * (IDD - 50)
C
      IDEWPT=ITEMP - IDEW
C
      RETURN
      END
      SUBROUTINE IC0877(IDD,IFF,IWW,ICOUNT,MINDIC,IDIR,ISPEED)
C
C****
C*
C*    NAME     :  IC0877
C*
C*    FUNCTION :  DECODE WIND DIRECTION AND SPEED
C*
C*    INPUT    :  IDD     - WIND DIRECTION IN TENS OF DEGREE
C*             :  IFF     - WIND SPEED
C*             :  IWW     - WIND SPEED INDICATOR , CODE TABLE 1855
C*             :  ICOUNT  - COUNTRY NUMBER (not used)
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  IDIR    - DECODED WIND DIRECTION
C*                ISPEED  - DECODED WIND SPEED
C*
C*             IDIR AND ISPEED ARE SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN IDD, IFF OR IWW
C*
C****
C
C***   SET MISSING VALUE
C
      implicit none
      integer IDD,IFF,IWW,ICOUNT,MINDIC,IDIR,ISPEED
      integer iw

      IDIR=MINDIC
      ISPEED=MINDIC

C     CHECK MISSING DATA INDICATOR
      IF(IWW .EQ. MINDIC .OR. (IDD .EQ. MINDIC .and.
     1   IFF .EQ. MINDIC)) RETURN

C     CHECK IF WIND INDICATOR IR IS CORRECT
      IF(IWW .NE. 0 .AND. IWW .NE. 1 .AND. IWW .NE. 3
     1  .AND. IWW .NE. 4) RETURN

C     IW IS THE MODIFIED WIND SPEED INDICATOR TO MAKE
C     IF-STATEMENTS SHORTER
C        IW=0 FOR METER/SEC
C        IW=1 FOR KNOTS
      IW=(IWW-1)/2
      IF (IFF.NE.MINDIC) THEN
         ISPEED=IFF
crr      change accuracy of wind by multiplying by 10.
         ISPEED=ISPEED*10
      END IF

C     IF SPEED IN KNOTS MODIFY TO M/S
      IF(IW.EQ.1 .AND. ISPEED.NE.MINDIC) CALL KTOMPSI(ISPEED)

c     In Bufr we cannot store wind speeds > 409 m/s. These
c     should be unphysical anyway, so we convert these ff 
c     into missing value, regarding them as typos
      IF (ISPEED .GT. 4090) THEN
         WRITE(*,*) 'wind speed set to missing; was ',ISPEED/10,'m/s'
         ISPEED=MINDIC
      END IF
      
C     dd=99 is not an allowed value for FM 12 and 13 (it is for FM 50),
C     but some countries still use dd=99, Use 0 for variable wind in bufr
      IF(IDD .EQ. 99) THEN
         IDIR=0
      ELSE IF(IDD.EQ.MINDIC .OR. IDD .GT. 36) THEN
         IDIR=MINDIC
      ELSE
         IDIR=IDD*10
      END IF

      RETURN
      END
      SUBROUTINE IC0857(IFF,IWW,MINDIC,IFMAX)
CLiL
C****
C*
C*    NAME     :  IC0857
C*
C*    FUNCTION :  DECODE WIND SPEED 
C*
C*    INPUT    :  IFF     - WIND SPEED
C*             :  IWW     - WIND SPEED INDICATOR , CODE TABLE 1855
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  IFMAX   - DECODED WIND SPEED
C*
C*             IFMAX ARE SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN  IFF OR IWW
C*
C* 
C*    AUTHOR   :  LISBET LOVHOIDEN DNMI
C*
C****
C
C***   SET MISSING VALUE
C
      implicit none
      integer IFF,IWW,MINDIC,IFMAX
      integer iw

      IFMAX=MINDIC
C
C
C
C***   CHECK MISSING DATA INDICATOR
C
      IF(IWW .EQ. MINDIC .OR. IFF .EQ. MINDIC) RETURN
C
C     CHECK IF WIND INDICATOR IR IS CORRECT
C
      IF(IWW .NE. 0 .AND. IWW .NE. 1 .AND. IWW .NE. 3
     1  .AND. IWW .NE. 4) RETURN
C
C
C     IW IS THE MODIFIED WIND SPEED INDICATOR TO MAKE
C     IF-STATEMENTS SHORTER
C        IW=0 FOR METER/SEC
C        IW=1 FOR KNOTS
C
      IW=(IWW-1)/2
      IFMAX=IFF

C     IF SPEED IN KNOTS MODIFY TO M/S
C
crr  change accuracy of wind by multiplying by 10.
      ifmax=ifmax*10
crr
      IF(IW .EQ. 1) CALL KTOMPSI(IFMAX)

cpsaug09 In Bufr we cannot store wind speeds > 409 m/s. These
cps      should be unphysical anyway, so we convert these ff 
cps      into missing value, regarding them as typos
      if (ifmax .gt. 4090) then
         write(*,*) 'wind speed set to missing; was ',ifmax/10,'m/s'
         ifmax=mindic
         return
      end if
      RETURN
      END
      SUBROUTINE ICSPSP1(ISPSP,MINDIC,IKDEC)
C
C****
C*
C*    NAME     :  ICSPSP1
C*
C*    FUNCTION :  DECODE SPECIAL PHENOMENA SPSP=10-13 
C*
C*    INPUT    :  ISPSP   - SPSP
C*             :  MINDIC  - MISSING DATA VALUE
C*             
C*    OUTPUT   :  IKDEC   - WHICH BOX IN KDEC TO PUT spsp 
C*
C*    AUTHOR   :  LISBET LOVHOIDEN DNMI
C*                P.S. feb09 added spsp=10 and simplified   
C*
C*             AT PRESENT ONLY SPSP=07,10,11 IS HANDLED.
C*             IKDEC IS SET TO MISSING VALUE IF ISPSP IS NOT 7,10-11
C*             
cpsapr07 Ought to send in and handle SPSP=0? also, as this tells which
cps      period ff or wwW1W2 is for. Complicated, but should be done!

      implicit none
      integer ISPSP,MINDIC,IKDEC

      IKDEC=MINDIC

      IF (ISPSP.EQ.11) THEN
C     ff=Highest gust since last observation.
         IKDEC=137
      ELSE IF (ISPSP.EQ.10) THEN
C     ff=Highest gust during 10 min immediatly prec. observation.
         IKDEC=138
      ELSE IF (ISPSP.EQ.7) THEN
C     tt=period for 91(1-4)ff, when (possibly) different from the one
C     covered by W1W2
         IKDEC=139
      END IF
C     Not decoded presently:
C     SPSP=12 ff= Max mean wind speed.
C     SPSP=13 ff= Mean wind speed.
C     SPSP=14 ff= Lowest mean wind speed.

      RETURN
      END
      SUBROUTINE IC1600(INHEI,LOWEST,MINDIC,OUTHEI)
C
C****
C*
C*    NAME     :  IC1600
C*
C*    FUNCTION :  DECODE THE HEIGHT OF LOWEST CLOUDS
C*
C*    INPUT    :  INHEI   - CODE FIGURE FOR THE HEIGHT
C*                LOWEST  - INDICATOR FOR LOWEST CLOUD
C*                          0 = LOW CLOUD
C*                          1 = MEDIUM CLOUD
C*                          2 = HIGH CLOUD
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTHEI  - DECODED HEIGHT OF LOWEST CLOUDS IN METRES
C*
C*             OUTHEI SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INHEI
C*
C****
C
      implicit none
      integer INHEI,LOWEST,MINDIC,OUTHEI
      integer iheight(12),ind
C
      DATA IHEIGHT/30,70,150,250,450,800,1250,1750,2250,2600,3500,8000/
C
C***   SET MISSING VALUE
C
      OUTHEI=MINDIC
C
C***   TEST THE VALIDITY OF THE CODE FIGURE
C
      IF(INHEI .LT. 0 .OR. INHEI .GT. 9) RETURN
C
C
C     FOR h = 9 DEFAULT HEIGHTS ARE ALLOCATED DEPENDING ON
C     WHETHER LOWEST CLOUD IS LOW, MEDIUM OR HIGH.
C
C
      IND=INHEI + 1 + LOWEST
      OUTHEI=IHEIGHT(IND)
      RETURN
      END
      SUBROUTINE IC1677(ICODE,MINDIC,IHEIGHT)
C
C****
C*    NAME     : IC1677
C*
C*    FUNCTION :  DECODE THE HEIGHT OF THE BASE OF THE
C*                LOWEST CLOUD  HH OR HSHS
cps                what is hh?
C*
C*    INPUT    :  ICODE    CODE NUMBER FOR THE HEIGHT
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  IHEIGHT   DECODED HEIGHT IN METRES
C*
C*             IHEIGHT SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN ICODE
C*
C****
C
      implicit none
      integer ICODE,MINDIC,IHEIGHT
      integer ihshs(9),ind
C
      DATA IHSHS/30,70,150,250,450,800,1250,1750,2250/
C
C
C***  SET MISSING VALUE
C
      IHEIGHT=MINDIC
C
      IF(ICODE .LT. 0 .OR. ICODE .GT. 98) RETURN
cpsoct06 In Bufr we cannot store heights > 20060 metres. These
cps      should be unphysical anyway, so we convert these hshs 
cps      into missing value, regarding them as typos
      if (icode .ge. 87 .and. icode .le. 89) then
         write(*,*) 'hshs set to missing; was ',icode
         return
      end if
C
cps      IF(ICODE .EQ. 89) THEN
cps                 IHEIGHT=22000
cps                 RETURN
cps                 END IF
C
      IF(ICODE .LE. 50) THEN
                 IHEIGHT=ICODE*30+15
                 IF(IHEIGHT .LT. 0) IHEIGHT=0
                 RETURN
                 END IF
C
      IF(ICODE .GE. 56 .AND. ICODE .LE. 80) THEN
                 IHEIGHT=(ICODE-50)*300+150
                 RETURN
                 END IF
C
      IF(ICODE .GE. 81 .AND. ICODE .LE. 88) THEN
                 IHEIGHT=(ICODE-80)*1500+9750
                 RETURN
                 END IF
C
C     CODE VALUE 99 IS NOT USED FOR HH, ONLY FOR HSHS
C     AND FOR TIME BEING 99 IS NOT DECODED AT ALL.
C
      IF(ICODE .GE. 90) THEN
                           IND=ICODE-89
                           IHEIGHT=IHSHS(IND)
                           RETURN
                        END IF
C
C
      END
      SUBROUTINE IC3590(INPRE,INPRE2,NILPRE,MINDIC,OUTPRE,OUTPRE2)
C****
C*
C*    NAME     : IC3590
C*
C*    FUNCTION :  DECODE THE AMOUNT OF PRECIPITATION
C*
C*    INPUT    :  INPRE   - PRECIPITATION CODE FIGURE
C*                INPRE2  - FIRST DECIMAL OF PRECIPITATION (NATIONAL GROUP)
C*                NILPRE  - INDICATOR FOR 'NIL' PRECIPITATION
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTPRE  - DECODED PRECIPITATION IN TENTHS OF MM
C*                OUTPRE2  - FIRST DECIMAL OF PRECIPITATION (NATIONAL GROUP)
C*
C*             OUTPRE SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INPRE
C*
C* CHANGES:
C* MARCH 1995 BY LISBET LOVHOIDEN DNMI.
C* Extended with inpre2 from national group for block 01, containes
C* first decimal of total amount of precipitation. to be added to RRR
C*
C* FEBRUARY 1996 BY LISBET LOVHOIDEN DNMI.
C* changed handling of RRR=990, set to 990, not zero, to mark less than 0.1 mm
C* It is not possible to store 2 decimals in precipitation buffercode.
C*
C* OCTOBER 2006 BY PAL SANNES MET.NO
C* 990 again coded as 0. We want Bufr message to be correct.
C* If RRR=0 with RT > 0, RRR and RT are set to missing
C*
C* JUNE 2008 BY PAL SANNES MET.NO
C* 990 coded as -0.1, according to B/C 1.10.3.3.2 (TM 307080)

C**** 
      implicit none
      integer INPRE,INPRE2,NILPRE,MINDIC,OUTPRE,OUTPRE2
C
C***   SET MISSING VALUE
C
      OUTPRE=MINDIC
      OUTPRE2=MINDIC
C***
C*    TEST IF 'NILPRE' ( IR , CODE TABLE 1819 ) INDICATES THAT
C*    PRECIPATION GROUP IS OMITTED BECAUSE RRR = 0 .
C***
C
      IF ( NILPRE.EQ.3 ) OUTPRE = 0
C
C     IR IS FREQUENTLY MISCODED SO AMOUNT OF RRR REPORTED IS ALSO
C     EXAMINED BEFORE RETURNING.
C
      IF ( INPRE.EQ.MINDIC ) RETURN

      IF (INPRE.EQ.0 .AND. INPRE2.NE. MINDIC .AND. INPRE2.NE.0) THEN
         WRITE(*,*) 'Inconsistency in precipitation groups: RRR=0, RT=',
     *        INPRE2
         WRITE(*,*) 'Precipitation set to missing'
         OUTPRE = MINDIC
         OUTPRE2 = MINDIC
         RETURN
      END IF

      IF (INPRE2.NE.MINDIC) THEN
         OUTPRE2=INPRE2
      END IF

      IF (INPRE .LE. 989) THEN
         OUTPRE=INPRE*10
         IF (INPRE2.NE.MINDIC) THEN
            IF (INPRE2.LT.5) THEN
               OUTPRE=OUTPRE+INPRE2
            ELSE
               OUTPRE=OUTPRE-10+INPRE2
            ENDIF
         ENDIF
      ELSE IF (INPRE.EQ.990) THEN
         OUTPRE = -1
         OUTPRE2 = MINDIC
      ELSE IF (INPRE .GT. 990 .AND. INPRE .LE. 999) THEN
         OUTPRE = INPRE - 990
         IF (INPRE2.NE.MINDIC) OUTPRE2=OUTPRE
      END IF
      RETURN
      END
      SUBROUTINE IC3850(INTEMP,ISIGN,MINDIC,OUTTEMP,METHOD)
C****
C*
C*    NAME     : IC3850
C*
C*    FUNCTION :  DECODE THE SEA SURFACE-TEMPERATURE
C*
C*    INPUT    :  INTEMP   TEMPERATURE VALUE
C*                ISIGN    SIGN INDICATOR FOR TEMPERATURE
C*                         0 = NOT NEGATIVE, 1 = NEGATIVE
C*                         2 = NOT NEGATIVE, 3 = NEGATIVE
C*                         ...... ETC
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTTEMP  OUTPUT TEMPERATURE WITH CORRECT SIGN
C*                METHOD   METHODE OF MEASUREMENT.
C*                         (0 IF ISIGN 0/1,1 IF ISIGN 2/3 ETC)
C*       
C*             OUTTEMP  AND METHOD IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INTEMP
C*
C*    AUTHOR   LISBET LOVHOIDEN  APRIL 1995     
C****
C
C
      implicit none
      integer INTEMP,ISIGN,MINDIC,OUTTEMP,METHOD
      integer isn
C
C***   SET MISSING VALUE
C
      OUTTEMP=MINDIC
      METHOD=MINDIC
C
C
C     TEST FOR MISSING DATA AND VALIDITY OF SIGN
C
      IF(INTEMP .EQ. MINDIC .OR. ISIGN .EQ. MINDIC) RETURN
C
C     Test if TwTwTw is too big (> 545) to be stored in BUFR
      IF ((INTEMP/10.+273.15)*100 .GE. 2**15 - 1) THEN
         WRITE(*,*) 'TwTwTw too big (>545). Changed to missing'
         RETURN
      END IF

       ISN=ISIGN+1 
       GO TO (10,11,12,13,14,15,16,17) ISN
       WRITE(*,*) 
     +      'Impossible value for sw (>7). sw and TwTwTw set to missing'
       GO TO 25
C
 10    CONTINUE
C*     ISIGN .EQ. 0
       OUTTEMP=INTEMP
       METHOD=0
       GO TO 25
C
 11    CONTINUE
C*     ISIGN .EQ. 1
       OUTTEMP=-1*INTEMP
       METHOD=0
       GO TO 25
C
 12    CONTINUE
C*     ISIGN .EQ. 2
       OUTTEMP=INTEMP
       METHOD=1
       GO TO 25
C
 13    CONTINUE
C*     ISIGN .EQ. 3
       OUTTEMP=-1*INTEMP
       METHOD=1
       GO TO 25
C
 14    CONTINUE
C*     ISIGN .EQ. 4
       OUTTEMP=INTEMP
       METHOD=2
       GO TO 25
C
 15    CONTINUE
C*     ISIGN .EQ. 5
       OUTTEMP=-1*INTEMP
       METHOD=2
       GO TO 25
C
 16    CONTINUE
C*     ISIGN .EQ. 6
       OUTTEMP=INTEMP
       METHOD=11
       GO TO 25
C
 17    CONTINUE
C*     ISIGN .EQ. 7
       OUTTEMP=-1*INTEMP
       METHOD=11
       GO TO 25
C
 25   CONTINUE
      RETURN
      END
      SUBROUTINE IC3855(INTEMP,ISIGN,MINDIC,OUTTEMP,METHOD)
C****
C*
C*    NAME     : IC3855
C*
C*    FUNCTION :  DECODE THE SEA-SURF. WET-BULBTEMPERATURE
C*
C*    INPUT    :  INTEMP   TEMPERATURE VALUE
C*                ISIGN    SIGN INDICATOR FOR TEMPERATURE
C*                         0 = NOT NEGATIVE, 1 = NEGATIVE
C*                         2 = NEGATIVE (ICED)
C*                         5 = NOT NEGATIVE, 6 = NEGATIVE
C*                         7 = NEGATIVE (ICED)
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTTEMP  OUTPUT TEMPERATURE WITH CORRECT SIGN
C*                METHOD   METHODE OF MEASUREMENT.
C*                         (0 IF ISIGN 0/1,1 IF ISIGN 2,
C*                          2 IF ISIGN 5/6,3 IF ISIGN 7)
C*       
C*             OUTTEMP  AND METHOD IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INTEMP
C*
C*    AUTHOR   LISBET LOVHOIDEN  APRIL 1995     
C****
C
C
      implicit none
      integer INTEMP,ISIGN,MINDIC,OUTTEMP,METHOD
      integer isn
C
C***   SET MISSING VALUE
C
      OUTTEMP=MINDIC
      METHOD=MINDIC
C
C
C     TEST FOR MISSING DATA AND VALIDITY OF SIGN
C
      IF(INTEMP .EQ. MINDIC .OR. ISIGN .EQ. MINDIC) RETURN
C
       ISN=ISIGN+1
       GO TO (10,11,12,13,14,15,16,17) ISN
       GO TO 25 
C
 10    CONTINUE
C*     ISIGN .EQ. 0
       OUTTEMP=INTEMP
       METHOD=0
       GO TO 25
C
 11    CONTINUE
C*     ISIGN .EQ. 1
       OUTTEMP=-1*INTEMP
       METHOD=0
       GO TO 25
C
 12    CONTINUE
C*     ISIGN .EQ. 2
       OUTTEMP=INTEMP
       METHOD=1
       GO TO 25
C
 13    CONTINUE
C*     ISIGN .EQ. 3
       GO TO 25
C
 14    CONTINUE
C*     ISIGN .EQ. 4
       GO TO 25
C
 15    CONTINUE
C*     ISIGN .EQ. 5
       OUTTEMP=INTEMP
       METHOD=2
       GO TO 25
C
 16    CONTINUE
C*     ISIGN .EQ. 6
       OUTTEMP=-1*INTEMP
       METHOD=2
       GO TO 25
C
 17    CONTINUE
C*     ISIGN .EQ. 7
       OUTTEMP=INTEMP
       METHOD=3
       GO TO 25
C
 25   CONTINUE
      RETURN
      END
      SUBROUTINE IC3845(INTEMP,ISIGN,ICOUNT,IREG,MINDIC,OUTTEMP)
C****
C*
C*    NAME     : IC3845
C*
C*    FUNCTION :  DECODE THE TEMPERATURE
C*
C*    INPUT    :  INTEMP   TEMPERATURE VALUE
C*                ISIGN    SIGN INDICATOR FOR TEMPERATURE
C*                         0 = NOT NEGATIVE, 1 = NEGATIVE
C*                ICOUNT  COUNTRY NUMBER.
C*                IREG  REGION NUMBER.
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTTEMP  OUTPUT TEMPERATURE WITH CORRECT SIGN
C*
C*             OUTTEMP IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INTEMP
C*
C****
C
      implicit none
      integer INTEMP,ISIGN,ICOUNT,IREG,MINDIC,OUTTEMP
C
C***   SET MISSING VALUE
C
      OUTTEMP=MINDIC
C
C
C     TEST FOR MISSING DATA AND VALIDITY OF SIGN
C
      IF(INTEMP .EQ. MINDIC .OR. ISIGN .EQ. MINDIC) RETURN
      IF(ISIGN .LT. 0 .OR. ISIGN .GT. 1) RETURN
C
      OUTTEMP=INTEMP
      IF(ISIGN .EQ. 1) OUTTEMP = -1*OUTTEMP
C
      RETURN
C     TEMPERATURE IS GIVEN IN FAHRENEIT IN CUBA,NICARAGUA AND PANAMA
C     ( REGION 4 COUNTRY NUMBERS 70,170 AND 190 ) AND HAS TO BE
C     CONVERTED TO CELSIUS.
C
C      IF ( IREG.NE.4 ) RETURN
C      IF ( ICOUNT.NE.70.AND.ICOUNT.NE.170.AND.ICOUNT.NE.190) RETURN
C      CALL FTOC2 ( OUTTEMP )
C
C
C      RETURN
      END
      SUBROUTINE IC3931(ITA,ITT,MINDIC,ITEMP)
C
C****
C*
C*    NAME     :  IC3931
C*
C*    FUNCTION :  DECODE TEMPERARURE IN TENTHS OF DEGREE
C*
C*    INPUT    :  ITA     - APPROXIMATE TENTHS VALUE AND SIGN BIT
C*             :  ITT     - TENS AND UNIT DIGITS OF TEMPERATURE
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  ITEMP   - DECODED TEMPERATURE
C*
C*
C*   A.HOLOPAINEN  JAN.83
C*
C****
C
C
      implicit none
      integer ITA,ITT,MINDIC,ITEMP
      integer iva
C
C***   CHECK MISSING VALUES
C
      IF(ITA .EQ. MINDIC .OR. ITT .EQ. MINDIC) RETURN
C
C     CHECK THE RANGE OF ITA
C
      IF(ITA .LT. 0 .OR. ITA .GT. 9) RETURN
C
C     POSITIVE TEMPERATURE
C
      IVA=2*(ITA/2)
      IF(IVA .EQ. ITA) THEN
                    ITEMP = 10 * ITT + ITA
                          RETURN
                       END IF
C
C     NEGATIVE TEMPERATURE
C
      ITEMP = -10 * ITT - ITA
      RETURN
      END
      SUBROUTINE IC4377(INVIS,MINDIC,OUTVIS)
C
C****
C*
C*    NAME     :  IC4377
C*
C*    FUNCTION :  DECODE HORIZONTAL VISIBILITY AT SURFACE VV
C*
C*    INPUT    :  INVIS   -VISIBILITY CODE FIGURE
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTVIS  -DECODED VISIBILITY IN METRES
C*
C*             OUTVIS IS SET TO MISSING DATA VALUE
C*             IF ANY ERRORS FOUND IN INVIS
C*
C****
C
C
C
      implicit none
      integer INVIS,MINDIC,OUTVIS
      integer ivisib(9),ind
C
      DATA IVISIB /50,200,500,1000,2000,4000,10000,20000,55000/
C
C***   SET OUTVIS TO MISSING VALUE
C
      OUTVIS=MINDIC
C
      IF(INVIS .EQ. MINDIC) RETURN
      IF(INVIS .GE. 51 .AND. INVIS .LE. 55) RETURN
      IF(INVIS .LT. 0 .OR. INVIS .GT. 99) RETURN
C
      IF(INVIS .EQ. 89) THEN
                              OUTVIS=75000
                              RETURN
                           END IF
C
      IF(INVIS .EQ. 90) THEN
                              OUTVIS=25
                              RETURN
                           END IF
C
      IF(INVIS .EQ. 0) THEN
                             OUTVIS=50
                             RETURN
                           END IF
C
C
      IF(INVIS .GE. 1 .AND. INVIS .LE. 50)
     1       THEN
                 OUTVIS=100 * INVIS
                 RETURN
             END IF
C
      IF(INVIS .GE. 56 .AND. INVIS .LE. 80)
     1       THEN
                 OUTVIS=(INVIS - 50) * 1000
                 RETURN
             END IF
C
      IF(INVIS .GE. 81 .AND. INVIS .LE. 88)
     1       THEN
                 OUTVIS=(INVIS - 80) * 5000 + 30000
                 RETURN
             END IF
C
      IF(INVIS .GE. 91 .AND. INVIS .LE. 99)
     1       THEN
                 IND=INVIS-90
                 OUTVIS=IVISIB(IND)
                 RETURN
             END IF
C
C
      RETURN
      END
      SUBROUTINE IC4451(INVS,MINDIC,OUTVS)
C
C****
C*
C*    NAME     : IC4451
C*
C*    FUNCTION :  DECODE SHIPS AVERAGE SPEED  VS
C*
C*    INPUT    :  INVS    - SPEED CODE FIGURE
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTVS   - DECODED SPEED M/S
C*
C*
cps            OUTVS IS SET TO MISSING DATA VALUE
C*             IF ANY ERRORS FOUND IN INVS
C*
C****
C
      implicit none
      integer INVS,MINDIC,OUTVS
      integer ispeed(10)
C
      DATA ISPEED/0,1,4,7,9,12,14,17,20,22/
C
C***   SET MISSING VALUE
C
cpsep06      OUTVIS = MINDIC
      OUTVS = MINDIC
C
      IF(INVS .LT. 0 .OR. INVS .GT. 9) RETURN
C
      OUTVS=ISPEED(INVS+1)
C
      RETURN
      END

      SUBROUTINE IC4077(TT,MINDIC,MINUTES)
C
C****
C*
C*    NAME     : IC4077
C*
C*    FUNCTION :  DECODE tt IN 90711 INTO MINUTES 
C*
C*    INPUT    :  TT    -  CODE FIGURE
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  MINUTES   - DECODED TT
C*    P.SANNES  2009
      implicit none
      integer TT,MINDIC,MINUTES

      MINUTES = MINDIC

      IF (TT.GT.68) THEN
         RETURN
      ELSE IF (TT.LE.60) THEN
         MINUTES = TT*6
      ELSE IF (TT.LE.66) THEN
         MINUTES = 6*60 - 30 + (TT-60)*60 
      ELSE IF (TT.EQ.67) THEN
         MINUTES = 15*60
      ELSE IF (TT.EQ.68) THEN
         MINUTES = 18*60
      END IF

      RETURN
      END

      subroutine set_descriptors(subtype)
      implicit none
      include 'comdescr.f'   ! ktdlen,ktdlst
      integer subtype
      
      ktdlen = 3
      if (subtype.eq.11 .or. subtype.eq.14) then ! land report, AAXX
            ktdlst(1) = 307079
c$$$         ktdlst(1) = 301090
c$$$         ktdlst(2) = 302031
c$$$         ktdlst(3) = 302035
c$$$         ktdlst(4) = 302036
c$$$         ktdlst(5) = 101000
c$$$         ktdlst(6) = 031000
c$$$         ktdlst(7) = 302047
c$$$         ktdlst(8) = 008002
c$$$         ktdlst(9) = 101000
c$$$         ktdlst(10) = 031000
c$$$         ktdlst(11) = 302048
c$$$         ktdlst(12) = 302037
c$$$         ktdlst(13) = 102000
c$$$         ktdlst(14) = 031000
c$$$         ktdlst(15) = 022061
c$$$         ktdlst(16) = 020058
c$$$         ktdlst(17) = 101000
c$$$         ktdlst(18) = 031000
c$$$         ktdlst(19) = 302056
c$$$         ktdlst(20) = 101000
c$$$         ktdlst(21) = 031000
c$$$         ktdlst(22) = 302055
c$$$         ktdlst(23) = 302043
c$$$         ktdlst(24) = 302044
c$$$         ktdlst(25) = 101000
c$$$         ktdlst(26) = 031000
c$$$         ktdlst(27) = 302045
c$$$         ktdlst(28) = 101000
c$$$         ktdlst(29) = 031000
c$$$         ktdlst(30) = 302046
c$$$         ktdlen = 30
         ktdlst(2) = 004025
         ktdlst(3) = 011042
      else if (subtype.ge.21 .and. subtype.le.24) then ! sea report, BBXX
         ktdlst(1) = 308009
         ktdlst(2) = 004025
         ktdlst(3) = 011042
      end if
C 307079 expands to
C   1 001001  WMO BLOCK NUMBER [NUMERIC] 0 0 7
C   2 001002  WMO STATION NUMBER [NUMERIC] 0 0 10
C   3 001015  STATION OR SITE NAME [CCITTIA5] 0 0 160
C   4 002001  TYPE OF STATION [CODE TABLE 2001] 0 0 2
C   5 004001  YEAR [YEAR] 0 0 12
C   6 004002  MONTH [MONTH] 0 0 4
C   7 004003  DAY [DAY] 0 0 6
C   8 004004  HOUR [HOUR] 0 0 5
C   9 004005  MINUTE [MINUTE] 0 0 6
C  10 005001  LATITUDE (HIGH ACCURACY) [DEGREE] 5 -9000000 25
C  11 006001  LONGITUDE (HIGH ACCURACY) [DEGREE] 5 -18000000 26
C  12 007030  HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL (SEE NOTE 3) [M] 1 -4000 17
C  13 007031  HEIGHT OF BAROMETER ABOVE MEAN SEA LEVEL (SEE NOTE 4) [M] 1 -4000 17
C  14 010004  PRESSURE [PA] -1 0 14
C  15 010051  PRESSURE REDUCED TO MEAN SEA LEVEL [PA] -1 0 14
C  16 010061  3-HOUR PRESSURE CHANGE [PA] -1 -500 10
C  17 010063  CHARACTERISTIC OF PRESSURE TENDENCY [CODE TABLE 10063] 0 0 4
C  18 010062  24-HOUR PRESSURE CHANGE [PA] -1 -1000 11
C  19 007004  PRESSURE [PA] -1 0 14
C  20 010009  GEOPOTENTIAL HEIGHT [GPM] 0 -1000 17
C  21 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  22 012101  TEMPERATURE/DRY-BULB TEMPERATURE [K] 2 0 16
C  23 012103  DEW-POINT TEMPERATURE [K] 2 0 16
C  24 013003  RELATIVE HUMIDITY [%] 0 0 7
C  25 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  26 020001  HORIZONTAL VISIBILITY [M] -1 0 13
C  27 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  28 013023  TOTAL PRECIPITATION PAST 24 HOURS [KG/M**2] 1 -1 14
C  29 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  30 020010  CLOUD COVER (TOTAL) [%] 0 0 7
C  31 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  32 020011  CLOUD AMOUNT [CODE TABLE 20011] 0 0 4
C  33 020013  HEIGHT OF BASE OF CLOUD [M] -1 -40 11
C  34 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  35 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  36 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C     104000
C  37 031001  DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 8
C  38 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  39 020011  CLOUD AMOUNT [CODE TABLE 20011] 0 0 4
C  40 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  41 020013  HEIGHT OF BASE OF CLOUD [M] -1 -40 11
C     105000
C  42 031001  DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 8
C  43 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  44 020011  CLOUD AMOUNT [CODE TABLE 20011] 0 0 4
C  45 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  46 020014  HEIGHT OF TOP OF CLOUD [M] -1 -40 11
C  47 020017  CLOUD TOP DESCRIPTION [CODE TABLE 20017] 0 0 4
C     106000
C  48 031000  SHORT DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 1
C  49 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  50 020054  TRUE DIRECTION FROM WHICH CLOUDS ARE MOVING [DEGREE TRUE] 0 0 9
C  51 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  52 020054  TRUE DIRECTION FROM WHICH CLOUDS ARE MOVING [DEGREE TRUE] 0 0 9
C  53 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  54 020054  TRUE DIRECTION FROM WHICH CLOUDS ARE MOVING [DEGREE TRUE] 0 0 9
C  55 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C     105000
C  56 031000  SHORT DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 1
C  57 005021  BEARING OR AZIMUTH [DEGREE TRUE] 2 0 16
C  58 007021  ELEVATION (SEE NOTE 2) [DEGREE] 2 -9000 15
C  59 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  60 005021  BEARING OR AZIMUTH [DEGREE TRUE] 2 0 16
C  61 007021  ELEVATION (SEE NOTE 2) [DEGREE] 2 -9000 15
C  62 020062  STATE OF THE GROUND (WITH OR WITHOUT SNOW) [CODE TABLE 20062] 0 0 5
C  63 013013  TOTAL SNOW DEPTH [M] 2 -2 16
C  64 012113  GROUND MINIMUM TEMPERATURE, PAST 12 HOURS [K] 2 0 16
C     102000
C  65 031000  SHORT DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 1
C  66 022061  STATE OF THE SEA [CODE TABLE 22061] 0 0 4
C  67 020058  VISIBILITY SEAWARDS FROM A COASTAL STATION [M] -1 0 13
C     104000
C  68 031000  SHORT DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 1
C  69 002038  METHOD OF WATER TEMPERATURE AND/OR SALINITY MEASUREMENT [CODE TABLE 2038] 0 0 4
C  70 007063  DEPTH BELOW SEA/WATER SURFACE [M] 2 0 20
C  71 022043  SEA/WATER TEMPERATURE [K] 2 0 15
C  72 007063  DEPTH BELOW SEA/WATER SURFACE [M] 2 0 20
C     108000
C  73 031000  SHORT DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 1
C  74 020031  ICE DEPOSIT (THICKNESS) [M] 2 0 7
C  75 020032  RATE OF ICE ACCRETION [CODE TABLE 20032] 0 0 3
C  76 020033  CAUSE OF ICE ACCRETION [FLAG TABLE 20033] 0 0 4
C  77 020034  SEA ICE CONCENTRATION [CODE TABLE 20034] 0 0 5
C  78 020035  AMOUNT AND TYPE OF ICE [CODE TABLE 20035] 0 0 4
C  79 020036  ICE SITUATION [CODE TABLE 20036] 0 0 5
C  80 020037  ICE DEVELOPMENT [CODE TABLE 20037] 0 0 5
C  81 020038  BEARING OF ICE EDGE (SEE NOTE 3) [DEGREE TRUE] 0 0 12
C  82 020003  PRESENT WEATHER (SEE NOTE 1) [CODE TABLE 20003] 0 0 9
C  83 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  84 020004  PAST WEATHER (1) (SEE NOTE 2) [CODE TABLE 20004] 0 0 5
C  85 020005  PAST WEATHER (2) (SEE NOTE 2) [CODE TABLE 20005] 0 0 5
C  86 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  87 014031  TOTAL SUNSHINE [MINUTE] 0 0 11
C  88 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  89 014031  TOTAL SUNSHINE [MINUTE] 0 0 11
C  90 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  91 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  92 013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT [KG/M**2] 1 -1 14
C  93 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  94 013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT [KG/M**2] 1 -1 14
C  95 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  96 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  97 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  98 012111  MAXIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED [K] 2 0 16
C  99 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C 100 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C 101 012112  MINIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED [K] 2 0 16
C 102 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C 103 002002  TYPE OF INSTRUMENTATION FOR WIND MEASUREMENT [FLAG TABLE 2002] 0 0 4
C 104 008021  TIME SIGNIFICANCE [CODE TABLE 8021] 0 0 5
C 105 004025  TIME PERIOD OR DISPLACEMENT [MINUTE] 0 -2048 12
C 106 011001  WIND DIRECTION [DEGREE TRUE] 0 0 9
C 107 011002  WIND SPEED [M/S] 1 0 12
C 108 008021  TIME SIGNIFICANCE [CODE TABLE 8021] 0 0 5
C 109 004025  TIME PERIOD OR DISPLACEMENT [MINUTE] 0 -2048 12
C 110 011043  MAXIMUM WIND GUST DIRECTION [DEGREE TRUE] 0 0 9
C 111 011041  MAXIMUM WIND GUST SPEED [M/S] 1 0 12
C 112 004025  TIME PERIOD OR DISPLACEMENT [MINUTE] 0 -2048 12
C 113 011043  MAXIMUM WIND GUST DIRECTION [DEGREE TRUE] 0 0 9
C 114 011041  MAXIMUM WIND GUST SPEED [M/S] 1 0 12
C 115 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C 116 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C 117 002004  TYPE OF INSTRUMENTATION FOR EVAPORATION MEASUREMENT OR TYPE OF C [CODE TABLE 2004] 0 0 4
C 118 013033  EVAPORATION/EVAPOTRANSPIRATION [KG/M**2] 1 0 10
C     107000
C 119 031000  SHORT DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 1
C 120 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C 121 014002  LONG-WAVE RADIATION, INTEGRATED OVER PERIOD SPECIFIED [J/M**2] -3 -2048 12
C 122 014004  SHORT-WAVE RADIATION, INTEGRATED OVER PERIOD SPECIFIED [J/M**2] -3 -2048 12
C 123 014016  NET RADIATION, INTEGRATED OVER PERIOD SPECIFIED [J/M**2] -4 -16384 15
C 124 014028  GLOBAL SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD S [J/M**2] -2 0 16
C 125 014029  DIFFUSE SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD [J/M**2] -2 0 16
C 126 014030  DIRECT SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD S [J/M**2] -2 0 16
C     103000
C 127 031000  SHORT DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 1
C 128 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C 129 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C 130 012049  TEMPERATURE CHANGE OVER SPECIFIED PERIOD [K] 0 -30 6
 
C 308009 expands to
C   1 001011  SHIP OR MOBILE LAND STATION IDENTIFIER [CCITTIA5] 0 0 72
C   2 001012  DIRECTION OF MOTION OF MOVING OBSERVING PLATFORM** [DEGREE TRUE] 0 0 9
C   3 001013  SPEED OF MOTION OF MOVING OBSERVING PLATFORM* [M/S] 0 0 10
C   4 002001  TYPE OF STATION [CODE TABLE 2001] 0 0 2
C   5 004001  YEAR [YEAR] 0 0 12
C   6 004002  MONTH [MONTH] 0 0 4
C   7 004003  DAY [DAY] 0 0 6
C   8 004004  HOUR [HOUR] 0 0 5
C   9 004005  MINUTE [MINUTE] 0 0 6
C  10 005002  LATITUDE (COARSE ACCURACY) [DEGREE] 2 -9000 15
C  11 006002  LONGITUDE (COARSE ACCURACY) [DEGREE] 2 -18000 16
C  12 007030  HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL (SEE NOTE 3) [M] 1 -4000 17
C  13 007031  HEIGHT OF BAROMETER ABOVE MEAN SEA LEVEL (SEE NOTE 4) [M] 1 -4000 17
C  14 010004  PRESSURE [PA] -1 0 14
C  15 010051  PRESSURE REDUCED TO MEAN SEA LEVEL [PA] -1 0 14
C  16 010061  3-HOUR PRESSURE CHANGE [PA] -1 -500 10
C  17 010063  CHARACTERISTIC OF PRESSURE TENDENCY [CODE TABLE 10063] 0 0 4
C  18 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  19 007033  HEIGHT OF SENSOR ABOVE WATER SURFACE (SEE NOTE 6) [M] 1 0 12
C  20 012101  TEMPERATURE/DRY-BULB TEMPERATURE [K] 2 0 16
C  21 002039  METHOD OF WET-BULB TEMPERATURE MEASUREMENT [CODE TABLE 2039] 0 0 3
C  22 012102  WET-BULB TEMPERATURE [K] 2 0 16
C  23 012103  DEW-POINT TEMPERATURE [K] 2 0 16
C  24 013003  RELATIVE HUMIDITY [%] 0 0 7
C  25 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  26 007033  HEIGHT OF SENSOR ABOVE WATER SURFACE (SEE NOTE 6) [M] 1 0 12
C  27 020001  HORIZONTAL VISIBILITY [M] -1 0 13
C  28 007033  HEIGHT OF SENSOR ABOVE WATER SURFACE (SEE NOTE 6) [M] 1 0 12
C  29 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  30 013023  TOTAL PRECIPITATION PAST 24 HOURS [KG/M**2] 1 -1 14
C  31 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  32 020010  CLOUD COVER (TOTAL) [%] 0 0 7
C  33 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  34 020011  CLOUD AMOUNT [CODE TABLE 20011] 0 0 4
C  35 020013  HEIGHT OF BASE OF CLOUD [M] -1 -40 11
C  36 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  37 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  38 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C     104000
C  39 031001  DELAYED DESCRIPTOR REPLICATION FACTOR [NUMERIC] 0 0 8
C  40 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  41 020011  CLOUD AMOUNT [CODE TABLE 20011] 0 0 4
C  42 020012  CLOUD TYPE [CODE TABLE 20012] 0 0 6
C  43 020013  HEIGHT OF BASE OF CLOUD [M] -1 -40 11
C  44 008002  VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS) [CODE TABLE 8002] 0 0 6
C  45 020031  ICE DEPOSIT (THICKNESS) [M] 2 0 7
C  46 020032  RATE OF ICE ACCRETION [CODE TABLE 20032] 0 0 3
C  47 020033  CAUSE OF ICE ACCRETION [FLAG TABLE 20033] 0 0 4
C  48 020034  SEA ICE CONCENTRATION [CODE TABLE 20034] 0 0 5
C  49 020035  AMOUNT AND TYPE OF ICE [CODE TABLE 20035] 0 0 4
C  50 020036  ICE SITUATION [CODE TABLE 20036] 0 0 5
C  51 020037  ICE DEVELOPMENT [CODE TABLE 20037] 0 0 5
C  52 020038  BEARING OF ICE EDGE (SEE NOTE 3) [DEGREE TRUE] 0 0 12
C  53 002038  METHOD OF WATER TEMPERATURE AND/OR SALINITY MEASUREMENT [CODE TABLE 2038] 0 0 4
C  54 007063  DEPTH BELOW SEA/WATER SURFACE [M] 2 0 20
C  55 022043  SEA/WATER TEMPERATURE [K] 2 0 15
C  56 007063  DEPTH BELOW SEA/WATER SURFACE [M] 2 0 20
C  57 022001  DIRECTION OF WAVES [DEGREE TRUE] 0 0 9
C  58 022011  PERIOD OF WAVES [S] 0 0 6
C  59 022021  HEIGHT OF WAVES [M] 1 0 10
C  60 022002  DIRECTION OF WIND WAVES [DEGREE TRUE] 0 0 9
C  61 022012  PERIOD OF WIND WAVES [S] 0 0 6
C  62 022022  HEIGHT OF WIND WAVES [M] 1 0 10
C  63 022003  DIRECTION OF SWELL WAVES [DEGREE TRUE] 0 0 9
C  64 022013  PERIOD OF SWELL WAVES [S] 0 0 6
C  65 022023  HEIGHT OF SWELL WAVES [M] 1 0 10
C  66 022003  DIRECTION OF SWELL WAVES [DEGREE TRUE] 0 0 9
C  67 022013  PERIOD OF SWELL WAVES [S] 0 0 6
C  68 022023  HEIGHT OF SWELL WAVES [M] 1 0 10
C  69 020003  PRESENT WEATHER (SEE NOTE 1) [CODE TABLE 20003] 0 0 9
C  70 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  71 020004  PAST WEATHER (1) (SEE NOTE 2) [CODE TABLE 20004] 0 0 5
C  72 020005  PAST WEATHER (2) (SEE NOTE 2) [CODE TABLE 20005] 0 0 5
C  73 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  74 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  75 013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT [KG/M**2] 1 -1 14
C  76 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  77 013011  TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT [KG/M**2] 1 -1 14
C  78 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  79 007033  HEIGHT OF SENSOR ABOVE WATER SURFACE (SEE NOTE 6) [M] 1 0 12
C  80 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  81 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  82 012111  MAXIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED [K] 2 0 16
C  83 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  84 004024  TIME PERIOD OR DISPLACEMENT [HOUR] 0 -2048 12
C  85 012112  MINIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED [K] 2 0 16
C  86 007032  HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM) [M] 2 0 16
C  87 007033  HEIGHT OF SENSOR ABOVE WATER SURFACE (SEE NOTE 6) [M] 1 0 12
C  88 002002  TYPE OF INSTRUMENTATION FOR WIND MEASUREMENT [FLAG TABLE 2002] 0 0 4
C  89 008021  TIME SIGNIFICANCE [CODE TABLE 8021] 0 0 5
C  90 004025  TIME PERIOD OR DISPLACEMENT [MINUTE] 0 -2048 12
C  91 011001  WIND DIRECTION [DEGREE TRUE] 0 0 9
C  92 011002  WIND SPEED [M/S] 1 0 12
C  93 008021  TIME SIGNIFICANCE [CODE TABLE 8021] 0 0 5
C  94 004025  TIME PERIOD OR DISPLACEMENT [MINUTE] 0 -2048 12
C  95 011043  MAXIMUM WIND GUST DIRECTION [DEGREE TRUE] 0 0 9
C  96 011041  MAXIMUM WIND GUST SPEED [M/S] 1 0 12
C  97 004025  TIME PERIOD OR DISPLACEMENT [MINUTE] 0 -2048 12
C  98 011043  MAXIMUM WIND GUST DIRECTION [DEGREE TRUE] 0 0 9
C  99 011041  MAXIMUM WIND GUST SPEED [M/S] 1 0 12

C     Later we ought to decode also synop mobile (307090) - OOXX
      end subroutine set_descriptors
C----------------------------------------------------------------------

      SUBROUTINE SYNEXP_LAND(IERR )
C
C**** *SYNEXP_LAND*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR SYNOP DATA.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SYNEXP_LAND(IERR)*
C
C     METHOD.
C     -------
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
C          Use template 307079
C

      implicit none
      integer ierr
      integer i,j,imonth,iyear,id,im,iy,icover,iv,ibcode,iwcode
      integer NCG          ! Number of non cumulonimbus cloud groups
      integer NCB          ! Number of cumulonimbus cloud groups
      integer CC,CL,CM,CH
      integer is4NCNNCt,is56DLDMDH,is57CDaec,isS,isTwTw,isIce,is55SSS
      integer is54g0sndT
      
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'    ! nsub
      INCLUDE 'comwtc.f'   ! cval
      INCLUDE 'comkey.f'
      INCLUDE 'comrepl.f'  ! kdata

      NSUB = 1

      DO I=1,JP22
         DO J=1,2
            VALUES(I,J)=999999.
         END DO
      END DO

C     Get current date (year and month)
      CALL DATUM(KDEC(1),IMONTH,IYEAR)

      VALUES( 1,NSUB)=(KINT( 9)-48)*10+KINT(10)-48      ! WMO BLOCK NUMBER
      VALUES( 2,NSUB)=(KINT(11)-48)*100+(KINT(12)-48)*10! WMO STATION NUMBER
     *                +KINT(13)-48
      VALUES( 3,NSUB)= 1020.0                           ! STATION OR SITE NAME 
C     Would like to use missing value here, but BUFREN then fails.
C     Later we might include station name in station.amend.dat and then
C     change locstat. Notice that this will make station.amend.dat much
C     bigger, however. We presently use 9 bytes for each station in
C     station file, for station name we need 20 more bytes.
      CVAL(1) = '                    '  
      VALUES( 4,NSUB)=999999.                           ! TYPE OF STATION
      IF(KDEC(4).EQ.11) VALUES(4,NSUB)=1. ! Manned
      IF(KDEC(4).EQ.14) VALUES(4,NSUB)=0. ! Automatic
      VALUES( 5,NSUB)=FLOAT(IYEAR)                      ! YEAR
      VALUES( 6,NSUB)=FLOAT(IMONTH)                     ! MONTH
      VALUES( 7,NSUB)=FLOAT(KDEC(1))                    ! DAY
      VALUES( 8,NSUB)=FLOAT(KDEC(2))                    ! HOUR
      VALUES( 9,NSUB)=FLOAT(KDEC(9))                    ! MINUTE
C     If 9GGgg is present in section 1 in synop: overwrite hour and 
C     minute (observation time) with GG = kint(48) and gg = kint(49)
      IF(KINT(47).EQ.9) THEN
         IF(KINT(48).NE.MINDIC) THEN
            VALUES( 8,NSUB)=FLOAT(KINT(48))
         ENDIF
         IF(KINT(49).NE.MINDIC) THEN
            VALUES( 9,NSUB)=FLOAT(KINT(49))
         ENDIF
C     If termin=kdec(2)=00 and GG in 9GGgg is 23: then previous day!
         IF(KDEC(2).EQ.0 .AND. KINT(48).EQ.23) THEN
            CALL DECR_DATE(KDEC(1),IMONTH,IYEAR,ID,IM,IY)
            VALUES( 5,NSUB)=FLOAT(IY)
            VALUES( 6,NSUB)=FLOAT(IM)
            VALUES( 7,NSUB)=FLOAT(ID)
         ENDIF
      ENDIF
      VALUES(10,NSUB)=KDEC(5)/100.                      ! LATITUDE (HIGH ACCURACY)
      VALUES(11,NSUB)=KDEC(6)/100.                      ! LONGITUDE (HIGH ACCURACY)
      VALUES(12,NSUB)=999999.                           ! HEIGHT OF STATION
      IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
c change later: kdec(8) = ha/hp, must differ between these
      VALUES(13,NSUB)=999999.                           ! HEIGHT OF BAROMETER ABOVE MEAN SEA LEVEL
      VALUES(14,NSUB)=999999.                           ! PRESSURE
      IF(KDEC(34).NE.MINDIC) VALUES(14,NSUB)=KDEC(34)*10.
      VALUES(15,NSUB)=999999.                           ! PRESSURE  REDUCED TO MEAN SEA LEVEL
      IF(KDEC(35).NE.MINDIC) VALUES(15,NSUB)=KDEC(35)*10.
      VALUES(16,NSUB)=999999.                           ! 3 HOUR PRESSURE CHANGE
      IF(KDEC(39).NE.MINDIC) VALUES(16,NSUB)=KDEC(39)*10.
      VALUES(17,NSUB)=999999.                           ! CHARACTERISTIC OF PRESSURE TENDENCY
      IF(KDEC(38).NE.MINDIC) VALUES(17,NSUB)=KDEC(38)
      VALUES(18,NSUB)=999999.                           ! 24-HOUR PRESSURE CHANGE (333 58(9)p24p24p24)
      IF(KDEC(140).NE.MINDIC) VALUES(18,NSUB)=KDEC(140)*10.
      VALUES(19,NSUB)=999999.                           ! 007004  PRESSURE
      IF(KDEC(36).NE.MINDIC) VALUES(19,NSUB)=KDEC(36)*100.
      VALUES(20,NSUB)=999999.                           ! GEOPOTENTIAL HEIGHT
      IF(KDEC(37).NE.MINDIC) VALUES(20,NSUB)=KDEC(37)  
      VALUES(21,NSUB)=2.                                ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(22,NSUB)=999999.                           ! TEMPERATURE/DRY-BULB TEMPERATURE
      IF(KDEC(31).NE.MINDIC)
     1     VALUES(22,NSUB)=KDEC(31)/10.+273.15
      VALUES(23,NSUB)=999999.                           ! DEW POINT TEMPERATURE AT 2M
      IF(KDEC(32).NE.MINDIC)
     1     VALUES(23,NSUB)=KDEC(32)/10.+273.15
      VALUES(24,NSUB)=999999.                           ! RELATIVE HUMIDITY
      IF(KDEC(33).NE.MINDIC) VALUES(24,NSUB)=FLOAT(KDEC(33))
      VALUES(25,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(26,NSUB)=999999.                           ! HORIZONTAL VISIBILITY
      IF(KDEC(27).NE.MINDIC) VALUES(26,NSUB)=FLOAT(KDEC(27))
      VALUES(27,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(28,NSUB)=999999.                           ! TOTAL PRECIPITATION PAST 24 HOURS
      IF(KDEC(101).NE.MINDIC) THEN
         VALUES(28,NSUB)=FLOAT(KDEC(101))/10.
      ELSEIF(KDEC(41).EQ.24.AND.KDEC(40).NE.MINDIC) THEN
         VALUES(28,NSUB)=FLOAT(KDEC(40))/10.
      ENDIF
      VALUES(29,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(30,NSUB)=999999.                           ! CLOUD COVER (TOTAL)
      CALL IC2700(KDEC(42),ICOVER)
      VALUES(30,NSUB)=FLOAT(ICOVER)
      VALUES(31,NSUB)=63.                               ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
                                                        ! Set to missing initially, adjusted later
      VALUES(32,NSUB)=999999.                           ! CLOUD AMOUNT
      IF(KDEC(44).NE.MINDIC) VALUES(32,NSUB)=FLOAT(KDEC(44))
      VALUES(33,NSUB)=999999.                           ! HEIGHT OF BASE OF CLOUD
      IF(KDEC(43).NE.MINDIC.AND.KDEC(43).NE.16382
     +        .AND.KDEC(43).NE.16381) THEN
         VALUES(33,NSUB)=FLOAT(KDEC(43))
      ENDIF
      VALUES(34,NSUB)=999999.                           ! CLOUD TYPE
      IF(KDEC(45).NE.MINDIC) THEN
	 IF(KDEC(45).NE.14) THEN
            VALUES(34,NSUB)=FLOAT(KDEC(45))+30.
         ELSE
            VALUES(34,NSUB)=62.
         END IF
      END IF
      VALUES(35,NSUB)=999999.                           ! CLOUD TYPE
      IF(KDEC(46).NE.MINDIC) THEN
	 IF(KDEC(46).NE.14) THEN
            VALUES(35,NSUB)=FLOAT(KDEC(46))+20.
         ELSE
            VALUES(35,NSUB)=61.
         END IF
      END IF
      VALUES(36,NSUB)=999999.                           ! CLOUD TYPE
      IF(KDEC(47).NE.MINDIC) THEN
	 IF(KDEC(47).NE.14)THEN
            VALUES(36,NSUB)=FLOAT(KDEC(47))+10.
         ELSE
            VALUES(36,NSUB)=60.
         END IF
      END IF

C     Vertical significance (for low and middle clouds)
      CL = NINT(VALUES(34,NSUB))
      CM = NINT(VALUES(35,NSUB))
      CH = NINT(VALUES(36,NSUB))
      IF(CL.NE.999999 .AND. CL.NE.62) THEN
         VALUES(31,NSUB)=7.     ! Low cloud
      ELSE IF(CM.NE.999999 .AND. CM.NE.61) THEN
         VALUES(31,NSUB)=8.     ! Middle cloud
      ELSE IF(CH.NE.999999 .AND. CH.NE.60) THEN
         VALUES(31,NSUB)=0.     ! Observing rules for base of lowest cloud and
                                ! cloud types of FM 12-IX EXT. SYNOP and FM 13-IX
                                ! EXT. ship apply
      ELSE IF(CL.EQ.62 .AND. CM.EQ.61 .AND. CH.EQ.60) THEN
         VALUES(31,NSUB)=5.     ! Ceiling
      ELSE IF(ICOVER.eq.0) THEN ! Sky clear
         VALUES(31,NSUB)=62.    ! Value not applicable
      END IF
      
C     Individual cloud layers or masses.
C     It is assumed that these layers in KDEC have been sorted by ascending height
C     IV should after this section equal first free index in VALUES array. 
      IF(ICOVER.EQ.0) THEN      ! Sky clear
         VALUES(37,NSUB)=0.
         IV = 38
      ELSE IF(ICOVER.EQ.113) THEN ! Sky is obscured by fog and/or other meteorological phenomena
         VALUES(37,NSUB)=1.
         VALUES(38,NSUB)=5.     ! Vertical significance set to 'Ceiling'
         IV = 42                ! Values for Ns,C,hshs already initialized to missing
      ELSE
         NCG = 0                ! Number of non-cumulonimbus cloud groups, to be found
         NCB = 0                ! Number of cumulonimbus cloud groups, to be found
         IV = 37                ! Index for values array
         DO I=0,3               ! Up to 4 cloud groups is possible
            IF(KDEC(75+I*3).NE.MINDIC.OR.KDEC(76+I*3).NE.MINDIC
     *           .OR.KDEC(77+I*3).NE.MINDIC) THEN
               NCG = NCG + 1
C     For automatic stations cloud type is usually missing. According to
C     'Often asked questions' of Eva Cervena, 4 layers should then have
C     vertical significance set to 1,2,3,missing. We do it simply:
C     expecting all cloud type to be missing if automatic station, else
C     at least one layer marked as type cumulonimbus if manned station,
C     without actually checking this
               IF(NCG.EQ.4) THEN                        ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
                  VALUES(IV+1,NSUB)=999999.
               ELSE
                  VALUES(IV+1,NSUB)=FLOAT(NCG)
               END IF
               VALUES(IV+2,NSUB)=999999.                ! CLOUD AMOUNT
               IF(KDEC(75+I*3).NE.MINDIC) 
     *              VALUES(IV+2,NSUB)=FLOAT(KDEC(75+I*3))
               VALUES(IV+3,NSUB)=999999.                ! CLOUD TYPE 
               IF(KDEC(76+I*3).NE.MINDIC) THEN
                  CC = KDEC(76+I*3)
                  VALUES(IV+3,NSUB)=FLOAT(CC)
                  IF(CC.EQ.9) THEN ! Cumulonimbus (cb)
                     NCB = NCB + 1
                     NCG = NCG - 1
                     VALUES(IV+1,NSUB)=4.               ! VERTICAL SIGNIFICANCE
                  END IF
               END IF
               VALUES(IV+4,NSUB)=999999.                ! HEIGHT OF BASE OF CLOUD
               IF(KDEC(77+I*3).NE.MINDIC)
     *              VALUES(IV+4,NSUB)=FLOAT(KDEC(77+I*3))
               IV = IV + 4
            END IF
         END DO
C     If no cloud groups, 031001 should be set to 1 and all values to missing,
C     (except when we know there are no cloud groups, i.e. when N=0,
C     or when sky is obscured by fog and/or other meteorological phenomena,
C     but these cases has already been covered)
         IF (NCG.EQ.0) THEN
            IF(ICOVER.EQ.9) THEN
               IV = IV + 4      ! Values already initialized to missing
            END IF
         END IF
         VALUES(37,NSUB)=FLOAT(NCG + NCB) ! DELAYED DESCRIPTOR REPLICATION FACTOR
         IV = IV + 1
         IF (NCB.GT.1) THEN
            WRITE(*,*) 'WARNING: more than one cb cloud group!'
         END IF
      END IF
      
C     Section 4 N'C'H'H'Ct, not yet decoded (should be!)
      is4NCNNCt = 0
      VALUES(IV,NSUB)=FLOAT(is4NCNNCt) ! DELAYED DESCRIPTOR REPLICATION FACTOR
c$$$      VALUES(IV+1,NSUB)=999999.                         ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
c$$$      VALUES(IV+2,NSUB)=999999.                         ! CLOUD AMOUNT
c$$$      VALUES(IV+3,NSUB)=999999.                         ! CLOUD TYPE
c$$$      VALUES(IV+4,NSUB)=999999.                         ! HEIGHT OF TOP OF CLOUD
c$$$      VALUES(IV+5,NSUB)=999999.                         ! CLOUD TOP DESCRIPTION
c$$$      IV = IV + 6
      IV = IV + 1

C     333 56DLDMDH, not decoded, repeated 3 times
      is56DLDMDH = 0
      VALUES(IV,NSUB)=FLOAT(is56DLDMDH) ! SHORT DELAYED DESCRIPTOR REPLICATION FACTOR
      IV = IV + 1
c$$$      DO I=1,3
c$$$         VALUES(IV,NSUB)=999999.                        ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
c$$$         VALUES(IV+1,NSUB)=999999.                      ! TRUE DIRECTION FROM WHICH CLOUDS ARE MOVING
c$$$         IV = IV + 2
c$$$      END DO

C     Set to missing to cancel previous value
      VALUES(IV,NSUB)=999999.                           ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
      IV = IV + 1

C     333 57CDaec, not decoded
      is57CDaec = 0
      VALUES(IV,NSUB)=FLOAT(is57CDaec) ! SHORT DELAYED DESCRIPTOR REPLICATION FACTOR
      IV = IV + 1
c$$$      VALUES(IV,NSUB)=999999.                           ! BEARING OR AZIMUTH
c$$$      VALUES(IV+1,NSUB)=999999.                         ! ELEVATION (SEE NOTE 2)
c$$$      VALUES(IV+2,NSUB)=999999.                         ! CLOUD TYPE
c$$$      VALUES(IV+3,NSUB)=999999.                         ! BEARING OR AZIMUTH
c$$$      VALUES(IV+4,NSUB)=999999.                         ! ELEVATION (SEE NOTE 2)
c$$$      IV = IV + 5

      VALUES(IV,NSUB)=999999.                           ! STATE OF THE GROUND (WITH OR WITHOUT SNOW)
      IF(KDEC(97).NE.MINDIC) VALUES(IV,NSUB)=FLOAT(KDEC(97))
      IF(KDEC(98).NE.MINDIC) VALUES(IV,NSUB)=KDEC(98)+10.
      VALUES(IV+1,NSUB)=999999.                         ! TOTAL SNOW DEPTH
      IF(KDEC(99).NE.MINDIC) VALUES(IV+1,NSUB)=FLOAT(KDEC(99))/100.
      VALUES(IV+2,NSUB)=999999.                         ! GROUND MINIMUM TEMPERATURE, PAST 12 HOURS
      IF(KDEC(70).NE.MINDIC) VALUES(IV+2,NSUB)=KDEC(70)/10.+273.15
      IV = IV + 3

      isS = 0
      IF(KDEC(130).NE.MINDIC) isS = 1
      VALUES(IV,NSUB)=FLOAT(isS)  ! SHORT DELAYED DESCRIPTOR REPLICATION FACTOR
      IV = IV + 1
      IF (isS.GT.0) THEN
         VALUES(IV,NSUB)=999999.                        ! STATE OF THE SEA
         IF(KDEC(130).NE.MINDIC) VALUES(IV,NSUB)=FLOAT(KDEC(130))
         VALUES(IV+1,NSUB)=999999.                      ! VISIBILITY SEAWARDS FROM A COASTAL STATION
         IV = IV + 2
      END IF

      isTwTw = 0
      IF(KDEC(51).NE.MINDIC) isTwTw = 1
      VALUES(IV,NSUB)=FLOAT(isTwTw) ! SHORT DELAYED DESCRIPTOR REPLICATION FACTOR
      IV = IV + 1
      IF (isTwTw.GT.0) THEN
         VALUES(IV,NSUB)=999999.                        ! METHOD OF WATER TEMPERATURE AND/OR SALINITY MEASUREMENT
         IF(KDEC(119).NE.MINDIC) VALUES(IV,NSUB)=FLOAT(KDEC(119))
         VALUES(IV+1,NSUB)=999999.                      ! DEPTH BELOW SEA/WATER SURFACE
         VALUES(IV+2,NSUB)=999999.                      ! SEA/WATER TEMPERATURE
         IF(KDEC(51).NE.MINDIC) VALUES(IV+2,NSUB)=KDEC(51)/10.+273.15
C     Set to missing to cancel previous value
         VALUES(IV+3,NSUB)=999999.                      ! DEPTH BELOW SEA/WATER SURFACE
         IV = IV + 4
      END IF

      isIce = 0
      IF(KDEC(63).NE.MINDIC.OR.KDEC(64).NE.MINDIC.OR.
     +     KDEC(62).NE.MINDIC.OR.KDEC(65).NE.MINDIC.OR.
     +     KDEC(67).NE.MINDIC.OR.KDEC(69).NE.MINDIC.OR.
     +     KDEC(66).NE.MINDIC.OR.KDEC(68).NE.MINDIC) isIce = 1
      VALUES(IV,NSUB)=FLOAT(isIce) ! SHORT DELAYED DESCRIPTOR REPLICATION FACTOR
      IV = IV + 1
      IF (isIce.GT.0) THEN
         VALUES(IV,NSUB)=999999.                        ! ICE DEPOSIT (THICKNESS)
         IF(KDEC(63).NE.MINDIC) VALUES(IV,NSUB)=KDEC(63)/100.0
         VALUES(IV+1,NSUB)=999999.                      ! RATE OF ICE ACCRETION
         IF(KDEC(64).NE.MINDIC) THEN
            CALL IC3551(KDEC(64),IBCODE)
            VALUES(IV+1,NSUB)=FLOAT(IBCODE) 
         END IF
         VALUES(IV+2,NSUB)=999999.                      ! CAUSE OF ICE ACCRETION
         IF(KDEC(62).NE.MINDIC) THEN
            CALL IC1751(KDEC(62),IBCODE)
            VALUES(IV+2,NSUB)=FLOAT(IBCODE)
         END IF 
         VALUES(IV+3,NSUB)=999999.                      ! SEA ICE CONCENTRATION
         IF(KDEC(65).NE.MINDIC) THEN
            CALL IC639(KDEC(65),IBCODE)
            VALUES(IV+3,NSUB)=FLOAT(IBCODE)
         END IF 
         VALUES(IV+4,NSUB)=999999.                      ! AMOUNT AND TYPE OF ICE
         IF(KDEC(67).NE.MINDIC) VALUES(IV+4,NSUB)=FLOAT(KDEC(67)) 
         VALUES(IV+5,NSUB)=999999.                      ! ICE SITUATION
         IF(KDEC(69).NE.MINDIC) VALUES(IV+5,NSUB)=FLOAT(KDEC(69)) 
         VALUES(IV+6,NSUB)=999999.                      ! ICE DEVELOPMENT
         IF(KDEC(66).NE.MINDIC) VALUES(IV+6,NSUB)=FLOAT(KDEC(66)) 
         VALUES(IV+7,NSUB)=999999.                      ! BEARING OF ICE EDGE
         IF(KDEC(68).NE.MINDIC) VALUES(IV+7,NSUB)=FLOAT(KDEC(68)) 
         IV = IV + 8
      END IF
      
      VALUES(IV,NSUB)=999999.                           ! PRESENT WEATHER
      IF(KDEC(28).NE.MINDIC) VALUES(IV,NSUB)=FLOAT(KDEC(28))
      VALUES(IV+1,NSUB)=999999.                         ! TIME PERIOD OR DISPLACEMENT
      IF(MOD(KDEC(2),6).EQ.0) THEN
         VALUES(IV+1,NSUB)=-6.
      ELSE IF(MOD(KDEC(2),3).EQ.0) THEN
         VALUES(IV+1,NSUB)=-3.
      ELSE
         VALUES(IV+1,NSUB)=-1. ! Might in fact be -2 for stations sending every
                               ! two hours. But how to know that?
      END IF
      VALUES(IV+2,NSUB)=999999.                         ! PAST WEATHER (1)
      IF(KDEC(29).NE.MINDIC) VALUES(IV+2,NSUB)=FLOAT(KDEC(29))
      VALUES(IV+3,NSUB)=999999.                         ! PAST WEATHER (2)
      IF(KDEC(30).NE.MINDIC) VALUES(IV+3,NSUB)=FLOAT(KDEC(30))
      IV = IV + 4

      VALUES(IV,NSUB)=-1.                               ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+1,NSUB)=999999.                         ! TOTAL SUNSHINE (past hour)
                                                        ! not yet decoded
      VALUES(IV+2,NSUB)=-24.                            ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+3,NSUB)=999999.                         ! TOTAL SUNSHINE (past 24 hours)
      IF(KDEC(111).NE.MINDIC) VALUES(IV+3,NSUB)=FLOAT(KDEC(111))
      IV = IV + 4

      VALUES(IV,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(IV+1,NSUB)=999999.                         ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(41).NE.24.AND.KDEC(41).NE.MINDIC)
     *     VALUES(IV+1,NSUB)=-FLOAT(KDEC(41))           ! must be negative
      VALUES(IV+2,NSUB)=999999.                         ! TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT
      IF(KDEC(41).NE.24.AND.KDEC(40).NE.MINDIC)
     *     VALUES(IV+2,NSUB)=FLOAT(KDEC(40))/10.0
      VALUES(IV+3,NSUB)=999999.                         ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(103).NE.MINDIC)
     *     VALUES(IV+3,NSUB)=-FLOAT(KDEC(103))          ! must be negative
      VALUES(IV+4,NSUB)=999999.                         ! TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT
      IF(KDEC(102).NE.MINDIC) VALUES(IV+4,NSUB)=FLOAT(KDEC(102))/10.0
      IV = IV + 5

      VALUES(IV,NSUB)=2.                                ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(IV+1,NSUB)=-12.                            ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(72).NE.MINDIC) VALUES(IV+1,NSUB)=-FLOAT(KDEC(72))
      VALUES(IV+2,NSUB)=0.                              ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+3,NSUB)=999999.                         ! MAXIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED
      IF(KDEC(71).NE.MINDIC) VALUES(IV+3,NSUB)=KDEC(71)/10.+273.15
      VALUES(IV+4,NSUB)=-12.                            ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(74).NE.MINDIC) VALUES(IV+4,NSUB)=-FLOAT(KDEC(74))
      VALUES(IV+5,NSUB)=0.                              ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+6,NSUB)=999999.                         ! MINIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED
      IF(KDEC(73).NE.MINDIC) VALUES(IV+6,NSUB)=KDEC(73)/10.+273.15
      IV = IV + 7

      VALUES(IV,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(IV+1,NSUB)=999999.                         ! TYPE OF INSTRUMENTATION FOR WIND MEASUREMENT
      IF(KINT(3).NE.MINDIC) THEN
         CALL IC1855(KINT(3),IWCODE)
         VALUES(IV+1,NSUB)=IWCODE
      END IF
      VALUES(IV+2,NSUB)=2.                              ! TIME SIGNIFICANCE
                                                        ! 2 = Time averaged
      VALUES(IV+3,NSUB)=-10.                            ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+4,NSUB)=999999.                         ! WIND DIRECTION AT 10 M
      IF(KDEC(25).NE.MINDIC) VALUES(IV+4,NSUB)=FLOAT(KDEC(25))
      VALUES(IV+5,NSUB)=999999.                         ! WIND SPEED AT 10 M
      IF(KDEC(26).NE.MINDIC) VALUES(IV+5,NSUB)=FLOAT(KDEC(26))/10.
      VALUES(IV+6,NSUB)=999999.                         ! TIME SIGNIFICANCE
                                                        ! Cancel previous value
      IV = IV + 7

      VALUES(IV,NSUB)=-10.                              ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+1,NSUB)=999999.                         ! MAXIMUM WIND GUST DIRECTION
      VALUES(IV+2,NSUB)=999999.                         ! MAXIMUM WIND GUST SPEED
      IF(KDEC(138).NE.MINDIC) VALUES(IV+2,NSUB)=FLOAT(KDEC(138))/10. ! 333 910fmfm
      IV = IV + 3

      VALUES(IV,NSUB)=999999.                           ! TIME PERIOD OR DISPLACEMENT
      IF (KDEC(139).NE.MINDIC) THEN                     ! Should be tt in 907tt if present, else the same period as used for W1W2
         VALUES(IV,NSUB)=-FLOAT(KDEC(139))
      ELSE IF (MOD(KDEC(2),6).EQ.0) THEN
         VALUES(IV,NSUB)=-360.
      ELSE IF (MOD(KDEC(2),3).EQ.0) THEN
         VALUES(IV,NSUB)=-180.
      ELSE
         VALUES(IV,NSUB)=-60.  ! Might in fact be -120. for stations sending every
                               ! two hours. But how to know that?
      END IF
      VALUES(IV+1,NSUB)=999999.                         ! MAXIMUM WIND GUST DIRECTION
      VALUES(IV+2,NSUB)=999999.                         ! MAXIMUM WIND GUST SPEED
      IF(KDEC(137).NE.MINDIC) VALUES(IV+2,NSUB)=FLOAT(KDEC(137))/10. ! 333 911fxfx
      IV = IV + 3

      VALUES(IV,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(IV+1,NSUB)=-24.                            ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+2,NSUB)=999999.                         ! TYPE OF INSTRUMENTATION FOR EVAPORATION MEASUREMENT OR TYPE OF C
      VALUES(IV+3,NSUB)=999999.                         ! EVAPORATION/EVAPOTRANSPIRATION
      IF(KDEC(110).NE.MINDIC) VALUES(IV+3,NSUB)=KDEC(110)/10.0
      IV = IV + 4

C     333 55SSS 553SS
      is55SSS = 0
      IF (KDEC(112).NE.MINDIC) is55SSS = 1
      VALUES(IV,NSUB)=FLOAT(is55SSS) ! SHORT DELAYED DESCRIPTOR REPLICATION FACTOR
      IV = IV + 1
      IF (is55SSS.GT.0) THEN
C     No data for 1 hour radiation yet decoded. Note that when such data
C     starts to be decoded, is55SSS might obtain the value 2, and this
C     part needs some rewriting.
         
c$$$         VALUES(IV,NSUB)=-1.    ! TIME PERIOD OR DISPLACEMENT
c$$$         VALUES(IV+1,NSUB)=999999. ! LONG-WAVE RADIATION, INTEGRATED OVER PERIOD SPECIFIED           
c$$$         VALUES(IV+2,NSUB)=999999. ! SHORT-WAVE RADIATION, INTEGRATED OVER PERIOD SPECIFIED
c$$$         VALUES(IV+3,NSUB)=999999. ! NET RADIATION, INTEGRATED OVER PERIOD SPECIFIED
c$$$         VALUES(IV+4,NSUB)=999999. ! GLOBAL SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD S
c$$$         VALUES(IV+5,NSUB)=999999. ! DIFFUSE SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD
c$$$         VALUES(IV+6,NSUB)=999999. ! DIRECT SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD S
c$$$         IV = IV + 7

         VALUES(IV,NSUB)=-24.      ! TIME PERIOD OR DISPLACEMENT
         VALUES(IV+1,NSUB)=999999. ! LONG-WAVE RADIATION, INTEGRATED OVER PERIOD SPECIFIED           
         VALUES(IV+2,NSUB)=999999. ! SHORT-WAVE RADIATION, INTEGRATED OVER PERIOD SPECIFIED
         VALUES(IV+3,NSUB)=999999. ! NET RADIATION, INTEGRATED OVER PERIOD SPECIFIED
         IF(KDEC(112).NE.MINDIC) VALUES(IV+3,NSUB)=KDEC(112)*1000.0 ! kJ in TAC, J in BUFR
         VALUES(IV+4,NSUB)=999999. ! GLOBAL SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD S
         VALUES(IV+5,NSUB)=999999. ! DIFFUSE SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD
         VALUES(IV+6,NSUB)=999999. ! DIRECT SOLAR RADIATION (HIGH ACCURACY), INTEGRATED OVER PERIOD S
         IV = IV + 7
      END IF

C     333 54g0sndT, not decoded
      is54g0sndT = 0
      VALUES(IV,NSUB)=FLOAT(is54g0sndT) ! SHORT DELAYED DESCRIPTOR REPLICATION FACTOR
      IV = IV + 1
c$$$      VALUES(IV,NSUB)=-6.       ! TIME PERIOD OR DISPLACEMENT
c$$$                                ! The same period as W1W2. Is this always 6 hours?
c$$$      VALUES(IV+1,NSUB)=0.      ! TIME PERIOD OR DISPLACEMENT
c$$$      VALUES(IV+2,NSUB)=999999. ! TEMPERATURE CHANGE OVER SPECIFIED PERIOD
c$$$      IV = IV + 3

C     fxfx in 555 0Stzfxfx or ff in 333 912ff
      VALUES(IV,NSUB)=999999.                           ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(132).NE.MINDIC) THEN  ! Don't provide any period if fxfx is missing
         IF (KDEC(139).NE.MINDIC) THEN ! Should be tt in 907tt if present, else the same period as used for W1W2
            VALUES(IV,NSUB)=-FLOAT(KDEC(139))
         ELSE IF (MOD(KDEC(2),6).EQ.0) THEN ! Might in fact be -720 at 06 for stations not observing 00
                                            !(this is different than for W1W2). But how to know that?
            VALUES(IV,NSUB)=-360.
         ELSE IF (MOD(KDEC(2),3).EQ.0) THEN
            VALUES(IV,NSUB)=-180.
         ELSE
            VALUES(IV,NSUB)=-60. ! Might in fact be -120. for stations sending every
                                 ! two hours. But how to know that?
         END IF
         VALUES(IV+1,NSUB)=FLOAT(KDEC(132))/10.0 ! MAXIMUM WIND SPEED (10-MIN MEAN WIND)
      END IF
      IV = IV + 2

C     Update number of elements.
      M = IV - 1   

C     Set some section 1 information
C     ------------------------------

      NYEAR =IYEAR
      NMONTH=IMONTH
      NDAY  =KDEC(1)
      NHOUR =KDEC(2)
      NMINUTE=KDEC(9)
      NSECOND=0

C     Set kdata (delayed replication factors)
C     ---------------------------------------
      KDATA(1) = VALUES(37,NSUB) ! Number of cloud groups
      KDATA(2) = is4NCNNCt   ! Number of cloud groups with bases below station level
      KDATA(3) = is56DLDMDH  ! Occurrence of 333 56DLDMDH
      KDATA(4) = is57CDaec   ! Occurrence of 333 57CDaec
      KDATA(5) = isS         ! Occurrence of S (state of sea)
      KDATA(6) = isTwTw      ! Occurrence of 0ssTwTwTw
      KDATA(7) = isIce       ! Occurrence of 222 6IsEsEsRs ciSibiDizi
      KDATA(8) = is55SSS     ! Occurrence of 333 55SSS or 333 553SS
      KDATA(9) = is54g0sndT  ! Occurrence of 333 54g0sndT

      RETURN
      END

      SUBROUTINE SYNEXP_SEA(IERR )
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR SHIP DATA.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SYNEXP_SEA(IERR)*
C
C     METHOD.
C     -------
C
C          Modified from synexp_land
C
C     AUTHOR.
C     -------
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C

      implicit none
      integer ierr
      integer i,j,imonth,iyear,id,im,iy,icover,iv,ibcode,iwcode
      integer NCG          ! Number of cloud groups
      integer CL,CM,CH

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'    ! nsub
      INCLUDE 'comwtc.f'   ! cval
      INCLUDE 'comkey.f'
      INCLUDE 'comrepl.f'  ! kdata

      NSUB = 1

      DO I=1,JP22
         DO J=1,2
            VALUES(I,J)=999999.
         END DO
      END DO

C     Get current date (year and month)
      CALL DATUM(KDEC(1),IMONTH,IYEAR)

      VALUES( 1,NSUB)=1000*NSUB+9
      CVAL(1)= CHAR(KINT(09))//CHAR(KINT(10))//
     *     CHAR(KINT(11))//
cpsjan24     *     CHAR(KINT(12))//CHAR(KINT(13))//'    '
     *     CHAR(KINT(12))//CHAR(KINT(13))//
     *     CHAR(KINT(109))//CHAR(KINT(110))//
     *     CHAR(KINT(111))//CHAR(KINT(112))

      VALUES( 2,NSUB)=999999.                           ! DIRECTION OF MOTION OF MOVING OBSERVING PLATFORM
      IF(KDEC(49).NE.MINDIC) VALUES(2,NSUB)=FLOAT(KDEC(49))
      VALUES( 3,NSUB)=999999.                           ! SPEED OF MOTION OF MOVING OBSERVING PLATFORM
      IF(KDEC(50).NE.MINDIC) VALUES(3,NSUB)=FLOAT(KDEC(50))
      VALUES( 4,NSUB)=999999.                           ! TYPE OF STATION (manned)
      IF(KDEC(4).GE.21.OR.KDEC(4).LE.23) VALUES(4,NSUB)=1. ! Manned
      ! (although I am not sure if this can be concluded for kdec(4)=22,23 - just
      ! copy code from nix_syno.F)
      IF(KDEC(4).EQ.24) VALUES(4,NSUB)=0. ! Automatic
      VALUES( 5,NSUB)=FLOAT(IYEAR)                      ! YEAR
      VALUES( 6,NSUB)=FLOAT(IMONTH)                     ! MONTH
      VALUES( 7,NSUB)=FLOAT(KDEC(1))                    ! DAY
      VALUES( 8,NSUB)=FLOAT(KDEC(2))                    ! HOUR
      VALUES( 9,NSUB)=FLOAT(KDEC(9))                    ! MINUTE
C     If 9GGgg is present in section 1 in synop: overwrite hour and 
C     minute (observation time) with GG = kint(48) and gg = kint(49)
      IF(KINT(47).EQ.9) THEN
         IF(KINT(48).NE.MINDIC) THEN
            VALUES( 8,NSUB)=FLOAT(KINT(48))
         ENDIF
         IF(KINT(49).NE.MINDIC) THEN
            VALUES( 9,NSUB)=FLOAT(KINT(49))
         ENDIF
C     If termin=kdec(2)=00 and GG in 9GGgg is 23: then previous day!
         IF(KDEC(2).EQ.0 .AND. KINT(48).EQ.23) THEN
            CALL DECR_DATE(KDEC(1),IMONTH,IYEAR,ID,IM,IY)
            VALUES( 5,NSUB)=FLOAT(IY)
            VALUES( 6,NSUB)=FLOAT(IM)
            VALUES( 7,NSUB)=FLOAT(ID)
         ENDIF
      ENDIF
      VALUES(10,NSUB)=KDEC(5)/100.                      ! LATITUDE (HIGH ACCURACY)
      VALUES(11,NSUB)=KDEC(6)/100.                      ! LONGITUDE (HIGH ACCURACY)
      VALUES(12,NSUB)=999999.                           ! HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL
      VALUES(13,NSUB)=999999.                           ! HEIGHT OF BAROMETER ABOVE MEAN SEA LEVEL
      VALUES(14,NSUB)=999999.                           ! PRESSURE
      IF(KDEC(34).NE.MINDIC) VALUES(14,NSUB)=KDEC(34)*10.
      VALUES(15,NSUB)=999999.                           ! PRESSURE  REDUCED TO MEAN SEA LEVEL
      IF(KDEC(35).NE.MINDIC) VALUES(15,NSUB)=KDEC(35)*10.
      VALUES(16,NSUB)=999999.                           ! 3 HOUR PRESSURE CHANGE
      IF(KDEC(39).NE.MINDIC) VALUES(16,NSUB)=KDEC(39)*10.
      VALUES(17,NSUB)=999999.                           ! CHARACTERISTIC OF PRESSURE TENDENCY
      VALUES(17,NSUB)=KDEC(38)
ccc       IF(KDEC(38).GE.9) VALUES(17,NSUB)=15.
      IF(KDEC(38).GE.9) VALUES(17,NSUB)=999999.
      VALUES(18,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(19,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE WATER SURFACE
      VALUES(20,NSUB)=999999.                           ! TEMPERATURE/DRY-BULB TEMPERATURE
      IF(KDEC(31).NE.MINDIC) VALUES(20,NSUB)=KDEC(31)/10.+273.15
      VALUES(21,NSUB)=999999.                           ! METHOD OF WET-BULB TEMPERATURE MEASUREMENT
      IF(KDEC(121).NE.MINDIC) VALUES(21,NSUB)=FLOAT(KDEC(121))
      VALUES(22,NSUB)=999999.                           ! WET-BULB TEMPERATURE
      IF(KDEC(120).NE.MINDIC) VALUES(22,NSUB)=KDEC(120)/10.+273.15
      VALUES(23,NSUB)=999999.                           ! DEW POINT TEMPERATURE AT 2M
      IF(KDEC(32).NE.MINDIC) VALUES(23,NSUB)=KDEC(32)/10.+273.15
      VALUES(24,NSUB)=999999.                           ! RELATIVE HUMIDITY
      IF(KDEC(33).NE.MINDIC) VALUES(24,NSUB)=FLOAT(KDEC(33))
      VALUES(25,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(26,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE WATER SURFACE
      VALUES(27,NSUB)=999999.                           ! HORIZONTAL VISIBILITY
      IF(KDEC(27).NE.MINDIC) VALUES(27,NSUB)=FLOAT(KDEC(27))
      VALUES(28,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE WATER SURFACE
      VALUES(29,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
                                                        ! Set to missing to cancel previous value
      VALUES(30,NSUB)=999999.                           ! TOTAL PRECIPITATION PAST 24 HOURS
      IF(KDEC(101).NE.MINDIC) THEN
         VALUES(30,NSUB)=FLOAT(KDEC(101))/10.
      ELSEIF(KDEC(41).EQ.24.AND.KDEC(40).NE.MINDIC) THEN
         VALUES(30,NSUB)=FLOAT(KDEC(40))/10.
      ENDIF
      VALUES(31,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(32,NSUB)=999999.                           ! CLOUD COVER (TOTAL)
      CALL IC2700(KDEC(42),ICOVER)
      VALUES(32,NSUB)=FLOAT(ICOVER)
      VALUES(33,NSUB)=63.                               ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
                                                        ! Set to missing initially, adjusted later
      VALUES(34,NSUB)=999999.                           ! CLOUD AMOUNT
      IF(KDEC(44).NE.MINDIC) VALUES(34,NSUB)=FLOAT(KDEC(44))
      VALUES(35,NSUB)=999999.                           ! HEIGHT OF BASE OF CLOUD
      IF(KDEC(43).NE.MINDIC.AND.KDEC(43).NE.16382
     +        .AND.KDEC(43).NE.16381) THEN
         VALUES(35,NSUB)=FLOAT(KDEC(43))
      ENDIF
      VALUES(36,NSUB)=999999.                           ! CLOUD TYPE
      IF(KDEC(45).NE.MINDIC) THEN
	 IF(KDEC(45).NE.14) THEN
            VALUES(36,NSUB)=FLOAT(KDEC(45))+30.
         ELSE
            VALUES(36,NSUB)=62.
         END IF
      END IF
      VALUES(37,NSUB)=999999.                           ! CLOUD TYPE
      IF(KDEC(46).NE.MINDIC) THEN
	 IF(KDEC(46).NE.14) THEN
            VALUES(37,NSUB)=FLOAT(KDEC(46))+20.
         ELSE
            VALUES(37,NSUB)=61.
         END IF
      END IF
      VALUES(38,NSUB)=999999.                           ! CLOUD TYPE
      IF(KDEC(47).NE.MINDIC) THEN
	 IF(KDEC(47).NE.14)THEN
            VALUES(38,NSUB)=FLOAT(KDEC(47))+10.
         ELSE
            VALUES(38,NSUB)=60.
         END IF
      END IF

C     Vertical significance (for low and middle clouds)
      CL = NINT(VALUES(36,NSUB))
      CM = NINT(VALUES(37,NSUB))
      CH = NINT(VALUES(38,NSUB))
      IF(CL.NE.999999 .AND. CL.NE.62) THEN
         VALUES(33,NSUB)=7.     ! Low cloud
      ELSE IF(CM.NE.999999 .AND. CM.NE.61) THEN
         VALUES(33,NSUB)=8.     ! Middle cloud
      ELSE IF(CH.NE.999999 .AND. CH.NE.60) THEN
         VALUES(33,NSUB)=0.     ! Observing rules for base of lowest cloud and
                                ! cloud types of FM 12-IX EXT. SYNOP and FM 13-IX
                                ! EXT. ship apply
      ELSE IF(CL.EQ.62 .AND. CM.EQ.61 .AND. CH.EQ.60) THEN
         VALUES(33,NSUB)=5.     ! Ceiling
      ELSE IF(ICOVER.eq.0) THEN ! Sky clear
         VALUES(33,NSUB)=62.    ! Value not applicable
      END IF
      
      NCG = 0  ! Number of cloud groups, to be found
      IV = 39  ! Index for values array
      DO I=0,3
         IF(KDEC(75+I*3).NE.MINDIC.OR.KDEC(76+I*3).NE.MINDIC
     *     .OR.KDEC(77+I*3).NE.MINDIC) THEN
            NCG = NCG + 1
            VALUES(IV+1,NSUB)=FLOAT(I)                  ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
            VALUES(IV+2,NSUB)=999999.                   ! CLOUD AMOUNT
            IF(KDEC(75+I*3).NE.MINDIC) 
     *           VALUES(IV+2,NSUB)=FLOAT(KDEC(75+I*3))
            VALUES(IV+3,NSUB)=999999.                   ! CLOUD TYPE 
            IF(KDEC(76+I*3).NE.MINDIC)
     *           VALUES(IV+3,NSUB)=FLOAT(KDEC(76+I*3))
            VALUES(IV+4,NSUB)=999999.                   ! HEIGHT OF BASE OF CLOUD
            IF(KDEC(77+I*3).NE.MINDIC)
     *           VALUES(IV+4,NSUB)=FLOAT(KDEC(77+I*3))
            IV = IV + 4
         END IF
      END DO
C     If no cloud groups, 031001 should be set to 1 and all values to missing,
C     except when we know there are no cloud groups, i.e. when N=0
      IF (NCG.EQ.0 .AND. ICOVER.NE.0) THEN
         NCG = 1
         IV = IV + 4  ! Values already initialized to missing
      END IF
      VALUES(39,NSUB)=FLOAT(NCG)                        ! DELAYED DESCRIPTOR REPLICATION FACTOR
      VALUES(IV+1,NSUB)=999999.                         ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
                                                        ! Cancel previous value
      IV = IV + 2

      VALUES(IV,NSUB)=999999.                           ! ICE DEPOSIT (THICKNESS)
      IF(KDEC(63).NE.MINDIC) VALUES(IV,NSUB)=KDEC(63)/100.
      VALUES(IV+1,NSUB)=999999.                         ! RATE OF ICE ACCRETION
      IF(KDEC(64).NE.MINDIC) THEN
         CALL IC3551(KDEC(64),IBCODE)
         VALUES(IV+1,NSUB)=FLOAT(IBCODE) 
      END IF
      VALUES(IV+2,NSUB)=999999.                         ! CAUSE OF ICE ACCRETION
      IF(KDEC(62).NE.MINDIC) THEN
         CALL IC1751(KDEC(62),IBCODE)
         VALUES(IV+2,NSUB)=FLOAT(IBCODE)
      END IF 
      VALUES(IV+3,NSUB)=999999.                         ! SEA ICE CONCENTRATION
      IF(KDEC(65).NE.MINDIC) THEN
         CALL IC639(KDEC(65),IBCODE)
         VALUES(IV+3,NSUB)=FLOAT(IBCODE)
      END IF 
      VALUES(IV+4,NSUB)=999999.                         ! AMOUNT AND TYPE OF ICE
      IF(KDEC(67).NE.MINDIC) VALUES(IV+4,NSUB)=FLOAT(KDEC(67)) 
      VALUES(IV+5,NSUB)=999999.                         ! ICE SITUATION
      IF(KDEC(69).NE.MINDIC) VALUES(IV+5,NSUB)=FLOAT(KDEC(69)) 
      VALUES(IV+6,NSUB)=999999.                         ! ICE DEVELOPMENT
      IF(KDEC(66).NE.MINDIC) VALUES(IV+6,NSUB)=FLOAT(KDEC(66)) 
      VALUES(IV+7,NSUB)=999999.                         ! BEARING OF ICE EDGE
      IF(KDEC(68).NE.MINDIC) VALUES(IV+7,NSUB)=FLOAT(KDEC(68)) 
      IV = IV + 8

      VALUES(IV,NSUB)=999999.                           ! METHOD OF WATER TEMPERATURE AND/OR SALINITY MEASUREMENT
      IF(KDEC(119).NE.MINDIC) VALUES(IV,NSUB)=FLOAT(KDEC(119))
      VALUES(IV+1,NSUB)=999999.                         ! DEPTH BELOW SEA/WATER SURFACE
      VALUES(IV+2,NSUB)=999999.                         ! SEA/WATER TEMPERATURE
      IF(KDEC(51).NE.MINDIC) VALUES(IV+2,NSUB)=KDEC(51)/10.+273.15
      VALUES(IV+3,NSUB)=999999.                         ! DEPTH BELOW SEA/WATER SURFACE
      IV = IV + 4

      VALUES(IV,NSUB)=999999.                           ! DIRECTION OF WAVES
      VALUES(IV+1,NSUB)=999999.                         ! PERIOD OF WAVES
      IF(KDEC(52).NE.MINDIC) VALUES(IV+1,NSUB)=FLOAT(KDEC(52)) 
      VALUES(IV+2,NSUB)=999999.                         ! HEIGHT OF WAVES
      IF(KDEC(53).NE.MINDIC) VALUES(IV+2,NSUB)=KDEC(53)/10. 
      VALUES(IV+3,NSUB)=999999.                         ! DIRECTION OF WIND WAVES
      VALUES(IV+4,NSUB)=999999.                         ! PERIOD OF WIND WAVES
      IF(KDEC(54).NE.MINDIC) VALUES(IV+4,NSUB)=FLOAT(KDEC(54)) 
      VALUES(IV+5,NSUB)=999999.                         ! HEIGHT OF WIND WAVES
      IF(KDEC(55).NE.MINDIC) VALUES(IV+5,NSUB)=KDEC(55)/10. 
      VALUES(IV+6,NSUB)=999999.                         ! DIRECTION OF SWELL WAVES
      IF(KDEC(56).NE.MINDIC) VALUES(IV+6,NSUB)=FLOAT(KDEC(56)) 
      VALUES(IV+7,NSUB)=999999.                         ! PERIOD OF SWELL WAVES
      IF(KDEC(58).NE.MINDIC) VALUES(IV+7,NSUB)=FLOAT(KDEC(58)) 
      VALUES(IV+8,NSUB)=999999.                         ! HEIGHT OF SWELL WAVES
      IF(KDEC(59).NE.MINDIC) VALUES(IV+8,NSUB)=KDEC(59)/10. 
      VALUES(IV+9,NSUB)=999999.                         ! DIRECTION OF SWELL WAVES
      IF(KDEC(57).NE.MINDIC) VALUES(IV+9,NSUB)=FLOAT(KDEC(57)) 
      VALUES(IV+10,NSUB)=999999.                        ! PERIOD OF SWELL WAVES
      IF(KDEC(60).NE.MINDIC) VALUES(IV+10,NSUB)=FLOAT(KDEC(60)) 
      VALUES(IV+11,NSUB)=999999.                        ! HEIGHT OF SWELL WAVES
      IF(KDEC(61).NE.MINDIC) VALUES(IV+11,NSUB)=KDEC(61)/10. 
      IV = IV + 12

      VALUES(IV,NSUB)=999999.                           ! PRESENT WEATHER
      IF(KDEC(28).NE.MINDIC) VALUES(IV,NSUB)=FLOAT(KDEC(28))
      VALUES(IV+1,NSUB)=999999.                         ! TIME PERIOD OR DISPLACEMENT
      IF(MOD(KDEC(2),6).EQ.0) THEN
         VALUES(IV+1,NSUB)=-6.
      ELSE IF(MOD(KDEC(2),3).EQ.0) THEN
         VALUES(IV+1,NSUB)=-3.
      ELSE
         VALUES(IV+1,NSUB)=-1. ! Might in fact be -2 for stations sending every
                               ! two hours. But how to know that?
      END IF
      VALUES(IV+2,NSUB)=999999.                         ! PAST WEATHER (1)
      IF(KDEC(29).NE.MINDIC) VALUES(IV+2,NSUB)=FLOAT(KDEC(29))
      VALUES(IV+3,NSUB)=999999.                         ! PAST WEATHER (2)
      IF(KDEC(30).NE.MINDIC) VALUES(IV+3,NSUB)=FLOAT(KDEC(30))
      IV = IV + 4

      VALUES(IV,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(IV+1,NSUB)=999999.                         ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(41).NE.24.AND.KDEC(41).NE.MINDIC)
     *     VALUES(IV+1,NSUB)=-FLOAT(KDEC(41))           ! must be negative
      VALUES(IV+2,NSUB)=999999.                         ! TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT
      IF(KDEC(41).NE.24.AND.KDEC(40).NE.MINDIC)
     *     VALUES(IV+2,NSUB)=FLOAT(KDEC(40))/10.0
      VALUES(IV+3,NSUB)=999999.                         ! TIME PERIOD OR DISPLACEMENT
                                                        ! not yet decoded
      VALUES(IV+4,NSUB)=999999.                         ! TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT
                                                        ! not yet decoded
      IV = IV + 5

      VALUES(IV,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(IV+1,NSUB)=999999.                         ! HEIGHT OF SENSOR ABOVE WATER SURFACE
      VALUES(IV+2,NSUB)=-12.                            ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(72).NE.MINDIC) VALUES(IV+2,NSUB)=FLOAT(KDEC(72))
      VALUES(IV+3,NSUB)=0.                              ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+4,NSUB)=999999.                         ! MAXIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED
      IF(KDEC(71).NE.MINDIC) VALUES(IV+4,NSUB)=KDEC(71)/10.+273.15
      VALUES(IV+5,NSUB)=-12.                            ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(74).NE.MINDIC) VALUES(IV+5,NSUB)=FLOAT(KDEC(74))
      VALUES(IV+6,NSUB)=0.                              ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+7,NSUB)=999999.                         ! MINIMUM TEMPERATURE, AT HEIGHT AND OVER PERIOD SPECIFIED
      IF(KDEC(73).NE.MINDIC) VALUES(IV+7,NSUB)=KDEC(73)/10.+273.15
      IV = IV + 8

      VALUES(IV,NSUB)=999999.                           ! HEIGHT OF SENSOR ABOVE LOCAL GROUND (OR DECK OF MARINE PLATFORM)
      VALUES(IV+1,NSUB)=999999.                         ! HEIGHT OF SENSOR ABOVE WATER SURFACE
      VALUES(IV+2,NSUB)=999999.                         ! TYPE OF INSTRUMENTATION FOR WIND MEASUREMENT
      IF(KINT(3).NE.MINDIC) THEN
         CALL IC1855(KINT(3),IWCODE)
         VALUES(IV+2,NSUB)=IWCODE
      END IF
      VALUES(IV+3,NSUB)=2.                              ! TIME SIGNIFICANCE
                                                        ! 2 = Time averaged
      VALUES(IV+4,NSUB)=-10.                            ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+5,NSUB)=999999.                         ! WIND DIRECTION AT 10 M
      IF(KDEC(25).NE.MINDIC) VALUES(IV+5,NSUB)=FLOAT(KDEC(25))
      VALUES(IV+6,NSUB)=999999.                         ! WIND SPEED AT 10 M
      IF(KDEC(26).NE.MINDIC) VALUES(IV+6,NSUB)=FLOAT(KDEC(26))/10.
      VALUES(IV+7,NSUB)=999999.                         ! TIME SIGNIFICANCE
                                                        ! Cancel previous value
      IV = IV + 8

      VALUES(IV,NSUB)=-10.                              ! TIME PERIOD OR DISPLACEMENT
      VALUES(IV+1,NSUB)=999999.                         ! MAXIMUM WIND GUST DIRECTION
      VALUES(IV+2,NSUB)=999999.                         ! MAXIMUM WIND GUST SPEED
      IF(KDEC(138).NE.MINDIC) VALUES(IV+2,NSUB)=FLOAT(KDEC(138))/10.! 333 910fmfm
      IV = IV + 3

      VALUES(IV,NSUB)=999999.                           ! TIME PERIOD OR DISPLACEMENT
      IF (KDEC(139).NE.MINDIC) THEN                     ! Should be tt in 907tt if present, else the same period as used for W1W2
         VALUES(IV,NSUB)=-FLOAT(KDEC(139))
      ELSE IF (MOD(KDEC(2),6).EQ.0) THEN
         VALUES(IV,NSUB)=-360.
      ELSE IF (MOD(KDEC(2),3).EQ.0) THEN
         VALUES(IV,NSUB)=-180.
      ELSE
         VALUES(IV,NSUB)=-60.  ! Might in fact be -120. for stations sending every
                               ! two hours. But how to know that?
      END IF
      VALUES(IV+1,NSUB)=999999.                         ! MAXIMUM WIND GUST DIRECTION
      VALUES(IV+2,NSUB)=999999.                         ! MAXIMUM WIND GUST SPEED
      IF(KDEC(137).NE.MINDIC) VALUES(IV+2,NSUB)=FLOAT(KDEC(137))/10. ! 333 911fxfx
      IV = IV + 3

C     fxfx in 555 0Stzfxfx or ff in 333 912ff
      VALUES(IV,NSUB)=999999.                           ! TIME PERIOD OR DISPLACEMENT
      IF(KDEC(132).NE.MINDIC) THEN  ! Don't provide any period if fxfx is missing
         IF (KDEC(139).NE.MINDIC) THEN ! Should be tt in 907tt if present, else the same period as used for W1W2
            VALUES(IV,NSUB)=-FLOAT(KDEC(139))
         ELSE IF (MOD(KDEC(2),6).EQ.0) THEN
            VALUES(IV,NSUB)=-360.
         ELSE IF (MOD(KDEC(2),3).EQ.0) THEN
            VALUES(IV,NSUB)=-180.
         ELSE
            VALUES(IV,NSUB)=-60. ! Might in fact be -120. for stations sending every
                                 ! two hours. But how to know that?
         END IF
         VALUES(IV+1,NSUB)=FLOAT(KDEC(132))/10.0 ! MAXIMUM WIND SPEED (10-MIN MEAN WIND)
      END IF
      IV = IV + 2

C     Update number of elements.
      M = IV - 1   

C     Set some information used in section 1
C     --------------------------------------

      NYEAR =IYEAR
      NMONTH=IMONTH
      NDAY  =KDEC(1)
      NHOUR =KDEC(2)
      NMINUTE=KDEC(9)
      NSECOND=0

C     Set kdata (delayed replication factors)
C     ---------------------------------------
      KDATA(1) = NCG ! Number of cloud groups

      RETURN
      END

      subroutine bufr_encode(subtype)
C     Purpose: Encode BUFR message
      implicit none
      integer subtype,ierr,i1,iiyear

      include 'parameter.f'  ! using jp1,jp2,jbufl
      include 'comwork.f'    ! kdec (and to be able to call prtkint/kdec/values)
      include 'comwt.f'      ! values

      include 'comwtc.f'     ! cval,cunit
      include 'combufren.f'  ! kbufl,kbuff
      include 'comdescr.f'   ! ktdlen,ktdlst
      include 'comrepl.f'    ! kdata,kdlen     kdata is set in SYNEXP_LAND/SEA
      include 'comkey.f'     ! nyear,nmonth,nday,nhour,nminute

      real*8 new_values(jp22),rvind
      parameter (rvind=1.7E38)

      character*80 cvals(jp22)
      integer kelem,kvals
      integer ksec0(3),ksec1(40),ksec2(64),ksec3(4),ksec4(2)

C     Used by BUXDES
      integer ktdexl
      integer ktdexp(200)
      character*64 cnames(jp22)
      character*24 cunits(jp22)

C     For debugging
c$$$      CALL PRTKINT(KINT,1,250,MINDIC)
c$$$      CALL PRTKDEC(KDEC,1,200,MINDIC)
c$$$      call prtvalues(120)

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

C     ktdlen and ktdlst were initialized in subroutine set_descriptors
C     kdata was initialized in subroutine SYNEXP_LAND/SEA

      kelem = jp22
      kvals = jp22
      kdlen = 100    ! Dimension of kdata array

      ksec0(1) = 0   ! LENGTH OF SECTION 0, not used by bufren
      ksec0(2) = 0   ! TOTAL LENGTH OF BUFR MESSAGE, not used by bufren
      ksec0(3) = 4   ! BUFR EDITION NUMBER

      ksec1(1) = 22       ! LENGTH OF SECTION 1, must be set by user
      ksec1(2) = 4        ! BUFR EDITION NUMBER
      ksec1(3) = 88       ! ORIGINATING CENTRE    (98 = ECMWF, 88 = Oslo)
                          ! Determines which tables to use
      ksec1(4) = kdec(21) ! UPDATE SEQUENCE NUMBER
CC      ksec1(5) = 128 ! FLAG (PRESENCE OF SECTION 2)
      ksec1(5) = 0        ! FLAG (PRESENCE OF SECTION 2)

      if (subtype.eq.11 .or. subtype.eq.14) then
         ksec1(6) = 0           ! BUFR MESSAGE TYPE, synop land
      else if (subtype.ge.21 .or. subtype.le.24) then
         ksec1(6) = 1           ! BUFR MESSAGE TYPE, synop ship
      else
         print*,'ERROR in bufr_encode: unknown subtype ',subtype
         call exit(2)
      end if

      ksec1(7) = 0   ! BUFR MESSAGE SUBTYPE
      ksec1(8) = 0   ! VERSION NUMBER OF LOCAL TABLE
      ksec1(9) = nyear
      ksec1(10) = nmonth
      ksec1(11) = nday
      ksec1(12) = nhour
      ksec1(13) = nminute
      ksec1(14) = 0  ! BUFR MASTER TABLE
      ksec1(15) = 14 ! VERSION NUMBER OF MASTER TABLE
      ksec1(16) = 0  ! ORIGINATING SUB-CENTRE
      if (ksec1(6).eq.0 .and. mod(nhour,6).eq.0) then
         ksec1(17) = 2 ! INTERNATIONAL DATA SUBCATEGORY (2 for ISM)
      else if (ksec1(6).eq.0 .and. mod(nhour,3).eq.0) then
         ksec1(17) = 1 ! (ISI)
      else
         ksec1(17) = 0 ! (ISN, ISS)
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
c$$$       call BUXDES(1,ksec1,ktdlen,ktdlst,kdlen,kdata,kelem,
c$$$     +     ktdexl,ktdexp,cnames,cunits,kerr)

      call bufren(ksec0,ksec1,ksec2,ksec3,ksec4,
     +     ktdlen,ktdlst,kdlen,kdata,kelem,kvals,
     +     new_values,cvals,kbufl,kbuff,ierr)
      if (ierr.ne.0) then
         write(*,*) 'Error in BUFREN: ierr=',ierr
         call exit(2)
      end if

      call writebufr

      return
      end subroutine bufr_encode
