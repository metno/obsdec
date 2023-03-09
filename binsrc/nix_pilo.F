       PROGRAM PILPROC
C
C**** *PILPROC*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING
C         PILOT DATA.
C
C
C**   INTERFACE.
C     ----------
C
c              msysdsi        YMSYS(1:ndx)//CDATFILE(msys)//'dataXXxx.dat' (usually msysdsi=4, opened in msysinit.f)
c              4              YPPDAT(1:ndx)//'station_amend.dat'
c              7              cstream(msys)//'/'//'pilo'//YEAR//YMONTH//YDAY//'MERGE'
c              8              cstream(msys)//'/'//'pilo'//YEAR//YMONTH//YDAY//'MERGE.index'
c              8              FILE=YPPDAT(1:ndx)//'std_atm.dat'
c              10             cstream(msys)//'/'//'pilo_error_file.dat'
c              17             cfstamp(msys)//'.timestamp.dat' (opened in msysinit.f)
c              55             cstream(msys)//'/'//'unknown_pilo_station.dat'
c              77             dir.dat
c              88             YPPDAT(1:ndx)//'ship_height.dat'
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
C        *CALL* *PROCRFB( IERR )*
C        *CALL* *PROCHDR( IERR )*
C        *CALL* *PROCTXT( IERR )*
C        *CALL* *PROCT1U( IERR )*
C        *call* *bswap4* (libmi)  
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comrec.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
      INCLUDE 'commdb.f'
C
      CHARACTER dir_file*80
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
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
      if(narg.lt.1.or.ctype.eq.'?') then
             print*,''
             print*,' Usage: pilo dir_file cmsys timestamp <cmsystop>'
             print*,'                                      (optional)'
             print*,' where  dir_file  - specification of directories'
             print*,'        cmsys - msys file number (=14 for pilo)'
cpsokt98             print*,'        ctstamp   - mmddhhmm of msys file '
             print*,'        ctstamp   - yyyymmddhhmm of msys file '
             print*,'        cmsystop - no. of records to read '
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
     1 PRINT*,' PILPROC - after getnext, ierr ',IERR
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
      CALL PROCRFB( IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' PILPROC - after PROCRFB, IERR= ',IERR
CTEST
      IF(IERR.NE.0) GO TO 200
C     ------------------------------------------------------------------
C*          4.   DECODE BULLETIN HEADER.
C                -----------------------
 400  CONTINUE
C
      CALL PROCHDR( IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' PILPROC - after PROCHDR, KERR= ',KERR
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
     1 PRINT*,' PILPROC - after PROCTXT, KERR= ',KERR
CTEST
      IF(KERR.NE.0) GO TO 200 
C
C     ------------------------------------------------------------------
C*          6.   CALL APPROPRIATE ROUTINE TO DECODE TEMP DATA.
C                ---------------------------------------------
 600  CONTINUE
C
      CALL PROCT1U( IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' PILPROC - after PROCT1U, IERR= ',IERR
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
      print*,' Open error on PILO.stop.dat'
      go to 1000
c
 1002 continue
c
      print*,' Read error on PILO.stop.dat'
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
C
      ntesttot=0
      do 1010 i=1,200
 1010    ntesttot=ntesttot+ntest(i)   
C
      print*,'number of reports written ',ntesttot
cps      print*,'number of duplicates ',ndup
      print*,' ***n.b.****** PUTBUFR : No testing of duplicates '
C
 1020 continue
      END
      SUBROUTINE PROCRFB ( IERR )
C**** *PROCRFB*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO FORMAT BULLETIN.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCRFB(IERR)*
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
C          J. HENNESSY         *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
C     ------------------------------------------------------------------
C*          1.   KEEP SOURCE OF DATA AND DATE/TIME OF ARRIVAL.
C                ---------------------------------------------
 100  CONTINUE
C
      DO 101 I=1,3
C
      KHEAD(I)=MINDIC      ! THIS WILL BE CHANGED ACCORDING TO THE KEY.
C
 101  CONTINUE
C     ------------------------------------------------------------------
C*          2.  DEFINE T1 AND T2 FROM ABBREVIATED HEADING.
C               ------------------------------------------
 200  CONTINUE

      DO 202 I=1,ILEN
C
      IF(KCHAR(I).GE.65.AND.KCHAR(I).LE.90) GO TO 203
C
 202  CONTINUE
C
      IT1=27
      GO TO 210
C
 203  CONTINUE
C
      IT1=KCHAR(I  )-64
      IT2=KCHAR(I+1)-64
C
      IF(IT1.NE.21) THEN
                       IERR=1
                       IT1=27
                       RETURN
                    END IF
C
C     CHECK IF 'T2' CHARACTER IS LETTER.
C
      IF(IT2.LT.0.OR.IT2.GT.26) IT1=27
C
C*          2.1  LAST CHARACTER OF BULLETIN CAN BE IN ANY OF THE LAST
C                -----------------------------------------------------
C                5 WORDS. IF CHARACTER  IS 'ETX' REPLACE BY 'GS' .
C                -------------------------------------------------
C                IF NEITHER CAN BE FOUND INSERT 'GS' AS LAST CHARACTER.
C                ------------------------------------------------------
 210  CONTINUE
C
      IST=ILEN-5
C
      DO 211 I=1,ILEN
C
      IF (KCHAR(I).EQ.3.OR.KCHAR(I).EQ.29) THEN
                                              KCHAR(I)= 29
                                              IGS=I
                                              RETURN
                                           END IF
C
 211  CONTINUE
C
      I=I-1
      KCHAR(I)= 29
      IGS=I
C
      RETURN
      END
      SUBROUTINE PROCTXT ( IERR )
C
C**** *PROCTXT*
C
C
C     PURPOSE.
C     --------
C          CHECKS WHETHER BULLETIN CONTAINS USEFUL DATA .
C          THE FOLLOWING BULLETINS ARE CONSIDERED TO CONTAIN
C          NO USEFUL DATA.
C                 1. TEXT OF 'NIL' , OR VARIANTS OF THIS.
C                 2. TEXT OF 'NO DATA AVAILABLE'.
C                 3. TEXT OF 'NO REPORTS AVAILABLE'.
C                      1. - 3.  ARE DETERMINED SIMPLY BY CHECKING
C                      THE LENGTH OF THE TEXT . IF IT IS LESS
C                      THAN 26 THERE CANT BE ANY USEFUL DATA IN IT
C                 4. UK AND GERMAN DOMESTIC BULLETINS WHICH DO
C                    NOT CONFORM TO WMO CODES.
C
C          INPUT     : BULLETIN IN ARRAY 'KCHAR' ,
C                      ONE CHARACTER PER WORD.
C
C          OUTPUT    : KERR = 0 INDICATES BULLETIN CONTENTS REQUIRED.
C                           = 1 MEANS TEXT OF 'NIL' ETC.
C                           = 2 UK OR GERMAN DOMESTIC BULLETIN.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCTXT(IERR)*
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
C         *CALL* *NEXTLET(I,J)*
C         *CALL* *PRTBULL(I,J)*
C         *CALL* *SAVBULL(IERR)*
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
C          J. HENNESSY         *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'combuff.f'
C
C     ------------------------------------------------------------------
C
C*          1.   CLEAR ERROR INDICATOR.
C                ----------------------
 100  CONTINUE
C
      KERR= 0
C
C
C*          1.1  CHECK IF BULLETIN IS TOO SHORT I.E. "NIL" BULLETIN.
C                ---------------------------------------------------
 110  CONTINUE
C
      ILE = IGS - IMI
      IF(ILE .LT. 26) THEN
                         KERR = 7
                         NUMBERR(7) = NUMBERR(7) + 1
                                KCHAR(IGS)=IOR(KCHAR(IGS),128)
C                               GO TO 900
                      END IF
C
C
C*          1.2  CHECK IF THE BULLETIN IS DOMESTIC FROM UK OR GERMANY.
C                -----------------------------------------------------
C                THIS IS DONE BY CHECKING IF THE BULLETIN HAS
C                --------------------------------------------
C                CCCC OF (EG--,ED--) OTHER THAN (EGRR,EDZW).
C                -------------------------------------------
 120  CONTINUE
C
      IPT=IAH+4
      CALL NEXTLET(IPT,JAH)
C
C     IF BULLETINS HAVE 'CCCC' NOT 'ED--' OR 'EG--' , RETURN.
C
      IF ( KCHAR(IPT).NE.69 ) RETURN
      IF ( KCHAR(IPT+1).NE.71.AND.KCHAR(IPT+1).NE.68 ) RETURN
C
C        FIRST UK
C
             IF (KCHAR(IPT+1).EQ.71)
     C                      THEN
                                 IF(KCHAR(IPT+2) .NE. 82 .OR.
     1                              KCHAR(IPT+3) .NE. 82)
     2                                   KERR = 8
                            ELSE
C
C        THEN GERMANY
C
                                 IF(KCHAR(IPT+2) .NE. 90 .OR.
     1                              KCHAR(IPT+3) .NE. 87)
     2                                   KERR = 8
                            END IF
C
C     MARK CCCC GROUP IF REQUIRED.
C
      IF ( KERR.EQ.8 ) THEN
                              KCHAR(IPT+4) = IOR(KCHAR(IPT+4),128)
                              NUMBERR(8) = NUMBERR(8) + 1
                          END IF
C
C           1.3 TREAT IN ACCORDANCE WITH DEFINED OPTIONS.
C               -----------------------------------------
 130  CONTINUE
C
C     RETURN IF NO ERROR.
C
      IF ( KERR.EQ.0 ) RETURN
C
C
 900  CONTINUE
C
      N = KERR - 1
cpsjun11      N1 = IAND(JISHFT(IOPTS(677),-N),1)
cpsjun11      N2 = IAND(JISHFT(IOPTS(678),-N),1)
      N1 = IAND(ISHFT(IOPTS(677),-N),1)
      N2 = IAND(ISHFT(IOPTS(678),-N),1)
C
C           1.4  PRINT BULLETIN IF REQUIRED.
C                ---------------------------
 140  CONTINUE
C
      IF (N1.EQ.1) THEN
                       WRITE (*,9900) KERR
                       CALL PRTBULL (1,IGS)
                   END IF
C
C           1.5 WRITE TO ERROR FILE IF REQUIRED.
C               --------------------------------
 150  CONTINUE
C
      IF ( N2.EQ.1 ) CALL SAVBULL(IERR)
      RETURN
C     -----------------------------------------------------------------
 9900 FORMAT (1H ,'BULLETIN ERROR NUMBER ',I2.2)
C     -----------------------------------------------------------------
      END
      SUBROUTINE PROCT1U ( IERR )
C
C**** *PROCT1U*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING UPPER AIR
C         DATA ( BULLETINS WITH 'T1' OF 'U' )
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
C         *CALL* *PROCT1U(IERR)*
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
C         *CALL* *BULLUP(IERR)*
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
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
C*              1.1  BULLETINS WITH 'TT' = 'UA'.
C                    ----------------------------------------------
 110  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.2  BULLETINS WITH 'TT' = 'UB'.
C                    ---------------------------
 120  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.3  BULLETINS WITH 'TT' = 'UC'.
C                    ---------------------------
  130 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.4  BULLETINS WITH 'TT' = 'UD'.
C                    ---------------------------
  140 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.5  BULLETINS WITH 'TT' = 'UE'.
C                    ----------------------------
  150 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.6  BULLETINS WITH 'TT' = 'UF'.
C                    ---------------------------
  160 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.7  BULLETINS WITH 'TT' = 'UG'.  PILOT PART B.
C                    ----------------------------
  170 CONTINUE
      CALL BULLUG( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              1.8  BULLETINS WITH 'TT' = 'UH'.  PILOT PART C.
C                    ---------------------------
  180 CONTINUE
      CALL BULLUH( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              1.9  BULLETINS WITH 'TT' = 'UI'.  PILOT PART A AND B.
C
  190 CONTINUE
      CALL BULLUI( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.0  BULLETINS WITH 'TT' = 'UJ'.
C                    ---------------------------
 200  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.1  BULLETINS WITH 'TT' = 'UK'.
C                    ---------------------------
 210  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.2  BULLETINS WITH 'TT' = 'UL'.
C                    ---------------------------
 220  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.3  BULLETINS WITH 'TT' = 'UM'.
C                    ---------------------------
 230  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.4  BULLETINS WITH 'TT' = 'UN'.
C                    ---------------------------
 240  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.5  BULLETINS WITH 'TT' = 'UO'.
C                    ---------------------------
 250  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.6  BULLETINS WITH 'TT' = 'UP'.  PILOT PART A.
C                    ---------------------------
 260  CONTINUE
      CALL BULLUP( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.7  BULLETINS WITH 'TT' = 'UQ'.  PILOT PART D.
C                    ---------------------------
 270  CONTINUE
      CALL BULLUQ( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.8  BULLETINS WITH 'TT' = 'UR'.
C                    ---------------------------
 280  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.9  BULLETINS WITH 'TT' = 'US'.
C                    ---------------------------
 290  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              3.0  BULLETINS WITH 'TT' = 'UT'.
C                    ---------------------------
 300  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              3.1  BULLETINS WITH 'TT' = 'UU'.
C
 310  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.2  BULLETINS WITH 'TT' = 'UV'.
C
 320  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.3  BULLETINS WITH 'TT' = 'UW'.
C
 330  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.4  BULLETINS WITH 'TT' = 'UX'.
C
 340  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.5  BULLETINS WITH 'TT' = 'UY'.  PILOT PART C AND D.
C
 350  CONTINUE
      CALL BULLUY( IERR )
      RETURN
C     ------------------------------------------------------------------
C*              3.6  BULLETINS WITH 'TT' = 'UZ'.
C
 360  CONTINUE
      RETURN
C
      END
      SUBROUTINE BULLUP ( IERR )
C
C**** *BULLUP*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF INDIVIDUAL
C         REPORTS FROM BULLETINS OF PILOT REPORTS.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLUP(IERR)*
C
C          INPUT    : BULLETIN IN CCITT 5 CHARACTER , 1 CHARACTER PER
C                     WORD IN 'KCHAR'.
C
C                      POINTERS TO BEGINNING AND END OF 'STARTING LINE',
C                     'ABBREVIATED HEADING','MIMIMJMJ LINE' AND END OF
C                     BULLETIN.
C
C                     IERR NOT USED.
C
C          OUTPUT   : DECODED REPORTS WRITTEN TO FILE AND ERROR REPORTS
C                     TO ERROR FILE.
C
C                     IERR = 1 , IF ANY FILE HANDLING ERROR.
C
C
C
C     METHOD.
C     -------
C
C         THIS ROUTINE HAS MULTIPLE ENTRY POINTS , ONE FOR
C         EACH OF THE POSSIBLE 'TT' CODES USED IN PILOT
C         BULLETIN HEADINGS . THIS ROUTINE CALLS THE APPROPRIATE
C         DECODING ROUTINES FOR EACH PART OF THE REPORTS BY
C         EXAMINING EACH 'MIMIMJMJ' GROUP.
C
C         THIS APPROACH HAS BEEN ADOPTED BECAUSE OF THE
C         FREQUENCY WITH WHICH INCORRECT BULLETINS ARE
C         RECEIVED (E.G. ALL PARTS OF A PILOT IN 1 BULLETIN)
C         AND MAINTAINS THE SUBROUTINE NAMING CONVENTIONS
C         USED.
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          A.HOLOPAINEN  NOVEMBER  -84.
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
                 ENTRY BULLUG ( IERR )
                 ENTRY BULLUH ( IERR )
                 ENTRY BULLUI ( IERR )
                 ENTRY BULLUQ ( IERR )
                 ENTRY BULLUY ( IERR )
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'printre.f'
C
CRR   DATA IBLOCKS/10*-1/
CRR   DATA IDONEA /200*-1/
CRR   DATA IDONEB /200*-1/
CRR   DATA IDONEC /200*-1/
CRR   DATA IDONED /200*-1/
CRR   DATA IPRLIM /2000/
CRR   DATA MODIFY /0/
C
CRR   DATA ISTASA/
CRR  C   50*-1/
CRR   DATA ISTASB/
CRR  C   50*-1/
CRR   DATA ISTASC/
CRR  C   50*-1/
CRR   DATA ISTASD/
CRR  C   50*-1/
CRR   DATA IERSHIP / 0 /
CRR   DATA IERALLA /0/
CRR   DATA IERALLB /0/
CRR   DATA IERALLC /0/
CRR   DATA IERALLD /0/
C
CRR   DATA IPRALLA /0/
CRR   DATA IPRALLB /0/
CRR   DATA IPRALLC /0/
CRR   DATA IPRALLD /0/
C
      DATA IBLOCKS/10*1/
      DATA IDONEA /200*1/
      DATA IDONEB /200*1/
      DATA IDONEC /200*1/
      DATA IDONED /200*1/
      DATA IPRLIM /2000/
      DATA MODIFY /0/
C
      DATA ISTASA/
     C   100*-1/
      DATA ISTASB/
     C   100*-1/
      DATA ISTASC/
     C   100*-1/
      DATA ISTASD/
     C   100*1/
      DATA IERSHIP / 1 /
      DATA IERALLA /1/
      DATA IERALLB /1/
      DATA IERALLC /1/
      DATA IERALLD /1/
C
      DATA IPRALLA /1/
      DATA IPRALLB /1/
      DATA IPRALLC /1/
      DATA IPRALLD /1/
C
C     ------------------------------------------------------------------
C*          1.   SET FLAFS AND WORKING POINTERS.
C                -------------------------------
 100  CONTINUE
C
C     PRINT BULLETIN IF REQUIRED.
C
C     CALL PRTBULL(1,IGS)
C
      ITIM = -1
C
      IF(ITIM .GE. 0)
     C   THEN
            ITIMES(1) = ITIM - 3
            IF(ITIMES(1) .LT. 0) ITIMES(1) = 21
            ITIMES(2) = ITIM + 2
         ELSE
            ITIMES(1) = 0
            ITIMES(2) = 24
         END IF
C
C
C     WORKING POINTER IS SET TO START OF 'MIMIMJMJ' LINE.
C
      IPT = IMI
C
C     CLEAR FLAG WHICH INDICATES BULLETIN HEADER ALREADY WRITTEN TO
C     ERROR FILE.
C
      IHEAD = 0
C
C     SET DATA BASE REPORT TYPE INDICATORS IN DECODED REPORT HEADER
C     AND PART OF REPORT INDICATOR.
C
C     DATA BASE REPORT TYPE  : 32 = PILOT
C                              33 = PILOT SHIP
C
C     PART INDICATOR         : 1 = PART A
C                              2 = PART B
C                              3 = PART C
C                              4 = PART D
C
C
C
  101 K = IPT + 15
C
C     CHECK IF LAND STATION ( 'MIMI' = PP )
C
      DO 200 I = IPT,K
         IF ( KCHAR(I).EQ.80.AND.KCHAR(I+1).EQ.80)
     C        THEN
                  KDEC(4) = 32
                  IPT = I
                  GO TO 600
              END IF
  200 CONTINUE
C
C
C     CHECK IF SHIP REPORT ( 'MIMI' = QQ ), ALSO CHECK IF THIS
C     IS SHIP WITH NAME STARTING QQ (LIKE UUQR), BY CHECKING
C     IF THERE IS AN OTHER GROUP QQ--.
C
      DO 300 II = IPT,K
         IF( KCHAR(II) .EQ. 81 .AND. KCHAR(II+1) .EQ. 81 )
     C        THEN
                  KDEC(4) = 33
C
                  DO 310 J=II+2,K
                  IF ( KCHAR(J) .EQ. 81 .AND. KCHAR(J+1) .EQ. 81)
     C               THEN
                        IPT = J
                        I = J
                        GO TO 600
                     END IF
310               CONTINUE
C
                  IPT = II
                  I = II
                  GO TO 600
              END IF
300   CONTINUE
C
C
C     INVALID 'MIMI'.WRITE REPORT TO ERROR FILE.
C
  500 KCHAR(IPT) = IOR(KCHAR(IPT),128)
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
      IF(IEQ .GT. IGS) RETURN
C
C     SEE IF REPORT TOO SHORT
C
      ILN = IEQ - IPT
      IF(ILN .LT. 17) GO TO 1500
C
      KDEC(4) = 32
      CALL SAVREP ( IHEAD,IERR )
      GO TO 1500
C
C
C     IDENTIFY PART OF REPORT.
C
  600 KDEC(23) = 0
      IF (KCHAR(I+2).EQ.65.OR.KCHAR(I+3).EQ.65) KDEC(23) = 1
      IF (KCHAR(I+2).EQ.66.OR.KCHAR(I+3).EQ.66) KDEC(23) = 2
      IF (KCHAR(I+2).EQ.67.OR.KCHAR(I+3).EQ.67) KDEC(23) = 3
      IF (KCHAR(I+2).EQ.68.OR.KCHAR(I+3).EQ.68) KDEC(23) = 4
C
      K = KDEC(23) + 1
C
C
C     LOCATE END OF REPORT AND INCREMENT COUNT.
C
C
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
      IF(IEQ .GT. IGS) RETURN
C
C
C     CHECK IF = - SIGN IS MISSING,TRYING TO FIND NEXT
C     PPAA, PPBB, PPCC, PPDD GROUP.
C
      CALL NEXTMI(IPT,IEQ,IIPT)
      IF(IIPT .LT. IEQ) IEQ= IIPT
C
      NUMREP(K+5) = NUMREP(K+5) + 1
C
C
      GO TO (500,700,900,1100,1300) K
C
C
C     DECODE PILOT REPORT PART A.
C
 700  CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL UPINT(IHEAD,IERR)
C
C
      IF(KERR .NE. 0) GO TO 1500
C
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL UPDEC(IHEAD,IERR)
      GO TO 1500
C
C
C     DECODE PILOT REPORT PART B.
C
 900  CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL UGINT ( IHEAD,IERR )
C
C
      IF ( KERR.NE.0 ) GO TO 1500
C
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL UGDEC ( IHEAD,IERR )
      GO TO 1500
C
C
C     DECODE PILOT REPORT PART C.
C
 1100 CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL UHINT ( IHEAD,IERR )
C
      IF ( KERR.NE.0 ) GO TO 1500
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL UHDEC ( IHEAD,IERR )
      GO TO 1500
C
C
C     DECODE PILOT REPORT PART D.
C
 1300 CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL UQINT ( IHEAD,IERR )
C
      IF ( KERR.NE.0 ) GO TO 1500
C
C
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL UQDEC ( IHEAD,IERR )
      GO TO 1500
C
C
C
C     LOCATE START OF NEXT REPORT.
C
C
 1500 IPT = IEQ + 1
      CALL NEXTPRT ( IPT,IGS )
C
C
C
C     CHECK FOR END OF BULLETIN.
C
C
      IF ( IPT.GE.IGS )
     C    THEN
C             END OF BULLETIN , SO IF ANY REPORT HAS BEEN WRITTEN TO
C             ERROR FILE , ADD 'GS' CHARACTER BEFORE RETURNING.
C
              IF ( IHEAD.EQ.0 ) RETURN
              IHEAD = 2
              CALL SAVREP ( IHEAD,IERR )
C
              RETURN
          END IF
C
C
C
C     RESET REPORTS DATA BASE TYPE AND REPORT PART INDICATORS.
C
C
      KDEC(23) = 0
      KDEC(4) = 0
C
C     SEARCH THE BEGINNING OF NEXT REPORT AND CHANGE THE POINTER IMI
C
      NPT = IEQ
      CALL NEXTLET(NPT,IGS)
      IF(NPT .GE. IGS) RETURN
      IMI = NPT
      CALL NEXTEND(NPT,IGS)
      IF(NPT .GE. IGS) RETURN
      JMI = NPT
C
      GO TO 101
C
      END
      SUBROUTINE UGDEC ( IHEAD,IERR )
C
C**** *UGDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE PILOT REPORT PART B FROM INTERMEDIATE
C         FORMAT TO FULL DECODED FORMAT.
C
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UGDEC(IERR)*
C
C         INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'.
C
C         OUTPUT   : REPORT WRITTEN TO DECODED REPORTS FILE.
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
C          A. HOLOPAINEN  MAY -84
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'printre.f'
C
      DATA IROU /0/
      DATA IPRNUM /0/
C
C     ------------------------------------------------------------------
C
C*          1.   SET UP FLAGS AND POINTERS.
C                --------------------------
 100  CONTINUE
C
C
C     DEFINE 'NIL' INDICATOR
C
      NIL = IAND(KDEC(20),1)
C
C
C     CLEAR ERROR INDICATOR
C
      KERR = 0
C
C     SET DECODED REPORT TO MISSING DATA VALUE
C
C
      DO 1 I=25,600
      KDEC(I)=MINDIC
1     CONTINUE
C
C
C
C       *************************************
C       *                                   *
C       *             SECTION 1             *
C       *                                   *
C       *   (IDENTIFICATION AND POSITION)   *
C       *************************************
C
C
C
C     REPORT HEADER
C
C     IN THIS SUBROUTINE ONLY THE FOLLOWING PARTS
C     OF THE HEADER WILL BE DECODED
C        - DAY OF THE MONTH
C        - TIME (HOUR & MIN) FROM REPORT
C        - IDENTIFIER
C        - LENGTH OF THE REPORT
C     AND IN CASE OF TEMP SHIP OR TEMP DROP
C        - LAT &LONG
C        - ALT = 0 (FOR SHIP)
C
C*     DAY AND TIME
C
C
      IF(KINT(2).LT.0.OR.KINT(2).GT.23) THEN
                                           RETURN
                                        END IF  
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C     CHECK IF SHIP PILOT
C
C
      IF(KDEC(4) .EQ. 33)
     1           THEN
C
C                   LAT & LONG
C
                    CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1                          KDEC(5),KDEC(6))
C
                    IF(KDEC(5).EQ.MINDIC.OR.KDEC(6).EQ.MINDIC) RETURN
C
C                   ALTIT=0, FOR SEA STATION
C
                    KDEC(8)=0
C
                 END IF
C
C
C
C
C
101   CONTINUE
C
C***  IDENT   STATION/SHIP IDENTIFICATION
C             IN KINT(12) - KINT(16)
C
C
      KDEC(7)=88
C
C
C
C***  INSTRUMENT SPECCIFICATION
C
      kdec(13)=9
      if(kint(3).ne.mindic) kdec(13)=kint(3)
C
C
C***  REPORT LENGTH
C
C
      KDEC(24)=384
C
C
C
      K=MINDIC
      IF(KDEC(4) .LT. 21) K=KDEC(17)*1000+KDEC(16)
C
C     CHECK IF WIND SPEED IS GIVEN IN KNOTS, IN
C     THIS CASE 50 HAS BEEN ADDED TO THE DATE YY.
C
      IWW=0
      IF(KDEC(1) .GT. 31 .AND. KDEC(1) .LE. 81)
     1             THEN
                      IWW=1
                      KDEC(1)=KDEC(1)-50
                   END IF
C
C
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LE. 0 .OR. KDEC(1) .GT. 31) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) GO TO 99800
C
      IROU = IROU +1
C
4000  CONTINUE
C
C
C
C
C
C       *********************************************************
C       *                                                       *
C       *                      SECTION 3                        *
C       *                                                       *
C       *       (FIXED LEVELS AND/OR SIGNIFICANT LEVELS)        *
C       *********************************************************
C
C
C
C
C ----- FIXED LEVELS  ----------------------------------------
C
         IPOS = 22
C
C -----  LOOP OVER 20 POSSIBLE SETS OF LEVELS
C
         DO 4200 I=1,20
         IND = 17 + (I-1)*11
         IBASE = 0
         IF(KINT(IND) .EQ. 9)
     C      THEN
               IREF = 300
            ELSE
               IF(KINT(IND) .EQ. 8)
     C            THEN
                     IREF = 500
                  ELSE
                     IF(KINT(IND) .EQ. 1)
     C                  THEN
                           IREF = 300
                           IBASE = 30000
                        ELSE
                           GO TO 4200
                        END IF
                  END IF
            END IF
C
C

         IF(KINT(IND+1).EQ.MINDIC) GO TO 4200 
         ITENS = KINT(IND+1) * 10
         IF(ITENS .LT. 0 .OR. ITENS .GT. 90) GO TO 4200
C
C
C -----     LOOP OVER 3 POSSIBLE LEVELS IN THE SET
C
            DO 4100 J=1,3
            IND2 = IND + 5 + (J-1)*2
C
C           IF FIRST LEVEL AND 'H = /' THIS IS SURFACE
C
            IF(KINT(IND+1+J) .EQ. MINDIC .AND. I .EQ. 1 .AND.
     C         J .EQ. 1) KINT(IND+1+J) = 0
C
            IF(KINT(IND+1+J) .EQ. MINDIC .OR. KINT(IND2) .EQ. MINDIC
     C         .OR. KINT(IND2+1) .EQ. MINDIC) GO TO 4100
C
            IHIGH = IBASE + (ITENS + KINT(IND+J+1)) * IREF
C
               DO 4050 II = IPOS,22,-3
               IF(KDEC(II) .NE. MINDIC .AND. IHIGH .LT. KDEC(II))
     C           GO TO 4300
4050           CONTINUE
C
            IPOS = IPOS + 3
            KDEC(IPOS) = IHIGH
            CALL DDFFF(KINT(IND2),KINT(IND2+1),IWW,K,MINDIC,
     C                 KDEC(IPOS+1),KDEC(IPOS+2))
4100        CONTINUE
C
C
4200     CONTINUE
C
C
4300     CONTINUE
C
C
C ----- SIGNIFICANT  LEVELS  ------------------------------------------
C
         LEVNUM = 0
C
         IF(KINT(237) .NE. 21212) GO TO 9900
C
            DO 4500 J=1,60
            IND = 239 + (J-1)*4
            IF(KINT(IND) .EQ. MINDIC) GO TO 4500
C
            LEVNUM = LEVNUM + 1
            IPOS = 205 + (LEVNUM-1)*3
C
C****       NN PPP   PRESSURE AT THE LEVEL
C
            KDEC(IPOS) = KINT(IND) * 10
C
C           CHECK IF THOUSAND DIGIT IS OMITTED
C
            IF(KDEC(IPOS) .LE. 2000)
     C         THEN
                  IF(LEVNUM .EQ. 1) KDEC(IPOS) = KDEC(IPOS) + 10000
C
                  IF(LEVNUM .GT. 1 .AND. LEVNUM .LE. 4)
     C               THEN
C
C                       CHECK IF PREVIOUS LEVEL IS > 1000MB
C                       AND IF IT IS LOWER THAN THIS ONE
C
                        IF(KDEC(IPOS-3) .GT. 10000)
     C                     THEN
                              IF( (KDEC(IPOS-3)-10000) .GT. KDEC(IPOS))
     C                            KDEC(IPOS) = KDEC(IPOS) + 10000
                           END IF
                     END IF
               END IF
C
C
C           REJECT IF LEVEL LOWER THAN PREVIOUS
C
            IF(LEVNUM .GT. 1)
     C         THEN
                  IF(KDEC(IPOS) .GE. KDEC(IPOS-3) .AND.
     C               KDEC(IPOS-3) .NE. MINDIC)
     C               THEN
                        KDEC(IPOS) = MINDIC
                        LEVNUM = LEVNUM - 1
                        GO TO 4500
                     END IF
               END IF
C
C****       DD FFF   WIND AT THE LEVEL
C
            CALL DDFFF(KINT(IND+1),KINT(IND+2),IWW,K,MINDIC,
     C                 KDEC(IPOS+1),KDEC(IPOS+2))
C
4500        CONTINUE
C
C
C
C
C
C       ***********************************************
C       *                 SECTION 9                   *
C       *                                             *
C       *              (REGIONAL DATA)                *
C       ***********************************************
C
C
C
C ------  NO REGIONAL DATA HANDLED  -----------------------------
C
C
C
9900  CONTINUE
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT (I.E. IF
C     THERE ARE NO DATA AND NO CODING ERRORS)
C
      NIL2 = 1
      DO 460 I=25,KDEC(24)
      IF(KDEC(I) .EQ. MINDIC) GO TO 460
      NIL2 = 0
      GO TO 465
460   CONTINUE
C
465   CONTINUE
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 99800
         END IF
C
C
9950  CONTINUE
C
C==========================================================
C====    PRINTING OF ERRORS & REPORTS & KINT & KDEC    ====
C==========================================================
C
C
      IPRI = 0
      IPRIALL = 0
C
      IS1 = KINT(12) - 48
      IS2 = KINT(13) - 48
      IS3 = KINT(14) - 48
      IS4 = KINT(15) - 48
      IS5 = KINT(16) - 48
C
      IBLOK = 10*IS1 + IS2
      ISTA = 10000*IS1 + 1000*IS2 + 100*IS3 + 10*IS4 + IS5
C
      DO 700 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
700   CONTINUE
C
      DO 710 I=1,100
      IF(ISTA .EQ. ISTASB(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
710   CONTINUE
C
C
C     PRINT THE ERRONEUS SHIPS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERSHIP .GT. 0)
     C   THEN
            IF(KDEC(4) .GE. 36 .AND. KDEC(20) .GT. 1) IPRI = 1
         END IF
C
C     PRINT ALL ERRONEUS REPORTS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERALL .GT. 0)
     C   THEN
            IPRI = 1
         END IF
C
      IF(IERALLB .GT. 0) IPRI = 1
C
      IF(KDEC(2) .LT. ITIMES(1) .OR. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
C
      IF(IPRALLB .EQ. 1)
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
C
C
      IF(IPRNUM .GT. IPRLIM) GO TO 99800
C
      IPRI = 0
C
      IF(IPRI .GT. 0)
     C   THEN
C
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IPRNUM=IPRNUM+1
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        IPRNUM=IPRNUM+1
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKINT(KINT,1,600,MINDIC)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
C
10000 FORMAT(1H ,///)
20000 FORMAT(1H ,/////2X,' PART B  NUMBER =',I5,/)
C
C
C
C
C
C
C
C
C
99800 CONTINUE
C
c      CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
      CALL PILOEXP( 2,IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UGDEC - after piloexp, ierr ',IERR
CTEST
      CALL MRGPIL( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UGDEC - after mrgpil, ierr ',IERR
CTEST
      CALL QCPILOT(kdec(4),IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UGDEC - after qcpilot, ierr ',IERR
CTEST
      CALL BUFRENC( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UGDEC - after bufrenc, ierr ',IERR
CTEST
C
      IERR=0
C
99900 CONTINUE
      RETURN
      END
      SUBROUTINE UGINT ( IHEAD,IERR )
C
C**** *UGINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT PILOT REPORTS PART B FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C         INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                     1 CHARACTER PER WORD.
C
C                     IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                               WRITTEN TO ERROR FILE.
C                           = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                     IERR NOT USED ON INPUT.
C
C         OUTPUT    : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                     DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION ).
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UGINT ( IHEAD,IERR )*
C
C     METHOD.
C     -------
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING
C         IS PROCEEDED UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON-FATAL ERROR HAS BEEN DETECTED THE
C         ROUTINE TRIES TO FIND THE BEGINNING OF CORRECT
C         DATA.  IF CORRECT DATA ARE FOUND THE DECODING
C         THE NUMBER OF NON-FATAL ERRORS IS COUNTED AND
C         IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE
C         DATA AFTER THE FIRST ERROR ARE REMOVED (THIS
C         IS DONE TO ENSURE THAT TOTALLY CORRUPTED DATA
C         ARE NOT USED E.G. WRONG PART OF PILOT).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C         NUMERR = NUMBER OF NON-FATAL ERRORS
C         IREMPOS= THE POINTER TO FIRST ERROR IN KINT.
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
C          A.HOLOPAINEN  NOVEMBER  -84.
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'printre.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
      DIMENSION IREG(9),INAT(9)
      DATA IREG /
     C    51515, 52525, 53535, 54545, 55555, 56565, 57575, 58585, 59595/
      DATA INAT /
     C    61616, 62626, 63636, 64646, 65656, 66666, 67676, 68686, 69696/
C
C     ------------------------------------------------------------------
C           1.  CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C               -------------------------------------------------------
C               CHARACTER OF REPORT.
C               --------------------
 100  CONTINUE
C
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      MODIFY = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR = 0
      INDH = 0
      INDP = 0
      INDLEV = 0
      LEVNUM = 0
      IREMPOS = 17
C
C
C***
C*    CHECK FOR 'NIL' REPORT.
C***
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.25 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(8,0,IFIRST,NUMERR)
             KERR = 0
         END IF
C
C
C
C***
C*     SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C***
C
      DO 200 I=1,600
         KINT(I) = MINDIC
  200 CONTINUE
C
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 300 I=12,14
         KINT(I) = 88
  300 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C***
C*    CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C***
C
      DO 400 I=1,3
         KDEC(I) = MINDIC
  400 CONTINUE
C
      DO 500 I=5,9
         KDEC(I) = MINDIC
  500 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = IAND(KDEC(15),4)
C
      KDEC(17) = MINDIC
      KDEC(22) = MINDIC
C
C
C
1000  CONTINUE
C
C
C   ***********************************************************
C   *                      SECTION 1                          *
C   *                                                         *
C   *     (IDENTIFICATION AND POSITION, THIS IS DIFFERENT     *
C   *      FOR LAND STATIONS AND SHIP REPORTS)                *
C   ***********************************************************
C
C
cpsmar07C               YY GG /
C               YY GG a4
C               -- -- -
C
C
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*10000)
                CALL NEXPRT2(IPT,IEQ,*10000)
                IF(KCHAR(IPT).GE.65) THEN
                                        GO TO 1011
                                     END IF
C
C               FIRST FIGURE OF THE GROUP MUST BE 0-3
C               OR 5-8 IF WIND SPEED IS GIVEN IN KNOTS,
C               OTHERWISE YYGGID GROUP IS MISSING
C
C
                IF ((KCHAR(IPT) .GE. 48) .AND. (KCHAR(IPT) .NE. 52)
     1                .AND. (KCHAR(IPT) .LE. 56))
     2                 CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
                IF(KINT(1).LE.0.AND.KINT(1).GT.31) THEN
                   KERR=1
                   GO TO 10000
                END IF
C
                JPT = IPT
C
C
C
C
  700 IF ( KDEC(4).EQ.32 )
     C      THEN
C
C               LAND STATION
C
C
C
C               IIIII     -   RETAIN POINTER TO IDENTIFIER
C               -----
C
C
                CALL NEXPRT2( IPT,IEQ,*701)
                GO TO 702
C
 701            CONTINUE
C           
                KERR=2
                GO TO 10000
C
 702            CONTINUE
C 
                KPT = IPT
                ID = 5
                CALL EXTINT ( IPT,5,4 )
C
C               EXTRACT STATION LAT/LONG ETC FROM WMO MASTER FILE.
C
                CALL LOCSTAT ( IWIND,IRET )
C
C
C               CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                IF(IRET .eq. 0)
     C              THEN
                       IF(NIL .EQ. 1)
     C                    THEN
                             KDEC(20)=1
                             kerr=1
                             RETURN
                          END IF
                    END IF
C
                IF ( IRET.EQ.1 )
     C              THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        CALL ERRSTA(8,1,IFIRST,NUMERR)
C
c                       PRINT IIIII WITH NO MASTER FILE ENTRY
               OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_pilo_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C
                        WRITE (55,9901) ctstamp,KINT(4)
                        CLOSE(55)
                        WRITE (*,9901) ctstamp,KINT(4)
cpsokt98 9901                FORMAT (1H ,a8,' PILO NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' PILO NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        GO TO 10000
                    END IF
C
                IF ( IRET.EQ.2 )
     C             THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2

                        CALL ERRSTA(8,2,IFIRST,NUMERR)
C
                        GO TO 10000
                    END IF
C
C
                 IF(IRET .EQ. 3)
     1              THEN
C
C                     INVALID OBSERVATION TYPE
C
                      KERR = 3
                      CALL ERRSTA(8,2,IFIRST,NUMERR)
C
                      GO TO 10000
                   END IF
C
            ELSE
C
C                PILOT SHIP
C
                 IF(KDEC(4) .EQ. 33)
     1              THEN
C
                        KINT(4)=0
C
C
C
C                       RETAIN POINTER TO STATION IDENTIFIER .
C
                        KPT=IMI
C
C
C                       CHECK THE CALL SIGN
C
                        IBUOY = 0
                        K = IMI
                        J = K + 4
C
                        DO 750 I=K,J
                        IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750                   CONTINUE
C
C
C                       IF A LETTER GROUP IS FOUND CHECK THAT THIS IS
C                       NOT MIMIMJMJ GROUP 'QQBB'. IN THIS CASE THE SHIP
C                       CALL SIGN GROUP IS PRESUMED MISSING.
C
cpsmar07                        IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85)
                        IF((KCHAR(K) .EQ. 81) .AND. (KCHAR(K+1) .EQ. 81)
     C                  .AND.
     C                  (KCHAR(K+2) .EQ. 66) .AND. (KCHAR(K+3) .EQ. 66))
     C                      THEN
                               IBUOY = 2
                            END IF
C
C
                        IF(IBUOY .NE. 0)
     C                     THEN
C
C                             SHIP,RIG OR PLATFORM. IDENTIFIER
C                             NUMBER SET TO 0.
C
C
                         ID=0
                              KINT(4) = 0
C
                              IF (IBUOY.EQ.1)
     C                           THEN
C
C                                   MOVE POINTER PAST CALL SIGN . THE
C                                   FIRST CHARACTER OF THE NEXT GROUP IS
C                                   LOCATED AS SOME SHIPS USE THE FULL
C                                   NAME ( WHICH CAN CONSIST OF 2 WORDS
C                                   INSTEAD OF CALL SIGN. SO IF THE NEXT
C                                   GROUP DOESN'T START WITH 'QQ' (WHICH
C                                   IS THE BEGINNING OF MIMIMJMJ GROUP)
C                                   IT MUST BE PART OF THE SHIP'S NAME
C                                   USED INSTEAD OF CALL SIGN.
C
                                    IIPT=K
                                    CALL NEXSEP2(IIPT,IEQ,*10000)
                                    ID = IIPT - KPT
                                    CALL NEXTLET (IIPT,IEQ)
C
                                    IF(KCHAR(IIPT) .NE. 81 .OR.
     C                                 KCHAR(IIPT+1) .NE. 81)
     C                                 THEN
                                          CALL NEXSEP2(IIPT,IEQ,*10000)
                                          ID = IIPT - K
                                       END IF
C
C
                         END IF
                       IF(IBUOY.EQ.2)
     1                    THEN
C
C                            SHIP CALL SIGN FOLLOWS MIMIMJMJ
C
                             IIPT=K
C
C                            SKIP MIMIMJMJ GROUP
C
                             CALL NEXSEP2(IIPT,IEQ,*10000)
                             CALL NEXPRT2(IIPT,IEQ,*10000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*10000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*10000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*10000)
                                       ID=IIPT-K
                                    END IF
                              END IF
                          END IF
C
                   END IF
C
C
C                       99 LALALA
C                       -- ------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                       CHECK VALIDITY OF LALALA
C
                        IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR = 4
                              CALL ERRSTA(8,1,IFIRST,NUMERR)
                              GO TO 10000
                           END IF
C
C                       QC LOLOLOLO
C                       -- --------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )
C
C                       CHECK VALIDITY OF LOLOLOLO
C
                        IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C                          KINT(7).NE.5.AND.KINT(7).NE.7) IRET=1
                        IF (KINT(8).LT.0.OR.KINT(8).GT.1800)IRET=1
                        IF (IRET.NE.0)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR=4
                              CALL ERRSTA(8,2,IFIRST,NUMERR)
                              GO TO 10000
                           END IF
C
C                       MMM ULA ULO
C                       --- --- ---
cps  MMMULaUlo acts as a control group for position (previous 2 groups).
cps  But currently we only compare MMM with position.No check for ULa and ULo.
C
                        CALL NEXPRT2(IPT,IEQ,*10000)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                               LAT,LON)
C
                        CALL MARDSEN(LAT,LON,KINT(9),IRET)
C
                        IF(IRET .NE. 0)
     1                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
C    --------------            WRONG CODE FOR MARSDEN SQUARE    ------
C
                               KERR = 4
                               CALL ERRSTA(8,1,IFIRST,NUMERR)
C
                               GO TO 10000
C
                            END IF
C
                    ELSE
C
C                       THIS IS NOT A PILOT REPORT
C
                        KERR = 3
                        CALL ERRSTA(8,1,IFIRST,NUMERR)
C
                        GO TO 10000
                END IF
             END IF
C
C
C     RETAIN STATION IDENTIFIER IN CHARACTER FORM ( IF ANY EXISTS )
C
      IF ( ID.NE.0 )
     C    THEN
              IF (ID.GT.5) ID = 5
              J = KPT
              K = 12 + ID -1
              DO 800 I=12,K
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C
C
C
C     CHECK VALUES OF YY GG FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4) .EQ. 33 )
     1     THEN
               IRET = 0
               IF ((KINT(1) .LT. 1) .OR. ((KINT(1) .GT. 31) .AND.
     2             (KINT(1) .LT. 51)) .OR. (KINT(1) .GT. 81))
     3             THEN
                       IRET = 2
                       KINT(1) = MINDIC
                   END IF
               IF (KINT(2).LT.0.OR.KINT(2).GT.23)
     C             THEN
                       IRET = 2
                       KINT(2) = MINDIC
                   END IF
C
               IF (IRET.NE.0)
     C             THEN
                       JPT=IABS(JPT)
                       KCHAR(JPT)=IOR(KCHAR(JPT),128)
                       KERR = 3
                       CALL ERRSTA(8,1,IFIRST,NUMERR)
                       GO TO 10000
C
                   END IF
           END IF
C
C
C
C
C     'NIL' REPORTS ARE NOT PROSESSED ANY FURTHER
C
       IF(NIL .EQ. 1) RETURN
C
C
C
C
C
4000  CONTINUE
C
C   ********************************************************
C   *                     SECTION 4                        *
C   *                                                      *
C   *   (FIXED REGIONAL LEVELS AND/OR SIGNIFICANT LEVELS   *
C   ********************************************************
C
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 21212) GO TO 4500
C
      DO 4050 I = 1 , 9
      IF(IVAL .EQ. IREG(I)) GO TO 10000
      IF(IVAL .EQ. INAT(I)) GO TO 10000
4050  CONTINUE
C
C
C
C
      CALL EXTVAL(IPT,1,IVAL)
C
      IF(IVAL .EQ. 9 .OR. IVAL .EQ. 8)
     C   THEN
C
C****        9/8/1 TN U1 U2 U3  ......  TENS & UNITS DIGITS OF ALTITUDE
C            - - - -- -- -- --
C
            INDLEV = INDLEV + 1
            IPOS = 17 + (INDLEV-1)*11
C
C
            CALL EXTGRP(IPT,1,1,1,1,1,IPOS,IRET)
C
            IF(IRET .NE. 0)
     C         THEN
                  KERR = 5
                  CALL ERRSTA(8,2,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  CALL NEXSEP2(IPT,IEQ,*10000)
                  CALL NEXPRT2(IPT,IEQ,*10000)
                  GO TO 4000
               END IF
C
C
C
            N = KINT(IPOS+1)
C
C
C              SEE HOW MANY WIND GROUPS THERE ARE.  THERE ARE SUPPOSED
C              TO BE ONLY THREE WIND GROUPS (MAX) AFTER 'ID-GROUP'
C              44NP1P1 / 55NP1P1.  HOWEVER QUITE A COMMON PRACTICE
C              IS TO HAVE THE ID-GROUP ONLY ONCE AND ALL THE WIND-GROUPS
C              AFTER THIS.  THEREFORE THIS PRACTICE IS ALSO ACCEPTED
C              ASSUMING DATA ARE CORRECT.
C
               NUMBER = 0
               DO 4100 I = 1,3
               IF(NUMBER .GT. 0 .AND. KINT(IPOS+I+1) .LT. KINT(IPOS+I))
     C         GO TO 4100
               IF(KINT(IPOS+I+1) .NE. MINDIC) NUMBER = I
4100           CONTINUE
C
C
               IPOS = 22 + (INDLEV-1) * 11
C
C****          DD FFF   .......  WIND DIRECTION & SPEED
C              -- ---
C
               DO 4200 I=1,NUMBER
               IPOS2 = IPOS + (I-1)*2
               CALL NEXPRT2(IPT,IEQ,*10000)
               CALL EXTVAL(IPT,1,IVAL)
               IF(IVAL .EQ. 9 .OR. IVAL .EQ. 8) GO TO 4000
C
               CALL EXTVAL(IPT,5,IVAL)
               DO 4130 J=1,9
                  IF(IVAL .EQ. IREG(J)) GO TO 10000
                  IF(IVAL .EQ. INAT(J)) GO TO 10000
4130           CONTINUE
C
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS2,IRET)
C
               IF ( (IRET .NE. 0)  .OR. (KINT(IPOS2) .GT. 36 .AND.
     C            KINT(IPOS2) .NE. MINDIC) )
     C               THEN
                        KERR = 7
C
                        CALL ERRSTA(8,1,IFIRST,NUMERR)
                        IF(NUMERR .EQ. 1) IREMPOS = IPOS2
C
                        DO 4150 J = IPOS2,IPOS2+1
                        KINT(J) = MINDIC
4150                    CONTINUE
                        CALL NEXSEP2(IPT,IEQ,*10000)
                        CALL NEXPRT2(IPT,IEQ,*10000)
                        IF(I .EQ. NUMBER) GO TO 4000
                     END IF
C
4200           CONTINUE
C
         ELSE
            GO TO 4900
         END IF
C
C
C
C
      CALL NEXSEP2(IPT,IEQ,*10000)
      GO TO 4000
C
C
C
C
C****  21212  ......  SIGNIFICANT LEVEL INDICATOR
C      -----
C
C
4500  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      IPOS = 237
      CALL EXTGRP(IPT,5,0,0,0,0,IPOS,IRET)
      IF (IRET .NE. 0)
     C   THEN
            KERR = 10
C
            CALL ERRSTA(8,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            CALL NEXSEP2(IPT,IEQ,*10000)
            CALL NEXPRT2(IPT,IEQ,*10000)
            GO TO 4900
         END IF
C
C
C
C
4600  CONTINUE
C
C
C**** NINI PIPIPI  ......  PRESSURE AT SIGNIFICANT LEVEL
C     ---- ------
C
C
      LEVNUM = LEVNUM + 1
      IPOS = 238 + (LEVNUM-1) * 4
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
C     CHECK IF THIS REALLY IS THE NEXT LEVEL
C
      IF(KCHAR(IPT) .NE. KCHAR(IPT+1)) GO TO 4900
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
      IF (IRET .NE. 0)
     C   THEN
            KERR = 5
            CALL ERRSTA(8,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
            CALL NEXSEP2(IPT,IEQ,*10000)
            CALL NEXPRT2(IPT,IEQ,*10000)
C
            GO TO 4700
         END IF
C
C
C**** DD FFF  ......  WIND AT SIGNIFICANT LEVEL
C     -- ---
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
      IF ( (IRET .NE. 0)  .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C      KINT(IPOS+2) .NE. MINDIC) )
     C   THEN
            KERR = 7
C
            CALL ERRSTA(8,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            DO 4650 J = IPOS,IPOS+3
            KINT(J) = MINDIC
4650        CONTINUE
            CALL NEXSEP2(IPT,IEQ,*10000)
            CALL NEXPRT2(IPT,IEQ,*10000)
            GO TO 4600
         END IF
C
C
      GO TO 4600
C
C
C
4700  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      IF(KCHAR(IPT) .EQ. KCHAR(IPT+1)) GO TO 4600
      CALL NEXSEP2(IPT,IEQ,*10000)
      GO TO 4700
C
C
C
4900  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTVAL(IPT,5,IVAL)
C
      IF(IVAL .EQ. 21212) GO TO 4500
C
      DO 5000 I = 1 , 9
      IF(IVAL .EQ. IREG(I)) GO TO 10000
      IF(IVAL .EQ. INAT(I)) GO TO 10000
5000  CONTINUE
C
      CALL EXTVAL(IPT,1,IVAL)
      IF( IVAL .EQ. 9 .OR. IVAL .EQ. 8) GO TO 4000
C
      CALL NEXSEP2(IPT,IEQ,*10000)
      GO TO 4900
C
C
C
C
C
10000 CONTINUE
C
C
      IF(KERR .EQ. 0) RETURN
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
C
C
                             IPT = KEEP
                             CALL FIXPILO
                             IFIRST=1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            ONLY REPORTS WITH ERROR IN DATA/TIME
C                            OR LAT/LONG ARE NOT PROSESSED ANY FURTHER
C
C                            IF THERE ARE MORE THAN 6 NON-FATAL ERRORS
C                            IN REPORT IT IS ONLY PROSESSED UNTIL THE
C                            FIRST ERROR
C
                             IF(NUMERR .GT. 6)
     C                          THEN
                                   DO 6000 I = IREMPOS,600
                                   KINT(I) = MINDIC
6000                               CONTINUE
                                END IF
C
                             IF(KERR .GT. 4) KERR = 0
                          END IF
C
C
C
C
      RETURN
      END
      SUBROUTINE UHDEC ( IHEAD,IERR )
C
C**** *UHDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE PILOT REPORT PART C FROM INTERMEDIATE
C         FORMAT TO FULL DECODED FORMAT.
C
C         INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'.
C
C         OUTPUT   : REPORT WRITTEN TO DECODED REPORTS FILE.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UHDEC ( IHEAD,IERR )*
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
C          A. HOLOPAINEN  MAY -84
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'printre.f'
C
      DIMENSION LEVSTAN(5)
C
      DATA LEVSTAN / 70, 50, 30, 20, 10/
C
C
      DATA IROU /0/
      DATA IPRNUM /0/
C
C     ------------------------------------------------------------------
C
C*          1.  DEFINE INDICATORS AND CLEAR FLAGS.
C                ---------------------------------
 100  CONTINUE
C
C
C     DEFINE 'NIL' INDICATOR
C
      NIL = IAND(KDEC(20),1)
C
C
C     CLEAR ERROR INDICATOR
C
      KERR = 0
C
C
C*    SET DECODED REPORT TO MISSING DATA VALUE
C
C
      DO 1 I=25,600
      KDEC(I)=MINDIC
1     CONTINUE
C
C
C
C       *************************************
C       *                                   *
C       *             SECTION 1             *
C       *                                   *
C       *   (IDENTIFICATION AND POSITION)   *
C       *************************************
C
C
C
C     REPORT HEADER
C
C     IN THIS SUBROUTINE ONLY THE FOLLOWING PARTS
C     OF THE HEADER WILL BE DECODED
C        - DAY OF THE MONTH
C        - TIME (HOUR & MIN) FROM REPORT
C        - IDENTIFIER
C        - LENGTH OF THE REPORT
C     AND IN CASE OF TEMP SHIP OR TEMP DROP
C        - LAT &LONG
C        - ALT = 0 (FOR SHIP)
C
C
C
C
C***   DAY AND TIME
C
C
      IF(KINT(2).LT.0.OR.KINT(2).GT.23) THEN
                                           RETURN
                                        END IF  
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C
C***
C*    CHECK IF SHIP PILOT
C***
C
      IF(KDEC(4) .EQ. 33)
     1           THEN
C
C***                LAT & LONG
C
                    CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1                          KDEC(5),KDEC(6))
                    IF(KDEC(5).EQ.MINDIC.OR.KDEC(6).EQ.MINDIC) RETURN
C
C***                ALTIT=0, FOR SEA STATION
C
                    KDEC(8)=0
C
                 END IF
C
C
C
C
C
 101  CONTINUE
C
C***  IDENT   STATION/SHIP IDENTIFICATION
C
C
      KDEC(7)=88
C
C
C
C***  INSTRUMENT SPECCIFICATION
C
      kdec(13)=9
      if(kint(3).ne.mindic) kdec(13)=kint(3)
C
C
C***  REPORT LENGTH
C
C
      KDEC(24)=384
C
C
C
      K=MINDIC
      IF(KDEC(4) .LT. 21) K=KDEC(17)*1000+KDEC(16)
C
C     CHECK IF WIND SPEED IS GIVEN IN KNOTS, IN
C     THIS CASE 50 HAS BEEN ADDED TO THE DATE YY.
C
      IWW=0
      IF(KDEC(1) .GT. 31 .AND. KDEC(1) .LE. 81)
     1             THEN
                      IWW=1
                      KDEC(1)=KDEC(1)-50
                   END IF
C
C
C
C
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LE. 0 .OR. KDEC(1) .GT. 31) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) GO TO 99800
C
C
      IROU = IROU +1
C
2000  CONTINUE
C
C
C       ************************************************
C       *                                              *
C       *                SECTION 2                     *
C       *                                              *
C       *       (STANDARD ISOBARIC SURFACES)           *
C       ************************************************
C
C
         DO 2500 I = 1,3
         IND = 17 + (I-1)*9
         IF(KINT(IND) .EQ. 44 .OR. KINT(IND) .EQ. 55)
     C      THEN
C
C              NUMBER OF LEVELS IN THIS GROUP
C
               NUMBER = KINT(IND+1)
C
C              GET THE FIRST LEVEL OF THE GROUP
C
                  DO 2100 J=1,5
                  IF(KINT(IND+2) .EQ. LEVSTAN(J))
     C               THEN
                        INDLEV = J
                        IPOS = 25 + (J-1)*3
                        GO TO 2200
                     END IF
2100              CONTINUE
C
               GO TO 2500
C
2200           CONTINUE
C
                  DO 2300 J = 1,NUMBER
                  IF( (INDLEV+J-1) .GT. 5) GO TO 2500
                  IND2 = IND + 3 + (J-1)*2
                  KDEC(IPOS) = LEVSTAN(INDLEV+J-1) * 10
                  CALL DDFFF(KINT(IND2),KINT(IND2+1),IWW,K,MINDIC,
     C                       KDEC(IPOS+1),KDEC(IPOS+2))
                  IF(KDEC(IPOS+1) .EQ. MINDIC .OR. KDEC(IPOS+2) .EQ.
     C               MINDIC) KDEC(IPOS) = MINDIC
                  IPOS = IPOS + 3
2300              CONTINUE
C
            END IF
C
2500     CONTINUE
C
C
C
C       *******************************************************
C       *                                                     *
C       *                     SECTION 3                       *
C       *                                                     *
C       *   (MAXIMUM WIND LEVEL(S) AND VERTICAL WIND SHEAR)   *
C       *******************************************************
C
C
3000  CONTINUE
C
C
C -----  BY MEANS OF PRESSURE  -------------------------------------
C
C
         DO 3100 I=1,3
         IND = 44 + (I-1)*7
         IF(KINT(IND) .NE. 77 .AND. KINT(IND) .NE. 66) GO TO 3100
         IF(KINT(IND + 1) .EQ. 999) GO TO 3200
C
         IPOS = 52 + (I-1)*5
         IF(KINT(IND+1) .NE. MINDIC)
     C      THEN
               KDEC(IPOS) = KINT(IND+1)
               CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     C                    KDEC(IPOS+1),KDEC(IPOS+2))
C
               IF(KINT(IND+4) .EQ. 4)
     C            THEN
                     KDEC(IPOS+3) = KINT(IND + 5)
                     KDEC(IPOS+4) = KINT(IND + 6)
                  END IF
C
               IF(  ( KDEC(IPOS+1) .EQ. MINDIC .OR.
     C                KDEC(IPOS+2) .EQ. MINDIC   )
     C                       .AND.
     C              ( KDEC(IPOS+3) .EQ. MINDIC .OR.
     C                KDEC(IPOS+4) .EQ. MINDIC   )   )
     C                KDEC(IPOS) = MINDIC
C
            END IF
3100     CONTINUE
C
C
3200     CONTINUE
C
C
C -----  BY MEANS OF GEOPOTENTIAL  --------------------------------
C
         DO 3500 I=1,3
C
         IND = 65 + (I-1)*7
         IF(KINT(IND) .NE. 7 .AND. KINT(IND) .NE. 6) GO TO 3500
         IF(KINT(IND + 1) .EQ. 999) GO TO 3600
C
         IPOS = 67 + (I-1)*5
         IF(KINT(IND+1) .NE. MINDIC)
     C      THEN
               KDEC(IPOS) = KINT(IND+1) * 10
C
               CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     C                    KDEC(IPOS+1),KDEC(IPOS+2))
C
               IF(KINT(IND+4) .EQ. 4)
     C            THEN
                     KDEC(IPOS+3) = KINT(IND + 5)
                     KDEC(IPOS+4) = KINT(IND + 6)
                  END IF
C
               IF(  ( KDEC(IPOS+1) .EQ. MINDIC .OR.
     C                KDEC(IPOS+2) .EQ. MINDIC   )
     C                       .AND.
     C              ( KDEC(IPOS+3) .EQ. MINDIC .OR.
     C                KDEC(IPOS+4) .EQ. MINDIC   )   )
     C                KDEC(IPOS) = MINDIC
C
            END IF
3500     CONTINUE
C
C
C
3600     CONTINUE
C
C
C       ***********************************************
C       *                 SECTION 9                   *
C       *                                             *
C       *              (REGIONAL DATA)                *
C       ***********************************************
C
C
C ------  NO REGIONAL DATA HANDLED  -------------------------------
C
9900  CONTINUE
C
C
C
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT (I.E. IF
C     THERE ARE NO DATA AND NO CODING ERRORS)
C
      NIL2 = 1
      DO 460 I=25,KDEC(24)
      IF(KDEC(I) .EQ. MINDIC) GO TO 460
      NIL2 = 0
      GO TO 465
460   CONTINUE
C
465   CONTINUE
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 99800
         END IF
C
C
C
C==========================================================
C====    PRINTING OF ERRORS & REPORTS & KINT & KDEC    ====
C==========================================================
C
C
      IPRI = 0
      IPRIALL = 0
C
      IS1 = KINT(12) - 48
      IS2 = KINT(13) - 48
      IS3 = KINT(14) - 48
      IS4 = KINT(15) - 48
      IS5 = KINT(16) - 48
C
      IBLOK = 10*IS1 + IS2
      ISTA = 10000*IS1 + 1000*IS2 + 100*IS3 + 10*IS4 + IS5
C
      DO 700 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
700   CONTINUE
C
      DO 710 I=1,100
      IF(ISTA .EQ. ISTASA(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
710   CONTINUE
C
C
C     PRINT THE ERRONEUS SHIPS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERSHIP .GT. 0)
     C   THEN
            IF(KDEC(4) .GE. 36 .AND. KDEC(20) .GT. 1) IPRI = 1
         END IF
C
C     PRINT ALL ERRONEUS REPORTS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERALL .GT. 0)
     C   THEN
            IPRI = 1
         END IF
C
      IF(IERALLC .GT. 0) IPRI = 1
C
      IF(KDEC(2) .LT. ITIMES(1) .OR. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
C
      IF(IPRALLC .EQ. 1)
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
C
C
      IF(IPRNUM .GT. IPRLIM) GO TO 99800
C
      IPRI = 0
C
      IF(IPRI .GT. 0)
     C   THEN
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IPRNUM=IPRNUM+1
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        IPRNUM=IPRNUM+1
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKINT(KINT,1,600,MINDIC)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
C
10000 FORMAT(1H ,///)
20000 FORMAT(1H ,/////2X,' PART C  NUMBER =',I5,/)
C
C
C
C
C
C
C
C
C
99800 CONTINUE
C
c      CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
      CALL PILOEXP( 3,IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UHDEC - after piloexp, ierr ',IERR
CTEST
      CALL MRGPIL( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UHDEC - after mrgpil, ierr ',IERR
CTEST
      CALL QCPILOT( kdec(4),IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UHDEC - after qcpilot, ierr ',IERR
CTEST
      CALL BUFRENC( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UHDEC - after bufrenc, ierr ',IERR
CTEST
      IERR=0
C
99900 CONTINUE
      RETURN
      END
      SUBROUTINE UHINT ( IHEAD,IERR )
C
C**** *UHINT*
C
C
C     PURPOSE.
C     --------
C
C          CONVERT PILOT REPORTS PART C FROM CCITT 5 CHARACTER
C          FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UHINT ( IHEAD,IERR )*
C
C          INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C                     IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                               WRITTEN TO ERROR FILE.
C                           = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                     IERR NOT USED ON INPUT.
C
C         OUTPUT    : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                     DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION ).
C
C
C     METHOD.
C     -------
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING
C         IS PROCEEDED UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON-FATAL ERROR HAS BEEN DETECTED THE
C         ROUTINE TRIES TO FIND THE BEGINNING OF CORRECT
C         DATA.  IF CORRECT DATA ARE FOUND THE DECODING
C         THE NUMBER OF NON-FATAL ERRORS IS COUNTED AND
C         IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE
C         DATA AFTER THE FIRST ERROR ARE REMOVED (THIS
C         IS DONE TO ENSURE THAT TOTALLY CORRUPTED DATA
C         ARE NOT USED E.G. WRONG PART OF PILOT).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C         NUMERR = NUMBER OF NON-FATAL ERRORS
C         IREMPOS= THE POINTER TO FIRST ERROR IN KINT.
C
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
C          A. HOLOPAINEN  NOVEMBER  -84.
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'printre.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
C     ------------------------------------------------------------------
C           1. CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C              -------------------------------------------------------
C              CHARACTER OF REPORT.
C              ---------------------
 100  CONTINUE
C
C
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      MODIFY = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR = 0
      INDH = 0
      INDP = 0
      NUMSTAN = 0
      IREMPOS = 17
C
C
C***
C*    CHECK FOR 'NIL' REPORT.
C***
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.25 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(9,0,IFIRST,NUMERR)
             KERR = 0
         END IF
C
C
C
C***
C*     SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C***
C
      DO 200 I=1,600
         KINT(I) = MINDIC
  200 CONTINUE
C
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 300 I=12,14
         KINT(I) = 88
  300 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C***
C*    CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C***
C
      DO 400 I=1,3
         KDEC(I) = MINDIC
  400 CONTINUE
C
      DO 500 I=5,9
         KDEC(I) = MINDIC
  500 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = IAND(KDEC(15),4)
C
      KDEC(17) = MINDIC
C
      KDEC(22) = MINDIC
C
C
C
1000  CONTINUE
C
C
C   ***********************************************************
C   *                      SECTION 1                          *
C   *                                                         *
C   *     (IDENTIFICATION AND POSITION, THIS IS DIFFERENT     *
C   *      FOR LAND STATIONS AND SHIP REPORTS)                *
C   ***********************************************************
C
C
C               YY GG /
C               -- -- -
C
C
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*10000)
                CALL NEXPRT2(IPT,IEQ,*10000)
                IF(KCHAR(IPT).GE.65) THEN
                                        GO TO 1011
                                     END IF
C
C               FIRST FIGURE OF THE GROUP MUST BE 0-3
C               OR 5-8 IF WIND SPEED IS GIVEN IN KNOTS,
C               OTHERWISE YYGGID GROUP IS MISSING
C
C
                IF ((KCHAR(IPT) .GE. 48) .AND. (KCHAR(IPT) .NE. 52)
     1                .AND. (KCHAR(IPT) .LE. 56))
     2                 CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
                IF(KINT(1).LE.0.AND.KINT(1).GT.31) THEN
                   KERR=1
                   GO TO 10000
                END IF
C
                JPT = IPT
C
C
C
C
  700 IF ( KDEC(4).EQ.32 )
     C      THEN
C
C               LAND STATION
C
C
C
C               IIIII     -   RETAIN POINTER TO IDENTIFIER
C               -----
C
C
                CALL NEXPRT2( IPT,IEQ,*701)
                GO TO 702
C
 701            CONTINUE
C      
                KERR=2
                GO TO 10000
C
 702            CONTINUE
C   
                KPT = IPT
                ID = 5
                CALL EXTINT ( IPT,5,4 )
C
C               EXTRACT STATION LAT/LONG ETC FROM WMO MASTER FILE.
C
                CALL LOCSTAT ( IWIND,IRET )
C
C
C               CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                IF(IRET .eq. 0)
     C              THEN
                       IF(NIL .EQ. 1)
     C                    THEN
                             KDEC(20)=1
                             kerr=1
                             RETURN
                          END IF  
                    END IF
C
                IF ( IRET.EQ.1 )
     C              THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        CALL ERRSTA(9,1,IFIRST,NUMERR)
C
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
                        WRITE (*,9901) KINT(4)
               OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_pilo_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C
                        WRITE (55,9901) ctstamp,KINT(4)
                        CLOSE(55)
                        WRITE (*,9901) ctstamp,KINT(4)
cpsokt98 9901                FORMAT (1H ,a8,' PILO NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' PILO NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        GO TO 10000
                    END IF
C
                IF ( IRET.EQ.2 )
     C             THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2

                        CALL ERRSTA(9,2,IFIRST,NUMERR)
C
                        GO TO 10000
                    END IF
C
C
                 IF(IRET .EQ. 3)
     1              THEN
C
C                     INVALID OBSERVATION TYPE
C
                      KERR = 3
                      CALL ERRSTA(9,2,IFIRST,NUMERR)
C
                      GO TO 10000
                   END IF
C
            ELSE
C
C                PILOT SHIP
C
                 IF(KDEC(4) .EQ. 33)
     1              THEN
C
                        KINT(4)=0
C
C
C
C                       RETAIN POINTER TO STATION IDENTIFIER .
C
                        KPT=IMI
C
C
C                       CHECK THE CALL SIGN
C
                        IBUOY = 0
                        K = IMI
                        J = K + 4
C
                        DO 750 I=K,J
                        IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750                   CONTINUE
C
C
C                       IF A LETTER GROUP IS FOUND CHECK THAT THIS IS
C                       NOT MIMIMJMJ GROUP 'QQBB'. IN THIS CASE THE SHIP
C                       CALL SIGN GROUP IS PRESUMED MISSING.
C
                        IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85)
     C                  .AND.
     C                  (KCHAR(K+2) .EQ. 66) .AND. (KCHAR(K+3) .EQ. 66))
     C                      THEN
                               IBUOY = 2
                            END IF
C
C
                        IF(IBUOY .NE. 0)
     C                     THEN
C
C                             SHIP,RIG OR PLATFORM. IDENTIFIER
C                             NUMBER SET TO 0.
C
C
                         ID=0
                              KINT(4) = 0
C
                              IF (IBUOY.EQ.1)
     C                           THEN
C
C                                   MOVE POINTER PAST CALL SIGN . THE
C                                   FIRST CHARACTER OF THE NEXT GROUP IS
C                                   LOCATED AS SOME SHIPS USE THE FULL
C                                   NAME ( WHICH CAN CONSIST OF 2 WORDS
C                                   INSTEAD OF CALL SIGN. SO IF THE NEXT
C                                   GROUP DOESN'T START WITH 'QQ' (WHICH
C                                   IS THE BEGINNING OF MIMIMJMJ GROUP)
C                                   IT MUST BE PART OF THE SHIP'S NAME
C                                   USED INSTEAD OF CALL SIGN.
C
                                    IIPT=K
                                    CALL NEXSEP2(IIPT,IEQ,*10000)
                                    ID = IIPT - KPT
                                    CALL NEXTLET (IIPT,IEQ)
C
                                    IF(KCHAR(IIPT) .NE. 81 .OR.
     C                                 KCHAR(IIPT+1) .NE. 81)
     C                                 THEN
                                          CALL NEXSEP2(IIPT,IEQ,*10000)
                                          ID = IIPT - K
                                       END IF
C
C
                         END IF
                       IF(IBUOY.EQ.2)
     1                    THEN
C
C                            SHIP CALL SIGN FOLLOWS MIMIMJMJ
C
                             IIPT=K
C
C                            SKIP MIMIMJMJ GROUP
C
                             CALL NEXSEP2(IIPT,IEQ,*10000)
                             CALL NEXPRT2(IIPT,IEQ,*10000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*10000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*10000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*10000)
                                       ID=IIPT-K
                                    END IF
                              END IF
                          END IF
C
                   END IF
C
C
C                       99 LALALA
C                       -- ------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                       CHECK VALIDITY OF LALALA
C
                        IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR = 4
                              CALL ERRSTA(9,1,IFIRST,NUMERR)
                              GO TO 10000
                           END IF
C
C                       QC LOLOLOLO
C                       -- --------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )
C
C                       CHECK VALIDITY OF LOLOLOLO
C
                        IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C                          KINT(7).NE.5.AND.KINT(7).NE.7) IRET=1
                        IF (KINT(8).LT.0.OR.KINT(8).GT.1800)IRET=1
                        IF (IRET.NE.0)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR=4
                              IF (IFIRST.EQ.0)
     C                           THEN
                                    CALL ERRSTA(9,2,IFIRST,NUMERR)
                                 END IF
                              GO TO 10000
                           END IF
C
C                       MMM ULA ULO
C                       --- --- ---
C
                        CALL NEXPRT2(IPT,IEQ,*10000)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                               LAT,LON)
                        CALL MARDSEN(LAT,LON,KINT(9),IRET)
C
                        IF(IRET .NE. 0)
     1                      THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
C    --------------            WRONG CODE FOR MARSDEN SQUARE    ------
                               KERR = 4
                               CALL ERRSTA(9,1,IFIRST,NUMERR)
C
                               GO TO 10000
C
                            END IF
C
                    ELSE
C
C                       THIS IS NOT A PILOT REPORT
C
                        KERR = 3
                        CALL ERRSTA(9,1,IFIRST,NUMERR)
C
                        GO TO 10000
                END IF
             END IF
C
C
C     RETAIN STATION IDENTIFIER IN CHARACTER FORM ( IF ANY EXISTS )
C
      IF ( ID.NE.0 )
     C    THEN
              IF (ID.GT.5) ID = 5
              J = KPT
              K = 12 + ID -1
              DO 800 I=12,K
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C
C
C
C     CHECK VALUES OF YY GG FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4) .EQ. 33 )
     1     THEN
               IRET = 0
               IF ((KINT(1) .LT. 1) .OR. ((KINT(1) .GT. 31) .AND.
     2             (KINT(1) .LT. 51)) .OR. (KINT(1) .GT. 81))
     3             THEN
                       IRET = 2
                       KINT(1) = MINDIC
                   END IF
               IF (KINT(2).LT.0.OR.KINT(2).GT.23)
     C             THEN
                       IRET = 2
                       KINT(2) = MINDIC
                   END IF
C
               IF (IRET.NE.0)
     C             THEN
                       JPT=IABS(JPT)
                       KCHAR(JPT)=IOR(KCHAR(JPT),128)
                       KERR = 3
                       CALL ERRSTA(9,1,IFIRST,NUMERR)
                       GO TO 10000
C
                   END IF
           END IF
C
C
C
C
C     'NIL' REPORTS ARE NOT PROSESSED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
C
C
      IPOS = 17
C
C
2000  CONTINUE
C
C   ************************************************
C   *                SECTION 2                     *
C   *                                              *
C   *         (STANDARD ISOBARIC SURFACES)         *
C   ************************************************
C
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTVAL(IPT,2,IVAL)
C
      IF(IVAL .EQ. 44 .OR. IVAL .EQ. 55)
     C   THEN
C
C            44NP1P1 OR 55NP1P1  ....... NUMBER OF LEVELS AND THE
C            -------    -------  ....... PRESSURE ON FIRST ONE
C
            NUMSTAN = NUMSTAN + 1
            IPOS = 17 + (NUMSTAN-1)*9
C
C
            CALL EXTGRP(IPT,2,1,2,0,0,IPOS,IRET)
C
            IF(IRET .NE. 0)
     C         THEN
                  KERR = 5
                  CALL ERRSTA(9,2,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  CALL NEXSEP2(IPT,IEQ,*10000)
                  CALL NEXPRT2(IPT,IEQ,*10000)
                  GO TO 3000
               END IF
C
C
C           DD FFF   .......  WIND DIRECTION & SPEED
C           -- ---
C
            N = KINT(IPOS+1)
            IF(N .EQ. MINDIC .OR. N .EQ. 0) GO TO 3000
C
C
C
               DO 2500 I=1,N
               CALL NEXTPRT(IPT,IEQ)
               IF(IPT .GE. IEQ)
     C            THEN
                     KERR = 7
                     CALL ERRSTA(9,1,IFIRST,NUMERR)
                     GO TO 3000
                  END IF
C
               CALL EXTVAL(IPT,2,IVAL)
               IF(IVAL .EQ. 44 .OR. IVAL .EQ. 55 .OR. IVAL .EQ. 66 .OR.
     C           IVAL .EQ. 77) GO TO 3000
C
               IPOS2 = IPOS + 3 + (I-1)*2
C
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS2,IRET)
C
               IF ( (IRET .NE. 0)  .OR. (KINT(IPOS2) .GT. 36 .AND.
     C            KINT(IPOS2) .NE. MINDIC) )
     C               THEN
                        KERR = 7
C
                        CALL ERRSTA(9,1,IFIRST,NUMERR)
                        IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                        DO 2400 J = IPOS2,IPOS2+1
                        KINT(J) = MINDIC
2400                    CONTINUE
                        CALL NEXSEP2(IPT,IEQ,*10000)
                        GO TO 2000
                     END IF
C
2500           CONTINUE
C
         END IF
C
C
C
C
C
C
C
3000  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTVAL(IPT,2,IVAL)
      CALL EXTVAL(IPT,1,IVAL2)
C
      IF(IVAL .EQ. 44 .OR. IVAL .EQ. 55) GO TO 2000
      IF(IVAL .EQ. 66 .OR. IVAL .EQ. 77) GO TO 4000
      IF(IVAL2 .EQ. 6 .OR. IVAL2 .EQ. 7) GO TO 4500
C
      KERR = 5
      CALL ERRSTA(9,1,IFIRST,NUMERR)
      IREM = MAX0(IPOS,17)
      IF(NUMERR .EQ. 1) IREMPOS = IREM
C
      CALL NEXSEP2(IPT,IEQ,*10000)
      GO TO 3000
C
C
C
C
4000  CONTINUE
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 77999) GO TO 10000
C
C
C   ***************************************************
C   *                  SECTION 8                      *
C   *                                                 *
C   *     (MAX. WIND LEVELS & VERTICAL WIND SHEAR)    *
C   ***************************************************
C
      INDP = INDP + 1
C
C
C     77 PMPMPM   ......  PRESSURE AT THE LEVEL
C     -- ------
C
      IPOS = 44 + (INDP-1)*7
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
      IF(IRET .NE. 0)
     C    THEN
             KERR = 12
             CALL ERRSTA(9,1,IFIRST,NUMERR)
             IF(NUMERR .EQ. 1) IREMPOS = 558
             CALL NEXSEP2(IPT,IEQ,*10000)
             CALL NEXPRT2(IPT,IEQ,*10000)
             GO TO 3000
          END IF
C
C
C     DMDM FMFMFM   ......  WIND DIRECTION & SPEED
C     ---- ------
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
      IF(IRET .NE. 0 .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C                     KINT(IPOS+2) .NE. MINDIC))
     C    THEN
             KERR = 13
             CALL ERRSTA(9,1,IFIRST,NUMERR)
             IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
             CALL NEXSEP2(IPT,IEQ,*10000)
             CALL NEXPRT2(IPT,IEQ,*10000)
             GO TO 3000
          END IF
C
C
C     4 VBVB VAVA   ......  VERTICAL WIND SHEAR
C     - ---- ----
C
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTVAL(IPT,1,IVAL)
      IF(IVAL .EQ. 4)
     C   THEN
            CALL EXTGRP(IPT,1,2,2,0,0,IPOS+4,IRET)
            IF(IRET .NE. 0)
     C          THEN
                   KERR = 14
                   CALL ERRSTA(9,1,IFIRST,NUMERR)
                   IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                   CALL NEXSEP2(IPT,IEQ,*10000)
                   CALL NEXPRT2(IPT,IEQ,*10000)
                   GO TO 3000
                END IF
          END IF
C
      GO TO 3000
C
C
C
C
4500  CONTINUE
C
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 77999) GO TO 10000
C
C
      INDH = INDH + 1
      IPOS = 65 + (INDH-1)*7
C
C
C     7HMHMHM  OR  6HMHMHM   ......  GEOPOTENTIAL AT THE LEVEL
C     -------      -------
C
C
      CALL EXTGRP(IPT,1,4,0,0,0,IPOS,IRET)
      IF(IRET .NE. 0)
     C    THEN
             KERR = 12
             CALL ERRSTA(9,1,IFIRST,NUMERR)
             IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
             CALL NEXSEP2(IPT,IEQ,*10000)
             CALL NEXPRT2(IPT,IEQ,*10000)
             GO TO 3000
          END IF
C
C
C     DMDM FMFMFM   ......  WIND IDRECTION & SPEED AT THE LEVEL
C     ---- ------
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
      IF ( (IRET .NE. 0)  .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C      KINT(IPOS+2) .NE. MINDIC) )
     C         THEN
                  KERR = 13
                  CALL ERRSTA(9,1,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  DO 4600 I = IPOS,IPOS+3
                  KINT(I) = MINDIC
4600              CONTINUE
                  CALL NEXSEP2(IPT,IEQ,*10000)
C
                  GO TO 3000
               END IF
C
C
C     4 VBVB VAVA   ......  VERTICAL WIND SHEAR AT THE LEVEL
C     - ---- ----
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTVAL(IPT,1,IVAL)
      IF(IVAL .EQ. 4)
     C   THEN
            CALL EXTGRP(IPT,1,2,2,0,0,IPOS+4,IRET)
            IF(IRET .NE. 0)
     C          THEN
                   KERR = 9
                   CALL ERRSTA(9,1,IFIRST,NUMERR)
                   IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                   CALL NEXSEP2(IPT,IEQ,*10000)
                   CALL NEXPRT2(IPT,IEQ,*10000)
                   GO TO 3000
                END IF
          END IF
C
C
      GO TO 3000
C
C
C
C
C
C
10000 CONTINUE
C
C
      IF(KERR .EQ. 0) RETURN
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
C
C
                             IPT = KEEP
                             CALL FIXPILO
                             IFIRST=1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            ONLY REPORTS WITH ERROR IN DATA/TIME
C                            OR LAT/LONG ARE NOT PROSESSED ANY FURTHER
C
C                            IF THERE ARE MORE THAN 6 NON-FATAL ERRORS
C                            IN REPORT IT IS ONLY PROSESSED UNTIL THE
C                            FIRST ERROR
C
                             IF(NUMERR .GT. 6)
     C                          THEN
                                   DO 6000 I = IREMPOS,600
                                   KINT(I) = MINDIC
6000                               CONTINUE
                                END IF
C
                             IF(KERR .GT. 4) KERR = 0
                          END IF
C
C
C
      RETURN
      END
      SUBROUTINE UPDEC ( IHEAD,IERR )
C
C**** *UPDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE PILOT REPORT PART A FROM INTERMEDIATE
C         FORMAT TO FULL DECODED FORMAT.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UPDEC ( IHEAD,IERR )*
C            INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'.
C
C            OUTPUT   : REPORT WRITTEN TO DECODED REPORTS FILE.
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
C          A. HOLOPAINEN  MAY -84
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'printre.f'
C
      DIMENSION LEVSTAN(9)
C
      DATA LEVSTAN / 85, 70, 50, 40, 30, 25, 20, 15, 10 /
C
C
      DATA IROU /0/
      DATA IPRNUM /0/
C     ------------------------------------------------------------------
C*          1.   DEFINE INDICATORS AND FLAFS.
C                ----------------------------
 100  CONTINUE
C
C
C
C     DEFINE 'NIL' INDICATOR
C
      NIL = IAND(KDEC(20),1)
C
C
C     CLEAR ERROR INDICATOR
C
      KERR = 0
C
C
C
C
C***
C*    SET DECODED REPORT TO MISSING DATA VALUE
C***
C
      DO 1 I=25,600
      KDEC(I)=MINDIC
1     CONTINUE
C
C
C
C       *************************************
C       *                                   *
C       *             SECTION 1             *
C       *                                   *
C       *   (IDENTIFICATION AND POSITION)   *
C       *************************************
C
C
C
C     REPORT HEADER
C
C     IN THIS SUBROUTINE ONLY THE FOLLOWING PARTS
C     OF THE HEADER WILL BE DECODED
C        - DAY OF THE MONTH
C        - TIME (HOUR & MIN) FROM REPORT
C        - IDENTIFIER
C        - LENGTH OF THE REPORT
C     AND IN CASE OF TEMP SHIP OR TEMP DROP
C        - LAT &LONG
C        - ALT = 0 (FOR SHIP)
C
C
C
C
C***   DAY AND TIME
C
C
      IF(KINT(2).LT.0.OR.KINT(2).GT.23) THEN
                                           RETURN
                                        END IF  
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C
C***
C*    CHECK IF SHIP PILOT
C***
C
      IF(KDEC(4) .EQ. 33)
     1           THEN
C
C***                LAT & LONG
C
                    CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1                          KDEC(5),KDEC(6))
                    IF(KDEC(5).EQ.MINDIC.OR.KDEC(6).EQ.MINDIC) RETURN
C
C***                ALTIT=0, FOR SEA STATION
C
                    KDEC(8)=0
C
                 END IF
C
C
C
C
C
101   CONTINUE
C
C***  IDENT   STATION/SHIP IDENTIFICATION
C
C
      KDEC(7)=88
C
C
C***  INSTRUMENT SPECCIFICATION
C
      kdec(13)=9
      if(kint(3).ne.mindic) kdec(13)=kint(3)
C
C
C
C***  REPORT LENGTH
C
C
      KDEC(24)=384
C
C
C
      K=MINDIC
      IF(KDEC(4) .LT. 21) K=KDEC(17)*1000+KDEC(16)
C
C     CHECK IF WIND SPEED IS GIVEN IN KNOTS, IN
C     THIS CASE 50 HAS BEEN ADDED TO THE DATE YY.
C
      IWW=0
      IF(KDEC(1) .GT. 31 .AND. KDEC(1) .LE. 81)
     1             THEN
                      IWW=1
                      KDEC(1)=KDEC(1)-50
                   END IF
C
C
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LE. 0 .OR. KDEC(1) .GT. 31) RETURN
      IF(KDEC(2) .LT. 0 .OR. KDEC(2) .GT. 24) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) GO TO 99800
C
      IROU = IROU +1
C
C
C
2000  CONTINUE
C
C
C       ************************************************
C       *                                              *
C       *                SECTION 2                     *
C       *                                              *
C       *       (STANDARD ISOBARIC SURFACES)           *
C       ************************************************
C
C
         DO 2500 I = 1,3
         IND = 17 + (I-1)*9
         IF(KINT(IND) .EQ. 44 .OR. KINT(IND) .EQ. 55)
     C      THEN
C
C              NUMBER OF LEVELS IN THIS GROUP
C
               NUMBER = KINT(IND+1)
C
C              GET THE FIRST LEVEL OF THE GROUP
C
                  DO 2100 J=1,9
                  IF(KINT(IND+2) .EQ. LEVSTAN(J))
     C               THEN
                        INDLEV = J
                        IPOS = 25 + (J-1)*3
                        GO TO 2200
                     END IF
2100              CONTINUE
C
               GO TO 2500
C
2200           CONTINUE
C
                  DO 2300 J = 1,NUMBER
                  IF( (INDLEV+J-1) .GT. 9) GO TO 2500
                  IND2 = IND + 3 + (J-1)*2
                  KDEC(IPOS) = LEVSTAN(INDLEV+J-1) * 100
                  CALL DDFFF(KINT(IND2),KINT(IND2+1),IWW,K,MINDIC,
     C                       KDEC(IPOS+1),KDEC(IPOS+2))
                  IF(KDEC(IPOS+1) .EQ. MINDIC .OR. KDEC(IPOS+2) .EQ.
     C               MINDIC) KDEC(IPOS) = MINDIC
                  IPOS = IPOS + 3
2300              CONTINUE
C
            END IF
C
2500     CONTINUE
C
C
C
C       *******************************************************
C       *                                                     *
C       *                     SECTION 3                       *
C       *                                                     *
C       *   (MAXIMUM WIND LEVEL(S) AND VERTICAL WIND SHEAR)   *
C       *******************************************************
C
C
3000  CONTINUE
C
C
C -----  BY MEANS OF PRESSURE  -------------------------------------
C
C
         DO 3100 I=1,3
         IND = 44 + (I-1)*7
         IF(KINT(IND) .NE. 77 .AND. KINT(IND) .NE. 66) GO TO 3100
         IF(KINT(IND + 1) .EQ. 999) GO TO 3200
C
         IPOS = 52 + (I-1)*5
         IF(KINT(IND+1) .NE. MINDIC)
     C      THEN
               KDEC(IPOS) = KINT(IND+1) * 10
               CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     C                    KDEC(IPOS+1),KDEC(IPOS+2))
C
               IF(KINT(IND+4) .EQ. 4)
     C            THEN
                     KDEC(IPOS+3) = KINT(IND + 5)
                     KDEC(IPOS+4) = KINT(IND + 6)
                  END IF
C
               IF(  ( KDEC(IPOS+1) .EQ. MINDIC .OR.
     C                KDEC(IPOS+2) .EQ. MINDIC   )
     C                       .AND.
     C              ( KDEC(IPOS+3) .EQ. MINDIC .OR.
     C                KDEC(IPOS+4) .EQ. MINDIC   )   )
     C                KDEC(IPOS) = MINDIC
C
            END IF
3100     CONTINUE
C
C
3200     CONTINUE
C
C
C -----  BY MEANS OF GEOPOTENTIAL  --------------------------------
C
         DO 3500 I=1,3
C
         IND = 65 + (I-1)*7
         IF(KINT(IND) .NE. 7 .AND. KINT(IND) .NE. 6) GO TO 3500
         IF(KINT(IND + 1) .EQ. 999) GO TO 3600
C
         IPOS = 67 + (I-1)*5
         IF(KINT(IND+1) .NE. MINDIC)
     C      THEN
               KDEC(IPOS) = KINT(IND+1) * 10
C
               CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     C                    KDEC(IPOS+1),KDEC(IPOS+2))
C
               IF(KINT(IND+4) .EQ. 4)
     C            THEN
                     KDEC(IPOS+3) = KINT(IND + 5)
                     KDEC(IPOS+4) = KINT(IND + 6)
                  END IF
C
               IF(  ( KDEC(IPOS+1) .EQ. MINDIC .OR.
     C                KDEC(IPOS+2) .EQ. MINDIC   )
     C                       .AND.
     C              ( KDEC(IPOS+3) .EQ. MINDIC .OR.
     C                KDEC(IPOS+4) .EQ. MINDIC   )   )
     C                KDEC(IPOS) = MINDIC
C
            END IF
3500     CONTINUE
C
C
C
3600     CONTINUE
C
C
C       ***********************************************
C       *                 SECTION 9                   *
C       *                                             *
C       *              (REGIONAL DATA)                *
C       ***********************************************
C
C
C ------  NO REGIONAL DATA HANDLED  -------------------------------
C
9900  CONTINUE
C
C
C
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT (I.E. IF
C     THERE ARE NO DATA AND NO CODING ERRORS)
C
      NIL2 = 1
      DO 460 I=25,KDEC(24)
      IF(KDEC(I) .EQ. MINDIC) GO TO 460
      NIL2 = 0
      GO TO 465
460   CONTINUE
C
465   CONTINUE
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 99800
         END IF
C
C
C
C==========================================================
C====    PRINTING OF ERRORS & REPORTS & KINT & KDEC    ====
C==========================================================
C
C
      IPRI = 0
      IPRIALL = 0
C
      IS1 = KINT(12) - 48
      IS2 = KINT(13) - 48
      IS3 = KINT(14) - 48
      IS4 = KINT(15) - 48
      IS5 = KINT(16) - 48
C
      IBLOK = 10*IS1 + IS2
      ISTA = 10000*IS1 + 1000*IS2 + 100*IS3 + 10*IS4 + IS5
C
      DO 700 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
700   CONTINUE
C
      DO 710 I=1,100
      IF(ISTA .EQ. ISTASA(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
710   CONTINUE
C
C
C     PRINT THE ERRONEUS SHIPS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERSHIP .GT. 0)
     C   THEN
            IF(KDEC(4) .GE. 36 .AND. KDEC(20) .GT. 1) IPRI = 1
         END IF
C
C     PRINT ALL ERRONEUS REPORTS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERALL .GT. 0)
     C   THEN
            IPRI = 1
         END IF
C
      IF(IERALLA .GT. 0) IPRI = 1
C
      IF(KDEC(2) .LT. ITIMES(1) .OR. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
C
      IF(IPRALLA .EQ. 1)
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
C
C
      IF(IPRNUM .GT. IPRLIM) GO TO 99800
C
      IPRI = 0
C
      IF(IPRI .GT. 0)
     C   THEN
C
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IPRNUM=IPRNUM+1
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        IPRNUM=IPRNUM+1
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKINT(KINT,1,600,MINDIC)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
C
10000 FORMAT(1H ,///)
20000 FORMAT(1H ,/////2X,' PART A  NUMBER =',I5,/)
C
C
C
C
C
C
C
C
C
99800 CONTINUE
C
c      CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
      CALL PILOEXP( 1,IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UPDEC - after piloexp, ierr ',IERR
CTEST
      CALL MRGPIL( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UPDEC - after mrgpil, ierr ',IERR
CTEST
      CALL QCPILOT( kdec(4),IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UPDEC - after qcpilot, ierr ',IERR
CTEST
      CALL BUFRENC( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UPDEC - after bufrenc, ierr ',IERR
CTEST
      IERR=0
C
99900 CONTINUE
      RETURN
      END
      SUBROUTINE UPINT(IHEAD,IERR)
C
C**** *UPINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT PILOT REPORTS PART A FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UPINT ( IHEAD,IERR )*
C
C          INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C                      IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                                WRITTEN TO ERROR FILE.
C                            = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                      IERR NOT USED ON INPUT.
C
C           OUTPUT    : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                       DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION ).
C
C
C     METHOD.
C     -------
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING
C         IS PROCEEDED UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON-FATAL ERROR HAS BEEN DETECTED THE
C         ROUTINE TRIES TO FIND THE BEGINNING OF CORRECT
C         DATA.  IF CORRECT DATA ARE FOUND THE DECODING
C         THE NUMBER OF NON-FATAL ERRORS IS COUNTED AND
C         IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE
C         DATA AFTER THE FIRST ERROR ARE REMOVED (THIS
C         IS DONE TO ENSURE THAT TOTALLY CORRUPTED DATA
C         ARE NOT USED E.G. WRONG PART OF PILOT).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C         NUMERR = NUMBER OF NON-FATAL ERRORS
C         IREMPOS= THE POINTER TO FIRST ERROR IN KINT.
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
C
C          A. HOLOPAINEN  NOVEMBER -84.
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'printre.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
      DATA IROU /0/
C
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C              -------------------------------------------------------
C              CHARACTER OF REPORT.
C              --------------------
 100  CONTINUE
C
C
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      MODIFY = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR = 0
      INDH = 0
      INDP = 0
      NUMSTAN = 0
      IREMPOS = 17
      IROU = IROU + 1
C
C
C***
C*    CHECK FOR 'NIL' REPORT.
C***
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.25 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(7,0,IFIRST,NUMERR)
             KERR = 0
         END IF
C
C
C
C***
C*     SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C***
C
      DO 200 I=1,600
         KINT(I) = MINDIC
  200 CONTINUE
C
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 300 I=12,14
         KINT(I) = 88
  300 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C***
C*    CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C***
C
      DO 400 I=1,3
         KDEC(I) = MINDIC
  400 CONTINUE
C
      DO 500 I=5,9
         KDEC(I) = MINDIC
  500 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = IAND(KDEC(15),4)
C
      KDEC(17) = MINDIC
C
C
      KDEC(22) = MINDIC
C
C
1000  CONTINUE
C
C
C   ***********************************************************
C   *                      SECTION 1                          *
C   *                                                         *
C   *     (IDENTIFICATION AND POSITION, THIS IS DIFFERENT     *
C   *      FOR LAND STATIONS AND SHIP REPORTS)                *
C   ***********************************************************
C
C
C               YY GG /
C               -- -- -
C
C
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*10000)
                CALL NEXPRT2(IPT,IEQ,*10000)
                IF(KCHAR(IPT).GE.65) THEN
                                        GO TO 1011
                                     END IF
C
C               FIRST FIGURE OF THE GROUP MUST BE 0-3
C               OR 5-8 IF WIND SPEED IS GIVEN IN KNOTS,
C               OTHERWISE YYGGID GROUP IS MISSING
C
C
                IF ((KCHAR(IPT) .GE. 48) .AND. (KCHAR(IPT) .NE. 52)
     1                .AND. (KCHAR(IPT) .LE. 56))
     2                 CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
c
                IF(KINT(1).LE.0.AND.KINT(1).GT.31) THEN
                   KERR=1
                   GO TO 10000
                END IF
C
                JPT = IPT
C
C
C
C
  700 IF ( KDEC(4).EQ.32 )
     C      THEN
C
C               LAND STATION
C
C
C
C               IIIII     -   RETAIN POINTER TO IDENTIFIER
C               -----
C
C
                CALL NEXPRT2( IPT,IEQ,*701)
                GO TO 702
C
 701            CONTINUE
C
                KERR=2
                GO TO 10000
C
 702            CONTINUE       
                KPT = IPT
                ID = 5
                CALL EXTINT ( IPT,5,4 )
C
C               EXTRACT STATION LAT/LONG ETC FROM WMO MASTER FILE.
C
                CALL LOCSTAT ( IWIND,IRET )
C
C
C               CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                IF(IRET .eq. 0)
     C              THEN
                       IF(NIL .EQ. 1)
     C                    THEN
                             KDEC(20)=1
                             kerr=1
                             RETURN
                          END IF               
                    END IF
C
                IF ( IRET.EQ.1 )
     C              THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        CALL ERRSTA(7,1,IFIRST,NUMERR)
C
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
               OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_pilo_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C
                        WRITE (55,9901) ctstamp,KINT(4)
                        CLOSE(55)
                        WRITE (*,9901) ctstamp,KINT(4)
cpsokt98 9901                FORMAT (1H ,a8,' PILO NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' PILO NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        GO TO 10000
                    END IF
C
                IF ( IRET.EQ.2 )
     C             THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2

                        CALL ERRSTA(7,2,IFIRST,NUMERR)
C
                        GO TO 10000
                    END IF
C
C
                 IF(IRET .EQ. 3)
     1              THEN
C
C                     INVALID OBSERVATION TYPE
C
                      KERR = 3
                      CALL ERRSTA(7,2,IFIRST,NUMERR)
C
                      GO TO 10000
                   END IF
C
            ELSE
C
C                PILOT SHIP
C
                 IF(KDEC(4) .EQ. 33)
     1              THEN
C
                        KINT(4)=0
C
C
C
C                       RETAIN POINTER TO STATION IDENTIFIER .
C
                        KPT=IMI
C
C
C                       CHECK THE CALL SIGN
C
                        IBUOY = 0
                        K = IMI
                        J = K + 4
C
                        DO 750 I=K,J
                        IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750                   CONTINUE
C
C
C                       IF A LETTER GROUP IS FOUND CHECK THAT THIS IS
C                       NOT MIMIMJMJ GROUP 'QQBB'. IN THIS CASE THE SHIP
C                       CALL SIGN GROUP IS PRESUMED MISSING.
C
                        IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85)
     C                  .AND.
     C                  (KCHAR(K+2) .EQ. 66) .AND. (KCHAR(K+3) .EQ. 66))
     C                      THEN
                               IBUOY = 2
                            END IF
C
C
                        IF(IBUOY .NE. 0)
     C                     THEN
C
C                             SHIP,RIG OR PLATFORM. IDENTIFIER
C                             NUMBER SET TO 0.
C
C
                         ID=0
                              KINT(4) = 0
C
                              IF (IBUOY.EQ.1)
     C                           THEN
C
C                                   MOVE POINTER PAST CALL SIGN . THE
C                                   FIRST CHARACTER OF THE NEXT GROUP IS
C                                   LOCATED AS SOME SHIPS USE THE FULL
C                                   NAME ( WHICH CAN CONSIST OF 2 WORDS
C                                   INSTEAD OF CALL SIGN. SO IF THE NEXT
C                                   GROUP DOESN'T START WITH 'QQ' (WHICH
C                                   IS THE BEGINNING OF MIMIMJMJ GROUP)
C                                   IT MUST BE PART OF THE SHIP'S NAME
C                                   USED INSTEAD OF CALL SIGN.
C
                                    IIPT=K
                                    CALL NEXSEP2(IIPT,IEQ,*10000)
                                    ID = IIPT - KPT
                                    CALL NEXTLET (IIPT,IEQ)
C
                                    IF(KCHAR(IIPT) .NE. 81 .OR.
     C                                 KCHAR(IIPT+1) .NE. 81)
     C                                 THEN
                                          CALL NEXSEP2(IIPT,IEQ,*10000)
                                          ID = IIPT - K
                       IF(IBUOY.EQ.2)
     1                    THEN
C
C                            SHIP CALL SIGN FOLLOWS MIMIMJMJ
C
                             IIPT=K
C
C                            SKIP MIMIMJMJ GROUP
C
                             CALL NEXSEP2(IIPT,IEQ,*10000)
                             CALL NEXPRT2(IIPT,IEQ,*10000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*10000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*10000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*10000)
                                       ID=IIPT-K
                                    END IF
                              END IF
                          END IF
                                       END IF
C
C
                         END IF
C
                   END IF
C
C
C                       99 LALALA
C                       -- ------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                       CHECK VALIDITY OF LALALA
C
                        IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR = 4
                              CALL ERRSTA(7,1,IFIRST,NUMERR)
                              GO TO 10000
                           END IF
C
C                       QC LOLOLOLO
C                       -- --------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )
C
C                       CHECK VALIDITY OF LOLOLOLO
C
                        IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C                          KINT(7).NE.5.AND.KINT(7).NE.7) IRET=1
                        IF (KINT(8).LT.0.OR.KINT(8).GT.1800)IRET=1
                        IF (IRET.NE.0)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR=4
                              IF (IFIRST.EQ.0)
     C                           THEN
                                    CALL ERRSTA(7,2,IFIRST,NUMERR)
                                 END IF
                              GO TO 10000
                           END IF
C
C                       MMM ULA ULO
C                       --- --- ---
C
                        CALL NEXPRT2(IPT,IEQ,*10000)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                               LAT,LON)
                        CALL MARDSEN(LAT,LON,KINT(9),IRET)
C
                        IF(IRET .NE. 0)
     1                      THEN
C
C    --------------            WRONG CODE FOR MARSDEN SQUARE    ------
                               KERR = 4
                               CALL ERRSTA(7,1,IFIRST,NUMERR)
C
                               GO TO 10000
C
                            END IF
C
                    ELSE
C
C                       THIS IS NOT A PILOT REPORT
C
                        KERR = 3
                        CALL ERRSTA(7,1,IFIRST,NUMERR)
C
                        GO TO 10000
                END IF
             END IF
C
C
C     RETAIN STATION IDENTIFIER IN CHARACTER FORM ( IF ANY EXISTS )
C
      IF ( ID.NE.0 )
     C    THEN
              IF (ID.GT.5) ID = 5
              J = KPT
              K = 12 + ID -1
              DO 800 I=12,K
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C
C
C
C     CHECK VALUES OF YY GG FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4) .EQ. 33 )
     1     THEN
               IRET = 0
               IF ((KINT(1) .LT. 1) .OR. ((KINT(1) .GT. 31) .AND.
     2             (KINT(1) .LT. 51)) .OR. (KINT(1) .GT. 81))
     3             THEN
                       IRET = 2
                       KINT(1) = MINDIC
                   END IF
               IF (KINT(2).LT.0.OR.KINT(2).GT.23)
     C             THEN
                       IRET = 2
                       KINT(2) = MINDIC
                   END IF
C
               IF (IRET.NE.0)
     C             THEN
                       JPT=IABS(JPT)
                       KCHAR(JPT)=IOR(KCHAR(JPT),128)
                       KERR = 3
                       CALL ERRSTA(7,1,IFIRST,NUMERR)
                       GO TO 10000
C
                   END IF
           END IF
C
C
      IPOS = 17
C
C
C     'NIL' REPORTS ARE NOT PROSESSED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
C
C
C
C
2000  CONTINUE
C
C   ************************************************
C   *                SECTION 2                     *
C   *                                              *
C   *         (STANDARD ISOBARIC SURFACES)         *
C   ************************************************
C
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTVAL(IPT,2,IVAL)
C
      IF(IVAL .EQ. 44 .OR. IVAL .EQ. 55)
     C   THEN
C
C            44NP1P1 OR 55NP1P1  ....... NUMBER OF LEVELS AND THE
C            -------    -------  ....... PRESSURE ON FIRST ONE
C
            NUMSTAN = NUMSTAN + 1
            IPOS = 17 + (NUMSTAN-1)*9
C
C
            CALL EXTGRP(IPT,2,1,2,0,0,IPOS,IRET)
C
            IF(IRET .NE. 0)
     C         THEN
                  KERR = 5
                  CALL ERRSTA(7,2,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  CALL NEXSEP2(IPT,IEQ,*10000)
                  CALL NEXPRT2(IPT,IEQ,*10000)
                  GO TO 3000
               END IF
C
C
C           DD FFF   .......  WIND DIRECTION & SPEED
C           -- ---
C
            N = KINT(IPOS+1)
            IF(N .EQ. MINDIC .OR. N .LE. 0 ) GO TO 3000
C
C
C
               DO 2500 I=1,N
               CALL NEXTPRT(IPT,IEQ)
               IF(IPT .GE. IEQ)
     C            THEN
                     KERR = 7
                     CALL ERRSTA(7,1,IFIRST,NUMERR)
                     GO TO 3000
                  END IF
C
               CALL EXTVAL(IPT,2,IVAL)
               IF(IVAL .EQ. 44 .OR. IVAL .EQ. 55 .OR. IVAL .EQ. 66 .OR.
     C            IVAL .EQ. 77) GO TO 3000
C
               IPOS2 = IPOS + 3 + (I-1)*2
C
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS2,IRET)
C
               IF ( (IRET .NE. 0)  .OR. (KINT(IPOS2) .GT. 36 .AND.
     C            KINT(IPOS2) .NE. MINDIC) )
     C               THEN
                        KERR = 7
C
                        CALL ERRSTA(7,1,IFIRST,NUMERR)
                        IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                        IREMBEG = IPOS2
C========               IF(I .GT. 1) IREMBEG = IPOS2-2
C
                        DO 2400 J = IREMBEG,IPOS2+1
                        KINT(J) = MINDIC
2400                    CONTINUE
                        CALL NEXSEP2(IPT,IEQ,*10000)
                     END IF
C
2500           CONTINUE
C
         END IF
C
C
C
C
C
C
C
3000  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTVAL(IPT,2,IVAL)
      CALL EXTVAL(IPT,1,IVAL2)
C
      IF(IVAL .EQ. 44 .OR. IVAL .EQ. 55) GO TO 2000
      IF(IVAL .EQ. 66 .OR. IVAL .EQ. 77) GO TO 4000
      IF(IVAL2 .EQ. 6 .OR. IVAL2 .EQ. 7) GO TO 4500
C
      KERR = 5
      CALL ERRSTA(7,1,IFIRST,NUMERR)
      IREM = MAX0(17,IPOS)
      IF(NUMERR .EQ. 1) IREMPOS = IREM
C
      CALL NEXSEP2(IPT,IEQ,*10000)
      GO TO 3000
C
C
C
C
4000  CONTINUE
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 77999) GO TO 10000
C
C
C   ***************************************************
C   *                  SECTION 8                      *
C   *                                                 *
C   *     (MAX. WIND LEVELS & VERTICAL WIND SHEAR)    *
C   ***************************************************
C
      INDP = INDP + 1
C
C
C     77 PMPMPM   ......  PRESSURE AT THE LEVEL
C     -- ------
C
      IPOS = 44 + (INDP-1)*7
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
      IF(IRET .NE. 0)
     C    THEN
             KERR = 12
             CALL ERRSTA(7,1,IFIRST,NUMERR)
             IF(NUMERR .EQ. 1) IREMPOS = 558
             CALL NEXSEP2(IPT,IEQ,*10000)
             CALL NEXPRT2(IPT,IEQ,*10000)
             GO TO 3000
          END IF
C
C
C     DMDM FMFMFM   ......  WIND DIRECTION & SPEED
C     ---- ------
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
      IF(IRET .NE. 0 .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C                     KINT(IPOS+2) .NE. MINDIC))
     C    THEN
             KERR = 13
             CALL ERRSTA(7,1,IFIRST,NUMERR)
             IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
             DO 4200 I=IPOS+2,IPOS+3
             KINT(I) = MINDIC
4200         CONTINUE
C
             CALL NEXSEP2(IPT,IEQ,*10000)
             CALL NEXPRT2(IPT,IEQ,*10000)
             GO TO 3000
          END IF
C
C
C     4 VBVB VAVA   ......  VERTICAL WIND SHEAR AT THE LEVEL
C     - ---- ----
C
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTVAL(IPT,1,IVAL)
      IF(IVAL .EQ. 4)
     C   THEN
            CALL EXTGRP(IPT,1,2,2,0,0,IPOS+4,IRET)
            IF(IRET .NE. 0)
     C          THEN
                   KERR = 14
                   CALL ERRSTA(7,1,IFIRST,NUMERR)
                   IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                   CALL NEXSEP2(IPT,IEQ,*10000)
                   CALL NEXPRT2(IPT,IEQ,*10000)
                   GO TO 3000
                END IF
          END IF
C
      GO TO 3000
C
C
C
C
4500  CONTINUE
C
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 77999) GO TO 10000
C
C
      INDH = INDH + 1
      IPOS = 65 + (INDH-1)*7
C
C
C     7HMHMHM  OR  6HMHMHM   ......  GEOPOTENTIAL AT THE LEVEL
C     -------      -------
C
C
      CALL EXTGRP(IPT,1,4,0,0,0,IPOS,IRET)
      IF(IRET .NE. 0)
     C    THEN
             KERR = 12
             CALL ERRSTA(7,1,IFIRST,NUMERR)
             IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
             CALL NEXSEP2(IPT,IEQ,*10000)
             CALL NEXPRT2(IPT,IEQ,*10000)
             GO TO 3000
          END IF
C
C
C     DMDM FMFMFM   ......  WIND DIRECTION & SPEED AT THE LEVEL
C     ---- ------
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
      IF ( (IRET .NE. 0)  .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C      KINT(IPOS+2) .NE. MINDIC) )
     C         THEN
                  KERR = 13
                  CALL ERRSTA(7,1,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  DO 4600 I = IPOS,IPOS+3
                  KINT(I) = MINDIC
4600              CONTINUE
                  CALL NEXSEP2(IPT,IEQ,*10000)
C
                  GO TO 3000
               END IF
C
C
C     4 VBVB VAVA   ......  VERTICAL WIND SHEAR AT THE LEVEL
C     - ---- ----
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTVAL(IPT,1,IVAL)
      IF(IVAL .EQ. 4)
     C   THEN
            CALL EXTGRP(IPT,1,2,2,0,0,IPOS+4,IRET)
            IF(IRET .NE. 0)
     C          THEN
                   KERR = 9
                   CALL ERRSTA(7,1,IFIRST,NUMERR)
                   IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                   CALL NEXSEP2(IPT,IEQ,*10000)
                   CALL NEXPRT2(IPT,IEQ,*10000)
                   GO TO 3000
                END IF
          END IF
C
C
      GO TO 3000
C
C
C
C
C
C
10000 CONTINUE
C
C
      IF(KERR .EQ. 0) RETURN
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
C
C
                             IPT = KEEP
                             CALL FIXPILO
                             IFIRST=1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            ONLY REPORTS WITH ERROR IN DATA/TIME
C                            OR LAT/LONG ARE NOT PROSESSED ANY FURTHER
C
C                            IF THERE ARE MORE THAN 6 NON-FATAL ERRORS
C                            IN REPORT IT IS ONLY PROSESSED UNTIL THE
C                            FIRST ERROR
C
                             IF(NUMERR .GT. 6)
     C                          THEN
                                   DO 6000 I = IREMPOS,600
                                   KINT(I) = MINDIC
6000                               CONTINUE
                                END IF
C
                             IF(KERR .GT. 4) KERR = 0
                          END IF
C
C
C
C
      RETURN
      END
      SUBROUTINE UQDEC ( IHEAD,IERR )
C
C**** *UQDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE PILOT REPORT PART D FROM INTERMEDIATE
C         FORMAT TO FULL DECODED FORMAT.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UQDEC ( IHEAD,IERR )*
C
C           INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'.
C
C           OUTPUT   : REPORT WRITTEN TO DECODED REPORTS FILE.
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
C         *CALL* *ERRSTA(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          A. HOLOPAINEN  MAY -84
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'printre.f'
C
      DATA IROU /0/
      DATA IPRNUM /0/
C
C     ------------------------------------------------------------------
C*          1.   SET UP INDICATORS AND FLAGS.
C                ----------------------------
 100  CONTINUE
C
C
C     DEFINE 'NIL' INDICATOR
C
      NIL = IAND(KDEC(20),1)
C
C
C     CLEAR ERROR INDICATOR
C
      KERR = 0
C
C
C
C
C***
C*    SET DECODED REPORT TO MISSING DATA VALUE
C***
C
      DO 1 I=25,600
      KDEC(I)=MINDIC
1     CONTINUE
C
C
C
C       *************************************
C       *                                   *
C       *             SECTION 1             *
C       *                                   *
C       *   (IDENTIFICATION AND POSITION)   *
C       *************************************
C
C
C
C     REPORT HEADER
C
C     IN THIS SUBROUTINE ONLY THE FOLLOWING PARTS
C     OF THE HEADER WILL BE DECODED
C        - DAY OF THE MONTH
C        - TIME (HOUR & MIN) FROM REPORT
C        - IDENTIFIER
C        - LENGTH OF THE REPORT
C     AND IN CASE OF TEMP SHIP OR TEMP DROP
C        - LAT &LONG
C        - ALT = 0 (FOR SHIP)
C
C
C
C
C***   DAY AND TIME
C
C
      IF(KINT(2).LT.0.OR.KINT(2).GT.23) THEN
                                           RETURN
                                        END IF  
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C
C***
C*    CHECK IF SHIP PILOT
C***
C
      IF(KDEC(4) .EQ. 33)
     1           THEN
C
C***                LAT & LONG
C
                    CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1                          KDEC(5),KDEC(6))
                    IF(KDEC(5).EQ.MINDIC.OR.KDEC(6).EQ.MINDIC) RETURN
C
C***                ALTIT=0, FOR SEA STATION
C
                    KDEC(8)=0
C
                 END IF
C
C
C
C
C
101   CONTINUE
C
C***  IDENT   STATION/SHIP IDENTIFICATION
C
      KDEC(7)=88
C
C
C
C***  INSTRUMENT SPECCIFICATION
C
      kdec(13)=9
      if(kint(3).ne.mindic) kdec(13)=kint(3)
C
C
C***  REPORT LENGTH
C
C
      KDEC(24)=384
C
C
C
      K=MINDIC
      IF(KDEC(4) .LT. 21) K=KDEC(17)*1000+KDEC(16)
C
C     CHECK IF WIND SPEED IS GIVEN IN KNOTS, IN
C     THIS CASE 50 HAS BEEN ADDED TO THE DATE YY.
C
      IWW=0
      IF(KDEC(1) .GT. 31 .AND. KDEC(1) .LE. 81)
     1             THEN
                      IWW=1
                      KDEC(1)=KDEC(1)-50
                   END IF
C
C
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LE. 0 .OR. KDEC(1) .GT. 31) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) GO TO 99800
C
      IROU = IROU +1
C
4000  CONTINUE
C
C
C
C
C
C       *********************************************************
C       *                                                       *
C       *                      SECTION 3                        *
C       *                                                       *
C       *       (FIXED LEVELS AND/OR SIGNIFICANT LEVELS)        *
C       *********************************************************
C
C
C
C
C ----- FIXED LEVELS  ----------------------------------------
C
         IPOS = 22
C
C -----  LOOP OVER 20 POSSIBLE SETS OF LEVELS
C
         DO 4200 I=1,20
         IND = 17 + (I-1)*11
         IBASE = 0
         IF(KINT(IND) .EQ. 9)
     C      THEN
               IREF = 300
            ELSE
               IF(KINT(IND) .EQ. 8)
     C            THEN
                     IREF = 500
                  ELSE
                     IF(KINT(IND) .EQ. 1)
     C                  THEN
                           IREF = 300
                           IBASE = 30000
                        ELSE
                           GO TO 4200
                        END IF
                  END IF
            END IF
C
         if(kint(ind+1).eq.mindic) go to 4200
C
         ITENS = KINT(IND+1) * 10
         IF(ITENS .LT. 0 .OR. ITENS .GT. 90) GO TO 4200
C
C
C -----     LOOP OVER 3 POSSIBLE LEVELS IN THE SET
C
            DO 4100 J=1,3
            IND2 = IND + 5 + (J-1)*2
C
C           IF FIRST LEVEL AND 'H = /' THIS IS SURFACE
C
            IF(KINT(IND+1+J) .EQ. MINDIC .AND. I .EQ. 1 .AND.
     C         J .EQ. 1) KINT(IND+1+J) = 0
C
            IF(KINT(IND+1+J) .EQ. MINDIC .OR. KINT(IND2) .EQ. MINDIC
     C         .OR. KINT(IND2+1) .EQ. MINDIC) GO TO 4100
C
            IHIGH = IBASE + (ITENS + KINT(IND+J+1)) * IREF
C
               DO 4050 II = IPOS,22,-3
               IF(KDEC(II) .NE. MINDIC .AND. IHIGH .LT. KDEC(II))
     C           GO TO 4300
4050           CONTINUE
C
            IPOS = IPOS + 3
            KDEC(IPOS) = IHIGH
            CALL DDFFF(KINT(IND2),KINT(IND2+1),IWW,K,MINDIC,
     C                 KDEC(IPOS+1),KDEC(IPOS+2))
4100        CONTINUE
C
C
4200     CONTINUE
C
C
4300     CONTINUE
C
C
C ----- SIGNIFICANT  LEVELS  ------------------------------------------
C
C
         IF(KINT(237) .NE. 21212) GO TO 9900
C
         LEVNUM = 0
C
            DO 4500 J=1,60
            IND = 239 + (J-1)*4
            IF(KINT(IND) .EQ. MINDIC) GO TO 4500
C
            LEVNUM = LEVNUM + 1
            IPOS = 205 + (LEVNUM-1)*3
C
C****       NN PPP   PRESSURE AT THE LEVEL
C
            KDEC(IPOS) = KINT(IND)
C
C           REJECT IF LEVEL LOWER THAN PREVIOUS
C
            IF(LEVNUM .GT. 1)
     C         THEN
                  IF(KDEC(IPOS) .GE. KDEC(IPOS-3) .AND.
     C               KDEC(IPOS-3) .NE. MINDIC)
     C               THEN
                        KDEC(IPOS) = MINDIC
                        LEVNUM = LEVNUM - 1
                        GO TO 4500
                     END IF
               END IF
C
C****       DD FFF   WIND AT THE LEVEL
C
            CALL DDFFF(KINT(IND+1),KINT(IND+2),IWW,K,MINDIC,
     C                 KDEC(IPOS+1),KDEC(IPOS+2))
C
4500        CONTINUE
C
C
C
C
C
C       ***********************************************
C       *                 SECTION 9                   *
C       *                                             *
C       *              (REGIONAL DATA)                *
C       ***********************************************
C
C
C
C ------  NO REGIONAL DATA HANDLED  -----------------------------
C
C
C
9900  CONTINUE
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT (I.E. IF
C     THERE ARE NO DATA AND NO CODING ERRORS)
C
      NIL2 = 1
      DO 460 I=25,KDEC(24)
      IF(KDEC(I) .EQ. MINDIC) GO TO 460
      NIL2 = 0
      GO TO 465
460   CONTINUE
C
465   CONTINUE
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 99800
         END IF
C
C
C
C==========================================================
C====    PRINTING OF ERRORS & REPORTS & KINT & KDEC    ====
C==========================================================
C
C
      IPRI = 0
      IPRIALL = 0
C
      IS1 = KINT(12) - 48
      IS2 = KINT(13) - 48
      IS3 = KINT(14) - 48
      IS4 = KINT(15) - 48
      IS5 = KINT(16) - 48
C
      IBLOK = 10*IS1 + IS2
      ISTA = 10000*IS1 + 1000*IS2 + 100*IS3 + 10*IS4 + IS5
C
      DO 700 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
700   CONTINUE
C
      DO 710 I=1,100
      IF(ISTA .EQ. ISTASD(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
710   CONTINUE
C
C
C     PRINT THE ERRONEUS SHIPS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERSHIP .GT. 0)
     C   THEN
            IF(KDEC(4) .GE. 36 .AND. KDEC(20) .GT. 1) IPRI = 1
         END IF
C
C     PRINT ALL ERRONEUS REPORTS WITHIN TIME LIMITS IF REQUIRED
C
      IF(IERALL .GT. 0)
     C   THEN
            IPRI = 1
         END IF
C
      IF(IERALLD .GT. 0) IPRI = 1
C
      IF(KDEC(2) .LT. ITIMES(1) .OR. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
C
      IF(IPRALLD .EQ. 1)
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
C
C
      IF(IPRNUM .GT. IPRLIM) GO TO 99800
C
      IPRI = 0
C
      IF(IPRI .GT. 0)
     C   THEN
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IPRNUM=IPRNUM+1
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        IPRNUM=IPRNUM+1
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKINT(KINT,1,600,MINDIC)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
C
10000 FORMAT(1H ,///)
20000 FORMAT(1H ,/////2X,' PART D  NUMBER =',I5,/)
C
C
C
C
C
C
C
C
C
99800 CONTINUE
C
c      CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
      CALL PILOEXP( 4,IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UQDEC - after piloexp, ierr ',IERR
CTEST
      CALL MRGPIL( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UQDEC - after mrgpil, ierr ',IERR
CTEST
      CALL QCPILOT( kdec(4),IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UQDEC - after qcpilot, ierr ',IERR
CTEST
      CALL BUFRENC( IERR )
CTEST
      IF(IERR.NE.0)
     1  PRINT*,' UQDEC - after bufrenc, ierr ',IERR
CTEST
      IERR=0
C
99900 CONTINUE
      RETURN
      END
      SUBROUTINE UQINT ( IHEAD,IERR )
C
C**** *UQINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT PILOT REPORTS PART D FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UQINT ( IHEAD,IERR )*
C
C         INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                     1 CHARACTER PER WORD.
C
C                     IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                               WRITTEN TO ERROR FILE.
C                           = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                     IERR NOT USED ON INPUT.
C
C         OUTPUT    : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                 DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION ).
C
C     METHOD.
C     -------
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING
C         IS PROCEEDED UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON-FATAL ERROR HAS BEEN DETECTED THE
C         ROUTINE TRIES TO FIND THE BEGINNING OF CORRECT
C         DATA.  IF CORRECT DATA ARE FOUND THE DECODING
C         THE NUMBER OF NON-FATAL ERRORS IS COUNTED AND
C         IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE
C         DATA AFTER THE FIRST ERROR ARE REMOVED (THIS
C         IS DONE TO ENSURE THAT TOTALLY CORRUPTED DATA
C         ARE NOT USED E.G. WRONG PART OF PILOT).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C                   NUMERR = NUMBER OF NON-FATAL ERRORS
C                   IREMPOS= THE POINTER TO FIRST ERROR IN KINT.
C
C
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
C          A. HOLOPAINEN  NOVEMBER  -84.
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'printre.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
      DIMENSION IREG(9),INAT(9)
      DATA IREG /
     C    51515, 52525, 53535, 54545, 55555, 56565, 57575, 58585, 59595/
      DATA INAT /
     C    61616, 62626, 63636, 64646, 65656, 66666, 67676, 68686, 69696/
C
C
C     ------------------------------------------------------------------
C            1. CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C               -------------------------------------------------------
C               CHARACTER OF REPORT.
C               --------------------
 100  CONTINUE
C
C
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      MODIFY = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR = 0
      INDH = 0
      INDP = 0
      INDLEV = 0
      LEVNUM = 0
      IREMPOS = 17
C
C
C***
C*    CHECK FOR 'NIL' REPORT.
C***
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.25 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(10,0,IFIRST,NUMERR)
             KERR = 0
         END IF
C
C
C
C***
C*     SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C***
C
      DO 200 I=1,600
         KINT(I) = MINDIC
  200 CONTINUE
C
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 300 I=12,14
         KINT(I) = 88
  300 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C***
C*    CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C***
C
      DO 400 I=1,3
         KDEC(I) = MINDIC
  400 CONTINUE
C
      DO 500 I=5,9
         KDEC(I) = MINDIC
  500 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = IAND(KDEC(15),4)
C
      KDEC(17) = MINDIC
      KDEC(22) = MINDIC
C
C
C
1000  CONTINUE
C
C
C   ***********************************************************
C   *                      SECTION 1                          *
C   *                                                         *
C   *     (IDENTIFICATION AND POSITION, THIS IS DIFFERENT     *
C   *      FOR LAND STATIONS AND SHIP REPORTS)                *
C   ***********************************************************
C
C
C               YY GG /
C               -- -- -
C
C
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*10000)
                CALL NEXPRT2(IPT,IEQ,*10000)
                IF(KCHAR(IPT).GE.65) THEN
                                        GO TO 1011
                                     END IF
C
C               FIRST FIGURE OF THE GROUP MUST BE 0-3
C               OR 5-8 IF WIND SPEED IS GIVEN IN KNOTS,
C               OTHERWISE YYGGID GROUP IS MISSING
C
C
                IF ((KCHAR(IPT) .GE. 48) .AND. (KCHAR(IPT) .NE. 52)
     1                .AND. (KCHAR(IPT) .LE. 56))
     2                 CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
                IF(KINT(1).LE.0.AND.KINT(1).GT.31) THEN
                   KERR=1
                   GO TO 10000
                END IF
C
                JPT = IPT
C
C
C
C
  700 IF ( KDEC(4).EQ.32 )
     C      THEN
C
C               LAND STATION
C
C
C
C               IIIII     -   RETAIN POINTER TO IDENTIFIER
C               -----
C
C
                CALL NEXPRT2( IPT,IEQ,*701)
                GO TO 702
C
 701            CONTINUE
C
                KERR=2
                GO TO 10000
 702            CONTINUE 
                KPT = IPT
                ID = 5
                CALL EXTINT ( IPT,5,4 )
C
C               EXTRACT STATION LAT/LONG ETC FROM WMO MASTER FILE.
C
                CALL LOCSTAT ( IWIND,IRET )
C
C
C               CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                IF(IRET .eq. 0)
     C              THEN
                       IF(NIL .EQ. 1)
     C                    THEN
                             KDEC(20)=1
                             kerr=1
                             RETURN
                          END IF    
                    END IF
C
                IF ( IRET.EQ.1 )
     C              THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        CALL ERRSTA(10,1,IFIRST,NUMERR)
C
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
               OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_pilo_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C
                        WRITE (55,9901) ctstamp,KINT(4)
                        CLOSE(55)
                        WRITE (*,9901) ctstamp,KINT(4)
cpsokt98 9901                FORMAT (1H ,a8,' PILO NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' PILO NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        GO TO 10000
                    END IF
C
                IF ( IRET.EQ.2 )
     C             THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2

                        CALL ERRSTA(10,2,IFIRST,NUMERR)
C
                        GO TO 10000
                    END IF
C
C
                 IF(IRET .EQ. 3)
     1              THEN
C
C                     INVALID OBSERVATION TYPE
C
                      KERR = 3
                      CALL ERRSTA(10,2,IFIRST,NUMERR)
C
                      GO TO 10000
                   END IF
C
            ELSE
C
C                PILOT SHIP
C
                 IF(KDEC(4) .EQ. 33)
     1              THEN
C
                        KINT(4)=0
C
C
C
C                       RETAIN POINTER TO STATION IDENTIFIER .
C
                        KPT=IMI
C
C
C                       CHECK THE CALL SIGN
C
                        IBUOY = 0
                        K = IMI
                        J = K + 4
C
                        DO 750 I=K,J
                        IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750                   CONTINUE
C
C
C                       IF A LETTER GROUP IS FOUND CHECK THAT THIS IS
C                       NOT MIMIMJMJ GROUP 'QQBB'. IN THIS CASE THE SHIP
C                       CALL SIGN GROUP IS PRESUMED MISSING.
C
                        IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85)
     C                  .AND.
     C                  (KCHAR(K+2) .EQ. 66) .AND. (KCHAR(K+3) .EQ. 66))
     C                      THEN
                               IBUOY = 2
                            END IF
C
C
                        IF(IBUOY .NE. 0)
     C                     THEN
C
C                             SHIP,RIG OR PLATFORM. IDENTIFIER
C                             NUMBER SET TO 0.
C
C
                         ID=0
                              KINT(4) = 0
C
                              IF (IBUOY.EQ.1)
     C                           THEN
C
C                                   MOVE POINTER PAST CALL SIGN . THE
C                                   FIRST CHARACTER OF THE NEXT GROUP IS
C                                   LOCATED AS SOME SHIPS USE THE FULL
C                                   NAME ( WHICH CAN CONSIST OF 2 WORDS
C                                   INSTEAD OF CALL SIGN. SO IF THE NEXT
C                                   GROUP DOESN'T START WITH 'QQ' (WHICH
C                                   IS THE BEGINNING OF MIMIMJMJ GROUP)
C                                   IT MUST BE PART OF THE SHIP'S NAME
C                                   USED INSTEAD OF CALL SIGN.
C
                                    IIPT=K
                                    CALL NEXSEP2(IIPT,IEQ,*10000)
                                    ID = IIPT - KPT
                                    CALL NEXTLET (IIPT,IEQ)
C
                                    IF(KCHAR(IIPT) .NE. 81 .OR.
     C                                 KCHAR(IIPT+1) .NE. 81)
     C                                 THEN
                                          CALL NEXSEP2(IIPT,IEQ,*10000)
                                          ID = IIPT - K
                                       END IF
C
C
                         END IF
                       IF(IBUOY.EQ.2)
     1                    THEN
C
C                            SHIP CALL SIGN FOLLOWS MIMIMJMJ
C
                             IIPT=K
C
C                            SKIP MIMIMJMJ GROUP
C
                             CALL NEXSEP2(IIPT,IEQ,*10000)
                             CALL NEXPRT2(IIPT,IEQ,*10000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*10000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*10000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*10000)
                                       ID=IIPT-K
                                    END IF
                              END IF
                          END IF
C
                   END IF
C
C
C                       99 LALALA
C                       -- ------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                       CHECK VALIDITY OF LALALA
C
                        IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR = 4
                              CALL ERRSTA(10,1,IFIRST,NUMERR)
                              GO TO 10000
                           END IF
C
C                       QC LOLOLOLO
C                       -- --------
C
                        CALL NEXPRT2( IPT,IEQ,*10000)
                        CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )
C
C                       CHECK VALIDITY OF LOLOLOLO
C
                        IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C                          KINT(7).NE.5.AND.KINT(7).NE.7) IRET=1
                        IF (KINT(8).LT.0.OR.KINT(8).GT.1800)IRET=1
                        IF (IRET.NE.0)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
                              KERR=4
                              IF (IFIRST.EQ.0)
     C                           THEN
                                    CALL ERRSTA(10,2,IFIRST,NUMERR)
                                 END IF
                              GO TO 10000
                           END IF
C
C                       MMM ULA ULO
C                       --- --- ---
C
                        CALL NEXPRT2(IPT,IEQ,*10000)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                               LAT,LON)
                        CALL MARDSEN(LAT,LON,KINT(9),IRET)
C
                        IF(IRET .NE. 0)
     1                      THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1)
     C                           THEN
                                    KDEC(20)=1
                                    kerr=1
                                    RETURN
                                 END IF
C
C    --------------            WRONG CODE FOR MARSDEN SQUARE    ------
                               KERR = 4
                               CALL ERRSTA(10,1,IFIRST,NUMERR)
C
                               GO TO 10000
C
                            END IF
C
                    ELSE
C
C                       THIS IS NOT A PILOT REPORT
C
                        KERR = 3
                        CALL ERRSTA(10,1,IFIRST,NUMERR)
C
                        GO TO 10000
                END IF
             END IF
C
C
C     RETAIN STATION IDENTIFIER IN CHARACTER FORM ( IF ANY EXISTS )
C
      IF ( ID.NE.0 )
     C    THEN
              IF (ID.GT.5) ID = 5
              J = KPT
              K = 12 + ID -1
              DO 800 I=12,K
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C
C
C
C     CHECK VALUES OF YY GG FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4) .EQ. 33 )
     1     THEN
               IRET = 0
               IF ((KINT(1) .LT. 1) .OR. ((KINT(1) .GT. 31) .AND.
     2             (KINT(1) .LT. 51)) .OR. (KINT(1) .GT. 81))
     3             THEN
                       IRET = 2
                       KINT(1) = MINDIC
                   END IF
               IF (KINT(2).LT.0.OR.KINT(2).GT.23)
     C             THEN
                       IRET = 2
                       KINT(2) = MINDIC
                   END IF
C
               IF (IRET.NE.0)
     C             THEN
                       JPT=IABS(JPT)
                       KCHAR(JPT)=IOR(KCHAR(JPT),128)
                       KERR = 3
                       CALL ERRSTA(10,1,IFIRST,NUMERR)
                       GO TO 10000
C
                   END IF
           END IF
C
C
C
C
C     'NIL' REPORTS ARE NOT PROSESSED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
C
C
C
C
4000  CONTINUE
C
C   ********************************************************
C   *                     SECTION 4                        *
C   *                                                      *
C   *   (FIXED REGIONAL LEVELS AND/OR SIGNIFICANT LEVELS   *
C   ********************************************************
C
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 21212) GO TO 4500
C
C
C     SEE IF THIS IS THE BEGINNING OF REGIONAL OR NATIONAL DATA
C
      IF( IVAL .EQ. 51515  .OR. IVAL .EQ. 52525 .OR.
     C    IVAL .EQ. 53535  .OR. IVAL .EQ. 54545 .OR.
     C    IVAL .EQ. 55555  .OR. IVAL .EQ. 56565 .OR.
     C    IVAL .EQ. 57575  .OR. IVAL .EQ. 58585 .OR.
     C    IVAL .EQ. 59595 ) GO TO 10000
C
      IF( IVAL .EQ. 61616  .OR. IVAL .EQ. 62626 .OR.
     C    IVAL .EQ. 63636  .OR. IVAL .EQ. 64646 .OR.
     C    IVAL .EQ. 65656  .OR. IVAL .EQ. 66666 .OR.
     C    IVAL .EQ. 67675  .OR. IVAL .EQ. 68686 .OR.
     C    IVAL .EQ. 69696 ) GO TO 10000
C
C
C
      CALL EXTVAL(IPT,1,IVAL)
C
      IF(IVAL .EQ. 9 .OR. IVAL .EQ. 8 .OR. IVAL .EQ. 1)
     C   THEN
C
C****        9/8/1 TN U1 U2 U3  ......  TENS & UNITS DIGITS OF ALTITUDE
C            - - - -- -- -- --
C
            INDLEV = INDLEV + 1
            IPOS = 17 + (INDLEV-1)*11
C
C
            CALL EXTGRP(IPT,1,1,1,1,1,IPOS,IRET)
C
            IF(IRET .NE. 0)
     C         THEN
                  KERR = 5
                  CALL ERRSTA(10,2,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  CALL NEXSEP2(IPT,IEQ,*10000)
                  CALL NEXPRT2(IPT,IEQ,*10000)
                  GO TO 4000
               END IF
C
C
C
            N = KINT(IPOS+1)
C
C
C              SEE HOW MANY WIND GROUPS THERE ARE.  THERE ARE SUPPOSED
C              TO BE ONLY THREE WIND GROUPS (MAX) AFTER 'ID-GROUP'
C              44NP1P1 / 55NP1P1.  HOWEVER QUITE A COMMON PRACTICE
C              IS TO HAVE THE ID-GROUP ONLY ONCE AND ALL THE WIND-GROUPS
C              AFTER THIS.  THEREFORE THIS PRACTICE IS ALSO ACCEPTED
C              ASSUMING DATA ARE CORRECT.
C
               NUMBER = 0
               DO 4100 I = 1,3
               IF(NUMBER .GT. 0 .AND. KINT(IPOS+I+1) .LT. KINT(IPOS+I))
     C            GO TO 4100
               IF(KINT(IPOS+I+1) .NE. MINDIC) NUMBER = I
4100           CONTINUE
C
C
               IPOS = 22 + (INDLEV-1) * 11
C
C****          DD FFF   .......  WIND DIRECTION & SPEED
C              -- ---
C
               DO 4200 I=1,NUMBER
               IPOS2 = IPOS + (I-1)*2
               CALL NEXPRT2(IPT,IEQ,*10000)
               CALL EXTVAL(IPT,1,IVAL)
               IF(IVAL .EQ. 9 .OR. IVAL .EQ. 8) GO TO 4000
C
               CALL EXTVAL(IPT,5,IVAL)
                  DO 4130 J = 1,9
                  IF(IVAL .EQ. IREG(J)) GO TO 10000
                  IF(IVAL .EQ. INAT(J)) GO TO 10000
4130              CONTINUE
C
C
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS2,IRET)
C
               IF ( (IRET .NE. 0)  .OR. (KINT(IPOS2) .GT. 36 .AND.
     C            KINT(IPOS2) .NE. MINDIC) )
     C               THEN
                        KERR = 7
C
                        CALL ERRSTA(10,1,IFIRST,NUMERR)
                        IF(NUMERR .EQ. 1) IREMPOS = IPOS2
C
                        DO 4150 J = IPOS2,IPOS2+1
                        KINT(J) = MINDIC
4150                    CONTINUE
                        CALL NEXSEP2(IPT,IEQ,*10000)
                        CALL NEXPRT2(IPT,IEQ,*10000)
                        IF(I .EQ. NUMBER) GO TO 4000
                     END IF
C
4200           CONTINUE
C
         ELSE
C
            CALL NEXSEP2(IPT,IEQ,*10000)
C
         END IF
C
C
C
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      GO TO 4000
C
C
C
C
C****  21212  ......  SIGNIFICANT LEVEL INDICATOR
C      -----
C
C
4500  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      IPOS = 237
      CALL EXTGRP(IPT,5,0,0,0,0,IPOS,IRET)
      IF (IRET .NE. 0)
     C   THEN
            KERR = 10
C
            CALL ERRSTA(10,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            CALL NEXSEP2(IPT,IEQ,*10000)
            CALL NEXPRT2(IPT,IEQ,*10000)
            GO TO 4900
         END IF
C
C
C
C
4600  CONTINUE
C
C
C**** NINI PIPIPI  ......  PRESSURE AT SIGNIFICANT LEVEL
C     ---- ------
C
      LEVNUM = LEVNUM + 1
C
      IPOS = 238 + (LEVNUM-1) * 4
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C====   PARANNA    ============================
C     CHECK IF THIS REALLY IS THE NEXT LEVEL
C
      IF(KCHAR(IPT) .NE. KCHAR(IPT+1)) GO TO 4900
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
      IF (IRET .NE. 0)
     C   THEN
            KERR = 5
            CALL ERRSTA(10,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
            CALL NEXSEP2(IPT,IEQ,*10000)
            CALL NEXPRT2(IPT,IEQ,*10000)
C
            GO TO 4700
         END IF
C
C
C**** DD FFF  ......  WIND AT SIGNIFICANT LEVEL
C     -- ---
C
      CALL NEXPRT2(IPT,IEQ,*10000)
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
      IF ( (IRET .NE. 0)  .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C      KINT(IPOS+2) .NE. MINDIC) )
     C   THEN
            KERR = 7
C
            CALL ERRSTA(10,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            DO 4650 J = IPOS,IPOS+3
            KINT(J) = MINDIC
4650        CONTINUE
            CALL NEXSEP2(IPT,IEQ,*10000)
            CALL NEXPRT2(IPT,IEQ,*10000)
            GO TO 4600
         END IF
C
C
      GO TO 4600
C
C
C
4700  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      IF(KCHAR(IPT) .EQ. KCHAR(IPT+1)) GO TO 4600
      CALL NEXSEP2(IPT,IEQ,*10000)
      GO TO 4700
C
C
C
4900  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*10000)
      CALL EXTVAL(IPT,5,IVAL)
C
      IF(IVAL .EQ. 21212) GO TO 4500
C
      IF( IVAL .EQ. 51515  .OR. IVAL .EQ. 52525 .OR.
     C    IVAL .EQ. 53535  .OR. IVAL .EQ. 54545 .OR.
     C    IVAL .EQ. 55555  .OR. IVAL .EQ. 56565 .OR.
     C    IVAL .EQ. 57575  .OR. IVAL .EQ. 58585 .OR.
     C    IVAL .EQ. 59595 ) GO TO 10000
C
      IF( IVAL .EQ. 61616  .OR. IVAL .EQ. 62626 .OR.
     C    IVAL .EQ. 63636  .OR. IVAL .EQ. 64646 .OR.
     C    IVAL .EQ. 65656  .OR. IVAL .EQ. 66666 .OR.
     C    IVAL .EQ. 67675  .OR. IVAL .EQ. 68686 .OR.
     C    IVAL .EQ. 69696 ) GO TO 10000
C
      CALL EXTVAL(IPT,1,IVAL)
      IF( IVAL .EQ. 9 .OR. IVAL .EQ. 8) GO TO 4000
C
      CALL NEXSEP2(IPT,IEQ,*10000)
      GO TO 4900
C
C
C***************************************************
C*                                                 *
C*               SECTIONS 5 & 6                    *
C*                                                 *
C*       REGIONAL & NATIONAL DATA (NOT USED)       *
C*                                                 *
C***************************************************
C
C
10000 CONTINUE
C
C
      IF(KERR .EQ. 0) RETURN
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
C
C
                             IPT = KEEP
                             CALL FIXPILO
                             IFIRST=1
                             NUMRERR(10) = NUMRERR(10) + 1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            ONLY REPORTS WITH ERROR IN DATA/TIME
C                            OR LAT/LONG ARE NOT PROSESSED ANY FURTHER
C
C                            IF THERE ARE MORE THAN 6 NON-FATAL ERRORS
C                            IN REPORT IT IS ONLY PROSESSED UNTIL THE
C                            FIRST ERROR
C
                             IF(NUMERR .GT. 6)
     C                          THEN
                                   DO 6000 I = IREMPOS,600
                                   KINT(I) = MINDIC
6000                               CONTINUE
                                END IF
C
                             IF(KERR .GT. 4) KERR = 0
                          END IF
C
C
C
C
      RETURN
      END
      SUBROUTINE FIXPILO
C
C**** *FIXPILO*
C
C
C     PURPOSE.
C     --------
C
C         FIX ERRORS FOUND IN PILOT DATA
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *FIXPILO*
C
C          INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C          OUTPUT    : - REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN FIXED FORM
C                        IF FIX WAS FOUND
C                      - MODIFY = 1  IF FIX WAS MADE
C                                 0  OTHERWISE
C
C
C     METHOD.
C     -------
C
C         THE POSSIBLE FIXES ARE 
C             - REMOVE 'E E E' S
C             - IF = IS MISSING (DETERMINED BY CHECKING IF
C                    PPAA, PPBB, PPCC OR PPDD ARE FOUND IN
C                    THE MIDDLE OF REPORT), ADD = TO THE END
C                    OF REPORT.
C             - IF THE LENGHT OF A GROUP IS INCORRECT;
C                    LENGTH = 10  ACCEPT DATA ('SP' MISSING)
C                    LENGTH = 11  SEE IF THE 6.TH CHAR. SHOULD
C                                 HAVE BEEN 'SP'
C                    LENGTH .LT. 5  SEE IF A FIVE FIGURE GROUP
C                                          HAS BEEN SPLIT INTO TWO.
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
C          A.HOLOPAINEN  NOVEMBER  -84
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'printre.f'
C
C     ------------------------------------------------------------------
C
C*          1.   CLEAR PARITY BIT.
C                -----------------
 100  CONTINUE
C
C
      MODIFY = 0
      IEQORIG = IEQ
C
      DO 101 I=IPT,IGS
         KCHAR(I) = IAND(KCHAR(I),127)
101   CONTINUE
C
C
      CALL REMEEE
C
C     SEE IF = -SIGN WAS MISSING BY CHECKING IF 'PPAA , PPBB , PPCC OR
C     PPDD ' CAN BE FOUND IN THE MIDDLE OF THE REPORT.
C
C
      NPT = IPT
      IEQADD = 0
      CALL NEXTFIG(NPT,IEQ)
      IF(NPT .GE. IEQ) GO TO 10000
C
400   CONTINUE
C
      MIMI = 0
C
      CALL NEXSEP2(NPT,IEQ,*10000)
      CALL NEXPRT2(NPT,IEQ,*10000)
C
      IF( ( KCHAR(NPT  ) .EQ. 80   .AND.   KCHAR(NPT+1) .EQ. 80)  .AND.
     C   (( KCHAR(NPT+2) .EQ. 65   .AND.   KCHAR(NPT+3) .EQ. 65  ) .OR.
     C    ( KCHAR(NPT+2) .EQ. 66   .AND.   KCHAR(NPT+3) .EQ. 66  ) .OR.
     C    ( KCHAR(NPT+2) .EQ. 67   .AND.   KCHAR(NPT+3) .EQ. 67  ) .OR.
     C    ( KCHAR(NPT+2) .EQ. 68   .AND.   KCHAR(NPT+3) .EQ. 68  ) ) )
     C     MIMI = 1
C
      IF(MIMI .EQ. 1)
     C   THEN
C
            MPT = NPT
            CALL PRESEP(MPT,IPT,*10000)
            CALL PREPRT(MPT,IPT,*10000)
C
            MODIFY = 1
            IEQADD = IEQADD + 1
C
               DO 500 I = IEQ+1 , IGS+1 , -1
               KCHAR(I) = KCHAR(I-1)
500            CONTINUE
C
            KCHAR(MPT+1) = 61
            IF(IEQADD .EQ. 1) IEQ = MPT+1
            IGS = IGS+1
         END IF
C
      GO TO 400
C
10000 CONTINUE
C
C
C
C     SEE IF THERE ARE GROUPS OF DIFFERENT LENGTH FROM 5
C
C
      NPT = IPT
      CALL NEXTFIG(NPT,IEQ)
      IF(NPT .GE. IEQ) GO TO 20000
C
200   CONTINUE
C
      MPT = NPT
      CALL NEXPRT2(MPT,IEQ,*20000)
      NPT = MPT
      CALL NEXSEP2(NPT,IEQ,*20000)
      LEN = NPT - MPT
C
C     SEE IF THE GROUP IS TOO LONG
C
      IF(LEN .GT. 5)
     C   THEN
C
C           IF LENGTH = 10 , NO ERROR
C
            IF(LEN .EQ. 10) GO TO 200
C
C
C           QUITE A COMMON ERROR IS THAT 'SPACE' BETWEEN
C           TWO GROUPS IS 8 OR 0 OR NOT FIGURE
C
            IF(LEN .EQ. 11)
     C         THEN
                  IF(KCHAR(MPT+5) .EQ. 56 .OR. KCHAR(MPT+5) .EQ. 48)
     C               THEN
C
                        KCHAR(MPT+5) = 32
                        MODIFY = 1
                        GO TO 200
                     ELSE
                        IF( (KCHAR(MPT+5) .LT. 48 .OR. KCHAR(MPT+5) .GT.
     C                       57) .AND. KCHAR(MPT+5) .NE. 32 )
     C                     THEN
C
                              KCHAR(MPT+5) = 32
                              MODIFY = 1
                              GO TO 200
                           END IF
                     END IF
                  GO TO 200
               END IF
         END IF
C
C     SEE IF THE GROUP IS SPLIT INTO TWO GROUPS
C
      IF(LEN .LT. 5)
     C   THEN
            NNPT = NPT
            CALL NEXPRT2(NNPT,IEQ,*20000)
            CALL NEXTVAL(NNPT,32,IEQ)
            IF(NNPT .GT. IEQ) GO TO 20000
            LEN2 = NNPT - NPT - 1
            LEN3 = LEN + LEN2
            IF(LEN3 .EQ. 5)
     C         THEN
C
                  DO 300 I = NPT,NNPT-2
                  KCHAR(I) = KCHAR(I+1)
  300             CONTINUE
                  KCHAR(NNPT-1) = 32
                  MODIFY = 1
               END IF
         END IF
C
      GO TO 200
C
20000 CONTINUE
C
C
C
90000 FORMAT(1H ,//2X,' THE ORIGINAL REPORT POS 1 ',/)
90001 FORMAT(1H ,//2X,' THE ORIGINAL REPORT POS 2 ',/)
91000 FORMAT(1H ,//2X,' THE MODIFIED REPORT ',/)
C
C
C
      RETURN
      END
      SUBROUTINE PRTEMP(NUMBER)
C
C**** *PRTEMP*
C
C
C     PURPOSE.
C     --------
C
C         PRINTS TEMP BULLETIN AND IT'S INTERMEDIATE
C         FORMAT ('KINT') AND DECODED FORMAT ('KDEC')
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTEMP(NUMBER)*
C
C          INPUT     : NUMBER - THE NUMBER OF THE REPORT
C                      KINT   - THE BULLETIN IN INTERMEDIATE FORMAT
C                      KDEC   - THE BULLETIN IN DECODED FORMAT
C                      KDEC(23) = 1 FOR PART A
C                               = 2 FOR PART B
C                               = 3 FOR PART C
C                               = 4 FOR PART D
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
C     A.HOLOPAINEN  SEP.83
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
      DIMENSION NERR(30)
      DIMENSION ILN(4)
C
      DATA ILN / 200, 600, 200, 600/
C
C
C     ------------------------------------------------------------------
C*          1.  PRINT TEMP REPORT.
C                -----------------
 100  CONTINUE
C
      DO 101 I=1,30
      NERR(I)=0
 101  CONTINUE
C
C
            KK = KDEC(20)
C
              DO 102 I=30,1,-1
               K = KK - (2**(I-1)) * (KK/(2**(I-1)))
               IF(K .GE. KK) GO TO 102
               KK = K
               NERR(I) = 1
               IF(KK .EQ. 0) GO TO 103
 102           CONTINUE
C
 103        CONTINUE
C
            WRITE(*,10000)
            WRITE(*,20000)
            IF(KDEC(23) .EQ. 1) WRITE(*,30000) NUMBER
            IF(KDEC(23) .EQ. 2) WRITE(*,40000) NUMBER
            IF(KDEC(23) .EQ. 3) WRITE(*,50000) NUMBER
            IF(KDEC(23) .EQ. 4) WRITE(*,60000) NUMBER
            WRITE(*,70000)
C
              DO 104 I=1,30
               IF(NERR(I) .EQ. 1) WRITE(*,80000) I
 104           CONTINUE
C
            WRITE(*,20000)
C
10000       FORMAT(2X,///)
20000       FORMAT(2X,'=======================')
30000       FORMAT(2X,'=  PART A NUMBER',I4,'  =')
40000       FORMAT(2X,'=  PART B NUMBER',I4,'  =')
50000       FORMAT(2X,'=  PART C NUMBER',I4,'  =')
60000       FORMAT(2X,'=  PART D NUMBER',I4,'  =')
70000       FORMAT(2X,'=                     =')
80000       FORMAT(2X,'=  ERROR',I3,'           =')
C
            CALL PRTBULL(IMI,IEQ)
            CALL PRTKINT(KINT,1,ILN(KDEC(23)),MINDIC)
            CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
C
      WRITE(*,90000)
90000 FORMAT(2X,116('='))
C
C
      RETURN
      END
      SUBROUTINE MARDSEN(LAT,LONG,M,IERROR)
C
C**** *MARDSEN*
C
C
C     PURPOSE.
C     --------
C
C         CHECK THE LAT&LONG AGAINST MARDSEN SQUARE
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *MARDSEN(LAT,LONG,M,IERROR)*
C
C          INPUT     : LATITUDE  IN HUNDREDTH'S OF DEGREE
C                      LONGITUDE IN HUNDREDTH'S OF DEGREE
C              M     - MARDSEN SQUARE VALUE GIVEN IN REPORT
C
C          OUTPUT   : IERROR  - ERROR INDICATOR
C
C
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
C     A. HOLOPAINEN  JUNE -84
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
C     ------------------------------------------------------------------
C
C*          1.   CHECK POSITION.
C                ---------------
 100  CONTINUE
C
C
      IERROR = 0
      LOLO = LONG
C
C     DON'T CHECK IF POSITION ON THE LINE BETWEEN TWO (OR FOUR) SQUARES
C
      LAA = 1000*(LAT/1000)
      LOO = 1000*(LONG/1000)
C
      IF(LAA .EQ. LAT) RETURN
      IF(LOO .EQ. LONG) RETURN
C
      LAT = LAT /10
      LONG= LONG/10
C
C
      IF(LAT .GE. 0)
     C   THEN
            LO = IABS(LONG/100)+1
            IF(LOLO .GE. 0) LO = 37 - LO
C
            IF( LAT .LT. 800 )
     C         THEN
                  MMM = (LAT/100)*36 + LO
               ELSE
                  MMM = 900 + LO
               END IF
         END IF
C
C
      IF(LAT .LT. 0)
     C   THEN
            LO = IABS( LONG/100 )
            IF(LOLO .GE. 0) LO = 35 - LO
            MMM = 300 + IABS( LAT/100 )*36 +LO
         END IF
C
      IF(MMM .NE. M) IERROR = 1
C
      RETURN
C
      END
      SUBROUTINE IC3333(IQUADR,LAT,LONG,MINDIC,LAT2,LONG2)
C
C****
C*
C*    NAME     :  IC3333
C*
C*    FUNCTION :  DECODE LATITUDE AND LONGITUDE GIVEN IN THE FORM
C*                99LALALA QCL0L0L0L0
C*
C*    INPUT    :  IQUADR   THE QUADRANT OF THE GLOBE (QC)
C*             :  LAT      LATITUDE IN TENTHS OF DEGREE
C*             :  LONG     LONGITUDE IN TENTHS OF DEGREE
C*             :  MINDIC   MISSING DATA VALUE
C*
C*
C*    OUTPUT   :  LAT2   LATITUDE IN HUNDREDTHS OF DEGREE
C*                       SOUTHERN LATITUDE NEGATIVE
C*             :  LONG2: LONGITUDE IN HUNDREDTHS OF DEGREE
C*                       WESTERN LONGITUDE NEGATIVE
C*
C*                LAT2 AND LONG2 ARE SET TO MISSING DATA VALUE IF
C*                ANY ERRORS FOUND IN LAT,LONG OR QUADRANT
C*
C****
C
C
      DIMENSION LATSIGN(4),LONSIGN(4)
C
      DATA LATSIGN/ 1,-1,-1, 1/
      DATA LONSIGN/ 1, 1,-1,-1/
C
C***  SET LAT2 AND LONG2 TO MISSING DATA VALUE
C
      LAT2=MINDIC
      LONG2=MINDIC
C
C
C     THE ARRAYS LATSIGN AND LONSIGN ARE USED TO DETERMINE IF
C     LAT. AND LONG. ARE NEGATIVE OR POSITIVE
C
C     CHECK THAT THE QUADRANT IS CORRECT
C
      IF(IQUADR .NE. 1 .AND. IQUADR .NE. 3 .AND. IQUADR .NE.
     1     5 .AND. IQUADR .NE. 7) RETURN
C
C     CHECK THAT THE LATITUDE AND LONGITUDE ARE SENSIBLE
C
      IF(LAT .LT. 0 .OR. LAT .GT. 900) RETURN
C
      IF(LONG .LT. 0 .OR. LONG .GT. 1800) RETURN
C
C
      IQ=(IQUADR+1)/2
C
      LAT2=10*LAT*LATSIGN(IQ)
      LONG2=10*LONG*LONSIGN(IQ)
C
      RETURN
C
C
      END
      SUBROUTINE STATION(IERR)
C
C**** *STATION*
C
C
C     PURPOSE.
C     --------
C         READ IN STATION LIST AND MAKE LIST OF IMPORTANT STATIONS.
C         ( WMO VOLUMEN A - LIST OF OBSERVING STATIONS)
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *STATION(IERR)*
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
C         *CALL* *IMPSTAT*
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'combuff.f'
CRR common for directories
      INCLUDE 'comdir.f'
      CHARACTER*1 ispc
C
C     ------------------------------------------------------------------
C*          1.   READ IN STATION LIST.
C                ---------------------
 100  CONTINUE
C
      ispc=char(32)
      ndx=0
C  - Find end of character string. 
      ndx=index(yppdat(1:80),ispc) - 1
      OPEN(UNIT=4,IOSTAT=IOS,ERR=300,
     1     FILE=YPPDAT(1:ndx)//'station_amend.dat',
     1     STATUS='OLD',
     1     FORM='UNFORMATTED')
C
      READ(4) IPARAMS,IPOINTS
C
      CLOSE(4)
C
C
C*           2.  FIND IMPORTANT STATIONS.
C                ------------------------
 200  CONTINUE
C
      CALL IMPSTAT
C
      RETURN
C
 300  CONTINUE
C
      WRITE(*,9901) IOS
 9901 FORMAT(1H ,' ERROR DURING OPENING  STATION FILE , ERROR=',I5)
C
C
      RETURN
      END
      SUBROUTINE IMPSTAT
C
C**** *IMPSTAT*
C
C
C     PURPOSE.
C     --------
C
C         DEFINES FROM WMO MASTER FILE THE SATION NUMBERS
C         FOR IMPORTANT STATIONS (ECMWF INTERNAL DEFINOTIONS)
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IMPSTAT*
C
C             INPUT     : IPARAMS   STATION INFORMATION IN PACKED FORM
C                         IPOINTS   NUMBER OF STATION / WMO BLOCK
C
C             OUTPUT    : IMPSTA    THE NUMBERS OF IMPORTATNT SATIONS
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
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      include 'parameter.f'
      include 'comwork.f'
      INCLUDE 'combuff.f'
C
C
C     ------------------------------------------------------------------
C*          1.   FIND IMPORTANT STATIONS.
C                ------------------------
C
      J = 0
      K = 1
C
C
            DO 300 I = 1,33000,3
C
            IF(IPARAMS(I) .EQ. MINDIC) GO TO 400
            IF(I .LT. IPOINTS(K+1)) GO TO 200
C
100         K = K + 1
            IF(K .GE. 99) GO TO 400
            IF(IPOINTS(K) .EQ. IPOINTS(K+1)) GO TO 100
C
200         CONTINUE
c
            CALL GBYTE(IPARAMS(I+2),ITEMP,28,1)
            CALL GBYTE(IPARAMS(I+2),IBIT ,25,1)
C
           IF(ITEMP .EQ. 1 .AND. IBIT .EQ. 1)
     C         THEN
                  CALL GBYTE(IPARAMS(I),III,0,10)
                  ISTA= 1000*K+ III
                  IF(J .NE. 0)
     C               THEN
                        DO 250 N=1,J
                        IF(ISTA .EQ. IMPSTA(N)) GO TO 300
250                     CONTINUE
                     END IF
                  J = J + 1
                  IF(J.GT.2000) THEN
                     PRINT*,'DIMENSION OF IMPSTA TOO SMALL'
                     GO TO 400
                  END IF
C
                  IMPSTA(J) = ISTA
               END IF
C
300        CONTINUE
C
C
400   CONTINUE
C
500   CONTINUE
C
      RETURN
      END
      SUBROUTINE LOCSTAT ( IWIND,IRC )
C
C**** *LOCSTAT*
C
C
C     PURPOSE.
C     --------
C
C         EXTRACT PARTICULARS OF WMO OBSERVING STATIONS AND
C         PUT IN DECODED REPORT HEADER.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *LOCSTAT(IWIND,IRC)*
C
C          INPUT    : ARGUMENTS NOT USED ON INPUT .
C
C                     KINT(4) - WMO STATION NUMBER IN INTEGER.
C                     KDEC(4) - INTEGER DENOTING OBSERVATION TYPE.
C
C          OUTPUT   : KDEC(5) - LATITUDE IN HUNDREDTHS OF DEGREES ,
C                               NORTH + , SOUTH - .
C                     KDEC(6) - LONGITUDE IN HUNDREDTHS OF DEGREES ,
C                               EAST + , WEST - .
C                     KDEC(8) - STATION PRESSURE ELEVATION (H/P) OR IF
C                               NONE EXISTS STATION GROUND ELEVATION (H/A).
C                               IF NEITHER EXIST MINDIC IS RETURNED . VALUE
C                               IS INTEGER IN METRES.
C
C                     KDEC(15) - IMPORTANT STATION OR GOOD QUALITY STATION
C                                FLAG BITS SET IN THIS WORD.
C
C                     KDEC(16) - WMO COUNTRY NUMBER , EXCLUDING FIRST 2
C                                DIGITS ( REGION NUMBER ) . INTEGER.
C                     KDEC(17) - WMO REGION NUMBER , INTEGER.
C
C                     KDEC(23) - PRESSURE LEVEL INDICATOR , INTEGER.
C                            0 = SEA LEVEL
C                            1 = STATION LEVEL
C                            2 = 850 HPA
C                            3 = 700 HPA
C                            4 = 500 HPA
C                            5 = 1000 GPM
C                            6 = 2000 GPM
C                            7 = 3000 GPM
C                            8 = 4000 GPM
C                            9 = 900 HPA
C
C                     IRC - INTEGER RETURN CODE
C                            0 = NO ERROR
C                            1 = STATION NUMBER NOT IN DIRECTORY
C                            2 = INVALID STATION NUMBER
C                            3 = INVALID OBSERVATION TYPE
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
C         *CALL* *GBYTE(KS,KD,KBPT,KSI)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC
C
C     MODIFICATIONS.
C     --------------
C
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
C
C     ------------------------------------------------------------------
C*          1.   LOCATE STATION.                   .
C                ---------------
 100  CONTINUE
CTEST
CTEST      PRINT*,' LOCSTAT - start, KINT(4),KDEC(4)',
CTEST     *         KINT(4),KDEC(4)
CTEST
C
C     CLEAR ERROR RETURN INDICATOR
C
      IRC = 0
C
C     INITIALIZE TYPE OF STATION
C
      ITYPE = 0
C
C     'ITYPE' IS SET TO 4 FOR SYNOP , 2 FOR PILOT AND 1 FOR TEMP.
C
      IF ( KDEC(4).EQ.11.OR.KDEC(4).EQ.14 ) ITYPE = 4
      IF ( KDEC(4).EQ.32 ) ITYPE = 2
      IF ( KDEC(4).EQ.35 ) ITYPE = 1
C
      IF ( ITYPE.EQ.0 ) THEN
                            IRC = 3
                            RETURN
                        END IF
C
C     CHECK VALIDITY OF STATION NUMBER
C
      IF ( KINT(4).LE.1000.OR.KINT(4).GT.99999 )
     C                       THEN
                                 IRC = 2
                                 RETURN
                             END IF
C
C
C     LOCATE STARTING POINT IN ARRAY 'IPARAMS' OF THE WMO BLOCK
C     OF THE STATION.
C
C     EXTRACT WMO BLOCK NUMBER
C
      II = KINT(4) / 1000
C
C     WORD 'II' OF 'IPOINTS' SHOWS WHERE THE ENTRIES FOR BLOCK 'II'
C     START IN 'IPARAMS'.
C
      IND1 = IPOINTS(II)
      IND2 = IPOINTS(II+1)-3
C
C     STARTING AT THIS WORD A SEQUENTIAL SEARCH IS MADE FOR AN ENTRY
C     FOR THE REQUIRED STATION NUMBER ( III ) >
C
      III = KINT(4) - ( II * 1000 )
C
C     FOR ONE STATION ENTRY 3 WORDS ARE USED
C
      DO 101 I=IND1,IND2,3
         CALL GBYTE(IPARAMS(I),ISTN,0,10)
         IF(ISTN.EQ.III) GO TO 200
  101 CONTINUE
C
C     STATION NUMBER NOT FOUND
C
      IRC = 1
      RETURN
C
C
C     -----------------------------------------------------------------
C*             2.  EXTRACT REQUIRED PARAMETERS FROM 1st WORD ENTRY.
C                  -----------------------------------------------
 200  CONTINUE
C
C     PRESSURE LEVEL CODE FIGURE
C
      IF(ITYPE .EQ. 4)
     C   CALL GBYTE(IPARAMS(I),KDEC(23),16,4)
C
C
C     WIND SPEED UNIT INDICATOR
C
      CALL GBYTE(IPARAMS(I),IWIND,20,1)
C
C
C     STATION ELEVATION
C
      CALL GBYTE(IPARAMS(I),KDEC(8),24,14)
      IF ( KDEC(8).GT.9999 ) KDEC(8) = KDEC(8)-16383
      IF ( KDEC(8).EQ.9999 ) KDEC(8) = MINDIC
C
C     LONGITUDE
C
      CALL GBYTE(IPARAMS(I+1),KDEC(6),8,16)
      IF ( KDEC(6).EQ.65535 ) KDEC(6) = MINDIC
      IF ( KDEC(6).NE.MINDIC.AND.KDEC(6).GT.18000)
     C      KDEC(6) = KDEC(6) - 36000
C
C
C     LATITUDE
C
      CALL GBYTE(IPARAMS(I+1),KDEC(5),24,14)
      IF ( KDEC(5).EQ.16383 ) KDEC(5) = MINDIC
      CALL GBYTE(IPARAMS(I+2),ISGN,6,1)
      IF(ISGN.EQ.1.AND.KDEC(5).NE.MINDIC)
     C                KDEC(5) = - KDEC(5)
C
C
C     WMO REGION NUMBER
C
      CALL GBYTE(IPARAMS(I+2),KDEC(17),8,3)
      IF ( KDEC(17).EQ.0 ) KDEC(17) = 8
C
C
C     WMO COUNTRY NUMBER ( LAST 3 DIGITS )
C
      CALL GBYTE(IPARAMS(I+2),KDEC(16),11,10)
C
C
C     IMPORTANT STATION AND GOOD QUALITY FLAGS.
C
      CALL GBYTE(IPARAMS(I+2),ISGQ,24,2)
cpsjun11      KDEC(15) = KDEC(15).OR.ISGQ
      KDEC(15) = IOR(KDEC(15),ISGQ)
C
C
C     CHECK THAT PARAMETERS ARE VALID FOR OBSERVATION TYPE REQUESTED.
C     SOME STATIONS HAVE MORE THAN 1 ENTRY , DEPENDING ON TYPE OF
C     OBSERVATION.
C
      IF(ITYPE.EQ.1) ISKIP=26
      IF(ITYPE.EQ.2) ISKIP=27
      IF(ITYPE.EQ.4) ISKIP=28
C
      CALL GBYTE(IPARAMS(I+2),ITP,ISKIP,1)
      IF (  ITP.NE.0 ) RETURN
C
C     PARAMETERS NOT CORRECT FOR CODE TYPE , SO USE NEXT ENTRY
C     IF IT EXIST
C
      I = I + 3
C
      CALL GBYTE(IPARAMS(I),ISTN,0,10)
      IF(ISTN.EQ.III) GO TO 200
C
C     RETAIN ALREADY EXTRACTED PARAMETERS
C     THAT MEAN THAT STATION TYPE DOES NOT CORRESPOND TO THE MESSAGE
C     RECEIVED. 
C     
      RETURN
C
C
      END
      SUBROUTINE FIXUS
cps This subroutine is never used
C
C**** *FIXUS*
C
C
C     PURPOSE.
C     --------
C         TRY TO FIX ERROR ON TEMP DECODING.
C
C**   INTERFACE.
C     ----------
C
C         *XXXX* *XXXXXXX*
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
C     ------------------------------------------------------------------
C
C*          1.   CLEAR PARITY BITS.
C                ------------------
 100  CONTINUE
C
      DO 101 I=IPT,IGS
         KCHAR(I) = IAND(KCHAR(I),127)
101   CONTINUE
C
C
      CALL REMEEE
C
      RETURN
      END
      SUBROUTINE NEXTNI(ISTEP,IFIRST,LEVNUM,LEVPRE,IREMPOS,NUMERR,
     C                  * , * , * , * , * , * , * , * )
C
C**** *NEXTNI*
C
C
C     PURPOSE.
C     --------
C
C
C         FIND THE NEXT PRESSURE LEVEL IN TEMP PART B & D
C
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTNI(ISTEP,IFIRST,LEVNUM,LEVPRE,IREMPOS,NUMERR,
C    C                  * , * , * , * , * , * , * , * )*
C          INPUT      ISTEP   = 5 FOR TEMPERATURE LEVELS
C                             = 4 FOR WIND LEVELS
C                     IFIRST  = 0 IF FIRST DECODING ATTEMPT
C                             = 1 OTHERWISE
C                     LEVNUM  = THE NUMBER OF THE LEVEL (RUNNING FROM 0 -)
C                     LEVPRE  = INDICATOR OF THE PREVIOUS LEVEL
C
C          OUTPUT     LEVNUM
C                     IREMPOS   INDICATOR TO THE POINT OF THE FIRST ERROR
C                               (USED AS POINTER OF DELETION IF THERE ARE
C                               TOO MANY ERRORS).
C
C
C     METHOD.
C     -------
C
C         MULTIPLE RETURN ADDRES
C         1.ST  IF THE BEGINNING OF WIND SECTION FOUND
C         2.ND  -- --- --------- -- CLOUD ------ -----
C         3.RD  -- --- --------- -- REGIONAL --- -----
C         4.TH  -- --- --------- -- NATIONAL --- -----
C         5.TH  -- --- END OF REPORT             -----
C         6.TH  --     NOTHING USEFUL            -----
C         7.TH  -- --- FOLLOWING LEVEL           -----
C
C
C
C     EXTERNALS.
C     ----------
C
C           SUBROUTINE CALLED
C           ERRSTA  -  TO REGISTER ERRORS
C           EXTVAL  -  TO EXTRACT A VALUE
C           LEVCHEC -  TO CHECK THE PRESSURE LEVELS
C           NEXPRT2 -  SCANNING ROUTINE
C           NEXSEP2 -  SCANNING ROUTINE
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
C          A. HOLOPAINEN  DEC.83
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
      DATA N1N2PRE/-9999/
      DATA IFIX / 0 /
C
C     ------------------------------------------------------------------
C
C*          1.   FIND NEXT LEVEL IN THE B AND D PART OF TEMP REPORT.
C                ---------------------------------------------------
 100  CONTINUE
C
C
      ITRY = 0
      IDUP = 0
      IPART = 4
      IF(KDEC(23) .EQ. 4) IPART = 6
C
      IF(LEVNUM .LE. 0) N1N2PRE = -9999
      IF(LEVNUM .LE. 0) IFIX = 0
C
101   CONTINUE
C
C     NINI PIPIPI
C     ---- ------
C
      CALL NEXPRT2(IPT,IEQ,*9999)
C
C
      N1 = IAND(KCHAR(IPT),127) - 48
      N2 = IAND(KCHAR(IPT+1),127) - 48
      N1N2 = 10*N1 + N2
C
C
C     CHECK IF THIS IS CORRECT BEGINNING OF SIGNIFICANT LEVEL (N1=N2)
C
C     CHECK IF N1N2 = //
C
      IF(N1 .EQ. -1 .AND. N2 .EQ. -1)
     C   THEN
            CALL NEXSEP2(IPT,IEQ,*9999)
            GO TO 101
         END IF
C
C     CHECK IF THIS IS PART D AND NINI=00.  REJECT IT BEACUSE
C     SOME STATIONS INCLUDE INCORRECTLY STATION LEVEL IN PART D.
C
      IF(IPART .EQ. 6)
     C   THEN
            IF(N1N2 .EQ. 0)
     C         THEN
                  CALL NEXPRT2(IPT,IEQ,*9999)
                  CALL NEXSEP2(IPT,IEQ,*9999)
                  CALL NEXPRT2(IPT,IEQ,*9999)
                  CALL NEXSEP2(IPT,IEQ,*9999)
                  GO TO 101
               END IF
         END IF
C
      IPOS = 17
      IF(ISTEP .EQ. 4) IPOS = 318
C
      IF(LEVNUM .GT. 0)
     C   THEN
            IPOS = 17 + (LEVNUM-1)*5
            IF(ISTEP .EQ. 4) IPOS = 318 + (LEVNUM-1)*4
         END IF
C
C
C
      IF (N1 .NE. N2)
     C               THEN
C
C                       CHECK IF THIS IS THE BEGINNING
C                       OF SECTION 6,7,8,9,10
C
                        CALL EXTVAL(IPT,5,IVAL)
C
                        IF(ISTEP .EQ. 5)
     C                     THEN
                              IF(IVAL .EQ. 21212) RETURN 1
                           END IF
C
                        IF(IVAL.EQ.31313) RETURN 8
                        IF(IVAL .EQ. 41414) RETURN 2
C
                        IF((IVAL .EQ. 51515) .OR. (IVAL .EQ. 52525) .OR.
     C                     (IVAL .EQ. 53535) .OR. (IVAL .EQ. 54545) .OR.
     C                     (IVAL .EQ. 55555) .OR. (IVAL .EQ. 56565) .OR.
     C                     (IVAL .EQ. 57575) .OR. (IVAL .EQ. 58585) .OR.
     C                     (IVAL .EQ. 59595) .OR. (IVAL .EQ. 51525))
     C                     RETURN 3
C
                        IF((IVAL .EQ. 61616) .OR. (IVAL .EQ. 62626) .OR.
     C                     (IVAL .EQ. 63636) .OR. (IVAL .EQ. 64646) .OR.
     C                     (IVAL .EQ. 65656) .OR. (IVAL .EQ. 66666) .OR.
     C                     (IVAL .EQ. 67676) .OR. (IVAL .EQ. 68686) .OR.
     C                     (IVAL .EQ. 69696)) RETURN 4
C
C                       ERROR, IF NOT ANY SIGNIFICANT LEVEL OR OTHER
C                       SECTION. CHECK IF ONLY NINI IS ERRONEUSLY CODED.
C                       IF THE LEVL HAS TO BE REJECTED TRY TO FIND THE
C                       NEXT LEVEL.
C
                        NPT = IPT
                        CALL NEXSEP2(NPT,IEQ,*9999)
                        CALL NEXPRT2(NPT,IEQ,*9999)
C
                        IF(ITRY .EQ. 0 .AND. IFIX .EQ. 0)
     C                     THEN
                              KERR = 5
                                CALL ERRSTA(IPART,1,IFIRST,NUMERR)
                              IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
C
C                             CHECK IF THERE WERE DUPLICATE GROUPS
C                             ON PREVIOUS LEVEL
C
                              IDUP = 0
                              CALL DUPLIGR(IDUP)
C
                              IF(IDUP .EQ. 1)
     C                           THEN
                                    ITRY = ITRY + 1
                                    IFIX = IFIX + 1
                                    IFIXPOS = IPOS + ISTEP
                                    DO 150 I = IPOS,IPOS+4
                                    KINT(I) = MINDIC
150                                 CONTINUE
                                    GO TO 101
                                 END IF
C
C
C                             CHECK IF NINI GROUP IS ERRONEUS SO THAT
C                             ONE OF THE NI'S IS CORRECT.  IN THIS
C                             CASE CHECK THE 'NINI' OF THE NEXT LEVEL
C                             AND THE PRESSURE OF THIS LEVEL COMPARED
C                             TO THE PREVIOUS LEVEL.  ONLY IF ALL THESE
C                             CONDITIONS ARE FULLFILLED ACCEPT THIS
C                             THIS LEVEL.
C
                              NPT = IPT
                              CALL NEXSEP2(NPT,IEQ,*9999)
                              CALL NEXPRT2(NPT,IEQ,*9999)
                              CALL NEXSEP2(NPT,IEQ,*9999)
                              CALL NEXPRT2(NPT,IEQ,*9999)
                              IF(KCHAR(NPT) .EQ. KCHAR(NPT+1))
     C                           THEN
                                    N1PRE = N1N2PRE/10
                                    N2PRE = N1N2PRE - 10*N1PRE
                                    IF(LEVNUM .EQ. 0)
     C                                 THEN
                                          N1PRE = 0
                                          N2PRE = 0
                                       END IF
                                    N1NEW = N1PRE + 1
                                    N2NEW = N2PRE + 1
                                    IF(N1NEW .EQ. 10) N1NEW = 1
                                    IF(N2NEW .EQ. 10) N2NEW = 1
C
                                    IF(N1 .EQ. N1NEW)
     C                                 THEN
                                          N1N2 = 10*N1 + N1
                                          ITRY = ITRY + 1
                                          IFIX = IFIX + 1
                                          IFIXPOS = IPOS + ISTEP
                                          GO TO 300
                                       ELSE
                                          IF(N2 .EQ. N2NEW)
     C                                       THEN
                                                N1N2 = 10*N2 + N2
                                                ITRY = ITRY + 1
                                                IFIX = IFIX + 1
                                                IFIXPOS = IPOS + ISTEP
                                                GO TO 300
                                             END IF
                                       END IF
                                 END IF
C
C
                             ELSE
                                CALL ERRSTA(IPART,1,IFIRST,NUMERR)
                                 IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                             END IF
C
C
                          IF(LEVNUM .GE. 1 .AND. ITRY .GT. 1)
     C                       THEN
                                DO 200 I=IPOS,IPOS+4
                                KINT(I) = MINDIC
200                             CONTINUE
                             END IF
C
                          IF(IFIX .GE. 2)
     C                       THEN
                                DO 250 I=IFIXPOS,600
                                KINT(I) = MINDIC
250                             CONTINUE
                             END IF
C
                             ITRY = ITRY + 1
                             IF(ITRY .EQ. 1)
     C                          THEN
                                   IF(N2 .EQ. -16)
     C                                THEN
                                         NPT = IPT
                                         CALL PRESEP(NPT,IMI,*270)
                                         CALL PREPRT(NPT,IMI,*270)
                                         LEN = IPT - NPT + 1
                                        IF(LEN .NE. 5 .AND. LEN .NE. 10)
     C                                     THEN
                                              DO 260 I = IPOS,IPOS+6
                                              KINT(I) = MINDIC
260                                           CONTINUE
                                           END IF
                                      END IF
                                END IF
C
270   CONTINUE
                          CALL NEXSEP2(IPT,IEQ,*9999)
                          GO TO 101
                       END IF
C
C
C
C
C
C
300   CONTINUE
C
      CALL EXTVAL(IPT+2,3,IVAL2)
C
C
C
      IF(IVAL2 .EQ. MINDIC)
     C   THEN
            IF(KCHAR(IPT+2).EQ.47 .AND. KCHAR(IPT+3).EQ.47
     C        .AND. KCHAR(IPT+4).EQ.47 )
     C        THEN
                 CALL NEXSEP2(IPT,IEQ,*9999)
                 GO TO 101
              END IF
C
            IF(IPART .EQ. 4)
     C         THEN
                  KERR = 5
                  CALL ERRSTA(IPART,2,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  CALL NEXSEP2(IPT,IEQ,*9999)
                  GO TO 101
               END IF
C
C           COMMON PRACTICE IN PART D IS TO REPLACE THE LAST DIGIT
C           OF PRESSURE WITH /.  THIS IS REPLACED WITH 0.
C
C
            IF(IPART .EQ. 6)
     C         THEN
                 IF( (KCHAR(IPT+2).GE.48 .AND. KCHAR(IPT+2).LE.57) .AND.
     C               (KCHAR(IPT+3).GE.48 .AND. KCHAR(IPT+3).LE.57) .AND.
     C               (KCHAR(IPT+4).EQ.47) )
     C              THEN
                       KCHAR(IPT+4) = 48
                       CALL EXTVAL(IPT+2,3,IVAL2)
                    ELSE
                       KERR = 5
                       CALL ERRSTA(IPART,2,IFIRST,NUMERR)
                       IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                       CALL NEXSEP2(IPT,IEQ,*9999)
                       CALL NEXPRT2(IPT,IEQ,*9999)
                       CALL NEXSEP2(IPT,IEQ,*9999)
                       GO TO 101
                    END IF
                END IF
         END IF
C
C     CHECK IF THIS IS PART B AND PRESSURE IS < 100 MB
C
      IF(IPART .EQ. 4 .AND. LEVNUM .GT. 3 .AND. IVAL2 .LT. 100)
     C   THEN
C
C           SEE IF '21212' - INDICATOR WAS PASSED UNDETECTED
C
            IF(ISTEP .EQ. 5)
     C         THEN
                  CALL PRE2121(IFOUND)
                  IF(IFOUND .EQ. 1)
     C               THEN
                        NEWSEC = 21212
                        KINT(317) = 21212
                        RETURN 1
                     END IF
               END IF
C
            CALL NEXSEP2(IPT,IEQ,*9999)
            GO TO 101
         END IF
C
C
C     IF ERROR FOUND IN 'NINI' DONT ACCEPT NINI .LE. THE PREVIOUS
C
      IF(ITRY .GT. 0 .AND. IDUP .NE. 1)
     C   THEN
            IF(N1N2 .LE. N1N2PRE .AND. N1N2PRE .NE. 99)
     C         THEN
                  CALL NEXSEP2(IPT,IEQ,*9999)
                  GO TO 101
               END IF
         END IF
C
C
C
C
C     CHECK THE PRESSURE AGAINST THE PREVIOUS LEVEL.  IF IT
C     LOWER CHECK WHETHER THIS LEVEL IS INCORRECT OR IF IT
C     WAS THE PREVIOUS ONE. REJECT THE ERRONEUS LEVEL.
C     FOR PART B THIS TEST IS CARRIED OUT ONLY FROM THIRD LEVEL
C     ONWARDS, BECAUSE FIRST TWO (OR THREE) LEVELS MAY BE OVER
C     1000 HPA AND ONLY THREE LAST FIGURES ARE USED (I.E. 11030,
C     22020,....)
C
      IF(LEVPRE .EQ. MINDIC) GO TO 500
C
      IF(LEVNUM .GT. 2 .OR. IPART .EQ. 6)
     C   THEN
            IF(IVAL2 .GT. LEVPRE)
     C         THEN
                  IF(IPART .EQ. 4 .AND. LEVNUM .EQ. 3 .AND.
     C               LEVPRE .LT. 50) GO TO 500
C
C
C                 CHECK IF ALL THE PREVIOUS LEVELS WERE ABOVE 1000MB.
C
                  IF(IPART .EQ. 4 .AND. LEVPRE .LT. 50)
     C               THEN
                        DO 400 I = 1,LEVNUM
                        IND = 18 + (I-1)*5
                        IF(ISTEP .EQ. 4) IND = 319 + (I-1)*4
                        IF(KINT(IND) .GT. 50) GO TO 410
400                     CONTINUE
                        GO TO 500
                     END IF
C
410            CONTINUE
C
C
               KERR = 8
C
               IND = 18 + (LEVNUM-1)*5
               IF(ISTEP .EQ. 4) IND = 319 + (LEVNUM-1)*4
               IF(KINT(IND) .LT. 100 .AND. KINT(IND) .NE. MINDIC)
     C            THEN
                     DO 450 I = 1,ISTEP
                     J = IND+I-1
                     KINT(J) = MINDIC
450                  CONTINUE
                  END IF
C
               CALL ERRSTA(IPART,1,IFIRST,NUMERR)
               IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
C                 CHECK WHICH LEVEL IS INCORRECT, OR IF
C                 THE BEGINNING OF NEXT SECTION HAS BEEN
C                 PASSED UNNOTICED (MAY HAPPEN IN CASE
C                 OF MISSING GROUP).
C
                  CALL LEVCHEC(LEVNUM,IPOS,ISTEP,LEVER,NEWSEC)
C
C
                  IF(NEWSEC .EQ. 21212) RETURN 1
                  IF(NEWSEC.EQ.31313) RETURN 8
                  IF(NEWSEC .EQ. 41414) RETURN 2
                  IF(NEWSEC .EQ. 51515) RETURN 3
                  IF(NEWSEC .EQ. 61616) RETURN 4
C
C
                  IF(LEVER .EQ. 2) GO TO 500
                  IF(LEVER .EQ. 3)
     C               THEN
                        ITRY = ITRY + 1
                        N1N2PRE = N1N2
                        GO TO 101
                     END IF
C
C
                 RETURN 6
              END IF
           END IF
C
C
500   CONTINUE
      N1N2PRE = N1N2
      RETURN 7
C
9999  CONTINUE
      RETURN 5
C
      END
      SUBROUTINE DDFFF(IDD,IFF,IWW,ICOUNT,MINDIC,IDIR,ISPEED)
C
C**** *DDFFF*
C
C
C     PURPOSE.
C
C
C         DECODE WIND DIRECTION AND SPEED
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DDFFF(IDD,IFF,IWW,ICOUNT,MINDIC,IDIR,ISPEED)*
C
C          INPUT    :  IDD     - WIND DIRECTION IN TENS OF DEGREE
C                   :  IFF     - WIND SPEEDIN METERS/SEC OR KNOTS
C                   :  IWW     - WIND SPEED INDICATOR (1 FOR KNOTS)
C                   :  ICOUNT  - COUNTRY NUMBER   cps: not used
C                   :  MINDIC  - MISSING DATA VALUE
C
C          OUTPUT   :  IDIR    - DECODED WIND DIRECTION
C                      ISPEED  - DECODED WIND SPEED
C
C              IDIR AND ISPEED ARE SET TO MISSING VALUE IF
C              ANY ERRORS FOUND IN IDD, IFF OR IWW
cps    Rather: nothing is done with idir and ispeed in this case. 
cps    So they keep their initial (hopefully) missing values.
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
C          A.HOLOPAINEN  JAN.83
C
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
cps: not used      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
C     ------------------------------------------------------------------
C*          1.   DECODE WIND DIRECTION AND SPEED.
C                --------------------------------
 100  CONTINUE
C
C      CHECK MISSING DATA INDICATOR
C
      IF(IDD .EQ. MINDIC .OR. IFF .EQ. MINDIC) RETURN
C
C     CHECK IF IDD IS FEASABLE
C
      IF(IDD .LT. 0 .OR. IDD .GT. 36) RETURN
C
      ISPEED=IFF
      IDIR=IDD
C
C
C     CHECK IF DD=99 .I.E. DIRECTION INDETERMINATE
cpsjun99 This is not a possible value for pilots. Have also 
cps      commented out the other tests below for idir, as 
cps      we don't see the reason behind these tests.
C
C
C     CHECK IF DD INDICATOR IS SENSIBLE
C
cps      IF(IDIR .GT. 36 .AND. IDIR .LT. 50) RETURN
cps      IF(IDIR .GT. 86 .AND. IDIR .NE. 99) RETURN
C
C
C
C
cps      IF(IDIR .GT. 50 .AND. IDIR .NE. 99) IDIR=IDIR-50
cpsCLIL      IF(IDIR .EQ. 99) IDIR=62
cps      IF(IDIR .EQ. 99) IDIR=0
C
      IDIR=IDIR*10
C
      IF(ISPEED .GE. 500) THEN
                             ISPEED=ISPEED-500
                             IDIR=IDIR+5
                            END IF
C
C
C     IF SPEED IN KNOTS MODIFY IT TO M/S
C
crr  change accuracy of wind by multiplying by 10.
      ispeed=ispeed*10
      IF(IWW .EQ. 1) CALL KTOMPSI(ISPEED)
C
cpssep06 12 bits with scale 1 in bufr means that values > 4094 cannot
cps      be stored correctly (4095 = missing value)
      if (ispeed.gt.4094) then
         print*,'FFF too big - set to missing ',IFF
         ispeed = mindic
      end if

      RETURN
      END
      SUBROUTINE PRE2121 (IFOUND)
C
C**** *XXXXXXX*
C
C
C     PURPOSE.
C     --------
C
C         SCANS THE REPORT BACKWARDS TRYING TO FIND
C         '21212'-GROUP OR CORRUPTED '21212'-GROUP, WHERE
C         ALL BUT ONE FIGURE MATCH WITH CORRECT GROUP.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRE2121 (IFOUND)*
C
C          INPUT     : IPT    -   POINTER TO THE BEGINNING OF SCANNING
C
C          OUTPUT    : IFOUND = 1  IF THE GROUP WAS FOUND
C                             = 0  OTHERWISE
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
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
      DIMENSION ICONST(5),KK(5)
C
      DATA ICONST / 20000 , 1000 , 200, 10 , 2 /
C
C
C     ------------------------------------------------------------------
C*          1.   TRY TO FIND 21212 GROUP.
C                ------------------------
 100  CONTINUE
C
C
      IFOUND = 0
      JPT = IPT
C
C     UP TO THREE PREVIOUS GROUPS ARE CHECKED
C
C
         DO 101 IROU=1,3
C
         CALL PRESEP(JPT,IMI,*10000)
         CALL PREPRT(JPT,IMI,*10000)
         CALL PRESEP(JPT,IMI,*10000)
         CALL NEXPRT2(JPT,IEQ,*10000)
C
C
C        SEE IF THIS IS COMPLETE '21212' - GROUP
C
         CALL EXTVAL(IPT,5,IVAL)
C
         IF(IVAL .EQ. 21212)
     C      THEN
               IFOUND = 1
               IPT = JPT
               RETURN
            END IF
C
            DO 102 I=1,5
            J = JPT+I-1
            KK(I) = IAND(KCHAR(J),127)
            KK(I) = KK(I) - 48
102         CONTINUE
C
C
            DO 103 I=1,5
            KCA = 0
               DO 104 J=1,5
               IF(J .EQ. I)
     C            THEN
                     KCA = KCA + ICONST(I)
                  ELSE
                     KCA = KCA + KK(J) * 10**(5-J)
                  END IF
104            CONTINUE
            IF(KCA .EQ. 21212)
     C         THEN
                  IFOUND = 1
                  IPT = JPT
                  RETURN
               END IF
103         CONTINUE
101      CONTINUE
C
10000 CONTINUE
C
      RETURN
      END
      SUBROUTINE INITVAR ( IERR )
C
C**** *INITVAR*
C
C
C     PURPOSE.
C     --------
C         DECLARATION OF COMMON AREAS AND INITIALISATION OF
C         VARIABLES.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *INITVAR(IERR)*
C
C           IERR - SET TO 0 .
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER *10 IFA
      CHARACTER CUNIT*24,CVAL*9
C
      CHARACTER CTABBEN*64,CTABBU*24
      CHARACTER*8 YDATE
      CHARACTER*5 YTIME,YFAMILY,YTASK
CTEST
      CHARACTER*24 CORINT
CTEST
      CHARACTER*1 ispc
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comtab.f'
      INCLUDE 'comtabc.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
      INCLUDE 'comstd.f'
CRR common for directories
      INCLUDE 'comdir.f'
C
C
C     ------------------------------------------------------------------
C           1.   INITIALISE MISSING DATA INDICATOR.
C                ----------------------------------
 100  CONTINUE
C
      DATA MINDIC / '17777777777'O /
      OPS2=.TRUE.
      NSUB=0
C     -------------------------------------------------------------------
C*          2.   INITIALIZE DEFAULT VALUES FOR ERROR HANDLING OPTIONS.
C                ------------------------------------------------------
 200  CONTINUE
C                'IOPTS' CONTAINS 1 WORD FOR ANY POSSIBLE
C                 BULLETIN TYPE FROM 'TT' OF 'AA' TO 'ZZ' .
C                 BIT SETTINGS HAVE THE FOLLOWING MEANING.
C                 BITS ARE NUMBERED LEFT TO RIGHT , 59 TO 0 .
C
C                 BIT N  = 1 MEANS REPORT WITH ERROR NUMBER N+1 IS TO BE
C                            WRITTEN TO THE ERROR FILE .
C                          0 MEANS REPORT NOT TO BE WRITTEN TO ERROR FILE.
C
C
C
C
C                 WORDS 677 AND 678 DEFINE DEFAULT OPTIONS FOR HANDLING OF
C                 BULLETIN ERRORS.
C
C                 WORD 678 : BIT N = 1 MEANS BULLETIN WITH ERROR N+1 IS TO BE
C                                      WRITTEN TO THE ERROR FILE.
C                 WORD 677 : BIT N = 1 MEANS BULLETIN WITH ERROR N+1 IS TO BE
C                                      PRINTED.
C                 NO ACTION IF BIT N = 0 .
C
C
C                 DEFAULT FOR HANDLING REPORT ERRORS IS TO WRITE
C                 ALL REPORTS WITH ERRORS TO THE ERROR FILE.
C
C                 DEFAULT BULLETIN HANDLING IS TO DISCARD 'NIL' BULLETINS ,
C                 SHORT ( LESS THAN 3 LINES ) BULLETINS AND NATIONAL
C                 BULLETINS FROM UK AND FRG. BULLETINS IN ERROR ARE NOT
C                 PRINTED.
C
      DATA IOPTS / 676*'17777777777'O,'0'O,'2'O /
CRR  for writing error report on screen
CRR   DATA IOPTS / 676*'17777777777'O,'2'O,'2'O /
C
C     -------------------------------------------------------------------
C*          3.    CLEAR COUNTERS.
C                 ---------------
 300  CONTINUE
C
C                 NUMBULL : NUMBER OF BULLETINS HANDLED BY DECODING RUN.
C                 NUMBERR : NUMBERS OF BULLETIN ERRORS.
C                 NOER    : NUMBERS OF REPORT ERRORS.
C                 NUMREP  : NUMBERS OF REPORTS HANDLED BY DECODING RUN.
C
      DATA NUMBULL / 0 /
      DATA NUMREP /26*0/
      DATA NUMBERR / 8*0 /
      DATA NOER /1560*0/
      DATA NUMRERR /26*0/
      DATA IMPSTA/2000*0/
      DATA IGRIBS /0/
CTEST
      DATA NTEST /200*0/
CTEST
C
C     -------------------------------------------------------------------
C*          4.   SET END OF FILE INDICATOR.
C                ---------------------------
 400  CONTINUE
C
      DATA IEOF / 0 /
C     -------------------------------------------------------------------
C*          5.   INITIALIZE ERROR INDICATOR.
C                ---------------------------
 500  CONTINUE
C
      IERR = 0
C     -----------------------------------------------------------------
C*          6.   SET UP NUMBER OF BITS PER COMPUTER WORD.
C                ----------------------------------------
 600  CONTINUE
C
      NBPW=32
C     -----------------------------------------------------------------
C*          7.   SET UP BIT MASKS.
C                -----------------
 700  CONTINUE
C
cpsjun11      DATA ( MASKS(J),J=1,32)/
      DATA ( MASKS(J),J=1,31)/
     1                             '1'O,                      '3'O,
     2                             '7'O,                     '17'O,
     3                            '37'O,                     '77'O,
     4                           '177'O,                    '377'O,
     5                           '777'O,                   '1777'O,
     6                          '3777'O,                   '7777'O,
     7                         '17777'O,                  '37777'O,
     8                         '77777'O,                 '177777'O,
     9                        '377777'O,                 '777777'O,
     A                       '1777777'O,                '3777777'O,
     1                       '7777777'O,               '17777777'O,
     2                      '37777777'O,               '77777777'O,
     3                     '177777777'O,              '377777777'O,
     4                     '777777777'O,             '1777777777'O,
     5                    '3777777777'O,             '7777777777'O,
cpsjun11     6                   '17777777777'O,            '37777777777'O  /
     6                   '17777777777'O  /
C
CRR
C     ------------------------------------------------------------------
C*          7A.  SET UP DIRECTORY NAMES FOR FILES
C                --------------------------------
 710  CONTINUE
C
CRR Moved to comdir.h
C
CRR
C     ------------------------------------------------------------------
C*          8.   SET UP BUFR TABLE B.
C                --------------------
 800  CONTINUE
C
      CALL SETTAB( IERR )
      IF(IERR.NE.0) CALL EXIT(2)
C     --------------------------------------------------------------------
C*          9.   SET UP WMO STATION LIST AND IMPORTANT STATIONS.
C                -----------------------------------------------
 900  CONTINUE
C
      CALL STATION(IERR)
      IF(IERR.NE.0) CALL EXIT(2)
C
C     --------------------------------------------------------------------
C*          10.   SET UP HEIGHT AND PRESSURE OF STANDARD ATMOSPHERE.
C                ---------------------------------------------------
1000  CONTINUE
C
      ispc=char(32)
      ndx=0
C  - Find end of character string. 
      ndx=index(yppdat(1:80),ispc) - 1
      OPEN(UNIT=8,FILE=YPPDAT(1:ndx)//'std_atm.dat',
     1             STATUS='OLD')
c
      DO 1001 I=1,512
      READ(UNIT=8,FMT=7777,ERR=1010,END=1100,IOSTAT=IOS)  SH(I),P(I)
 7777 FORMAT(1X,F5.0,2X,F8.2)
 1001 CONTINUE
C
      CLOSE(8)
C
      GO TO 1100
c
 1020 CONTINUE
C
      PRINT*,' Open error ',ios,' on file standard_atmosphere.dat'
      CALL EXIT(2)
C
 1010 CONTINUE
c
      PRINT*,' Read error ',ios,' on file standard_atmosphere.dat'
      CALL EXIT(2)
C  
C*            11. GET SUPERVISOR DATE,TIME,FAMILY AND TASK.
C                 -----------------------------------------
 1100 CONTINUE
C
CTEST
      PRINT*,'  INITVAR - Read of supervisor date, etc - commented out '
CTEST
CRR   READ(*,'(A8,A5,A5,A5)') YDATE,YTIME,YFAMILY,YTASK
CTEST added CORINT-character*24
      CALL FDATE(CORINT)
      PRINT*,' Current time = ',CORINT
CTEST
C
      RETURN
      END
      SUBROUTINE ERRFILE( IHEAD,IERR )
cps This subroutine is never used
C
C**** *ERRFILE*
C
C
C     PURPOSE.
C     --------
C         WRITE PROBLEM BULLETIN TO THE ERROR FILE TOGATHER WITH
C         KEY.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ERRFILE(IHEAD,IERR)*
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
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
      CHARACTER*1 Y63
      CHARACTER*80 YOUT,YOUTA
      CHARACTER*16384 YCHAR
      CHARACTER*3 YSPEC
C
      YSPEC=CHAR(13)//CHAR(13)//CHAR(10)
      Y63  =CHAR(63)
C     ------------------------------------------------------------------
C
C*          1.   WRITE BULLETIN TO THE ERROR FILE.
C                ---------------------------------
 100  CONTINUE
C
      J1=1
C
C*          1.1  OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 110  CONTINUE
C
              OPEN(UNIT=10,IOSTAT=IOS,ERR=400,
     C        FILE=cstream(msys)//'/'//'pilo_error_file',
     C        STATUS='UNKNOWN'   )
C
C     -----------------------------------------------------------------
C*          2. INITIALIZE POINTERS.
C              --------------------
 200  CONTINUE
C
      IST=1
      IEND=1
C
C
C     -----------------------------------------------------------------
C*          3. WRITE BULLETIN INTO ERROR FILE AND MARK ERROR.
C              ----------------------------------------------
 300  CONTINUE
C
      YOUT=' '
      YOUTA=' '
C
      CALL NEXTEND(IEND,ILEN)
      IF(IEND.GT.ILEN) GO TO 500
      CALL NEXTPRT(IEND,ILEN)
C      IF(IEND.GT.ILEN) GO TO 500
      IEND=IEND-1
C
      II=0
C
      DO 301 I=IST,IEND
C
      II=II+1
      IF(KCHAR(I).GT.127) THEN
                             YOUTA(II:II)=Y63
                             ISIGN=1
                          END IF
      YOUT(II:II)=CHAR(IAND(KCHAR(I),127))
C
 301  CONTINUE
C
      IEND=IEND+1
      IST=IEND
C
      WRITE(10,'(A)') YOUT
      IF(ISIGN.EQ.1)  WRITE(10,'(A)') YOUTA
      ISIGN=0
C
C
      GO TO 300
C
 400  CONTINUE
C
      PRINT*,'+++ ERROR DURING OPENNING UNIT 10 +++, IOS=',IOS
C
 500  CONTINUE
C
      RETURN
      END
      SUBROUTINE PILOEXP(K,IERR )
C
C**** *PILOEXP*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR PILOT DATA.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PILOEXP(K,IERR)*
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
C         *CALL* *DATUM(I,J,K)*
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comtab.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f'
C
      CHARACTER*9 CVAL,CIDENT
      CHARACTER*24 CUNIT
C
      INCLUDE 'comwtc.f'
C
C     ------------------------------------------------------------------
C*          1.   INCREASE COUNTER OF SUBSETS BY ONE.
C                -----------------------------------
 100  CONTINUE
C
      IF(IERR.NE.0) RETURN
C
C     CHECK IF NIL REPORT
C
      IF(KDEC(20).EQ.1) THEN
         IERR=1
         RETURN
      END IF
C       
      NSUB=1
      N   =NSUB
      IREP=0
C
      DO 101 I=1,JP22
      DO 102 J=1,JP1
      VALUES(I,J)=999999.
 102  CONTINUE
 101  CONTINUE
C
C*          1.1  GET CURRENT DATE (YEAR AND MONTH).
C                ----------------------------------
 110  CONTINUE
C
      CALL DATUM(KDEC(1),IMONTH,IYEAR)
C
C     ------------------------------------------------------------------
C*          2.   PILOT - LAND BASED STATION.
C                --------------------------
 200  CONTINUE
C
      IF(KDEC(4).EQ.32) THEN
C     -----------------------------------------------------------------
C*          2.1  PILOT PART A OR C.
C                ------------
 210  CONTINUE
C
         IF(K.EQ.1.OR.K.EQ.3) THEN
            VALUES( 1,NSUB)=(KINT(12)-48)*10+KINT(13)-48
            VALUES( 2,NSUB)=(KINT(14)-48)*100+(KINT(15)-48)*10
     1                      +KINT(16)-48
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
cpsapr98            VALUES( 5,NSUB)=FLOAT(IYEAR)+1900
            VALUES( 5,NSUB)=FLOAT(IYEAR)
            VALUES( 6,NSUB)=FLOAT(IMONTH)
            VALUES( 7,NSUB)=FLOAT(KDEC(1))
            VALUES( 8,NSUB)=FLOAT(KDEC(2))
            VALUES( 9,NSUB)=FLOAT(KDEC(9))
            VALUES(10,NSUB)=KDEC(5)/100.
            VALUES(11,NSUB)=KDEC(6)/100.
            VALUES(12,NSUB)=999999.
            IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
C
            IREP=0
            IP=14
            IPKD=25
C
            IL=9
            IF(K.EQ.3) IL=5
C
            DO 211 I=1,IL
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 211
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=kdec(IPKD)*10.
            VALUES(IP+1,NSUB)=32.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+5
            IPKD=IPKD+3
C
 211        CONTINUE
C
C
C           MAX. WIND LEVEL  ( pressure)
C
            IPKD=52
C
            DO 213 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
               ELSE
                  GO TO 215
               END IF

C
 213        CONTINUE
C
 215  CONTINUE
C
C           MAX. WIND LEVEL  ( geopotential )
C
            IPKD=67
C
            DO 2130 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=999999.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  IF(KDEC(IPKD).NE.MINDIC)
     1             VALUES(IP+2,NSUB)=kdec(ipkd)*9.8
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
               ELSE
                  GO TO 2150
               END IF

C
 2130       CONTINUE
C
 2150 CONTINUE
C
C
C           UPDATE REPLICATION FACTOR
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C 
            VALUES(13,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           WIND SHEAR AT MAX. WIND LEVEL ( pressure )
C
            IPKD=52
            ISHEAR=0
            IPTSH=IP
            IP=IP+1
C
            DO 216 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  ISHEAR=ISHEAR+1
                  VALUES(IPTSH,NSUB)=FLOAT(ISHEAR)
                  VALUES(IP  ,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
C
               ELSE
                  IF(ISHEAR.EQ.0) GO TO 217 
                  GO TO 2170
               END IF

C
 216        CONTINUE
C
 217  CONTINUE
C
C           WIND SHEAR AT MAX. WIND LEVEL ( geopotential )
C
            IPKD=67
            ISHEAR=0
C
            DO 2160 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  ISHEAR=ISHEAR+1
                  VALUES(IPTSH,NSUB)=FLOAT(ISHEAR)
                  VALUES(IP  ,NSUB)=999999.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  IF(KDEC(IPKD).NE.MINDIC)
     1             VALUES(IP+2,NSUB)=kdec(ipkd)*9.8
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
C
               ELSE
                  GO TO 2170
               END IF

C
 2160       CONTINUE
C
 2170 CONTINUE
C
C     UPDATE M TOTAL LENGTH OF REPORT
C
      IF(ISHEAR.NE.0) M=IP-1
C
C             SET INFORMATION NEEDED FOR RDB KEY DEFINITION.
C             ----------------------------------------------
C
      NBUFTYPE=2       ! BUFR TABLE A 
      NTYPE=4                           ! WIND PROFILE
      IF(KDEC(4).EQ. 32) NSBTYPE=91      ! PILOT
      IF(KDEC(4).EQ. 33) NSBTYPE=92     ! PILOT SHIP
C
cpsapr98      NYEAR =IYEAR+1900
      NYEAR =IYEAR
      NMONTH=IMONTH
      NDAY  =KDEC(1)
      NHOUR =KDEC(2)
      NMINUTE=KDEC(9)
      NSECOND=0
C
      JSCALE=5
      JREFLAT= -9000000
      JREFLON=-18000000
C
      NLAT1=NINT(VALUES(10,NSUB)*10.**JSCALE)-JREFLAT
      NLON1=NINT(VALUES(11,NSUB)*10.**JSCALE)-JREFLON
C
      NLAT2=0  
      NLON2=0
C
      NOBS=NSUB
C
      CIDENT=CHAR(KINT(12))//CHAR(KINT(13))//
     1       CHAR(KINT(14))//CHAR(KINT(15))//
     2       CHAR(KINT(16))//'    '
      NRECR=1
C
            RETURN
                    END IF
C     -----------------------------------------------------------------
C*          2.3  PILOT PART B OR D.
C                ------------------
 230  CONTINUE
C
         IF(K.EQ.2.OR.K.EQ.4) THEN
            VALUES( 1,NSUB)=(KINT(12)-48)*10+KINT(13)-48
            VALUES( 2,NSUB)=(KINT(14)-48)*100+(KINT(15)-48)*10
     1                      +KINT(16)-48
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
cpsapr98            VALUES( 5,NSUB)=FLOAT(IYEAR)+1900
            VALUES( 5,NSUB)=FLOAT(IYEAR)
            VALUES( 6,NSUB)=FLOAT(IMONTH)
            VALUES( 7,NSUB)=FLOAT(KDEC(1))
            VALUES( 8,NSUB)=FLOAT(KDEC(2))
            VALUES( 9,NSUB)=FLOAT(KDEC(9))
            VALUES(10,NSUB)=KDEC(5)/100.
            VALUES(11,NSUB)=KDEC(6)/100.
            VALUES(12,NSUB)=999999.
            IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
C
C
C           GEOPOTENTIAL AS VERTICAL COORDINATE
C
            IREP=0
            IP=14
            IPKD=25
C
            DO 231 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 231
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=999999.
            VALUES(IP+1,NSUB)=2.
            VALUES(IP+2,NSUB)=999999.
            IF(KDEC(IPKD).NE.MINDIC) THEN
               IF(KDEC(IPKD).EQ.0) THEN
                  VALUES(IP+1,NSUB)=66.
cpsoct06                  VALUES(IP+2,NSUB)=KDEC(8)*9.8
                  if (kdec(8).ne.mindic)VALUES(IP+2,NSUB)=KDEC(8)*9.8
               ELSE
                   VALUES(IP+2,NSUB)=kdec(ipkd)*9.8
               END IF
            END IF
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+5
            IPKD=IPKD+3
C
 231        CONTINUE
C
C           PRESSURE AS VERICAL COORDINATE
C
C
            IPKD=205
C
            DO 232 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 232
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            VALUES(IP+1,NSUB)=2.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+5
            IPKD=IPKD+3
C
 232        CONTINUE
C
C
C
C           UPDATE REPLICATION FACTOR
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C 
            VALUES(13,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           SET INFORMATION NEEDED FOR RDB KEY DEFINITION.
C           ----------------------------------------------
C
      NBUFTYPE=2       ! BUFR TABLE A 
      NTYPE=4                           ! WIND PROFILE
      IF(KDEC(4).EQ. 32) NSBTYPE=91      ! PILOT
      IF(KDEC(4).EQ. 33) NSBTYPE=92     ! PILOT SHIP
C
cpspr98      NYEAR =IYEAR+1900
      NYEAR =IYEAR
      NMONTH=IMONTH
      NDAY  =KDEC(1)
      NHOUR =KDEC(2)
      NMINUTE=KDEC(9)
      NSECOND=0
C
      JSCALE=5
      JREFLAT= -9000000
      JREFLON=-18000000
C
      NLAT1=NINT(VALUES(10,NSUB)*10.**JSCALE)-JREFLAT
      NLON1=NINT(VALUES(11,NSUB)*10.**JSCALE)-JREFLON
C
      NLAT2=0  
      NLON2=0
C
      NOBS=NSUB
C
      CIDENT=CHAR(KINT(12))//CHAR(KINT(13))//
     1       CHAR(KINT(14))//CHAR(KINT(15))//
     2       CHAR(KINT(16))//'    '
      NRECR=1
C
            RETURN
                    END IF
                       END IF 
C     -----------------------------------------------------------------
C*          3.   PILOT - SHIP REPORTS.
C                -------------------------
 300  CONTINUE
C
      IF(KDEC(4).EQ.33)
     1   THEN
C     -----------------------------------------------------------------
C*          3.1  PILOT PART A OR C.
C                ------------
 310  CONTINUE
C
         IF(K.EQ.1.OR.K.EQ.3) THEN
            VALUES( 1,NSUB)=1000*NSUB+9
            CVAL(NSUB)=CHAR(KINT(12))//CHAR(KINT(13))//CHAR(KINT(14))//
     1                 CHAR(KINT(15))//CHAR(KINT(16))//'    '
            VALUES( 2,NSUB)=999999.
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
            VALUES( 5,NSUB)=999999.
cpsapr98            VALUES( 6,NSUB)=FLOAT(IYEAR)+1900
            VALUES( 6,NSUB)=FLOAT(IYEAR)
            VALUES( 7,NSUB)=FLOAT(IMONTH)
            VALUES( 8,NSUB)=FLOAT(KDEC(1))
            VALUES( 9,NSUB)=FLOAT(KDEC(2))
            VALUES(10,NSUB)=FLOAT(KDEC(9))
            VALUES(11,NSUB)=KDEC(5)/100.
            VALUES(12,NSUB)=KDEC(6)/100.
C
            IREP=0
C
            IP=14
            IPKD=25
c
            IL=9
            IF(K.EQ.3) IL=5
C
            DO 311 I=1,IL
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 311
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=kdec(IPKD)*10.
            VALUES(IP+1,NSUB)=32.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+5
            IPKD=IPKD+3
C
 311        CONTINUE
C
C
C           MAX. WIND LEVEL ( pressure )
C
            IPKD=52
C
            DO 313 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
               ELSE
                  GO TO 315
               END IF

C
 313        CONTINUE
C
 315  CONTINUE
C
C           MAX. WIND LEVEL ( geopotential )
C
            IPKD=67
C
            DO 3130 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=999999.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  IF(KDEC(IPKD).NE.MINDIC)
     1             VALUES(IP+2,NSUB)=kdec(ipkd)*9.8
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
               ELSE
                  GO TO 3150
               END IF

C
 3130       CONTINUE
C
 3150 CONTINUE
C
C
C           UPDATE REPLICATION FACTOR
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C 
            VALUES(13,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           WIND SHEAR AT MAX. WIND LEVEL ( pressure )
C
            IPKD=52
            ISHEAR=0
            IPTSH=IP
            IP=IP+1
C
            DO 316 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  ISHEAR=ISHEAR+1
                  VALUES(IPTSH,NSUB)=FLOAT(ISHEAR)
                  VALUES(IP  ,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
C
               ELSE
                  IF(ISHEAR.NE.0) GO TO 3170
                  GO TO 317
               END IF

C
 316        CONTINUE
C
 317  CONTINUE
C
C           WIND SHEAR AT MAX. WIND LEVEL ( geopotential )
C
            IPKD=67
            ISHEAR=0
C
            DO 3160 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  ISHEAR=ISHEAR+1
                  VALUES(IPTSH,NSUB)=FLOAT(ISHEAR)
                  VALUES(IP  ,NSUB)=999999.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  IF(KDEC(IPKD).NE.MINDIC)
     1             VALUES(IP+2,NSUB)=kdec(ipkd)*9.8
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+5
               ELSE
                  GO TO 3170
               END IF

C
 3160       CONTINUE
C
 3170 CONTINUE
C
C     UPDATE M TOTAL LENGTH OF REPORT
C
      IF(ISHEAR.NE.0) M=IP-1
C
C             SET INFORMATION NEEDED FOR RDB KEY DEFINITION.
C             ----------------------------------------------
C
      NBUFTYPE=2       ! BUFR TABLE A 
      NTYPE=4                           ! WIND PROFILE
      IF(KDEC(4).EQ. 32) NSBTYPE=91      ! PILOT
      IF(KDEC(4).EQ. 33) NSBTYPE=92     ! PILOT SHIP
C
cpsapr98      NYEAR =IYEAR+1900
      NYEAR =IYEAR
      NMONTH=IMONTH
      NDAY  =KDEC(1)
      NHOUR =KDEC(2)
      NMINUTE=KDEC(9)
      NSECOND=0
C
      JSCALE=5
      JREFLAT= -9000000
      JREFLON=-18000000
C
      NLAT1=NINT(VALUES(11,NSUB)*10.**JSCALE)-JREFLAT
      NLON1=NINT(VALUES(12,NSUB)*10.**JSCALE)-JREFLON
C
      NLAT2=0  
      NLON2=0
C
      NOBS=NSUB
C
      CIDENT= CVAL(NSUB)
      NRECR=1
C
            RETURN
                    END IF
C
C     -----------------------------------------------------------------
C*          3.3  PILOT PART B OR D.
C                ------------------
 330  CONTINUE
C
         IF(K.EQ.2.OR.K.EQ.4) THEN
            VALUES( 1,NSUB)=1000*NSUB+9
            CVAL(NSUB)=CHAR(KINT(12))//CHAR(KINT(13))//CHAR(KINT(14))//
     1                 CHAR(KINT(15))//CHAR(KINT(16))//'    '
            VALUES( 2,NSUB)=999999.
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
            VALUES( 5,NSUB)=999999.
cpsapr98            VALUES( 6,NSUB)=FLOAT(IYEAR)+1900
            VALUES( 6,NSUB)=FLOAT(IYEAR)
            VALUES( 7,NSUB)=FLOAT(IMONTH)
            VALUES( 8,NSUB)=FLOAT(KDEC(1))
            VALUES( 9,NSUB)=FLOAT(KDEC(2))
            VALUES(10,NSUB)=FLOAT(KDEC(9))
            VALUES(11,NSUB)=KDEC(5)/100.
            VALUES(12,NSUB)=KDEC(6)/100.
cpsmar07 We do not store station elevation for ship, especially not by overwriting longitude!
cps            IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
C
C
C           GEOPOTENTIAL AS VERTICAL COORDINATE
C
            IREP=0
            IP=14
            IPKD=25
C
            DO 331 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 331
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=999999.
            VALUES(IP+1,NSUB)=2.
            VALUES(IP+2,NSUB)=999999.
            IF(KDEC(IPKD).NE.MINDIC)
     1       VALUES(IP+2,NSUB)=kdec(ipkd)*9.8
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+5
            IPKD=IPKD+3
C
 331        CONTINUE
C
C           PRESSURE AS VERICAL COORDINATE
C
C
            IPKD=205
C
            DO 332 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 332
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            VALUES(IP+1,NSUB)=2.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+4,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+5
            IPKD=IPKD+3
C
 332        CONTINUE
C
C
C
C           UPDATE REPLICATION FACTOR
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C 
            VALUES(13,NSUB)=FLOAT(IREP)
            M=IP-1
C
C               SET INFORMATION NEEDED FOR RDB KEY DEFINITION.
C               ----------------------------------------------
C
      NBUFTYPE=2       ! BUFR TABLE A 
      NTYPE=4                           ! WIND PROFILE
      IF(KDEC(4).EQ. 32) NSBTYPE=91      ! PILOT
      IF(KDEC(4).EQ. 33) NSBTYPE=92     ! PILOT SHIP
C
cpsapr98      NYEAR =IYEAR+1900
      NYEAR =IYEAR
      NMONTH=IMONTH
      NDAY  =KDEC(1)
      NHOUR =KDEC(2)
      NMINUTE=KDEC(9)
      NSECOND=0
C
      JSCALE=5
      JREFLAT= -9000000
      JREFLON=-18000000
C
      NLAT1=NINT(VALUES(11,NSUB)*10.**JSCALE)-JREFLAT
      NLON1=NINT(VALUES(12,NSUB)*10.**JSCALE)-JREFLON
C
      NLAT2=0  
      NLON2=0
C
      NOBS=NSUB
C
      CIDENT= CVAL(NSUB)
      NRECR=1
C
            RETURN
                    END IF
                       END IF
C
      RETURN
      END
      SUBROUTINE BUFRENC(IERR )
C
C**** *BUFRENC*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO ENCODE INDIVIDUAL TEMP
C         REPORT FROM FULLY EXPANDED BUFR FORMAT INTO BUFR MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BUFRENC(IERR)*
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
C         *CALL* *SETWT(IERR)*
C         *CALL* *SEC0(IERR)*
C         *CALL* *SEC1(IERR)*
C         *CALL* *SEC2(IERR)*
C         *CALL* *SEC3(IERR)*
C         *CALL* *SEC4(IERR)*
C         *CALL* *SEC5(IERR)*
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'compoin.f'
CRR common for directories
      INCLUDE 'comdir.f'
C
C     ------------------------------------------------------------------
C*          1.   SET UP *BUFR* MESSAGE.
C                ----------------------
 100  CONTINUE
C
CTEST
      IF( IERR.NE.0 )
     1  PRINT*,' BUFRENC - start of subr., ierr ',IERR
CTEST
      IF( IERR.NE.0 ) RETURN
C
C           1.1 SET WORKING TABLES AND BUFR SECTION DATA.
C               -----------------------------------------
 110  CONTINUE
C
      CALL SETWT( IERR )
      CALL SEC0 ( IERR )
      CALL SEC1 ( IERR )
      CALL SEC2 ( IERR )
      CALL SEC3 ( IERR )
      CALL SEC4 ( IERR )
      CALL SEC5 ( IERR )
C
      CALL PUTBUFR( IERR )
C     ------------------------------------------------------------------
C
      RETURN
      END
      SUBROUTINE SEC1( IERR )
C
C**** *SEC1*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO SET UP SECTION 1 OF *BUFR
C         MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SEC1(IERR)*
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
C         *CALL* *PACK(KBPW,KD,KS,KWPT,KBPT,KSI)*
C
C            *KBPW*  - NUMBER OF BITS IN COMPUTER WORD.
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KWPT*  - POINTER TO WORD IN KD ARRAY.
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C         *CALL* *OCTNUM(KWPTB,KBPTB)*
C
C            *KWPTB* - POINTER TO WORD AT BEGINING OF SECTION.
C            *KBPTB* - POINTER TO BIT ISIDE THE WORD.
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
C          M. Dragosavac 13/11/1991  : change to Bufr Edition 2
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comkey.f'
      include 'comkeyc.f'  ! ncorn

C     ------------------------------------------------------------------
C
C*          1.   SET UP *SECTION 1 ( IDENTIFICATION SECTION).
C                --------------------------------------------
 100  CONTINUE
C
      IF( IERR.NE.0) RETURN
C     ------------------------------------------------------------------
C*          2.   KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 200  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C     ------------------------------------------------------------------
C*          3.   SET UP LENGTH OF SECTION (OCTET 1-3 ) TO ZERO.
C                ----------------------------------------------
 300  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,24)
C
C     ------------------------------------------------------------------
C*          4.   BUFR MASTER TABLE( ZERO IF STANDARD WMO FM - 94 USED
C                ----------------------------------------------------
 400  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          5.   SET UP ORIGINATING CENTRE.
C                --------------------------
 500  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),98,NWPT,NBPT,16)
C
C     ------------------------------------------------------------------
C*          6.   SET UP UPDATE SEQUENCE NUMBER.
C                ------------------------------
 600  CONTINUE
C
cpsmar07      CALL PACK(NBPW,MBUF(NWPT),NSNUM,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,8)
C
C
C     ------------------------------------------------------------------
C*          7.   SET UP PRESENCE OF SECTION 2.
C                -----------------------------
 160  CONTINUE
C
      IPS2=0
      IF(OPS2) IPS2=128
      CALL PACK(NBPW,MBUF(NWPT),IPS2,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          1.7  SET UP BUFR MESSAGE TYPE.
C                -------------------------
 700  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),NBUFTYPE,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          8.   SET UP BUFR MESSAGE SUB-TYPE.
C                -----------------------------
 800  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),NSBTYPE,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          9.   VERSION NUMBER OF MASTER TABLE USED.
C                ------------------------------------
 900  CONTINUE
crr 990909 version 7
CRR 950317 version 3
crr N.B. change Bufr Table B in ..../libsrc/settab.f
      CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT,8)
C
C*          9.1  VERSION NUMBER OF LOCAL TABLES USED.
C                ------------------------------------
 910  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          10.  SET UP YEAR OF CENTURY.
C                -----------------------
 1000 CONTINUE
C
      RYEAR=VALUES(5,1)
cpsmar07      IF(KDEC(4).EQ.36) RYEAR=VALUES(6,1)
      IF(KDEC(4).EQ.33) RYEAR=VALUES(6,1)
C
cpsapr98      IYEAR=RYEAR-1900.
cpsapr98      CALL PACK(NBPW,MBUF(NWPT),IYEAR,NWPT,NBPT,8)
      iiyear=mod(nint(RYEAR),100)
      if (iiyear.eq.0) iiyear = 100
      CALL PACK(NBPW,MBUF(NWPT),iiyear,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          11.  SET UP MONTH.
C                -------------
 1100 CONTINUE
C
      IMONTH=VALUES(6,1)
cpsmar07      IF(KDEC(4).EQ.36) IMONTH=VALUES(7,1)
      IF(KDEC(4).EQ.33) IMONTH=VALUES(7,1)
      CALL PACK(NBPW,MBUF(NWPT),IMONTH,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          12.  SET UP DAY.
C                -----------
 1200 CONTINUE
C
cpsmar09      IDAY=VALUES(7,1)
cpsmar07      IF(KDEC(4).EQ.36) IDAY=VALUES(8,1)
cpsmar09      IF(KDEC(4).EQ.33) IDAY=VALUES(8,1)
      IDAY = KDEC(1)
      CALL PACK(NBPW,MBUF(NWPT),IDAY,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          13.  SET UP HOUR.
C                -------------
 1300 CONTINUE
C
cpsmar09      IHOUR=VALUES(8,1)
cpsmar07      IF(KDEC(4).EQ.36) IHOUR=VALUES(9,1)
cpsmar09      IF(KDEC(4).EQ.33) IHOUR=VALUES(9,1)
      IHOUR = KDEC(2)
      CALL PACK(NBPW,MBUF(NWPT),IHOUR,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          14.  SET UP MINUTE.
C                --------------
 1400 CONTINUE
C
cpsmar09      IMIN=VALUES(9,1)
cpsmar07      IF(KDEC(4).EQ.36) IMIN=VALUES(10,1)
cpsmar09      IF(KDEC(4).EQ.33) IMIN=VALUES(10,1)
      IMIN = KDEC(9)
cpsjun11      CALL PACK(NBPW,MBUF(NWPT),IMINUTE,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),IMIN,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          15.  SET UP LENGTH OF THE SECTION 1.
C                --------------------------------
 1500 CONTINUE
C
      CALL OCTNUM(IWPTB,IBPTB)
C
C     ------------------------------------------------------------------
C
 1600 CONTINUE
C
      RETURN
      END
      SUBROUTINE SEC3(IERR )
C
C**** *SEC3*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO SET UP SECTION 3 OF *BUFR
C         MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SEC3(IERR)*
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
C         *CALL* *PACK(KBPW,KD,KS,KWPT,KBPT,KSI)*
C
C            *KBPW*  - NUMBER OF BITS IN COMPUTER WORD.
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KWPT*  - POINTER TO WORD IN KD ARRAY.
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C         *CALL* *OCTNUM(KWPTB,KBPTB)*
C
C            *KWPTB* - POINTER TO WORD AT BEGINING OF SECTION.
C            *KBPTB* - POINTER TO BIT ISIDE THE WORD.
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
cpsjun11      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
      IMPLICIT LOGICAL(O,G,L), CHARACTER*8(C,H,Y)
C
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comwt.f'
C     ------------------------------------------------------------------
C
C*          1.   SET UP *SECTION 3 ( DATA DESCRIPTION SECTION ).
C                -----------------------------------------------
 100  CONTINUE
C
      IF( IERR.NE.0 ) RETURN
C
C     ------------------------------------------------------------------
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C     ------------------------------------------------------------------
C*          1.2  SET UP LENGTH OF SECTION (OCTET 1-3 ) TO ZERO.
C                ----------------------------------------------
 120  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,24)
C
C     ------------------------------------------------------------------
C*          1.3  SET UP RESERVED BYTE  TO ZERO.
C                ------------------------------
 130  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          1.4  SET UP NUMBER OF DATA SUB-SETS.
C                -------------------------------
 140  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),N,NWPT,NBPT,16)
C
C     ------------------------------------------------------------------
C*          1.5  SET UP INFORMATION ABOUT TYPE OF DATA AND COMPRESSION.
C                ------------------------------------------------------
 150  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),128,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          1.6  SET UP COLLECTION OF DESCRIPTORS WHICH DEFINE DATA.
C                --------------------------------------------------
 160  CONTINUE
C
C     PILOT - LAND STATION
C
      IF(KDEC(4).EQ.32) THEN
            CALL PACK(NBPW,MBUF(NWPT), 3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 2,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 8)
C 
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 2,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),22,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 5,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 7,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 4,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT), 8,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),10,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 3,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 1,NWPT,NBPT, 8)
C
            CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT), 2,NWPT,NBPT, 8)
C
            IF(LSHEAR) THEN
               CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
               CALL PACK(NBPW,MBUF(NWPT),5,NWPT,NBPT, 6)
               CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 8)
C
               CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
               CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 6)
               CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
C
               CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
               CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 6)
               CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 8)
C
               CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
               CALL PACK(NBPW,MBUF(NWPT),8,NWPT,NBPT, 6)
               CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
C
               CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
               CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 6)
               CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 8)
C
               CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
               CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
               CALL PACK(NBPW,MBUF(NWPT),61,NWPT,NBPT, 8)
C
               CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
               CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
               CALL PACK(NBPW,MBUF(NWPT),62,NWPT,NBPT, 8)
            END IF
            GO TO 170
         END IF
C
C     PILOT - SHIP
C
      IF(KDEC(4).EQ.33)
     1   THEN
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),23,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),5,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),8,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),10,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 8)
            IF(LSHEAR)
     1         THEN
                  CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
                  CALL PACK(NBPW,MBUF(NWPT),5,NWPT,NBPT, 6)
                  CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 8)
c
                  CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
                  CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 6)
                  CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
c
                  CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
                  CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 6)
                  CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 8)
c
                  CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
                  CALL PACK(NBPW,MBUF(NWPT),8,NWPT,NBPT, 6)
                  CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
c
                  CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
                  CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 6)
                  CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 8)
c
                  CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
                  CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
                  CALL PACK(NBPW,MBUF(NWPT),61,NWPT,NBPT, 8)
c
                  CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
                  CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
                  CALL PACK(NBPW,MBUF(NWPT),62,NWPT,NBPT, 8)
               END IF
         END IF
C
C     ------------------------------------------------------------------
C*          1.7  SET UP QUALITY CONTROL DECSCRIPTORS.
C                ------------------------------------
 170  CONTINUE
C
C
C     CHECK IF Q/C PERFORMED.
C
      IBIT1=0
      DO 171 I=M+1,M+M
         IF(VALUES(I,NSUB).NE.999999.) IBIT1=IBIT1+1
 171  CONTINUE
C
      IF(IBIT1.EQ.0) GO TO 180
C
      MQ=M+4
      CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT), 22,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),  1,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),  1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),  1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),  1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT), 32,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),  1,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),  1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 8)
c
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT), 33,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),  7,NWPT,NBPT, 8)
C
C     ------------------------------------------------------------------
C*          1.8  SET UP LENGTH OF THE SECTION 3.
C                -------------------------------
 180  CONTINUE
C
      CALL OCTNUM(IWPTB,IBPTB)
C
C     ------------------------------------------------------------------
 200  CONTINUE
C
      RETURN
      END
      SUBROUTINE SEC4( IERR )
C
C**** *SEC4*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO SET UP SECTION 4 OF *BUFR
C         MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SEC4(IERR)*
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
C         *CALL* *PACK(KBPW,KD,KS,KWPT,KBPT,KSI)*
C
C            *KBPW*  - NUMBER OF BITS IN COMPUTER WORD.
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KWPT*  - POINTER TO WORD IN KD ARRAY.
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C         *CALL* *OCTNUM(KWPTB,KBPTB)*
C
C            *KWPTB* - POINTER TO WORD AT BEGINING OF SECTION.
C            *KBPTB* - POINTER TO BIT ISIDE THE WORD.
C
C         *CALL* *GBYTE(KS,KD,KSKI,KSI)
C
C            *KS*    - SOURCE
C            *KD*    - DESTINATION
C            *KSKI*  - NUMBER OF BITS TO BE SKIPPED
C            *KSI*   - NUMBER OF BITS TO BE EXTRACTED
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comwt.f'
C
      CHARACTER*24 CUNIT,YUNIT
      CHARACTER*9 CVAL,YVAL
C
      INCLUDE 'comwtc.f'
C
C     ------------------------------------------------------------------
C
C*          1.   SET UP *SECTION 4 ( DATA SECTION).
C                ----------------------------------
 100  CONTINUE
C
      IF( IERR.NE.0 ) RETURN
C     ------------------------------------------------------------------
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C     ------------------------------------------------------------------
C*          1.2  SET UP LENGTH OF SECTION (OCTET 1-3 ) TO ZERO.
C                ----------------------------------------------
 120  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,24)
C
C     ------------------------------------------------------------------
C*          1.3  SET UP RESERVED BYTE  TO ZERO.
C                ------------------------------
 130  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          2.   SET UP BINARY DATA AS DEFINED BY SEQUENCE DESCRIPTOR.
C                ------------------------------------------------
 200  CONTINUE
C
      DO 201 I=1,N
      DO 202 J=1,M
C
      IREF  =IWTRV(J)
      ISCALE=IWTS (J)
      IBDW  =IWTDW(J)
      YUNIT =CUNIT(J)
C
      VAL=VALUES(J,I)
C
      IF(YUNIT.EQ.'CCITTIA5') THEN
         IST=NINT(VAL)/1000
         NCHAR=NINT(VAL)-IST*1000
         YVAL=CVAL(IST)
         DO 203 II=1,NCHAR
         IPACK=ICHAR(YVAL(II:II))
         CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,8)
 203     CONTINUE
         GO TO 202
      END IF
C
      IF(VAL.EQ.999999.) THEN
         CALL PACK(NBPW,MBUF(NWPT),MASKS(IBDW),NWPT,NBPT,IBDW)
      ELSE
         IF(ISCALE.LT.0) THEN
            ISCALE=IABS(ISCALE)
            IPACK=NINT(VAL/10.**ISCALE) - IREF
         ELSE
            IPACK=NINT(VAL*10.**ISCALE) - IREF
         END IF
C
         CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
      END IF
C
 202  CONTINUE
C
      IF(M.LT.MQ) THEN
C
        DO 210 J=M+1,MQ      
C
        IREV  =IWTR (J)
        IREF  =IWTRV(J)
        ISCALE=IWTS (J)
        IBDW  =IWTDW(J)
        YUNIT =CUNIT(J)
C
        IF(IREV.EQ.031031) THEN
C 
c          set replication factor for bit presence indicator
c
           CALL PACK(NBPW,MBUF(NWPT),M,NWPT,NBPT,16)
C
C          SET BIT FLAG
C
           IBIT1=0
C
           DO 215 JI=M+1,M+M
            IPACK=1
            IF(VALUES(JI,I).NE.999999.) IPACK=0
            CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,1)
 215       CONTINUE
C           
        END IF
C
       IF(IREV.EQ.001031) THEN
          VAL=98.
          IF(VAL.EQ.999999.) THEN
             CALL PACK(NBPW,MBUF(NWPT),MASKS(IBDW),NWPT,NBPT,IBDW)
          ELSE
             IF(ISCALE.LT.0) THEN
                ISCALE=IABS(ISCALE)
                IPACK=NINT(VAL/10**ISCALE) - IREF
             ELSE
                IPACK=NINT(VAL*10**ISCALE) - IREF
             END IF
C
             CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
           END IF
        END IF 
C
       IF(IREV.EQ.001032) THEN
          VAL=1.
          IF(VAL.EQ.999999.) THEN
             CALL PACK(NBPW,MBUF(NWPT),MASKS(IBDW),NWPT,NBPT,IBDW)
          ELSE
             IF(ISCALE.LT.0) THEN
                ISCALE=IABS(ISCALE)
                IPACK=NINT(VAL/10**ISCALE) - IREF
             ELSE
                IPACK=NINT(VAL*10**ISCALE) - IREF
             END IF
C
             CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
           END IF
        END IF 
C
        IF(IREV.EQ.033007) THEN
C
C          SET REPLICATION FACTOR FOR % CONFIDENCE
C
C
           IBIT1=0
           DO 217 IK=M+1,M+M
            IF(VALUES(IK,I).NE.999999.) IBIT1=IBIT1+1
 217       CONTINUE
C
           CALL PACK(NBPW,MBUF(NWPT),IBIT1,NWPT,NBPT,16)
C
           DO 216 JI=M+1,M+M
           VAL=VALUES(JI,I)
           IF(VAL.EQ.999999.) GO TO 216
C
           IF(ISCALE.LT.0) THEN
              ISCALE=IABS(ISCALE)
              IPACK=NINT(VAL/10**ISCALE) - IREF
           ELSE
              IPACK=NINT(VAL*10**ISCALE) - IREF
           END IF
C
           CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
C
 216       CONTINUE
C
        END IF
C
 210    CONTINUE
C
      END IF
C
 201  CONTINUE
C
C
C     ------------------------------------------------------------------
C*         12.   SET UP LENGTH OF THE SECTION 4.
C                -------------------------------
 1200 CONTINUE
C
      CALL OCTNUM(IWPTB,IBPTB)
C
C     ------------------------------------------------------------------
 1300 CONTINUE
C
      RETURN
      END
      SUBROUTINE SETWT(IERR)
C
C**** *SETWT*
C
C
C     PURPOSE.
C     --------
C         SETS UP WORKING TABLE B INFORMATION.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SETWT(IERR)*
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
      IMPLICIT LOGICAL(O,G,L), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.f'
C
      CHARACTER*24 CTABBU
      CHARACTER*64 CTABBEN
C
      CHARACTER*9  CVAL
      CHARACTER*24 CUNIT
C
      INCLUDE 'comwork.f'
      INCLUDE 'comtab.f'
      INCLUDE 'comtabc.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
C
      DIMENSION ITBTRL(28),ITBTRS(28),ITBTRA(1000)
C
      DATA ITBTRL/001001,001002,002011,002012,004001
     1,           004002,004003,004004,004005,005001
     2,           006001,007001,031001,007004,008001
     5,           007003,011001,011002,031001,007004
     6,           008001,007003,011061,011062,031031
     7,           001031,001032,033007/
      DATA ITBTRS/001011,001012,001013,002011,002012
     1,           004001,004002,004003,004004,004005
     2,           005002,006002,031001,007004,008001
     4,           007003,011001,011002,031001,007004
     5,           008001,007003,011061,011062,031031
     6,           001031,001032,033007/
C     ------------------------------------------------------------------
C*          1.   SET UP WORKING TABLES FOR PILOT DATA.
C                -------------------------------------
 100  CONTINUE
C
      IF(IERR.NE.0) RETURN
C
      LSHEAR=.FALSE.
C
      IF(KDEC(4).EQ.32)
     1   THEN
            J=0
C
            DO 101 I=1,13
            J=J+1
            ITBTRA(J)=ITBTRL(I)
 101        CONTINUE
C
            JJ=14
C
            IREP=NINT(VALUES(13,NSUB))
C
            DO 102 I=1,IREP
C
            J=J+1
            ITBTRA(J)=ITBTRL(JJ  )
            J=J+1
            ITBTRA(J)=ITBTRL(JJ+1)
            J=J+1
            ITBTRA(J)=ITBTRL(JJ+2)
            J=J+1
            ITBTRA(J)=ITBTRL(JJ+3)
            J=J+1
            ITBTRA(J)=ITBTRL(JJ+4)
C
 102        CONTINUE
C
            IS=IREP*5+13
            IF(IS.LT.M)
     1         THEN
                  LSHEAR=.TRUE.
                  J=J+1
                  ITBTRA(J)=ITBTRL(19)
                  IPTSH=IS+1
                  IREPS=NINT(VALUES(IPTSH,NSUB))
C
                  JJ=20
C
                  DO 103 I=1,IREPS
                  J=J+1
                  ITBTRA(J)=ITBTRL(JJ  )
                  J=J+1
                  ITBTRA(J)=ITBTRL(JJ+1)
                  J=J+1
                  ITBTRA(J)=ITBTRL(JJ+2)
                  J=J+1
                  ITBTRA(J)=ITBTRL(JJ+3)
                  J=J+1
                  ITBTRA(J)=ITBTRL(JJ+4)
 103              CONTINUE
C
               END IF
C
            J=J+1
            ITBTRA(J)=ITBTRL(25)
            J=J+1
            ITBTRA(J)=ITBTRL(26)
            J=J+1
            ITBTRA(J)=ITBTRL(27)
            J=J+1
            ITBTRA(J)=ITBTRL(28)
C
            GO TO 110
         END IF
C
      IF(KDEC(4).EQ.33)
     1   THEN
            J=0
C
            DO 111 I=1,13
            J=J+1
            ITBTRA(J)=ITBTRS(I)
 111        CONTINUE
C
            JJ=14
C
            IREP=NINT(VALUES(13,NSUB))
C
            DO 112 I=1,IREP
C
            J=J+1
            ITBTRA(J)=ITBTRS(JJ  )
            J=J+1
            ITBTRA(J)=ITBTRS(JJ+1)
            J=J+1
            ITBTRA(J)=ITBTRS(JJ+2)
            J=J+1
            ITBTRA(J)=ITBTRS(JJ+3)
            J=J+1
            ITBTRA(J)=ITBTRS(JJ+4)
C
 112        CONTINUE
C
            IS=IREP*5+13
            IF(IS.LT.M)
     1         THEN
                  LSHEAR=.TRUE.
                  J=J+1
                  ITBTRA(J)=ITBTRS(19)
                  IPTSH=IS+1
                  IREPS=NINT(VALUES(IPTSH,NSUB))
C
                  JJ=20
C
                  DO 113 I=1,IREPS
                  J=J+1
                  ITBTRA(J)=ITBTRS(JJ  )
                  J=J+1
                  ITBTRA(J)=ITBTRS(JJ+1)
                  J=J+1
                  ITBTRA(J)=ITBTRS(JJ+2)
                  J=J+1
                  ITBTRA(J)=ITBTRS(JJ+3)
                  J=J+1
                  ITBTRA(J)=ITBTRS(JJ+4)
 113              CONTINUE
C
               END IF
C
C
            J=J+1
            ITBTRA(J)=ITBTRS(25)
            J=J+1
            ITBTRA(J)=ITBTRS(26)
            J=J+1
            ITBTRA(J)=ITBTRS(27)
            J=J+1
            ITBTRA(J)=ITBTRS(28)
         END IF
C
 110  CONTINUE
C
      DO 121 I=1,J
C
      ITR=ITBTRA(I)
C
      DO 122 II=1,JP2
C
      IF(ITR.EQ.NTABBTR(II)) THEN
                               IWTR (I)=NTABBTR(II)
                               IWTS (I)=NTABBS (II)
                               IWTDW(I)=NTABBDW(II)
                               IWTRV(I)=NTABBRV(II)
                               CUNIT(I)=CTABBU(II)
                               GO TO 121
                            END IF
C
 122  CONTINUE
C
      PRINT*,'BUFR TABLE B REFERENCE ENTRY',ITR,'NOT FOUND'
C
 121  CONTINUE
C
      RETURN
      END
      SUBROUTINE PUTBUFR( IERR )
C
C**** *PUTBUFR*
C
C
C     PURPOSE.
C     --------
C         PUT BUFR MESSAGE INTO REPORT DATA BASE.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PUTBUFR(IERR)*
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
C         *call* *rlunit(lrunit)* (libmi)
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
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G,L), CHARACTER*8(C,H,Y)
      integer lrunit
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f'
      INCLUDE 'comrec.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
      CHARACTER*15000 YOUT,YOUTOLD
      CHARACTER*9  CIDENT
      CHARACTER*48 CKEY,YKEYOLD
      CHARACTER*32 YKEY
      CHARACTER*16 YTYPE
cpsapr98      CHARACTER*12 CFNAME
      CHARACTER*14 CFNAME
      CHARACTER*14 YTIME
      CHARACTER*23 CTIME,YCTIME
      CHARACTER*10 CUSE
      CHARACTER*29 YCFNAME
C
      DIMENSION IKEY(12),IKEYOLD(12)
C
      EQUIVALENCE (IKEY(1),YKEY,CKEY)
      EQUIVALENCE (IKEYOLD(1),YKEYOLD)
cpsapr04      EQUIVALENCE (MBUF(1),YOUT)
      EQUIVALENCE (MBUF(2),YOUT)
C
C
C     ------------------------------------------------------------------
C*          1. DEFINE RDB FILE NAME AND OPEN RDB FILE.
C                -------------------------------------
 100  CONTINUE
CTEST
      IF(IERR.NE.0)
     1   PRINT*,' PUTBUFR - start of subr., ierr ',IERR
CTEST
C
      IF(IERR.NE.0) RETURN
C
      IRET=0
CRR      YTYPE='wind_profile'   n.b. name used in mrgpil !!
      YTYPE='pilot'
cpsapr98      NY=NYEAR - 1900
      NY=NYEAR
C
cpsapr98      WRITE(YTIME,'(6I2.2)',IOSTAT=IOS,ERR=101)
      WRITE(YTIME,'(I4,5I2.2)',IOSTAT=IOS,ERR=101)
     1      NY,NMONTH,NDAY,NHOUR,NMINUTE,NSECOND
      GO TO 102
C
 101  CONTINUE
C
      PRINT*,'BAD DATE OR TIME, REPORT SKIPPED. IOS=',IOS
      RETURN
C
 102  CONTINUE
C
C     DEFINE FILE NAME
C
CTEST open file ?????? .rdb for testing
      CALL ISFILE(YTYPE,YTIME,CFNAME,IRET)
CTEST
cc      PRINT*,' PUTBUFR - cfname ', CFNAME
CTEST
      IF(IRET.NE.0) THEN
                       PRINT*,'ISFILE: Wrong file name, IRET=',IRET
         IRET=0
         RETURN
      END IF
C
C*          2.  DEFINE RDB KEY AND HOUSEHEEPING INFORMATION.
C               --------------------------------------------
 200  CONTINUE
C
      IW=1
      IBT=0
C
C*          2.1 DEFINE RDB KEY.
C               ---------------
 210  CONTINUE
C 
      CALL PACK(32,IKEY(IW),NTYPE,IW,IBT,8)
      CALL PACK(32,IKEY(IW),NSBTYPE,IW,IBT,8)
      CALL PACK(32,IKEY(IW),NYEAR,IW,IBT,12)
      CALL PACK(32,IKEY(IW),NMONTH,IW,IBT,4)
      CALL PACK(32,IKEY(IW),NDAY,IW,IBT,6)
      CALL PACK(32,IKEY(IW),NHOUR,IW,IBT,5)
      CALL PACK(32,IKEY(IW),NMINUTE,IW,IBT,6)
      CALL PACK(32,IKEY(IW),NSECOND,IW,IBT,6)
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)
      CALL PACK(32,IKEY(IW),NLON1,IW,IBT,26)
      CALL PACK(32,IKEY(IW),0,IW,IBT,6)
      CALL PACK(32,IKEY(IW),NLAT1,IW,IBT,25)
      CALL PACK(32,IKEY(IW),0,IW,IBT,7)
      YKEY(16:24)=CIDENT
      YKEY(25:32)=' '
C
C*          2.2 DEFINE HOUSE KEEPING INFORMATION.      
C               ---------------------------------
 220  CONTINUE
C
      ILENGTH=(NWPT-2)*4+NBPT/8
C
      IF(KDEC(21).EQ.MINDIC) THEN
                                NCORR=0
                             ELSE
                                NCORR=KDEC(21)
                             END IF
C
C     FIND LOWEST QUALITY CONTROL PERCENTAGE. 
C
CRR - M and MQ in common comwt
c
      IF(M.LT.MQ) THEN
                     RMIN=VALUES(M+1,NSUB)
                     DO 221 I=M+1,M+M
                     IF(VALUES(I,NSUB).LT.RMIN) RMIN=VALUES(I,NSUB)
 221                 CONTINUE
                     NQC=NINT(RMIN) 
                  END IF
C      
      IW =9
      IBT=0
C
      CALL ASCTIM(CTIME)
      READ(CTIME,'(I2,10X,I2,1X,I2,1X,I2,1X,I2)') IDD,IHH,IMM,ISS,ICC
C
C       SET LENGTH OF RECORD
C
      CALL PACK(32,IKEY(IW),ILENGTH,IW,IBT,16)      
C
C        SET TIME RECORD WAS LAST WRITTEN TO RDB
C 
      CALL PACK(32,IKEY(IW),IDD,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),IHH,IW,IBT,5)      
      CALL PACK(32,IKEY(IW),IMM,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),ISS,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
C
C       SET TIME BULLETIN WAS RECEIVED
C
      CALL PACK(32,IKEY(IW),NRDAY,   IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRHOUR,  IW,IBT,5)      
      CALL PACK(32,IKEY(IW),NRMIN,   IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRSEC,   IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,       IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(1),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(1),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,       IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(2),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(2),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,       IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(3),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(3),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,       IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(4),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(4),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,       IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NQC,     IW,IBT,8)      
      CALL PACK(32,IKEY(IW),0,       IW,IBT,24)      
C
C
C     WRITE RDB KEY INTO SECTION 2 OF BUFR MESSAGE.
C
C     WORD 8 BIT 16
C
cpsapr04      YOUT(35:82)=CKEY(1:48)
      YOUT(31:78)=CKEY(1:48)
      mbuf(1)=ilength
C
C
C*          3.  WRITE REPORT INTO RDB.
C               ----------------------        
 300  CONTINUE
C
C*          3.1 CHECK IF CORRECTION.
C               --------------------
 310  CONTINUE
C
      LREP=.TRUE.
CTEST
C
      IF(NCORR.NE.0) THEN
CTEST
      Print*,' This part has not been written to overwrite old records '
      PRINT*,' ***** PUTBUFR - NCORR NE 0 *****',NCORR
CTEST
CRR      IF(.NOT.LREP) GO TO 320
CRR      CALL GBYTE(IKEYOLD(9),ICOR,0,6)
CRR      IF(NCORR.LT.ICOR) LREP=.FALSE.
      END IF
C
C
C*         3.2 WRITE RECORD INTO RDB.
C              ----------------------
 320  CONTINUE
c
CTEST Check if file already opened
      INQUIRE(FILE=cstream(msys)//'/'//CFNAME//'.rdb',OPENED=OPN,
     1        NUMBER=IU)
      IF (OPN) GO TO 350
CTEST check if unit used - opn logical
      IU=90
330   CONTINUE
      INQUIRE(UNIT=IU,OPENED=OPN)
      IF (OPN) THEN
               IU=IU+1
               GO TO 330
      ENDIF
CTEST Here got unused unit
cc      PRINT*,' PUTBUFR - unused unit for .rdb ', IU
      call rlunit(lrunit)
      OPEN(UNIT=IU,
     1     FILE=cstream(msys)//'/'//CFNAME//'.rdb',
     2     ACCESS='DIRECT',
     3     RECL=2048/lrunit,
     4     FORM='UNFORMATTED',
     5     STATUS='UNKNOWN'     )
C
C using NTEST for record in comdir.com common comtest
      NTEST(IU)=0
      PRINT*,' PUTBUFR - IU ',IU
350   CONTINUE
C
CTEST Check if .key file already opened
      INQUIRE(FILE=cstream(msys)//'/'//CFNAME//'.key',OPENED=OPN,
     1        NUMBER=IUNIT)
      IF (OPN) GO TO 360
CTEST check if unit used - opn logical
      IUNIT=90
340   CONTINUE
      INQUIRE(UNIT=IUNIT,OPENED=OPN)
      IF (OPN) THEN
               IUNIT=IUNIT+1
               GO TO 340
      ENDIF
CTEST
CTEST Open test file for keys.
C
cc      PRINT*,' PUTBUFR - unused unit for .key ', IUNIT
               OPEN(UNIT=IUNIT,
     1         FILE=cstream(msys)//'/'//CFNAME//'.key',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C
CTEST write to file
CTEST
 360  CONTINUE         
crr ilength is length of bufr message
crr ilenout is length of bufr message + mbuf(1)
      ilenout=ilength+4
      if(ilenout.le.2048) then
cps Since Bufr message is read and written as a character array (a series of bytes),
cps we cannot easily decide if the bufr message has beeen created on a BE or LE machine.
cps We therefore have to agree on a definite endianess for mbuf(1)=ilength, and the 
cps natural choice is BE, since all numerical data in a Bufr message is packed BE.
#ifdef LITTLE_ENDIAN
         call bswap4(1,mbuf)
#endif
         NTEST(IU)=NTEST(IU)+1
ccccccccc      WRITE(IU,REC=NTEST(IU))ILENGTH,YOUT(:ILENGTH)
cpsapr04         WRITE(IU,REC=NTEST(IU))YOUT(:ilenout)
         WRITE(IU,REC=NTEST(IU))mbuf(1),YOUT(:ilength)
cps Swap back again after writing to bufr file, in case mbuf(1) is used later on.
#ifdef LITTLE_ENDIAN
         call bswap4(1,mbuf)
#endif
         print*,' PUTBUFR - ',cstream(msys)//'/'//CFNAME,' ',cident,
     *        ' ilength',ilength
cc      PRINT*,' PUTBUFR - IU,NTEST(IU),ILENGTH ',IU,NTEST(IU),ILENGTH
C
C  - Write keys to test file '.key'
crr changed 941207 write current(decoding) time
cpsapr01 added writing of correction field NCORR (>0 if CCA/CCB/...)
crr      WRITE(IUNIT,10001)CIDENT,ILENGTH,NTEST(IU),NQC,
crr     1    NTYPE,NSBTYPE,NYEAR,NMONTH,NDAY,NHOUR,NMINUTE,
crr     2    NSECOND,NLON1,NLAT1
         WRITE(IUNIT,10001)CIDENT,ILENGTH,NTEST(IU),NQC,
     1        NTYPE,NSBTYPE,NYEAR,NMONTH,IDD,IHH,IMM,
     2        ISS,NLON1,NLAT1,NCORR
10001    FORMAT(1X,A9,2I5,2I3,I4,I5,5I3,2I10,I3)
C
cc      print*,' ********* PUTBUFR : No testing of duplicates '
      else
         print*,' PUTBUFR - ',cstream(msys)//'/'//CFNAME,' ',cident,
     *        ' ilength',ilength
         print*,' PUTBUFR - length of bufr data gt rec. length of .rdb '
         print*,' PUTBUFR - ilenout = ',ilenout
         call exit(2)
      endif
CTEST
C
C
C     -----------------------------------------------------------------
 400  CONTINUE
C
      RETURN
      END
      FUNCTION P(Z)
C
      DATA A/5.252368255329/, B/44330.769230769/
      DATA  C/0.000157583169442/
      DATA  PTRO/226.547172/, PO/1013.25/
C
      IF (Z.GT.11000.) GO TO 50
      Y = 1.-Z/B
      P = PO*(Y**A)
      RETURN
C
50    Y = -C*(Z-11000.)
      P = PTRO*EXP(Y)
      RETURN
      END
      SUBROUTINE SAVBULL ( IERR )
C
C**** *SAVBULL*
C
C
C     PURPOSE.
C     --------
C
C         WRITE COMPLETE BULLETIN TO ERROR FILE .
C
C         BULLETIN IN KCHAR(1)-KCHAR(IGS) IN CCITT 5.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SAVBULL(IERR)*
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
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comindx.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
C
      CHARACTER*80 YLINE,YLINEA
      CHARACTER*1 Y63
      CHARACTER*4 YGS
C
      Y63=CHAR(63)
      YGS=CHAR(13)//CHAR(13)//CHAR(10)//CHAR(29)
C     ------------------------------------------------------------------
C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 100  CONTINUE
C
C      IF(IERR .EQ. 0) RETURN
C
      YLINE=' '
      YLINEA=' '
C
C
C
      OPEN(UNIT=10,IOSTAT=IOS,ERR=400,
     C     FILE=cstream(msys)//'/'//'pilo_error_file.dat',
#ifdef FORTRAN90
     C         POSITION='APPEND',
#else
     C         ACCESS='APPEND',
#endif
     C     STATUS='UNKNOWN'    )
C
      IP = 1
      JP = IP
      J = IGS
C
C*          1.1  OUTPUT INITIAL CONTROL CHARACTERS.
C                ----------------------------------
 110  CONTINUE
C
      CALL NEXTPRT (JP,IGS)
      K = JP - IP
      N1 = 0
      DO 111 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127) THEN
                                   YLINEA(N1:N1)=Y63
                                   ISIGN=1
                                   KCHAR(IP)=IAND(KCHAR(IP),127)
                                          END IF
         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
C
  111 CONTINUE
C
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
         ISIGN=0
      END IF
      YLINE=' '
      YLINEA=' '
C
 120  CONTINUE
C
      CALL NEXTPRT (IP,J)
      IF (IP .GT. J) GO TO 400
      JP = IP
      CALL NEXTEND (JP,J)
      CALL NEXTPRT (JP,J)
      K = JP - IP
      N1 = 0
      DO 112 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127 )
     C                      THEN
                               YLINEA(N1:N1) = Y63
                               ISIGN= 1
                               KCHAR(IP) = IAND(KCHAR(IP),127)
                            END IF
C
      YLINE(N1:N1) = CHAR(KCHAR(IP))
      IP = IP + 1
C
  112 CONTINUE
C
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
         ISIGN=0
      END IF
C
C
      YLINE=' '
      YLINEA=' '
C
      GO TO 120
C
 300  IERR = -1
      PRINT*,' ERROR IN WRITING INTO ERROR FILE IOS=',IOS
 400  CONTINUE
C
      WRITE(10,'(A)') YGS
      CLOSE(10)
C
C     REMOVE PARITY BIT
C
      DO 410 I = 1 , IGS
      KCHAR(I) = IAND(KCHAR(I),127)
 410  CONTINUE
C
      RETURN
C
C
      END
      SUBROUTINE SAVREP ( IHEAD,IERR)
C
C**** *SAVREP*
C
C
C     PURPOSE.
C     --------
C
C         WRITE REPORT IN ERROR TO THE ERROR FILE.
C
C                 IHEAD = 0 , WRITE BULLETIN HEADER AND ERROR REPORT
C                             TO ERROR FILE.
C                       = 1 , WRITE ERROR REPORT ONLY.
C                       = 2 , WRITE 'GS' CHARACTER AT END.
C
C                 IERR NOT USED.
C
C                 IHEAD SET TO 1 IF HEADER WRITTEN , OTHERWISE
C                       UNCHANGED.
C
C                 IERR SET TO -1 , IF ERROR IN WRITE , OTHERWISE
C                       UNCHANGED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SAVREP(IHEAD,IERR)*
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comindx.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
C
      DIMENSION ISTART(4),IFIN(4)
C
      CHARACTER *80 YLINE,YLINEA
      CHARACTER*1 Y63
      CHARACTER*4 YGS
C
      Y63=CHAR(63)
      YGS=CHAR(13)//CHAR(13)//CHAR(10)//CHAR(29)
C     ------------------------------------------------------------------
C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 100  CONTINUE
C
C      IF(IERR .EQ. 0) RETURN
C
      YLINE=' '
      YLINEA=' '
C
C
C
      OPEN(UNIT=10,IOSTAT=IOS,ERR=400,
     C     FILE=cstream(msys)//'/'//'pilo_error_file.dat',
#ifdef FORTRAN90
     C     POSITION='APPEND',
#else
     C     ACCESS='APPEND',
#endif
     C     STATUS='UNKNOWN'    )
C
C
C
C*          1.2  WRITE BULLETIN HEADER TO ERROR
C                ------------------------------
C                FILE IF NOT ALREADY DONE.
C                -------------------------
 120  CONTINUE
C
      IF ( IHEAD.NE.0 ) GO TO 130
C
C
      ISTART(1) = 1
      ISTART(2) = ISL
      ISTART(3) = IAH
      ISTART(4) = IMI
C
      IFIN(1) = ISL
      IFIN(2) = IAH
      IFIN(3) = JAH
      CALL NEXTEND(IFIN(3),IGS)
      CALL NEXTPRT(IFIN(3),IGS)
      IFIN(4) = JMI
      CALL NEXTEND(IFIN(4),IGS)
      CALL NEXTPRT(IFIN(4),IGS)
C
      IP = 0
      N2 = 0
C
      IHEAD = 1
C
      NN = 3
C
      DO 121 N=1,NN
         K = IFIN(N) - ISTART(N)
         DO 122 N1=1,K
            N2 = N2 + 1
            IP = IP + 1
            IF (KCHAR(IP).GT.127)
     C                      THEN
                                YLINEA(N2:N2) = Y63
                                ISIGN=1
                                KCHAR(IP)=IAND(KCHAR(IP),127)
                            END IF
C
            YLINE(N2:N2) = CHAR(KCHAR(IP))
C
  122    CONTINUE
C
C
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N2)
      IF(ISIGN.EQ.1) THEN
         WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N2)
         ISIGN=0
      END IF
C
         YLINE=' '
         YLINEA=' '
C
      N2 = 0
C
  121 CONTINUE
C
C
C*           1.3   WRITE ERROR REPORT , IF REQUIRED.
C***               ---------------------------------
 130  CONTINUE
C
      IF ( IHEAD.EQ.2 ) GO TO 140
C
C
C     AVOID WRITING REPORT TWICE IF MIMIMJMJ  LINE IS
C     MISSING.
C
      IF ( IAH.EQ.KPT ) THEN
           CLOSE(10)
           RETURN
      END IF
C
C     AVOID WRITING MIMIMJMJ TWICE ( CORRUPT MIMIMJMJ CAN BE TAKEN
C     AS STATION NUMBER )
C
      IF ( KPT.LT.IFIN(4) ) KPT = IFIN(4)
C
C
      IP = KPT
      IF(KDEC(4) .EQ. 35 .OR. KDEC(4) .EQ. 36) IP = IMI
      IF(KDEC(4) .EQ. 32 .OR. KDEC(4) .EQ. 33) IP = IMI
      J = IEQ+ 3
      IF ( J.GT.IGS) J = IGS
C
  133 CALL NEXTPRT (IP,J)
      IF (IP.GT.J) THEN
          CLOSE(10)
          RETURN
      END IF
C 
      JP = IP
      CALL NEXTEND(JP,J)
      CALL NEXTPRT(JP,J)
      K= JP - IP
      IF(K.GT.80) THEN
         K=80
      END IF  
      N1 = 0
      DO 131 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127)
     C                        THEN
                                  YLINEA(N1:N1) = Y63
                                  ISIGN= 1
                                  KCHAR(IP) = IAND(KCHAR(IP),127)
                              END IF
C
         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
C
 131  CONTINUE
C
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
         ISIGN=0
      END IF
C
      YLINE=' '
      YLINEA=' '
C
C
      GO TO 133
C
C
C*              1.4 WRITE 'GS' AT END OF BULLETIN.
C                   ------------------------------
 140  CONTINUE
C
      YLINE(1:4)= YGS
C
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE
      CLOSE(10)
C
      GO TO 400
C     ----------------------------------------------------------------
 300  IERR = -1
      PRINT*,'ERROR IN WRITING INTO ERROR FILE IOS=',IOS
C     ----------------------------------------------------------------
 400  CONTINUE
C
C     REMOVE PARITY BIT
C
      DO 500 I = 1 , IGS
      KCHAR(I) = IAND(KCHAR(I),127)
 500  CONTINUE
C
      RETURN
      END
      SUBROUTINE MRGPIL( IERR)
C
C**** *MRGPIL*
C
C
C     PURPOSE.
C     --------
C         MERGE PILOT PARTS A, B, C AND D INTO SINGLE OBSERVATION.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *MRGPIL( IERR )*
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
C        *call* *asctim*
C        *call* bswap4* (libmi)
C        function *iget_next_recnumber*
C        function *iget_record_number*
C        *call* *istime*
C        *call* *rlunit* (libmi)
C        logical function *swapfile* (libmi)
C        *call* *update_index_file*
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
      IMPLICIT LOGICAL(O,G,L), CHARACTER*8(C,H,Y)
      integer lrunit
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
      INCLUDE 'comstd.f'
      INCLUDE 'comkey.f'
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
      logical ocont(4),new_record,swap,swapfile,file_exist
C
      DIMENSION VALUESA(900),VALUESB(900),VALUESC(900)
      DIMENSION PRESS(400),PRESSC(600),INX(400),INXA(400)
      DIMENSION ITEML(2000),YSHNAM(100),CVALA(10),IDM(12)
C
      CHARACTER CVAL*9,CVALA*9,CUNIT*24,YSHNAM*9
cpsapr98      CHARACTER*2 YEAR,YMONTH,YDAY,YHOUR,YMIN
      character*4 year
      CHARACTER*2 YMONTH,YDAY,YHOUR,YMIN
      CHARACTER*80 YFILENAME
      character*3 cmonth(12)
      character*23 ct,current      
cpsapr98      CHARACTER*19 YSD
      CHARACTER*21 YSD
C
      DATA ITEML/2000*999999/,YSHNAM/100*'       '/
      DATA VALUESA/900*0.0/,PRESS/400*0.0/,INX/400*0/,INXA/400*0/
      DATA VALUESB/900*0.0/,VALUESC/900*0.0/
      DATA LSHEAR/.FALSE./
      DATA IDM/31,28,31,30,31,30,31,31,30,31,30,31/
C
      data cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     1            'Aug','Sep','Oct','Nov','Dec'/
C
      integer*2 endian,Eendian
cpsjun11      structure /merge/
      TYPE :: merge
cpsapr98      CHARACTER*19 STATION_DATE
      CHARACTER*21 STATION_DATE
cpsmar07 INTEGER*4    NNSNUM  ! Don't understand the purpose of this variable
                              ! (see update_sequence_number.hlp). Therefore removed
      LOGICAL*1    OOCONT(4)
      INTEGER*4    NNCORN(4)
      INTEGER*4    MMA
      REAL         VVALUESA(900)
cpsapr04      INTEGER*4    IILENC
      CHARACTER*9  CCVALA(10)
cpsjun11      end structure
      end TYPE merge
C
cpsjun11      record /merge/ old_pilot
      TYPE (merge) old_pilot
      integer iget_record_number,iget_next_recnumber
      DATA keylen/21/
C     ------------------------------------------------------------------
C
C*          1.   SET UP RECORD NUMBER FOR DIRECT ACCESS.
C                ---------------------------------------
 100  CONTINUE
C
CTEST
      IF(IERR.NE.0) then
         PRINT*,' MRGPIL - start of subr.,ierr ',IERR
CTEST         CALL KDECOUT(KDEC,1,KDEC(24),MINDIC,KINT(4))
      endif
CTEST
      if(ierr.ne.0) return
c
      ocont(1)=.false.
      ocont(2)=.false.
      ocont(3)=.false.
      ocont(4)=.false.
C
      NCORN(1)= 0
      NCORN(2)= 0
      NCORN(3)= 0
      NCORN(4)= 0
C
      NPART(1)=0
      NPART(2)=0
      NPART(3)=0
      NPART(4)=0
c
      new_record=.false.
C
C
C*          1.1  READ IN PREVIOUS REPORT IF ANY.
C                --------------------------------
 110  CONTINUE
C
C     DEFINE FILE NAME
C
cpsapr98      IYEAR=VALUES(5,1)-1900
      IYEAR=nint(VALUES(5,1))
cpsapr98      IF(KDEC(4).EQ.33) IYEAR =VALUES(6,1)-1900
      IF(KDEC(4).EQ.33) IYEAR =nint(VALUES(6,1))
      IMONTH=VALUES(6,1)
      IF(KDEC(4).EQ.33) IMONTH=VALUES(7,1)
      IDAY=VALUES(7,1)
      IF(KDEC(4).EQ.33) IDAY  =VALUES(8,1)
      IHOUR=VALUES(8,1)
      IF(KDEC(4).EQ.33) IHOUR =VALUES(9,1)
      imin=VALUES(9,1)
      IF(KDEC(4).EQ.33) imin=VALUES(10,1)
c
cpsapr98      iy=iyear+1900
      iy=iyear
      write(ct(1:2),'(i2.2)') iday
      ct(3:3)='-'
      ct(4:6)=cmonth(imonth)
      ct(7:7)='-'
      write(ct(8:11),'(i4)') iy
      ct(12:12)=' '
      write(ct(13:14),'(i2.2)') ihour
      ct(15:15)=':'
      write(ct(16:17),'(i2.2)') imin
      ct(18:18)=':'
      write(ct(19:20),'(i2.2)') 0
      ct(21:21)='.'
      write(ct(22:23),'(i2.2)') 0
c
      call asctim(current)
      call istime(current,nm1,iret)
      call istime(ct,nm0,iret)
      idf=nm1-nm0
c
      if(idf.le.0) then
         print*,' mrgpilo: data for future',ct
         ierr=1
         return
      end if
c      if(idf.gt.2880) then
c         print*,' mrgpilo: data to old ',ct
c         ierr=1
c         return
c      end if
C
 115  CONTINUE
C
cpsapr98      WRITE(YEAR,'(I2.2)') IYEAR
      WRITE(YEAR,'(I4)') IYEAR
      WRITE(YMONTH,'(I2.2)') IMONTH
      WRITE(YDAY,'(I2.2)') IDAY
      WRITE(YHOUR,'(I2.2)')IHOUR
      WRITE(YMIN,'(I2.2)') IMIN
C
      OLD_PILOT%STATION_DATE=' '
C
      IF(KDEC(4).EQ.32) THEN
         ISTNO=NINT(VALUES(1,NSUB))*1000+NINT(VALUES(2,NSUB))
         WRITE(OLD_PILOT%STATION_DATE(1:5),'(I5.5)') ISTNO
      ELSE
         OLD_PILOT%STATION_DATE(1:9)=CVAL(1)
      END IF
C
cpsapr98      OLD_PILOT.STATION_DATE(10:19)=YEAR//YMONTH//YDAY//YHOUR//YMIN
      OLD_PILOT%STATION_DATE(10:21)=YEAR//YMONTH//YDAY//YHOUR//YMIN
cpsapr98      YSD(1:19)=OLD_PILOT.STATION_DATE
      YSD(1:21)=OLD_PILOT%STATION_DATE
C
      YFILENAME=cstream(msys)//'/'//'pilo'//YEAR//YMONTH//YDAY//'MERGE'
C
      inquire(file=YFILENAME,exist=file_exist)
      call rlunit(lrunit)
      OPEN(UNIT=7,IOSTAT=IOS,ERR=120,FILE=YFILENAME,
     1            ACCESS='DIRECT',
     2            FORM='UNFORMATTED',
     4            RECL=4*1200/lrunit,
     5            STATUS='UNKNOWN')
C
c     Decide endianess of merge file by investigating first 2 bytes of file
      swap=.false.
      if(file_exist) swap=swapfile(-7)

      iunit=8
      irec=iget_record_number(YFILENAME,iunit,OLD_PILOT%STATION_DATE)
      if(irec.lt.0) goto 130

      READ(UNIT=7,rec=irec,IOSTAT=IOS,ERR=130) Eendian,OLD_PILOT
      if(swap) then
         call bswap4(4,old_pilot%nncorn)
         call bswap4(1,old_pilot%mma)
         call bswap4(900,old_pilot%vvaluesa)
      end if
C
      DO 12 I=1,4
      OCONT(I)=OLD_PILOT%OOCONT(I)
      NCORN(I)=OLD_PILOT%NNCORN(I)
 12   CONTINUE
C
      DO 13 I=1,900
      VALUESA(I)=OLD_PILOT%VVALUESA(I)
 13   CONTINUE
C
      MA   =OLD_PILOT%MMA
cpsapr04      ILENC=OLD_PILOT.IILENC
cpsmar07      NSNUM=OLD_PILOT.NNSNUM
C
      DO 14 I=1,10
      CVALA(I)=OLD_PILOT%CCVALA(I)
 14   CONTINUE
      
      GO TO 200
C
 120  CONTINUE
C
      PRINT*,'FILE NAME=',YFILENAME 
      WRITE(*,9901) IOS
 9901 FORMAT(1H ,' ERROR DURING OPENING UNIT 7, IOS=',I5)
      call exit(2)
C
 130  CONTINUE
C
C     IF RECORD DOES NOT EXIST WRITE THE RECORD
cps   i.e. we continue as if the MERGE file contained exactly this
c     record, and merge it with itself. This seems like unnecessary
c     work, but as a byproduct we do for example get the pressure 
c     levels sorted.
C     
      new_record=.true.
      MA=M
cpsapr04      ILENC=1
      CVALA(1)=CVAL(1)
      DO 131 I=1,M
         VALUESA(I)=VALUES(I,NSUB)
 131  CONTINUE
C
C     ------------------------------------------------------------------
C*           2. MERGE NEW AND OLD REPORT.
C               -------------------------
 200  CONTINUE
C
cpsjun11      IF(KDEC(23).EQ.1.AND.OCONT(1).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.1.AND.OCONT(1)) THEN
         IF(KDEC(21).LT.NCORN(1)) THEN
            GO TO 231
         ELSE
            NCORN(1)=KDEC(21)
         END IF
      END IF
cpsjun11      IF(KDEC(23).EQ.1.AND.OCONT(1).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.1.AND.OCONT(1)) THEN
         IF(KDEC(21).LE.NCORN(1)) THEN
            GO TO 231
         ELSE
            NCORN(1)=KDEC(21)
            GO TO 230
         END IF 
      END IF
cpsjun11      IF(KDEC(23).EQ.1.AND.OCONT(1).EQ..FALSE.) THEN
      IF(KDEC(23).EQ.1.AND..NOT.OCONT(1)) THEN
         OCONT(1)=.TRUE.
         NCORN(1)=KDEC(21)
         GO TO 230
      END IF
C
cpsjun11      IF(KDEC(23).EQ.2.AND.OCONT(2).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.2.AND.OCONT(2)) THEN
         IF(KDEC(21).LT.NCORN(2)) THEN
            GO TO 231
         ELSE
            NCORN(2)=KDEC(21)
         END IF
      END IF
cpsjun11 IF(KDEC(23).EQ.2.AND.OCONT(2).EQ..FALSE.) THEN
      IF(KDEC(23).EQ.2.AND..NOT.OCONT(2)) THEN
         OCONT(2)=.TRUE.
         NCORN(2)=KDEC(21)
         GO TO 230
      END IF
C
cpsjun11      IF(KDEC(23).EQ.3.AND.OCONT(3).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.3.AND.OCONT(3)) THEN
         IF(KDEC(21).LT.NCORN(3)) THEN
            GO TO 231
         ELSE
            NCORN(3)=KDEC(21)
         END IF
      END IF
cpsjun11      IF(KDEC(23).EQ.3.AND.OCONT(3).EQ..FALSE.) THEN
      IF(KDEC(23).EQ.3.AND..NOT.OCONT(3)) THEN
         OCONT(3)=.TRUE.
         NCORN(3)=KDEC(21)
         GO TO 230
      END IF
C
cpsjun11      IF(KDEC(23).EQ.4.AND.OCONT(4).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.4.AND.OCONT(4)) THEN
         IF(KDEC(21).LT.NCORN(4)) THEN
            GO TO 231
         ELSE
            NCORN(4)=KDEC(21)
         END IF
      END IF
cpsjun11      IF(KDEC(23).EQ.4.AND.OCONT(4).EQ..FALSE.) THEN
      IF(KDEC(23).EQ.4.AND..NOT.OCONT(4)) THEN
         OCONT(4)=.TRUE.
         NCORN(4)=KDEC(21)
         GO TO 230
      END IF
C
 231  CONTINUE
C
      MC=MA
      DO 232 I=1,MA
      VALUESC(I)=VALUESA(I)
 232  CONTINUE
C
      GO TO 400
C  
 230  CONTINUE
C
      MB=M+MA-13
C
      IS=NINT(VALUES(13,NSUB))*5+13
      ISA=NINT(VALUESA(13))*5+13
C
C     COPY VALUESA INTO VALUESB
C
      J=0
      DO 201 I=1,ISA
      J=J+1
      VALUESB(J)=VALUESA(I)
 201  CONTINUE
C
C     COPY VALUES INTO VALUESB
C
      DO 202 I=14,IS
      J=J+1
      VALUESB(J)=VALUES(I,NSUB)
 202  CONTINUE
C
C     UPDATE NUMBER OF LEVELS
C
      VALUESB(13)=VALUES(13,NSUB)+VALUESA(13)
C
      ISB=NINT(VALUESB(13))*5+13
C
C*           2.1 SORT PRESSURE LEVELS IN DESCENDING ORDER.
C                -----------------------------------------
 210  CONTINUE
C
      J=0
      DO 211 I=14,ISB,5
C
      J=J+1
C
      if(valuesb(i).eq.999999.)
     1   then
            sh1=valuesb(i+2)/9.8
            do 2222 ik=2,511
            shl=(sh(ik-1)+sh(ik))/2.
            shh=(sh(ik)+sh(ik+1))/2.
            if(sh1.ge.shl.and.sh1.le.shh)
     1         then
                  press(j)=p(ik)
                  inx(j)=i
                  go to 211
               end if
2222        continue
         end if   
c
      PRESS(J)=VALUESB(I)
      INX(J)=I
C
 211  CONTINUE
C
      JJA=9
      JJ=0
      DO 212 II=1,J
C
      PMAX=PRESS(1)
      IMAX=1
C
      DO 213 I=1,J
      IF(PRESS(I).GT.PMAX)
     1   THEN
            PMAX=PRESS(I)
            IMAX=I
         END IF
 213  CONTINUE
C
      PRESS(IMAX)=0.0
      JJA=JJA+5
      JJ=JJ+1
      PRESSC(JJA)=PMAX
      INXA(JJ)=INX(IMAX)
 212  CONTINUE
C
C     COPY FIRST 13 ELEMENTS OF VALUESB INTO VALUESC
C
      DO 214 I=1,13
      VALUESC(I)=VALUESB(I)
 214  CONTINUE
C
C
      IJ=14
C
      DO 215 I=1,JJ
C
      J=INXA(I)
C
      VALUESC(IJ  )=VALUESB(J)
      VALUESC(IJ+1)=VALUESB(J+1)
      VALUESC(IJ+2)=VALUESB(J+2)
      VALUESC(IJ+3)=VALUESB(J+3)
      VALUESC(IJ+4)=VALUESB(J+4)
C
      IJ=IJ+5
C
 215  CONTINUE
C
 220  CONTINUE
C
      ISC=ISB
C
C
C     DISCARD LEVELS WITH SAME PRESSURE
C
      ICOUNT=1
      I =14
      K =9
      IC=14
C
 250  CONTINUE
C
      K=K+5
      IF(K.GT.ISC) GO TO 260
C  
      IF(PRESSC(I).EQ.PRESSC(K)) THEN
         IA=NINT(VALUESC(I+1))
         IB=NINT(VALUESC(K+1))
cpsjun11         IAIB=IA.OR.IB
         IAIB=IOR(IA,IB)
C
         VALUESC(IC  )=VALUESC(K)
         VALUESC(IC+1)=FLOAT(IAIB)
         VALUESC(IC+2)=VALUESC(K+2) 
         VALUESC(IC+3)=VALUESC(K+3) 
         VALUESC(IC+4)=VALUESC(K+4) 
C
         I=K
         GO TO 250
      ELSE
         ICOUNT=ICOUNT+1
         IC=IC+5
C
         VALUESC(IC  )=VALUESC(K)
         VALUESC(IC+1)=VALUESC(K+1)
         VALUESC(IC+2)=VALUESC(K+2) 
         VALUESC(IC+3)=VALUESC(K+3) 
         VALUESC(IC+4)=VALUESC(K+4) 
C
         I=K
         GO TO 250
C
      END IF
C
C
 260  CONTINUE         
C
C     UPDATE NUMBER OF LEVELS
C
      VALUESC(13)=FLOAT(ICOUNT)
      ISC=VALUESC(13)*5+13
      MC=ISC 
C
 280  CONTINUE
C
C     -----------------------------------------------------------------
C*         3. ADD WIND SHEAR LEVELS IF ANY.
C             -----------------------------
 300  CONTINUE
C
      IWLEVS=0
      IWLEVA=0
      DO 301 KB=1,600
      PRESSC(KB)=0.
 301  CONTINUE
C
      IF(IS.LT.M.OR.ISA.LT.MA)
     1   THEN
            J=ISC+1
            IF(IS.LT.M) THEN
                           IWLEVS=NINT(VALUES(IS+1,NSUB))
                           DO 262 I=IS+2,M
                           J=J+1
                           VALUESC(J)=VALUES(I,NSUB)
 262                       CONTINUE
                        END IF
C
            IF(ISA.LT.MA) THEN
                             IWLEVA=NINT(VALUESA(ISA+1))
                             DO 263 I=ISA+2,MA
                             J=J+1
                             VALUESC(J)=VALUESA(I)
 263                         CONTINUE
                          END IF
C
           IWLEVC=IWLEVS+IWLEVA
           VALUESC(ISC+1)=FLOAT(IWLEVC)
           MC=J
C
C*           3.1 SORT PRESSURE LEVELS IN DESCENDING ORDER.
C                -----------------------------------------
 310  CONTINUE
C
           J=0
           DO 311 I=ISC+2,MC,5
C
           J=J+1
           if(valuesc(i).eq.999999.)
     1        then
                 sh1=valuesc(i+2)/9.8
                 do 3222 ii=2,511
                 shl=(sh(ii-1)+sh(ii))/2.
                 shh=(sh(ii)+sh(ii+1))/2.
                 if(sh1.ge.shl.and.sh1.le.shh)
     1              then
                       press(j)=p(ii)
                       inx(j)=i
                       go to 311
                    end if
3222             continue
              end if
c
           PRESS(J)=VALUESC(I)
           INX(J)=I
C
 311       CONTINUE
C
           JJS=-4
           JJ=0
           DO 312 II=1,J
C
           PMAX=PRESS(1)
           IMAX=1
C
           DO 313 I=1,J
           IF(PRESS(I).GT.PMAX)
     1        THEN
                 PMAX=PRESS(I)
                 IMAX=I
              END IF
 313       CONTINUE
C
           PRESS(IMAX)=0.0
           JJ=JJ+1
           JJS=JJS+5
           PRESSC(JJS)=PMAX
           INXA(JJ)=INX(IMAX)
 312       CONTINUE
C
C
           DO 315 I=1,JJ
C
           J=INXA(I)
           IJ=(I-1)*5+1
C
           VALUESB(IJ  )=VALUESC(J)
           VALUESB(IJ+1)=VALUESC(J+1)
           VALUESB(IJ+2)=VALUESC(J+2)
           VALUESB(IJ+3)=VALUESC(J+3)
           valuesb(ij+4)=valuesc(j+4)
C
 315       CONTINUE
C
           IJ=IJ+4 
C
 320       CONTINUE
C
C         DISCARD LEVELS WITH SAME PRESSURE
C
          ICOUNT=1
          I=1
          K=-4
          IC=1
C
 330      CONTINUE
C
          K=K+5
          IF(K.GT.IJ) GO TO 360
C  
          IF(PRESSC(I).EQ.PRESSC(K)) THEN
             IA=NINT(VALUESB(I+1))
             IB=NINT(VALUESB(K+1))
cpsjun11         IAIB=IA.OR.IB
             IAIB=IOR(IA,IB)
C
             VALUESB(IC  )=VALUESB(K)
             VALUESB(IC+1)=FLOAT(IAIB)
             VALUESB(IC+2)=VALUESB(K+2) 
             VALUESB(IC+3)=VALUESB(K+3) 
             VALUESB(IC+4)=VALUESB(K+4) 
C
             I=K
             GO TO 330
         ELSE
            ICOUNT=ICOUNT+1
            IC=IC+5
C
            VALUESB(IC  )=VALUESB(K)
            VALUESB(IC+1)=VALUESB(K+1)
            VALUESB(IC+2)=VALUESB(K+2) 
            VALUESB(IC+3)=VALUESB(K+3) 
            VALUESB(IC+4)=VALUESB(K+4) 
C
            I=K
            GO TO 330
C
         END IF
C
C
 360  CONTINUE         
C
C
C         UPDATE NUMBER OF LEVELS
C
          VALUESC(ISC+1)=FLOAT(ICOUNT)
          MC=NINT(VALUESC(ISC+1))*5+ISC+1
C
C
 380  CONTINUE
C
         JJ=ISC+1
         IIII=ICOUNT*5
C
         DO 362 II=1,IIII
         JJ=JJ+1
         VALUESC(JJ)=VALUESB(II)
 362     CONTINUE
C
      END IF
C
cpsapr04      ILENC=1
C
      CVALA(1)=CVAL(1)
C
C     ------------------------------------------------------------------
C*         4.  REWRITE  REPORT.
C              ----------------
 400  CONTINUE
C
      M=MC
      N=NSUB
C
      DO 401 I=1,MC
      VALUES(I,NSUB)=VALUESC(I)
 401  CONTINUE
C
cpsmar07      NSNUM=NSNUM+1
C
      DO 402 I=1,4
      IF(OCONT(I)) NPART(I)=1
 402  CONTINUE
C
      OLD_PILOT%STATION_DATE=YSD
      DO 403 I=1,4
      OLD_PILOT%OOCONT(I)=OCONT(I)
      OLD_PILOT%NNCORN(I)=NCORN(I)
 403  CONTINUE
C
cpsmar07      OLD_PILOT.NNSNUM=NSNUM
      OLD_PILOT%MMA=MC
cpsapr04      OLD_PILOT.IILENC=ILENC
C
      DO 404 I=1,900
      OLD_PILOT%VVALUESA(I)=VALUESC(I)
 404  CONTINUE
C
      DO 405 I=1,10
      OLD_PILOT%CCVALA(I)=CVALA(I)
 405  CONTINUE
C
      IF(new_record) then
         irec=iget_next_recnumber(yfilename,iunit,keylen)
         endian=1
      else
         endian=Eendian
      end if
c
      if(swap) then
         call bswap4(4,old_pilot%nncorn)
         call bswap4(4,old_pilot%nncorn)
         call bswap4(1,old_pilot%mma)
         call bswap4(900,old_pilot%vvaluesa)
      end if
      WRITE(UNIT=7,REC=irec,IOSTAT=IOS,ERR=410) endian,OLD_PILOT
c
      IF(new_record) THEN
         call update_index_file(yfilename,iunit,irec,
     *        OLD_PILOT%STATION_DATE)
      END IF
C
      CLOSE(7)
C
      RETURN
C
 410  CONTINUE
C
      print*,'Error during write in ',yfilename,' Error ',ios
      CALL exit(2)
C
      RETURN
      END
      SUBROUTINE DUPLIGR(IFOUND)
C
C**** *DUPLIGR*
C
C
C     PURPOSE.
C     --------
C         FIND OUT IF TWO PREVIOUS GROUPS ARE DUPLICATES
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DUPLIGR(IFOUND)*
C
C          OUTPUT     IFOUND  = 1 IF DUPLICATE GROUPS FOUND
C                     IREMPOS   INDICATOR TO THE POINT OF THE FIRST ERROR
C                     IPT         POINTS TO THE BEGINNING OF THE SECOND
C                                 DUPLICATE.
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
C          A. HOLOPAINEN  JAN 84
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
       INCLUDE 'parameter.f'
       INCLUDE 'comwork.f'
       INCLUDE 'comindx.f'
C
C     ------------------------------------------------------------------
C*          1.   FIND DUPLICATED GROUPS.
C                -----------------------
 100  CONTINUE
C
      IFOUND = 0
      NPT = IPT
C
      CALL PRESEP(NPT,IMI,*1000)
      CALL PREPRT(NPT,IMI,*1000)
      CALL PRESEP(NPT,IMI,*1000)
      CALL PREPRT(NPT,IMI,*1000)
      CALL PRESEP(NPT,IMI,*1000)
      CALL NEXPRT2(NPT,IEQ,*1000)
C
C     POINTER TO THE FIRST POSSIBLE DUPLICATE
C
      IPO1 = NPT
C
      CALL NEXSEP2(NPT,IEQ,*1000)
      CALL NEXPRT2(NPT,IEQ,*1000)
C
C     POINTER TO THE SECOND POSSIBLE DUPLICATE
C
      IPO2 = NPT
C
C     CHECK THE VALUES
C
      CALL EXTVAL(IPO1,5,IVAL1)
      CALL EXTVAL(IPO2,5,IVAL2)
C
      IF(IVAL1 .EQ. MINDIC .OR. IVAL2 .EQ. MINDIC) RETURN
C
      IF(IVAL1 .EQ. IVAL2)
     C   THEN
C
C           DUPLICATE GROUPS FOUND  --  SET THE POINTER IPT TO THE
C                                       BEGINNING OF THE DUPLICATE.
C                                       DUPLICATE.
C
            IPT = NPT
            IFOUND = 1
            RETURN
         END IF
C
1000  CONTINUE
      RETURN
C
      END
      SUBROUTINE LEVCHEC (LEVNUM,IPOS,ISTEP,LEVER,NEWSEC)
C
C**** *LEVCHEC*
C
C
C     PURPOSE.
C     --------
C
C         TRY TO FIND MORE CORRECT LEVELS IN TEMP PARTB AND
C         PART D AFTER AND ERROR OF PRESSURE HAS BEEN FOUND.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *LEVCHEC (LEVNUM,IPOS,ISTEP,LEVER,NEWSEC)*
C
C          INPUT     : LEVNUM - THE LEVEL NUMBER (0,1,2,...)
C                    : IPOS   - POINTER OF THE LEVEL IN KINT
C                    : ISTEP  - THE STEP OF LEVELS IN KINT
C
C          OUTPUT    : LEVER  - THE INDICATOR OF DELETED LEVEL
C                      NEWSEC - THE INDICATOR OF POSSIBLE NEW SECTION
C                               (= 21212 FOR WIND SECTION ETC.)
C
C     METHOD.
C     -------
C
C         FOUR LEVELS, TWO BEFORE AND AND ONE AFTER THE ERROR,
C         WILL BE CHECKED, BECAUSE THE HEIGHT MIGHT BE WRONG
C         EITHER IN THE 'ERROR' GROUP,IF IT IS TOO BIG OR THE
C         PREVIOUS GROUP IF IT IS TOO SMALL.
C         IF THE ERROR IS ON THE SECOND LEVEL, ONLY THREE
C         LEVELS WILL BE CHECKED.
C
C
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
C          A. HOLOPAINEN DEC.83
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
      DIMENSION LEV(4)
C
      DATA LEV /4*-999999/
C
C     ------------------------------------------------------------------
C
C*          1.   LEVEL CHECK.
C                ------------
 100  CONTINUE
C
C
      LEVER = 0
      NEWSEC = 0
C
C     SEE IF THE WIND SECTION INICATOR HAS BEEN PASSED
C
      IF(ISTEP .NE. 5) GO TO 102
C
      IF(KINT(IPOS+1) .GE. 100) GO TO 102
C
      CALL PRE2121(IFOUND)
C
      IF(IFOUND .EQ. 1)
     C   THEN
            NEWSEC = 21212
            DO 101 I=IPOS,600
            KINT(I) = MINDIC
101         CONTINUE
            KINT(317) = 21212
            RETURN
         END IF
C
 102  CONTINUE
C
C
C*          1.1  SEE IF THE CLOUD SECTION, OR REGIONAL/NATIONAL
C                SECTION INDICAROR HAS BEEN PASSED
 110  CONTINUE
C
C===  CALL PRESEC(NEWSEC)
C===  IF(NEWSEC .GT. 0) RETURN
C
C     GET THE POINTER OF THE THREE LEVELS
C
C
C
      NPT = IPT
      IPO3 = NPT
      CALL PRESEP(NPT,LPT,*1000)
C
      NN = 4
      IF(LEVNUM .LE. 1) NN = 2
C
         DO 111 I=1,NN
         CALL PREPRT(NPT,IMI,*1000)
         CALL PRESEP(NPT,IMI,*1000)
         IF(I .EQ. 2) IPO2 = NPT
         IF(I .EQ. 4) IPO1 = NPT
 111     CONTINUE
C
         CALL NEXPRT2(IPO2,IEQ,*1000)
         IF(LEVNUM .GT. 1) CALL NEXPRT2(IPO1,IEQ,*1000)
C
C     THE LOOP TO GET THE LEVEL INDICTORS AND PRESSURES
C
C
C
112   CONTINUE
C
      IF(LEVNUM .GT. 1) CALL EXTVAL(IPO1+2,3,LEV(1))
      CALL EXTVAL(IPO2+2,3,LEV(2))
      CALL EXTVAL(IPO3+2,3,LEV(3))
C
      NPT = IPT
      CALL NEXSEP2(NPT,IEQ,*1000)
      CALL NEXPRT2(NPT,IEQ,*1000)
      CALL NEXSEP2(NPT,IEQ,*1000)
      CALL NEXPRT2(NPT,IEQ,*1000)
C
      N1 = IAND(KCHAR(NPT),127)  - 48
      N2 = IAND(KCHAR(NPT+1),127) - 48
C
C
C     CHECK IF THIS IS CORECT BEGINNING OF SIGNIFICANT LEVEL (N1=N2)
C
      IF(N1 .NE. N2) THEN
C
C                       CHECK IF THIS IS THE BEGINNING
C                       OF SECTION 6, 8, 9 OR 10
C
                        CALL EXTVAL(IPT,5,IVAL)
C
                        IF(ISTEP .EQ. 5)
     C                     THEN
                              IF(IVAL .EQ. 21212)
     C                           THEN
                                    LEVER = 3
                                    IPT = NPT
                                    RETURN
                                  END IF
                            END IF
C
                        IF(IVAL .EQ. 41412)
     C                     THEN
                              LEVER = 3
                              IPT = NPT
                              RETURN
                           END IF
C
                        IF((IVAL .EQ. 51515) .OR. (IVAL .EQ. 52525) .OR.
     C                     (IVAL .EQ. 53535) .OR. (IVAL .EQ. 54545) .OR.
     C                     (IVAL .EQ. 55555) .OR. (IVAL .EQ. 56565) .OR.
     C                     (IVAL .EQ. 57575) .OR. (IVAL .EQ. 58585) .OR.
     C                     (IVAL .EQ. 59595) .OR. (IVAL .EQ. 51525))
     C                        THEN
                                 LEVER = 3
                                 IPT = NPT
                                 RETURN
                              END IF
C
C
                        IF((IVAL .EQ. 61616) .OR. (IVAL .EQ. 62626) .OR.
     C                     (IVAL .EQ. 63636) .OR. (IVAL .EQ. 64646) .OR.
     C                     (IVAL .EQ. 65656) .OR. (IVAL .EQ. 66666) .OR.
     C                     (IVAL .EQ. 67676) .OR. (IVAL .EQ. 68686) .OR.
     C                     (IVAL .EQ. 69696))
     C                        THEN
                                 LEVER = 3
                                 IPT = NPT
                                 RETURN
                              END IF
C
C
               LEVER = 3
               IPT = NPT
                       RETURN
C
                    END IF
C
      CALL EXTVAL(NPT+2,3,LEV(4))
C
C
C
C
1000  CONTINUE
C
      NN = 1
      IF(LEVNUM .LE. 1) NN = 2
C
      DO 113 I=NN,4
      IF(LEV(I) .EQ. -999999) RETURN
 113  CONTINUE
C
C     CHECK WHICH OF THE LEVELS IS OUT OF SEQUENCE
C
      IF(LEV(3) .LT. LEV(1) .OR. LEVNUM .LE. 1)
     C   THEN
            IF(LEV(4) .LT. LEV(2))
     C         THEN
C
C                 LEV(3) IS INCORRECT
C
                  IPT = NPT
                  LEVER = 3
                  RETURN
               ELSE
C
C                 LEV(2) IS INCORRECT
C
                  DO 114 I = IPOS , IPOS+ISTEP-1
                  KINT(I) = MINDIC
 114              CONTINUE
                  LEVER = 2
                  RETURN
               END IF
         ELSE
C
C           LEV(3) IS INCORRECT
C
            LEVER = 3
            IPT = NPT
            RETURN
         END IF
C
C
2000  CONTINUE
      RETURN
      END
      SUBROUTINE KDECOUT(IA,K,J,MINDIC,KISST)
CTEST print to file 98
C**** *PRTKDEC*
C
C
C     PURPOSE.
C     --------
C         PRINTS THE DECODED FORMAT ARRAY (KDEC)
C         OF DECODING DATA (PHASE II).
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PRTKEDEC(IA,K,J,MINDIC)*
C
C         INPUT     : IA     - THE 'KDEC' ARRAY
C                      K      - PRINT STARTS AT WORD I.
C                      J      - PRINT STOPS AT WORD J .
C                      MINDIC - MISSING VALUE INDICATOR
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
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
CRR common for directories
      INCLUDE 'comdir.f'
C
      DIMENSION IA(1)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT ARRAY 'KDEC'.
C                -------------------
 100  CONTINUE
C
CTEST
ccc      ispc=char(32)
ccc      ndx=0
C  - Find end of character string. 
ccc      ndx=index(ytest(1:80),ispc) - 1
CTEST               OPEN(UNIT=98,
CTEST     1         FILE=YTEST(1:ndx)//'pilo-out.dat',
CTEST     2         ACCESS='APPEND',
CTEST     3         CARRIAGECONTROL='LIST',
CTEST     4         FORM='FORMATTED',
CTEST     5         STATUS='UNKNOWN'        )
CTEST
C
C
      LODATA = .TRUE.
      LODOT = .TRUE.
C
      WRITE(98,*) ' KINT(4) = ',KISST
      WRITE(98,10000)
10000 FORMAT(1H ,2X,'  DATA IN DECODED FORMAT ( KDEC ) ',/)
C
C
         DO 101 I=K,J,10
         I2 = I+9
C
            DO 102 JJ=I,I2
            IF(IA(JJ) .EQ. MINDIC) GO TO 102
            LODATA = .FALSE.
102         CONTINUE
C
         IF(LODATA) THEN
C
C                      CHECK IF THERE ARE MORE DATA
C
                       DO 103 JJ=I2,J
                       IF(IA(JJ) .EQ. MINDIC) GO TO 103
                       GO TO 104
103                    CONTINUE
                       RETURN
C
104                    CONTINUE
C
                       IF(LODOT) THEN
                                    WRITE(98,20000)
20000                               FORMAT(1H ,3X,'---',/1H ,3X,'---')
                                    LODOT = .FALSE.
                                 END IF
                       GO TO 101
                    END IF
C
         LODOT = .TRUE.
         LODATA = .TRUE.
C
         IF(I .EQ. 1)  WRITE(98,30000) I,(IA(ik),ik=I,I2)
         IF(I .EQ. 11) WRITE(98,40000) I,(IA(ik),ik=I,I2)
         IF(I .GT. 20) WRITE(98,50000) I,(IA(ik),ik=I,I2)
30000    FORMAT(1H ,2X,I4,4X,6(I10,1X),6X,A4,1X,3(I10,1X))
40000    FORMAT(1H ,2X,I4,4X,2(I10,1X),O10,1X,I10,1X,O10,1X,2(I10,1X),
     C          2(I10,1X),I10)
50000    FORMAT(1H ,2X,I4,4X,10(I10,1X))
C
101      CONTINUE
C
C
C
C
C
      RETURN
      END
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comstat.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comwt.f'
      INCLUDE 'commdb.f'
      INCLUDE 'commdbc.f'
      INCLUDE 'comkey.f'
C
c
      character*15000 ychar
      character*23 ytime
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*14 CTIME
      character*4 cstream(40)
      character*78 cstr
c
      data cstr/'                   pilot decoding'/
C
      DATA NSKIP/0/
C     ------------------------------------------------------------------
C*          1.   RESET ERROR INDICATOR, WORD AND BIT POINTERS.
C                ---------------------------------------------
 100  CONTINUE
C
      IERR=0
c  nwpt here is not used - it is reset by sec0.f
      NWPT=2
      NBPT=0
      NSUB=0
      ISTREAM=msys
C
C
c                write last time stamp processed.
c
crr      if(ostream) then
         call asctim(ytime)
         WRITE(17,9906,IOSTAT=IOS,ERR=410)
     1      CSTR,CSTREAM(msys),CTSTAMP,YTIME
CRR         WRITE(6,9906,IOSTAT=IOS,ERR=410)
CRR  1      CSTR,CSTREAM(msys),CTSTAMP,YTIME
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
      ostream=.true.
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
      SUBROUTINE ERRSTA (IPART,IMARK,IFIRST,NUMBER)
C
C**** *ERRSTA*
C
C
C     PURPOSE.
C     --------
C
C         COUNTS THE NUMBER OF ERRORS IN THE REPORT
C         COUNTS THE NUMBER OF ERRORS IN THE DECODING JOB
C         MARKS THE ERROR BIT TO KDEC
C         ADDS ?-MARK TO KCHAR AT THE ERRONEUS GROUP
C
C         INPUT     : IPART    - INDICATOR OF NOER
C                                (IN TEMPS  3 = A, 4 = B, 5 = C, 6 = D)
C                     IMARK    = 1 IF ? IS TO BE ADDED TO ERRONEUS GROUP
C                     IFIRST   = O IF FIRST DECODING ATTEMPT OF THE REPORT
C                              = 1 OTHERWISE
C                     NUMBER   - NUMBER OF ERRORS IN THE REPORT SO FAR
C
C         OUTPUT    : NUMBER   - NEW NUMBER OF ERRORS
C                     NOER     - NUMBER OF ERRONEUS REPORT (INCREASED BY 1
C                                IF THE FIRST ERROR IN THE REPORT)
C                     KDEC(20) - ERROR BITS
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ERRSTA(IPART,IMARK,IFIRST,NUMBER)*
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
C          A.HOLOPAINEN  NOV.83
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
C
C     ------------------------------------------------------------------
C
C*          1.                                     .
C                -----------------------------------
 100  CONTINUE
C
      IF(NUMBER .GE. 0) NUMBER = NUMBER + 1
      IF ( IFIRST.EQ.0 )
     C   THEN
            IF(NUMBER .EQ. 1)
     C          NUMRERR(IPART)=NUMRERR(IPART) + 1
            NOER(IPART,KERR)=NOER(IPART,KERR) + 1
            KERBIT =IAND(ISHFT(KDEC(20),1-KERR),1)
            IF(KERBIT .EQ. 0) KDEC(20) = KDEC(20) + 2**(KERR-1)
         END IF
C
      IPT = IABS(IPT)
      IF(IMARK .EQ. 1) KCHAR(IPT) = IOR(KCHAR(IPT),128)
      IF(IMARK .EQ. 2)
     C   THEN
            NPT = IPT
            CALL NEXSEP2(NPT,IEQ,*200)
            CALL PREPRT(NPT,IMI,*200)
            KCHAR(NPT) = IOR(KCHAR(NPT),128)
         END IF
C
200   CONTINUE
C
      RETURN
      END
