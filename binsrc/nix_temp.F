        PROGRAM TEMPROC
C
C**** *TEMPROC*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING
C         TEMP DATA.
C
C
C**   INTERFACE.
C     ----------
C
c              msysdsi        YMSYS(1:ndx)//CDATFILE(msys)//'dataXXxx.dat' (usually msysdsi=4, opened in msysinit.f)
c              4              YPPDAT(1:ndx)//'station_amend.dat'
c              7              cstream(msys)//'/'//'temp'//YEAR//YMONTH//YDAY//'MERGE'
c              8              cstream(msys)//'/'//'temp'//YEAR//YMONTH//YDAY//'MERGE.index' (opened in direct2indexed.f)
c              11             cstream(msys)//'/'//'temp_error_file.dat'
c              17             cfstamp(msys)//'.timestamp.dat' (opened in msysinit.f)
c              55             cstream(msys)//'/'//'unknown_temp_station.dat'
c              65             YPPDAT(1:ndx)//'biascor1.tab'   (opened in rcbfiles.f, rbcfiles called from qctemp)
c              66             YPPDAT(1:ndx)//'biascor2.tab'   (opened in rcbfiles.f, rbcfiles called from qctemp)
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
C        *CALL* *MSYSINIT( IERR )*
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
c
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
cpsjun03      if(narg.lt.1.or.ctype.eq.'?') then
      if(narg.lt.1) then
             print*,''
             print*,' Usage: temp dir_file cmsys timestamp <cmsystop>'
             print*,'                                      (optional)'
             print*,' where  dir_file  - specification of directories'
             print*,'        cmsys     - msys file number '
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
CTEST      IF(IERR.NE.0)
CTEST     1 PRINT*,' TEMPROC - after getnext, ierr ',IERR
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
CTEST PRINT*,' TEMPROC - kint(1-4)',(KINT(IR),IR=1,4)
      IF(IERR.NE.0)
     1 PRINT*,' TEMPPROC - after PROCRFB, IERR= ',IERR
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
     1 PRINT*,' TEMPROC - after PROCHDR, KERR= ',KERR
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
     1 PRINT*,' TEMPROC - after PROCTXT, KERR= ',KERR
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
     1 PRINT*,' TEMPROC - after PROCT1U, IERR= ',IERR
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
C
      ntesttot=0
      do 1010 i=1,200
 1010    ntesttot=ntesttot+ntest(i)   
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
      IF ( N2.EQ.1 ) CALL SAVBULL(IERR)          ! N2=1 <=> KERR=2
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
C         *CALL* *BULLUA(IERR)*
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
C*              1.1  BULLETINS WITH 'TT' = 'UA'.  AIRCRAFT REPORTS.
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
C*              1.5  BULLETINS WITH 'TT' = 'UE'.  TEMP PART D.
C                    ----------------------------
  150 CONTINUE
      CALL BULLUE( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              1.6  BULLETINS WITH 'TT' = 'UF'.  TEMP PARTS C AND D.
C                    ---------------------------
  160 CONTINUE
      CALL BULLUF( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              1.7  BULLETINS WITH 'TT' = 'UG'.
C                    ----------------------------
  170 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.8  BULLETINS WITH 'TT' = 'UH'.
C                    ---------------------------
  180 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              1.9  BULLETINS WITH 'TT' = 'UI'.
C
  190 CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.0  BULLETINS WITH 'TT' = 'UJ'.
C                    ---------------------------
 200  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.1  BULLETINS WITH 'TT' = 'UK'.   TEMP PART B.
C                    ---------------------------
 210  CONTINUE
      CALL BULLUK( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.2  BULLETINS WITH 'TT' = 'UL'.   TEMP PART C.
C                    ---------------------------
 220  CONTINUE
      CALL BULLUL( IERR )
      RETURN
C     -----------------------------------------------------------------
C*              2.3  BULLETINS WITH 'TT' = 'UM'.   TEMP PARTS A AND B.
C                    ---------------------------
 230  CONTINUE
      CALL BULLUM( IERR )
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
C*              2.6  BULLETINS WITH 'TT' = 'UP'.
C                    ---------------------------
 260  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.7  BULLETINS WITH 'TT' = 'UQ'.
C                    ---------------------------
 270  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.8  BULLETINS WITH 'TT' = 'UR'.
C                    ---------------------------
 280  CONTINUE
      RETURN
C     -----------------------------------------------------------------
C*              2.9  BULLETINS WITH 'TT' = 'US'.  TEMP PART A.
C                    ---------------------------
 290  CONTINUE
      CALL BULLUS( IERR )
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
C*              3.5  BULLETINS WITH 'TT' = 'UY'.
C
 350  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.6  BULLETINS WITH 'TT' = 'UZ'.  TEMP DROP.
C
 360  CONTINUE
      CALL BULLUZ(IERR)
      RETURN
C
      END
      SUBROUTINE BULLUE ( IERR )
C
C**** *BULLUE*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF INDIVIDUAL
C         REPORTS FROM BULLETINS OF TEMP REPORTS.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLUE( IERR )*
C
C         INPUT    : BULLETIN IN CCITT 5 CHARACTER , 1 CHARACTER PER
C                    WORD IN 'KCHAR'.
C
C                    POINTERS TO BEGINNING AND END OF 'STARTING LINE',
C                    'ABBREVIATED HEADING','MIMIMJMJ LINE' AND END OF
C                    BULLETIN.
C
C                    IERR NOT USED.
C
C         OUTPUT   : DECODED REPORTS WRITTEN TO FILE AND ERROR REPORTS
C                    TO ERROR FILE.
C
C                    IERR = 1 , IF ANY FILE HANDLING ERROR.
C
C     METHOD.
C     -------
C
C         THIS ROUTINE HAS MULTIPLE ENTRY POINTS , ONE FOR
C         EACH OF THE POSSIBLE 'TT' CODES USED IN TEMP
C         BULLETIN HEADINGS . THIS ROUTINE CALLS THE APPROPRIATE
C         DECODING ROUTINES FOR EACH PART OF THE REPORTS BY
C         EXAMINING EACH 'MIMIMJMJ' GROUP.
C
C         THIS APPROACH HAS BEEN ADOPTED BECAUSE OF THE
C         FREQUENCY WITH WHICH INCORRECT BULLETINS ARE
C         RECEIVED (E.G. ALL PARTS OF A TEMP IN 1 BULLETIN)
C         AND MAINTAINS THE SUBROUTINE NAMING CONVENTIONS
C         USED.
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
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      ENTRY BULLUF ( IERR )
      ENTRY BULLUK ( IERR )
      ENTRY BULLUL ( IERR )
      ENTRY BULLUM ( IERR )
      ENTRY BULLUS ( IERR )
      ENTRY BULLUZ ( IERR )
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'printre.f'
C
C
CRR   DATA IBLOCKS/10*-1/
CRR   DATA IDONEA /200*-1/
CRR   DATA IDONEB /200*-1/
CRR   DATA IDONEC /200*-1/
CRR   DATA IDONED /200*-1/
CRR   DATA IPRLIM /50/
C
CRR   DATA ISTASA/
CRR  C   100*-1/
CRR   DATA ISTASB/
CRR  C   100*-1/
CRR   DATA ISTASC/
CRR  C   100*-1/
CRR   DATA ISTASD/
CRR  C   100*-1/
CRR   DATA IERSHIP / 0 /
CRR   DATA IERALLA /0/
CRR   DATA IERALLB /0/
CRR   DATA IERALLC /0/
CRR   DATA IERALLD /0/
C
CTEST
clil      DATA IBLOCKS/10*1/
clil      DATA IDONEA /200*1/
clil      DATA IDONEB /200*1/
clil      DATA IDONEC /200*1/
clil      DATA IDONED /200*1/
clil      DATA IPRLIM /50/
clilC
clil      DATA ISTASA/
clil     C   100*1/
clil      DATA ISTASB/
clil     C   100*1/
clil      DATA ISTASC/
clil     C   100*1/
clil      DATA ISTASD/
clil     C   100*1/
clil      DATA IERSHIP / 1 /
clil      DATA IERALLA /1/
clil      DATA IERALLB /1/
clil      DATA IERALLC /1/
clil      DATA IERALLD /1/
CTEST
C
Clil/5/27/1997/
      DATA IBLOCKS/10*-1/
      DATA IDONEA /200*-1/
      DATA IDONEB /200*-1/
      DATA IDONEC /200*-1/
      DATA IDONED /200*-1/
      DATA IPRLIM /50/
C
      DATA ISTASA/
     C   100*-1/
      DATA ISTASB/
     C   100*-1/
      DATA ISTASC/
     C   100*-1/
      DATA ISTASD/
     C   100*-1/
C
      DATA IERSHIP /1/
      DATA IERALLA /1/
      DATA IERALLB /1/
      DATA IERALLC /1/
      DATA IERALLD /1/
cpsnov02 Set this explicitely
      DATA IERALL /0/
cps
C
C     ------------------------------------------------------------------
C*          1.   SET FLAGS,INDICATORS AND WORKING POINTERS.
C                -------------------------------------------
 100  CONTINUE
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
C-----PRINT INPUT BULLETINS OF TEMP REPORTS
C      CALL PRTBULL ( 1,IGS )
C-----PRINT INPUT BULLETINS OF TEMP REPORTS
C
C     SET FLAGS AND WORKING POINTER.
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
C     DATA BASE REPORT TYPE  : 35 = TEMP
C                              36 = TEMP SHIP
C                             135 = TEMP DROP
C                             137 = TEMP MOBILE
C     PART INDICATOR         : 1 = PART A
C                              2 = PART B
C                              3 = PART C
C                              4 = PART D
C
  101 CONTINUE
C
      K = IPT + 20
C
C*          1.1 CHECK IF LAND STATION ( 'MIMI' = TT ).
C               --------------------------------------
 110  CONTINUE
C
      DO 111 I = IPT,K
         IF ( KCHAR(I).EQ.84.AND.KCHAR(I+1).EQ.84)
     C        THEN
                  KDEC(4) = 35
                  IPT = I
                  GO TO 200
              END IF
  111 CONTINUE
C
C
C*          1.2 CHECK IF SHIP REPORT ( 'MIMI' = UU ), ALSO CHECK IF THIS
C               IS SHIP WITH NAME STARTING UU (LIKE UUQR), BY CHECKING
C               IF THERE IS AN OTHER GROUP UU--.
C               ALSO 'MIMI' = UM IS SHIP, WHERE PARTS A & B
C               ARE IN SAME BULLETIN
 120  CONTINUE
C
      DO 121 II = IPT,K
         IF( KCHAR(II) .EQ. 85 .AND. (KCHAR(II+1) .EQ. 85 .OR.
     C                                KCHAR(II+1) .EQ. 77) )
     C        THEN
                 if(kchar(ii+2).eq.65.and.kchar(ii+3).eq.65) go to 311
                 if(kchar(ii+2).eq.66.and.kchar(ii+3).eq.66) go to 311
                 if(kchar(ii+2).eq.67.and.kchar(ii+3).eq.67) go to 311
                 if(kchar(ii+2).eq.68.and.kchar(ii+3).eq.68) go to 311
C
                  DO 122 J=II+2,K
                  IF ( KCHAR(J).EQ.85.AND.KCHAR(J+1).EQ.85)
     C               THEN
c                       
                 if(kchar( j+2).eq.65.and.kchar( j+3).eq.65) go to 312
                 if(kchar( j+2).eq.66.and.kchar( j+3).eq.66) go to 312
                 if(kchar( j+2).eq.67.and.kchar( j+3).eq.67) go to 312
                 if(kchar( j+2).eq.68.and.kchar( j+3).eq.68) go to 312
                     END IF
c
 122             CONTINUE
c
                 go to 121
c
 312             continue
c
                 KDEC(4) = 36
                 IPT = J
                 I = J
                 GO TO 200
C
 311              continue
c
                  KDEC(4) = 36
                  IPT = II
                  I = II
                  GO TO 200
              END IF
121   CONTINUE
C
C*          1.3  CHECK IF DROP SONDE ( 'MIMI' = XX ).
C                ------------------------------------
 130  CONTINUE
C
      DO 131 I = IPT,K
         IF ( KCHAR(I).EQ.88.AND.KCHAR(I+1).EQ.88)
     C        THEN
                  KDEC(4) = 135
                  IPT=I
                  GO TO 200
              END IF
  131 CONTINUE
C
C*          1.4  CHECK IF TEMP MOBILE ( 'MIMI' = II ).
C                ------------------------------------
 140  CONTINUE
C
      DO 141 I = IPT,K
         IF ( KCHAR(I).EQ.73.AND.KCHAR(I+1).EQ.73) THEN
              KDEC(4) = 137
              IPT=I
              GO TO 200
         END IF
  141 CONTINUE
C
C*          1.5 INVALID 'MIMI'.WRITE REPORT TO ERROR FILE.
C               -------------------------------------------
 150  CONTINUE
C
      KCHAR(IPT) = IOR(KCHAR(IPT),128)
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
      IF(IEQ .GT. IGS) RETURN
C
C     SEE IF REPORT TOO SHORT
C
      ILN = IEQ - IPT
      IF(ILN .LT. 21) GO TO 700
C
      KDEC(4) = 35
      CALL SAVREP ( IHEAD,IERR )
      GO TO 700
C
C     -----------------------------------------------------------------
C*          2. IDENTIFY PART OF REPORT.
C              ------------------------
 200  CONTINUE
C
C
      KDEC(23) = 0
      IF (KCHAR(I+2).EQ.65.OR.KCHAR(I+3).EQ.65) KDEC(23) = 1
      IF (KCHAR(I+2).EQ.66.OR.KCHAR(I+3).EQ.66) KDEC(23) = 2
      IF (KCHAR(I+2).EQ.67.OR.KCHAR(I+3).EQ.67) KDEC(23) = 3
      IF (KCHAR(I+2).EQ.68.OR.KCHAR(I+3).EQ.68) KDEC(23) = 4
C
      K = KDEC(23) + 1
C
C
C*          2.1  LOCATE END OF REPORT AND INCREMENT COUNT.
C                -----------------------------------------
 210  CONTINUE
C
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
      IF(IEQ .GT. IGS) THEN
         print*,'End of report missing'
         igs=ieq
      END IF
C
C     CHECK IF = - SIGN WAS MISSING BY TRYING TO FIND THE NEXT
C     TTAA, TTBB, TTCC OR TTDD -GROUP.
C
      CALL NEXTMI(IPT,IEQ,IIPT)
      if(iipt.ne.9999) then
         IF(IIPT .LT. IEQ) IEQ = IIPT
      end if
C
C
C     SEE IF REPORT TOO SHORT
C
      ILN = IEQ - IPT
      IF(ILN .LT. 21) GO TO 700
C
      NUMREP(K+1) = NUMREP(K+1) + 1
C
C
      GO TO (140,300,400,500,600) K
C
C     ------------------------------------------------------------------
C*          3. DECODE TEMP REPORT PART A.
C              --------------------------
 300  CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL USINT(IHEAD,IERR)
C
CTEST
      IF(KERR.NE.0)
     1 PRINT*,' BULLUE - after call usint, kerr ',KERR
CTEST
      IF(KERR .NE. 0) GO TO 700
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL USDEC(IHEAD,IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' BULLUE - after call usdec, ierr ',IERR
CTEST
      GO TO 700
C
C     ------------------------------------------------------------------
C*          4. DECODE TEMP REPORT PART B.
C              --------------------------
 400  CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL UKINT ( IHEAD,IERR )
C
CTEST
      IF(KERR.NE.0)
     1 PRINT*,' BULLUE - after call ukint, kerr ',KERR
CTEST
      IF ( KERR.NE.0 ) GO TO 700
C
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL UKDEC ( IHEAD,IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' BULLUE - after call ukdec, ierr ',IERR
CTEST
      GO TO 700
C
C     -----------------------------------------------------------------
C*          5. DECODE TEMP REPORT PART C.
C              --------------------------
 500  CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL ULINT ( IHEAD,IERR )
C
CTEST
      IF(KERR.NE.0)
     1 PRINT*,' BULLUE - after call ulint, kerr ',KERR
CTEST
      IF ( KERR.NE.0 ) GO TO 700
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL ULDEC ( IHEAD,IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' BULLUE - after call uldec, ierr ',IERR
CTEST
      GO TO 700
C
C     -----------------------------------------------------------------
C*          6. DECODE TEMP REPORT PART D.
C              ---------------------------
 600  CONTINUE
C
C     CONVERT TO INTERMEDIATE FORMAT.
C
      CALL UEINT ( IHEAD,IERR )
C
CTEST
      IF(KERR.NE.0)
     1 PRINT*,' BULLUE - after call ueint, kerr ',KERR
CTEST
      IF ( KERR.NE.0 ) GO TO 700
C
C     CONVERT INTERMEDIATE FORMAT TO DECODED FORMAT AND WRITE
C     TO FILE.
C
      CALL UEDEC ( IHEAD,IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' BULLUE - after call uedec, ierr ',IERR
CTEST
      GO TO 700
C
C
C     ----------------------------------------------------------------
C*          7. LOCATE START OF NEXT REPORT.
C              ----------------------------
 700  CONTINUE
C
      IPT = IEQ + 1
      CALL NEXTPRT ( IPT,IGS )
C
C
C
C*          7.1 CHECK FOR END OF BULLETIN.
C               ---------------------------
 710  CONTINUE
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
C-----        PRINT TEMP DATA WRITTEN TO ERROR FILE
C-----
c              CALL PRTBULL ( 1,IGS )
C-----
C-----        PRINT TEMP DATA WRITTEN TO ERROR FILE
C
              RETURN
          END IF
C
C
C*          7.2 RESET REPORTS DATA BASE TYPE AND REPORT PART INDICATORS.
C               --------------------------------------------------------
 720  CONTINUE
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
      SUBROUTINE UEDEC ( IHEAD,IERR )
C
C**** *UEDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE TEMP REPORT PART D FROM INTERMEDIATE FORMAT
C         TO FULL DECODED FORMAT.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UEDEC(IHEAD,IERR)*
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
C
C     ------------------------------------------------------------------
C*          1.   DECODE TO FULLY DECODED FORMAT.
C                -------------------------------
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
      DO 101 I=25,700
      KDEC(I)=MINDIC
101   CONTINUE
C     -----------------------------------------------------------------
C*          2.  SECTION 1.
C               ----------
 200  CONTINUE
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
C*           2.1 DAY AND TIME.
C                -------------
 210  CONTINUE
C
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C*           2.2 CHECK IF SHIP OR DROP TEMP.
C                ---------------------------
 220  CONTINUE
C
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
C
C        LAT & LONG
C
         CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1   KDEC(5),KDEC(6))
C
         IF(KDEC(5).EQ.MINDIC) RETURN
         IF(KDEC(6).EQ.MINDIC) RETURN   
C
C        ALTIT=0, FOR SEA STATION
C
         IF(KDEC(4) .EQ. 36) KDEC(8)=0
C
      END IF
C
C
C*          2.3 IDENT   STATION/SHIP/AIRCRAFTIDENTIFICATION
C               --------------------------------------------
C
 230  CONTINUE
C
      KDEC(7)=88
C
      IF(KDEC(4).EQ.137) THEN
         IF(KINT(699).NE.MINDIC.AND.KINT(700).NE.MINDIC) THEN
            IF(KINT(700).GE.1.AND.KINT(700).LE.4) KDEC(8)=KINT(699)
            IF(KINT(700).GE.5.AND.KINT(700).LE.8) THEN
               KDEC(8)=NINT(KINT(699)*.3)
            END IF
         END IF
      END IF
C
 233  CONTINUE
C
C
C
C*          2.4 REPORT LENGTH .
C               ---------------
 240  CONTINUE
C
      KDEC(24)=426
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
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LT. 0 .OR. KDEC(1) .GT. 31) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) GO TO 800
C
      IROU = IROU +1
C
C     ----------------------------------------------------------------
C*          3. SECTION 5 (SIGNIFICANT LEVELS WITH RESPECT TO
C              ----------------------------------------------
C              TEMPERATURE AND/OR RELATIVE HUMIDITY).
C              --------------------------------------
 300  CONTINUE
C
      LEVNUM = 0
C
      DO 301 I=1,60
C
C
      IND = 17 + (I-1)*5
      IF(KINT(IND) .EQ. -999999) GO TO 302
      IF(KINT(IND+1) .EQ. MINDIC) GO TO 301
C
      LEVNUM = LEVNUM + 1
      IND2 = 25 + (LEVNUM - 1) * 3
C
C     NINI PIPIPI    PRESSURE AT THE LEVEL
C
      KDEC(IND2) = KINT(IND+1)
C
C           REJECT IF LEVEL LOWER THAN PREVIOUS
C
            IF(LEVNUM .GT. 1)
     C         THEN
                  IF(KDEC(IND2) .GE. KDEC(IND2-3) .AND.
     C               KDEC(IND2-3) .NE. MINDIC)
     C               THEN
                        KDEC(IND2) = MINDIC
                        LEVNUM = LEVNUM - 1
                        GO TO 301
                     END IF
               END IF
C
C     TITI     TEMPERATURE AT THE LEVEL
C
      CALL IC3931(KINT(IND+3),KINT(IND+2),MINDIC,KDEC(IND2+1))
C
C
C
C     TDTD     DEW-POINT TEMPERATURE AT THE LEVEL
C
      CALL IC0777(KINT(IND+4),KDEC(IND2+1),MINDIC,KDEC(IND2+2))
C
C
C
 301  CONTINUE
C
C
 302  CONTINUE
C
C     ----------------------------------------------------------------
C*          4. SECTION 6 (SIGNIFICANT LEVELS WITH RESPECT TO WIND).
C              ----------------------------------------------------
 400  CONTINUE
C
C     CHECK IF THIS SECTION EXISTS
C
      IF(KINT(317) .EQ. MINDIC) GO TO 402
C
      LEVNUM = 0
C
      DO 401 I=1,60
C
            IND = 318 + (I-1)*4
C
C
C           CHECK FOR MISSING DATA
C
            IF(KINT(IND) .EQ. -999999) GO TO 402
            IF(KINT(IND+1) .EQ. MINDIC) GO TO 401
C
C     NINI PIPIPI     PRESSURE AT THE LEVEL
C
C
            LEVNUM = LEVNUM + 1
            IND2 = 206 + (LEVNUM - 1) * 3
            KDEC(IND2) = KINT(IND+1)
C
C           REJECT IF LEVEL LOWER THAN PREVIOUS
C
            IF(LEVNUM .GT. 1)
     C         THEN
                  IF(KDEC(IND2) .GE. KDEC(IND2-3) .AND.
     C               KDEC(IND2-3) .NE. MINDIC)
     C               THEN
                        KDEC(IND2) = MINDIC
                        LEVNUM = LEVNUM - 1
                        GO TO 401
                     END IF
               END IF
C
C
C     DTDT FTFTFT   WINDS AT TROPOPAUSE
C
            CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     1                 KDEC(IND2+1),KDEC(IND2+2))
C
 401  CONTINUE
C
C
 402  CONTINUE
C
C     ----------------------------------------------------------------
C*          5. SECTION 9  (REGIONAL DATA).
C              ---------------------------
 500  CONTINUE
C
C     ----------------------------------------------------------------
C*          6. SECTION 10 (NATIONAL DATA) .              *
C              ----------------------------
 600  CONTINUE
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT (I.E. IF
C     THERE ARE NO DATA AND NO CODING ERRORS)
C
      NIL2 = 1
      DO 601 I=25,KDEC(24)
      IF(KDEC(I) .EQ. MINDIC) GO TO 601
      NIL2 = 0
      GO TO 602
601   CONTINUE
C
602   CONTINUE
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 800
         END IF
C     -----------------------------------------------------------------
C*          7. PRINTING OF ERRORS & REPORTS & KINT & KDEC.
C              -------------------------------------------
 700  CONTINUE
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
      DO 701 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
701   CONTINUE
C
      DO 702 I=1,100
      IF(ISTA .EQ. ISTASD(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
702   CONTINUE
C
C
C*          7.1 PRINT THE ERRONEUS SHIPS WITHIN TIME LIMITS IF REQUIRED.
C               --------------------------------------------------------
 710  CONTINUE
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
      IF(KDEC(2) .LT. ITIMES(1) .AND. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
C
C
      IF(IPRI .GT. 0)
     C   THEN
            DO 711 I=1,IPRLIM
711         CONTINUE
            IPRNUM=IPRNUM+1
            IF(IPRNUM .LE. IPRLIM) IDONED(IPRNUM) = KINT(4)
C
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
C
10000 FORMAT(///)
20000 FORMAT(//2X,' PART D  NUMBER =',I5,/)
30000 FORMAT(2X,' PART D  NUMBER =',I4,1X,2(I10,1X),5X,A5,
     C       2(I10,1X),2(A10,1X))
C
C
C
 800  CONTINUE
C
c      CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
       CALL TEMPEXP(4,IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UEDEC - after tempexp, ierr ',IERR
CTEST
       CALL MRGTEM (IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UEDEC - after mrgtem, ierr ',IERR
CTEST
       CALL QCTEMP (KDEC(4),IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UEDEC - after qctemp, ierr ',IERR
CTEST
       CALL BUFRENC(IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UEDEC - after bufrenc, ierr ',IERR
CTEST
c
 900  CONTINUE
C
      RETURN
      END
      SUBROUTINE UEINT ( IHEAD,IERR ) 
C**** *UEINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT TEMP REPORTS PART D FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UEINT(IHEAD,IERR)*
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
C     METHOD.
C     -------
C
C
C         IF AN ERROR IS FOUND IN THE DATA, THE DECODING
C         IS PROCEEDED UNLESS THE ERROR IS FATAL I.E.
C         LAT/LONG OR TIME/DATE IS INCORRECT.
C         WHEN A NON-FATAL ERROR HAS BEEN DETECTED THE
C         ROUTINE TRIES TO FIND THE BEGINNING OF CORRECT
C         DATA.  IF CORRECT DATA ARE FOUND THE DECODING
C         WILL CONTINUE FROM THAT POINT.
C         THE NUMBER OF NON-FATAL ERRORS IS COUNTED AND
C         IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE
C         DATA AFTER THE FIRST ERROR ARE REMOVED (THIS
C         IS DONE TO ENSURE THAT TOTALLY CORRUPTED DATA
C         ARE NOT USED E.G. WRONG PART OF TEMP).
C                 FOR THIS PURPOSE TWO VARIABLES ARE USED
C                 NUMERR = NUMBER OF NON-FATAL ERRORS
C                 IREMPOS= THE POINTER TO FIRST ERROR IN KINT.
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
C*          1. CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C              --------------------------------------------------------
C              CHARACTER OF REPORT.
C              ---------------------
 100  CONTINUE
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR=0
      IREMPOS = 17
C
C
C
C     CHECK FOR 'NIL' REPORT.
C
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.26 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(6,0,IFIRST,NUMERR)
         END IF
C
C      SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C
      DO 102 I=1,700
         KINT(I) = MINDIC
  102 CONTINUE
C
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP/AIRCRAFT IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 103 I=12,14
         KINT(I) = 88
  103 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C     CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C
      DO 104 I=1,3
         KDEC(I) = MINDIC
  104 CONTINUE
C
      DO 105 I=5,9
         KDEC(I) = MINDIC
  105 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = 0
C
      KDEC(17) = MINDIC
      KDEC(22) = MINDIC
C
      KDEC(23)=4
C     -----------------------------------------------------------------
C*          2. SECTION 1  (IDENTIFICATION AND POSITION, THIS IS
C              -------------------------------------------------
C              DIFFERENT FOR LAND STATIONS AND SHIP/AIRCRAFT REPORTS).
C              -------------------------------------------------------
1000  CONTINUE
C
C               YY GG /
C               -- -- -
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*25000)
                CALL NEXPRT2(IPT,IEQ,*25000)
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
     1             .AND. (KCHAR(IPT) .LE. 56))
     2              CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
C
                JPT = IPT
C
C
C
C
  700 IF ( KDEC(4).EQ.35 )
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
                CALL NEXPRT2( IPT,IEQ,*25000)
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
                             RETURN
                          END IF
                    END IF
C
                IF ( IRET.EQ.1 ) THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        CALL ERRSTA(6,0,IFIRST,NUMERR)
C
c                       PRINT IIIII WITH NO MASTER FILE ENTRY
                        OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
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
cpsokt98 9901                FORMAT (1H ,a8,' TEMP NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' TEMP NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        RETURN
                END IF
C
                IF ( IRET.EQ.2 ) THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2
C
C                       RETURN IF REPORT CAN'T CONTAIN IDENT
C
                        IF( (KPT+5) .GE. IEQ) RETURN
C
                        CALL ERRSTA(6,2,IFIRST,NUMERR)
                        RETURN
                 END IF
C
C
                 IF(IRET .EQ. 3) THEN
C
C                     INVALID OBSERVATION TYPE
C
                      KERR = 3
C
                        IF ( IFIRST.EQ.0 )
     C                     THEN
                              NOER(6,KERR) = NOER(6,KERR) + 1
c                              KERBIT = SHIFT(KDEC(20),1-KERR) .AND. 1
c                              IF(KERBIT .EQ. 0)
c     C                              KDEC(20) = KDEC(20) + 2**(KERR-1)
                           END IF
C
                      RETURN
                 END IF
C
                IF ( IRET.EQ.4 ) THEN
C
C                   STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                   ENTRY , FOR THAT OBSERVATION TYPE. 
C
c                   PRINT IIIII WITH NO MASTER FILE ENTRY
                   OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C                        
                    WRITE (55,9991) ctstamp,KINT(4)
                    CLOSE(55)
                    WRITE (*,9991) ctstamp,KINT(4)
cpsokt98 9991               FORMAT (1H ,a8,' TEMP WRONG TYPE OF STATION  - ',I5)
 9991              FORMAT (1H ,a12,' TEMP WRONG TYPE OF STATION  - ',I5)
c                    CALL PRTBULL(1,IGS)
C-----              PRINT IIIII WITH NO MASTER FILE ENTRY
C
                END IF
C
            ELSE
C
C                TEMP SHIP OR TEMP DROP
C
                 IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137)
     1              THEN
C
                        KINT(4)=0
C
C
C
C              RETAIN POINTER TO STATION IDENTIFIER .
C
               KPT=IMI
C
C
C              CHECK THE CALL SIGN
C
               IBUOY = 0
               K = IMI
               J = K + 4
C
               DO 750 I=K,J
                    IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750          CONTINUE
C
C
C              IF A LETTER GROUP IS FOUND CHECK THAT THIS IS NOT
C              MIMIMJMJ GROUP 'UUDD'. IN THIS CASE THE SHIP CALL
C              SIGN GROUP IS PRESUMED MISSING.
C
               IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85) .AND.
     C            (KCHAR(K+2) .EQ. 68) .AND. (KCHAR(K+3) .EQ. 68))
     C                   THEN
                             IBUOY = 2
                        END IF
C
               IF((KCHAR(K) .EQ. 73) .AND. (KCHAR(K+1) .EQ. 73) .AND.
     C            (KCHAR(K+2) .EQ. 68) .AND. (KCHAR(K+3) .EQ. 68))
     C                   THEN
                             IBUOY = 2
                         END IF
C
               IF((KCHAR(K) .EQ. 88) .AND. (KCHAR(K+1) .EQ. 88) .AND.
     C            (KCHAR(K+2) .EQ. 68) .AND. (KCHAR(K+3) .EQ. 68))
     C                   THEN
                             IBUOY = 2
                         END IF
C
C
               IF ( IBUOY.NE.0 )
     C             THEN
C
C                      SHIP,RIG OR PLATFORM. IDENTIFIER NO. SET TO 0
C
                         ID=0
                       KINT(4) = 0
C
                       IF (IBUOY.EQ.1)
     C                    THEN
C
C                             MOVE POINTER PAST CALL SIGN . THE FIRST
C                             CHARACTER OF THE NEXT GROUP IS
C                             LOCATED AS SOME SHIPS USE THE FULL NAME
C                             ( WHICH CAN CONSIST OF 2 WORDS ) INSTEAD
C                             OF CALL SIGN. SO IF THE NEXT GROUP DOESN'T
C                             START WITH 'UU' (WHICH IS THE BEGINNING
C                             OF MIMIMJMJ GOUP) IT MUST BE PART OF THE
C                             SHIP'S NAME USED INSTEAD OF CALL SIGN.
C
                              IIPT=K
                              CALL NEXSEP2(IIPT,IEQ,*25000)
                              ID = IIPT - KPT
                              CALL NEXTLET (IIPT,IEQ)
C
                              IF(KCHAR(IIPT) .NE. 85 .OR. KCHAR(IIPT+1)
     C                           .NE. 85)
     C                                THEN
                                         CALL NEXSEP2(IIPT,IEQ,*25000)
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
                             CALL NEXSEP2(IIPT,IEQ,*25000)
                             CALL NEXPRT2(IIPT,IEQ,*25000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*25000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*25000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*25000)
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
                        CALL NEXPRT2( IPT,IEQ,*25000)
                        CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                       CHECK VALIDITY OF LALALA
C
                        IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                     THEN
C
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                              KERR = 4
                              CALL ERRSTA(6,1,IFIRST,NUMERR)
                              GO TO 25000
                           END IF
C
C                       QC LOLOLOLO
C                       -- --------
C
                        CALL NEXPRT2( IPT,IEQ,*25000)
                        CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )
C
C                       CHECK VALIDITY OF LOLOLOLO
C
                        IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C                          KINT(7).NE.5.AND.KINT(7).NE.7) IRET=1
                        IF (KINT(8).LT.0.OR.KINT(8).GT.1800)IRET=1
C
                        IF (IRET.NE.0)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                              KERR = 4
                              CALL ERRSTA(6,1,IFIRST,NUMERR)
                              GO TO 25000
                           END IF
C
C                       MMM ULA ULO
C                       --- --- ---
C
                        CALL NEXPRT2(IPT,IEQ,*25000)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                              kdec(5),kdec(6))
C
                        IF(KDEC(5).EQ.MINDIC) RETURN
                        IF(KDEC(6).EQ.MINDIC) RETURN   
C
                        CALL MARDSEN(kdec(5),kdec(6),KINT(9),IRET)
C
                        IF(IRET .NE. 0)
     1                      THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1            
                                 RETURN
                              END IF
C
C
C    --------------            WRONG CODE FOR MARSDEN SQUARE    ------
                               KERR = 4
                               CALL ERRSTA(6,1,IFIRST,NUMERR)
                              GO TO 25000
C
                            END IF
C
C                       hohohohoim ( heigth of mobile platform)
C                       ---------------------------------------
C                         HOHOHOHO - KINT(699)
C                         IM       - KINT(700)
C
                        IF(KDEC(4).EQ.137) THEN
                           CALL NEXTPRT(IPT,IEQ)
                           CALL EXTGRP(IPT,4,1,0,0,0,699,IRET)
                        END IF
C
                    ELSE
C
C                       THIS IS NOT A TEMP REPORT
C
C
C                       CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                        IF(NIL .EQ. 1) THEN
                           KDEC(20)=1 
                           RETURN
                        END IF 
C
                        KERR = 3
                        NOER(6,KERR) = NOER(6,KERR)+1
                              GO TO 25000
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
C
C                  IF LETTER ENCOUNTERED FOR LAND STATION CONVERT
C                  IT TO FIGURE (NOT FOR SHIP'S CALL SIGN)
C
                   IF(KDEC(4) .EQ. 35) CALL LETFIG(KCHAR(J))
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C     'NIL' REPORTS ARE NOT PROSESSED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
C     CHECK VALUES OF YY GG FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4).GE.35 )
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
C
C                      CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                       IF(NIL .EQ. 1 ) THEN
                          KDEC(20)=1
                          RETURN
                       END IF
C
                       KERR = 3
                       CALL ERRSTA(6,1,IFIRST,NUMERR)
                       GO TO 25000
C
                   END IF
           END IF
C      ----------------------------------------------------------------
C*          3. SECTION 5 (SIGNIFICANT LEVELS WITH RESPECT TO
C              -----------------------------------------------
C              TEMPERATURE AND/OR RELATIVE HUMIDITY).
C              --------------------------------------
 5000 CONTINUE
C
C
C     GET THE POINTER TO THE FIRST LEVEL
C
      CALL NEXPRT2(IPT,IEQ,*25000)
      LPT = IPT
C
C
C
      LEVNUM=0
      LEVPRE=99999
      ITRY = 0
C
5100  CONTINUE
C
C     TRY TO FIND WIND DATA IF ALREADY FIVE ERRORS DETECTED
C
      IF(NUMERR .GE. 5) GO TO 5900
C
C
C     NINI PIPIPI
C     ---- ------
C
      CALL NEXTNI(5,IFIRST,LEVNUM,LEVPRE,IREMPOS,NUMERR,
     1           *6000,*9000,*9000,*10000,*25000,*5900,*5200,*25000)
C
C
5200  CONTINUE
C
      LEVNUM=LEVNUM+1
      IPOS=17+(LEVNUM-1)*5
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
      IF(IRET .NE. 0)
     1   THEN
            KERR = 5
            CALL ERRSTA(6,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            CALL NEXSEP2(IPT,IEQ,*25000)
            CALL NEXPRT2(IPT,IEQ,*25000)
            LEVNUM = LEVNUM - 1
            GO TO 5100
         END IF
C
C
      IF(KINT(IPOS+1) .EQ. MINDIC)
     1   THEN
            KERR = 5
            CALL ERRSTA(6,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
            KINT(IPOS) = MINDIC
            CALL NEXPRT2(IPT,IEQ,*25000)
            CALL NEXSEP2(IPT,IEQ,*25000)
            LEVNUM = LEVNUM - 1
            GO TO 5100
         END IF
C
C
5600  CONTINUE
C
C
C     TITI TAI DIDI
C     ---- --- ----
C
      CALL NEXPRT2(IPT,IEQ,*5800)
      CALL EXTGRP(IPT,2,1,2,0,0,IPOS+2,IRET)
C
      IF(IRET .NE. 0)
     1   THEN
            KERR = 6
            CALL ERRSTA(6,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            KINT(IPOS) = MINDIC
            KINT(IPOS+1) = MINDIC
            CALL NEXSEP2(IPT,IEQ,*25000)
            GO TO 5100
         END IF
C
C
C     REJECT IF THE TEMPERATURE IS POSITIVE
C
      IF(IRET .EQ. 0)
     C   THEN
            ISIGN = 2*(KINT(IPOS+3)/2)
            IF(ISIGN .EQ. KINT(IPOS+3))
     C         THEN
                  KERR = 6
                  CALL ERRSTA(6,1,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
                     DO 5700 I = IPOS,IPOS+4
                     KINT(I) = MINDIC
5700                 CONTINUE
                  GO TO 5100
               END IF
         END IF
C
C
      LEVPRE = KINT(IPOS+1)
      KINT(IPOS+5) = -999999
C
C
      GO TO 5100
C
5800  CONTINUE
      KINT(IPOS  ) = MINDIC
      KINT(IPOS+1) = MINDIC
      GO TO 25000
C
C
 5900 CONTINUE
C
C     SEARCH FOR THE NEXT SECTION
C     ====== === === ==== =======
C
C     DON'T CONTINUE IF ALREADY SIX ERRORS DETECTED
C
      IF(NUMERR .GT. 6) GO TO 25000
      CALL NEXPRT2(IPT,IEQ,*25000)
      CALL EXTVAL(IPT,5,IVAL)
C
cpssep06
      if(ival .eq. 21212) go to 6000
C
cpsjan01 Section 7 is now obligatory also in part D, but we
cps      decode this section in part B, so skip it (and the
cps      rest of the report)
      IF(IVAL .EQ. 31313) GO TO 25000
cps
      IF((IVAL .EQ. 51515) .OR. (IVAL .EQ. 52525) .OR.
     C   (IVAL .EQ. 53535) .OR. (IVAL .EQ. 54545) .OR.
     C   (IVAL .EQ. 55555) .OR. (IVAL .EQ. 56565) .OR.
     C   (IVAL .EQ. 57575) .OR. (IVAL .EQ. 58585) .OR.
     C   (IVAL .EQ. 59595)) GO TO 9000
C
      IF((IVAL .EQ. 61616) .OR. (IVAL .EQ. 62626) .OR.
     C   (IVAL .EQ. 63636) .OR. (IVAL .EQ. 64646) .OR.
     C   (IVAL .EQ. 65656) .OR. (IVAL .EQ. 66666) .OR.
     C   (IVAL .EQ. 67676) .OR. (IVAL .EQ. 68686) .OR.
     C   (IVAL .EQ. 69696)) GO TO 10000
C
      CALL NEXSEP2(IPT,IEQ,*25000)
      GO TO 5900
C
C      ----------------------------------------------------------------
C*          4. SECTION 6 (SIGNIFICANT LEVELS WITH RESPECT TO WIND).
C              ----------------------------------------------------
 6000 CONTINUE
C
C     21212
C     -----
C     CHECK IF VALUE 21212 HAS ALREADY BEEN INSERTED ( MAY
C     HAPPEN IN CASE OF CORRUPTED '21212'-GROUP).
C
      IF(KINT(317) .EQ. 21212)
     C   THEN
            CALL NEXSEP2(IPT,IEQ,*25000)
            CALL NEXPRT2(IPT,IEQ,*25000)
            GO TO 6050
         END IF
C
      CALL EXTGRP(IPT,5,0,0,0,0,317,IRET)
C
      IF(IRET .NE. 0)
     C   THEN
            KERR = 10
            CALL ERRSTA(6,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = 317
            CALL NEXSEP2(IPT,IEQ,*25000)
         END IF
C
 6050 CONTINUE
C
      LEVPRE=99999
      LEVNUM=0
      ITRY = 0
C
 6100 CONTINUE
C
C     DON'T CONTINUE IF ALREADY SIX ERRORS DETECTED
C
      IF(NUMERR .GT. 6) GO TO 25000
C
C     NINI PIPIPI
C     ---- ------
      CALL NEXTNI(4,IFIRST,LEVNUM,LEVPRE,IREMPOS,NUMERR,
     1           *6000,*9000,*9000,*10000,*25000,*5900,*6150,*25000)
C
 6150 CONTINUE
C
      LEVNUM=LEVNUM+1
      IPOS = 318 + (LEVNUM-1) * 4
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
      IF(IRET .NE. 0)
     1   THEN
            KERR = 5
            CALL ERRSTA(6,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            CALL NEXSEP2(IPT,IEQ,*25000)
            CALL NEXPRT2(IPT,IEQ,*25000)
            LEVNUM = LEVNUM - 1
            GO TO 6100
         END IF
C
      IF(KINT(IPOS+1) .EQ. MINDIC)
     1   THEN
            KERR = 5
            CALL ERRSTA(6,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
            KINT(IPOS) = MINDIC
            CALL NEXPRT2(IPT,IEQ,*25000)
            CALL NEXSEP2(IPT,IEQ,*25000)
            LEVNUM = LEVNUM - 1
            GO TO 6100
         END IF
C
 6400 CONTINUE
C
C     DIDI FIFIFI
C     ---- ------
      CALL NEXPRT2(IPT,IEQ,*6200)
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
      IF ( (IRET .NE. 0)  .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C      KINT(IPOS+2) .NE. MINDIC) )
     C         THEN
                  KERR = 7
                  CALL ERRSTA(6,1,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                  DO 6500 I = IPOS,IPOS+3
                  KINT(I) = MINDIC
 6500             CONTINUE
                  CALL NEXSEP2(IPT,IEQ,*25000)
               END IF
C
      LEVPRE = KINT(IPOS+1)
      KINT(IPOS+4) = -999999
C
      GO TO 6100
C
 6200 CONTINUE
      KINT(IPOS  ) = MINDIC
      KINT(IPOS+1) = MINDIC
      GO TO 25000
C     ---------------------------------------------------------------
C*          5. SECTION 9 (REGIONAL GROUPS).
C              ----------------------------
 9000 CONTINUE
C
C     NO REGIONAL DATA FOR PART D
C     ---------------------------------------------------------------
C*          6. SECTION 10 (NATIONAL GROUPS).
C              -----------------------------
10000 CONTINUE
C
C
C     NO NATIONAL DATA FOR PART D
C
C
25000 CONTINUE
C
C
C
C
C     ERROR CHECKING AND HANDLING
C
C     RETURN IF NO ERRORS IN REPORT
C
C
      IF(KERR .EQ. 0) RETURN
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
                             IPT=KEEP
                             CALL FIXUS
                             IFIRST=1
                             NUMRERR(6) = NUMRERR(6) + 1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
                             DO 25500 I=KEEP,IGS
                             KCHAR(I) = IAND(KCHAR(I),127)
25500                        CONTINUE
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
                                   DO 26000 I = IREMPOS,700
                                   KINT(I) = MINDIC
26000                              CONTINUE
                                END IF
C
                             IF(KERR .GT. 4) KERR=0
                             RETURN
                          END IF
C
29000 CONTINUE
C
      RETURN
      END
      SUBROUTINE UKDEC ( IHEAD,IERR )
C
C**** *UKDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE TEMP REPORT PART B FROM INTERMEDIATE FORMAT
C         TO FULL DECODED FORMAT.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UKDEC(IHEAD,IERR)*
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
C     ------------------------------------------------------------------
C*          1.   SET FLAGS AND INDICATORS.
C                -------------------------
 100  CONTINUE
C
C      CALL PRTKINT(KINT,1,700,MINDIC)
C
C     DEFINE 'NIL' INDICATOR
C
      NIL = IAND(KDEC(20),1)
C
      IREGPRI = 0
C
C     CLEAR ERROR INDICATOR
C
C     SET DECODED REPORT TO MISSING DATA VALUE
C
C
      DO 101 I=25,700
      KDEC(I)=MINDIC
 101  CONTINUE
C     ------------------------------------------------------------------
C*          2. SECTION 1.
C              ----------
 200  CONTINUE
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
C*          2.1  DAY AND TIME.
C                -------------
 210  CONTINUE
C
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C
C
C*          2.2  CHECK IF SHIP OR DROP TEMP.
C                ---------------------------
 220  CONTINUE
C
      IF(KDEC(4) .EQ. 36 .OR. KDEC(4) .EQ. 135.OR.KDEC(4).EQ.137)
     1           THEN
C
C                   LAT & LONG
C
                    CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1                          KDEC(5),KDEC(6))
C
                    IF(KDEC(5).EQ.MINDIC) RETURN
                    IF(KDEC(6).EQ.MINDIC) RETURN   
C
C                   ALTIT=0, FOR SEA STATION
C
                    IF(KDEC(4) .EQ. 36) KDEC(8)=0
C
                 END IF
C
C
C*          2.3 IDENT   STATION/SHIP/AIRCRAFTIDENTIFICATION .
C               ---------------------------------------------
 230  CONTINUE
C
      KDEC(7)=88
C
      IF(KDEC(4).EQ.137) THEN
         IF(KINT(699).NE.MINDIC.AND.KINT(700).NE.MINDIC) THEN
            IF(KINT(700).GE.1.AND.KINT(700).LE.4) KDEC(8)=KINT(699)
            IF(KINT(700).GE.5.AND.KINT(700).LE.8) THEN
                KDEC(8)=NINT(KINT(699)*.3)
            END IF
         END IF
      END IF
C
 233  CONTINUE
C
C
C*          2.4 REPORT LENGTH  .
C               ----------------
 240  CONTINUE
C
      KDEC(24)=426
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
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LT. 0 .OR. KDEC(1) .GT. 31) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) GO TO 99800
C
      IROU = IROU + 1
C     ---------------------------------------------------------------
C*          3. SECTION 5 (SIGNIFICANT LEVELS WITH RESPECT TO
C              ----------------------------------------------
C              TEMPERATURE AND/OR RELATIVE HUMIDITY).
C              --------------------------------------
 300  CONTINUE
C
C
      LEVNUM=0
C
      DO 301 I=1,60
C
      IND = 17 + (I-1)*5
      IF(KINT(IND) .EQ. -999999) GO TO 302
      IF(KINT(IND+1) .EQ. MINDIC) GO TO 301
C
      LEVNUM=LEVNUM+1
      IND2 = 25 + (LEVNUM-1)*3
C
C
C     NINI PIPIPI    PRESSURE AT THE LEVEL
C
      KDEC(IND2) = KINT(IND+1)*10
C
C     CHECK IF THOUSAND DIGIT OMITTED
C
            IF(KDEC(IND2) .LE. 2000)
     C         THEN
                  IF(LEVNUM .EQ. 1) KDEC(IND2) = KDEC(IND2) + 10000
C
cpsjan01                  IF(LEVNUM .GT. 1 .AND. LEVNUM .LE. 4)
cps Changed strategy: if level is among the 7 first ones and pressure < 50hPa
cps then pressure is almost certain to be 1000hPa higher, so add 1000hPa.
cps In the old code (commented out) we risked to loose all the rest of the temp
cps if one level (among the first 4, which we because of changes other places
cps in the code must increase to 7) was lower than the previous but still higher
cps than 1000hPa (i.e. kdec(ind2) .le. 2000 originally - all this is admittedly 
cps quite a bit messy!)
c     C               THEN
cC
cC                   CHECK IF THE PREVIOUS LEVEL IS > 1000MB,
cC                   AND IF IT WAS LOWER THAN THIS.
cC
c                    IF(KDEC(IND2-3) .GT. 10000)
c     C                 THEN
cC
cC                         CHECK IF PREVIOUS LEVEL IS HIGHER THAN THIS
cC
c                          IF( (KDEC(IND2-3) - 10000) .GT. KDEC(IND2))
c     C                         KDEC(IND2) = KDEC(IND2) + 10000
c                       END IF
c                     END IF
                  IF(LEVNUM .GT. 1 .AND. LEVNUM .LE. 7 
     C                 .and. KDEC(IND2) .LE. 500) then
                     KDEC(IND2) = KDEC(IND2) + 10000
                  endif
cps end of changes jan01
               END IF
C
C           REJECT IF LEVEL LOWER THAN PREVIOUS
C
            IF(LEVNUM .GT. 1)
     C         THEN
                  IF(KDEC(IND2) .GE. KDEC(IND2-3) .AND.
     C               KDEC(IND2-3) .NE. MINDIC)
     C         THEN
cpsjan01
                  print*,'WARNING: Removed (temp) level ',levnum,
     C                    ' PPP= ',kdec(ind2)/10 
cps
                  KDEC(IND2) = MINDIC
                  LEVNUM = LEVNUM - 1
                  GO TO 301
               END IF
         END IF
C
C     TITI     TEMPERATURE AT THE LEVEL
C
      CALL IC3931(KINT(IND+3),KINT(IND+2),MINDIC,KDEC(IND2+1))
C
C     TDTD     DEW-POINT TEMPERATURE AT THE LEVEL
C
      CALL IC0777(KINT(IND+4),KDEC(IND2+1),MINDIC,KDEC(IND2+2))
C
C
C
 301  CONTINUE
C
 302  CONTINUE
C     ---------------------------------------------------------------
C*          4. SECTION 6 (SIGNIFICANT LEVELS WITH RESPECT TO WIND).
C              ----------------------------------------------------
 400  CONTINUE
C
C     CHECK IF THIS SECTION EXISTS
C
      IF(KINT(317) .EQ. MINDIC) GO TO 402
C
C
      LEVNUM=0
C
      DO 401 I=1,60
C
            IND = 318 + (I-1)*4
C
C
C
C
C           CHECK FOR MISSING DATA
C
            IF(KINT(IND) .EQ. -999999) GO TO 402
            IF(KINT(IND+1) .EQ. MINDIC) GO TO 401
C
            LEVNUM=LEVNUM+1
            IND2 = 206 + (LEVNUM-1)*3
C
C           NINI PIPIPI     PRESSURE AT THE LEVEL
C
            KDEC(IND2) = KINT(IND+1) * 10
C
C           CHECK IF THOUSAND DIGIT OMITTED
C
            IF(KDEC(IND2) .LE. 2000)
     C         THEN
                  IF(LEVNUM .EQ. 1) KDEC(IND2) = KDEC(IND2) + 10000
C
cpsjan01                  IF(LEVNUM .GT. 1 .AND. LEVNUM .LE. 4)
cps Changed strategy: see section 5 above 
c     C               THEN
cC
cC                   CHECK IF THE PREVIOUS LEVEL IS > 1000MB,
cC                   AND IF IT WAS LOWER THAN THIS.
cC
c                    IF(KDEC(IND2-3) .GT. 10000)
c     C                 THEN
cC
cC                         CHECK IF PREVIOUS LEVEL IS HIGHER THAN THIS
cC
c                          IF( (KDEC(IND2-3) - 10000) .GT. KDEC(IND2))
c     C                         KDEC(IND2) = KDEC(IND2) + 10000
c                       END IF
c                     END IF
                  IF(LEVNUM .GT. 1 .AND. LEVNUM .LE. 7 
     C                 .and. KDEC(IND2) .LE. 500) then
                     KDEC(IND2) = KDEC(IND2) + 10000
                  endif
cps end of changes jan01
               END IF
C
C
C           REJECT IF LEVEL LOWER THAN PREVIOUS
C
            IF(LEVNUM .GT. 1)
     C         THEN
                  IF(KDEC(IND2) .GE. KDEC(IND2-3) .AND.
     C               KDEC(IND2-3) .NE. MINDIC)
     C               THEN
cpsjan01
                  print*,'WARNING: Removed (wind) Level ',levnum,
     C                    ' PPP= ',kdec(ind2)/10 
cps
                        KDEC(IND2) = MINDIC
                        LEVNUM = LEVNUM - 1
                        GO TO 401
                     END IF
               END IF
C
C
C           DTDT FTFTFT
C
            CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     1                 KDEC(IND2+1),KDEC(IND2+2))
C
 401        CONTINUE
C
C
 402  CONTINUE
C
C
C     ---------------------------------------------------------------
C           5. DATA ON SEA SURFACE AND SOUNDING SYSTEM.
C              -----------------------------------------
 500  CONTINUE
C
cpssep06 Commented out this section
c$$$      KDEC(422)=MINDIC
c$$$      KDEC(423)=MINDIC
c$$$      IF(KINT(600).NE.MINDIC) THEN
c$$$C
c$$$C        Sr   OLD CODE
c$$$C
c$$$         IF(KINT(605).NE.MINDIC) KDEC(422)=KINT(605)
c$$$C
c$$$C        RaRa
c$$$C
c$$$         IF(KINT(604).NE.MINDIC) KDEC(423)=KINT(604)
c$$$
c$$$      ELSE
C
C        Sr     NEW CODE (AFTER 1.11.1991)
C
         KDEC(422)=MINDIC
         IF(KINT(611).NE.MINDIC) KDEC(422)=KINT(611)
C
C        RaRa
C
         KDEC(423)=MINDIC
         IF(KINT(612).NE.MINDIC) KDEC(423)=KINT(612)
c$$$      END IF
C
C     ---------------------------------------------------------------
C*          5.1  SECTION 8 (CLOUD DATA).
C                -----------------------
 510  CONTINUE
C
      IF(KINT(558) .NE. MINDIC)
     C   THEN
C
C           NH      TOTAL LOW CLOUDS
C
            IF(KINT(559) .NE. MINDIC) KDEC(386)=KINT(559)
C
C           CL CM CH  TYPE OF MEDIUM AND HIGH CLOUDS
C
            IF(KINT(560) .NE. MINDIC) KDEC(387)=KINT(560)
            IF(KINT(562) .NE. MINDIC) KDEC(389)=KINT(562)
            IF(KINT(563) .NE. MINDIC) KDEC(390)=KINT(563)
C
C           H       HEIGHT OF THE BASE OF THE LOWEST CLOUD
C
            LOWEST = MINDIC
C
c           fix 18.3.94
c
            if(kint(563).ne.mindic) lowest=2
            if(kint(562).ne.mindic) lowest=1
            if(kint(560).ne.mindic) lowest=0
c
c            DO 511 I=563,560,-1
c            IF(I .EQ. 561) GO TO 511
c            IF(KINT(I) .NE. MINDIC) LOWEST = I-560
c511         CONTINUE
C
            CALL IC1600(KINT(561),LOWEST,MINDIC,KDEC(388))
C
         END IF
C
C*          6. SECTION 9 (REGIONAL DATA).
C              --------------------------
 600  CONTINUE
C
C     REGION I
C     ------ -
C
      IF(KDEC(17) .EQ. 1)
     C   THEN
            IF(KINT(564) .EQ. MINDIC) GO TO 700
C
C           LOOP OVER THREE POSSIBLE SPECIAL LEVELS
C           (925HPA, 775HPA AND 600HPA)
C
               LEVNUM = 0
C
               DO 601 I=1,3
               IND = 565 + (I-1)*7
C
               IF(KINT(IND) .EQ. MINDIC) GO TO 601
C
               LEVNUM = LEVNUM + 1
               IND2 = 392 + (LEVNUM-1)*6
C
C              PPP     PRESSURE AT THE LEVEL
C
               IF(I .EQ. 1) KDEC(IND2)=9250
               IF(I .EQ. 2) KDEC(IND2)=7750
               IF(I .EQ. 3) KDEC(IND2)=6000
C
C              HIHIHI    GEOPOTENTIAL AT THE LEVEL
C
C
               LEVIN = KDEC(IND2) / 10
               CALL GEOPOTE(LEVIN,KINT(IND+1),MINDIC,KDEC(IND2+1))
C
C
C              TITI      TEMPERATURE AT THE LEVEL
C
C
               CALL IC3931(KINT(IND+3),KINT(IND+2),MINDIC,KDEC(IND2+2))
C
C
C              TDITDI    DEW POINT TEMPERATURE AT THE LEVEL
C
               CALL IC0777(KINT(IND+4),KDEC(IND2+2),MINDIC,KDEC(IND2+3))
C
C
C              DIDI FIFIFI  WIND AT THE LEVEL
C
               CALL DDFFF(KINT(IND+5),KINT(IND+6),IWW,K,MINDIC,
     C                     KDEC(IND2+4),KDEC(IND2+5))
C
C
 601           CONTINUE
               GO TO 700
         END IF
C
C     REGION II
C     ------ --
C
C     IN REGION II THERE MAY BE REGIONAL DATA IN FORMS
C
C        92H9H9H9  T9T9TA9D9D9  D9D9F9F9F9
C                   OR
C        11PPP DDFFF  22800 DDFFF  33600 DDFFF
C
C
      IF(KDEC(17) .EQ. 2)
     C   THEN
C
C           92H9H9H9  T9T9TA9D9D9  D9D9F9F9F9
C
            IF(KINT(564) .EQ. MINDIC) GO TO 602
            IF(KINT(565) .NE. 92) GO TO 602
            IREGPRI = 1
C
C           PPP       PRESSURE AT 925HPA
C
            KDEC(392) = 9250
C
C           HHH       GEOPOTENTIAL AT 925HPA
C
            LEVIN = KDEC(392) / 10
            CALL GEOPOTE(LEVIN,KINT(566),MINDIC,KDEC(393))
C
C           TTT       TEMPERATURE AT 925HPA
C
            if(kint(567).eq.mindic) then
               inttemp=mindic
               idetemp=mindic
            else
               INTTEMP = KINT(567)/10
               IDETEMP = KINT(567) - 10*INTTEMP
            end if 
C
            CALL IC3931(IDETEMP,INTTEMP,MINDIC,KDEC(394))
C
C           TDTDTD    DEW POINT TEMPERATURE AT 925HPA
C
            CALL IC0777(KINT(568),KDEC(394),MINDIC,KDEC(395))
C
C           DD FFF    WINDS AT 925HPA
C
            CALL DDFFF(KINT(569),KINT(570),IWW,K,MINDIC,KDEC(396),
     C                  KDEC(397))
C
C
 602        CONTINUE
C
C           11PPP DDFFF  22800 DDFFF  33600 DDFFF
C
C
      LEVNUM = 0
C
            DO 603 I=1,3
            IND = 573 + (I-1)*4
C
            IF(KINT(IND+1) .EQ. MINDIC) GO TO 603
C
            IREGPRI = 1
            LEVNUM = LEVNUM + 1
            IND2 = 392 + (LEVNUM-1)*6
C
C           PPP     PRESSURE AT THE LEVEL
C
            KDEC(IND2) = KINT(IND+1) * 10
C
C
C           DIDI FIFIFI  WIND AT THE LEVEL
C
            CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     C                 KDEC(IND2+4),KDEC(IND2+5))
C
C
 603        CONTINUE
C
            GO TO 700
C
         END IF
C
 604  CONTINUE
C
C     REGION VI
C     ------ --
C
      IF(KDEC(17) .EQ. 2 .OR. KDEC(17) .EQ. 6)
     C   THEN
            IND2 = 392
            IF(KINT(564) .EQ. MINDIC) GO TO 605
            IF(KINT(566) .EQ. MINDIC) GO TO 605
C
C           PRESSURE AND WINDS AT 900 OR 1000 METRES
C
            IF(KINT(565) .EQ. 11)
     C         THEN
C
                  KDEC(IND2) = KINT(566)*10
C
                  CALL DDFFF(KINT(567),KINT(568),IWW,K,MINDIC,
     C                       KDEC(IND2+4),KDEC(IND2+5))
                  IND2 = IND2+6
C
                END IF
C
C
C
C           WINDS AT 800 HPA
C
            IF(KINT(569) .EQ. 22800)
     C         THEN
C
                  KDEC(IND2) = 8000
C
                  CALL DDFFF(KINT(570),KINT(571),IWW,K,MINDIC,
     C                       KDEC(IND2+4),KDEC(IND2+5))
                  IND2 = IND2+6
C
                END IF
C
C
C
C           WINDS AT 600 HPA
C
            IF(KINT(572) .EQ. 33600)
     C         THEN
C
                  KDEC(IND2) = 6000
C
                  CALL DDFFF(KINT(573),KINT(574),IWW,K,MINDIC,
     C                       KDEC(IND2+4),KDEC(IND2+5))
                  IND2 = IND2+6
C
                END IF
C
C
 605        CONTINUE
C
C           CHECK IF THERE ARE DATA FOR 925HPA
C
            IF(KINT(575) .EQ. MINDIC) GO TO 700
C
            IF(KINT(576) .EQ. 92)
     C         THEN
C
C
C                 HHH    GEOPOTENTIAL AT 925HPA
C
                  KDEC(IND2) = 9250
                  KDEC(IND2+1) = KINT(577)
C
C                 TTT    TEMPERATURE AT 925HPA
C
                  CALL IC3931(KINT(579),KINT(578),MINDIC,KDEC(IND2+2))
C
C                 TDTDTD    DEW POINT TEMPERATURE AT 925HPA
C
                  CALL IC0777(KINT(580),KDEC(IND2+2),MINDIC,
     C                        KDEC(IND2+3))
C
C                 DIDI FIFIFI     WIND AT 925HPA
C
                  CALL DDFFF(KINT(581),KINT(582),IWW,K,MINDIC,
     C                       KDEC(IND2+4),KDEC(IND2+5))
C
               END IF
C
         END IF
C     ----------------------------------------------------------------
C*          7. SECTION 10   (NATIONAL DATA).
C              -----------------------------
 700  CONTINUE
C
C ===== NOT PROSESSED FOR TIME BEING =====
C
C
C
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT (I.E. IF
C     THERE ARE NO DATA AND NO CODING ERRORS)
C
      NIL2 = 1
      DO 701 I=25,KDEC(24)
      IF(KDEC(I) .EQ. MINDIC) GO TO 701
      NIL2 = 0
      GO TO 702
701   CONTINUE
C
702   CONTINUE
C
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 99800
         END IF
C
C     ---------------------------------------------------------------
C*          8.  PRINTING OF ERRORS & REPORTS & KINT & KDEC.
C               -------------------------------------------
 800  CONTINUE
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
      DO 801 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
801   CONTINUE
C
      DO 802 I=1,100
      IF(ISTA .EQ. ISTASB(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
 802  CONTINUE
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
C
      IF(KDEC(2) .LT. ITIMES(1) .AND. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
C
C
      IF(IPRI .GT. 0)
     C   THEN
            DO 803 I=1,IPRLIM
C===        IF(KINT(4) .EQ. IDONEB(I)) GO TO 99800
803         CONTINUE
            IPRNUM=IPRNUM+1
            IF(IPRNUM .LE. IPRLIM) IDONEB(IPRNUM) = KINT(4)
C
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
10000 FORMAT(//)
20000 FORMAT(//2X,' PART B  NUMBER =',I5,/)
30000 FORMAT(2X,' PART B  NUMBER =',I4,1X,2(I10,1X),5X,A5,
     C       2(I10,1X),2(A10,1X))
C
C
99800 CONTINUE
C
C     CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
      CALL TEMPEXP(2,IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UKDEC - after tempexp, ierr ',IERR
CTEST
      CALL MRGTEM(IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UKDEC - after mrgtem, ierr ',IERR
CTEST
      CALL QCTEMP(KDEC(4),IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UKDEC - after qctemp, ierr ',IERR
CTEST
      CALL BUFRENC(IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' UKDEC - after bufrenc, ierr ',IERR
CTEST
C
99900 CONTINUE
      RETURN
      END
      SUBROUTINE UKINT ( IHEAD,IERR )
C
C**** *UKINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT TEMP REPORTS PART B FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UKINT(IHEAD,IERR)*
C
C          INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C                      IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                                WRITTEN TO ERROR FILE.
C                       = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                 IERR NOT USED ON INPUT.
C
C          OUTPUT    : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                      DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION ).
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
C         ARE NOT USED E.G. WRONG PART OF TEMP).
C                 FOR THIS PURPOSE TWO VARIABLES ARE USED
C                 NUMERR = NUMBER OF NON-FATAL ERRORS
C                 IREMPOS= THE POINTER TO FIRST ERROR IN KINT.
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
      INCLUDE 'comstat.f'
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
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C              --------------------------------------------------------
C              CHARACTER OF REPORT.
C              --------------------
 100  CONTINUE
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR = 0
      IREMPOS = 17
C
C     CHECK FOR 'NIL' REPORT.
C
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.26 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(4,0,IFIRST,NUMERR)
         END IF
C
C      SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C
      DO 102 I=1,700
         KINT(I) = MINDIC
  102 CONTINUE
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP/AIRCRAFT IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 103 I=12,14
         KINT(I) = 88
  103 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C
C     CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C
      DO 104 I=1,3
         KDEC(I) = MINDIC
  104 CONTINUE
C
      DO 105 I=5,9
         KDEC(I) = MINDIC
  105 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = 0
C
      KDEC(17) = MINDIC
      KDEC(22) = MINDIC
C     ---------------------------------------------------------------
C*          2. SECTION  (IDENTIFICATION AND POSITION,
C              THIS IS DIFFERENT FOR LAND STATIONS AND
C              SHIP/AIRCRAFT REPORTS)
1000  CONTINUE
C
C               YY GG /
C               -- -- -
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*25000)
                CALL NEXPRT2(IPT,IEQ,*25000)
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
C
                JPT = IPT
C
C
C
C
  700 IF ( KDEC(4).EQ.35 )
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
                CALL NEXPRT2( IPT,IEQ,*25000)
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
                IF(IRET .EQ. 0) THEN
                       IF(NIL .EQ. 1) THEN
                          KDEC(20)=1 
                          RETURN
                       END IF
                END IF
C
                IF ( IRET.EQ.1 ) THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        CALL ERRSTA(4,1,IFIRST,NUMERR)
C
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
                        OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
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
cpsokt98 9901                FORMAT (1H ,a8,' TEMP NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' TEMP NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        RETURN
                    END IF
C
                IF ( IRET.EQ.2 ) THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2
C
C                       RETURN IF REPORT CAN'T CONTAIN IDENT
C
                        IF( (KPT+5) .GE. IEQ) RETURN
C

                        CALL ERRSTA(4,2,IFIRST,NUMERR)
C
                        RETURN
                    END IF
C
                 IF(IRET .EQ. 3) THEN
C
C                     INVALID OBSERVATION TYPE
C
                      KERR = 3
                      CALL ERRSTA(4,2,IFIRST,NUMERR)
C
                      RETURN
                 END IF
C 
                 IF ( IRET.EQ.4 ) THEN
C
C                   STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                   ENTRY , FOR THAT OBSERVATION TYPE. 
C
c                   PRINT IIIII WITH NO MASTER FILE ENTRY
                        OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C                        
                    WRITE (55,9991) ctstamp,KINT(4)
                    CLOSE(55)
                    WRITE (*,9991) ctstamp,KINT(4)
cpsokt98 9991               FORMAT (1H ,a8,' TEMP WRONG TYPE OF STATION  - ',I5)
 9991              FORMAT (1H ,a12,' TEMP WRONG TYPE OF STATION  - ',I5)
c                    CALL PRTBULL(1,IGS)
C-----              PRINT IIIII WITH NO MASTER FILE ENTRY
                 END IF 
C
            ELSE
C
CTEST
      PRINT*,' UKINT - temp ship/drop/mobile, kdec(4) ',KDEC(4)
CTEST
C                TEMP SHIP OR TEMP DROP
C
                 IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137)
     1              THEN
C
                        KINT(4)=0
C
C              RETAIN POINTER TO STATION IDENTIFIER .
C
               KPT=IMI
C
C
C              CHECK THE CALL SIGN
C
               IBUOY = 0
               K = IMI
               J = K + 4
C
               DO 750 I=K,J
                    IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750          CONTINUE
C
C              IF A LETTER GROUP IS FOUND CHECK THAT THIS IS NOT
C              MIMIMJMJ GROUP 'UUBB'. IN THIS CASE THE SHIP CALL
C              SIGN GROUP IS PRESUMED MISSING.
C
               IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85) .AND.
     C            (KCHAR(K+2) .EQ. 66) .AND. (KCHAR(K+3) .EQ. 66))
     C                   THEN
                             IBUOY = 2
                        END IF
CTEST
      PRINT*,' UKINT - ibuoy  ',IBUOY
CTEST
C
               IF((KCHAR(K) .EQ. 73) .AND. (KCHAR(K+1) .EQ. 73) .AND.
     C            (KCHAR(K+2) .EQ. 66) .AND. (KCHAR(K+3) .EQ. 66))
     C                   THEN
                             IBUOY = 2
                         END IF
C
               IF((KCHAR(K) .EQ. 88) .AND. (KCHAR(K+1) .EQ. 88) .AND.
     C            (KCHAR(K+2) .EQ. 66) .AND. (KCHAR(K+3) .EQ. 66))
     C                   THEN
                             IBUOY = 2
                         END IF
C
C
               IF ( IBUOY.NE.0 )
     C             THEN
C
C                      SHIP,RIG OR PLATFORM. IDENTIFIER NO. SET TO 0
C
                         ID=0
                       KINT(4) = 0
C
                       IF (IBUOY.EQ.1)
     C                    THEN
C
C                             MOVE POINTER PAST CALL SIGN . THE FIRST
C                             CHARACTER OF THE NEXT GROUP IS
C                             LOCATED AS SOME SHIPS USE THE FULL NAME
C                             ( WHICH CAN CONSIST OF 2 WORDS ) INSTEAD
C                             OF CALL SIGN. SO IF THE NEXT GROUP DOESN'T
C                             START WITH 'UU' (WHICH IS THE BEGINNING
C                             OF MIMIMJMJ GOUP) IT MUST BE PART OF THE
C                             SHIP'S NAME USED INSTEAD OF CALL SIGN.
C
                              IIPT=K
                              CALL NEXSEP2(IIPT,IEQ,*25000)
                              ID = IIPT - KPT
                              CALL NEXTLET (IIPT,IEQ)
C
                              IF(KCHAR(IIPT) .NE. 85 .OR. KCHAR(IIPT+1)
     C                           .NE. 85)
     C                                THEN
                                         CALL NEXSEP2(IIPT,IEQ,*25000)
                                         ID = IIPT - K
                                      END IF
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
                             CALL NEXSEP2(IIPT,IEQ,*25000)
                             CALL NEXPRT2(IIPT,IEQ,*25000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*25000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*25000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*25000)
                                       ID=IIPT-K
                                    END IF
                              END IF
                          END IF
C
                   END IF
C
C                       99 LALALA
C                       -- ------
                        CALL NEXPRT2( IPT,IEQ,*25000)
                        CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                       CHECK VALIDITY OF LALALA
C
                        IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1  
                                 RETURN
                              END IF
C
                              KERR = 4
                              CALL ERRSTA(4,1,IFIRST,NUMERR)
                              GO TO 25000
                           END IF
C
C                       QC LOLOLOLO
C                       -- --------
C
                        CALL NEXPRT2( IPT,IEQ,*25000)
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
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                              KERR=4
                              IF (IFIRST.EQ.0)
     C                           THEN
                                    CALL ERRSTA(4,2,IFIRST,NUMERR)
                                 END IF
                              GO TO 25000
                           END IF
C
C                       MMM ULA ULO
C                       --- --- ---
C
                        CALL NEXPRT2(IPT,IEQ,*25000)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                              kdec(5),kdec(6))
                        IF(KDEC(5).EQ.MINDIC) RETURN
                        IF(KDEC(6).EQ.MINDIC) RETURN   
                        CALL MARDSEN(kdec(5),kdec(6),KINT(9),IRET)
C
                        IF(IRET .NE. 0)
     1                      THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
C
C    --------------            WRONG CODE FOR MARSDEN SQUARE    ------
                               KERR = 4
                               CALL ERRSTA(4,1,IFIRST,NUMERR)
C
                              GO TO 25000
C
                            END IF
C
C                      hohohohoim ( height of mobile platform)
C                      ---------------------------------------
C                        HOHOHOHO - KINT(699)
C                        IM       - KINT(700)
C
                       IF(KDEC(4).EQ.137) THEN
                          CALL NEXTPRT(IPT,IEQ)
                          CALL EXTGRP(IPT,4,1,0,0,0,699,IRET)
                       END IF
C
                    ELSE
C
C                       THIS IS NOT A TEMP REPORT
C
                            KERR = 3
                            NOER(4,KERR) = NOER(4,KERR)+1
C
                              GO TO 25000
                END IF
             END IF
C
C     RETAIN STATION IDENTIFIER IN CHARACTER FORM ( IF ANY EXISTS )
C
      IF ( ID.NE.0 )
     C    THEN
              IF (ID.GT.5) ID = 5
              J = KPT
              K = 12 + ID -1
              DO 800 I=12,K
C
C                  IF LETTER ENCOUNTERED FOR LAND STATION CONVERT
C                  IT TO FIGURE (NOT FOR SHIP'S CALL SIGN)
C
                   IF(KDEC(4) .EQ. 35) CALL LETFIG(KCHAR(J))
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C     CHECK VALUES OF YY GG FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4).GE.35 )
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
                       CALL ERRSTA(4,1,IFIRST,NUMERR)
                       GO TO 25000
C
                   END IF
           END IF
C
C      -------------------------------------------------------------
C*          3. SECTION 5 (SIGNIFICANT LEVELS WITH RESPECT TO
C              ---------------------------------------------
C              TEMPERATURE AND/OR RELATIVE HUMIDITY).
C              --------------------------------------
C
5000  CONTINUE
C
C     'NIL' REPORTS ARE NOT PROSESSED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
C
C     GET THE POINTER TO THE FIRST LEVEL
C
      CALL NEXPRT2(IPT,IEQ,*25000)
      LPT = IPT
C
      LEVNUM=0
      LEVPRE=99999
      ITRY = 0
C
5100  CONTINUE
C
C     TRY TO FIND WIND DATA IF ALREADY FIVE ERRORS DETECTED
C
      IF(NUMERR .GE. 5) GO TO 5900
C
C     NINI PIPIPI
C     ---- ------
      CALL NEXTNI(5,IFIRST,LEVNUM,LEVPRE,IREMPOS,NUMERR,
     C           *6000,*8000,*9000,*10000,*25000,*5900,*5200,*7000)
C
5200  CONTINUE
C
      LEVNUM = LEVNUM+1
      IPOS=17+(LEVNUM-1)*5
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
      IF(IRET .NE. 0)
     1   THEN
            KERR = 5
            CALL ERRSTA(4,2,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            CALL NEXSEP2(IPT,IEQ,*25000)
            CALL NEXPRT2(IPT,IEQ,*25000)
            LEVNUM = LEVNUM - 1
            GO TO 5100
         END IF
C
      IF(KINT(IPOS+1) .EQ. MINDIC)
     1   THEN
            KERR = 5
            CALL ERRSTA(4,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
            KINT(IPOS) = MINDIC
            CALL NEXPRT2(IPT,IEQ,*25000)
            CALL NEXSEP2(IPT,IEQ,*25000)
            LEVNUM = LEVNUM - 1
            GO TO 5100
         END IF
C
5400  CONTINUE
C
C     TITI TAI DIDI
C     ---- --- ----
C
      CALL NEXPRT2(IPT,IEQ,*5800)
      LPT = IPT
      CALL EXTGRP(IPT,2,1,2,0,0,IPOS+2,IRET)
C
                   IF(IRET .NE. 0 .OR. KINT(IPOS+2) .EQ. MINDIC)
     1                THEN
C
C                         SEE IF THE TEMPERATURE IS = /////
C
                          DO 5405 I=LPT,LPT+4
                          IF(KCHAR(I) .EQ. 47) GO TO 5405
                          GO TO 5406
5405                      CONTINUE
C
                          GO TO 5100
5406                      CONTINUE
                          KERR = 6
                          CALL ERRSTA(4,1,IFIRST,NUMERR)
                          IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                          DO 5410 I = IPOS,IPOS+4
                          KINT(I) = MINDIC
5410                      CONTINUE
                          CALL NEXSEP2(IPT,IEQ,*25000)
                          GO TO 5100
                      END IF
C
C
C     REJECT IF THE TEMPERATURE IS POSITIVE ABOVE 400 MB
C
crrr test depends on less than 3 levels under 1000 otherwise kerr=6.
crrr changed to less than 4 levels.
cpsjan01 changed to less than 8 levels
crrr            IF(KINT(IPOS+1) .LE. 400 .AND. LEVNUM .GT. 3)
            IF(KINT(IPOS+1) .LE. 400 .AND. LEVNUM .GT. 7)
     C         THEN
                  ISIGN = 2*(KINT(IPOS+3)/2)
                  IF(ISIGN .EQ. KINT(IPOS+3))
     C               THEN
                        KERR = 6
               print*,'ukint - temperature/level test, kerr=6,levnum=',levnum
                        CALL ERRSTA(4,1,IFIRST,NUMERR)
                        IF(NUMERR .EQ. 1) IREMPOS = IPOS
                           DO 5500 I = IPOS,IPOS+4
                           KINT(I) = MINDIC
5500                       CONTINUE
                           CALL NEXPRT2(IPT,IEQ,*25000)
                           GO TO 5100
                      END IF
               END IF
C
      LEVPRE = KINT(IPOS+1)
      KINT(IPOS+5) = -999999
C
      GO TO 5100
C
5800  CONTINUE
      KINT(IPOS  ) = MINDIC
      KINT(IPOS+1) = MINDIC
      GO TO 25000
C
C
C
5900  CONTINUE
C
C
C     SEARCH FOR THE NEXT SECTION
C     ====== === === ==== =======
C
C
C     DON'T CONTINUE IF ALREADY SIX ERRORS DETECTED
C
      IF(NUMERR .GT. 6) GO TO 25000
C
C
      CALL NEXPRT2(IPT,IEQ,*25000)
C
      CALL EXTVAL(IPT,5,IVAL)
C
      IF(IVAL .EQ. 21212) GO TO 6000
C
      IF(IVAL.EQ.31313) GO TO 7000
C
      IF(IVAL .EQ. 41414) GO TO 8000
C
      IF((IVAL .EQ. 51515) .OR. (IVAL .EQ. 52525) .OR.
     C   (IVAL .EQ. 53535) .OR. (IVAL .EQ. 54545) .OR.
     C   (IVAL .EQ. 55555) .OR. (IVAL .EQ. 56565) .OR.
     C   (IVAL .EQ. 57575) .OR. (IVAL .EQ. 58585) .OR.
     C   (IVAL .EQ. 59595) .OR. (IVAL .EQ. 51525)) GO TO 9000
C
      IF((IVAL .EQ. 61616) .OR. (IVAL .EQ. 62626) .OR.
     C   (IVAL .EQ. 63636) .OR. (IVAL .EQ. 64646) .OR.
     C   (IVAL .EQ. 65656) .OR. (IVAL .EQ. 66666) .OR.
     C   (IVAL .EQ. 67676) .OR. (IVAL .EQ. 68686) .OR.
     C   (IVAL .EQ. 69696)) GO TO 10000
C
      CALL NEXSEP2(IPT,IEQ,*25000)
      GO TO 5900
C
C
C      -------------------------------------------------------------
C*          6. SECTION 6 (SIGNIFICAT LEVELS WITH RESPECT TO WIND).
C              ---------------------------------------------------
6000  CONTINUE
C
C
C     21212
C     -----
C     CHECK IF VALUE 21212 GAS ALREADY BEEN INSERTED ( MAY
C     HAPPEN IN CASE OF CORRUPTED '21212'-GROUP).
C
      IF(KINT(317) .EQ. 21212)
     C   THEN
            CALL NEXSEP2(IPT,IEQ,*25000)
            CALL NEXPRT2(IPT,IEQ,*25000)
            GO TO 6050
         END IF
C
C
      CALL EXTGRP(IPT,5,0,0,0,0,317,IRET)
C
      IF(IRET .NE. 0)
     C   THEN
            KERR = 10
            CALL ERRSTA(4,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = 317
            CALL NEXSEP2(IPT,IEQ,*25000)
         END IF
C
C
6050  CONTINUE
C
      LEVPRE=99999
      LEVNUM=0
      ITRY = 0
C
6100  CONTINUE
C
C     DON'T CONTINUE IF ALREADY SIX ERRORS DETECTED
C
      IF(NUMERR .GT. 6) GO TO 25000
C
      CALL NEXTNI(4,IFIRST,LEVNUM,LEVPRE,IREMPOS,NUMERR,
     C            *6000,*8000,*9000,*10000,*25000,*5900,*6200,*7000)
C
6200  CONTINUE
C
      LEVNUM=LEVNUM+1
      IPOS=318+(LEVNUM-1)*4
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
      IF(IRET .NE. 0) THEN
                          KERR = 5
                          CALL ERRSTA(4,2,IFIRST,NUMERR)
                          IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                          CALL NEXSEP2(IPT,IEQ,*25000)
                          CALL NEXPRT2(IPT,IEQ,*25000)
                          LEVNUM = LEVNUM - 1
                          GO TO 6100
                      END IF
C
      IF(KINT(IPOS+1) .EQ. MINDIC)
     1   THEN
            KERR = 5
            CALL ERRSTA(4,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
            KINT(IPOS) = MINDIC
            CALL NEXPRT2(IPT,IEQ,*25000)
            CALL NEXSEP2(IPT,IEQ,*25000)
            LEVNUM = LEVNUM - 1
            GO TO 6100
         END IF
C
6400  CONTINUE
C
C     DIDI FIFIFI
C     ---- ------
      CALL NEXPRT2(IPT,IEQ,*6800)
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
      IF ( (IRET .NE. 0)  .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C      KINT(IPOS+2) .NE. MINDIC) ) THEN
            KERR = 7
C
            CALL ERRSTA(4,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
            DO 6500 I = IPOS,IPOS+3
            KINT(I) = MINDIC
6500        CONTINUE
            CALL NEXSEP2(IPT,IEQ,*25000)
C
            GO TO 6100
      END IF
C
cps   This section (from ECMWF) sets ddfff to missing if = 0. This
cps   means it is impossible to report 'calm' (no wind). On the other hand,
cps   it seems like quite a few temps code 'missing' as 00000, especially
cps   for surface level.
      if(kint(ipos+2).eq.0.and.kint(ipos+3).eq.0) then
         kint(ipos+2)=mindic
         kint(ipos+3)=mindic
      end if
C
      LEVPRE = KINT(IPOS+1)
      KINT(IPOS+4) = -999999
C
      GO TO 6100
C
C
6800  CONTINUE
      KINT(IPOS  ) = MINDIC
      KINT(IPOS+1) = MINDIC
      GO TO 25000
C
C     ---------------------------------------------------------------
C*          7. SECTION 7.
C              ----------
 7000 CONTINUE
C
cpssep06 Removed commented out code from before 1991, then added initializing
cps      KINT to missing, in case more than 60 levels in section 6 has caused
cps      data to be written into part of KINT reserved for section 7.
      do i=611,618
         kint(i)=mindic
      end do

C     WMO CODE AFTER 1.11.1991.
C 
      CALL EXTGRP(IPT,5,0,0,0,0,610,IRET)
C
C     SrRaRaSaSa GROUP
C
      CALL NEXPRT2(IPT,IEQ,*25000)
      CALL EXTGRP(IPT,1,2,2,0,0,611,IRET)
      IF(IRET.NE.0)
     1   THEN
            KERR=7
            CALL ERRSTA(4,1,IFIRST,NUMERR)
            IF(NUMERR.EQ.1) IREMPOS=611
            CALL NEXSEP2(IPT,IEQ,*25000)
         END IF
C
      CALL NEXPRT2(IPT,IEQ,*25000)
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL.EQ.41414) GO TO 5900
C
C     8GGgg GROUP
C
      CALL EXTGRP(IPT,1,2,2,0,0,614,IRET)
      IF(IRET.NE.0)
     1   THEN
            KERR=7
            CALL ERRSTA(4,1,IFIRST,NUMERR)
            IF(NUMERR.EQ.1) IREMPOS=614
            CALL NEXSEP2(IPT,IEQ,*25000)
            GO TO 25000
         END IF
C
      CALL NEXPRT2(IPT,IEQ,*25000)
      CALL EXTVAL(IPT,5,IVAL)
cpssep06      IF(IVAL.EQ.41414) GO TO 5900
      IF(  (IVAL.EQ.41414) .or. 
     *     (IVAL .EQ. 51515) .OR. (IVAL .EQ. 52525) .OR.
     C     (IVAL .EQ. 53535) .OR. (IVAL .EQ. 54545) .OR.
     C     (IVAL .EQ. 55555) .OR. (IVAL .EQ. 56565) .OR.
     C     (IVAL .EQ. 57575) .OR. (IVAL .EQ. 58585) .OR.
     C     (IVAL .EQ. 59595) .OR. (IVAL .EQ. 51525)
     *     ) GO TO 5900
C
C     9SnTwTwTw GROUP           ! optional group
C
      CALL EXTGRP(IPT,1,1,3,0,0,617,IRET)
      IF(IRET.NE.0)
     1   THEN
            KERR=7
            CALL ERRSTA(4,1,IFIRST,NUMERR)
cpssep06            IF(NUMERR.EQ.1) IREMPOS=604
            IF(NUMERR.EQ.1) IREMPOS=617
            CALL NEXSEP2(IPT,IEQ,*25000)
            GO TO 25000
         END IF
C
      GO TO 5900
C
C     -----------------------------------------------------------------
C*          8. SECTION 8    (CLOUD DATA).
C              --------------------------
8000  CONTINUE
C
cpssep06 Added resetting of KINT to missing, in case more than 60 
cps      levels in section 6 has caused data to be written into 
cps      part of KINT reserved for section 8.
      do i=558,563
         kint(i)=mindic
      end do
C
C     41414
C     -----
C
      CALL EXTGRP(IPT,5,0,0,0,0,558,IRET)
      IF(IRET .NE. 0)
     C    THEN
C
C            NOT CORRECT SECTION 8 TRY
C            TO LOCATE NEXT SECTION
C
             KERR = 9
             CALL ERRSTA(4,1,IFIRST,NUMERR)
             IF(NUMERR .EQ. 1) IREMPOS = 558
             CALL NEXSEP2(IPT,IEQ,*25000)
             GO TO 5900
          END IF
C
C
C     NH CL H CM CH
C     -- -- - -- --
C
      CALL NEXPRT2(IPT,IEQ,*25000)
      CALL EXTGRP(IPT,1,1,1,1,1,559,IRET)
      IF(IRET .NE. 0)
     C    THEN
             KERR = 8
             CALL ERRSTA(4,1,IFIRST,NUMERR)
cpssep06             IF(NUMERR .EQ. 1) IREMPOS = 558
             IF(NUMERR .EQ. 1) IREMPOS = 559
             CALL NEXSEP2(IPT,IEQ,*25000)
          END IF
C
C     -----------------------------------------------------------------
C*          9. SECTION 9  (REGIONAL GROUPS).
C              -----------------------------
9000  CONTINUE
C
cpssep06 Added resetting of KINT to missing, in case more than 60 
cps      levels in section 6 has caused data to be written into 
cps      part of KINT reserved for section 9.
      do i=564,610
         kint(i)=mindic
      end do
C
      CALL NEXPRT2(IPT,IEQ,*25000)
C
C     THERE ARE REGIONAL DIFFERENCIES FOR REGIONS I,II,IV AND VI
C
      IF(KDEC(17) .EQ. 1) GO TO 9100
      IF(KDEC(17) .EQ. 2) GO TO 9200
      IF(KDEC(17) .EQ. 4) GO TO 9400
      IF(KDEC(17) .EQ. 6) GO TO 9600
C
      GO TO 10000
C
C
9100  CONTINUE
C
C     REGION I
C     ------ -
C     51515
C     -----
C
C     THERE MAY BE UP TO THREE SPECIAL LEVELS IN REGION I
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .NE. 51515)
     C   THEN
            CALL NEXSEP2(IPT,IEQ,*25000)
            GO TO 5900
         END IF
C
      CALL EXTGRP(IPT,5,0,0,0,0,564,IRET)
      IF(IRET .NE. 0)
     C      THEN
                KERR = 21
                CALL ERRSTA(4,1,IFIRST,NUMERR)
                IF(NUMERR .EQ. 1) IREMPOS = 564
                CALL NEXSEP2(IPT,IEQ,*25000)
                GO TO 5900
             END IF
C
9150  CONTINUE
C
      CALL NEXPRT2(IPT,IEQ,*25000)
C
C     CHECK THE LEVEL
C
      CALL EXTVAL(IPT,2,IVAL)
C
      IF(IVAL .EQ. 92)
     C   THEN
            IND=565
         ELSE
            IF(IVAL .EQ. 77)
     C         THEN
                  IND=572
               ELSE
                  IF(IVAL .EQ. 60)
     C               THEN
                        IND=579
                     ELSE
                        GO TO 5900
                     END IF
               END IF
         END IF
C
C     92 H9H9H9 , 77 H7H7H7 OR 60 H6H6H6  GEOPOTENTIAL AT THE LEVEL
C     -- ------   -- ------    -- ------
C
      CALL EXTGRP(IPT,2,3,0,0,0,IND,IRET)
      IF(IRET .NE. 0) THEN
                         KERR = 25
                         CALL ERRSTA(4,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IND
                         CALL NEXSEP2(IPT,IEQ,*25000)
                         GO TO 9170
                       END IF
C
C     TITI TAI DIDI    TEMPERATURE AND DEW-POINT TEMPERATURE
C     ---- --- ----
C
      CALL NEXPRT2(IPT,IEQ,*25000)
C
C     CHECK IF THIS ACTULLY IS THE BEGINNING OF NEXT LEVEL
C
      CALL EXTVAL(IPT,2,IVAL2)
      IF(IVAL .EQ. 92 .AND. IVAL2 .EQ. 77) GO TO 9150
      IF(IVAL .EQ. 77 .AND. IVAL2 .EQ. 60) GO TO 9150
C
      CALL EXTGRP(IPT,2,1,2,0,0,IND+2,IRET)
      IF(IRET .NE. 0) THEN
                         KERR = 26
                         CALL ERRSTA(4,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IND + 2
                         CALL NEXSEP2(IPT,IEQ,*25000)
                         GO TO 9170
                      END IF
C
C     DIDI FIFIFI      WINDS AT THE LEVEL
C     ---- ------
      CALL NEXPRT2(IPT,IEQ,*25000)
C
C     CHECK IF THIS ACTULLY IS THE BEGINNING OF NEXT LEVEL
C
      CALL EXTVAL(IPT,2,IVAL)
      IF(IVAL .EQ. 77 .OR. IVAL .EQ. 60) GO TO 9150
C
      CALL EXTGRP(IPT,2,3,0,0,0,IND+5,IRET)
      IF( IRET .NE. 0  .OR.  ( KINT(IND+5) .GT. 36 .AND.
     C                         KINT(IND+5) .NE. MINDIC) )
     C               THEN
                         KERR = 27
                         CALL ERRSTA(4,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IND + 5
                         CALL NEXSEP2(IPT,IEQ,*25000)
                      END IF
      GO TO 9150
C
C
9170  CONTINUE
C
      DO 9180 I=IND,IND+6
      KINT(IND) = MINDIC
9180  CONTINUE
      GO TO 5900
C
9200  CONTINUE
C
C     REGION II
C     ------ --
C
C          51525  52525
C          -----  -----
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 51525 .OR. IVAL .EQ. 52525)
     C   THEN
C
           IND = 564
C
           CALL EXTGRP(IPT,5,0,0,0,0,IND,IRET)
           IF(IRET .NE. 0)
     C        THEN
                 KERR = 21
                 CALL ERRSTA(4,1,IFIRST,NUMERR)
                 IF(NUMERR .EQ. 1) IREMPOS = IND
                 CALL NEXSEP2(IPT,IEQ,*25000)
                 GO TO 9250
              END IF
C
C          92 H9H9H9
C          -- ------
C
           CALL NEXPRT2(IPT,IEQ,*25000)
C
C          TEST IF THE FIRST GROUP IS 92---
C
           CALL EXTVAL(IPT,2,IVAL)
           IF(IVAL .NE. 92) GO TO 5900
C
           CALL EXTGRP(IPT,2,3,0,0,0,IND+1,IRET)
           IF(IRET .NE. 0)
     C        THEN
                 KERR = 25
                 CALL ERRSTA(4,1,IFIRST,NUMERR)
                 IF(NUMERR .EQ. 1) IREMPOS = IND
                 CALL NEXSEP2(IPT,IEQ,*25000)
                 GO TO 9250
              END IF
C
C          T9T9 TA9 D9D9
C          ---- --- ----
C
           CALL NEXPRT2(IPT,IEQ,*25000)
           CALL EXTGRP(IPT,3,2,0,0,0,IND+3,IRET)
           IF(IRET .NE. 0)
     C        THEN
                 KERR = 26
                 CALL ERRSTA(4,1,IFIRST,NUMERR)
                 IF(NUMERR .EQ. 1) IREMPOS = IND + 1
                 CALL NEXSEP2(IPT,IEQ,*25000)
                 GO TO 9250
              END IF
C
C          D9D9 F9F9F9
C          ---- ------
C
           CALL NEXPRT2(IPT,IEQ,*25000)
           CALL EXTGRP(IPT,2,3,0,0,0,IND+5,IRET)
           IF(IRET .NE. 0 .OR. (KINT(IND+5) .GT. 36 .AND.
     C        KINT(IND+5) .NE. MINDIC))
     C        THEN
                 KERR = 27
                 CALL ERRSTA(4,1,IFIRST,NUMERR)
                 IF(NUMERR .EQ. 1) IREMPOS = IND + 5
                 CALL NEXSEP2(IPT,IEQ,*25000)
                 GO TO 9250
              END IF
C
           GO TO 5900
C
        ELSE
           IF(IVAL .EQ. 51515)
     C        THEN
C
                 IND = 572
                 CALL EXTGRP(IPT,5,0,0,0,0,IND,IRET)
                 IF(IRET .NE. 0)
     C              THEN
                       KERR = 21
                       CALL ERRSTA(4,2,IFIRST,NUMERR)
                       IF(NUMERR .EQ. 1) IREMPOS = IND
                       CALL NEXSEP2(IPT,IEQ,*25000)
                       GO TO 5900
                    END IF
C
9210          CONTINUE
C
              CALL NEXPRT2(IPT,IEQ,*25000)
C
C             DETERMINE THE LEVEL
C
              CALL EXTVAL(IPT,2,IVAL)
C
              IF(IVAL .EQ. 11)
     C           THEN
                     IND = 573
                 ELSE
                    IF(IVAL .EQ. 22)
     C                 THEN
                           IND = 577
                       ELSE
                          IF(IVAL .EQ. 33)
     C                       THEN
                                 IND = 581
                             ELSE
                                GO TO 5900
                             END IF
                       END IF
                 END IF
C
C
C             11 PPP , 22 800 OR 33 600  SPECIAL WIND LEVEL
C             -- ---   -- ---    -- ---
C
              CALL EXTGRP(IPT,2,3,0,0,0,IND,IRET)
              IF(IRET .NE. 0)
     C           THEN
                    KERR = 25
                    CALL ERRSTA(4,2,IFIRST,NUMERR)
                    IF(NUMERR .EQ. 1) IREMPOS = IND + 1
                    CALL NEXSEP2(IPT,IEQ, *25000)
                    GO TO 5900
                 END IF
C
C                DD FFF  WIND AT THE LEVEL
C                -- ---
C
                 CALL NEXPRT2(IPT,IEQ,*25000)
                 CALL EXTGRP(IPT,2,3,0,0,0,IND+2,IRET)
                 IF(IRET .NE. 0 .OR. (KINT(IND+2) .GT. 36 .AND.
     C              KINT(IND+2) .NE. MINDIC))
     C                 THEN
                         KERR = 27
                         CALL ERRSTA(4,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IND + 5
                         CALL NEXSEP2(IPT,IEQ, *25000)
                         GO TO 5900
                     END IF
C
C                CHECK IF THERE IS SUPPOSED TO BE MORE LEVELS
C
                 IF(IVAL .EQ. 33) GO TO 5900
                 GO TO 9210
              END IF
           END IF
C
C
C
9250  CONTINUE
C
      DO 9260 I=564,569
      KINT(I) = MINDIC
9260  CONTINUE
      CALL NEXSEP2(IPT,IEQ,*25000)
      GO TO 5900
C
C
C
9400  CONTINUE
C
cps Region IV is not decoded in UKDEC!
C     REGION IV
C     ------ --
C
C
C          51515, 52525, ...
C          -----  -----
C
           CALL EXTGRP(IPT,5,0,0,0,0,564,IRET)
           IF(IRET .NE. 0)
     C        THEN
                 KERR = 21
                 CALL ERRSTA(4,1,IFIRST,NUMERR)
                 IF(NUMERR .EQ. 1) IREMPOS = 564
                 CALL NEXSEP2(IPT,IEQ, *25000)
                 GO TO 5900
              END IF
C
           NUMADF=0
C
9450       CONTINUE
C
C          101 ADF ADF
C          --- --- ---
C
           CALL NEXPRT2(IPT,IEQ,*25000)
C
C          CHECK IF THIS GROUP STARTS WITH 101, OTHERWISE
C
           CALL EXTVAL(IPT,3,IVAL)
           IF(IVAL .NE. 101) GO TO 5900
C
           NUMADF = NUMADF+1
           IPOS = 565 + (NUMADF-1)*2
C
           CALL EXTGRP(IPT,3,2,0,0,0,IPOS,IRET)
           IF(IRET .NE. 0)
     C        THEN
                 KERR = 28
                 CALL ERRSTA(4,1,IFIRST,NUMERR)
                 IF(NUMERR .EQ. 1) IREMPOS = IPOS
                 CALL NEXSEP2(IPT,IEQ,*25000)
                 GO TO 5900
              END IF
C
           GO TO 9450
C
C
C
C
9600  CONTINUE
C
C     REGION VI
C     ------ --
C
C          CHECK IF THIS IS THE SUBSECTION 51515 OR 52525
C
           CALL EXTVAL(IPT,5,IVAL)
           IF(IVAL .EQ. 51515)
     C        THEN
C
C             51515
C             -----
C
              CALL EXTGRP(IPT,5,0,0,0,0,564,IRET)
              IF(IRET .NE. 0)
     C           THEN
                    KERR = 21
                    CALL ERRSTA(4,1,IFIRST,NUMERR)
                    IF(NUMERR .EQ. 1) IREMPOS = 564
                    CALL NEXSEP2(IPT,IEQ,*25000)
                 GO TO 9700
              END IF
C
C             11 P1P1P1
C             -- ------
C
              CALL NEXPRT2(IPT,IEQ,*25000)
C
C             CHECK THE FIRST GROUP IS 11---
C
              CALL EXTVAL(IPT,2,IVAL)
              IF(IVAL .EQ. 11)
     C            THEN
                      CALL EXTGRP(IPT,2,3,0,0,0,565,IRET)
                      IF (IRET .NE. 0)
     C                   THEN
                            KERR = 25
                            CALL ERRSTA(4,1,IFIRST,NUMERR)
                            IF(NUMERR .EQ. 1) IREMPOS = 565
                            CALL NEXSEP2(IPT,IEQ,*25000)
                            GO TO 9700
                          END IF
                  ELSE
                      GO TO 9700
                  END IF
C
C             D1D1 F1F1F1
C             ---- ------
C
              CALL NEXPRT2(IPT,IEQ,*25000)
              CALL EXTGRP(IPT,2,3,0,0,0,567,IRET)
              IF(IRET .NE. 0 .OR. (KINT(567) .GT. 36 .AND.
     C                             KINT(567) .NE. MINDIC))
     C           THEN
                    KERR = 27
                    CALL ERRSTA(4,1,IFIRST,NUMERR)
                    IF(NUMERR .EQ. 1) IREMPOS = 567
                    CALL NEXSEP2(IPT,IEQ,*25000)
                    GO TO 9700
                 END IF
C
C
C
9700          CONTINUE
C
              CALL NEXPRT2(IPT,IEQ,*25000)
C
C
C             TRY TO FIND GROUP 22800 OR 33600 OR NEXT SECTION
C
              CALL EXTVAL(IPT,5,IVAL)
              IF(IVAL .EQ. 52525) GO TO 9750
              IF((IVAL .EQ. 61616) .OR. (IVAL .EQ. 62626) .OR.
     C           (IVAL .EQ. 63636) .OR. (IVAL .EQ. 64646) .OR.
     C           (IVAL .EQ. 65656) .OR. (IVAL .EQ. 66666) .OR.
     C           (IVAL .EQ. 67676) .OR. (IVAL .EQ. 68686) .OR.
     C           (IVAL .EQ. 69696)) GO TO 10000
C
C
              IF((IVAL .NE. 22800) .AND. (IVAL .NE. 33600))
     C            THEN
                     CALL NEXSEP2(IPT,IEQ,*25000)
                     CALL NEXPRT2(IPT,IEQ,*25000)
                     GO TO 9700
                  END IF
C
              IF(IVAL .EQ. 22800)
     C           THEN
C
C                    22800
C                    -----
C
                     CALL EXTGRP(IPT,5,0,0,0,0,569,IRET)
                     IF(IRET .NE. 0)
     C                  THEN
                           KERR = 21
                           CALL ERRSTA(4,1,IFIRST,NUMERR)
                           IF(NUMERR .EQ. 1) IREMPOS = 569
                           CALL NEXSEP2(IPT,IEQ,*25000)
                           GO TO 9700
                         END IF
C
C                    DD FFF
C                    -- ---
C
                     CALL NEXPRT2(IPT,IEQ,*25000)
                     CALL EXTGRP(IPT,2,3,0,0,0,570,IRET)
                     IF(IRET .NE. 0 .OR. (KINT(570) .GT. 36 .AND.
     C                  KINT(570) .NE. MINDIC))
     C                  THEN
                           KERR = 27
                           CALL ERRSTA(4,1,IFIRST,NUMERR)
                           IF(NUMERR .EQ. 1) IREMPOS = 569
                           KINT(569) = MINDIC
                           CALL NEXSEP2(IPT,IEQ,*25000)
                        END IF
C
C
                     CALL NEXPRT2(IPT,IEQ,*25000)
                     GO TO 9700
C
                 END IF
C
C
             IF(IVAL .EQ. 33600)
     C            THEN
C
C                    33600
C                    -----
C
                     CALL EXTGRP(IPT,5,0,0,0,0,572,IRET)
                     IF(IRET .NE. 0)
     C                  THEN
                           KERR = 21
                           CALL ERRSTA(4,1,IFIRST,NUMERR)
                           IF(NUMERR .EQ. 1) IREMPOS = 572
                           CALL NEXSEP2(IPT,IEQ,*25000)
                           GO TO 9700
                       END IF
C
C                    DD FFF
C                    -- ---
C
                     CALL NEXPRT2(IPT,IEQ,*25000)
                     CALL EXTGRP(IPT,2,3,0,0,0,573,IRET)
                     IF(IRET .NE. 0 .OR. (KINT(573) .GT. 36 .AND.
     C                  KINT(573) .NE. MINDIC))
     C                  THEN
                           KERR = 27
                           CALL ERRSTA(4,1,IFIRST,NUMERR)
                           IF(NUMERR .EQ. 1) IREMPOS = 572
                           CALL NEXSEP2(IPT,IEQ,*25000)
                           GO TO 9700
                        END IF
C
                  END IF
                  CALL EXTVAL(IPT,5,IVAL)
             END IF
C
C        END OF SUBSECTION 51515
C
9750  CONTINUE
C
C
cps06 Do not find 52525 in WMO Manual of Codes, Vol II, Regional
cps   and National practices for region VI - but some countries use it.
           IF(IVAL .EQ. 52525)
     C        THEN
C
C
C             52525
C             -----
C
              CALL EXTGRP(IPT,5,0,0,0,0,575,IRET)
              IF(IRET .NE. 0)
     C             THEN
                      KERR = 21
                      CALL ERRSTA(4,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = 574
                      CALL NEXSEP2(IPT,IEQ,*25000)
                      GO TO 9800
                   END IF
C
C             92 H9H9H9
C             -- ------
C
              CALL NEXPRT2(IPT,IEQ,*25000)
C
C             CHECK IF THE FIRST GROUP IS 92---
C
              CALL EXTVAL(IPT,2,IVAL)
              IF(IVAL .NE. 92) GO TO 5900
C
              CALL EXTGRP(IPT,2,3,0,0,0,576,IRET)
              IF(IRET .NE. 0)
     C             THEN
                      KERR = 25
                      CALL ERRSTA(4,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = 576
                      CALL NEXSEP2(IPT,IEQ,*25000)
                      GO TO 9800
                   END IF
C
C
C             T9T9 TA9 D9D9
C             ---- --- ----
C
              CALL NEXPRT2(IPT,IEQ,*25000)
              CALL EXTGRP(IPT,2,1,2,0,0,578,IRET)
              IF(IRET .NE. 0)
     C             THEN
                      KERR = 26
                      CALL ERRSTA(4,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = 578
                      CALL NEXSEP2(IPT,IEQ,*25000)
                      GO TO 9800
                   END IF
C
C             D9D9 F9F9F9
C             ---- ------
C
              CALL NEXPRT2(IPT,IEQ,*25000)
              CALL EXTGRP(IPT,2,3,0,0,0,581,IRET)
              IF(IRET .NE. 0 .OR. (KINT(581) .GT. 36 .AND.
     C           KINT(581) .NE. MINDIC))
     C             THEN
                      KERR = 27
                      CALL ERRSTA(4,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = 581
                      CALL NEXSEP2(IPT,IEQ,*25000)
                      GO TO 9800
                   END IF
C
          END IF
C
      CALL NEXSEP2(IPT,IEQ,*25000)
      GO TO 5900
C
9800  CONTINUE
C
      DO 9810 I=575,582
      KINT(I) = MINDIC
9810  CONTINUE
      CALL NEXSEP2(IPT,IEQ,*25000)
      GO TO 5900
C
C     ----------------------------------------------------------------
C*          10. SECTION 10 (NATIONAL GROUPS).
C               -----------------------------
10000 CONTINUE
C
C     NO NATIONAL DATA DECODED AT THE MOMENT
C
25000 CONTINUE
C

cpssep06 In case section 8 and/or section 9 is missing in report, we need
c        to guard against the part in KINT reserved for these sections being
c        overwritten by more than 60 levels in section 6.
      if(kint(558).ne.41414) then
         do i=558,563
            kint(i)=mindic
         end do
      endif
      if(kint(564).ne.51515 .and. kint(564).ne.52525) then
         do i=564,610
            kint(i)=mindic
         end do
      end if

C     ERROR CHECKING AND HANDLING
C     RETURN IF NO ERRORS IN REPORT
C
      IF(KERR .EQ. 0) RETURN
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
C
C
                             IPT=KEEP
                             CALL FIXUS
                             IFIRST=1
                             NUMRERR(4) = NUMRERR(4) + 1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
                             DO 25500 I=KEEP,IGS
                             KCHAR(I) = IAND(KCHAR(I),127)
25500                        CONTINUE
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
                                   DO 26000 I = IREMPOS,700
                                   KINT(I) = MINDIC
26000                              CONTINUE
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
      SUBROUTINE ULDEC ( IHEAD,IERR )
C**** *ULDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE TEMP REPORT PART C FROM INTERMEDIATE FORMAT
C         TO FULL DECODED FORMAT.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ULDEC(IHEAD,IERR)*
C
C          INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'.
C
C          OUTPUT   : REPORT WRITTEN TO DECODED REPORTS FILE.
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
      INCLUDE 'printre.f'
C
      DATA IROU/0/
      DATA IPRNUM /0/
C
C     ------------------------------------------------------------------
C*          1.  SET FLAGS AND INDICATOES.
C                ------------------------
 100  CONTINUE
C
C     DEFINE 'NIL' INDICATOR
C
      NIL = IAND(KDEC(20),1)
C
C     CLEAR ERROR INDICATOR
C
      KERR = 0
C
C     CLEAR ERROR INDICATOR.
C
      KERR = 0
C
C     SET DECODED REPORT TO MISSING DATA VALUE
C
      DO 101 I=25,421
      KDEC(I)=MINDIC
 101  CONTINUE
C       --------------------------------------------------------------
C*          2.  SECTION 1.
C               ----------
 200  CONTINUE
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
C*          2.1  DAY AND TIME.
C                -------------
 210  CONTINUE
C
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C
C
C*          2.2  CHECK IF SHIP OR DROP TEMP.
C                ---------------------------
 220  CONTINUE
C
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
C
C        LAT & LONG
C
         CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1               KDEC(5),KDEC(6))
         IF(KDEC(5).EQ.MINDIC) RETURN
         IF(KDEC(6).EQ.MINDIC) RETURN   
C
C        ALTIT=0, FOR SEA STATION
C
         IF(KDEC(4) .EQ. 36) KDEC(8)=0
C
      END IF
C
C
C
C*          2.3 IDENT   STATION/SHIP/AIRCRAFT IDENTIFICATION
C               --------------------------------------------
230   CONTINUE
c
      KDEC(7)=88
C
      IF(KDEC(4).EQ.137) THEN
         IF(KINT(699).NE.MINDIC.AND.KINT(700).NE.MINDIC) THEN
            IF(KINT(700).GE.1.AND.KINT(700).LE.4) KDEC(8)=KINT(699)
            IF(KINT(700).GE.5.AND.KINT(700).LE.8) THEN
               KDEC(8)=NINT(KINT(699)*.3)
            END IF
         END IF
      END IF
C
 233  CONTINUE
C
C
C
C*          2.4  REPORT LENGTH .
C                ---------------
 240  CONTINUE
C
      KDEC(24) = 426
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
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LT. 0 .OR. KDEC(1) .GT. 31) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) GO TO 99800
C
      IROU = IROU + 1
C
C     ----------------------------------------------------------------
C*          3.  SECTION 2   PRESSURE LEVELS.
C               ----------------------------
 300  CONTINUE
C
      DO 301 I=1,14
C
      IND=(I-1)*7+17
      IND2=(I-1)*5+30
      IF(KINT(IND) .EQ. MINDIC) GO TO 301
C
C
C     PIPI HIHIHI    HEIGHT AT LEVEL I
C
      CALL GEOPOTE(KINT(IND),KINT(IND+1),MINDIC,KDEC(IND2))
C
C
C
C     TITI     TEMPERATURE AT LEVEL I
C
      CALL IC3931(KINT(IND+3),KINT(IND+2),MINDIC,KDEC(IND2+1))
C
C
C
C     TDTD     DEW-POINT TEMPERATURE AT LEVEL I
C
      CALL IC0777(KINT(IND+4),KDEC(IND2+1),MINDIC,KDEC(IND2+2))
C
C
C
C     DIDI FIFIFI    WIND DIRECTION & SPEED AT LEVEL I
C
      CALL DDFFF(KINT(IND+5),KINT(IND+6),IWW,K,MINDIC,KDEC(IND2+3),
     1            KDEC(IND2+4))
C
C
 301  CONTINUE
C
C     ---------------------------------------------------------------
C*          4. SECTION 3   TROPOPAUSE(S) .
C              ----------------------------
 400  CONTINUE
C
      DO 401 I=1,3
C
            IND =115 + (I-1)*7
            IND2=100 + (I-1)*5
C
C     88PTPTPT   PRESSURE AT TROPOPAUSE LEVEL
C
C           CHECK FOR MISSING DATA
C
            IF(KINT(IND) .EQ. MINDIC) GO TO 402
            IF(KINT(IND+1) .EQ. MINDIC .OR. KINT(IND+1) .EQ. 999)
     1          GO TO 401
            IF(KINT(IND+1) .EQ. 0 ) GO TO 401
C
            KDEC(IND2) = KINT(IND+1)
C
C     TTTT   TEMPERATURE AT TROPOPAUSE LEVEL
C
            CALL IC3931(KINT(IND+3),KINT(IND+2),MINDIC,KDEC(IND2+1))
C
C      TDTD   DEW-POINT TEMPERATURE AT TROPOPAUSE LEVEL
C
            CALL IC0777(KINT(IND+4),KDEC(IND2+1),MINDIC,KDEC(IND2+2))
C
C
C     DTDT FTFTFT   WINDS AT TROPOPAUSE
C
            CALL DDFFF(KINT(IND+5),KINT(IND+6),IWW,K,MINDIC,
     1                 KDEC(IND2+3),KDEC(IND2+4))
C
 401        CONTINUE
C
C
 402  CONTINUE
C
C
C     ----------------------------------------------------------------
C*          5. SECTION 4  MAX WIND LEVEL(S).
C              -----------------------------
 500  CONTINUE
C
      DO 501 I=1,3
C
            IND = 136 + (I-1)*7
            IND2= 115 + (I-1)*5
C
C     77PMPMPM OR 66PMPMPM   PRESSURE AT MAX. WIND LEVEL
C
C           CHECK FOR MISSING DATA
C
            IF(KINT(IND) .EQ. MINDIC) GO TO 502
            IF(KINT(IND+1) .EQ. MINDIC .OR. KINT(IND+1) .EQ. 999)
     1         GO TO 502
            IF(KINT(IND+1) .EQ. 0 ) GO TO 502
C
            KDEC(IND2) = KINT(IND+1)
C
C     DMDM FMFMFM   WIND AT MAX. WIND LEVEL
C
            CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     1                 KDEC(IND2+1),KDEC(IND2+2))
C
C     4VBVBVAVA   VERTICAL WIND SHEAR ASSOSIATED TO THIS MAX WIND LEVEL
C
            IF(KINT(IND+4) .EQ. MINDIC) GO TO 501
C
crr  change accuracy of wind by multiplying by 10.
crr            IF(KINT(IND+5) .NE. MINDIC) KDEC(IND2+3) = KINT(IND+5)
crr            IF(KINT(IND+6) .NE. MINDIC) KDEC(IND2+4) = KINT(IND+6)
            IF(KINT(IND+5) .NE. MINDIC) KDEC(IND2+3) = KINT(IND+5)*10
            IF(KINT(IND+6) .NE. MINDIC) KDEC(IND2+4) = KINT(IND+6)*10
C
C           IF WIND SPEED IN KNOTS TRANSFER TO M/S
C
            IF(IWW .EQ. 1) THEN
                              IF(KDEC(IND2+3) .NE. MINDIC)
     C                           CALL KTOMPSI(KDEC(IND2+3))
                              IF(KDEC(IND2+4) .NE. MINDIC)
     C                           CALL KTOMPSI(KDEC(IND2+4))
                           END IF
C
 501        CONTINUE
C
C
 502  CONTINUE
C
C
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT
C     (I.E. IF THERE ARE NO DATA AND NO CODING ERRORS)
C
         NIL2 = 1
         DO 503 I=25,KDEC(24)
         IF(KDEC(I) .EQ. MINDIC) GO TO 503
         NIL2 = 0
         GO TO 504
503      CONTINUE
C
504      CONTINUE
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 99800
         END IF
C
C
C     ----------------------------------------------------------------
C*          6. PRINTING OF ERRORS & REPORTS & KINT & KDEC .
C              --------------------------------------------
 600  CONTINUE
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
      DO 601 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
601   CONTINUE
C
      DO 602 I=1,100
      IF(ISTA .EQ. ISTASC(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
 602  CONTINUE
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
      IF(IERALLC .GT. 0) IPRI = 1
C
C
C
      IF(KDEC(2) .LT. ITIMES(1) .AND. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
      IF(IPRI .GT. 0)
     C   THEN
            DO 603 I=1,IPRLIM
C===        IF(KINT(4) .EQ. IDONEC(I)) GO TO 99800
 603        CONTINUE
            IPRNUM=IPRNUM+1
            IF(IPRNUM .LE. IPRLIM) IDONEC(IPRNUM) = KINT(4)
C
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
C
10000 FORMAT(///)
20000 FORMAT(//2X,' PART C  NUMBER =',I5,/)
30000 FORMAT(2X,' PART C  NUMBER =',I4,1X,2(I10,1X),5X,A5,
     C       2(I10,1X),2(A10,1X))
C
C
C
C
99800 CONTINUE
C
C     CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
      CALL TEMPEXP(3,IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' ULDEC - after tempexp, ierr ',IERR
CTEST
      CALL MRGTEM(IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' ULDEC - after mrgtem, ierr ',IERR
CTEST
      CALL QCTEMP(KDEC(4),IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' ULDEC - after qctemp, ierr ',IERR
CTEST
      CALL BUFRENC(IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' ULDEC - after bufrenc, ierr ',IERR
CTEST
C
99900 CONTINUE
C
      RETURN
      END
      SUBROUTINE ULINT ( IHEAD,IERR )
C**** *ULINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT TEMP REPORTS PART C FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ULINT(IHEAD,IERR)*
C
C          INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C                      IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                           WRITTEN TO ERROR FILE.
C                            = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                      IERR NOT USED ON INPUT.
C
C          OUTPUT    : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                      DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION ).
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
C         WILL CONTINUE FROM THAT POINT.
C         THE NUMBER OF NON-FATAL ERRORS IS COUNTED AND
C         IF IT IS GREATER THAN LIMIT (5 FOR NOW) ALL THE
C         DATA AFTER THE FIRST ERROR ARE REMOVED (THIS
C         IS DONE TO ENSURE THAT TOTALLY CORRUPTED DATA
C         ARE NOT USED E.G. WRONG PART OF TEMP).
C         FOR THIS PURPOSE TWO VARIABLES ARE USED
C         NUMERR = NUMBER OF NON-FATAL ERRORS
C         IREMPOS= THE POINTER TO FIRST ERROR IN KINT.
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *ERRSTA()*
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
      INCLUDE 'comstat.f'
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
      DIMENSION IPLEV(14)
C
      DATA IPLEV/
     1   70,50,30,20,10,9,8,7,6,5,4,3,2,1/
C
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C              -------------------------------------------------------
C              CHARACTER OF REPORT.
C              --------------------
 100  CONTINUE
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR = 0
      IREMPOS = 17
C
C
C     CHECK FOR 'NIL' REPORT.
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.26 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(5,0,IFIRST,NUMERR)
         END IF
C
C      SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C
cpssep06      DO 102 I=1,200
      DO 102 I=1,700
         KINT(I) = MINDIC
  102 CONTINUE
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP/AIRCRAFT IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 103 I=12,14
         KINT(I) = 88
  103 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C     CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C
      DO 104 I=1,3
         KDEC(I) = MINDIC
  104 CONTINUE
C
      DO 105 I=5,9
         KDEC(I) = MINDIC
  105 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = 0
C
      KDEC(17) = MINDIC
C
C
      KDEC(22) = MINDIC
C
C     -----------------------------------------------------------------
C*          2. SECTION 1 (IDENTIFICATION AND POSITION,
C              THIS IS DIFFERENT FOR LAND STATIONS AND
C              SHIP/AIRCRAFT REPORTS)
C
1000  CONTINUE
C
C               YY GG ID
C               -- -- --
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*5000)
                CALL NEXPRT2(IPT,IEQ,*5000)
                IF(KCHAR(IPT).GE.65) THEN
                                        GO TO 1011
                                     END IF
C
C               FIRST FIGURE OF THE GROUP MUST BE 0-3
C               OR 5-8 IF WIND SPEED IS GIVEN IN KNOTS,
C               OTHERWISE YYGGID GROUP IS MISSING
C
                IF ((KCHAR(IPT) .GE. 48) .AND. (KCHAR(IPT) .NE. 52)
     1             .AND. (KCHAR(IPT) .LE. 56))
     2             CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
C
                JPT = IPT
C
  700 IF ( KDEC(4).EQ.35 )
     C      THEN
C
C               LAND STATION
C
C               IIIII     -   RETAIN POINTER TO IDENTIFIER
C               -----
C
                CALL NEXPRT2( IPT,IEQ,*5000)
                KPT = IPT
                ID = 5
                CALL EXTINT ( IPT,5,4 )
C
C               EXTRACT STATION LAT/LONG ETC FROM WMO MASTER FILE.
C
                CALL LOCSTAT ( IWIND,IRET )
C
C               CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                IF(IRET .EQ. 0) THEN
                       IF(NIL .EQ. 1) THEN
                          KDEC(20)=1
                          RETURN
                       END IF
                END IF
C
                IF ( IRET.EQ.1 ) THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        CALL ERRSTA(5,0,IFIRST,NUMERR)
C
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
                        OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
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
cpsokt98 9901                FORMAT (1H ,a8,' TEMP NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' TEMP NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        RETURN
                    END IF
C
                IF ( IRET.EQ.2 ) THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2
C
C                       RETURN IF REPORT CAN'T CONTAIN IDENT
C
                        IF( (KPT+5) .GE. IEQ) RETURN
C
                        CALL ERRSTA(5,2,IFIRST,NUMERR)
                        RETURN
                    END IF
C
C
                 IF(IRET .EQ. 3) THEN
C
C                     INVALID OBSERVATION TYPE
C
                      KERR = 3
C
                      CALL ERRSTA(5,0,IFIRST,NUMERR)
C
                      RETURN
                 END IF
C
                 IF ( IRET.EQ.4 ) THEN
C
C                   STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                   ENTRY , FOR THAT OBSERVATION TYPE. 
C
c                   PRINT IIIII WITH NO MASTER FILE ENTRY
                        OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C                        
                        WRITE (55,9991) ctstamp,KINT(4)
                        CLOSE(55)
                    WRITE (*,9991) ctstamp,KINT(4)
cpsokt98 9991               FORMAT (1H ,a8,' TEMP WRONG TYPE OF STATION  - ',I5)
 9991              FORMAT (1H ,a12,' TEMP WRONG TYPE OF STATION  - ',I5)
c                    CALL PRTBULL(1,IGS)
C-----              PRINT IIIII WITH NO MASTER FILE ENTRY
                 END IF
C
            ELSE
C
C                TEMP SHIP OR TEMP DROP
C
                 IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137)
     1              THEN
C
                        KINT(4)=0
C
C              RETAIN POINTER TO STATION IDENTIFIER .
C
               KPT=IMI
C
C              CHECK THE CALL SIGN
C
               IBUOY = 0
               K = IMI
               J = K + 4
C
               DO 750 I=K,J
                    IF ( KCHAR(I).GE.65.AND.KCHAR(I).LE.90 ) IBUOY=1
  750          CONTINUE
C
C              IF A LETTER GROUP IS FOUND CHECK THAT THIS IS NOT
C              MIMIMJMJ GROUP 'UUCC'. IN THIS CASE THE SHIP CALL
C              SIGN GROUP IS PRESUMED MISSING.
C
               IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85) .AND.
     C            (KCHAR(K+2) .EQ. 67) .AND. (KCHAR(K+3) .EQ. 67))
     C                   THEN
                             IBUOY = 2
                        END IF
C
               IF((KCHAR(K) .EQ. 73) .AND. (KCHAR(K+1) .EQ. 73) .AND.
     C            (KCHAR(K+2) .EQ. 67) .AND. (KCHAR(K+3) .EQ. 67))
     C                   THEN
                             IBUOY = 2
                         END IF
C
               IF((KCHAR(K) .EQ. 88) .AND. (KCHAR(K+1) .EQ. 88) .AND.
     C            (KCHAR(K+2) .EQ. 67) .AND. (KCHAR(K+3) .EQ. 67))
     C                   THEN
                             IBUOY = 2
                         END IF
C
C
               IF ( IBUOY.NE.0 )
     C             THEN
C
C                      SHIP,RIG OR PLATFORM. IDENTIFIER NO. SET TO 0
C
                         ID=0
                       KINT(4) = 0
C
                       IF (IBUOY.EQ.1)
     C                    THEN
C
C                             MOVE POINTER PAST CALL SIGN . THE FIRST
C                             CHARACTER OF THE NEXT GROUP IS
C                             LOCATED AS SOME SHIPS USE THE FULL NAME
C                             ( WHICH CAN CONSIST OF 2 WORDS ) INSTEAD
C                             OF CALL SIGN. SO IF THE NEXT GROUP DOESN'T
C                             START WITH 'UU' (WHICH IS THE BEGINNING
C                             OF MIMIMJMJ GOUP) IT MUST BE PART OF THE
C                             SHIP'S NAME USED INSTEAD OF CALL SIGN.
C
                              IIPT=K
                              CALL NEXSEP2(IIPT,IEQ,*5000)
                              ID = IIPT - KPT
                              CALL NEXTLET (IIPT,IEQ)
C
                              IF(KCHAR(IIPT) .NE. 85 .OR. KCHAR(IIPT+1)
     C                           .NE. 85)
     C                                THEN
                                         CALL NEXSEP2(IIPT,IEQ,*5000)
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
                             CALL NEXSEP2(IIPT,IEQ,*5000)
                             CALL NEXPRT2(IIPT,IEQ,*5000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*5000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*5000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*5000)
                                       ID=IIPT-K
                                    END IF
                              END IF
                          END IF
C
                   END IF
C
C                       99 LALALA
C                       -- ------
C
                        CALL NEXPRT2( IPT,IEQ,*5000)
                        CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                       CHECK VALIDITY OF LALALA
C
                        IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                              KERR = 4
                              CALL ERRSTA(5,1,IFIRST,NUMERR)
                              GO TO 5000
                           END IF
C
C                       QC LOLOLOLO
C                       -- --------
C
                        CALL NEXPRT2( IPT,IEQ,*5000)
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
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                              KERR=4
                              CALL ERRSTA(5,1,IFIRST,NUMERR)
                              GO TO 5000
                           END IF
C
C                       MMM ULA ULO
C                       --- --- ---
C
                        CALL NEXPRT2(IPT,IEQ,*5000)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                              KDEC(5),KDEC(6))
                        IF(KDEC(5).EQ.MINDIC) RETURN
                        IF(KDEC(6).EQ.MINDIC) RETURN
C   
                        CALL MARDSEN(KDEC(5),KDEC(6),KINT(9),IRET)
C
                        IF(IRET .NE. 0)
     1                      THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) RETURN
C
C    --------------            WRONG CODE FOR MARSDEN SQUARE    ------
C
                               KERR = 4
                               CALL ERRSTA(5,1,IFIRST,NUMERR)
                              GO TO 5000
C
                            END IF
C
C                      hohohohoim ( height of mobile platform)
C                      ---------------------------------------
C                        HOHOHOHO - KINT(699)
C                        IM       - KINT(700)
C
                       IF(KDEC(4).EQ.137) THEN
                          CALL NEXTPRT(IPT,IEQ)
                          CALL EXTGRP(IPT,4,1,0,0,0,699,IRET)
                       END IF
C
                    ELSE
C
C                       THIS IS NOT A TEMP REPORT
C
                        KERR = 3
                        NOER(5,KERR) = NOER(5,KERR) + 1
                              GO TO 5000
                END IF
             END IF
C
C     RETAIN STATION IDENTIFIER IN CHARACTER FORM ( IF ANY EXISTS )
C
      IF ( ID.NE.0 )
     C    THEN
              IF (ID.GT.5) ID = 5
              J = KPT
              K = 12 + ID -1
              DO 800 I=12,K
C
C                  IF LETTER ENCOUNTERED FOR LAND STATION CONVERT
C                  IT TO FIGURE (NOT FOR SHIP'S CALL SIGN)
C
                   IF(KDEC(4) .EQ. 35) CALL LETFIG(KCHAR(J))
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C     CHECK VALUES OF YY GG FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4).GE.35 )
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
                      KERR = 3
                      CALL ERRSTA(5,1,IFIRST,NUMERR)
                      GO TO 5000
                   END IF
           END IF
C
C
C
C     -----------------------------------------------------------------
C*          3. SECTION 2  (STANDARD ISOBARIC SURFACES).
C              ----------------------------------------
2000  CONTINUE
C
C     'NIL' REPORTS WILL NOT BE PROSESSED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
C
C
C     GET THE POINTER TO THE FIRST LEVEL
C
      CALL NEXPRT2(IPT,IEQ,*5000)
      LPT = IPT
C
C
C
      LEVPRE=-1
      NOWIND=0
      NUMFIX = 0
C
2100  CONTINUE
C
C     PIPI HIHIHI
C     ---- ------
C
      LEVOLD = LEVPRE
C
      CALL NEXTPRT(IPT,IEQ)
      IF(IPT .GE. IEQ) GO TO 5000
C
      ILEV1 = KCHAR(IPT) - 48
      ILEV2 = KCHAR(IPT+1) - 48
C
      IF(ILEV2 .EQ. -12 .AND. LEVPRE .GE. 0)
     C   THEN
C
C           REJECT THE PREVIOUS LEVEL
C
            DO 2110 I = IPOS,IPOS+6
            KINT(I) = MINDIC
2110        CONTINUE
         END IF
C
C     CHECK IF THESE CHARACTERS ARE NOT FIGURES
C
      IF(ILEV1 .LT. 0 .OR. ILEV1 .GT. 9 .OR.
     C   ILEV2 .LT. 0 .OR. ILEV2 .GT. 9 )
     C      THEN
               ILEVP = -1
               GO TO 2120
            END IF
C
C     CHECK IF THIS IS THE BEGINNING OF SECTION
C     3 OR 4 (88---,77---, OR 66---)
C
      ILEV = 10*ILEV1 + ILEV2
      IF(ILEV .EQ. 88) GO TO 3000
      IF(ILEV .EQ. 77) GO TO 4000
      IF(ILEV .EQ. 66) GO TO 4000
cpsnov02 Check first for section 7, which we decode in UKINT only,
c        and for section 9 and 10 which we do not decode at all.
      call extval(ipt,5,ival)
      if((ival.eq.31313) .or.
     *     (ival.eq.51515) .or. (ival.eq.52525) .or.
     *     (ival.eq.53535) .or. (ival.eq.54545) .or.
     *     (ival.eq.55555) .or. (ival.eq.56565) .or.
     *     (ival.eq.57575) .or. (ival.eq.58585) .or.
     *     (ival.eq.59595) .or. 
     *     (ival.eq.61616) .or. (ival.eq.62626) .or.
     *     (ival.eq.63636) .or. (ival.eq.64646) .or.
     *     (ival.eq.65656) .or. (ival.eq.66666) .or.
     *     (ival.eq.67676) .or. (ival.eq.68686) .or.
     *     (ival.eq.69696)) then 
         go to 5000
      endif
cps
C
C     CHECK IF THIS IS /////-GROUP (E.G. GROUP OF 'MISSING WIND')
C
      IF((KCHAR(IPT  ) .EQ. 47) .AND. (KCHAR(IPT+1) .EQ. 47) .AND.
     C   (KCHAR(IPT+2) .EQ. 47) .AND. (KCHAR(IPT+3) .EQ. 47) .AND.
     C   (KCHAR(IPT+4) .EQ. 47))
     C      THEN
               CALL NEXSEP2(IPT,IEQ,*5000)
               GO TO 2100
            END IF
C
C     SOME STATIONS ARE REPORTING AN EXTRA LEVELS  (NOT
C     STANDARD ISOBARIC SURFACES). THIS LEVEL DATA ARE ELIMINATED.
C
      IF(ILEV .LT. 10)
     C   THEN
            CALL NEXSEP2(IPT,IEQ,*5000)
            CALL NEXPRT2(IPT,IEQ,*5000)
            CALL NEXSEP2(IPT,IEQ,*5000)
            CALL NEXPRT2(IPT,IEQ,*5000)
            CALL NEXSEP2(IPT,IEQ,*5000)
            GO TO 2100
         END IF
C
C     SOME S. AFRICA STATIONS ARE REPORTING NON STANDARD LEVEL
C     OF 15 MB INFORMATION. SKIP GROUPS FOR THAT LEVEL.
C
      IF(ILEV .EQ. 15) THEN
            CALL NEXSEP2(IPT,IEQ,*5000)
            CALL NEXPRT2(IPT,IEQ,*5000)
            CALL NEXSEP2(IPT,IEQ,*5000)
            CALL NEXPRT2(IPT,IEQ,*5000)
            CALL NEXSEP2(IPT,IEQ,*5000)
            GO TO 2100
         END IF
C
C     DETERMINE THE LEVEL
C
      CALL DETLEV(3,ILEV,ILEVP)
C
C     ILEVP IS USED AS AN LEVEL-INDICATOR OF THE PARTICULAR
C     LEVEL THIS TIME ROUND.
C
C           ILEVP = 0  FOR 70MB
C                 = 1  FOR 50MB
C           ---------------------
C                 = 9  FOR  1MB
C                 < 0  IF THIS WASNT A PRESSURE GROUP
C
C     LEVPRE IS USED AS AN INDICATOR OF THE LEVEL REACHED
C     SO FAR BEFORE THIS LEVEL.
C
C        IF LEVELS ARE OUT OF SEQUENCE OR THERE IS SOME OTHER
C        FORMAL CODING ERROR, SO THAT THE NEXT PRESSURE/HEIGHT
C        GROUP WAS NOT FOUND, AN ATTEMPT IS MADE TO FIX THE
C        ERROR AND FIND CORRECT DATA LEVELS OR TROPOPAUSE
C        OR MAX. WIND.
C
2120  CONTINUE
C
      MORE = 0
C
C
      IF(ILEVP .LE. LEVPRE .OR. ILEVP .LT. 0)
     1       THEN
                KERR = 8
C
C
C               TRY TO FIX THE PHASE ERROR
C
                NUMFIX = NUMFIX + 1
                IF(NUMFIX .GT. 3) GO TO 5000
C
C
C
                CALL FIXPHAS(LEVPRE,NOWIND,MORE,IFIX,LEVFIX,*3000,*4000)
C
                IF(IFIX .GT. 0)
     C             THEN
                      CALL ERRSTA(5,2,IFIRST,NUMERR)
                      IPOS = 17 + LEVFIX*7
                      KINT(IPOS) = IFIX
                      CALL EXTGRP(IPT,3,0,0,0,0,IPOS+1,IRET)
c
c                     fix in case ilevp = -1
c
                      ilevp=levfix
c                      
                      GO TO 2300
                   END IF
C
                CALL ERRSTA(5,2,IFIRST,NUMERR)
C
                IF(NUMERR .EQ. 1)
     C             THEN
                      IF(LEVPRE .EQ. -1) IREMPOS = 17
                      IF(LEVPRE .NE. -1) IREMPOS = IPOS
                   END IF
C
                IF(MORE .GE. 2) GO TO 2100
C
C              IF NO FIX FOUND REJECT THE PREVIOUS LEVEL
C
               IF(LEVPRE .GE. 0 .AND. IPOS .GT. 17)
     C            THEN
                     DO 2150 J = IPOS , IPOS+6
                     KINT(J) = MINDIC
2150                 CONTINUE
                  END IF
C
                GO TO 5000
             END IF
C
C
      IPOS = 17 + ILEVP*7
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
                IF(IRET .NE. 0)
     1             THEN
                      KERR = 5
                      CALL ERRSTA(5,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = IPOS
                      CALL NEXSEP2(IPT,IEQ,*5000)
                      GO TO 2100
                  END IF
C
2300  CONTINUE
C
C     TITI TAI DIDI
C     ---- --- ----
      CALL NEXTPRT(IPT,IEQ)
      IF(IPT .GE. IEQ) GO TO 5000
C
C     CHECK IF THE GROUP IS 88999, 77999 OR 66999
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 88999 .OR. IVAL .EQ. 77999 .OR. IVAL .EQ. 66999)
     C   GO TO 2100
C
      CALL EXTGRP(IPT,2,1,2,0,0,IPOS+2,IRET)
C
      IF(IRET .NE. 0)
     1   THEN
            KERR = 6
            CALL ERRSTA(5,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS + 2
            DO 2250 I=IPOS,IPOS+6
            KINT(I) = MINDIC
2250        CONTINUE
            CALL NEXSEP2(IPT,IEQ,*5000)
            GO TO 2100
         END IF
C
C     REJECT IF THE TEMPERATURE IS POSITIVE
C
            ISIGN = 2*(KINT(IPOS+3)/2)
            IF(ISIGN .EQ. KINT(IPOS+3))
     C         THEN
                  KERR = 6
                  CALL ERRSTA(5,1,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
                     DO 2310 I = IPOS+2,IPOS+4
                     KINT(I) = MINDIC
2310                 CONTINUE
               END IF
C
C     DIDI FIFIFI
C     ---- ------
C
      CALL NEXPRT2(IPT,IEQ,*5000)
C
C     CHECK IF THE NEXT GROUP IS WIND OR PRESSURE/HEIGHT
C     OF NEXT LEVEL OR BEGINNING OF NEXT SECTION
C
      CALL WINDHEI(ILEVP,NOWIND,IWIND,ITROPO,MAXWIN,IEND)
C
      IF(IEND .EQ. 1) GO TO 5000
C
      IF(ITROPO .EQ. 1)
     1      THEN
C           ----- TROPOPAUSE SECTION FOUND  ----
               GO TO 3000
            END IF
C
      IF(MAXWIN .EQ. 1)
     1      THEN
C           ----- MAX.WIND SECTION FOUND  ----
               GO TO 4000
            END IF
C
      IF(NOWIND .GE. 2)
     C      THEN
               LEVPRE = ILEVP
               GO TO 2100
            END IF
C
      IF(IWIND .EQ. 1)
     1      THEN
C           ----- WIND GROUP FOUND  ----
               IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
               GO TO 2400
            END IF
C
      IF(IWIND .EQ. 0)
     1      THEN
C           ----- PRESSURE/HEIGHT GROUP FOUND  ----
               IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
               GO TO 2100
            END IF
C
      IF(IWIND .EQ. -1)
     1     THEN
C          -----CORRUPTED WIND GROUP FOUND  ----
              CALL NEXSEP2(IPT,IEQ,*5000)
              KERR = 7
              CALL ERRSTA(5,1,IFIRST,NUMERR)
              IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
              IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
              GO TO 2100
           END IF
C
C
      IF(IWIND .EQ. -2)
     1     THEN
C          ----- '/////' - WIND GROUP FOUND  ----
              CALL NEXSEP2(IPT,IEQ,*5000)
              IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
              GO TO 2100
           END IF
C
      IF(IWIND .EQ. -3)
     1     THEN
C          ----- FORMAL ERROR IN GROUP ----
              KERR=7
              CALL ERRSTA(5,1,IFIRST,NUMERR)
              IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
              DO 2370 I=IPOS,IPOS+6
              KINT(I) = MINDIC
2370          CONTINUE
              CALL NEXSEP2(IPT,IEQ,*5000)
              GO TO 2100
           END IF
C
C
            IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
C
      IF(NOWIND .GE. 2) GO TO 2100
C
C
2400  CONTINUE
C
C     EXTRACTING THE WIND GROUP
C
C     DONT EXTRACT IF THE PRESSURE GROUP IS MISSING
C
      IF(KINT(IPOS) .EQ. MINDIC)
     C   THEN
            CALL NEXPRT2(IPT,IEQ,*5000)
            CALL NEXSEP2(IPT,IEQ,*5000)
            GO TO 2100
         END IF
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+5,IRET)
      NOWIND=0
C
      IF(IRET .NE. 0) THEN
                          KERR = 7
                          CALL ERRSTA(5,1,IFIRST,NUMERR)
                          IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
                          CALL NEXSEP2(IPT,IEQ,*5000)
                      END IF
C
C     GO TO FIND THE NEXT LEVEL
C
      GO TO 2100
C
C
C
C
C     ----------------------------------------------------------------
C*          4. SECTION 3 (TROPOPAUSE LEVELS).
C              ------------------------------
3000  CONTINUE
C
C
      IPOS = 108
C
C     THREE POSSIBLE TROPOPAUSES
C     ----- -------- -----------
C
      DO 3500 I=1,3
C
      CALL NEXPRT2(IPT,IEQ,*5000)
      IREMOVE = 0
      IPOS = IPOS + 7
C
3100  CONTINUE
C
C     CHECK IF THIS IS MAX WIND  (I.E. 77---, 66---)
C
      IF(((KCHAR(IPT) .EQ. 54) .AND. (KCHAR(IPT+1) .EQ. 54)) .OR.
     C  ((KCHAR(IPT) .EQ. 55) .AND. (KCHAR(IPT+1) .EQ. 55))) GO TO 4000
C
C     CHECK THAT THIS IS TROPOPAUSE  (I.E. 88---)
C
      IF(KCHAR(IPT) .EQ. 56 .AND. KCHAR(IPT+1) .EQ. 56)
     1   THEN
            CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
            IF(IRET .NE. 0)
     C         THEN
                  KERR = 9
                  CALL ERRSTA(5,1,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
                  CALL NEXSEP2(IPT,IEQ,*5000)
                  GO TO 3300
                END IF
C
C           CHECK THAT GROUP WAS NOT 88999  ( = "NO TROPOPAUSE")
C
            IF(KINT(IPOS+1) .NE. 999)
     1         THEN
C
C                  TTTT TAT DTDT
C                  ---- --- ----
                   CALL NEXTPRT(IPT,IEQ)
                   IF(IPT .GE. IEQ) GO TO 5000
C
C                   CHECK IF THE NEXT GROUP IS 88999, 77999 OR 66999
C
                    CALL EXTVAL(IPT,5,IVAL)
                    IF(IVAL .EQ. 88999 .OR. IVAL .EQ. 77999 .OR.
     C                 IVAL .EQ. 66999) GO TO 3500
C
                   CALL EXTGRP(IPT,2,1,2,0,0,IPOS+2,IRET)
C
                   IF(IRET .NE. 0)
     C                THEN
                         KERR = 10
                         CALL ERRSTA(5,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS + 2
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 3300
                      END IF
C
C                  REJECT IF THE TEMPERATURE IS POSITIVE
C
                         ISIGN = 2*(KINT(IPOS+3)/2)
                         IF(ISIGN .EQ. KINT(IPOS+3))
     C                      THEN
                               KERR = 10
                               CALL ERRSTA(5,1,IFIRST,NUMERR)
                               IF(NUMERR .EQ. 1) IREMPOS = IPOS
                               DO 3150 J = IPOS+2,IPOS+4
                               KINT(J) = MINDIC
3150                           CONTINUE
                            END IF
C
C                  DTDT FTFTFT
C                  ---- ------
C
                   CALL NEXTPRT(IPT,IEQ)
                   IF(IPT .GE. IEQ) GO TO 5000
C
C                  CHECK IF THIS IS NEXT TROPOPAUSE
C                  OR MAX. WIND LEVEL.
C
                   CALL EXTVAL(IPT,2,IVAL)
C
                   IF(IVAL .EQ. 88) GO TO 3500
C
                   IF(IVAL .EQ. 77 .OR. IVAL .EQ. 66) GO TO 4000
C
                   CALL EXTGRP(IPT,2,3,0,0,0,IPOS+5,IRET)
C
                   IF(IRET .NE. 0)
     C                THEN
                         KERR = 11
                         CALL ERRSTA(5,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 3300
                      END IF
C
                       IF(KINT(IPOS+5) .GT. 36 .AND.
     C                    KINT(IPOS+5) .NE. MINDIC)
     1                    THEN
                             KERR = 11
                             CALL ERRSTA(5,1,IFIRST,NUMERR)
                             GO TO 3300
                          END IF
C
                   if(KINT(IPOS+5).eq.0.and.KINT(IPOS+6).eq.0) then
                      KINT(IPOS+5)=mindic
                      KINT(IPOS+6)=mindic
                   end if
C
                   CALL NEXTPRT(IPT,IEQ)
                   IF(IPT .GE. IEQ) GO TO 5000
               ELSE
                   GO TO 3500
               END IF
         ELSE
            GO TO 3300
         END IF
C
      GO TO 3500
C
C
3300        CONTINUE
C
C           ERROR IN DATA ---  REMOVE THE PREVIOUS TROPOPAUSE BECAUSE
C                              IT MIGHT HAVE BEEN CORRUPTED
C
                            DO 3400 J=IPOS,IPOS+6
                            KINT(J) = MINDIC
3400                        CONTINUE
C
                            KERR = 9
                            CALL ERRSTA(5,1,IFIRST,NUMERR)
                            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                            CALL NEXSEP2(IPT,IEQ,*5000)
                            CALL NEXPRT2(IPT,IEQ,*5000)
C
                            GO TO 3100
C
3500  CONTINUE
C     ----------------------------------------------------------------
C*          5. SECTION 4 (MAX WIND LEVELS AND VERTICAL WIND SHEAR).    *
C              ----------------------------------------------------
4000  CONTINUE
C
C     THREE POSSIBLE MAX WIND LEVELS
C     ----- -------- --- ---- ------
C
      IPOS = 129
C
      DO 4500 I=1,3
C
      CALL NEXPRT2(IPT,IEQ,*5000)
      IREMOVE = 0
      IPOS = IPOS + 7
C
4100  CONTINUE
C
cpsnov02 Check first for section 7, which we decode in UKINT only,
c        and for section 9 and 10 which we do not decode at all.
      call extval(ipt,5,ival)
      if((ival.eq.31313) .or.
     *     (ival.eq.51515) .or. (ival.eq.52525) .or.
     *     (ival.eq.53535) .or. (ival.eq.54545) .or.
     *     (ival.eq.55555) .or. (ival.eq.56565) .or.
     *     (ival.eq.57575) .or. (ival.eq.58585) .or.
     *     (ival.eq.59595) .or. 
     *     (ival.eq.61616) .or. (ival.eq.62626) .or.
     *     (ival.eq.63636) .or. (ival.eq.64646) .or.
     *     (ival.eq.65656) .or. (ival.eq.66666) .or.
     *     (ival.eq.67676) .or. (ival.eq.68686) .or.
     *     (ival.eq.69696)) then 
         go to 5000
      endif
cps
C     CHECK THAT THIS IS MAX WIND (I.E. 77--- OR 66---)
C
      IF((KCHAR(IPT) .EQ. 55 .AND. KCHAR(IPT+1) .EQ. 55) .OR.
     1   (KCHAR(IPT) .EQ. 54 .AND. KCHAR(IPT+1) .EQ. 54))
     2     THEN
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
                   IF(IRET .NE. 0)
     C                THEN
                         KERR = 12
                         CALL ERRSTA(5,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 4300
                      END IF
C
C              CHECK THAT GROU IS NOT 77999 (='NO MAX WINDS)
C
               IF(KINT(IPOS+1) .EQ. 999) GO TO 5000
C
C              DMDM FMFMFM
C              ---- ------
C
               CALL NEXTPRT(IPT,IEQ)
               IF(IPT .GE. IEQ) GO TO 5000
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
                   IF(IRET .NE. 0)
     C                THEN
                         KERR = 13
                         CALL ERRSTA(5,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS + 2
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 4300
                      END IF
C
                   IF(KINT(IPOS+2) .GT. 36 .AND.
     C                KINT(IPOS+5) .NE. MINDIC)
     C                THEN
                         KERR = 13
                         CALL ERRSTA(5,1,IFIRST,NUMERR)
                         GO TO 4300
                      END IF
C
                   CALL NEXTPRT(IPT,IEQ)
                   IF(IPT .GE. IEQ) GO TO 5000
C
C              4 VBVBVAVA
C              - --------
C
C              CHECK IF THIS IS WIND SHEAR GROUP  (I.E. 4----)
C
               IF(KCHAR(IPT) .EQ. 52)
     1            THEN
                      CALL EXTGRP(IPT,1,2,2,0,0,IPOS+4,IRET)
C
                      IF(IRET .NE. 0)
     C                   THEN
                           KERR = 14
                           CALL ERRSTA(5,1,IFIRST,NUMERR)
                           IF(NUMERR .EQ. 1) IREMPOS = IPOS + 4
                           CALL NEXSEP2(IPT,IEQ,*5000)
                           GO TO 4300
                         END IF
C
                      CALL NEXTPRT(IPT,IEQ)
                      IF(IPT .GE. IEQ) GO TO 5000
                  ELSE
C
C                     CHECK IF THIS IS THE BEGINNING
C                     OF NEXT MAX WIND
C
                      IF(((KCHAR(IPT)   .EQ. 54) .AND.
     1                    (KCHAR(IPT+1) .EQ. 54))
     2                  .OR.
     3                   ((KCHAR(IPT)   .NE. 55) .OR.
     4                    (KCHAR(IPT+1) .EQ. 55)))
     5                    GO TO 4500
                      GO TO 4300
                  END IF
          ELSE
                  GO TO 4300
          END IF
C
      IF(IPT .GE. IEQ) GO TO 5000
C
      GO TO 4500
C
C
4300        CONTINUE
C
C           ERROR IN DATA ---  REMOVE THE PREVIOUS MAX WIND BECAUSE
C                              IT MIGHT HAVE BEEN CORRUPTED
C
                            DO 4350 J=IPOS,IPOS+6
                            KINT(J) = MINDIC
4350                        CONTINUE
                            KERR = 12
                            CALL ERRSTA(5,1,IFIRST,NUMERR)
                            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                            CALL NEXSEP2(IPT,IEQ,*5000)
                            CALL NEXPRT2(IPT,IEQ,*5000)
C
                            GO TO 4100
C
4500  CONTINUE
C
C     ERROR IF NOT END OF REPORT
C
5000  CONTINUE
C
C     ERROR CHECKING AND HANDLING
C
C     RETURN IF NO ERRORS IN REPORT
C
      IF(KERR .EQ. 0) RETURN
C
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
                             IPT=KEEP
                             CALL FIXUS
                             IFIRST=1
                             NUMRERR(5) = NUMRERR(5) + 1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
                             DO 25500 I=KEEP,IGS
                             KCHAR(I) = IAND(KCHAR(I),127)
25500                        CONTINUE
C
C                            ONLY REPORTS WITH ERROR IN DATA/TIME
C                            OR LAT/LONG ARE NOT PROSESSED ANY FURTHER
C
C                            IF THERE ARE MORE THAN 5 NON-FATAL ERRORS
C                            IN REPORT IT IS ONLY PROSESSED UNTIL THE
C                            FIRST ERROR
C
                             IF(NUMERR .GT. 5)
     C                          THEN
                                   DO 26000 I = IREMPOS,700
                                   KINT(I) = MINDIC
26000                              CONTINUE
                                END IF
C
                             IF(KERR .GT. 4) KERR=0
                             RETURN
                          END IF
C
9000  CONTINUE
      RETURN
      END
      SUBROUTINE USDEC(IHEAD,IERR)
C**** *USDEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE TEMP REPORT PART A FROM INTERMEDIATE FORMAT
C         TO FULL DECODED FORMAT.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *USDEC(IHEAD,IERR)*
C
C          INPUT    : REPORT IN INTERMEDIATE FORMAT IN 'KINT'.
C
C          OUTPUT   : REPORT WRITTEN TO DECODED REPORTS FILE.
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
      INCLUDE 'printre.f'
C
      DATA IROU/0/
      DATA IPRNUM /0/
C
C
C     ------------------------------------------------------------------
C*          1.  SET FLAGS AND INDICATORS.
C                ------------------------
 100  CONTINUE
C
C      CALL PRTKINT(KINT,1,200,MINDIC)
C
C     DEFINE 'NIL' INDICATOR
C
      NIL = IAND(KDEC(20),1)
C
C     CLEAR ERROR INDICATOR
C
      KERR = 0
C
C     SET DECODED REPORT TO MISSING DATA VALUE
C
      DO 101 I=25,700
      KDEC(I)=MINDIC
 101  CONTINUE
C
C
C     -----------------------------------------------------------------
C*          2. SECTION 1.
C              -----------
 200  CONTINUE
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
C*          2.1 DAY AND TIME .
C               --------------
 210  CONTINUE
C
      KDEC(1)=KINT(1)
      KDEC(2)=KINT(2)
      KDEC(9)=0
C
C
C*          2.2 CHECK IF SHIP OR DROP TEMP.
C               ---------------------------
 220  CONTINUE
C
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
C
C        LAT & LONG
C
         CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     1               KDEC(5),KDEC(6))
         IF(KDEC(5).EQ.MINDIC) RETURN
         IF(KDEC(6).EQ.MINDIC) RETURN   
C        ALTIT=0, FOR SEA STATION
C
         IF(KDEC(4) .EQ. 36) KDEC(8)=0
C
      END IF
C
C
C     -----------------------------------------------------------------
C*          2.3 IDENT   STATION/SHIP/AIRCRAFTIDENTIFICATION.
C               --------------------------------------------
 230  CONTINUE
C
      KDEC(7)=88
C
      IF(KDEC(4).EQ.137) THEN
         IF(KINT(699).NE.MINDIC.AND.KINT(700).NE.MINDIC) THEN
            IF(KINT(700).GE.1.AND.KINT(700).LE.4) KDEC(8)=KINT(699)
            IF(KINT(700).GE.5.AND.KINT(700).LE.8) THEN
                KDEC(8)=NINT(KINT(699)*.3)
            END IF
         END IF
      END IF
C
 233  CONTINUE
C
C
C
C*          2.4 REPORT LENGTH.
C               --------------
      KDEC(24) = 426
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
C     IF DATE INCORRECT REJECT THE REPORT BECAUSE
C     IT IS NOT KNOWN IF WIND IS IN M/S OR KNOTS
C
      IF(KDEC(1) .LT. 0 .OR. KDEC(1) .GT. 31) RETURN
C
C
C     'NIL'-REPORTS WILL NOT BE DECODED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
      IROU = IROU + 1
C
C     ---------------------------------------------------------------
C*          3.  SECTION 2   PRESSURE LEVELS.
C               ----------------------------
 300  CONTINUE
C
C     P0P0P0   PRESSURE AT STATION LEVEL IN TENTHS OF HECTOPASCAL
C
      IF(KINT(18) .NE. MINDIC) THEN
C
         KDEC(25) = KINT(18)*10
C
C        CHECK IF THOUSAND-DIGIT IS OMITTED
C
         IF(KDEC(25) .LT. 3000)  KDEC(25) = KDEC(25) + 10000
C
      END IF
C
C     T0T0    TEMPERATURE AT STATION LEVEL IN TENTHS OF DEGREE
C
      CALL IC3931(KINT(20),KINT(19),MINDIC,KDEC(26))
C
C     TDTD   DEW-POINT TEMPERATURE AT STATION LEVEL
C
      IF(KDEC(26) .NE. MINDIC)
     1  CALL IC0777(KINT(21),KDEC(26),MINDIC,KDEC(27))
C
C     DD & FFF  WIND DIRECTION AND SPEED
C
C
      CALL DDFFF(KINT(22),KINT(23),IWW,K,MINDIC,KDEC(28),KDEC(29))
C
      LEVPRE = -999999
      INDPRE = 30
C
      DO 301 I=1,11
C
            IND=(I-1)*7+24
            IND2=(I-1)*5+30
            IF(KINT(IND) .EQ. MINDIC) GO TO 301
C
C           PIPI HIHIHI    HEIGHT AT LEVEL I
C
            LEV = KINT(IND) * 10
            IF(LEV.EQ.920) LEV=925
C
            CALL GEOPOTE(LEV,KINT(IND+1),MINDIC,KDEC(IND2))
C
cc      print*,' usdec- lev,levpre,KDEC(IND2) ',lev,levpre,KDEC(IND2)
            IF(KDEC(IND2) .EQ. MINDIC) GO TO 301
C
C           CHECK IF THIS LEVEL IS LOWER OR SAME AS PREVIOUS.
C           IF IT IS THE SAME REJECT THIS LEVEL,  IF IT IS
C           LOWER REJECT BOTH.
C
            IF(KDEC(IND2) .EQ. LEVPRE)
     C         THEN
                  KDEC(IND2) = MINDIC
                  GO TO 301
               END IF
C
            IF(KDEC(IND2) .LT. LEVPRE)
     C         THEN
                  DO 302 J=INDPRE,IND2
                  KDEC(J) = MINDIC
302               CONTINUE
                  GO TO 301
            END IF
C
            LEVPRE = KDEC(IND2)
            INDPRE = IND2
C
C           TITI     TEMPERATURE AT LEVEL I
C
            CALL IC3931(KINT(IND+3),KINT(IND+2),MINDIC,KDEC(IND2+1))
C
C
C           TDTD     DEW-POINT TEMPERATURE AT LEVEL I
C
            CALL IC0777(KINT(IND+4),KDEC(IND2+1),MINDIC,KDEC(IND2+2))
cc      print*,' usdec- KDEC(IND2+1),KDEC(IND2+2)',
cc     1       KDEC(IND2+1),KDEC(IND2+2)
C
C           DIDI FIFIFI    WIND DIRECTION & SPEED AT LEVEL I
C
            CALL DDFFF(KINT(IND+5),KINT(IND+6),IWW,K,MINDIC,
     1                 KDEC(IND2+3),KDEC(IND2+4))
C
C
301         CONTINUE
C
C      ----------------------------------------------------------------
C*          4. SECTION 3   TROPOPAUSE(S).
C              --------------------------
 400  CONTINUE
C
C
      DO 401 I=1,3
C
            IND =101 + (I-1)*7
            IND2=100 + (I-1)*5
C
C
C           88PTPTPT   PRESSURE AT TROPOPAUSE LEVEL
C
C
C           CHECK FOR MISSING DATA
C
            IF(KINT(IND) .EQ. MINDIC) GO TO 402
            IF(KINT(IND+1) .EQ. MINDIC .OR. KINT(IND+1) .EQ. 999)
     1          GO TO 401
            IF(KINT(IND+1) .EQ. 0 ) GO TO 401
C
            KDEC(IND2) = KINT(IND+1)*10
C
C
C           TTTT   TEMPERATURE AT TROPOPAUSE LEVEL
C
            CALL IC3931(KINT(IND+3),KINT(IND+2),MINDIC,KDEC(IND2+1))
C
C
C           TDTD   DEW-POINT TEMPERATURE AT TROPOPAUSE LEVEL
C
            CALL IC0777(KINT(IND+4),KDEC(IND2+1),MINDIC,KDEC(IND2+2))
C
C
C           DTDT FTFTFT   WINDS AT TROPOPAUSE
C
            CALL DDFFF(KINT(IND+5),KINT(IND+6),IWW,K,MINDIC,
     1                 KDEC(IND2+3),KDEC(IND2+4))
C
401         CONTINUE
C
C
 402  CONTINUE
C
C
C     ---------------------------------------------------------------
C*          5. SECTION 4  MAX WIND LEVEL(S).
C              ------------------------------
 500  CONTINUE
C
C
C
      DO 501 I=1,3
C
            IND = 122 + (I-1)*7
            IND2= 115 + (I-1)*5
C
C
C     77PMPMPM OR 66PMPMPM   PRESSURE AT MAX. WIND LEVEL
C
C
C           CHECK FOR MISSING DATA
C
            IF(KINT(IND) .EQ. MINDIC) GO TO 502
            IF(KINT(IND+1) .EQ. MINDIC .OR. KINT(IND+1) .EQ. 999)
     1         GO TO 502
            IF(KINT(IND+1) .EQ. 0 ) GO TO 502
C
            KDEC(IND2) = KINT(IND+1)*10
C
C
C     DMDM FMFMFM   WIND AT MAX. WIND LEVEL
C
            CALL DDFFF(KINT(IND+2),KINT(IND+3),IWW,K,MINDIC,
     1                 KDEC(IND2+1),KDEC(IND2+2))
C
C
C     4VBVBVAVA   VERTICAL WIND SHEAR ASSOSIATED TO THIS MAX WIND LEVEL
C
            IF(KINT(IND+4) .EQ. MINDIC) GO TO 501
C
            IF(KINT(IND+5) .NE. MINDIC) THEN
crr  change accuracy of wind by multiplying by 10.
crr              ISPD=KINT(IND+5)
              ISPD=KINT(IND+5)*10
              IF(IWW.EQ.1) CALL KTOMPSI(ISPD)
              KDEC(IND2+3) = ISPD
            END IF
C
            IF(KINT(IND+6) .NE. MINDIC) THEN
crr  change accuracy of wind by multiplying by 10.
crr               ISPD=KINT(IND+6)
               ISPD=KINT(IND+6)*10
               IF(IWW.EQ.1) CALL KTOMPSI(ISPD)
               KDEC(IND2+4) = ISPD
            END IF
C
 501        CONTINUE
C
C
 502  CONTINUE
C
C
C
C     FLAG THE REPORT TO BE 'NIL' IF THERE ARE NO DATA IN IT (I.E. IF
C     THERE ARE NO DATA AND NO CODING ERRORS)
C
      NIL2 = 1
      DO 503 I=25,KDEC(24)
      IF(KDEC(I) .EQ. MINDIC) GO TO 503
      NIL2 = 0
      GO TO 504
503   CONTINUE
C
504   CONTINUE
C
      IF(NIL2 .EQ. 1)
     C   THEN
            KDEC(20) = IOR(KDEC(20),1)
            GO TO 99800
         END IF
C
C
C     ----------------------------------------------------------------
C*          6. PRINTING OF ERRORS & REPORTS & KINT & KDEC.
C              --------------------------------------------
 600  CONTINUE
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
      DO 601 I=1,10
      IF(IBLOK .EQ. IBLOCKS(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
601   CONTINUE
C
      DO 602 I=1,100
      IF(ISTA .EQ. ISTASA(I))
     C   THEN
            IPRI = 1
            IPRIALL = 1
         END IF
 602  CONTINUE
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
      IF(IERALLA .GT. 0) IPRI = 1
C
C
C
      IF(KDEC(2) .LT. ITIMES(1) .AND. KDEC(2) .GT. ITIMES(2))
     C   THEN
            IPRI = 0
            IPRIALL = 0
         END IF
C
C
C
      IF(IPRI .GT. 0)
     C   THEN
            DO 603 I=1,IPRLIM
C===        IF(KINT(4) .EQ. IDONEA(I)) GO TO 99800
 603        CONTINUE
            IPRNUM=IPRNUM+1
            IF(IPRNUM .LE. IPRLIM) IDONEA(IPRNUM) = KINT(4)
C
            IF(KDEC(20) .GT. 1)
     C         THEN
                  IF(NIL2 .EQ. 0) CALL PRTEMP(IROU)
                  WRITE(*,10000)
               ELSE
                  IF(IPRIALL .EQ. 1)
     C               THEN
                        WRITE(*,20000) IROU
                        CALL PRTBULL(IMI,IEQ)
                        CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
                     END IF
               END IF
         END IF
C
10000 FORMAT(///)
20000 FORMAT(//2X,' PART A  NUMBER =',I5,/)
30000 FORMAT(2X,' PART A  NUMBER =',I4,1X,2(I10,1X),5X,A5,
     C       2(I10,1X),2(A10,1X))
C
C
C
C
C
99800 CONTINUE
C
C      CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
cc      CALL KDECout(KDEC,1,KDEC(24),MINDIC,kint(4))
      CALL TEMPEXP(1,IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' USDEC - after tempexp, ierr ',IERR
CTEST
      CALL MRGTEM(IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' USDEC - after mrgtem, ierr ',IERR
CTEST
      CALL QCTEMP(KDEC(4),IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' USDEC - after qctemp, ierr ',IERR
CTEST
      CALL BUFRENC(IERR)
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' USDEC - after bufrenc, ierr ',IERR
CTEST
C
99900 CONTINUE
C
C
      RETURN
      END
      SUBROUTINE USINT ( IHEAD,IERR )
C**** *USINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT TEMP REPORTS PART A FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *USINT(IHEAD,IERR)*
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
C          OUTPUT    : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                      DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION ).
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
C         ARE NOT USED E.G. WRONG PART OF TEMP).
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
      DIMENSION IPLEV(10)
C
      DATA IPLEV/
     1   92,85,70,50,40,30,25,20,15,10/
C
C
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR . KEEP POINTER TO FIRST
C              --------------------------------------------------------
C              CHARACTER OF REPORT.
C              --------------------
 100  CONTINUE
C
C     POINTER TO FIRST CHARACTER.
C
      KEEP = IPT
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      KDEC(20) = 0
C
C     ERROR INDICATOR.
C
  101 KERR = 0
      NUMERR = 0
      IREMPOS = 17
C
C     CHECK FOR 'NIL' REPORT.
C
C
      NIL=0
C
      ILN = IEQ - IPT
      IF ( ILN.LE.26 )
     C   THEN
             KERR = 1
             NIL = 1
             CALL ERRSTA(3,0,IFIRST,NUMERR)
         END IF
C
C      SET AREA FOR DECODED REPORT TO MISSING DATA VALUE.
C
cpssep06      DO 102 I=1,200
      DO 102 I=1,700
         KINT(I) = MINDIC
  102 CONTINUE
C
C     WORDS 12-16 SET 'XXX  ' . STATION/SHIP/AIRCRAFT IDENTIFIER
C     KEPT IN THESE WORDS IN CCITT 5 CHARACTERS.
C
      DO 103 I=12,14
         KINT(I) = 88
  103 CONTINUE
C
      KINT(15) = 32
      KINT(16) = 32
C
C     CLEAR DATA FOR PREVIOUS REPORT FROM DECODED REPORT HEADER AREA.
C
      DO 104 I=1,3
         KDEC(I) = MINDIC
  104 CONTINUE
C
      DO 105 I=5,9
         KDEC(I) = MINDIC
  105 CONTINUE
C
C    CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = 0
C
      KDEC(17) = MINDIC
      KDEC(22) = MINDIC
C
C
C     ----------------------------------------------------------------
C*          2.  SECTION 1 (IDENTIFICATION AND POSITION,
C               ---------------------------------------
C               THIS IS DIFFERENT FOR LAND STATIONS AND
C               ---------------------------------------
C               SHIP/AIRCRAFT.REPORTS).
C               -----------------------
1000  CONTINUE
C
C               YY GG ID
C               -- -- --
C
 1011           CONTINUE
C
                CALL NEXSEP2(IPT,IEQ,*5000)
                CALL NEXPRT2(IPT,IEQ,*5000)
                IF(KCHAR(IPT).GE.65) THEN
                                        GO TO 1011
                                     END IF
C
C               FIRST FIGURE OF THE GROUP MUST BE 0-3
C               OR 5-8 IF WIND SPEED IS GIVEN IN KNOTS,
C               OTHERWISE YYGGID GROUP IS MISSING
C
                IF ((KCHAR(IPT) .GE. 48) .AND. (KCHAR(IPT) .NE. 52)
     1            .AND. (KCHAR(IPT) .LE. 56))
     2             CALL EXTGRP ( IPT,2,2,1,0,0,1,IRET )
C
                JPT = IPT
C
C
C
C
  700 IF ( KDEC(4).EQ.35 )
     C      THEN
C
C               LAND STATION
C
C               IIIII     -   RETAIN POINTER TO IDENTIFIER
C               -----
C
                CALL NEXPRT2(IPT,IEQ,*5000)
                KPT = IPT
                ID = 5
                CALL EXTINT ( IPT,5,4 )
C
C               EXTRACT STATION LAT/LONG ETC FROM WMO MASTER FILE.
C
                CALL LOCSTAT ( IWIND,IRET )
C
C               CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                IF(IRET .EQ. 0) THEN
                        IF(NIL .EQ. 1) THEN
                           KDEC(20)=1
                           RETURN
                        END IF
                    END IF
C
                IF ( IRET.EQ.1 ) THEN
C
C                       STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                       ENTRY , SO REPORT IS IGNORED.
C
                        KERR = 2
                        NOER(3,KERR) = NOER(3,KERR) + 1
C
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
                        OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
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
cpsokt98 9901                FORMAT (1H ,a8,' TEMP NO MASTER FILE ENTRY - ',I5)
 9901                FORMAT (1H ,a12,' TEMP NO MASTER FILE ENTRY - ',I5)
c                        CALL PRTBULL(1,IGS)
C-----                  PRINT IIIII WITH NO MASTER FILE ENTRY
C
                        RETURN
                    END IF
C
                IF ( IRET.EQ.2 ) THEN
C
C                       CORRUPT NUMBER - FATAL DECODING ERROR
C
                        KPT = IABS(KPT)
                        KERR = 2
C
C                       RETURN IF REPORT CAN'T CONTAIN IDENT
C
                        IF( (KPT+5) .GE. IEQ) RETURN
C
                        CALL ERRSTA(3,2,IFIRST,NUMERR)
                        RETURN
                    END IF
C
C
                 IF(IRET .EQ. 3) THEN
C
C                      INVALID OBSERVATION TYPE
C
                       KERR = 3
                       NOER(3,KERR) = NOER(3,KERR) + 1
                       RETURN
                    END IF
C
                 IF ( IRET.EQ.4 ) THEN
C
C                   STATION NUMBER IN RANGE , BUT NO MASTER FILE
C                   ENTRY , FOR THAT OBSERVATION TYPE. 
C
c                   PRINT IIIII WITH NO MASTER FILE ENTRY
                        OPEN(UNIT=55,
     1         FILE=cstream(msys)//'/'//'unknown_temp_station.dat',
#ifdef FORTRAN90
     2         POSITION='APPEND',
#else
     2         ACCESS='APPEND',
#endif
     4         FORM='FORMATTED',
     5         STATUS='UNKNOWN'        )
C                        
                    WRITE (55,9991) ctstamp,KINT(4)
                    CLOSE(55)
                    WRITE (*,9991) ctstamp,KINT(4)
cpsokt98 9991               FORMAT (1H ,a8,' TEMP WRONG TYPE OF STATION  - ',I5)
 9991              FORMAT (1H ,a12,' TEMP WRONG TYPE OF STATION  - ',I5)
c                    CALL PRTBULL(1,IGS)
C-----              PRINT IIIII WITH NO MASTER FILE ENTRY
                 end if 
C
            ELSE
C
CTEST
       PRINT*,' USINT - Temp ship/temp drop '
CTEST
C                TEMP SHIP OR TEMP DROP
C
                 IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137)
     1              THEN
C
                        KINT(4)=0
C
C                       RETAIN POINTER TO STATION IDENTIFIER .
C
                        KPT=IMI
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
C              IF A LETTER GROUP IS FOUND CHECK THAT THIS IS NOT
C              MIMIMJMJ GROUP 'UUAA'. IN THIS CASE THE SHIP CALL
C              SIGN GROUP IS PRESUMED MISSING.
C
               IF((KCHAR(K) .EQ. 85) .AND. (KCHAR(K+1) .EQ. 85) .AND.
     C            (KCHAR(K+2) .EQ. 65) .AND. (KCHAR(K+3) .EQ. 65))
     C                   THEN
                             IBUOY = 2
                        END IF
C
               IF((KCHAR(K) .EQ. 73) .AND. (KCHAR(K+1) .EQ. 73) .AND.
     C            (KCHAR(K+2) .EQ. 65) .AND. (KCHAR(K+3) .EQ. 65))
     C                   THEN
                             IBUOY = 2
                         END IF
C
               IF((KCHAR(K) .EQ. 88) .AND. (KCHAR(K+1) .EQ. 88) .AND.
     C            (KCHAR(K+2) .EQ. 65) .AND. (KCHAR(K+3) .EQ. 65))
     C                   THEN
                             IBUOY = 2
                         END IF
C
C
               IF ( IBUOY.NE.0 )
     C             THEN
C
C                      SHIP,RIG OR PLATFORM. IDENTIFIER NO. SET TO 0
C
                       KINT(4) = 0
                         ID=0
C
                       IF (IBUOY.EQ.1)
     C                    THEN
C
C                             MOVE POINTER PAST CALL SIGN . THE FIRST
C                             CHARACTER OF THE NEXT GROUP IS
C                             LOCATED AS SOME SHIPS USE THE FULL NAME
C                             ( WHICH CAN CONSIST OF 2 WORDS ) INSTEAD
C                             OF CALL SIGN. SO IF THE NEXT GROUP DOESN'T
C                             START WITH 'UU' (WHICH IS THE BEGINNING
C                             OF MIMIMJMJ GOUP) IT MUST BE PART OF THE
C                             SHIP'S NAME USED INSTEAD OF CALL SIGN.
C
                              IIPT=K
                              CALL NEXSEP2(IIPT,IEQ,*5000)
                              ID = IIPT - KPT
                              CALL NEXTLET (IIPT,IEQ)
C
                              IF(KCHAR(IIPT) .NE. 85 .OR. KCHAR(IIPT+1)
     C                           .NE. 85)
     C                                THEN
                                         CALL NEXSEP2(IIPT,IEQ,*5000)
                                         ID = IIPT-K
                                      END IF
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
                             CALL NEXSEP2(IIPT,IEQ,*5000)
                             CALL NEXPRT2(IIPT,IEQ,*5000)
                           IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                        THEN
                                 K=IIPT
                                 KPT=K
                                 CALL NEXSEP2(IIPT,IEQ,*5000)
                                 ID=IIPT-K
C
C                               CHECK IF CALL SIGN IS SHIP NAME
C
                                 CALL NEXPRT2(IIPT,IEQ,*5000)
                            IF(KCHAR(IIPT).GE.65.AND.KCHAR(IIPT).LE.90)
     1                              THEN
                                       CALL NEXSEP2(IIPT,IEQ,*5000)
                                       ID=IIPT-K
                                    END IF
                              END IF
                          END IF
C
                             END IF
C
C                     99 LALALA
C                     -- ------
C
                      CALL NEXTPRT ( IPT,IEQ )
                      CALL EXTGRP ( IPT,2,3,0,0,0,5,IRET )
C
C                     CHECK VALIDITY OF LALALA
C
                      IF (KINT(6).LT.0.OR.KINT(6).GT.900)
     C                  THEN
C
C                          CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                           KERR = 4
                           CALL ERRSTA(3,1,IFIRST,NUMERR)
                           GO TO 5000
                        END IF
C
C                    QC LOLOLOLO
C                    -- --------
C
                     CALL NEXTPRT ( IPT,IEQ )
                     CALL EXTGRP ( IPT,1,4,0,0,0,7,IRET )
C
C                    CHECK VALIDITY OF LOLOLOLO
C
                     IF (KINT(7).NE.1.AND.KINT(7).NE.3.AND.
     C                   KINT(7).NE.5.AND.KINT(7).NE.7) IRET=1
                     IF (KINT(8).LT.0.OR.KINT(8).GT.1800)IRET=1
C
                     IF (IRET.NE.0)
     C                 THEN
C
C                         CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                          KERR = 4
                          CALL ERRSTA(3,1,IFIRST,NUMERR)
                          GO TO 5000
                       END IF
C
C                       MMM ULA ULO
C                       --- --- ---
C
                        CALL NEXTPRT(IPT,IEQ)
C
                        CALL EXTGRP(IPT,3,1,1,0,0,9,IRET)
C
C                       CHECK VALIDITY OF MMM
C
C
                        CALL IC3333(KINT(7),KINT(6),KINT(8),MINDIC,
     C                              KDEC(5),KDEC(6))
C
                        IF(KDEC(5).EQ.MINDIC) RETURN
                        IF(KDEC(6).EQ.MINDIC) RETURN   
C
                        CALL MARDSEN(KDEC(5),KDEC(6),KINT(9),IRET)
                        IF(IRET .NE. 0)
     C                     THEN
C
C                             CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                              KERR = 4
                              CALL ERRSTA(3,1,IFIRST,NUMERR)
                              GO TO 5000
                           END IF
C
C                       hohohohoim ( height of mobile platform)
C                       --------------------------------------
C                         HOHOHOHO - KINT(699)
C                         IM       - KINT(700)
C
                       IF(KDEC(4).EQ.137) THEN
                          CALL NEXTPRT(IPT,IEQ)
                          CALL EXTGRP(IPT,4,1,0,0,0,699,IRET)
                       END IF
C               
                    ELSE
C
C                       THIS IS NOT A TEMP REPORT
C
C
C                       CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                        KERR = 3
                        NOER(3,KERR) = NOER(3,KERR) + 1
                              GO TO 5000
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
C
C                  IF LETTER ENCOUNTERED FOR LAND STATION CONVERT
C                  IT TO FIGURE (NOT FOR SHIP'S CALL SIGN)
C
                   IF(KDEC(4) .EQ. 35) CALL LETFIG(KCHAR(J))
                   KINT(I) = KCHAR(J)
                   J = J + 1
  800         CONTINUE
          END IF
C
C
C     CHECK VALUES OF YY GG IW FOR SHIP REPORTS. IF AN ERROR
C     IS FOUND IN YY OR GG THE ERROR IS FATAL AS SHIP REPORTS
C     FREQUENTLY HAVE DIFFERENT TIMES FROM BULLETIN HEADER TIMES.
C
      IF ( KDEC(4).GE.35 )
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
C
C     		   CHECK IF ALREADY KNOWN TO BE 'NIL'-REPORT
C
                              IF(NIL .EQ. 1) THEN
                                 KDEC(20)=1
                                 RETURN
                              END IF
C
                       KERR = 3
                       CALL ERRSTA(3,1,IFIRST,NUMERR)
                       GO TO 5000
C
                   END IF
           END IF
C
C
C
C
C     ---------------------------------------------------------------
C*          3. SECTION 2 (STANDARD ISOBARIC SURFACES) .
C              ----------------------------------------
2000  CONTINUE
C
C     'NIL'-REPORTS WILL NOT BE PROSESSED ANY FURTHER
C
      IF(NIL .EQ. 1) RETURN
C
C     GET THE POINTER TO THE FIRST LEVEL
C
      CALL NEXPRT2(IPT,IEQ,*5000)
      LPT = IPT
C
C
      NOWIND = 0
      LEVPRE = -1
      LEVNUM = 0
C
C     99 P0P0P0
C     -- ------
C
      CALL NEXTPRT(IPT,IEQ)
      IF(IPT .GE. IEQ) GO TO 5000
C
C     CHECK VALIDITY OF 99
C
      IF(KCHAR(IPT) .EQ. 57)
     1       THEN
C
                CALL EXTGRP(IPT,2,3,0,0,0,17,IRET)
C
                IF(IRET .NE. 0)
     1             THEN
                      KERR = 5
                      CALL ERRSTA(3,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = 17
                      CALL NEXSEP2(IPT,IEQ,*5000)
                      GO TO 2100
                   END IF
C
C               T0T0 TA0 D0D0
C               ---- --- ----
C
                CALL NEXTPRT(IPT,IEQ)
                IF(IPT .GE. IEQ) GO TO 5000
                CALL EXTGRP(IPT,2,1,2,0,0,19,IRET)
C
                IF(IRET .NE. 0)
     1             THEN
                      KERR = 6
                      CALL ERRSTA(3,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = 19
                      CALL NEXSEP2(IPT,IEQ,*5000)
                   END IF
C
C               CHECK IF THIS IS WIND OR THE NEXT PRESSURE GROUP
C
                CALL NEXPRT2(IPT,IEQ,*5000)
                CALL EXTVAL(IPT,2,IVAL)
                IF(IVAL .GT. 36)
     C             THEN
                      NOWIND = NOWIND + 1
                     CALL NEXPRT2(IPT,IEQ,*5000)
                     CALL NEXSEP2(IPT,IEQ,*5000)
                      GO TO 2100
                   END IF
C
                IF(KINT(3) .EQ. MINDIC)
     C             THEN
                      IF(IVAL .EQ. 0)
     C                   THEN
C
C                           CHECK THE NEXT GROUP, IF IT STARTS WITH
C                           00 THE THIS WAS WIND GROUP.
C
                            NPT = IPT
                            CALL NEXSEP2(NPT,IEQ,*5000)
                            CALL NEXPRT2(NPT,IEQ,*5000)
                            CALL EXTVAL(NPT,2,IVAL2)
                            IF(IVAL2 .NE. 0)
     C                         THEN
                                  NOWIND = NOWIND+1
                                  GO TO 2100
                               END IF
                         END IF
                   END IF
C
C               D0D0 F0F0F0
C               ---- ------
C
C
                CALL NEXTPRT(IPT,IEQ)
                IF(IPT .GE. IEQ) GO TO 5000
                CALL EXTGRP(IPT,2,3,0,0,0,22,IRET)
C
cpsjun99 Let QCTEMP.f take care of gross limit checks!
cps                IF(IRET .NE. 0 .OR. (KINT(22) .GT. 36 .AND.
cps     C             KINT(22) .NE. MINDIC))
                IF(IRET .NE. 0)
     C             THEN
                      KERR = 7
                      CALL ERRSTA(3,1,IFIRST,NUMERR)
                       IF(NUMERR .EQ. 1) IREMPOS = 22
                      CALL NEXSEP2(IPT,IEQ,*5000)
                      GO TO 2100
                   END IF
c
cpsjun99 Commented out the below
cps                if(kint(22).eq.0.and.kint(23).eq.0) then
cps                   kint(22)=mindic
cps                   kint(23)=mindic
cps                end if
C
cpsjul06 If no section 2, i.e. do not find 99P0P0P0, we skip the report
cps      (probably this is not a Part A temp). Unfortunately some CCA's and RRA's
cps      from KWBC containing only section 9 (regional section) ends up in error file
cps      this way.
                else
                   KERR = 3
                   CALL ERRSTA(3,1,IFIRST,NUMERR)
                   GO TO 5000
            END IF
C
C
2100  CONTINUE
C
      LEVNUM = LEVNUM + 1
      LEVOLD = LEVPRE
      NUMFIX = 0
C
C     PIPI HIHIHI
C     ---- ------
C
      CALL NEXTPRT(IPT,IEQ)
      IF(IPT .GE. IEQ) GO TO 5000
C
cpsnov02 Check first for section 7, which we decode in UKINT only,
c        and for section 9 and 10 which we do not decode at all.
      call extval(ipt,5,ival)
      if((ival.eq.31313) .or.
     *     (ival.eq.51515) .or. (ival.eq.52525) .or.
     *     (ival.eq.53535) .or. (ival.eq.54545) .or.
     *     (ival.eq.55555) .or. (ival.eq.56565) .or.
     *     (ival.eq.57575) .or. (ival.eq.58585) .or.
     *     (ival.eq.59595) .or. 
     *     (ival.eq.61616) .or. (ival.eq.62626) .or.
     *     (ival.eq.63636) .or. (ival.eq.64646) .or.
     *     (ival.eq.65656) .or. (ival.eq.66666) .or.
     *     (ival.eq.67676) .or. (ival.eq.68686) .or.
     *     (ival.eq.69696)) then 
         go to 5000
      endif
cps
C
      ILEV1=IAND(KCHAR(IPT),127)
      ILEV2=IAND(KCHAR(IPT+1),127)
C
      ILEV1=ILEV1-48
      ILEV2=ILEV2-48
C
C     IF THE SECOND CHARACTER IS 'SPACE' IT MOST PROBABLY MEANS THAT
C     PREVIOUS GROUP WAS TOO LONG AND THAT LEVEL HAS TO BE DELETED
C
      IF(ILEV2.EQ.-16.AND.LEVPRE.GE.0)
     C   THEN
C
C           REJECT THE PREVIOUS LEVEL
C
            DO 2110 I = IPOS,IPOS+6
            KINT(I) = MINDIC
2110        CONTINUE
            CALL NEXSEP2(IPT,IEQ,*5000)
            GO TO 2100
         END IF
C
C     CHECK IF THESE CHARACTERS ARE NOT FIGURES
C
      IF(ILEV1 .LT. 0 .OR. ILEV1 .GT. 9 .OR.
     C   ILEV2 .LT. 0 .OR. ILEV2 .GT. 9 )
     C      THEN
               ILEVP = -1
               GO TO 2120
            END IF
C
C
C     CHECK IF THIS IS THE BEGINNING OF SECTION
C     3 OR 4 (88---,77---, OR 66---)
C
      ILEV = 10*ILEV1 + ILEV2
C
      IF(ILEV .EQ. 88) GO TO 3000
      IF(ILEV .EQ. 77) GO TO 4000
      IF(ILEV .EQ. 66) GO TO 4000
C
C     CHECK IF THIS IS /////-GROUP (E.G. GROUP OF 'MISSING WIND')
C
      IF((KCHAR(IPT  ) .EQ. 47) .AND. (KCHAR(IPT+1) .EQ. 47) .AND.
     C   (KCHAR(IPT+2) .EQ. 47) .AND. (KCHAR(IPT+3) .EQ. 47) .AND.
     C   (KCHAR(IPT+4) .EQ. 47))
     C      THEN
               CALL NEXSEP2(IPT,IEQ,*5000)
               GO TO 2100
            END IF
C
C     DETERMINE THE LEVEL
C
      CALL DETLEV(1,ILEV,ILEVP)
C
C     ILEVP IS USED AS AN LEVEL-INDICATOR OF THE PARTICULAR
C     LEVEL THIS TIME ROUND.
C
C           ILEVP = 0  FOR 1000MB
C                 = 1  FOR  925MB
C                 = 2  FOR  850MB
C           ---------------------
C                 = 10 FOR   10MB
C                 < 0  IF THIS WASNT A PRESSURE GROUP
C
C     LEVPRE IS USED AS AN INDICATOR OF THE LEVEL REACHED
C     SO FAR BEFORE THIS LEVEL.
C
C        IF LEVELS ARE OUT OF SEQUENCE OR THERE IS SOME OTHER
C        FORMAL CODING ERROR, SO THAT THE NEXT PRESSURE/HEIGHT
C        GROUP WAS NOT FOUND, THE DATA FOR THE PREVIOUS LEVEL
C        HAS TO BE DELETED.
C        IN THIS CASE THE BEGINING OF NEW 'CORRECT SEQUENCE'
C        WILL BE SEARCHED AND IF FOUND THE DECODING CONTINUES
C        FROM THAT POINT.
C
2120  CONTINUE
C
      MORE = 0
C
      IF(ILEVP .LE. LEVPRE .OR. ILEVP .LT. 0)
     1       THEN
                KERR = 8
C
C               TRY TO FIX THE PHASE ERROR
C
                NUMFIX = NUMFIX + 1
                IF(NUMFIX .GT. 3) GO TO 5000
C
C
               CALL FIXPHAS(LEVPRE,NOWIND,MORE,IFIX,LEVFIX,*3000,*4000)
C
               IF(IFIX .GT. 0)
     C            THEN
                     CALL ERRSTA(3,2,IFIRST,NUMERR)
                     IPOS = 24 + LEVFIX*7
                     KINT(IPOS) = IFIX
                     CALL EXTGRP(IPT,3,0,0,0,0,IPOS+1,IRET)
c
c                     fix in case ilevp = -1
c
                      ilevp=levfix
c                      
                     GO TO 2300
                  END IF
C
                 CALL ERRSTA(3,2,IFIRST,NUMERR)
C
                IF(NUMERR .EQ. 1)
     C             THEN
                      IF(LEVPRE .EQ. -1) IREMPOS = 24
                      IF(LEVPRE .NE. -1) IREMPOS = IPOS
                   END IF
C
                IF(MORE .GE. 2) GO TO 2100
C
C              IF NO FIX FOUND REJECT THE PREVIOUS LEVEL
C
               IF(LEVPRE .GE. 0 .AND. IPOS .GT. 24)
     C            THEN
                     DO 2150 J = IPOS , IPOS+6
                     KINT(J) = MINDIC
2150                 CONTINUE
                  END IF
C
                GO TO 5000
             END IF
C
C
C     QUITE COMMON ERROR IS TO USE P1P1=10 INSTEAD OF P1P1=00.
C     THEREFORE IF LEVNUM=1 AND THE TEMPERATURE IS POSITIVE FOR
C     P1P1=10 THIS LEVEL IS TAKEN AS 1000MB.
C
      IF(LEVNUM .EQ. 1)
     C   THEN
            IF(ILEVP .EQ. 9)
     C         THEN
                  JPT = IPT
                  CALL NEXSEP2(JPT,IEQ,*5000)
                  CALL NEXPRT2(JPT,IEQ,*5000)
                  ISIGN = IAND(KCHAR(JPT+2),127) - 48
                  ISIGN2 = 2 * ( ISIGN / 2)
                  IF(ISIGN .EQ. ISIGN2)
     C               THEN
C
C                       TEMPERATURE POSITIVE SO THIS IS 1000MB
C
                        ILEVP = 0
                     END IF
               END IF
         END IF
C
      IPOS=24+ILEVP*7
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
                IF(IRET .NE. 0)
     1             THEN
                      KERR = 5
                      CALL ERRSTA(3,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = IPOS
                      CALL NEXSEP2(IPT,IEQ,*5000)
                      GO TO 2100
                   END IF
C
C     IF ILEVP WAS CHANGED TO POINT 1000MB LEVEL INSTEAD OF 100MB
C     CHANGE THE VALUE KINT(IPOS) AS WELL.
C
      IF(LEVNUM .EQ. 1 .AND. ILEVP .EQ. 0 .AND. KINT(IPOS) .EQ. 10)
     C   KINT(IPOS) = 0
C
2300  CONTINUE
C
C     TITI TAI DIDI
C     ---- --- ----
C
      CALL NEXTPRT(IPT,IEQ)
      IF(IPT .GE. IEQ) GO TO 5000
C
C     CHECK IF THE NEXT GROUP IS 88999, 77999 OR 66999
C
      CALL EXTVAL(IPT,5,IVAL)
      IF(IVAL .EQ. 88999 .OR. IVAL .EQ. 77999 .OR. IVAL .EQ. 66999)
     C   GO TO 2100
cpsnov02 Check first for section 7, which we decode in UKINT only,
c        and for section 9 and 10 which we do not decode at all.
      call extval(ipt,5,ival)
      if((ival.eq.31313) .or.
     *     (ival.eq.51515) .or. (ival.eq.52525) .or.
     *     (ival.eq.53535) .or. (ival.eq.54545) .or.
     *     (ival.eq.55555) .or. (ival.eq.56565) .or.
     *     (ival.eq.57575) .or. (ival.eq.58585) .or.
     *     (ival.eq.59595) .or. 
     *     (ival.eq.61616) .or. (ival.eq.62626) .or.
     *     (ival.eq.63636) .or. (ival.eq.64646) .or.
     *     (ival.eq.65656) .or. (ival.eq.66666) .or.
     *     (ival.eq.67676) .or. (ival.eq.68686) .or.
     *     (ival.eq.69696)) then 
         go to 5000
      endif
cps
C
      CALL EXTGRP(IPT,2,1,2,0,0,IPOS+2,IRET)
C
                IF(IRET .NE. 0)
     1             THEN
                      KERR = 6
                      CALL ERRSTA(3,1,IFIRST,NUMERR)
                      IF(NUMERR .EQ. 1) IREMPOS = IPOS + 2
                      DO 2250 I=IPOS,IPOS+6
                      KINT(I) = MINDIC
2250                  CONTINUE
                      CALL NEXSEP2(IPT,IEQ,*5000)
                      GO TO 2100
                  END IF
C
C
C     REJECT IF THE TEMPERATURE IS POSITIVE ABOVE 400 MB
C
      IF(IRET .EQ. 0)
     C   THEN
            IF(KINT(IPOS) .LE. 40 .AND. KINT(IPOS) .NE. 0)
     C         THEN
                  ISIGN = 2*(KINT(IPOS+3)/2)
                  IF(ISIGN .EQ. KINT(IPOS+3))
     C               THEN
                        KERR = 6
                        CALL ERRSTA(3,1,IFIRST,NUMERR)
                        IF(NUMERR .EQ. 1) IREMPOS = IPOS
                           DO 2350 I = IPOS+2,IPOS+4
                           KINT(I) = MINDIC
2350                       CONTINUE
                      END IF
               END IF
         END IF
C
      IF(NOWIND .GE. 2) GO TO 2380
C
C     DIDI FIFIFI
C     ---- ------
C
      CALL NEXPRT2(IPT,IEQ,*5000)
C
C     CHECK IF THE NEXT GROUP IS WIND OR PRESSURE/HEIGHT
C     OF NEXT LEVEL OR BEGINNING OF NEXT SECTION
C
      CALL WINDHEI(ILEVP,NOWIND,IWIND,ITROPO,MAXWIN,IEND)
C
      IF(IEND .EQ. 1) GO TO 5000
C
      IF(ITROPO .EQ. 1)
     1      THEN
C           ----- TROPOPAUSE SECTION FOUND  ----
               GO TO 3000
            END IF
C
      IF(MAXWIN .EQ. 1)
     1      THEN
C           ----- MAX.WIND SECTION FOUND  ----
               GO TO 4000
            END IF
C
      IF(NOWIND .GE. 2)
     C      THEN
               LEVPRE = ILEVP
               GO TO 2100
            END IF
C
      IF(IWIND .EQ. 1)
     1      THEN
               IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
               GO TO 2400
            END IF
C
      IF(IWIND .EQ. 0)
     1      THEN
C           ----- PRESSURE/HEIGHT GROUP FOUND  ----
               IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
               GO TO 2100
            END IF
C
      IF(IWIND .EQ. -1)
     1     THEN
C          -----CORRUPTED WIND GROUP FOUND  ----
              CALL NEXSEP2(IPT,IEQ,*5000)
              KERR = 7
              CALL ERRSTA(3,1,IFIRST,NUMERR)
              IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
              IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
              GO TO 2100
           END IF
C
C
      IF(IWIND .EQ. -2)
     1     THEN
C          ----- '/////' - WIND GROUP FOUND  ----
              CALL NEXSEP2(IPT,IEQ,*5000)
              IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
              GO TO 2100
           END IF
C
      IF(IWIND .EQ. -3)
     1     THEN
C          ----- FORMAL ERROR IN GROUP ----
              KERR=7
              CALL ERRSTA(3,1,IFIRST,NUMERR)
              IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
              DO 2370 I=IPOS,IPOS+6
              KINT(I) = MINDIC
2370          CONTINUE
              CALL NEXSEP2(IPT,IEQ,*5000)
              GO TO 2100
           END IF
C
2380  CONTINUE
C
      IF(KINT(IPOS) .NE. MINDIC) LEVPRE = ILEVP
      IF(NOWIND .GE. 2) GO TO 2100
C
2400  CONTINUE
C
C     EXTRACTING THE WIND GROUP
C
C     DONT EXTRACT IF PRESSURE GROUP WAS MISSING
C
      IF(KINT(IPOS) .EQ. MINDIC)
     C   THEN
            CALL NEXPRT2(IPT,IEQ,*5000)
            CALL NEXSEP2(IPT,IEQ,*5000)
            GO TO 2100
         END IF
C
      CALL EXTGRP(IPT,2,3,0,0,0,IPOS+5,IRET)
      NOWIND=0
C
      IF(IRET .NE. 0)
     C   THEN
            KERR = 7
            CALL ERRSTA(3,1,IFIRST,NUMERR)
            IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
            CALL NEXSEP2(IPT,IEQ,*5000)
         END IF
C
C     GO TO FIND THE NEXT LEVEL
C
      GO TO 2100
C
C
C
C
C      -----------------------------------------------------------------
C*          4. SECTION 3  (TROPOPAUSE LEVELS).
C              -------------------------------
3000  CONTINUE
C
C     THREE POSSIBLE TROPOPAUSES
C     ----- -------- -----------
C
      IPOS = 94
C
      DO 3500 I=1,3
C
      CALL NEXPRT2(IPT,IEQ,*5000)
      IREMOVE = 0
      IPOS = IPOS + 7
C
cpsnov02 Check first for section 7, which we decode in UKINT only,
c        and for section 9 and 10 which we do not decode at all.
      call extval(ipt,5,ival)
      if((ival.eq.31313) .or.
     *     (ival.eq.51515) .or. (ival.eq.52525) .or.
     *     (ival.eq.53535) .or. (ival.eq.54545) .or.
     *     (ival.eq.55555) .or. (ival.eq.56565) .or.
     *     (ival.eq.57575) .or. (ival.eq.58585) .or.
     *     (ival.eq.59595) .or. 
     *     (ival.eq.61616) .or. (ival.eq.62626) .or.
     *     (ival.eq.63636) .or. (ival.eq.64646) .or.
     *     (ival.eq.65656) .or. (ival.eq.66666) .or.
     *     (ival.eq.67676) .or. (ival.eq.68686) .or.
     *     (ival.eq.69696)) then 
         go to 5000
      endif
cps
C
C     CHECK IF THIS IS MAX WIND  (I.E. 77---, 66---)
C
      CALL EXTVAL(IPT,2,IVAL)
c
      IF(IVAL .EQ. 77 .OR. IVAL .EQ. 66) GO TO 4000
C
C
C     CHECK THAT THIS IS TROPOPAUSE  (I.E. 88---)
C
      IF(IVAL .EQ. 88)
     1   THEN
            CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
            IF(IRET .NE. 0)
     C         THEN
                  KERR = 9
                  CALL ERRSTA(3,1,IFIRST,NUMERR)
                  IF(NUMERR .EQ. 1) IREMPOS = IPOS
                  CALL NEXSEP2(IPT,IEQ,*5000)
                  GO TO 3300
                END IF
C
C
C           CHECK THAT GROUP WAS NOT 88999  ( = "NO TROPOPAUSE")
C
            IF(KINT(IPOS+1) .NE. 999 .AND. KINT(IPOS+1) .NE. MINDIC)
     1         THEN
C
C                 REJECT IF PRESSURE > 900  (MOST PROPABLY THIS IS
C
                  IF(KINT(IPOS+1) .GE. 900)
     C               THEN
                        KERR = 9
                        CALL ERRSTA(3,1,IFIRST,NUMERR)
                        IF(NUMERR .EQ. 1) IREMPOS = IPOS
                        CALL NEXSEP2(IPT,IEQ,*5000)
                        GO TO 3300
                     END IF
C
C                  TTTT TAT DTDT
C                  ---- --- ----
                   CALL NEXTPRT(IPT,IEQ)
                   IF(IPT .GE. IEQ) GO TO 5000
C
C                   CHECK IF THE NEXT GROUP IS 88999, 77999 OR 66999
C
                    CALL EXTVAL(IPT,5,IVAL)
                    IF(IVAL .EQ. 88999 .OR. IVAL .EQ. 77999 .OR.
     C                 IVAL .EQ. 66999) GO TO 3500
C
                   CALL EXTGRP(IPT,2,1,2,0,0,IPOS+2,IRET)
C
                   IF(IRET .NE. 0)
     C                THEN
                         KERR = 10
                         CALL ERRSTA(3,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS + 2
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 3300
                      END IF
C
C                  REJECT IF THE TEMPERATURE IS POSITIVE ABOVE 400 MB
C
                    IF(KINT(IPOS+1) .LE. 400)
     C                THEN
                         ISIGN = 2*(KINT(IPOS+3)/2)
                         IF(ISIGN .EQ. KINT(IPOS+3))
     C                      THEN
                               KERR = 10
                               CALL ERRSTA(3,1,IFIRST,NUMERR)
                               IF(NUMERR .EQ. 1) IREMPOS = IPOS
                  DO 3150 J = IPOS+2 , IPOS+4
                  KINT(J) = MINDIC
3150              CONTINUE
                            END IF
                      END IF
C
C                  DTDT FTFTFT
C                  ---- ------
                   CALL NEXTPRT(IPT,IEQ)
                   IF(IPT .GE. IEQ) GO TO 5000
C
C                  CHECK THAT THIS IS NOT 77--- OR 66--- GROUP
C
                   IF((KCHAR(IPT) .EQ. 55 .AND. KCHAR(IPT+1) .EQ. 55)
     1             .OR. (KCHAR(IPT) .EQ. 54 .AND. KCHAR(IPT+1) .EQ. 54))
     2                GO TO 4000
C
C                  CHECK THAT THIS IS NOT THE NEXT TROPOPAUSE
C
                   CALL EXTVAL(IPT,2,IVAL)
                   IF(IVAL .EQ. 88) GO TO 3500
C
                   CALL EXTGRP(IPT,2,3,0,0,0,IPOS+5,IRET)
C
                   IF(IRET .NE. 0)
     C                THEN
                         KERR = 11
                         CALL ERRSTA(3,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS + 5
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 3300
                      END IF
C
                   IF(KINT(IPOS+5) .GT. 36 .AND.
     C                KINT(IPOS+5) .NE. MINDIC)
     1                    THEN
                             KERR=9
                             CALL ERRSTA(3,1,IFIRST,NUMERR)
                             CALL NEXSEP2(IPT,IEQ,*5000)
                             GO TO 3300
                          END IF
C
                   if(KINT(IPOS+5).eq.0.and.KINT(IPOS+6).eq.0) then
                      KINT(IPOS+5)=mindic
                      KINT(IPOS+6)=mindic
                   end if
c
                   CALL NEXTPRT(IPT,IEQ)
                   IF(IPT .GE. IEQ) GO TO 5000
                   GO TO 3500
               ELSE
                   GO TO 3500
               END IF
         ELSE
C
           GO TO 3300
C
         END IF
C
C
3300        CONTINUE
C
C           ERROR IN DATA ---  REMOVE THE PREVIOUS TROPOPAUSE BECAUSE
C                              IT MIGHT HAVE BEEN CORRUPTED
C
                            DO 3400 J = IPOS,IPOS+6
                            KINT(J) = MINDIC
3400                        CONTINUE
                            KERR = 9
                            CALL EXTVAL(IPT,5,IVAL)
                            CALL ERRSTA(3,1,IFIRST,NUMERR)
C
C                           CHECK IF THIS IS INCORRECTLY USED
C                           REGIONAL SECTION, WHICH IS PRACTICE
C                           AT LEAST IN SOME STATIONS IN REGION 4
C
                            IF(IVAL .EQ. 51515 .OR. IVAL .EQ. 52525)
     C                         GO TO 5000
C
                            IF(NUMERR .EQ. 1) IREMPOS = IPOS
C
                            CALL NEXSEP2(IPT,IEQ,*5000)
                            CALL NEXPRT2(IPT,IEQ,*5000)
C
                            GO TO 3500
C
3500  CONTINUE
C
C
C     ----------------------------------------------------------------
C*          5. SECTION 4 (MAX WIND LEVELS AND VERTICAL WIND SHEAR).
C              -----------------------------------------------------
4000  CONTINUE
C
C     THREE POSSIBLE MAX WIND LEVELS
C     ----- -------- --- ---- ------
C
      IPOS = 115
C
      DO 4500 I=1,3
C
      CALL NEXPRT2(IPT,IEQ,*5000)
      IREMOVE = 0
      IPOS = IPOS + 7
C
cpsnov02 Check first for section 7, which we decode in UKINT only,
c        and for section 9 and 10 which we do not decode at all.
      call extval(ipt,5,ival)
      if((ival.eq.31313) .or.
     *     (ival.eq.51515) .or. (ival.eq.52525) .or.
     *     (ival.eq.53535) .or. (ival.eq.54545) .or.
     *     (ival.eq.55555) .or. (ival.eq.56565) .or.
     *     (ival.eq.57575) .or. (ival.eq.58585) .or.
     *     (ival.eq.59595) .or. 
     *     (ival.eq.61616) .or. (ival.eq.62626) .or.
     *     (ival.eq.63636) .or. (ival.eq.64646) .or.
     *     (ival.eq.65656) .or. (ival.eq.66666) .or.
     *     (ival.eq.67676) .or. (ival.eq.68686) .or.
     *     (ival.eq.69696)) then 
         go to 5000
      endif
cps
C     CHECK THAT THIS IS MAX WIND (I.E. 77--- OR 66---)
C
4100  CONTINUE
C
      CALL EXTVAL(IPT,2,IVAL)
      IF(IVAL .EQ. 66 .OR. IVAL .EQ. 77)
     2     THEN
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS,IRET)
C
                   IF(IRET .NE. 0)
     C                THEN
                         KERR = 12
                         CALL ERRSTA(3,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 4300
                      END IF
C
C              CHECK THAT GROU IS NOT 77999 (='NO MAX WINDS)
C
               IF(KINT(IPOS+1) .EQ. 999) GO TO 5000
C
C              DMDM FMFMFM
C              ---- ------
C
               CALL NEXTPRT(IPT,IEQ)
               IF(IPT .GE. IEQ) GO TO 5000
               CALL EXTGRP(IPT,2,3,0,0,0,IPOS+2,IRET)
C
               IF(IRET .NE. 0 .OR. (KINT(IPOS+2) .GT. 36 .AND.
     C                              KINT(IPOS+2) .NE. MINDIC))
     C                THEN
                         KERR = 13
                         CALL ERRSTA(3,1,IFIRST,NUMERR)
                         IF(NUMERR .EQ. 1) IREMPOS = IPOS + 2
                         CALL NEXSEP2(IPT,IEQ,*5000)
                         GO TO 4300
                      END IF
C
               CALL NEXTPRT(IPT,IEQ)
               IF(IPT .GE. IEQ) GO TO 5000
C
C              4 VBVBVAVA
C              - --------
C
C              CHECK IF THIS IS WIND SHEAR GROUP  (I.E. 4----)
C
               IF(KCHAR(IPT) .EQ. 52)
     1            THEN
                      CALL EXTGRP(IPT,1,2,2,0,0,IPOS+4,IRET)
C
                      IF(IRET .NE. 0)
     C                   THEN
                           KERR = 14
                           CALL ERRSTA(3,1,IFIRST,NUMERR)
                           IF(NUMERR .EQ. 1) IREMPOS = IPOS + 4
                           CALL NEXSEP2(IPT,IEQ,*5000)
                           GO TO 4300
                         END IF
C
                      CALL NEXTPRT(IPT,IEQ)
                      IF(IPT .GE. IEQ) GO TO 5000
                      GO TO 4500
                  ELSE
C
C                     CHECK IF THIS IS THE BEGINNING
C                     OF NEXT MAX WIND
C
                      IF(((KCHAR(IPT)   .EQ. 54) .AND.
     1                    (KCHAR(IPT+1) .EQ. 54))
     2                  .OR.
     3                   ((KCHAR(IPT)   .NE. 55) .OR.
     4                    (KCHAR(IPT+1) .EQ. 55)))
     5                    GO TO 4500
                      GO TO 4300
                  END IF
          ELSE
C
C              CHECK IF THIS IS AN EXTRA (FOURTH) TROPOPAUSE
C
               IF(IVAL .EQ. 88)
     C            THEN
4200                 CONTINUE
                     CALL NEXSEP2(IPT,IEQ,*5000)
                     CALL NEXPRT2(IPT,IEQ,*5000)
                     CALL EXTVAL(IPT,2,IVAL)
                     IF(IVAL .EQ. 77 .OR. IVAL .EQ. 66) GO TO 4100
                     GO TO 4200
                  END IF
C
             GO TO 4300
C
          END IF
C
C
4300        CONTINUE
C
C           ERROR IN DATA ---  REMOVE THE PREVIOUS MAX WIND BECAUSE
C                              IT MIGHT HAVE BEEN CORRUPTED
C
C
C                           CHECK IF THIS IS INCORRECTLY USED
C                           REGIONAL SECTION, WHICH IS PRACTICE
C                           AT LEAST IN SOME STATIONS IN REGION 4
C
                            CALL EXTVAL(IPT,5,IVAL)
                            IF(IVAL .EQ. 51515 .OR. IVAL .EQ. 52525)
     C                         GO TO 5000
C
                            KERR = 12
                            CALL ERRSTA(3,1,IFIRST,NUMERR)
C
                            IF(NUMERR .EQ. 1) IREMPOS = IPOS
                            DO 4350 J = IPOS,IPOS+6
                            KINT(J) = MINDIC
4350                        CONTINUE
C
                            CALL NEXSEP2(IPT,IEQ,*5000)
                            CALL NEXPRT2(IPT,IEQ,*5000)
C
                            GO TO 4100
C
4500  CONTINUE
C
C
C
C     ERROR IF NOT END OF REPORT
C
5000  CONTINUE
C
CTEST
      IF(KERR.NE.0)
     1 PRINT*,' USINT - after 5000, kerr ',KERR
CTEST
C
C     ERROR CHECKING AND HANDLING
C
C     RETURN IF NO ERRORS IN REPORT
C
      IF(KERR .EQ. 0) RETURN
C
C
C     IF FIRST DECODING ATTEMPT, TRY TO CORRECT
C     THE ERROR AND DECODE AGAIN
C
      IF (IFIRST .EQ. 0) THEN
                             IPT=KEEP
                             CALL FIXUS
                             IFIRST=1
                             NUMRERR(3) = NUMRERR(3) + 1
                             GO TO 101
                          ELSE
                             CALL SAVREP(IHEAD,IERR)
C
C                            CLEAR PARITY BIT AFTER SAVING ERROR FILE
C
                             DO 25500 I=KEEP,IGS
                             KCHAR(I) = IAND(KCHAR(I),127)
25500                        CONTINUE
C
C                            ONLY REPORTS WITH ERROR IN DATA/TIME
C                            OR LAT/LONG ARE NOT PROSESSED ANY FURTHER
C
C                            IF THERE ARE MORE THAN 5 NON-FATAL ERRORS
C                            IN REPORT IT IS ONLY PROSESSED UNTIL THE
C                            FIRST ERROR
C
C
                             IF(NUMERR .GT. 5)
     C                          THEN
                                   DO 26000 I = IREMPOS,700
                                   KINT(I) = MINDIC
26000                              CONTINUE
                                END IF
C
C
                             IF(KERR .GT. 4) KERR=0
                          END IF
C
C
C
C
9000  CONTINUE
      RETURN
      END
      SUBROUTINE WINDHEI(ILEVP,NOWIND,IWIND,
     1                   ITROPOP,MAXWIND,IEND)
C**** *WINDHEI*
C
C
C     PURPOSE.
C     --------
C
C         CHECK THE NEXT GROUP IF IT IS WIND GROUP
C         OR PRESSURE/HEIGHT GROUP OF NEXT LEVEL.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *WINDHEI(ILEVP,NOWIND,IWIND,
C    1                   ITROPOP,MAXWIND,IEND)*
C
C          INPUT     : ILEVP  - INDICATOR OF THE PRESSURE LEVEL
C                      NOWIND - INDICATES IF THERE IS SUPPOSED
C                               TO BE WIND ON THIS LEVEL
C
C
C          OUTPUT    : NOWIND = 0 IF IT IS WIND GROUP
C                             = NOWIND+1 IF THIS IS NOT WIND GROUP,
C                             = 2 IF THERE CANT BE ANY MORE WINDS
C                      IWIND  = 1 IF WIND GROUP
C                             =-1 IF WIND GROUP BUT UNUSABLE
C                             =-2 IF WIND GROUP = /////
C                             =-3 IF GROUP IS CORRUPTED
C                             = 0 OTHERWISE
C                      ITROPOP= 1 IF BEGINNING OF TROPOPAUSE SECTION
C                             = 0 OTHERWISE
C                      MAXWIND= 1 IF BEGINNING OF MAX.WIND SECTION
C                             = 0 OTHERWISE
C                      IEND   = 1 IF END OF REPORT FOUND
C                             = 0 OTHERWISE
C
C
C     METHOD.
C     -------
C
C         THIS SUBROUTINE IS DESIGNED TO CATCH ALL THE ODDITIES
C         OF REPORTING WINDS IN TEMP PART A & C.
C         THE MAIN PROBLEM IS TO DECIDE IF THE GROUP IS WIND
C         OR PRESSURE/HEIGHT OF THE NEXT LEVEL.
C         THE TEST IS CARRIED OUT IN TWO SEPARATE PARTS
C         DEPENDING IF WIND-LEVEL INDICATOR ID (KINT(3)) IS
C         AVAILABLE OR NOT.
C         BEACUSE THIS ID IS SO FREQUENTLY INCORRECTLY USED WE
C         CANT SOLELY RELY ON IT, BUT WE HAVE TO CHECK THE
C         PRESENT GROUP AND THE NEXT GROUP TO SEE IF EITHER
C         OF THEM STARTS WITH PIPI = THE PRESSURE OF NEXT LEVEL
C         OR IF PIPI = THE PRESSURE OF SOME STANDARD LEVEL
C         HIGHER THAN THE PREVIOUS.
C
C     VARIABLES : IVAL1  = THE VALUE OF TWO FIRST FIGURES IN THIS GROUP
C     ---------   IVAL2  = THE VALUE OF TWO FIRST FIGURES IN NEXT GROUP
C                 IGROUP1= THE VALUE OF THE FIGURES IN THIS GROUP (5FIG)
C                 IGROUP2= THE VALUE OF THE FIGURES IN NEXT GROUP (5FIG)
C
C                 AT FIRST THE FOLLOWING CHECKS ARE MADE
C                 -  IVAL1 = 99 SO WIND WITH VARIABLE DIRECTION
C                 -  IVAL1 = 88 SO BEGINNING OF TROPOPAUSE SECTION
C                 -  IVAL1 = 66 OR 77 SO BEGINNING OF MAX. WIND SECTION
C                 -  IVAL2 = 66 OR 77 OR 88 SO THE NEXT GROUP IS
C                            BEGINNING OF MAX. WIND OR TROPOP. SECTION
C                            SO THIS IS WIND GROUP.
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
C          A.HOLOPAINEN  JAN.84
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
      DIMENSION LEVSTAN(24)
C
      DATA LEVSTAN/
     1  92,85,70,50,40,30,25,20,15,10, 70,50,30,20,10,9,8,7,6,5,4,3,2,1/
C
C     ---------------------------------------------------------------
C           1. GET THE VALUE OF TWO FIRST FIGURES
C              ----------------------------------
C              IN THE NEXT TWO GROUPS .
C              ------------------------
 100  CONTINUE
C
      IWIND=0
      ITROPOP=0
      IEND=0
      MAXWIND=0
C
C     CHECK IF THIS IS MISSING WIND GROUP '/////'
C
      DO 101 I=IPT,IPT+4
      KAR = IAND(KCHAR(I),127)
      IF(KAR .NE. 47) GO TO 102
101   CONTINUE
C
C     THIS IS '/////' WIND GROUP
C
      IWIND = -2
      RETURN
C
102   CONTINUE
C
C
C*          1.1 GET THE VALUE OF THE BEGINNING OF NEXT THREE GROUPS.
C               ----------------------------------------------------
 110  CONTINUE
C
      CALL EXTVAL(IPT,2,IVAL1)
      CALL EXTVAL(IPT,5,IGROUP1)
C
      IPTX = IPT
      CALL NEXSEP2(IPTX,IEQ,*111)
      CALL NEXPRT2(IPTX,IEQ,*111)
      CALL EXTVAL(IPTX,5,IGROUP2)
      CALL EXTVAL(IPTX,2,IVAL2)
C
      CALL NEXSEP2(IPTX,IEQ,*112)
      CALL NEXPRT2(IPTX,IEQ,*112)
      CALL EXTVAL(IPTX,2,IVAL3)
C
      GO TO 112
C
111   CONTINUE
C
C     THIS IS THE LAST GROUP, SO IT'S WIND
C
      IWIND = 1
      IF(IVAL1 .GT. 36) IWIND = -1
      RETURN
C
112   CONTINUE
C
C
C     RETURN IF PRESENT GROUP IS CORRUPTED
C
      IF(IGROUP1 .EQ. MINDIC)
     C   THEN
            IWIND = -3
            RETURN
         END IF
C
C     CHECK IF 99, SO ITS WIND GROUP
C
      IF(IVAL1 .EQ. 99) THEN
                           IWIND=1
                           NOWIND=0
                           RETURN
                        END IF
C
C     CHECK IF BEGINNING OF SECTION 3 OR 4 (88 OR 77 OR 66)
C
      IF(IVAL1 .EQ. 88)
     1    THEN
             CALL NEXTPRT(IPT,IEQ)
             IF(IPT .GE. IEQ) RETURN
             ITROPOP=1
             NOWIND=2
             RETURN
          END IF
C
      IF(IVAL1 .EQ. 77 .OR. IVAL1 .EQ. 66)
     1    THEN
             CALL NEXTPRT(IPT,IEQ)
             IF(IPT .GE. IEQ) RETURN
             MAXWIND=1
             NOWIND=2
             RETURN
          END IF
C
C     CHECK IF ALREADY KNOWN THAT THERE ARE NO MORE WINDS
C
      IF(NOWIND .GE. 2) RETURN
C
C     SET THE VARIABLES FOR THE TEST OF WIND AGAINST PRESSURE/HEIGHT
C
      LEVBEG = MAX0(1,ILEVP)
      LEVEND = 10
      IF(KDEC(23) .EQ. 3)
     C   THEN
            LEVBEG = MAX0(11,ILEVP+11)
            LEVEND = 24
         END IF
C
      IF(KDEC(23) .EQ. 1)
     C   THEN
            IF(ILEVP .EQ. 0)
     C         THEN
                  INDWIN = 10    ! 11
               ELSE
                  INDWIN = LEVSTAN(ILEVP)/10  !11
               END IF
         END IF
C
      IF(KDEC(23) .EQ. 3)
     C   THEN
            IF(ILEVP .EQ. 0)
     C         THEN
                  INDWIN = 7
               ELSE
                  INDWIN = LEVSTAN(ILEVP+11)/10   ! 11)/11
               END IF
         END IF
C
      ID = KINT(3)
C
      LEVNEX = -1
      LEVNEX2 = -1
C
      IF(KDEC(23) .EQ. 1)
     C   THEN
            IF(ILEVP .LE. 9) LEVNEX  = LEVSTAN(ILEVP+1)
            IF(ILEVP .LE. 8) LEVNEX2 = LEVSTAN(ILEVP+2)
         END IF
C
      IF(KDEC(23) .EQ. 3)
     C   THEN
            IF(ILEVP .LE. 13) LEVNEX  = LEVSTAN(ILEVP+12)
            IF(ILEVP .LE. 12) LEVNEX2 = LEVSTAN(ILEVP+13)
         END IF
C
C     CHECK IF THE NEXT GROUP IS TROPOP OR MAX. WIND
C
      IF(IVAL2 .EQ. 66 .OR. IVAL2 .EQ. 77 .OR. IVAL2 .EQ. 88)
     C   THEN
C
C           IT MIGHT BE, IN WHICH CASE THIS ID WIND.  CHECK
C           IF THIS IS NOT THE NEXT LEVEL OR DD>36
C
            IF(IVAL1 .NE. LEVNEX .AND. IGROUP2 .NE. MINDIC .AND.
     C         IVAL1 .LE. 36)
     C            THEN
                     IWIND = 1
                     RETURN
                  END IF
         END IF
C
C
      IF(ID .EQ. MINDIC) GO TO 300
C
C
C
C     -----------------------------------------------------------------
C*          2.  CHECK WIND AGAINST PRESS/HEIGHT,
C               IF WIND-LEVEL IND. AVAILABLE.
 200  CONTINUE
C
C     ID-INDICATOR IS NOT USEFUL ABOVE 10MB, SO IT HAS TO BE CHANGED.
C
      IF(KDEC(23) .EQ. 3 .AND. LEVNEX .LT. 10 .AND. NOWIND .EQ. 0)
     1   INDWIN=1
C
C
      IF(ID .GT. INDWIN)
     C   THEN
C
C           SUPPOSED TO BE PRESSURE.  CHECK IF IT IS THE
C           NEXT STANDARD LEVEL.
C
            IF(IVAL1 .EQ. LEVNEX)
     C         THEN
C
C                 THIS IS THE PRESSURE
C
                  NOWIND = NOWIND + 1
                  IWIND = 0
                  RETURN
               ELSE
C
C                 CHECK IF THE NEXT GROUP IS THE NEXT STANDARD LEVEL
C                 OR TROPOPAUSE OR WIND SECTION.
C
                  IF(IVAL2 .EQ. LEVNEX .OR. IVAL2 .EQ. 88 .OR.
     C               IVAL2 .EQ. 77 .OR. IVAL2 .EQ. 66)
     C               THEN
C
C                       THE NEXT GROUP IS PRESSURE, SO THIS IS WIND
C
                        IF(IVAL1 .GT. 36)
     C                     THEN
C
C                             CORRUPTED WIND
C
                              IWIND = -1
                              RETURN
                           ELSE
                              IWIND = 1
                              RETURN
                           END IF
                     ELSE
C
C                       CHECK IF THIS IS ONE OF THE STANDARD LEVELS
C
                        DO 201 I = LEVBEG , LEVEND
                        IF(IVAL1 .EQ. LEVSTAN(I))
     C                     THEN
C
C                             THIS IS PRESSURE
C
                              IWIND = 0
                              NOWIND = NOWIND + 1
                           END IF
201                     CONTINUE
C
C                       CHECK IF THE NEXT IS ONE OF THE STANDARD LEVELS
C
                        DO 202 I = LEVBEG , LEVEND
                        IF(IVAL2 .EQ. LEVSTAN(I))
     C                     THEN
C
C                             THIS IS WIND
C
                              IF(IVAL1 .GT. 36)
     C                           THEN
                                    IWIND = -1
                                    RETURN
                                 ELSE
                                    IWIND = 1
                                    RETURN
                                 END IF
                           END IF
 202                    CONTINUE
C
C                       CHECK IF THE NEXT GROUP IS TROPOPAUSE
C                       OR MAX. WIND
C
                        IF(IVAL2 .EQ. 66 .OR. IVAL2 .EQ. 77 .OR.
     C                     IVAL2 .EQ. 88)
     C                        THEN
                                 IF(IVAL1 .LE. 36)
     C                              THEN
                                       IWIND = 1
                                       RETURN
                                    ELSE
                                       IWIND = -1
                                       RETURN
                                    END IF
                              END IF
C
C
                        IWIND = 0
                        NOWIND = NOWIND + 1
                        RETURN
                  END IF
             END IF
C
C
         ELSE
C
C           THIS IS SUPPOSED TO BE WIND
C
C           CHECK IF DIRECTION > 36
C
            IF(IVAL1 .GT. 36)
     C         THEN
C
C                 CHECK IF THE NEXT IS THE NEXT STANDARD LEVEL
C
                  IF(IVAL2 .EQ. LEVNEX)
     C               THEN
C
C                       NEXT LEVEL IS PRESSURE, SO THIS IS CORRUPT WIND
C
                        IWIND = -1
                        RETURN
                     ELSE
C
C                   CHECK IF THE NEXT GROUP IS TROPOPAUSE OR MAX. WIND
C
                    IF(IVAL2 .EQ. 66 .OR. IVAL2 .EQ. 77 .OR.
     C                 IVAL2 .EQ. 88)
     C                    THEN
                             IWIND = -1
                             RETURN
                       END IF
C
C                       CHECK IF THIS IS THE NEXT STANDARD LEVEL
C
                        IF(IVAL1 .EQ. LEVNEX)
     C                     THEN
                              IWIND = 0
                              NOWIND = NOWIND + 1
                              RETURN
                           ELSE
C
C                             CHECK IF THE NEXT GEOUP IS ONE OF
C                             THE STANDARD LEVELS.
C
                              DO 203 I = LEVBEG,LEVEND
                              IF(IVAL2 .EQ. LEVSTAN(I))
     C                           THEN
C
C                                   CORRUPTED WIND
C
                                    IWIND = -1
                                    RETURN
                                 END IF
 203                          CONTINUE
C
C                             CHECK IF THIS GEOUP IS ONE OF
C                             THE STANDARD LEVELS.
C
                              DO 204 I = LEVBEG,LEVEND
                              IF(IVAL1 .EQ. LEVSTAN(I))
     C                           THEN
C
C                                   PRESSURE/HEIGHT
C
                                    IWIND = 0
                                    NOWIND = NOWIND + 1
                                    RETURN
                                 END IF
 204                          CONTINUE
C
C                             THIS HAS TO BE CORRUPTED WIND ALTHOUGH
C                             THE NEXT IS NOT ANY STANDARD LEVEL.
C
                              IWIND = -1
                              RETURN
                           END IF
                     END IF
C
               ELSE
C
C              DIRECTION < 36, STILL CHECK FOR POSSIBILITY
C              OF BEING PRESSURE.
C
C                 CHECK IF THE NEXT IS THE NEXT STANDARD LEVEL
C
                  IF(IVAL2 .EQ. LEVNEX)
     C               THEN
                        IWIND = 1
                        RETURN
                     ELSE
C
C                       CHECK IF THE NEXT GROUP IS TROPOPAUSE
C                       OR MAX. WIND
C
                        IF(IVAL2 .EQ. 66 .OR. IVAL2 .EQ. 77 .OR.
     C                     IVAL2 .EQ. 88)
     C                        THEN
                                 IWIND = 1
                                 RETURN
                              END IF
C
C
C                       CHECK IF THE NEXT GROUP IS ONE OF
C                       THE STANDARD LEVELS.
C
                        DO 205 I = LEVBEG,LEVEND
                        IF(IVAL2 .EQ. LEVSTAN(I))
     C                     THEN
C
C                             THIS IS WIND
C
                              IWIND = 1
C
C                             CHECK IF THE NEXT GROUP IS TROPOPAUSE
C                             OR MAX. WIND
C
                              IF(IVAL2 .EQ. 88 .OR. IVAL2 .EQ. 77 .OR.
     C                           IVAL2 .EQ. 66)
     C                              THEN
C
C                                      THIS IS WIND
C
                                       IWIND = 1
                                       RETURN
                                    END IF
C
C
                              RETURN
                           END IF
 205                    CONTINUE
C
C                       CHECK IF THIS IS THE NEXT STANDARD LEVEL
C
                        IF(IVAL1 .EQ. LEVNEX)
     C                     THEN
                              IWIND = 0
                              NOWIND = NOWIND + 1
                              RETURN
                           ELSE
C
C                             CHECK IF THIS GROUP IS ONE OF
C                             THE STANDARD LEVELS.
C
                              DO 206 I = LEVBEG,LEVEND
                              IF(IVAL1 .EQ. LEVSTAN(I))
     C                           THEN
C
C                                   THIS IS PRESSURE
C
                                    IWIND = 0
                                    NOWIND = NOWIND + 1
                                    RETURN
                                 END IF
 206                          CONTINUE
C
C                             THIS HAS TO BE WIND ALTHOUGH THE
C                             NEXT IS NOT ANY STANDARD LEVEL.
C
                              IWIND = 1
                              RETURN
                        END IF
                  END IF
            END IF
       END IF
C
C
C
C
C     ---------------------------------------------------------------
C*          3. THIS PART IS USED IF WIND-LEVEL
C              INDICATOR IS NOT AVAILABLE.
 300  CONTINUE
C
C     CHECK THE FIRST GROUP
C
      IF(IVAL1 .EQ. LEVNEX)
     C   THEN
C
C           POSSIBLY  PRESSURE
C
            IF(IVAL1 .GT. 36)
     C         THEN
C
C                 PRESSURE
C
                  IWIND = 0
                  NOWIND = NOWIND + 1
                  RETURN
C
               ELSE
C
C                 CHECK IF THE NEXT GROUP IS THE NEXT STANDARD LEVEL
C
                  IF(IVAL2 .EQ. LEVNEX)
     C               THEN
C
C                       CHECK THE THIRD GROUP
C
                        IF(IVAL3 .EQ. LEVNEX2)
     C                     THEN
C                             THIRD GROUP IS PRESSURE SO IS ALSO FIRST
C
                              IWIND = 0
                              NOWIND = NOWIND + 1
                              RETURN
                           END IF
C===                          IWIND = 1
C===                          RETURN
C===                       END IF
                     ELSE
C
C                       SECOND GROUP IS NOT THE NEXT LEVEL SO FIRST IS
C
                        IWIND = 0
                        NOWIND = NOWIND + 1
                        RETURN
                    END IF
              END IF
        ELSE
C
C          FIRST GROUP IS NOT THE NEXT LEVEL, CHECK IF THE SECOND IS
C
           IF(IVAL2 .EQ. LEVNEX)
     C        THEN
C
C                THE SECOND IS PRESSURE SO THE FIRST IS WIND
C
                 IF(IVAL1 .GT. 36)
     C              THEN
                       IWIND = 1
                       RETURN
                    ELSE
                       IWIND = 1
                       RETURN
                    END IF
              ELSE
C
C                NEITHER FIRST OR SECOND IS THE NEXT LEVEL, SO CHECK
C                IF THEY ARE ANY OF THE NEXT LEVELS.
C
                 DO 301 I = LEVBEG,LEVEND
C
                 IF(IVAL1 .EQ. LEVSTAN(I))
     C              THEN
C
C                      POSSIBLY PRESSURE
C
                       IF(IVAL1 .GT. 36)
     C                    THEN
C
C                            PRESSURE
C
                             IWIND = 0
                             NOWIND = NOWIND + 1
                             RETURN
C
                          ELSE
C
                             IF(IVAL2 .EQ. LEVSTAN(I))
     C                          THEN
C
C                                  POSSIBLY CORRUPTED WIND,
C                                  CHECK NEXT GROUP
C
                                    IF(IVAL3 .EQ. LEVSTAN(I) .OR.
     C                                 IVAL3 .EQ. 88 .OR. IVAL3 .EQ. 77
     C                                 .OR. IVAL3 .EQ. 66)
     C                                THEN
C
C                                        THIRD IS PRESSURE SO IS FIRST
C
                                         IWIND = 0
                                         NOWIND = NOWIND + 1
                                         RETURN
C
                                      ELSE
C
                                         IWIND = 1
                                         RETURN
                                      END IF
                                ELSE
C
C                                  HAS TO BE PRESSURE
C
                                   IWIND = 0
                                   NOWIND = NOWIND + 1
                                   RETURN
                                END IF
                          END IF
                    END IF
 301             CONTINUE
C
              END IF
         END IF
C
C
      IF(IVAL2 .EQ. 88 .OR. IVAL2 .EQ. 77 .OR. IVAL2 .EQ. 66)
     C   THEN
            IWIND = 1
            IF(IVAL1 .GT. 36) IWIND = -1
            RETURN
         END IF
C
C
C     NO MATCH FOUND, SO THIS MUST ME WIND OR CORRUPT WIND
C
      IWIND = 1
      IF(IVAL1 .GT. 36) IWIND = -1
      RETURN
C
      END
      SUBROUTINE PRTEMP(NUMBER)
C**** *PRTEMP*
C
C
C     PURPOSE.
C     --------
C
C         PRINTS TEMP BULLETIN AND IT'S INTERMEDIATE
C         FORMAT ('KINT') AND DECODED FORMAT ('KDEC')
cps Only bulletin is printed out, together with PART .., NUMBER = (bulletin no.)
cps and ERROR = kerr (several such lines is possible)
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
      INTEGER LEN(4)
C
      DATA LEN / 200, 600, 200, 600/
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
c            CALL PRTKINT(KINT,1,LEN(KDEC(23)),MINDIC)
c            CALL PRTKDEC(KDEC,1,KDEC(24),MINDIC)
C
      WRITE(*,90000)
90000 FORMAT(2X,116('='))
C
C
      RETURN
      END
      SUBROUTINE MARDSEN(LAT,LONG,M,IERROR)
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
      SUBROUTINE IC3931(ITA,ITT,MINDIC,ITEMP)
C**** *IC3931*
C
C
C     PURPOSE.
C     --------
C
C         DECODE TEMPERARURE IN TENTHS OF DEGREE
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC3931(ITA,ITT,MINDIC,ITEMP)*
C
C          INPUT    :  ITA     - APPROXIMATE TENTHS VALUE AND SIGN BIT
C                   :  ITT     - TENS AND UNIT DIGITS OF TEMPERATURE
C                   :  MINDIC  - MISSING DATA VALUE
C
C          OUTPUT   :  ITEMP   - DECODED TEMPERATURE
C
C
C         XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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
C*          1.  CHECK MISSING VALUES.
C                --------------------
 100  CONTINUE
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
      SUBROUTINE IC3333(IQUADR,LAT,LONG,MINDIC,LAT2,LONG2)
C**** *IC3333*
C 
C    NAME     :  IC3333
C
C    FUNCTION :  DECODE LATITUDE AND LONGITUDE GIVEN IN THE FORM
C                99LALALA QCL0L0L0L0
C
C    INPUT    :  IQUADR   THE QUADRANT OF THE GLOBE (QC)
C             :  LAT      LATITUDE IN TENTHS OF DEGREE
C             :  LONG     LONGITUDE IN TENTHS OF DEGREE
C             :  MINDIC   MISSING DATA VALUE
C
C
C    OUTPUT   :  LAT2   LATITUDE IN HUNDREDTHS OF DEGREE
C                       SOUTHERN LATITUDE NEGATIVE
C             :  LONG2: LONGITUDE IN HUNDREDTHS OF DEGREE
C                       WESTERN LONGITUDE NEGATIVE
C
C                LAT2 AND LONG2 ARE SET TO MISSING DATA VALUE IF
C                ANY ERRORS FOUND IN LAT,LONG OR QUADRANT
C
C
C
C
      DIMENSION LATSIGN(4),LONSIGN(4)
C
      DATA LATSIGN/ 1,-1,-1, 1/
      DATA LONSIGN/ 1, 1,-1,-1/
C
C*    SET LAT2 AND LONG2 TO MISSING DATA VALUE
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
      SUBROUTINE IC1600(INHEI,LOWEST,MINDIC,OUTHEI)
C**** *IC1600*
C
C    NAME     :  IC1600
C
C    FUNCTION :  DECODE THE HEIGHT OF LOWEST CLOUDS
C
C    INPUT    :  INHEI   - CODE FIGURE FOR THE HEIGHT
C                LOWEST  - INDICATOR FOR LOWEST CLOUD
C                          0 = LOW CLOUD
C                          1 = MEDIUM CLOUD
C                          2 = HIGH CLOUD
C                MINDIC  - MISSING DATA VALUE
C
C    OUTPUT   :  OUTHEI  - DECODED HEIGHT OF LOWEST CLOUDS IN METRES
C
C             OUTHEI SET TO MISSING VALUE
C             IF ANY ERRORS FOUND IN INHEI
C
C
C
      INTEGER OUTHEI
C
      DIMENSION IHEIGHT(12)
C
      DATA IHEIGHT/25,75,150,250,450,800,1250,1750,2250,2600,3500,8000/
C
C*     SET MISSING VALUE
C
      OUTHEI=MINDIC
C
C*     TEST THE VALIDITY OF THE CODE FIGURE
C
      IF(INHEI .EQ. MINDIC .OR. LOWEST .EQ. MINDIC) RETURN
C
      IF(INHEI .LT. 0 .OR. INHEI .GT. 9) RETURN
C
C
C     FOR N = 9 DEFAULT HEIGHTS ARE ALLOCATED DEPENDING ON
C     WHETHER LOWEST CLOUD IS LOW, MEDIUM OR HIGH.
C
      IF (INHEI.NE.9 ) LOWEST = 0
C
      IND=INHEI + 1 + LOWEST
      OUTHEI=IHEIGHT(IND)
      RETURN
      END
      SUBROUTINE IC0777(IDD,ITEMP,MINDIC,IDEWPT)
C**** *IC0777*
C
C    NAME     :  IC0777
C
C    FUNCTION :  DECODE DEW-POINT TEMPERATURE IN TENS OF DEGREE
C
C    INPUT    :  IDD     - DEW-POINT EPRESSION
C             :  ITEMP   - TEMPERATURE
C             :  MINDIC  - MISSING DATA VALUE
C
C    OUTPUT   :  IDEWPT  - DECODED DEW-POINT TEMPERATURE
C
C             IDEWPT IS SET TO MISSING VALUE IF
C             ANY ERRORS FOUND IN IDD
C
C   A.HOLOPAINEN  JAN.83
C
C
C
C
C
C*     CHECK MISSING DATA INDICATOR
C
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
      SUBROUTINE STATION(IERR)
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
     1           FILE=YPPDAT(1:ndx)//'station_amend.dat',
cpsjun11     1            READONLY,
     1            ACTION='READ',
     1            FORM='UNFORMATTED',
     1            STATUS='OLD')
C
C
      READ(4) iparams,ipoints
C
      CLOSE(4)
C
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
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'combuff.f'
CRR common for directories
      INCLUDE 'comdir.f'
C
C
C     ------------------------------------------------------------------
C*          1.   FIND IMPORTANT STATIONS.
C                ------------------------
C
      J = 0
      K = 1
C
CTEST
      PRINT*,' IMPSTAT - mindic =', MINDIC
CTEST
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
                  if(j.gt.2000) then
                     print*,'impsta dimension too small'
                     go to 400
                  end if
                  IMPSTA(J) = ISTA
               END IF
C
300        CONTINUE
C
cps if the CTEST code is commented in again, it probably has to be moved
cps to after 400 CONTINUE to be reached (see nix_syno.f)
CTEST
ctestfile
ccc      ispc=char(32)
ccc      ndx=0
C  - Find end of character string. 
ccc      ndx=index(ytest(1:80),ispc) - 1
c
ccc               OPEN(UNIT=91,
ccc     1         FILE=YTEST(1:ndx)//'imp_temp_station.dat',
ccc     2         ACCESS='APPEND',
ccc     3         CARRIAGECONTROL='LIST',
ccc     4         FORM='FORMATTED',
ccc     5         STATUS='UNKNOWN'        )
ccc      WRITE(91,9999) IMPSTA
ccc9999  FORMAT(1X,10I7)
CTEST
C
400   CONTINUE
C
500   CONTINUE
C
      RETURN
      END
      SUBROUTINE LOCSTAT ( IWIND,IRC )
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
C                            4 = STATION NUMBER IN THE LIST BUT
C                                 NOT FOR THAT OBSERVATION TYPE 
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
      IRC=4
C
      RETURN
C
C
      END
      SUBROUTINE FIXUS
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
      SUBROUTINE FIXPHAS(LASTLEV,NOWIND,MORE,IFIX,LEVFIX,*,*)
C
C**** *FIXPHAS*
C
C
C     PURPOSE.
C     --------
C
C         TRIES TO FIND CORRECTLY CODED DATA AFTER
C         PHASE ERROR (WHERE THE PRESSURE LEVELS ARE
C         OUT OF ORDER)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *FIXPHAS(LASTLEV,NOWIND,MORE,IFIX,LEVFIX,*,*)*
C
C         INPUT     : LASTLEV - THE LEVEL NUMBER OF THE LAST CORRECT LEVEL
C                     NOWIND  - WIND INDICATOR (IS THERE SUPPOSED TO BE
C                               WIND AT THESE LEVELS)
C                     IFIX    - THE TWO DIGIT LEVEL INDICATOR IN CASE
C                               WHEN ONLY THIS INDICATOR WAS INCORRECT
C                               I.E. IF NINI=32 INSTEAD OF 30 SO IFIX=30
C                     LEVFIX  - THE ONE DIGIT LEVEL INDICATOR IN ABOVE
C                               MENTIONED ERROR CASE (SAME AS ILEVP IN
C                               SUBR. DETLEV).
C
C         OUTPUT    : MORE    - THE NUMBER OF CORRECT LEVELS FOUND AFTER
C                               THE CORRUPTION (IF MORE .GE. 2 RETURN TO
C                               THE MAIN SUBROUTINE)
C                     IPT     - POINTS TO THE BEGINNING OF FIRST CORRECT
C                               GROUP.
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
C          A.HOLOPAINEN  SEP.83
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
      DIMENSION IPRES(11),JPRES(14)
C
      DATA IPRES/ 0, 92, 85, 70, 50, 40, 30, 25, 20, 15, 10/
      DATA JPRES/70, 50, 30, 20, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1/
C
C     ------------------------------------------------------------------
C*          1.                                     .
C                -----------------------------------
C
      ITRY = 0
      IFIX = 0
      LEVFIX = 0
      MORE=0
      LEVPRE=LASTLEV
      MPT=IPT
      NPT=IPT
C
100   CONTINUE
C
      ITRY = ITRY + 1
C
      IF(ITRY .GT. 5) THEN
                         MORE = 0
                         IFIX = 0
                         RETURN
                      END IF
C
      IF(MORE .EQ. 0) THEN
                         IPT = NPT
                         LEVPRE = LASTLEV
                      END IF
C
      IF(ITRY .GT. 1 .OR. LASTLEV .LT. 0)
     C   THEN
            CALL NEXSEP2(IPT,IEQ,*5000)
            CALL NEXPRT2(IPT,IEQ,*5000)
         END IF
C
      IF(MORE .EQ. 0) NPT = IPT
C
150   CONTINUE
C
C     JJPT HOLDS THE POINTER TO THE BEGINNING OF THE FIRST CORRECT GROUP
C
      IF(MORE .EQ. 0) JJPT = IPT
C
      KC1 = IAND(KCHAR(IPT),127)
      KC2 = IAND(KCHAR(IPT+1),127)
      ILEV1 = KC1 -48
      ILEV2 = KC2 -48
C
      IF(ILEV1 .LT. 0 .OR. ILEV1 .GT. 9 .OR.
     C   ILEV2 .LT. 0 .OR. ILEV2 .GT. 9 )
     C      THEN
               IFIX = 0
               MORE = 0
               GO TO 100
            END IF
C
C
C     IF THE SECOND CHARACTER IS 'SPACE' IT MOST PROBABLY MEANS THAT
C     PREVIOUS GROUP WAS TOO LONG AND THAT LEVEL HAS TO BE DELETED
C
C
C     CHECK IF THIS IS THE BEGINNING OF SECTION
C     3 OR 4 (88---,77---, OR 66---)
C
      ILEV = 10*ILEV1 + ILEV2
C
      IF((ILEV .EQ. 88) .OR. (ILEV .EQ. 77) .OR.
     C   (ILEV .EQ. 66))
     C      THEN
               MORE = 2
               IF(MORE .GE. 2) GO TO 5000
            END IF
C
C
C     CHECK IF THIS IS /////-GROUP (E.G. GROUP OF 'MISSING WIND')
C
      IF((KCHAR(IPT) .EQ. 47) .AND. (KCHAR(IPT+1) .EQ. 47)) GO TO 100
C
C
C     FIRST CHECK IF THE PRESSURE/HEIGHT GROUP IS ONLY CORRUPTED
C     IN THE PART OF INDICATOR, E.G. NINI = 32 INSTEAD OF 30. IN
C     THIS CASE THE TWO NEXT LEVELS ARE CHECKED IF THEY ARE CORRECT
C     AND THIS IS NOT THE FIRST LEVEL THIS LEVEL IS ACCEPTED.
C
C
C     fix 12.4.94-------
C
      IF(MORE .EQ. 0 .AND. ITRY .EQ. 1 .AND. LASTLEV .GE. 0) THEN
         ilastp2=lastlev+2
         if((ilastp2.le.9.and.kdec(23).eq.1).or.
     1      (ilastp2.le.12.and.kdec(23).eq.3)) then
            IF(KDEC(23) .EQ. 1) LEVTES = IPRES(LASTLEV+2)
            IF(KDEC(23) .EQ. 3) LEVTES = JPRES(LASTLEV+2)
            LEVT1 = LEVTES/10
            LEVT2 = LEVTES - 10*LEVT1
            IF(ILEV1 .EQ. LEVT1 .OR. ILEV2 .EQ. LEVT2)
     C         THEN
                  ILEV = LEVTES
                  IFIX = ILEV
               END IF
         end if
      END IF
C     ------------------
C     SEE IF THE GROUP IS CORRUPT
C
      CALL EXTVAL(IPT,5,IGROUP)
      IF(IGROUP .EQ. MINDIC)
     C   THEN
            IF(MORE.GT.0) GO TO 100
            IFIX = 0
            GO TO 100
         END IF
C
C
C     DETERMINE THE LEVEL
C
      CALL DETLEV(KDEC(23),ILEV,ILEVP)
C
C
      IF(ILEVP .LE. LEVPRE .OR. ILEVP .LT. 0)
     1       THEN
                MORE = 0
                GO TO 100
             END IF
C
C
      IF(ITRY .EQ. 1 .AND. IFIX .GT. 0) LEVFIX = ILEVP
C
C     TITI TAI DIDI
C     ---- --- ----
C
      CALL NEXSEP2(IPT,IEQ,*5000)
      CALL NEXPRT2(IPT,IEQ,*5000)
C
C     DIDI FIFIFI
C     ---- ------
C
      CALL NEXSEP2(IPT,IEQ,*5000)
      CALL NEXPRT2(IPT,IEQ,*5000)
C
C     CHECK IF THE NEXT GROUP IS WIND OR PRESSURE/HEIGHT
C     OF NEXT LEVEL OR BEGINNING OF NEXT SECTION
C
C
C     KEEP NOWIND INDICATOR (IT'S NOT USEFUL IN CASE CORRUTED LEVELS)
C
      KPWIND = NOWIND
C
      CALL WINDHEI(ILEVP,NOWIND,IWIND,ITROPO,MAXWIN,IEND)
C
      NOWIND = KPWIND
C
C
      IF(IEND .EQ. 1) GO TO 5000
C
      IF(ITROPO .EQ. 1)
     1      THEN
C           ----- TROPOPAUSE SECTION FOUND  ----
               MORE = 2
               LEVPRE = ILEVP
               GO TO 5000
            END IF
C
      IF(MAXWIN .EQ. 1)
     1      THEN
C           ----- MAX.WIND SECTION FOUND  ----
               MORE = 2
               LEVPRE = ILEVP
               GO TO 5000
            END IF
C
      IF(IWIND .EQ. -3)
     1   THEN
C        ----  CORRUPTED GROUP  ----
            MORE = 0
            GO TO 100
         END IF
C
      IF(NOWIND .GE. 2) THEN
                           MORE = MORE + 1
                           LEVPRE = ILEVP
                           IF(MORE .GE. 2) GO TO 5000
                           GO TO 150
                        END IF
C
      IF(IWIND .EQ. 1)
     1      THEN
C           ----- WIND GROUP FOUND  ----
               MORE = MORE + 1
               LEVPRE = ILEVP
               IF(MORE .GE. 2) GO TO 5000
               GO TO 100
            END IF
C
      IF(IWIND .EQ. 0)
     1      THEN
C           ----- PRESSURE/HEIGHT GROUP FOUND  ----
               MORE = MORE + 1
               LEVPRE = ILEVP
               IF(MORE .GE. 2) GO TO 5000
               GO TO 150
            END IF
C
      IF(IWIND .EQ. -1)
     1     THEN
C          -----CORRUPTED WIND GROUP FOUND  ----
              MORE = 0
              GO TO 100
           END IF
C
C
      IF(IWIND .EQ. -2)
     1     THEN
C          ----- '/////' - WIND GROUP FOUND  ----
               MORE = MORE + 1
               LEVPRE = ILEVP
               IF(MORE .GE. 2) GO TO 5000
               GO TO 100
           END IF
C
C
C
C
5000  CONTINUE
C
      IF(IPT .GE. IEQ)
     C   THEN
            MORE = 0
            IFIX = 0
            RETURN
         END IF
C
      IPT = MPT
      IF(MORE .GE. 2) IPT = JJPT
      IF(IFIX .GT. 0 .AND. MORE .GE. 2)
     C   THEN
            IPT = MPT + 2
         ELSE
            IFIX = 0
         END IF
C
      RETURN
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
            DO 101 I=IPOS,700
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
                                KERR=5
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
                                DO 250 I=IFIXPOS,700
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
                                         ILN = IPT - NPT + 1
                                        IF(ILN .NE. 5 .AND. ILN .NE. 10)
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
cpsjan01      IF(IPART .EQ. 4 .AND. LEVNUM .GT. 3 .AND. IVAL2 .LT. 100)
cps  Changed 'LEVNUM .GT. 3' into 'LEVNUM .GT. 6'
cps  IVAL2=P0P0P0, where 1014 mB (hPa) is coded as 014, which may
cps  easily be misinterpreted as 14 mB. Hopefully no station will never
cps  measure pressure > 1000 mB at level higher than 7, or measure
cps  pressure < 100 mB at level less than 8 (note that levnum starts at
cps  0 in this subroutine)
      IF(IPART .EQ. 4 .AND. LEVNUM .GT. 6 .AND. IVAL2 .LT. 100)
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
cpsjan01 Changed to 8th level onward. Note that this means that if the temp
cps      has a level below 8th level which has higher pressure than the 
cps      previous level, then this and all succeeding levels will be rejected
cps      in UKDEC (we never get LEVCHK to remove the erronous level).
C     ONWARDS, BECAUSE FIRST TWO (OR THREE) LEVELS MAY BE OVER
C     1000 HPA AND ONLY THREE LAST FIGURES ARE USED (I.E. 11030,
C     22020,....)
C
      IF(LEVPRE .EQ. MINDIC) GO TO 500
C
cpsjan01      IF(LEVNUM .GT. 2 .OR. IPART .EQ. 6)
      IF(LEVNUM .GT. 6 .OR. IPART .EQ. 6)
     C   THEN
            IF(IVAL2 .GT. LEVPRE)
     C         THEN

cpsjan01                  IF(IPART .EQ. 4 .AND. LEVNUM .EQ. 3 .AND.
                  IF(IPART .EQ. 4 .AND. LEVNUM .EQ. 7 .AND.
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
      SUBROUTINE GEOPOTE(ILEV,IHH,MINDIC,IHEIGH)
C
C**** *GEOPOTE*
C
C
C     PURPOSE.
C     --------
C
C         DECODE THE GEOPOTENTIAL OF A LEVEL
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *GEOPOTE(ILEV,IHH,MINDIC,IHEIGH)*
C
C          INPUT    :  ILEV    - LEVEL INDICATOR (0,925,850,700,500,...)
C                   :  IHH     - THE HEIGHT FIGURE IN THREE DIGITS
C                   :  MINDIC  - MISSING DATA VALUE
C
C          OUTPUT   :  IHEIGH  - DECODED GEOPOTENTIAL IN METERS
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
C          A.HOLOPAINEN  JAN.83
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
      DIMENSION LEVELS(27)
C
      DATA LEVELS/
     1           0, 925, 850, 775, 700, 600, 500, 400, 300, 250,
     2         200, 150, 100,  70,  50,  30,  20,  10,   9,   8,
     3           7,   6,  5,  4,  3,  2,  1/
C
C
C     ------------------------------------------------------------------
C
C*          1.   CHECK FOR MISSING VALUES.
C                -------------------------
 100  CONTINUE
C
C
      IF(ILEV .EQ. MINDIC .OR. IHH .EQ. MINDIC) RETURN
C
C
      ICONST=0
C
C     RETAIN THE ORIGINAL IHH VALUE
C
      KEEPHH = IHH
C
C
      DO 111 I=1,27
      IF(ILEV .EQ. LEVELS(I)) GO TO 112
  111 CONTINUE
C
      IHEIGH=MINDIC
      RETURN
C
  112 CONTINUE
C
C
      GO TO( 1000, 925, 850, 775, 700, 600, 500, 500, 300, 300,
     1        200, 200, 200,  70,  50,  30,  30,  10,  10,  10,
     2         10,   6,   6,   4,   4,   2,   2) I
C
C
C     ---  1000MB  ---
C
 1000 CONTINUE
C
C
C     CHECK IF 1000MB IS BELOW SEA-LEVEL
C
      IF(IHH .GT. 500) IHH = 500 - IHH
C
      GO TO 5000
C
C
C     ---  925MB  ---
C
  925 CONTINUE
C
      IF(IHH .LT. 250) ICONST = 1000
      GO TO 5000
C
C     ---  850MB  ---
C
  850 CONTINUE
C
      IF(IHH .LT. 750) ICONST = 1000
      GO TO 5000
C
C
C     ---  775MB  ---
C
  775 CONTINUE
C
      ICONST = 2000
      IF(IHH .GT. 750) ICONST = 1000
      GO TO 5000
C
C
C     ---  700MB  ---
C
  700 CONTINUE
C
      ICONST=2000
      IF(IHH .LT. 300) ICONST = 3000
      GO TO 5000
C
C
C     ---  600MB  ---
C
  600 CONTINUE
C
      ICONST = 4000
      IF(IHH .GT. 750) ICONST = 3000
      GO TO 5000
C
C     ---  500MB OR 400MB  ---
C
  500 CONTINUE
C
      IHH = 10*IHH
      GO TO 5000
C
C
C     ---  300MB OR 250MB  ---
C
  300 CONTINUE
C
      IF(IHH .LT. 200) ICONST = 10000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  200MB OR 150MB OR 100MB  ---
C
  200 CONTINUE
C
      ICONST = 10000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  70MB ---
C
   70 CONTINUE
C
      ICONST = 10000
      IF(IHH .LE. 200) ICONST = 20000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  50MB ---
C
   50 CONTINUE
C
      ICONST = 20000
      IF(IHH .GE. 800) ICONST = 10000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  30MB OR 20MB ---
C
   30 CONTINUE
C
      ICONST = 20000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  10MB OR 9MB OR 8MB OR 7MB  ---
C
   10 CONTINUE
C
      ICONST = 30000
      IF(IHH .GE. 800) ICONST = 20000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  6MB OR 5MB  ---
C
    6 CONTINUE
C
      ICONST = 30000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  4MB OR 3MB  ---
C
    4 CONTINUE
C
      ICONST = 30000
      IF(IHH .LE. 200) ICONST = 40000
      IHH = 10 * IHH
      GO TO 5000
C
C
C     ---  2MB OR 1MB  ---
C
    2 CONTINUE
C
      ICONST = 40000
      IF(IHH .GE. 800) ICONST = 30000
      IHH = 10 * IHH
C
C
 5000 CONTINUE
C
      IHEIGH = ICONST + IHH
      IHH = KEEPHH
C
      RETURN
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
cpsjun99 This is not a possible value for temps. Have also 
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
      SUBROUTINE DETLEV ( IPART,ILEV,ILEVP)
C
C**** *DETLEV*
C
C
C     PURPOSE.
C     --------
C
C         DETERMINE THE PRESSURE LEVEL
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DETLEV ( IPART,ILEV,ILEVP)*
C
C          INPUT     : IPART  = 1 FOR TEMP PAR A
C                             = 3 FOR TEMP PAR C
C                      ILEV   = VALUE OF THE FIRST TWO FIGURES IN THE GROUP
C                               (I.E.  85,  70,  50, ...)
C
C          OUTPUT    : ILEVP  THE LEVEL NUMBER
C                       0  FOR 1000MB  )
C                       1  FOR  925MB  )
C                       2  FOR  850MB  ) FOR PART A
C                       -------------  )
C                       10  FOR  10MB  )
C
C                       0  FOR   70MB  )
C                       1  FOR   50MB  ) FOR PART C
C                       -------------  )
C                      13  FOR    1MB  )
C
C
C                      -1  IF NOT PRESSURE/HEIGHT GROUP
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
C          NONE.
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
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION ILEVINA(22),ILEVINC(28)
C
      DATA ILEVINA/
     C   0, 0,   92, 1,   85, 2,   70, 3,   50, 4,   40, 5,   30, 6,
     C  25, 7,   20, 8,   15, 9,   10, 10/
C
      DATA ILEVINC/
     C  70, 0,   50, 1,   30, 2,   20, 3,   10, 4,    9, 5,   8, 6,
     C   7, 7,    6, 8,    5, 9,    4,10,    3,11,    2,12,   1,13/
C
C     ------------------------------------------------------------------
C*          1.   DETERMINE THE PRESSURE LEVEL.
C                -----------------------------
 100  CONTINUE
C
      ILEVP=-1
C
C     CHECK VALIDITY OF ILEV1 AND ILEV2
C
C     CHECK VALIDITY OF LEV AND IPART
C
      IF((ILEV .LT. 0) .OR. (ILEV .GT. 92)) RETURN
      IF((IPART .NE. 1) .AND. (IPART .NE. 3)) RETURN
C
      IF(IPART .EQ. 1) THEN
                          DO 101 I=1,21,2
                             IF(ILEV .EQ. ILEVINA(I))
     C                          THEN
                                   ILEVP = ILEVINA(I+1)
                                   RETURN
                                END IF
101                       CONTINUE
                       END IF
C
      IF(IPART .EQ. 3) THEN
                          DO 102 I=1,27,2
                             IF(ILEV .EQ. ILEVINC(I))
     C                          THEN
                                   ILEVP = ILEVINC(I+1)
                                   RETURN
                                END IF
102                       CONTINUE
                       END IF
C
C
C
      RETURN
      END
      SUBROUTINE DETLEVC ( ILEV1,ILEV2,ILEVP)
C
C**** *DETLEVC*
C
C
C     PURPOSE.
C     --------
C
C         DETERMINE THE PRESSURE LEVEL IN TEMP PART C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *DETLEVC ( ILEV1,ILEV2,ILEVP)*
C
C          INPUT     : ILEV1  VALUE OF THE FIRST FIGURE IN THE GROUP
C                      ILEV2  ----- -- --- SECOND ----- -- --- -----
C
C          OUTPUT    : ILEVP  THE LEVEL NUMBER
C                      0  FOR 70 MB
C                      1  FOR 50 MB
C                      -------------
C                      13  FOR  1 MB
C
C                      -1  IF NOT PRESSURE/HEIGHT GROUP
C
C
C*     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
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
C*          1. DETERMINE PRESSURE LEVEL, TEMP PART C.
C              --------------------------------------
 100  CONTINUE
C
      ILEVP=-1
C
C     CHECK VALIDITY OF ILEV1 AND ILEV2
C
      IF((ILEV1 .LT. 0) .OR. (ILEV1 .GT. 9) .OR.
     1   (ILEV2 .LT. 0) .OR. (ILEV2 .GT. 9)) RETURN
C
C
C     FORM THE LEVEL FIGURE
C
      ILEV=10*ILEV1 + ILEV2
C
      IF(ILEV .EQ. 70) THEN
         ILEVP=0
         RETURN
      END IF
C
      IF(ILEV .EQ. 50) THEN
         ILEVP=1
         RETURN
      END IF
C
C
      IF(ILEV .EQ. 30) THEN
         ILEVP=2
         RETURN
      END IF
C
      IF(ILEV .EQ. 20) THEN
         ILEVP=3
         RETURN
      END IF
C
      IF(ILEV .EQ. 10) THEN
         ILEVP=4
         RETURN
      END IF
C
      IF(ILEV .EQ. 9) THEN
         ILEVP=5
         RETURN
      END IF
C
      IF(ILEV .EQ. 8) THEN
         ILEVP=6
         RETURN
      END IF
C
      IF(ILEV .EQ. 7) THEN
         ILEVP=7
         RETURN
      END IF
C
      IF(ILEV .EQ. 6) THEN
         ILEVP=8
         RETURN
      END IF
C
C
      IF(ILEV .EQ. 5) THEN
         ILEVP=9
         RETURN
      END IF
C
      IF(ILEV .EQ. 4) THEN
         ILEVP=10
         RETURN
      END IF
C
      IF(ILEV .EQ. 3) THEN
         ILEVP=11
         RETURN
      END IF
C
      IF(ILEV .EQ. 2) THEN
         ILEVP=12
         RETURN
      END IF
C
      IF(ILEV .EQ. 1) THEN
         ILEVP=13
         RETURN
      END IF
C
      RETURN
      END
      SUBROUTINE PRE2121 (IFOUND)
C
C**** *PRE2121*
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
      CHARACTER*64 CTABBEN
      CHARACTER*24 CTABBU
      CHARACTER*8 YDATE
      CHARACTER*9 YSNAME
      CHARACTER*5 YTIME,YFAMILY,YTASK
CTEST
      CHARACTER*23 CORINT
CTEST
      CHARACTER*1 ispc
C
      INCLUDE 'parameter.f'
      INCLUDE 'combuff.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comtab.f'
      INCLUDE 'comtabc.f'
      INCLUDE 'compoin.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
      INCLUDE 'comsh.f'
      INCLUDE 'comshc.f'
CRR common for directories
      INCLUDE 'comdir.f'
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
CRR - For writing error reort on screen
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
C     --------------------------------------------------------------------
C*         7A.   SET UP DIRECTORY NAMES FOR FILES.
C                ---------------------------------
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
C     --------------------------------------------------------------------
C*          9.   SET UP WMO STATION LIST AND IMPORTANT STATIONS.
C                -----------------------------------------------
 900  CONTINUE
C
      CALL STATION(IERR)
C
C*          10.  READ IN LIST OF SHIP HEIGHTS.
C                -----------------------------
 1000 CONTINUE
C
      ispc=char(32)
      ndx=0
C  - Find end of character string. 
      ndx=index(yppdat(1:80),ispc) - 1
      OPEN(88,IOSTAT=IOS,ERR=1010,
     1     FILE=YPPDAT(1:ndx)//'ship_height.dat',
     2     FORM='FORMATTED',
     3     STATUS='OLD')
C
      I=0
      READ(88,'(A9,F3.0)',ERR=1020,END=1100) Y
C
 1030 CONTINUE
      I=I+1
      READ(88,'(A9,F3.0)',ERR=1020,END=1100) YSNAME(I),SHGT(I)
      GO TO 1030
C
 1020 CONTINUE
C
      PRINT*,'Error during read ship_height file'
      PRINT*,'Iostat=',ios
      ierr=1
      return
C
 1010 CONTINUE
C
      PRINT*,'Error opening ship_height file'
      PRINT*,'Iostat=',ios
      ierr=1
      return
C 
C
C*          11. GET SUPERVISOR DATE,TIME,FAMILY AND TASK.
C               -----------------------------------------
 1100 CONTINUE
C
CTEST added CORINT-character*24
      CALL ASCTIM(CORINT)
      READ(CORINT,'(I2,10X,I2,1X,I2,1X,I2,1X,I2)') IDD,IHH,IMM,ISS,ICC
      PRINT*,' Current time = ',CORINT
CTEST
C
      CLOSE(88)
C
      RETURN
      END
      SUBROUTINE TEMPEXP(K,IERR )
C
C**** *TEMPEXP*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR TEMP DATA.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *TEMPEXP(K,IERR)*
cps       K=1,2,3,4 (= Part A,B,C,D)
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
C
      CHARACTER*9 CVAL,CIDENT,YSNAME
      CHARACTER*24 CUNIT
C
      INCLUDE 'comwtc.f'
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f'
      INCLUDE 'comsh.f'
      INCLUDE 'comshc.f'
C
      DIMENSION PRESL(11),PRESU(12)
C
      DATA PRESL/100000.,92500.,85000.,70000.,50000.,40000.,30000,
     1            25000.,20000.,15000.,10000./
      DATA PRESU/7000.,5000.,3000.,2000.,1000.,900.,800.,
     1            500., 400., 300., 200., 100./
C     ------------------------------------------------------------------
C*          1.   INCREASE COUNTEROF SUBSETS BY ONE.
C                ----------------------------------
 100  CONTINUE
cc      CALL PRTKINT(KINT,1,700,MINDIC)
cc      CALL PRTKDEC(KDEC,1,420,MINDIC)
C
      IF(IERR.NE.0) RETURN
C
      NSUB=1
      N   =NSUB  
      IREP=0
C
      do 101 i=1,jp22
      do 102 j=1,jp1
      values(i,j)=999999.
 102  continue
 101  continue
C
C*          1.1  GET CURRENT DATE (YEAR AND MONTH).
C                ----------------------------------
 110  CONTINUE
C
      CALL DATUM(KDEC(1),IMONTH,IYEAR)
C
C     ------------------------------------------------------------------
C*          2.   TEMP - LAND BASED STATION.
C                --------------------------
 200  CONTINUE
C
      IF(KDEC(4).EQ.35) THEN
C     -----------------------------------------------------------------
C*          2.1  TEMP PART A.
C                ------------
 210  CONTINUE
C
         IF(K.EQ.1) THEN
            VALUES( 1,NSUB)=(KINT(12)-48)*10+KINT(13)-48
            VALUES( 2,NSUB)=(KINT(14)-48)*100+(KINT(15)-48)*10
     1                      +KINT(16)-48
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
cpsapr98            VALUES( 5,NSUB)=IYEAR+1900.
            VALUES( 5,NSUB)=FLOAT(IYEAR)
            VALUES( 6,NSUB)=FLOAT(IMONTH)
            VALUES( 7,NSUB)=FLOAT(KDEC(1))
            VALUES( 8,NSUB)=FLOAT(KDEC(2))
            VALUES( 9,NSUB)=FLOAT(KDEC(9))
            VALUES(10,NSUB)=KDEC(5)/100.
            VALUES(11,NSUB)=KDEC(6)/100.
            VALUES(12,NSUB)=999999.
            IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
            VALUES(13,NSUB)=999999.
            VALUES(14,NSUB)=999999.
            VALUES(15,NSUB)=999999.
            VALUES(16,NSUB)=999999.
            VALUES(17,NSUB)=999999.
            VALUES(18,NSUB)=999999.
            VALUES(19,NSUB)=999999.
            VALUES(20,NSUB)=999999.
            IREP=IREP+1
            VALUES(21,NSUB)=999999.
            IF(KDEC(25).NE.MINDIC) VALUES(21,NSUB)=KDEC(25)*10.
            VALUES(22,NSUB)=64.
            VALUES(23,NSUB)=999999.
            VALUES(24,NSUB)=999999.
            IF(KDEC(26).NE.MINDIC) VALUES(24,NSUB)=KDEC(26)/10.+273.16
            VALUES(25,NSUB)=999999.
            IF(KDEC(27).NE.MINDIC) VALUES(25,NSUB)=KDEC(27)/10.+273.16
            VALUES(26,NSUB)=999999.
            IF(KDEC(28).NE.MINDIC) VALUES(26,NSUB)=FLOAT(KDEC(28))
            VALUES(27,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(29).NE.MINDIC) VALUES(27,NSUB)=FLOAT(KDEC(29))
            IF(KDEC(29).NE.MINDIC) VALUES(27,NSUB)=FLOAT(KDEC(29))/10.
C
            IP=28
            IPKD=30
C
            DO 211 I=1,11
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+5
                                        GO TO 211
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=PRESL(I)
            VALUES(IP+1,NSUB)=32.
            VALUES(IP+2,NSUB)=999999.
            IF(KDEC(IPKD).NE.MINDIC)
     1         VALUES(IP+2,NSUB)=KDEC(IPKD)*9.8
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
cc      if(i.eq.1) then
cc      print*,' tempexp-VALUES(IP+3,NSUB),VALUES(IP+4,NSUB)',
cc     1        VALUES(IP+3,NSUB),VALUES(IP+4,NSUB)
cc      endif
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+3).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+4).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
            IF(KDEC(IPKD+4).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
            IP=IP+7
            IPKD=IPKD+5
C
 211        CONTINUE
C
C           TROPOPAUSE LEVEL
C
            IPKD=100
C
            DO 212 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=16.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
                  VALUES(IP+4,NSUB)=999999.
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 214
               END IF
C
 212        CONTINUE
C
 214  CONTINUE
C
C           MAX. WIND LEVEL
C
            IPKD=115
            ISHEAR=0 
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
                  VALUES(IP+4,NSUB)=999999.
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 215
               END IF

C
 213        CONTINUE
C
 215  CONTINUE
C
C
C           UPDATE REPLICATION FACTOR
C
            VALUES(20,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
            IF(IREP.EQ.1) THEN
               IERR=1
               RETURN
            END IF
C
C
C           WIND SHEAR AT MAX. WIND LEVEL
C
            IPKD=115
            ISHEAR=0 
            IPTSH=IP
            VALUES(IPTSH,NSUB)=0.0
C
            DO 216 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IF(KDEC(IPKD+3).NE.MINDIC) 
     1               THEN
                        ISHEAR=ISHEAR+1
                        VALUES(IPTSH,NSUB)=FLOAT(ISHEAR) 
cpsoct06 Inserted following line, then ajusted all following IP-indexes down one
cps      Previous version worked correctly only for one level
                        IF (ISHEAR.EQ.1) IP=IP+1 ! should be included just once
                        VALUES(IP,NSUB)=KDEC(IPKD)*10.
cpsdec11                        VALUES(IP+1,NSUB)=8.
                        VALUES(IP+1,NSUB)=10. ! maximum wind level & significant level, wind
                        VALUES(IP+2,NSUB)=999999.
cpsjul06 changed accuracy of wind shear (vava)
                        IF(KDEC(IPKD+3).NE.MINDIC)
cps     1                  VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))
     1                  VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))/10.0
                        VALUES(IP+3,NSUB)=999999.
crr  changed accuracy of wind speed    cps: wind shear (vbvb)
                        IF(KDEC(IPKD+4).NE.MINDIC)
crr     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))    
     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))/10.    
C
                        IPKD=IPKD+5
                        IP=IP+4
                     ELSE
                        GO TO 217
                     END IF
C
               ELSE
                  GO TO 217
               END IF

C
 216        CONTINUE
C
 217  CONTINUE
C
C            UPDATE M TOTAL LENGTH OF REPORT.
C            --------------------------------
cpsoct06      IF(ISHEAR.NE.0) IP=IP+1
      M=IP-1
C 
C
            RETURN
                    END IF
C     -----------------------------------------------------------------
C*          2.2  TEMP PART C.
C                ------------
 220  CONTINUE
C
         IF(K.EQ.3) THEN
            VALUES( 1,NSUB)=(KINT(12)-48)*10+KINT(13)-48
            VALUES( 2,NSUB)=(KINT(14)-48)*100+(KINT(15)-48)*10
     1                      +KINT(16)-48
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
cpsapr98            VALUES( 5,NSUB)=IYEAR+1900.
            VALUES( 5,NSUB)=FLOAT(IYEAR)
            VALUES( 6,NSUB)=FLOAT(IMONTH)
            VALUES( 7,NSUB)=FLOAT(KDEC(1))
            VALUES( 8,NSUB)=FLOAT(KDEC(2))
            VALUES( 9,NSUB)=FLOAT(KDEC(9))
            VALUES(10,NSUB)=KDEC(5)/100.
            VALUES(11,NSUB)=KDEC(6)/100.
            VALUES(12,NSUB)=999999.
            IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
            VALUES(13,NSUB)=999999.
            VALUES(14,NSUB)=999999.
            VALUES(15,NSUB)=999999.
            VALUES(16,NSUB)=999999.
            VALUES(17,NSUB)=999999.
            VALUES(18,NSUB)=999999.
            VALUES(19,NSUB)=999999.
            VALUES(20,NSUB)=999999.
C
            IP=21
            IPKD=30
C
            DO 221 I=1,14
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+5
                                        GO TO 221
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=PRESU(I)
            VALUES(IP+1,NSUB)=32.
            VALUES(IP+2,NSUB)=999999.
            IF(KDEC(IPKD).NE.MINDIC)
     1       VALUES(IP+2,NSUB)=KDEC(IPKD)*9.8
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+3).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+4).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
            IF(KDEC(IPKD+4).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
            IP=IP+7
            IPKD=IPKD+5
C
 221        CONTINUE
C
C           TROPOPAUSE LEVEL
C
            IPKD=100
C
            DO 222 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=16.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
                  VALUES(IP+4,NSUB)=999999.
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 224
               END IF
C
 222        CONTINUE
C
 224  CONTINUE
C
C           MAX. WIND LEVEL
C
            IPKD=115
C
            DO 223 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  VALUES(IP+4,NSUB)=999999.
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 225
               END IF

C
 223        CONTINUE
C
 225  CONTINUE
C
            VALUES(20,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C
C
C
C           WIND SHEAR AT MAX. WIND LEVEL
C
            IPKD=115
            ISHEAR=0 
            IPTSH=IP
            VALUES(IPTSH,NSUB)=0.0
C
            DO 226 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IF(KDEC(IPKD+3).NE.MINDIC) 
     1               THEN
                        ISHEAR=ISHEAR+1
                        VALUES(IPTSH,NSUB)=FLOAT(ISHEAR) 
cpsoct06 Inserted following line, then ajusted all following IP-indexes down one
cps      Previous version worked correctly only for one level
                        IF (ISHEAR.EQ.1) IP=IP+1 ! should be included just once
                        VALUES(IP,NSUB)=KDEC(IPKD)*10.
cpsdec11                        VALUES(IP+1,NSUB)=8.
                        VALUES(IP+1,NSUB)=10. ! maximum wind level & significant
                        VALUES(IP+2,NSUB)=999999.
cpsjul06 changed accuracy of wind shear (vava)
                        IF(KDEC(IPKD+3).NE.MINDIC)
cps     1                  VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))
     1                  VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))/10.0
                        VALUES(IP+3,NSUB)=999999.
crr  changed accuracy of wind speed  cps: wind shear (vbvb)
                       IF(KDEC(IPKD+4).NE.MINDIC)
crr     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))    
     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))/10.    
C
                        IPKD=IPKD+5
                        IP=IP+4
                     ELSE
                        GO TO 227
                     END IF
C
               ELSE
                  GO TO 227
               END IF

C
 226        CONTINUE
C
 227  CONTINUE
C
C     UPDATE M
C
cpsoct06      IF(ISHEAR.NE.0) IP=IP+1
      M=IP-1
C 
C
            RETURN
                    END IF
C     -----------------------------------------------------------------
C*          2.3  TEMP PART B.
C                ------------
 230  CONTINUE
C
         IF(K.EQ.2) THEN
            VALUES( 1,NSUB)=(KINT(12)-48)*10+KINT(13)-48
            VALUES( 2,NSUB)=(KINT(14)-48)*100+(KINT(15)-48)*10
     1                      +KINT(16)-48
            VALUES( 3,NSUB)=999999.
            IF(KDEC(423).NE.MINDIC) VALUES( 3,NSUB)=KDEC(423)
            VALUES( 4,NSUB)=999999.
cpsmar07 kdec(422)=sr=002013 in BUFR, not 002012
cpsmar07            IF(KDEC(422).NE.MINDIC) VALUES( 4,NSUB)=KDEC(422)
cpsapr98            VALUES( 5,NSUB)=IYEAR+1900.
            VALUES( 5,NSUB)=FLOAT(IYEAR)
            VALUES( 6,NSUB)=FLOAT(IMONTH)
            VALUES( 7,NSUB)=FLOAT(KDEC(1))
            VALUES( 8,NSUB)=FLOAT(KDEC(2))
            VALUES( 9,NSUB)=FLOAT(KDEC(9))
            VALUES(10,NSUB)=KDEC(5)/100.
            VALUES(11,NSUB)=KDEC(6)/100.
            VALUES(12,NSUB)=999999.
            IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
            VALUES(13,NSUB)=999999.
            VALUES(14,NSUB)=1.
            VALUES(15,NSUB)=15.
            IF(KDEC(386).NE.MINDIC) VALUES(15,NSUB)=FLOAT(KDEC(386))
            VALUES(16,NSUB)=999999.
            IF(KDEC(388).NE.MINDIC) VALUES(16,NSUB)=FLOAT(KDEC(388))
            VALUES(17,NSUB)=999999.
            IF(KDEC(387).NE.MINDIC) THEN
                                    CALL IC513(KDEC(387),KOUT)
                                    VALUES(17,NSUB)=FLOAT(KOUT)
                                    END IF
            VALUES(18,NSUB)=999999.
            IF(KDEC(389).NE.MINDIC) THEN
                                    CALL IC515(KDEC(389),KOUT)
                                    VALUES(18,NSUB)=FLOAT(KOUT)
                                    END IF
            VALUES(19,NSUB)=999999.
            IF(KDEC(390).NE.MINDIC) THEN
                                    CALL IC509(KDEC(390),KOUT)
                                    VALUES(19,NSUB)=FLOAT(KOUT)
                                    END IF
            VALUES(20,NSUB)=999999.
C
C           TEMPERATURE SIGNIFICANCE
C
            IP=21
            IPKD=25
C
            DO 231 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 231
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            sign=4.
            if(ipkd.eq.25.and.kint(17).eq.0.and.
     1         kint(18).ne.mindic)  sign=68.
c            if(ipkd.eq.25) sign=68.
            VALUES(IP+1,NSUB)=sign
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            VALUES(IP+6,NSUB)=999999.
C
            IP=IP+7
            IPKD=IPKD+3
C
 231        CONTINUE
C
C           WIND SIGNIFICANCE
C
C
            IPKD=206
C
            DO 232 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 232
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            sign=2.
            if(ipkd.eq.206.and.kint(318).eq.0.and.
     1         kint(319).ne.mindic) sign=66.
c            if(ipkd.eq.206) sign=66.
            VALUES(IP+1,NSUB)=sign
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            VALUES(IP+4,NSUB)=999999.
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+7
            IPKD=IPKD+3
C
 232        CONTINUE
C
cps         REGIONAL GROUPS
cpssep06 Rewritten this section somewhat. Vertical sounding significance now set to missing
cps      for levels of regional groups.
C
            IPKD=392
            DO I=1,4
               IF(KDEC(IPKD).NE.MINDIC) THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.   ! pressure
                  VALUES(IP+1,NSUB)=999999.        ! vertical sounding significance 
                  VALUES(IP+2,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)       ! geopotential height
     *                 VALUES(IP+2,NSUB)=KDEC(IPKD+1)*9.8
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+2).NE.MINDIC)       ! TTT 
     *                 VALUES(IP+3,NSUB)=KDEC(IPKD+2)/10.+273.16
                  VALUES(IP+4,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)       ! TDTD
     *                 VALUES(IP+4,NSUB)=KDEC(IPKD+3)/10.+273.16
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+4).NE.MINDIC)       ! dd
     *                 VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+4))
                  VALUES(IP+6,NSUB)=999999.
                  IF(KDEC(IPKD+5).NE.MINDIC)       ! fff
     *                 VALUES(IP+6,NSUB)=KDEC(IPKD+5)/10.
                  IP=IP+7
               END IF
               IPKD=IPKD+6
            END DO
C
C
C           UPDATE REPLICATION FACTOR
C
            VALUES(20,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C
C
C
            RETURN
                    END IF
C     -----------------------------------------------------------------
C*           2.4 TEMP PART D.
C                ------------
 240  CONTINUE
C
         IF(K.EQ.4) THEN
            VALUES( 1,NSUB)=(KINT(12)-48)*10+KINT(13)-48
            VALUES( 2,NSUB)=(KINT(14)-48)*100+(KINT(15)-48)*10
     1                      +KINT(16)-48
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
cpsapr98            VALUES( 5,NSUB)=IYEAR+1900.
            VALUES( 5,NSUB)=FLOAT(IYEAR)
            VALUES( 6,NSUB)=FLOAT(IMONTH)
            VALUES( 7,NSUB)=FLOAT(KDEC(1))
            VALUES( 8,NSUB)=FLOAT(KDEC(2))
            VALUES( 9,NSUB)=FLOAT(KDEC(9))
            VALUES(10,NSUB)=KDEC(5)/100.
            VALUES(11,NSUB)=KDEC(6)/100.
            VALUES(12,NSUB)=999999.
            IF(KDEC(8).NE.MINDIC) VALUES(12,NSUB)=FLOAT(KDEC(8))
            VALUES(13,NSUB)=999999.
            VALUES(14,NSUB)=999999.
            VALUES(15,NSUB)=999999.
            VALUES(16,NSUB)=999999.
            VALUES(17,NSUB)=999999.
            VALUES(18,NSUB)=999999.
            VALUES(19,NSUB)=999999.
            VALUES(20,NSUB)=999999.
c
c           temperature significance
C
            IP=21
            IPKD=25
C
            DO 241 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 241
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            VALUES(IP+1,NSUB)=4.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            VALUES(IP+6,NSUB)=999999.
C
            IP=IP+7
            IPKD=IPKD+3
C
 241        CONTINUE
C
C           WIND SIGNIFICANCE
C
            IPKD=206
C
            DO 242 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 242
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            VALUES(IP+1,NSUB)=2.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            VALUES(IP+4,NSUB)=999999.
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+7
            IPKD=IPKD+3
C
 242        CONTINUE
C
C
C           UPDATE REPLICATION FACTOR
C
            VALUES(20,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C
C
C
            RETURN
                    END IF
                  END IF
C     -----------------------------------------------------------------
C*          3.   TEMP - SHIP/DROP REPORTS.
C                -------------------------
 300  CONTINUE
C
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
C     -----------------------------------------------------------------
C*          3.1  TEMP PART A.
C                ------------
 310  CONTINUE
C
         IF(K.EQ.1) THEN
            VALUES( 1,NSUB)=1000*NSUB+9
            CVAL(NSUB)=CHAR(KINT(12))//CHAR(KINT(13))//CHAR(KINT(14))//
     1                 CHAR(KINT(15))//CHAR(kint(16))//'    '
            VALUES( 2,NSUB)=999999.
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
            VALUES( 5,NSUB)=999999.
cpsapr98            VALUES( 6,NSUB)=IYEAR+1900.
            VALUES( 6,NSUB)=FLOAT(IYEAR)
            VALUES( 7,NSUB)=FLOAT(IMONTH)
            if(KDEC(4).EQ.135) then
c              for drop sondes hour and minue from header
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(12))
               if(KDEC(1).ne.KDEC(10).or.kdec(2).ne.kdec(11)) then
                  if(abs(KDEC(1)-KDEC(10)).lt.2) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  else
                     VALUES( 8,NSUB)=FLOAT(KDEC(1))
                     VALUES( 9,NSUB)=FLOAT(KDEC(2))
                     VALUES(10,NSUB)=FLOAT(KDEC(9))
                  end if
                  if(abs(KDEC(2)-KDEC(11)).le.1.or.
     1               abs(KDEC(2)-KDEC(11)).eq.23) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  end if
               end if
            else
c              day hour and minute from data
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(9))
            end if
c
            VALUES(11,NSUB)=KDEC(5)/100.
            VALUES(12,NSUB)=KDEC(6)/100.
            VALUES(13,NSUB)=999999.
            IF(KDEC(4).EQ.137) THEN
               IF(KDEC(8).NE.MINDIC) VALUES(13,NSUB)=FLOAT(KDEC(8))
            ELSEIF(KDEC(4).EQ.36) THEN
               DO 291 I=1,100
               IF(YSNAME(I).EQ.CVAL(NSUB)) VALUES(13,NSUB)=SHGT(I)
 291           CONTINUE
            END IF
            VALUES(14,NSUB)=999999.
            VALUES(15,NSUB)=999999.
            VALUES(16,NSUB)=999999.
            VALUES(17,NSUB)=999999.
            VALUES(18,NSUB)=999999.
            VALUES(19,NSUB)=999999.
            VALUES(20,NSUB)=999999.
            VALUES(21,NSUB)=999999.
            IREP=IREP+1
            VALUES(22,NSUB)=999999.
            IF(KDEC(25).NE.MINDIC) VALUES(22,NSUB)=KDEC(25)*10.
            VALUES(23,NSUB)=64.
            VALUES(24,NSUB)=999999.
            VALUES(25,NSUB)=999999.
            IF(KDEC(26).NE.MINDIC) VALUES(25,NSUB)=KDEC(26)/10.+273.16
            VALUES(26,NSUB)=999999.
            IF(KDEC(27).NE.MINDIC) VALUES(26,NSUB)=KDEC(27)/10.+273.16
            VALUES(27,NSUB)=999999.
            IF(KDEC(28).NE.MINDIC) VALUES(27,NSUB)=FLOAT(KDEC(28))
            VALUES(28,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(29).NE.MINDIC) VALUES(28,NSUB)=FLOAT(KDEC(29))
            IF(KDEC(29).NE.MINDIC) VALUES(28,NSUB)=FLOAT(KDEC(29))/10.
C
            IP=29
            IPKD=30
C
            DO 311 I=1,11
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+5
                                        GO TO 311
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=PRESL(I)
            VALUES(IP+1,NSUB)=32.
            VALUES(IP+2,NSUB)=999999.
            IF(KDEC(IPKD).NE.MINDIC) 
     1       VALUES(IP+2,NSUB)=KDEC(IPKD)*9.8
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+3).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+4).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
            IF(KDEC(IPKD+4).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
            IP=IP+7
            IPKD=IPKD+5
C
 311        CONTINUE
C
C           TROPOPAUSE LEVEL
C
            IPKD=100
C
            DO 312 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=16.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
                  VALUES(IP+4,NSUB)=999999.
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 314
               END IF
C
 312        CONTINUE
C
 314  CONTINUE
C
C           MAX. WIND LEVEL
C
            IPKD=115
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
                  VALUES(IP+4,NSUB)=999999.
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 315
               END IF

C
 313        CONTINUE
C
 315  CONTINUE
C
C
C           UPDATE REPLICATION FACTOR
C
            VALUES(21,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
            IF(IREP.EQ.1) THEN
               IERR=1
               RETURN
            END IF
C
C
C           WIND SHEAR AT MAX. WIND LEVEL
C
            IPKD=115
            ISHEAR=0 
            IPTSH=IP
            VALUES(IPTSH,NSUB)=0.0
C
            DO 316 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IF(KDEC(IPKD+3).NE.MINDIC) 
     1               THEN
                        ISHEAR=ISHEAR+1
                        VALUES(IPTSH,NSUB)=FLOAT(ISHEAR) 
cpsoct06 Inserted following line, then ajusted all following IP-indexes down one
cps      Previous version worked correctly only for one level
                        IF (ISHEAR.EQ.1) IP=IP+1 ! should be included just once
                        VALUES(IP,NSUB)=KDEC(IPKD)*10.
cpsdec11                        VALUES(IP+1,NSUB)=8.
                        VALUES(IP+1,NSUB)=10. ! maximum wind level & significant
                        VALUES(IP+2,NSUB)=999999.
                        IF(KDEC(IPKD+3).NE.MINDIC)
cpsoct06     1                   VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))
     1                   VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))/10.0
                        VALUES(IP+3,NSUB)=999999.
crr  changed accuracy of wind speed
crr                        IF(KDEC(IPKD+4).NE.MINDIC)
crr     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))    
                        IF(KDEC(IPKD+4).NE.MINDIC)
     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))/10.    
C
                        IPKD=IPKD+5
cpsoct06                        IP=IP+5
                        IP=IP+4
                     ELSE
                        GO TO 317
                     END IF
C
               ELSE
                  GO TO 317
               END IF

C
 316        CONTINUE
C
 317  CONTINUE
C
C     UPDATE M TOTAL LENGTH OF REPORT
C
      M=IP-1
C 
C
            RETURN
                    END IF
C
C     -----------------------------------------------------------------
C*          3.2  TEMP PART C.
C                ------------
 320  CONTINUE
C
         IF(K.EQ.3) THEN
            VALUES( 1,NSUB)=1000*NSUB+9
            CVAL(NSUB)=CHAR(KINT(12))//CHAR(KINT(13))//CHAR(KINT(14))//
     1                 CHAR(KINT(15))//CHAR(kint(16))//'    '
            VALUES( 2,NSUB)=999999.
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
            VALUES( 5,NSUB)=999999.
cpsapr98            VALUES( 6,NSUB)=IYEAR+1900.
            VALUES( 6,NSUB)=FLOAT(IYEAR)
            VALUES( 7,NSUB)=FLOAT(IMONTH)
            if(KDEC(4).EQ.135) then
c              for drop sondes hour and minue from header
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(12))
               if(KDEC(1).ne.KDEC(10).or.kdec(2).ne.kdec(11)) then
                  if(abs(KDEC(1)-KDEC(10)).lt.2) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  else
                     VALUES( 8,NSUB)=FLOAT(KDEC(1))
                     VALUES( 9,NSUB)=FLOAT(KDEC(2))
                     VALUES(10,NSUB)=FLOAT(KDEC(9))
                  end if
                  if(abs(KDEC(2)-KDEC(11)).le.1.or.
     1               abs(KDEC(2)-KDEC(11)).eq.23) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  end if
               end if
            else
c              day hour and minute from data
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(9))
            end if
c
            VALUES(11,NSUB)=KDEC(5)/100.
            VALUES(12,NSUB)=KDEC(6)/100.
            VALUES(13,NSUB)=999999.
            IF(KDEC(4).EQ.137) THEN
               IF(KDEC(8).NE.MINDIC) VALUES(13,NSUB)=FLOAT(KDEC(8))
            ELSEIF(KDEC(4).EQ.36) THEN
               DO 292 I=1,100
               IF(YSNAME(I).EQ.CVAL(NSUB)) VALUES(13,NSUB)=SHGT(I)
 292           CONTINUE
            END IF
            VALUES(14,NSUB)=999999.
            VALUES(15,NSUB)=999999.
            VALUES(16,NSUB)=999999.
            VALUES(17,NSUB)=999999.
            VALUES(18,NSUB)=999999.
            VALUES(19,NSUB)=999999.
            VALUES(20,NSUB)=999999.
            VALUES(21,NSUB)=999999.
C
            IP=22
            IPKD=30
C
            DO 321 I=1,14
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+5
                                        GO TO 321
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=PRESU(I)
            VALUES(IP+1,NSUB)=32.
            VALUES(IP+2,NSUB)=999999.
            IF(KDEC(IPKD).NE.MINDIC)
     1       VALUES(IP+2,NSUB)=KDEC(IPKD)*9.8
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+3).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+4).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
            IF(KDEC(IPKD+4).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
            IP=IP+7
            IPKD=IPKD+5
C
 321        CONTINUE
C
C           TROPOPAUSE LEVEL
C
            IPKD=100
C
            DO 322 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=16.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
                  VALUES(IP+4,NSUB)=999999.
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+3).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+3))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+4).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))
                  IF(KDEC(IPKD+4).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+4))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 324
               END IF
C
 322        CONTINUE
C
 324  CONTINUE
C
C           MAX. WIND LEVEL
C
            IPKD=115
C
            DO 323 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IREP=IREP+1
                  VALUES(IP,NSUB)=KDEC(IPKD)*10.
                  VALUES(IP+1,NSUB)=8.
                  VALUES(IP+2,NSUB)=999999.
                  VALUES(IP+3,NSUB)=999999.
                  VALUES(IP+4,NSUB)=999999.
                  VALUES(IP+5,NSUB)=999999.
                  IF(KDEC(IPKD+1).NE.MINDIC)
     1               VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
                  VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr                  IF(KDEC(IPKD+2).NE.MINDIC)
crr     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
                  IF(KDEC(IPKD+2).NE.MINDIC)
     1               VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
                  IPKD=IPKD+5
                  IP=IP+7
               ELSE
                  GO TO 325
               END IF

C
 323        CONTINUE
C
 325  CONTINUE
C
C           UPDATE REPLICATION FACTOR
C
            VALUES(21,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C
C
C           WIND SHEAR AT MAX. WIND LEVEL
C
            IPKD=115
            ISHEAR=0 
            IPTSH=IP
            VALUES(IPTSH,NSUB)=0.0
C
            DO 326 I=1,3
C
            IF(KDEC(IPKD).NE.MINDIC)
     1         THEN
                  IF(KDEC(IPKD+3).NE.MINDIC) 
     1               THEN
                        ISHEAR=ISHEAR+1
                        VALUES(IPTSH,NSUB)=FLOAT(ISHEAR) 
cpsoct06 Inserted following line, then ajusted all following IP-indexes down one
cps      Previous version worked correctly only for one level
                        IF (ISHEAR.EQ.1) IP=IP+1 ! should be included just once
                        VALUES(IP,NSUB)=KDEC(IPKD)*10.
cpsdec11                        VALUES(IP+1,NSUB)=8.
                        VALUES(IP+1,NSUB)=10. ! maximum wind level & significant
                        VALUES(IP+2,NSUB)=999999.
cpssep06 changed accuracy of wind shear (vava)
                        IF(KDEC(IPKD+3).NE.MINDIC)
cps     1                  VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))
     1                  VALUES(IP+2,NSUB)=FLOAT(KDEC(IPKD+3))/10.0
                        VALUES(IP+3,NSUB)=999999.
crr  changed accuracy of wind speed  cps: wind shear (vbvb)
                       IF(KDEC(IPKD+4).NE.MINDIC)
crr     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))    
     1                  VALUES(IP+3,NSUB)=FLOAT(KDEC(IPKD+4))/10.    
C
                        IPKD=IPKD+5
cpssep06                        IP=IP+5
                        IP=IP+4
                     ELSE
                        GO TO 327
                     END IF
C
               ELSE
                  GO TO 327
               END IF

C
 326        CONTINUE
C
 327  CONTINUE
C
C     UPDATE M
C
      M=IP-1
C 
C
            RETURN
                    END IF
C     -----------------------------------------------------------------
C*          3.3  TEMP PART B.
C                ------------
 330  CONTINUE
C
         IF(K.EQ.2) THEN
            VALUES( 1,NSUB)=1000*NSUB+9
            CVAL(NSUB)=CHAR(KINT(12))//CHAR(KINT(13))//CHAR(KINT(14))//
     1                 CHAR(KINT(15))//CHAR(kint(16))//'    '
            VALUES( 2,NSUB)=999999.
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
            IF(KDEC(423).NE.MINDIC) VALUES(4,NSUB)=KDEC(423)
            VALUES( 5,NSUB)=999999.
cpsmar07 kdec(422)=sr=002013 in BUFR, not 002012
cpsmar07            IF(KDEC(422).NE.MINDIC) VALUES( 4,NSUB)=KDEC(422)
cpsapr98            VALUES( 6,NSUB)=IYEAR+1900.
            VALUES( 6,NSUB)=FLOAT(IYEAR)
            VALUES( 7,NSUB)=FLOAT(IMONTH)
            if(KDEC(4).EQ.135) then
c              for drop sondes hour and minue from header
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(12))
               if(KDEC(1).ne.KDEC(10).or.kdec(2).ne.kdec(11)) then
                  if(abs(KDEC(1)-KDEC(10)).lt.2) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  else
                     VALUES( 8,NSUB)=FLOAT(KDEC(1))
                     VALUES( 9,NSUB)=FLOAT(KDEC(2))
                     VALUES(10,NSUB)=FLOAT(KDEC(9))
                  end if
                  if(abs(KDEC(2)-KDEC(11)).le.1.or.
     1               abs(KDEC(2)-KDEC(11)).eq.23) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  end if
               end if
            else
c              day hour and minute from data
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(9))
            end if
c
            VALUES(11,NSUB)=KDEC(5)/100.
            VALUES(12,NSUB)=KDEC(6)/100.
            VALUES(13,NSUB)=999999.
            IF(KDEC(4).EQ.137) THEN
               IF(KDEC(8).NE.MINDIC) VALUES(13,NSUB)=FLOAT(KDEC(8))
            ELSEIF(KDEC(4).EQ.36) THEN
               DO 293 I=1,100
               IF(YSNAME(I).EQ.CVAL(NSUB)) VALUES(13,NSUB)=SHGT(I)
 293           CONTINUE
            END IF
            VALUES(14,NSUB)=999999.
            VALUES(15,NSUB)=1.       ! First significant layer
            VALUES(16,NSUB)=999999.
            IF(KDEC(386).NE.MINDIC) VALUES(16,NSUB)=FLOAT(KDEC(386))
            VALUES(17,NSUB)=999999.
            IF(KDEC(388).NE.MINDIC) VALUES(17,NSUB)=FLOAT(KDEC(388))
            VALUES(18,NSUB)=999999.
            IF(KDEC(387).NE.MINDIC) THEN
                                    CALL IC513(KDEC(387),KOUT)
                                    VALUES(18,NSUB)=FLOAT(KOUT)
                                    END IF
            VALUES(19,NSUB)=999999.
            IF(KDEC(389).NE.MINDIC) THEN
                                    CALL IC515(KDEC(389),KOUT)
                                    VALUES(19,NSUB)=FLOAT(KOUT)
                                    END IF
            VALUES(20,NSUB)=999999.
            IF(KDEC(390).NE.MINDIC) THEN
                                    CALL IC509(KDEC(390),KOUT)
                                    VALUES(20,NSUB)=FLOAT(KOUT)
                                    END IF
C
            VALUES(21,NSUB)=999999.
C
C           TEMPERATURE SIGNIFICANCE
C
            IP=22
            IPKD=25
C
            DO 331 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 331
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            sign=4.
            if(ipkd.eq.25.and.kint(17).eq.0.and.
     1         kint(18).ne.mindic)  sign=68.
c            if(ipkd.eq.25) sign=68.
            VALUES(IP+1,NSUB)=sign
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            VALUES(IP+6,NSUB)=999999.
C
            IP=IP+7
            IPKD=IPKD+3
C
 331        CONTINUE
C
C           WIND SIGNIFICANCE
C
C
            IPKD=206
C
            DO 332 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 332
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            sign=2.
            if(ipkd.eq.206.and.kint(318).eq.0.and.
     1         kint(319).ne.mindic) sign=66.
c            if(ipkd.eq.206) sign=66.
            VALUES(IP+1,NSUB)=sign
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            VALUES(IP+4,NSUB)=999999.
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+7
            IPKD=IPKD+3
C
 332        CONTINUE
C
            IPKD=392
C
            DO 333 I=1,4
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+6
                                        GO TO 333
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            VALUES(IP+1,NSUB)=6.
            VALUES(IP+2,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+2,NSUB)=KDEC(IPKD+1)*9.8
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+3).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+3)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+4).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+4))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+5).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+5))
            IF(KDEC(IPKD+5).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+5))/10.
C
            IP=IP+7
            IPKD=IPKD+6
C
 333        CONTINUE
C
C           UPDATE REPLICATION FACTOR
C
            VALUES(21,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
            IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C
C
C
            RETURN
                    END IF
C     -----------------------------------------------------------------
C*           3.4 TEMP PART D.
C                ------------
 340  CONTINUE
C
         IF(K.EQ.4) THEN
            VALUES( 1,NSUB)=1000*NSUB+9
            CVAL(NSUB)=CHAR(KINT(12))//CHAR(KINT(13))//CHAR(KINT(14))//
     1                 CHAR(KINT(15))//CHAR(kint(16))//'    '
            VALUES( 2,NSUB)=999999.
            VALUES( 3,NSUB)=999999.
            VALUES( 4,NSUB)=999999.
            VALUES( 5,NSUB)=999999.
cpsapr98            VALUES( 6,NSUB)=IYEAR+1900.
            VALUES( 6,NSUB)=FLOAT(IYEAR)
            VALUES( 7,NSUB)=FLOAT(IMONTH)
            if(KDEC(4).EQ.135) then
c              for drop sondes hour and minue from header
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(12))
               if(KDEC(1).ne.KDEC(10).or.kdec(2).ne.kdec(11)) then
                  if(abs(KDEC(1)-KDEC(10)).lt.2) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  else
                     VALUES( 8,NSUB)=FLOAT(KDEC(1))
                     VALUES( 9,NSUB)=FLOAT(KDEC(2))
                     VALUES(10,NSUB)=FLOAT(KDEC(9))
                  end if
                  if(abs(KDEC(2)-KDEC(11)).le.1.or.
     1               abs(KDEC(2)-KDEC(11)).eq.23) then
                     VALUES( 8,NSUB)=FLOAT(KDEC(10))
                     VALUES( 9,NSUB)=FLOAT(KDEC(11))
                     VALUES(10,NSUB)=FLOAT(KDEC(12))
                  end if
               end if
            else
c              day hour and minute from data
c
               VALUES( 8,NSUB)=FLOAT(KDEC(1))
               VALUES( 9,NSUB)=FLOAT(KDEC(2))
               VALUES(10,NSUB)=FLOAT(KDEC(9))
            end if
c
            VALUES(11,NSUB)=KDEC(5)/100.
            VALUES(12,NSUB)=KDEC(6)/100.
            VALUES(13,NSUB)=999999.
            IF(KDEC(4).EQ.137) THEN
               IF(KDEC(8).NE.MINDIC) VALUES(13,NSUB)=FLOAT(KDEC(8))
            ELSEIF(KDEC(4).EQ.36) THEN
               DO 294 I=1,100
               IF(YSNAME(I).EQ.CVAL(NSUB)) VALUES(13,NSUB)=SHGT(I)
 294           CONTINUE
            ELSE
            END IF
            VALUES(14,NSUB)=999999.
            VALUES(15,NSUB)=1.       ! First significant layer
            VALUES(16,NSUB)=999999.
            IF(KDEC(386).NE.MINDIC) VALUES(16,NSUB)=FLOAT(KDEC(386))
            VALUES(17,NSUB)=999999.
            IF(KDEC(388).NE.MINDIC) VALUES(17,NSUB)=FLOAT(KDEC(388))
            VALUES(18,NSUB)=999999.
            IF(KDEC(387).NE.MINDIC) THEN
                                    CALL IC513(KDEC(387),KOUT)
                                    VALUES(18,NSUB)=FLOAT(KOUT)
                                    END IF
            VALUES(19,NSUB)=999999.
            IF(KDEC(389).NE.MINDIC) THEN
                                    CALL IC515(KDEC(389),KOUT)
                                    VALUES(19,NSUB)=FLOAT(KOUT)
                                    END IF
            VALUES(20,NSUB)=999999.
            IF(KDEC(390).NE.MINDIC) THEN
                                    CALL IC509(KDEC(390),KOUT)
                                    VALUES(20,NSUB)=FLOAT(KOUT)
                                    END IF
C
            VALUES(21,NSUB)=999999.
C
c
c           temperature significance
C
            IP=22
            IPKD=25
C
            DO 341 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 341
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            VALUES(IP+1,NSUB)=4.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+3,NSUB)=KDEC(IPKD+1)/10.+273.16
            VALUES(IP+4,NSUB)=999999.
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+4,NSUB)=KDEC(IPKD+2)/10.+273.16
            VALUES(IP+5,NSUB)=999999.
            VALUES(IP+6,NSUB)=999999.
C
            IP=IP+7
            IPKD=IPKD+3
C
 341        CONTINUE
C
C           WIND SIGNIFICANCE
C
            IPKD=206
C
            DO 342 I=1,60
C
            IF(KDEC(IPKD).EQ.MINDIC) THEN
                                        IPKD=IPKD+3
                                        GO TO 342
                                     END IF
            IREP=IREP+1
            VALUES(IP,NSUB)=KDEC(IPKD)*10.
            VALUES(IP+1,NSUB)=2.
            VALUES(IP+2,NSUB)=999999.
            VALUES(IP+3,NSUB)=999999.
            VALUES(IP+4,NSUB)=999999.
            VALUES(IP+5,NSUB)=999999.
            IF(KDEC(IPKD+1).NE.MINDIC)
     1         VALUES(IP+5,NSUB)=FLOAT(KDEC(IPKD+1))
            VALUES(IP+6,NSUB)=999999.
crr  changed accuracy of wind speed
crr            IF(KDEC(IPKD+2).NE.MINDIC)
crr     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))
            IF(KDEC(IPKD+2).NE.MINDIC)
     1         VALUES(IP+6,NSUB)=FLOAT(KDEC(IPKD+2))/10.
C
            IP=IP+7
            IPKD=IPKD+3
C
 342        CONTINUE
C
C
C           UPDATE REPLICATION FACTOR
C
            VALUES(21,NSUB)=FLOAT(IREP)
            M=IP-1
C
C           CHECK IF THERE ARE ANY USEFUL DATA
C
           IF(IREP.EQ.0) THEN
               IERR=1
               RETURN
            END IF
C
C
C
            RETURN
         END IF
      END IF
C
      RETURN
      END
      SUBROUTINE IC509(KN,KOUT)
C
C**** *IC509*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO MAKE CONVERSION OF CODE FIGURS
C         FROM CODE TABLE 509  TO *BUFR* CODE TABLE 020012.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC509(KN,KOUT)*
C
C                *KN*      - INPUT CODE FIGURE
C                *KOUT*    - CONVERTED CODE FIGURE ON OUTPUT
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
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
      DIMENSION I509(10)
      DIMENSION I20012(10)

C
      DATA I509/0,1,2,3,4,5,6,7,8,9/
      DATA I20012/10,11,12,13,14,15,16,17,18,19/
C
C     ------------------------------------------------------------------
C
C*          1.   FIND CORRESPONDING CODE FIGURE OF 020012 CODE TABLE
C                ---- ------------- ---- ------ -- ------ ---- -----
 100  CONTINUE
C
      DO 101 J=1,10
C
      IF(I509(J).EQ.KN) THEN
                           KOUT=I20012(J)
                           RETURN
                        END IF
C
 101  CONTINUE
C
      KOUT=63
C
      RETURN
      END
      SUBROUTINE IC513(KN,KOUT)
C
C**** *IC513*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO MAKE CONVERSION OF CODE FIGURS
C         FROM CODE TABLE 513  TO *BUFR* CODE TABLE 020012.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC513(KN,KOUT)*
C
C                *KN*      - INPUT CODE FIGURE
C                *KOUT*    - CONVERTED CODE FIGURE ON OUTPUT
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
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
      DIMENSION I513(10)
      DIMENSION I20012(10)
C
      DATA I513/0,1,2,3,4,5,6,7,8,9/
      DATA I20012/30,31,32,33,34,35,36,37,38,39/
C
C     ------------------------------------------------------------------
C
C*          1.   FIND CORRESPONDING CODE FIGURE OF 020012 CODE TABLE
C                ---- ------------- ---- ------ -- ------ ---- -----
 100  CONTINUE
C
      DO 101 J=1,10
C
      IF(I513(J).EQ.KN) THEN
                           KOUT=I20012(J)
                           RETURN
                        END IF
C
 101  CONTINUE
C
      KOUT=63
C
      RETURN
      END
      SUBROUTINE IC515(KN,KOUT)
C
C**** *IC515*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO MAKE CONVERSION OF CODE FIGURS
C         FROM CODE TABLE 515  TO *BUFR* CODE TABLE 020012.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IC515(KN,KOUT)*
C
C                *KN*      - INPUT CODE FIGURE
C                *KOUT*    - CONVERTED CODE FIGURE ON OUTPUT
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
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
      DIMENSION I515(10)
      DIMENSION I20012(10)
C
      DATA I515/0,1,2,3,4,5,6,7,8,9/
      DATA I20012/20,21,22,23,24,25,26,27,28,29/
C
C     ------------------------------------------------------------------
C
C*          1.   FIND CORRESPONDING CODE FIGURE OF 020012 CODE TABLE
C                ---- ------------- ---- ------ -- ------ ---- -----
 100  CONTINUE
C
      DO 101 J=1,10
C
      IF(I515(J).EQ.KN) THEN
                           KOUT=I20012(J)
                           RETURN
                        END IF
C
 101  CONTINUE
C
      KOUT=63
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
      INCLUDE 'compoin.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'
CRR common for directories
      INCLUDE 'comdir.f'
C
C     ------------------------------------------------------------------
C*          1.   SET UP *BUFR* MESSAGE.
C                ----------------------
 100  CONTINUE
C
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' BUFRENC - start of subr., ierr ',IERR
CTEST
      IF( IERR.NE.0 ) RETURN
C
CTEST
ctestfile
ccc      IUNIT=96
C
ccc      ispc=char(32)
ccc      ndx=0
C  - Find end of character string. 
ccc      ndx=index(ytest(1:80),ispc) - 1
c
ccc               OPEN(UNIT=IUNIT,
ccc     1         FILE=YTEST(1:ndx)//'qctemp.dat',
ccc     2         ACCESS='APPEND',
ccc     3         CARRIAGECONTROL='LIST',
ccc     4         FORM='FORMATTED',
ccc     5         STATUS='UNKNOWN'        )
C
ccc      WRITE(IUNIT,10001)M,N,MQ,NSUB,NSNUM
ccc10001 FORMAT(1X,' M ',I5,' N ',I5,' MQ ',I5,' NSUB ',I5,' NSNUM ',I5)
ccc      DO 900 J=1,N
ccc      WRITE(IUNIT,10003) N
ccc10003 FORMAT(1X,' VALUES ',I5)
ccc      WRITE(IUNIT,10002)(VALUES(I,J),I=1,20)
ccc      WRITE(IUNIT,10002)(VALUES(I,J),I=21,M)
ccc      WRITE(IUNIT,10004)
ccc      WRITE(IUNIT,10002)(VALUES(I,J),I=M+1,M+20)
ccc      WRITE(IUNIT,10002)(VALUES(I,J),I=M+21,M+M)
ccc900   CONTINUE
C
ccc10004 FORMAT(1X,' VALUES m+1,m+m')
ccc10002 FORMAT(1X,7F11.2)
CTEST
C           1.1 SET WORKING TABLES AND BUFR SECTION DATA.
C               -----------------------------------------
 110  CONTINUE
C
      CALL SETWT( IERR )
cc      print*,' NBPW,NWPT,NBPT,mbuf(1),mbuf(2) ',
cc     * NBPW,NWPT,NBPT,mbuf(1),mbuf(2)
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
cpsdec03      CALL PACK(NBPW,MBUF(NWPT),98,NWPT,NBPT,16)
      CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT, 8)
      CALL PACK(NBPW,MBUF(NWPT),98,NWPT,NBPT, 8)

C
C     ------------------------------------------------------------------
C*          6.   SET UP UPDATE SEQUENCE NUMBER.
C                ------------------------------
 600  CONTINUE
C
cpsmar07      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT,8)
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
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
     1   KDEC(4).EQ.137) RYEAR=VALUES(6,1)

cpsapr98      IYEAR=NINT(RYEAR-1900.)
cpsapr98      CALL PACK(NBPW,MBUF(NWPT),IYEAR,NWPT,NBPT,8)
      iiyear=mod(nint(ryear),100)
      if (iiyear.eq.0) iiyear = 100
      CALL PACK(NBPW,MBUF(NWPT),iiyear,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          11.  SET UP MONTH.
C                -------------
 1100 CONTINUE
C
      IMONTH=VALUES(6,1)
      IF(KDEC(4).EQ.36.OR.
     1   KDEC(4).EQ.135.OR.KDEC(4).EQ.137) IMONTH=VALUES(7,1)
C
      CALL PACK(NBPW,MBUF(NWPT),IMONTH,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          12.  SET UP DAY.
C                -----------
 1200 CONTINUE
C
cpsmar09      IDAY=VALUES(7,1)
cpsmar09      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
cpsmar09     1   KDEC(4).EQ.137) IDAY=VALUES(8,1)
      IDAY = KDEC(1)
C
      CALL PACK(NBPW,MBUF(NWPT),IDAY,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          13.  SET UP HOUR.
C                -------------
 1300 CONTINUE
C
cpsmar09      IHOUR=VALUES(8,1)
cpsmar09      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
cpsmar09     1   KDEC(4).EQ.137) IHOUR=VALUES(9,1)
      IHOUR = KDEC(2)
C
      CALL PACK(NBPW,MBUF(NWPT),IHOUR,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          14.  SET UP MINUTE.
C                -------------
 1400 CONTINUE
C
cpsmar09      IMIN=VALUES(9,1)
cpsmar09      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
cpsmar09     1   KDEC(4).EQ.137) IMIN=VALUES(10,1)
C
      IMIN = KDEC(9)
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
C     TEMP- LAND STATION
C
      IF(KDEC(4).EQ.35) THEN
         CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),9,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 8)
         IF(LSHEAR) THEN
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 6)
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
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),61,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),62,NWPT,NBPT, 8)
         END IF
C
C        CHECK IF Q/C PERFORMED.
C
         IBIT1=0
         DO 161 I=M+1,M+M
            IF(VALUES(I,NSUB).NE.999999.) IBIT1=IBIT1+1
 161     CONTINUE
C
         IF(IBIT1.EQ.0) GO TO 175
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
C
         CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 8)
C
         CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 33,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),  7,NWPT,NBPT, 8)
C
 175     CONTINUE
C
C        CHECK IF SUBTITUTION PERFORMED.
C
         M2=M*2
         M3=M*3
         IBIT1=0
         DO 162 I=M2+1,M3
            IF(VALUES(I,NSUB).NE.999999.) IBIT1=IBIT1+1
 162     CONTINUE
C
         IF(IBIT1.EQ.0) GO TO 170
C
         MSS=M+8
         CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 23,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),000,NWPT,NBPT, 8)
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
C
         CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 8)
C
         CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 23,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),255,NWPT,NBPT, 8)
C
         GO TO 170
      END IF
C
C     TEMP - SHIP
C
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
         IF(KDEC(4).EQ.36) THEN
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),9,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),196,NWPT,NBPT, 8)
         END IF
         IF(KDEC(4).EQ.135) THEN
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),9,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),194,NWPT,NBPT, 8)
         END IF
         IF(KDEC(4).EQ.137) THEN
            CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),9,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),195,NWPT,NBPT, 8)
         END IF
         IF(LSHEAR) THEN
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 6)
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
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),61,NWPT,NBPT, 8)
c
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),62,NWPT,NBPT, 8)
         END IF
C
C        CHECK IF Q/C PERFORMED.
C
         IBIT1=0
         DO 163 I=M+1,M+M
            IF(VALUES(I,NSUB).NE.999999.) IBIT1=IBIT1+1
 163     CONTINUE
C
         IF(IBIT1.EQ.0) GO TO 176
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
 176     CONTINUE
C
C
C        CHECK IF SUBTITUTION PERFORMED.
C
         M2=M*2
         M3=M*3
         IBIT1=0
         DO 164 I=M2+1,M3
            IF(VALUES(I,NSUB).NE.999999.) IBIT1=IBIT1+1
 164     CONTINUE
C
         IF(IBIT1.EQ.0) GO TO 170
C
         MSS=M+8
         CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 23,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),000,NWPT,NBPT, 8)
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
C
         CALL PACK(NBPW,MBUF(NWPT),  0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 31,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 8)
C
         CALL PACK(NBPW,MBUF(NWPT),  2,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT), 23,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),255,NWPT,NBPT, 8)
C
      END IF
C
C     ------------------------------------------------------------------
C*          1.7  SET UP LENGTH OF THE SECTION 3.
C                -------------------------------
 170  CONTINUE
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
                IPACK=NINT(VAL/10.**ISCALE) - IREF
             ELSE
                IPACK=NINT(VAL*10.**ISCALE) - IREF
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
                IPACK=NINT(VAL/10.**ISCALE) - IREF
             ELSE
                IPACK=NINT(VAL*10.**ISCALE) - IREF
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
              IPACK=NINT(VAL/10.**ISCALE) - IREF
           ELSE
              IPACK=NINT(VAL*10.**ISCALE) - IREF
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
C*            3. SUBSTITUTED VALUES.
C                -------------------
 300  CONTINUE
C
      I031002=0
      IF(M.LT.MSS) THEN
C
        DO 310 J=M+5,MSS      
C
        IREV  =IWTR (J)
        IREF  =IWTRV(J)
        ISCALE=IWTS (J)
        IBDW  =IWTDW(J)
        YUNIT =CUNIT(J)
C
C       031002 IS EXTENDED DELAYED REPLICATION FACTOR
C
        IF(IREV.EQ.031002.AND.I031002.EQ.0) THEN
C 
           I031002=1
c          set replication factor for data present indicator
c          for substituted values
c
c
           CALL PACK(NBPW,MBUF(NWPT),M,NWPT,NBPT,IBDW)
C
C          SET BIT FLAG
C
           IBIT1=0
C
           M2=M*2
           M3=M*3
           DO 315 JI=M2+1,M3
            IPACK=1
            IF(VALUES(JI,I).NE.999999.) IPACK=0
            CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,1)
 315       CONTINUE
C           
           GO TO 310
C
        END IF
        IF(IREV.EQ.001031) THEN
C 
C          SET GENERATING CENTER
C
           CALL PACK(NBPW,MBUF(NWPT),98,NWPT,NBPT,IBDW)
C
        END IF
        IF(IREV.EQ.001032) THEN
C 
C          SET GENERATING APPLICATION
C
C          255 - MISSING VALUE( ALL BITS SET TO ONE)
C
           CALL PACK(NBPW,MBUF(NWPT),10,NWPT,NBPT,IBDW)
C
        END IF
C
C       EXTENDED DELAYED REPLICATION FACTOR  FOR MARKER OPERATOR
C       223255
C
        IF(IREV.EQ.031002.AND.I031002.EQ.1) THEN
           IBIT1=0
           M2=M*2
           M3=M*3
           DO 417 IK=M2+1,M3
            IF(VALUES(IK,I).NE.999999.) IBIT1=IBIT1+1
 417       CONTINUE
C
           CALL PACK(NBPW,MBUF(NWPT),IBIT1,NWPT,NBPT,IBDW)
C
C          PACK SUBSTITUTED VALUES
C
           M2=M*2
           M3=M*3
           J1=0
           DO 418 JI=M2+1,M3
           J1=J1+1
           VAL=VALUES(JI,I)
           IF(VAL.EQ.999999.) GO TO 418
C
           IREF  =IWTRV(J1)
           ISCALE=IWTS (J1)
           IBDW  =IWTDW(J1)
C
           IF(ISCALE.LT.0) THEN
              ISCALE=IABS(ISCALE)
              IPACK=NINT(VAL/10.**ISCALE) - IREF
           ELSE
              IPACK=NINT(VAL*10.**ISCALE) - IREF
           END IF
C
           CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
C
 418       CONTINUE
C
        END IF
C
 310    CONTINUE
C
      END IF
C
 201  CONTINUE
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
c
      INCLUDE 'comwork.f'
      INCLUDE 'comtab.f'
      INCLUDE 'comtabc.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
C
      DIMENSION ITBTRL(40),ITBTRS(41),ITBTRA(JP22)
C
      DATA ITBTRL/001001,001002,002011,002012,004001 
     1,           004002,004003,004004,004005,005001
     2,           006001,007001,020010,008002,020011 
     3,           020013,020012,020012,020012,031001
     4,           007004,008001,010003,012001,012003
     5,           011001,011002,031001,007004,008001
     6,           011061,011062,031031,001031,001032
     7,           033007,031002,001031,001032,031002/
      DATA ITBTRS/001011,001012,001013,002011,002012
     1,           004001,004002,004003,004004,004005
     2,           005002,006002,007001,020010,008002,020011 
     3,           020013,020012,020012,020012,031001
     4,           007004,008001,010003,012001,012003
     5,           011001,011002,031001,007004,008001
     6,           011061,011062,031031,001031,001032
     7,           033007,031002,001031,001032,031002/
C     ------------------------------------------------------------------
C*          1.   SET UP WORKING TABLES FOR TEMP DATA.
C                ------------------------------------
 100  CONTINUE
C
      IF(IERR.NE.0) RETURN
C
      LSHEAR=.FALSE.
C
      IF(KDEC(4).EQ.35) THEN
            j=0
c
            DO 101 I=1,20
            j=j+1
            itbtra(j)=itbtrl(i)
 101        continue
c  
            jj=21
c
            irep=NINT(values(20,nsub))
c
            do 102 i=1,irep
c
            j=j+1
            itbtra(j)=itbtrl(jj  )
            j=j+1
            itbtra(j)=itbtrl(jj+1)
            j=j+1
            itbtra(j)=itbtrl(jj+2)
            j=j+1
            itbtra(j)=itbtrl(jj+3)
            j=j+1
            itbtra(j)=itbtrl(jj+4)
            j=j+1
            itbtra(j)=itbtrl(jj+5)
            j=j+1
            itbtra(j)=itbtrl(jj+6)
C
 102        continue
c
            is=irep*7+20
            if(is.lt.m)
     1         then
                  lshear=.true. 
                  j=j+1
                  itbtra(j)=itbtrl(28)
                  iptsh=is+1
                  ireps=NINT(values(iptsh,nsub))
c
                  jj=29
c
                  do 103 i=1,ireps
                  j=j+1
                  itbtra(j)=itbtrl(jj  )
                  j=j+1
                  itbtra(j)=itbtrl(jj+1)
                  j=j+1
                  itbtra(j)=itbtrl(jj+2)
                  j=j+1
                  itbtra(j)=itbtrl(jj+3)
 103              continue
c
               end if
C
C           ADD QUALETY CONTROL DECSCRIPTORS.
C
            J=J+1
            ITBTRA(J)=ITBTRL(33)
            J=J+1
            ITBTRA(J)=ITBTRL(34)
            J=J+1 
            ITBTRA(J)=ITBTRL(35)
            J=J+1 
            ITBTRA(J)=ITBTRL(36)
C
C           ADD SUBSTITUTED VALUES DECSRIPTOR
C
            J=J+1
            ITBTRA(J)=ITBTRL(37)
            J=J+1 
            ITBTRA(J)=ITBTRL(38)
            J=J+1 
            ITBTRA(J)=ITBTRL(39)
            J=J+1
            ITBTRA(J)=ITBTRL(40)
C
            GO TO 110
         END IF
C
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
            J=0
c
            DO 111 I=1,21
            j=j+1
            itbtra(j)=itbtrs(i)
 111        continue
c  
            jj=22
c
            irep=NINT(values(21,nsub))
c
            do 112 i=1,irep
c
            j=j+1
            itbtra(j)=itbtrs(jj  )
            j=j+1
            itbtra(j)=itbtrs(jj+1)
            j=j+1
            itbtra(j)=itbtrs(jj+2)
            j=j+1
            itbtra(j)=itbtrs(jj+3)
            j=j+1
            itbtra(j)=itbtrs(jj+4)
            j=j+1
            itbtra(j)=itbtrs(jj+5)
            j=j+1
            itbtra(j)=itbtrs(jj+6)
C
 112        continue
c
            is=irep*7+21
            if(is.lt.m)
     1         then
                  lshear=.true.
                  j=j+1
                  itbtra(j)=itbtrs(29)
                  iptsh=is+1
                  ireps=NINT(values(iptsh,nsub))
c
                  jj=30
c
                  do 113 i=1,ireps
                  j=j+1
                  itbtra(j)=itbtrs(jj  )
                  j=j+1
                  itbtra(j)=itbtrs(jj+1)
                  j=j+1
                  itbtra(j)=itbtrs(jj+2)
                  j=j+1
                  itbtra(j)=itbtrs(jj+3)
 113              continue
c
               end if
C
C           ADD QUALETY CONTROL DECSCRIPTORS.
C
            J=J+1
            ITBTRA(J)=ITBTRS(34)
            J=J+1
            ITBTRA(J)=ITBTRS(35)
            J=J+1 
            ITBTRA(J)=ITBTRS(36)
            J=J+1 
            ITBTRA(J)=ITBTRS(37)
C
C           ADD SUBSTITUTED VALUE DECSCRIPTORS.
C
            J=J+1
            ITBTRA(J)=ITBTRS(38)
            J=J+1 
            ITBTRA(J)=ITBTRS(39)
            J=J+1 
            ITBTRA(J)=ITBTRS(40)
            J=J+1
            ITBTRA(J)=ITBTRS(41)
C
         END IF
C
 110  CONTINUE
C
      IF(KDEC(4).EQ.135) ITBTRA(1)=001194
      IF(KDEC(4).EQ.137) ITBTRA(1)=001195
C
      DO 121 I=1,J
C
      ITR=ITBTRA(I)
c
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
C        *call* *asctim*
C        *call* *isfile*
C        *call* *rlunit* (libmi)
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
      CHARACTER*9 CIDENT
      CHARACTER*48 CKEY,YKEYOLD
      CHARACTER*32 YKEY
      CHARACTER*16 YTYPE
cpsapr98      CHARACTER*12 CFNAME
      CHARACTER*14 CFNAME
      CHARACTER*14 YTIME
      CHARACTER*23 CTIME
      CHARACTER*10 CUSE
cps      CHARACTER*29 YCFNAME   brukes ikke!
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
     1 PRINT*,' PUTBUFR - start of subr., ierr ', IERR
CTEST
C
      IF(IERR.NE.0) RETURN
C
      IRET=0
cpsjun03      YTYPE='temperature'
      YTYPE='temp'
      if (msys.eq.4) YTYPE='amte'
cps
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
      PRINT*,'BAD DATE OR TIME. REPORT SKIPPED. IOS=',IOS
      RETURN
C
 102  CONTINUE
C
C     DEFINE FILE NAME
C
CTEST open file tempxxxxxxxx.rdb for testing
      CALL ISFILE(YTYPE,YTIME,CFNAME,IRET)
CTEST
CTEST PRINT*,' PUTBUFR - cfname ', CFNAME
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
CRR   Copied from pilo.f 
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
C
CTEST
CTEST PRINT*,' PUTBUFR - QC: M,MQ,NSUB ', M,MQ,NSUB
CTEST
      IF(M.LT.MQ) THEN
                     RMIN=VALUES(M+1,NSUB)
                     DO 221 I=M+1,M+M
                     IF(VALUES(I,NSUB).LT.RMIN) RMIN=VALUES(I,NSUB)
 221                 CONTINUE
                     NQC=NINT(RMIN) 
                  END IF
CTEST
      PRINT*,' PUTBUFR - QC: NQC ', NQC
CTEST
C      
C
C
      CALL ASCTIM(CTIME)
      READ(CTIME,'(I2,10X,I2,1X,I2,1X,I2,1X,I2)') IDD,IHH,IMM,ISS,ICC

CTEST
CTEST PRINT*,' PUTBUFR - idd,ihh,imm,iss,icc ',IDD,IHH,IMM,ISS,ICC
CTEST
C
C       SET LENGTH OF RECORD (=ksec0(3) in bufrdc.f)
C
      IW=9
      IBT=0
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
      CALL PACK(32,IKEY(IW),NRDAY,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRHOUR,IW,IBT,5)      
      CALL PACK(32,IKEY(IW),NRMIN,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRSEC,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(1),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(1),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(2),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(2),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(3),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(3),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORN(4),IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NPART(4),IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NQC,IW,IBT,8)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,24)      
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
C
CTEST
CTEST PRINT*,' PUTBUFR - NCORR ', NCORR
CTEST      PRINT*,' PUTBUFR - CIDENT  ', CIDENT
CTEST      PRINT*,' PUTBUFR - CKEY  ', CKEY
CTEST      PRINT*,' PUTBUFR - YKEY  ', YKEY
CTEST
CRR  Where does NCORR get a value from ???
CRR  In temp program this part will not happen i.e. no overwrite of
CRR  old records. This is different in syno becuase ther is an extra test here.
CRR  This tests if KDEC(21).eq.mindic
CRR  n.b.  This test has been added (copied from pilo.f.
      IF(NCORR.NE.0) THEN
CTEST
      Print*,' This part has not been written to overwrite old records '
      PRINT*,' ***** PUTBUFR - NCORR NE 0 ******', NCORR
CTEST
      END IF
C
C
C*         3.2 CHECK IF FILE IS ALREADY OPENED.
C              --------------------------------
 320  CONTINUE
c
cpsdec02 All other obs types use recl 512*4 bytes
cpsdec20 The next comments are no longer relevant (don't use bufr_obs anymore)
cpsxx      NB: if iurecl is changed - must also change recl when opening 
cpsxx      .rdb-file in bufr_obs.f. Might also have to change recl in 
cpsxx      MERGE file
      call rlunit(lrunit)
cpsmai05      iurecl = 750*4/lrunit
cpsdec20      iurecl = 800*4/lrunit
cpsjan21      iurecl = 850*4/lrunit
      iurecl = 900*4/lrunit
cps
CTEST Check if .rdb file already opened
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
               OPEN(UNIT=IU,
     1         FILE=cstream(msys)//'/'//CFNAME//'.rdb',
     2         ACCESS='DIRECT',
     3         RECL=iurecl,
     4         FORM='UNFORMATTED',
     5         STATUS='UNKNOWN'        )
C
C using NTEST for record number, declared in comdir.f: common comtest
      NTEST(IU)=0
      PRINT*,' PUTBUFR - IU',IU
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
crr ilength is length of bufr message    cps: in bytes
crr ilenout is length of bufr message + [cps: length of] mbuf(1)
cps Note: Because of the equivalence mbuf <-> yout, we will write to
cps .rdb-file first mbuf(1) = length of bufr message, then as seen in
cps sec0.f, the characters 'B','U','F','R', then mbuf(3) = mbuf(1).
cps The starting mbuf(1) is not part of the real Bufr-message, and 
cps will be skipped by the Bufr-decoder (but READIN in bufr_obs_ex.f uses
cps ilen=mbuf(1)). Since mbuf is an integer array and integer is 32 bits
cps  = 4 bytes, this explains why we set ilenout=ilength+4
      ilenout=ilength+4
cpsdec02      if(ilenout.le.2720) then
      if(ilenout.le.iurecl) then
cps Note: ilenout is number of bytes, recl is number of words, and word 
cps = 4 bytes on SGI (while on Linux recl is measured in bytes!)
c
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
cc       PRINT*,' PUTBUFR - IU,NTEST(IU),ILENGTH ',IU,NTEST(IU),ILENGTH
C
C  - Write keys to test file '.key'
crr changed 941207 write current(decoding) time
cpsapr01 added writing of correction field NCORR (>0 if CCA/CCB/...)crr
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
         write(*,10002) ilenout,iurecl
10002    format(1X,'ERROR: PUTBUFR - length of bufr', I8,
     *        ' gt rec. length of .rdb', I7)
         call exit(2)
      end if
CTEST
CRR   nwrep=nwrep+1
C
C
C     -----------------------------------------------------------------
 400  CONTINUE
c
      RETURN
      END
cps Not used!
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
      INCLUDE 'comindx.f'
      INCLUDE 'combuff.f'
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
CRR
      CHARACTER*14 cimport
      data cimport/'important_temp'/
CRR
      Y63=CHAR(63)
      YGS=CHAR(13)//CHAR(13)//CHAR(10)//CHAR(3)
C     ------------------------------------------------------------------
C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 100  CONTINUE
C
      OTEMP=.FALSE.
      OIMPT=.FALSE.
C
      IF(KDEC(4).EQ.35.OR.KDEC(4).EQ.36.OR.
     1   KDEC(4).EQ.135.OR.KDEC(4).EQ.137) OTEMP=.TRUE.
C
      IF(OTEMP) THEN
         IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
            OIMPT=.TRUE. 
            GO TO 110
         ELSE
            DO 101 I=1,2000
            IF(KINT(4).NE.IMPSTA(I)) GO TO 101
            OIMPT=.TRUE.
            GO TO 110
 101        CONTINUE
C
cps why this????
cpsoct06            IF(KERR.GT.1.AND.KERR.GE.4) OIMPT=.TRUE.        
C
         END IF
      END IF
C
      YLINE=' '
      YLINEA=' '
C
      IUNIT=11
CRR      IF(OIMPT) IUNIT=10
C
C
CRR      IF(OIMPT) THEN
CRR         OPEN(UNIT=IUNIT,IOSTAT=IOS,ERR=400,
CRR     C     FILE=cstream(msys)//'/'//'temp_important_error_file.dat',
CRR     C     ACCESS='APPEND',
CRR     C     STATUS='UNKNOWN'    )
C
CRR      ELSE
         OPEN(UNIT=IUNIT,IOSTAT=IOS,ERR=400,
     C     FILE=cstream(msys)//'/'//'temp_error_file.dat',
#ifdef FORTRAN90
     C         POSITION='APPEND',
#else
     C         ACCESS='APPEND',
#endif
     C     STATUS='UNKNOWN'    )
C
CRR      END IF  
CRR Mark important temp on the error file 
      IF(OIMPT) THEN
      WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS)  cimport     
      END IF  
CRR  
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
      WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
         ISIGN=0
      END IF
C
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
      WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
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
      WRITE(IUNIT,'(A)') YGS
      CLOSE(IUNIT) 
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
      INCLUDE 'comindx.f'
      INCLUDE 'combuff.f'
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
CRR
      CHARACTER*14 cimport
      data cimport/'important_temp'/
CRR
      Y63=CHAR(63)
      YGS=CHAR(13)//CHAR(13)//CHAR(10)//CHAR(3)
C     ------------------------------------------------------------------
C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
 100  CONTINUE
C
CTEST
      PRINT*,' SAVREP - start of subr., ihead ',IHEAD
CTEST
      OTEMP=.FALSE.
      OIMPT=.FALSE.
C
      IF(KDEC(4).EQ.35.OR.KDEC(4).EQ.36.OR.
     1   KDEC(4).EQ.135.OR.KDEC(4).EQ.137) OTEMP=.TRUE.
C
      IF(OTEMP) THEN
         IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
            OIMPT=.TRUE. 
            GO TO 110
         ELSE
            DO 101 I=1,2000
            IF(KINT(4).NE.IMPSTA(I)) GO TO 101
            OIMPT=.TRUE.
            GO TO 110
 101        CONTINUE
C
cps why this????
cpsoct06            IF(KERR.GT.1.AND.KERR.GE.4) OIMPT=.TRUE.        
C
         END IF
      END IF
C
 110  CONTINUE
C
      YLINE=' '
      YLINEA=' '
CRR  
      IUNIT=11
CRR      IF(OIMPT) IUNIT=10 
C
CRR      IF(OIMPT) THEN
CRR         OPEN(UNIT=10,IOSTAT=IOS,ERR=400,
CRR     C     FILE=cstream(msys)//'/'//'temp_important_error_file.dat',
CRR     C     ACCESS='APPEND',
CRR     C     STATUS='UNKNOWN'     )
C
CRR      ELSE
         OPEN(UNIT=11,IOSTAT=IOS,ERR=400,
     C     FILE=cstream(msys)//'/'//'temp_error_file.dat',
#ifdef FORTRAN90
     C     POSITION='APPEND',
#else
     C     ACCESS='APPEND',
#endif
     C     STATUS='UNKNOWN'    )
C
CRR      END IF
CRR Mark important temp on the error file 
      IF(OIMPT) THEN
      WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS)  cimport     
      END IF  
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
         if(k.gt.80) k=80
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
      WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N2)
      IF(ISIGN.EQ.1) THEN
         WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N2)
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
C                 ---------------------------------
 130  CONTINUE
C
      IF ( IHEAD.EQ.2 ) GO TO 140
C
C
C     AVOID WRITING REPORT TWICE IF MIMIMJMJ  LINE IS
C     MISSING.
C
      IF ( IAH.EQ.KPT ) RETURN
C
C     AVOID WRITING MIMIMJMJ TWICE ( CORRUPT MIMIMJMJ CAN BE TAKEN
C     AS STATION NUMBER )
C
      IF ( KPT.LT.IFIN(4) ) KPT = IFIN(4)
C
C
      IP = KPT
      IF(KDEC(4) .EQ. 35 .OR. KDEC(4) .EQ. 36) IP = IMI
      IF(KDEC(4) .EQ.135 .OR. KDEC(4) .EQ.137) IP = IMI
      IF(KDEC(4) .EQ. 32 .OR. KDEC(4) .EQ. 33) IP = IMI
      J = IEQ+ 3
      IF ( J.GT.IGS) J = IGS
C
  133 CALL NEXTPRT (IP,J)
      IF (IP.GT.J) RETURN
      JP = IP
      CALL NEXTEND(JP,J)
      CALL NEXTPRT(JP,J)
      K= JP - IP 
      if(k.gt.80) k=80
      N1 = 0
      DO 131 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127) THEN
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
      WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
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
      WRITE (IUNIT,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:4)
C
      CLOSE(IUNIT)
C  
      GO TO 400
C     ----------------------------------------------------------------
 300  CONTINUE
C
      IERR = -1
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
      SUBROUTINE MRGTEM( IERR)
C
C**** *MRGTEM*
C
C
C     PURPOSE.
C     --------
C         MERGE TEPM PARTS A, B, C AND D INTO SINGLE OBSERVATION.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *MRGTEM( IERR )*
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
C     Aug 2003 P.S.: replaced use of indexed file for MERGE file with direct access
C     Dec 2003 P.S.: swapping bytes in MERGE file if different endianess
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f' 
CRR common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
c
      CHARACTER*12 CTSTAMP
      CHARACTER*4 cstream(40)
      logical ocont(4),new_record,swap,swapfile,file_exist
C
      DIMENSION VALUESA(2200),VALUESB(2200),VALUESC(2200)
      DIMENSION PRESS(400),INX(400),INXA(400)
      DIMENSION cvala(10)
C
      CHARACTER CVAL*9,CVALA*9,CIDENT*9
      CHARACTER*2 YMONTH,YDAY,YHOUR,YMIN
      character*4 year
      CHARACTER*80 YFILENAME
      character*3 cmonth(12)
      character*23 ct,current      
      CHARACTER*21 YSD
      CHARACTER ctmp*9
      integer keylen,lrunit,maxva
      PARAMETER (maxva=2200)
C
      DATA VALUESA/maxva*0.0/,PRESS/400*0.0/,INX/400*0/,INXA/400*0/
      data valuesb/maxva*0.0/,valuesc/maxva*0.0/,idimvalues/maxva/
      data cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     1            'Aug','Sep','Oct','Nov','Dec'/
c
      integer*2 endian,Eendian
cpsjun11      structure /merge/
      TYPE :: merge
         character*21 STATION_DATE
         logical*1 Oocont(4)
         integer*4 Nncorn(4)
         integer*4 Mma
         real      Vvaluesa(maxva)
         character*9 Ccvala(10)
cpsjun11      end structure
      end TYPE merge
c
cpsjun11      record /merge/ old_temp
      TYPE (merge) old_temp
      integer iget_record_number,iget_next_recnumber
      DATA keylen/21/
c
cps   oocont: oocont(i)=.true. if the old record exist and contains part i (1=A, 2=B, 3=C, 4=D)
c     nncorn: ncorn(i) = value av kdec(21) = 0 if no correction, 1 if CCA, 2 if CCB etc for old record part i
c     mma: number of data descriptors for old record
c     vvaluesa: values of data fields for old record
c     ccvala: station identificator
c     ocount,ncorn,ma,valuesa,cvala: first set to value of oocount etc of old record
c           read in from MERGE file, then updated to the value we want to write to
c           new MERGE file.
c     irepp: index of delayed descriptor in VALUES (how many pressure levels)
c     npart: same as ocont. Part of COMMON comkey, gets its values here
c     istno: station number as integer (e.g. 1400 for 01400)
c     is: index of last data descriptor in pressure levels in VALUES
c     isa: ditto for record in MERGE file
c     isb,isc: ditto for new record to be written
c     valuesb: will contain all data values in old and new record
c     valuesc: = valuesb, where pressure levels have been sorted and duplicate levels removed.
c              This is the values to be finally written to MERGE file, and also
c              rewritten to VALUES for later encoding to bufr report.
c     new_record: set to true if this temp station and termin is not present
c                 in MERGE file
c     endian: set to 1 (possibly byte swapped); each merge record is preceded by this 
c             value to be able to determine if merge file is little endian or big endian.
c             swapfile() may then be used to decide if we need to swap bytes later.
cps
C     ------------------------------------------------------------------
C
C*          1.   SET UP RECORD NUMBER FOR DIRECT ACCESS.
C                ---------------------------------------
 100  CONTINUE
C
      IF(IERR.NE.0) RETURN
C  
      IREPP=21
      IF(KDEC(4).EQ.35) IREPP=20
C
      IREPP1=IREPP+1
C
      ocont(1)=.false.
      ocont(2)=.false.
      ocont(3)=.false.
      ocont(4)=.false.
C
      NCORN(1)=0
      NCORN(2)=0
      NCORN(3)=0
      NCORN(4)=0
C
      NPART(1)=0
      NPART(2)=0
      NPART(3)=0
      NPART(4)=0
c
      new_record=.false.
C
C*          1.1  READ IN PREVIOUS REPORT IF ANY.
C                --------------------------------
 110  CONTINUE
C
C     DEFINE FILE NAME
C
      IYEAR=nint(VALUES(5,1))
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
     1   KDEC(4).EQ.137) IYEAR=nint(VALUES(6,NSUB))
      IMONTH=VALUES(6,1)
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
     1   KDEC(4).EQ.137) IMONTH=VALUES(7,NSUB)
      IDAY=VALUES(7,1)
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
     1   KDEC(4).EQ.137) IDAY=VALUES(8,NSUB)
      IHOUR=VALUES(8,1)
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
     1   KDEC(4).EQ.137) IHOUR=VALUES(9,NSUB)
      Imin=VALUES(9,1)
      IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.
     1   KDEC(4).EQ.137) IMIN=VALUES(10,NSUB)
c
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
c
      idf=nm1-nm0
c
      if(idf.le.0) then
         print*,' mrgtemp: data for future',ct
         ierr=1
         return
      end if
C
      if(idf.gt.2880) then         ! 2 days 
         print*,' mrgtemp: data too old ',ct
         ierr=1
         return
      end if
C
 115  CONTINUE
C
      WRITE(YEAR,'(I4)') IYEAR
      WRITE(YMONTH,'(I2.2)') IMONTH
      WRITE(YDAY,'(I2.2)') IDAY
      WRITE(YHOUR,'(I2.2)') IHOUR
      WRITE(YMIN,'(I2.2)') IMIN
c
cc      print*,'mrgtem1 - cident,cval(1):',cident,cval(1)
      if(kdec(4).eq.135) then
c  create temporary call sign for drop sondes (temp drop) using lat/long info
c  this is used as a key for the MERGE file.
c  save call sign in ctmp
         ctmp(1:9)=cval(1)(1:9)
         ill=iabs(nint(values(11,1)*100))
         ilo=iabs(nint(values(12,1)*100))
         write(cval(1)(1:4),'(i4.4)') ill
         write(cval(1)(5:9),'(i5.5)') ilo
cc      print*,'mrgtem2 - cident,cval(1):',cident,cval(1)
      end if

C
      OLD_TEMP%STATION_DATE=' '
C
      IF(KDEC(4).EQ.35) THEN
         ISTNO=NINT(VALUES(1,NSUB))*1000+NINT(VALUES(2,NSUB))
         WRITE(OLD_TEMP%STATION_DATE(1:5),'(I5.5)') ISTNO
      ELSE
         OLD_TEMP%STATION_DATE(1:9)=CVAL(1)
      END IF
C
      IF(KDEC(4).EQ.35) THEN
         OLD_TEMP%STATION_DATE(6:17)=YEAR//YMONTH//YDAY//YHOUR//YMIN
      ELSE
         II=INDEX(OLD_TEMP%STATION_DATE,' ')
         OLD_TEMP%STATION_DATE(II:21)=YEAR//YMONTH//YDAY//YHOUR//YMIN
      END IF
C
      YSD(1:21)=OLD_TEMP%STATION_DATE
C
      YFILENAME=cstream(msys)//'/'//'temp'//YEAR//YMONTH//YDAY//'MERGE'
C
      inquire(file=YFILENAME,exist=file_exist)
      call rlunit(lrunit)
      OPEN(UNIT=7,IOSTAT=IOS,ERR=120,FILE=YFILENAME,
     1            ACCESS='DIRECT',
     2            FORM='UNFORMATTED',
     4            RECL=2250*4/lrunit,
     5            STATUS='UNKNOWN')
C
c     Decide endianess of merge file by investigating first 2 bytes of file
      swap=.false.
      if(file_exist) swap=swapfile(-7)

      iunit=8
      irec=iget_record_number(YFILENAME,iunit,OLD_TEMP%STATION_DATE)
      if(irec.lt.0) goto 130

      READ(UNIT=7,rec=irec,IOSTAT=IOS,ERR=130) Eendian,OLD_TEMP
      if(swap) then
         call bswap4(4,old_temp%nncorn)
         call bswap4(1,old_temp%mma)
         call bswap4(maxva,old_temp%vvaluesa)
      end if
C
      DO 12 I=1,4
         OCONT(I)=OLD_TEMP%OOCONT(I)
         NCORN(I)=OLD_TEMP%NNCORN(I)
 12   CONTINUE
C
      MA=OLD_TEMP%MMA
C
      DO 13 I=1,maxva
         VALUESA(I)=OLD_TEMP%VVALUESA(I)
 13   CONTINUE
C
      DO 14 I=1,10
         CVALA(I)=OLD_TEMP%CCVALA(I)
 14   CONTINUE
C
      GO TO 200
C
 120  CONTINUE
C
      PRINT*,'FILE NAME=',YFILENAME 
      WRITE(*,9901) IOS
      call exit(2)
C
 121  CONTINUE
C
      PRINT*,'FILE NAME=',YFILENAME 
      WRITE(*,9902) IOS
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
      CVALA(1)=CVAL(1)
      DO 131 I=1,M
         VALUESA(I)=VALUES(I,NSUB)
 131  CONTINUE
C     
C     ------------------------------------------------------------------
C*           2. MERGE NEW AND OLD REPORT.
C               -------------------------
 200  CONTINUE
c KDEC(21)=1 if BULLETIN IS 'COR' , OTHERWISE= 0 .
c KDEC(21) = 1 IF BULLETIN IS 'CCA', = 2 IF BULLETIN IS 'CCB', etc.
C
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
         NCORN(1)=KDEC(21)
         OCONT(1)=.TRUE.
         GO TO 230
      END IF
C
cpsjun11      IF(KDEC(23).EQ.2.AND.OCONT(2).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.2.AND.OCONT(2)) THEN
         IF(KDEC(21).LE.NCORN(2)) THEN
            GO TO 231
         ELSE
            NCORN(2)=KDEC(21)
            GO TO 230
         END IF 
      END IF
cpsjun11      IF(KDEC(23).EQ.2.AND.OCONT(2).EQ..FALSE.) THEN
      IF(KDEC(23).EQ.2.AND..NOT.OCONT(2)) THEN
         NCORN(2)=KDEC(21)
         OCONT(2)=.TRUE.
         GO TO 230
      END IF
C
cpsjun11      IF(KDEC(23).EQ.3.AND.OCONT(3).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.3.AND.OCONT(3)) THEN
         IF(KDEC(21).LE.NCORN(3)) THEN
            GO TO 231
         ELSE
            NCORN(3)=KDEC(21)
            GO TO 230
         END IF 
      END IF
cpsjun11      IF(KDEC(23).EQ.3.AND.OCONT(3).EQ..FALSE.) THEN
      IF(KDEC(23).EQ.3.AND..NOT.OCONT(3)) THEN
         NCORN(3)=KDEC(21)
         OCONT(3)=.TRUE.
         GO TO 230
      END IF
C
cpsjun11      IF(KDEC(23).EQ.4.AND.OCONT(4).EQ..TRUE.) THEN
      IF(KDEC(23).EQ.4.AND.OCONT(4)) THEN
         IF(KDEC(21).LE.NCORN(4)) THEN
            GO TO 231
         ELSE
            NCORN(4)=KDEC(21)
            GO TO 230
         END IF 
      END IF
cpsjun11      IF(KDEC(23).EQ.4.AND.OCONT(4).EQ..FALSE.) THEN
      IF(KDEC(23).EQ.4.AND..NOT.OCONT(4)) THEN
         NCORN(4)=KDEC(21)
         OCONT(4)=.TRUE.
         GO TO 230
      END IF
C     -----------------------------------------------------------------
 231  CONTINUE
C
C     IN CASE OF BRAKEDOWN THAT ALLOWS TO RECOVER.
C
      MC=MA
      DO 232 ISS=1,MA
         VALUESC(ISS)=VALUESA(ISS)
 232  CONTINUE
      GO TO 400
C     -----------------------------------------------------------------
 230  CONTINUE
C
      MB=M+MA-IREPP
C
      IS=NINT(VALUES(IREPP,NSUB))*7+IREPP
      ISA=NINT(VALUESA(IREPP))*7+IREPP
C
C     COPY VALUESA INTO VALUESB
cps   i.e. all values in old record (from MERGE file) are copied to valuesb
C
C     INSTRUMENTATION IS REPORTED ONLY IN PART B.
      DO 203 I=1,5
         IF(VALUESA(I).NE.999999.) VALUES(I,NSUB)=VALUESA(I)
         IF(VALUES(I,NSUB).NE.999999.) VALUESA(I)=VALUES(I,NSUB)
 203  CONTINUE
C
C
C     insert clouds from part B in the same way as above
C
      inuv=20
      if(kdec(4).eq.35) inuv=19
      do 205 i=14,inuv
         IF(VALUESA(I).NE.999999.) VALUES(I,NSUB)=VALUESA(I)
         IF(VALUES(I,NSUB).NE.999999.) VALUESA(I)=VALUES(I,NSUB)
 205  continue
c-------------------------------------------------------
      J=0
      DO 201 I=1,ISA
         J=J+1
         VALUESB(J)=VALUESA(I)
 201  CONTINUE
C
C     COPY VALUES INTO VALUESB
cps   i.e. all pressure levels from new record are put into (end of) valuesb
C
      DO 202 I=IREPP1,IS
         J=J+1
         VALUESB(J)=VALUES(I,NSUB)
 202  CONTINUE
C
C     UPDATE NUMBER OF LEVELS
C
      VALUESB(IREPP)=VALUES(IREPP,NSUB)+VALUESA(IREPP)
C
      ISB=NINT(VALUESB(IREPP))*7+IREPP
C
C*           2.1 SORT PRESSURE LEVELS IN DESCENDING ORDER.
C                -----------------------------------------
 210  CONTINUE
C
      J=0
      DO 211 I=IREPP1,ISB,7
C
         J=J+1
         PRESS(J)=VALUESB(I)
         INX(J)=I
C
 211  CONTINUE
C
      JJ=0
      DO 212 II=1,J
C
         PMAX=PRESS(1)
         IMAX=1
C
         DO 213 I=1,J
            IF(PRESS(I).GT.PMAX)
     1           THEN
               PMAX=PRESS(I)
               IMAX=I
            END IF
 213     CONTINUE
C     
         PRESS(IMAX)=0.0
         JJ=JJ+1
         INXA(JJ)=INX(IMAX)
 212  CONTINUE
C
C     COPY FIRST IREPP ELEMENTS OF VALUESB INTO VALUESC
C
      DO 214 I=1,IREPP
         VALUESC(I)=VALUESB(I)
 214  CONTINUE
C
C
      IJ=IREPP1
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
         VALUESC(IJ+5)=VALUESB(J+5)
         VALUESC(IJ+6)=VALUESB(J+6)
C     
         IJ=IJ+7
C
 215  CONTINUE
C
 220  CONTINUE
C
      ISC=ISB
C
C     DISCARD LEVELS WITH SAME PRESSURE
C
      ICOUN=1
      I=IREPP1
      K=15
      IF(KDEC(4).EQ.35) K=14
      IC=IREPP1
C
 250  CONTINUE
C
      K=K+7
      IF(K.GT.ISC) GO TO 260
C
      IF(VALUESC(I).EQ.VALUESC(K)) THEN
         IA=NINT(VALUESC(I+1))
         IB=NINT(VALUESC(K+1))
cpsjun11         IAIB=IA.OR.IB
         IAIB=IOR(IA,IB)
C
         VALUESC(K +1)=FLOAT(IAIB)
         VALUESC(IC  )=VALUESC(K)
         VALUESC(IC+1)=FLOAT(IAIB)
         IF(VALUESC(K+2).NE.999999.) THEN 
            VALUESC(IC+2)=VALUESC(K+2)
         END IF
         IF(VALUESC(K+3).NE.999999.) THEN 
            VALUESC(IC+3)=VALUESC(K+3)
         END IF
         IF(VALUESC(K+4).NE.999999.) THEN 
            VALUESC(IC+4)=VALUESC(K+4)
         END IF
         IF(VALUESC(K+5).NE.999999.) THEN 
            VALUESC(IC+5)=VALUESC(K+5)
         END IF
         IF(VALUESC(K+6).NE.999999.) THEN 
            VALUESC(IC+6)=VALUESC(K+6)
         END IF
C
         I=K
         GO TO 250
C
      ELSE
C
         ICOUN=ICOUN+1
         IC=IC+7
C
         VALUESC(IC  )=VALUESC(K  )
         VALUESC(IC+1)=VALUESC(K+1)
         VALUESC(IC+2)=VALUESC(K+2)
         VALUESC(IC+3)=VALUESC(K+3)
         VALUESC(IC+4)=VALUESC(K+4)
         VALUESC(IC+5)=VALUESC(K+5)
         VALUESC(IC+6)=VALUESC(K+6)
C
         I=K
         GO TO 250
      END IF
C
 260  CONTINUE
C
C     UPDATE NUMBER OF LEVELS
C
      VALUESC(IREPP)=FLOAT(ICOUN)
      ISC=NINT(VALUESC(IREPP))*7+IREPP
      MC=ISC
C
C
 280  CONTINUE
C
C     -----------------------------------------------------------------
C*         3. ADD WIND SHEAR LEVELS IF ANY.
C             -----------------------------
 300  CONTINUE
C
      IF(IS.LT.M.OR.ISA.LT.MA) THEN
         J=ISC+1
         IF(IS.LT.M) THEN
            IWLEVS=NINT(VALUES(IS+1,NSUB))
            DO 262 I=IS+2,M
               J=J+1
               VALUESC(J)=VALUES(I,NSUB)
 262        CONTINUE
         END IF
C
         IF(ISA.LT.MA) THEN
            IWLEVA=NINT(VALUESA(ISA+1))
            DO 263 I=ISA+2,MA
               J=J+1
               VALUESC(J)=VALUESA(I)
 263        CONTINUE
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
           DO 311 I=ISC+2,MC,4
C
              J=J+1
              PRESS(J)=VALUESC(I)
              INX(J)=I
C
 311       CONTINUE
C
           JJ=0
           DO 312 II=1,J
C
              PMAX=PRESS(1)
              IMAX=1
C     
           DO 313 I=1,J
              IF(PRESS(I).GT.PMAX)
     1             THEN
                 PMAX=PRESS(I)
                 IMAX=I
              END IF
 313       CONTINUE
C
           PRESS(IMAX)=0.0
           JJ=JJ+1
           INXA(JJ)=INX(IMAX)
 312       CONTINUE
C
           IJ=1
C
           DO 315 I=1,JJ
C
              J=INXA(I)
C
              VALUESB(IJ  )=VALUESC(J)
              VALUESB(IJ+1)=VALUESC(J+1)
              VALUESB(IJ+2)=VALUESC(J+2)
              VALUESB(IJ+3)=VALUESC(J+3)
C     
              IJ=IJ+4
C
 315       CONTINUE
C
 320       CONTINUE
C
C     DISCARD LEVELS WITH SAME PRESSURE
C
           ICOUN=1
           I= 1
           K= -3
           IC=1
C
 350       CONTINUE
C
           K=K+4
           IF(K.GE.IJ) GO TO 360
C
           IF(VALUESB(I).EQ.VALUESB(K)) THEN
              IA=NINT(VALUESB(I+1))
              IB=NINT(VALUESB(K+1))
cpsjun11              IAIB=IA.OR.IB
              IAIB=IOR(IA,IB)
C
              VALUESB(K +1)=FLOAT(IAIB)
              VALUESB(IC  )=VALUESB(K)
              VALUESB(IC+1)=FLOAT(IAIB)
              IF(VALUESB(K+2).NE.999999.) THEN 
                 VALUESB(IC+2)=VALUESB(K+2)
              END IF 
              IF(VALUESB(K+3).NE.999999.) THEN 
                 VALUESB(IC+3)=VALUESB(K+3)
              END IF
C 
              I=K
              GO TO 350
C
           ELSE
C
           ICOUN=ICOUN+1
           IC=IC+4
C
           VALUESB(IC  )=VALUESB(K  )
           VALUESB(IC+1)=VALUESB(K+1)
           VALUESB(IC+2)=VALUESB(K+2)
           VALUESB(IC+3)=VALUESB(K+3)
C
           I=K
           GO TO 350
C
        END IF
C
 360       CONTINUE
C
C     UPDATE NUMBER OF LEVELS
C
           VALUESC(ISC+1)=FLOAT(ICOUN)
           MC=NINT(VALUESC(ISC+1))*4+ISC+1
C
C
 380       CONTINUE
C
           JJ=ISC+1
C
           DO 362 II=1,I-1
              JJ=JJ+1
              VALUESC(JJ)=VALUESB(II)
 362       CONTINUE
C
      END IF
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
      DO 402 I=1,4
         IF(OCONT(I)) NPART(I)=1
 402  CONTINUE
C
      OLD_TEMP%STATION_DATE=YSD
      DO 711 I=1,4
         OLD_TEMP%OOCONT(I)=OCONT(I)
         OLD_TEMP%NNCORN(I)=NCORN(I)
 711  CONTINUE
      OLD_TEMP%MMA=MC
      DO 712 I=1,idimvalues
         OLD_TEMP%VVALUESA(I)=VALUESC(I)
 712  CONTINUE
      DO 713 I=1,10
         OLD_TEMP%CCVALA(I)=CVALA(I)
 713  CONTINUE
C
      IF(new_record) then
         irec=iget_next_recnumber(yfilename,iunit,keylen)
         endian=1
      else
         endian=Eendian
      end if
c
      if(swap) then
         call bswap4(4,old_temp%nncorn)
         call bswap4(1,old_temp%mma)
         call bswap4(maxva,old_temp%vvaluesa)
      end if
      WRITE(UNIT=7,REC=irec,IOSTAT=IOS,ERR=410) endian,OLD_TEMP
c
      IF(new_record) THEN
         call update_index_file(yfilename,iunit,irec,
     *        OLD_TEMP%STATION_DATE)
      END IF
C
C 
C            SET INFORMATION NEEDED FOR RDB KEY DEFINITION.
C            ----------------------------------------------
C
      NBUFTYPE=2      !   VERTICAL SOUNDINGS OTHER THEN SATELLITE 
      NTYPE=5                           ! TEMPERATURE PROFILE
      IF(KDEC(4).EQ. 35) NSBTYPE=101    ! TEMP LAND
      IF(KDEC(4).EQ. 36) NSBTYPE=102    ! TEMP SHIP
      IF(KDEC(4).EQ.135) NSBTYPE=103    ! TEMP DROP
      IF(KDEC(4).EQ.137) NSBTYPE=106    ! TEMP MOBILE
C
      IPO=5
      IF(KDEC(4).NE.35) IPO=6
      NYEAR =NINT(VALUES(IPO,1))
      NMONTH=NINT(VALUES(IPO+1,1))
      NDAY  =NINT(VALUES(IPO+2,1))
      NHOUR =NINT(VALUES(IPO+3,1))
      NMINUTE=NINT(VALUES(IPO+4,1))
      NSECOND=0
C
      JSCALE=5
      JREFLAT= -9000000
      JREFLON=-18000000
C
      IF(KDEC(4).EQ.35) THEN
         NLAT1=NINT(VALUES(10,NSUB)*10.**JSCALE)-JREFLAT
         NLON1=NINT(VALUES(11,NSUB)*10.**JSCALE)-JREFLON
      ELSE
         NLAT1=NINT(VALUES(11,NSUB)*10.**JSCALE)-JREFLAT
         NLON1=NINT(VALUES(12,NSUB)*10.**JSCALE)-JREFLON
      END IF 
C
      NLAT2=0  
      NLON2=0
C
      NOBS=NSUB
C
      CIDENT=CHAR(KINT(12))//CHAR(KINT(13))//CHAR(KINT(14))//
     1       CHAR(KINT(15))//CHAR(KINT(16))//'    '
c
c put cval(1) back to original call sign
c n.b. milan stores in bufr code using lat/long call sign
      if(kdec(4).eq.135) then
cc         print*,'mrgtem - cident,cval(1):',cident,cval(1)
         cval(1)(1:9)=ctmp(1:9)
ccc         cident=' '
ccc         cident=cval(1)
      end if
C 
      NRECR=1
C 
      RETURN
C
 410  CONTINUE
C
      WRITE(*,9903) IOS
      CALL exit(2)
C
 9901 FORMAT(1H ,' ERROR DURING OPENING UNIT 7, IOS=',I5)
 9902 FORMAT(1H ,' ERROR DURING READING UNIT 7, IOS=',I5)
 9903 FORMAT(1H ,' ERROR DURING WRITING UNIT 7, IOS=',I5)
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
c
CTEST               OPEN(UNIT=98,
CTEST     1         FILE=YTEST(1:ndx)//'temp-out.dat',
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
      INCLUDE 'compoin.f'
      INCLUDE 'comstat.f'
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
      data cstr/'                   temp decoding'/
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
cccccccccccccccccccc temp_lib.f ccccccccccccccccccccccccccccccccccccccccccccc
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
            KERBIT = IAND(ISHFT(KDEC(20),1-KERR),1)
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
