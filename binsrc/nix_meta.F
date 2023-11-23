      PROGRAM METPROC 
C
C**** *METPROC*
C
C
C     PURPOSE.
C     --------
C         controlling routine for decoding metar data
C
C
C**   INTERFACE.
C     ----------
C
c              msysdsi        YMSYS(1:ndx)//CDATFILE(msys)//'dataXXxx.dat'
c              4              'ppdat/meta.station.dat'
c              11             cstream(msys)//'/'meta_error_file.dat'
c              17             cfstamp(msys)//'.timestamp.dat' (opened in msysinit.f)
c              77             dir.dat
c              93             cstream(msys)//'stop.dat' (an empty file will be created if not present)
c              98             cstream(msys)//'/'//'trend'//cdate//'.dat'
c              99             cstream(msys)//'/'//'taf'//cdate//'.dat'
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
C        *CALL* *INITVAR( IERR )*
C        *CALL* *PROCRFB( IERR )*
C        *CALL* *PROCHDR( IERR )*
C        *CALL* *PROCTXT( IERR )*
C        *CALL* *PROCT1S( IERR )*
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
C          LISBET LOVHOIDEN    *DNMI*       MAY 1995.
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
CNH common for directories
      INCLUDE 'comdir.f'
Crr common for numbull
      INCLUDE 'comstat.f'
Crr for cstream
      INCLUDE 'commdbc.f'
      INCLUDE 'commdb.f'
c
      CHARACTER DIR_FILE*80
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   CSTREAM(40)
      CHARACTER*15 CSTOP
c
      CHARACTER*2   CMSYS
      CHARACTER*3   CMSYSTOP
c..functions:
      INTEGER       IARGC
C
C     ------------------------------------------------------------------
C*          1.   Initialize variables and constants.
C                ----------------------------------
 100  CONTINUE
C
crr
C   Read arguments: cmsys cmsystop
C
      NARG=IARGC()
      MSYSTOP=0
C
      IF( (NARG.LT.3).OR.(CTYPE.EQ.'?') )THEN
             PRINT*,''
             PRINT*,' Usage: metar dir_file cmsys timestamp <cmsystop>'
             PRINT*,'                                      (optional)'
             PRINT*,' where  dir_file  - specification of directories'
             PRINT*,'        cmsys     - msys file number '
cpsokt98             PRINT*,'        ctstamp   - mmddhhmm of msys file '
             PRINT*,'        ctstamp   - yyyymmddhhmm of msys file '
             PRINT*,'        cmsystop   - no. of records to read '
             PRINT*,''
             GOTO 1020
      ELSE
         CALL GETARG(1,DIR_FILE)
         CALL GETARG(2,CMSYS)
         READ(CMSYS(1:2),'(I2)') MSYS
         CALL GETARG(3,CTSTAMP)
      ENDIF
      IF(NARG.EQ.4) THEN
         CALL GETARG(4,CMSYSTOP)
         READ(CMSYSTOP(1:3),'(I3)') MSYSTOP
      ENDIF
C
      OPEN(UNIT=77,IOSTAT=IOS,ERR=2001,
     1            FILE=DIR_FILE,
     2     FORM='FORMATTED',
     3     STATUS='UNKNOWN' )
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) YMSYS
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) YPPDAT
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) YDATDEF
       READ(77,'(A)',ERR=2002,IOSTAT=IOS,END=1020) YTEST
      print*,'YMSYS',YMSYS
C
      CALL INITVAR( IERR )
      IF(IERR.NE.0) CALL exit(2)
c
C           1.1  Initialize MDB access.
C                ----------------------
 110  CONTINUE
C
      CALL MSYSINIT(IERR )
      IF(IERR.NE.0) THEN
                       PRINT*,'MSYSINIT: ERROR=',IERR
                       CALL EXIT(2)
                    END IF 
c
C
      IUNIT=93
C
      PRINT*,' IUNIT ',IUNIT
      OPEN(UNIT=IUNIT,IOSTAT=IOS,ERR=1001,
     1            FILE=CSTREAM(MSYS)//'.stop.dat',
     2     FORM='FORMATTED',
     3     STATUS='UNKNOWN' )
C
C
C     ------------------------------------------------------------------
C*          2.   Read in message.
C                ----------------
 200  CONTINUE
C
      READ(IUNIT,'(a)',IOSTAT=IOS,ERR=1002) CSTOP
      REWIND(IUNIT)
C
      IF(CSTOP.EQ.'stop_processing') GO TO 1000
C
      CALL GETNEXT(IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' METPROC - after getnext, ierr ',IERR
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
         CALL EXIT(2)
      END IF 
C     ------------------------------------------------------------------
C*          3.   Format bulletin.
C                ----------------
 300  CONTINUE
C
      CALL PROCRFB( IERR )
CTEST
CTEST PRINT*,' METROC - kint(1-4)',(KINT(IR),IR=1,4)
      IF(IERR.NE.0)THEN
         PRINT*,' METPROC - after PROCRFB, IERR= ',IERR
      END IF
CTEST
      IF(IERR.NE.0) GO TO 200
C     ------------------------------------------------------------------
C*          4.   Decode bulletin header.
C                -----------------------
 400  CONTINUE
C
      CALL PROCHDR( IERR )
CTEST
       IF(IERR.NE.0)THEN
         PRINT*,' METPROC - after PROCHDR, KERR= ',KERR
       END IF 
CTEST
      IF(KERR.NE.0) GO TO 200
C     ------------------------------------------------------------------
C*          5.   Check next of bulletin.
C                -----------------------
 500  CONTINUE
C
      CALL PROCTXT( IERR )
CTEST
       IF(IERR.NE.0)THEN
          PRINT*,' METPROC - after PROCTXT, KERR= ',KERR
       END IF 
CTEST
      IF(KERR.NE.0) GO TO 200
C
C     ------------------------------------------------------------------
C*          6.   Call appropriate routine to decode metar data.
C                ---------------------------------------------
 600  CONTINUE
C
      CALL PROCT1S( IERR )

CTEST
      IF(IERR.NE.0)THEN
          PRINT*,' METPROC - after PROCT1S, IERR= ',IERR
      END IF 
CTEST
C
C
      GO TO 200
C
C     ------------------------------------------------------------------
C
C
 1001 CONTINUE
C
      PRINT*,' Open error on meta.stop.dat'
      GO TO 1000
C
 1002 CONTINUE
C
      PRINT*,' Read error on meta.stop.dat'
      GO TO 1000
C
 2001 CONTINUE
C
      PRINT*,' Open error on dir-file'
      GO TO 1000
C
 2002 CONTINUE
C
      PRINT*,' Read error on dir-file'
C
 1000 CONTINUE
c
      NTESTTOT=0
      DO 1010 I=1,200
         NTESTTOT=NTESTTOT+NTEST(I)
 1010 CONTINUE
C
      PRINT*,'number of reports written ',ntesttot
cps      PRINT*,'number of duplicates ',ndup
      PRINT*,' ****n.b.***** PUTBUFR : No testing of duplicates '
C
 1020 CONTINUE
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
CTEST       print*,'procrfb-start'
CTEST WRITE(*,999)ISL,JSL,IAH,JAH,IMI,JMI,IEQ,IGS,IPT,KPT,LPT
999   FORMAT(' PROCRFB- ISL=',I4,' JSL=',I4,' IAH=',I4,' JAH=',I4,
     *       ' IMI=',I4,' JMI=',I4,/,' IEQ=',I4,' IGS=',I4,' IPT=',I4,
     *       ' KPT=',I4,' LPT=',L2)
CTEST
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
C     Check on first letter T1 in Heading.
C     Metar and Speci start with T1=S
C     Taf's start with T1=F.
C     Bulletins with other values in T1 are skipped.
C
      IT1=KCHAR(I  )-64
      IF( (IT1.NE.19).AND.(IT1.NE.6) )THEN
                       IERR=1
                       IT1=27
                       GO TO 290
                    END IF
C
C     CHECK IF 'T2' CHARACTER IS LETTER.
C
      IT2=KCHAR(I+1)-64
      IF(IT2.LT.1.OR.IT2.GT.26) IT1=27
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
                                              GO TO 290
                                           END IF
C
 211  CONTINUE
C
      I=I-1
      KCHAR(I)= 29
      IGS=I
C
CTEST 
 290  CONTINUE
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
C        ???????  4. UK AND GERMAN DOMESTIC BULLETINS WHICH DO
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
CX    print*,'PROCTXT,IMI,IGS',IMI,IGS
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
C        GO TO 900
      END IF
C
C
C           1.3 TREAT IN ACCORDANCE WITH DEFINED OPTIONS.
C               -----------------------------------------
 130  CONTINUE
C
C     RETURN IF NO ERROR.
C
C     check for illegal characters in bulletin.
C     The following are legal.
      DO 200 I=IMI,IGS
C        integer?
         IF(OKINT(KCHAR(I),IERR))GO TO 200
C        letter?
         IF(OKLTR(KCHAR(I),IERR))GO TO 200
C        Separator?
         IF(OKSEP(KCHAR(I),IERR))GO TO 200
C        Slash
         IF(KCHAR(I).EQ.47)GO TO 200
C        + Followed by letter?
         IF((KCHAR(I).EQ.43).AND.(OKLTR(KCHAR(I+1),IERR)) )GO TO 200
C        -   Followed by letter?
         IF((KCHAR(I).EQ.45).AND.(OKLTR(KCHAR(I+1),IERR)) )GO TO 200
CX         print*,CHAR(KCHAR(I))
         KCHAR(I)=32
C
 200  CONTINUE
C
      IF ( KERR.EQ.0 ) RETURN
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
CTEST
CTEST      PRINT*,' PROCT1S - IT2 ',IT2
CTEST
C
C     When entering this routine we have TT=Sx or TT=Fx
C     We want to decode Metars (TT=SA),
C                   and Speci  (TT=SP).
C
C     When T2 is 'A' or 'P' we call BULLSA for decoding of 
C     Metar/Speci. This returns if TT is anything but SA or SP.
C
c
C     In addition to decoding metar/speci, we want to save Tafs to file.
C     Taf's have TT=FC or TT=FT.
C
C     When T2 is 'C' or 'T' we call BULLFC for saving Taf's.
C     This returns if TT is anything but FC or FT.
C
C
      IF( (IT2.eq.1).OR.(IT2.eq.16) )THEN
C
C*       BULLETINS WITH 'TT' = 'SA' or 'FA'
C*       BULLETINS WITH 'TT' = 'SP' or 'FP'
C      
         CALL BULLSA( IERR )
         IF(IERR.NE.0)PRINT*,'After bullsa, IERR=',IERR
C 
      ELSEIF( (IT2.eq.2))THEN
C           
C*       BULLETINS WITH 'TT' = 'FB' or 'SB'
C          
C
C        Nothing is currently done with these.
C
C        PRINT*,'FB or FC'
C 
      ELSEIF( (IT2.eq.3).OR.(IT2.eq.20) )THEN
C 
C*       BULLETINS WITH 'TT' = 'FC' or 'SC'
C*       BULLETINS WITH 'TT' = 'ST' or 'FT'
C  
C        TAF's should be dumped to file.
C
         call bullfc(ierr)
         IF(IERR.NE.0)PRINT*,'After bullfc, IERR=',IERR
C   
      ENDIF
C   
      RETURN
      END
      SUBROUTINE BULLFC ( IERR )
C
C**** *BULLFC*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF METAR.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLFC(IERR)*
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
C          OUTPUT     : TAF DUMPED  TO FILE, NO DECODING.
C
C                       IERR = 1 , IF ANY FILE HANDLING ERROR .
C
C
C
C     METHOD.
C     -------
C
C         THIS ROUTINE HAS 2 ENTRY POINTS TAF's with TT=FC or FT
C         Bulletins with TT='ST' or 'SF' are returned immediatly.
C
C         This programs run only on directory msys8 which containes
C         'SA', 'SP', 'FB', 'FC' and 'FT'
C
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
C
C
C     MODIFICATIONS.
C     --------------
C
C          LISBET LOVHOIDEN    *DNMI*       FEBRUARY 1997.
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
Crr for cstream
      INCLUDE 'commdbc.f'
      INCLUDE 'commdb.f'
C     ------------------------------------------------------------------
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
cpsapr98      CHARACTER*6   cdate
      CHARACTER*8   cdate
      CHARACTER*1 ILINE(80)
C     ------------------------------------------------------------------
C
C*          1.   SET FLAGS AND WORKING POINTERS.
C                -------------------------------
C*    Output to terminal
 100  CONTINUE
C
C
C
C     WORKING POINTER SET TO POINT TO FIRST LETTER OF ABR HEADER.
C
      IPT = IAH
C
C
C     Check that TT is 'FC' or 'FT',
C     and skip FCNNnn
C
CX    CALL PRTST(IPT,IPT+6,'FCNNnn')
      IF(  (KCHAR(IPT).EQ.70).AND.
     c  (  (KCHAR(IPT+1).EQ.67).OR.(KCHAR(IPT+1).EQ.84) ) )THEN
C
CX       print*,'FCNNnn Found'
         IPT=IPT+2
C
C        Only save Tafs from nordic countries 
C        (Norway, Sweden, Denmark,Finland,Island)
C        Check that NN is one of NO,DN,EN,IL,FI or SN.
         IF(  (KCHAR(IPT).EQ.78).AND.(KCHAR(IPT+1).EQ.79) )THEN
C
C           NO:
CX          print*,'FCNOnn Found'
            IPT=IPT+4
         ELSEIF( (KCHAR(IPT).EQ.68).AND.(KCHAR(IPT+1).EQ.78) )THEN
C
C           DN:
CX          print*,'FCDNnn Found'
            IPT=IPT+4
C
         ELSEIF( (KCHAR(IPT).EQ.69).AND.(KCHAR(IPT+1).EQ.78) )THEN
C
C           EN:
CX          print*,'FCENnn Found'
            IPT=IPT+4
C
         ELSEIF( (KCHAR(IPT).EQ.73).AND.(KCHAR(IPT+1).EQ.76) )THEN
C
C           IL:

CX          print*,'FCILnn Found'
            IPT=IPT+4
C
         ELSEIF( (KCHAR(IPT).EQ.70).AND.(KCHAR(IPT+1).EQ.73) )THEN
C
C           FI:
CX          print*,'FCILnn Found'
            IPT=IPT+4
C
         ELSEIF( (KCHAR(IPT).EQ.83).AND.(KCHAR(IPT+1).EQ.78) )THEN
C
C           SN:
CX          print*,'FCSNnn Found'
            IPT=IPT+4
C
         ELSE
CX          print*,'Not wanted nationality on Taf'
            IERR=9
            RETURN
         END IF
      ELSE
CX       print*,'Not FCNNnn '
         IERR=9
         RETURN
      END IF
C     
C
C     Skip sending center:
      CALL NEXTPRT ( IPT,IGS )
CX    CALL PRTST(IPT,IPT+6,'CCCC       ')
      IF( OKLTR(KCHAR(IPT),IERR) .AND. OKLTR(KCHAR(IPT+1),IERR) .AND.
     C   OKLTR(KCHAR(IPT+2),IERR) .AND. OKLTR(KCHAR(IPT+3),IERR) )THEN
C
C        Found sending center:
CX       print*,'CCCC Found'
         IPT=IPT+4
      ELSE
C
C        Not valid heading
         print*,'Not valid heading, cannot open taf-file!'
         IERR=8
         RETURN
C
      ENDIF
C
C
C     Find date of month for use in file-name:
      CALL NEXTPRT ( IPT,IGS )
CX    CALL PRTST(IPT,IPT+6,'DDHHMM     ')
      IF( OKINT(KCHAR(IPT),IERR).AND.OKINT(KCHAR(IPT+1),IERR) )THEN
C
C        Found date:
CX       print*,'Date Found'
         idd=(kchar(ipt)-48)*10 + kchar(ipt+1)-48
CX       print*,'Date=',idd
C
      ELSE
C
C        Not valid date:
         print*,'Not valid date for taf, cannot open taf-file!'
         IERR=7
         RETURN
C
      ENDIF
C
C     SET FLAG FOR HEADER,
      IHEAD = 0
C
C***
C*    Open taf-file.
C***
C
C     Make filename with date in it:
      call datum(idd,imm,iyy)
CX    print*,'Date:',idd,imm,iyy
cpsapr98      write(cdate(1:2),'(i2.2)')iyy
cpsapr98      write(cdate(3:4),'(i2.2)')imm
cpsapr98      write(cdate(5:6),'(i2.2)')idd
      write(cdate(1:4),'(i4)')iyy
      write(cdate(5:6),'(i2.2)')imm
      write(cdate(7:8),'(i2.2)')idd
c
cx    print*,'BULLFC - ',
cx   c            cstream(msys),'/taf'//cdate//'.dat'
Cx   c            cstream(msys),'/taf_file.dat'
c
      OPEN(UNIT=99,IOSTAT=IOS,ERR=400,
     1     FILE=cstream(msys)//'/'//'taf'//cdate//'.dat',
C     1     FILE=cstream(msys)//'/'//'taf_file.dat',
C     1     FILE='taf_file.dat',
#ifdef FORTRAN90
     2     POSITION='APPEND',
#else
     2     ACCESS='APPEND',
#endif
     3     STATUS='UNKNOWN'    )
C
C
 101  CONTINUE
C
C
C
C
C
C***
C*    Print bulletin.
C***
C
C
C     ------------------------------------------------------------------
C
      IP = IAH
      J = IGS
C
C*          1.1  SET OUTPUT LINE TO ALL SPACES .
C                -------------------------------
 210  CONTINUE
C
      K = 80
      DO 211 N=1,K
CRR             LINE(N) = 32
             ILINE(N) = CHAR(32)
  211 CONTINUE
C
C     LOCATE START AND END OF NEXT LINE OF CHARACTERS ( IF ANY ).
C
      CALL NEXTPRT ( IP,J )
      IF ( IP.GE.J ) GO TO 300
      JP = IP
      CALL NEXTEND ( JP,J )
      K = JP - IP
      IF(K.GT.80) K=80
C
C     INSERT IN OUTPUT LINE AND SUPPRESS PARITY BIT.
C
      DO 212 N =1,K
CRR             LINE(N) = IAND(KCHAR(IP),127)
             ILINE(N) = CHAR(KCHAR(IP))
             IP = IP + 1
  212 CONTINUE
C
      WRITE (99,9900,ERR=401) (ILINE(N),N=1,K)
C      WRITE (*,9900) (ILINE(N),N=1,K)
 9900 FORMAT (1H ,80A1)
C
C
C     print*,'Done 1 line'
C     GET NEXT LINE
C
      GO TO 210 
C
C
  300 CONTINUE
C     print*,'Done 1 bulletin'
C     put some space between bulletins:
      WRITE (99,9901,ERR=401)
      WRITE (99,9901,ERR=401)
 9901 FORMAT (1H ,'  ')
C
      close(99)
      RETURN
C
C
 400  CONTINUE
C
      PRINT*,' Open error on taf_file.dat'
      RETURN
C
 401  CONTINUE
C
      PRINT*,' WRITE error on taf_file.dat'
      RETURN
C

      END
      SUBROUTINE BULLSA ( IERR )
C
C**** *BULLSA*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF METAR.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLSA(IERR)*
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
C
C
C     METHOD.
C     -------
C
C         THIS ROUTINE HAS 1 ENTRY POINT, METAR or SPECI.
C
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *SADEC(IHEAD,IERR)*
C         *CALL* *SAINT(IHEAD,IERR)*
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
C          LISBET LOVHOIDEN    *DNMI*       MAY 1995.
C
C
C
CLiL                   ENTRY BULLSI ( IERR )
CLiL                   ENTRY BULLSN ( IERR )
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
C     ------------------------------------------------------------------
C
C*          1.   SET FLAGS AND WORKING POINTERS.
C                -------------------------------
C*    Output to terminal
 100  CONTINUE
C
C
C-----PRINT INPUT BULLETINS OF METAR
C      CALL PRTBULL (1,IGS)
C
C     WORKING POINTER SET TO POINT TO FIRST LETTER OF ABR HEADER.
C
      IPT = IAH
C
C
C     CLEAR FLAG WHICH INDICATES BULLETIN HEADER ALREADY WRITTEN
C     TO ERROR FILE.
C
      IHEAD = 0
C
C
C     Move pointer IPT to YYGG
C
C     First Find out if 'SA' or 'SP'
CX    print*,'SANNnn'
CX    CALL PRTST(IPT,IPT+6,'SANNnn')
      IF( (KCHAR(IPT).EQ.83).and.(KCHAR(IPT+1).EQ.65) )THEN
C
C        METAR, set observation type, skip to next group.
         KDEC(4)=85
         IPT=IPT+6
C
      ELSEIF( (KCHAR(IPT).EQ.83).and.(KCHAR(IPT+1).EQ.80) )THEN
C
C        SPECI, set observation type, skip to next group.
         KDEC(4)=86
         IPT=IPT+6
C
      ELSE
C
C        Not METAR, nor SPECI - RETURN.
         RETURN
      ENDIF
C
C     Skip space (if any).
      IF(KCHAR(IPT).EQ.32)IPT=IPT+1
C
C     CCCC (ID of sending center).
CX    CALL PRTST(IPT,IEQ,'Sending center')
C
C     Skip decoding of US-METARS:
      IF( (KCHAR(IPT).EQ.75).and.(KCHAR(IPT+1).EQ.87).and.
     C    (KCHAR(IPT+2).EQ.66).and.(KCHAR(IPT+3).EQ.67) )THEN
C
C        US-Metar. No decoding.
         print*,'KWBC-Metar. No decoding!'
         IERR=9
         RETURN
C
      ELSE 
C
C        Skip EXXX (ID of sending center).
         IPT=IPT+4
C
      ENDIF
C
C     Skip space if any.
      IF(KCHAR(IPT).EQ.32)IPT=IPT+1
C    
C
CX    CALL PRTST(IPT,IEQ,'YYGGmm:')
      CALL EXTGRP ( IPT,2,2,2,0,0,1,IRET)
      ISYYGG = 0
C
C     CHECK VALUES OF YY,GG AND MM . IF ANY ERROR IS
C     FOUND DECODED VALUE IS CHANGED TO MISSING DATA VALUE
C     AND GROUP FLAGGED AS BEING IN ERROR . ERRORS IN THIS
C     GROUP ARE NOT FATAL AS YYGMMG FROM HEADING 
C     ARE SUBSTITUTED LATER , IF
C     NECESSARY.
C
      I = 0
      IF (KINT(1).LT.1.OR.KINT(1).GT.31)THEN
          I = 1
          KINT(1) = MINDIC
      END IF
C
      IF ( KINT(2).LT.0.OR.KINT(2).GT.23)THEN
           I = 1
           KINT(2) = MINDIC
      END IF
C
      IF ( KINT(3).LT.0.OR.KINT(3).GT.59)THEN
           I = 1
           KINT(3) = MINDIC
      END IF
C
      IF ( I.NE.0 ) THEN
           KCHAR(IPT-1) = IOR(KCHAR(IPT-1),128)
           NOER(1,3)=NOER(1,3)+1
      END IF
C
C
C*    HANDLE METAR/SPECI LINE . SET DATA BASE REPORT TYPE INDICATORS
C     IN DECODED REPORT HEADER  AND LOCATE START OF FIRST REPORT .
C
C     WORKING POINTER SET TO POINT TO FIRST LETTER OF METAR/SPECI GROUP.
      IPT = IMI
C
C
C*MARK
C     SET DATA BASE CODE TYPE INDICATOR AND METAR/SPECI FLAG.
C
CX    CALL PRTST(IPT,IEQ,'METAR:')
C
      IF( (KCHAR(IPT).EQ.77).AND.(KCHAR(IPT+1).EQ.69).AND.
     C    (KCHAR(IPT+2).EQ.84).AND.(KCHAR(IPT+3).EQ.65).AND.     
     C    (KCHAR(IPT+4).EQ.82)) THEN
C
C          METAR
C
           KINT(4) = 1
C
          
      ELSEIF( (KCHAR(IPT).EQ.83).AND.(KCHAR(IPT+1).EQ.80).AND.
     C    (KCHAR(IPT+2).EQ.69).AND.(KCHAR(IPT+3).EQ.67).AND.     
     C    (KCHAR(IPT+4).EQ.73)) THEN
C
C          SPECI
C
           KINT(4) = 2
C
      ELSE   
C
C          METAR/SPECI line missing
           KINT(4) = 0
C
      END IF
C
C
C
cpsjan04  Had to rewrite following code, as CCCC etc may follow METAR/SPECI on same line
c
cpsC      METAR/SPECI-line to be skipped.
cpsC      Set IPT to end of line + 3
cpsC
cps       IF(KINT(4).NE.0) IPT=JMI+3
C     METAR/SPECI to be skipped.
      IF(KINT(4).NE.0) then
         IPT=IPT + 5
         CALL NEXTPRT(IPT,IGS)
cpsoct04 COR may follow METAR/SPECI from nov 3 2004.
         IF( (KCHAR(IPT).EQ.67).AND.(KCHAR(IPT+1).EQ.79).AND.
     +        (KCHAR(IPT+2).EQ.82)) THEN
cpsnov06 Consider 'COR' an CCA if not marked as CCx already
            if (kdec(21).eq.0) kdec(21) = 1
            IPT = IPT + 3
            CALL NEXTPRT(IPT,IGS)
         END IF

      END IF
cps End new code
C
C
C
 101  CONTINUE
C
C
C
C
C***
C*    Locate end of report ( = ) and increment counter.
C***
C
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
cpsnov06
      if (ieq.ge.igs) return
C
C
      NUMREP(1) = NUMREP(1) + 1
C
C
C
C***
C*    Decode report .
C***
C
C     Convert report to intermediate format.
C
C      
CX      print*,'SAINT-IPT=',IPT
      CALL SAINT( IHEAD,IERR )
CTEST
      IF ( KERR.EQ.1 )THEN
c        NIL - no printout
C         print*,' WARNING  - NIL ',(char(kint(i)),i=5,8)
C       
      ELSEIF ( KERR.EQ.2 )THEN
c        print*,' Too short report.'
c 
      ELSEIF ( KERR.EQ.99 )THEN
c         print*,(char(kint(i)),i=5,8),' not in stationlist'
c
      ELSEIF( (KERR.NE.0 ).or.(IERR.NE.0) )THEN
         print*,' BULLSA - after call saint, kerr,ierr',KERR,IERR
      ENDIF
CTEST
C
C
C     "NIL" reports (KERR=1) will not be decoded for
C     data monitoring purposes
C     Stations not in stationlist (KERR=20) will not be decoded.
C
      IF(KERR .NE. 0) GO TO 200
C
C
C     Convert intermediate to decoded format and write to file
C
CX    print*,'SADEC-IPT=',IPT
      CALL SADEC ( IHEAD,IERR )
CTEST
      IF ( KERR.NE.0 )THEN
         print*,' BULLSA - after call sadec, kerr,ierr',KERR,IERR
      ELSEIF ( IERR.NE.0 )THEN
         print*,' BULLSA - after call sadec, kerr,ierr',KERR,IERR
      ENDIF
CTEST
C
      IF ( KERR.NE.0 ) GO TO 200
C
C
C
C
C***
C*    Locate start of next report.
C***
C
200   IPT = IEQ + 1
CX    print*,'Next report, IPT=',IPT,'IGS=',IGS
      CALL NEXTPRT ( IPT,IGS )

CX    print*,'Next report after NEXTPRT, IPT=',
CX   C         IPT,'IGS=',IGS
C
C***
C*    Check for end of bulletin.
C***
C
      IF ( IPT.GT.IGS ) THEN
C          end of bulletin, so if any report has been written to
C          error file, add'GS' character before returning.
C
           IF (IHEAD.EQ.0) RETURN
           IHEAD = 2
           CALL SAVREP ( IHEAD,IERR )
C
C          Print METAR data written to error file
C          CALL PRTBULL (1,IGS)
C
           RETURN
C
      END IF
C
C
CX      print*,'Ready for next report.'
      GO TO 101
C
      END
      SUBROUTINE SADEC ( IHEAD,IERR )
C
C**** *SADEC*
C
C
C     PURPOSE.
C     --------
C
C         DECODE METAR REPORTFROM INTERMEDIATE FORMAT
C         TO DECODED METAR REPORT
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SADEC(IHEAD,IERR)*
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
C*MARK************************************************
C     EXTERNALS.
C     ----------
C
CLiL      *CALL* *METASTA(     )*      FIND SYNOPNUMBER FOR STATION
C         *CALL* *IC3333 (     )*      FOR LATTITUDE AND LONGITUDE
C         *CALL* *IT5TODC(     )*      CONVERSION FROM CITT5 TO DISPLAY CODE
CLiL      *CALL* *IC2875 (     )*      WIND DIRECTION
CLiL      *CALL* *IC1875 (     )*      WIND SPEED
CLiLMar97*REMOVED*      *CALL* *IC18RR (     )*      WIND SHEAR
CLiL      *CALL* *IC4367 (     )*      HORIZONTAL VISIBILITY
CLiL      *CALL* *IC43DD (     )*      DIRECTION FOR HORIZONTAL VISIBILITY
CLiL      *CALL* *ICRWVV (     )*      RUNWAY VISIBILITY
CLiL      *CALL* *IC3876 (     )*      TEMPERATURE
CLiL      *CALL* *ICCLCVR(     )*      CLOUD AMOUNT
CLiL      *CALL* *ICPRESS(     )*      PRESSURE AT STATIONLEVEL- QNH
CLiLMar97*REMOVED*      *CALL* *ICSEAPR(     )*      PRESSURE AT SEA-LEVEL
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
C     ------------------------------------------------------------------
C           1. IN THIS SUBROUTINE THE ONLY PARTS OF THE HEADER
C              DECODED ARE IDENTIFIER , THE LENGTH OF REPORT ,
C              DAY OF THE MONTH AND TIME (HOUR & MIN) FROM REPORT.
C
 100  CONTINUE
CTEST
CTEST      PRINT*,' SADEC - at start '
CTEST
C
C
C     Clear error indicator.
      KERR = 0
      IERR = 0 
C
C*    Output to terminal
C
C*    Set decoded report to missing data value
      DO 1 I=25,100
         KDEC(I)=MINDIC
 1    CONTINUE
C

C*KDEC(4-17)
C
C     Observation type. Metar- 85? (86=Speci)
C     Already done when decoding header.
C     KDEC(4)=85
C
      KDEC(7)=88
      KDEC(9)=0
C
C
C
C*KDEC(1-3)
C
C*    Day and time
C
C     Check if day and hour are missing. In that case use 
C     date from abbreviated header.
C
      KDEC(1)=KINT(166)
      KDEC(2)=KINT(167)
      KDEC(3)=KINT(168)
C
C
C      Print*,'kdec1,kdec2,kdec3:',kdec(1),kdec(2),kdec(3)
C      Print*,'kint1,kint2,kint3:',kint(1),kint(2),kint(3)
C
      IF(KDEC(1).EQ.MINDIC) THEN
          KDEC(1)=KINT(1)
          IF(KDEC(1).EQ.MINDIC) RETURN
      ENDIF
C
      IF(KDEC(2).EQ.MINDIC) THEN
          KDEC(2)=KINT(2)
          IF(KDEC(2).EQ.MINDIC) RETURN
      ENDIF

      IF(KDEC(3).EQ.MINDIC) THEN
          KDEC(3)=KINT(3)
          IF(KDEC(3).EQ.MINDIC) RETURN
      ENDIF

C
      IF(KDEC(2).EQ.23)THEN
C
C            If hour in header is zero, day in Metar-line must
C            be day before. Not if we already are using day from heading.
C
             IF( (KINT(2).EQ.0).and.(KDEC(1).eq.KINT(1)) )
     C            KDEC(1)=KDEC(1)-1
      ENDIF
C          
C      Print*,'kdec1,kdec2,kdec3:',kdec(1),kdec(2),kdec(3)
C          
C*KDEC(18-23)
C
C*KDEC(24-29)
C
C*    report length
      KDEC(24)=120
CX    print*,'KDEC(2/3)',KDEC(2),KDEC(3)
C
C
C*    DDD & FF   mean wind direction and speed last 10 minutes
      CALL IC2875(KINT(11),MINDIC,KDEC(25))
      CALL IC1875(KINT(12),KINT(18),MINDIC,KDEC(26))
C
C
C*    Gust FMFM
      CALL IC1875(KINT(14),KINT(18),MINDIC,KDEC(27))
C    
C     DDD V DDD   variation in winddirection last  10 minutes
      IF(KINT(20).EQ.86)THEN
         CALL IC2875(KINT(19),MINDIC,KDEC(28))
         CALL IC2875(KINT(21),MINDIC,KDEC(29))
      ENDIF
C
C*KDEC(30-50)
C
C*    horizontal visibility.(min)
      CALL IC4367(KINT(23),KINT(22),MINDIC,KDEC(30))
      CALL IC43DD(KINT(24),KINT(25),MINDIC,KDEC(31))
C*    max
      CALL IC4367(KINT(26),KINT(22),MINDIC,KDEC(32))
      CALL IC43DD(KINT(27),KINT(28),MINDIC,KDEC(33))
C
C*    air temperature 
      CALL IC3876(KINT(103),KINT(102),MINDIC,KDEC(37))
C
C*    dew point temperature
      CALL IC3876(KINT(106),KINT(105),MINDIC,KDEC(38))
C
C*    pressure at stationlevel
      CALL ICPRESS(KINT(108),KINT(107),MINDIC,KDEC(39))
C
C*mar97 removed.
C*    pressure at sea-level
C      CALL ICSEAPR(KDEC(40),KDEC(8),MINDIC,KDEC(40))
C
CLiL 17/2-97: No calculation of seapressure. There is not enough information
C             present at this stage.
C
CLiL 11/3-97: No handling of present and recent weather in SADEC.
C             These observations are now stored as CCITTA5 in BUFR, and
C             are taken directly from KINT in METEXP1. We do clean up a
C             bit, though.
C
C     Fill ww/reww with space if not reported.(if mindic)
      DO 154 I=1,3
C
C        WW in KINT(110)-KINT(139)
         KI=99 + I*10
         DO 151 J=1,10
            IF(KINT(KI+J).EQ.MINDIC)KINT(KI+J)=32
 151     CONTINUE
C
C
C        REWW in KINT(140)-KINT(157)
         KI=133 + (I)*6
         DO 152 J=1,6
            IF(KINT(KI+J).EQ.MINDIC)KINT(KI+J)=32
 152     CONTINUE
C
 154  CONTINUE
C
C*    CAVOK:
      KDEC(98)=KINT(22)
C*   
C*    Cloudcover and height (4 groups)
C*   
      IREP=0
C*   
 155  CONTINUE
C*   
      IF(IREP.GE.4)GO TO 160
C
         KIPT=IREP*5 + 80
         KDPT=IREP*3 + 45
         IF((KINT(KIPT).EQ.86).AND.(KINT(KIPT+1).EQ.86))THEN
C
C*          Vertical visibility.
cpsnov06            KDEC(44)=KINT(KIPT+3)*30
cps      7 bits with scale -1 in bufr means that (kint-)values > 42 cannot
cps      be stored correctly. Actually, it is no uncommon mistake to use VV999
cps      for clear sky, but 'vertical visibility' is in metar code defined as
cps      'vertical visual range into an obscuring medium'
            if (KINT(KIPT+3).gt.42) then
               print*,'(VV)hshshs too big - set to missing ',
     *              kint(kipt+3)
               kdec(44) = mindic
            else
               KDEC(44)=KINT(KIPT+3)*30
            end if
            IREP=4   ! to exit the loop (could have used goto 160 instead)
C
         ELSE
C
C*          Cloud-group.
C*          Cloudamount.
            CALL ICCLCVR(KINT(KIPT),KINT(KIPT+1),
     C                KINT(KIPT+2),MINDIC,KDEC(KDPT))
C*          Cloudheigth.
            CALL ICCLHHH(KINT(KIPT+3),MINDIC,KDEC(KDPT+1))
C*          Cloudtype.
            CALL ICCLTYP(KINT(KIPT+4),MINDIC,KDEC(KDPT+2))
         ENDIF
C
         IREP=IREP+1
C
         GO TO 155
C
C
160   CONTINUE
C
C*    Runway visibility.
      CALL ICRWVV(30,57)
      CALL ICRWVV(40,64)
      CALL ICRWVV(50,71)
      CALL ICRWVV(60,78)
      CALL ICRWVV(70,85)
C
C*    Windshear
C*New:mar97 - Windshear is taken as CCITTAI5 directly from KINT in 
C*            subroutine METEXP1.
C
c      CALL IC18RR(158,92)
c      CALL IC18RR(162,95)
C
C
C
950   CONTINUE
C 
C
C
CTEST
CX       print*,'End of SADEC - KDEC:'
CX       DO 951 I=1,100
CX          IF(KDEC(I).NE.MINDIC)PRINT*,'KDEC-',I,KDEC(I)
CX951    CONTINUE     
CTEST
C                
C
CX       PRINT*,' Before METEXP1 , KINT(159) ',KINT(159)
      CALL METEXP1( IERR )
      IF(IERR.NE.0)
     1 PRINT*,' After METEXP1 , IERR ',IERR
C
      CALL QCMETAR(KDEC(4),KDEC(23),IERR )
      IF(IERR.NE.0)
     1 PRINT*,' After QCMETAR(1) , IERR ',IERR
C
      CALL BUFRENC( 1,IERR )
      IF(IERR.NE.0)
     1 PRINT*,' After BUFRENC(1) , IERR ',IERR
C
C---- PRINT OF INTERMEDIATE AND DECODED FORM OF REPORTS
      RETURN
C
C
      END
      SUBROUTINE SAINT (IHEAD,IERR)
C
C**** *SAINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT META REPORTS FROM CCITT. NO.5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SAINT(IHEAD,IERR)*
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
C         *CALL* *XXXXXXX(XXXX)*
CLiL      *CALL* *METASTA(     )*      FIND SYNOPNUMBER FOR STATION
CLiL      *CALL* *ICOKWW (     )*      PRESENT WEATHER (SYNTAX)
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          Lisbet Lovhoiden     *DNMI*    MAY 1995
C
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
      CHARACTER*4   cstream(40)
      CHARACTER*20  ERRTXT
      LOGICAL ok
cpsjun06
      logical ddVRB
      ddVRB = .false.
C
C     ------------------------------------------------------------------
C     LABELS:
C 100 START
C 110 Iinitiate KINT
C 120 Iinitiate ?
C 130 Iinitiate KINT
C 140 Iinitiate KDEC
C 150 Iinitiate KDEC
C 160 Iinitiate KDEC
C 170 Iinitiate KDEC
C
C 200 CCCC
C 210 Nil-check,stationnumber
C 220 YYGGggZ/GGggZ
C 230 AUTO
C 240 DDDff(GFmFm)Iw
C 250 DnDnDnVDxDxDx    
C 260 VVVV    
C 270 VxVxVxVx
C 280 RDrDr/VrVrVrVrVVrVrVrVrI
C 290 w'w'    
C 300 NNNhhh  
C 310 TT/TdTd 
C 320 Qpppp 
C 330 REw'w'
C 340 WS R (ldg/tkof)
C3000 IPT.GT.IEQ - end of report.
C4000 
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR.KEEP POINTER
C              TO FIRST CHARACTER OF REPORT.
C
C     THIS MAY BE LINE METAR/SPECI/TAF OR START OF MESSAGE (CCCC)
C


 100  CONTINUE
CTEST
ctestcc      print*,' SAINT - at start, KDEC(4) ', KDEC(4)
C*    Output to terminal
      IERR=0
C
C     Pointer to first character.
      KEEP = IPT
C
C     Flag indicating first decoding attempt on report.
      IFIRST = 0
      OKTT=.FALSE.
      OKPP=.FALSE.
C
C     Error indicator.
   10 KERR = 0         ! We start again here if ifirst > 0, with IPT=KEEP
C
C
C     Check for 'NIL' report.
      LEN = IEQ - IPT
C
C     Return if report shorter than 5 chars, flag error = 2
C     so it wont be decoded at all (even as 'NIL')
      IF(LEN .LT. 4)THEN
         KERR = 2
         RETURN
      END IF
C
      NIL = 0
C
cpsoct06      IF (KDEC(4).LE.14.AND.LEN.LE.17) NIL = 1   ! kdec(4)=85/86 for metar/speci
      if (len.le.25) nil = 1         ! This is what is used in mdb_metar.f (ecmwf)
C
      IF (NIL.EQ.1) THEN
          KERR = 1
          NOER(1,1) = NOER(1,1) + 1
C
C-----    PRINT 'NIL' METAR REPORTS
C-----
C-----    CALL PRTBULL (IPT,IEQ)
C-----
C-----    PRINT 'NIL' METAR REPORTS
C
      END IF
C
C
C*    Set area for decoded report to missing data value.
C*    Start is in word 4 as YY GG IW may already have been inserted.
      DO 110 I=5,300
         KINT(I) = MINDIC
  110 CONTINUE
C
  120 CONTINUE
C
C     Words 5-8 set to CCITT.5 'XXX  ' ( 'X' = 88, 'SPACE' = 32 )
C     Station identifier retained in these words in  CCITT 5 CHARS.
      DO 130 I=5,8
         KINT(I) = 88
  130 CONTINUE
C
C*    Clear data for previous report from decoded report header area.
      DO 140 I=1,3
         KDEC(I) = MINDIC
  140 CONTINUE
C
      DO 150 I=5,9
         KDEC(I) = MINDIC
 150  CONTINUE
C
C     Clear flag fields , retaining 'COR' flag.
      KDEC(13) = 0
      KDEC(15) = IAND(KDEC(15),4)
C
      DO 160 I=16,17
         KDEC(I) = MINDIC
  160 CONTINUE
C
      KDEC(20)=0
C
      DO 170 I=22,23
         KDEC(I) = MINDIC
  170 CONTINUE
C
C     Retain pointer to station identifier .
C
      KPT = IPT
      ID = 4
C
C     For "NIL" reports only the header will be decoded
      IF(NIL .EQ. 1) THEN
         KDEC(20)=1
      ENDIF
C
  200 CONTINUE
C     CCCC (ICAO)
C     Retain station identifier in character form 

cpsoct06 First check for 'METAR', which we skip
      if (kchar(ipt).eq.77 .and. kchar(ipt+1).eq.69 .and.
     1    kchar(ipt+2).eq.84 .and. kchar(ipt+3).eq.65 .and.     
     2    kchar(ipt+4).eq.82) then
         ipt = ipt + 5
         call nextprt(ipt,igs)
      end if
cpsnov06 Repeat for 'SPECI'
      if (kchar(ipt).eq.83 .and. kchar(ipt+1).eq.80 .and.
     1    kchar(ipt+2).eq.69 .and. kchar(ipt+3).eq.67 .and.     
     2    kchar(ipt+4).eq.73) then
         ipt = ipt + 5
         call nextprt(ipt,igs)
      end if

CX    CALL PRTST(IPT,IEQ,'CCCC:               ')
      IF( OKVAL(IPT,5,2,2,2,2,3,0,0,IERR) )THEN
          CALL EXTLTR(IPT,4,5,IRET)
      ELSE  
C         Error
          IRET=1
cpsaug06          CALL MARKERR(IPT,99,0)
          CALL MARKERR(IPT,59,0)  ! second argument must be <=60
          CALL NXTBLNK(IPT,IRET)
      ENDIF  
C
C
      IF(IRET.NE. 0) RETURN
      IF(NIL .EQ. 1) RETURN
C
  210 CONTINUE
C     GG gg (Z)
      CALL NEXTPRT(IPT,IEQ)
      IF (IPT.GE.IEQ) GO TO 3000
C
C     DOUBBLE-CHECK FOR NIL:
CX    CALL PRTST(IPT,IEQ,'NIL???              ')
C     PRINT*,'KCHAR:',CHAR(KCHAR(IPT)),CHAR(KCHAR(IPT+1)),
C    C    CHAR(KCHAR(IPT+2))
      IF( OKVAL(IPT,3,78,73,76,0,0,0,0,IERR) )THEN
C
          NIL=1
          KERR=1
      ENDIF
C
      IF(IRET.NE. 0) RETURN
      IF(NIL .EQ. 1) RETURN
C
C     Check if station is in stationlist.
C     Station number:
CX    print*,'CCCC-Stationnumber:'
      CALL METASTA(KINT(5),KINT(6),KINT(7),KINT(8),MINDIC,
     C             KDEC(16),KDEC(17),KDEC(5),KDEC(6),KDEC(8),KERR,IERR)
C
      IF(KERR.EQ.99)RETURN
      IF(IERR.NE.0)print*,'Error reading metastationfile, IERR=',IERR
      IF((KDEC(16).GT.0).AND.(IFIRST.EQ.0))
     C  PRINT*,(CHAR(KINT(I)),I=5,8),' stationnumber:',KDEC(16),KDEC(17)
C
C 
  220 CONTINUE
      IWPT=IPT
CX    CALL PRTST(IPT,IEQ,'GGggZ/YYGGggZ:      ')
C
C     Check if GGgg-group:
      IF( OKVAL(IPT,5,1,1,1,1,3,0,0,IERR) .OR.   
     C    OKVAL(IPT,5,1,1,1,1,90,0,0,IERR) )THEN
C
         CALL IC11DD(IPT,2,IERR)
         IF(IERR.EQ.0)THEN
            CALL EXTGRP ( IPT,2,2,0,0,0,167,IRET )
C
C           Skip Z if it is there
            IF(KCHAR(IPT).EQ.90) IPT=IPT+1
C
         ENDIF  
C
C     or  YYGGgg-group:
      ELSEIF( OKVAL(IPT,7,1,1,1,1,1,1,3,IERR) .OR.   
     C        OKVAL(IPT,7,1,1,1,1,1,1,90,IERR) )THEN
C       
c 960701-RR  TYPE here should be 1 i.e. test day first.
c 960701-RR         CALL IC11DD(IPT,2,IERR)
         CALL IC11DD(IPT,1,IERR)
         IF(IERR.EQ.0)THEN
C
            CALL EXTGRP ( IPT,2,2,2,0,0,166,IRET )
C           Skip Z if it is there
            IF(KCHAR(IPT).EQ.90) IPT=IPT+1
C
         ENDIF  
C
      ENDIF
C
  230 CONTINUE
C     (AUTO) 
      CALL NEXTPRT(IPT,IEQ)
      IWPT=IPT
      IF (IPT.GE.IEQ) GO TO 3000
cpsfeb99 check once more for 'NIL'
      IF( OKVAL(IPT,3,78,73,76,0,0,0,0,IERR) )THEN
C
          NIL=1
          KERR=1
      ENDIF
C
      IF(NIL .EQ. 1) RETURN
cps
C
CX    CALL PRTST(IPT,IEQ,'(AUTO)......?       ')
C
      IF( OKVAL(IPT,5,65,85,84,79,3,0,0,IERR) )THEN
C    
C         (AUTO)  
C
C        Extract 'A' in kint(10),
C        Increase IPT to next group.(Skip 'UTO')
         CALL EXTLTR ( IPT,1,10,IRET )
         IPT=IPT+3
C
      ENDIF
C
  240 CONTINUE
      CALL NEXTPRT(IPT,IEQ)
      IWPT=IPT
      IF (IPT.GE.IEQ) GO TO 3000
C
CX    CALL PRTST(IPT,IEQ,'DDDFF-GROUP:        ')
C     ddd ff Iw  OR  ddd ff G fmfm Iw
C
C     First check on nnn
C
cpsaug01 Added check for ddd'fff' (ff should be replaced by the
cps      exact number of wind speed units if > 100 (15.5.6 in WMO 
cps      Manuals on Code, FM 15&16))
      IF( OKVAL(IPT,6,1,1,1,1,1,1,0,IERR) )THEN
c          extract ddd and three digit ff
           CALL EXTGRP ( IPT,3,3,0,0,0,11,IRET )
cpsaug01
      ELSEIF( OKVAL(IPT,5,1,1,1,1,1,0,0,IERR) )THEN
C
C          DDD=nnn (integer) extract ddd 
C          ff=nn (integer) extract ff 
           CALL EXTGRP ( IPT,3,2,0,0,0,11,IRET )
C
cpsaug01 From 7. nov 2001 ff (and fmfm) may be preceded by P and reported
cps      as P99 (KT) or P50 (MPS) or P199 (KHM) if ff > 99 knots or > 50 m/s 
cps      or > 199 km/h. Choose to put P in kint(180) when P is present for ff.
cps      Note that no kdec is defined for P.
      ELSEIF( OKVAL(IPT,7,1,1,1,80,1,1,1,IERR) )THEN
cps        dddP199
           CALL EXTGRP ( IPT,3,0,0,0,0,11,IRET )
           CALL EXTLTR ( IPT,1,180,IRET )
           CALL EXTGRP ( IPT,3,0,0,0,0,12,IRET )
      ELSEIF( OKVAL(IPT,6,1,1,1,80,1,1,0,IERR) )THEN
cps        dddP99 or dddP50
           CALL EXTGRP ( IPT,3,0,0,0,0,11,IRET )
           CALL EXTLTR ( IPT,1,180,IRET )
           CALL EXTGRP ( IPT,2,0,0,0,0,12,IRET )
      ELSEIF( OKVAL(IPT,5,86,82,66,1,1,0,0,IERR) )THEN
C
Clil          ddd=VRB, set kint(11) to 999, extract ff
Clil           KINT(11)=999
ClilNew21/10/96:ddd=0, if ddd=VRB.
cpsjun99: changed to 510, because this is the value we want to
cps       use for variable wind in bufr, to conform to changes 
cps       made in quality control routine libsrc/cddff.f
cps           KINT(11)=0
cpsjun06 Changed back to old code, but added a new variable to indicate VRB,
cps      used to possible change kint(11) from 0 to 360 later
cps           KINT(11)=510
           KINT(11)=0
           ddVRB=.true.
           IPT=IPT+3
C          ff=nn (integer) extract ff 
           CALL EXTGRP ( IPT,2,0,0,0,0,12,IRET )
C
      ELSEIF( OKVAL(IPT,5,47,47,47,1,1,0,0,IERR) )THEN
C     
C          ddd=///
           IPT=IPT+3
C          ff=nn (integer) extract ff 
           CALL EXTGRP ( IPT,2,0,0,0,0,12,IRET )
C
      ELSEIF( OKVAL(IPT,5,1,1,1,47,47,0,0,IERR) )THEN
C     
C          ddd=nnn
C          ff=//
           CALL EXTGRP ( IPT,3,0,0,0,0,11,IRET )
C          Go to next group.
           CALL NXTBLNK(IPT,IRET)
C     
      ELSEIF( OKVAL(IPT,5,47,47,47,47,47,0,0,IERR) )THEN
C     
C          ddd=///
C          ff=//
C          Go to next group.
           CALL NXTBLNK(IPT,IRET)
C     
      ELSE
C
C          Not wind-group. 
CX         CALL PRTST(IPT,IEQ,'ERROR IN DDD-GROUP:')
           GO TO 250
      ENDIF
C
C
C     Check range of values.
C
C     IF (KINT(11).LT.-1) IRET = 1
C     IF (KINT(11).GT.86.AND.KINT(11).NE.99) IRET = 1
C     IF (KINT(11).GT.36.AND.KINT(11).LT.51) IRET = 1
C     IF (KINT(11).EQ.0.AND.KINT(12).NE.0) IRET = 1
C     IF (KINT(11).EQ.MINDIC) IRET = 0
C
C
C     Group as 'DDDFFIw'  or  'DDDFFGFmFmIw' ??
C     Check for gust.
C
      IF(KCHAR(IPT).EQ.71)THEN
C        
C        DDD FF G FmFm Iw
C        Extract 'G' in kint(13), extract FmFm (kint(14))
         CALL EXTLTR ( IPT,1,13,IRET )
cpsaug01 Added check for ddd'fmfmfm' (fmfm should be replaced by the
cps      exact number of wind speed units if > 100 (15.5.6 in WMO 
cps      Manuals on Code, FM 15&16)).
cps      Also, from 7. nov 2001 fmfm (and ff) may be preceded by P and reported
cps      as P99 (KT) or P50 (MPS) or P199 (KHM) if fmfm > 99 knots or > 50 m/s or 
cps      > 199 km/h. Choose to put P in kint(181) when P is present for fmfm
         IF( OKVAL(IPT,3,1,1,1,0,0,0,0,IERR) )THEN
c           extract three digit fmfm
            CALL EXTGRP ( IPT,3,0,0,0,0,14,IRET )
         ELSEIF( OKVAL(IPT,3,80,1,1,1,0,0,0,IERR) )THEN
cps         P199
            CALL EXTLTR ( IPT,1,181,IRET )
            CALL EXTGRP ( IPT,3,0,0,0,0,14,IRET )
         ELSEIF( OKVAL(IPT,3,80,1,1,0,0,0,0,IERR) )THEN
cps         P99 or P50
            CALL EXTLTR ( IPT,1,181,IRET )
            CALL EXTGRP ( IPT,2,0,0,0,0,14,IRET )
cpsaug01
         ELSEIF( OKVAL(IPT,2,1,1,0,0,0,0,0,IERR) )THEN
            CALL EXTGRP ( IPT,2,0,0,0,0,14,IRET )
         ELSE
C          Something wrong in wind-group.
C          Go to next group.
CX         CALL PRTST(IPT,IEQ,'ERROR IN FmFm-GROUP:')
           CALL MARKERR(IPT,6,IFIRST)
           CALL NXTBLNK(IPT,IRET)
           GO TO 250
         ENDIF
C
      ENDIF

C     Iw: Windindicator
CX    CALL PRTST(IPT,IEQ,'IW-GROUP:        ')
C
C     Set windindicator in KINT(18)
      IF( OKVAL(IPT,3,75,77,72,0,0,0,0,IERR) )THEN
C
C        kmt - km/hour - IW=2
CX       print*,'KMT:'
         KINT(18)=2
C
      ELSEIF( OKVAL(IPT,2,75,84,0,0,0,0,0,IERR) )THEN
C
C        kt - knot - IW=1
CX       print*,'KT:'
         KINT(18)=1
C
      ELSEIF( OKVAL(IPT,3,77,80,83,0,0,0,0,IERR) )THEN
C
C        mps - m/sec - IW=0
CX       print*,'MPS:'
         KINT(18)=0
C
      ELSE
C
C        Not accepted IW=MINDIC
CX       CALL PRTST(IPT,IEQ,'Not accepted IW=MINDIC')
         KINT(18)=MINDIC
C        In case wind-group is missing, move IPT one back
C        to avoid error-mark on next group
         IPT=IPT-1
         CALL MARKERR(IPT,6,IFIRST)
C
      ENDIF
C
C     Find next spc, group after Iw
      IF(.NOT.OKSEP(KCHAR(IPT),IERR) )CALL NXTBLNK(IPT,IERR)
C
  250 CONTINUE
C     Locate next group .
      CALL NEXTPRT(IPT,IEQ)
      IWPT=IPT
      IF (IPT.GE.IEQ) GO TO 3000
C
CX    CALL PRTST(IPT,IEQ,'DDDVDDD-GROUP:      ')
C
C     DNDNDN V DXDXDX ?
C
      IF( OKVAL(IPT,7,1,1,1,86,1,1,1,IERR) )THEN
C
C        DNDNDN V DXDXDX 
C
CX       print*,'FOUND DDDVDDD-GROUP:'
C
         CALL EXTGRP( IPT,3,0,0,0,0,19,IRET )
         CALL EXTLTR( IPT,1,20,IRET )
         CALL EXTGRP( IPT,3,0,0,0,0,21,IRET )
C
      END IF
C
  260 CONTINUE
C
C     Locate next group .
      IF (IPT.GE.IEQ) GO TO 3000
      IF( OKSEP(KCHAR(IPT),IERR))CALL NEXTPRT(IPT,IEQ)
C
CX    CALL PRTST(IPT,IEQ,'VVVV..-GROUP:      ')
C     Is VVVV obligatory?
cps Yes! But may be coded as CAVOK
C
C     CAVOK?
CNBNBNEW: New BUFR-table means new values for cavok.
C     Set flag for .not. CAVOK, before testing. 
cmar97      KINT(22)=0
      KINT(22)=15
C
C
      IF( OKVAL(IPT,5,67,65,86,79,75,0,0,IERR) )THEN
C
C         CAVOK, set flag. 
C
CX        print*,'CAVOK!'
CNBNBNEW: New table:Flag value for CAVOK=2, not 1. 
Cmar97          KINT(22)=1
          KINT(22)=2
cokt18          KINT(23)=0
          IPT=IPT+5
          GO TO 280
C
      ELSEIF( OKVAL(IPT,5,1,1,1,1,3,0,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND VVVV  ')
C        Found group of 4 integers - VVVV
C        Extract  these
         CALL EXTGRP( IPT,4,0,0,0,0,23,IRET )
C
      ELSEIF( OKVAL(IPT,6,1,1,1,1,2,3,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND VVVVDv')
C        Found group of 4 integers, followed by 1 letter 
C        Extract these
         CALL EXTGRP( IPT,4,0,0,0,0,23,IRET )
C
C        DV saved in KINT(24).
         CALL EXTLTR( IPT,1,24,IRET )
C
      ELSEIF( OKVAL(IPT,7,1,1,1,1,2,2,3,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND VVVVDvDv')
C        Found group of 4 integers, followed by 2 letters 
C        Extracts these
         CALL EXTGRP( IPT,4,0,0,0,0,23,IRET )
C
C        DV saved in KINT(24)&KINT(25)
         CALL EXTLTR( IPT,2,24,IRET )
C
cpsnov05
cps would like to search for following space also, but okval can't take 8 arguments
      ELSEIF( OKVAL(IPT,7,1,1,1,1,78,68,86,IERR) )THEN
C
         CALL PRTST(IPT,IEQ,'FOUND VVVVNDV       ')
C        Found group of 4 integers, followed by letters NDV
C        Extracts these
         CALL EXTGRP( IPT,4,0,0,0,0,23,IRET )
C
C        NDV Extract first letter into KINT (note: currently not decoded to Bufr)
         CALL EXTLTR( IPT,1,183,IRET )
         IPT=IPT+2 
C
      ELSEIF( OKVAL(IPT,5,47,47,47,47,3,0,0,IERR) )THEN
C
C         VVVV=////
C         This is accepted, no error-flag
          CALL NXTBLNK(IPT,IRET)
          GO TO 280
C
       ELSE
C
C         Not CAVOK and not numeric  and ///// - ERROR?
C         CALL PRTST(IPT,IEQ,'ERROR IN VVVV          ')
C         CALL MARKERR(IPT,8,IFIRST)
          GO TO 280
C
      ENDIF
C
C
  270 CONTINUE
C     Check if  VxVxVxVx Dv / VxVxVxVx DvDv
CX    CALL PRTST(IPT,IEQ,'VVVVD/VVVVDD-GROUP: ')
C
      IF (IPT.GE.IEQ) GO TO 3000
      IF( OKSEP(KCHAR(IPT),IERR))CALL NEXTPRT(IPT,IEQ)
C
C
cps To do it properly, we ought to mark as error if a second 
cps VVVVD/VVVVDD-GROUP follows after VVVVNDV. To be implemented.

      IF( OKVAL(IPT,5,1,1,1,1,3,0,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND SECOND VVVV  ')
C        Found group of 4 integers - VVVV
C        Extracts these
cpsnov05 Second group must be VNVNVNVDv(Dv) in new code (2005),
cps      or VxVxVxVxDv(Dv) in old code
cps         CALL EXTGRP( IPT,4,0,0,0,0,26,IRET )
          CALL PRTST(IPT,IEQ,'ERROR IN 2 VVVV        ')
          CALL MARKERR(IPT,8,IFIRST)
          CALL NXTBLNK(IPT,IRET)
C
      ELSEIF( OKVAL(IPT,6,1,1,1,1,2,3,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND 2.VVVVDv')
C        Found group of 4 integers 
C        Extracts these
         CALL EXTGRP( IPT,4,0,0,0,0,26,IRET )
C
C        DV saved in KINT(27).
         CALL EXTLTR( IPT,1,27,IRET )
C
      ELSEIF( OKVAL(IPT,7,1,1,1,1,2,2,3,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND 2.VVVVDvDv')
C        Found group of 4 integers 
C        Extracts these
         CALL EXTGRP( IPT,4,0,0,0,0,26,IRET )
C
C        DV saved in KINT(27)&KINT(28)
         CALL EXTLTR( IPT,2,27,IRET )
C
       ELSEIF( OKVAL(IPT,5,47,47,47,47,3,0,0,IERR) )THEN
C
C         VVVV=////
C         This is not accepted for 2. VVVV,
CX        CALL PRTST(IPT,IEQ,'ERROR IN 2 VVVV        ')
          CALL MARKERR(IPT,8,IFIRST)
          CALL NXTBLNK(IPT,IRET)
C
       ELSE
C
C          Not CAVOK, not numeric, not ///// - ERROR?
cpsnov05 No error, there need be no second VVVV group
cps           CALL PRTST(IPT,IEQ,'ERROR IN VVVV          ')
C          CALL MARKERR(IPT,8,IFIRST)
      ENDIF
C
C
C
  280 CONTINUE
C
C
C     Locate next group .
C
C     Runway-visibility. UP to 5 times.
      IREP=0
C
C
C
  281 CONTINUE
      IF (IPT.GE.IEQ) GO TO 3000
CX    CALL PRTST(IPT,IEQ,'RVVVV-GROUPS:      ')
      CALL NEXTPRT(IPT,IEQ)
C
C     Check if Rnn.
      IF( OKVAL(IPT,3,82,1,1,0,0,0,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND RnnVV-GROUPS: ')
         IWPT=IPT
C
C
C      
C        R DrDr / VrVrVrVr I  or
C        R DrDr / VrVrVrVr V VrVrVrVr I
C
C        First convert (R DrDr / VrVrVrVr)
C        Set KINTpointer
         KINTPT=IREP*10 + 30
C
C        Extract R,Dr,/ in kint.
C        kintpt=30,40,50,60,70
         CALL EXTLTR(IPT,1,KINTPT,IRET)
CTEST
CX       CALL PRTST(IPT,IEQ,'RVVVV- R,KINTPT='//
CX   C              CHAR(KINTPT/10+48)//
CX   C              CHAR(KINTPT-(KINTPT/10)*10+48) )
CTEST
C
         KINTPT=KINTPT+1
C        Extract Dr in kint.
C        kintpt=31,41,51,61,71
         CALL EXTGRP( IPT,2,0,0,0,0,KINTPT,IRET )
CTEST
CX       CALL PRTST(IPT,IEQ,'RVVVV-Dr,KINTPT='//
CX   C              CHAR(KINTPT/10+48)//
CX   C              CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
         KINTPT=KINTPT+1
C
C        Extract / in kint.
C        We have either just 1 /
C                1 letter followed by /
C                2 letters followed by /
C
C        kintpt=32,42,52,62,72
C
         IF( OKVAL(IPT,1,47,0,0,0,0,0,0,IERR) )THEN
C
C           / 
CTEST
CX          CALL PRTST(IPT,IEQ,'RVVVV - /,KINTPT='//
CX   C                 CHAR(KINTPT/10+48)//
CX   C                 CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
            CALL EXTLTR(IPT,1,KINTPT+2,IRET)
            KINT(KINTPT )=32
            KINT(KINTPT+1)=32
C
         ELSEIF( OKVAL(IPT,2,2,47,0,0,0,0,0,IERR) )THEN
C 
C           L/ or C/ or R/
CTEST
CX          CALL PRTST(IPT,IEQ,'RVVVV - L/,KINTPT='//
CX   C                 CHAR(KINTPT/10+48)//
CX   C                 CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
            CALL EXTLTR(IPT,1,KINTPT,IRET)
            KINT(KINTPT+1)=32
            CALL EXTLTR(IPT,1,KINTPT+2,IRET)
C
         ELSEIF( OKVAL(IPT,3,2,2,47,0,0,0,0,IERR) )THEN
C 
C           LL/ or RR/
CTEST
CX          CALL PRTST(IPT,IEQ,'RVVVV-LL/,KINTPT='//
CX   C                 CHAR(KINTPT/10+48)//
CX   C                 CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
            CALL EXTLTR(IPT,3,KINTPT,IRET)
C  
         ELSE
C
C           Something wrong with runwaygroup
C           Find next space
CX          print*,'ERROR in RW-group'
            IPT=IWPT
            CALL MARKERR(IPT,9,IFIRST)
            CALL NXTBLNK(IPT,IRET)
            GO TO 281
C
         ENDIF
C
         KINTPT=KINTPT+3
C
C
C        Extract VVVV
C        kintpt=35,45,55,65,75
C        Check if preceeded by 'P' or 'M'
         IF((KCHAR(IPT).EQ.80).OR.(KCHAR(IPT).EQ.77)) THEN
C
CTEST
CX       CALL PRTST(IPT,IEQ,'RVVVV- P,KINTPT='//     
CX   C                 CHAR(KINTPT/10+48)//
CX   C                 CHAR(KINTPT-(KINTPT/10)*10+48) )
CTEST
C
            CALL EXTLTR(IPT,1,KINTPT,IRET)
C
         ELSE 
C           Mark accurate reading
            KINT(KINTPT)=65
         ENDIF
         KINTPT=KINTPT+1    
C
C        kintpt=36,46,56,66,76
C        Check if numeric.
         IF( OKVAL(IPT,4,1,1,1,1,0,0,0,IERR) )THEN
C
CTEST
CX          CALL PRTST(IPT,IEQ,'RVVVV,KINTPT='//
CX   C                 CHAR(KINTPT/10+48)//
CX   C                 CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
            CALL EXTGRP( IPT,4,0,0,0,0,KINTPT,IRET )
C
         ELSE
C
C           Something wrong with runwaygroup
C           Find next space
CX          print*,'ERROR in RW-group'
            IPT=IWPT
            CALL MARKERR(IPT,9,IFIRST)
            CALL NXTBLNK(IPT,IRET)
            GO TO 281
C
         ENDIF
C
         KINTPT=KINTPT+1
C 
C        Next letter 'V'
         IF(KCHAR(IPT).EQ.86)THEN
C 
C            V VrVrVrVr 
C
C            Extract V in kint.
C            kintpt=37,47,57,67,77
             CALL EXTLTR(IPT,1,KINTPT,IRET)
CTEST
CX           CALL PRTST(IPT,IEQ,'RVVVV - V,KINTPT='//
CX   C        CHAR(KINTPT/10+48)//
CX   C        CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
C
C            Extract VVVV
C            Check if preceeded by 'P'
C            If so, it is put in kint instead of'V'
             IF(KCHAR(IPT).EQ.80)THEN
C
CTEST
CX              CALL PRTST(IPT,IEQ,'RVVVV- P,KINTPT='//
CX   C                     CHAR(KINTPT/10+48)//
CX   C                     CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
C
                CALL EXTLTR(IPT,1,KINTPT,IRET)
                IF(IRET.NE.0)THEN
C
C                  Something wrong with runwaygroup
C                  Find next space
CX                 print*,'ERROR in RW-group'
                   IPT=IWPT
                   CALL MARKERR(IPT,9,IFIRST)
                   CALL NXTBLNK(IPT,IRET)
                   GO TO 281
C
                ENDIF
             ENDIF
C
C            Now extract VVVV
             KINTPT=KINTPT+1    
C            kintpt=38,48,58,68,78
             IF( OKVAL(IPT,4,1,1,1,1,0,0,0,IERR) )THEN
                CALL EXTGRP( IPT,4,0,0,0,0,KINTPT,IRET )
             ELSE
C
C               Something wrong with runwaygroup
C               Find next space
CX              print*,'ERROR in RW-group'
                IPT=IWPT
                CALL MARKERR(IPT,9,IFIRST)
                CALL NXTBLNK(IPT,IRET)
                GO TO 281
C
             ENDIF
C  
CTEST
CX           CALL PRTST(IPT,IEQ,'RVVVV,KINTPT='//
CX   C        CHAR(KINTPT/10+48)//
CX   C        CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
             KINTPT=KINTPT+1
C
          ELSE
C
C            Adjust kintpt
             KINTPT=KINTPT+2
C
          ENDIF  
C
C        Extract I in kint.
C        kintpt=39,49,59,69,79
C
         IF(.NOT.OKSEP(KCHAR(IPT),IERR))THEN
C
CTEST
CX          CALL PRTST(IPT,IEQ,'Rvvvv - I ,KINTPT='//
CX   C                 CHAR(KINTPT/10+48)//
CX   C                 CHAR(KINTPT-KINTPT/10*10+48) )
CTEST
C
            CALL EXTLTR(IPT,1,KINTPT,IRET)
            IF(IRET.NE.0)THEN
C
C              Something wrong with runwaygroup
C              Find next space
CX             print*,'ERROR in RW-group'
               IPT=IWPT
               CALL MARKERR(IPT,9,IFIRST)
               CALL NXTBLNK(IPT,IRET)
               GO TO 281
C
            ENDIF
         ENDIF
C
         IREP=IREP+1
         IF(IREP.LT.5)GO TO 281      
      ENDIF
C
      IREP=0
      IREP2=0
 290  CONTINUE
C
C
C     Locate next group .
      IF (IPT.GE.IEQ) GO TO 3000
      CALL NEXTPRT(IPT,IEQ)
CX    CALL PRTST(IPT,IEQ,"TESTING FOR W'W'-GROUP:")
C
C     W'W'  up to 3 times
C     From 2 to 9 characters
C
C     Mark start of w'w'.
      IWPT=IPT
C
C
C     Check if 'SKC' or 'NSC'
cpsnov05 or 'NCD'
cpsnov07 SKC no longer allowed (but we keep checking for it)
C     These must be eliminated because they are not followed by numbers,
C     and therefore can be mistaken as ww-group.
C     We do not bother at this stage with the internal syntax for ww, as
C     this may change. We only state that ww is a group of letters. (+/-)
C
      IF( OKVAL(IPT,4,83,75,67,3,0,0,0,IERR) .OR.
     C        OKVAL(IPT,4,78,83,67,3,0,0,0,IERR) .OR.
     C        OKVAL(IPT,4,78,67,68,3,0,0,0,IERR) )THEN
C
C        Cloudgroups.
         GO TO 300
C
C     Check if REcent ww:
C     We also eliminate RE, because this is not correct form of ww, but could
C     be mistaken for it. 
C
      ELSEIF( OKVAL(IPT,4,82,69,2,2,0,0,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'Found recent weather:')
         GO TO 330
C
cpsnov05 Some report (missing) w'w' as '//'
      ELSEIF( OKVAL(IPT,3,47,47,3,0,0,0,0,IERR) )THEN
         CALL PRTST(IPT,IEQ,'Found //            ')
         IPT=IPT+3
         GO TO 300
      ENDIF

C     First letter may be +/-
CX    print*,'KCHAR',IPT,KCHAR(IPT) 
      IF((KCHAR(IPT).EQ.43).OR.(KCHAR(IPT).EQ.45))THEN
         IPT=IPT+1
      ENDIF
C
      DO WHILE(OKLTR(KCHAR(IPT),IERR))
CX       CALL PRTST(IPT,IEQ,'Counting letters,ww:')
         IPT=IPT+1
      END DO
C
C     If not followed by a separator this is no ww-group
      IF( .NOT.OKSEP(KCHAR(IPT),IERR) )THEN
C
         IPT=IWPT
         GO TO 300
C        
      ELSE
         KLEN=IPT-IWPT 
CX       print*,'Found ww,len=',KLEN
         IF(KLEN.GT.9)THEN
C
C           Something wrong with ww-group
C           Find next space
CX          print*,'Too long WW-group'
            IPT=IWPT
            CALL MARKERR(IPT,10,IFIRST)
            CALL NXTBLNK(IPT,IEQ)
            GO TO 290
C
C        Check if any has been extracted.
         ELSEIF(KLEN.LT.2)THEN
C
C           Something wrong with ww-group
C           Find next space
CX          print*,'Too short WW-group'
            IPT=IWPT
            GO TO 300
C
         ELSE
C
C           Kintpt=110,120,130
            KINTPT=110 + IREP*10 
            IPT=IWPT
C
            KINT(KINTPT)=KLEN
            CALL EXTLTR(IPT,KLEN,KINTPT+1,IERR)
C
C
CX          CALL PRTST(IPT,IEQ,
CX   C                   'Extracted '//CHAR(KLEN2+48)//' ww:      ')
C
C
            IF(IERR.NE.0)THEN
C           Something wrong with ww-group
               CALL MARKERR(IPT,10,IFIRST)
               CALL NXTBLNK(IPT,IEQ)
C              Go to next group (Clouds)
               IPT=IWPT
               GO TO 300
            ENDIF
C
C
C         
            IREP=IREP+1
            IF (IREP.LT.3) GO TO 290
         ENDIF
      ENDIF
C
C
C
C
C
 300  CONTINUE
      IERR=0
C
C     Locate next group.
C
C     NsNsNs hshshs (CU/TCB)
C     VV HsHsHs
C     SKC
cpsnov05 NSC, NCD også mulig.
cpsnov07 SKC no longer allowed (but we keep checking for it)
cps Hvordan dekodes NCD til Bufr? Sjekk om ny 020 009 fra WMO
C   
      IF (IPT.GE.IEQ) GO TO 3000
C
C     Check if NsNsNshshshs GROUP
C     May occur 4 times:
      IREP=0
C
 305  CONTINUE
C
C     First check if we already have done 4 cloudgroups.
      IF(IREP.EQ.4)GO TO 310
C
      CALL NEXTPRT(IPT,IEQ)
CX    CALL PRTST(IPT,IEQ,'Cloud-groups:       ')
C
      IWPT=IPT
C     Set KINTpointer
      KINTPT=IREP*5 + 80
C
C
C 
C
C VVnnn_
      IF( OKVAL(IPT,6,86,86,1,1,1,3,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'Found VVhhh-group:  ')
         CALL EXTLTR( IPT,2,KINTPT,IRET )
         KINTPT=KINTPT+2
         KINT(KINTPT)=32
         KINTPT=KINTPT+1
         CALL EXTGRP(IPT,3,0,0,0,0,KINTPT,IRET)
C
C   
C NNNnnn
      ELSEIF( OKVAL(IPT,7,2,2,2,1,1,1,3,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'Found cloudgroup    ')
         CALL EXTLTR( IPT,3,KINTPT,IRET )
         KINTPT=KINTPT+3
         CALL EXTGRP(IPT,3,0,0,0,0,KINTPT,IRET)
C
C   
C NNN_
      ELSEIF( OKVAL(IPT,4,2,2,2,3,0,0,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'Found NSC/SKC       ')
         CALL EXTLTR( IPT,3,KINTPT,IRET )
         GO TO 310
C
C  
C 
C NNNnnn(CB/TCU_)
      ELSEIF( OKVAL(IPT,7,2,2,2,1,1,1,2,IERR) )THEN
C
         IPTCC=IPT+6
         IF( OKVAL(IPTCC,3,67,66,3,0,0,0,0,IERR) )THEN
C           Significant convective cloud CB
CX          CALL PRTST(IPT,IEQ,'Found NNN-CBgroup:  ')
            CALL EXTLTR( IPT,3,KINTPT,IRET )
            KINTPT=KINTPT+3
            CALL EXTGRP(IPT,3,0,0,0,0,KINTPT,IRET)
            KINTPT=KINTPT+1
            CALL EXTLTR( IPTCC,1,KINTPT,IRET )
C
         ELSEIF( OKVAL(IPTCC,4,84,67,85,3,0,0,0,IERR) )THEN
C
C           Significant convective cloud TCU
CX          CALL PRTST(IPT,IEQ,'Found NNN-TCUgroup: ')
            CALL EXTLTR( IPT,3,KINTPT,IRET )
            KINTPT=KINTPT+3
            CALL EXTGRP(IPT,3,0,0,0,0,KINTPT,IRET)
            KINTPT=KINTPT+1
            CALL EXTLTR( IPTCC,1,KINTPT,IRET )
C
         ELSE
C
C
CX          CALL PRTST(IPT,IEQ,'Not Cloudgroup     ')
            GO TO 310
         ENDIF
C
C
C        If convective cloud skip CB/CTU.
         CALL NXTBLNK(IPT,IEQ)
C
C   
C NNN///_
      ELSEIF( OKVAL(IPT,7,2,2,2,47,47,47,3,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'Found NNN///        ')
         CALL NXTBLNK(IPT,IRET)
         GO TO 305
C
C   
C VV///_
      ELSEIF( OKVAL(IPT,6,86,86,47,47,47,3,0,IERR) )THEN
C
CX       CALL PRTST(IPT,IEQ,'Found VV///        ')
         CALL NXTBLNK(IPT,IRET)
         GO TO 305
C
C
C Not recognized cloud-group.
      ELSE
C
C
C
CX       CALL PRTST(IPT,IEQ,'Not Cloudgroup     ')
         GO TO 310
C
      ENDIF
C
      IREP=IREP+1
C
      IF (IPT.GE.IEQ) GO TO 3000
      GO TO 305       
C
C
C
 310  CONTINUE
C
C     
C     Locate next group .
C
      CALL NEXTPRT(IPT,IEQ)
      IWPT=IPT
      IF (IPT.GE.IEQ) GO TO 3000
C
CX    CALL PRTST(IPT,IEQ,'TT/TT..-group:      ')
      IWPT=IPT
C
C     TT/TT or MTT/TT or TT/MTT or MTT/MTT or /////
C
      IF( OKVAL(IPT,6,47,47,47,47,47,3,0,IERR) )THEN
C
C        Temperature is missing /////
         OKTT=.TRUE.
         CALL NXTBLNK(IPT,IRET)
         GO TO 320
C         
      ELSEIF( OKVAL(IPT,3,1,1,47,0,0,0,0,IERR) )THEN
C
C        Positive TT
         CALL EXTGRP( IPT,2,0,0,0,0,103,IRET )
         CALL EXTLTR(IPT,1,104,IRET) 
C        Extracted TT, set boolean.
         OKTT=.true.
C 
      ELSEIF( OKVAL(IPT,4,77,1,1,47,0,0,0,IERR) )THEN
C
C        Negative TT
         CALL EXTLTR(IPT,1,102,IRET) 
         CALL EXTGRP( IPT,2,0,0,0,0,103,IRET )
         CALL EXTLTR(IPT,1,104,IRET) 
C        Extracted TT, set boolean.
         OKTT=.true.
C
      ELSE
C
C
C        No temp group
         GO TO 320
C         
      ENDIF
C
C
CX    CALL PRTST(IPT,IEQ,'TDTD..-GROUP:       ')
C     TDTD or MTDTD
C
      IF( OKVAL(IPT,4,77,1,1,3,0,0,0,IERR) )THEN
C
C        Negative TDTD
         CALL EXTLTR(IPT,1,105,IRET) 
         CALL EXTGRP( IPT,2,0,0,0,0,106,IRET )
C
      ELSEIF( OKVAL(IPT,3,1,1,3,0,0,0,0,IERR)   )THEN
C
C        Positive TDTD
         CALL EXTGRP( IPT,2,0,0,0,0,106,IRET )
C
      ELSEIF( OKVAL(IPT,3,47,47,3,0,0,0,0,IERR)   )THEN
C
C        Missing TDTD (//)
C        Skip //
         CALL NXTBLNK(IPT,IRET)
         GO TO 320
C
      ELSE
C
C        Error
CX       print*,'TD:,KCHAR(IPT+2):',KCHAR(IPT+2)
         CALL MARKERR(IPT,9,IFIRST)
         CALL NXTBLNK(IPT,IRET)
         GO TO 320
C         
      ENDIF
C

C
 320  CONTINUE
C
C     Locate next group .
C
C
C     Q PhPhPhPh
C     A PhPhPhPh
cpsnov07 A no longer allowed (but we keep checking for it)
C     
      CALL NEXTPRT(IPT,IEQ)
      IWPT=IPT
      IF (IPT.GE.IEQ) GO TO 3000
C
CX    CALL PRTST(IPT,IEQ,'QPPPP-GROUP         ')
      IF( (KCHAR(IPT).EQ.81).OR.(KCHAR(IPT).EQ.65) )THEN
C
CX       CALL PRTST(IPT,IEQ,'FOUND pppp         ')
         CALL EXTLTR( IPT,1,107,IRET )
C
         IF( OKVAL(IPT,5,1,1,1,1,3,0,0,IERR) )THEN
C
            CALL EXTGRP( IPT,4,0,0,0,0,108,IRET )
C           Extracted PP, set boolean.
            OKPP=.true.
C
         ELSEIF( OKVAL(IPT,5,47,47,47,47,3,0,0,IERR) )THEN
C
C           PP is missing Q////
C           We accept this ass pp-group, set boolean.
            OKPP=.true.
            CALL NXTBLNK(IPT,IRET)
C
C
         ELSE
C
C           Something wrong with ppp-group
C           Find next space
CX          print*,'ERROR in Pressure-group'
            CALL MARKERR(IPT,13,IFIRST)
            CALL NXTBLNK(IPT,IRET)
            GO TO 330
C
C
         ENDIF
C 
      ENDIF  
C
C
C     
 330  CONTINUE
      IREP=0

 331  CONTINUE
C
C     Locate next group .
      CALL NEXTPRT(IPT,IEQ)
      IWPT=IPT
      IF (IPT.GE.IEQ) GO TO 3000
C
CX    CALL PRTST(IPT,IEQ,'REww       -GROUP:  ')
C
      IF( OKVAL(IPT,4,82,69,2,2,0,0,0,IERR).or.
     C    OKVAL(IPT,4,82,69,2,2,2,2,0,IERR) )THEN
C
CX          CALL PRTST(IPT,IEQ,
CX     C                'FOUND REww '//CHAR(IREP+49)//'        ')

cpsoct05 Rewritten rest by copying from w'w'
C         Save IPT
         KIPT=IPT 
Cps Note that REw'w' shall not include intensity (+/-/VC)
C        Check lenght of group.
         DO WHILE (OKLTR(KCHAR(IPT),IERR))
            IPT=IPT+1 
         ENDDO  
         KLEN=IPT-KIPT 
         IF(KLEN.GT.6)THEN
C
C           Something wrong with REw'w'-group
C           Find next space
            print*,"Too long REw'w'-group"
            IPT=KIPT
            CALL MARKERR(IPT,10,IFIRST)
            CALL NXTBLNK(IPT,IEQ)
            GO TO 340
C
C        Check if any has been extracted.
         ELSEIF(KLEN.LT.2)THEN
C
C           Something wrong with REw'w'-group
C           Find next space
            print*,'Too short REWW-group'
            IPT=KIPT
            GO TO 340
C
         ELSE
            KINTPT=140 + IREP*6
            CALL EXTLTR(KIPT,KLEN,KINTPT,IRET)
            IF(IRET.NE.0)THEN
C
C            Something wrong with REww-group
C            Find next space
CX             print*,'ERROR in REww-group'
               CALL MARKERR(IPT,14,IFIRST)
               CALL NXTBLNK(IPT,IRET)
               GO TO 340
            ENDIF
         ENDIF
C
         IREP=IREP + 1
         IF(IREP.LE.3)GO TO 331
C
      ENDIF  
C
 340  CONTINUE
cpsoct01 moved irep=0 here
      IREP=0
 341  continue
C
C     Find next group.
      CALL NEXTPRT(IPT,IEQ)
      IWPT=IPT
      IF (IPT.GE.IEQ) GO TO 3000
C
cpsoct01     IREP=0
 345  CONTINUE
C     Check if WS:
      IF( OKVAL(IPT,3,87,83,3,0,0,0,0,IERR) )THEN
C
C        WS
CX       CALL PRTST(IPT,IEQ,'FOUND WS            ')
         IPT=IPT+2 
         KINTPT=158+IREP*4
C
C        Find  ALL
         CALL NEXTPRT(IPT,IEQ)
C
cpsnov05 US domestic metar might use LDG (LanDinG) or TKO (TaKe Off)
cps      between WS and R(WY)nn. Skip this!
         IF( OKVAL(IPT,4,76,68,71,3,0,0,0,IERR) .OR.
     *        OKVAL(IPT,4,84,75,79,3,0,0,0,IERR) )THEN
            IPT=IPT+3
            CALL NEXTPRT(IPT,IEQ)
         ENDIF

         IF( OKVAL(IPT,4,65,76,76,3,0,0,0,IERR) )THEN
C
C           ALL - Extract first letter into KINT
cps         i.e. 'A'
CX          CALL PRTST(IPT,IEQ,'FOUND  ALL       ')
            CALL EXTLTR( IPT,1,KINTPT,IRET )
            IPT=IPT+2 
C
         ELSEIF( OKVAL(IPT,6,82,87,89,1,1,3,0,IERR) )THEN 
cpsnov07 WY no longer allowed (but we keep checking for it)
C
C           Found RWYnn
CX          CALL PRTST(IPT,IEQ,'FOUND RWYnn      ')
C
C           WS RUNWAY?- Extract first letter into KINT
cps         This should always be 'R' 
            CALL EXTLTR( IPT,1,KINTPT,IRET )
            IPT=IPT+2 
C
C           RWY - Extract runwaydesignator. 
            CALL EXTGRP( IPT,2,0,0,0,0,KINTPT+1,IRET )
            KINT(KINTPT+2)=32
            KINT(KINTPT+3)=32
C
cpsnov07 New branch: WY should no longer be included
         ELSEIF( OKVAL(IPT,4,82,1,1,3,0,0,0,IERR) )THEN 
C
C           Found Rnn
CX          CALL PRTST(IPT,IEQ,'FOUND RWY        ')
C
C           WS RUNWAY?- Extract first letter into KINT
cps         This should always be 'R' 
            CALL EXTLTR( IPT,1,KINTPT,IRET )
C           DRDR - Extract runwaydesignator. 
            CALL EXTGRP( IPT,2,0,0,0,0,KINTPT+1,IRET )
            KINT(KINTPT+2)=32
            KINT(KINTPT+3)=32
C
         ELSEIF( OKVAL(IPT,7,82,87,89,1,1,2,3,IERR) )THEN 
C
C           Found RWYnnA
CX          CALL PRTST(IPT,IEQ,'FOUND RWYnnA     ')
C
C           WS RUNWAY?- Extract first letter into KINT
cps         This should always be 'R' 
            CALL EXTLTR( IPT,1,KINTPT,IRET )
            IPT=IPT+2 
C
C           RWY - Extract runwaydesignator. 
            CALL EXTGRP( IPT,2,0,0,0,0,KINTPT+1,IRET )
C           L,C or R - Extract
cpsjan03            CALL EXTLTR(IPT,1,KINTPT+2)
            CALL EXTLTR(IPT,1,KINTPT+2,IRET)
            KINT(KINTPT+3)=32
C
cpsnov07 New branch: WY should no longer be included
         ELSEIF( OKVAL(IPT,5,82,1,1,2,3,0,0,IERR) )THEN 
C
C           Found RnnA
CX          CALL PRTST(IPT,IEQ,'FOUND RnnA       ')
C
C           WS RUNWAY?- Extract first letter into KINT
cps         This should always be 'R' 
            CALL EXTLTR( IPT,1,KINTPT,IRET )
C
C           RDRDR - Extract runwaydesignator. 
            CALL EXTGRP( IPT,2,0,0,0,0,KINTPT+1,IRET )
C           L,C or R - Extract
cpsjan03            CALL EXTLTR(IPT,1,KINTPT+2)
            CALL EXTLTR(IPT,1,KINTPT+2,IRET)
            KINT(KINTPT+3)=32
C
         ELSEIF( OKVAL(IPT,7,82,87,89,1,1,2,2,IERR) )THEN 
C
C           Found RWYnnAA
CX          CALL PRTST(IPT,IEQ,'FOUND RWYnnA     ')
C
C           WS RUNWAY?- Extract first letter into KINT
cps         This should always be 'R' 
            CALL EXTLTR( IPT,1,KINTPT,IRET )
            IPT=IPT+2 
C
C           RWY - Extract runwaydesignator. 
            CALL EXTGRP( IPT,2,0,0,0,0,KINTPT+1,IRET )
C
C           L,C or R - Extract
cpsjan03            CALL EXTLTR(IPT,2,KINTPT+2)
            CALL EXTLTR(IPT,2,KINTPT+2,IRET)
C
cpsnov07 New branch: WY should no longer be included
         ELSEIF( OKVAL(IPT,5,82,1,1,2,2,0,0,IERR) )THEN 
C
C           Found RnnAA
CX          CALL PRTST(IPT,IEQ,'FOUND RnnA       ')
C
C           WS RUNWAY?- Extract first letter into KINT
cps         This should always be 'R' 
            CALL EXTLTR( IPT,1,KINTPT,IRET )
C
C           RDRDR - Extract runwaydesignator. 
            CALL EXTGRP( IPT,2,0,0,0,0,KINTPT+1,IRET )
C
C           L,C or R - Extract
cpsjan03            CALL EXTLTR(IPT,2,KINTPT+2)
            CALL EXTLTR(IPT,2,KINTPT+2,IRET)
C
         ELSE
C
C           Something wrong with WS RWY-group
C           Find next space
            print*,'ERROR in WS RWY-group'
            CALL MARKERR(IPT,15,IFIRST)
            CALL NXTBLNK(IPT,IRET)
            GO TO 350
C
         ENDIF
C
C
C
CX         print*,'irep,kintpt,kint(158,167):',
CX     C          irep,kintpt,(kint(j),j=158,167)
CX         print*,'kint(159):',kint(159)
         IREP=IREP+1
C        go back and check if more WS-groups.
cpsoct01 should we allow more than 2 WS groups? Obsfile does not permit more...
         IF(IREP.LE.2)GO TO 341
C
      ENDIF  
C
  350 CONTINUE                     
cpsoct01 From Nov 1 2001 two additional groups are possible at end of Metar/Speci
cps      right after WS-group: WTsTs/SS' RDrDr/ErCrererBrBr. The presence of these 
cps      should not cause any problems decoding the other groups, and we choose 
cps      to wait making changes to present code until we know that someone want 
cps      us to decode these new groups.


CX    print*,'KERR=',KERR   
C
      IF (IPT.GE.IEQ) GO TO 3000
C
C     Check if temp and pressure has been decoded, 
C     If KERR = 0, and no temp or pressure, something is wrong
C      IF(KERR.EQ.0)THEN
         IF(.NOT.OKTT)THEN
C                
CX           print*,'Not decoded temp-group'
             CALL MARKERR(IPT,21,IFIRST)
C
         ELSEIF(.NOT.OKPP)THEN
C                
CX           print*,'Not decoded pp-group'
             CALL MARKERR(IPT,22,IFIRST)
C
         ENDIF
C      ENDIF
      GO TO 3500
C     ERROR IF NOT END OF REPORT.

 3000 CONTINUE                     
      IF (IPT.LT.0)THEN
          PRINT*,'IPT LESS THAN ZERO'   
      END IF
      IF (IPT.LT.IEQ)THEN
           CALL MARKERR(IPT,60,IFIRST)
       END IF
C
C***  END OF EXTRACT GROUPS,
C
 3500 CONTINUE                     
C***  TREND??????
C
      CALL NEXTPRT(IPT,IEQ)
CX      CALL PRTST(IPT,IEQ+5,'CHECKING FOR TREND')
C
C     Check for BECMG or TEMPO or NOSIG.
      IF( OKVAL(IPT,6,66,69,67,77,71,3,0,IERR).or.
     C    OKVAL(IPT,6,84,69,77,80,79,3,0,IERR).or.  
     C    OKVAL(IPT,6,78,79,83,73,71,3,0,IERR)  )THEN
C     
C         Found TREND,
C     
CX         print*,'Found TREND'
         CALL TREND(IPT,IEQ)
C
      ENDIF
C     
C

cpsjun06 If ddd=0 and ff>0, dd ought to be reported as 360,
cps      as 0 is reserved for 'variable' wind in BUFR, or used
cps      for calm, where both dd and ff is 0
      if (kint(11).eq.0 .and. kint(12).gt.0 .and. .not. ddVRB) then
         print*,'ddd=0, with ff>0. Changed to 360'
         kint(11) = 360
      end if

C
C***
C*    ERROR CHECKING AND HANDLING.
C***
C
C     RETURN IF NO ERROR IN REPORT.
C
CX    print*,'KERR=',KERR   
C
 4000 CONTINUE
CTEST
CX       DO 901 I=1,153
CX          IF(KINT(I).NE.MINDIC)PRINT*,'KINT-',I,KINT(I)
CX901    CONTINUE     
CTEST
      IF ( KERR.EQ.0 ) RETURN
C
 
C     IF FIRST DECODING ATTEMPT , TRY TO CORRECT ERROR AND DECODE
C     AGAIN.
C
CRR
 4001 continue
CRR
      IF ( IFIRST.EQ.0 ) THEN
         IPT = KEEP
C
CX       print*,'SAINT - call savrep iwpt',iwpt
         CALL SAVREP( IHEAD,IERR )
C
      ENDIF
C
C     Not decoded pp or tt and not at end of report.
      IF( (KERR.GT.20).AND.(IWPT.LT.IEQ))THEN
C
         IFIRST = IFIRST+1
         IF(IFIRST.LT.3)THEN
            IERRPT= IWPT
            CALL FIXSA(IERRPT)
            IPT = KEEP
            GO TO 10
         ENDIF
      ENDIF
C
C     ONLY REPORTS WITH ERROR IN DATE/TIME OR
C     LAT/LONG ARE NOT PROCESSED FURTHER.
      IF (KERR.GT.4) KERR = 0
C
      RETURN
      END
      SUBROUTINE FIXSA(IERRPT)
C
C**** *FIXSA*
C
C
C     PURPOSE.
C     --------
C
C
C         *CALL* *FIXSA*
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
C          Lisbet lovhoiden     *DNMI*    MAY 1995
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
      INCLUDE 'comindx.f'
      CHARACTER*100 LINE
C
C     ------------------------------------------------------------------
C*    Set error-group to blanks.
C
C
CX    print*,'FIXSA-IPT,IEQ,IERRPT',IPT,IEQ,IERRPT
C
      IF(IERRPT.LT.IEQ)THEN
         K=IERRPT
         CALL NXTBLNK(K,IEQ)
         IF(K.EQ.IEQ)K=K-1    
         DO 15 J=IERRPT,K
            KCHAR(J)=32
  15     CONTINUE
C
      ENDIF

C
CX    then
CX       J=1
CX       DO 20 I=IPT,IEQ
CX          LINE(J:J)=CHAR(KCHAR(I))
CX          J=J+1
CX20     CONTINUE
CX       LINE(J:J)=CHAR(KCHAR(IEQ))
CX       print*,'FIXSA:',LINE
CX    ENDIF
C

C*          1.  CLEAR PARITY BITS.??
C                -----------------
 100  CONTINUE
C
      DO 101 I=IPT,IGS
         KCHAR(I) = IAND(KCHAR(I),127)
  101 CONTINUE
C
      ICOR = 0
C
cpsnov06 Had to comment out this call, as 'E's are too common in metar/speci to
cps      have this subroutine work correctly (and isn't the 'E E E' procedure
cps      obsolete by now anyway?)
cps     CALL REMEEE
C
C
CC
      K = IPT
      CALL NEXTVAL (K,32,IEQ)
      K = K - IPT
      IF (K.LE.1) THEN
C--                   WRITE (*,9901) ICOR
C--                   CALL PRTBULL (IPT,IEQ)
                      IPT = K+ IPT
                      CALL NEXTPRT (IPT,IEQ)
                      ICOR = 4
                  END IF
C
C
C
  400 CONTINUE
C
C     RESET POINTER TO END OF REPORT
C
CX    print*,'FIXSA-NEXTEQ,IPT,IEQ',IPT,IEQ
CX    print*,'FIXSA-NEXTEQ,IEQ',KCHAR(IEQ)
      IEQ = IPT
      CALL NEXTEQ (IEQ,IGS)
CX    print*,'FIXSA-NEXTEQ,IPT,IEQ',IPT,IEQ
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
      SUBROUTINE IC2875(IDD,MINDIC,IDIR)
C
C****
C*
C*    NAME     :  IC2875
C*
C*    FUNCTION :  DECODE WIND DIRECTION
C*
C*    INPUT    :  IDD     - WIND DIRECTION IN DEGREES
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  IDIR    - DECODED WIND DIRECTION
cps   =kdec, which simply equals idd=kint: metar uses ddd, 
cps   not dd as synop, temp, dribu etc do for wind direction.
cps   So this subroutine does mostly nothing! 
C*
C*             IDIR IS SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN IDD
C*
C****
C
C***   SET MISSING VALUE
C
      IDIR=MINDIC
C
C
C
C***   CHECK MISSING DATA INDICATOR
C
      IF( IDD .EQ. MINDIC ) RETURN
      IF( IDD .LT. 0 ) RETURN
cpsjun06
      if( idd .gt.360 ) return
      IDIR=IDD
C
C
C     CHECK IF DD=99 .I.E. DIRECTION INDETERMINATE
C
C
C      IF(IDIR .GT. 36 .AND. IDIR .LT. 50) RETURN
C      IF(IDIR .GT. 86 .AND. IDIR .NE. 99) RETURN
C
C
100   CONTINUE
C
C
C      IF(IDIR .GT. 50 .AND. IDIR .NE. 99) IDIR=IDIR-50
C      IF(IDIR .EQ. 99) IDIR=62
C
C      IDIR=IDIR*10
C
      RETURN
      END
      SUBROUTINE IC11DD(IPT,ITYPE,IRET)
C
C****
C*
C*    NAME     :  IC11DD
C*
C*    FUNCTION :  CHECK IF LEGAL DATE.
C*
C*    INPUT    :  IPT     - POINTER TO KCHAR. 
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  IRET    - 0 IF CORRECT DATE IN KCHAR
C*
C*
C****
      IMPLICIT LOGICAL(O)
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C****
C
C
      IRET=0
      I=IPT
C
C
CX    print*,'IC11DD,IPT,ITYPE',IPT,ITYPE
      GO TO (10,20,30)ITYPE
C
      IRET=8
      RETURN
C
 10   CONTINUE
C
C     Check if dd is in range 1 to 31
C     This means first box be in range 0-3
      IF((KCHAR(I).GE.48).AND.(KCHAR(I).LE.51))THEN
C
C        First ipt ok.
         I=I+1
C        Next ipt in range 0-9
         IF(OKINT(KCHAR(I),IERR))THEN
C           Second ipt ok.
            I=I+1
         ELSE
            IRET=1
         ENDIF
      ELSE
         IRET=1
      ENDIF
C
 20   CONTINUE
C
C     Check if hh is in range 0 to 23
C     This means first box be in range 0-2
      IF((KCHAR(I).GE.48).AND.(KCHAR(I).LE.50))THEN
C
C        First ipt ok.
         I=I+1
C        Next ipt in range 0-9
         IF(OKINT(KCHAR(I),IERR))THEN
C           Second ipt ok.
            I=I+1
         ELSE
            IRET=IRET+2
         ENDIF
      ELSE
         IRET=IRET+2
      ENDIF
C
 30   CONTINUE
C
C     Check if mm is in range 0 to 59
C     This means first box be in range 0-5
      IF((KCHAR(I).GE.48).AND.(KCHAR(I).LE.53))THEN
C
C        First ipt ok.
         I=I+1
C        Next ipt in range 0-9
         IF(OKINT(KCHAR(I),IERR))THEN
C           Second ipt ok.
            I=I+1
         ELSE
            IRET=IRET+4
         ENDIF
      ELSE
         IRET=IRET+4
      ENDIF
C
C
C
      RETURN
      END
      SUBROUTINE ICRWVV(KINTPT,KDECPT)
C
C****
C*
C*    NAME     :  ICRWVV
C*
C*    FUNCTION :  DECODE RUNWAY VISIBILITY
C*
C*    INPUT    :  KINTPT  - POINTER TO STARTBOX IN KINT
C*    OUTPUT   :  KDECPT  - POINTER TO STARTBOX IN KDEC
C*
C*
C*
C*
C*   020194 0004 0000 01 MISSING VALUE
C*               0001 01 U
C*               0002 01 N 
C*               0003 01 D 
C*   008014 0004 0000 01 NORMAL VALUE
C*               0001 01 M 
C*               0002 01 P 
C*                  .      
C*                  .      
C*                  .      
C*               0015 01 MISSING VALUE
C****
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
C     Set some flags and pointers.
      I=KINTPT
      J=KDECPT
C
C***   SET MISSING VALUE
C
      DO 10 K=J,J+5
         KDEC(K)=MINDIC
 10   CONTINUE
C 
C***   Check missing data indicator (first box in KINT)
C*R
CX    print*,'KINT(',I,')',KINT(I)
      IF( KINT(I) .EQ. MINDIC ) THEN
CX        print*,'Flag for no RWVV in KINT(',I,')'
          RETURN
      ENDIF
C
      IF( KINT(I) .NE.82 ) THEN
CX        print*,'Not  R in KINT(',I,')'
          RETURN
      ELSE
          I=I+1
      ENDIF
C
C*Runwaydirection
C*DR
C
C     Integer
C     J=KDECPT
cmar97KDEC(J)=KINT(I)*10
      KDEC(J)=KINT(I)
      I=I+1
      J=J+1
C
C*Parallell runway
C*first letter  (any of L,C,R)
C*DRA
C     J=KDECPT+1
C
      I=I+2
      J=J+1
C     
C*/
      IF(KINT(I).NE.47)RETURN
CX    print*,'Read /'
       I=I+1
C
C*P or M?  (A denotes accurate reading)
C     J=KDECPT+2
      IF(KINT(I+1).EQ.MINDIC)THEN
CX        print*,'Runway VVVV-Missing'
         KDEC(J)=MINDIC
      ELSEIF(KINT(I).EQ.65)THEN
CX        print*,'Runway /A'
          KDEC(J)=0
      ELSEIF(KINT(I).EQ.80)THEN
CX        print*,'Runway /P'
          KDEC(J)=1
      ELSEIF(KINT(I).EQ.77)THEN
CX        print*,'Runway /M'
           KDEC(J)=2
      ELSE
         KDEC(J)=15
      ENDIF
      I=I+1
      J=J+1
C
C*RVRVRVRV
C     J=KDECPT+3
      KDEC(J)=KINT(I)
CX    print*,'Runway 1. VVVV-',KDEC(J)
      I=I+1
      J=J+1
C
C
C*P or M?  (V denotes accurate reading)
C     J=KDECPT+4
      IF(KINT(I+1).EQ.MINDIC)THEN
CX        print*,'Runway 2. VVVV-Missing'
      ELSEIF(KINT(I).EQ.86)THEN
CX        print*,'Runway V'
         KDEC(J)=0
      ELSEIF(KINT(I).EQ.80)THEN
CX        print*,'Runway P'
          KDEC(J)=1
      ELSEIF(KINT(I).EQ.77)THEN
CX        print*,'Runway M'
          KDEC(J)=2
      ELSE
         KDEC(J)=15
      ENDIF
      I=I+1
      J=J+1

C*RVRVRVRV
C     J=KDECPT+5
      KDEC(J)=KINT(I)
CX    print*,'Runway 2. VVVV-',KDEC(J)
      J=J+1
      I=I+1
C
 20   CONTINUE
C*Ir (U,N,D)
C     J=KDECPT+6
CX    print*,'U,N,D-',KINT(I)
      IF(KINT(I).EQ.85)THEN
CX        print*,'Runway U'
         KDEC(J)=0
      ELSEIF(KINT(I).EQ.68)THEN
CX        print*,'Runway D'
           KDEC(J)=1
      ELSEIF(KINT(I).EQ.78)THEN
CX        print*,'Runway N'
          KDEC(J)=2
      ELSE
         KDEC(J)=3
      ENDIF
C
      RETURN
      END
      SUBROUTINE IC1875(IFF,IWW,MINDIC,ISPEED)
C
C****
C*
C*    NAME     :  IC1875
C*
cpsC*    FUNCTION :  DECODE WIND DIRECTION AND SPEED
C*    FUNCTION :  DECODE WIND SPEED
C*
C*    INPUT    :  IFF     - WIND SPEED
C*             :  IWW     - WIND SPEED INDICATOR
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  ISPEED  - DECODED WIND SPEED
C*
C*             ISPEED IS SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN IFF OR IWW
C*
C****
C
C***   SET MISSING VALUE
C
      ISPEED=MINDIC
C
C
C
C***   CHECK MISSING DATA INDICATOR
C
      IF(IWW .EQ. MINDIC .OR. IFF .EQ. MINDIC) RETURN
C
C     CHECK IF WIND INDICATOR IR IS CORRECT
C
      IF(IWW .NE. 0 .AND. IWW .NE. 1 .AND. IWW .NE. 2) RETURN
C
C
C     IW IS THE MODIFIED WIND SPEED INDICATOR TO MAKE
C     IF-STATEMENTS SHORTER
C        IWW=0 FOR METER/SEC
C        IWW=1 FOR KNOTS
C        IWW=2 FOR KM/HOUR
C
      ISPEED=IFF
C
C
100   CONTINUE
C
C     IF SPEED IN KNOTS MODIFY TO M/S
C
      IF(IWW .EQ. 1)THEN
C
         ISPEED=ISPEED*10
         CALL KTOMPSI(ISPEED)
C        
      ELSEIF(IWW .EQ. 2) THEN
C
C        KM/HOUR TO M/HOUR:
cpsaug06         SPEED=ISPEED*10000
         ISPEED=ISPEED*10000
C
C        M/HOUR TO M/SEC:
cpsaug06         SPEED=ISPEED/36000
         ISPEED=ISPEED/36000
C
      ENDIF
C
C
C
      RETURN
      END
      SUBROUTINE PRTST(IPT,IEQ,ATEXT)
      CHARACTER*20 ATEXT
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
      print*,ATEXT
      print*,'IPT=',IPT
      print*,'IEQ=',IEQ
      PRINT*,'KCHAR:',CHAR(KCHAR(IPT)),CHAR(KCHAR(IPT+1)),
     C    CHAR(KCHAR(IPT+2)),CHAR(KCHAR(IPT+3)),CHAR(KCHAR(IPT+4))
      RETURN
      END
      SUBROUTINE EXTLTR(IPT,ILTR,KINTPT,IRET)
CLiL
C****
C*
C*    NAME     :  EXTLTR
C*
C*    FUNCTION :  EXTRACT ILTR LETTERS from CONSECUTIVE BOXES IN KCHAR
C*                STARTING AT IPT
C*                INTO CONSECUTIVE BOXES IN KINT STARTING AT KINTPT   
C*
C*    INPUT    :  IPT     - POINTER TO KCHAR ARRAY
C*             :  ILTR    - NUMBER OF LETTERS
C*             :  KINTPT - POINTER TO KINT-ARRAY.
C*
C*    OUTPUT   :  IRET    - RETURNCODE
C*
C*             IRET IS SET TO NOT ZERO IF
C*             ANY ERRORS FOUND IN  IPT OR KINTPT OR KCHAR
C*
C* 
C*    AUTHOR   :  LISBET LOVHOIDEN DNMI
C*
C****
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
      IRET=0
      IF(ILTR.LT.1)THEN
         IRET=1
         RETURN
      ENDIF
C
      DO 10 I=1,ILTR
         J=KINTPT+I-1
         KINT(J)=KCHAR(IPT)
         IPT=IPT+1
 10   CONTINUE
C    
      RETURN
      END
      SUBROUTINE ICCLCVR(ICOV1,ICOV2,ICOV3,MINDIC,ICOVER)
CLiL
C****
C*
C*    NAME     :  ICCLCVR
C*
C*    FUNCTION :  DECODE CLOUDCOVER FROM 3 letters to amount in integer.
cps               corresponding to BUFR table 020 012
C*
C*    INPUT    :  ICOV1   - FIRST LETTER OF CLOUDCOVER
C*             :  ICOV2   - SECOND LETTER OF CLOUDCOVER
C*             :  ICOV3   - THIRD LETTER OF CLOUDCOVER
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  ICOVER  - INTEGER DENOTING CLOUDCOVER
C*
C*             OCOVER IS SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN  ICOV1-3
C*
C* 
C*    AUTHOR   :  LISBET LOVHOIDEN DNMI
C*
C****
C
C
C     CHECK IF SKC/NSC/NCD/FEW/SCT/BKN/OVC
cps Actually, SKC and NSC ought to be decoded to 020 009 in Bufr, not
cps to 020011 as done here. But where to put NCD?
cpsnov07 SKC no longer allowed (but we keep checking for it)
      IF((ICOV1.EQ.83).AND.(ICOV2.EQ.75).AND.(ICOV3.EQ.67))THEN
C
C         SKC
CX        print*,'SKC'
          ICOVER=0
C
      ELSEIF((ICOV1.EQ.78).AND.(ICOV2.EQ.83).AND.(ICOV3.EQ.67))THEN
C
C         NSC
CX        print*,'NSC'
          ICOVER=1
C
cpsnov05 Added NCD, even if it is not at all clear that this should be coded 
c        as 15 = 'Cloud cover is indiscernable...') in Bufr table 020 011.
c        Actually 15 is the same as 'missing', so in Bufr we cannot
c        redetect NCD.
      ELSEIF((ICOV1.EQ.78).AND.(ICOV2.EQ.67).AND.(ICOV3.EQ.68))THEN
C
C         NCD
CX        print*,'NCD'
          ICOVER=15
C
      ELSEIF((ICOV1.EQ.70).AND.(ICOV2.EQ.69).AND.(ICOV3.EQ.87))THEN
C
C         FEW
CX        print*,'FEW'
          ICOVER=13
C
      ELSEIF((ICOV1.EQ.83).AND.(ICOV2.EQ.67).AND.(ICOV3.EQ.84))THEN
C
C         SCT
CX        print*,'SCT'
          ICOVER=11
C
      ELSEIF((ICOV1.EQ.66).AND.(ICOV2.EQ.75).AND.(ICOV3.EQ.78))THEN
C
C         BKN
CX        print*,'BKN'
          ICOVER=12
C
      ELSEIF((ICOV1.EQ.79).AND.(ICOV2.EQ.86).AND.(ICOV3.EQ.67))THEN
C
C         OVC
CX        print*,'OVC'
          ICOVER=8
C
      ELSE
C
C         NO LEGAL VALUE
          ICOVER=MINDIC
C
      ENDIF
CX        print*,'ICOV1,ICOV2,ICOV3',icov1,icov2,icov3
CX        print*,'ICOVER',icover
C
C    
      RETURN
      END
      SUBROUTINE ICCLTYP(ITYP,MINDIC,ICTYPE)
CLiL
C****
C*
C*    NAME     :  ICCLTYP
C*
C*    FUNCTION :  DECODE CLOUDTYPE FROM 3 letters to type in integer.
C*
C*    INPUT    :  ITYP   - FIRST LETTER OF CLOUDTYPE
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  ICTYPE  - INTEGER DENOTING CLOUDTYPE
C*
C*             ICTYPE IS SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN  ICOV1-3
C*
C* 
C*    AUTHOR   :  LISBET LOVHOIDEN DNMI
C*
C****
C
C
C     CHECK IF TCU/CB
      IF(ITYP.EQ.84)THEN
C
C         TCU
CX        print*,'TCU'
          ICTYPE=3
C
      ELSEIF(ITYP.EQ.67)THEN
C
C         CB 
CX        print*,'CB'
          ICTYPE=9
C
      ELSE
C
C         NO LEGAL VALUE
          ICTYPE=MINDIC
C
      ENDIF
CX        print*,'ITYP',ityp
CX        print*,'ICTYPE',ictype
C
C    
      RETURN
      END
      SUBROUTINE ICCLHHH(IHHH,MINDIC,OHHH)
CLiL
C****
C*
C*    NAME     :  ICCLHHH
C*
C*    FUNCTION :  DECODE hight of base of cloud from feet to meters.
C*
C*    INPUT    :  IHHH    - HIGHT IN FEET
C*             :  MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OHHH    - HIGHT IM METRES.              
C*
C*             OHHH   IS SET TO MISSING VALUE IF
C*             ANY ERRORS FOUND IN  IHHH   
C*
C* 
C*    AUTHOR   :  LISBET LOVHOIDEN DNMI
C*
C****
      INTEGER  OHHH
C
      OHHH=MINDIC
      IF(IHHH.EQ.MINDIC)RETURN
      OHHH=IHHH*30
C
C 
C    
      RETURN
      END
      SUBROUTINE IC3876(INTEMP,ISIGN,MINDIC,OUTTEMP)
C****
C*
C*    NAME     : IC3876
C*
C*    FUNCTION :  DECODE THE TEMPERATURE FROM METAR
C*
C*    INPUT    :  INTEMP   TEMPERATURE VALUE
C*                ISIGN    SIGN INDICATOR FOR TEMPERATURE
C*                         77 = NEGATIVE, MINDIC = NOT NEGATIVE
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTTEMP  OUTPUT TEMPERATURE*10 WITH CORRECT SIGN
C*
C*             OUTTEMP IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INTEMP
C*

C****
C
      INTEGER OUTTEMP
C
C***   SET MISSING VALUE
C
      OUTTEMP=MINDIC
C
C
C     TEST FOR MISSING DATA AND VALIDITY OF SIGN
C
      IF(INTEMP .EQ. MINDIC) RETURN
      OUTTEMP=INTEMP*10
cps   Later (when put into VALUES-array) outtemp is multiplied by 0.1
cps   Does this to distinguish betwen 00 and M00: this last value is 
cps   stored in VALUES as -0.1 (see below)
      IF(ISIGN .EQ. 77)THEN
C
C        ISIGN=77 ('M')
C 
         IF(OUTTEMP.EQ.0)THEN
            OUTTEMP = OUTTEMP - 1
         ELSE 
            OUTTEMP = OUTTEMP*(-1)
         ENDIF
      ENDIF
C
C
      RETURN
      END
      SUBROUTINE ICPRESS(INPP,IUNIT,MINDIC,OUTPP)
C****
C*
C*    NAME     : ICPRESS
C*
C*    FUNCTION :  DECODE THE PRESSURE FROM METAR
C*
C*    INPUT    :  INPP     PRESSURE VALUE
C*                IUNIT    UNIT INDICATOR FOR PRESSURE
C*                         Q = hPA, A = INCH- MERCURY
cpsnov07 A no longer allowed (but we keep checking for it)
C*                MINDIC   MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTPP   OUTPUT PRESURE IN HPA
C*
C*             OUTPP IS SET TO MISSING VALUE
C*             IF ANY ERRORS FOUND IN INPP OR IUNIT
C*

C****
C
      INTEGER OUTPP
C
C***   SET MISSING VALUE
C
      OUTPP=MINDIC
C
      IF(INPP .EQ. MINDIC) RETURN
      IF(IUNIT .EQ. MINDIC) RETURN
C
      IF(IUNIT.EQ.81)THEN
C
C        IUNIT='Q' , unit is hPa
         OUTPP=INPP
C
      ELSEIF(IUNIT.EQ.65)THEN
C
C        pppp=pp.pp (inchHG)
         OUTPP=INPP
C        inchHg to mmHg
         OUTPP=OUTPP*254
C        mmHg to hPa
         OUTPP=OUTPP/760
         OUTPP=OUTPP*1013
         OUTPP=OUTPP/1000
      ENDIF
C
      RETURN
      END
      LOGICAL FUNCTION OKLTR(IASC,IRET)
C
C     RETURNS .TRUE. IF ASCIIVALUE OF IASC IS IN LETTER-RANCH.
      OKLTR=( (IASC.GT.64) .AND. (IASC.LT.91) )
      RETURN
      END
      LOGICAL FUNCTION OKINT(IASC,IRET)
C
C     RETURNS .TRUE. IF ASCIIVALUE OF IASC IS IN INTEGER-RANCH.
      OKINT=( (IASC.GT.47) .AND. (IASC.LT.58) )
      RETURN
      END
      LOGICAL FUNCTION OKSEP(IASC,IRET)
C
C     RETURNS .TRUE. IF ASCIIVALUE OF IASC IS ONE OF THE SEPARATORS
C     (SPC,CR,LF,EQUALS)
      OKSEP=( (IASC.EQ.32) .OR. (IASC.EQ.10) .OR. (IASC.EQ.13)
     C        .OR. (IASC.EQ.61) )
      RETURN
      END
      LOGICAL FUNCTION OKVAL(IPT,IN,IA1,IA2,IA3,IA4,IA5,IA6,IA7,IRET)
      IMPLICIT LOGICAL(O)
      DIMENSION IA(7)
      LOGICAL OK(7)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
C     RETURNS .TRUE. IF ASCIIVALUE OF KCHAR(IPT) - (IPT+IN)
C     IS THE SAME AS THE CONTENTS OF IA1 TO IA7.
C
C     IF IA = 1 RETURNS TRUE IF KCHAR(IPT) IS INTEGER
C     IF IA = 2 RETURNS TRUE IF KCHAR(IPT) IS A - Z
C     IF IA = 3 RETURNS TRUE IF KCHAR(IPT) IS A SEPARATOR
cps   IF IA = 4 RETURNS TRUE IF KCHAR(IPT) IS A NON-SEPARATOR
cps   ELSE RETURNS TRUE IF KCHAR(IPT) EQUALS IA (ASCII-VERDI)
C
      IA(1)=IA1
      IA(2)=IA2
      IA(3)=IA3
      IA(4)=IA4
      IA(5)=IA5
      IA(6)=IA6
      IA(7)=IA7
C
      DO 1 I=1,7
         OK(I)=.FALSE.
C
 1    CONTINUE
C
      DO 10 I=1,IN
         KI=IPT-1+I
         OK(I)=.FALSE.
CX       print*,'OKVAL-',KI,I,IA(I)
         IF(IA(I).EQ.1)THEN
C
CX          print*,'OKVAL- testing for integer',KCHAR(KI)
            IF(OKINT(KCHAR(KI),IRET) ) OK(I)=.TRUE.
C
         ELSEIF(IA(I).EQ.2)THEN
C
CX          print*,'OKVAL- testing for letter',KCHAR(KI)
            IF(OKLTR(KCHAR(KI),IRET) ) OK(I)=.TRUE.
C
         ELSEIF(IA(I).EQ.3)THEN
C
CX          print*,'OKVAL- testing for separator',KCHAR(KI)
            IF(OKSEP(KCHAR(KI),IRET) ) OK(I)=.TRUE.
C
         ELSEIF(IA(I).EQ.4)THEN
C
CX          print*,'OKVAL- testing for nonseparator',KCHAR(KI)
            IF(.NOT.OKSEP(KCHAR(KI),IRET) ) OK(I)=.TRUE.
C
         ELSE
C
CX          print*,'OKVAL- testing for asciivalue',KCHAR(KI)
            IF(KCHAR(KI).EQ.IA(I)) OK(I)=.TRUE.
         ENDIF
C
         IF(.NOT.OK(I))GO TO 15
C
 10   CONTINUE
C
CX    print*,'OKVAL- Returns true'
      OKVAL=.TRUE.
      RETURN
C
 15   CONTINUE
C
CX    print*,'OKVAL- Returns false'
      OKVAL=.FALSE.
      RETURN
      END
      SUBROUTINE MARKERR(IPT,IERRN,IFIRST)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comstat.f'
C
      KERR=IERRN
      IF (IFIRST.EQ.0) NOER(1,IERRN) = NOER(1,IERRN) + 1
      KCHAR(IPT) = IOR(KCHAR(IPT),128)
      RETURN
      END
      SUBROUTINE NXTBLNK(IPT1,IRET)
C
C     Scans bulletin for next separator (SPC,CR,LF,=)
C
cpsnov06 Modified to prevent KCHAR from going out of bounds
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'   ! kchar
      include 'comindx.f'   ! igs
C
      DO WHILE( (KCHAR(IPT1).NE.32).AND.(KCHAR(IPT1).NE.13)
     1     .AND.(KCHAR(IPT1).NE.10).AND.(KCHAR(IPT1).NE.61)
     2     .AND. IPT1.LT.IGS )
         IPT1=IPT1+1
      ENDDO
C
      RETURN
      END
      SUBROUTINE IC4367(INVIS,INDIC,MINDIC,OUTVIS)
C
C****
C*
C*    NAME     :  IC4367
C*
C*    FUNCTION :  DECODE HORIZONTAL VISIBILITY AT SURFACE VV
C*
C*    INPUT    :  INVIS   -VISIBILITY CODE FIGURE
C*                INDIC  - Flag for meters/cavok/miles
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTVIS  -DECODED VISIBILITY IN METRES
C*
C*             OUTVIS IS SET TO MISSING DATA VALUE
C*             IF ANY ERRORS FOUND IN INVIS,indic
C*
C****
C
C
C
      INTEGER OUTVIS
C
C
C***   SET OUTVIS TO MISSING VALUE
C
      OUTVIS=MINDIC
C
      IF(INVIS .EQ. MINDIC) RETURN
      OUTVIS=INVIS
C
C     IF(INDIC.EQ.2)THEN
C 
C        visibility in unit miles.(1609.344m)
C        OUTVIS=INVIS*1609
C        round to nearest 1000 metres
C        KK=OUTVIS/1000
C        KK=KK*1000
C        IF((OUTVIS-KK).GE.500)KK=KK+1000
C        OUTVIS=KK
C        RETURN
C     ENDIF
C
      RETURN
      END
      SUBROUTINE IC43DD(INDD1,INDD2,MINDIC,OUTDD)
C
C****
C*
C*    NAME     :  IC43DD
C*
C*    FUNCTION :  DECODE DIRECTION OF HORIZONTAL VISIBILITY AT SURFACE VV
C*
C*    INPUT    :  INDD1   -VISIBILITY CODE FIGURE 1
C*                INDD2  - VISIBILITY CODE FIGURE 2
C*                MINDIC  - MISSING DATA VALUE
C*
C*    OUTPUT   :  OUTDD   -DECODED DIRECTION IN DEGREE TRUE 05021
C*
C*             OUTDD  IS SET TO MISSING DATA VALUE
C*             IF ANY ERRORS FOUND IN INDD1,indd2
C*
C
C
C
      INTEGER OUTDD
C
C
C***   SET OUTDD  TO MISSING VALUE
C
      OUTDD=MINDIC
C
      IF(INDD1 .EQ. MINDIC) RETURN
C
C*    first letter INDD1 (any of N,E,S,W)
      IF(INDD1.EQ.78)THEN
C
C         NW, N, NE
CX        print*,'IC43DD,IDD=N?'
C
          IF(INDD2.EQ.87)THEN
CX           print*,'IC43DD,IDD=NW'
             OUTDD=315
          ELSEIF(INDD2.EQ.69)THEN
CX           print*,'IC43DD,IDD=NE'
               OUTDD=45
          ELSE
CX           print*,'IC43DD,IDD=N'
              OUTDD=0
          ENDIF
C
      ELSEIF(INDD1.EQ.69)THEN
C
C         E
C
CX        print*,'IC43DD,IDD=E'
          OUTDD=90
C
      ELSEIF(INDD1.EQ.83)THEN
C
C         SE, S, SW
CX        print*,'IC43DD,IDD=S?'
C
          IF(INDD2.EQ.69)THEN
CX           print*,'IC43DD,IDD=SE'
             OUTDD=135
          ELSEIF(INDD2.EQ.87)THEN
CX           print*,'IC43DD,IDD=SW'
               OUTDD=225
          ELSE
CX           print*,'IC43DD,IDD=S'
              OUTDD=180
          ENDIF
C
C
      ELSEIF(INDD1.EQ.87)THEN
C
C         W
C
CX        print*,'IC43DD,IDD=W'
          OUTDD=270
C
      ELSE
C
C         Set missing value.
CX        print*,'IC43DD,IDD=',CHAR(INDD1),CHAR(INDD2)
          OUTDD=MINDIC
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE METASTA(IC1,IC2,IC3,IC4,MINDIC,
     C     NR1,NR2,LAT,LON,MOH,KERR,IERR)
C
C**** *METASTA*
C
C
C     PURPOSE.
C     --------
C         READ IN meta.station.dat and find wmo-station number
C         ( WMO VOLUMEN A - LIST OF OBSERVING STATIONS)
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *METASTATION(IC1,IC2,IC3,IC4,MINDIC,NR1,NR2,LAT,LON,MOH,IERR)*
cps               NR1=sone nr. NR2=WMO station nr.   
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
C          L. LOVHOIDEN     *DNMI*       JUNE 1995
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
Cps                                       ?
C:    FORMAT:CCCC STATION NAME     NNnnn nnn lati longi moh
C:           A4,1X,A19,I2,I3,1X,I3,3X,I4,3X,I4,3X,I4
cps NN (=XNUM1) = WMO country (region) number (2 digits)
cps nnn (=XNUM2) = WMO station number (3 digits)
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'combuff.f'
CNH common for directories
      INCLUDE 'comdir.f'
C
      CHARACTER CCCC*4,XCCC*4,XNAME*19
      INTEGER   XNUM1,XNUM2,XNUM3,XLAT,XLONG,XMOH
C     ------------------------------------------------------------------
C*          1.   READ IN STATION LIST.
C                ---------------------
 10   CONTINUE
C
      IF(IC1.EQ.MINDIC)RETURN
      IF(IC2.EQ.MINDIC)RETURN
      IF(IC3.EQ.MINDIC)RETURN
      IF(IC4.EQ.MINDIC)RETURN
      IERR=0
      NDX=0
      CCCC=CHAR(IC1)//CHAR(IC2)//CHAR(IC3)//CHAR(IC4)
      NR1=MINDIC 
      NR2=MINDIC
      LAT=MINDIC 
      LON=MINDIC
C
C  - Find end of character string. 
c
CX    print*,'open meta.station.dat'
      OPEN(UNIT=4,IOSTAT=IOS,ERR=300, 
C    1            FILE=CSTREAM(MSYS)//'.station.dat',
     1            FILE='ppdat/meta.station.dat',
     2     FORM='FORMATTED',
cpsjun11     4     READONLY,
     4     ACTION='READ',
     5     STATUS='OLD'
     7     )
C
C
      XCCC='XXXX' 
      DO WHILE (XCCC.NE.CCCC)
         READ(4,40,IOSTAT=IOS,ERR=400,END=200) 
     C        XCCC,XNAME,XNUM1,XNUM2,XNUM3,XLAT,XLONG,XMOH
cpsjan08 40      FORMAT(A4,1X,A19,I2,I3,1X,I3,3X,I4,3X,I4,3X,I4)
cps need one more 'digit' for lat/long in case negative value
 40      FORMAT(A4,1X,A19,I2,I3,1X,I3,2X,I5,2X,I5,3X,I4)
cps      xnum3 is read in but never used (is almost always equal to xnum2 anyway,
cps      what is this?)
C
         IF(IOS.NE.0)GO TO 400
CX       print*,XCCC,XNUM1,XNUM2,XNUM3,XLAT,XLONG,XMOH
C 
      END DO
C
CX    print*,'FOUND METASTATION:',XCCC,XNUM1,XNUM2
      NR1=XNUM1 
      NR2=XNUM2
      LAT=XLAT
      LON=XLONG
      MOH=XMOH
C
      CLOSE(4)
      RETURN
C
 200  CONTINUE
      CLOSE(4)
      KERR=99
      RETURN

C
C
 300  CONTINUE
C
      WRITE(*,9901) IOS
 9901 FORMAT(1H ,' ERROR OPENING METASTATION FILE , ERROR=',I5)
C
      IERR=1
      RETURN
C
 400  CONTINUE
C
      WRITE(*,9902) IOS
 9902 FORMAT(1H ,' ERROR READING METASTATION FILE , ERROR=',I5)
C
      IERR=1
C
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
c     CHARACTER CUNIT*24,CVAL*8
C
      CHARACTER CTABBEN*64,CTABBU*24
C
      CHARACTER*8 YDATE
      CHARACTER*5 YTIME,YFAMILY,YTASK
CTEST
      CHARACTER*24 CORINT
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
      INCLUDE 'comstd.f'
CNH common for directories
      INCLUDE 'comdir.f'
C
C     ------------------------------------------------------------------
C           1.   INITIALISE MISSING DATA INDICATOR.
C                ----------------------------------
 100  CONTINUE
C
      DATA MINDIC / O'17777777777' /
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
CTEST
      DATA IOPTS / 676*O'17777777777',O'0',O'2' /
CRR - For writing error report on screen
CRR   DATA IOPTS / 676*'17777777777'O,'2'O,'2'O /
CTEST
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
     1                            O'1',                     O'3',
     2                            O'7',                    O'17',
     3                           O'37',                    O'77',
     4                          O'177',                   O'377',
     5                          O'777',                  O'1777',
     6                         O'3777',                  O'7777',
     7                        O'17777',                 O'37777',
     8                        O'77777',                O'177777',
     9                       O'377777',                O'777777',
     A                      O'1777777',               O'3777777',
     1                      O'7777777',              O'17777777',
     2                     O'37777777',              O'77777777',
     3                    O'177777777',             O'377777777',
     4                    O'777777777',            O'1777777777',
     5                   O'3777777777',            O'7777777777',
cpsjun11     6                   '17777777777'O,            '37777777777'O  /
     6                   O'17777777777'  /
C
C     --------------------------------------------------------------------
C*         7A.   SET UP DIRECTORY NAMES FOR FILES.
C                ---------------------------------
 710  CONTINUE
C
CRR Moved to comdir.h
C
C     ------------------------------------------------------------------
C*          8.   SET UP BUFR TABLE B.
C                --------------------
 800  CONTINUE
C
      CALL SETTAB( IERR )
      IF(IERR.NE.0) return
C     --------------------------------------------------------------------
C*          9.   SET UP WMO STATION LIST AND IMPORTANT STATIONS.
C                -----------------------------------------------
 900  CONTINUE
C
C      CALL STATION(IERR)
C      IF(IERR.NE.0) return
C
C     --------------------------------------------------------------------
C*          10.   SET UP HEIGHT AND PRESSURE OF STANDARD ATMOSPHERE.
C                ---------------------------------------------------
1000  CONTINUE
C
      ispc=char(32)
      ndx=0
C  - Find end of character string. Open MSYSn file.
      ndx=index(yppdat(1:80),ispc) - 1
c
      OPEN(UNIT=11,ERR=1005,FILE=YPPDAT(1:ndx)//'std_atm.dat',
     1     STATUS='OLD')
C
      DO 1001 I=1,512
      READ(11,FMT=7777,ERR=1010,END=1100,IOSTAT=IOS)  SH(I),P(I)
 7777 FORMAT(1X,F5.0,2X,F8.2)
 1001 CONTINUE
C
      CLOSE(11)
C
      GO TO 1100
C
 1005 CONTINUE
C
      print*,' Open error ',ios,' on file standard_atmosphere.dat'
      ierr=1
      return
C
 1010 CONTINUE
C
      print*,' Read error ',ios,' on file standard_atmosphere.dat'
      ierr=1
      return
C
C
C*            11. GET SUPERVISOR DATE,TIME,FAMILY AND TASK.
C                 -----------------------------------------
 1100 CONTINUE
C
CTEST
      print*,' INITVAR - Read of supervisor date, etc - commented out '
CTEST
CNH   READ(*,'(A8,A5,A5,A5)') YDATE,YTIME,YFAMILY,YTASK
CTEST added CORINT-character*24
      CALL FDATE(CORINT)
      print*,' Current time = ',CORINT
CTEST
C
      RETURN
      END
      SUBROUTINE METEXP1(IERR )
C
C**** *METEXP1*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR METAR DATA.
C         BASIC REPORT. 
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *METEXP1(IERR)*
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
C          Lisbet Lovhoiden        *DNMI*      May 1995
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
      CHARACTER*9  CVAL
      CHARACTER*24 CUNIT
C
C
      INCLUDE 'comwtc.f'
      INCLUDE 'comkey.f'
C
      CHARACTER*8 CIDENT
      INCLUDE 'comkeyc.f'
C
C     ------------------------------------------------------------------
C*          1.   INCREASE COUNTER OF SUBSETS BY ONE.
C                -----------------------------------
 100  CONTINUE
C
cx       PRINT*,' Start METEXP1 , KINT(159) ',KINT(159)
      IF(IERR.NE.0) RETURN
C
      NSUB=0
      NSUB=NSUB+1
      N   =NSUB
C
      DO 101 I=1,900
         DO 102 J=1,2
            VALUES(I,J)=999999.
102      CONTINUE
101   CONTINUE 
C
C*          1.1  GET CURRENT DATE (YEAR AND MONTH).
C                ----------------------------------
 110  CONTINUE
C
      CALL DATUM(KDEC(1),IMONTH,IYEAR)
C
C     ------------------------------------------------------------------
C*          2.   METAR - LAND BASED STATION.
C                --------------------------
 200  CONTINUE
C
C     -----------------------------------------------------------------
 210  CONTINUE
      IVP=0
C
C1-3
CStation identifier. 001063
      IVP=IVP+1
      VALUES(IVP,NSUB)=IVP*1000*NSUB+8
      CVAL(IVP)= CHAR(KINT(05))//CHAR(KINT(06))//
     1               CHAR(KINT(07))//CHAR(KINT(08))
     2               //'    ' 
C 
CWMO-Block-number.001001
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(16).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(16))
CWMO-Station-number. 001002
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(17).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(17))
C
C4-8
CYear.004001
      IVP=IVP+1
cpsapr98      VALUES(IVP,NSUB)=IYEAR+1900.
      VALUES(IVP,NSUB)=FLOAT(IYEAR)
CMonth.004002
      IVP=IVP+1
      VALUES(IVP,NSUB)=FLOAT(IMONTH)
CDay.004003
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(1).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(1))
CHour.004004
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(2).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(2))
CMinute.004005
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(3).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(3))
C
C9-11
CLatitude.005001
      IVP=IVP+1
      VALUES(IVP,NSUB)=0.
      IF(KDEC(5).NE.MINDIC) VALUES(IVP,NSUB)=KDEC(5)/100.
CLongitude.006001
      IVP=IVP+1
      VALUES(IVP,NSUB)=0.
      IF(KDEC(6).NE.MINDIC) VALUES(IVP,NSUB)=KDEC(6)/100.
CHeight of station.007001
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(8).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(8))
C
C12
CPressure. 010051 QNH
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(39).NE.MINDIC) VALUES(IVP,NSUB)=KDEC(39)*100.
CClilmar97 - Removed.
CSea-level pressure. 010005
C     IVP=IVP+1
C     VALUES(IVP,NSUB)=999999.
C     IF(KDEC(40).NE.MINDIC) VALUES(IVP,NSUB)=KDEC(40)*100.
C     
C13-17
CWind-direction. 011011
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(25).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(25))
CWind-speed. 011012
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(26).NE.MINDIC) VALUES(IVP,NSUB)=KDEC(26)*0.1
CGust. 011041
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(27).NE.MINDIC) VALUES(IVP,NSUB)=KDEC(27)*0.1
CMIN:Wind-direction. 011208
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(28).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(28))
CMAX:Wind-direction. 011209
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(29).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(29))
C
C18-19
cmar97
CRunwaydesignator for windshear. 011070
       IVP=IVP+1
       VALUES(IVP,NSUB)=IVP*1000*NSUB+4
       IF(KINT(158).EQ.MINDIC)THEN
          CVAL(IVP)='    '
       ELSEIF(KINT(158).EQ.65)THEN
          CVAL(IVP)='ALL '
       ELSE
          II=KINT(159)/10
          CVAL(IVP)= CHAR(II+48)//CHAR(KINT(159)-(II*10)+48)//
     1               CHAR(KINT(160))//CHAR(KINT(161))
       ENDIF
cx       print*,'011070,kint(159)',kint(159),cval(ivp)
CRunwaydesignator for windshear. 011070
       IVP=IVP+1
       VALUES(IVP,NSUB)=IVP*1000*NSUB+4
       IF(KINT(162).EQ.MINDIC)THEN
          CVAL(IVP)='    '
       ELSEIF(KINT(162).EQ.65)THEN
          CVAL(IVP)='ALL '
       ELSE
          II=KINT(163)/10
          CVAL(IVP)= CHAR(II+48)//CHAR(KINT(163)-(II*10)+48)//
     1               CHAR(KINT(164))//CHAR(KINT(165))
       ENDIF
cx       print*,'011070,kint(163)',kint(163),cval(ivp)
C
C20-21
CTemperature. 012004
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(37).NE.MINDIC)
     1    VALUES(IVP,NSUB)=(KDEC(37)*0.1)+273.16
C  
CDew-point temperature. 012006
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(38).NE.MINDIC)
     1    VALUES(IVP,NSUB)=(KDEC(38)*0.1)+273.16
C
C22
CVertical visibility. 020002
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(44).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(44))
C23
CCAVOK. 
CGeneral indicator of weather.020009
      IVP=IVP+1
      VALUES(IVP,NSUB)=999999.
      IF(KDEC(98).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(98))
C
C24-27
CDirection of horizontal visibility. 005021
       IVP=IVP+1
       VALUES(IVP,NSUB)=999999.
       IF(KDEC(31).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(31))
C(MIN)Horizontal visibility. 020001
       IVP=IVP+1
       VALUES(IVP,NSUB)=999999.
       IF(KDEC(30).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(30))
C
CDirection of horizontal visibility. 005021
       IVP=IVP+1
       VALUES(IVP,NSUB)=999999.
       IF(KDEC(33).NE.MINDIC)VALUES(IVP,NSUB)=FLOAT(KDEC(33))
CMAXHorizontal visibility. 020001
       IVP=IVP+1
       VALUES(IVP,NSUB)=999999.
       IF(KDEC(32).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(32))
C    
C
C28-30
CPresent weather. 020019
      IVP=IVP+1
      VALUES(IVP,NSUB)=IVP*1000*NSUB+9
      CVAL(IVP)= CHAR(KINT(111))//CHAR(KINT(112))//CHAR(KINT(113))//
     1           CHAR(KINT(114))//CHAR(KINT(115))//CHAR(KINT(116))// 
     2           CHAR(KINT(117))//CHAR(KINT(118))//CHAR(KINT(119)) 
c     print*,'cval,values,ww,1',cval(IVP),':',VALUES(IVP,NSUB)
CPresent weather. 020019
      IVP=IVP+1
      VALUES(IVP,NSUB)=IVP*1000*NSUB+9
      CVAL(IVP)= CHAR(KINT(121))//CHAR(KINT(122))//CHAR(KINT(123))//
     1           CHAR(KINT(124))//CHAR(KINT(125))//CHAR(KINT(126))// 
     2           CHAR(KINT(127))//CHAR(KINT(128))//CHAR(KINT(129)) 
c      print*,'cval,values,ww,2',cval(IVP),':',VALUES(IVP,NSUB)
CPresent weather. 020019
      IVP=IVP+1
      VALUES(IVP,NSUB)=IVP*1000*NSUB+9
      CVAL(IVP)= CHAR(KINT(131))//CHAR(KINT(132))//CHAR(KINT(133))//
     1           CHAR(KINT(134))//CHAR(KINT(135))//CHAR(KINT(136))// 
     2           CHAR(KINT(137))//CHAR(KINT(138))//CHAR(KINT(139)) 
c      print*,'cval,values,ww,3',cval(IVP),':',VALUES(IVP,NSUB)
C
C31-33
CRecent weather 020020
      IVP=IVP+1
      VALUES(IVP,NSUB)=IVP*1000*NSUB+4
      CVAL(IVP)= CHAR(KINT(142))//CHAR(KINT(143))//CHAR(KINT(144))//
     1           CHAR(KINT(145))
c      print*,'cval,values,reww,1',cval(IVP),':',VALUES(IVP,NSUB)
CRecent weather 020020
      IVP=IVP+1
      VALUES(IVP,NSUB)=IVP*1000*NSUB+4
      CVAL(IVP)= CHAR(KINT(148))//CHAR(KINT(149))//CHAR(KINT(150))//
     1           CHAR(KINT(151))
c      print*,'cval,values,reww,2',cval(IVP),':',VALUES(IVP,NSUB)
CRecent weather 020020
      IVP=IVP+1
      VALUES(IVP,NSUB)=IVP*1000*NSUB+4
      CVAL(IVP)= CHAR(KINT(154))//CHAR(KINT(155))//
     1           CHAR(KINT(156))//CHAR(KINT(157))
c      print*,'cval,values,reww,3',cval(IVP),':',VALUES(IVP,NSUB)
C
C
C34-36/37-39/40-42/43-45
CCLOUDGROUPS:
C
      DO 214 IRW=1,4
         IST=45+((IRW-1)*3)
CCloud cover 1. 020011
         IVP=IVP+1
         VALUES(IVP,NSUB)=999999.
         IF(KDEC(IST).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(IST))
         IST=IST+1
CHeight of base of cloud 1. 020013
         IVP=IVP+1
         VALUES(IVP,NSUB)=999999.
         IF(KDEC(IST).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(IST))
         IST=IST+1
CCloud type 1. 020012
         IVP=IVP+1
         VALUES(IVP,NSUB)=999999.
         IF(KDEC(IST).NE.MINDIC) VALUES(IVP,NSUB)=FLOAT(KDEC(IST))
 214  CONTINUE
C
C  
C46-51
C52-57
C58-63
      DO 215 IRW=1,3
         IST=57+((IRW-1)*7)
C
C*46/52/59
C        Runwaydesignator. 001064
cx            print*,'Runway:',KDEC(IST)
             IVP=IVP+1
             VALUES(IVP,NSUB)=IVP*1000*NSUB+4
             IF(KDEC(IST).NE.MINDIC)THEN
                II=KDEC(IST)/10
                kintp=20+(IRW*10)
                CVAL(IVP)= CHAR(II+48)// CHAR(KDEC(IST)-(II*10)+48)//
     1               CHAR(KINT(kintp+2))//CHAR(KINT(kintp+3))
             ELSE
                CVAL(IVP)='    '
             ENDIF
cx      print*,'Metexp1:Runwaydesignator,ivp=',
cx     C    ivp,':',kdec(ist),cval(ivp)
C
C
C           008014
C           Qualification for Runway visual range.
            IVP=IVP+1
            VALUES(IVP,NSUB)=999999.
            IF(KDEC(IST+2).NE.MINDIC) 
     C         VALUES(IVP,NSUB)=FLOAT(KDEC(IST+2))
C
C           Runwayvisibility no 1. 020061
            IVP=IVP+1
            VALUES(IVP,NSUB)=999999.
            IF(KDEC(IST+2).NE.MINDIC) 
     C            VALUES(IVP,NSUB)=FLOAT(KDEC(IST+3))
c
caug97 Missing test for missing value.     
Caug97           VALUES(IVP,NSUB)=FLOAT(KDEC(IST+3))
c
C
C           008014
C           Qualification for Runway visual range.
            IVP=IVP+1
            IF(KDEC(IST+5).NE.MINDIC) VALUES(IVP,NSUB)=999999.
            IF(KDEC(IST+4).NE.MINDIC) 
     C         VALUES(IVP,NSUB)=FLOAT(KDEC(IST+4))
C
C           Runwayvisibility no 2. 020061
            IVP=IVP+1
            VALUES(IVP,NSUB)=999999.
            IF(KDEC(IST+5).NE.MINDIC)
     C         VALUES(IVP,NSUB)=FLOAT(KDEC(IST+5))
C
C           Tendency of Runway visual range. 020018
            IVP=IVP+1
            VALUES(IVP,NSUB)=999999.
            IF(KDEC(IST+6).NE.MINDIC)
     C         VALUES(IVP,NSUB)=FLOAT(KDEC(IST+6))
C
 215  CONTINUE
C
 219  CONTINUE
C
C      UPDATE NUMBER OF ELEMENTS.
C
       M=63
C       DO 218 i=1,m
C          print*,i,'values ',values(i,1)
 218   CONTINUE




C
C*          2.2   SET INFORMATION NEEDED FOR RDB KEY DEFINITION.
C                 ----------------------------------------------
 220  CONTINUE
C
C
      NBUFTYPE=0
      NTYPE=8                          ! METAR/SPECI
      NSBTYPE=KDEC(4)-80               ! metar 5, speci 6
C      NSBTYPE=5                        ! MANUELL
C
C
cpsapr98      NYEAR =IYEAR+1900
      NYEAR =IYEAR
      NMONTH=IMONTH
      NDAY  =KDEC(1)
      NHOUR =KDEC(2)
      NMINUTE=KDEC(3)
      NSECOND=0
C
      JSCALE=5
      JREFLAT= -9000000
      JREFLON=-18000000
C
      NLAT1=0  
      NLON1=0
c
      IF(VALUES(9,NSUB).NE.MINDIC)
     1   NLAT1=NINT(VALUES( 9,NSUB)*10.**JSCALE)-JREFLAT
      IF(VALUES(10,NSUB).NE.MINDIC)
     1    NLON1=NINT(VALUES(10,NSUB)*10.**JSCALE)-JREFLON
C
c     print*,'nlat1',nlat1
c     print*,'nlon1',nlon1 
      NLAT2=0  
      NLON2=0
C
      NOBS=NSUB
C
      CIDENT= CHAR(KINT(5))//CHAR(KINT(6))//CHAR(KINT(7))//
     1        CHAR(KINT(8))//'    '  
C
      NRECR=1
C
      RETURN
      END
      SUBROUTINE BUFRENC(K,IERR )
C
C**** *BUFRENC*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO ENCODE INDIVIDUAL METAR
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
C         *CALL* *SETWT(K,IERR)*
C         *CALL* *SEC0(IERR)*
C         *CALL* *SEC1(K,IERR)*
C         *CALL* *SEC2(IERR)*
C         *CALL* *SEC3(K,IERR)*
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
C
C     ------------------------------------------------------------------
C*          1.   SET UP *BUFR* MESSAGE.
C                ----------------------
 100  CONTINUE
C
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' BUFRENC - start of subr., ierr ',IERR
C      PRINT*,' BUFRENC - start of subr., ierr ',IERR
CTEST
      IF( IERR.NE.0 ) RETURN
C
C           1.1 SET WORKING TABLES AND BUFR SECTION DATA.
C               -----------------------------------------
 110  CONTINUE
C
      CALL SETWT( K,IERR )
CX     PRINT*,' AFTER SETWT ., ierr ',IERR
      CALL SEC0 ( IERR )
CX     PRINT*,' AFTER SEC0 ., ierr ',IERR
      CALL SEC1 ( IERR )
CX     PRINT*,' AFTER SEC1 ., ierr ',IERR
      CALL SEC2 ( IERR )
CX     PRINT*,' AFTER SEC2 ., ierr ',IERR
      CALL SEC3 ( K,IERR )
CX     PRINT*,' AFTER SEC3 ., ierr ',IERR
      CALL SEC4 ( IERR )
CX     PRINT*,' AFTER SEC4 ., ierr ',IERR
      CALL SEC5 ( IERR )
CX     PRINT*,' AFTER SEC5 ., ierr ',IERR
      CALL PUTBUFR( K,IERR )
CX     PRINT*,' AFTER putbufr ., ierr ',IERR
C     ------------------------------------------------------------------
C
      RETURN
      END
      SUBROUTINE SEC1(IERR)
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
C          M. Dragosavac 14/11/1991  Bufr Edition 2 introduced.
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
C*          4.   BUFR master table (zero if WMO  FM 94-IX Ext used)
C                ---------------------------------------------------
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
C*          1.7  SET UP BUFR  MESSAGE TYPE.
C                --------------------------
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
C*          9.   VERSION NUMBER OF MASTER TABLE USED (CURRENTLY 7).
C                --------------------------------------------------
 900  CONTINUE
C
crr 990809 version 7  
crr 950317 version 3  
crr N.B. change Bufr Table B in ..../libsrc/settab.f
      CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT,8)
crr      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,8)
C
C*          9.1  VERSION NUMBER OF LOCAL TABLES USED.
C                ------------------------------------
 910  CONTINUE
C
ctest version 1
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT,8)
crr      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          9.2  SET UP STATION IDENTIFIER.
C                -----------------------
 920  CONTINUE
C
C      RYEAR=VALUES(4,1)
C      IF(KDEC(4).GE.21.AND.KDEC(4).LE.24) RYEAR=VALUES(5,1)
C
Cps      IYEAR=RYEAR-1900.
Cps      CALL PACK(NBPW,MBUF(NWPT),IYEAR,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C*          10.  SET UP YEAR OF CENTURY.
C                -----------------------
 1000 CONTINUE
C
      RYEAR=VALUES(4,1)
      IF(KDEC(4).GE.21.AND.KDEC(4).LE.24) RYEAR=VALUES(5,1)

cpsapr98      IYEAR=RYEAR-1900.
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
      IMONTH=VALUES(5,1)
      IF(KDEC(4).GE.21.AND.KDEC(4).LE.24) IMONTH=VALUES(6,1)
      CALL PACK(NBPW,MBUF(NWPT),IMONTH,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          12.  SET UP DAY.
C                -----------
 1200 CONTINUE
C
      IDAY=VALUES(6,1)
      IF(KDEC(4).GE.21.AND.KDEC(4).LE.24) IDAY=VALUES(7,1)
      CALL PACK(NBPW,MBUF(NWPT),IDAY,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          13.  SET UP HOUR.
C                -------------
 1300 CONTINUE
C
      IHOUR=VALUES(7,1)
      IF(KDEC(4).GE.21.AND.KDEC(4).LE.24) IHOUR=VALUES(8,1)
      CALL PACK(NBPW,MBUF(NWPT),IHOUR,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          14.  SET UP MINUTE.
C                -------------
 1400 CONTINUE
C
      IMIN=VALUES(8,1)
      IF(KDEC(4).GE.21.AND.KDEC(4).LE.24) IMIN=VALUES(9,1)
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
      SUBROUTINE SEC3(K,IERR )
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
C         *CALL* *SEC3(K,IERR)*
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
C          L. LOVHOIDEN         *DNMI*       MARCH 1995.
C
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
C     METAR - BASIC REPORT
C
      IF(K.EQ.1) THEN
C
C001063
CICAO Location Indicator
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),63,NWPT,NBPT, 8)
C001001
CWMO-blocknumber.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
C001001
CWMO-Stationnumber.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 8)
C004001
CYear.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
C004002
CMonth.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 8)
C004003
CDay.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 8)
C004004
CHour.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 8)
C004005
CMinute.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),5,NWPT,NBPT, 8)
C*9
C005001
CLatitude.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),5,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
C006001
CLongitude.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),6,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
C007001
CHeight of station.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
C010004
CPressure.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),10,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),52,NWPT,NBPT, 8)
C*13
C011011
CWind.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 8)
C011012
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 8)
C
C011041
CGust
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),41,NWPT,NBPT, 8)
C
C011016
CVariation in direction of wind.
Cmin:
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),16,NWPT,NBPT, 8)
C011017
Cmax:
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),17,NWPT,NBPT, 8)
C*18
C011070
CRunwaydesignator for windshear. 011070
          CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
          CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
          CALL PACK(NBPW,MBUF(NWPT),70,NWPT,NBPT, 8)
C011070
CRunwaydesignator for windshear. 011070
          CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
          CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
          CALL PACK(NBPW,MBUF(NWPT),70,NWPT,NBPT, 8)
C*20
C012004
CTemperature.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),4,NWPT,NBPT, 8)
C012006
CDew-point temperature.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),6,NWPT,NBPT, 8)
C*22
C020002
CVertical visibility.
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 8)
C*23
C020009
CCAVOK.
CGeneral indicator of weather
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),9,NWPT,NBPT, 8)
C*24
CHorizontal visibility.
C
         DO 161 II=1,2
C005021
CDirection of Horizontal visibility.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),5,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),21,NWPT,NBPT, 8)
C020001
CHorizontal visibility.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
 161     CONTINUE
C
C*28
C020019
CPresent weather 3 times.
         DO 162 II=1,3
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),19,NWPT,NBPT, 8)
 162     CONTINUE
C
C*31
C020020
CRecent weather 3 times.
         DO 163 II=1,3
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 8)
 163     CONTINUE
C
C*34
CCloudgroups 4 times.
C
         DO 164 II=1,4
C020011
CCloud Amount 1.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 8)
C020013
CHeigth of base 1.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),13,NWPT,NBPT, 8)
C020012
CCloud type 1.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),12,NWPT,NBPT, 8)
 164     CONTINUE
C
C  
C*46
CRunwayvisibility.
C
         DO 165 II=1,3
C001064
CRunwaydesignator
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),64,NWPT,NBPT, 8)
C008014
CQualification for Runway visual range.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),8,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),14,NWPT,NBPT, 8)
C020061
CRunwayvisibility.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),61,NWPT,NBPT, 8)
C008014
CQualification for Runway visual range.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),8,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),14,NWPT,NBPT, 8)
C020061
CRunwayvisibility.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),61,NWPT,NBPT, 8)
C020018
CTendency of Runwayvisibility.
            CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
            CALL PACK(NBPW,MBUF(NWPT),20,NWPT,NBPT, 6)
            CALL PACK(NBPW,MBUF(NWPT),18,NWPT,NBPT, 8)
 165     CONTINUE
C
C
C*67
C        CHECK IF Q/C PERFORMED.
C
         IBIT1=0
         DO 169 I=M+1,M+M
            IF(VALUES(I,NSUB).NE.999999.) IBIT1=IBIT1+1
 169        CONTINUE
C
         IF(IBIT1.EQ.0) GO TO 170
C
cc       MQ=76
         MQ=68
C          print*,' sec3 - first record,k,kdec(4),m,mq ',k,kdec(4),m,mq
C2220
         CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),22,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 8)
C101M
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),M,NWPT,NBPT, 8)
C031031
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 8)
C001031
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 8)
C001032
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),32,NWPT,NBPT, 8)
C101M
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),M,NWPT,NBPT, 8)
C033007
         CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
         CALL PACK(NBPW,MBUF(NWPT),33,NWPT,NBPT, 6)
         CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 8)
         GO TO 170
C
C
      END IF
C
C
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
      CHARACTER*9   CVAL,YVAL
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
CX    print*,' sec4- j,IREV,IREF,ISCALE,IBDW,YUNIT '
CX    print*,' sec4- ',j,IREV,IREF,ISCALE,IBDW,YUNIT
CX    print*,' sec4- VALUES(J,I) ',VALUES(J,I)
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
            IPACK=NINT(VAL/10**ISCALE) - IREF
         ELSE
            IPACK=NINT(VAL*10**ISCALE) - IREF
         END IF
ctest      if(j.eq.35) print*,'m=35, ipack=',ipack,' ibdw=',ibdw,
ctest     *        ' nbpt=',nbpt,' iref=',iref
ctest      nwptx=nwpt
ctest         print*,'j=',j,' ipack=',ipack,' nwpt=',nwpt,
ctest     *        ' nbpt=',nbpt,' ibdw=',ibdw
C
         CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
c         print*,'mbuf(nwpt)=',mbuf(nwptx),' nwpt=',nwpt,' nbpt=',nbpt
c      if(j.eq.35) print*,'m=35, mbuf(nwpt)=',mbuf(nwptx)
      END IF
C
CX    print*,' sec4- IPACK ',IPACK
 202  CONTINUE
C
Ctest print*,' sec4 - m,mq ',m,mq
      IF(M.LT.MQ) THEN
C
        DO 210 J=M+1,MQ      
C
Ctest print*,' sec4 - j,m,mq ',j,m,mq

        IREV  =IWTR (J)
        IREF  =IWTRV(J)
        ISCALE=IWTS (J)
        IBDW  =IWTDW(J)
        YUNIT =CUNIT(J)
ctest        print*,' sec4 - j,IREV,IREF,ISCALE,IBDW,YUNIT ',
ctest     *         j,IREV,IREF,ISCALE,IBDW,YUNIT
C
        IF(IREV.EQ.031031) THEN
C 
C          SET BIT FLAG
C
           IBIT1=0
C
           DO 215 JI=M+1,M+M
C
           IPACK=0
           IF(VALUES(JI,I).NE.999999.) IPACK=0
           CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,1)
C
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
              CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
           END IF
        END IF 
C
        IF(IREV.EQ.033007) THEN
           DO 216 JI=M+1,M+M
           VAL=VALUES(JI,I)
           IF(VAL.EQ.999999.) THEN
              CALL PACK(NBPW,MBUF(NWPT),MASKS(IBDW),NWPT,NBPT,IBDW)
              GO TO 216
           END IF
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
      SUBROUTINE SETWT(K,IERR)
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
C         *CALL* *SETWT(K,IERR)*
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
C          L. lovhoiden     *DNMI*       MAY 1995
C
C
C     MODIFICATIONS.
C     --------------
C
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.f'
C
      CHARACTER*24 CTABBU
      CHARACTER*64 CTABBEN
C
      CHARACTER*9   CVAL
      CHARACTER*24 CUNIT
C
      INCLUDE 'comwork.f'
      INCLUDE 'comtab.f'
      INCLUDE 'comtabc.f'
      INCLUDE 'comwt.f'
      INCLUDE 'comwtc.f'
C
cc    DIMENSION ITBTRM(75),ITBTRA(75)
      DIMENSION ITBTRM(67),ITBTRA(67)
C
      DATA ITBTRM/001063,001001,001002
C4:Time
     c,           004001,004002,004003,004004,004005
C9:Lat/long/hh
     c,           005001,006001,007001
C12:pp
cc   c,           010052,010051
     c,           010052
C13:wind            
cccccc,           011011,011012,011041,011208,011209
cccccc,           011211,005229,005230,011211,005229,005230
     c,           011011,011012,011041,011016,011017
     c,           011070,011070
C20:Temp
     c,           012004,012006
C22:vertvv
cccccc,           020002,020197
     c,           020002,020009
C24:vvvv
cmar97,           020001,005231,020001,005231
     c,           005021,020001,005021,020001
C28:ww
clilmar97.c,      020003,020003,020003,020196,020196,020196
     c,           020019,020019,020019,020020,020020,020020
C34:cl
clilfeb.97c,      020011,020013,020011,020013,020011,020013
     c,           020011,020013,020012,020011,020013,020012
     c,           020011,020013,020012,020011,020013,020012
C46:RDRDR
cccccc,           005229,005230,020195,020061,020195,020061,020194
cccccc,           005229,005230,020195,020061,020195,020061,020194
cccccc,           005229,005230,020195,020061,020195,020061,020194
     c,           001064,008014,020061,008014,020061,020018
     c,           001064,008014,020061,008014,020061,020018
     c,           001064,008014,020061,008014,020061,020018
C64:
     c,           031031,001031,001032,033007/
C
C
C     ------------------------------------------------------------------
C*          1.   SET UP WORKING TABLES FOR METAR DATA.
C                ----------------------------------------------
 100  CONTINUE
C
      IF(IERR.NE.0) RETURN
C
C     SET UP BASIC REPORT.
C
C
cc       DO 101 I=1,75
         DO 101 I=1,67
            ITBTRA(I)=ITBTRM(I)
 101        CONTINUE 
C
         GO TO 110
C 
C
C     -----------------------------------------------------------------  
C         IERR=1
C         RETURN
C     -----------------------------------------------------------------
C
 110   CONTINUE
C
cc     DO 111 I=1,75
         DO 111 I=1,67
C
          ITR=ITBTRA(I)
C
          DO 112 II=1,JP2
C
          IF(ITR.EQ.NTABBTR(II)) THEN
             IWTR (I)=NTABBTR(II)
             IWTS (I)=NTABBS (II)
             IWTDW(I)=NTABBDW(II)
             IWTRV(I)=NTABBRV(II)
             CUNIT(I)=CTABBU(II)
             GO TO 111
          END IF
C
 112      CONTINUE
C
      PRINT*,'setwt1-BUFR TABLE B REFERENCE ENTRY',ITR,'NOT FOUND'
C
 111  CONTINUE
C
      RETURN
      END
      SUBROUTINE PUTBUFR(K,IERR )
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
cpsjun11      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
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
CNH common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
C
      CHARACTER*15000 YOUT,YOUTOLD
      CHARACTER*8  CIDENT
      CHARACTER*48 CKEY,YKEYOLD
      CHARACTER*32 YKEY
      CHARACTER*16 YTYPE,CRDBFN(20)
cpsapr98      CHARACTER*12 CFNAME
      CHARACTER*14 CFNAME
      CHARACTER*14 YTIME
      CHARACTER*23 CTIME,CRDBT(20),CTIME_DIF(20),YCTIME
      CHARACTER*10 CUSE
      CHARACTER*29 YCFNAME
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
C
C
      DIMENSION IKEY(12),IRDBFP(20),IKEYOLD(12)
C
      EQUIVALENCE (IKEY(1),YKEY,CKEY)
      EQUIVALENCE (IKEYOLD(1),YKEYOLD)
cpsapr04      EQUIVALENCE (MBUF(1),YOUT)
      EQUIVALENCE (MBUF(2),YOUT)
C
C     ------------------------------------------------------------------
C*          1. DEFINE RDB FILE NAME AND OPEN RDB FILE.
C                -------------------------------------
 100  CONTINUE
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' PUTBUFR - start of subr., ierr ', IERR
CX    PRINT*,' PUTBUFR - start of subr., ierr ', IERR
CTEST
C
      IF(IERR.NE.0) RETURN
C
      IRET=0
      YTYPE='meta'
cpsapr98      NY=NYEAR - 1900
      NY=NYEAR
      NCONR=KDEC(16)
      NSTNR=KDEC(17)
      IF(NCONR.GT.9999)NCONR=0
      IF(NSTNR.GT.9999)NSTNR=0
C
cpsapr98      WRITE(YTIME,'(6I2.2)',IOSTAT=IOS,ERR=101)
      WRITE(YTIME,'(I4,5I2.2)',IOSTAT=IOS,ERR=101)
     1       NY,NMONTH,NDAY,NHOUR,NMINUTE,NSE3OND
C
CX      PRINT*,' PUTBUFR -  ', NY,NMONTH,NDAY,NHOUR,NMINUTE,NSECOND
      GO TO 102
C
 101  CONTINUE
C
      PRINT*,'BAD DATE OR TIME , OBSERVATION IS SKIPPED. IOSTAT=',IOS
      RETURN
C
 102  CONTINUE
C
C
C     DEFINE FILE NAME
CX    PRINT*,'DEFINE FILE NAME:'
CX    PRINT*,YTYPE,':',YTIME,':',CFNAME,':',IRET
C
      CALL ISFILE48(YTYPE,YTIME,CFNAME,IRET)
CTEST
CX    PRINT*,' PUTBUFR - cfname ', CFNAME
CTEST
      IF(IRET.NE.0) THEN
         PRINT*,'ISFILE48:WRONG FILE NAME, IRET=',IRET
         IRET=0
         RETURN
      END IF 
C
C*          2.  DEFINE RDB KEY AND HOUSEKEEPING INFORMATION.
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
CX    PRINT*,' PUTBUFR --RDB-KEY:'
C
C     CALL PACK(32,IKEY(IW),NCONR,IW,IBT,8)
C     CALL PACK(32,IKEY(IW),NSTNR,IW,IBT,8)
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
      YKEY(16:23)=CIDENT
      YKEY(24:32)=' '
C
C*          2.2 DEFINE HOUSE KEEPING INFORMATION.      
C               ---------------------------------
 220  CONTINUE
C
      ILENGTH=(NWPT-2)*4+NBPT/8
CTEST
CTEST       PRINT*,' PUTBUFR - ILENGTH,NWPT,NBPT ',ILENGTH,NWPT,NBPT
CTEST
C
      IF(KDEC(21).EQ.MINDIC) THEN
                                NCORR=0
                             ELSE
                                NCORR=KDEC(21)
                             END IF
C
C     FIND LOWEST QUALITY CONTROL PERCENTAGE. 
C
CNH - M and MQ in common comwt
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
CTEST      PRINT*,' PUTBUFR - QC: NQC ', NQC
CTEST
C      
      IW =9
      IBT=0
C
      CALL FDATE(CTIME)
      READ(CTIME,'(8X,I2,1X,I2,1X,I2,1X,I2)') IDD,IHH,IMM,ISS
      ICC=0
CTEST
CTEST PRINT*,' PUTBUFR - idd,ihh,imm,iss,icc ',IDD,IHH,IMM,ISS,ICC
CTEST
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
      CALL PACK(32,IKEY(IW),NRDAY,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRHOUR,IW,IBT,5)      
      CALL PACK(32,IKEY(IW),NRMIN,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRSEC,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORR,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRECR,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
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
C
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
      LREP=.FALSE.
C 
CTEST
CTEST PRINT*,' PUTBUFR - NCORR ', NCORR
CTEST PRINT*,' PUTBUFR - CKEY  ', CKEY
CTEST PRINT*,' PUTBUFR - IKEY  ', IKEY
CTEST
      IF(NCORR.NE.0) THEN
      Print*,' PUTBUFR - No overwrite of old records, NCORR=',NCORR
CTEST
      END IF 
C
C
C*         3.2 CHECK IF FILE IS ALREADY OPENED.
C              --------------------------------
 320  CONTINUE
C
CTEST Check if .rdb file already opened
CX    print*,' CFNAME:',CFNAME
      INQUIRE(FILE=cstream(msys)//'/'//CFNAME//'.rdb',OPENED=OPN,
     1        NUMBER=IU)
CTEST PRINT*,' PUTBUFR - open? ', OPN
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
     5     STATUS='UNKNOWN'        )
C
C using NTEST for record in comdir.com common comtest
      NTEST(IU)=0
350   CONTINUE
C
CTEST Check if .key file already opened
      INQUIRE(FILE=cstream(msys)//'/'//CFNAME//'.key',OPENED=OPN,
     1        NUMBER=IUNIT)
CTEST PRINT*,' PUTBUFR - open? ', OPN
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
cpsapr01 added writing of correction field NCORR (>0 if CCA/CCB/...)crr
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
CRR   nwrep=nwrep+1 
C
C
C     -----------------------------------------------------------------
 400  CONTINUE
C
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
CNH common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
C
C
      CHARACTER*80 YLINE,YLINEA
      CHARACTER*1 Y63
      CHARACTER*4 YGS
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
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
      print*,'SAVBULL - opening meta_error_file.dat'
      OPEN(UNIT=10,IOSTAT=IOS,ERR=400,
     C     FILE=cstream(msys)//'/'//'meta_error_file.dat',
#ifdef FORTRAN90
     C     POSITION='APPEND',
#else
     C     ACCESS='APPEND',
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
C
C     REMOVE PARITY BIT
C
      DO 410 I = 1 , IGS
      KCHAR(I) = IAND(KCHAR(I),127)
 410  CONTINUE
C
      CLOSE(10)
C 
      RETURN
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
CNH common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
C
      DIMENSION ISTART(4),IFIN(4)
C
      CHARACTER *80 YLINE,YLINEA
      CHARACTER*1 Y63
      CHARACTER*4 YGS
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
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
CX    print*,'SAVREP - opening meta_error_file.dat'
CX    print*,'IHEAD=',IHEAD,'KPT=',KPT
      OPEN(UNIT=10,IOSTAT=IOS,ERR=400,
     C     FILE=cstream(msys)//'/'//'meta_error_file.dat',
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
      IF(KINT(4).EQ.1) NN=4
C
      DO 121 N=1,NN
         K = IFIN(N) - ISTART(N) 
         IF(K.GT.80) K=80
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
      
CX    print*,'SAVREP - writing heading to error-file.'
CX    WRITE (10,'(A)',ERR=300,IOSTAT=IOS)
CX   C            'SAVREP - writing heading to error-file.'
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N2)
      IF(ISIGN.EQ.1) THEN
         WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N2)
         ISIGN=0
      END IF
CX    WRITE (10,'(A)',ERR=300,IOSTAT=IOS)'Written heading.'
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
C          print*,'CLOSE(10) -  IAH EQ KPT'
           CLOSE(10)
           RETURN
      END IF
C
C     AVOID WRITING MIMIMJMJ TWICE ( CORRUPT MIMIMJMJ CAN BE TAKEN
C     AS STATION NUMBER )
C
C   IF ( KPT.LT.IFIN(4) ) KPT = IFIN(4)
C
C
      IP = KPT
C     IF(KDEC(4) .EQ. 85) IP = IMI
      IF(KDEC(4) .EQ. 35 .OR. KDEC(4) .EQ. 36) IP = IMI
      IF(KDEC(4) .EQ. 32 .OR. KDEC(4) .EQ. 33) IP = IMI
      J = IEQ+ 3
      IF ( J.GT.IGS) J = IGS
C
  133 CALL NEXTPRT (IP,J)
C     print*,'SAVREP:',char(kchar(ip)),',IP,J',ip,j   
      IF (IP.GT.J) THEN
C         print*,'CLOSE(10) -  IP GT J'
          CLOSE(10)
          RETURN
       END IF
      JP = IP
      CALL NEXTEND(JP,J)
      CALL NEXTPRT(JP,J)
      K = JP - IP
      N1 = 0
      IF(K.GT.80) K=80
      DO 131 N=1,K
         N1 = N1 + 1
CX       WRITE (10,'(A)',ERR=300,IOSTAT=IOS)
CX   C                'SAVREP:',char(kchar(ip))   
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
CX    print*,'SAVREP - writing to error-file.'
c
CX    WRITE (10,'(A)',ERR=300,IOSTAT=IOS)'Writing error-report.'
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
CX    WRITE (10,'(A)',ERR=300,IOSTAT=IOS)'Written 1. line.'
      IF(ISIGN.EQ.1) THEN
         WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
         ISIGN=0
      END IF
CX    WRITE (10,'(A)',ERR=300,IOSTAT=IOS)'Written error-report.'
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
      WRITE (10,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:4)
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
      CLOSE(10)
C
C     REMOVE PARITY BIT
C
      DO 500 I = 1 , IGS
      KCHAR(I) = IAND(KCHAR(I),127)
 500  CONTINUE
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
      data cstr/'                   metar decoding'/
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
cps
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
cccccccccccccccccccccccc meta_lib.f cccccccccccccccccccccccccccccccccccccccccc
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
C
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
      IF ( IFIRST.EQ.0 ) THEN
         IF(NUMBER .EQ. 1) NUMRERR(IPART)=NUMRERR(IPART) + 1
         NOER(IPART,KERR)=NOER(IPART,KERR) + 1
C        KERBIT =ISHFT(KDEC(20),1-KERR) .AND. 1
C        IF(KERBIT .EQ. 0) KDEC(20) = KDEC(20) + 2**(KERR-1)
      END IF
C
      IPT = IABS(IPT)
      IF(IMARK .EQ. 1) KCHAR(IPT) = IOR(KCHAR(IPT),128)
      IF(IMARK .EQ. 2)THEN
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
      SUBROUTINE TREND(IPT,IEQ)
cps   Writes rest of message starting at kchar(ipt) into meta/trendxxxxxx.dat
cps   This should be the BECMG, TEMPO or NOSIG part of message and is not 
cps   decoded.
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
Crr for cstream
      INCLUDE 'commdbc.f'
      INCLUDE 'commdb.f'
C
      CHARACTER*12 CTSTAMP
      CHARACTER*4   CSTREAM(40)
      character*4 ciao
cpsapr98      character*6 cdate
      character*8 cdate
      character*1 ctrend(65)
C
c     print*,'Procedure TREND'
c
c
c     Find icao-ident:
      write(ciao,'(4a)')char(kint(5)),char(kint(6)),
     C               char(kint(7)),char(kint(8))
C
c
c     Find date:
      iday=kint(1)
      if(kint(166).ne.MINDIC)then
C
         iday=kint(166)
C        If time is more than 23.45, then write to file for next day.
cps why?
cpsmay04         if((ihour.eq.23).and.(imin.gt.45))iday=iday+1
C
      endif
C
      ihour=kint(2)
      if(kint(167).ne.MINDIC) ihour=kint(167)
C
      imin=kint(3)
      if(kint(168).ne.MINDIC)imin=kint(168)
C
C
C
C     Initiate ctrend-array:
      do 100 i=1,65
         ctrend(i)=' '
100   CONTINUE
C
C
C     Put wanted section of kchar
C     into ctrend-array:
      i=1
      do 150 j=ipt,ieq
cpsaug06 Break loop if trend too long for ctrend
         if(i.gt.65) goto 151
         if((kchar(j).eq.13).OR.(kchar(j).eq.10))then
            ctrend(i)=' '
         else
            ctrend(i)=char(kchar(j))
         endif
C
C        Not more than 1 space, skip next space.
         if(ctrend(i).ne.' ')then
            i=i+1
         else
C
C           ctrend(i)=' '
            if(ctrend(i-1).ne.' ')i=i+1
C
         endif
C
150   CONTINUE
 151  continue
c
C
C
C***
C*    Open trend-file.
C***
C
C     Make filename with date in it:
      call datum(iday,imm,iyy)
c      print*,'Date:',iday,imm,iyy
cpsapr98      write(cdate(1:2),'(i2.2)')iyy
cpsapr98      write(cdate(3:4),'(i2.2)')imm
cpsapr98      write(cdate(5:6),'(i2.2)')idd
      write(cdate(1:4),'(i4)')iyy
      write(cdate(5:6),'(i2.2)')imm
cpsjan00      write(cdate(7:8),'(i2.2)')idd
      write(cdate(7:8),'(i2.2)')iday
c
      print*,' TREND  -  ',
     c            cstream(msys),'/trend'//cdate//'.dat'
      OPEN(UNIT=98,IOSTAT=IOS,ERR=400,
     1     FILE=cstream(msys)//'/'//'trend'//cdate//'.dat',
C     1     FILE=cstream(msys)//'/'//'trend.dat',
C     1     FILE='trend_file.dat',
#ifdef FORTRAN90
     2     POSITION='APPEND',
#else
     2     ACCESS='APPEND',
#endif
     3     STATUS='UNKNOWN'    )
C
      write(98,222)ciao,iday,ihour,imin,ctrend
c      write(*,222)ciao,iday,ihour,imin,ctrend
C
 222  FORMAT(1x,a4,i3.2,2(':',i2.2),1x,65a)
200   CONTINUE
      RETURN
C
C
 400  CONTINUE
C
      PRINT*,' Open error on trend_file.dat'
      RETURN
      END
