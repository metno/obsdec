      PROGRAM AIRPROC
C
C**** *AIRPROC*
C
C
C     PURPOSE.
C     --------
C         CONTROLLING ROUTINE FOR DECODING
C         AIREP DATA.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *AIRPROC*
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
CNH common for directories
      INCLUDE 'comdir.f'
Crr common for numbull
      INCLUDE 'comstat.f'
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
      if(narg.lt.3.or.ctype.eq.'?') then
             print*,''
             print*,' Usage: arep dir_file cmsys timestamp <cmsystop>'
             print*,'                                      (optional)'
             print*,' where  dir_file  - specification of directories'
             print*,'        cmsys     - msys file number '
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
c
C           1.1  INITIALIZE MDB ACCESS.
C                ----------------------
 110  CONTINUE
C
      CALL MSYSINIT(IERR )
      IF(IERR.NE.0) THEN
                       PRINT*,'MSYSINIT: ERROR=',IERR
                       CALL exit(2)
                    END IF 
c
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
C*          2.   READ IN MESSAGE.
C                ----------------
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
     1 PRINT*,' AIRPROC - after getnext, ierr ',IERR
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
CTEST PRINT*,' AIRPROC - kint(1-4)',(KINT(IR),IR=1,4)
      IF(IERR.NE.0)
     1 PRINT*,' AIRPROC - after PROCRFB, IERR= ',IERR
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
     1 PRINT*,' AIRPROC - after PROCHDR, KERR= ',KERR
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
     1 PRINT*,' AIRPROC - after PROCTXT, KERR= ',KERR
CTEST
      IF(KERR.NE.0) GO TO 200
C
C     ------------------------------------------------------------------
C*          6.   CALL APPROPRIATE ROUTINE TO DECODE AIRCRAFT DATA.
C                -------------------------------------------------
 600  CONTINUE
C
      CALL PROCT1U( IERR )
CTEST
      IF(IERR.NE.0)
     1 PRINT*,' AIREPROC - after PROCT1U, IERR= ',IERR
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
      print*,' Open error on arep.stop.dat'
      go to 1000
c
 1002 continue
c
      print*,' Read error on arep.stop.dat'
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
      ntesttot=0
      do 1010 i=1,200
         ntesttot=ntesttot+ntest(i)   
 1010 continue
C     
      print*,'number of reports written ',ntesttot
cps      print*,'number of duplicates ',ndup
      print*,' ****n.b.***** PUTBUFR : No testing of duplicates '
c
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
CTEST
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
C         *CALL* *savbull(IERR)*
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
C*          1.1  CHECK IF BULLETIN IS TOO SHORT I.E. 'NIL' BULLETIN.
C                ---------------------------------------------------
 110  CONTINUE
C
      ILE = IGS - IMI
      IF(ILE .LT. 26) THEN
                         KERR = 7
                         NUMBERR(7) = NUMBERR(7) + 1
cpsaug03                                KCHAR(IGS)=KCHAR(IGS).OR.128
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
      CALL BULLUA ( IERR )
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
      CALL BULLUA ( IERR )
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
C*              3.5  BULLETINS WITH 'TT' = 'UY'.
C
 350  CONTINUE
      RETURN
C     ------------------------------------------------------------------
C*              3.6  BULLETINS WITH 'TT' = 'UZ'.
C
 360  CONTINUE
      RETURN
C
C
      END



      SUBROUTINE BULLUA ( IERR )
C
C**** *BULLUA*
C
C
C     PURPOSE.
C     --------
C
C         CONTROLLING ROUTINE FOR DECODING OF INDIVIDUAL
C         REPORTS FROM BULLETINS OF AIRCRAFT OBSERVATIONS
C
C         INPUT         : BULLETIN IN CCITT 5 CHARACTERS , 1 CHARACTER
C                         PER WORD IN 'KCHAR'.
C
C                         POINTERS TO BEGINNING AND END OF 'STARTING LINE',
C                         'ABBREVIATED HEADING','MIMIMJMJ LINE' AND END
C                         OF BULLETIN.
C
C                         IERR NOT USED.
C
C         OUTPUT        : DECODED REPORTS WRITTEN TO FILE AND ERROR
C                         REPORTS TO ERROR FILE.
C
C                         IERR = 1 , IF ANY FILE HANDLING ERROR.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BULLUA(IERR)*
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
C         *CALL* *PRTBULL(I,J)*
C         *CALL* *SAVREP(IHEAD,IERR)*           
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
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
      INCLUDE 'comair.f'
C
C     ------------------------------------------------------------------
C*          1.   PRINT INPUT BULLETIN OF AIRCRAFT REPORT.
C                ----------------------------------------
 100  CONTINUE
C
c      CALL PRTBULL (1,IGS)
C
C
C     ------------------------------------------------------------------
C*          1.1  SET FLAGS AND WORKING POINTER.
C                ------------------------------
 110  CONTINUE
C
C     WORKING POINTER SET TO POINT TO FIRST LETTER OF MIMIMJMJ
C     GROUP.
C
      IPT = IMI
C
C     CLEAR FLAG WHICH INDICATES BULLETIN HEADER ALREADY WRITTEN TO
C     ERROR FILE.
C
      IHEAD = 0
C
C
C     ------------------------------------------------------------------
C*          1.2 HANDLE MIMIMJMJ LINE . SET DATA BASE CODE TYPE INDICATOR
C               --------------------------------------------------------
C              IN DECODED REPORT HEADER AND LOCATE START OF FIRST REPORT.
C              ----------------------------------------------------------
C              CODE TYPE INDICATOR IS 141 FOR AIREPS.
C              CODE TYPE INDICATOR IS 144 FOR AMDARS.
C              --------------------------------------
 120  CONTINUE
C
      KDEC(4) = 141
      IF(IT1.EQ.21.AND.IT2.EQ.4) KDEC(4)= 144
C
C
C     MIMIMJMJ MAY BE 'ARP','AIREP' OR 'ARS' . THE MIMIMJMJ LINE
C     IS ALSO FREQUENTLY OMITTED.
cps   or is AMDAR YYGG
C
C     SET K1 TO POINT TO THE NEXT 'SPACE' CHARACTER.
C
      K1 = IPT
      CALL NEXTVAL ( K1,32,IGS )
C
C     SET K2 TO POINT TO THE NEXT 'CR' CHARACTER.
C
      K2 = IPT
      CALL NEXTEND ( K2,IGS )
C
C      IF A 'CR' CHARACTER IS FOUND BEFORE 'SPACE',REPORT STARTS
C      ON NEXT LINE.(airep)
C
      IF ( K2.LT.K1 )
     C    THEN
              IPT = K2
          END IF
Crr Check if AMDAR
      IF ( KDEC(4).eq.144 )
     C    THEN
              IPT = K2
          END IF
Crr
      CALL NEXTPRT (IPT,IGS)
C
C     VERY FREQUENTLY THE MIMIMJMJ GROUP IS INCLUDED AT THE START
C     OF EACH REPORT . IF THIS IS THE CASE SKIP TO NEXT GROUP.
cps In case of amdars, this section locates and stores phase of flight
cps indicator ipipip (UNS/LVR/ASC/DES/LVW/'///') coded as a number in NPHASE
C
  121 ISMIMJ = 0
      NPHASE = 999999
      ierr=0
C
C     ARP
C
      IF ( KCHAR(IPT).EQ.65.AND.KCHAR(IPT+1).EQ.82.AND.KCHAR(IPT+2)
     C    .EQ.80 ) ISMIMJ = 1
C
C     ARS
C
      IF ( KCHAR(IPT).EQ.65.AND.KCHAR(IPT+1).EQ.82.AND.KCHAR(IPT+2)
     C    .EQ.83 ) ISMIMJ = 1
C
C     AIR
C
      IF ( KCHAR(IPT).EQ.65.AND.KCHAR(IPT+1).EQ.73.AND.KCHAR(IPT+2)
     C    .EQ.82 ) ISMIMJ = 1
C
C     UNS
C
      IF ( KCHAR(IPT).EQ.85.AND.KCHAR(IPT+1).EQ.78.AND.KCHAR(IPT+2)
     C    .EQ.83 ) THEN
          ISMIMJ = 1
          NPHASE = 2
      END IF
C
C     LVR
C
      IF ( KCHAR(IPT).EQ.76.AND.KCHAR(IPT+1).EQ.86.AND.KCHAR(IPT+2)
     C    .EQ.82 ) THEN
          ISMIMJ = 1
          NPHASE = 3
      END IF
C
C     ASC
C
      IF ( KCHAR(IPT).EQ.65.AND.KCHAR(IPT+1).EQ.83.AND.KCHAR(IPT+2)
     C    .EQ.67 ) THEN
          ISMIMJ = 1
          NPHASE = 5
      END IF
C
C     DES
C
      IF ( KCHAR(IPT).EQ.68.AND.KCHAR(IPT+1).EQ.69.AND.KCHAR(IPT+2)
     C    .EQ.83 ) THEN
          ISMIMJ = 1
          NPHASE = 6
      END IF
C
C     LVW
C
      IF ( KCHAR(IPT).EQ.76.AND.KCHAR(IPT+1).EQ.86.AND.KCHAR(IPT+2)
     C    .EQ.87 ) THEN
          ISMIMJ = 1
          NPHASE = 4
      END IF
C
C     ///
C
      IF ( KCHAR(IPT).EQ.47.AND.KCHAR(IPT+1).EQ.47.AND.KCHAR(IPT+2)
     C    .EQ.47 ) THEN
          IF(KCHAR(IPT+3).NE.32) THEN
             IPT=IPT+3
             GO TO 130
          END IF
          ISMIMJ = 1
          NPHASE = 999999
      END IF
C
      IF ( ISMIMJ.EQ.1 ) THEN
                             CALL NEXTVAL (IPT,32,IGS)
                             CALL NEXTPRT (IPT,IGS)
                         END IF
C
C
C     -------------------------------------------------------------------
C*              1.3 LOCATE END OF REPORT (=) AND INCREMENT REPORT COUNTER.
C                   ------------------------------------------------------
c                   if '=' sign is missing try to find next mimimjmj.
c                   ------------------------------------------------ 
 130  CONTINUE
C
      IEQ = IPT
      CALL NEXTEQ ( IEQ,IGS )
cpsfeb06      IF(IEQ.GT.IGS) THEN
      IF(IEQ.GE.IGS) THEN
          PRINT*,' = SIGN MISSING AT THE END OF REPORT '
      END IF
C
C     TRY TO FIND NEXT MIMJ GROUP
C
      CALL NXMIMJ(IPT,IEQ,IIPT)
      IF(IIPT.LT.IEQ) IEQ=IIPT
          
C
      NUMREP(2) = NUMREP(2) + 1
C
C
C     --------------------------------------------------------------------
C*               2.  DECODE REPORT.
C                    ---------------
 200  CONTINUE
C
C                2.1 CONVERT REPORT TO INTERMEDIATE FORMAT.
C                    --------------------------------------
 210  CONTINUE
C
      CALL UAINT ( IHEAD,IERR )
C
      IF (KERR.NE.0 ) GO TO 230
C
C
C*               2.2 CONVERT INTERMEDIATE TO DECODED FORMAT AND
C                    ------------------------------------------
C                    WRITE TO FILE.
C                    ---------------
 220  CONTINUE
C
      CALL UADEC ( IHEAD,IERR )
C
      IF ( KERR.NE.0 ) GO TO 230
C
C*               2.3  LOCATE START OF NEXT REPORT.
C                     ----------------------------
 230  CONTINUE
C
      IPT = IEQ + 1
      call nextsep  ( ipt,igs)
      CALL NEXTPRT ( IPT,IGS )
C
C*               2.4  CHECK FOR END OF BULLETIN.
C                     ---------------------------
 240  CONTINUE
C
      IF (IPT.GT.IGS)
     C    THEN
C
C             END OF BULLETIN , SO IF ANY REPORT HAS BEEN WRITTEN TO
C             THE ERROR FILE , ADD 'GS' CHARACTER BEFORE RETURNING.
C
              IF ( IHEAD.EQ.0 ) RETURN
              IHEAD = 2
              CALL SAVREP( IHEAD,IERR )
              RETURN
          END IF
C
C
C
C*               2.5 GO TO NEXT REPORT.
C                    ------------------
 250  CONTINUE
C
      GO TO 121
C
C
      END
      SUBROUTINE UAINT ( IHEAD,IERR )
C
C**** *UAINT*
C
C
C     PURPOSE.
C     --------
C
C         CONVERT AIRCRAFT REPORTS FROM CCITT 5 CHARACTER
C         FORMAT TO INTERMEDIATE ( INTEGER ) FORMAT.
C
C         INPUT      : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                      1 CHARACTER PER WORD.
C
C                      IHEAD = 0 INDICATES BULLETIN HEADER NOT ALREADY
C                                WRITTEN TO ERROR FILE.
C                            = 1 MEANS HEADER ALREADY WRITTEN TO ERROR FILE.
C
C                       IERR IS NOT USED ON INPUT.
C
C          OUTPUT     : REPORT IN INTEGER FORMAT IN ARRAY 'KINT' IN
C                       DESIRED FORMAT ( SEE SEPARATE DOCUMENTATION)
C
C                       IERR = 1 IF ANY FILE HANDLING ERROR ENCOUNTERED.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UAINT(IHEAD,IERR)*
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
C         *CALL* *PRTBULL(I,J)*
C         *CALL* *NEXTVAL(I,J)*
C         *CALL* *NEXTPRT(I,J)*
C         *CALL* *EXTINT(I,J)*
C         *CALL* *EXTGRP(I,J)*
C         *CALL* *NEXTFIG(I,J)*
C         *CALL* *FIXUA*
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
      INCLUDE 'comstat.f'
      INCLUDE 'comident.f'
C
      CHARACTER*10 YLOCAT(140),YLOC
C
      DIMENSION ILAT(140) ,ILONG(140), ILOC(10)
C
      DATA NDEF /140 /
C
C
C
      DATA (YLOCAT(I),I=1,60) /
     C    'OTBON'    ,   'EMRAX'    ,   'LENBA'    ,   'NTN'      ,
     C    'URDOG'    ,   'TAM'      ,   'VAMPI'    ,   'SANBI'    ,
     C    'TURLI'    ,   'AMMI'     ,   'SANOK'    ,   'TATIL'    ,
     C    'APCR'     ,   'ABRK'     ,   'DOLIM'    ,   'BIGIL'    ,
     C    'REOLA'    ,   'POMAS'    ,   'ANIVI'    ,   'KALUG'    ,
     C    'LANAG'    ,   'GUPLA'    ,   'PADPA'    ,   'MDG'      ,
     C    'ADGV'     ,   'MCO'      ,   'MANTA'    ,   'BUVLA'    ,
     C    'SAMON'    ,   'CMU'      ,   'SUKAR'    ,   'AHDB'     ,
     C    'KANTO'    ,   'PATKA'    ,   'KIBAG'    ,   'TAPON'    ,
     C    'LEKAT'    ,   'LAMOB'    ,   'UNTOK'    ,   'SAPDA'    ,
     C    'OLENG'    ,   'LAMEP'    ,   'ANIVI'    ,   'SEBRI'    ,
     C    'ELBAR'    ,   'PATLA'    ,   'ADPER'    ,   'TAPOK'    ,
     C    'PALTI'    ,   'OPABA'    ,   'PEARL'    ,   'RULSA'    ,
     C    'ALFA'     ,   'OYST'     ,   'REMOVED'  ,   'LOACH'    ,
     C    'PKS'      ,   'CYYT'     ,   'CYQX'     ,   'GULGO'    /
C
      DATA (YLOCAT(I),I=61,120) /
     C    'PATKA'    ,   'GOLED'    ,   'BUTLA'    ,   'ASAS'     ,
     C    'LOTRA'    ,   'AHPD'     ,   'DOGAR'    ,   'SAPAR'    ,
     C    'UPLOK'    ,   'EMPUL'    ,   'YYT'      ,   'LAGOD'    ,
     C    'OYSTR'    ,   'SABKI'    ,   'SADMI'    ,   'APGN'     ,
     C    'BOLAK'    ,   'OLDIS'    ,   'DOTTI'    ,   'ASMA'     ,
     C    'PBO'      ,   'CARPE'    ,   'OYSTER'   ,   'GULOR'    ,
     C    'JARRA'    ,   'GURNO'    ,   'OKTOM'    ,   'SEMKO'    ,
     C    'VILOK'    ,   'MAKVO'    ,   'MTP'      ,   'SULRA'    ,
     C    'BOTRA'    ,   'EKUSA'    ,   'DOGIS'    ,   'INSREP'   ,
     C    'TAMUS'    ,   'SELNI'    ,   'SEMKU'    ,   'FLO'      ,
     C    'COLOR'    ,   'SCROD'    ,   'YYR'      ,   'ULTEM'    ,
     C    'YQX'      ,   'CYYR'     ,   'ABTL'     ,   'ALBAR'    ,
     C    'BRAVO'    ,   'ABMA'     ,   'UNSOK'    ,   'HOGAN'    ,
     C    'ALGAE'    ,   'MALIM'    ,   'PASKA'    ,   'SBBG'     ,
     C    'UGURA'    ,   'SBFL'     ,   'DOTTY'    ,   'GOTSI'    /
C
      DATA (YLOCAT(I),I=121,140) /
     C    'ANOLA'    ,   'DUGES'    ,   'REDOK'    ,   'RPB'      ,
     C    'FIR'      ,   'YSG'      ,   'AY'       ,   'QX'       ,
     C    'VYSTA'    ,   'SG'       ,   'BANCS'    ,   'YAY'      ,
     C    'SBFZ'     ,   'SBPL'     ,   'KONSA'    ,   'AMMG'     ,
     C    'TOKSO'    ,   'OSCAR'    ,   'LIMA'     ,   'PAKSI'    /
C
C
      DATA (ILAT(I),I=1,60) /
     C    -3392      ,    -1400     ,    -3620     ,    -1767     ,
     C    -3838      ,    -2580     ,    -2083     ,    -3000     ,
     C    -2197      ,    -3423     ,    -3188     ,    -1800     ,
     C    -2487      ,    -2338     ,    -1200     ,    -2547     ,
     C    -3017      ,     2850     ,    -1937     ,    -2538     ,
     C    -1900      ,    -3740     ,    -3547     ,    -3255     ,
     C    -1227      ,    -3760     ,    -3192     ,    -3768     ,
     C    -4067      ,    -2803     ,     1237     ,    -1737     ,
     C      683      ,      800     ,     1100     ,      700     ,
     C    -1800      ,    -1200     ,    -4058     ,    -1200     ,
     C     -300      ,    -1400     ,    -1937     ,    -2367     ,
     C    -3285      ,    -3792     ,    -2888     ,    -3888     ,
     C    -3985      ,     -885     ,    -3800     ,    -2228     ,
     C     5080      ,     2621     ,     5707     ,     5555     ,
     C    -3315      ,     4734     ,     4857     ,    -4053     /
C
      DATA (ILAT(I),I=61,120) /
     C      800      ,    -3912     ,   -3257      ,    -2380     ,
     C    -3360      ,    -2038     ,    -200      ,    -3113     ,
     C    -1200      ,     -800     ,    4766      ,    -3000     ,
     C     5400      ,    -2903     ,   -2550      ,    -2877     ,
     C      600      ,     1190     ,       0      ,    -2073     ,
     C    -2317      ,     5320     ,    2621      ,    -3788     ,
     C    -2050      ,    -3432     ,   -3223      ,    -2508     ,
     C    -1400      ,    -2880     ,   -3413      ,    -4175     ,
     C    -2600      ,     1540     ,   -1400      ,    99999     ,
     C    -2000      ,    -3547     ,   -3442      ,    99999     ,
     C    99999      ,     5460     ,   99999      ,    99999     ,
     C    99999      ,    99999     ,   -1925      ,    -2038     ,
     C     4748      ,    99999     ,   99999      ,     1775     ,
     C    -2113      ,    99999     ,   99999      ,    99999     ,
     C    99999      ,    99999     ,   99999      ,    99999     /
C
      DATA (ILAT(I),I=121,140) /
     C    -2172      ,    99999     ,   99999      ,    99999     ,
     C    99999      ,    99999     ,    5215      ,    99999     ,
     C    99999      ,    99999     ,   99999      ,    99999     ,
     C    99999      ,    99999     ,   99999      ,    -3773     ,
     C    99999      ,     4117     ,   99999      ,    -3618     /
C
C
      DATA (ILONG(I),I=1,60) /
     C   16500       ,    15842     ,    16500     ,    14107     ,
     C   16000       ,    14988     ,    15583     ,    15832     ,
     C   13083       ,    14208     ,    15800     ,    11062     ,
     C   11365       ,    15047     ,    11833     ,    16300     ,
     C   14310       ,    13015     ,    16300     ,    13672     ,
     C   14925       ,    16000     ,    16000     ,    14960     ,
     C   13683       ,    14973     ,    16000     ,    16500     ,
     C   16000       ,    14562     ,    11090     ,    12365     ,
     C   10380       ,    10650     ,    10952     ,    10597     ,
     C   11098       ,    10888     ,    16500     ,    11143     ,
     C   13348       ,    14785     ,    16300     ,    12237     ,
     C   16300       ,    15500     ,    12900     ,    15500     ,
     C   16000       ,    13807     ,    16000     ,    15963     ,
     C     835       ,    12300     ,    5906     ,    -5650     ,
     C   14825       ,     -683     ,   -5474     ,    17000     /
C
      DATA (ILONG(I),I=61,120) /
     C   10650       ,   16500      ,   16000      ,    13388     ,
     C   15415       ,   11862      ,    8785      ,    15800     ,
     C   12137       ,     9308     ,   -5250     ,    17925     ,
     C   -5500       ,    16300     ,   16800     ,    11465     ,
     C    9550       ,    8253      ,   15607      ,    13952     ,
     C   11773       ,    -5400     ,   12300     ,    16500     ,
     C   13332       ,   15415      ,   16000      ,    15410     ,
     C   14223       ,   15500      ,   13533      ,    16500     ,
     C   10892       ,   11400      ,    9830      ,    99999     ,
     C   10333       ,   13000      ,   12500      ,    99999     ,
     C   99999       ,    -5580     ,   99999     ,    99999     ,
     C   99999       ,   99999      ,   14675      ,    15300     ,
     C     608       ,   99999      ,   99999      ,    13283     ,
     C   15800       ,   99999      ,   99999      ,    99999     ,
     C   99999       ,   99999      ,   99999      ,    99999     /
C
      DATA (ILONG(I),I=121,140) /
     C   16300       ,   99999      ,   99999      ,    99999     ,
     C   99999       ,   99999      ,    2100      ,    99999     ,
     C   99999       ,   99999      ,   99999      ,    99999     ,
     C   99999       ,   99999      ,   99999      ,    14078     ,
     C   99999       ,     501      ,   99999      ,    17185     /
C
C     ------------------------------------------------------------------
C*          1. CLEAR FLAGS AND ERROR INDICATOR . RETAIN POINTER TO FIRST
C              ---------------------------------------------------------
C              CHARACTER OF REPORT.
C              --------------------
  100 CONTINUE
C
C     FLAG INDICATING FIRST DECODING ATTEMPT ON REPORT.
C
      IFIRST = 0
      ISLESH = 0
C
C     POINTER TO FIRST CHARACTER
C
      KPT = IPT
C
C     ERROR INDICATOR
C
  101 KERR = 0
C
C
C
C*                1.1  CHECK FOR 'NIL' REPORT.
C                      -----------------------
 110  CONTINUE
C
      LEN = IEQ -IPT
      IF (LEN.LE.25) THEN
                         KERR = 1
                         NOER(2,1) = NOER(2,1) + 1
C
C-----                   PRINT 'NIL' AIRCRAFT REPORTS
C-----
C-----                   CALL PRTBULL (IPT,IEQ)
C-----
C-----                   PRINT 'NIL' AIRCRAFT REPORTS
C
                         KDEC(20)=1
C
                         RETURN
                     END IF
C
C
C*               1.2 SET AREA FOR INTERMEDIATE FORM OF REPORT
C                    ----------------------------------------
C                    TO MISSING DATA VALUE.
C                    ----------------------
 120  CONTINUE
C
      DO 121 I=6,25
         KINT(I) = MINDIC
  121 CONTINUE
C
C
C     WORDS 1-65 ARE SET TO CCITT 5 'XXX  ' .AIRCRAFT CALL SIGN ( IF
C     IT EXISTS ) OVERWRITES THESE WORDS.
C
      DO 122 I=1,3
         IDENT(I) = 88
  122 CONTINUE
C
      IDENT(4) = 32
      IDENT(5) = 32
      IDENT(6) = 32
      IDENT(7) = 32
      IDENT(8) = 32
      IDENT(9) = 32
C
C
C     PRESET MINUTES OF LATITUDE AND LONGITUDE TO 0.
C
      KINT(7)  = 0
      KINT(10) = 0
C
C     PRESET AUTOMATIC REPORT INDICATOR TO 0 .
C
      KINT(24) = 0
C
C
C
C*               1.3 CLEAR DATA FOR PREVIOUS REPORT FROM
C                    -----------------------------------
C                    DECODED REPORT HEADER AREA.
C                    ---------------------------
 130  CONTINUE
C
      DO 131 I=1,3
         KDEC(I) = MINDIC
  131 CONTINUE
C
      DO 132 I=5,9
         KDEC(I) = MINDIC
  132 CONTINUE
C
C     CLEAR FLAG FIELDS , RETAINING 'COR' FLAG.
C
      KDEC(13) = 0
C
      KDEC(15) = IAND(KDEC(15),4)
C
      KDEC(16) = MINDIC
C
      KDEC(17) = MINDIC
C
      KDEC(20) = 0
C
      DO 133 I=22,24
         KDEC(I) = MINDIC
  133 CONTINUE
C
C
C*                   1.4 TEST FOR CALL SIGN - FIRST CHARACTER NON-NUMERIC.
C                        -------------------------------------------------
C     Note that extraction of ipipip (ASC etc) has been done in bullua
 140  CONTINUE
C
      IF ( KCHAR(IPT).LT.48.OR.KCHAR(IPT).GT.57)
     C    THEN
C
C             INSERT FIRST 5 CHARACTERS OF CALL SIGN IN WORDS 1-5.
C
C             ANY 'SPACE' IS IGNORED IF NO FIGURE IS ENCOUNTERED .
C             CALL SIGN NORMALLY CONSISTS OF 2 LETTERS AND FIGURES ,
cps There are exceptions, like SVHZAPN, AFZA41 and JP9Z45VV

C             BUT SOME CENTRES LEAVE A 'SPACE' BETWEEN LETTERS AND
C             FIGURES.
C             E.G.  AF13 , AF 13 .
cps Never seen this in practice, so probably we could simplify code here
C
              J = 1
              IFIG = 0
              DO 141 K=1,8
                     IF (KCHAR(IPT).GE.48.AND.KCHAR(IPT).LE.57)IFIG=1
cpsmai06                     IF (IFIG.EQ.0.AND.KCHAR(IPT).EQ.32) GO TO 142
                     IF (IFIG.EQ.0.AND.KCHAR(IPT).EQ.32.and.k.lt.4)
     C                    GO TO 142
                     if (ifig.eq.0.and.kchar(ipt).eq.13) then
c
c                        skip this group of characters
c
                        call nextprt(ipt,ieq)
                        go to 140
                     end if
c
                     IF (KCHAR(IPT).EQ.32) GO TO 145
                     if (kchar(ipt).eq.13) go to 145
                     if (kchar(ipt).eq.10) go to 145
                     IDENT(J) = KCHAR(IPT)
                     J = J + 1
  142                IPT = IPT + 1
  141         CONTINUE
C
C             CHECK FOR AUTOMATIC REPORT ( LETTER 6 OF CALL SIGN A Z )
CRR N.B. Should we have definition of an automatic report for AMDARs?
C
              KINT(24) = 0
cpsfeb07              IF ( KCHAR(IPT).EQ.90 ) KINT(24) = 1
         IF (KDEC(4).EQ.141.AND.KCHAR(IPT).EQ.90) KINT(24) = 1
C
C             LOCATE LAT/LONG GROUP . SOME REPORTS DO NOT HAVE
C             ABBREVIATED CALL SIGNS , SO IF NO FIGURE HAS BEEN
C             ENCOUNTERED THE LAT/LONG GROUP IS THE FIRST GROUP
C             AFTER THE NEXT FIGURE .
C             E.G.  ASCOTT 123 , KIWI 123 .
C
              IF ( IFIG.EQ.0 ) CALL NEXTFIG (IPT,IEQ)
              CALL NEXTVAL (IPT,32,IEQ)
C
  145         CONTINUE
C
              CALL NEXTPRT (IPT,IEQ)
C
          ELSE
C
C             MISSING CALL SIGN .
C
              IF (IFIRST.EQ.0) NOER(2,2) = NOER(2,2) + 1
C
C
          END IF
C
C
C*                1.5   CONVERT LAT/LONG GROUP.
C                       -----------------------
C
 150  CONTINUE
C
C     IF THE CHARACTER ENCOUNTERED IS A LETTER , LAT/LONG IS
C     GIVEN BY A PLACE NAME AND LAT/LONG MUST BE TAKEN FROM THE
C     TABLES OF LAT/LONG/LOCATION INDICATORS.
C
C     FIRST CHECK IF LATITUDE ONLY IS GIVEN BY 'EQ' , DENOTING
C     EQUATOR.
C
      ISEQU = 0
      IF (KCHAR(IPT).GE.65.AND.KCHAR(IPT+1).LE.90) ISEQU = 1
C
      IF (KCHAR(IPT).EQ.69.AND.KCHAR(IPT+1).EQ.81.AND.
     C    KCHAR(IPT+2).EQ.32)
     C          THEN
                    KINT(6) = 0
                    KINT(8) = 0
                    ISEQU = 2
                    IPT = IPT + 2
                ENDIF
C
      IF ( ISEQU.EQ.1 )
     C    THEN
C
C             SET ARRAY FOR LOCATION CHARACTERS TO 'SPACES'.
C
              DO 151 I=1,10
                     ILOC(I) = 32
  151         CONTINUE
C
C             CHECK LOCATION INDICATOR.
C
              I1 = IPT
              I2 = IPT
              CALL NEXTVAL (I2,32,IEQ)
              I2 = I2 - 1
              IF((I2-I1).GE.10) THEN
                 KERR=4
                 GO TO 9000
              END IF
C
              K = 0
C
              DO 152  I=I1,I2
                      K = K + 1
                      ILOC(K) = KCHAR(I)
                      IF ( ILOC(K).EQ.32 ) GO TO 153
 152          CONTINUE
C
 153          CONTINUE
C
              YLOC=' '
              DO 154 I=1,10
              YLOC(I:I)=CHAR(ILOC(I))
 154          CONTINUE

C
              DO 155  I=1,NDEF
                     IF (YLOC.EQ.YLOCAT(I)) GO TO 156
 155          CONTINUE
C
C             LOCATION INDICATOR NOT IN TABLE.
C
              CALL NEXTVAL (IPT,32,IEQ)
              KCHAR(IPT) = IOR(KCHAR(IPT),128)
              KERR = 4
              IF (IFIRST.EQ.0) NOER(2,4) = NOER(2,4) + 1
              GO TO 9000
C
C
C             INSERT LATITUDE .
C
C             RETURN IF LOCATION IDENTIFIER IS RECOGNISED BUT
C             COORDINATES NOT KNOWN.
C
 156          IF (ILAT(I).EQ.99999 )
     C                 THEN
                           NOER(2,4) = NOER(2,4) + 1
                           KERR = 4
                           RETURN
                       ENDIF
C
              KINT(6) =ILAT(I)
              KINT(8) = 0
C
C             INSERT LONGITUDE .
C
              KINT(9) =ILONG(I)
              KINT(11) = 0
C
C             MOVE WORKING POINTER PAST LOCATION INDICATOR GROUP.
C
              CALL NEXTVAL (IPT,32,IEQ)
C
C
          ELSE
C
C
C             CHECK IF LATITUDE ALREADY GIVEN BY 'EQ'.
C
              IF ( ISEQU.EQ.2 ) GO TO 157
C
C             SOME CENTRES USE 3 FIGURES FOR DEGREES OF LATITUDE.
C             THE EXTRA FIGURE IS A LEADING  0 .
C
C             CHECK FOR LEADING 0 WHEN LATITUDE IS GIVEN IN
C             WHOLE DEGREES.
C
              IF ( KCHAR(IPT).EQ.48.AND.KCHAR(IPT+1).GE.48.AND.
     C             KCHAR(IPT+1).LE.57.AND.KCHAR(IPT+2).GE.48.AND.
     C             KCHAR(IPT+2).LE.57.AND.KCHAR(IPT+3).GE.65)
     C               IPT = IPT + 1
C
C              CHECK FOR LEADING 0 WHEN LATITUDE IS IN DEGREES
C              AND MINUTES .
C
              IF ( KCHAR(IPT).EQ.48.AND.KCHAR(IPT+1).GE.48.AND.
     C             KCHAR(IPT+1).LE.57.AND.KCHAR(IPT+2).GE.48.AND.
     C             KCHAR(IPT+2).LE.57.AND.KCHAR(IPT+3).GE.48.AND.
     C             KCHAR(IPT+3).LE.57.AND.KCHAR(IPT+4).GE.48.AND.
     C             KCHAR(IPT+4).LE.57.AND.KCHAR(IPT+5).GE.65)
     C               IPT = IPT + 1
C
C              CONVERT DEGREES OF LATITUDE.
C
               CALL EXTINT(IPT,2,6)
               IF ( KINT(6).EQ.MINDIC)
     C              THEN
                        IF ( IFIRST.EQ.0 ) NOER(2,4) = NOER(2,4) + 1
                        KERR = 4
                        IPT=IABS(IPT)
                        KCHAR(IPT) = IOR(KCHAR(IPT),128)
                        GO TO 9000
                    END IF
C
C
C
C             TEST FOR MINUTES OF LATITUDE ( NEXT CHARACTER NOT 'N'
C             OR 'S' OR 'SPACE' .
C
              IF (KCHAR(IPT).NE.78.AND.KCHAR(IPT).NE.83.AND.
     C            KCHAR(IPT).NE.32)
     C             THEN
                       CALL EXTINT (IPT,2,7)
                       IF ( KINT(7).EQ.MINDIC)
     C                   THEN
                           IF ( IFIRST.EQ.0 ) NOER(2,4)=NOER(2,4)+1
                           KERR = 4
                           IPT=IABS(IPT)
                        KCHAR(IPT) = IOR(KCHAR(IPT),128)
                           GO TO 9000
                         END IF
                   END IF
C
C
C             'N' (+) OR 'S' (-) .
C
              IF (KCHAR(IPT).EQ.78) KINT(8) = 1
              IF (KCHAR(IPT).EQ.83) KINT(8) = -1
C
C             CHECK FOR LATITUDE OF 00 WITH MISSING 'N' OR 'S'.
C
              IF (KINT(8).EQ.MINDIC.AND.KINT(6).EQ.0) KINT(8)=1
C
              IF (KINT(8).EQ.MINDIC)
     C                   THEN
                           IF ( IFIRST.EQ.0 ) NOER(2,4)=NOER(2,4)+1
                           KERR = 4
                           IPT=IABS(IPT)
                           KCHAR(IPT) = IOR(KCHAR(IPT),128)
                           GO TO 9000
                         ELSE
                             IPT=IPT+1
                         END IF
C
C
C             CONVERT DEGREES OF LONGITUDE . SOME CENTRES USE ONLY
C             2 DIGITS WHEN LONGITUDE IS LESS THAN 100 .
C
 157          CALL NEXTPRT ( IPT,IEQ )
              N = 3
              if(kchar(ipt+2).eq.69.or.kchar(ipt+2).eq.87.or. 
     1           kchar(ipt+4).eq.69.or.kchar(ipt+4).eq.87) n=2
c
c             IF (KCHAR(IPT+3).EQ.32.OR.KCHAR(IPT+5).EQ.32) N=2
              CALL EXTINT (IPT,N,9)
              IF ( KINT(9).EQ.MINDIC )
     C                   THEN
                           IF ( IFIRST.EQ.0 ) NOER(2,4)=NOER(2,4)+1
                           KERR = 4
                           IPT=IABS(IPT)
                           KCHAR(IPT) = IOR(KCHAR(IPT),128)
                           GO TO 9000
                         END IF
C
C             TEST FOR MINUTES OF LONGITUDE ( NEXT CHARACTER NOT
C             'E' OR 'W' OR 'SPACE' . FIRST CORRECT IF INCORRECT
C             FIGURE-SHIFT.
C
              IF (KCHAR(IPT).EQ.51.AND.KCHAR(IPT+1).EQ.32)
     C                KCHAR(IPT) = 69
              IF (KCHAR(IPT).EQ.50.AND.KCHAR(IPT+1).EQ.32)
     C                KCHAR(IPT) = 87
C
              IF ( KCHAR(IPT).NE.69.AND.KCHAR(IPT).NE.87 )
     C             THEN
                       CALL EXTINT (IPT,2,10)
                       IF ( KINT(10).EQ.MINDIC )
     C                     THEN
                               IF (IFIRST.EQ.0) NOER(2,4)=NOER(2,4)+1
                               KERR = 4
                               IPT=IABS(IPT)
                               KCHAR(IPT) = IOR(KCHAR(IPT),128)
                               GO TO 9000
                           END IF
C
                   END IF
C
C
C             'E' (+) OR 'W' (=).
C
              IF (KCHAR(IPT).EQ.69) KINT(11) = 1
              IF (KCHAR(IPT).EQ.87) KINT(11) = -1
C
C             CHECK FOR LONGITUDE OF 180 WITH MISSING 'E' OR 'W'.
C
              IF (KINT(11).EQ.MINDIC.AND.KINT(9).EQ.180)KINT(11)=1
C
              IF (KINT(11).EQ.MINDIC)
     C             THEN
                               KERR = 4
                               IPT=IABS(IPT)
                               KCHAR(IPT) = IOR(KCHAR(IPT),128)
                               GO TO 9000
                           ELSE
                               IPT=IPT+1
                           END IF
          END IF
C
C
C*           1.6 LOCATE AND CONVERT TIME GROUP . THIS IS NORMALLY HHMM BUT
C                SOME REPORTS GIVE ONLY THE MINUTES (MM) . IN THIS CASE
C                THE HOURS VALUE (HH) IS TAKEN FROM THE BULLETIN HEADER ( -1
C                IF MINUTES IN REPORT IS GREATER THAN MINUTES IN HEADER ).
C                OCCASIONALLY THE DAY IS ALSO INCLUDED , AND IN THIS CASE
C                THE POINTER IS MOVED PAST THE DAY TO THE HH FIGURES.
C                TIME GROUP IS FOLLOWED BY '/' OR 'SPACE'.
C
C
      CALL NEXTPRT ( IPT,IEQ )
C
      K = IPT
      CALL NEXTVAL ( K,32,IEQ )
      K = K - IPT
      J = IPT
      CALL NEXTVAL ( J,47,IEQ )
      J = J -IPT
      IF ( K.GT.J ) K = J
      IF ( K.EQ.6 ) IPT = IPT + 2
C
      IF ( KCHAR(IPT+2).EQ.47.OR.KCHAR(IPT+2).EQ.32 )
     C      THEN
                KINT(12) = KDEC(11)
                CALL EXTGRP ( IPT,2,0,0,0,0,13,IRET)
                IF ( KINT(13).GT.KDEC(12) ) KINT(12) = KINT(12) - 1
                IF ( KINT(12).EQ.-1 ) KINT(12)=23
            ELSE
                CALL EXTGRP ( IPT,2,2,0,0,0,12,IRET )
            ENDIF
C
C
      IF (KINT(12).LT.0.OR.KINT(12).GT.23) IRET = 1
      IF (KINT(13).LT.0.OR.KINT(13).GT.59) IRET = 1
      IF (IRET.NE.0)
     C    THEN
              IPT=IABS(IPT)
              KCHAR(IPT) = IOR(KCHAR(IPT),128)
              KERR = 3
              IF (IFIRST.EQ.0) NOER(2,3) = NOER(2,3) + 1
              GO TO 9000
          END IF
C
C*           1.7 CONVERT FLIGHT LEVEL . THIS IS GIVEN IN 3 FIGURES
C                IN THE NEXT GROUP AND IS NORMALLY PRECEDED BY
C                THE LETTER 'F' or 'A' . A NUMBER OF VARIATIONS ARE
C                F330 , F 330 , FL330 , FL 330 , /330 , 330 , F/L 330 .
C                TO ACCOMMODATE THESE VARIATIONS THE NEXT FIGURE IS LOCATED.
C
 170  CONTINUE
cpsfeb07 Next line replaced with new code to treat negative flight levels
C     Need to know if flight level is preceded by 'A' or 'F', so store
C     the (first) letter preceding flight level in KINT(14), unless this
C     is different from 'A' and 'F', in which case flight level is
C     assumed missing
c$$$      CALL NEXTFIG ( IPT,IEQ )
      K = IPT
      CALL NEXTLET ( K,IEQ )
      CALL NEXTFIG ( IPT,IEQ )

      IF (K.LT.IPT) THEN     ! Letter before next number
         IF (KCHAR(K).EQ.65 .OR. KCHAR(K).EQ.70) THEN   ! 'A' or 'F'
            KINT(14) = KCHAR(K)
         ELSE
            IPT = K
            GO TO 185           ! No flight level
         END IF
      END IF

      CALL EXTINT (IPT,3,15)
      IF ( KINT(15).EQ.MINDIC )
     C              THEN
                      IPT = IABS(IPT)
                      KCHAR(IPT) = IOR(KCHAR(IPT),128)
                      KERR = 4
                      IF (IFIRST.EQ.0) NOER(2,4)=NOER(2,4)+1
                      GO TO 9000
                    END IF
C
C
C
C
C*                  1.8  LOCATE AND CONVERT TEMPERATURE GROUP
cpsfeb07                        ( 'S' PRECEDED BY 'P' OR 'M' )
c                   ('P' OR 'M' in airep, followed by 'S' in amdar)
C
 180  CONTINUE
C
 185  CALL NEXTPRT (IPT,IEQ)
      IF (IPT.GE.IEQ)
     C   THEN
             KCHAR(IPT) = IOR(KCHAR(IPT),128)
             KERR = 5
             IF (IFIRST.EQ.0) NOER(2,5)=NOER(2,5)+1
             GO TO 9000
         END IF
C
C     CHECK FOR 'PS' OR 'MS' OR 'P' OR 'M'.
C
      IF ((KCHAR(IPT).EQ.80.OR.KCHAR(IPT).EQ.77).AND.KCHAR(IPT+1).
     C    GE.48.AND.KCHAR(IPT+1).LE.57) THEN
          INCR=1
          GO TO 181
      END IF
      IF ((KCHAR(IPT).EQ.80.OR.KCHAR(IPT).EQ.77).AND.KCHAR(IPT+1).
     C    EQ.83) THEN
          INCR=2
          GO TO 181
      END IF
C
cps     After temperature group, there might follow dew point temperature
cps     or humidity. Have never seen this in any report, however.

C     CHECK IF GROUP IS A WIND GROUP . ANY OTHER GROUPS ARE SKIPPED.
cps   For amdars this means that TBBa and Ss1s2s3 group and 333 section is skipped
C
C
      IF (KCHAR(IPT).GE.48.AND.KCHAR(IPT).LE.57)
     C       THEN
C
C                IF GROUP STARTS WITH A FIGURE AND 4TH CHARACTER IS /
C                IT IS A WIND GROUP.
C
                 ISLESH=0
                 IF (KCHAR(IPT+3).EQ.47) GO TO 182
                 III=IPT
                 CALL NEXTSEP(III,IEQ)
                 IIIIPT=III-IPT
cpsfeb07 Must test for dddfff being last group before '='
                 IF (III.EQ.IEQ+1) IIIIPT = IIIIPT - 1
                 IF(IIIIPT.GE.4.AND.IIIIPT.LE.6) THEN
                    ISLESH=1        ! 4-6 chars, no '/', i.e. dddf(f(f))
                    GO TO 182
                 END IF 
C
             ELSE
C
C                IF GROUP IS 'LV' OR 'L/V' IT IS A WIND GROUP.
C
                 IF (KCHAR(IPT).EQ.76.AND.KCHAR(IPT+1).EQ.47)GO TO 182
                 IF (KCHAR(IPT).EQ.76.AND.KCHAR(IPT+1).EQ.86)GO TO 182
C
C                IF GROUP IS 'CALM' IT IS A WIND GROUP.
C
                 IF (KCHAR(IPT).EQ.67.AND.KCHAR(IPT+1).EQ.65.AND.
     C            KCHAR(IPT+2).EQ.76.AND.KCHAR(IPT+3).EQ.77) GO TO 182
C
C                IF GROUP IS 'VRB' IT IS A WIND GROUP.
C
                 IF (KCHAR(IPT).EQ.86.AND.KCHAR(IPT+1).EQ.82.AND.
     C            KCHAR(IPT+2).EQ.66) GO TO 182
C
             ENDIF
C
C     SKIP GROUP.
C
      CALL NEXTVAL (IPT,32,IEQ)
      GO TO 185
C
C
C     'P' ( + )   OR 'M'  ( - )
C
 181  IF (KCHAR(IPT).EQ.80 ) KINT(18) = 1
      IF (KCHAR(IPT).EQ.77 ) KINT(18) = -1
C
C     TEMPERATURE
C
      IPT=IPT+INCR
      CALL NEXTPRT ( IPT,IEQ )
crr
      if (kdec(4).eq.141) then
crr AIREP:temperature given in whole degrees, 2 numbers
         CALL EXTINT (IPT,2,19)
      elseif (kdec(4).eq.144) then
crr AMDAR:temperature given in tenths of a degrees Celsius, 3 numbers
         CALL EXTINT (IPT,3,19)
      else
         print*,'ERROR: UAINT - kdec(4)=',kdec(4)
      endif
crr
C
 183  IF (IPT.LT.0)
     C    THEN
              IPT = IABS(IPT)
              KCHAR(IPT) = IOR(KCHAR(IPT),128)
              KERR = 5
              IF (IFIRST.EQ.0) NOER(2,5)=NOER(2,5)+1
          END IF
C
C
C
C*               1.9  LOCATE AND CONVERT WIND GROUP.
C                     ------------------------------
 190  CONTINUE
C
      CALL NEXTSEP (IPT,IEQ)
      CALL NEXTPRT (IPT,IEQ)
      IF (IPT.GE.IEQ) GO TO 9000
C
C
C     CHECK IF GROUP IS   L/V  . CONVERTED AS 990/02  .
C
      IF (KCHAR(IPT).EQ.76)
     C    THEN
              KINT(20) = 990
              KINT(21) = 2
              GO TO 9000
          END IF
C
C
C     CHECK IF GROUP IS 'CALM' . CONVERTED AS 000/00 .
C
      IF (KCHAR(IPT).EQ.67)
     C    THEN
              KINT(20) = 0
              KINT(21) = 0
              GO TO 9000
          END IF
C
C     CHECK IF DDD IS GIVEN AS 'VRB'.
C
      IF (KCHAR(IPT).EQ.86)
     C   THEN
            KINT(20) = 990
            CALL NEXTFIG (IPT,IEQ)
            GO TO 191
         END IF
C
      IF (KCHAR(IPT).GE.48.AND.KCHAR(IPT).LE.57) THEN
C
C       IF GROUP STARTS WITH A FIGURE AND 4TH CHARACTER IS /
C       IT IS A WIND GROUP.
C
        ISLESH=0
        IF (KCHAR(IPT+3).EQ.47) GO TO 182
        III=IPT
        CALL NEXTSEP(III,IEQ)
        IIIIPT=III-IPT
cpsfeb07 Must test for dddfff being last group before '='
        IF (III.EQ.IEQ+1) IIIIPT = IIIIPT - 1
        IF(IIIIPT.GE.4.AND.IIIIPT.LE.6) THEN
           ISLESH=1        ! 4-6 chars, no '/', i.e. dddf(f(f))
           GO TO 182
        ELSE
C
C          SKIP THAT GROUP
C
           IPT=III
           GO TO 190
C
        END IF 
C
      ELSE
C
C         IF GROUP IS 'LV' OR 'L/V' IT IS A WIND GROUP.
C
          IF (KCHAR(IPT).EQ.76.AND.KCHAR(IPT+1).EQ.47)GO TO 182
          IF (KCHAR(IPT).EQ.76.AND.KCHAR(IPT+1).EQ.86)GO TO 182
C
C         IF GROUP IS 'CALM' IT IS A WIND GROUP.
C
          IF (KCHAR(IPT).EQ.67.AND.KCHAR(IPT+1).EQ.65.AND.
     C    KCHAR(IPT+2).EQ.76.AND.KCHAR(IPT+3).EQ.77) GO TO 182
C
C         IF GROUP IS 'VRB' IT IS A WIND GROUP.
C
          IF (KCHAR(IPT).EQ.86.AND.KCHAR(IPT+1).EQ.82.AND.
     C    KCHAR(IPT+2).EQ.66) GO TO 182
cps This means that the next figures will be coded as ddd, which seems
cps strange (isn't VRB used instead of ddd?). Where is the documentation
cps of aireps?
C
C         WIND GROUP GAN BE /////// MISSING
C
          IF (KCHAR(IPT).EQ.47.AND.KCHAR(IPT+1).EQ.47.AND.
     C    KCHAR(IPT+2).EQ.47.AND.KCHAR(IPT+3).EQ.47) GO TO 9000
      ENDIF
C
C     CONVERT DDD TO INTEGER. FIND THE POSITION OF THE NEXT
C     FIGURE , AS SOMETIMES THE WORD 'SPOT' OR 'SPT' OR 'S'
C     PRECEDES THE WIND GROUP.
C
 182  CALL NEXTFIG (IPT,IEQ)
      IF (IPT.GE.IEQ) GO TO 9000
      CALL EXTINT(IPT,3,20)
      IF ( KINT(20).EQ.MINDIC)
     C    THEN
             IPT=IABS(IPT)
             KCHAR(IPT) = IOR(KCHAR(IPT),128)
             KERR = 6
             IF (IFIRST.EQ.0) NOER(2,6)=NOER(2,6)+1
          END IF
C
C     FFF CAN BE 1,2 OR 3 FIGURES , SO CONVERTED SEPARATELY.
C     SOME CENTRES LEAVE A 'SPACE' BETWEEN / AND FFF .
C
 191  IF(ISLESH.NE.1) IPT = IPT+1
      CALL NEXTFIG (IPT,IEQ)
      CALL EXTINT (IPT,1,21)
      IF ( KINT(21).EQ.MINDIC)
     C    THEN
             IPT=IABS(IPT)
             KCHAR(IPT) = IOR(KCHAR(IPT),128)
             KERR = 6
             IF (IFIRST.EQ.0) NOER(2,6)=NOER(2,6)+1
          END IF
C
      CALL EXTINT (IPT,1,22)
      CALL EXTINT (IPT,1,23)

C     There should be no more data in airep except possibly debrief remarks
C     For amdar we continue to extract some more data

      IF (KDEC(4).EQ.144) THEN    ! amdar
         
C*       Turbulence
C        ----------

         CALL NEXTSEP (IPT,IEQ)
         CALL NEXTPRT (IPT,IEQ)
         IF (IPT.GE.IEQ) GO TO 9000

         IF (KCHAR(IPT).EQ.84.AND.KCHAR(IPT+1).EQ.66) THEN   ! TB
            IPT = IPT + 2
            CALL EXTINT(IPT,1,25)    ! BA (Turbulence)

            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ)
            IF (IPT.GE.IEQ) GO TO 9000
         END IF

C*       Type of navigation system etc
C        -------------------------


         IF (KCHAR(IPT).EQ.83) THEN   ! Ss1s2s3
            CALL EXTGRP ( IPT+1,1,1,1,0,0,26,IRET )
            IF (IRET.NE.0) THEN
               KCHAR(IPT) = IOR(KCHAR(IPT),128)
               KERR = 7
               IF (IFIRST.EQ.0) NOER(2,7)=NOER(2,7)+1
            END IF

            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ)
            IF (IPT.GE.IEQ) GO TO 9000
         END IF

C*       Section 3
C        ---------

         IF (KCHAR(IPT).EQ.51.AND.KCHAR(IPT+1).EQ.51.AND.
     *        KCHAR(IPT+2).EQ.51.AND.KCHAR(IPT+3).EQ.32) THEN  ! 333
 
            CALL NEXTSEP (IPT,IEQ)
            CALL NEXTPRT (IPT,IEQ)
            IF (IPT.GE.IEQ) GO TO 9000

            IF (KCHAR(IPT).EQ.70) THEN ! Fhdhdhd
               CALL EXTGRP ( IPT+1,3,0,0,0,0,29,IRET )
               IF (IRET.NE.0) THEN
                  KCHAR(IPT) = IOR(KCHAR(IPT),128)
                  KERR = 8
                  IF (IFIRST.EQ.0) NOER(2,8)=NOER(2,8)+1
               END IF

               CALL NEXTSEP (IPT,IEQ)
               CALL NEXTPRT (IPT,IEQ)
               IF (IPT.GE.IEQ) GO TO 9000
            END IF
            
            IF (KCHAR(IPT).EQ.86.AND.KCHAR(IPT+1).EQ.71) THEN ! VGfgfgfg
               CALL EXTGRP ( IPT+2,3,0,0,0,0,30,IRET )
               IF (IRET.NE.0) THEN
                  KCHAR(IPT) = IOR(KCHAR(IPT),128)
                  KERR = 8
                  IF (IFIRST.EQ.0) NOER(2,9)=NOER(2,9)+1
               END IF
            END IF

         END IF   ! end section 3
      END IF      ! end special decoding for amdar

C
C     RETURN IF NO ERROR ENCOUNTERED.
C
C
C
 9000 IF (KERR.EQ.0) RETURN
C
C     IF FIRST DECODING ATTEMPT , TRY TO CORRECT ERRORS AND
C     DECODE AGAIN.
C
      IF (IFIRST.EQ.0)
     C    THEN
              IPT = KPT
              CALL FIXUA
              IFIRST = 1
              GO TO 101
          ELSE
              CALL SAVREP(IHEAD,IERR)
C
C             CLEAR ERROR INDICATOR ,IF THE ERROR IS NOT IN
C             LOCATION OR TIME .
C
              IF ( KERR.GT.4 ) KERR = 0
C
C             WRITE REPORT TO ERROR FILE FOR VISUAL INSPECTION.
C
              RETURN
          END IF
C
C
      RETURN
      END
      SUBROUTINE FIXUA
C
C**** *FIXUA*
C
C
C     PURPOSE.
C     --------
C         ERROR FIXING ROUTINE FOR AIREP REPORTS.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *FIXUA*
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
C     ------------------------------------------------------------------
C
C*          1.   CLEAR PARRITY BIT.                .
C                ------------------
 100  CONTINUE
C
C
      DO 101 I=KPT,IEQ
         KCHAR(I) = IAND(KCHAR(I),127)
  101 CONTINUE
C
C
C     KWBC INSERT //// IF NO TEMPERATURE IS REPORTED.
C     GROUP REPLACED BY MS// .
C
      DO 102 I=KPT,IEQ
         IF (KCHAR(I).EQ.47.AND.KCHAR(I+1).EQ.47.AND.KCHAR(I+2)
     C      .EQ.47.AND.KCHAR(I+3).EQ.47 )
     C            THEN
                      KCHAR(I) = 77
                      KCHAR(I+1) = 83
                  END IF
  102 CONTINUE
C
C
C
      RETURN
      END
      SUBROUTINE UADEC ( IHEAD,IERR )
C
C**** *UADEC*
C
C
C     PURPOSE.
C     --------
C         CONVERSION FROM INTEGER REPRESENTATION TO FULLY DECODED
C         FORMAT.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *UADEC(IHEAD,IERR)*
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
C         *CALL* *EXBAIR (IERR)*
C         *CALL* *KTOMPSI(I)*
C         *CALL* *QCAIREP(IERR)*
C         *CALL* *BUFRENC(IERR)*          
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
      INCLUDE 'comstat.f'
      include 'comwt.f'
C
      DIMENSION IDM(12)
      DATA IDM/31,28,31,30,31,30,31,31,30,31,30,31/
C     ------------------------------------------------------------------
C*          1.   DECODE INTO DECODED FORMAT.       .
C                ---------------------------
 100  CONTINUE
C
c     call prtkint(kint,1,30,mindic)
C
C     CLEAR ERROR INDICATOR
C
      KERR = 0
C
C     SET DECODED REPORT AREA TO MISSING DATA VALUE.
C
      DO 101 I=25,32
         KDEC(I) = MINDIC
  101 CONTINUE
C
C     ------------------------------------------------------------------
C*          1.2  Y'Y' IS NOT REPORTED.
C                ---------------------
 120  CONTINUE
C
C     ------------------------------------------------------------------
C*          1.3  G'G'. HOUR FROM REPORT
C                -----
 130  CONTINUE
C
      KDEC(2) = KINT(12)
      KDEC(1) = KDEC(10) 
      IF(KINT(12).GT.KDEC(11)) THEN
         KDEC(1)=KDEC(1)-1
         IF(KDEC(1).EQ.0) THEN
            CALL DATUM(KDEC(10),IMONTH,IYEAR)
cpsapr98            IF(IMONTH.EQ.1) THEN
cpsapr98               KDEC(1)=31
cpsapr98            END IF
cpsapr98            IF(IMONTH.EQ.3) THEN
cpsapr98               KDEC(1)=28
cpsapr98               IF(MOD(IYEAR,4).EQ.0) KDEC(1)=29
cpsapr98            END IF
            IF(IMONTH.EQ.1) THEN
               KDEC(1)=IDM(12)
            ELSE
               KDEC(1)=IDM(IMONTH-1)     
            END IF
cpsapr98
            IF(IMONTH.EQ.3) THEN
               IF(MOD(IYEAR,400).EQ.0) then
                  KDEC(1)=29
               else if(mod(iyear,4).eq.0 .and. mod(iyear,100).ne.0)then
                  KDEC(1)=29
               end if
            end if
cps
         END IF
      END IF 
C
C     ------------------------------------------------------------------
C*          1.4  LATITUDE.
C                ---------
 140  CONTINUE
C
      IF (KINT(8).EQ.0)
     C    THEN
              KDEC(5) = KINT(6)
          ELSE
              KDEC(5) = (KINT(6)*100 +(100*KINT(7))/60) * KINT(8)
          END IF
      IF(KDEC(5).LT.-9000.OR.KDEC(5).GT.9000) THEN
         KERR=4
         PRINT*,' UADEC : ERROR IN LATITUDE: ',kdec(5)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          1.5  LONGITUDE.
C                ----------
 150  CONTINUE
C
      IF (KINT(11).EQ.0)
     C    THEN
              KDEC(6) = KINT(9)
          ELSE
              KDEC(6) = (KINT(9)*100 +(100*KINT(10))/60) * KINT(11)
          END IF
      IF(KDEC(6).LT.-18000.OR.KDEC(6).GT.18000) THEN
         KERR=4
         PRINT*,' UADEC : ERROR IN LONGITUDE: ',kdec(6)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          1.6  AIRCRAFT CALL SIGN.
C                -------------------
 160  CONTINUE
C
C
C     FIVE CHARATERS IN KINT(1) - KINT(5)  IN CCITT 5 .
C
C
C     ------------------------------------------------------------------
C*          1.7  ALTITUDE.
C                ---------
 170  CONTINUE
C
cpsnov08 KDEC(8) = INT( KINT(15) * 30.48 + 0.5 )
      IF (KINT(15).NE.MINDIC) THEN
         KDEC(8) = INT( KINT(15) * 30.48 + 0.5 )
         IF (KINT(14).EQ.65)    ! 'A', meaning below the 1013.2 hPa
     *        KDEC(8) = - KDEC(8)
      END IF
cc      print*,' uadec - kint(15),kdec(8) ',kint(15),kdec(8)
C
C     ------------------------------------------------------------------
C*          1.8  LITTLE G'G'.
C                ------------
 180  CONTINUE
C
      KDEC(9) = KINT(13)
C
C     ------------------------------------------------------------------
C*          1.9  PPP.
C                ----
 190  CONTINUE
C
cpsnov08 Added check for missing value
      IF (KDEC(8).NE.MINDIC) THEN
         Z = KDEC(8)
crr Pressure returned from function P(z) in mb?
crr      Z = P(Z) * 10.0
         Z = P(Z) * 100.0
C
         KDEC(25) = INT ( Z + 0.5 )
      END IF
cc      print*,' uadec - z,kdec(25) ',z,kdec(25)
C
C     ------------------------------------------------------------------
C*          2.   TT.
C                ---
 200  CONTINUE
C
      IF (KINT(19).NE.MINDIC.AND.KINT(18).NE.MINDIC)
     C    THEN
crr
         if (kdec(4).eq.141) then
crr AIREP
              KDEC(26) = KINT(19)*10*KINT(18)
         elseif (kdec(4).eq.144) then
crr AMDAR
              KDEC(26) = KINT(19)*KINT(18)
         endif
      END IF
C
C
C     ------------------------------------------------------------------
C*          2.1  DD.
C                ---
 210  CONTINUE
C
      KDEC(27) = KINT(20)
cpsjun99      IF ( KDEC(27).EQ.990 ) KDEC(27) = 0
cpsjun06 Changed back again: bufr use 0 for variable direction, 360 for 0 degrees.
cps      Next line rewritten to check for direction being unphysical,
cps      in which case MINDIC is inserted
cps      IF ( KDEC(27).EQ.990 ) KDEC(27) = 510
      IF ( KDEC(27).EQ.990 ) then 
         KDEC(27) = 0
      else if (kdec(27).ne. mindic .and. kdec(27).gt.360) then
         print*,'DD unphysical - set to missing ',kdec(27)
         kdec(27) = mindic
      end if
C
C     ------------------------------------------------------------------
C*          2.2  FFF.
C                ----
 220  CONTINUE
C
      KDEC(28) = KINT(21)
      IF (KDEC(28).NE.MINDIC.AND.KINT(22).NE.MINDIC)
     C    KDEC(28) = KDEC(28)*10 + KINT(22)
      IF (KDEC(28).NE.MINDIC.AND.KINT(23).NE.MINDIC)
     C    KDEC(28) = KDEC(28)*10 + KINT(23)
C
crr  change accuracy of wind by multiplying by 10.
      IF (KDEC(28).NE.MINDIC) then
         KDEC(28) = KDEC(28)*10
         CALL KTOMPSI (KDEC(28))
      endif

cpsjul06 12 bits with scale 1 in bufr means that values > 4094 cannot
cps      be stored correctly (4095 = missing value)
      if (kdec(28).ne. mindic .and. kdec(28).gt.4094) then
         print*,'FFF too big - set to missing ',KDEC(28),' (m/s)'
         kdec(28) = mindic
      end if
C
C     SOME REPORTS USE 00000 INSTEAD ///// FOR WIND GROUP
C     SO RELPACE 0 DD AND 0 FFF WITH MINDIC
C
      IF(KDEC(27).EQ.0.AND.KDEC(28).EQ.0) THEN
         KDEC(27)=MINDIC
         KDEC(28)=MINDIC
      END IF
C
C     ------------------------------------------------------------------
C*          3.  WRITE REPORT TO FILE . FILE IS NUMBER 5 AND REPORT
C               LENGTH IS 32 WORDS .
C
 300  CONTINUE
C
      KDEC(24) = 32
C
c     call prtkdec(kdec,1,kdec(24),mindic)
c
      CALL EXBAIR(IERR)
      if(ierr.ne.0) then
         print*,'exbair:ierr=',ierr
         return
      end if
C
C     ------------------------------------------------------------------
C*          4.   QUALITY CONTROL TOVS DATA IN BUFR FORM.
C                ---------------------------------------
 400  CONTINUE
C
      CALL QCAIREP( IERR )
      if(ierr.ne.0) then
         print*,'qcairep:ierr=',ierr
         return
      end if
C
C
C     ------------------------------------------------------------------
C*          5.   ENCODE DATA IN BUFR MESSAGE AND PUT IN THE RDB.
C                -----------------------------------------------
 500  CONTINUE
C
      CALL BUFRENC( IERR )
      if(ierr.ne.0) then
         print*,'bufrenc:ierr=',ierr
         return
      end if
C
      RETURN
C
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
      CHARACTER CTABBEN*64,CTABBU*24
      CHARACTER*8 YDATE
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
cpsjun11      DATA IOPTS / 676*'37777777777'O,'0'O,'2'O /
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
C     ------------------------------------------------------------------
C*          8.   SET UP BUFR TABLE B.
C                --------------------
 800  CONTINUE
C
      CALL SETTAB( IERR )
      CALL SETWT ( IERR ) 
C 
      RETURN
      END
      SUBROUTINE ERRFILE( ihead,IERR )
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
C         *CALL* *ERRFILE(ihead,IERR)*
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
CNH common for directories
      INCLUDE 'comdir.f'
Crr for cstream
      INCLUDE 'commdbc.f'
Crr for msys
      INCLUDE 'commdb.f'
C
      CHARACTER*1 Y63
      CHARACTER*80 YOUT,YOUTA
      CHARACTER*16384 YCHAR
      CHARACTER*3 YSPEC
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
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
      OPEN(UNIT=11,IOSTAT=IOS,ERR=400,
     C     FILE=cstream(msys)//'/'//'arep_error_file.dat',
     c     STATUS='UNKNOWN' )
C
C     -----------------------------------------------------------------
C*          2. initialize pointers.
C              --------------------
 200  CONTINUE
c 
      ist=1
      iend=1
c
C
C     -----------------------------------------------------------------
C*          3. WRITE BULLETIN INTO ERROR FILE AND MARK ERROR.
C              ----------------------------------------------
 300  CONTINUE
C
      YOUT=' '
      YOUTA=' '
C
      call nextend(iend,ilen)
      if(iend.gt.ilen) go to 500
      call nextprt(iend,ilen)
c      if(iend.gt.ilen) go to 500 
      iend=iend-1
c
      ii=0
c
      do 301 i=ist,iend
c
      ii=ii+1
      if(kchar(i).gt.127) then
                             youta(ii:ii)=y63
                             isign=1
                          end if
      yout(ii:ii)=char(iand(kchar(i),127))
c
 301  continue
c
      iend=iend+1
      ist=iend
C
      WRITE(11,'(A)') YOUT
      if(isign.eq.1)  WRITE(11,'(A)') YOUTA
      isign=0
C
C
      GO TO 300
C
 400  CONTINUE
C
      PRINT*,' +++ ERROR DURING OPENNING UNIT 10 +++, IOS=',IOS
C
 500  CONTINUE
C
      RETURN
      END
      SUBROUTINE EXBAIR( IERR )
C
C**** *EXBAIR*
C
C
C     PURPOSE.
C     --------
C         SET UP BUFR EXPANDED FORMAT FOR AIREP DATA.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXBAIR( IERR )*
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
      INCLUDE 'comident.f'
C
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f'
C
      CHARACTER*9 CVAL,CIDENT 
      character*24 cunit
C
      INCLUDE 'comwtc.f'      ! CONTAIN CVAL ARRAY
      INCLUDE 'comair.f'      ! AICRAFT FLIGHT PHASE
C
C     ------------------------------------------------------------------
C*          1.   INCREASE SUBSETS COUNTER BY ONE.
C                ----------------------------------
 100  CONTINUE
C
      NSUB=NSUB+1
      N=NSUB
      CVAL(NSUB)=' '
C
C*          1.1  GET CURRENT DATE(MONTH AND YEAR).
C                --------------------------------- 
 110  CONTINUE
C
      CALL DATUM(KDEC(1),IMONTH,IYEAR)
C     ------------------------------------------------------------------
C*          2.   AIRCRAFT IDENTIFIER.
C                --------------------
 200  CONTINUE
C
      VALUES(1,NSUB)=NSUB*1000+8
      CVAL(NSUB)=CHAR(IDENT(1))//CHAR(IDENT(2))//CHAR(IDENT(3))//
     1           CHAR(IDENT(4))//CHAR(IDENT(5))//CHAR(IDENT(6))//
     2           CHAR(IDENT(7))//CHAR(IDENT(8))
C
C*          2.1  AIRCRAFT NAVIGATIONAL SYSTEM.
C                -----------------------------
 210  CONTINUE
C
      VALUES(2,NSUB)=999999.
C
C*          2.2  SET UP YEAR.
C                ------------
 220  CONTINUE
C
cpsapr98      VALUES(3,NSUB)=IYEAR+1900.
      VALUES(3,NSUB)=FLOAT(IYEAR)
C
C*          2.3  SET UP MONTH
C                ------------
 230  CONTINUE
C
      VALUES(4,NSUB)=FLOAT(IMONTH)
C
C*          2.4  SET UP DAY.
C                -----------
 240  CONTINUE
C
      VALUES(5,NSUB)=FLOAT(KDEC(1))
C
C*          2.5  SET UP HOUR.
C                ------------
 250  CONTINUE
C
      VALUES(6,NSUB)=FLOAT(KDEC(2))
C
C*          2.6  SET UP MINUTE.
C                --------------
 260  CONTINUE
C
      VALUES(7,NSUB)=float(KDEC(9))
C
C*          2.7  SET UP LATITUDE.
C                ----------------
 270  CONTINUE
C
      VALUES(8,NSUB)=FLOAT(KDEC(5))/100.
C
C*          2.8  SET UP LONGITUDE.
C                -----------------
 280  CONTINUE
C
      VALUES( 9,NSUB)=FLOAT(KDEC(6))/100.
C
C     ------------------------------------------------------------------
C*          3.  PHASE OF AIRCRAFT FLIGHT.
C               -------------------------
 300  CONTINUE
C
      VALUES(10,NSUB)=FLOAT(NPHASE)
C
C*          3.1  HEIGHT OR ALTITUDE.
C                -------------------
 310  CONTINUE
C
cpsnov08      VALUES(11,NSUB)=FLOAT(KDEC(8))
      VALUES(11,NSUB)=999999.
      IF(KDEC(8).NE.MINDIC) THEN
         VALUES(11,NSUB)=FLOAT(KDEC(8))
      END IF

C
C*          3.2  TEMPERATURE.
C                ------------
 320  CONTINUE
C
      VALUES(12,NSUB)=999999.
      IF(KDEC(26).NE.MINDIC)
     1   then
            VALUES(12,NSUB)=FLOAT(KDEC(26))/10.+273.16
         end if
C
C*          3.3  WIND DIRECTION.
C                ---------------
 330  CONTINUE
C
      VALUES(13,NSUB)=999999.
      IF(KDEC(27).NE.MINDIC) VALUES(13,NSUB)=FLOAT(KDEC(27))
C
C*          3.4  WIND SPEED.
C                -----------
 340  CONTINUE
C
      VALUES(14,NSUB)=999999.
crr  changed accuracy of wind speed
crr      IF(KDEC(28).NE.MINDIC) VALUES(14,NSUB)=FLOAT(KDEC(28))
      IF(KDEC(28).NE.MINDIC) VALUES(14,NSUB)=FLOAT(KDEC(28))/10.
C
C*          3.5  DEGREE OF TURBULENCE.
C                ---------------------
 350  CONTINUE
C
      VALUES(15,NSUB)=999999.
C
C*          3.6  HEIGHT OF BASE OF TURBULENCE.
C                -----------------------------
 360  CONTINUE
C
      VALUES(16,NSUB)=999999.
C
C*          3.7  HEIGHT OF TOP OF TURBULENCE.
C                ----------------------------
 370  CONTINUE
C
      VALUES(17,NSUB)=999999.
C     ------------------------------------------------------------------
C*          3.8  AIRFRAME ICING.
C                ---------------
 380  CONTINUE
C
      VALUES(18,NSUB)=999999.
C
C     ------------------------------------------------------------------
C*Crr          3.8.5  PRESSURE.
C                ---------------
 385  CONTINUE
C
      VALUES(19,NSUB)=999999.
      IF(KDEC(25).NE.MINDIC) VALUES(19,NSUB)=FLOAT(KDEC(25))
C*Crr
C     ------------------------------------------------------------------
C*          3.9  SET NUMBER OF ELEMENTS.
C                -----------------------
 390  CONTINUE
C
Crr      M=18
      M=19
C
C     ------------------------------------------------------------------
C*          4.   SET INFORMATION NEEDED FOR RDB KEY DEFINITION.
C                ----------------------------------------------
 400  CONTINUE
C
      NBUFTYPE=4                         ! SINGLE LEVEL UPPER-AIR(NON SAT)
      NTYPE=7                            ! RDB TYPE NUMBER
      IF(KDEC(4).EQ. 41) NSBTYPE=141     ! CODAR
      IF(KDEC(4).EQ.141) NSBTYPE=142     ! AIREP 
      IF(KDEC(4).EQ.241) NSBTYPE=143     ! COLBA
      IF(KDEC(4).EQ.144) NSBTYPE=144     ! AMDAR
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
      NLAT1=NINT(VALUES(8,NSUB)*10.**JSCALE)-JREFLAT
      NLON1=NINT(VALUES(9,NSUB)*10.**JSCALE)-JREFLON
C
      NLAT2=0  
      NLON2=0
C
      NOBS=NSUB
C
      CIDENT= CVAL(NSUB)
      NRECR=1
      NCORR=KDEC(21)
      NNIL=0
C
C     -----------------------------------------------------------------
      RETURN
      END
      SUBROUTINE BUFRENC( IERR )
C
C**** *BUFRENC*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO ENCODE INDIVIDUAL TOVS
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
C
C     ------------------------------------------------------------------
C*          1.   SET UP *BUFR* MESSAGE.
C                ----------------------
 100  CONTINUE
C
      IF( IERR.NE.0 ) RETURN
C
C*          1.1  SET UP SECTION 0.
C                -----------------
 110  CONTINUE
C
      CALL SEC0( IERR )
C
C*          1.2  SET UP SECTION 1.
C                -----------------
 120  CONTINUE
C
      CALL SEC1( IERR )
C
C*          1.3  SET UP SECTION 2.
C                -----------------
 130  CONTINUE
C
      CALL SEC2( IERR )
C
C*          1.4  SET UP SECTION 3.
C                -----------------
 140  CONTINUE
C
      CALL SEC3( IERR )
C
C*          1.5  SET UP SECTION 4.
C                -----------------
 150  CONTINUE
C
      CALL SEC4( IERR )
C
C*          1.6  SET UP SECTION 5.
C                -----------------
 160  CONTINUE
C
      CALL SEC5( IERR )
C
C*          2.   PUT BUFR MESSAGE INTO RDB.
C                --------------------------
 200  CONTINUE
C
      CALL PUTBUFR( IERR )
C
C
C*          2.1  RESET NUMBER OF SUBSETS TO 0.
C                -----------------------------
 210  CONTINUE
C
      NSUB=0
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
cpsmai11      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT,8)
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
C
crr 990909 version 7  
crr 950317 version 3  
crr N.B. change Bufr Table B in ..../libsrc/settab.f
      CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT,8)
crr      CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT,8)
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
cpsapr98      IYEAR=NYEAR-1900.
cpsapr98      CALL PACK(NBPW,MBUF(NWPT),IYEAR,NWPT,NBPT,8)
      iiyear=mod(nyear,100)
      if (iiyear.eq.0) iiyear = 100
      CALL PACK(NBPW,MBUF(NWPT),iiyear,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          11.  SET UP MONTH.
C                -------------
 1100 CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),NMONTH,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          12.  SET UP DAY.
C                -----------
 1200 CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),NDAY,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          13.  SET UP HOUR.
C                -------------
 1300 CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),NHOUR,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          14.  SET UP MINUTE.
C                --------------
 1400 CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),NMINUTE,NWPT,NBPT,8)
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
      SUBROUTINE SEC3( IERR )
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
      INCLUDE 'parameter.f'
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
C*          1.6  SET UP COLLECTION OF DESCRIPTORS TO DEFINE DATA.
C                --------------------------------------------------
 160  CONTINUE
C
C     ASDAR/AIREP
C
Crr      MQ=18
      MQ=19
      CALL PACK(NBPW,MBUF(NWPT),3,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),11,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 8)
Crr
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),07,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),004,NWPT,NBPT, 8)
Crr
C
C     CHECK IF Q/C PERFORMED.
C
      IBIT1=0
      DO 161 I=M+1,M+M
         IF(VALUES(I,NSUB).NE.999999.) IBIT1=IBIT1+1
 161  CONTINUE
C
      IF(IBIT1.EQ.0) GO TO 170
C
crr      MQ=22
      MQ=23
      CALL PACK(NBPW,MBUF(NWPT),2,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),22,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),M,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),31,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),32,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),1,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),M,NWPT,NBPT, 8)
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT, 2)
      CALL PACK(NBPW,MBUF(NWPT),33,NWPT,NBPT, 6)
      CALL PACK(NBPW,MBUF(NWPT),7,NWPT,NBPT, 8)
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
         YVAL=CVAL(IST)
         NCHAR=NINT(VAL)-IST*1000
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
cc       print*,' sec4 - j,VAL,ISCALE,IREF,IPACK ',
cc     * j,VAL,ISCALE,IREF,IPACK
C
         CALL PACK(NBPW,MBUF(NWPT),IPACK,NWPT,NBPT,IBDW)
      END IF
C
 202  CONTINUE
C
cc      print*,'sec4 - m,mq',m,mq
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
cc      print*,'sec4 - irev,m,mq,j',irev,m,mq,j
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
C*         22.   SET UP LENGTH OF THE SECTION 4.
C                -------------------------------
 2200 CONTINUE
C
      CALL OCTNUM(IWPTB,IBPTB)
C
C     ------------------------------------------------------------------
 2300 CONTINUE
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'comtab.f'
c
      character*24 ctabbu
      character*64 ctabben
c
      INCLUDE 'comtabc.f'
      INCLUDE 'comwt.f'
c
      character*9 cval
      character*24 cunit
c
      INCLUDE 'comwtc.f'
C
crr      DIMENSION ISEQ(22)
      DIMENSION ISEQ(23)
C
      DATA ISEQ  /001006,002061,004001,004002,004003
     1,           004004,004005,005001,006001,008004
     2,           007002,012001,011001,011002,011031
crr     4,           011032,011033,020041,031031,001031
     4,           011032,011033,020041,07004,031031,001031
     5,           001032,033007/
C     ------------------------------------------------------------------
C*          1.   SET UP WORKING TABLES FOR AIREP  DATA.
C                ----------------------------------------------
 100  CONTINUE
C
crr      DO 101 I=1,22
      DO 101 I=1,23
C
      ITR=ISEQ(I)
C
      DO 102 J=1,JP2
C
      IF(ITR.EQ.NTABBTR(J)) THEN
                               IWTR (I)=NTABBTR(J)
                               IWTS (I)=NTABBS (J)
                               IWTDW(I)=NTABBDW(J)
                               IWTRV(I)=NTABBRV(J)
                               cunit(i)=ctabbu(j)
                               GO TO 101
                            END IF
C
 102  CONTINUE
C
      PRINT*,' BUFR TABLE B REFERENCE ENTRY',ITR,'NOT FOUND'
C
 101  CONTINUE
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
C     call asctim
C     call isfile
C     call pack
C     call rlunit (libmi)
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
      CHARACTER*15000 YOUT
      CHARACTER*9  CIDENT
      CHARACTER*48 CKEY
      CHARACTER*32 YKEY
      CHARACTER*16 YTYPE,CRDBFN(20)
cpsapr98      CHARACTER*12 CFNAME
      CHARACTER*14 CFNAME
      CHARACTER*14 YTIME
      CHARACTER*23 CTIME,YCTIME
      CHARACTER*10 CUSE
cpsapr98      CHARACTER*29 YCFNAME   not used!
c
cpsokt98      CHARACTER*8 CTSTAMP
      CHARACTER*12 CTSTAMP
      CHARACTER*4   cstream(40)
C
C
      DIMENSION IKEY(12)
C
      EQUIVALENCE (IKEY(1),YKEY,CKEY)
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
CTEST
C
      IF(IERR.NE.0) RETURN
C
      ierr=0
      IRET=0
      YTYPE='arep'
cpsapr98      NY=NYEAR - 1900
      NY=NYEAR
C
cpsapr98      WRITE(YTIME,'(6I2.2)',IOSTAT=IOS,ERR=101) 
      WRITE(YTIME,'(I4,5I2.2)',IOSTAT=IOS,ERR=101) 
     1         NY,NMONTH,NDAY,NHOUR,NMINUTE,NSECOND
C
crr      PRINT*,' PUTBUFR -  ', NY,NMONTH,NDAY,NHOUR,NMINUTE,NSECOND
      GO TO 102
C
 101  CONTINUE
C
      PRINT*,' BAD DATE OR TIME, REPORT SKIPPED. IOSTAT=',IOS
      RETURN
C
 102  CONTINUE
C 
C
C     DEFINE FILE NAME
C
      CALL ISFILE(YTYPE,YTIME,CFNAME,IRET)
CTEST
CTEST      PRINT*,' PUTBUFR - cfname ', CFNAME
CTEST
      IF(IRET.NE.0) THEN
         PRINT*,'ISFILE:WRONG FILE NAME, IRET=',IRET
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
C
C*          2.2 DEFINE HOUSE KEEPING INFORMATION.      
C               ---------------------------------
 220  CONTINUE
C
      ILENGTH=(NWPT-2)*4+NBPT/8
C
C     FIND LOWEST QUALITY CONTROL PERCENTAGE. 
C
CNH - M and MQ in common comwt
C
CTEST
CTEST      PRINT*,' PUTBUFR - QC: M,MQ,NSUB ', M,MQ,NSUB
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
C     SET LENGTH OF RECORD
C
      IW =9
      IBT=0
C
      CALL PACK(32,IKEY(IW),ILENGTH,IW,IBT,16)      
C
C     SET TIME RECORD WAS LAST WRITTEN TO RDB
C 
      CALL PACK(32,IKEY(IW),IDD,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),IHH,IW,IBT,5)      
      CALL PACK(32,IKEY(IW),IMM,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),ISS,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
C
C     SET TIME BULLETIN WAS RECEIVED
C
C
      CALL PACK(32,IKEY(IW),NRDAY,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRHOUR,IW,IBT,5)      
      CALL PACK(32,IKEY(IW),NRMIN,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRSEC,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),0,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NCORR,IW,IBT,6)      
      CALL PACK(32,IKEY(IW),NRECR,IW,IBT,1)      
      CALL PACK(32,IKEY(IW),NNIL,IW,IBT,1)      
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
C                ----------------------        
 300  CONTINUE
C
C*          3.1 CHECK IF CORRECTION.
C               --------------------
 310  CONTINUE
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
     1         FILE=cstream(msys)//'/'//CFNAME//'.rdb',
     2         ACCESS='DIRECT',
     3         RECL=2048/lrunit,
     4         FORM='UNFORMATTED',
     5         STATUS='UNKNOWN'        )
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
     1     FILE=cstream(msys)//'/'//CFNAME//'.key',
#ifdef FORTRAN90
     2     POSITION='APPEND',
#else
     2     ACCESS='APPEND',
#endif
     4     FORM='FORMATTED',
     5     STATUS='UNKNOWN'        )
C
CTEST write to file
CTEST
 360  CONTINUE         
crr ilength is length of bufr message
crr ilenout is length of bufr message + mbuf(1)
      ilenout=ilength+4
cps      ilenout=12
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
cps      WRITE(IU,REC=NTEST(IU))ILENGTH,(mbuf(i),i=2,256)
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
         WRITE(IUNIT,10001)CIDENT,ILENGTH,NTEST(IU),NQC,
     1        NTYPE,NSBTYPE,NYEAR,NMONTH,IDD,IHH,IMM,
     2        ISS,NLON1,NLAT1,NCORR
10001    FORMAT(1X,A9,2I5,2I3,I4,I5,5I3,2I10,I3)
C     
      else
         print*,' PUTBUFR - ',cstream(msys)//'/'//CFNAME,' ',cident,
     *        ' ilength',ilength
         print*,' PUTBUFR - length of bufr data gt rec. length of .rdb '
         print*,' PUTBUFR - ilenout = ',ilenout
         call exit(2)
      end if
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
c      IF(IERR .EQ. 0) RETURN
C
      YLINE=' '
      YLINEA=' '
C
C
C
      open(unit=11,iostat=ios,err=400,
     1     FILE=cstream(msys)//'/'//'arep_error_file.dat',
#ifdef FORTRAN90
     2     POSITION='APPEND',
#else
     2     ACCESS='APPEND',
#endif
     3     STATUS='UNKNOWN' )
c
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
      WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
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
                               KCHAR(IP)=IAND(KCHAR(IP),127)
                            END IF
C
      YLINE(N1:N1) = CHAR(KCHAR(IP))
      IP = IP + 1
C
  112 CONTINUE
C
      WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
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
      WRITE(11,'(A)') YGS
      CLOSE(11)
C
C     REMOVE PARITY BIT
C
      DO 410 I = 1 , IGS
         KCHAR(I)=IAND(KCHAR(I),127)
 410  CONTINUE
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
c      IF(IERR .EQ. 0) RETURN
C
      YLINE=' '
      YLINEA=' '
C
C
C
      OPEN(UNIT=11,IOSTAT=IOS,ERR=400,
     1     FILE=cstream(msys)//'/'//'arep_error_file.dat',
#ifdef FORTRAN90
     2     POSITION='APPEND',
#else
     2     ACCESS='APPEND',
#endif
     3     STATUS='UNKNOWN' )
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
      NN = 4
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
      WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N2)
      IF(ISIGN.EQ.1) THEN
         WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N2)
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
           CLOSE(11)
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
          CLOSE(11)
          RETURN
      END IF 
      JP = IP
      CALL NEXTEND(JP,J)
      CALL NEXTPRT(JP,J)
      K = JP - IP
      if(k.gt.80) k=80
      N1 = 0
      DO 131 N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127)
     C                        THEN
                                  YLINEA(N1:N1) = Y63
                                  ISIGN= 1
                                  KCHAR(IP)=IAND(KCHAR(IP),127)
                              END IF
C
         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
C
 131  CONTINUE
C
      WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:N1)
      IF(ISIGN.EQ.1) THEN
         WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINEA(1:N1)
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
      WRITE (11,'(A)',ERR=300,IOSTAT=IOS) YLINE(1:4)
      CLOSE(11)
C
      GO TO 400
C     ----------------------------------------------------------------
 300  IERR = -1
      PRINT*,' ERROR IN WRITING INTO ERROR FILE IOS=',IOS 
C     ----------------------------------------------------------------
 400  CONTINUE
C
C     REMOVE PARITY BIT
C
      DO 500 I = 1 , IGS
         KCHAR(I)=IAND(KCHAR(I),127)
 500  CONTINUE
C
      RETURN
      END
      SUBROUTINE NXMIMJ(I,J,II)
C
C**** *NXMIMJ*
C
C
C     PURPOSE.
C     --------
C
C         TO FIND NEXT MIMIMJMJ GROUP IN THE BULLETIN.
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT GROUP OF
C         ('TTAA' OR 'TTBB' OR 'TTCC' OR 'TTDD' ETC.)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTMI(I,J,II)*
C
C         INPUT     : I - SCAN STARTS AT WORD I.
C                     J - SCAN STOPS AT WORD J .
C
C
C         OUTPUT    : II- POSITION OF THE FIRST CHARACTER
C                         IN REQUIRED GROUP
Cpsmay04                         IF CHARACTER NOT FOUND II = 99999
C                         IF CHARACTER NOT FOUND II = 999999
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
C
      DIMENSION MIMJ(26)
C
      CHARACTER*4 YMIMJ
C
      DATA MIMJ /65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     &           80,81,82,83,84,85,86,87,88,89,90/
C     ------------------------------------------------------------------
C*          1.   FIND NEXT MIMIMJMJ GROUP.
C                -------------------------
 100  CONTINUE
C
C
cpsmay04      II=9999
      II=99999
      K =IABS(I)
      M =IABS(I)
C
 101  CONTINUE
C
      CALL NEXSEP2(M,J,*1000)
      CALL NEXPRT2(M,J,*1000)
C
      IF(M.GE.J) RETURN
C
      YMIMJ=CHAR(KCHAR(M  ))//CHAR(KCHAR(M+1))//CHAR(KCHAR(M+2))//
     1      CHAR(KCHAR(M+3))
C
      ISTART=0
      IF(YMIMJ(1:4).EQ.'ARP ') ISTART=1 
      IF(YMIMJ(1:4).EQ.'ARS ') ISTART=1 
      IF(YMIMJ(1:4).EQ.'AIR ') ISTART=1 
      IF(YMIMJ(1:4).EQ.'LVR ') ISTART=1 
      IF(YMIMJ(1:4).EQ.'ASC ') ISTART=1 
      IF(YMIMJ(1:4).EQ.'DES ') ISTART=1 
      IF(YMIMJ(1:4).EQ.'LVW ') ISTART=1
      IF(YMIMJ(1:4).EQ.'UNS ') ISTART=1 
C     '///' is another possibility, but this may also mark a missing humidity
C     group, so it is dangerous to test for that
C
      IF(ISTART.EQ.1) THEN
         CALL PRESEP(M,K,*1000)
         CALL PREPRT(M,K,*1000)
         II= M+1
         RETURN
      END IF
C
C
       GO TO 101
C
 1000 CONTINUE
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
      data cstr/'                   airep decoding'/
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
