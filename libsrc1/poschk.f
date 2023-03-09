      SUBROUTINE POSCHK (CFNAME,CALLSN,IUNIQ,CUNIQ,JTYPE,JSBTYPE,
     X     IY,IM,ID,IH,IN,RLAT,RLON,LANSEA,LSHIP,IFLAG,IRET)
C
C****  *SHIPCHK*
C
C       PURPOSE.
C      ----------
C
C           MASTER ROUTINE FOR TIME CONTINUITY CHECKING
C
C       INTERFACE.
C      ------------
C
C           CALL POSCHK (CFNAME,CALLSN,IUNIQ,CUNIQ,JTYPE,JSBTYPE,
C                IY,IM,ID,IH,IN,RLAT,RLON,LANSEA,LSHIP,IFLAG,IRET)
C
C        INPUT
C          CFNAME  -  NAME OF TIME CONTINUITY FILE
C          CALLSN  -  CALL SIGN OF CURRENT REPORT
C          IUNIQ   -  NUMBER OF NON-UNIQUE CALL SIGNS
C          CUNIQ   -  LIST OF NON-UNIQUE CALL SIGNS
C          JTYPE   -  BUFR TYPE FOR CURRENT REPORT
C          JSBTYPE -  BUFR SUBTYPE FOR CURRENT REPORT
C          IY,IM,ID,IH,IN  -  YEAR, MONTH, DAY, HOUR, MINUTE OF
C                     CURRENT REPORT
C          RLAT,RLON       -  LATITUDE AND LONGITUDE OF CURRENT REPORT
C          LANSEA  -  LOGICAL FOR LAND SEA MASK CHECK
C          LSHIP   -  
C
C        OUTPUT
C          IFLAG   -  CONFIDENCE APPLIED TO POSITION
C          IRET    -  RETURN CODE
C
C       METHOD.
C      ---------
C
C         CHECK WHETHER CALL SIGN IS UNIQUE;
C         CHECK WHETHER POSITION IS OVER LAND (if LANSEA = .true.);
C         COMPARE CURRENT POSITION WITH PREVIOUS POSITION;
C         WRITE UPDATED RECORD TO TIME SERIES FILE.
C
C       EXTERNALS.
C      ------------
C
C         ISTIME   -  GET NUMBER OF MINUTES SINCE 1 JAN 1978
C         SHIPUP   -  UNPACK TIME SERIES RECORD
C         SHIPPK   -  PACK TIME SERIES RECORD
C         SHIPAD   -  ADD CURRENT INFORMATION TO TIME SERIES RECORD
C         COMPOS   -  COMPARE CURRENT POSITION WITH PREVIOUS POSITIONS
C         LANDSEA  -  CHECK WHETHER POSITION IS OVER LAND 
c         update_index_file    - subroutines/functions for treating a direct
c         iget_next_recnumber    access file as if it was an indexed file
c         iget_record_number
c         iget_end_of_string   - get index of last character (before first space)
c         rlunit   -  return number of bytes in unit for record length (libmi)
C
c       FILES.
c      --------
c              76              CFNAME (=<obstype>/<obstype>_shipposition)
c              77              CFNAME//'.index'
c              81              YPPDAT(1:ndx)//'land_sea_mask.dat'
c
C       REFERENCE.
C      ------------
C
C         NONE
C
C       AUTHOR.
C      ---------
C
C         B. NORRIS,  ECMWF,  MARCH 1990
C         BASED ON CODE WRITTEN FOR NOS/BE IN 1982
C
C       MODIFICATIONS.
C      ----------------
C
C     Aug 2003 P.S.: replaced use of indexed file for shipposition file with direct access
C
      IMPLICIT LOGICAL (L,O,G)
      INCLUDE 'paramsh.f'
      INCLUDE 'comship.f'
      INCLUDE 'compref.f'
      INCLUDE 'comlsm.f'
      INCLUDE 'const.f'
CRR common for directories
      INCLUDE 'comdir.f'
      CHARACTER*3 CMONTH(12)
      CHARACTER*23 CT
      CHARACTER*(*) CALLSN,CFNAME
      CHARACTER*(*)   CUNIQ(IUNIQ)
      DIMENSION IREC(512)
      integer iunit,jrec,iget_record_number,iget_next_recnumber,lrunit
      LOGICAL LCHECK,LANSEA,LSHIP,LUNIQ,OVERLND,opn,new_record
c
cpsjul10      structure/track/
      TYPE :: track
      character*9 yident
      integer*4   kslen
      integer*4   ksent
      integer*4   kspar
      integer*4   kinnt
      integer*4   krec(512)
cpsjul10      end structure
      END TYPE track
cpsjul10      record /track/ record
      TYPE (track) record
c
cps   iunit is unit for key file for shipposition file
      data iunit/77/
      new_record=.false.

C    --------------------------------------------------------------------
C
C                      1.  INITIALISATION.
C                     ---------------------
C
  100 CONTINUE
C
C          1.1  SET REFERENCE VALUES AND SCALING FACTORS
C
C       JREF = REFERENCE VALUE FOR PACKING LAT AND LONG
      DATA JREF/-9000000,-18000000/
C       JSCAL = SCALING FACTOR FOR PACKING LAT AND LONG
      DATA JSCAL/5,5/
      DATA CMONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     X            'JUL','AUG','SEP','OCT','NOV','DEC'/
      IF(IRET.NE.0) RETURN
      IF(IY.EQ.IMISS.OR.IM.EQ.IMISS.OR.ID.EQ.IMISS.OR.
     X   IH.EQ.IMISS.OR.IN.EQ.IMISS.OR.
     X   RLAT.EQ.RMISS.OR.RLON.EQ.RMISS) GO TO 500
C
C          1.2  OPEN AND READ LAND SEA MASK
C
cpsjul10      IF(ISFIR.NE.3H1ST.AND.LANSEA) THEN
      ISFIR=0
      IF(ISFIR.NE.1.AND.LANSEA) THEN
c
      ndx=iget_end_of_string(yppdat)
c
           OPEN (81,IOSTAT=IOS,ERR=122,
     X           FILE=YPPDAT(1:ndx)//'land_sea_mask.dat',
     X           STATUS='OLD',FORM='UNFORMATTED')
           READ (81,IOSTAT=IOS,ERR=124) MASK
           CLOSE (81)
           ISFIR=1
           GO TO 130
  122      CONTINUE
           WRITE (*,'(1H ,'' ERROR OPENING LAND SEA MASK FILE '',
     X              ''  IOSTAT = '',I5)') IOS
           IRET=1001
           GO TO 500
  124      CONTINUE
           WRITE (*,'(1H ,'' ERROR READING LAND SEA MASK FILE '',
     X              ''  IOSTAT = '',I5)') IOS
           IRET=1002
           GO TO 500
      ENDIF
C
C          1.3  CONVERT TIME OF REPORT TO MINUTES
C
  130 CONTINUE
      WRITE(CT(1:2),'(I2.2)') ID
      CT(3:3)='-'
      CT(4:6)=CMONTH(IM)
      CT(7:7)='-'
      WRITE(CT(8:11),'(I4)') IY
      CT(12:12)=' '
      WRITE(CT(13:14),'(I2.2)') IH
      CT(15:15)=':'
      WRITE(CT(16:17),'(I2.2)') IN
      CT(18:18)=':'
      WRITE(CT(19:20),'(I2.2)') 0
      CT(21:21)='.'
      WRITE(CT(22:23),'(I2.2)') 0
c      CALL ISTIME (CT,JMINSIN,IRET)
C     WRITE (*,'(1X,A5,2X,I4,4I2.2,I10,2F8.2)')
C    X         CALLSN,IY,IM,ID,IH,IN,JMINSIN,RLAT,RLON
C
C          1.4  CHECK WHETHER CALL SIGN IS UNIQUE
C
      LUNIQ=.TRUE.
      DO 142 KUNIQ=1,IUNIQ
      KP=INDEX(CUNIQ(KUNIQ),' ')
      KP=KP-1
      IF(KP.LT.1) KP=1
      IF(CALLSN(1:KP).EQ.CUNIQ(KUNIQ)(1:KP)) THEN
           LUNIQ=.FALSE.
           GO TO 144
      ENDIF
  142 CONTINUE
  144 CONTINUE
C
C    ----------------------------------------------------------------------
C
C                2.  MANAGE TIME SERIES FILE
C               -----------------------------
C
  200 CONTINUE
      IF(.NOT.LUNIQ) GO TO 300
C
C          2.1  MAKE SURE FILE IS OPEN
C
C
      call rlunit(lrunit)
      inquire(unit=76,opened=opn,iostat=ios,err=202)
      if(.not. opn) then
         open(unit=76,iostat=ios,err=201,file=CFNAME,
     1            access='direct',
     2            form='unformatted',
     4            recl=2080/lrunit,
     5            status='unknown')
cps   Note: if file CFNAME does not exist, ios will normally return 0, 
c     but errno will be set to 2 ("No such file or directory"). This 
c     always happens when trying to open a non-existing file. If we use
c     status='old', ios will be set to 2 also.
      end if
      go to 220
c
 201  continue
c
      print*,'Open error on ',cfname,' is ',ios
      call exit(2)
c
 202  continue
c
      print*,'Inquire error on ',cfname,' is ',ios
      call exit(2)
C
C            2.2  Read record
C
 220  continue
c
cpsjul10 record converted from structure to TYPE, so use % not .
cps      record.yident(1:9)=callsn
      record%yident(1:9)=callsn
      jrec=iget_record_number(CFNAME,iunit,record%yident)
      if(jrec.lt.0) goto 230
c
      read(unit=76,rec=jrec,iostat=ios,err=225) record
c
      nslen=record%kslen
      nsent=record%ksent
      nspar=record%kspar
      minnt=record%kinnt
      do 221 kk=1,512
         irec(kk)=record%krec(kk)
 221  continue
      go to 250
c
 225  continue
      print*,'Read error ',ios,' on  ',cfname 
      call exit(2)
C
C            2.3  RECORD DOES NOT EXIST, BUILD EMPTY RECORD
C
 230  continue
c
      new_record=.true.
      NSENT=0
      NSLEN=0
      NSPAR=2
      do 231 kk=1,512
         irec(kk)=0
 231  continue
      go to 260
C
C            2.5  UNPACK RECORD
C
 250  continue
c
      CALL SHIPUP (IREC)
C
C            2.6  ADD LATEST INFORMATION TO RECORD
C
 260  continue
c
      CALL SHIPAD (JTYPE,JSBTYPE,JMINSIN,RLAT,RLON)
C     WRITE (*,'(1H ,5I10)') NCREP,NSLEN,NSENT,NSPAR,MINNT
C     DO 800 KENT=1,NSENT
C     WRITE (*,'(1H ,8I8)')
C    X     (NSTYPE(KSUB,KENT),NSSTYP(KSUB,KENT),KSUB=1,NTSUBT)
C     WRITE (*,'(1H ,I10)') MINSIN(KENT)
C     WRITE (*,'(1H ,2(F10.2,I8,F10.2))')
C    X     (SPARAM(KPAR,KENT),ISCONF(KPAR,KENT),SSUBST(KPAR,KENT),
C    X       KPAR=1,NSPAR)
C 800 CONTINUE
C
C    ------------------------------------------------------------------
C
C                 3.  POSITION CHECKING
C                -----------------------
C
  300 CONTINUE
C
C          3.1  INITIALISE FLAG VALUES
C
      IFLAG=IUNCH
      IFLAG0=0
      IFLAG1=0
      IFLAG2=0
      IFLAG3=0
      LCHECK=.FALSE.
      NCHECK=0
C
C            3.2  CHECK WHETHER POSITION OVER LAND
C
      OVERLND=.FALSE.
      IF(LANSEA) THEN
           CALL LANDSEA (RLAT,RLON,ISUM)
           IF(ISUM.EQ.4) THEN
                IFLAG=10
                OVERLND=.TRUE.
                WRITE (*,'(1H ,'' POSITION OVER LAND: '',
     X                     A,2X,I4,4I2.2,2F10.2)')
     X                     CALLSN,IY,IM,ID,IH,IN,RLAT,RLON
           ENDIF
      ENDIF
      IF(.NOT.LUNIQ) GO TO 500
      J=NCREP
      IF(J.LE.0) GO TO 500
      IF(OVERLND) GO TO 400
C
C            3.3  CHECK WITH PREVIOUS UNFLAGGED POSITIONS
C
      K0=J
      K1=J-1
  332 CONTINUE
      IF(K1.GT.0) THEN
           IF(ISCONF(1,K1).GE.60) THEN
                K2=K1-1
  334           CONTINUE
                IF(K2.GT.0) THEN
                     IF(ISCONF(1,K2).GE.60) THEN
C
C            COMPARE WITH N-1, N-2
C
                          CALL COMPOS (SPARAM(1,K2),SPARAM(2,K2),
     X                                 MINSIN(K2),
     X                                 SPARAM(1,K1),SPARAM(2,K1),
     X                                 MINSIN(K1),
     X                                 SPARAM(1,K0),SPARAM(2,K0),
     X                                 MINSIN(K0),
     X                                 LSHIP,LCHECK,IFLAG0)
                          IF(LCHECK) NCHECK=NCHECK+1
                          K3=K2-1
  336                     CONTINUE
                          IF(K3.GT.0) THEN
                             IF(ISCONF(1,K3).GE.60) THEN
C
C              COMPARE WITH N-2, N-3
C
                                CALL COMPOS (SPARAM(1,K3),SPARAM(2,K3),
     X                                       MINSIN(K3),
     X                                       SPARAM(1,K2),SPARAM(2,K2),
     X                                       MINSIN(K2),
     X                                       SPARAM(1,K0),SPARAM(2,K0),
     X                                       MINSIN(K0),
     X                                       LSHIP,LCHECK,IFLAG1)
                                IF(LCHECK) NCHECK=NCHECK+1
C
C              COMPARE WITH N-1, N-3
C
                                CALL COMPOS (SPARAM(1,K3),SPARAM(2,K3),
     X                                       MINSIN(K3),
     X                                       SPARAM(1,K1),SPARAM(2,K1),
     X                                       MINSIN(K1),
     X                                       SPARAM(1,K0),SPARAM(2,K0),
     X                                       MINSIN(K0),
     X                                       LSHIP,LCHECK,IFLAG2)
                                IF(LCHECK) NCHECK=NCHECK+1
                                GO TO 340
                             ELSE
                                K3=K3-1
                                GO TO 336
                             ENDIF
                          ENDIF
                     ELSE
                          K2=K2-1
                          GO TO 334
                     ENDIF
                ENDIF
           ELSE
                K1=K1-1
                GO TO 332
           ENDIF
      ENDIF
C
C            3.4  CHECK 3 LATEST POSITIONS REGARDLESS OF CONFIDENCE
C
  340 CONTINUE
      J2=J-2
      IF(J2.GT.0.AND.J2.NE.K2) THEN
           J1=J-1
           CALL COMPOS (SPARAM(1,J2),SPARAM(2,J2),MINSIN(J2),
     X                  SPARAM(1,J1),SPARAM(2,J1),MINSIN(J1),
     X                  SPARAM(1,J),SPARAM(2,J),MINSIN(J),
     X                  LSHIP,LCHECK,IFLAG3)
           IF(LCHECK) NCHECK=NCHECK+1
      ENDIF
C
C             3.5  ASSIGN CONFIDENCE
C
      IF(MAX(IFLAG0,IFLAG1,IFLAG2).GE.70) THEN
         IF(NCHECK.GT.1) IFLAG=MAX(IFLAG0,IFLAG1,IFLAG2,IFLAG3)
      ELSE
         IF(NCHECK.GT.1) IFLAG=MIN(IFLAG0,IFLAG1,IFLAG2,IFLAG3)
      END IF
C 
C     WRITE (*,'(1H ,11I4)') K0,K1,K2,K3,J,J1,J2,
C    X                      IFLAG0,IFLAG1,IFLAG2,IFLAG3
      IF(IFLAG.LT.IUNCH)
     X WRITE (*,'(1H ,'' POSITION SUSPECT:   '',A,2X,I4,4I2.2,2F10.2,
     X '' CONFIDENCE: '',I4,''; PREVIOUS POSITIONS: ''
     X /(1H ,4(2X,2F10.2)))')
     X CALLSN,IY,IM,ID,IH,IN,RLAT,RLON,IFLAG,
     X (SPARAM(1,K),SPARAM(2,K),K=1,NCREP-1)
C
C    -------------------------------------------------------------------
C
C                  4.  UPDATE RECORD
C                 -------------------
C
  400 CONTINUE
      ISCONF(1,NCREP)=IFLAG
      ISCONF(2,NCREP)=IFLAG
C
C           4.1  PACK TIME SERIES RECORD
C
      CALL SHIPPK (IREC)
C
C           4.2  WRITE TIME SERIES RECORD
C
      record%kslen=nslen
      record%ksent=nsent
      record%kspar=nspar
      record%kinnt=minnt
      do 421 kk=1,512
      record%krec(kk)=irec(kk)
 421  continue
c
      if(new_record) jrec=iget_next_recnumber(CFNAME,iunit,
     *     len(record%yident))
c
      write(unit=76,rec=jrec,iostat=ios,err=410) record
c
      if(new_record) then
         call update_index_file(CFNAME,iunit,jrec,record%yident)
      end if
c
      go to 500
c
 410  continue
c
      print*,'Error during write on file ',cfname
      iret=ios
      return
c
C
C    -------------------------------------------------------------------
C
C                       5.  EXIT
C                      -----------
C
  500 CONTINUE
      RETURN
      END
