      SUBROUTINE INSHIZ (IERR,CALLSN,ZSTN)
C
C****  *INSHIZ*
C
C       PURPOSE.
C      ----------
C
C         RETURN HEIGHT OF SHIP'S PLATFORM
C
C       INTERFACE.
C      ------------
C
C         CALL INSHIZ (IERR,CALLSN,ZSTN)
C
C        INPUT
C           CALLSN   -  CALL SIGN OF CURRENT REPORT
C
C        OUTPUT
C           ZSTN     -  HEIGHT OF PLATFORM
C           IERR     -  RETURN CODE
C
C       METHOD.
C      ---------
C
C           MAKE SURE FILE OF SHIP CALL SIGNS AND HEIGHTS IS READ;
C           EXTRACT HEIGHT FOR CURRENT CALL SIGN.
C
C       EXTERNALS.
C      ------------
C
C        NONE
C
C       REFERENCE.
C      ------------
C
C        NONE
C
C       AUTHOR.
C      ---------
C
C        B. NORRIS,  ECMWF,  JANUARY 1991
C
C       MODIFICATIONS.
C      ----------------
C
C        NONE
C
      INCLUDE 'paramsh.f'
      INCLUDE 'comshiz.f'
CRR common for directories
      INCLUDE 'comdir.f'
      CHARACTER*(*) CALLSN
cpsmai02 
      character spc
      IERR=0
C
C    ---------------------------------------------------------------------
C
C                    1. READ SHIP HEIGHT FILE
C                   --------------------------
C
  100 CONTINUE
cpsjul10      IF(IFSHIZ.NE.3H1ST) THEN
      IFSHIZ=0
      IF(IFSHIZ.NE.-1) THEN
C
C              1.1  OPEN FILE
C
      spc=char(32)
      ndx=0
C  - Find end of character string. 
      ndx=index(yppdat(1:80),spc) - 1
c
           OPEN(82,IOSTAT=IOS,ERR=122,
     X          FILE=YPPDAT(1:ndx)//'ship_height.dat',
     X          STATUS='OLD',FORM='FORMATTED')
           NSHIZ=0
  111      CONTINUE
           IF(NSHIZ+1.GT.NMSHIZ) THEN
                WRITE(*,'(1H ,'' INSHIZ : TOO MANY SHIPS SPECIFIED :'',
     X            I5)') NSHIZ
                IERR=1003
                GO TO 128
           ENDIF
           NSHIZ=NSHIZ+1
C
C              1.2  READ FILE
C
           READ (82,'(A9,F3.0)',ERR=124,END=128)
     X        CSHIZ(NSHIZ),SHIZ(NSHIZ)
           GO TO 111
  122      CONTINUE
           WRITE (*,'(1H ,'' ERROR OPENING SHIP HEIGHT FILE '',
     X       ''  IOSTAT = '',I5)') IOS
           IERR=1001
           GO TO 300
  124      CONTINUE
           WRITE (*,'(1H ,'' ERROR READING SHIP HEIGHT FILE '',
     X       ''  IOSTAT = '',I5)') IOS
           IERR=1002
           GO TO 300
  128      CONTINUE
C
C              1.3  CLOSE FILE
C
           CLOSE (82)
cpsjul10           IFSHIZ=3H1ST
           IFSHIZ=-1
      ENDIF
C
C    ---------------------------------------------------------------------
C
C                    2. CHECK MATCH IN CALL SIGN
C                   -----------------------------
C
  200 CONTINUE
C
C         2.1  SEARCH ARRAY OF CALL SIGNS
C
      DO 215 I=1,NSHIZ
      IF(CALLSN.EQ.CSHIZ(I)) THEN
           ZSTN=SHIZ(I)
           WRITE (*,'(1H ,'' HEIGHT FOR SHIP '',A9,'' SET TO '',
     X             F5.1)') CALLSN,ZSTN
           GO TO 300
      ENDIF
  215 CONTINUE
C
C                2.2  NO MATCH FOUND, SET HEIGHT TO ZERO
C
      ZSTN=0.0
C
C    ---------------------------------------------------------------------
C
C                    3. EXIT
C                   ---------
C
  300 CONTINUE
      RETURN
      END
