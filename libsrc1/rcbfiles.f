      SUBROUTINE RBCFILES (IERR,YSTIDBC,YSTYPBC,NSTBC,
     X                     YSNAMBC,RCORBC,NSOBC)
C
C**** *RBCFILES*
C
C       PURPOSE.
C      ----------
C
C          READ BIASCORRECTION REFERENCE FILES
C
C       INTERFACE.
C      ------------
C
C        CALL BIASCOR (IERR,YSTIDBC,YSTYPBC,NSTBC,YSNAMBC,RCORBC,NSOBC)
C
C        INPUT
C         NONE
C         
C
C        OUTPUT
C         IERR    ERROR CODE
C         YSTID   ARRAY WITH STATION IDENTIFIERS
C         YSTYP   ARRAY WITH SONDE TYPES
C         NST     NUMBER OF ENTRIES IN YSTIDBC, YSTYPBC
C         YSNAM   ARRAY WITH STATION IDENTIFIERS
C         RCOR    ARRAY (3DIM) WITH BIAS CORRECTION VALUES
C         NSO     NUMBER OF SONDE TYPES IN YSNAMBC
C
C       METHOD.
C      ---------
C
C        
C
C       EXTERNALS.
C      ------------
C
C        NONE
C
C       REFERENCE.
C      ------------
C
C       AUTHOR.
C      ---------
C
C
C       MODIFICATIONS.
C      ----------------
C
C-----------------------------------------------------------------
      IMPLICIT LOGICAL (L,O,G)
      INCLUDE 'parameter.f'
      INCLUDE 'paramq.f'
      INCLUDE 'paramsh.f'
      PARAMETER (IUNIQ=12)
      PARAMETER (JTEMP=100)
      INCLUDE 'comwt.f'
      INCLUDE 'comkey.f'
      INCLUDE 'comkeyc.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
      DIMENSION SPP(JTEMP)
      DIMENSION SZ(JTEMP)
      CHARACTER*9 CIDENT
cpsmai02 
      character spc
      INCLUDE 'parabias.f'
CRR common for directories
      INCLUDE 'comdir.f'
c
C-------------------------------------------------------------------
C
C
C              OPEN FILES
C
cpsmai02      ispc=char(32)
      spc=char(32)
      ndx=0
C  - Find end of character string. 
      ndx=index(yppdat(1:80),spc) - 1
      OPEN(UNIT=65, FILE=YPPDAT(1:ndx)//'biascor1.tab',
     X     IOSTAT=IOS,ERR=55,STATUS='OLD')
      GO TO 56
   55 CONTINUE
      WRITE(*,'(1H ,''BIASCOR ERROR: OPENING OF BIASCOR1 FAILED.'',
     X        ''  IOSTAT = '',I5)') IOS
   56 CONTINUE
C
      OPEN(UNIT=66, FILE=YPPDAT(1:ndx)//'biascor2.tab',
     X     IOSTAT=IOS,ERR=65,STATUS='OLD')
      GO TO 66
   65 CONTINUE
      WRITE(*,'(1H ,''BIASCOR ERROR: OPENING OF BIASCOR2 FAILED.'',
     X        ''  IOSTAT = '',I5)') IOS
   66 CONTINUE
C
C
C              READ TABLE 1 : STATION LIST & SONDE TYPE
C
      NSTBC=0
  102 CONTINUE
      IF (NSTBC.LT.NSTNBC) THEN
           NSTBC=NSTBC+1
           READ (65,'(A,1X,A)',END=104) YSTIDBC(NSTBC),YSTYPBC(NSTBC)
           GO TO 102
      ELSE
           WRITE (*,'(1X,A,I5)') ' DIMENSION NSTN TOO SMALL :',NSTNBC
           IERR=1
           GO TO 400
      ENDIF
  104 CONTINUE
      NSTBC=NSTBC-1
      WRITE (*,*) ' NUMBER OF STATIONS IN TABLE 1 ',NSTBC
C
C              READ TABLE 2 : SONDE CORRECTIONS
C
      NSOBC=0
  112 CONTINUE
      IF(NSOBC.LT.NSONBC) THEN
           NSOBC=NSOBC+1
           READ (66,'(2X,A)',END=118) YSNAMBC(NSOBC)
           READ (66,'(A)',END=118) YDUMMYBC
           DO 116 J=1,NLEVBC
           READ (66,'(5X,8F8.2/5X,8F8.2)',END=118)
     X            (RCORBC(K,J,NSOBC),K=1,NCORBC)
  116      CONTINUE
           READ (66,'(A)',END=118) YDUMMYBC
           GO TO 112
      ELSE
           WRITE (*,'(1X,A,I5)') ' DIMENSION NSON TOO SMALL :',NSONBC
           IERR=2
           GO TO 400
      ENDIF
  118 CONTINUE
      WRITE (*,*) ' NUMBER OF SONDES IN TABLE 2 ',NSOBC
C
C                CLOSE FILES
C
      CLOSE(65)
      CLOSE(66)

C
C-----------------------------------------------------------------
C                   4.  EXIT
C                  -----------
  400 CONTINUE
C
C
C
      RETURN
      END
