      SUBROUTINE LAPS (IERR,NST,PP,TT,IVS,NNPP,NNTT,RLAT,RLONG,
     X                 CALLSN,IY,IM,ID,IH,IN)
C
C****  *LAPS*
C
C       PURPOSE.
C      ----------
C
C           LAPSE RATE AND INVERSION CHECK FOR VERTICAL
C            TEMPERATURE PROFILES
C
C       INTERFACE.
C      ------------
C
C           CALL LAPS (IERR,NST,PP,TT,IVS,NNPP,NNTT,RLAT,RLONG,
C                      CALLSN,IY,IM,ID,IH,IN)
C
C        INPUT
C          NST     -  NUMBER OF TEMPERATURE LEVELS
C          PP      -  PRESSURE (PA)
C          TT      -  TEMPERATURE (DEG K)
C          IVS     -  VERTICAL SIGNIFICANCE
C          NNPP    -  FLAG POSITIONS FOR PRESSURE
C          NNTT    -  FLAG POSITIONS FOR TEMPERATURE
C          RLAT    -  LATITUDE VALUE OF REPORT
C          RLONG   -  LONGITUDE VALUE OF REPORT
C          CALLSN  -  STATION NUMBER OR CALL SIGN
C          IY,IM,ID,IH,IN - YEAR,MONTH,DAY,HOUR,MINUTE FOR REPORT
C
C        OUTPUT
C          IERR    -  ERROR CODE
C          CONFIDENCE VALUES UPDATED
C
C       METHOD.
C      ---------
C
C         CHECK FOR UNREASONABLE INVERSION;
C         CHECK FOR SUPERADIABATIC LAYER.
C
C       EXTERNALS.
C      ------------
C
C         PINTER   -  INTERPOLATE TO PRESSURE OR HEIGHT LEVEL
C         SETINT   -  SET CONFIDENCE FLAGS FOR INTERNAL CONSISTENCY
C                      CHECK
C         SETGRS   -  SET CONFIDENCE FLAGS FOR GROSS LIMIT CHECK
C
C       REFERENCE.
C      ------------
C
C         GUIDE ON THE GLOBAL DATA PROCESSING SYSTEM WMO N305 1982
C            CHAPTER 6, QUALITY CONTROL PROCEDURES
C
C       AUTHOR.
C      ---------
C
C         B. NORRIS,  ECMWF,  MARCH 1990
C
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
      INCLUDE 'paramq.f'
      PARAMETER (NZONE=3)
      INCLUDE 'const.f'
      INCLUDE 'ctua.f'
      DIMENSION PP(*),TT(*),IVS(*),NNPP(*),NNTT(*)
      DIMENSION ULAYT(NLEV),ULAPS(NLEV)
      DIMENSION UINL(NSEASN,NZONE,NLEV)
      DIMENSION UICL(NSEASN,NZONE,NLEV)
      CHARACTER*(*) CALLSN
      LOGICAL LFLAG
C
C    --------------------------------------------------------------------
C
C                     1. SET UP
C                   ------------
C
  100 CONTINUE
C
C          1.1  TABLE (IN PA) FOR BOUNDARY BETWEEN
C                             THIN AND THICK LAYER
C
      DATA (ULAYT(I),I=1,10)/
     X       2000.0,
     X       2000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0/
      DATA (ULAYT(I),I=11,NLEV)/
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0,
     X       1000.0/
C
C          1.2  TABLE (IN DEG K PER HPA) FOR MAXIMUM INVERSION
C                   OVER THIN LAYER
C
      DATA (((UINL(I,J,K),I=1,NSEASN),J=1,NZONE),K=1,10)/
C            0-30 DEG            30-60 DEG           60-90 DEG LATITUDE
C       WINTER    SUMMER    WINTER    SUMMER    WINTER    SUMMER
     X    1.5,      1.2,      2.0,      1.5,      2.5,      1.8,
     X    1.5,      1.2,      2.0,      1.5,      2.5,      1.8,
     X    1.4,      1.2,      1.8,      1.5,      2.2,      1.8,
     X    1.2,      1.2,      1.5,      1.5,      1.8,      1.8,
     X    1.2,      1.2,      1.5,      1.5,      1.8,      1.8,
     X    1.2,      1.2,      1.5,      1.5,      1.8,      1.8,
     X    1.4,      1.4,      1.5,      1.5,      1.8,      1.8,
     X    1.8,      1.8,      1.8,      1.8,      1.8,      1.8,
     X    2.2,      2.2,      2.2,      2.2,      2.2,      2.2,
     X    2.6,      2.6,      2.6,      2.6,      2.6,      2.6/
      DATA (((UINL(I,J,K),I=1,NSEASN),J=1,NZONE),K=11,NLEV)/
C            0-30 DEG            30-60 DEG           60-90 DEG LATITUDE
C       WINTER    SUMMER    WINTER    SUMMER    WINTER    SUMMER
     X    3.0,      3.0,      3.0,      3.0,      3.0,      3.0,
     X    3.5,      3.5,      3.5,      3.5,      3.5,      3.5,
     X    4.5,      4.5,      4.5,      4.5,      4.5,      4.5,
     X    5.5,      5.5,      5.5,      5.5,      5.5,      5.5,
     X    6.5,      6.5,      6.5,      6.5,      6.5,      6.5,
     X    7.5,      7.5,      7.5,      7.5,      7.5,      7.5,
     X    8.5,      8.5,      8.5,      8.5,      8.5,      8.5,
     X    8.5,      8.5,      8.5,      8.5,      8.5,      8.5,
     X    8.5,      8.5,      8.5,      8.5,      8.5,      8.5,
     X    8.5,      8.5,      8.5,      8.5,      8.5,      8.5,
     X    8.5,      8.5,      8.5,      8.5,      8.5,      8.5,
     X    8.5,      8.5,      8.5,      8.5,      8.5,      8.5/
C
C          1.3  TABLE (IN DEG K) FOR MAXIMUM INVERSION
C                   OVER THICK LAYER
C
      DATA (((UICL(I,J,K),I=1,NSEASN),J=1,NZONE),K=1,10)/
C            0-30 DEG            30-60 DEG           60-90 DEG LATITUDE
C       WINTER    SUMMER    WINTER    SUMMER    WINTER    SUMMER
     X   30.0,     24.0,     40.0,     30.0,     50.0,     36.0,
     X   30.0,     24.0,     40.0,     30.0,     50.0,     36.0,
     X   14.0,     12.0,     18.0,     15.0,     22.0,     18.0,
     X   12.0,     12.0,     15.0,     15.0,     18.0,     18.0,
     X   12.0,     12.0,     15.0,     15.0,     18.0,     18.0,
     X   12.0,     12.0,     15.0,     15.0,     18.0,     18.0,
     X   14.0,     14.0,     15.0,     15.0,     18.0,     18.0,
     X   18.0,     18.0,     18.0,     18.0,     18.0,     18.0,
     X   26.0,     22.0,     26.0,     22.0,     26.0,     22.0,
     X   29.0,     26.0,     29.0,     26.0,     29.0,     26.0/
      DATA (((UICL(I,J,K),I=1,NSEASN),J=1,NZONE),K=11,NLEV)/
C            0-30 DEG            30-60 DEG           60-90 DEG LATITUDE
C       WINTER    SUMMER    WINTER    SUMMER    WINTER    SUMMER
     X   32.0,     30.0,     32.0,     30.0,     32.0,     30.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0,
     X   35.0,     35.0,     35.0,     35.0,     35.0,     35.0/
C
C          1.4  TABLE (IN DEG K) FOR SUPERADIABATIC CORRECTION
C
      DATA (ULAPS(I),I=1,10)/
     X          4.5,
     X          3.5,
     X          2.5,
     X          1.5,
     X          1.0,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5/
      DATA (ULAPS(I),I=11,NLEV)/
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5,
     X          0.5/
C
      IERR=0
      U=R/RCP
      IF(NST.LE.1) GO TO 400
C
C           1.5  ZONE DEPENDS ON LATITUDE
C
      IZONE=INT(ABS(RLAT)/30.0)+1
      IF(IZONE.GT.3) IZONE=3
C
C    -------------------------------------------------------------------
C
C                    2.  INVERSION CHECK
C                   ---------------------
C
  200 CONTINUE
      DO 355 ILEV=1,NST-1
C
C           2.1  INTERPOLATE POSITION IN TABLES
C
      CALL PINTER (IERR,UPZ(1,1),PP(ILEV),POS)
      IF(IERR.NE.0) THEN
           IERR=0
           GO TO 355
      ENDIF
      IPOS=INT(POS)
C
C           2.2  DETERMINE MAXIMUM INVERSION ALLOWED
C
      PDIFF=PP(ILEV)-PP(ILEV+1)
      TDIFF=TT(ILEV+1)-TT(ILEV)
      IF(PDIFF.GT.ULAYT(IPOS)) THEN
           TLIM=UICL(ISEASN,IZONE,IPOS)
      ELSEIF(PDIFF.EQ.0) THEN
           TLIM=0.1
      ELSE
           TLIM=UINL(ISEASN,IZONE,IPOS)*PDIFF/100.0
      ENDIF
C
C           2.3  SET CONFIDENCE
C
      IF(TDIFF.GT.TLIM) THEN
           CALL SETINT (IFAIL,NNTT(ILEV),NNTT(ILEV+1),IEND)
C           WRITE (*,'(1H ,'' INVERSION CHECK     '',
C     X               A,2X,I4,4I2.2,4(A,F10.2))')
C     X               CALLSN,IY,IM,ID,IH,IN,
C     X               ' PP:',PP(ILEV),  ', TT:',TT(ILEV),
C     X              ', PP:',PP(ILEV+1),', TT:',TT(ILEV+1)
      ENDIF
C
C    -------------------------------------------------------------------
C
C                  3.  LAPSE RATE CHECK
C                -----------------------
C
  300 CONTINUE
C
C        3.1  LOOK FOR SUPERADIABATIC LAYER
C
      TTI=TT(ILEV)*((PP(ILEV+1)/PP(ILEV))**U)-ULAPS(IPOS)
C     WRITE (*,'(1H ,''LAPS'',I3,8F10.2)')
C    X IPOS,PP(ILEV),PP(ILEV+1),TT(ILEV),TT(ILEV+1),PDIFF,TDIFF,TLIM,TTI
      IF(TT(ILEV+1).LT.TTI)
     X THEN
           LFLAG=.FALSE.
           IF(ILEV+2.LE.NST.AND.ILEV-1.GE.1) THEN
C
C        3.2  USE SURROUNDING LEVELS TO DETERMINE ERRONEOUS LEVEL
C
                CALL PINTER (IERR,UPZ(1,1),PP(ILEV-1),PI1)
                IF(IERR.NE.0) THEN
                     IERR=0
                     GO TO 355
                ENDIF
                IPI1=INT(PI1)
                TTI1=TT(ILEV-1)*((PP(ILEV+1)/PP(ILEV-1))**U)
                TTI2=TT(ILEV)  *((PP(ILEV+2)/PP(ILEV))  **U)
C               WRITE (*,'(1H ,''LAPS FAIL'',I3,10F10.2)')
C    X            IPI1,PP(ILEV-1),PP(ILEV),PP(ILEV+1),PP(ILEV+2),
C    X                 TT(ILEV-1),TT(ILEV),TT(ILEV+1),TT(ILEV+2),
C    X                 TTI1,TTI2
                IF(TT(ILEV+1).LT.TTI1-ULAPS(IPI1).AND.
     X             TT(ILEV+2).GE.TTI2-ULAPS(IPOS)) THEN
C
C        3.3  LEVEL I+1 IN ERROR
C
                     CALL SETGRS(NNTT(ILEV+1),2)
C                     WRITE (*,'(1H ,'' LAPSE RATE CHECK    '',
C     X               A,2X,I4,4I2.2,2(A,F10.2))')
C     X               CALLSN,IY,IM,ID,IH,IN,
C     X               ' PP:',PP(ILEV+1),', TT:',TT(ILEV+1)
                     LFLAG=.TRUE.
                ENDIF
                IF(TT(ILEV+1).GE.TTI1-ULAPS(IPI1).AND.
     X             TT(ILEV+2).LT.TTI2-ULAPS(IPOS)) THEN
C
C        3.4  LEVEL I IN ERROR
C
                     CALL SETGRS(NNTT(ILEV),2)
C                     WRITE (*,'(1H ,'' LAPSE RATE CHECK    '',
C     X               A,2X,I4,4I2.2,2(A,F10.2))')
C     X               CALLSN,IY,IM,ID,IH,IN,
C     X               ' PP:',PP(ILEV),  ', TT:',TT(ILEV)
                     LFLAG=.TRUE.
                ENDIF
           ENDIF
C
C        3.5  EITHER I OR I+1 IN ERROR
C
           IF(.NOT.LFLAG) THEN
                CALL SETINT (IFAIL,NNTT(ILEV),NNTT(ILEV+1),IEND)
C                WRITE (*,'(1H ,'' LAPSE RATE CHECK    '',
C     X               A,2X,I4,4I2.2,4(A,F10.2))')
C     X               CALLSN,IY,IM,ID,IH,IN,
C     X               ' PP:',PP(ILEV),  ', TT:',TT(ILEV),
C     X              ', PP:',PP(ILEV+1),', TT:',TT(ILEV+1)
           ENDIF
      ENDIF
  355 CONTINUE
C
C    ---------------------------------------------------------------------
C
C                       4.  EXIT
C                     -----------
C
  400 CONTINUE
      RETURN
      END
