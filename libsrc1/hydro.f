      SUBROUTINE HYDRO (IERR,NST,PP,TV,IVS,Z,NNZ,NNTT,NNTD,
     X                  CALLSN,IY,IM,ID,IH,IN)
C
C****  *HYDRO*
C
C       PURPOSE.
C      ----------
C
C           HYDROSTATIC CHECK OF STANDARD LEVEL HEIGHTS
C            AND TEMPERATURES
C
C       INTERFACE.
C      ------------
C
C           CALL HYDRO (IERR,NST,PP,TV,IVS,Z,NNZ,NNTT,NNTD,
C                       CALLSN,IY,IM,ID,IH,IN)
C
C        INPUT
C          NST     -  NUMBER OF TEMPERATURE LEVELS
C          PP      -  PRESSURE (PA)
C          TV      -  VIRTUAL TEMPERATURE (DEG K)
C          IVS     -  VERTICAL SIGNIFICANCE
C          Z       -  GEOPOTENTIAL (M)
C          NNZ     -  FLAG POSITIONS FOR GEOPOTENTIAL
C          NNTT    -  FLAG POSITIONS FOR TEMPERATURE
C          NNTD    -  FLAG POSITIONS FOR DEW POINT
C          CALLSN  -  STATION NUMBER OR CALL SIGN
C          IY,IM,ID,IH,IN - YEAR,MONTH,DAY,HOUR,MINUTE OF REPORT
C
C        OUTPUT
C          IERR    -  ERROR CODE
C          CONFIDENCE VALUES UPDATED
C
C       METHOD.
C      ---------
C
C         EXTRACT STANDARD LEVELS;
C         ADJUST TEMPERATURE IF TROPOPAUSE OCCURS;
C         CALCULATE EXTREME VALUES FOR THICKNESS;
C         SET CONFIDENCE VALUES DEPENDING ON RATIO OF THICKNESS
C         DEPARTURES IN NEIGHBOURING LAYERS.
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
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
      INCLUDE 'ctua.f'
      DIMENSION PP(*),TV(*),IVS(*),Z(*),NNZ(*),NNTT(*),NNTD(*)
      DIMENSION STDPP(NLEV),STDTV(NLEV),STDZ(NLEV),STDTC(NLEV)
      DIMENSION ISTDZ(NLEV),ISTDTT(NLEV),ISTDTD(NLEV)
      DIMENSION E(NLEV),D(NLEV),DA(NLEV),DB(NLEV),IPOS(NLEV)
      CHARACTER*(*) CALLSN
      LOGICAL LFLAG,LTROP
C
C    --------------------------------------------------------------------
C
C                     1. SET UP
C                   ------------
C
  100 CONTINUE
C
C         CRITICAL CONFIDENCE VALUE
      DATA ICRIT/65/
      IERR=0
      U=R/RCP
      IF(NST.LE.1) GO TO 400
C
C           1.1  EXTRACT STANDARD AND TROPOPAUSE LEVELS
C
      LTROP=.FALSE.
      NSTD=0
      DO 112 ILEV=1,NST
cpssep03      IF(((IVS(ILEV).AND.32).EQ.32).AND.NSTD.LT.NLEV) THEN
      IF((IAND(IVS(ILEV),32).EQ.32).AND.NSTD.LT.NLEV) THEN
           NSTD=NSTD+1
           STDPP (NSTD)=  PP(ILEV)
           STDTV (NSTD)=  TV(ILEV)
           STDTC (NSTD)=STDTV(NSTD)
           STDZ  (NSTD)=  Z (ILEV)
           ISTDZ (NSTD)= NNZ(ILEV)
           ISTDTT(NSTD)=NNTT(ILEV)
           ISTDTD(NSTD)=NNTD(ILEV)
C
C       1.11  OBTAIN POSITION OF EACH LEVEL WITHIN TABLE OF
C                 STANDARD LEVELS
C
           CALL PINTER (IERR,UPZ(1,1),STDPP(NSTD),POS)
           IF(IERR.NE.0) THEN
                IERR=0
                IPOS(NSTD)=1000
           ELSE
                IPOS(NSTD)=NINT(POS)
           ENDIF
C
C       1.12  ADJUST TEMPERATURE AT TOP OF LAYER IF TROPOPAUSE
C                 OCCURS WITHIN LAYER
C
           IF(NSTD.GT.1) THEN
           IF(LTROP.AND.IPOS(NSTD)-IPOS(NSTD-1).EQ.1.AND.
     X        TTROP.NE.RMISS.AND.PTROP.NE.RMISS.AND.
     X        STDTV(NSTD).NE.RMISS.AND.STDTV(NSTD-1).NE.RMISS)
     X            STDTC(NSTD)=STDTV(NSTD)+TTROP-STDTV(NSTD-1)-
     X               (STDTV(NSTD)-STDTV(NSTD-1))*
     X               (ALOG(STDPP(NSTD-1)/PTROP)/
     X                ALOG(STDPP(NSTD-1)/STDPP(NSTD)))
           END IF
           LTROP=.FALSE.
      ENDIF
C
C          1.13  EXTRACT TROPOPAUSE
C
cpssep03      IF((IVS(ILEV).AND.16).EQ.16) THEN
      IF(IAND(IVS(ILEV),16).EQ.16) THEN
           PTROP=PP(ILEV)
           TTROP=TV(ILEV)
           LTROP=.TRUE.
      ENDIF
  112 CONTINUE
      IF(NSTD.LE.1) GO TO 400
C
C          1.2  INITIALIZE ARRAYS
C
      DO 122 ILEV=1,NLEV
      E (ILEV)=RMISS
      D (ILEV)=RMISS
      DA(ILEV)=RMISS
      DB(ILEV)=RMISS
  122 CONTINUE
C
C    -------------------------------------------------------------------
C
C                2.  CALCULATE EXTREME THICKNESSES
C               -----------------------------------
C
  200 CONTINUE
      DO 265 ILEV=1,NSTD-1
C
C              2.0  ENSURE STANDARD LEVELS ARE ADJACENT
C
      IF(IABS(IPOS(ILEV)-IPOS(ILEV+1)).GT.1) GO TO 265
      IF(STDTV(ILEV).NE.RMISS.AND.STDTC(ILEV+1).NE.RMISS) THEN
           RAL=(R/(2.0*RG))*ALOG(STDPP(ILEV)/STDPP(ILEV+1))
           SU=(STDPP(ILEV+1)/STDPP(ILEV))**U
C
C              2.1  CALCULATED THICKNESS
C
           D(ILEV)=RAL*(STDTV(ILEV)+STDTC(ILEV+1))
C
C              2.2  TEMPERATURE AT ILEV+1 EXTRAPOLATED FROM ILEV
C
           TTI1=STDTV(ILEV)*SU
C
C              2.3  TEMPERATURE AT ILEV EXTRAPOLATED FROM ILEV+1
C
           TTI=STDTC(ILEV+1)/SU
C
C              2.4  MAXIMUM POSSIBLE THICKNESS
C
           DA(ILEV)=RAL*(TTI+STDTC(ILEV+1))
C
C              2.5  MINIMUM POSSIBLE THICKNESS
C
           DB(ILEV)=RAL*(STDTV(ILEV)+TTI1)
C
C              2.6  THICKNESS DEPARTURE
C
           IF(STDZ(ILEV).NE.RMISS.AND.STDZ(ILEV+1).NE.RMISS.AND.
     X        IFLAG(1,ISTDZ(ILEV)).GE.ICRIT.AND.
     X        IFLAG(1,ISTDZ(ILEV+1)).GE.ICRIT)
     X     E(ILEV)=STDZ(ILEV+1)-STDZ(ILEV)-D(ILEV)
C          WRITE (*,'(1H ,''HYDRO'',2I3,10F10.2)')
C    X       IPOS(ILEV),IPOS(ILEV+1),
C    X       STDPP(ILEV),STDTV(ILEV),STDTC(ILEV),STDZ(ILEV),D(ILEV),
C    X       TTI1,TTI,DA(ILEV),DB(ILEV),E(ILEV)
      ENDIF
  265 CONTINUE
C
C    -------------------------------------------------------------------
C
C                  3.  COMPARE WITH TOLERANCE
C                ------------------------------
C
  300 CONTINUE
      LFLAG=.FALSE.
      DO 375 ILEV=1,NSTD-1
      IF(LFLAG) THEN
C
C           3.1  SKIP ALREADY FLAGGED LEVEL
C
           LFLAG=.FALSE.
           GO TO 375
      ENDIF
C
C           3.2  CALCULATE TOLERANCE
C
      IF(E(ILEV).NE.RMISS) THEN
           TOL=0.375*ABS(DA(ILEV)-DB(ILEV))
           IF(TOL.LT.20.0) TOL=20.0
           IF(STDPP(ILEV).GT.40000.0) THEN
                IF(TOL.GT.50.0) TOL=50.0
           ELSE
                IF(TOL.GT.80.0) TOL=80.0
           ENDIF
C          WRITE (*,'(1H ,''HYDRO'',3F10.2)') STDPP(ILEV),TOL,E(ILEV)
           IF(ABS(E(ILEV)).GT.TOL) THEN
C
C        3.3  E EXCEEDS TOLERANCE; COMPARE WITH ADJACENT LAYER
C
                IF(E(ILEV+1).NE.RMISS.AND.ABS(E(ILEV+1)).GT.0.001) THEN
                     F=E(ILEV)/E(ILEV+1)
C                    WRITE (*,'(1H ,''HYDRO FAIL'',5F10.2)')
C    X                  STDPP(ILEV),TOL,E(ILEV),E(ILEV+1),F
                     IF(F.GT.0.5.AND.F.LT.2.0) THEN
C
C        3.4  TEMPERATURE AT ILEV+1 IS SUSPECT
C
                          CALL SETGRS (ISTDTT(ILEV+1),3)
                          CALL SETGRS (ISTDTD(ILEV+1),3)
C                          WRITE (*,'(1H ,'' HYDROSTATIC CHECK   '',
C     X                    A,2X,I4,4I2.2,2(A,F10.2))')
C     X                    CALLSN,IY,IM,ID,IH,IN,
C     X                    ' PP:',STDPP(ILEV+1),', TV:',STDTV(ILEV+1)
                          LFLAG=.TRUE.
                     ELSEIF(F.GT.-2.0.AND.F.LT.-0.5) THEN
C
C        3.5  GEOPOTENTIAL AT ILEV+1 IS SUSPECT
C
                          CALL SETGRS (ISTDZ(ILEV+1),3)
C                          WRITE (*,'(1H ,'' HYDROSTATIC CHECK   '',
C     X                    A,2X,I4,4I2.2,2(A,F10.2))')
C     X                    CALLSN,IY,IM,ID,IH,IN,
C     X                    ' PP:',STDPP(ILEV+1),',  Z:',STDZ(ILEV+1)
                          LFLAG=.TRUE.
                     ELSEIF(ABS(F).GE.2.0) THEN
C
C        3.6  ALL HEIGHTS AT AND ABOVE ILEV+1 ARE SUSPECT
C
                          DO 362 I=ILEV+1,NSTD
                          CALL SETGRS (ISTDZ(I),3)
C                          WRITE (*,'(1H ,'' HYDROSTATIC CHECK   '',
C     X                    A,2X,I4,4I2.2,2(A,F10.2))')
C     X                    CALLSN,IY,IM,ID,IH,IN,
C     X                    ' PP:',STDPP(I),',  Z:',STDZ(I)
  362                     CONTINUE
                          GO TO 400
                     ENDIF
                ENDIF
C
C        3.7  NO DEFINITE DECISION
C
                IF(.NOT.LFLAG) THEN
                     CALL SETINT (IFAIL,
     X                             ISTDZ(ILEV), ISTDZ(ILEV+1),
     X                            ISTDTT(ILEV),ISTDTT(ILEV+1),
     X                            ISTDTD(ILEV),ISTDTD(ILEV+1),IEND)
C                     WRITE (*,'(1H ,'' HYDROSTATIC CHECK   '',
C     X               A,2X,I4,4I2.2,3(A,F10.2)/1H ,44X,3(A,F10.2))')
C     X               CALLSN,IY,IM,ID,IH,IN,
C     X  ' PP:',STDPP(ILEV),  ',  Z:',STDZ(ILEV),  ', TV:',STDTV(ILEV),
C     X  ' PP:',STDPP(ILEV+1),',  Z:',STDZ(ILEV+1),', TV:',STDTV(ILEV+1)
                ENDIF
           ENDIF
      ENDIF
  375 CONTINUE
C
C    ---------------------------------------------------------------------
C
C                       4.  EXIT
C                     -----------
C
  400 CONTINUE
      RETURN
      END
