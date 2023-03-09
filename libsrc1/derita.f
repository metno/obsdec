      SUBROUTINE DERITA (IERR,MPAR)
C
C**** *DERITA*
C
C       PURPOSE.
C      ----------
C
C              SET UP LIMIT TABLES AND INITIALISE CONFIDENCE ARRAY
C
C       INTERFACE.
C      -------------
C
C         CALL DERITA (IERR,MPAR)
C
C        INPUT
C         MPAR    -  NUMBER OF PARAMETERS IN REPORT
C
C        OUTPUT
C         IERR    -  ERROR CODE
C         GROSS LIMIT TABLES INITIALISED
C         IFLAG (CONF) - CONFIDENCE FLAGS INITIALISED
C         THICKNESS LIMIT TABLE DERIVED
C
C       METHOD.
C      ---------
C
C         SURFACE AND UPPER AIR LIMIT TABLES IN DATA STATEMENTS
C         CONFIDENCE FLAGS SET TO UNCHECKED VALUE
C         ON FIRST CALL, THICKNESS LIMIT TABLE DERIVED
C
C
C       EXTERNALS.
C      ------------
C
C         NONE
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
C         B. NORRIS,  ECMWF,  JANUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
      INCLUDE 'paramq.f'
      INCLUDE 'ctua.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
      INCLUDE 'ctsf.f'
C
C    --------------------------------------------------------------------
C
C                  1.  SET CONSTANT VALUES
C                 --------------------------
C
  100 CONTINUE
C            MISSING DATA INDICATORS
      DATA RMISS /999999./,IMISS/999999/
C            GAS CONSTANT; GRAVITY; LAPSE RATE
      DATA R/287.0/,RG/9.8/,RGAMMA/0.0065/
C            ABSOLUTE TEMPERATURE; SPECIFIC HEAT OF DRY AIR
      DATA RABS/273.16/,RCP/1003.1/
C            VARIABLE WIND DIRECTION
cps Note that in BUFR 360 should be used if direction is 0 degrees.
cps 0 is reserved for variable direction or calm (wind speed = 0).
      DATA RVAR/0.0/
C            LATITUDE LIMIT FOR DETERMINING AREA WITHIN TABLES
      DATA RLLIM/45.0/
C            PASS, FAIL, END INDICATORS
cpsjul10      DATA IPASS/1/,IFAIL/-1/,IEND/3HEND/
      DATA IPASS/1/,IFAIL/-1/,IEND/-1/
C            UNCHECKED FLAG VALUE
      DATA IUNCH/70/
C
C    -------------------------------------------------------------------
C
C               2.  SET CONFIDENCE CORRECTIONS
C              --------------------------------
C
  200 CONTINUE
C
C           2.1  GROSS LIMIT CHECKS
C
C            DIM1 = CONFIDENCE CORRECTION, DIM2 = PASS/FAIL
      DATA (ICBO(I,1),I=1,NPOSS)/
C       IMPOSS VALUE    OUTSIDE MAX2   MAX2>X>MAX1    MAX1>X>MIN1
     X     -100,            -60,           -30,            0/
      ICBO (1,2)=IFAIL
      ICBO (2,2)=IFAIL
      ICBO (3,2)=0
      ICBO (4,2)=IPASS
C
C            2.2  INTERNAL CONSISTENCY CONFIDENCE UPDATES
C
      DATA ((ICOCO(I,J),I=1,NCOCO),J=1,2)/
C        FAIL        1ST   2ND   3RD   4TH   5TH
     X               -10,  -25,  -20,  -15,  -10,
C        PASS        1ST   2ND   3RD   4TH   5TH
     X                 5,    4,    3,    2,    1/
C
C    --------------------------------------------------------------------
C
C              3.  GROSS LIMITS FOR SURFACE PARAMETERS
C            -------------------------------------------
C
  300 CONTINUE
C
C          3.1  LIMIT TABLE FOR SURFACE WIND SPEED
C
      DATA (((SFF(I,J,K),I=1,NMM),J=1,NSEASN),K=1,NAREA)/
C                WINTER                        SUMMER
C        MIN2  MIN1   MAX1  MAX2       MIN2  MIN1   MAX1  MAX2
     X   0.0,  0.0,  60.0, 125.0,      0.0,  0.0,  90.0, 150.0,
     X   0.0,  0.0,  50.0, 100.0,      0.0,  0.0,  40.0,  75.0/
C
C          3.2  LIMIT TABLE FOR SURFACE TEMPERATURE
C
      DATA (((STT(I,J,K),I=1,NMM),J=1,NSEASN),K=1,NAREA)/
C                WINTER                        SUMMER
C        MIN2  MIN1   MAX1  MAX2       MIN2  MIN1   MAX1  MAX2
     X -40.0,-30.0,  50.0,  55.0,    -30.0,-20.0,  50.0,  60.0,
     X -90.0,-80.0,  35.0,  40.0,    -40.0,-30.0,  40.0,  50.0/
C
C          3.3  LIMIT TABLE FOR SURFACE DEWPOINT
C
      DATA (((STD(I,J,K),I=1,NMM),J=1,NSEASN),K=1,NAREA)/
C                WINTER                        SUMMER
C        MIN2  MIN1   MAX1  MAX2       MIN2  MIN1   MAX1  MAX2
     X -45.0,-35.0,  35.0,  40.0,    -35.0,-25.0,  35.0,  40.0,
     X -99.0,-85.0,  30.0,  35.0,    -45.0,-35.0,  35.0,  40.0/
C
C          3.4  LIMIT TABLE FOR STATION PRESSURE
C
      DATA (((SPS(I,J,K),I=1,NMM),J=1,NSEASN),K=1,NAREA)/
C        MIN2      MIN1       MAX1      MAX2
C                WINTER   AREA 1
     X 30000.0,  40000.0,  108000.0,  110000.0,
C                SUMMER   AREA 1
     X 30000.0,  40000.0,  108000.0,  110000.0,
C                WINTER   AREA 2
     X 30000.0,  40000.0,  108000.0,  110000.0,
C                SUMMER   AREA 2
     X 30000.0,  40000.0,  108000.0,  110000.0/
C
C          3.5  LIMIT TABLE FOR MEAN SEA LEVEL PRESSURE
C
      DATA (((SPPP(I,J,K),I=1,NMM),J=1,NSEASN),K=1,NAREA)/
C        MIN2      MIN1       MAX1      MAX2
C                WINTER   AREA 1
     X 87000.0,  91000.0,  108000.0,  110000.0,
C                SUMMER   AREA 1
     X 85000.0,  90000.0,  108000.0,  110000.0,
C                WINTER   AREA 2
     X 88000.0,  91000.0,  108000.0,  110000.0,
C                SUMMER   AREA 2
     X 88000.0,  92000.0,  108000.0,  110000.0/
C
C          3.6  LIMIT TABLE FOR 3-HOUR MEAN SEA LEVEL PRESSURE TENDENCY
C
      DATA (((SPP(I,J,K),I=1,NMM),J=1,NSEASN),K=1,NAREA)/
C        MIN2      MIN1       MAX1       MAX2
C                WINTER   AREA 1
     X -5000.0,  -4000.0,    4000.0,    5000.0,
C                SUMMER   AREA 1
     X -5000.0,  -4000.0,    4000.0,    5000.0,
C                WINTER   AREA 2
     X -5000.0,  -4000.0,    4000.0,    5000.0,
C                SUMMER   AREA 2
     X -5000.0,  -4000.0,    4000.0,    5000.0/
C
C          3.7  LIMIT TABLE FOR SEA SURFACE TEMPERATURE
C
      DATA (((STW(I,J,K),I=1,NMM),J=1,NSEASN),K=1,NAREA)/
C                WINTER                        SUMMER
C        MIN2  MIN1   MAX1  MAX2       MIN2  MIN1   MAX1  MAX2
     X   0.0,  2.0,  32.0,  35.0,      0.0,  2.0,  32.0,  35.0,
     X  -2.1, -1.0,  27.0,  30.0,     -2.1, -1.0,  30.0,  35.0/
C
C    -------------------------------------------------------------------
C
C                4.  TABLE FOR UPPER AIR LEVELS
C               --------------------------------
C
  400 CONTINUE
      DATA ((UPZ(I,J),J=1,2),I=1,10)/
C              PRESSURE          HEIGHT
     X         110000.0,         -600.0,
     X         100000.0,          300.0,
     X          85000.0,         1500.0,
     X          70000.0,         3000.0,
     X          50000.0,         5500.0,
     X          40000.0,         7000.0,
     X          30000.0,         9000.0,
     X          25000.0,        10000.0,
     X          20000.0,        12000.0,
     X          15000.0,        14000.0/
      DATA ((UPZ(I,J),J=1,2),I=11,NLEV)/
C              PRESSURE          HEIGHT
     X          10000.0,        16500.0,
     X           7000.0,        18500.0,
     X           5000.0,        20000.0,
     X           3000.0,        22000.0,
     X           2000.0,        26000.0,
     X           1000.0,        30000.0,
     X            700.0,        33000.0,
     X            500.0,        36000.0,
     X            300.0,        39000.0,
     X            200.0,        42000.0,
     X            100.0,        48000.0,
     X              1.0,        99999.0/
C
C    -------------------------------------------------------------------
C
C                5.  GROSS LIMITS FOR UPPER AIR PARAMETERS
C              --------------------------------------------
C
  500 CONTINUE
C
C           5.1  LIMIT TABLE FOR UPPER AIR HEIGHTS
C
      DATA (((UZ(I,J,K),I=1,NMM),J=1,NAREA),K=1,10)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X -1800.0,-1600.0, -200.0,    0.0,-1500.0,-1350.0, -150.0,    0.0,
     X -1000.0, -800.0,  600.0,  800.0, -700.0, -550.0,  650.0,  800.0,
     X     0.0,  200.0, 2000.0, 2200.0,  200.0,  400.0, 2000.0, 2200.0,
     X  2200.0, 2350.0, 3450.0, 3600.0, 2300.0, 2450.0, 3450.0, 3600.0,
     X  4500.0, 4700.0, 6100.0, 6300.0, 4500.0, 4700.0, 6100.0, 6300.0,
     X  6100.0, 6300.0, 7800.0, 8000.0, 6100.0, 6300.0, 7800.0, 8000.0,
     X  7300.0, 7550.0, 9800.0,10100.0, 7300.0, 7550.0, 9800.0,10100.0,
     X  8500.0, 8800.0,11100.0,11400.0, 8500.0, 8800.0,11100.0,11400.0,
     X 10000.0,10300.0,12900.0,13200.0,10000.0,10300.0,12900.0,13200.0,
     X 12000.0,12300.0,14900.0,15200.0,12000.0,12300.0,14900.0,15200.0/
      DATA (((UZ(I,J,K),I=1,NMM),J=1,NAREA),K=11,NLEV)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X 14000.0,14400.0,17700.0,18100.0,14000.0,14400.0,17700.0,18100.0,
     X 15500.0,16100.0,20900.0,21500.0,15500.0,16100.0,20900.0,21500.0,
     X 17700.0,18300.0,23100.0,23700.0,17700.0,18300.0,23100.0,23700.0,
     X 20500.0,21100.0,25900.0,26500.0,20500.0,21100.0,25900.0,26500.0,
     X 23300.0,23900.0,29700.0,30300.0,23300.0,23900.0,29700.0,30300.0,
     X 26000.0,26800.0,33200.0,34000.0,26000.0,26800.0,33200.0,34000.0,
     X 30700.0,31300.0,35800.0,36400.0,30700.0,31300.0,35800.0,36400.0,
     X 33300.0,33800.0,37800.0,38300.0,33300.0,33800.0,37800.0,38300.0,
     X 36600.0,37100.0,41600.0,42100.0,36600.0,37100.0,41600.0,42100.0,
     X 39400.0,39800.0,44400.0,44900.0,39400.0,39800.0,44400.0,44900.0,
     X 44900.0,45500.0,50100.0,50700.0,44900.0,45500.0,50100.0,50700.0,
     X 99999.0,99999.0,99999.0,99999.0,99999.0,99999.0,99999.0,99999.0/
C
C           5.2  LIMIT TABLE FOR UPPER AIR TEMPERATURES
C
      DATA (((UTT(I,J,K),I=1,NMM),J=1,NAREA),K=1,10)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X   -50.0,  -30.0,   50.0,   60.0,  -90.0,  -70.0,   40.0,   50.0,
     X   -50.0,  -30.0,   50.0,   60.0,  -90.0,  -70.0,   40.0,   50.0,
     X   -65.0,  -50.0,   30.0,   40.0,  -90.0,  -70.0,   20.0,   30.0,
     X   -80.0,  -70.0,   20.0,   30.0,  -90.0,  -70.0,   10.0,   20.0,
     X   -95.0,  -80.0,    5.0,   10.0, -100.0,  -80.0,   -5.0,    5.0,
     X  -100.0,  -85.0,   -5.0,    0.0, -100.0,  -85.0,  -10.0,   -5.0,
     X  -100.0,  -85.0,  -10.0,   -5.0, -100.0,  -85.0,  -10.0,   -5.0,
     X  -100.0,  -85.0,  -10.0,   -5.0, -100.0,  -85.0,  -10.0,   -5.0,
     X  -100.0,  -85.0,  -10.0,   -5.0, -100.0,  -85.0,  -10.0,   -5.0,
     X  -100.0,  -85.0,  -10.0,   -5.0, -100.0,  -85.0,  -10.0,   -5.0/
         DATA (((UTT(I,J,K),I=1,NMM),J=1,NAREA),K=11,NLEV)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X  -100.0,  -85.0,  -10.0,   -5.0, -100.0,  -85.0,  -10.0,   -5.0,
     X  -100.0,  -85.0,   -5.0,    5.0, -100.0,  -85.0,   -5.0,    5.0,
     X  -100.0,  -85.0,   -5.0,    5.0, -100.0,  -85.0,   -5.0,    5.0,
     X  -100.0,  -85.0,   -5.0,    5.0, -100.0,  -85.0,   -5.0,    5.0,
     X  -100.0,  -85.0,   -5.0,    5.0, -100.0,  -85.0,   -5.0,    5.0,
     X  -100.0,  -85.0,   -5.0,    5.0, -100.0,  -85.0,   -5.0,    5.0,
     X   -90.0,  -80.0,   10.0,   20.0,  -90.0,  -80.0,   10.0,   20.0,
     X   -80.0,  -70.0,   15.0,   30.0,  -80.0,  -70.0,   15.0,   30.0,
     X   -70.0,  -60.0,   25.0,   35.0,  -70.0,  -60.0,   25.0,   35.0,
     X   -70.0,  -60.0,   30.0,   40.0,  -70.0,  -60.0,   30.0,   40.0,
     X   -70.0,  -60.0,   30.0,   40.0,  -70.0,  -60.0,   30.0,   40.0,
     X   -70.0,  -60.0,   30.0,   40.0,  -70.0,  -60.0,   30.0,   40.0/
C
C           5.3  LIMIT TABLE FOR UPPER AIR WIND SPEED
C
      DATA (((UFF(I,J,K),I=1,NMM),J=1,NAREA),K=1,10)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X     0.0,    0.0,   60.0,  100.0,    0.0,    0.0,   60.0,  100.0,
     X     0.0,    0.0,   60.0,  100.0,    0.0,    0.0,   60.0,  100.0,
     X     0.0,    0.0,   65.0,  100.0,    0.0,    0.0,   65.0,  100.0,
     X     0.0,    0.0,   70.0,  100.0,    0.0,    0.0,   70.0,  100.0,
     X     0.0,    0.0,  100.0,  120.0,    0.0,    0.0,  100.0,  120.0,
     X     0.0,    0.0,  130.0,  150.0,    0.0,    0.0,  130.0,  150.0,
     X     0.0,    0.0,  160.0,  180.0,    0.0,    0.0,  160.0,  180.0,
     X     0.0,    0.0,  160.0,  180.0,    0.0,    0.0,  160.0,  180.0,
     X     0.0,    0.0,  160.0,  180.0,    0.0,    0.0,  160.0,  180.0,
     X     0.0,    0.0,  150.0,  170.0,    0.0,    0.0,  150.0,  170.0/
         DATA (((UFF(I,J,K),I=1,NMM),J=1,NAREA),K=11,NLEV)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X     0.0,    0.0,  150.0,  170.0,    0.0,    0.0,  150.0,  170.0,
     X     0.0,    0.0,  150.0,  170.0,    0.0,    0.0,  150.0,  170.0,
     X     0.0,    0.0,  150.0,  170.0,    0.0,    0.0,  150.0,  170.0,
     X     0.0,    0.0,   90.0,  110.0,    0.0,    0.0,   90.0,  110.0,
     X     0.0,    0.0,   90.0,  110.0,    0.0,    0.0,   90.0,  110.0,
     X     0.0,    0.0,   75.0,   95.0,    0.0,    0.0,   75.0,   95.0,
     X     0.0,    0.0,   80.0,  100.0,    0.0,    0.0,   80.0,  100.0,
     X     0.0,    0.0,  120.0,  140.0,    0.0,    0.0,  120.0,  140.0,
     X     0.0,    0.0,  150.0,  170.0,    0.0,    0.0,  150.0,  170.0,
     X     0.0,    0.0,  200.0,  220.0,    0.0,    0.0,  200.0,  220.0,
     X     0.0,    0.0,  200.0,  220.0,    0.0,    0.0,  200.0,  220.0,
     X     0.0,    0.0,  200.0,  220.0,    0.0,    0.0,  200.0,  220.0/
C
C           5.4  LIMIT TABLE FOR UPPER AIR PRECIPITABLE WATER
C
      DATA (((UWL(I,J,K),I=1,NMM),J=1,NAREA),K=1,10)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X     0.0,    0.0,  0.100,  0.150,    0.0,    0.0,  0.100,  0.150,
     X     0.0,    0.0,  0.100,  0.150,    0.0,    0.0,  0.100,  0.150,
     X     0.0,    0.0,  0.040,  0.060,    0.0,    0.0,  0.040,  0.060,
     X     0.0,    0.0,  0.030,  0.045,    0.0,    0.0,  0.030,  0.045,
     X     0.0,    0.0,  0.020,  0.030,    0.0,    0.0,  0.020,  0.030,
     X     0.0,    0.0,  0.015,  0.025,    0.0,    0.0,  0.015,  0.025,
     X     0.0,    0.0,  0.010,  0.015,    0.0,    0.0,  0.010,  0.015,
     X     0.0,    0.0,  0.005,  0.008,    0.0,    0.0,  0.005,  0.008,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002/
         DATA (((UWL(I,J,K),I=1,NMM),J=1,NAREA),K=11,NLEV)/
C                  AREA1                         AREA2
C        MIN2     MIN1    MAX1    MAX2    MIN2    MIN1    MAX1    MAX2
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002,
     X     0.0,    0.0,  0.001,  0.002,    0.0,    0.0,  0.001,  0.002/
C
C    ------------------------------------------------------------------
C
C              6.   DERIVE THICKNESS LIMITS
C             -------------------------------
C
  600 CONTINUE
      IERR=0
cpsjul10      IF(IFIRST.NE.3H1ST) THEN
      IFIRST=0
      IF(IFIRST.NE.1) THEN
           DO 606 IAREA=1,NAREA
           DO 604 IMM=1,NMM
           DO 602 ILEV=1,NLEV-1
           UTL(IMM,IAREA,ILEV)=(R/RG)*
     X     ((UTT(IMM,IAREA,ILEV)+RABS+UTT(IMM,IAREA,ILEV+1)+RABS)/2.0)*
     X     ALOG(UPZ(ILEV,1)/UPZ(ILEV+1,1))
  602      CONTINUE
  604      CONTINUE
  606      CONTINUE
cpsjul10           IFIRST=3H1ST
           IFIRST=1
      ENDIF
C
C    --------------------------------------------------------------------
C
C               7.  INITIALISE FLAG ARRAY
C             ------------------------------
C
  700 CONTINUE
      IF(MPAR.GT.NPARAM) THEN
           WRITE (*,'(1H ,''   FLAG ARRAY TOO SMALL, NUMBER OF ENTRIES R
     XEQUIRED:'',I7)') MPAR
           IERR=2901
           GO TO 800
      ENDIF
      DO 702 I=1,MPAR
C         CONFIDENCE = UNCHECKED VALUE
      IFLAG(1,I)=IUNCH
C         NUMBER OF FAILS
      IFLAG(2,I)=0
C         NUMBER OF PASSES
      IFLAG(3,I)=0
  702 CONTINUE
C
C    ------------------------------------------------------------------
C
C                    8.  EXIT
C                  -------------
C
  800 CONTINUE
      RETURN
      END
