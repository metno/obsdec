C
C      COMMON *CTSF* - GROSS LIMIT VALUES FOR SURFACE PARAMETERS
C
C         SFF  -  TABLE FOR WIND SPEED
C         STT  -  TABLE FOR TEMPERATURE
C         STD  -  TABLE FOR DEWPOINT
C         SPS  -  TABLE FOR STATION PRESSURE
C         SPPP -  TABLE FOR MEAN SEA LEVEL PRESSURE
C         SPP  -  TABLE FOR PRESSURE TENDENCY
C         STW  -  TABLE FOR SEA SURFACE TEMPERATURE
C
      COMMON /CTSF/ SFF,STT,STD,SPS,SPPP,SPP,STW
      DIMENSION SFF (NMM,NSEASN,NAREA)
      DIMENSION STT (NMM,NSEASN,NAREA)
      DIMENSION STD (NMM,NSEASN,NAREA)
      DIMENSION SPS (NMM,NSEASN,NAREA)
      DIMENSION SPPP (NMM,NSEASN,NAREA)
      DIMENSION SPP (NMM,NSEASN,NAREA)
      DIMENSION STW (NMM,NSEASN,NAREA)
C 
C    -----------------------------------------------------------------------
