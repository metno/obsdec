C                          
C      COMMON *CTUA* - GROSS LIMIT VALUES FOR UPPER AIR PARAMETERS
C
C          UPZ  -  TABLE OF STANDARD PRESSURE AND HEIGHT LEVELS
C          UZ   -  TABLE FOR GEOPOTENTIAL
C          UTT  -  TABLE FOR TEMPERATURE
C          UFF  -  TABLE FOR WIND SPEED
C          UTL  -  TABLE FOR THICKNESS
C          UWL  -  TABLE FOR PRECIPITABLE WATER
C
      COMMON /CTUA/ UPZ,UZ,UTT,UFF,UTL,UWL
      DIMENSION UZ (NMM,NAREA,NLEV)
      DIMENSION UTT (NMM,NAREA,NLEV)
      DIMENSION UFF (NMM,NAREA,NLEV)
      DIMENSION UTL (NMM,NAREA,NLEV)
      DIMENSION UWL (NMM,NAREA,NLEV)
      DIMENSION UPZ (NLEV,2)
C
C    --------------------------------------------------------------------