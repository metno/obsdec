      SUBROUTINE D2PTS (RLAT1,RLON1,RLAT2,RLON2,ZSCALE,DIST)
C
C****  *D2PTS*
C
C       PURPOSE.
C      ----------
C
C         CALCULATE THE DISTANCE BETWEEN 2 POINTS ON THE
C                   EARTH'S SURFACE
C
C       INTERFACE.
C      ------------
C
C           CALL D2PTS (RLAT1,RLON1,RLAT2,RLON2,ZSCALE,DIST)
C
C        INPUT
C          RLAT1,RLON1 -  LATITUDE AND LONGITUDE OF FIRST POSITION
C          RLAT2,RLON2 -  LATITUDE AND LONGITUDE OF SECOND POSITION
C          ZSCALE  -  SCALING FACTOR
C                   ZSCALE=1.      GIVES DISTANCE IN EARTH RADII
C                   ZSCALE=3960.   GIVES DISTANCE IN MILES
C                   ZSCALE=6371.   GIVES DISTANCE IN KM
C
C        OUTPUT
C          DIST  -  DISTANCE IN UNITS GIVEN BY ZSCALE
C
C       METHOD.
C      ---------
C
C         DELTAX = COS(LAT1)*COS(LON1) - COS(LAT2)*COS(LON2)
C         DELTAY = COS(LAT1)*SIN(LON1) - COS(LAT2)*SIN(LON2)
C         DELTAX = SIN(LAT1) - SIN(LAT2)
C         DIST = SQRT (DELTAX**2 + DELTAY**2 + DELTAZ**2)
C
C       EXTERNALS.
C      ------------
C
C         NONE
C
C       REFERENCE.
C      ------------
C
C         NONE
C
C       AUTHOR.
C      ---------
C
C         B. NORRIS,  ECMWF,  MARCH 1990
C         BASED ON CODE WRITTEN BY DATA ASSIMILATION
C
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
C
C    -------------------------------------------------------------------
C
C                     1.  INITIALISATION
C                   ----------------------
C
  100 CONTINUE
C              PIF = (PI/180)
      DATA PIF/0.017453/
C
C    --------------------------------------------------------------------
C
C                     2.  CALCULATION
C                   -------------------
C
  200 CONTINUE
      ALA1=RLAT1*PIF
      ALO1=RLON1*PIF
      ALA2=RLAT2*PIF
      ALO2=RLON2*PIF
      CALA1=COS(ALA1)
      CALA2=COS(ALA2)
      DELX=CALA1*COS(ALO1)-CALA2*COS(ALO2)
      DELY=CALA1*SIN(ALO1)-CALA2*SIN(ALO2)
      DELZ=SIN(ALA1)-SIN(ALA2)
      DIST=SQRT(DELX*DELX+DELY*DELY+DELZ*DELZ)*ZSCALE
C
C    ----------------------------------------------------------------------
C
C                         3.  EXIT
C                       ------------
C
  300 CONTINUE
      RETURN
      END
