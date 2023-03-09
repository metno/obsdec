      SUBROUTINE DPLIM (Z,RLAT,RLONG,PLIM)
C
C**** *DPLIM*
C
C       PURPOSE.
C      ----------
C
C           OBTAIN LIMITS OF PRESSURE GIVEN HEIGHT
C
C       INTERFACE.
C      ------------
C
C         CALL DPLIM (Z,RLAT,RLONG,PLIM)
C
C       INPUT
C        Z     -  HEIGHT LEVEL
C        RLAT  -  LATITUDE VALUE FOR REPORT
C        RLONG -  LONGITUDE VALUE FOR REPORT
C
C       OUTPUT
C        PLIM  -  GROSS LIMITS FOR PRESSURE
C
C       METHOD.
C      ---------
C
C         FROM LIMIT TABLE FOR UPPER AIR HEIGHTS,
C         LINEARLY INTERPLOTATE TO OBTAIN GROSS LIMITS FOR PRESSURE
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
C          CHAPTER 6, QUALITY CONTROL PROCEDURES
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
C
      DIMENSION PLIM(NMM)
C
C    -------------------------------------------------------------------
C
C             1.  DETERMINE WHICH AREA REPORT FALLS INTO
C            --------------------------------------------
C
  100 CONTINUE
      IAREA=1
      IF(ABS(RLAT).GT.RLLIM) IAREA=2
C
C    --------------------------------------------------------------------
C
C             2.  LINEAR INTERPOLATION
C           -----------------------------
C
  200 CONTINUE
      DO 234 IMM=1,NMM
      IF(Z.GE.UZ(IMM,IAREA,NLEV)) THEN
C
C           2.1  Z OUTSIDE TOP OF TABLE
C
           PLIM(IMM)=UPZ(NLEV,1)
           GO TO 234
      ELSEIF(Z.LE.UZ(IMM,IAREA,1)) THEN
C
C           2.2  Z OUTSIDE BOTTOM OF TABLE
C
           PLIM(IMM)=UPZ(1,1)
           GO TO 234
      ELSE
C
C           2.3  INTERPOLATE
C
           DO 232 ILEV=1,NLEV-1
           IF(Z.GT.UZ(IMM,IAREA,ILEV).AND.Z.LE.UZ(IMM,IAREA,ILEV+1))
     X     THEN
                PLIM(IMM)=
     X          (UPZ(ILEV+1,1)*(Z-UZ(IMM,IAREA,ILEV))+
     X           UPZ(ILEV,1)  *(UZ(IMM,IAREA,ILEV+1)-Z))/
     X          (UZ(IMM,IAREA,ILEV+1)-UZ(IMM,IAREA,ILEV))
                GO TO 234
           ENDIF
  232      CONTINUE
      ENDIF
  234 CONTINUE
C
C    -------------------------------------------------------------------
C
C                   3.  EXIT
C                  ----------
C
  300 CONTINUE
      RETURN
      END
