      SUBROUTINE BIASCOR (LBC,NST,SPP,SZ,SZNBC,
     X                    YSTIDBC,YSTYPBC,NSTBC,YSNAMBC,RCORBC,NSOBC, 
     X                    CIDENT,IM,ID,IH,RLAT,RLONG)
C
C**** *BIASCOR*
C
C       PURPOSE.
C      ----------
C
C          BIASCORRECTION FOR GEOPOTENTIAL HEIGHTS OF TEMPS
C
C       INTERFACE.
C      ------------
C
C          CALL BIASCOR (IERR,NST,SPP,SZ,SZNBC,
C     X                  YSTIDBC,YSTYPBC,NSTBC,YSNAMBC,RCORBC,NSOBC, 
C     X                  CIDENT,IM,ID,IH,IN,RLAT,RLONG)
C
C        INPUT
C         NST       NUMBER OF LEVELS
C         SPP       ARRAY WITH PRESSURE VALUES
C         SZ        ARRAY WITH Z VALUES
C         YSTIDBC   ARRAY WITH TEMP IDENTIFIER
C         YSTYPBC   ARRAY WITH SONDE TYPES
C         NSTBC     NUMBER OF ENTRIES IN T1ID AND T1TYP
C         YSNAMBC   ARRAY WITH SONDE TYPES
C         RCORBC    ARRAY (3DIM) WITH BIAS CORRECTION VALUES
C         NSOBC     NUMBER OF ENTRIES IN T2TYP
C         CIDENT    STATION IDENTIFIER
C         IM        MONTH
C         ID        DAY
C         IH        HOUR
C         RLAT      LATITUDE
C         RLONG     LONGITUDE
C         
C
C        OUTPUT
C         LBC       LOGICAL TO STATE IF BIAS COR. WAS SUCCESSFUL
C         SZNBC     ARRAY WITH BIAS CORRECTED Z VALUES
C
C       METHOD.
C      ---------
C
C        
C
C       EXTERNALS.
C      ------------
C
C        DIURNAL
C        PNTERP
C
C       REFERENCE.
C      ------------
C
C       AUTHOR.
C      ---------
C
C
C
C       MODIFICATIONS.
C      ----------------
C
C-------------------------------------------------------------------
      IMPLICIT LOGICAL (L,O,G)
      INCLUDE 'parameter.f'
      INCLUDE 'paramq.f'
      INCLUDE 'paramsh.f'
      PARAMETER (IUNIQ=12)
      PARAMETER (JTEMP=100)
      INCLUDE 'comwt.f'
      INCLUDE 'comkey.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
      DIMENSION SPP(JTEMP)
      DIMENSION SZ(JTEMP)
      CHARACTER*9 CIDENT
      INCLUDE 'parabias.f'
C-------------------------------------------------------------------
C
C          1.BC  DETERMINE SONDE TYPE
C
      DO 222 I=1,NSTBC
      IF(CIDENT.EQ.YSTIDBC(I)) THEN
           KSOBC=I
           GO TO 224
      ENDIF
  222 CONTINUE
      KSOBC=0
  224 CONTINUE
C
C
crr 20.03.2002 
crr The following statement causes the program to stop when compiled with -C.
crr  and the value of YSTYPBC(KSOBC) is therefore unpredictable when KSOBC = 0. 
crr Logically the compiler should not evaluate YSTYPBC(KSOBC) if KSOBC.EQ.0
crr  but it does so.   
crr 20.03.2002      IF(KSOBC.EQ.0.OR.YSTYPBC(KSOBC).EQ.'unknown'.OR.
      IF(KSOBC.eq.0) then
            LBC=.FALSE.
            GO TO 400
      ELSEIF(KSOBC.NE.0) then
         IF(YSTYPBC(KSOBC).EQ.'unknown'.OR.
     X                 YSTYPBC(KSOBC).EQ.'Vais/Gr'.OR.
     X                 YSTYPBC(KSOBC).EQ.'Mesei'.OR.
     X                 YSTYPBC(KSOBC).EQ.'few obs') THEN
            LBC=.FALSE.
            GO TO 400
         ENDIF
      ENDIF
C
C              DETERMINE SECTION OF TABLE 2
C
      DO 228 I=1,NSOBC
      IF(YSTYPBC(KSOBC).EQ.YSNAMBC(I)) THEN
            ISON=I
            GO TO 229
      ENDIF
  228 CONTINUE
      LBC=.FALSE.
      GO TO 230
  229 CONTINUE
C
C      WRITE (*,*) ' ISON ',ISON
C
C              CALCULATE SOLAR ELEVATION
C
C     DLATBC=FLOAT(IAOF(8))/100000.0 - 90.0
C     DLONBC=FLOAT(IAOF(9))/100000.0 - 180.0
C     HH=FLOAT(IAOF(11))/100.0
C
      CALL DIURNAL ( IM, ID, IH, RLAT,RLONG, ANGLEBC )
C      WRITE (*,*) ' LAT,LON,HH,ANGLE ',RLAT,RLONG,IH,ANGLEBC
C
C              CALCULATE CORRECT COLUMN OF TABLE 2
C
      NELEVBC=NINT(ANGLEBC)
      IF(NELEVBC.LT.0) NELEVBC=NELEVBC-4
      ICOL=(NELEVBC+2)/5+3
      IF(ICOL.LT.1) ICOL=1
      IF(ICOL.GT.NCORBC) ICOL=NCORBC
C      WRITE (*,*) ' ICOL,NELEVBC,NCORBC ',ICOL,NELEVBC,NCORBC
  230 CONTINUE
C
C#########################################################
      DO 240 I=1,NST
      IF(SPP(I).EQ.RMISS) GO TO 240
      IPP=NINT(SPP(I))
      CALL PNTERP (ILEVBC,NLEVBC,IPP,POS)
C
      ILOW=IFIX(POS)
      IHGH=ILOW+1
      RADD=RCORBC(ICOL,ILOW,ISON)*(FLOAT(IHGH)-POS)+
     X          RCORBC(ICOL,IHGH,ISON)*(POS-FLOAT(ILOW))
C
      IF(SZ(I).NE.RMISS) THEN
         SZNBC(I)=SZ(I)+RADD
      ENDIF

C      WRITE (*,*) 'NST, PP,Z,ZB,POS,RCOR,RADD,IADD ',
C     X         NST,IPP,SZ(I),SZNBC(I),POS,
C     X         RCORBC(ICOL,ILOW,ISON),RADD,IADD
C
  240 CONTINUE
C#############################################################
C--------------------------------------------------------------------
C    
C                   4.  EXIT
C                  -----------
C
  400 CONTINUE
      RETURN
      END
