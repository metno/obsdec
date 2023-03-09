      SUBROUTINE GUALIM (IERR,UPZ,TABLE,RLAT,RLONG,PP1,PP2,Z,
     X     LAYER,LSUM,VLIM)
C
C**** *GUALIM*
C
C       PURPOSE.
C      ---------
C
C             GET PARAMETER LIMITS FROM UPPER AIR TABLE
C
C       INTERFACE.
C      -----------
C
C          CALL GUALIM (IERR,UPZ,TABLE,RLAT,RLONG,PP1,PP2,Z,
C                            LAYER,LSUM,VLIM)
C
C        INPUT
C         UPZ    -  TABLE OF STANDARD PRESSURE AND HEIGHT LEVELS
C         TABLE  -  GROSS LIMITS FOR PARAMETER
C         RLAT   -  LATITUDE VALUE FOR REPORT
C         RLONG  -  LONGITUDE VALUE FOR REPORT
C         PP1    -  PRESSURE LEVEL OR BASE OF LAYER
C         PP2    -  TOP OF LAYER
C         Z      -  HEIGHT LEVEL
C         LAYER  -  LOGICAL FOR PRESENCE OF LAYER
C         LSUM   -  LOGICAL FOR SUMMING OR MEANING
C
C        OUTPUT
C         VLIM   -  GROSS LIMITS FOR PARAMETER
C
C               POSSIBILITIES:
C
C           PP1     PP2     LAYER   Z    LSUM
C (1)        X
C (2)                               X
C (3)        X       X        X
C (4)        X       X        X           X
C
C       (1) = INDIVIDUAL PRESSURE LEVEL
C       (2) = INDIVIDUAL HEIGHT LEVEL
C       (3) = LAYER (MEAN TABLE VALUES OVER LAYER)
C       (4) = LAYER (SUM  TABLE VALUES OVER LAYER)
C
C       METHOD.
C      ---------
C
C         IF NOT LAYER, INTERPOLATE TO INDIVIDUAL LEVEL
C         IF LAYER, INTERPOLATE TO BASE AND TOP LEVELS,
C             SUM OR MEAN OVER LAYER AS REQUIRED
C
C       EXTERNALS.
C      ------------
C
C         PINTER  -  INTERPOLATE TO PRESSURE OR HEIGHT LEVEL
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
      INCLUDE 'const.f'
C
      DIMENSION UPZ(NLEV,2)
      DIMENSION TABLE(NMM,NAREA,NLEV)
      DIMENSION VLIM(NMM)
      LOGICAL LSUM,LAYER,LSTD
      DATA EPS/0.00001/
C
C    --------------------------------------------------------------------
C
C                         1.  SETUP
C                        -----------
C
  100 CONTINUE
      LSTD=.FALSE.
      IERR=0
C
C           1.1  DETERMINE WHICH AREA REPORT FALLS INTO
C
      IAREA=1
      IF(ABS(RLAT).GT.RLLIM) IAREA=2
C
      IF(LAYER) GO TO 400
C
C    ---------------------------------------------------------------------
C
C              2.  INDIVIDUAL LEVEL
C             ----------------------
C
  200 CONTINUE
C
C           2.1  HEIGHT LEVEL
C
C    (2):
C
      IF(Z.NE.RMISS)
     X         CALL PINTER (IERR,UPZ(1,2),Z,POS)
C
C           2.2  PRESSURE LEVEL
C
C    (1):
C
      IF(PP1.NE.RMISS)
     X         CALL PINTER (IERR,UPZ(1,1),PP1,POS)
C
      IF(IERR.NE.0) GO TO 500
C
C             2.3  OBTAIN GROSS LIMITS
C
      ILOW=IFIX(POS)
      IHGH=ILOW+1
      DO 235 IMM=1,NMM
      VLIM(IMM)=
     X   TABLE(IMM,IAREA,ILOW)*(FLOAT(IHGH)-POS)+
     X   TABLE(IMM,IAREA,IHGH)*(POS-FLOAT(ILOW))
  235 CONTINUE
      GO TO 500
C
C    -------------------------------------------------------------------
C
C                      4.  LAYER
C                     -----------
C
C     (3) & (4):
C
  400 CONTINUE
      IF(PP1.EQ.RMISS.OR.PP2.EQ.RMISS) THEN
           IERR=2301
           GO TO 500
      ENDIF
C
C          4.1  INTERPOLATE TO TOP AND BASE OF LAYER
C
      CALL PINTER (IERR,UPZ(1,1),PP1,POSP1)
      IF(IERR.NE.0) GO TO 500
      CALL PINTER (IERR,UPZ(1,1),PP2,POSP2)
      IF(IERR.NE.0) GO TO 500
      ILOW1=IFIX(POSP1)
      IHGH1=ILOW1+1
      ILOW2=IFIX(POSP2)
      IHGH2=ILOW2+1
      PDIFF=POSP2-POSP1
      IF(ABS(PDIFF).LT.EPS) PDIFF=SIGN(EPS,PDIFF)
C
C          4.2  CHECK WHETHER LAYER IS BOUNDED BY STANDARD LEVELS
C
      IF(ABS(FLOAT(ILOW1)-POSP1).LT.EPS.AND.
     X   ABS(FLOAT(ILOW2)-POSP2).LT.EPS) LSTD=.TRUE.
C
      DO 448 IMM=1,NMM
      SUM=0.0
      IF(LSUM) THEN
C
C          4.3   SUMMATION REQUIRED
C
           IF(LSTD.AND.ILOW2.GT.ILOW1) THEN
C
C          4.31  STANDARD LAYERS ONLY
C
                DO 431 J=ILOW1,ILOW2-1
                SUM=SUM+TABLE(IMM,IAREA,J)
  431           CONTINUE
                GO TO 435
           ENDIF
           IF(ILOW2.GT.IHGH1) THEN
C
C           4.32  ADD WHOLE STANDARD LAYERS
C
                DO 432 J=IHGH1,ILOW2-1
                SUM=SUM+TABLE(IMM,IAREA,J)
  432           CONTINUE
           ENDIF
           IF(ILOW2.LT.IHGH1) THEN
C
C           4.33  LAYER IS LESS THAN 1 STANDARD LAYER
C
                SUM=SUM+TABLE(IMM,IAREA,ILOW1)*PDIFF
           ELSE
C
C           4.34  ADD PARTIAL LAYERS AT TOP AND BOTTOM
C
                SUM=SUM+TABLE(IMM,IAREA,ILOW1)*(FLOAT(IHGH1)-POSP1)+
     X                  TABLE(IMM,IAREA,ILOW2)*(POSP2-FLOAT(ILOW2))
           ENDIF
  435      CONTINUE
           VLIM(IMM)=SUM
      ELSE
C
C           4.4  MEAN OVER LAYER REQUIRED
C
           IF(LSTD.AND.ILOW2.GT.ILOW1) THEN
C
C            4.41  STANDARD LAYERS ONLY
C
                DO 441 J=ILOW1,ILOW2-1
                SUM=SUM+(TABLE(IMM,IAREA,J)+TABLE(IMM,IAREA,J+1))/2.0
  441           CONTINUE
                GO TO 445
           ENDIF
           IF(ILOW2.GT.IHGH1) THEN
C
C            4.42  ADD WHOLE STANDARD LAYERS
C
                DO 442 J=IHGH1,ILOW2-1
                SUM=SUM+(TABLE(IMM,IAREA,J)+TABLE(IMM,IAREA,J+1))/2.0
  442           CONTINUE
           ENDIF
           PLOW=TABLE(IMM,IAREA,ILOW1)*(FLOAT(IHGH1)-POSP1)+
     X          TABLE(IMM,IAREA,IHGH1)*(POSP1-FLOAT(ILOW1))
           PHIGH=TABLE(IMM,IAREA,ILOW2)*(FLOAT(IHGH2)-POSP2)+
     X          TABLE(IMM,IAREA,IHGH2)*(POSP2-FLOAT(ILOW2))
           IF(ILOW2.LT.IHGH1) THEN
C
C           4.43  LAYER IS LESS THAN 1 STANDARD LAYER
C
                SUM=SUM+((PLOW+PHIGH)/2.0)*PDIFF
           ELSE
C
C           4.44  ADD PARTIAL LAYERS AT TOP AND BOTTOM
C
                SUM=SUM+((PLOW+TABLE(IMM,IAREA,IHGH1))/2.0)*
     X                       (FLOAT(IHGH1)-POSP1)+
     X                 ((TABLE(IMM,IAREA,ILOW2)+PHIGH)/2.0)*
     X                       (POSP2-FLOAT(ILOW2))
           ENDIF
  445      CONTINUE
           VLIM(IMM)=SUM/PDIFF
      ENDIF
  448 CONTINUE
C
C    --------------------------------------------------------------------
C
C                     5.   EXIT
C                   ------------
C
  500 CONTINUE
      RETURN
      END
