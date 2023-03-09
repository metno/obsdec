      SUBROUTINE LANDSEA (RLAT,RLON,ISUM)
C
C****  *LANDSEA*
C
C       PURPOSE.
C      ----------
C
C         CHECK WHETHER POSITION IS OVER LAND
C
C       INTERFACE.
C      ------------
C
C           CALL LANDSEA (RLAT,RLON,ISUM)
C
C        INPUT
C           RLAT,RLON  -  POSITION
C           MASK (COMMON COMLSM)
C                      -  LAND/SEA MASK RECORD
C                         (97 LATITUDE ROWS * 192 LONGITUDE COLUMNS,
C                         EACH ROW PACKED INTO 6 WORDS)
C
C        OUTPUT
C          ISUM  -  NUMBER OF SURROUNDING LAND POINTS
C
C       METHOD.
C      ---------
C
C         CALCULATE POSITIONS OF 4 SURROUNDING GRID POINTS OF MASK;
C         SUM THE NUMBER OF LAND POINTS.
C
C       EXTERNALS.
C      ------------
C
C         GBYTE  -  EXTRACT BIT FIELD FROM ARRAY
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
C         BASED ON CODE WRITTEN FOR NOS/BE IN 1982
C
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
      INCLUDE 'paramsh.f'
      INCLUDE 'comlsm.f'
      DIMENSION IPTS(2,4)
C
C    ----------------------------------------------------------------------
C
C                       1.  INITIALISATION
C                      ---------------------
C
  100 CONTINUE
C
C          1.1  GRID = GRID OF MASK FIELD;
C                      MASK FIELD HAS DIMENSION IDIM1 * IDIM2
C
      DATA GRID/1.875/,IDIM1/97/,IDIM2/192/
      ISUM=0
C
C          1.2  CHECK FOR REASONABLE LATITUDE AND LONGITUDE
C
      IF(ABS(RLAT).GT.90.0.OR.ABS(RLON).GT.180.0) THEN
           ISUM=4
           GO TO 400
      ENDIF
C
C    ----------------------------------------------------------------------
C
C                  2.  FIND 4 SURROUNDING GRID POINTS
C                --------------------------------------
C
  200 CONTINUE
C
C           2.1  SCALE THE POSITION TO BE CONSISTENT WITH MASK ARRAY
C
      SCLAT=90.0-RLAT
      SCLON=RLON
      IF(SCLON.LT.0.0) SCLON=SCLON+360.0
C
C         2.2  IPTS CONTAINS POSITIONS WITHIN MASK ARRAY
C              OF 4 SURROUNDING POINTS
C              POINT 1 = NW
C              POINT 2 = NE
C              POINT 3 = SW
C              POINT 4 = SE
C
      IPTS(1,1)=IFIX(SCLAT/GRID)+1
      IPTS(2,1)=IFIX(SCLON/GRID)+1
      IF(IPTS(1,1).EQ.IDIM1)   IPTS(1,1)=IDIM1-1
      IF(IPTS(2,1).EQ.IDIM2+1) IPTS(2,1)=IDIM2
      NLONG=IPTS(2,1)+1
      IF(NLONG.EQ.IDIM2+1) NLONG=1
      IPTS(1,2)=IPTS(1,1)
      IPTS(2,2)=NLONG
      IPTS(1,3)=IPTS(1,1)+1
      IPTS(2,3)=IPTS(2,1)
      IPTS(1,4)=IPTS(1,3)
      IPTS(2,4)=NLONG
C
C    ------------------------------------------------------------------------
C
C                 3.  SUM THE LAND POINTS
C                -------------------------
C
  300 CONTINUE
      ISUM=0
      DO 312 I=1,4
C
C           3.1  UNPACK APPROPRIATE WORDS OF MASK
C
      IWRD=(IPTS(2,I)-1)/NSBPW+1
      IPOS=MOD(IPTS(2,I)-1,NSBPW)
      CALL GBYTE (MASK(IWRD,IPTS(1,I)),ISMASK,IPOS,1)
C     WRITE(*,'(1H ,3I4,1X,O11)') IWRD,IPOS,ISMASK,MASK(IWRD,IPTS(1,I))
      ISUM=ISUM+ISMASK
  312 CONTINUE
C
C    -----------------------------------------------------------------------
C
C                         4.  EXIT
C                       ------------
C
  400 CONTINUE
C     WRITE (*,'(1H ,2F10.2,9I4)') RLAT,RLON,IPTS,ISUM
      RETURN
      END
