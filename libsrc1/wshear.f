      SUBROUTINE WSHEAR (IERR,NSTD,STDPP,STDDD,STDFF,
     X                                   ISTDDD,ISTDFF)
C
C**** *WSHEAR*
C
C       PURPOSE.
C      ----------
C
C           CHECK CONSECUTIVE STANDARD LEVELS OF UPPER AIR
C             REPORT FOR EXCESSIVE WIND SHEAR
C
C       INTERFACE.
C      ------------
C
C         CALL WSHEAR (IERR,NSTD,STDPP,STDDD,STDFF,ISTDDD,ISTDFF)
C
C        INPUT
C         NSTD   -  NUMBER OF STANDARD LEVELS
C         STDPP  -  STANDARD LEVEL PRESSURE VALUES
C         STDDD  -  CORRESPONDING STANDARD LEVEL WIND DIRECTION VALUES
C         STDFF  -  CORRESPONDING STANDARD LEVEL WIND SPEED VALUES
C         ISTDDD -  CORRESPONDING STANDARD LEVEL WIND DIR FLAG POSITIONS
C         ISTDFF -  CORRESPONDING STANDARD LEVEL WIND SPD FLAG POSITIONS
C
C        OUTPUT
C         IERR   -  ERROR CODE
C         IFLAG (CONF)  -  CONFIDENCE FLAGS UPDATED
C
C       METHOD.
C      ---------
C
C           CHECKS THE SPEED SHEAR AND DIRECTIONAL SHEAR OF WIND
C           BETWEEN 2 STANDARD LEVELS.
C           MAXIMUM SPEED SHEAR PERMITTED IS 20.6+0.275(F1+F2) M/S
C           MAXIMUM PERMITTED SUM OF SPEEDS FOR PARTICULAR DIRECTIONAL
C           SHEAR IS GIVEN BY A TABLE.
C
C       EXTERNALS.
C      -----------
C
C         SETINT  -  SET CONFIDENCE FLAGS FOR INTERNAL CONSISTENCY CHECK
C         PINTER  -  INTERPOLATE TO PRESSURE OR HEIGHT LEVEL
C
C       REFERENCE.
C      ------------
C
C         NONE
C
C       AUTHOR.
C      ---------
C
C         B. NORRIS,  ECMWF,  FEBRUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
      INCLUDE 'paramq.f'
      PARAMETER (NSCOL=7)
      INCLUDE 'ctua.f'
      INCLUDE 'const.f'
      DIMENSION STDPP(*),STDDD(*),STDFF(*)
      DIMENSION ISTDDD(*),ISTDFF(*)
      DIMENSION IPOS(NLEV),USH(NSCOL,NLEV)
      LOGICAL LDIR,LSPD
C
C    -------------------------------------------------------------------
C
C                       1.  SETUP
C                     ------------
C
  100 CONTINUE
C
C           1.1  LIMIT TABLE FOR SUM OF SPEEDS FOR CERTAIN
C                      DIRECTIONAL SHEAR
C
      DATA ((USH(I,J),I=1,NSCOL),J=1,10)/
C                   DIRECTIONAL SHEAR (DEGREES)
C         >30      >40      >50      >60      >70      >80      >90
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X  110.0,    84.0,    77.0,    70.0,    63.0,    52.0,    50.0,
     X  110.0,    84.0,    77.0,    70.0,    63.0,    52.0,    50.0,
     X  110.0,    84.0,    77.0,    70.0,    63.0,    52.0,    50.0,
     X  110.0,    84.0,    77.0,    70.0,    63.0,    52.0,    50.0,
     X  110.0,    84.0,    77.0,    70.0,    63.0,    52.0,    50.0,
     X  110.0,    84.0,    77.0,    70.0,    63.0,    52.0,    50.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0/
      DATA ((USH(I,J),I=1,NSCOL),J=11,NLEV)/
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0,
     X   72.0,    61.0,    57.0,    53.0,    49.0,    46.0,    41.0/
C
C        1.2  OBTAIN POSITION OF EACH LEVEL WITHIN TABLE OF
C                       STANDARD LEVELS
C
      DO 122 ILEV=1,NSTD
      CALL PINTER (IERR,UPZ(1,1),STDPP(ILEV),POS)
      IF(IERR.NE.0) THEN
           IPOS(ILEV)=1000
      ELSE
           IPOS(ILEV)=NINT(POS)
      ENDIF
  122 CONTINUE
C
C    -------------------------------------------------------------------
C
C           2.  CALCULATE LIMITS FOR SHEAR
C         ----------------------------------
C
  200 CONTINUE
      IERR=0
C
C         2.1  LOOP FOR STANDARD LEVELS
C
      DO 245 ILEV=1,NSTD-1
      LDIR=.FALSE.
      LSPD=.FALSE.
      IF(IABS(IPOS(ILEV)-IPOS(ILEV+1)).LE.1) THEN
C
C         2.2  SPEED SHEAR
C
           SUMWIN=STDFF(ILEV)+STDFF(ILEV+1)
           SHEWIN=ABS(STDFF(ILEV)-STDFF(ILEV+1))
           SHEMAX=20.6+0.275*SUMWIN
           IF(SHEWIN.GT.SHEMAX) LSPD=.TRUE.
C
C         2.3  DIRECTIONAL SHEAR
C
           IF(STDDD(ILEV).EQ.0.0.OR.STDDD(ILEV+1).EQ.0.0) GO TO 235
           DIRSHE=ABS(STDDD(ILEV)-STDDD(ILEV+1))
           IF(DIRSHE.GT.180.0) DIRSHE=360.0-DIRSHE
           IND=(DIRSHE-20.0)/10.0
           IF(IND.LT.1) GO TO 235
           IF(IND.GT.NSCOL) IND=NSCOL
           SUMPER=USH(IND,IPOS(ILEV))
           IF(SUMWIN.GT.SUMPER) LDIR=.TRUE.
  235      CONTINUE
C
C         2.4  UPDATE CONFIDENCE FLAGS
C
           IF(LDIR.OR.LSPD) CALL SETINT
     X           (IFAIL,ISTDDD(ILEV),ISTDFF(ILEV),
     X                 ISTDDD(ILEV+1),ISTDFF(ILEV+1),IEND)
      ENDIF
  245 CONTINUE
C
C    --------------------------------------------------------------------
C
C                   3.   EXIT
C                  -----------
C
  300 CONTINUE
      RETURN
      END
