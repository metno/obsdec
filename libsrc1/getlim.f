      SUBROUTINE GETLIM (RLAT,RLONG,TABLE,VLIM)
C
C**** *GETLIM*
C
C       PURPOSE.
C      ---------
C
C          GET LIMITS FOR SURFACE PARAMETER
C
C       INTERFACE.
C      -------------
C
C         CALL GETLIM (RLAT,RLONG,TABLE,VLIM)
C
C        INPUT
C         RLAT   -  LATITUDE VALUE FOR REPORT
C         RLONG  -  LONGITUDE VALUE FOR REPORT
C         TABLE  -  GROSS LIMIT TABLE FOR SURFACE PARAMETER
C
C        OUTPUT
C         VLIM   -  APPROPRIATE GROSS LIMITS
C
C       METHOD.
C      ---------
C
C         EXTRACT CORRECT SET OF LIMITS FROM TABLE
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
C           CHAPTER 6,  QUALITY CONTROL PROCEDURES
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
      DIMENSION VLIM(NMM),TABLE(NMM,NSEASN,NAREA)
C
C    ----------------------------------------------------------------------
C
C              1.  DETERMINE WHICH AREA REPORT FALLS INTO
C             --------------------------------------------
C
  100 CONTINUE
      IAREA=1
      IF(ABS(RLAT).GT.RLLIM) IAREA=2
C
C    ----------------------------------------------------------------------
C
C              2.  EXTRACT RELEVANT LIMITS FROM TABLE
C            ------------------------------------------
C
  200 CONTINUE
      DO 202 IMM=1,NMM
      VLIM(IMM)=TABLE(IMM,ISEASN,IAREA)
  202 CONTINUE
C
C    --------------------------------------------------------------------
C
C               3.  EXIT
C             ------------
C
  300 CONTINUE
      RETURN
      END
