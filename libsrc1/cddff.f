      SUBROUTINE CDDFF (DD,FF,IDD,IFF)
C
C**** *CDDFF*
C
C      PURPOSE.
C     ---------
C
C           CHECK CONSISTENCY OF WIND SPEED AND DIRECTION
C
C      INTERFACE.
C     ------------
C
C           CALL CDDFF (DD,FF,IDD,IFF)
C
C       INPUT
C         DD    -  VALUE OF WIND DIRECTION
C         FF    -  VALUE OF WIND SPEED
C         IDD   -  POSITION OF CONFIDENCE FLAG FOR DD
C         IFF   -  POSITION OF CONFIDENCE FLAG FOR FF
C
C       OUTPUT
C         IFLAG (CONF) - CONFIDENCE FLAGS UPDATED
C
C      METHOD.
C     ---------
C
C          CHECK WHETHER ONE COMPONENT IS MISSING AND THE OTHER NOT
C          CHECK WHETHER ONE COMPONENT IS ZERO AND THE OTHER NOT
C          CHECK WHETHER DD HAS PERMITTED VALUE
C          IF DD IS VARIABLE, CHECK FF IS WITHIN LIMITS
C
C      EXTERNALS.
C     ------------
C
C         SETGRS  -  UPDATE CONFIDENCE FOR GROSS LIMIT CHECK
C         SETINT  -  UPDATE CONFIDENCE FOR INTERNAL CONSISTENCY CHECK
C
C      REFERENCE.
C     ------------
C
C         GUIDE ON THE GLOBAL DATA PROCESSING SYSTEM WMO N305 1982
C              CHAPTER 6 - QUALITY CONTROL PROCEDURES
C
C      AUTHOR.
C     ---------
C
C         B. NORRIS,  ECMWF,  JANUARY 1989.
C
C      MODIFICATIONS.
C     ----------------
C
C         B. NORRIS, 15/06/89,  LIGHT AND VARIABLE ASSUMED TO BE
C                               DD=0,FF>0
cpsjun99  This is contrary to code table 0877 which uses 99 for dd
cps       in this case. But dd is fetched from values array in BUFR,
cps       and in BUFR 0 is used for variable wind direction.
cps       However, it is now too late to detect if dd=0,ff>0 actually
cps       is a "typo" for dd=36,ff>0. This check must be done in 
cps       nix_... program, and preferrably change 0 to 360 there.
cps       For temp and pilot the direction can never be 'variable'. Here
cps       dd is assured to be between 0.0 and 360.0
C
C
      INCLUDE 'const.f'
C
cpsjun99 Feeling uneasy about comparing real vaues for equality, as
cps      is done in original cddff.f from ECMW
cps      Has therefore chosen to replace dd, ff, rmiss and rvar by integer
cps      values ndd, nff, nrmiss and nrvar before starting comparing
      nDD = nint(DD)
      nFF = nint(FF)
      nRMISS = nint(RMISS)
      nRVAR = nint(RVAR)
cps
C    --------------------------------------------------------------------
C
C                1.  CHECKING OF DD AND FF
C               ---------------------------
C
  100 CONTINUE
      IF(nDD.EQ.nRMISS.AND.nFF.EQ.nRMISS) GO TO 200
C
C         1.1  ONE OF DD OR FF MISSING
C
      IF(nDD.EQ.nRMISS) CALL SETINT (IFAIL,IFF,IEND)
      IF(nFF.EQ.nRMISS) CALL SETINT (IFAIL,IDD,IEND)
C
      IF(nDD.NE.nRMISS) THEN
C
C         1.2  CHECK DD WITHIN PERMITTED VALUES
C
           IF((nDD.GE.0.AND.nDD.LE.360).OR.nDD.EQ.nRVAR) GO TO 122
           CALL SETGRS (IDD,1)
  122      CONTINUE
C
           IF(nFF.NE.nRMISS) THEN
C
C         1.3  CHECK WHETHER DD IS NON-ZERO AND FF IS ZERO
C
                IF(nDD.NE.0.AND.nFF.EQ.0)
     X            CALL SETINT (IFAIL,IDD,IFF,IEND)
C
C         1.4  VARIABLE DIRECTION, LIMIT VALUE FOR FF
C
                IF(nDD.EQ.nRVAR) THEN
cps      Remark: FF is measured in bufr units m/s here
                     IF(nFF.GT.3 .AND. nFF.LE.6) THEN
                           CALL SETINT (IFAIL,IDD,IEND)
                           CALL SETGRS (IFF,3)
                     ELSEIF(nFF.GT.6) THEN
                           CALL SETINT (IFAIL,IDD,IEND)
                           CALL SETGRS (IFF,2)
                     ELSE
                           CALL SETINT (IPASS,IDD,IFF,IEND)
                     ENDIF
                ENDIF
           ENDIF
      ENDIF
C
C    -------------------------------------------------------------------
C
C                     2.   EXIT
C                    -------------
C
  200 CONTINUE
      RETURN
      END
