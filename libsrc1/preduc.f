      SUBROUTINE PREDUC (ZRED,ZSTN,PRED,PSTN,TSTN,LPRES,
     X                  IZRED,IZSTN,IPRED,IPSTN,ITSTN)
C
C**** *PREDUC*
C
C       PURPOSE.
C      ----------
C
C         CHECK ON STATION PRESSURE REDUCTION
C
C       INTERFACE.
C      ------------
C
C         CALL PREDUC (ZRED,ZSTN,PRED,PSTN,TSTN,LPRES,
C                     IZRED,IZSTN,IPRED,IPSTN,ITSTN)
C
C        INPUT
C         ZRED   -  HEIGHT REDUCED TO STANDARD PRESSURE LEVEL
C         ZSTN   -  HEIGHT OF STATION
C         PRED   -  PRESSURE REDUCED TO STANDARD HEIGHT LEVEL
C         PSTN   -  STATION LEVEL PRESSURE
C         TSTN   -  TEMPERATURE AT STATION
C        I----   -  POSITIONS OF CONFIDENCE FLAGS OF ABOVE
C        LPRES   -  LOGICAL FOR CHECKING PRESSURE OR HEIGHT REDUCTION
C
C        OUTPUT
C         IFLAG (CONF) - CONFIDENCE FLAGS UPDATED
C
C       METHOD.
C      ---------
C
C        USE OF HYDROSTATIC EQUATION AND BY ASSUMING A LINEAR
C          TEMPERATURE VARIATION WITH RESPECT TO HEIGHT
C
C       EXTERNALS.
C      ------------
C
C         NONE
C
C       REFERENCE.
C      ------------
C
C        GUIDE ON THE GLOBAL DATA PROCESSING SYSTEM WMO N305 1982
C          CHAPTER 6, QUALITY CONTROL PROCEDURES PARA 6.3.2.1
C
C       AUTHOR.
C      ---------
C
C        B. NORRIS,  ECMWF,  JANUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C        NONE
C
      INCLUDE 'const.f'
      LOGICAL LPRES
      ICONF=IFAIL
C
      ipr=0
      if(ipr.ne.0)print*,'preduc-ZRED,ZSTN,PRED,PSTN,TSTN,LPRES',
     c  ZRED,ZSTN,PRED,PSTN,TSTN,LPRES
      if(ipr.ne.0)print*,'preduc-IZRED,IZSTN,IPRED,IPSTN,ITSTN',
     c  IZRED,IZSTN,IPRED,IPSTN,ITSTN
C
      IF(LPRES) THEN
C
C    --------------------------------------------------------------------
C
C              1.   REDUCTION TO PRESSURE LEVEL
C            ------------------------------------
C
 100       CONTINUE
           if(ipr.ne.0)print*,'REDUCTION TO PRESSURE LEVEL'
           ALPP=ALOG(PSTN/PRED)
           RRG=R/RG
           RG2=RGAMMA/2.0
           ZRED1=
     X     (ZSTN+RRG*ALPP*(TSTN+RG2*ZSTN))/
     X     (1.0+RRG*ALPP*RG2)
           if(ipr.ne.0)print*,'ALPP,ZRED1',ALPP,ZRED1
           IF(ABS(PRED-PSTN).LE.2000.0) THEN
                if(ipr.ne.0)print*,'ABS(PRED-PSTN)  OK!'
                IF(ABS(ZRED-ZRED1).LE.5.0) THEN
                     ICONF=IPASS
                     if(ipr.ne.0)print*,'REDUCTION  OK!'
                     GO TO 300
                ENDIF
           ELSE
                IF(ABS(ZRED-ZRED1).LE.0.0025*ABS(PRED-PSTN)) THEN
                     ICONF=IPASS
                     if(ipr.ne.0)print*,'REDUCTION  OK!'
                     GO TO 300
                ENDIF
           ENDIF
      ELSE
C
C    ---------------------------------------------------------------------
C
C                 2.  REDUCTION TO HEIGHT LEVEL
C               ---------------------------------
c      ipr=3
C
  200      CONTINUE
           if(ipr.ge.3)print*,'REDUCTION TO HEIGHT LEVEL'
clil
c           ZSTN=307.0
clil
           TM=TSTN+(RGAMMA*(ZSTN-ZRED))/2.0
           ALPHA=RG*(ZSTN-ZRED)/(R*TM)
           PRED1=PSTN*EXP(ALPHA)
           if(ipr.ge.3)print*,'PSTN,PRED,PRED1',PSTN,PRED,PRED1
           if(ipr.ge.3)print*,'ZSTN,ALPHA,TSTN',ZSTN,ALPHA,TSTN
       
           IF(ABS(ZRED-ZSTN).LE.100.0) THEN
                if(ipr.ge.1)print*,'ABS(ZRED-ZSTN) == ',ABS(ZRED-ZSTN)
cc                IF(ABS(PRED-PRED1).LE.40.0) THEN
clil
                if(ipr.ne.0)print*,'ABS(PRED-PRED1) == ',ABS(PRED-PRED1)
                IF(ABS(PRED-PRED1).LE.40.0) THEN
clil
                     ICONF=IPASS
                     if(ipr.ge.1)print*,'REDUCTION OK'
                     GO TO 300
                ENDIF
           ELSE
cc                IF(ABS(PRED-PRED1).LE.0.4*ABS(ZRED-ZSTN)) THEN
clil
                if(ipr.ge.1)print*,'ABS(ZRED-ZSTN) == ',ABS(ZRED-ZSTN)
                if(ipr.ge.1)print*,'ABS(PRED-PRED1) == ',ABS(PRED-PRED1)
                if(ipr.ge.1)print*,'0.4*ABS(ZRED-ZSTN) == ',
     C                              0.4*ABS(ZRED-ZSTN)
                IF(ABS(PRED-PRED1).LE.0.4*ABS(ZRED-ZSTN)) THEN
clil
                     if(ipr.ge.1)print*,'REDUCTION  OK'
                     ICONF=IPASS
                     GO TO 300
                ENDIF
           ENDIF
      ENDIF
C    --------------------------------------------------------------------------
C
C                   3.  UPDATE CONFIDENCE
C                  ------------------------
C
  300 CONTINUE
      CALL SETINT (ICONF,IZRED,IZSTN,IPRED,IPSTN,ITSTN,IEND)
C
C    ----------------------------------------------------------------------
C
C                    4.   EXIT
C                  ------------
C
  400 CONTINUE
      RETURN
      END
