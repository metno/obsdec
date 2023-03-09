      SUBROUTINE SHIPAD (JTYPE,JSBTYPE,JMINSIN,RLAT,RLON)
C
C****  *SHIPAD*
C
C       PURPOSE.
C      ----------
C
C         ADD NEW REPORT TO TIME SERIES RECORD
C
C       INTERFACE.
C      ------------
C
C           CALL SHIPAD (JTYPE,JSBTYPE,JMINSIN,RLAT,RLON)
C
C        INPUT
C          JTYPE    -  BUFR TYPE FOR NEW REPORT
C          JSBTYPE  -  BUFR SUBTYPE FOR NEW REPORT
C          JMINSIN  -  MINUTE OF NEW REPORT
C          RLAT,RLON - LATITUDE AND LONGITUDE OF NEW REPORT
C          TIME SERIES RECORD IN COMMON COMSHIP
C
C        OUTPUT
C          TIME SERIES RECORD UPDATED SUCH THAT REPORTS ARE ORDERED
C          CHRONOLOGICALLY
C
C       METHOD.
C      ---------
C
C         IF LAT, LONG AND MINUTE MATCH A PREVIOUS REPORT, THEN ADD
C         TYPE AND SUBTYPE;
C         IF NUMBER OF ENTRIES IS LESS THAN MAXIMUM, ADD NEW REPORT
C         IN CORRECT POSITION DEPENDING ON TIME;
C         IF NUMBER OF ENTRIES IS ALREADY AT MAXIMUM, ADD NEW REPORT
C         IN CORRECT POSITION AND EXCLUDE OLDEST ENTRY.
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
C         BASED ON CODE WRITTEN FOR NOS/BE IN 1982
C
C       MODIFICATIONS.
C      ----------------
C
C         NONE
C
      INCLUDE 'paramsh.f'
      INCLUDE 'comship.f'
      INCLUDE 'const.f'
C
C    ----------------------------------------------------------------------
C
C                        1.  INITIALISATION
C                      ---------------------
C
  100 CONTINUE
C
C           1.1  NCREP = ENTRY NUMBER OF CURRENT REPORT
C
      NCREP=0
      EPS=0.001
C
C    ----------------------------------------------------------------------
C
C               2.  LOOK FOR MATCH IN POSITION AND TIME
C             ------------------------------------------
C
  200 CONTINUE
      IF(NSENT.EQ.0) GO TO 324
C
C         2.1  LOOP FOR EXISTING ENTRIES
C
      DO 224 JENT=1,NSENT
      IF(JMINSIN.EQ.MINSIN(JENT).AND.
     X   ABS(RLAT-SPARAM(1,JENT)).LT.EPS.AND.
     X   ABS(RLON-SPARAM(2,JENT)).LT.EPS) THEN
C
C         2.2  MATCH FOUND, ADD TYPE AND SUBTYPE
C
           NCREP=JENT
           DO 222 JSUB=1,NTSUBT
           IF(JTYPE.EQ.NSTYPE(JSUB,JENT).AND.
     X        JSBTYPE.EQ.NSSTYP(JSUB,JENT)) GO TO 600
           IF(NSTYPE(JSUB,JENT).EQ.IMISS) THEN
                NSTYPE(JSUB,JENT)=JTYPE
                NSSTYP(JSUB,JENT)=JSBTYPE
                GO TO 600
           ENDIF
  222      CONTINUE
      ENDIF
  224 CONTINUE
C
C    ----------------------------------------------------------------------
C
C              3.  FIND POSITION FOR NEW ENTRY
C             ---------------------------------
C
  300 CONTINUE
      NSE=NSENT
      DO 322 JENT=1,NSE
      IF(JMINSIN.LE.MINSIN(JENT)) THEN
           IF(NSENT.LT.NENTMAX) THEN
C
C        3.1  ENTRIES < MAXIMUM, TIME < LATEST TIME
C
                IFF=NSENT+1
                IFL=JENT+1
                K1=-1
                IF(IFL.GT.IFF) K1=0
                NCREP=JENT
                NSENT=NSENT+1
           ELSE
C
C        3.2  ENTRIES = MAXIMUM, TIME < LATEST TIME
C
                IFF=1
                IFL=JENT-2
                K1=1
                IF(IFL.LT.IFF) K1=0
                NCREP=JENT-1
           ENDIF
           GO TO 345
      ENDIF
  322 CONTINUE
  324 CONTINUE
C
      IF(NSENT.LT.NENTMAX) THEN
C
C        3.3  ENTRIES < MAXIMUM, TIME > LATEST TIME
C
           K1=0
           NSENT=NSENT+1
           NCREP=NSENT
      ELSE
C
C        3.4  ENTRIES = MAXIMUM, TIME > LATEST TIME
C
           IFF=1
           IFL=NENTMAX-1
           K1=1
           IF(IFL.LT.IFF) K1=0
           NCREP=NENTMAX
      ENDIF
  345 CONTINUE
C
C    ----------------------------------------------------------------------
C
C                4.  MOVE OLD ENTRIES
C               ----------------------
C
  400 CONTINUE
      IF(K1.NE.0) THEN
           DO 406 I=IFF,IFL,K1
           DO 402 JTYP=1,NTSUBT
           NSTYPE(JTYP,I)=NSTYPE(JTYP,I+K1)
           NSSTYP(JTYP,I)=NSSTYP(JTYP,I+K1)
  402      CONTINUE
           MINSIN(I)=MINSIN(I+K1)
           DO 404 JPAR=1,NSPAR
           SPARAM(JPAR,I)=SPARAM(JPAR,I+K1)
           ISCONF(JPAR,I)=ISCONF(JPAR,I+K1)
           SSUBST(JPAR,I)=SSUBST(JPAR,I+K1)
  404      CONTINUE
  406      CONTINUE
      ENDIF
C
C    --------------------------------------------------------------------------
C
C                   5.  ADD NEW ENTRY
C                 --------------------
C
  500 CONTINUE
      IF(NCREP.GT.0) THEN
           NSTYPE(1,NCREP)=JTYPE
           NSSTYP(1,NCREP)=JSBTYPE
           DO 502 JTYP=2,NTSUBT
           NSTYPE(JTYP,NCREP)=IMISS
           NSSTYP(JTYP,NCREP)=IMISS
  502      CONTINUE
           MINSIN(NCREP)=JMINSIN
           SPARAM(1,NCREP)=RLAT
           ISCONF(1,NCREP)=IMISS
           SSUBST(1,NCREP)=RMISS
           SPARAM(2,NCREP)=RLON
           ISCONF(2,NCREP)=IMISS
           SSUBST(2,NCREP)=RMISS
      ENDIF
C
C    -----------------------------------------------------------------------
C
C                          6.  EXIT
C                        -----------
C
  600 CONTINUE
      MINNT=MINSIN(NSENT)
      RETURN
      END
