      SUBROUTINE SHIPUP (IREC)
C
C****  *SHIPUP*
C
C       PURPOSE.
C      ----------
C
C         UNPACK TIME SERIES RECORD
C
C       INTERFACE.
C      ------------
C
C           CALL SHIPUP (IREC)
C
C        INPUT
C          IREC -  PACKED RECORD
C
C        OUTPUT
C          UNPACKED RECORD IN COMMON COMSHIP
C
C       METHOD.
C      ---------
C
C         UNPACK HOUSEKEEPING ITEMS FROM KEY;
C         UNPACK TYPE, SUBTYPE, TIME, PARAMETER, CONFIDENCE AND
C         SUBSTITUTIONS FROM RECORD, APPLYING REFERENCE VALUES
C         AND SCALING FACTORS WHERE APPROPRIATE
C
C       EXTERNALS.
C      ------------
C
C         UNPACK  -  EXTRACT BIT FIELDS FROM ARRAY
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
      INCLUDE 'compref.f'
      INCLUDE 'const.f'
      DIMENSION IREC(*)
C
C    -------------------------------------------------------------------
C
C                     1.  UNPACK KEY
C                    ----------------
C
  100 CONTINUE
C    ---------------------------------------------------------------------
C
C                     2.  UNPACK RECORD
C                   ---------------------
C
  200 CONTINUE
      IW=1
      IBT=0
C
C            2.1  LOOP FOR NUMBER OF ENTRIES
C
      DO 234 JENT=1,NSENT
C
C            2.2  LOOP FOR NUMBER OF TYPES AND SUBTYPES
C
      DO 222 JTYP=1,NTSUBT
      CALL UNPACK (NSBPW,IREC(IW),NSTYPE(JTYP,JENT),IW,IBT,8,IRET)
      IF(NSTYPE(JTYP,JENT).EQ.2**8-1) NSTYPE(JTYP,JENT)=IMISS
      CALL UNPACK (NSBPW,IREC(IW),NSSTYP(JTYP,JENT),IW,IBT,8,IRET)
      IF(NSSTYP(JTYP,JENT).EQ.2**8-1) NSSTYP(JTYP,JENT)=IMISS
  222 CONTINUE
      CALL UNPACK (NSBPW,IREC(IW),MINSIN(JENT),IW,IBT,32,IRET)
C
C           2.3  LOOP FOR NUMBER OF PARAMETERS
C
      DO 232 JPAR=1,NSPAR
      CALL UNPACK (NSBPW,IREC(IW),KPARAM,IW,IBT,28,IRET)
      IF(KPARAM.EQ.2**28-1) THEN
           SPARAM(JPAR,JENT)=RMISS
      ELSE
           SPARAM(JPAR,JENT)=FLOAT(KPARAM+JREF(JPAR))/10.**JSCAL(JPAR)
      ENDIF
      CALL UNPACK (NSBPW,IREC(IW),ISCONF(JPAR,JENT),IW,IBT,8,IRET)
      IF(ISCONF(JPAR,JENT).EQ.2**8-1) ISCONF(JPAR,JENT)=IMISS
      CALL UNPACK (NSBPW,IREC(IW),KPARAM,IW,IBT,28,IRET)
      IF(KPARAM.EQ.2**28-1) THEN
           SSUBST(JPAR,JENT)=RMISS
      ELSE
           SSUBST(JPAR,JENT)=FLOAT(KPARAM+JREF(JPAR))/10.**JSCAL(JPAR)
      ENDIF
  232 CONTINUE
  234 CONTINUE
C
C    ---------------------------------------------------------------------
C
C                         3.  EXIT
C                        -----------
C
  300 CONTINUE
      RETURN
      END
