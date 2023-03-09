      SUBROUTINE SHIPPK (IREC)
C
C****  *SHIPPK*
C
C       PURPOSE.
C      ----------
C
C         PACK TIME SERIES RECORD
C
C       INTERFACE.
C      ------------
C
C           CALL SHIPPK (IREC)
C
C        INPUT
C          UNPACKED RECORD IN COMMON COMSHIP
C
C        OUTPUT
C          IKEY -  FULL KEY OF RECORD
C          IREC -  PACKED RECORD
C
C       METHOD.
C      ---------
C
C         PACK TYPE, SUBTYPE, TIME, PARAMETER, CONFIDENCE AND
C         SUBSTITUTIONS INTO RECORD, APPLYING REFERENCE VALUES
C         AND SCALING FACTORS WHERE APPROPRIATE;
C         PACK HOUSEKEEPING ITEMS INTO KEY.
C
C       EXTERNALS.
C      ------------
C
C         PACK  -  WRITE BIT FIELDS INTO ARRAY
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
C    ---------------------------------------------------------------------
C
C                     1.  PACK RECORD
C                   -------------------
C
  100 CONTINUE
      IW=1
      IBT=0
C
C           1.1  LOOP FOR NUMBER OF ENTRIES
C
      DO 134 JENT=1,NSENT
C
C           1.2  LOOP FOR NUMBER OF TYPES AND SUBTYPES
C
      DO 122 JTYP=1,NTSUBT
      KSTYPE=NSTYPE(JTYP,JENT)
      IF(KSTYPE.EQ.IMISS) KSTYPE=2**8-1
      CALL PACK (NSBPW,IREC(IW),KSTYPE,IW,IBT,8,IRET)
      KSSTYP=NSSTYP(JTYP,JENT)
      IF(KSSTYP.EQ.IMISS) KSSTYP=2**8-1
      CALL PACK (NSBPW,IREC(IW),KSSTYP,IW,IBT,8,IRET)
  122 CONTINUE
      CALL PACK (NSBPW,IREC(IW),MINSIN(JENT),IW,IBT,32,IRET)
C
C            1.3  LOOP FOR NUMBER OF PARAMETERS
C
      DO 132 JPAR=1,NSPAR
      IF(SPARAM(JPAR,JENT).EQ.RMISS) THEN
           KPARAM=2**28-1
      ELSE
           KPARAM=NINT(SPARAM(JPAR,JENT)*10.**JSCAL(JPAR))-JREF(JPAR)
      ENDIF
      CALL PACK (NSBPW,IREC(IW),KPARAM,IW,IBT,28,IRET)
      KSCONF=ISCONF(JPAR,JENT)
      IF(KSCONF.EQ.IMISS) KSCONF=2**8-1
      CALL PACK (NSBPW,IREC(IW),KSCONF,IW,IBT,8,IRET)
      IF(SSUBST(JPAR,JENT).EQ.RMISS) THEN
           KPARAM=2**28-1
      ELSE
           KPARAM=NINT(SSUBST(JPAR,JENT)*10.**JSCAL(JPAR))-JREF(JPAR)
      ENDIF
      CALL PACK (NSBPW,IREC(IW),KPARAM,IW,IBT,28,IRET)
  132 CONTINUE
  134 CONTINUE
C
C    ---------------------------------------------------------------------
C
C                     2.  PACK KEY
C                   -----------------
C
  200 CONTINUE
      NSLEN=(IW-1)*4+IBT/8
C
C    -----------------------------------------------------------------------
C
C                        3.  EXIT
C                       ----------
C
  300 CONTINUE
      RETURN
      END
