C
C      COMMON *COMSHIP* - ITEMS IN TIME SERIES RECORD
C
C         NCREP  -  POINTER TO CURRENT REPORT
C         NSLEN  -  LENGTH OF PACKED RECORD
C         NSENT  -  NUMBER OF REPORTS IN RECORD
C         NSPAR  -  NUMBER OF PARAMETERS STORED FOR REPORT
C         MINNT  -  TIME OF MOST RECENT REPORT
C         NSTYPE -  BUFR REPORT TYPES FOR THIS REPORT
C         NSSTYP -  BUFR REPORT SUBTYPES FOR THIS REPORT
C         MINSIN -  TIMES OF REPORTS
C         SPARAM -  PARAMETER VALUES
C         ISCONF -  CONFIDENCE FOR PARAMETER VALUES
C         SSUBST -  SUBSTITUTED PARAMETER VALUES
C
      COMMON/COMSHIP/NCREP,NSLEN,NSENT,NSPAR,MINNT,
     X               NSTYPE(NTSUBT,NENTMAX),NSSTYP(NTSUBT,NENTMAX),
     X               MINSIN(NENTMAX),
     X               SPARAM(NSPARAM,NENTMAX),
     X               ISCONF(NSPARAM,NENTMAX),
     X               SSUBST(NSPARAM,NENTMAX)
C
C    -----------------------------------------------------------------------
C
