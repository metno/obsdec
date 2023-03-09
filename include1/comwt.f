c
      COMMON /COMWT/ IWTR(JP22),IWTS(JP22),IWTRV(JP22),
     1               VALUES(JP22,JP1),valuew(jp22,jp1),
     1               IWTDW(JP22),M,N,NSUB,MQ,MS,MSS,
     2               L11,L12,L13,L21,L22,L31,L32,L33,NDATA,NT,NC,
     3               LSHEAR,NSNUM
C
C           IWTR -    BUFR TABLE B REFERENCE
C           IWTS -    SCALE
C           IWTRV-    REFERENCE VALUE
C           VALUES-   ARRAY CONTAINING REAL VALUES.
C           VALUEW-   ARRAY CONTAINING REAL VALUES.
C
C           M      -  NUMBER OF DATA DESCRIPTORS
C           N      -  NUMBER OF SUB-SETS  cps: alltid lik 1 hos oss?
C           NSUB   -  NUMBER OF SUB-SETS (COUNTER)   
C           NSNUM  -  UPDATED SEQUENCE NUMBER OF BUFR REPORT
C           MQ     -  NUMBER OF DESCRIPTORS FOR Q/C
C           MSS    -  NUMBER OF DESCRIPTORS FOR SUBSTITUTED VALUES
cps         L11-L33 - Logical variables, set true if
c           L11: DRIBU SURFACE REPORT
c           L12: DRIBU SUBSURFACE Z,T,S REPORT
c           L13: DRIFTER SUBSURFACE Z,DD,CCC REPORT (CURRENT)
c           L21: SURFACE BATHY
c           L22: OCEANOGRAPHIC BATHY
c           L31: SURFACE TESAC
c           L32: OCEANOGRAPHIC TESAC (Z, T, S)
c           L33: OCEANOGRAPHIC TESAC (Z, DD CCC)
c           NDATA - not used, except in nix_drau.f, subr. setwtd and setwtb
c                   Should be removed from common?
c           NT - number of subsurface temperature levels (nix_drau.f)
c           NC - number of subsurface current levels (nix_drau.f)
cps
