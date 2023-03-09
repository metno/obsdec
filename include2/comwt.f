      integer iwtr,iwts,iwtrv,iwtdw,m,n,nsub,mcv,ms,mss
      logical lshear
      real values,valuew

      COMMON /COMWT/ IWTR(JP22),IWTS(JP22),IWTRV(JP22),
     1               VALUES(JP22,JP1),valuew(jp22,jp1),
     2               IWTDW(JP22),M,N,NSUB,MCV,MS,MSS,
     3               LSHEAR

C           IWTR -    BUFR TABLE B REFERENCE
C           IWTS -    SCALE
C           IWTRV-    REFERENCE VALUE
C           VALUES-   ARRAY CONTAINING REAL VALUES.
C           VALUEW-   ARRAY CONTAINING REAL VALUES. used in nix_drau.F only
C
C           M      -  NUMBER OF DATA DESCRIPTORS
C           N      -  NUMBER OF SUB-SETS  cps: alltid lik 1 hos oss?
C           NSUB   -  NUMBER OF SUB-SETS (COUNTER)   
C           MCV    -  NUMBER OF CONFIDENCE VALUES
C           MS     -  number of data descriptors for sub surface data (used for drau only)
C           MSS    -  NUMBER OF DESCRIPTORS FOR SUBSTITUTED VALUES
c           LSHEAR: true if wind shear (4vbvbvava) is present in temp/pilo
