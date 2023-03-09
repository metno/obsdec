C
C     'COMWORK' CONTAINS WORK AREAS USED BY DECODING ROUTINES AND
C     ALSO THE MISSING DATA INDICATOR.
C
cpssep02 From Nov 2000 the number of octes allowed in a message
cps      was increased from 3800 to 15000 (WMO's Manual on GTS)
cps     COMMON / COMWORK / KCHAR(jp15),KINT(jp15),KDEC(jp11),
      COMMON / COMWORK / KCHAR(jp17),KINT(jp15),KDEC(jp11),
     C                   KERR,IT1,IT2,MINDIC,KHEAD(jp12),ilen
C
