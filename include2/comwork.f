C
C     'COMWORK' CONTAINS WORK AREAS USED BY DECODING ROUTINES AND
C     ALSO THE MISSING DATA INDICATOR.
C
      integer kchar,kint,kdec,kerr,it1,it2,mindic,khead,ilen

      COMMON / COMWORK / KCHAR(jp17),KINT(jp15),KDEC(jp11),
     C                   KERR,IT1,IT2,MINDIC,ilen
cpssep02 Replaced jp15 with jp17 in KCHAR
cpsoct06 Removed KHEAD(jp12)


c     kerr = 1:  NIL report or error in extracting YYGG group
c     kerr = 2:  (pilo) if error in NEXTPRT2 or invalid station number
c                (temp) if invalid station number
c                (syno) report shorter than 5 chars
c                (meta) report shorter than 4 chars
c     kerr = 3:  (pilo) invalid observation type in station list, or not a pilot,
c                       or error in YYGG for ship report
c     kerr = 4:  (pilo) wrong code for marsden square      
c     kerr = 7:  (proctxt) Too short bulletine
c     kerr = 8:  (proctxt) Domestic bulletine from UK or Germany
c     kerr = 99: (meta) Not in station list
