      integer ieof,iparams,ipoints,impsta
C
C     'COMBUFF' CONTAINS READ IN AREAS USED BY VARIOUS FILES ,
C     AS WELL AS END-OF-FILE INDICATOR.
C
cpsdec04      COMMON / COMBUFF / IEOF,IPARAMS(33000),IPOINTS(128)
      COMMON / COMBUFF / IEOF,IPARAMS(50000),IPOINTS(128),IMPSTA(2000)
