      integer IBLOCKS,ISTASA,ISTASB
     C                  ,ISTASC,ISTASD
     C                  ,ITIMES,IERSHIP,IERALL
     C                  ,IERALLA,IERALLB,IERALLC,IERALLD
     C                  ,IDONEA,IDONEB,IDONEC,IDONED
     C                  ,IPRLIM,IPRALLA,IPRALLB,IPRALLC,IPRALLD
     C                  ,MODIFY
C
C  'PRINTRE' CONTAINS PRINTING DIRECTIVES FOR TESTING PURPOSES.
C        IBLOCKS - THE WMO BLOCK NUMBERS FOR PRINTED DATA
C        ISTASA  - STATION TO BE PRINTED FOR PART A
C        ISTASB  - ------- -- -- ------- --- ---- B
C        ISTASC  - ------- -- -- ------- --- ---- C
C        ISTASD  - ------- -- -- ------- --- ---- D
C        ITIMES  - TIME LIMITS FOR REPORTS TO BE PRINTED
C        IERSHIP - > 0  THEN PRINT ALL ERRONEUS SHIP REPORTS WITHIN
C                       THE TIME LIMITS
C        IERALL  - > 0  THEN PRINT ALL ERRONEUS REPORTS WITHIN
C                       THE TIME LIMITS
C
      COMMON / PRINTRE / IBLOCKS(10),ISTASA(100),ISTASB(100)
     C                  ,ISTASC(100),ISTASD(100)
     C                  ,ITIMES(2),IERSHIP,IERALL
     C                  ,IERALLA,IERALLB,IERALLC,IERALLD
     C                  ,IDONEA(200),IDONEB(200),IDONEC(200),IDONED(200)
     C                  ,IPRLIM,IPRALLA,IPRALLB,IPRALLC,IPRALLD
     C                  ,MODIFY
