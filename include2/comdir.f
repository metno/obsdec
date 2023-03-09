C
C
C     Definition of directories used for input and output files
C
C     YMSYS    -    Bulletin files
C     YPPDAT   -    Preprocessing data i.e station data
C     YDATDEF  -    BUFR tables
C     YTEST    -    Output files for testing
C
      COMMON /COMDIR/ YMSYS,YPPDAT,YDATDEF,YTEST
      CHARACTER YMSYS*80
      CHARACTER YPPDAT*80
      CHARACTER YDATDEF*80
      CHARACTER YTEST*80
