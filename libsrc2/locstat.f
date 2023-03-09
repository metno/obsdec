      SUBROUTINE LOCSTAT ( IWIND,IRET )
C
C     PURPOSE.
C     --------
C
C         EXTRACT PARTICULARS OF WMO OBSERVING STATIONS AND
C         PUT IN DECODED REPORT HEADER.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *LOCSTAT(IWIND,IRET)*
C
C          INPUT    : ARGUMENTS NOT USED ON INPUT .
C
C                     KINT(4) - WMO STATION NUMBER IN INTEGER.
C                     KDEC(4) - INTEGER DENOTING OBSERVATION TYPE.
C
C          OUTPUT   : KDEC(5) - LATITUDE IN HUNDREDTHS OF DEGREES ,
C                               NORTH + , SOUTH - .
C                     KDEC(6) - LONGITUDE IN HUNDREDTHS OF DEGREES ,
C                               EAST + , WEST - .
C                     KDEC(8) - STATION PRESSURE ELEVATION (H/P) OR IF
C                               NONE EXISTS STATION GROUND ELEVATION (H/A).
C                               IF NEITHER EXIST MINDIC IS RETURNED . VALUE
C                               IS INTEGER IN METRES.
C
C                     KDEC(15) - IMPORTANT STATION OR GOOD QUALITY STATION
C                                FLAG BITS SET IN THIS WORD.
C
C                     KDEC(16) - WMO COUNTRY NUMBER , EXCLUDING FIRST 2
C                                DIGITS ( REGION NUMBER ) . INTEGER.
C                     KDEC(17) - WMO REGION NUMBER , INTEGER.
C
C                     KDEC(23) - PRESSURE LEVEL INDICATOR , INTEGER.
cps This is the value of last column in wmovola.sort, which corresponds to a 
cps certain column in Pub9volAyymmdd.flatfile, see geopotential.hlp for the correspondence 
C                            0 = SEA LEVEL
C                            1 = STATION LEVEL
C                            2 = 850 HPA
C                            3 = 700 HPA
C                            4 = 500 HPA   cps: all stations had 1000 HPA (no 500 HPA in vol A)
C                            5 = 1000 GPM  cps: GPM is probably geopotential metres, but what siginifies this?
C                            6 = 2000 GPM
C                            7 = 3000 GPM
C                            8 = 4000 GPM
C                            9 = 900 HPA   cps: except for 3 stations in Sri Lanka, this is 925 HPA (a3=2)
C
C
C                     IWIND - Wind speed indicator
C                            0 = wind in knots
C                            1 = wind in m/s
C                            This information is fetched from IPARAMS (via 
C                            station_amend.dat from wmovola.sort from hard coded
C                            list (ICLIST) in wmo_make.f of countries which report
C                            wind in m/s (someone ought to check that list!!))
C
C                     IRET - INTEGER RETURN CODE
C                            0 = NO ERROR
C                            1 = STATION NUMBER NOT IN DIRECTORY
C                            2 = INVALID STATION NUMBER
C                            3 = INVALID OBSERVATION TYPE (not syno, pilo or temp)
C                            4 = STATION NUMBER IN THE LIST BUT
C                                NOT FOR THAT OBSERVATION TYPE. 
C
C
C     EXTERNALS.
C     ----------
C
C         *CALL* *GBYTE(KS,KD,KBPT,KSI)*
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC
C
C     MODIFICATIONS.
C     --------------
C
C

      implicit none
      integer IWIND,IRET
      integer itype,wmo_block,ind1,ind2,stnr,i,istn,isgn,isgq,iskip,itp

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'    ! kint,kdec
      INCLUDE 'combuff.f'    ! iparams,ipoints

C     ------------------------------------------------------------------
C*          1.   LOCATE STATION.                   .
C                ---------------

C     CLEAR ERROR RETURN INDICATOR

      IRET = 0

C     TYPE OF STATION 'ITYPE' IS SET TO 4 FOR SYNOP, 2 FOR PILOT AND 1 FOR TEMP.

      IF ( KDEC(4).EQ.11.OR.KDEC(4).EQ.14 ) THEN
         ITYPE = 4   ! synop
      ELSE IF ( KDEC(4).EQ.32 ) THEN
         ITYPE = 2   ! pilot
      ELSE IF ( KDEC(4).EQ.35 ) THEN
         ITYPE = 1   ! temp
      ELSE
         IRET = 3
         RETURN
      END IF

C     CHECK VALIDITY OF STATION NUMBER

      IF ( KINT(4).LE.1000.OR.KINT(4).GT.99999 ) THEN
         IRET = 2
         RETURN
      END IF


C     LOCATE STARTING POINT IN ARRAY 'IPARAMS' OF THE WMO BLOCK
C     OF THE STATION.

C     EXTRACT WMO BLOCK NUMBER

      WMO_BLOCK = KINT(4) / 1000

C     WORD 'WMO_BLOCK' OF 'IPOINTS' SHOWS WHERE THE ENTRIES FOR BLOCK 'WMO_BLOCK'
C     START IN 'IPARAMS'.

      IND1 = IPOINTS(WMO_BLOCK)
      IND2 = IPOINTS(WMO_BLOCK+1)-3

C     STARTING AT THIS WORD A SEQUENTIAL SEARCH IS MADE FOR AN ENTRY
C     FOR THE REQUIRED STATION NUMBER ( STNR ) >

      STNR = KINT(4) - ( WMO_BLOCK * 1000 )

C     FOR ONE STATION ENTRY 3 WORDS ARE USED

      DO I=IND1,IND2,3
         CALL GBYTE(IPARAMS(I),ISTN,0,10)
         IF(ISTN.EQ.STNR) GO TO 200
      END DO

C     STATION NUMBER NOT FOUND

      IRET = 1
      RETURN


C     -----------------------------------------------------------------
C*             2.  EXTRACT REQUIRED PARAMETERS FROM 1ST WORD ENTRY.
C                  -----------------------------------------------
 200  CONTINUE

C     PRESSURE LEVEL CODE FIGURE
C     In wmovola.sort, this is last digit on line (normally 0)

      IF(ITYPE .EQ. 4)
     *   CALL GBYTE(IPARAMS(I),KDEC(23),16,4)


C     WIND SPEED UNIT INDICATOR

      CALL GBYTE(IPARAMS(I),IWIND,20,1)

C     STATION ELEVATION

      CALL GBYTE(IPARAMS(I),KDEC(8),24,14)
      IF ( KDEC(8).GT.9999 ) KDEC(8) = KDEC(8)-16383
      IF ( KDEC(8).EQ.9999 ) KDEC(8) = MINDIC
      CALL GBYTE(IPARAMS(I+1),ISGN,6,2)
      IF (KDEC(8).NE.MINDIC.AND.ISGN.EQ.1) KDEC(8)=-KDEC(8)

C     LONGITUDE

      CALL GBYTE(IPARAMS(I+1),KDEC(6),8,16)
      IF ( KDEC(6).EQ.65535 ) KDEC(6) = MINDIC
      IF ( KDEC(6).NE.MINDIC.AND.KDEC(6).GT.18000 )
     *     KDEC(6) = KDEC(6) - 36000

C     LATITUDE

      CALL GBYTE(IPARAMS(I+1),KDEC(5),24,14)
      IF ( KDEC(5).EQ.16383 ) KDEC(5) = MINDIC
      CALL GBYTE(IPARAMS(I+2),ISGN,6,1)
      IF (ISGN.EQ.1.AND.KDEC(5).NE.MINDIC)
     *     KDEC(5) = - KDEC(5)

CTEST
CTEST     WRITE(*,998)(KDEC(IR),IR=1,20)
CTEST998  FORMAT(' LOCSTAT - after lat., kdec(1-23) ',/,1H ,10I11)
CTEST

C     WMO REGION NUMBER

      CALL GBYTE(IPARAMS(I+2),KDEC(17),8,3)
      IF ( KDEC(17).EQ.0 ) KDEC(17) = 8

C     WMO COUNTRY NUMBER ( LAST 3 DIGITS )

      CALL GBYTE(IPARAMS(I+2),KDEC(16),11,10)


C     IMPORTANT STATION AND GOOD QUALITY FLAGS.

      CALL GBYTE(IPARAMS(I+2),ISGQ,24,2)
      KDEC(15) = IOR(KDEC(15),ISGQ)


C     CHECK THAT PARAMETERS ARE VALID FOR OBSERVATION TYPE REQUESTED.
C     SOME STATIONS HAVE MORE THAN 1 ENTRY, DEPENDING ON TYPE OF
C     OBSERVATION.

      IF (ITYPE.EQ.1) ISKIP=26
      IF (ITYPE.EQ.2) ISKIP=27
      IF (ITYPE.EQ.4) ISKIP=28

      CALL GBYTE(IPARAMS(I+2),ITP,ISKIP,1)
      IF (  ITP.NE.0 ) RETURN   ! station found and data extracted

C     PARAMETERS NOT CORRECT FOR CODE TYPE, SO USE NEXT ENTRY
C     IF IT EXIST

      I = I + 3

      CALL GBYTE(IPARAMS(I),ISTN,0,10)
      IF (ISTN.EQ.STNR) GO TO 200

CTEST
CTEST      WRITE(*,999)kint(4),(KDEC(IR),IR=1,20)
CTEST999   FORMAT(' LOCSTAT - WMO NUMBER, kdec(1-20) ',/,1H ,10I11)
CTEST      PRINT*,' LOCSTAT - ITP,ISTN,STNR ',ITP,ISTN,STNR
CTEST
C     RETAIN ALREADY EXTRACTED PARAMETERS
C     THAT MEANS THAT STATION TYPE DOES NOT CORRESPOND TO THE MESSAGE
C     RECEIVED. 

      IRET = 4

      RETURN
      END
