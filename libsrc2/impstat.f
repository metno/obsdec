      SUBROUTINE IMPSTAT
C
C     PURPOSE.
C     --------
C
C         DEFINES FROM WMO MASTER FILE THE STATION NUMBERS
C         FOR IMPORTANT STATIONS (ECMWF INTERNAL DEFINITIONS)
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *IMPSTAT*
C
C             INPUT     : IPARAMS   STATION INFORMATION IN PACKED FORM
C                         IPOINTS   NUMBER OF STATION / WMO BLOCK
C
c     Arrays IPARAMS(50000) and IPOINTS(128):
c     IPARAMS packs information found from wmovola.sort into three 32 bits
c     words for each station. See the list of PACK statements in wmo_make.f
c     for how this is done. Only stations which are located in zones 
c     1-7 and 20-23, or stations found in imp_station.dat are included.
c     IPOINTS(K) is the index in IPARAMS where entries for stations in
c     WMO block number (zone) K starts. (wmo_make.f and imp_station.dat are
c     found on routine:~station/statliste/)
C
C             OUTPUT    : IMPSTA    THE NUMBERS (IIiii) OF IMPORTANT STATIONS
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C

      implicit none
      integer j,zone,i,itemp,ibit,iii,ista,n

      INCLUDE 'parameter.f'
      INCLUDE 'combuff.f'  ! iparams,ipoints,impsta
      include 'comwork.f'  ! mindic

C     ------------------------------------------------------------------
C*          1.   FIND IMPORTANT STATIONS.
C                ------------------------
      J = 0
      ZONE = 1

      DO I = 1,50000,3
         IF (IPARAMS(I) .EQ. MINDIC) GO TO 400

C        Have we reached next zone?
         IF (I .GE. IPOINTS(ZONE+1)) THEN
 100        ZONE = ZONE + 1
            IF (ZONE .GE. 99) GO TO 400
            IF (IPOINTS(ZONE) .EQ. IPOINTS(ZONE+1)) GO TO 100
         END IF

         CALL GBYTE(IPARAMS(I+2),ITEMP,28,1)
         CALL GBYTE(IPARAMS(I+2),IBIT ,25,1)

         IF (ITEMP .EQ. 1 .AND. IBIT .EQ. 1) THEN
            CALL GBYTE(IPARAMS(I),III,0,10)
            ISTA = 1000*ZONE + III

C           Is station already included in IMPSTA?
            IF (J .NE. 0) THEN
               DO N=1,J
                  IF (ISTA .EQ. IMPSTA(N)) GO TO 300
               END DO
            END IF

            J = J + 1
            IF (J.GT.2000) THEN
               WRITE(*,*) 'DIMENSION OF IMPSTA TOO SMALL'
               GO TO 400
            END IF

            IMPSTA(J) = ISTA
         END IF
         
 300     CONTINUE
      END DO

 400  CONTINUE

      RETURN
      END
