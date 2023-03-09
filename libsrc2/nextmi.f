      SUBROUTINE NEXTMI(I,J,II,KDEC4)
C
C     PURPOSE.
C     --------
C
C         TO FIND NEXT MIMIMJMJ GROUP IN THE BULLETIN.
C         SCANS BULLETIN IN 'KCHAR' FOR NEXT GROUP OF
C         ('TTAA' OR 'TTBB' OR 'TTCC' OR 'TTDD' ETC.)
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTMI(I,J,II)*
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR'
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR'
C
C         OUTPUT    : II- POSITION OF THE FIRST CHARACTER
C                         IN REQUIRED GROUP.
C                         IF CHARACTER NOT FOUND II = 99999
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
C
C     MODIFICATIONS.
C     --------------
C
C         PS Feb 2007. Search for explicit MiMiMjMj according to (new) argument 
C                      kdec(4). This avoids treating NNNN as a MiMiMjMj!

      implicit none
      integer i,j,ii,kdec4,k,m,istart
      character*4 ymimj

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'    ! kchar

C     ------------------------------------------------------------------
C*          1.   FIND NEXT MIMIMJMJ GROUP.
C                -------------------------
      II=999999
      K =IABS(I)
      M =IABS(I)

 101  CONTINUE

      CALL NEXSEP2(M,J,*1000)
      CALL NEXPRT2(M,J,*1000)

      IF(M.GE.J) RETURN

      YMIMJ=CHAR(KCHAR(M  ))//CHAR(KCHAR(M+1))//CHAR(KCHAR(M+2))//
     1      CHAR(KCHAR(M+3))

      ISTART=0
      IF (KDEC4.EQ.165) THEN                   ! Drau
         IF(YMIMJ(1:4).EQ.'ZZYY') ISTART=1
      ELSE IF (KDEC4.EQ.63) THEN               ! Tesac
         IF(YMIMJ(1:4).EQ.'KKYY') ISTART=1
         IF(YMIMJ(1:4).EQ.'KKXX') ISTART=1
      ELSE IF (KDEC4.EQ.64) THEN               ! Bathy
         IF(YMIMJ(1:4).EQ.'JJVV') ISTART=1
         IF(YMIMJ(1:4).EQ.'JJYY') ISTART=1
         IF(YMIMJ(1:4).EQ.'JJXX') ISTART=1
      ELSE IF (KDEC4.EQ.35) THEN               ! Temp land
         IF(YMIMJ(1:4).EQ.'TTAA') ISTART=1
         IF(YMIMJ(1:4).EQ.'TTBB') ISTART=1
         IF(YMIMJ(1:4).EQ.'TTCC') ISTART=1
         IF(YMIMJ(1:4).EQ.'TTDD') ISTART=1
      ELSE IF (KDEC4.EQ.36) THEN               ! Temp ship
         IF(YMIMJ(1:4).EQ.'UUAA') ISTART=1
         IF(YMIMJ(1:4).EQ.'UUBB') ISTART=1
         IF(YMIMJ(1:4).EQ.'UUCC') ISTART=1
         IF(YMIMJ(1:4).EQ.'UUDD') ISTART=1
      ELSE IF (KDEC4.EQ.135) THEN              ! Temp drop
         IF(YMIMJ(1:4).EQ.'XXAA') ISTART=1
         IF(YMIMJ(1:4).EQ.'XXBB') ISTART=1
         IF(YMIMJ(1:4).EQ.'XXCC') ISTART=1
         IF(YMIMJ(1:4).EQ.'XXDD') ISTART=1
      ELSE IF (KDEC4.EQ.137) THEN              ! Temp mobile
         IF(YMIMJ(1:4).EQ.'IIAA') ISTART=1
         IF(YMIMJ(1:4).EQ.'IIBB') ISTART=1
         IF(YMIMJ(1:4).EQ.'IICC') ISTART=1
         IF(YMIMJ(1:4).EQ.'IIDD') ISTART=1
      ELSE IF (KDEC4.EQ.32) THEN               ! Pilot land
         IF(YMIMJ(1:4).EQ.'PPAA') ISTART=1
         IF(YMIMJ(1:4).EQ.'PPBB') ISTART=1
         IF(YMIMJ(1:4).EQ.'PPCC') ISTART=1
         IF(YMIMJ(1:4).EQ.'PPDD') ISTART=1
      ELSE IF (KDEC4.EQ.33) THEN               ! Pilot ship
         IF(YMIMJ(1:4).EQ.'QQAA') ISTART=1
         IF(YMIMJ(1:4).EQ.'QQBB') ISTART=1
         IF(YMIMJ(1:4).EQ.'QQCC') ISTART=1
         IF(YMIMJ(1:4).EQ.'QQDD') ISTART=1
      ELSE IF (KDEC4.EQ.141) THEN              ! Airep
         IF(YMIMJ(1:4).EQ.'ARP ') ISTART=1 
         IF(YMIMJ(1:4).EQ.'ARS ') ISTART=1 
         IF(YMIMJ(1:4).EQ.'AIR ') ISTART=1 
      ELSE IF (KDEC4.EQ.144) THEN              ! Amdar
         IF(YMIMJ(1:4).EQ.'LVR ') ISTART=1 
         IF(YMIMJ(1:4).EQ.'ASC ') ISTART=1 
         IF(YMIMJ(1:4).EQ.'DES ') ISTART=1 
         IF(YMIMJ(1:4).EQ.'LVW ') ISTART=1
         IF(YMIMJ(1:4).EQ.'UNS ') ISTART=1
C     '///' is another possibility, but this may also mark a missing humidity
C     group, so it is dangerous to test for that
      ELSE IF (KDEC4.EQ.85) THEN               ! Metar
         IF(YMIMJ(1:4).EQ.'META') ISTART=1 
      ELSE IF (KDEC4.EQ.86) THEN               ! Speci
         IF(YMIMJ(1:4).EQ.'SPEC') ISTART=1 
      ELSE
         WRITE(*,*) 'This KDEC(4) is not supported in NEXTMI!', KDEC(4)
         RETURN
      END IF

      IF(ISTART.EQ.1) THEN
         CALL PRESEP(M,K,*1000)
         CALL PREPRT(M,K,*1000)
         II= M+1
         RETURN
      END IF

      GO TO 101

 1000 CONTINUE

      RETURN
      END
