      SUBROUTINE PROCRFB ( IERR, T1expected, T1alt )
C**** *PROCRFB*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO FORMAT BULLETIN.
cps i.e.: - extract T1 and T2 (as integer: 1 for A, 2 for B...)
cps       - if not found or T1 not the expected one: set T1 to 27 
cps         (=the equivalent of '[' = 91 in ascii numbering)
cps       - set last character in message to 'GS' (ascii 29)
cps       - set IGS
cps       Returns IERR=1 if T1 is not T1expected (and not T1alt,
cps       which ought to be ' ' except for metar, which has 2
cps       alternatives for T1
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCRFB(IERR,T1expected,T1alt)*
C
C         INPUT     : T1expected - the character expected as T1 (in T1T2A1A2...)
C                     T1alt      - an alternative character for T1 (set this
C                                  to space (' ') if there is no alternative)
C         OUTPUT:     IERR > 0 if error, see 'PURPOSE'
C         SETS:       IT1,IT2,IGS,KCHAR(IGS)
C                     IT1 set to 27 (']') if T1 is not as expected
C 
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C          J. HENNESSY         *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C
C          PS oct06 Added arguments T1expected and T1alt and tidying up
C

      implicit none
      integer ierr
      character T1expected,T1alt
      integer i,IT1expected,IT1alt

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'    ! kchar,it1,it2,ilen
      INCLUDE 'comindx.f'    ! igs

C     ------------------------------------------------------------------
C*          2.  DEFINE T1 AND T2 FROM ABBREVIATED HEADING.
C               ------------------------------------------

      DO I=1,ILEN
         IF(KCHAR(I).GE.65.AND.KCHAR(I).LE.90) GO TO 203
      END DO

      IT1=27
      GO TO 210

 203  CONTINUE

      IT1=KCHAR(I  )-64
      IT2=KCHAR(I+1)-64
      IT1expected = ichar(T1expected) - 64
      IT1alt = ichar(T1alt) - 64

      IF(IT1.NE.IT1expected .and. IT1.NE.IT1alt) THEN
         IERR=1
         IT1=27
         RETURN
      END IF

C     CHECK IF 'T2' CHARACTER IS LETTER.

c               A           Z       [
      IF(IT2.LT.1.OR.IT2.GT.26) IT1=27

C*          2.1  LAST CHARACTER OF BULLETIN CAN BE IN ANY OF THE LAST
C                -----------------------------------------------------
C                5 WORDS. IF CHARACTER  IS 'ETX' REPLACE BY 'GS' .
C                -------------------------------------------------
C                IF NEITHER CAN BE FOUND INSERT 'GS' AS LAST CHARACTER.
C                ------------------------------------------------------
 210  CONTINUE

      DO I=1,ILEN
         IF (KCHAR(I).EQ.3.OR.KCHAR(I).EQ.29) THEN
            KCHAR(I)= 29
            IGS=I
            RETURN
         END IF
      END DO

      KCHAR(ILEN+1)= 29
      IGS=ILEN+1

      RETURN
      END
