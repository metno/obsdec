      SUBROUTINE SEC0( IERR )
C
C**** *SEC0*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO SET UP SECTION 0 OF *BUFR
C         MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SEC0(IERR)*
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
C         *CALL* *PACK(KBPW,KD,KS,KWPT,KBPT,KSI)*
C
C            *KBPW*  - NUMBER OF BITS IN COMPUTER WORD.
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KWPT*  - POINTER TO WORD IN KD ARRAY.
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
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
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. Dragosavac 13/11/1991  : change to Bufr Edition 2
c          P.S.          10/09/2003  : change to Bufr Edition 3
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'compoin.f'
C     ------------------------------------------------------------------
C
C*          1.   SET UP *SECTION 0 ( INDICATOR SECTION )
C                --- --  ------- -   --------- -------
 100  CONTINUE
C
      IF( IERR.NE.0 ) RETURN
C
C*          1.1  IT IS FOUR OCTETS LONG CHARACTER CODED ACCORDING TO THE
C                -------------------------------------------------------
C                *INTERNATIONAL *TELEGRAPHIC *ALPHABET *NO. 5 AS *BUFR*.
C                 -------------------------------------------------------
 110  CONTINUE
C
cps We start with mbuf(2). For convenience we will in nix_<obstype>.f put length 
cps of bufr_message in mbuf(1). The real BUFR-message start at mbuf(2)!
      NWPT= 2
      NBPT= 0
C
C     B IS CCITT.5 DECIMAL 66
C     U IS CCITT.5 DECIMAL 85
C     F IS CCITT.5 DECIMAL 70
C     R IS CCITT.5 DECIMAL 82
C
      CALL PACK(NBPW,MBUF(NWPT),66,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),85,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),70,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),82,NWPT,NBPT,8)
C
C            Bufr Edition 3
C
c Sets length of bufr record to 0. This will be updated in PUTBUFR in nix_<obstype>.f
      CALL PACK(NBPW,MBUF(NWPT), 0,NWPT,NBPT,24)
cpssep03      CALL PACK(NBPW,MBUF(NWPT), 2,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT), 3,NWPT,NBPT,8)
C
      RETURN
      END
