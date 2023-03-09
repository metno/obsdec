      SUBROUTINE SEC5( IERR )
C
C**** *SEC5*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO SET UP SECTION 5 OF *BUFR
C         MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SEC5(IERR)*
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
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C
C
      INCLUDE 'parameter.f'
      INCLUDE 'compoin.f'
C
C     ------------------------------------------------------------------
C
C*          1.   SET UP *SECTION 5 ( END SECTION ).
C                ----------------------------------
 100  CONTINUE
C
      IF( IERR.NE.0 ) RETURN
C
C*          1.1  IT IS FOUR OCTETS LONG CHARACTER CODED ACCORDING TO THE
C                -------------------------------------------------------
C                *INTERNATIONAL *TELEGRAPHIC *ALPHABET *NO. 5 AS *BUFR*.
C                 ------------------------------------------------------
 110  CONTINUE
C
C
C     7 IS CCITT.5 DECIMAL 55
C
      CALL PACK(NBPW,MBUF(NWPT),55,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),55,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),55,NWPT,NBPT,8)
      CALL PACK(NBPW,MBUF(NWPT),55,NWPT,NBPT,8)
C
C                                                                       
C*          1.3  SET TOTAL LENGTH OF BUFR MESSAGE.
C                ---------------------------------
 130  CONTINUE
C
      IBYTES=(NWPT-2)*NBPW/8+NBPT/8
C
C*          1.4  SET TOTAL LENGTH OF BUFR MESSAGE IN SECTION 0.
C                ----------------------------------------------
 140  CONTINUE
C
C        FOR BUFR EDITION 2 LENGTH OF MESSAGE STARTS AT 5 TH BYTE.
cps      in Bufr message. But we start Bufr message in mbuf(2), so length of
cps      message will start in mbuf(3), i.e. 9th byte
C
      IWPT= 32/NBPW + 2
cpsdec03 The statement below makes IBPT = -32. With SGI the implementation of
cps      sbyte() called from PACK actually treats this as 0 (so it did work!), while
cps      on linux we are transfered 32 bits backwards, so we overwrite 'BUF'.
cps      IBPT= 32 - (IWPT-1)* NBPW
      IBPT= 32 - (IWPT-2)* NBPW
C
      CALL PACK(NBPW,MBUF(IWPT),IBYTES,IWPT,IBPT,24)
C
      RETURN
      END
