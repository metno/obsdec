      SUBROUTINE SEC2( IERR )
C
C**** *SEC2*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO SET UP SECTION 2 OF *BUFR
C         MESSAGE.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SEC2(IERR)*
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
C         *CALL* *OCTNUM(KWPTB,KBPTB)*
C
C            *KWPTB* - POINTER TO WORD AT BEGINING OF SECTION.
C            *KBPTB* - POINTER TO BIT ISIDE THE WORD.
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
C     ------------------------------------------------------------------
C
C*          1.   SET UP *SECTION 2 ( OPTIONA SECTION).
C                -------------------------------------
 100  CONTINUE
C
      IF( IERR.NE.0 ) RETURN
      IF(.NOT. OPS2 ) RETURN
C     ------------------------------------------------------------------
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---- -------- -- --- -------- -- --- -------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C     ------------------------------------------------------------------
C*          1.2  SET UP LENGTH OF SECTION (OCTET 1-3 ) TO ZERO.
C                ----------------------------------------------
 120  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,24)
C
C     ------------------------------------------------------------------
C*          1.3  SET UP RESERVED BYTE  TO ZERO.
C                ------------------------------
 130  CONTINUE
C
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,8)
C
C     ------------------------------------------------------------------
C*          1.4  SET UP RDB KEY (48 BYTES).
C
 140  CONTINUE
C 
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
      CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,32)
C
C*          1.5  SET UP LENGTH OF THE SECTION 2.
C                ------------------------------
 150  CONTINUE
C
      CALL OCTNUM(IWPTB,IBPTB)
C
C     ------------------------------------------------------------------
 200  CONTINUE
C
      RETURN
      END
