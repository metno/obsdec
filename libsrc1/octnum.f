      SUBROUTINE OCTNUM(KWPTB,KBPTB)
C
C**** *OCTNUM*
C
C
C     PURPOSE.
C     --------
C             CALCULATE NUMBER OF OCTETS FROM BIT POSITION DEFINED BY
C     KWPT,KBPT  AND KWPTB,KBPTB; NUMBER OF OCTETS MUST BE EVEN. IF IT IS
C     NEEDED PADING WITH 0 BIT PERFORMS. NUMBER OF OCTETS IS WRITTEN AT
C     BEGINIG OF THE SECTION.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *OCTNUM(KWPTB,KBPTB)*
C
C     *METHOD.
C      -------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C     *CALL* *PACK(KBPW,KD,KS,KWPT,KBPT,KSI)*
C
C            *KBPW*  - NUMBER OF BITS IN COMPUTER WORD.
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KWPT*  - POINTER TO WORD IN KD ARRAY.
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C      *CALL* *SBYTE(KD,KS,KSKIP,KSIZE)
C
C             *KD*    - DESTINATION
C             *KS*    - SOURCE
C             *KSKIP* - NUMBER OF BITS TO BE SKIPPED
C             *KSIZE* - BIT LENGTH
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       07/10/87.
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
      INCLUDE 'parameter.f'
      INCLUDE 'compoin.f'
C     ------------------------------------------------------------------
C*          1.    CALCULATE NUMBER OF OCTETS.
C                 ---------------------------
 100  CONTINUE
C
      IB   =(NWPT-1) * NBPW + NBPT
      IBB  =(KWPTB-1)* NBPW + KBPTB
C
      IDIFB= IB - IBB
C
      NOCT = IDIFB/8
      IBDW = IDIFB - NOCT*8
C
      IF(IBDW .NE.0)THEN
                       NOCT=NOCT+1
                       IBDW=8-IBDW
                       CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,IBDW )
                    END IF
C
C     ------------------------------------------------------------------
C*          2.  CHECK IF THERE ARE EVEN NUMBER OF OCTETS IN BLOCK.
C               --------------------------------------------------
 200  CONTINUE
C
      IF(MOD(NOCT,2).NE.0) THEN
          IBDW = 8
          CALL PACK(NBPW,MBUF(NWPT),0,NWPT,NBPT,IBDW )
          NOCT = NOCT+1
      END IF
C     ------------------------------------------------------------------
C*          3.  WRITE NUMBER OF OCTETS AT BEGINING OF BLOCK.
C               --------------------------------------------
 300  CONTINUE
C
      IBDW  = 24
      CALL SBYTE(MBUF(KWPTB),NOCT,KBPTB,IBDW)
C
      RETURN
      END
