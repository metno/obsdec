      SUBROUTINE PACK(KBPW,KD,KS,KWPT,KBPT,KSI)
C
C**** *PACK*
C
C
C     PURPOSE.
C     --------
C            PURPOSE OF THIS ROUTINE IS TO PACK VALUE *KS* IN
C         *KSI* BITS, STARTED AT WORD KWPT OF ARRAY *KD* AFTER
C         SKIPPING KBPT BITS.  AT THE END
C         POINTERS *KWPT* AND *KBPT* ARE ADJUSTED.
C
C**   INTERFACE.
C     ----------
C
C     *CALL* *PACK(KBPW,KD,KS,KWPT,KBPT,KSI)*
C
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
cps          *KBPW*  - number of bits in a word
C
C     *METHOD.
C      -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C            *CALL SBYTE(KD,KS,KBPT,KSI)*
C
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       09/06/86.
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
      DIMENSION KD(*)
C
C     ------------------------------------------------------------------
C*          1.   SET UP BIT PATTERN.
C                -------------------
C
 100  CONTINUE
C
      CALL SBYTE(KD,KS,KBPT,KSI)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + KSI
C
      IF(KBPT.GE.KBPW) THEN
                          KW  = KBPT/ KBPW
                          KBPT= KBPT - KW * KBPW
                          KWPT= KWPT +KW
                       END IF
C
      RETURN
      END
