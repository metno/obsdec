      SUBROUTINE SETFLG (IF,IP)
C
C**** *SETFLG*
C
C       PURPOSE.
C      ----------
C
C         UPDATE FLAGS AND FLAG COUNTERS (INTERNAL CONSISTENCY CHECK)
C
C       INTERFACE.
C      ------------
C
C        CALL SETFLG (IF,IP)
C
C        INPUT
C         IF  -  'PASS' OR 'FAIL' INDICATOR
C         IP  -  POSITION OF CONFIDENCE FLAG
C
C        OUTPUT
C         IFLAG (CONF) - COUNTERS AND CONFIDENCE FLAGS UPDATED:
C           IFLAG(1,--) = CONFIDENCE FLAG
C           IFLAG(2,--) = NUMBER OF FAILS
C           IFLAG(3,--) = NUMBER OF PASSES
C
C       METHOD.
C      ---------
C
C        INCREMENT RELEVANT COUNTER;
C        UPDATE CONFIDENCE FLAG WITH A VALUE DEPENDING ON NUMBER OF
C        PREVIOUS FAILS AND PASSES.
C
C       EXTERNALS.
C      ------------
C
C        NONE.
C
C       REFERENCE.
C      ------------
C
C        NONE
C
C       AUTHOR.
C      ---------
C
C        B. NORRIS,  ECMWF,  JANUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C        NONE
C
      INCLUDE 'paramq.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
C
C    -------------------------------------------------------------------
C
C                1.  UPDATE CONFIDENCE AND COUNTERS.
C               -------------------------------------
C
  100 CONTINUE
      IF(IP.EQ.IMISS) GO TO 200
C
C          1.1  PASS
C
      IF(IF.EQ.IPASS) THEN
          IFLAG(3,IP)=IFLAG(3,IP)+1
          IF(IFLAG(3,IP).LE.NCOCO) IFLAG(1,IP)=IFLAG(1,IP)
     X      +ICOCO(IFLAG(3,IP),2)
      ENDIF
C
C          1.2  FAIL
C
      IF(IF.EQ.IFAIL) THEN
          IFLAG(2,IP)=IFLAG(2,IP)+1
          IF(IFLAG(2,IP).LE.NCOCO) IFLAG(1,IP)=IFLAG(1,IP)
     X      +ICOCO(IFLAG(2,IP),1)
      ENDIF
C
C    ----------------------------------------------------------------------
C
C                   2.  EXIT.
C                  -----------
C
  200 CONTINUE
      RETURN
      END
