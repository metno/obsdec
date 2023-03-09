      SUBROUTINE SETGRS (IP,N)
C
C**** *SETGRS*
C
C       PURPOSE.
C      ----------
C
C         SET FLAGS FOR GROSS LIMIT CHECK
C
C       INTERFACE.
C      ------------
C
C        CALL SETGRS (IP,N)
C
C        INPUT
C         IP  -  POSITION OF CONFIDENCE FLAG
C         N   -  SEVERITY OF FAILURE (1 = IMPOSSIBLE VALUE,
C                                     2 = OUTSIDE MIN2/MAX2,
C                                     3 = BETWEEN MAX1 AND MAX2 ETC,
C                                     4 = BETWEEN MIN1 AND MAX1)
C
C        OUTPUT
C         IFLAG (CONF) - COUNTER AND CONFIDENCE VALUE UPDATED
C
C       METHOD.
C      ---------
C
C        UPDATE RELAVANT COUNTER AND CONFIDENCE DEPENDING ON
C          DEGREE OF PASS/FAIL
C
C       EXTERNALS.
C      ------------
C
C        NONE.
C
C       AUTHOR.
C      ---------
C
C        B. NORRIS,  ECMWF,  JANUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C        NONE.
C
      INCLUDE 'paramq.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
C
C    ---------------------------------------------------------------------
C
C                 1.  UPDATE COUNTERS AND CONFIDENCE
C                ------------------------------------
C
  100 CONTINUE
      IF(ICBO(N,2).EQ.IPASS) THEN
C
C           1.1  PASS COUNTER
C
           IFLAG(3,IP)=IFLAG(3,IP)+1
      ELSEIF (ICBO(N,2).EQ.IFAIL) THEN
C
C           1.2  FAIL COUNTER
C
           IFLAG(2,IP)=IFLAG(2,IP)+1
      ELSE
C
      ENDIF
C
C           1.3  CONFIDENCE VALUE
C
      IFLAG(1,IP)=IFLAG(1,IP)+ICBO(N,1)
C
C    -------------------------------------------------------------------
C
C                   2.  EXIT
C                  -----------
C
  200 CONTINUE
      RETURN
      END
