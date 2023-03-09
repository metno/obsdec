      SUBROUTINE SETINT (IF,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP9,IP10)
C
C**** *SETINT*
C
C       PURPOSE.
C      ---------
C
C          SET FLAGS FOR A NUMBER OF PARAMETERS,
C               INTERNAL CONSISTENCY CHECKS
C
C       INTERFACE.
C      ------------
C
C        CALL SETINT (IF,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP9,IP10)
C
C        INPUT
C         IF  -  'PASS' OR 'FAIL' INDICATOR
C         IP- -  POSITIONS OF CONFIDENCE FLAGS
C                IF IP- = 'END' MARKER, NO FURTHER ARGUMENTS ARE
C                 CONSIDERED
C
C        OUTPUT
C         IFLAG (CONF) - COUNTERS AND CONFIDENCE FLAGS UPDATED
C
C       METHOD.
C      ---------
C
C        FOR EACH POSITION, CALL SETFLG UNTIL 'END' MARKER IS REACHED.
C
C       EXTERNALS.
C      ------------
C
C        SETFLG  -  UPDATE COUNTERS AND CONFIDENCE FLAGS
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
      INCLUDE 'conf.f'
      INCLUDE 'const.f'
C
C    --------------------------------------------------------------------
C
C                 1.  SET FLAGS FOR EACH PARAMETER
C                -----------------------------------
C
  100 CONTINUE
      IF(IP1.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP1)
      IF(IP2.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP2)
      IF(IP3.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP3)
      IF(IP4.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP4)
      IF(IP5.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP5)
      IF(IP6.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP6)
      IF(IP7.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP7)
      IF(IP8.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP8)
      IF(IP9.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP9)
      IF(IP10.EQ.IEND) GO TO 200
      CALL SETFLG (IF,IP10)
C
C    --------------------------------------------------------------------
C
C                      2.  EXIT
C                    ------------
C
  200 CONTINUE
      RETURN
      END
