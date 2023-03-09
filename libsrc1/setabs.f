      SUBROUTINE SETABS (IF,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP9,IP10)
C
C**** *SETABS*
C
C       PURPOSE.
C      ---------
C
C          SET CONFIDENCE FOR A NUMBER OF PARAMETERS
C
C       INTERFACE.
C      ------------
C
C        CALL SETABS (IF,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP9,IP10)
C
C        INPUT
C         IF  -  CONFIDENCE VALUE TO BE APPLIED
C         IP- -  POSITIONS OF CONFIDENCE FLAGS
C                IF IP- = 'END' MARKER, NO FURTHER ARGUMENTS ARE
C                 CONSIDERED
C
C        OUTPUT
C         IFLAG (CONF) - CONFIDENCE FLAGS UPDATED
C
C       METHOD.
C      ---------
C
C        FOR EACH POSITION, SET CONFIDENCE UNTIL 'END' MARKER IS REACHED.
C
C       EXTERNALS.
C      ------------
C
C        NONE
C
C       REFERENCE.
C      ------------
C
C        NONE
C
C       AUTHOR.
C      ---------
C
C        B. NORRIS,  ECMWF,  MARCH 1990.
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
C    --------------------------------------------------------------------
C
C                 1.  SET FLAGS FOR EACH PARAMETER
C                -----------------------------------
C
  100 CONTINUE
      IF(IP1.EQ.IEND) GO TO 200
      IFLAG(1,IP1)=IF
      IF(IP2.EQ.IEND) GO TO 200
      IFLAG(1,IP2)=IF
      IF(IP3.EQ.IEND) GO TO 200
      IFLAG(1,IP3)=IF
      IF(IP4.EQ.IEND) GO TO 200
      IFLAG(1,IP4)=IF
      IF(IP5.EQ.IEND) GO TO 200
      IFLAG(1,IP5)=IF
      IF(IP6.EQ.IEND) GO TO 200
      IFLAG(1,IP6)=IF
      IF(IP7.EQ.IEND) GO TO 200
      IFLAG(1,IP7)=IF
      IF(IP8.EQ.IEND) GO TO 200
      IFLAG(1,IP8)=IF
      IF(IP9.EQ.IEND) GO TO 200
      IFLAG(1,IP9)=IF
      IF(IP10.EQ.IEND) GO TO 200
      IFLAG(1,IP10)=IF
C
C    --------------------------------------------------------------------
C
C                      2.  EXIT
C                    ------------
C
  200 CONTINUE
      RETURN
      END
