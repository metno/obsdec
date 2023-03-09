      SUBROUTINE REMEEE
C
C**** *REMEEE*
C
C
C     PURPOSE.
C     --------
C
C         HANDLE TYPING ERRORS CORRECTED BY THE 'E E E'
C         PROCEDURE AS SPECIFIED IN GTS MANUAL.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *REMEEE*
C
C         INPUT     : REPORT IN KCHAR(IPT) - KCHAR(IEQ) , IN CCITT 5 ,
C                     1 CHARACTER PER WORD.
C
C         OUTPUT    : E'S , ERRONEUS CHARACTERS AND REPEATED GROUPS REPLACED
C                     BY SPACE CHARACTERS. THESE ARE IGNORED IN SCANNING
C                     ROUTINES.
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
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
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
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
      INCLUDE 'comindx.f'
C
C     ------------------------------------------------------------------
C*          1.  CHECK FOR 'E E E'. 'EEE' IS ACCEPTED EVEN
C               THOUGH THIS MAY BE  AMBIGUOUS WITH A SYNOP '333' GROUP IN
C               LETTER SHIFT. FREQUENTLY  ONLY 1 OR 2 'E'S MAY BE USED.
C               THESE ALSO CATERED FOR.
cps   This means that all groups containing an 'E' in letter shift is spaced 
cps   out, unless there are no more figures ('CALL NEXTFIG') in the message. 
cps   Actually, this usually means that a letter shifted messages is not 
cps   altered by REMEEE, even if it contains EE corrections. Not so easy
cps   to change that.
 100  CONTINUE
C
C     SKIP PAST SHIP'S CALL SIGN AND LOCATE 'E' IF ANY EXISTS.
C
      I = IPT + 4
      N = 69
      CALL NEXTVAL(I,N,IEQ)
      IF(I .GT. IEQ) RETURN
cpsfeb98   Don't want 'ICE' to be spaced out!
      if (char(kchar(i-1)) .eq. 'C' .and.
     *    char(kchar(i-2)) .eq. 'I' ) return
cps
C
C     'E' CHARACTER FOUND. REPLACE 'E' AND ANY FOLLOWING 'E'S BY SPACES
C     E.G.  40118 7012EE 40118 70500  BECOMES
C           40118 7012   40118 70500 .
C
      K = I
      CALL NEXTFIG(K,IEQ)
      IF(K .GE. IEQ) RETURN
C
            DO 101 J=I,K-1
               IF((KCHAR(J) .NE. 10) .AND. (KCHAR(J) .NE. 13))
     1             KCHAR(J) = 32
101         CONTINUE
C
C     SET POINTER TO CHARACTER BEFORE THE 'E'. CHANGE
C     THIS CHARACTER TO A 'SPACE'.
C       E.G.  40118 7012   40118 70500  BECOMES
C             40118 701    40118 70500 .
C
      N = I - 1
      IF((KCHAR(N) .NE. 10) .AND. (KCHAR(N) .NE. 13))
     1    KCHAR(N) = 32
C
C     SCANNING BACKWARDS REPLACE CHARACTERS BY 'SPACE'
C     UNTIL A 'SPACE' CHARACTER IS ENCOUNTERED.
C        E.G.  40118 701    40118 70500  BECOMES
C              40118        40118 70500 .
C
      DO 102 I=N-1,IPT,-1
          IF(KCHAR(I) .EQ. 32) GO TO 300
          IF((KCHAR(I) .NE. 10) .AND. (KCHAR(I) .NE. 13))
     1        KCHAR(I) = 32
  102 CONTINUE
C
C     NO CHARACTER FOUND
C
      GO TO 100
C
C     ---------------------------------------------------------------------
C*           3. 'I' NOW POINTS TO THE 'SPACE' AFTER LAST FIGURE OF THE GROUP
C               BEFORE THE SERIES OF SPACES AND 'K' POINTS TO FIRST FIGURE
C               OF FOLLOWING GROUP. IF THESE GROUPS ARE THE SAME REMOVE
C               ONE GROUP (SECOND).
C               E.G.  40118        40118 70500  BECOMES
C                     40118              70500 .
C
 300  CONTINUE
C
      I = I - 5
      IF((KCHAR(I)   .EQ. KCHAR(K))   .AND.
     1   (KCHAR(I+1) .EQ. KCHAR(K+1)) .AND.
     2   (KCHAR(I+2) .EQ. KCHAR(K+2)) .AND.
     3   (KCHAR(I+3) .EQ. KCHAR(K+3)) .AND.
     4   (KCHAR(I+4) .EQ. KCHAR(K+4)))
     5         THEN
                   N = K + 4
                   DO 301 I=K,N
                       KCHAR(I) = 32
  301              CONTINUE
               END IF
C
C
C*           4.  SOMETIMES MORE THAN 1 GROUP HAS TO BE DELETED
C                E.G.  40118 59623 7012EE 40118 70500 .
C                THIS WILL NOW HAVE BECOME
C                40118 59623        40118         AND POINTERS ARE
C                      I            K
C                40118 59623 NEED TO BE REMOVED.
 400  CONTINUE
C
      I = I - 6
      N = K - 1
      IF((KCHAR(I)   .EQ. KCHAR(K))   .AND.
     1   (KCHAR(I+1) .EQ. KCHAR(K+1)) .AND.
     2   (KCHAR(I+2) .EQ. KCHAR(K+2)) .AND.
     3   (KCHAR(I+3) .EQ. KCHAR(K+3)) .AND.
     4   (KCHAR(I+4) .EQ. KCHAR(K+4)))
     5         THEN
                   DO 401 J=I,N
                       KCHAR(J) = 32
  401              CONTINUE
               END IF
C
C     GO BACK TO BEGINNING OF SUBROUTINE TO FIND OUT
C     IF THERE ARE MORE 'E'-CORRECTIONS.
C
      GO TO 100
C
      END
