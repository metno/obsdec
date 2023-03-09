      SUBROUTINE EXTINT ( I,N,K )
C
C**** *EXTINT*
C
C
C     PURPOSE.
C     --------
C         EXTRACTS N FIGURES FROM ARRAY 'KCHAR' , STARTING AT
C         WORD I , CONVERTS CHARACTERS TO INTEGER AND PLACES
C         IN WORD K OF 'KINT' .
C
C         INPUT     : I - POINTS TO FIRST CHARACTER TO BE EXTRACTED.
C                     N - NUMBER OF CCITT NO. 5 CHARACTERS TO BE EXTRACTED.
C
C         OUTPUT    : I - POINTS TO CHARACTER AFTER THE LAST ONE EXTRACTED.
C                         MADE NEGATIVE IF A 'SEPARATOR' IS FOUND IN THE
C                         CHARACTERS BEING EXTRACTED.
C                         IF NEGATIVE , THE ABSOLUTE VALUE IS POSITION OF
C                         'SEPARATOR' ENCOUNTERED .
C                     K - INTEGER VALUE IN WORD K OF 'KINT'. MISSING DATA
C                         VALUE INSERTED IF '/' OR NON DIGIT ENCOUNTERED.
cps                       But some letters are converted into figures!
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXTINT ( I,N,K )*
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
C         *XXXX* *XXXXXXX(XXXX)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          J. HENNESSY
C
C     MODIFICATIONS.
C     --------------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'
C
      DIMENSION ILET(11),IFIG(11)
C
      DATA (ILET(J),J=1,11) /
C        E    O,   P,   Q,   R,   T,   U,   W,   X,   Y,   I.
     C  69,  79,  80,  81,  82,  84,  85,  87,  88,  89,  73 /
C
      DATA (IFIG(J),J=1,11) /
C        3    9,   0,   1,   4,   5,   7,   2,   /,   6,   8.
     C  51,  57,  48,  49,  52,  53,  55,  50,  47,  54,  56 /
C
C
C     ------------------------------------------------------------------
C
C*          1.   EXTRACT N FIGURES.
C                ------------------
 100  CONTINUE
C
      IAC = 0
      IA = IABS(I)
      IB = IA + N - 1
C
C*           1.1 STORE KCHAR(J) SO THAT IT WONT BE ALTERED IN THE SUBROUTINE.
C                ------------------------------------------------------------
 110  CONTINUE
C
      DO 111 J=IA,IB
C
      KTEMP=KCHAR(J)
      KTEMP=KCHAR(J)
cpssep03      KAR = KCHAR(J) .AND. 127
      KAR = IAND(KCHAR(J),127)
C
C
C          CHECK FOR SPACE,LINE FEED AND CARRIAGE RETURN CHARACTER .
C
           IF ( KAR .EQ. 32 .OR. KAR .EQ. 10 .OR. KAR .EQ. 13)
     C                           THEN
                                      I = - J
                                      KINT(K) = MINDIC
                                      RETURN
                                 END IF
C
C          CHECK FOR / CHARACTER .
C
           IF ( KAR.EQ.47 ) THEN
                                     I = IB + 1
                                     KINT(K) = MINDIC
                                     RETURN
                                  END IF
C
C          IF LETTER ENCOUNTERED CONVERT TO FIGURE USING THE
C          CCITT NO.2 LETTER/FIGURE RELATIONSHIP.
C
           IF ( KAR .LT. 48 .OR. KAR .GT. 57 )
     C                      THEN
                                  DO 112 JA=1,11
                                       IF ( KAR .EQ. ILET(JA))
     C                                       KAR = IFIG(JA)
  112                             CONTINUE
                           END IF
C
           IF ( KAR .GE. 48 .AND. KAR .LE. 57 )
     C                             THEN
cpssep03                                    IAC = (IAC + (KAR.AND.15))*10
                                    IAC = (IAC + IAND(KAR,15))*10
                                   ELSE
                                      KINT(K) = MINDIC
                                      I = IB + 1
                                      RETURN
                                   END IF
C
C
C
      KCHAR(J)=KTEMP
C
  111 CONTINUE
C
      KINT(K) = IAC / 10
      I = J
C
C
      RETURN
      END
