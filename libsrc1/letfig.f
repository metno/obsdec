      SUBROUTINE LETFIG(K)
C
C*****
C*
C*    NAME      : LETFIG
C*
C*    FUNCTION  : IF K IS NOT FIGURE CONVERT IT USING THE
C*                CCITT NO.2 LETTER/FIGURE RELATION SHIP.
C*
C*    INPUT     : K - KHARACTER VALUE TO BE CONVERTED
C*
C*    OUTPUT    : K - CONVERTED TO FIGURE IF IT WAS EITHER
C*                    E,O,P,Q,R,T,U,W,X,Y OR I, OTHERWISE
C*                    K REMAINS UNCHANGED.
C*
C*****
C
C
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
C
C          IF LETTER ENCOUNTERED CONVERT TO FIGURE USING THE
C          CCITT NO.2 LETTER/FIGURE RELATIONSHIP.
C
           IF ( K.LT.48.OR.K.GT.57 )
     C                      THEN
                                  DO 100 JA=1,11
                                       IF ( K.EQ.ILET(JA))
     C                                       K = IFIG(JA)
  100                             CONTINUE
                           END IF
C
C
      RETURN
      END
