      SUBROUTINE EXTVAL ( I,N,IVAL)
C
C**** *EXTVAL*
C
C
C     PURPOSE.
C     --------
C
C         EXTRACTS N FIGURES FROM ARRAY 'KCHAR' , STARTING AT
C         WORD I , CONVERTS CHARACTERS TO INTEGER AND PLACES
C         IN IVAL
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *EXTVAL(I,N,IVAL)*
C
C         INPUT     : I - POINTS TO FIRST CHARACTER TO BE EXTRACTED.
C                     N - NUMBER OF CCITT NO. 5 CHARACTERS TO BE EXTRACTE
C
C         OUTPUT    : IVAL - INTEGER VALUE
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
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. DRAGOSAVAC    *ECMWF*       AUG 1988.
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
C     ------------------------------------------------------------------
C
C*          1.   EXTRACT N FIGURES FROM KCHAR ARRAY.
C                -----------------------------------
 100  CONTINUE
C
      IAC = 0
      IA = IABS(I)
      IB = IA + N - 1
C
      DO 101 J=IA,IB
C
C
C     STORE KCHAR(J) SO THAT IT WONT BE ALTERED IN THE SUBROUTINE
C
      KTEMP=KCHAR(J)
cpssep03      KAR = KCHAR(J) .AND. 127
      KAR = IAND(KCHAR(J),127)
C
C          CHECK FOR SPACE CHARACTER .
C
           IF ( KAR.EQ.32 ) THEN
                                      IVAL = MINDIC
                                      RETURN
                                 END IF
C
C          CHECK FOR / CHARACTER .
C
           IF ( KAR.EQ.47 ) THEN
                                     IVAL = MINDIC
                                     RETURN
                                  END IF
C
C          IF LETTER ENCOUNTERED CONVERT TO FIGURE USING THE
C          CCITT NO.2 LETTER/FIGURE RELATIONSHIP.
C
           IF ( KAR.LT.48.OR.KAR.GT.57 )
     C                      THEN
                                  DO 102 JA=1,11
                                       IF ( KAR.EQ.ILET(JA))
     C                                       KAR = IFIG(JA)
  102                             CONTINUE
                           END IF
C
           IF ( KAR.GE.48.AND.KAR.LE.57 )
     C                             THEN
cpssep03                                    IAC = (IAC + (KAR.AND.15))*10
                                    IAC = (IAC + IAND(KAR,15))*10
                                   ELSE
                                      IVAL = MINDIC
                                      RETURN
                                   END IF
C
C
C
      KCHAR(J)=KTEMP
C
  101 CONTINUE
C
      IVAL = IAC / 10
C
C
      RETURN
      END
