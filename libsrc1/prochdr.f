      SUBROUTINE PROCHDR ( IERR )
C
C**** *PROCHDR
C
C
C     PURPOSE.
C     --------
C         DEODE BULLETIN HEADER AND INSERT REQUIRED PARAMETERS
C         IN DECODED REPORT HEADER.
C
C         LOCATES BEGINNING  AND END OF ABBREVIATED HEADER AND
C         'MIMIMJMJ' LINES.
C
C         INPUT     : BULLETIN IN KCHAR(1) - KCHAR(IGS)
C
C                     BULLETIN RECORD HEADER IN KINT(1) - KINT(5)
cps       ????  kint(1) - kint(5) is not used at all in PROCHDR
cps             and is also not set before PROCHDR is called in nix_syno.f
C
C                     'IT1' = 27 INDICATING BULLETIN HAS NOT BEEN
C                            IDENTIFIED FROM 'TT' OF ABBREVIATED HEADER.
C
C         OUTPUT    : KDEC(10) = DAY OF MONTH ( INTEGER ) . YY
C                     KDEC(11) = TIME OF BULLETIN - HOURS ( INTEGER ) . GG
C                     KDEC(12) = TIME OF BULLETIN - MINS ( INTEGER ) . gg
cps                   In all the bulletines I have seen, gg=00. kdec(12) is
cps                   anyway not used in nix_syno.f (hardcoding it to a
cps                   value different from 0 has no effect on output)
C
C                     KDEC(14) = 0 IF ORIGIN OF REPORT IS FGGE.
C                                1  "   "     "    "    " BRACKNELL.
C                                2  "   "     "    "    " OFFENBACH.
C
C                     KDEC(20) =1 NIL
C                     KDEC(21) =1 IF BULLETIN IS 'COR' , OTHERWISE
C                                    = 0 .
C
C                     KDEC(21) = 1 IF BULLERIN IS 'CCA'
C                     KDEC(21) = 2 IF BULLERIN IS 'CCB'
C                     KDEC(21) = 3 IF BULLERIN IS 'CCC'
C                     KDEC(21) = 4 IF BULLERIN IS 'CCD'
C                     .
C                     .
C
C                     KDEC(18) = DATE OF BULLETIN ARRIVAL ( ON VAX )
C                     KDEC(19) = TIME  "    "        "      "     "
C
C                     IAH =    "      " BEGINNING OF 'ABBREVIATED HEADER'
C                     JAH =    "      "     END   "       "         "
C
C                     IMI =    "      " BEGINNING OF 'MIMIMJMJ' LINE.
C                     JMI =    "      "     END   "       "       "
C
C                     KERR = 0 IF NO ERROR FATAL TO DECODING ENCOUNTERED.
C                         = 1 IF BULLETIN HAS LESS THAN 3 LINES.
C                         = 2 IF BULLETIN IS NOT RECOGNISED.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCHDR( IERR )*
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
C         *CALL* *NEXTPRT(I,J)*
C         *CALL* *NEXTEND(I,J)*
C         *CALL* *NEXTFIG(I,J)*
C         *CALL* *EXTGRP (I,N1,N2,N3,N4,N5,N,IRET)*
C         *CALL* *NEXTLET(I,J)*
C         *CALL* *SAVBULL(IERR )*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C          J. HENNESSY         *ECMWF*
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
      INCLUDE 'combuff.f'
      INCLUDE 'comindx.f'
      INCLUDE 'comstat.f'
C
      DIMENSION ILST(26)
      DATA ILST/65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
     1          81,82,83,84,85,86,87,88,89,90/
C     ------------------------------------------------------------------
C
C*          1.   CLEAR ERROR INDICATOR AND SET REPORT HEADER AREA
C                ------------------------------------------------
C                TO MISSING DATA INDICATOR.
C                --------------------------
 100  CONTINUE
C
      KERR=0
C
      DO 101 I=1,24
C
      KDEC(I)= MINDIC
C
 101  CONTINUE
C
C*          1.1  FLAG FIELDS SET TO ZERO.
C                ------------------------
 110  CONTINUE
C
      KDEC(13)=0
      KDEC(15)=0
      KDEC(21)=0
C
C*          2.  LOCATE BEGINNING AND END OF 'STARTING LINE' ,
C               ---------------------------------------------
C               'ABBREVIATED HEADER' AND 'MIMIMJMJ LINE ' .
C               -------------------------------------------
C
C  Starting line = SOH,s,e,e,CR,LF (seq is some form of sequence no.??
      ISL = 1
      CALL NEXTPRT ( ISL,IGS )
      JSL = ISL
      CALL NEXTEND ( JSL,IGS )
      IAH = JSL
      CALL NEXTPRT ( IAH,IGS ) 
      JAH =IAH
      CALL NEXTEND ( JAH,IGS )
      IMI = JAH
      CALL NEXTPRT ( IMI,IGS )
      JMI = IMI
      CALL NEXTEND ( JMI,IGS )
C
C*          2.1 IF THESE 3 LINES CANNOT BE LOCATED , BULLETIN CONSISTS
C               ------------------------------------------------------
C               OF LESS THAN 3 LINES.
C               ---------------------
 210  CONTINUE
C
      IF ( JMI.GT.IGS ) THEN
C
C                           SET ERROR NUMBER AND MARK ERROR.
C
                            KERR = 1
cpssep03                            KCHAR(IGS)=KCHAR(IGS).OR.128
                            KCHAR(IGS)=IOR(KCHAR(IGS),128)
                            NUMBERR(1) = NUMBERR(1) + 1
                            GO TO 300
                        END IF
C
C
C
C*          2.2  BULLETIN CANNOT BE IDENTIFIED FROM 'TT' .
C                -----------------------------------------
 220  CONTINUE
C
      IF ( IT1.EQ.27 ) THEN
C
C                          SET ERROR NUMBER AND MARK ERROR.
C
                           KERR = 2
cpssep03                           KCHAR(IAH+2)=KCHAR(IAH+2).OR.128
                           KCHAR(IAH+2)=IOR(KCHAR(IAH+2),128)
                           NUMBERR(2) = NUMBERR(2) + 1
                           GO TO 300
                       END IF
C
C
C*          2.3  NO CHECKS ARE MADE ON TTAAII OR CCCC GROUPS.
C                --------------------------------------------
 230  CONTINUE
C
clil
cNBNBNBNB!
C 28.3.96
C Testing on CCCC, result put in KDEC(14).
C
      ipt=iah+6
      call nextprt(ipt,igs)
c      print*,'IPT,KCHAR',ipt,(kchar(i),i=ipt,ipt+10)
c         KDEC(14) = 0 IF ORIGIN OF REPORT IS FGGE.
cps FGGE - what is that? We did not receive anything from CCCC=FGGE 03.07.2002
c                    1  "   "     "    "    " BRACKNELL (EGRR).
c                    2  "   "     "    "    " OFFENBACH (EDZW).
c                    3  "   "     "    "    " TOULOUSE (LFPW).
C
      IF( (KCHAR(IPT).EQ.70).AND.(KCHAR(IPT+1).EQ.71).AND.            
     c   (KCHAR(IPT+2).EQ.71).AND.(KCHAR(IPT+3).EQ.69) )THEN
C
         KDEC(14)=0
C
      ELSEIF( (KCHAR(IPT).EQ.69).AND.(KCHAR(IPT+1).EQ.70).AND.
     c        (KCHAR(IPT+2).EQ.82).AND.(KCHAR(IPT+3).EQ.82) )THEN
C
         KDEC(14)=1
C
cpsjul02      ELSEIF( (KCHAR(IPT).EQ.69).AND.(KCHAR(IPT+1).EQ.78).AND.
      ELSEIF( (KCHAR(IPT).EQ.69).AND.(KCHAR(IPT+1).EQ.68).AND.
     c        (KCHAR(IPT+2).EQ.90).AND.(KCHAR(IPT+3).EQ.87) )THEN
C
         KDEC(14)=2
C
      ELSEIF( (KCHAR(IPT).EQ.76).AND.(KCHAR(IPT+1).EQ.70).AND.
     c        (KCHAR(IPT+2).EQ.80).AND.(KCHAR(IPT+3).EQ.87) )THEN
C
         KDEC(14)=3
C
      ELSE                                            
C
         KDEC(14)=MINDIC
C
      ENDIF
C
clil
C
C
C*          2.4  LOCATE AND DECODE 'YYGGGG' GROUP .
C                ----------------------------------
 240  CONTINUE
C
C     SCAN 'KCHAR' FOR FIRST FIGURE AFTER 'II' FIGURES.
C
      IPT = IAH + 6
      CALL NEXTFIG ( IPT,JAH )
      IF ( IPT.GE.JAH ) THEN
                          KERR = 5
                        ELSE
C
C                         EXTRACT YY,GG AND GG AND CONVERT TO INTEGERS
C                         IN WORDS 10-12 OF 'KINT' .
C
                          CALL EXTGRP( IPT,2,2,2,0,0,10,IRET )
                          IPT = IABS(IPT)
C
C                         TEST VALIDITY OF YY,GG AND GG.
C                         THIS TEST MAKES CHECKING RETURN CODE 'IRET'
C                         UNNECESSARY.
C
                          IF ( KINT(10).LT.1.OR.KINT(10).GT.31 )
     C                             THEN
                                        KERR = 5
                                        KINT(10) = MINDIC
                                   END IF
C
                          IF ( KINT(11).LT.0.OR.KINT(11).GT.23 )
     C                             THEN
                                        KERR = 5
                                        KINT(11) = MINDIC
                                   END IF
C
                          IF ( KINT(12).LT.0.OR.KINT(12).GT.59 )
     C                             THEN
                                        KERR = 5
                                        KINT(12) = MINDIC
                                   END IF
C
C
C                                  COPY TO DECODED REPORT HEADER AREA
C                                  AND RESET WORDS IN 'KINT'.
C
                                   DO 241 I=10,12
                                          KDEC(I) = KINT(I)
                                          KINT(I) = MINDIC
  241                              CONTINUE
C
                        END IF
C
C     MARK ERROR IN YYGGGG GROUP.
C
      IF ( KERR.EQ.5 ) THEN
cpssep03                              KCHAR(IPT)=KCHAR(IPT).OR.128
                              KCHAR(IPT)=IOR(KCHAR(IPT),128)
                              NUMBERR(5) = NUMBERR(5) + 1
                          END IF
C
C
C
C
C*          2.5 THE ONLY CHECK ON 'BBB' IS FOR 'COR'.
C               -------------------------------------
 250  CONTINUE
C
C     FIND NEXT LETTER IN ABBREVIATED HEADER LINE AND CHECK IF 'C' (67)
C
C      CALL PRTBULL(1,IGS)
      CALL NEXTLET ( IPT,JAH )
C
      IF ( KCHAR(IPT).EQ.67.AND.KCHAR(IPT+1).EQ.79) THEN
                                                       KDEC(21)=1
                                                       GO TO 260
                                                    END IF
      IF ( KCHAR(IPT).EQ.67.AND.KCHAR(IPT+1).EQ.67)
     1     THEN
              DO 251 IJ=1,26
              IF(KCHAR(IPT+2).EQ.ILST(IJ)) THEN
                                              KDEC(21)=IJ
                                              GO TO 260
                                           END IF
C
 251          CONTINUE
C
           END IF
C
C
C
C*          2.6 INSERT ORIGIN AND DATE/TIME OF ARRIVAL OF BULLETIN.
C               ---------------------------------------------------
 260  CONTINUE
C
C     ORIGIN IS DERIVED FROM KEY . BRACKNELL FILE
C     NAMES START WITH 'B' AND OFFENBACH WITH 'C'. THIS FILENAME IS
C     IN ASCCI CODE       ( 'B' =66 , 'C' = 67)
C
clil      KDEC(14) = MINDIC    !    ORIGIN WILL BE DEFINED FROM KEY.
C
C
      KDEC(18) = MINDIC               ! DATE AND TIME OF ARRIVAL
      KDEC(19) = MINDIC               !
C
C
C
C*          2.7 TREAT ERROR BULLETIN IN ACCORDANCE WITH DEFINED OPTIONS.
C               --------------------------------------------------------
 270  CONTINUE
C
C     RETURN IF NO ERRORS FOUND.
C
      IF ( KERR.EQ.0 ) RETURN
C
C
C*          3. HANDLE ERROR BULLETIN.
C              ----------------------
 300  CONTINUE
C
      N = KERR-1
cpssep03      N1 =JISHFT(IOPTS(677),-N).AND.1
cpsjul10      N1 =IAND(JISHFT(IOPTS(677),-N),1)
cpsjul10      N2 =IAND(JISHFT(IOPTS(678),-N),1)
      N1 =IAND(ISHFT(IOPTS(677),-N),1)
      N2 =IAND(ISHFT(IOPTS(678),-N),1)
C
C*          3.1 PRINT BULLETIN IF REQUIRED.
C               ---------------------------
 310  CONTINUE
C
      IF ( N1.EQ.1 ) THEN
                         WRITE (*,9900) KERR
                         CALL PRTBULL ( 1,IGS)
                     END IF
C
C*          3.2 WRITE BULLETIN TO ERROR FILE IF REQUIRED.
C               -----------------------------------------
 320  CONTINUE
C
      IF ( N2.EQ.1 ) CALL SAVBULL(IERR)
C
C*          3.3  ONLY ERRORS 1 AND 2 ARE FATAL TO DECODING , SO CLEAR
C                -----------------------------------------------------
C                ERROR INDICATOR BEFORE RETURNING.
C                ---------------------------------
 330  CONTINUE

      IF ( KERR.GT.2 ) KERR = 0
C
C
      RETURN
C
C
 9900 FORMAT (1H ,'BULLETIN ERROR NUMBER ',I2.2)
C
C
      END
