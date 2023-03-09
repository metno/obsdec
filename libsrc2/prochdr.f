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
      implicit none
      integer ierr,i1,iret,correction_num
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'   ! kdec,mindic,kerr,it1
      INCLUDE 'combuff.f'
      INCLUDE 'comindx.f'   ! isl,jsl,iah,jah,imi,jmi,igs,ipt
      INCLUDE 'comstat.f'   ! numberr
C
C     ------------------------------------------------------------------
C
C*          1.   CLEAR ERROR INDICATOR AND SET REPORT HEADER AREA
C                ------------------------------------------------
C                TO MISSING DATA INDICATOR.
C                --------------------------
      KERR=0
C
      DO I1=1,24
         KDEC(I1)= MINDIC
      END DO
C
C*          1.1  FLAG FIELDS SET TO ZERO.
C                ------------------------
      KDEC(13)=0
      KDEC(15)=0
      KDEC(21)=0
C
C
C*          2.  LOCATE BEGINNING AND END OF 'STARTING LINE',
C               --------------------------------------------
C               'ABBREVIATED HEADER' AND 'MIMIMJMJ LINE'.
C               -----------------------------------------
C
C  Starting line = SOH, csn,CR,CR,LF (csn = channel sequence number - 3 digits)
      ISL = 1
      CALL NEXTPRT ( ISL,IGS )  ! ISL ought to be 3
      JSL = ISL
      CALL NEXTEND ( JSL,IGS )  ! usually JSL = 6
      IAH = JSL
      CALL NEXTPRT ( IAH,IGS )  ! usually IAH = 9
      JAH =IAH
      CALL NEXTEND ( JAH,IGS )  ! usually JAH is 27 (or 31 if BBB is present)
      IMI = JAH 
      CALL NEXTPRT ( IMI,IGS )  ! usually IMI is 30 (or 34 if BBB is present)
      JMI = IMI
      CALL NEXTEND ( JMI,IGS )
ctest      print*,'ISL,JSL,IAH,JAH,IMI,JMI,IGS=',ISL,JSL,IAH,JAH,IMI,JMI,IGS
C
C*          2.1 IF THESE 3 LINES CANNOT BE LOCATED, BULLETIN CONSISTS
C               -----------------------------------------------------
C               OF LESS THAN 3 LINES.
C               ---------------------
C
      IF ( JMI.GT.IGS ) THEN
C     Set error number and mark error
         KERR = 1
         KCHAR(IGS) = IOR(KCHAR(IGS),128)
         NUMBERR(1) = NUMBERR(1) + 1
         GO TO 300
      END IF
C
C*          2.2  BULLETIN CANNOT BE IDENTIFIED FROM 'TT'.
C                ----------------------------------------
C
      IF ( IT1.EQ.27 ) THEN    ! IT1 was set by PROCRFB, 27 indicating error
C     Set error number and mark error
         KERR = 2
         KCHAR(IAH+2) = IOR(KCHAR(IAH+2),128)
         NUMBERR(2) = NUMBERR(2) + 1
         GO TO 300
      END IF
C
C*          2.3  NO CHECKS ARE MADE ON TTAAII OR CCCC GROUPS.
C                --------------------------------------------
Clil 28.3.96 Testing on CCCC, result put in KDEC(14).
      ipt = iah + 6
      call nextprt(ipt,igs)
c     KDEC(14) = 0 if origin of report is FGGE.
c     FGGE - what is that? We did not receive anything from CCCC=FGGE 03.07.2002
c                    1  "   "     "    "    " BRACKNELL (EGRR).
c                    2  "   "     "    "    " OFFENBACH (EDZW).
c                    3  "   "     "    "    " TOULOUSE (LFPW).
C
      IF( (KCHAR(IPT).EQ.70).AND.(KCHAR(IPT+1).EQ.71).AND.            
     *   (KCHAR(IPT+2).EQ.71).AND.(KCHAR(IPT+3).EQ.69) )THEN
C
         KDEC(14)=0    ! FGGE
C
      ELSEIF( (KCHAR(IPT).EQ.69).AND.(KCHAR(IPT+1).EQ.70).AND.
     *        (KCHAR(IPT+2).EQ.82).AND.(KCHAR(IPT+3).EQ.82) )THEN
C
         KDEC(14)=1    ! EGRR
C
      ELSEIF( (KCHAR(IPT).EQ.69).AND.(KCHAR(IPT+1).EQ.68).AND.
     *        (KCHAR(IPT+2).EQ.90).AND.(KCHAR(IPT+3).EQ.87) )THEN
C
         KDEC(14)=2    ! EDZW
C
      ELSEIF( (KCHAR(IPT).EQ.76).AND.(KCHAR(IPT+1).EQ.70).AND.
     *        (KCHAR(IPT+2).EQ.80).AND.(KCHAR(IPT+3).EQ.87) )THEN
C
         KDEC(14)=3    ! LFPW
C
      ELSE                                            
C
         KDEC(14)=MINDIC
C
      ENDIF
C
clil
C
C*          2.4  LOCATE AND DECODE 'YYGGGG' GROUP.  (DTG group i abbrev. header)
C                ---------------------------------
C
C     SCAN 'KCHAR' FOR FIRST FIGURE AFTER 'II' FIGURES.
C
      IPT = IAH + 6
      CALL NEXTFIG ( IPT,JAH )
      IF ( IPT.GE.JAH ) THEN
         KERR = 5
      ELSE
C
C     EXTRACT YY,GG AND GG AND CONVERT TO INTEGERS
C     IN WORDS 10-12 OF 'KINT'.
C
         CALL EXTGRP( IPT,2,2,2,0,0,10,IRET )
         IPT = IABS(IPT)
C
C     TEST VALIDITY OF YY,GG AND GG.
C     THIS TEST MAKES CHECKING RETURN CODE 'IRET' UNNECESSARY.
C
         IF ( KINT(10).LT.1.OR.KINT(10).GT.31 ) THEN
            KERR = 5
            KINT(10) = MINDIC
         END IF
         IF ( KINT(11).LT.0.OR.KINT(11).GT.23 ) THEN
            KERR = 5
            KINT(11) = MINDIC
         END IF
         IF ( KINT(12).LT.0.OR.KINT(12).GT.59 ) THEN
            KERR = 5
            KINT(12) = MINDIC
         END IF
C
C     COPY TO DECODED REPORT HEADER AREA
C     AND RESET WORDS IN 'KINT'.
C
         DO I1=10,12
            KDEC(I1) = KINT(I1)
            KINT(I1) = MINDIC
         END DO
C
      END IF
C
C     MARK ERROR IN YYGGGG GROUP.
C
      IF ( KERR.EQ.5 ) THEN
         KCHAR(IPT) = IOR(KCHAR(IPT),128)
         NUMBERR(5) = NUMBERR(5) + 1
      END IF
C
C*          2.5 THE ONLY CHECK ON 'BBB' IS FOR 'COR' AND 'CCx'.
C               -----------------------------------------------
C
C     FIND NEXT LETTER IN ABBREVIATED HEADER LINE AND CHECK IF 'C' (67)
C
      CALL NEXTLET ( IPT,JAH )
C
      IF ( KCHAR(IPT).EQ.67.AND.KCHAR(IPT+1).EQ.79) THEN   ! 'CO'
         KDEC(21)=1
      ELSE IF (KCHAR(IPT).EQ.67 .AND. KCHAR(IPT+1).EQ.67) THEN ! 'CC'
         CORRECTION_NUM = KCHAR(IPT+2) - 64     ! 1 for 'A', 2 for 'B' etc
         IF (CORRECTION_NUM.GT.0 .AND. CORRECTION_NUM.LT.27) THEN
            KDEC(21) = CORRECTION_NUM
         ELSE
C     Set error number and mark error
            KERR = 2
            KCHAR(JAH) = IOR(KCHAR(JAH),128)
            NUMBERR(2) = NUMBERR(2) + 1
            GO TO 300
         END IF
      END IF
C
C*     RETURN IF NO ERRORS FOUND.
C      --------------------------
C
      IF ( KERR.EQ.0 ) RETURN

 300  CONTINUE
C
C*          3. HANDLE ERROR BULLETIN.
C              ----------------------
C
      CALL SAVBULL(IERR)
C
C     Only errors 1 and 2 are fatal to decoding, so clear
C     error indicator before returning.
      IF ( KERR.GT.2 ) KERR = 0
C
      RETURN
      END
