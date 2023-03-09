      SUBROUTINE PROCTXT ( IERR )
C
C**** *PROCTXT*
C
C
C     PURPOSE.
C     --------
C          CHECKS WHETHER BULLETIN CONTAINS USEFUL DATA .
C          THE FOLLOWING BULLETINS ARE CONSIDERED TO CONTAIN
C          NO USEFUL DATA.
C                 1. TEXT OF 'NIL' , OR VARIANTS OF THIS.
C                 2. TEXT OF 'NO DATA AVAILABLE'.
C                 3. TEXT OF 'NO REPORTS AVAILABLE'.
C                      1. - 3.  ARE DETERMINED SIMPLY BY CHECKING
C                      THE LENGTH OF THE TEXT . IF IT IS LESS
C                      THAN 26 THERE CANT BE ANY USEFUL DATA IN IT
C                 4. UK AND GERMAN DOMESTIC BULLETINS WHICH DO
C                    NOT CONFORM TO WMO CODES.
C
C          INPUT     : BULLETIN IN ARRAY 'KCHAR' ,
C                      ONE CHARACTER PER WORD.
C
C          OUTPUT    : KERR = 0 INDICATES BULLETIN CONTENTS REQUIRED.
C                           = 1 MEANS TEXT OF 'NIL' ETC.
C                           = 2 UK OR GERMAN DOMESTIC BULLETIN.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *PROCTXT(IERR)*
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
C         *CALL* *NEXTLET(I,J)*
C         *CALL* *PRTBULL(I,J)*
C         *CALL* *SAVBULL(IERR)*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/08/88.
C          J. HENNESSY         *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      implicit none
      integer ierr
      integer ile,n,n1,n2
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'   ! kchar,kerr
      INCLUDE 'comindx.f'   ! igs,imi,iah,jah,ipt
      INCLUDE 'comstat.f'   ! numberr
C
C     ------------------------------------------------------------------
C
C*          1.   CLEAR ERROR INDICATOR.
C                ----------------------
C
      KERR= 0
C
C
C*          1.1  CHECK IF BULLETIN IS TOO SHORT I.E. "NIL" BULLETIN.
C                ---------------------------------------------------
C
      ILE = IGS - IMI
      IF (ILE .LT. 26) THEN
         KERR = 7
         NUMBERR(7) = NUMBERR(7) + 1
cps   'Setting the parity bit':
cps   The pattern below appears repeatedly: if a character is
cps   considered erronous, it is marked by extending the word with 
cps   an extra '1' bit (128 = 2**6, while kchar(i) < 128 for all i 
cps   unless the following command is executed). 
         KCHAR(IGS) = IOR(KCHAR(IGS),128)
      END IF
C
C
C*          1.2  CHECK IF THE BULLETIN IS DOMESTIC FROM UK OR GERMANY.
C                -----------------------------------------------------
C                THIS IS DONE BY CHECKING IF THE BULLETIN HAS
C                --------------------------------------------
C                CCCC OF (EG--,ED--) OTHER THAN (EGRR,EDZW).
C                -------------------------------------------
C
      IPT=IAH+4
      CALL NEXTLET(IPT,JAH)
C
C     IF BULLETINS HAVE 'CCCC' NOT 'ED--' OR 'EG--' , RETURN.
C
      IF ( KCHAR(IPT).NE.69 ) RETURN
      IF ( KCHAR(IPT+1).NE.71.AND.KCHAR(IPT+1).NE.68 ) RETURN
C
C        FIRST UK
C
      IF (KCHAR(IPT+1).EQ.71) THEN
         IF (KCHAR(IPT+2) .NE. 82 .OR.
     1        KCHAR(IPT+3) .NE. 82)
     2        KERR = 8
      ELSE
C
C        THEN GERMANY
C
         IF(KCHAR(IPT+2) .NE. 90 .OR.
     1        KCHAR(IPT+3) .NE. 87)
     2        KERR = 8
      END IF
C
C     MARK CCCC GROUP IF REQUIRED.
C
      IF ( KERR.EQ.8 ) THEN
         KCHAR(IPT+4) = IOR(KCHAR(IPT+4),128)
         NUMBERR(8) = NUMBERR(8) + 1
      END IF
C
C
      IF ( KERR.EQ.2 ) THEN
C     Print bulletine to error file
      print*,'savbull called from proctxt, kerr=',kerr
         CALL SAVBULL(IERR)
      END IF

      RETURN
      END
