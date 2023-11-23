      SUBROUTINE INITVAR ( IERR )
C
C**** *INITVAR*
C
C
C     PURPOSE.
C     --------
C         DECLARATION OF COMMON AREAS AND INITIALISATION OF
C         VARIABLES.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *INITVAR(IERR)*
C
C           IERR - SET TO 0, but returns ierr > 0 if file handling errors
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
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C

      IMPLICIT NONE
      INTEGER IERR,J

      INCLUDE 'parameter.f'
      INCLUDE 'combuff.f'  ! ieof,impsta
      INCLUDE 'comwork.f'  ! mindic
      INCLUDE 'comstat.f'  ! numbull,numberr,numrerr,noer,numrep
      INCLUDE 'comwt.f'    ! nsub
      INCLUDE 'commsys.f'  ! msys
C
C     ------------------------------------------------------------------
C           1.   INITIALISE MISSING DATA INDICATOR.
C                ----------------------------------
C
      DATA MINDIC / O'17777777777' /
      NSUB=0
C     -------------------------------------------------------------------
C*          2.   INITIALIZE DEFAULT VALUES FOR ERROR HANDLING OPTIONS.
C                ------------------------------------------------------
C
C                 DEFAULT FOR HANDLING REPORT ERRORS IS TO WRITE
C                 ALL REPORTS WITH ERRORS TO THE ERROR FILE.
C
C                 DEFAULT BULLETIN HANDLING IS TO DISCARD 'NIL' BULLETINS,
C                 SHORT ( LESS THAN 3 LINES ) BULLETINS AND NATIONAL
C                 BULLETINS FROM UK AND FRG.
C
cpsoct06 Removed all use of IOPTS
C
C     -------------------------------------------------------------------
C*          3.    CLEAR COUNTERS.
C                 ---------------
C
C                 NUMBULL : NUMBER OF BULLETINS HANDLED BY DECODING RUN.
C                 NUMBERR : NUMBERS OF BULLETIN ERRORS.
C                 NOER    : NUMBERS OF REPORT ERRORS.
C                 NUMREP  : NUMBERS OF REPORTS HANDLED BY DECODING RUN.

      DATA NUMBULL / 0 /
      DATA NUMREP /26*0/
      DATA NUMBERR / 8*0 /
      DATA NOER /1560*0/
      DATA NUMRERR /26*0/
      DATA IMPSTA/2000*0/

C     -------------------------------------------------------------------
C*          4.   SET END OF FILE INDICATOR.
C                ---------------------------

      DATA IEOF / 0 /
C     -------------------------------------------------------------------
C*          5.   INITIALIZE ERROR INDICATOR.
C                ---------------------------

      IERR = 0

C     --------------------------------------------------------------------
C*          6.   Set up descriptors
C                --------------------

C     Section 3 is common for all metars, so we can set the 
C     descriptors here
      IF (MSYS.EQ.8) THEN    ! metar
         CALL SET_DESCRIPTORS
      END IF

C     --------------------------------------------------------------------
C*          7.   SET UP WMO STATION LIST AND IMPORTANT STATIONS.
C                -----------------------------------------------
C
      IF (MSYS.EQ.2 .OR. MSYS.EQ.3 .OR. MSYS.EQ.14) THEN    ! syno,temp,pilo
         CALL STATION(IERR)
         IF (IERR.NE.0) CALL EXIT(2)
      END IF

C     --------------------------------------------------------------------
C*          8.   READ IN LIST OF SHIP HEIGHTS.
C                -----------------------------

      IF (MSYS.EQ.3) THEN   ! temp (actually needed only for temp ship)
         CALL SHIPHEIGHTS(IERR)
         IF (IERR.NE.0) CALL EXIT(2)
      END IF


C     --------------------------------------------------------------------
C*          9.   SET UP HEIGHT AND PRESSURE OF STANDARD ATMOSPHERE.
C                ---------------------------------------------------

      IF (MSYS.EQ.14) THEN   ! pilo
         CALL STDATM(IERR)
         IF (IERR.NE.0) CALL EXIT(2)
      END IF

      RETURN
      END
