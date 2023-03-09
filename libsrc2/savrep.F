      SUBROUTINE SAVREP ( IHEAD,IERR )
C
C     PURPOSE.
C     --------
C
C         WRITE REPORT IN ERROR TO THE ERROR FILE.
c         with errors marked with '?' on following line.
c         In KCHAR, errors are recognized by having bit 8 set. This
c         bit is set back to 0 after having procesessed the characters.
c         For temps, 'important temps' are marked.
C
C                 IHEAD = 0 , WRITE BULLETIN HEADER AND ERROR REPORT
C                             TO ERROR FILE.
C                       = 1 , WRITE ERROR REPORT ONLY.
C
C                 IERR NOT USED.
C
C                 IHEAD SET TO 1 IF HEADER WRITTEN, OTHERWISE
C                       UNCHANGED.
C
C                 IERR SET TO -1  IF ERROR IN WRITE, OTHERWISE
C                       UNCHANGED.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SAVREP(IHEAD,IERR)*
C
C     EXTERNALS.
C     ----------
C
C         NONE.
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
C          PS jan 07. Removed all code for ihead=2 and tidying up
C

      implicit none
      integer istart,ifinish
      integer ihead,ierr,ios,ip,nn,n1,n2,k,j,jp,i,iunit
      logical otemp   ! true if this report is a temp
      logical oimpt   ! true if this is an important temp
      logical cerror  ! true if the character processed is marked as in error

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'  ! kdec
      INCLUDE 'comindx.f'  ! igs,kpt   what is kpt?
      INCLUDE 'combuff.f'  ! impsta
      INCLUDE 'commdbc.f'  ! cstream
      INCLUDE 'commsys.f'  ! msys

      DIMENSION ISTART(4),IFINISH(4)

      CHARACTER*80 YLINE,YLINEA    ! YLINE is used to print a line in the report,
                                   ! YLINEA is used to print the line with '?' to 
                                   ! mark where the error in line YLINE occurs
      CHARACTER*1 Y63              ! The ascii character '?'
      CHARACTER*14 cimport
      DATA CIMPORT/'important_temp'/
     
      Y63=CHAR(63)                                 ! '?'
C     ------------------------------------------------------------------

      IF ( IHEAD.GT.1 ) RETURN

C     First some preparations for temps
C     ---------------------------------
      OTEMP=.FALSE.
      OIMPT=.FALSE.

      IF(KDEC(4).EQ.35.OR.KDEC(4).EQ.36.OR.
     1   KDEC(4).EQ.135.OR.KDEC(4).EQ.137) OTEMP=.TRUE.

      IF(OTEMP) THEN
         IF(KDEC(4).EQ.36.OR.KDEC(4).EQ.135.OR.KDEC(4).EQ.137) THEN
            OIMPT=.TRUE. 
            GO TO 110
         ELSE
            DO I=1,2000
               IF(KINT(4).EQ.IMPSTA(I)) THEN
                  OIMPT=.TRUE.
                  GO TO 110
               END IF
            END DO
         END IF
      END IF
C     End preparations for temps
 110  CONTINUE


C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------

      WRITE(*,*) ' SAVREP - start of subr., ihead ',IHEAD

      IUNIT=11
      OPEN(UNIT=IUNIT,IOSTAT=IOS,ERR=500,
     1     FILE=cstream(msys)//'/'//cstream(msys)//'_error_file.dat',
#ifdef FORTRAN90
     2     POSITION='APPEND',
#else
     2     ACCESS='APPEND',
#endif
     3     STATUS='UNKNOWN'    )

C     Mark important temp on the error file 
      IF (OIMPT) THEN
         WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) cimport     
      END IF  

C*          1.2  WRITE BULLETIN HEADER TO ERROR
C                ------------------------------
C                FILE IF NOT ALREADY DONE.
C                -------------------------

      IF ( IHEAD.NE.0 ) GO TO 130

      ISTART(1) = 1
      ISTART(2) = ISL
      ISTART(3) = IAH
      ISTART(4) = IMI

      IFINISH(1) = ISL
      IFINISH(2) = IAH
      IFINISH(3) = JAH
      CALL NEXTEND(IFINISH(3),IGS)
      CALL NEXTPRT(IFINISH(3),IGS)
      IFINISH(4) = JMI
      CALL NEXTEND(IFINISH(4),IGS)
      CALL NEXTPRT(IFINISH(4),IGS)

      IP = 0
      IHEAD = 1

      NN = 4
      IF (KDEC(4) .EQ. 35 .OR. KDEC(4) .EQ. 36         ! Temp land/ship
     *     .OR. KDEC(4) .EQ.135 .OR. KDEC(4) .EQ.137   ! Temp drop/mobile
     *     .OR. KDEC(4) .EQ. 32 .OR. KDEC(4) .EQ. 33   ! Pilo
     *     .OR. KDEC(4) .EQ. 86) NN = 3                ! Speci

      DO N1=1,NN    ! Starting line, abbreviated header line, MIMIMJMJ line
         YLINE=' '
         YLINEA=' '
         CERROR = .FALSE.

         K = IFINISH(N1) - ISTART(N1) 
         IF(K.GT.80) K=80
         DO N2=1,K        ! Position in line
            IP = IP + 1   ! Position in KCHAR
            IF (KCHAR(IP).GT.127) THEN
C           This means that more than 7 bits are needed to represent kchar(ip),
C           which indicates that kchar(ip) has earlier (in statements like 
C           "KCHAR(IPT)=IOR(KCHAR(IPT),128)") been extended with an extra bit 1
C           in  position 8 to mark that an error has occured at this character.
           
C              Mark the erroneous character with a '?'. This will 
C              be the only non-blank character(s) in YLINEA, which
C              is later written out below the original input
C              line to mark where an error has been detected.
               YLINEA(N2:N2) = Y63
               CERROR = .TRUE.
C              Set all but last 7 bits in kchar(ip) to 0,
C              i.e. restore the original character in input:
               KCHAR(IP)=IAND(KCHAR(IP),127)
            END IF
            YLINE(N2:N2) = CHAR(KCHAR(IP))
         END DO

         WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINE(1:K)
         IF (CERROR) THEN
            WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINEA(1:K)
            CERROR = .FALSE.
         END IF

      END DO


C*           1.3   WRITE ERROR REPORT , IF REQUIRED.
C                 ---------------------------------
 130  CONTINUE

C     AVOID WRITING REPORT TWICE IF MIMIMJMJ  LINE IS
C     MISSING.

      IF ( IAH.EQ.KPT ) THEN
           CLOSE(IUNIT)
           RETURN
      END IF

C     AVOID WRITING MIMIMJMJ TWICE ( CORRUPT MIMIMJMJ CAN BE TAKEN
C     AS STATION NUMBER )

      IF ( KPT.LT.IFINISH(4) ) KPT = IFINISH(4)

      IP = KPT
      IF(KDEC(4) .EQ. 35 .OR. KDEC(4) .EQ. 36) IP = IMI   ! Temp land/ship
      IF(KDEC(4) .EQ.135 .OR. KDEC(4) .EQ.137) IP = IMI   ! Temp drop/mobile
      IF(KDEC(4) .EQ. 32 .OR. KDEC(4) .EQ. 33) IP = IMI   ! Pilo

C     Set J to last character in end of line of report
      J = IEQ
      CALL NEXTVAL(J,10,IGS)   ! 10 = LF
      IF (J.GT.IGS) J = IGS

  133 CONTINUE

      YLINE=' '
      YLINEA=' '
      CERROR = .FALSE.

      CALL NEXTPRT (IP,J)   ! skipping blanks in beginning of line
      IF (IP.GT.J) THEN
          CLOSE(IUNIT)
          RETURN
      END IF 
      JP = IP
c$$$      CALL NEXTVAL(JP,10,IGS)   ! next LF
c$$$      IF (JP.GT.IGS) JP = IGS
      CALL NEXTEND(JP,J)
      CALL NEXTPRT(JP,J)
      K= JP - IP 
      IF (K.GT.80) K=80
      DO N1=1,K
         IF ( KCHAR(IP).GT.127 ) THEN ! errors are marked by having high bit set
            YLINEA(N1:N1) = Y63
            CERROR = .TRUE.
            KCHAR(IP) = IAND(KCHAR(IP),127) ! remove parity bit
         END IF

         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
      END DO
     
C     Note that it seems that if YLINE(K:K)=CR, this will not be displayed
      WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINE(1:K)
      IF (CERROR) THEN
         WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINEA(1:K)
         CERROR = .FALSE.
      END IF

      GO TO 133

 
 500  IERR = -1
      WRITE(*,*) ' ERROR IN OPENING ',cstream(msys),'_error_file.dat,',
     *     'IOS=',IOS
      RETURN
 
 600  IERR = -1
      WRITE(*,*) ' ERROR IN WRITING INTO ',cstream(msys),
     *     '_error_file.dat,','IOS=',IOS

      RETURN
      END
