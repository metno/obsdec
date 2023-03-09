      SUBROUTINE SAVBULL ( IERR )
C
C**** *SAVBULL*
C
C
C     PURPOSE.
C     --------
C
C         WRITE COMPLETE BULLETIN TO ERROR FILE .
c         with errors marked with '?' on following line.
c         In KCHAR, errors are recognized by having bit 8 set. This
c         bit is set back to 0 after having procesessed the characters.
c         For temps, 'important temps' are marked.
C
C         BULLETIN IN KCHAR(1)-KCHAR(IGS) IN CCITT 5.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *SAVBULL(IERR)*
C
C         OUTPUT   : ierr = -1 if error opening or writing to error file
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
      implicit none
      integer ierr,i,iunit,ios,ip,jp,k,n1,n
      logical otemp   ! true if this report is a temp
      logical oimpt   ! true if this is an important temp
      logical cerror  ! true if the character processed is marked as in error
C
      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'  ! kdec
      INCLUDE 'comindx.f'  ! igs
      INCLUDE 'combuff.f'  ! impsta
      INCLUDE 'commdbc.f'  ! cstream
      INCLUDE 'commsys.f'  ! msys
C
C
      CHARACTER*80 YLINE,YLINEA    ! YLINE is used to print a line in the report,
                                   ! YLINEA is used to print the line with '?' to 
                                   ! mark where the error in line YLINE occurs
      CHARACTER*1 Y63              ! The ascii character '?'
      CHARACTER*4 YGS              ! end line of report
      CHARACTER*14 cimport
      data cimport/'important_temp'/
C
      Y63=CHAR(63)                                 ! '?'
      YGS=CHAR(13)//CHAR(13)//CHAR(10)//CHAR(29)   ! CR CR LF GS
C     ------------------------------------------------------------------

C     First some preparations for temps
C     ---------------------------------

      OTEMP=.FALSE.
      OIMPT=.FALSE.
C
      IF(KDEC(4).EQ.35.OR.KDEC(4).EQ.36.OR.
     1   KDEC(4).EQ.135.OR.KDEC(4).EQ.137) OTEMP=.TRUE.
C
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

      YLINE  =' '
      YLINEA =' '
      cerror = .false.
C
C*          1.   OPEN ERROR FILE AT THE BEGINNING OF THE PROCESS.
C                -------------------------------------------------
C
      IUNIT=11
      OPEN(UNIT=IUNIT,IOSTAT=IOS,ERR=500,
     1     FILE=cstream(msys)//'/'//cstream(msys)//'_error_file.dat',
#ifdef FORTRAN90
     2     POSITION='APPEND',
#else
     2     ACCESS='APPEND',
#endif
     3     STATUS='UNKNOWN'    )
C
CRR   Mark important temp on the error file 
      IF(OIMPT) THEN
         WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) cimport     
      END IF  

      IP = 1
      JP = IP
C
C*          1.1  OUTPUT INITIAL CONTROL CHARACTERS.
C                ----------------------------------
C           i.e. SOH (looks like ^A in emacs), because KCHAR(1)=SOH (=001o)
C
      CALL NEXTPRT (JP,IGS)
      K = JP - IP
      N1 = 0
      DO  N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127) THEN
            YLINEA(N1:N1)=Y63
            cerror = .true.
            KCHAR(IP)=IAND(KCHAR(IP),127)
         END IF
         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
      END DO
C
      WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINE(1:N1)
      IF (cerror) THEN
         WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINEA(1:N1)
         cerror  = .false.
      END IF
C
 120  CONTINUE
C
      YLINE  = ' '
      YLINEA = ' '
C
      CALL NEXTPRT (IP,IGS)
      IF (IP .GT. IGS) GO TO 400
      JP = IP
      CALL NEXTEND (JP,IGS)
      CALL NEXTPRT (JP,IGS)
      K = JP - IP
      N1 = 0
cpsjun15 Skip rest of message if line exceeds 80 characters
      if(k.gt.80) then
         write(*,*) 'Warning: too long line, skipping rest of message'
         write(*,*) 'K=',k, ' > 80'
         goto 400
      endif
cpsjun15
      DO N=1,K
         N1 = N1 + 1
         IF ( KCHAR(IP).GT.127 ) THEN  ! errors are marked by having high bit set
            YLINEA(N1:N1) = Y63
            cerror = .true.
            KCHAR(IP) = IAND(KCHAR(IP),127)  ! remove parity bit
         END IF
C
         YLINE(N1:N1) = CHAR(KCHAR(IP))
         IP = IP + 1
      END DO
C     
      WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINE(1:N1)
      IF (cerror) THEN
         WRITE (IUNIT,'(A)',ERR=600,IOSTAT=IOS) YLINEA(1:N1)
         cerror = .false.
      END IF
C
      GO TO 120
C
 400  CONTINUE
C
cpsoct06 Don't see why this line should be printed, as GS has already
cps      (in procrfb) been inserted as last character in bulletine
cps      WRITE(IUNIT,'(A)',ERR=600,IOSTAT=IOS) YGS
      CLOSE(IUNIT) 
C
C     REMOVE PARITY BIT (they should in fact already have been removed...)
C
      DO I = 1,IGS
         KCHAR(I) = IAND(KCHAR(I),127)
      END DO
C
      RETURN
C 
 500  IERR = -1
      PRINT*,' ERROR IN OPENING ',cstream(msys),'_error_file.dat,',
     *     'IOS=',IOS
      RETURN
C 
 600  IERR = -1
      PRINT*,' ERROR IN WRITING INTO ',cstream(msys),'_error_file.dat,',
     *     'IOS=',IOS

      RETURN
      END
