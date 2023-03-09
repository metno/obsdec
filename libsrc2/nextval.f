      SUBROUTINE NEXTVAL ( I,N,J )
C
C     PURPOSE.
C     --------
C
C         LOCATE THE FIRST WORD CONTAINING THE (ASCII) VALUE 'N' IN
C         ARRAY 'KCHAR' BETWEEN WORD 'I' AND WORD 'K'.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *NEXTVAL(I,N,J)*
C
C         INPUT     : I - SCAN STARTS AT WORD 'I' OF 'KCHAR'
C                     J - SCAN ENDS AT WORD 'J' OF 'KCHAR'
C
C         OUTPUT    : I = REQUIRED LOCATION.
C                     I > J INDICATES THE VALUE 'N' NOT FOUND.
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C     MODIFICATIONS.
C     --------------
C
C          PS nov 2006 Returns I = J+1 if character(s) not found

      implicit none
      integer I,N,J
      integer k,kar

      INCLUDE 'parameter.f'
      INCLUDE 'comwork.f'    ! kchar

C     ------------------------------------------------------------------
C*          1.   SCAN BULLETIN.
C                --------------
      I = IABS(I)
      K = I
      DO I=K,J
         KAR = IAND(KCHAR(I),127)
         IF (KAR .EQ. N) RETURN
      END DO

C     CHARACTER NOT FOUND
      I = J + 1

      RETURN
      END
