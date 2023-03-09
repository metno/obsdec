      SUBROUTINE UPFLAG (IERR,J,VALUE,IDIM1,IDIM2,M,IOFF)
C
C**** *UPFLAG*
C
C       PURPOSE.
C      ----------
C
C        INSERT CONFIDENCE VALUES IN BUFR MESSAGE
C
C       INTERFACE.
C      ------------
C
C        CALL UPFLAG (IERR,J,VALUE,IDIM1,IDIM2,M,IOFF)
C
C        INPUT
C         J     - NUMBER OF REPORT WITHIN ARRAY 'VALUE'
C         VALUE - ARRAY OF BUFR MESSAGES
C         IDIM1,IDIM2 -  DIMENSIONS OF ARRAY VALUE
C         M     - NUMBER OF ENTRIES IN EACH REPORT
C         IOFF  - OFFSET WITHIN CONFIDENCE FLAG ARRAY 'IFLAG'
C
C        OUTPUT
C         IERR  - ERROR CODE
C         VALUE - CONFIDENCE FLAGS ADDED, AREA FOR SUBSTITUTIONS
C                     SET TO MISSING
C
C       METHOD.
C      ---------
C
C        ADD CONFIDENCE VALUES TO END OF REPORT;
C        INITIALISE AREA FOR SUBSTITUTIONS TO MISSING
C
C       EXTERNALS.
C      ------------
C
C        NONE
C
C       REFERENCE.
C      ------------
C
C        NONE
C
C       AUTHOR.
C      ---------
C
C        B. NORRIS,  ECMWF,  JANUARY 1989.
C
C       MODIFICATIONS.
C      ----------------
C
C        B. NORRIS, 90/02/20, SET AREA FOR SUBSTITUTIONS TO MISSING
C
      INCLUDE 'paramq.f'
      INCLUDE 'const.f'
      INCLUDE 'conf.f'
      DIMENSION VALUE (IDIM1,IDIM2)
C
      IERR=0
C
C    --------------------------------------------------------------------
C
C             1.  CHECK WHETHER ARRAY IS LARGE ENOUGH
C            -----------------------------------------
C
  100 CONTINUE
      IF(M*3.GT.IDIM1) THEN
           WRITE (*,'(1H ,'' VALUES ARRAY TOO SMALL, NUMBER OF ENTRIES R
     XEQUIRED:'',I7)') M*3
           IERR=2501
           GO TO 300
      ENDIF
C
C    ------------------------------------------------------------------------
C
C             2.  ADD CONFIDENCE VALUES TO RECORD, SUBJECT TO LIMITS
C            --------------------------------------------------------
C
  200 CONTINUE
      DO 202 I=1,M
      VALUE(M+I,J)=IFLAG(1,IOFF+I)
      IF(VALUE(M+I,J).LT.0.0)   VALUE(M+I,J)=0.0
      IF(VALUE(M+I,J).GT.100.0) VALUE(M+I,J)=100.0
      IF(VALUE(I,J).EQ.RMISS)   VALUE(M+I,J)=RMISS
      VALUE(2*M+I,J)=RMISS
  202 CONTINUE
C
C    -----------------------------------------------------------------------------
C
C                     3.  EXIT
C                   ------------
C
  300 CONTINUE
      RETURN
      END
