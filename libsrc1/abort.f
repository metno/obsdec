      SUBROUTINE ABORT
C
C**** *ABORT*
C
C
C     PURPOSE.
C     --------
C
C         Abort subroutine which is called from several of the decoding
C         routines. This is our own version becuase we didn't get one from
C         ECMWF.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ABORT*
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
C          R.RUDSAR  DNMI SEPT.1993
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
C
C     ------------------------------------------------------------------
C
C*          1.   ERROR MESSAGE
C                -------------
 100  CONTINUE
C
C
      print*,' ********* STOP 999 from subroutine ABORT *********'
C
      stop 999
C
      END
