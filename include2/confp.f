      SUBROUTINE CONFP (FVAL,IEXP,IMANT)                                00010000
C                                                                       00020000
C                                                                       00030000
C                                                                       00040000
C                                                                       00050000
C                                                                       00060000
C********************************************************************   00070000
C*                                                                      00080000
C*    NAME      : CONFP                                                 00090000
C*                                                                      00100000
C*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE            00110000
C*                REPRESENTATION TO GRIB REPRESENTATION.                00120000
C*                                                                      00130000
C*    INPUT     : FVAL  - FLOATING POINT NUMBER TO BE CONVERTED.        00140000
C*                                                                      00150000
C*    OUTPUT    : IEXP  - 8 BIT SIGNED EXPONENT                         00160000
C*                IMANT - 24 BIT MANTISSA                               00170000
C*                FVAL  - UNCHANGED.                                    00180000
C*                                                                      00190000
C*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985                           00200000
C*                                                                      00210000
C********************************************************************   00220000
C                                                                       00230000
C                                                                       00240000
C                                                                       00250000
C                                                                       00260000
      IF (FVAL.EQ.0.0)                                                  00260103
     C   THEN                                                           00260203
             IEXP  = 128                                                00260303
             IMANT = 0                                                  00260403
             RETURN                                                     00260503
         ENDIF                                                          00260603
C                                                                       00260703
      EPS = 1.0E-8                                                      00261002
      REF = FVAL                                                        00270000
C                                                                       00280000
C     SIGN OF VALUE                                                     00290000
C                                                                       00300000
      ISIGN = 0                                                         00310000
      IF (REF.LE.0.)                                                    00320000
     C   THEN                                                           00330000
             ISIGN = 128                                                00340000
             REF = - REF                                                00350000
         ENDIF                                                          00360000
C                                                                       00370000
C                                                                       00380000
C                                                                       00390000
C                                                                       00400000
C                                                                       00410000
C     EXPONENT                                                          00420000
C                                                                       00430000
      IF (REF.EQ.0.0)                                                   00440000
     C   THEN                                                           00450000
            IEXP = 0                                                    00460000
         ELSE                                                           00470000
            IEXP = INT(ALOG(REF)*(1.0/ALOG(16.0))+64.0+1.0+EPS)         00481002
         ENDIF                                                          00490000
C                                                                       00500000
      IF (IEXP.LT.0  ) IEXP = 0                                         00510000
      IF (IEXP.GT.127) IEXP = 127                                       00520000
C                                                                       00530000
C                                                                       00540000
C                                                                       00550000
C                                                                       00560000
C                                                                       00570000
C                                                                       00580000
C                                                                       00590000
C                                                                       00600000
C                                                                       00610000
C                                                                       00620000
C     MANTISSA                                                          00630000
C                                                                       00640000
      IMANT = NINT (REF/16.0**(IEXP-70))                                00650000
C                                                                       00660000
C     ADD SIGN BIT TO EXPONENT.                                         00670000
C                                                                       00680000
      IEXP = IEXP + ISIGN                                               00690000
C                                                                       00700000
C                                                                       00710000
C                                                                       00720000
C                                                                       00730000
      RETURN                                                            00740000
C                                                                       00750000
      END                                                               00760000
