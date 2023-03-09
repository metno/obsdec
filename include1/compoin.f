C
      COMMON /COMPOIN/ NBPW,NWPT,NBPT,OPS2,
     1                 MASKS(JP14),MBUF(JP3)
C
C     NBPW          -  NUMBER OF BITS PER COMPUTER WORD
C     NWPT          -  POINTER TO WORD IN ARRAY MBUFF
C     NBPT          -  POINTER TO BIT INSIDE AN COMPUTER WORD
cps   ops2          -  .TRUE. if optional section 2 in bufr is present
c                      Set to .true. in INITVAR in all nix_<obstype>.f
C     MASKS         -  ARRAY CONTAINING BIT MASKS
C     MINDIC        -  MISSING DATA INDICATOR
C     MBUF          -  ARRAY CONTAINING SINGLE BUFR MESSAGE
C
