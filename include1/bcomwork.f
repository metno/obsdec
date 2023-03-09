C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NXXP,NYYP,NZZP
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
