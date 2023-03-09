      integer jp1,jp2,jp3,jp4,jp5,jp6,jp7,jp8,jp9,jp10,jp11,jp13,
     +     jp14,jp15,jp17,jp18,jp19,jp20,jp21,jp22
      integer jbufl             ! Size of buffer to hold BUFR message

      PARAMETER (JP1 =  100,JP2 = 3000,JP3 = 4096,JP4 =  678,JP5 =60,
     1           JP6 =   50,JP7 =  200,JP8 = 100,JP9  =  200,JP10= 200,
     2           JP11= 4000,JP13=   26,JP14=   32,
     3           jp15= 8192,jp17=20480,jp18=    8,jp19= 2000,jp20=128,
     4           JP21=    2,JP22= 4000,jbufl=512000) 
C
cpsaug03 increased jp11 from 1536 to 4000, jp22 from 3600 to 4000
cpsdec03 increased jp2 from 1000 to 3000
cpsmai06 added jbufl and declared all variables
cpsoct06 removed jp12
