      integer ktdlen       ! number of descriptors in section 3
      integer ktdlst(70)  ! the descriptors in section 3
C  use a parameter in parameter.f later instead of 70

      common / comdescr / ktdlen, ktdlst
