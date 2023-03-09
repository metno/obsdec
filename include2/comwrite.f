      integer sunit       ! Unit number of the file bufr messages are written to
      integer num_records ! Number of bufr records currently written.
                          ! Note that this starts anew on 1 for each new
                          ! msys file decoded. 
      COMMON /COMWRITE/ sunit,num_records 
