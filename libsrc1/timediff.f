      subroutine timediff(h1,m1,h2,m2,iret)

c     Returns iret=-1 if h1 or h2 is not in [0,23] or m1,m2 not in [0,59] 
c     else (smallest possible) time difference in minutes. 
c     Example: h1,m1=23,31 h2,m2=0,12: returns iret=41

c     Author: P. Sannes EDB, DNMI
c     Date:   18/10-1999

      integer h1,h2,m1,m2,day,iret

      if( h1.lt.0 .or. h1.gt.23 .or. h2.lt.0 .or .h2.gt.23 ) then
         iret = -1
      else if( m1.lt.0 .or. m1.gt.59 .or. m2.lt.0 .or .m2.gt.59 ) then
         iret = -1
      else
         mm1=h1*60 + m1
         mm2=h2*60 + m2
         day=24*60
         iret=min0(iabs(mm1-mm2), iabs(mm1-mm2+day), iabs(mm1-mm2-day))
      endif

      return 
      end
