      subroutine decr_date(iday,imonth,iyear,pday,pmonth,pyear)

c     Input: iday,imonth,iyear
c     Output: pday,pmonth,pyear which is one day earlier than 
c             iday,imonth,iyear
c     Example: iday,imonth,iyear = 1,3,1999 => pday,pmonth,pyear = 28,2,1999
c     Warning: Does not check if input time (or output time!) is a legal date

c     Author: P. Sannes EDB, DNMI
c     Date:   20/10-1999

      integer iday,imonth,iyear,pday,pmonth,pyear
      logical leap_year

      pmonth=imonth
      pyear=iyear

      if(iday.gt.1) then
         pday=iday-1
      else if(imonth.eq.1) then
         pday=31
         pmonth=12
         pyear=iyear-1
      else
         pmonth=imonth-1
         if(imonth.eq.3) then
            if(leap_year(iyear)) then
               pday=29
            else
               pday=28
            endif
         else
            if(imonth.eq.5.or.imonth.eq.7.or.imonth.eq.10
     *           .or.imonth.eq.12) then
               pday=30
            else
               pday=31
            endif
         endif
      endif

      return
      end
         

      logical function leap_year(iy)
      integer iy
c     Returns true if iy is leap year, else false

      if( mod(iy,400).eq.0 ) then
         leap_year=.true.
      else if (mod(iy,4).eq.0 .and. mod(iy,100).ne.0 ) then
         leap_year=.true.
      else
         leap_year=.false.
      endif

      return
      end
