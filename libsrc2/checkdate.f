      subroutine checkdate(year,month,day,iret)

c     Returns iret=-1 if (year,month,day) is not a valid date.
C     Else returns 0.

c     Author: P. Sannes, IT-div
c     Date:   01/03-2002

      implicit none
      integer year,month,day,iret,daysinmonth
      dimension daysinmonth(12)
      data daysinmonth/31,28,31,30,31,30,31,31,30,31,30,31/

c Change days in february if leap year.
      if ( mod(year,400).eq.0 ) daysinmonth(2)=29
      if ( mod(year,4).eq.0 .and. mod(year,100).ne.0) 
     x     daysinmonth(2)=29

c Check if valid date.
      if ( month.le.0 .or. month.gt.12 .or. day.le.0 
     x     .or. day.gt.daysinmonth(month) ) then
        iret = -1
      else 
         iret = 0
      endif

      return
      end
