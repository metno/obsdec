      subroutine writebufr

C     PURPOSE: Write Bufr message to Bufr file (sunit)
C     AUTHOR: PS aug 2006
C     Note: drau2bufr uses its own version (named writebufr_drau)

      implicit none
      include 'parameter.f'  ! jbufl, used in combufren.f
      include 'combufren.f'  ! kbufl,kbuff
      include 'comwrite.f'   ! num_records,sunit
      integer len,iret
      character*4 c1,c2

C     kbufl is length of kbuff in words; pbwrite expects length in
C     bytes.  Last 1-3 bytes of last word in kbuff might be garbage (if
C     length of BUFR message is not a multiple of 4 bytes), so try to
C     localize last '7777' and do not write anything after this

      write(c1,'(A4)') kbuff(kbufl-1)
      write(c2,'(A4)') kbuff(kbufl)
      if(c2.eq.'7777') then
         len = kbufl*4 
      elseif(c1(4:4).eq.'7' .and. c2(1:3).eq.'777') then
         len = kbufl*4 - 1
      elseif(c1(3:4).eq.'77' .and. c2(1:2).eq.'77') then
         len = kbufl*4 - 2
      elseif(c1(2:4).eq.'777' .and. c2(1:1).eq.'7') then
         len = kbufl*4 - 3
      else
         print*,'Error in writebufr, found no ending 7777'
         call exit(2)
      end if

      call pbwrite(sunit,kbuff,len,iret)
      if (iret.lt.0) then
         print*,'Error in PBWRITE, returned ',iret
         call exit(2)
      end if

      num_records = num_records + 1
      end subroutine writebufr
