      subroutine prtvalues(nmax)
C     Print first nmax (rounded upwards to nearest multiple of 10)
C     entries of array VALUES. For debugging.

      implicit none
      include 'parameter.f'  ! jp22,jp1
      include 'comwt.f'      ! values

      integer nmax,i,j,k,imax,jmax

      imax = int(nmax/10)
      if (mod(nmax,10).gt.0) imax=imax+1
      write(*,1000) imax*10
 1000 format(/1x,' Values of array VALUES(1-',I5,')')

      k=0
      do i=1,imax
         write(*,2000) k+1,(values(j,1),j=k+1,k+10)
         k=k+10
      end do

2000  format(1x,i4,4x,10(f9.1,1x))
      return
      end
