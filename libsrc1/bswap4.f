c
c  milib
c  
c  $Id: bswap4.f 3273 2010-05-18 17:32:21Z dages $
c
c  Copyright (C) 2006 met.no
c
c  Contact information:
c  Norwegian Meteorological Institute
c  Box 43 Blindern
c  0313 OSLO
c  NORWAY
c  email: diana@met.no
c  
c  This library is free software; you can redistribute it and/or
c  modify it under the terms of the GNU Lesser General Public
c  License as published by the Free Software Foundation; either
c  version 2.1 of the License, or (at your option) any later version.
c
c  This library is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
c  Lesser General Public License for more details.
c  
c  You should have received a copy of the GNU Lesser General Public
c  License along with this library; if not, write to the Free Software
c  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c
      subroutine bswap4(ldata,idata)
c
c       swap bytes in integer*4 words
c
c-----------------------------------------------------------------------
c  DNMI/FoU  xx.xx.199x  Anstein Foss
c-----------------------------------------------------------------------
c
      implicit none
c
      integer   ldata
      integer*4 idata(ldata)
c
      integer i,iswap,iswapo
c
      do i=1,ldata
        iswap=idata(i)
        iswapo=ior(iand(ishft(iswap,-24),255),
     +             iand(ishft(iswap,-8),65280))
        iswapo=ior(iswapo,ishft(iand(iswap,65280),+8))
        iswapo=ior(iswapo,ishft(iand(iswap,255),+24))
        idata(i)=iswapo
      end do
c
      return
      end
