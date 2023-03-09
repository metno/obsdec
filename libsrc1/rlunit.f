c
c  milib
c  
c  $Id: rlunit.f 3273 2010-05-18 17:32:21Z dages $
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
      subroutine rlunit(lrunit)
c
c  PURPOSE: Return the unit length in bytes used for RECL= when
c           opening a direct access file.
c           According to the ANSI FORTRAN 77 standard this unit
c           is machine dependant.
c
c-----------------------------------------------------------------------
c  DNMI/FoU  09.10.1992  Anstein Foss
c-----------------------------------------------------------------------
c
c..output:
      integer lrunit
c
c.SGI and DEC: record length in unit 32 bit words (= 4 bytes)
cc    lrunit=4
c
c.SUN, IBM RS/6000, "PC", Linux: record length in unit bytes
      lrunit=1
c
      return
      end
