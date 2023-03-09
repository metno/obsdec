C (C) Copyright 2010, met.no
C
C This program is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This program is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this program; if not, write to the Free Software
C Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
C 02110-1301, USA.

C     Variables used for filtering av observations (--filter option set
C     in bufrread or bufrdump)

      INTEGER dim1_fid, dim2_fid, dim_fiv
      PARAMETER (dim1_fid=10, dim2_fid=5, dim_fiv=1000)

      INTEGER fid(1:dim1_fid,1:dim2_fid)
                                ! filter descriptors arrays. fid(i,j) is descriptor
                                ! j in filter criterium i
      CHARACTER*10 fidformat(1:dim1_fid,1:dim2_fid)
                                ! filter descriptor format (I2.2, A9 etc)
                                ! for fid(i,j)
      INTEGER nd_fid(1:dim1_fid)  ! number of descriptors for each filter criterium
                                ! (max value of j for fid(i,j))
      INTEGER nvl_fid(1:dim_fiv) ! nvl_fid(i) is number av lines with filter
                                ! values described by filter descriptor line i
      INTEGER nfidlines         ! number of filter descriptor lines
                                ! (max value of i for fid(i,j))

      INTEGER fivI(1:dim_fiv,1:dim2_fid) ! the filter values of integer type 
      CHARACTER*32 fivC(1:dim_fiv,1:dim2_fid) ! the filter values of character type

      INTEGER nfivlines         ! number of lines with filter values

      COMMON /COM_FILTER/ fid,nd_fid,nvl_fid,nfidlines,nfivlines,fivI

      COMMON /COM_FILTERC/ fidformat,fivC
