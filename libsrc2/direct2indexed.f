C      subroutine update_index_file(filename,iunit,irec,key)
C      integer function iget_record_number(filename,iunit,key)
C      integer function iget_next_recnumber(filename,iunit,keylen)
C      integer function iget_end_of_string(string)

C     Some routines to treat direct access files as if they were 
C     indexed files. See start comment in update_index_file.
C     iget_end_of_string is a more general help routine (and
C     should therefore be moved to a 'utility' file/library?)

c     Author: P. Sannes, August 2003


      subroutine update_index_file(filename,iunit,irec,key)

c     Appends key and irec to file named "filename".index
c     This key file is meant to provide a mapping between the
c     record number of a direct access file and the corresponding
c     "key" of that record, so that the direct access file may be
c     accessed as if it was an indexed file (indexed files are not
c     provided on Linux, at least not by GNU and Portland Group 
c     fortran compilers)

      implicit none
      integer iunit,irec,ndx,ios
      integer iget_end_of_string
      character*(*) filename,key

      ndx=iget_end_of_string(filename)
      
      OPEN(UNIT=iunit,
     1     FILE=filename(1:ndx)//'.index',
cf77     2        ACCESS='APPEND',
     2     POSITION='APPEND',
     3     IOSTAT=ios,
     4     FORM='FORMATTED',
     5     STATUS='UNKNOWN'        )

      if(ios.ne.0) then
         write(*,9000) iunit,ios
         print*,'filename=',filename(1:ndx)//'.index'
         return
      end if
      
      write(iunit,fmt='(1X,a,1X,i5)',IOSTAT=ios) key(1:len(key)),irec

      if(ios.ne.0) then
         write(*,9001) iunit,ios
         return
      end if

      close(iunit)

 9000 format(1x,'Error in update_index_file: opening ',I4,' ios=',I5)
 9001 format(1x,'Error in update_index_file: writing ',I4,' ios=',I5)
      
      return
      end


      integer function iget_record_number(filename,iunit,key)

c     Returns record number for record matching key in direct access file 
c     filename by searching key file "filename".index. Returns -1 if no record
c     matches. See subroutine update_index_file for explanation.

      implicit none
      integer iunit,keylen,ndx,ios,irec
      integer iget_end_of_string
      character*(*) filename,key
      character*100 keyfield
      
      iget_record_number=-1

      keylen=len(key)

      ndx=iget_end_of_string(filename)

      OPEN(UNIT=iunit,
     1     FILE=filename(1:ndx)//'.index',
     2     ACCESS='SEQUENTIAL',
     3     IOSTAT=ios,
     4     FORM='FORMATTED',
     5     STATUS='UNKNOWN'        )

      if(ios.ne.0) then
         write(*,9000) iunit,ios
         return
      end if

 100  read(iunit,fmt='(1X,a,1X,i5)',end=200,iostat=ios)
     *     keyfield(1:keylen),irec
      if(ios.ne.0) then
         write(*,9001) iunit,ios
         return
      end if
      if(keyfield(1:keylen) .eq. key) then
         iget_record_number=irec
         goto 200
      end if
      goto 100

 200  close(iunit)

 9000 format(1x,'Error in iget_record_number: opening ',I4,' ios=',I5)
 9001 format(1x,'Error in iget_record_number: reading ',I4,' ios=',I5)
      
      return
      end


      integer function iget_next_recnumber(filename,iunit,keylen)

c     Returns next free record number in direct access file filename,
c     i.e. 1 more than highest record number currently used in filename.
c     We do this by searching through file "filename".index; 
c     see subroutine update_index_file. keylen is length of keys.

c     Isn't there an easier way to do this? 

      implicit none
      integer iunit,keylen,ndx,ios,max_irec,irec
      integer iget_end_of_string
      character*100 keyfield
      character*(*) filename
      logical exist

      max_irec=0

      ndx=iget_end_of_string(filename)
      
      INQUIRE(FILE=filename(1:ndx)//'.index',
     1     EXIST=exist)

      if(exist) then
         OPEN(UNIT=iunit,
     1        FILE=filename(1:ndx)//'.index',
     2        ACCESS='SEQUENTIAL',
     3        IOSTAT=ios,
     4        FORM='FORMATTED',
     5        STATUS='OLD'        )

         if(ios.ne.0) then
            write(*,9000) iunit,ios
            return
         end if

 100     read(iunit,fmt='(1X,a,1X,i5)',end=200,iostat=ios)
     *        keyfield(1:keylen),irec
         if(ios.ne.0) then
            write(*,9001) iunit,ios
            return
         end if
         if(irec.gt.max_irec) max_irec=irec
         goto 100

 200     close(iunit)
      endif

      iget_next_recnumber=max_irec+1

 9000 format(1x,'Error in iget_next_recnumber: opening ',I4,' ios=',I5)
 9001 format(1x,'Error in iget_next_recnumber: writing ',I4,' ios=',I5)
      
      return
      end


      integer function iget_end_of_string(string)
c     Return index of last character before first space in string, 
c     or length of string if no space present

      implicit none
      character*(*) string
      character*1 space
      integer ndx

      space=char(32)
      ndx=index(string,space) - 1
      if(ndx .eq. -1) ndx=len(string)

      iget_end_of_string=ndx

      return
      end
