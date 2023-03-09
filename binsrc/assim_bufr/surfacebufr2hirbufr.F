c  NwpPreprocObs - Free source codes and example scripts for preprocessing
c                  of meteorological observations

c  $Id: surfacebufr2hirbufr.F,v 1.4 2011/09/23 13:16:34 a001723 Exp $

c  Copyright (C) 2009 smhi

c  Contact information:
c  Swedish Meteorological and Hydrological Institute
c  Folkborgsvagen 1
c  601 76 NORRKOPING
c  SWEDEN
c  email:

c  This file is part of NwpPreprocObs

c  NwpPreprocObs is free software; you can redistribute it and/or
c  modify it under the terms of the GNU General Public License as
c  published by the Free Software Foundation; either version 2
c  of the License, or (at your option) any later version.

c  NwpPreprocObs is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
c  General Public License for more details.

c  You should have received a copy of the GNU General Public License along
c  with NwpPreprocObs; if not, write to the Free Software Foundation Inc.,
c  51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

C Usage: surfacebufr2hirbufr <bufr file> <outfile> [--verbose n]

C Purpose: Convert the message(s) in bufr file
C into HIRLAM BUFR templates, using BUFR edition 3.
C Print the converted messages to <outfile>.

C 4 different HIRLAM BUFR templates are implemented:
C - synop land low altitude
C - synop land high altitude
C - ship
C - dribu surface

C Confidence values (all set to 70%) are included.


C Remarks:

C Only BUFR messages with data category 0 and 1 are converted. One of
C the synop land templates is used if WMO block number (001001) is
C provided (or no WMO block number but instead 001101 state indentifier
C is provided, in which case a fake wmonr is constructed), the ship
C template if call sign (001011) is provided, the dribu surface template
C if buoy identifier (001005 or 001087) is provided. For synop land the
C template for high altitude stations is selected if a3 (007004) is
C provided, else the template for low altitude stations.

C If 025053 observation quality is present with flag for GOOD not set,
C the report is skipped if not containing (non zero) snow depth (SA),
C else included but with all other observations set to missing.

C The symbols used for the meteorological parameters are fetched
C from the KVOSS project, else we (normally) use the symbols in
C synop code.

C If an error in libbufr occurs while decoding (e.g. if the table is
C not found in BUFR_TABLES), the message will be skipped, with an
C error message written to STDOUT (should we use STDERR instead?).

C BUFR messages with data category different from 0 and 1 are
C rejected. Possibly data category 31 (oceanographic data) could
C contain surface data also, but all BUFR BUOY messages I have seen
C so far use data category 1.

C The date and time parameters used in section 1 in the converted
C BUFR messages are fetched from section 4 of the individual
C observations (subsets) of the original BUFR message for dribu
C surface, from section 1 in the original BUFR message for the other
C Hirlam templates.

C Some parameters might occur more than one time in a BUFR message
C E.g. for buoy data there are usually 2 sets of year, month, day, hour,
C minute where one set (the one we do not want to encode) is preceded by
C 008021 TIME SIGNIFICANCE set to 26 ("Time of last known
C position"). Except for this special case, we choose always to include
C the first non missing occurrence of a parameter, ignoring all
C successive occurrences.

C Cloud data are hard to decode in the general case. Had to assume
C that there always will be three 020012 ('Cloud type') descriptors
C before the individual cloud layers start (corresponding to 8-groups
C in section 3 in synop).

C For 011001 WIND DIRECTION and 011002 WIND SPEED we ought to check
C that these are preceded by 007032 HEIGHT OF SENSOR ABOVE LOCAL
C GROUND set to approximately 10, 008021 TIME SIGNIFICANCE set to 2
C (Time average) and 004025 TIME PERIOD OR DISPLACEMENT (minutes) set
C to a negative value 10 or less. There is, however, only one 011001
C and one 011002 in all WMO templates I have seen. And if more winds
C are included, I expect that the one we want (10 meter, time
C average over last 10 minutes) will be the first one to occur. And
C the present implementation will include the first occurrence of
C 011001 and 011002 with non missing value only.

C The same applies for TA (T2M): we do not check the values of the
C 007032 preceding 12101 (or 12001), but assume that if there is
C more than one temperature in the message, the 2M temperature will
C come first.

C Precipitation should for land stations according to setblsno.F90
C and setblshi.F90 in OBSPROC be included as 13023 (RR_24), but I
C have copied what is done in nix_syno.F at met.no for producing
C surf...rdb files, using one of 13023=RR_24, 13022=RR_12,
C 13021=RR_06 and 13020=RR_03, in that priority. Note that RR_01 is
C ignored: the filtered message will have RR_24 with missing value
C if RR_01 is only precipitation measurement in BUFR report (I once
C tried to include 13019=RR_01 in nix_syno.F, but that caused Hirlam
C to crash).

C 013013 'Total snow depth' is included in Hirlam land templates
C while 020062 'State of the ground' is not. Since it is quite common
C to have 013013 set to missing value when 020062 is included with
C value 0-10 which all implies 'without snow or measurable ice cover',
C I have chosen to set SA = 0 in this case.

C 020004 PAST WEATHER (1) and 020005 PAST WEATHER (2) are in the WMO
C templates preceded by 004024 TIME PERIOD OR DISPLACEMENT (in hours).
C This value is not checked (should we?).

C Data present indicator is set to 0 for all data, also missing data
C (for which confidence value is set to missing value).


      PROGRAM surfacebufr2hirbufr

      IMPLICIT NONE

      CHARACTER(LEN=80) Usage,argument
      CHARACTER(LEN=80) bufr_file  ! Bufr file to read from
      CHARACTER(LEN=80) outfile    ! Bufr file to write converted BUFR messages to

      INTEGER jbufl                ! Size of buffer to hold BUFR message
      PARAMETER (jbufl=512000)
      INTEGER ibuff(jbufl)         ! Buffer to hold BUFR message
      INTEGER ibflen               ! Size in BYTES of the array ibuff
      PARAMETER (ibflen=4*jbufl)

      INTEGER verbose              ! Level of verbose output: 0 - 4 (default 0).
                                   ! 0: no output
                                   ! 1: warnings
                                   ! 2: message level
                                   ! 3: subset level
                                   ! 4: you probably don't want to use this
      INTEGER nlibbufr_errors      ! Number of errors encountered in libbufr calls

      INTEGER ilen                 ! Size in BYTES of the BUFR product read
      INTEGER wlen                 ! Size in WORDS of the BUFR product read (one word = 4 bytes)
      INTEGER iunit,ounit
      INTEGER iarg,iloop,iret
      LOGICAL eof

      Usage = '\nUsage: surfacebufr2hirbufr <bufr file> <outfile>'
     +     // ' [--verbose n] \n'

      iarg = IARGC()
      IF (iarg.LT.2 .OR. iarg.EQ.3) THEN
         WRITE(*,*) Usage
         CALL EXIT(0)
      END IF

      CALL GETARG(1,bufr_file)
      CALL GETARG(2,outfile)

      IF (iarg.EQ.4) THEN
         CALL GETARG(3,argument)
         IF (argument.NE.'--verbose') THEN
            WRITE(*,*) Usage
            CALL EXIT(0)
         END IF
         CALL GETARG(4,argument)
         READ (argument, '(I1)') verbose
      ELSE
         verbose = 0 ! default value: no verbose output
      END IF

C     Open bufr file - for read
      CALL PBOPEN(iunit,bufr_file,'r',iret)
      IF (iret.NE.0) CALL err_msg('PBOPEN failed for ' // bufr_file)

C     Open output bufr file - for write
      CALL PBOPEN(ounit,outfile,'w',iret)
      IF (iret.NE.0) CALL err_msg('PBOPEN failed for ' // outfile)

      nlibbufr_errors = 0
      iloop = 0
      DO WHILE (.TRUE.)
         iloop = iloop + 1

C     Get the next BUFR message
         CALL get_next_bufr_message(iunit,ibflen,iloop,verbose,
     +        ibuff,ilen,eof)
         IF (eof) GOTO 900

C     Decode and process BUFR message
         wlen = ilen/4 + 1   ! shouldn't really add 1 when ilen is divisible with 4,
                             ! but copy how it is done in decode_bufr in libbufr
         CALL process_bufr(wlen,ibuff,ounit,verbose,nlibbufr_errors)
      END DO
 900  CONTINUE

      IF (nlibbufr_errors.GT.0) THEN
         WRITE(*,*) '\n\n  NOTE: At least ',nlibbufr_errors,
     +        ' BUFR message(s) skipped due to errors in libbufr calls',
     +        ' Check output for details of the errors encountered!',
     +        ' (Search on "ERROR")'
      END IF

      END PROGRAM surfacebufr2hirbufr

C     -----------------------------------------------------------------

      SUBROUTINE get_next_bufr_message(iunit,ibflen,iloop,verbose,
     +        ibuff,ilen,eof)
      IMPLICIT none
      INTEGER iunit,ibflen,iloop,verbose ! Input
      INTEGER ibuff(*),ilen              ! Output
      LOGICAL eof                        ! Output
      INTEGER iret

      eof = .FALSE.
      iret = 0
      CALL PBBUFR(iunit,ibuff,ibflen,ilen,iret)
      IF (iret.EQ.-1) THEN
         eof = .TRUE.
         RETURN
      ELSEIF (iret.EQ.-2) THEN
         CALL err_msg('ERROR: File handling problem')
      ELSEIF (iret.EQ.-3) THEN
         CALL err_msg('ERROR: Array too small')
      END IF
      IF (verbose.GT.1) THEN
         WRITE (*,'(1X,A,I10)') 'BUFR message number= ',iloop
         WRITE (*,'(1X,A,I10)') 'Length of BUFR message = ',ilen
      END IF

      END SUBROUTINE get_next_bufr_message

C     -----------------------------------------------------------------

      SUBROUTINE process_bufr(wlen,ibuff,ounit,verbose,nlibbufr_errors)
C     Decode BUFR message into fully decoded form, then for each subset
C     encode a BUFR message using one of the Hirlam surface templates if
C     one such is applicable, then write the encoded BUFR message to file.

      IMPLICIT NONE

      INTEGER wlen              ! Input: Size in WORDS of the BUFR message read
      INTEGER ibuff(*)          ! Input: Buffer to hold BUFR message
      INTEGER ounit             ! Input: unit to write BUFR message to
      INTEGER verbose           ! Input
      INTEGER nlibbufr_errors   ! Output: Number of errors encountered in libbufr calls

      INTEGER kxelem            ! Expected (max) number of expanded elements
      INTEGER ksub,kerr

      INTEGER surface_type      ! Set to the HIRLAM surface template that is to be used:
                                ! 1 = synop land low altitude
                                ! 2 = synop land high altitude
                                ! 3 = ship
                                ! 4 = dribu surface
      REAL walues(200)          ! expanded data values for surface templates
      CHARACTER*80 cwals(1)     ! CCITTIA5 data values for surface templates
      REAL year,month,day,hour,minute ! Output from get_surface_values
      INTEGER iyear,imonth,iday,ihour,iminute ! Date/time in section 1
      INTEGER bufr_edition

      INTEGER kbuff(1000)       ! integer array containing encoded bufr message
      INTEGER kbufl             ! length of encoded bufr message (words)

C     BUFREX variables
C     Dimensions of arrays used by libbufr routines are the same as those used in
C     example program decode_bufr.F in bufr_000310/example/
      INTEGER kelem             ! expected (max) number of expanded elements
      INTEGER kvals             ! expected (max) number of data values
      PARAMETER (kelem = 160000, kvals = 4096000)
      INTEGER ksup(9)           ! integer array containing supplementary information
      INTEGER ksec0(3)          ! Bufr section 0 information
      INTEGER ksec1(40)
      INTEGER ksec2(4096)
      INTEGER ksec3(4)
      INTEGER ksec4(2)
      CHARACTER*64 cnames(kelem)! Bufr Table B element names
      CHARACTER*24 cunits(kelem)! Bufr Table B units
      CHARACTER*80 cvals(kelem) ! CCITTIA5 Bufr elements entries (one subset)
      REAL values(kvals)      ! expanded data values (one subset)

C     BUSEL variables
      INTEGER ktdlst(kelem)     ! array containing data descriptors in section 3
      INTEGER ktdexp(kelem)     ! array containing expanded data descriptors
      INTEGER ktdlen            ! number of data descriptors in section 3
      INTEGER ktdexl            ! number of entries in list of expanded data descriptors

      INTEGER update_sequence_number
      INTEGER category
      INTEGER subcategory
      INTEGER precip_desc
      INTEGER num_data
      LOGICAL skip_this_message

      kerr = 0

C     Using parameter kelem in call to BUFREX might be too big for
C     multisubset messages. Have copied the method used in decode_bufr.F
C     in libbufr, first calling BUS012 in order to get number of subsets
C     ksup(6)
      CALL BUS012(wlen,ibuff,ksup,ksec0,ksec1,ksec2,kerr)
      IF (kerr.NE.0) THEN
         WRITE(*,*) 'ERROR IN BUS012: KERR= ',kerr
         nlibbufr_errors = nlibbufr_errors + 1
         RETURN
      END IF
      kxelem = kvals/ksup(6)
      IF (kxelem.GT.kelem) kxelem = kelem

C     Extract standard time from section 1
      bufr_edition = ksec0(3)
      iyear = ksec1(9)
      IF (bufr_edition .LT. 4) THEN
         IF (iyear.EQ.100) THEN
            iyear = 2000
         ELSE
            iyear = iyear + 2000
         END IF
      END IF
      imonth = ksec1(10)
      iday = ksec1(11)
      ihour = ksec1(12)
      iminute = ksec1(13)

      CALL BUFREX (wlen,ibuff,ksup,ksec0,ksec1,ksec2,
     +     ksec3,ksec4,kxelem,cnames,cunits,kvals,values,
     +     cvals,kerr)
      IF (kerr.NE.0) THEN
         WRITE(*,*) 'ERROR IN BUFREX: KERR= ',kerr
         nlibbufr_errors = nlibbufr_errors + 1
         RETURN
      END IF

C     Convert messages with data category (BUFR table A) 0 and 1 only.
C     0 = Surface data - land, 1 = Surface data - sea
      IF (ksec1(6).GT.1) RETURN

      update_sequence_number = ksec1(4)
      IF (verbose.GT.1) WRITE (*,'(1X,A,I10)')
     +     'Number of subsets:',ksup(6)

      subset_loop: DO ksub=1,ksup(6)
         CALL BUSEL2(ksub,kxelem,ktdlen,ktdlst,ktdexl,ktdexp,
     +        cnames,cunits,kerr)
         IF (kerr.NE.0) THEN
            WRITE(*,*) 'ERROR IN BUSEL2: KERR= ',kerr
            nlibbufr_errors = nlibbufr_errors + 1
            RETURN
         END IF
         skip_this_message = .FALSE.
         CALL get_surface_values(ksub,kxelem,ktdexl,ktdexp,values,
     +      cvals,verbose,surface_type,walues,cwals,year,month,day,hour,
     +      minute,category,subcategory,precip_desc,num_data,
     +      skip_this_message)
         IF (surface_type.GE.1 .AND. surface_type.LE.4) THEN
            IF (skip_this_message) THEN
               IF (verbose.GT.0) THEN
                  WRITE(*,*) 'Data contains nonsense/unreliable values'
     +                 // ' and will be ignored'
               END IF
               CYCLE subset_loop
            END IF
C     For section 1 in the new message use date/time from section 1 in
C     old message for synop and ship, while use date/time from section 4
C     for dribu
            IF (surface_type.LE.3) THEN
               CALL encode_bufr(surface_type,walues,cwals,iyear,
     +              imonth,iday,ihour,iminute,
     +              update_sequence_number,category,subcategory,
     +              precip_desc,kbuff,kbufl,num_data)
            ELSE
               CALL encode_bufr(surface_type,walues,cwals,NINT(year),
     +              NINT(month),NINT(day),NINT(hour),NINT(minute),
     +              update_sequence_number,category,subcategory,
     +              precip_desc,kbuff,kbufl,num_data)
            END IF
            CALL PBWRITE(ounit,kbuff,kbufl*4,kerr)
            IF (kerr.LT.0) CALL err_msg1('Error in PBWRITE: KERR=',
     +           kerr)
         ELSE IF (verbose.GT.0) THEN
            WRITE(*,'(1X,A,I10)') 'No appropriate Hirlam template found'
     +           // ' for this message, subset ',ksub
         END IF
      END DO subset_loop

      END SUBROUTINE process_bufr

C     -----------------------------------------------------------------

      SUBROUTINE encode_bufr(surface_type,walues,cwals,year,
     +           month,day,hour,minute,update_sequence_number,
     +           category,subcategory,precip_desc,kbuff,kbufl,
     +           num_data)
C     Encode BUFR message by calling BUFREN.
      IMPLICIT NONE

      INTEGER surface_type      ! Input: the HIRLAM surface template that is to be used:
                                ! 1 = synop land low altitude
                                ! 2 = synop land high altitude
                                ! 3 = ship
                                ! 4 = dribu surface
      REAL walues(*)            ! Input: expanded data values for surface template
      CHARACTER*80 cwals(*)     ! Input: CCITTIA5 data values for surface template
      INTEGER year,month,day,hour,minute,update_sequence_number,
     +     category,subcategory
                                ! Input: needed in section 1. Note that the date
                                ! time parameters are fetched from section 4 in
                                ! each subset of the messages to convert, not from
                                ! section 1
      INTEGER precip_desc       ! Input: the precipitation descriptor to be used in section 3
      INTEGER num_data          ! Input: used in section 3 to set replication factor in QC part
      INTEGER kbuff(*)          ! Output: integer array containing encoded bufr message
      INTEGER kbufl             ! Output: length of encoded bufr message (words)


C     BUFREN variables. See corresponding k.... variables in subroutine
C     process_bufr for explanation
      INTEGER nsec0(3),nsec1(40),nsec2(4096),nsec3(4),nsec4(2),
     +     ntdlen,ntdlst(10),nelem,nvals
      INTEGER ndlen             ! integer (dimension of kdata array)
      INTEGER ndata(1)          ! integer array containing data needed for data
                                ! descriptor expansion (delayed replication factors)
                                ! which appear in the values array
      INTEGER kerr

      nsec0(1) = 0    ! Length of section 0, not used by BUFREN
      nsec0(2) = 0    ! Total length of BUFR message, not used by BUFREN
      nsec0(3) = 3    ! BUFR edition number

      nsec1(1) = 18   ! Length of section 1 (bytes). Must be set by user
      nsec1(2) = 3    ! BUFR edition number
      nsec1(3) = 98   ! Originating centre (98=ECMWF. Should we use 88=Oslo instead?)
      nsec1(4) = update_sequence_number
      nsec1(5) = 0    ! No section 2 (optional section)
      nsec1(6) = category
      nsec1(7) = subcategory
!OV      nsec1(8) = 0    ! Version number of local table used
      nsec1(8) = 1    ! Version number of local table used
      nsec1(9) = year ! Should be year of century in edition 3 (100 for new millennia)
      IF (year.GT.100) nsec1(9) = mod(year,100)
      IF (nsec1(9).EQ.0) nsec1(9) = 100
      nsec1(10) = month
      nsec1(11) = day
      nsec1(12) = hour
      nsec1(13) = minute
      nsec1(14) = 0   ! BUFR Master table used
!OV      nsec1(15) = 13  ! Version number of Master table used
      nsec1(15) = 7   ! Version number of Master table used
      nsec1(16) = 0   ! Originating sub-centre
      nsec1(17) = 0   ! International subcategory (not used for BUFR edition 3)
      nsec1(18) = 0   ! Second (not used for BUFR edition 3)
      nsec1(19) = 0   ! Local ADP centre information

      nsec2(1) = 0    ! Length of section 2 (bytes). Must be set by user

      nsec3(1) = 0    ! Length of section 3, not used by BUFREN
      nsec3(2) = 0    ! Reserved
      nsec3(3) = 1    ! Number of subsets
      nsec3(4) = 128  ! Observed data, no compression

      nsec4(1) = 0    ! Length of section 4, not used by BUFREN

      IF (surface_type.EQ.1) THEN ! synop land low altitude
         ntdlen = 10
         ntdlst(1) = 307005
         ntdlst(2) = precip_desc
         ntdlst(3) = 13013
         ntdlst(4) = 222000
         ntdlst(5) = 101000 + num_data
         ntdlst(6) = 31031
         ntdlst(7) = 1031
         ntdlst(8) = 1201
         ntdlst(9) = 101000 + num_data
         ntdlst(10)= 33007
      ELSE IF (surface_type.EQ.2) THEN ! synop land high altitude
         ntdlen = 10
         ntdlst(1) = 307007
         ntdlst(2) = precip_desc
         ntdlst(3) = 13013
         ntdlst(4) = 222000
         ntdlst(5) = 101000 + num_data
         ntdlst(6) = 31031
         ntdlst(7) = 1031
         ntdlst(8) = 1201
         ntdlst(9) = 101000 + num_data
         ntdlst(10)= 33007
      ELSE IF (surface_type.EQ.3) THEN ! ship
         ntdlen = 9
         ntdlst(1) = 308004
         ntdlst(2) = 12005
         ntdlst(3) = 222000
         ntdlst(4) = 101000 + num_data
         ntdlst(5) = 31031
         ntdlst(6) = 1031
         ntdlst(7) = 1201
         ntdlst(8) = 101000 + num_data
         ntdlst(9) = 33007
      ELSE IF (surface_type.EQ.4) THEN ! dribu surface
         ntdlen = 8
         ntdlst(1) = 308003
         ntdlst(2) = 222000
         ntdlst(3) = 101000 + num_data
         ntdlst(4) = 31031
         ntdlst(5) = 1031
         ntdlst(6) = 1201
         ntdlst(7) = 101000 + num_data
         ntdlst(8) = 33007
      ELSE
C     No appropriate Hirlam template found for this message
         RETURN
      END IF

      ndlen = 0          ! No delayed replication
      nelem = 1000
      nvals = 1000

      CALL BUFREN(nsec0,nsec1,nsec2,
     +     nsec3,nsec4,ntdlen,ntdlst,
     +     ndlen,ndata,nelem,nvals,walues,cwals,
     +     kbufl,kbuff,kerr)
      IF (kerr.LT.0) CALL err_msg1('Error in BUFREN: KERR=',kerr)

      END SUBROUTINE encode_bufr

C     -----------------------------------------------------------------

      SUBROUTINE get_surface_values(ksub,kxelem,ktdexl,ktdexp,values,
     +     cvals,verbose,surface_type,walues,cwals,year,month,day,hour,
     +     minute,category,subcategory,precip_desc,num_data,
     +     skip_this_message)

C     Extract all values needed in any of the Hirlam surface
C     templates. Decide which template should be used based on which
C     station descriptors are present, then insert the values into
C     arrays walues and cwals. Also returns some parameters needed in
C     section 1 and 3.
      IMPLICIT NONE

      INTEGER ksub              ! Input: number of subset currently processed
      INTEGER kxelem            ! Input: expected (max) number of expanded elements
      INTEGER ktdexl            ! Input: number of entries in list of expanded data descriptors
      INTEGER ktdexp(*)         ! Input: array containing expanded data descriptors
      REAL values(*)            ! Input: expanded data values (one subset)
      CHARACTER*80 cvals(*)     ! Input: CCITTIA5 Bufr elements entries (one subset)
      integer num_data
      INTEGER verbose
      INTEGER surface_type      ! Output: Set to HIRLAM surface template that is to be used:
                                ! 1 = synop land low altitude
                                ! 2 = synop land high altitude
                                ! 3 = ship
                                ! 4 = dribu surface
      REAL walues(*)            ! Output: expanded data values for Hirlam surface template
      CHARACTER*80 cwals(*)     ! Output: CCITTIA5 data values for Hirlam surface template
      REAL year,month,day,hour,minute ! Output
      INTEGER category,subcategory    ! Output
      INTEGER precip_desc             ! Output

      REAL rvind                ! missing value for real data
      PARAMETER (rvind=1.7E38)

      CHARACTER*9 DDDD,missing
      REAL AA,CH,CL,CM,DD,FF,HL,
     +     NH,NN,PO,PP,PR,RR_3,RR_6,RR_12,RR_24,
     +     SA,TA,TD,TW,UU,VV,W1,W2,WW,
     +     latitude,longitude,height,vert_sign_first,
     +     II,iii,ix,a3,hhh,Ds,vs,TbTbTb,buoy_id5,buoy_id7,
     +     wmo_region,wmo_subregion,stateid,wigosid
c      REAL vert_sign(4),CC(4),HS(4),NS(4)
      REAL vert_sign(100),CC(100),HS(100),NS(100)
      INTEGER idx,cidx
      REAL hour_period
      INTEGER cloud_type_count  ! Will be increased by one for each 020012
                                ! (cloud type) encountered (0 initially)
      INTEGER num_cloud_layers  ! Number of individual cloud layers,
                                ! set to value of 031001 (delayed
                                ! descriptor) if this is met immediately
                                ! after a 020012 descriptor (-1 initially)
      LOGICAL bad_cloud_data    ! Set to true if something serious wrong is
                                ! found in cloud data. No more cloud
                                ! data will then be attempted decoded.
      INTEGER cloud_layer       ! Numbers the individual cloud layers
      LOGICAL surface_data      ! Set to false if 007062 ('Depth below sea/water
                                ! surface') is encountered with a value different
                                ! from 0
      LOGICAL quality_not_good  ! Set to true if 025053 is included with bit for
                                ! 'Good' not set
      LOGICAL time_of_last_position ! Set to true if 008021 time significance is
                                    ! included with value 26
      CHARACTER one_bits
      INTEGER desc,obs_quality,i
      REAL value
      LOGICAL skip_this_message
      LOGICAL date_and_position_ok ! function

      one_bits = CHAR(255)
      WRITE(missing,'(9A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits,one_bits,one_bits,one_bits,one_bits

      hour_period = rvind
      cloud_type_count = 0
      bad_cloud_data = .FALSE.
      num_cloud_layers = -1
      surface_data = .TRUE.
      quality_not_good = .FALSE.
      time_of_last_position = .FALSE.

C     Initialize all parameters to missing values
      DDDD = missing
      AA = rvind
      CH = rvind
      CL = rvind
      CM = rvind
      DD = rvind
      FF = rvind
      HL = rvind
      NH = rvind
      NN = rvind
      PO = rvind
      PP = rvind
      PR = rvind
      RR_3 = rvind
      RR_6 = rvind
      RR_12 = rvind
      RR_24 = rvind
      SA = rvind
      TA = rvind
      TD = rvind
      TW = rvind
      UU = rvind
      VV = rvind
      W1 = rvind
      W2 = rvind
      WW = rvind
      year = rvind
      month = rvind
      day = rvind
      hour = rvind
      minute = rvind
      latitude = rvind
      longitude = rvind
      height = rvind
      vert_sign_first = rvind
      II = rvind
      iii = rvind
      ix = rvind
      a3 = rvind
      hhh = rvind
      Ds = rvind
      vs = rvind
      TbTbTb = rvind
      buoy_id5 = rvind
      buoy_id7 = rvind
      wmo_region=rvind
      wmo_subregion=rvind
      stateid = rvind
      wigosid = rvind

      DO i=1,100
         vert_sign(i) = rvind
         CC(i) = rvind
         HS(i) = rvind
         NS(i) = rvind
      END DO

C     Loop through all expanded descriptors
      DO idx=1,ktdexl
         desc = ktdexp(idx)
         value = values(idx + (ksub-1)*kxelem)

C     The missing value returned from libbufr might not be exactly equal to rvind
         IF (ABS(value - rvind)/rvind.LE.0.001) value = rvind

         IF (desc.EQ.25053 .AND. value.NE.rvind) THEN ! Observation quality
            obs_quality = NINT(value)
C     Reject unless flag for GOOD is set
            IF (btest(obs_quality,12-1)) THEN
               IF (verbose.GT.2) write(*,*) '025053 Obs quality GOOD'
            ELSE
               IF (verbose.GT.2) write(*,*)
     +              '025053 Obs quality not good: ',obs_quality
               surface_type = 1 ! To get the appropriate verbose message later
               quality_not_good = .TRUE.
            END IF
         END IF

         IF (desc.EQ.4024) THEN ! Time period or displacement (hours)
            hour_period = value
         ELSE IF (desc.EQ.8021) THEN ! Time significance
            IF (NINT(value).EQ.26) THEN
               time_of_last_position = .TRUE.
            ELSE
               time_of_last_position = .FALSE.
            END IF
         ELSE IF (desc.EQ.1001) THEN ! WMO block number
            IF (value.NE.rvind .AND. II.EQ.rvind) THEN
               II = value
            END IF
         ELSE IF (desc.EQ.1002) THEN ! WMO station number
            IF (value.NE.rvind .AND. iii.EQ.rvind) THEN
               iii = value
            END IF
         ELSE IF (desc.EQ.1101) THEN ! state identifier
            IF (value.NE.rvind .AND. stateid.EQ.rvind) THEN
               stateid = value
            END IF
         ELSE IF (desc.EQ.1128) THEN ! wigos local identifier
            IF (value.NE.rvind .AND. wigosid.EQ.rvind) THEN
               wigosid = value
            END IF
         ELSE IF (desc.EQ.2001) THEN ! Type of station
            IF (value.NE.rvind .AND. ix.EQ.rvind) THEN
               ix = value
            END IF
         ELSE IF (desc.EQ.4001) THEN ! Year
            IF (value.NE.rvind .AND. year.EQ.rvind
     +           .AND. .NOT. time_of_last_position) THEN
               year = value
            END IF
         ELSE IF (desc.EQ.4002) THEN ! Month
            IF (value.NE.rvind .AND. month.EQ.rvind
     +           .AND. .NOT. time_of_last_position) THEN
               month = value
            END IF
         ELSE IF (desc.EQ.4003) THEN ! Day
            IF (value.NE.rvind .AND. day.EQ.rvind
     +           .AND. .NOT. time_of_last_position) THEN
               day = value
            END IF
         ELSE IF (desc.EQ.4004) THEN ! Hour
            IF (value.NE.rvind .AND. hour.EQ.rvind
     +           .AND. .NOT. time_of_last_position) THEN
               hour = value
            END IF
         ELSE IF (desc.EQ.4005) THEN ! Minute
            IF (value.NE.rvind .AND. minute.EQ.rvind
     +           .AND. .NOT. time_of_last_position) THEN
               minute = value
            END IF
         ELSE IF (desc.EQ.5001) THEN ! Latitude (high accuracy)
            IF (value.NE.rvind .AND. latitude.EQ.rvind) THEN
               latitude = value
            END IF
         ELSE IF (desc.EQ.5002) THEN ! Latitude (coarse accuracy)
            IF (value.NE.rvind .AND. latitude.EQ.rvind) THEN
               latitude = value
            END IF
         ELSE IF (desc.EQ.6001) THEN ! Longitude (high accuracy)
            IF (value.NE.rvind .AND. longitude.EQ.rvind) THEN
               longitude = value
            END IF
         ELSE IF (desc.EQ.6002) THEN ! Longitude (coarse accuracy)
            IF (value.NE.rvind .AND. longitude.EQ.rvind) THEN
               longitude = value
            END IF
         ELSE IF (desc.EQ.7001) THEN ! Height of station
            IF (value.NE.rvind .AND. height.EQ.rvind) THEN
               height = value
            END IF
         ELSE IF (desc.EQ.7030) THEN ! Height of station ground above mean sea level
            IF (value.NE.rvind .AND. height.EQ.rvind) THEN
               height = value
            END IF
         ELSE IF (desc.EQ.7031) THEN ! Height of barometer above mean sea level
                                     ! Use this for height (7001 in HIRLAM template)
                                     ! if available (rather than 7001 or 7030)
            IF (value.NE.rvind .AND. value.LT.9000.) THEN
               height = value
            END IF
         ELSE IF (desc.EQ.10004) THEN ! Pressure
            IF (value.NE.rvind .AND. PO.EQ.rvind) THEN
               PO = value
            END IF
         ELSE IF (desc.EQ.10051) THEN ! Pressure reduced to mean sea level
            IF (value.NE.rvind .AND. PR.EQ.rvind) THEN
               PR = value
            END IF
         ELSE IF (desc.EQ.10061) THEN ! 3-hour pressure change
            IF (value.NE.rvind .AND. PP.EQ.rvind) THEN
               PP = value
            END IF
         ELSE IF (desc.EQ.10063) THEN ! Characteristic of pressure tendency
            IF (value.NE.rvind .AND. AA.EQ.rvind) THEN
               AA = value
            END IF
         ELSE IF (desc.EQ.11011) THEN ! Wind direction at 10 m
            IF (value.NE.rvind .AND. DD.EQ.rvind) THEN
               DD = value
            END IF
         ELSE IF (desc.EQ.11001) THEN ! Wind direction
            IF (value.NE.rvind .AND. DD.EQ.rvind) THEN
               DD = value
            END IF
         ELSE IF (desc.EQ.11012) THEN ! Wind speed at 10 m
            IF (value.NE.rvind .AND. FF.EQ.rvind) THEN
               FF = value
            END IF
         ELSE IF (desc.EQ.11002) THEN ! Wind speed
            IF (value.NE.rvind .AND. FF.EQ.rvind) THEN
               FF = value
            END IF
         ELSE IF (desc.EQ.12104) THEN ! Dry bulb temperature at 2m (data width 16 bits)
            IF (value.NE.rvind .AND. TA.EQ.rvind) THEN
               TA = value
            END IF
         ELSE IF (desc.EQ.12004) THEN ! Dry bulb temperature at 2m (12 bits)
            IF (value.NE.rvind .AND. TA.EQ.rvind) THEN
               TA = value
            END IF
         ELSE IF (desc.EQ.12101) THEN ! Temperature/dry bulb temperature (16 bits)
            IF (value.NE.rvind .AND. TA.EQ.rvind) THEN
               TA = value
            END IF
         ELSE IF (desc.EQ.12001) THEN ! Temperature/dry bulb temperature (12 bits)
            IF (value.NE.rvind .AND. TA.EQ.rvind) THEN
               TA = value
            END IF
         ELSE IF (desc.EQ.12106) THEN ! Dew-point temperature at 2m (16 bits)
            IF (value.NE.rvind .AND. TD.EQ.rvind) THEN
               TD = value
            END IF
         ELSE IF (desc.EQ.12006) THEN ! Dew-point temperature at 2m (12 bits)
            IF (value.NE.rvind .AND. TD.EQ.rvind) THEN
               TD = value
            END IF
         ELSE IF (desc.EQ.12103) THEN ! Dew-point temperature (16 bits)
            IF (value.NE.rvind .AND. TD.EQ.rvind) THEN
               TD = value
            END IF
         ELSE IF (desc.EQ.12003) THEN ! Dew-point temperature (12 bits)
            IF (value.NE.rvind .AND. TD.EQ.rvind) THEN
               TD = value
            END IF
         ELSE IF (desc.EQ.13003) THEN ! Relative humidity
            IF (value.NE.rvind .AND. UU.EQ.rvind) THEN
               UU = value
            END IF
         ELSE IF (desc.EQ.20001) THEN ! Horizontal visibility
            IF (value.NE.rvind .AND. VV.EQ.rvind) THEN
               VV = value
            END IF
         ELSE IF (desc.EQ.20003) THEN ! Present weather
            IF (value.NE.rvind .AND. WW.EQ.rvind) THEN
               WW = value
            END IF
         ELSE IF (desc.EQ.20004) THEN ! Past weather (1)
            IF (value.NE.rvind .AND. W1.EQ.rvind) THEN
               W1 = value
            END IF
         ELSE IF (desc.EQ.20005) THEN ! Past weather (2)
            IF (value.NE.rvind .AND. W2.EQ.rvind) THEN
               W2 = value
            END IF
         ELSE IF (desc.EQ.20010) THEN ! Cloud cover (total)
            IF (value.NE.rvind .AND. NN.EQ.rvind) THEN
               NN = value
            END IF
         ELSE IF (desc.EQ.31001) THEN
C     Delayed descriptor replication factor; this should be
C     number of cloud layers if previous descriptor is cloud
C     type, according to all WMO recommended templates
            IF (ktdexp(idx - 1).EQ.20012) THEN
               IF (value.EQ.rvind) THEN
                  WRITE(*,*) 'WARNING: delayed descriptor replication'
     +                 // ' factor after 020012 undefined   !!!'
                  bad_cloud_data = .TRUE.
               ELSE
                  num_cloud_layers = NINT(value)
               END IF
            END IF
         ELSE IF (desc.EQ.8002 .AND. .NOT.bad_cloud_data) THEN ! Vertical significance (surface observations)
            IF (cloud_type_count.EQ.0) THEN ! First occurrence
               IF (value.NE.rvind .AND. vert_sign_first.EQ.rvind) THEN
                  vert_sign_first = value
               END IF
            ELSE IF (cloud_type_count.LT.3) THEN
               bad_cloud_data = .TRUE. ! There should always be three 020012
                                       ! following first 008002
            ELSE IF (num_cloud_layers.GT.-1) THEN
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LE.num_cloud_layers) THEN
                  IF (value.NE.rvind) THEN
                     vert_sign(cloud_layer) = value
                  END IF
               END IF
            ELSE                ! rdb-files always have 0 or 4 cloud layers
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LT.5) THEN
                  IF (value.NE.rvind) THEN
                     vert_sign(cloud_layer) = value
                  END IF
               END IF
            END IF
         ELSE IF (desc.EQ.20011 .AND. .NOT.bad_cloud_data) THEN ! Cloud amount
            IF (cloud_type_count.EQ.0) THEN ! First occurrence
               IF (value.NE.rvind .AND. NH.EQ.rvind) THEN
                  NH = value
               END IF
            ELSE IF (cloud_type_count.LT.3) THEN
               bad_cloud_data = .TRUE. ! There should always be three 020012
                                       ! following first 008002
            ELSE IF (num_cloud_layers.GT.-1) THEN
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LE.num_cloud_layers) THEN
                  IF (value.NE.rvind) THEN
                     NS(cloud_layer) = value
                  END IF
               END IF
            ELSE                ! rdb-files always have 0 or 4 cloud layers
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LT.5) THEN
                  IF (value.NE.rvind) THEN
                     NS(cloud_layer) = value
                  END IF
               END IF
            END IF
         ELSE IF (desc.EQ.20012 .AND. .NOT.bad_cloud_data) THEN ! Cloud type
            cloud_type_count = cloud_type_count + 1
            IF (cloud_type_count.GT.3) THEN
               cloud_layer = cloud_type_count - 3
               IF (num_cloud_layers .GT.-1) THEN
                  IF (cloud_layer.LE.num_cloud_layers) THEN
                     IF (value.NE.rvind) THEN
                        CC(cloud_layer) = value
                     END IF
                  END IF
               ELSE IF (cloud_layer.LT.5) THEN ! rdb-files always have 0 or 4 cloud layers
                  IF (value.NE.rvind) THEN
                     CC(cloud_layer) = value
                  END IF
               END IF
            ELSE
               IF (cloud_type_count.EQ.1) THEN
                  IF (value.NE.rvind .AND. CL.EQ.rvind) THEN
                     CL = value
                  END IF
               ELSE IF (cloud_type_count.EQ.2) THEN
                  IF (value.NE.rvind .AND. CM.EQ.rvind) THEN
                     CM = value
                  END IF
               ELSE IF (cloud_type_count.EQ.3) THEN
                  IF (value.NE.rvind .AND. CH.EQ.rvind) THEN
                     CH = value
                  END IF
               END IF
            END IF
         ELSE IF (desc.EQ.20013 .AND. .NOT.bad_cloud_data) THEN ! Height of base of cloud
            IF (cloud_type_count.EQ.0) THEN ! First occurrence
               IF (value.NE.rvind .AND. HL.EQ.rvind) THEN
                  HL = value
               END IF
C     Note that 020013 in individual cloud layers comes
C     AFTER 020012 and therefore must be treated
C     differently than 008002 and 020011
            ELSE IF (cloud_type_count.LT.4) THEN
               bad_cloud_data = .TRUE. ! There should always be three 020012
                                       ! following first 008002
            ELSE IF (num_cloud_layers.GT.-1) THEN
               cloud_layer = cloud_type_count - 3
               IF (cloud_layer.LE.num_cloud_layers) THEN
                  IF (value.NE.rvind) THEN
                     HS(cloud_layer) = value
                  END IF
               END IF
            ELSE ! rdb-files always have 0 or 4 cloud layers
               IF (value.NE.rvind) THEN
                  cloud_layer = cloud_type_count - 3
                  IF (cloud_layer.LT.5) THEN
                     IF (value.NE.rvind) THEN
                        HS(cloud_layer) = value
                     END IF
                  END IF
               END IF
            END IF
         ELSE IF (desc.EQ.13023) THEN ! Total precipitation past 24 hours
            IF (value.NE.rvind .AND. RR_24.EQ.rvind) THEN
               RR_24 = value
            END IF
         ELSE IF (desc.EQ.13022) THEN ! Total precipitation past 12 hours
            IF (value.NE.rvind .AND. RR_12.EQ.rvind) THEN
               RR_12 = value
            END IF
         ELSE IF (desc.EQ.13021) THEN ! Total precipitation past 6 hours
            IF (value.NE.rvind .AND. RR_6.EQ.rvind) THEN
               RR_6 = value
            END IF
         ELSE IF (desc.EQ.13020) THEN ! Total precipitation past 3 hours
            IF (value.NE.rvind .AND. RR_3.EQ.rvind) THEN
               RR_3 = value
            END IF
         ELSE IF (desc.EQ.13011) THEN ! Total precipitation/total water equivalent
            IF (value.NE.rvind .AND. hour_period.NE.rvind) THEN
               IF (NINT(hour_period).EQ.-24) THEN
                  RR_24 = value
               ELSE IF (NINT(hour_period).EQ.-12) THEN
                  RR_12 = value
               ELSE IF (NINT(hour_period).EQ.-6) THEN
                  RR_6 = value
               ELSE IF (NINT(hour_period).EQ.-3) THEN
                  RR_3 = value
               END IF
            END IF
         ELSE IF (desc.EQ.13013) THEN ! Total snow depth
            IF (value.NE.rvind .AND. SA.EQ.rvind) THEN
               SA = value
            END IF
         ELSE IF (desc.EQ.20062) THEN ! State of the ground (with or without snow)
            IF (value.NE.rvind .AND. value.LE.10 .AND. SA.EQ.rvind) THEN
               SA = 0
            END IF
C     Special for high altitude stations
         ELSE IF (desc.EQ.7004) THEN ! Pressure (location class)
            IF (value.NE.rvind .AND. a3.EQ.rvind) THEN
               a3 = value
            END IF
         ELSE IF (desc.EQ.10009) THEN ! Geopotential height
            IF (value.NE.rvind .AND. hhh.EQ.rvind) THEN
               hhh = value
            END IF
         ELSE IF (desc.EQ.10008) THEN ! Geopotential (20 bits)
            IF (value.NE.rvind .AND. hhh.EQ.rvind) THEN
               hhh = value * 9.8
            END IF
         ELSE IF (desc.EQ.10003) THEN ! Geopotential (17 bits)
            IF (value.NE.rvind .AND. hhh.EQ.rvind) THEN
               hhh = value * 9.8
            END IF
C     Special for ship or marine stations (and buoys)
         ELSE IF (desc.EQ.1011) THEN  ! Ship or mobile land station identifier
            IF (value.NE.rvind) THEN
               cidx = int(value/1000)
               DDDD = cvals(cidx) ! CCITTIA5 data
            END IF
         ELSE IF (desc.EQ.1012) THEN ! Direction of motion of moving observing platform
            IF (value.NE.rvind .AND. Ds.EQ.rvind) THEN
               Ds = value
            END IF
         ELSE IF (desc.EQ.1013) THEN ! Speed of motion of moving observing platform
            IF (value.NE.rvind .AND. vs.EQ.rvind) THEN
               vs = value
            END IF
         ELSE IF (desc.EQ.22049) THEN ! Sea-surface temperature
            IF (value.NE.rvind .AND. surface_data) THEN
               TW = value
            END IF
         ELSE IF (desc.EQ.22043) THEN ! Sea/water temperature (15 bits)
            IF (value.NE.rvind .AND. surface_data) THEN
               TW = value
            END IF
         ELSE IF (desc.EQ.22042) THEN ! Sea/water temperature (12 bits)
            IF (value.NE.rvind .AND. TW.EQ.rvind
     +           .AND. surface_data) THEN
               TW = value
            END IF
         ELSE IF (desc.EQ.12102) THEN ! Wet-bulb temperature (16 bits)
            IF (value.NE.rvind .AND. TbTbTb.EQ.rvind) THEN
               TbTbTb = value
            END IF
         ELSE IF (desc.EQ.12005) THEN ! Wet-bulb temperature (12 bits)
            IF (value.NE.rvind .AND. TbTbTb.EQ.rvind) THEN
               TbTbTb = value
            END IF
C     Special for buoys
         ELSE IF (desc.EQ.1087) THEN ! WMO marine observing platform extended identifier
            IF (value.NE.rvind .AND. buoy_id7.EQ.rvind) THEN
               buoy_id7 = value
            END IF
         ELSE IF (desc.EQ.1005) THEN ! Buoy/platform identifier
            IF (value.NE.rvind .AND. buoy_id5.EQ.rvind) THEN
               buoy_id5 = value
            END IF
         ELSE IF (desc.EQ.1003) THEN ! WMO region number/geographical area
            IF (value.NE.rvind .AND. wmo_region.EQ.rvind) THEN
               wmo_region = value
            END IF
         ELSE IF (desc.EQ.1020) THEN ! WMO region sub-area
            IF (value.NE.rvind .AND. wmo_subregion.EQ.rvind) THEN
               wmo_subregion = value
            END IF
         ELSE IF (desc.EQ.7062) THEN ! Depth below sea/water surface
            IF (value.NE.rvind) THEN
C     Some buoy reports starts with depth 1.5 m, others starts with 0 m
C     then 1.5 m and always have same sea/water temperature for these 2
C     levels, so it seems like 0 m should be considered equivalent with 1.5 m
               IF (value.LT.1.6) THEN
                  surface_data = .TRUE.
               ELSE
                  surface_data = .FALSE.
               END IF
            END IF
         END IF

      END DO

C     The precipitation descriptor to include for surface land
      IF (RR_24.NE.rvind) THEN
         precip_desc = 13023
      ELSE IF (RR_12.NE.rvind) THEN
         precip_desc = 13022
      ELSE IF (RR_6.NE.rvind) THEN
         precip_desc = 13021
      ELSE IF (RR_3.NE.rvind) THEN
         precip_desc = 13020
      ELSE
         precip_desc = 13023
      END IF

      IF (.NOT.date_and_position_ok(latitude,longitude,
     +     year,month,day,hour,minute)) THEN
         skip_this_message = .TRUE.
         RETURN
      END IF

C     If BUFR message contains nationalnr or wigosid instead of wmonr,
C     we have to make a fake wmonr (which can be reused, since the model
C     doesn't care if same wmonr is used more than once)
      IF (II.EQ.rvind .AND. stateid.NE.rvind) THEN
         IF (NINT(stateid).EQ.633) THEN      ! Norway
            II = 1.0
         ELSE IF (NINT(stateid).EQ.643) THEN ! Sweden
            II = 2.0
         ELSE IF (NINT(stateid).EQ.611) THEN ! Denmark
            II = 6.0
         ELSE
            II = 99.0
         END IF
         iii = 0.0
      ELSE IF (II.EQ.rvind .AND. wigosid.NE.rvind) THEN
         II = 98.0
         iii = 0.0
      END IF

      IF (quality_not_good) THEN
C     Keep message only if containing (non zero) snow depth, in which
C     case all other observations are excluded
         IF (SA.EQ.rvind .OR. SA.LE.0) THEN
            skip_this_message = .TRUE.
            RETURN
         END IF
         IF (verbose.GT.2) write(*,*) 'Including SA only'
         AA = rvind
         CH = rvind
         CL = rvind
         CM = rvind
         DD = rvind
         FF = rvind
         HL = rvind
         NH = rvind
         NN = rvind
         PO = rvind
         PP = rvind
         PR = rvind
         RR_3 = rvind
         RR_6 = rvind
         RR_12 = rvind
         RR_24 = rvind
C     SA = rvind
         TA = rvind
         TD = rvind
         TW = rvind
         UU = rvind
         VV = rvind
         W1 = rvind
         W2 = rvind
         WW = rvind
         vert_sign_first = rvind
         a3 = rvind
         hhh = rvind
         Ds = rvind
         vs = rvind
         TbTbTb = rvind
         DO i=1,100
            vert_sign(i) = rvind
            CC(i) = rvind
            HS(i) = rvind
            NS(i) = rvind
         END DO
      END IF

      IF (II.NE.rvind) THEN
         category = 0           ! Surface data - land
         subcategory = 1        ! synop land - surface
         IF (a3.EQ.rvind) THEN
            surface_type = 1    ! synop land low altitude
            CALL low_altitude_data(II,iii,ix,year,month,day,hour,
     +           minute,latitude,longitude,height,PO,PR,PP,AA,
     +           DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +           vert_sign,NH,HL,CL,CM,CH,NS,CC,HS,RR_3,RR_6,RR_12,
     +           RR_24,SA,rvind,verbose,walues,num_data)
         ELSE
            surface_type = 2    ! synop land high altitude
            CALL high_altitude_data(II,iii,ix,year,month,day,hour,
     +           minute,latitude,longitude,height,PO,a3,hhh,PP,AA,
     +           DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +           vert_sign,NH,HL,CL,CM,CH,NS,CC,HS,RR_3,RR_6,RR_12,
     +           RR_24,SA,rvind,verbose,walues,num_data)
         END IF
      ELSE IF (DDDD.NE.missing) THEN
         category = 1           ! Surface data - sea
         subcategory = 11       ! ship
         surface_type = 3       ! ship
         CALL ship_data(DDDD,Ds,vs,ix,year,month,day,hour,
     +        minute,latitude,longitude,PO,PR,PP,AA,
     +        DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +        NH,HL,CL,CM,CH,TW,TbTbTb,rvind,verbose,
     +        walues,cwals,num_data)
      ELSE IF (buoy_id5.NE.rvind .OR. buoy_id7.NE.rvind) THEN
         category = 1           ! Surface data - sea
         subcategory = 21       ! dribu - surface
         surface_type = 4       ! dribu surface
         CALL dribu_data(buoy_id5,buoy_id7,wmo_region,wmo_subregion,
     +        Ds,vs,ix,year,month,day,hour,
     +        minute,latitude,longitude,PO,PR,PP,AA,
     +        DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +        NH,HL,CL,CM,CH,TW,rvind,verbose,walues,num_data)
      ELSE
         surface_type = 0
         IF (verbose.GT.0) THEN
            WRITE(*,*) 'Not a surface report. Skipping this'
         END IF
      END IF

       IF (verbose.GT.3) THEN
          WRITE(*,*) II,iii,ix,year,month,day,hour,
     +         minute,latitude,longitude,height,PO,PR,PP,AA,
     +         DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +         vert_sign,NH,HL,CL,CM,CH,NS,CC,HS,RR_3,RR_6,RR_12,
     +         RR_24,SA,a3,hhh,DDDD,Ds,vs,
     +         TbTbTb,TW,buoy_id5,buoy_id7,wmo_region,wmo_subregion
          WRITE(*,*) surface_type,category,subcategory,precip_desc
       END IF

      END SUBROUTINE get_surface_values

C     -----------------------------------------------------------------

      SUBROUTINE low_altitude_data(II,iii,ix,year,month,day,hour,
     +           minute,latitude,longitude,height,PO,PR,PP,AA,
     +           DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +           vert_sign,NH,HL,CL,CM,CH,NS,CC,HS,RR_3,RR_6,RR_12,
     +           RR_24,SA,rvind,verbose,walues,num_data)
      IMPLICIT NONE

      REAL II,iii,ix,year,month,day,hour,
     +     minute,latitude,longitude,height,vert_sign_first,
     +     PO,PR,PP,AA,DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,NH,HL,CL,CM,
     +     CH,RR_3,RR_6,RR_12,RR_24,SA,rvind
      REAL vert_sign(*),NS(*),CC(*),HS(*)
      INTEGER verbose
      REAL walues(*)            ! Output
      INTEGER num_data          ! Output

      walues(1) = II
      walues(2) = iii
      walues(3) = ix
      walues(4) = year
      walues(5) = month
      walues(6) = day
      walues(7) = hour
      walues(8) = minute
      walues(9) = latitude
      walues(10) = longitude
      walues(11) = height
      walues(12) = PO
      walues(13) = PR
      walues(14) = PP
      walues(15) = AA
      walues(16) = DD
      walues(17) = FF
      walues(18) = TA
      walues(19) = TD
      walues(20) = UU
      walues(21) = VV
      walues(22) = WW
      walues(23) = W1
      walues(24) = W2
      walues(25) = NN
      walues(26) = vert_sign_first
      walues(27) = NH
      walues(28) = HL
      walues(29) = CL
      walues(30) = CM
      walues(31) = CH
      walues(32) = vert_sign(1)
      walues(33) = NS(1)
      walues(34) = CC(1)
      walues(35) = HS(1)
      walues(36) = vert_sign(2)
      walues(37) = NS(2)
      walues(38) = CC(2)
      walues(39) = HS(2)
      walues(40) = vert_sign(3)
      walues(41) = NS(3)
      walues(42) = CC(3)
      walues(43) = HS(3)
      walues(44) = vert_sign(4)
      walues(45) = NS(4)
      walues(46) = CC(4)
      walues(47) = HS(4)

      IF (RR_24.NE.rvind) THEN
         walues(48) = RR_24
      ELSE IF (RR_12.NE.rvind) THEN
         walues(48) = RR_12
      ELSE IF (RR_6.NE.rvind) THEN
         walues(48) = RR_6
      ELSE IF (RR_3.NE.rvind) THEN
         walues(48) = RR_3
      ELSE
C     No precipitation, use default RR_24 (set to missing value)
         walues(48) = RR_24
      END IF
      walues(49) = SA

C     Last descriptor used at met.no is 002002 TYPE OF INSTRUMENTATION
C     FOR WIND MEASUREMENT (iw), but this is not part of the original
C     HIRLAM template, so is left out

      IF (verbose.GT.2) WRITE(*,*) 'is low_altitude_data'
      num_data=49
      call add_quality_info(rvind,num_data,walues)

      END SUBROUTINE low_altitude_data

C     -----------------------------------------------------------------

      SUBROUTINE high_altitude_data(II,iii,ix,year,month,day,hour,
     +           minute,latitude,longitude,height,PO,a3,hhh,PP,AA,
     +           DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +           vert_sign,NH,HL,CL,CM,CH,NS,CC,HS,RR_3,RR_6,RR_12,
     +           RR_24,SA,rvind,verbose,walues,num_data)
      IMPLICIT NONE

      REAL II,iii,ix,year,month,day,hour,
     +     minute,latitude,longitude,height,vert_sign_first,
     +     PO,a3,hhh,PP,AA,DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,NH,HL,CL,CM,
     +     CH,RR_3,RR_6,RR_12,RR_24,SA,rvind
      REAL vert_sign(*),NS(*),CC(*),HS(*)
      INTEGER verbose
      REAL walues(*)            ! Output
      INTEGER num_data          ! Output

      walues(1) = II
      walues(2) = iii
      walues(3) = ix
      walues(4) = year
      walues(5) = month
      walues(6) = day
      walues(7) = hour
      walues(8) = minute
      walues(9) = latitude
      walues(10) = longitude
      walues(11) = height
      walues(12) = PO
      walues(13) = a3
      walues(14) = hhh
      walues(15) = PP
      walues(16) = AA
      walues(17) = DD
      walues(18) = FF
      walues(19) = TA
      walues(20) = TD
      walues(21) = UU
      walues(22) = VV
      walues(23) = WW
      walues(24) = W1
      walues(25) = W2
      walues(26) = NN
      walues(27) = vert_sign_first
      walues(28) = NH
      walues(29) = HL
      walues(30) = CL
      walues(31) = CM
      walues(32) = CH
C     Note that there are no cloud layers in the high altitude
C     template (in the low altitude template there are 4 layers)

      IF (RR_24.NE.rvind) THEN
         walues(33) = RR_24
      ELSE IF (RR_12.NE.rvind) THEN
         walues(33) = RR_12
      ELSE IF (RR_6.NE.rvind) THEN
         walues(33) = RR_6
      ELSE IF (RR_3.NE.rvind) THEN
         walues(33) = RR_3
      ELSE
C     No precipitation, use default RR_24 (set to missing value)
         walues(33) = RR_24
      END IF
      walues(34) = SA

C     Last descriptor used at met.no is 002002 TYPE OF INSTRUMENTATION
C     FOR WIND MEASUREMENT (iw), but this is not part of the original
C     HIRLAM template, so is left out

      IF (verbose.GT.2) write(*,*) 'is high_altitude_data'
      num_data=34
      call add_quality_info(rvind,num_data,walues)

      END SUBROUTINE high_altitude_data

C     -----------------------------------------------------------------

      SUBROUTINE ship_data(DDDD,Ds,vs,ix,year,month,day,hour,
     +     minute,latitude,longitude,PO,PR,PP,AA,
     +     DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +     NH,HL,CL,CM,CH,TW,TbTbTb,rvind,verbose,walues,cwals,num_data)
      IMPLICIT NONE

      CHARACTER*9 DDDD
      REAL Ds,vs,ix,year,month,day,hour,
     +     minute,latitude,longitude,height,vert_sign_first,
     +     PO,PR,PP,AA,DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,NH,HL,CL,CM,
     +     CH,Tw,TbTbTb,rvind
      INTEGER verbose
      REAL walues(*)            ! Output
      CHARACTER*80 cwals(*)     ! Output: CCITTIA5 data values for ship template
      INTEGER num_data          ! Output

      walues(1) = 1009
      cwals(1) = DDDD
      walues(2) = Ds
      walues(3) = vs
      walues(4) = ix
      walues(5) = year
      walues(6) = month
      walues(7) = day
      walues(8) = hour
      walues(9) = minute
      walues(10) = latitude
      walues(11) = longitude
      walues(12) = PO
      walues(13) = PR
      walues(14) = PP
      walues(15) = AA
      walues(16) = DD
      walues(17) = FF
      walues(18) = TA
      walues(19) = TD
      walues(20) = UU
      walues(21) = VV
      walues(22) = WW
      walues(23) = W1
      walues(24) = W2
      walues(25) = NN
      walues(26) = vert_sign_first
      walues(27) = NH
      walues(28) = HL
      walues(29) = CL
      walues(30) = CM
      walues(31) = CH
      walues(32) = TW
      walues(33) = TbTbTb

C     Last descriptor used at met.no is 002002 TYPE OF INSTRUMENTATION
C     FOR WIND MEASUREMENT (iw), but this is not part of the original
C     HIRLAM template, so is left out

      IF (verbose.GT.2) write(*,*) 'is ship'
      num_data=33
      call add_quality_info(rvind,num_data,walues)

      END SUBROUTINE ship_data

C     -----------------------------------------------------------------

      SUBROUTINE dribu_data(buoy_id5,buoy_id7,wmo_region,wmo_subregion,
     +           Ds,vs,ix,year,month,day,hour,
     +           minute,latitude,longitude,PO,PR,PP,AA,
     +           DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +           NH,HL,CL,CM,CH,TW,rvind,verbose,walues,num_data)
      IMPLICIT NONE

      REAL buoy_id5,buoy_id7,wmo_region,wmo_subregion,
     +     Ds,vs,ix,year,month,day,hour,
     +     minute,latitude,longitude,PO,PR,PP,AA,
     +     DD,FF,TA,TD,UU,VV,WW,W1,W2,NN,vert_sign_first,
     +     NH,HL,CL,CM,CH,TW,rvind
      INTEGER verbose
      INTEGER num_data          ! Output
      REAL walues(*)            ! Output
      INTEGER buoy_number
      REAL buoy_id

      buoy_id = rvind

C     In Hirlam template buoy_id is expected to include wmo_region and wmo_subregion
      IF (buoy_id5.NE.rvind) THEN
         IF (wmo_region.EQ.rvind .AND. wmo_subregion.EQ.rvind) THEN
C     This is probably a BUFR message already using Hirlam template!
            buoy_id = buoy_id5
         ELSE IF (buoy_id5.LT.1000 .AND.
     +           wmo_region.NE.rvind .AND. wmo_subregion.NE.rvind) THEN
            buoy_id = wmo_region*10000 + wmo_subregion*1000 + buoy_id5
         ELSE
C     Usually this means buoy number > 999, not a possible value when
C     Hirlam template was made (illegal in TAC BUOY)
            buoy_id = 99999.0
         END IF
      END IF

C     If bouy_id is fetched from the 7 digit 001087, some magic needs to
C     be done to convert it into the 5 digit 001005 used in Hirlam template
      IF (buoy_id7.NE.rvind .AND. buoy_id7.GT.100000) THEN ! e.g. 6300561 or 6901459
         buoy_number = MOD(NINT(buoy_id7),100000)          ! 00561 or 01459
         IF (buoy_number.LT.1000) THEN
            buoy_id = NINT((buoy_id7-buoy_number)/100) + buoy_number ! 63561
         ELSE
C     Impossible to compress to 5 digits. Use a hard coded value for this case
            buoy_id = 99999.0
         END IF
      END IF

      walues(1) = buoy_id
      walues(2) = Ds
      walues(3) = vs
      walues(4) = ix
      walues(5) = year
      walues(6) = month
      walues(7) = day
      walues(8) = hour
      walues(9) = minute
      walues(10) = latitude
      walues(11) = longitude
      walues(12) = PO
      walues(13) = PR
      walues(14) = PP
      walues(15) = AA
      walues(16) = DD
      walues(17) = FF
      walues(18) = TA
      walues(19) = TD
      walues(20) = UU
      walues(21) = VV
      walues(22) = WW
      walues(23) = W1
      walues(24) = W2
      walues(25) = NN
      walues(26) = vert_sign_first
      walues(27) = NH
      walues(28) = HL
      walues(29) = CL
      walues(30) = CM
      walues(31) = CH
      walues(32) = TW

      IF (verbose.GT.2) write(*,*) 'is dribu'
      num_data=32
      call add_quality_info(rvind,num_data,walues)

      END SUBROUTINE dribu_data

C     -----------------------------------------------------------------

      LOGICAL FUNCTION date_and_position_ok(latitude,longitude,
     +        year,month,day,hour,minute)
C     Returns TRUE if date, time, lat, lon are present with physical
C     valid values, else returns FALSE
      IMPLICIT NONE

      REAL latitude,longitude,year,month,day,hour,minute,altitude,PO ! Input
      LOGICAL ok

      ok = .TRUE.

      IF (latitude.LT.-90.0.OR.latitude.GT.90.0) THEN
         ok = .FALSE.
      ELSE IF (longitude.LT.-180.0.OR.longitude.GT.180.0) THEN
         ok = .FALSE.
      ELSE IF (year.LT.1900.0.OR.year.GT.2100.0) THEN
         ok = .FALSE.
      ELSE IF (month.LT.1.0.OR.month.GT.12.0) THEN
         ok = .FALSE.
      ELSE IF (day.LT.1.0.OR.day.GT.31.0) THEN
         ok = .FALSE.
      ELSE IF (hour.LT.0.0.OR.hour.GT.23.0) THEN
         ok = .FALSE.
      ELSE IF (minute.LT.0.0.OR.minute.GT.59.0) THEN
         ok = .FALSE.
      END IF

      date_and_position_ok = ok

      END FUNCTION date_and_position_ok

C     -----------------------------------------------------------------

      SUBROUTINE err_msg(msg)
      IMPLICIT NONE
      CHARACTER(len = *) msg
      WRITE(*,*) msg
      STOP
      END SUBROUTINE err_msg

C     -----------------------------------------------------------------

      SUBROUTINE err_msg1(msg,kerr)
      IMPLICIT NONE
      CHARACTER(len = *) msg
      INTEGER kerr
      WRITE(*,*) msg,kerr
      STOP
      END SUBROUTINE err_msg1

C     -----------------------------------------------------------------

      SUBROUTINE add_quality_info(rvind,num_data,walues)
C     For now we simply set confidence value for all data values present
C     to 70%, providing quality value (set to missing) also for missing
C     data values. Later quality routines should be added.
      IMPLICIT NONE

      REAL rvind        ! Input: missing value for real data
      INTEGER num_data  ! Input: number of data values
      REAL walues(*)    ! Input/output: expanded data values for acars template

      INTEGER start_dpi ! Index of last data value before the data present
                        ! indicators (dpi) starts
      INTEGER start_cv  ! Index of last data value before the
                        ! confidence values (cv) starts
      INTEGER i

C     222000 needs a value! - this is just how BUFREN works
      walues(num_data + 1) = 0

C     Generating centre and application
      walues(2*num_data + 2) = 98 ! ECMWF
      walues(2*num_data + 3) = 1  ! PREPROCESSING CONSISTENCY CHECK

      start_dpi = num_data + 1
      start_cv = start_dpi + num_data + 2

C     Set data present indicator (031031) and confidence values (033007)
      DO i=1,num_data
         walues(start_dpi + i) = 0 ! DATA PRESENT
         IF (walues(i).NE.rvind) THEN
            walues(start_cv + i) = 70.0
         ELSE
            walues(start_cv + i) = rvind
         END IF
      END DO

      END SUBROUTINE add_quality_info
