c  NwpPreprocObs - Free source codes and example scripts for preprocessing
c                  of meteorological observations

c  $Id: amdarbufr2hirbufr.F,v 1.3 2011/09/23 13:16:34 a001723 Exp $

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

C Usage: amdarbufr2hirbufr <bufr file> <outfile> [--verbose n]

C Purpose: Convert the amdar messages in <bufr file> into HIRLAM
C BUFR templates 311001 or 311193, using BUFR edition 3. Print the
C converted messages to <outfile>.

C The message is considered an amdar if data category is 4 and one
C of the descriptors 001006 and 001008 is present with defined
C value. Uses HIRLAM BUFR (AMDAR) template 311001 and setting BUFR
C message subtype to 144 if high accuracy is used for position, else
C (if course accuracy for position) uses ACARS template 311193,
C setting BUFR message subtype to 145.

C Confidence values (all set to 70%) are included for template 311001.


C Remarks:

C The Hirlam AMDAR template (subtype 145) does not contain
C 001008. If the BUFR amdar contains 001008 only, the value of this
C will be inserted for the 001006 descriptor (i.e. we are cheating
C here to be able to identify the aircraft).

C Version number of Master table used in encoded message is the
C fairly new 13, to be able to use the ACARS template descriptor
C 311193 in section 3.

C If an error in libbufr occurs while decoding (e.g. if the table is
C not found in BUFR_TABLES), the message will be skipped, with an
C error message written to STDOUT (should we use STDERR instead?).

C The date and time parameters used in section 1 in the converted
C BUFR messages are fetched from section 4 of the individual
C observations (subsets) of the original BUFR message.

C The Hirlam templates do not include 004006 SECOND and the ACARS
C template uses coarse accuracy for latitude and longitude (005002
C and 006002), which means that rapid observations made at
C ascents/descents may easily end up in the filtered messages as
C duplicates (same station, date/time and position).

C Data present indicator is set to 0 for all data, also missing data
C (for which confidence value is set to missing value).


      PROGRAM amdarbufr2hirbufr

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
                                   ! 3: subset level (not used yet)
                                   ! 4: you probably don't want to use this
      INTEGER nlibbufr_errors      ! Number of errors encountered in libbufr calls

      INTEGER ilen                 ! Size in BYTES of the BUFR product read
      INTEGER wlen                 ! Size in WORDS of the BUFR product read (one word = 4 bytes)
      INTEGER iunit,ounit
      INTEGER iarg,iloop,iret
      LOGICAL eof

      Usage = '\nUsage: amdarbufr2hirbufr <bufr file> <outfile>'
     +     // ' [--verbose n] \n'

      iarg = IARGC()
      IF (iarg.NE.2 .AND. iarg.NE.4) THEN
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

      END PROGRAM amdarbufr2hirbufr

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

      SUBROUTINE process_bufr(ilen,ibuff,ounit,verbose,nlibbufr_errors)
C     Decode BUFR message into fully decoded form, then for each subset
C     encode a BUFR message using the Hirlam templates and write the
C     encoded BUFR message to file.

      IMPLICIT NONE

      INTEGER ilen              ! Input: Size in BYTES of the BUFR message read
      INTEGER ibuff(*)          ! Input: Buffer to hold BUFR message
      INTEGER ounit             ! Input: unit to write BUFR message to
      INTEGER verbose           ! Input
      INTEGER nlibbufr_errors   ! Output: Number of errors encountered in libbufr calls

      INTEGER kxelem            ! Expected (max) number of expanded elements
      INTEGER ksub,kerr

      LOGICAL is_amdar          ! Set to TRUE if at least one of 001006 or 001008
                                ! is present with non missing value
      REAL walues(60)           ! expanded data values for Hirlam template
      CHARACTER*80 cwals(2)     ! CCITTIA5 data values for Hirlam template
      REAL year,month,day,hour,minute ! Output from get_amdar_values
      INTEGER num_data          ! Output from get_amdar_values

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
      CHARACTER*64 cnames(kelem) ! Bufr Table B element names
      CHARACTER*24 cunits(kelem) ! Bufr Table B units
      CHARACTER*80 cvals(kelem) ! CCITTIA5 Bufr elements entries (one subset)
      REAL values(kvals)        ! expanded data values (one subset)

C     BUSEL variables
      INTEGER ktdlst(kelem)     ! array containing data descriptors in section 3
      INTEGER ktdexp(kelem)     ! array containing expanded data descriptors
      INTEGER ktdlen            ! number of data descriptors in section 3
      INTEGER ktdexl            ! number of entries in list of expanded data descriptors

      INTEGER update_sequence_number
      INTEGER subcategory
      LOGICAL skip_this_message

      kerr = 0

C     Using parameter kelem in call to BUFREX might be too big for
C     multisubset messages. Have copied the method used in decode_bufr.F
C     in libbufr, first calling BUS012 in order to get number of subsets
C     ksup(6)
      CALL BUS012(ilen,ibuff,ksup,ksec0,ksec1,ksec2,kerr)
      IF (kerr.NE.0) THEN
         WRITE(*,*) 'ERROR IN BUS012: KERR= ',kerr
         nlibbufr_errors = nlibbufr_errors + 1
         RETURN
      END IF
      kxelem = kvals/ksup(6)
      IF (kxelem.GT.kelem) kxelem = kelem

      CALL BUFREX (ilen,ibuff,ksup,ksec0,ksec1,ksec2,
     +     ksec3,ksec4,kxelem,cnames,cunits,kvals,values,
     +     cvals,kerr)
      IF (kerr.NE.0) THEN
         WRITE(*,*) 'ERROR IN BUFREX: KERR= ',kerr
         nlibbufr_errors = nlibbufr_errors + 1
         RETURN
      END IF

C     Convert messages with data category (BUFR table A) 4 only.
C     4 = Single level upper air data (other than satellite)
      IF (ksec1(6).NE.4) RETURN

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
         CALL get_amdar_values(ksub,kxelem,ktdexl,ktdexp,values,
     +     cvals,verbose,is_amdar,walues,cwals,year,month,day,hour,
     +     minute,num_data,subcategory,skip_this_message)

         IF (skip_this_message) THEN
            IF (verbose.GT.0) THEN
               WRITE(*,*) 'Data contains nonsense values'
     +              // ' and will be ignored'
            END IF
            CYCLE subset_loop
         END IF

         IF (is_amdar) THEN
            CALL encode_bufr(walues,cwals,NINT(year),NINT(month),
     +           NINT(day),NINT(hour),NINT(minute),num_data,
     +           update_sequence_number,subcategory,kbuff,kbufl)
            CALL PBWRITE(ounit,kbuff,kbufl*4,kerr)
            IF (kerr.LT.0) CALL err_msg1('Error in PBWRITE: KERR=',
     +           kerr)
         END IF
      END DO subset_loop

      END SUBROUTINE process_bufr

C     -----------------------------------------------------------------

      SUBROUTINE encode_bufr(walues,cwals,year,month,day,hour,minute,
     +     num_data,update_sequence_number,subcategory,kbuff,kbufl)
C     Encode BUFR message by calling BUFREN.
      IMPLICIT NONE

      REAL walues(*)            ! Input: expanded data values for acars template
      CHARACTER*80 cwals(*)     ! Input: CCITTIA5 data values for acars template
      INTEGER year,month,day,hour,minute,update_sequence_number,
     +     subcategory
                                ! Input: needed in section 1
      INTEGER num_data          ! Input: number of data values, needed in QC part in section 3
      INTEGER kbuff(*)          ! Output: integer array containing encoded bufr message
      INTEGER kbufl             ! Output: length of encoded bufr message (words)


C     BUFREN variables. See corresponding k.... variables in subroutine
C     process_bufr for explanation
      INTEGER nsec0(3),nsec1(40),nsec2(4096),nsec3(4),nsec4(2),
     +     ntdlen,ntdlst(8),nelem,nvals
      INTEGER ndlen             ! integer (dimension of kdata array)
      INTEGER ndata(1)          ! integer array containing data needed for data
                                ! descriptor expansion (delayed replication factors)
                                ! which appear in the values array
      INTEGER kerr

      nsec0(1) = 0    ! Length of section 0, will be set by BUFREN
      nsec0(2) = 0    ! Total length of BUFR message, will be set by BUFREN
      nsec0(3) = 3    ! BUFR edition number

      nsec1(1) = 18   ! Length of section 1 (bytes). Must be set by user
      nsec1(2) = 3    ! BUFR edition number
      nsec1(3) = 98   ! Originating centre (98=ECMWF. Should we use 88=Oslo instead?)
      nsec1(4) = update_sequence_number
      nsec1(5) = 0    ! No section 2 (optional section)
      nsec1(6) = 4    ! Single level upper air data (other than satellite)
      nsec1(7) = subcategory  ! 144 or 145
      nsec1(8) = 0    ! Version number of local table used
      nsec1(9) = year ! Should be year of century in edition 3 (100 for new millennia)
      IF (year.GT.100) nsec1(9) = mod(year,100)
      IF (nsec1(9).EQ.0) nsec1(9) = 100
      nsec1(10) = month
      nsec1(11) = day
      nsec1(12) = hour
      nsec1(13) = minute
      nsec1(14) = 0   ! BUFR Master table used
      nsec1(15) = 13  ! Version number of Master table used
      nsec1(16) = 0   ! Originating sub-centre
      nsec1(17) = 0   ! International subcategory (not used for BUFR edition 3)
      nsec1(18) = 0   ! Second (not used for BUFR edition 3)
      nsec1(19) = 0   ! Local ADP centre information

      nsec2(1) = 0    ! Length of section 2 (bytes). Must be set by user

      nsec3(1) = 0    ! Length of section 3, will be set by BUFREN
      nsec3(2) = 0    ! Reserved
      nsec3(3) = 1    ! Number of subsets
      nsec3(4) = 128  ! Observed data, no compression

      nsec4(1) = 0    ! Length of section 4, will be set by BUFREN

      IF (subcategory.EQ.144) THEN
         ntdlen = 8
         ntdlst(1) = 311001     ! The AMDAR/AIREP template
         ntdlst(2) = 222000     ! Quality information follows
         ntdlst(3) = 101000 + num_data ! Replication factor for 031031
         ntdlst(4) = 031031     ! Data present indicator
         ntdlst(5) = 001031     ! Identification of originating/generating centre
         ntdlst(6) = 001032     ! Generating application
         ntdlst(7) = 101000 + num_data ! Replication factor for 033007
         ntdlst(8) = 033007     ! Per cent confidence
      ELSE IF (subcategory.EQ.145) THEN
         ntdlen = 1
         ntdlst(1) = 311193     ! The ACARS template
      ELSE
         WRITE(*,*) 'Internal error: subcategory = ',subcategory
         RETURN
      END IF
      ndlen = 0          ! No delayed replication
      nelem = 1000
      nvals = 1000

      CALL BUFREN(nsec0,nsec1,nsec2,nsec3,nsec4,ntdlen,ntdlst,
     +     ndlen,ndata,nelem,nvals,walues,cwals,kbufl,kbuff,kerr)
      IF (kerr.LT.0) CALL err_msg1('Error in BUFREN: KERR=',kerr)

      END SUBROUTINE encode_bufr

C     -----------------------------------------------------------------

      SUBROUTINE get_amdar_values(ksub,kxelem,ktdexl,ktdexp,values,
     +     cvals,verbose,is_amdar,walues,cwals,year,month,day,hour,
     +     minute,num_data,subcategory,skip_this_message)
C     Extract all values needed in the Hirlam AMDAR and ACARS
C     templates. If one of the descriptors 001006 and 001008 is present
C     with defined value, is_amdar is set to true. If so, which template
C     should be used is decided based on whether position is provided
C     with high or coarse accuracy, and the values then inserted into
C     arrays walues and cwals. Also returns date/time parameters and the
C     subcategory needed in section 1.
      IMPLICIT NONE

      INTEGER ksub              ! Input: number of subset currently processed
      INTEGER kxelem            ! Input: expected (max) number of expanded elements
      INTEGER ktdexl            ! Input: number of entries in list of expanded data descriptors
      INTEGER ktdexp(*)         ! Input: array containing expanded data descriptors
      REAL values(*)            ! Input: expanded data values (one subset)
      CHARACTER*80 cvals(*)     ! Input: CCITTIA5 Bufr elements entries (one subset)
      INTEGER verbose
      LOGICAL is_amdar          ! Output: TRUE if at least one of 001006 or 001008 is
                                ! present with non missing value
      REAL walues(*)            ! Output: expanded data values for Hirlam template
      CHARACTER*80 cwals(*)     ! Output: CCITTIA5 data values for Hirlam template
      REAL year,month,day,hour,minute ! Output
      INTEGER num_data          ! Output: number of data values
      INTEGER subcategory       ! Output (144 or 145)
      LOGICAL skip_this_message ! Output

      REAL rvind                ! missing value for real data
      PARAMETER (rvind=1.7E38)

      CHARACTER*8 flight,registration,aircraft,missing,spaces
      REAL navigation,relay_system,iw,Tprecision,oslatlon,
     +     roll_angle,ix,latitude_high,latitude_coarse,longitude_high,
     +     longitude_coarse,phase,PO,time_significance,DD,FF,BA,
     +     turb_base,turb_top,vert_gust_vel,vert_gust_acc,TA,TD,UU,
     +     icing,altitude
      INTEGER idx,cidx
      CHARACTER one_bits
      INTEGER desc
      REAL value
      LOGICAL date_and_position_ok ! function

C     A missing character value should be coded as 1 bits in BUFR
      one_bits = CHAR(255)
      WRITE(missing,'(8A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits,one_bits,one_bits,one_bits
C     But sometimes missing character values are coded as spaces in BUFR messages
      WRITE(spaces,'(8A)') ' '

C     Initialize all parameters to missing values
      flight = missing
      registration = missing
      navigation = rvind
      relay_system = rvind
      iw = rvind
      Tprecision = rvind
      oslatlon = rvind
      roll_angle = rvind
      ix = rvind
      year = rvind
      month = rvind
      day = rvind
      hour = rvind
      minute = rvind
      latitude_high = rvind
      longitude_high = rvind
      latitude_coarse = rvind
      longitude_coarse = rvind
      phase = rvind
      PO = rvind
      time_significance = rvind
      DD = rvind
      FF = rvind
      BA = rvind
      turb_base = rvind
      turb_top = rvind
      vert_gust_vel = rvind
      vert_gust_acc = rvind
      TA = rvind
      TD = rvind
      UU = rvind
      icing = rvind
      altitude = rvind

      num_data = 0

C     Loop through all expanded descriptors
      DO idx=1,ktdexl
         desc = ktdexp(idx)
         value = values(idx + (ksub-1)*kxelem)

C     The missing value returned from libbufr might not be exactly equal to rvind
         IF (ABS(value - rvind)/rvind.LE.0.001) value = rvind

         IF (desc.EQ.1006) THEN ! Aircraft flight number
            IF (value.NE.rvind) THEN
               cidx = int(value/1000)
               flight = cvals(cidx) ! CCITTIA5 data
               IF (flight.EQ.spaces) flight = missing
            END IF
         ELSE IF (desc.EQ.1008) THEN ! Aircraft registration number or other identification
            IF (value.NE.rvind) THEN
               cidx = int(value/1000)
               registration = cvals(cidx) ! CCITTIA5 data
               IF (registration.EQ.spaces) registration = missing
            END IF
         ELSE IF (desc.EQ.2061) THEN ! Aircraft navigational system
            IF (value.NE.rvind) THEN
               navigation = value
            END IF
         ELSE IF (desc.EQ.2062) THEN ! Type of aircraft data relay system
            IF (value.NE.rvind) THEN
               relay_system = value
            END IF
         ELSE IF (desc.EQ.2002) THEN ! Type of instrumentation for wind measurement
            IF (value.NE.rvind) THEN
               iw = value
            END IF
         ELSE IF (desc.EQ.2005) THEN ! Type of instrumentation for wind measurement
            IF (value.NE.rvind) THEN
               Tprecision = value
            END IF
         ELSE IF (desc.EQ.2070) THEN ! Original specification of latitude/longitude
            IF (value.NE.rvind) THEN
               oslatlon = value
            END IF
         ELSE IF (desc.EQ.2063) THEN ! Aircraft roll angle
            IF (value.NE.rvind) THEN
               roll_angle = value
            END IF
         ELSE IF (desc.EQ.2001) THEN ! Aircraft roll angle
            IF (value.NE.rvind) THEN
               ix = value
            END IF
         ELSE IF (desc.EQ.4001) THEN ! Year
            IF (value.NE.rvind) THEN
               year = value
            END IF
         ELSE IF (desc.EQ.4002) THEN ! Month
            IF (value.NE.rvind) THEN
               month = value
            END IF
         ELSE IF (desc.EQ.4003) THEN ! Day
            IF (value.NE.rvind) THEN
               day = value
            END IF
         ELSE IF (desc.EQ.4004) THEN ! Hour
            IF (value.NE.rvind) THEN
               hour = value
            END IF
         ELSE IF (desc.EQ.4005) THEN ! Minute
            IF (value.NE.rvind) THEN
               minute = value
            END IF
         ELSE IF (desc.EQ.5001) THEN ! Latitude (high accuracy)
            IF (value.NE.rvind) THEN
               latitude_high = value
            END IF
         ELSE IF (desc.EQ.5002) THEN ! Latitude (coarse accuracy)
            IF (value.NE.rvind) THEN
               latitude_coarse = value
            END IF
         ELSE IF (desc.EQ.6001) THEN ! Longitude (high accuracy)
            IF (value.NE.rvind) THEN
               longitude_high = value
            END IF
         ELSE IF (desc.EQ.6002) THEN ! Longitude (coarse accuracy)
            IF (value.NE.rvind) THEN
               longitude_coarse = value
            END IF
         ELSE IF (desc.EQ.8004) THEN ! Phase of aircraft flight
            IF (value.NE.rvind) THEN
               phase = value
            END IF
         ELSE IF (desc.EQ.8009) THEN ! Detailed phase of aircraft flight
            IF (value.NE.rvind .AND. phase.EQ.rvind) THEN
               CALL convert_phase_of_aircraft(rvind,value)
               phase = value
            END IF
         ELSE IF (desc.EQ.7004) THEN ! Pressure (location class)
            IF (value.NE.rvind) THEN
               PO = value
            END IF
         ELSE IF (desc.EQ.10004) THEN ! Pressure
            IF (value.NE.rvind .AND. PO.EQ.rvind) THEN
               PO = value
            END IF
         ELSE IF (desc.EQ.8021) THEN ! Time significance
            IF (value.NE.rvind) THEN
               time_significance = value
            END IF
         ELSE IF (desc.EQ.11001) THEN ! Wind direction
            IF (value.NE.rvind) THEN
               DD = value
            END IF
         ELSE IF (desc.EQ.11002) THEN ! Wind speed
            IF (value.NE.rvind) THEN
               FF = value
            END IF
         ELSE IF (desc.EQ.11031) THEN ! Degree of turbulence
            IF (value.NE.rvind) THEN
               BA = value
            END IF
         ELSE IF (desc.EQ.11032) THEN ! Height of base of turbulence
            IF (value.NE.rvind) THEN
               turb_base = value
            END IF
         ELSE IF (desc.EQ.11033) THEN ! Height of top of turbulence
            IF (value.NE.rvind) THEN
               turb_top = value
            END IF
         ELSE IF (desc.EQ.11034) THEN ! Vertical gust velocity
            IF (value.NE.rvind) THEN
               vert_gust_vel = value
            END IF
         ELSE IF (desc.EQ.11035) THEN ! Vertical gust velocity
            IF (value.NE.rvind) THEN
               vert_gust_acc = value
            END IF
         ELSE IF (desc.EQ.12101) THEN ! Temperature/dry bulb temperature (16 bits)
            IF (value.NE.rvind) THEN
               TA = value
            END IF
         ELSE IF (desc.EQ.12001) THEN ! Temperature/dry bulb temperature (12 bits)
            IF (value.NE.rvind .AND. TA.EQ.rvind) THEN
               TA = value
            END IF
         ELSE IF (desc.EQ.12103) THEN ! Dew-point temperature (16 bits)
            IF (value.NE.rvind) THEN
               TD = value
            END IF
         ELSE IF (desc.EQ.12003) THEN ! Dew-point temperature (12 bits)
            IF (value.NE.rvind .AND.TD.EQ.rvind) THEN
               TD = value
            END IF
         ELSE IF (desc.EQ.13003) THEN ! Relative humidity
            IF (value.NE.rvind) THEN
               UU = value
            END IF
         ELSE IF (desc.EQ.20041) THEN ! Airframe icing
            IF (value.NE.rvind) THEN
               icing = value
            END IF
         ELSE IF (desc.EQ.7002) THEN ! Height or altitude
            IF (value.NE.rvind) THEN
               altitude = value
            END IF
         ELSE IF (desc.EQ.7010) THEN ! Flight level
            IF (value.NE.rvind .AND.altitude.EQ.rvind) THEN
               altitude = value
            END IF
         ELSE IF (desc.EQ.10070) THEN ! Indicated aircraft altitude
            IF (value.NE.rvind .AND.altitude.EQ.rvind) THEN
               altitude = value
            END IF
         END IF

      END DO

      IF (flight.EQ.missing .AND. registration.EQ.missing) THEN
         is_amdar = .FALSE.
      ELSE
         is_amdar = .TRUE.
      END IF

      IF (latitude_high.NE.rvind .AND. longitude_high.NE.rvind) THEN
C     High accuracy for position: use AMDAR template
         subcategory = 144
C     There is only one aircraft descriptor in AMDAR template:
C     001006. If 001006 is not present in incoming BUFR amdar, cheat by
C     using 001008 instead
         IF (flight.NE.missing) THEN
            aircraft = flight
         ELSE
            aircraft = registration
         END IF

         IF (.NOT.date_and_position_ok(latitude_high,longitude_high,
     +        year,month,day,hour,minute,altitude,PO,rvind)) THEN
            skip_this_message = .TRUE.
            RETURN
         END IF

         CALL amdar_template(aircraft,navigation,year,
     +        month,day,hour,minute,latitude_high,longitude_high,
     +        phase,altitude,PO,TA,DD,FF,BA,turb_base,turb_top,
     +        icing,rvind,walues,cwals,num_data)

      ELSE IF (latitude_coarse.NE.rvind .AND.
     +        longitude_coarse.NE.rvind) THEN
C     Coarse accuracy for position: use ACARS template

         IF (.NOT.date_and_position_ok(latitude_coarse,longitude_coarse,
     +        year,month,day,hour,minute,altitude,PO,rvind)) THEN
            skip_this_message = .TRUE.
            RETURN
         END IF

         subcategory = 145
         CALL acars_template(flight,registration,navigation,
     +        relay_system,iw,Tprecision,oslatlon,roll_angle,ix,year,
     +        month,day,hour,minute,latitude_coarse,longitude_coarse,
     +        phase,PO,time_significance,DD,FF,BA,turb_base,turb_top,
     +        vert_gust_vel,vert_gust_acc,TA,TD,UU,icing,altitude,rvind,
     +        walues,cwals)
      ELSE
C     If position is missing, the BUFR message is useless
         skip_this_message = .TRUE.
      END IF

      IF (verbose.GT.3) THEN
         WRITE(*,*) flight,registration,navigation,relay_system,iw,
     +        Tprecision,oslatlon,roll_angle,ix,year,month,day,hour,
     +        minute,latitude_high,longitude_high,latitude_coarse,
     +        longitude_coarse,phase,PO,time_significance,
     +        DD,FF,BA,turb_base,turb_top,vert_gust_vel,vert_gust_acc,
     +        TA,TD,UU,icing,altitude,is_amdar,num_data
      END IF

      END SUBROUTINE get_amdar_values

C     -----------------------------------------------------------------

      SUBROUTINE amdar_template(aircraft,navigation,year,
     +     month,day,hour,minute,latitude_high,longitude_high,
     +     phase,altitude,PO,TA,DD,FF,BA,turb_base,turb_top,
     +     icing,rvind,walues,cwals,num_data)
C     Insert values into arrays walues and cvals according to the
C     Hirlam AMDAR template 311001
      IMPLICIT NONE

      CHARACTER*8 aircraft
      REAL navigation,year,
     +     month,day,hour,minute,latitude_high,longitude_high,
     +     phase,altitude,PO,TA,DD,FF,BA,turb_base,turb_top,icing,rvind
      REAL walues(*)       ! Output: expanded data values for amdar template
      CHARACTER*80 cwals(*)! Output: CCITTIA5 data values for amdar template
      INTEGER num_data     ! Output: number of data values
      REAL Z               ! function

      IF (PO.NE.rvind .AND. altitude.EQ.rvind) THEN
         altitude = Z(PO)
      END IF

      cwals(1) = aircraft ! AIRCRAFT FLIGHT NUMBER
      walues(1) = 1008
      walues(2) = navigation ! AIRCRAFT NAVIGATIONAL SYSTEM
      walues(3) = year
      walues(4) = month
      walues(5) = day
      walues(6) = hour
      walues(7) = minute
      walues(8) = latitude_high  ! LATITUDE (HIGH ACCURACY)
      walues(9) = longitude_high ! LONGITUDE (HIGH ACCURACY)
      walues(10) = phase ! PHASE OF AIRCRAFT FLIGHT
      walues(11) = altitude ! HEIGHT OR ALTITUDE
      walues(12) = TA ! TEMPERATURE/DRY BULB TEMPERATURE
      walues(13) = DD ! WIND DIRECTION
      walues(14) = FF ! WIND SPEED
      walues(15) = BA ! DEGREE OF TURBULENCE
      walues(16) = turb_base ! HEIGHT OF BASE OF TURBULENCE
      walues(17) = turb_top  ! HEIGHT OF TOP OF TURBULENCE
      walues(18) = icing ! AIRFRAME ICING

      num_data = 18

      CALL add_quality_info(rvind,num_data,walues)

      END SUBROUTINE amdar_template

C     -----------------------------------------------------------------

      SUBROUTINE acars_template(flight,registration,navigation,
     +     relay_system,iw,Tprecision,oslatlon,roll_angle,ix,year,
     +     month,day,hour,minute,latitude_coarse,longitude_coarse,
     +     phase,PO,time_significance,DD,FF,BA,turb_base,turb_top,
     +     vert_gust_vel,vert_gust_acc,TA,TD,UU,icing,altitude,rvind,
     +     walues,cwals)
C     Insert values into arrays walues and cvals according to the
C     Hirlam AMDAR template 311193
      IMPLICIT NONE

      CHARACTER*8 flight,registration
      REAL navigation,
     +     relay_system,iw,Tprecision,oslatlon,roll_angle,ix,year,
     +     month,day,hour,minute,latitude_coarse,longitude_coarse,
     +     phase,PO,time_significance,DD,FF,BA,turb_base,turb_top,
     +     vert_gust_vel,vert_gust_acc,TA,TD,UU,icing,altitude,rvind
      REAL walues(*)       ! Output: expanded data values for acars template
      CHARACTER*80 cwals(*)! Output: CCITTIA5 data values for acars template
      REAL P               ! function

C     Normally PO (PRESSURE) is not part of the BUFR amdar, and must
C     be calculated from altitude
      IF (PO.EQ.rvind .AND. altitude.NE.rvind) THEN
         PO = P(altitude)
      END IF

      cwals(1) = flight ! AIRCRAFT FLIGHT NUMBER
      cwals(2) = registration ! AIRCRAFT REGISTRATION NUMBER OR OTHER IDENTIFICATION
      walues(1) = 1008
      walues(2) = 2008
      walues(3) = navigation ! AIRCRAFT NAVIGATIONAL SYSTEM
      walues(4) = relay_system ! TYPE OF AIRCRAFT DATA RELAY SYSTEM
      walues(5) = iw ! TYPE OF INSTRUMENTATION FOR WIND MEASUREMENT
      walues(6) = Tprecision ! PRECISION OF TEMPERATURE OBSERVATION
      walues(7) = oslatlon ! ORIGINAL SPECIFICATION OF LATITUDE/LONGITUDE
      walues(8) = roll_angle ! AIRCRAFT ROLL ANGLE
      walues(9) = ix ! TYPE OF STATION
      walues(10) = year
      walues(11) = month
      walues(12) = day
      walues(13) = hour
      walues(14) = minute
      walues(15) = latitude_coarse  ! LATITUDE (COARSE ACCURACY)
      walues(16) = longitude_coarse ! LONGITUDE (COARSE ACCURACY)
      walues(17) = phase ! PHASE OF AIRCRAFT FLIGHT
      walues(18) = PO ! PRESSURE
      walues(19) = time_significance
      walues(20) = DD ! WIND DIRECTION
      walues(21) = FF ! WIND SPEED
      walues(22) = BA ! DEGREE OF TURBULENCE
      walues(23) = vert_gust_vel ! VERTICAL GUST VELOCITY
      walues(24) = vert_gust_acc ! VERTICAL GUST ACCELERATION
      walues(25) = TA ! TEMPERATURE/DRY BULB TEMPERATURE
      walues(26) = TD ! DEW-POINT TEMPERATURE
      walues(27) = UU ! RELATIVE HUMIDITY
      walues(28) = icing ! AIRFRAME ICING

      END SUBROUTINE acars_template

C     -----------------------------------------------------------------

      FUNCTION P(Z)
C     Calculate pressure as function of altitude.
C     Copied from nix_arep.F, except that return value has been multiplied by 100
      IMPLICIT NONE

      REAL Z,A,B,C,PTRO,PO,Y,P
      DATA A/5.252368255329/,B/44330.769230769/
      DATA  C/0.000157583169442/
      DATA  PTRO/226.547172/,PO/1013.25/

      IF (Z.GT.11000.) GO TO 50
      Y = 1.-Z/B
      P = PO*(Y**A)*100.0
      RETURN

50    Y = -C*(Z-11000.)
      P = PTRO*EXP(Y)*100.0

      END FUNCTION P

C     -----------------------------------------------------------------

      FUNCTION Z(P)
C     The inverse of function P(Z)
      IMPLICIT NONE

      REAL Z,A,B,C,PTRO,PO,Y,P
      DATA A/5.252368255329/,B/44330.769230769/
      DATA  C/0.000157583169442/
      DATA  PTRO/226.547172/,PO/1013.25/

      IF (P.LE.22654.7171933658) GO TO 50
      Y = (P/(100.0*PO))**(1./A)
      Z = B*(1.0-Y)
      RETURN

 50   Y = P/(100.0*PTRO)
      Z = 11000.0-1.0/C*log(Y)

      END FUNCTION Z

C     -----------------------------------------------------------------

      SUBROUTINE convert_phase_of_aircraft(rvind,value)
C     Convert table 008009 "Detailed phase of aircraft flight" into table
C     008004 "Phase of aircraft flight"
      IMPLICIT NONE

      REAL rvind                ! Input: missing value for real data
      REAL value                ! Input: the value to convert
                                ! Output: the converted value
      REAL new_value
      INTEGER v,phase(16)

C     Conversion table from 008009 to 008004
C     2=UNS, 3=LVR, 4=LVW, 5=ASC, 6=DES, 7=missing in 008004
      DATA phase /3,4,2,3,4,5,6,5,5,5,5, 6, 6, 6, 6, 7/
C                 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15  008009 values

      v = NINT(value)
      IF (v.GT.15) THEN
         new_value = rvind
      ELSE
         new_value = phase(v + 1)
      END IF

      value = new_value

      END SUBROUTINE convert_phase_of_aircraft

C     -----------------------------------------------------------------

      LOGICAL FUNCTION date_and_position_ok(latitude,longitude,
     +        year,month,day,hour,minute,altitude,PO,rvind)
C     Returns TRUE if date, time, lat, lon, altitude (or pressure) are
C     present with physical valid values, else returns FALSE
      IMPLICIT NONE

      REAL latitude,longitude,year,month,day,hour,minute,altitude,PO ! Input
      REAL rvind        ! Input: missing value for real data
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
      ELSE IF (altitude.EQ.rvind.AND.PO.EQ.rvind) THEN
         ok = .FALSE.
C     altitude is often extracted from 007010, but is encoded in 007002,
C     so we need to check that the value is indeed codable in BUFR as 007002
      ELSE IF (altitude.NE.rvind.AND.altitude.LT.-400) THEN
         ok = .FALSE. 
      END IF

      date_and_position_ok = ok

      END FUNCTION date_and_position_ok

C     -----------------------------------------------------------------

      SUBROUTINE err_msg(msg)
      IMPLICIT NONE
      CHARACTER*80 msg
      WRITE(*,*) msg
      STOP
      END SUBROUTINE err_msg

C     -----------------------------------------------------------------

      SUBROUTINE err_msg1(msg,kerr)
      IMPLICIT NONE
      CHARACTER*80 msg
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
