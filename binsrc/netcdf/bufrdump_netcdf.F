C (C) Copyright 2010-2019 MET Norway
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

C This is bufrdump.F modified so that the output (after run through
C bufrdump.pl) is appropriate for bufr2cdl and bufr2nc perl programs to
C handle. I.e.

C - NN is not translated to octas, but using the BUFR unit percent,
C including the special value NN=113 for "Sky obscured by fog or other
C meteorological phenomena"

C - CH,CM,CL are printed with their BUFR code table values (not
C translated into corresponding TAC SYNOP code table values). Same for
C WW (so do not distinguish between WW and WAWA) and W1 and W2 (so do
C not distinguish between W1 and WA1, W2 and WA2). For WW value 509 is
C not printed

C - vss has same value as in BUFR (not decoded and stringified as 'sTw'
C etc)

C - PP gh TT TD FF DD will always be present in output for every level,
C written as 'TT=' etc if missing value. The same is true for tp, la, lo
C if at least one level with non missing tp exists

C - Extra line 'resolution=' included in print_sounding_values after
C obstime line, with values high or low
      
C Extract BUFR messages from bufr file(s) and print the data as
C 'parameter=value' lines. See usage_verbose for explanation of the
C options allowed.
C
C Usage: bufrdump_netcdf <bufr file>
C        [--help]
C        [--filter <filter file>]
C        [--obstype] <amdar|ocea|surface|sounding|sounding-|soundingh>
C        [--stop_on_error]
C        [--rectangle x1 y1 x2 y2]
C        or one or more of
C        [--lon1 x1]
C        [--lat1 y1]
C        [--lon2 x2]
C        [--lat2 y2]
C
C Extract BUFR messages from bufr file and print the data as
C 'parameter = value' lines. See subroutine get_arguments for
C explanation of the options and DokIT for more documentation.
C
C Probably you should set
C export PRINT_TABLE_NAMES=false
C export BUFR_TABLES=/usr/local/lib/bufrtables/
C
C Author: P.Sannes, MET Norway


      PROGRAM BUFRDUMP

      IMPLICIT NONE

      CHARACTER(LEN=200) bufr_file       ! Bufr file to read from
      LOGICAL data_only                  ! TRUE if only data section is to be printed
      LOGICAL filter                     ! TRUE if observations should be filtered
      LOGICAL rectangle                  ! TRUE if observations are wanted for a rectangle only
      LOGICAL stop_on_error              ! TRUE if program should stop if a libbufr call returns an error
      CHARACTER(LEN=10) obstype
      CHARACTER(LEN=200) filter_file     ! File containing the filter criteria
      CHARACTER(LEN=200) descriptor_file ! File containing the descriptors requested for partial expansion
      INTEGER verbose                    ! Level of verbose output: 0 - 3 (default 1)
      INTEGER nlibbufr_errors            ! Number of errors encountered in libbufr calls

C     Dimensions of arrays used by libbufr routines are the same as those used in
C     example program decode_bufr.F in bufr_000280/example/

      INTEGER ibuff(512000)               ! Buffer to hold BUFR message
      INTEGER ibflen                      ! Size in BYTES of the array ibuff
      INTEGER ilen                        ! Size in BYTES of the BUFR message read
      INTEGER wlen                        ! Size in WORDS of the BUFR message read
      INTEGER iunit,iret
      INTEGER iloop
      LOGICAL eof

      PARAMETER (ibflen=4*512000)

      INCLUDE 'comfilter.f'     ! contains variables used when --filter is set

      verbose = 1

      CALL get_arguments(bufr_file,filter,filter_file,rectangle,
     +     stop_on_error,obstype)

      IF (filter) CALL read_filter(filter,filter_file,verbose)

C     Open bufr file - for read
      CALL PBOPEN(iunit,bufr_file,'r',iret)
      IF (iret.NE.0) CALL err_msg('PBOPEN failed for ' // bufr_file)

      nlibbufr_errors = 0
      iloop = 0
      DO WHILE (.TRUE.)
         iloop = iloop + 1

C     Get the next BUFR message
         CALL get_next_bufr_message(iunit,ibuff,ibflen,ilen,
     +        iloop,verbose,eof)
         IF (eof) GOTO 900

C     Decode Bufr message
         wlen = ilen/4 + 1   ! shouldn't really add 1 when ilen is divisible with 4,
                             ! but copy how it is done in decode_bufr in libbufr
         CALL BUFRdecode(ibuff,wlen,filter,rectangle,verbose,
     +        stop_on_error,obstype,nlibbufr_errors)
      END DO
 900  CONTINUE

      IF (nlibbufr_errors.GT.0) THEN
         WRITE(*,*) '\n\n  NOTE: At least ',nlibbufr_errors,
     +        ' BUFR message(s) skipped due to errors in libbufr calls',
     +        ' Check output for details of the errors encountered!',
     +        ' (Search on "ERROR")'
      END IF

      END PROGRAM BUFRDUMP


C     -----------------------------------------------------------------

      SUBROUTINE get_next_bufr_message(iunit,ibuff,ibflen,ilen,
     +        iloop,verbose,eof)
      IMPLICIT none
      INTEGER ibuff(*)
      INTEGER iunit,ibflen,ilen,iloop,iret,verbose
      LOGICAL eof

      eof = .FALSE.
      iret = 0
      CALL PBBUFR(iunit,ibuff,ibflen,ilen,iret)
      IF (iret .EQ. -1 ) THEN
         eof = .TRUE.
         RETURN
      ELSEIF (iret .EQ. -2 ) THEN
         CALL err_msg('ERROR: File handling problem')
      ELSEIF (iret .EQ. -3 ) THEN
         CALL err_msg('ERROR: Array too small')
      END IF
      IF (verbose .GT. 2) THEN
         WRITE (*,'(1X,A,I10)') 'BUFR message number= ',iloop
         WRITE (*,'(1X,A,I10)') 'Length of BUFR message = ',ilen
      END IF
      END SUBROUTINE get_next_bufr_message

C     -----------------------------------------------------------------

      SUBROUTINE BUFRdecode(ibuff,ilen,filter,rectangle,
     +     verbose,stop_on_error,obstype,nlibbufr_errors)
      IMPLICIT NONE
      INTEGER ibuff(*)
      INTEGER ilen              ! size in WORDS (not bytes) of the BUFR message read
      LOGICAL filter
      LOGICAL rectangle
      LOGICAL stop_on_error
      CHARACTER(LEN=*) obstype
      INTEGER nlibbufr_errors   ! Number of errors encountered in libbufr calls
      INTEGER verbose

      LOGICAL metar             ! Set to TRUE if metar (data subcategory 5)
      
      INTEGER kelem,kxelem      ! expected (max) number of expanded elements
      INTEGER kvals             ! expected (max) number of data values

C     Dimensions of arrays used by libbufr routines are the same as those used in
C     example program decode_bufr.F in bufr_000310/example/
      PARAMETER (kelem = 160000,kvals = 4096000)

C     BUFREX variables
      INTEGER ksup(9)           ! Integer array containing suplementary information
      INTEGER ksec0(3)          ! Bufr section 0 information
      INTEGER ksec1(40)
      INTEGER ksec2(4096)
      INTEGER ksec3(4)
      INTEGER ksec4(2)
      CHARACTER*64 cnames(kelem) ! Bufr Table B element names
      CHARACTER*24 cunits(kelem) ! Bufr Table B units
      CHARACTER*80 cvals(kelem) ! Bufr code table or CCITTIA5 Bufr elements entries
                                ! Using kelem as dimension seems somewhat arbitrary,
                                ! - just copying from decode_bufr.F in libbufr
      REAL*8 values(kvals)      ! expanded data values

C     BUSEL variables
      INTEGER ktdlst(2*kelem)     ! array containing data descriptors in section 3
      INTEGER ktdexp(2*kelem)     ! array containing expanded data descriptors
                                  ! Use 2*kelem, not kelem; copied from decode_bufr.F in libbufr
      INTEGER ktdlen            ! number of data descriptors in section 3
      INTEGER ktdexl            ! number of entries in list of expanded data descriptors
      INTEGER kerr              ! returned error code - but note that some libbufr
                                ! routines will return immediately if this is not 0 on input
      INTEGER ksub,ksub1,ksub2

C     BUSQR variables
      INTEGER kreq(2)
      INTEGER krql
      INTEGER krq(kelem)
      REAL*8 rqv(kelem)
      INTEGER rqd(101)           ! Requested descriptors

C     BUUKEY variables
      INTEGER key(46)

      INTEGER found_list(10000),num_found

      INTEGER i

      kerr = 0
      metar = .FALSE.
      
C     Do a partial expansion without quality control
      kreq(1) = 1               ! All original elements without quality control
      kreq(2) = 0
      krql = 0

C     Partial expansion
      CALL BUSRQ(kreq,krql,rqd,rqv,kerr)
      IF (kerr.NE.0) THEN
         IF (stop_on_error) STOP 1
         WRITE(*,*) 'ERROR IN BUSRQ: KERR= ',kerr
         nlibbufr_errors = nlibbufr_errors + 1
         RETURN
      END IF
C
C     Decode BUFR message into fully decoded form.
C

C     Using parameter kelem in call to BUFREX might be too big for some
C     multisubset messages (or too small for other messages). Have
C     copied the method used in decode_bufr.F in libbufr, first calling
C     BUS012 in order to get number of subsets ksup(6)
      CALL BUS012(ilen,ibuff,ksup,ksec0,ksec1,ksec2,kerr)
      IF (kerr.NE.0) THEN
         IF (stop_on_error) STOP 1
         WRITE(*,*) 'ERROR IN BUS012: KERR= ',kerr
         nlibbufr_errors = nlibbufr_errors + 1
         RETURN
      END IF
      kxelem = kvals/ksup(6)
C     The second IF-condition is not in decode_bufr.F, but else I get 'KELEM
C     ARGUMENT TOO SMALL' from BUUPWT when decoding large radiosondefiles
      IF (kxelem.GT.kelem .AND. ksup(6).GT.10) kxelem = kelem

      CALL BUFREX (ilen,ibuff,ksup,ksec0,ksec1,ksec2,
     +     ksec3,ksec4,kxelem,cnames,cunits,kvals,values,
     +     cvals,kerr)
      IF (kerr.EQ.45) RETURN      ! No requested elements in data
      IF (kerr.NE.0) THEN
         IF (stop_on_error) STOP 1
         WRITE(*,*) 'ERROR IN BUFREX: KERR= ',kerr
         nlibbufr_errors = nlibbufr_errors + 1
         RETURN
      END IF
      IF (verbose .GT. 2) WRITE (*,'(1X,A,I10)')
     +     'Number of subsets:',ksup(6)

C     Convert messages with data category (BUFR table A) 0-2,4,6 and 31 only.
C     0 = Surface data - land, 1 = Surface data - sea, 2 = Vertical
C     sounding (other than satellite) 4 = Single level upper-air data
C     (other than satellite) 6 = Radar data 31 = Oceanographic data
      IF (ksec1(6).GT.2 .AND. ksec1(6).NE.4 .AND. ksec1(6).NE.6
     +     .AND. ksec1(6).NE.31) RETURN

C     MET (and perhaps also other ECMWF based software?) uses local
C     subcategory 5 for metar (and BUFR edition 3)
      IF (KSEC1(2).EQ.3 .AND. KSEC1(6).EQ.0 .AND. KSEC1(7).EQ.5) THEN
         metar = .TRUE.
      END IF

      IF (filter) THEN
C     Return list of Data Descriptors from Section 3 of Bufr message,
C     and total/requested list of elements. BUFREX must have been called
C     before BUSEL. If filter is set, BUSEL2 will later be called to be
C     used for printing each subset, but for filter we need to know the
C     first members of ktdlst and ktdexl already now, to be able to call
C     find_stations.
         CALL BUSEL(ktdlen,ktdlst,ktdexl,ktdexp,kerr)
         IF (kerr .GT. 0) THEN
            IF (stop_on_error) STOP 1
            WRITE(*,*) 'ERROR IN BUSEL: KERR= ',kerr
            nlibbufr_errors = nlibbufr_errors + 1
            RETURN
         END IF
      END IF

      IF (filter) THEN
         CALL find_stations(ktdexp,ktdexl,values,cvals,ksup(6),
     +         kxelem,found_list,num_found)
      ENDIF
      IF (filter .AND. num_found.GT.0) THEN
         DO i=1,num_found
            ksub = found_list(i)
            CALL BUSEL2(ksub,kxelem,ktdlen,ktdlst,ktdexl,ktdexp,
     +           cnames,cunits,kerr)
            IF (kerr.NE.0) THEN
               IF (stop_on_error) STOP 1
               WRITE(*,*) 'ERROR IN BUSEL2: KERR= ',kerr
               nlibbufr_errors = nlibbufr_errors + 1
               RETURN
            END IF

            IF (obstype.EQ.'surface') THEN
               CALL print_surface_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,metar,verbose)
            ELSE IF (obstype(1:8).EQ.'sounding') THEN
               CALL print_sounding_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,obstype,verbose)
            ELSE IF (obstype.EQ.'amdar') THEN
               CALL print_amdar_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,verbose)
            ELSE IF (obstype.EQ.'ocea') THEN
               CALL print_oceanographic_values(ksub,kxelem,ktdexl,
     +              ktdexp,values,cvals,rectangle,verbose)
            ELSE IF (ksec1(6).LE.1) THEN
               CALL print_surface_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,metar,verbose)
            ELSE IF (ksec1(6).EQ.2) THEN
               CALL print_sounding_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,obstype,verbose)
            ELSE IF (ksec1(6).EQ.4) THEN
               CALL print_amdar_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,verbose)
            ELSE IF (ksec1(6).EQ.6) THEN
               CALL print_radar_profiler_values(ksub,kxelem,ktdexl,
     +              ktdexp,values,cvals,rectangle,verbose)
            ELSE IF (ksec1(6).EQ.31) THEN
               CALL print_oceanographic_values(ksub,kxelem,ktdexl,
     +              ktdexp,values,cvals,rectangle,verbose)
            END IF
         ENDDO
      ELSE IF (.NOT. filter) THEN
         DO ksub=1,ksup(6)
            CALL BUSEL2(ksub,kxelem,ktdlen,ktdlst,ktdexl,ktdexp,
     +           cnames,cunits,kerr)
            IF (kerr.NE.0) THEN
               IF (stop_on_error) STOP 1
               WRITE(*,*) 'ERROR IN BUSEL2: KERR= ',kerr
               nlibbufr_errors = nlibbufr_errors + 1
               RETURN
            END IF

            IF (obstype.EQ.'surface') THEN
               CALL print_surface_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,metar,verbose)
            ELSE IF (obstype(1:8).EQ.'sounding') THEN
               CALL print_sounding_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,obstype,verbose)
            ELSE IF (obstype.EQ.'amdar') THEN
               CALL print_amdar_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,verbose)
            ELSE IF (obstype.EQ.'ocea') THEN
               CALL print_oceanographic_values(ksub,kxelem,ktdexl,
     +              ktdexp,values,cvals,rectangle,verbose)
            ELSE IF (ksec1(6).LE.1) THEN
               CALL print_surface_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,metar,verbose)
            ELSE IF (ksec1(6).EQ.2) THEN
               CALL print_sounding_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,obstype,verbose)
            ELSE IF (ksec1(6).EQ.4) THEN
               CALL print_amdar_values(ksub,kxelem,ktdexl,ktdexp,
     +              values,cvals,rectangle,verbose)
            ELSE IF (ksec1(6).EQ.6) THEN
               CALL print_radar_profiler_values(ksub,kxelem,ktdexl,
     +              ktdexp,values,cvals,rectangle,verbose)
            ELSE IF (ksec1(6).EQ.31) THEN
C     For ocea files at met.no data category is 31. But note that IOB
C     bulletins all have data category 1 (and sub-category 25 or 255),
C     so for these we end up calling print_surface_values (unless
C     --obstype ocea is used)
               CALL print_oceanographic_values(ksub,kxelem,ktdexl,
     +              ktdexp,values,cvals,rectangle,verbose)
            END IF
         END DO
      END IF

      END SUBROUTINE BUFRdecode

C     -----------------------------------------------------------------

      SUBROUTINE get_arguments(bufr_file,filter,filter_file,
     +     rectangle,stop_on_error,obstype)

      IMPLICIT NONE

      CHARACTER(LEN=*) bufr_file ! Bufr file to read from
      LOGICAL data_only          ! TRUE if only data section is to be printed
      LOGICAL filter             ! TRUE if observations should be filtered
      LOGICAL rectangle          ! TRUE if observations are wanted for a rectangle only
      LOGICAL stop_on_error      ! TRUE if program should stop if a libbufr call returns an error
      CHARACTER(LEN=*) filter_file ! File containing the filter criteria
      CHARACTER(LEN=*) obstype

      CHARACTER(LEN=200) argument ! Buffer for next argument
      CHARACTER(LEN=1350) Usage
      CHARACTER(LEN=950) Help
      INTEGER iargc,iarg

C     Variables used for geographical filtering av observations
      REAL*8 x1,y1,x2,y2
      COMMON /COM_RECTANGLE/  x1,y1,x2,y2
      x1 = -180.00001
      y1 = -90.00001
      x2 = 180.00001
      y2 = 90.00001

      Usage = '\nUsage: bufrdump <bufr file> [options]\n\n'
     +     // 'Will print section 4 in BUFR messages in <bufr file>'
     +     // ' as "parameter=value" lines.'
     +     // '\nOptions (may be abbreviated to one character)'
     +     // ' are:\n'
     +     // '\t--help\t\tPrint this Usage plus some more help\n'
     +     // '\t--filter <filter file>\n'
     +     // '\t\t\tDecode observations meeting criteria in '
     +     // '<filter file> only\n'
     +     // '\t--obstype <amdar|ocea|surface|sounding|sounding-'
     +     // '|soundingh>\n'
     +     // '\t\t\tForce observation type. If this option '
     +     // 'is not set, bufrdump\n\t\t\twill make an '
     +     // 'educated guess of observation type based on\n'
     +     // '\t\t\tmetadata in section 1 of each BUFR message. '
     +     // 'The special value\n\t\t\t"sounding-" results in '
     +     // 'levels with no vss being skipped,\n\t\t\t'
     +     // 'while "soundingh" will include a sounding only if '
     +     // 'containing\n\t\t\thigh resolution data\n'
     +     // '\t--stop_on_error\tIf a libbufr call returns an '
     +     // 'error, bufrdump will return\n\t\t\timmediately with '
     +     // 'exit status 1\n'
     +     // '\t--rectangle x1 y1 x2 y2\n'
     +     // '\t\t\tDecode observations inside rectangle with lower '
     +     // 'left coordinates\n\t\t\t(x1,y1) (i.e.lon,lat) '
     +     // 'and upper right coordinates (x2,y2) only\n'
     +     // 'As an alternative to --rectangle you can use one or '
     +     // 'more of\n'
     +     // '\t--lon1 x1'
     +     // '\tDecode observations with longitude >= x1 only\n'
     +     // '\t--lat1 y1'
     +     // '\tDecode observations with latitude >= y1 only\n'
     +     // '\t--lon2 x2'
     +     // '\tDecode observations with longitude <= x2 only\n'
     +     // '\t--lat2 y2'
     +     // '\tDecode observations with latitude <= y2 only\n'
     +     // '\t\t\tx1,y1,x2,y2 should be degrees or decimal degrees'

      Help = '\nYou should probably set\n'
     +     // '\texport BUFR_TABLES=/usr/local/lib/bufrtables/\n'
     +     // '\texport PRINT_TABLE_NAMES=false\n'
     +     // '\nUsing --filter will decode only those observations '
     +     // 'that meet the criteria in\n<filter file>. An example '
     +     // 'of a filter file is\n\n'
     +     // 'D: 001001 I2.2\n'
     +     // '01\n'
     +     // 'D: 001001 I2.2 001002 I3.3\n'
     +     // '03 895\n'
     +     // '06 252\n'
     +     // 'D: 001011 A9\n'
     +     // 'LMEL     \n\n'
     +     // 'which decodes all observations with block number 01, '
     +     // 'two other specific wmo\nstations and one ship. '
     +     // 'So far only filtering on exact matches on integer and\n'
     +     // 'character valued BUFR descriptors has been implemented.'
     +     // ' But note that the\nclosely related program bufrdump.pl'
     +     // ' has a lot more options for filtering.'
     +     // '\n\nIf an error occurs during decoding (typically '
     +     // 'because the required BUFR table\nis missing or message '
     +     // 'is corrupt) the message is skipped, and the number '
     +     // 'of\nerrors is reported at end of output - unless '
     +     // 'option --stop_on_error has been\napplied (note that '
     +     // 'some error messages from libbufr might still have been '
     +     // 'sent\nto STDOUT).\n'

C     Default values
      filter = .FALSE.
      obstype = ''
      rectangle = .FALSE.
      stop_on_error = .FALSE.
      bufr_file = ''
      iarg = 0

      IF (iargc() .LT. 1) THEN
         WRITE(*,*) Usage
         CALL EXIT(0)
      END IF

C     Loop: reading of command options
 100  CONTINUE
      iarg = iarg + 1
      CALL getarg(iarg,argument)
      IF (argument .EQ. '--help' .OR.
     +     argument .EQ. '--h' .OR. argument .EQ. '-h') THEN
         WRITE(*,*) Usage
         WRITE(*,*) Help
         CALL EXIT(0)
      ELSE IF (argument .EQ. '--filter' .OR.
     +     argument .EQ. '--f' .OR. argument .EQ. '-f') THEN
         filter = .TRUE.
         iarg = iarg + 1
         CALL getarg(iarg,filter_file)
      ELSE IF (argument .EQ. '--obstype' .OR.
     +     argument .EQ. '--o' .OR. argument .EQ. '-o') THEN
         iarg = iarg + 1
         CALL getarg(iarg,obstype)
      ELSE IF (argument .EQ. '--stop_on_error') THEN
         stop_on_error = .TRUE.
      ELSE IF (argument .EQ. '--rectangle' .OR.
     +     argument .EQ. '--r' .OR. argument .EQ. '-r') THEN
         rectangle = .TRUE.
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) x1
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) y1
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) x2
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) y2
      ELSE IF (argument .EQ. '--lon1') THEN
         rectangle = .TRUE.
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) x1
      ELSE IF (argument .EQ. '--lat1') THEN
         rectangle = .TRUE.
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) y1
      ELSE IF (argument .EQ. '--lon2') THEN
         rectangle = .TRUE.
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) x2
      ELSE IF (argument .EQ. '--lat2') THEN
         rectangle = .TRUE.
         iarg = iarg + 1
         CALL getarg(iarg,argument)
         READ (argument,*) y2
      ELSE IF (argument(1:2) .EQ. '--' .OR.
     +     argument .EQ. '--d' .OR. argument .EQ. '-d') THEN
         WRITE(*,*) 'Unknown option ',argument
         WRITE(*,*) Usage
         CALL EXIT(1)
      ELSE
C     Read in Bufr file to decode
         bufr_file = argument
      END IF
      IF (iarg .LT. iargc()) GOTO 100

      IF (bufr_file .EQ. '') THEN
         WRITE(*,*) 'No BUFR file given'
         CALL EXIT(1)
      END IF

      IF (obstype.NE.'' .AND. obstype.NE.'amdar' .AND. obstype.NE.'ocea'
     +     .AND. obstype.NE.'surface' .AND. obstype.NE.'sounding'
     +     .AND. obstype.NE.'sounding-'.AND.obstype.NE.'soundingh') THEN
         WRITE(*,*) 'Argument to --obstype must be one of amdar, ocea,',
     +   ' surface, sounding, sounding- or soundingh'
         CALL EXIT(1)
      END IF

      END SUBROUTINE get_arguments

C     -----------------------------------------------------------------

      SUBROUTINE err_msg(msg)
      IMPLICIT NONE
      CHARACTER(LEN=*) msg
      WRITE(*,*) msg
      STOP
      END SUBROUTINE err_msg

C     -----------------------------------------------------------------

      SUBROUTINE err_msg1(msg,kerr)
      IMPLICIT NONE
      CHARACTER(LEN=*) msg
      INTEGER kerr
      WRITE(*,*) msg,kerr
      STOP
      END SUBROUTINE err_msg1

C     -----------------------------------------------------------------

      SUBROUTINE read_filter(filter,filter_file,verbose)
C
C     Reads the content of filter file into matrices fid and fidformat,
C     arrays nd_fid and nvl_fid for the descriptors, into matrices fivI
C     and fivC for the values to filter on. Also sets nfidlines and
C     nfidvalues. See comfilter.f for explanation of variables. See Help
C     in get_arguments for an example of filter file.
C
      IMPLICIT NONE
      LOGICAL filter            ! TRUE on input. Might be changed to FALSE on output
                                ! if no filter condition is found in filter file
      CHARACTER(LEN=200) filter_file
      INTEGER verbose,ios,inplen,i1,i2,i3,ifid,jfid,lenstr,fmt_len
      CHARACTER*100 inpline,readformat
      CHARACTER*10 fmt
      LOGICAL new_fid

      INCLUDE 'comfilter.f'

      OPEN (UNIT = 11,FILE = filter_file,STATUS = 'OLD',
     +     IOSTAT = ios)
      IF (ios.NE.0) CALL err_msg('Cannot open ' // filter_file)

      nfidlines = 0
      nfivlines = 0
      ifid = 0
      filter_loop: DO WHILE (.TRUE.)
C     Note: a line consisting of blanks or the new line character only
C     marks end of filter conditions
         READ (11,'(A100)',END=11) inpline
         IF (verbose.GT.2) WRITE (*,*) inpline
         inplen = lenstr(inpline,1)
         IF (inplen.EQ.0) EXIT filter_loop
         i1 = 0
         i2 = 0
         CALL advance_word(inpline,inplen,i1,i2)
         IF (i1 .EQ. 0) EXIT filter_loop
         IF (inpline(i1:i1).EQ.'#') CYCLE filter_loop ! Comment line

         IF (inpline(i1:i1+1).EQ.'D:') THEN
C     Descriptor line (e.g. D: 001001 I2.2 001002 I3.3)
            ifid = ifid + 1
            IF (ifid.GT.dim1_fid) CALL err_msg(
     +      'bufrread: dimension 1 limit exeeded for array fid')
            jfid = 0
            DO WHILE (i1 .NE. 0)
               CALL advance_word(inpline,inplen,i1,i2)
               IF (i1 .NE. 0) THEN
                  jfid = jfid + 1
                  IF (jfid.GT.dim1_fid) CALL err_msg(
     +                 'bufrread: dimension 2 exeeded for array fid')
C     Read descriptor
                  READ (inpline(i1:i2),*) fid(ifid,jfid)
                  CALL advance_word(inpline,inplen,i1,i2)
                  IF (i1 .EQ. 0)
     +                 CALL err_msg('Wrong format in filter file')
C     Read format
                  READ (inpline(i1:i2),*) fidformat(ifid,jfid)
               END IF
            END DO
            nd_fid(ifid) = jfid
            nfidlines = ifid
         ELSE

C     Value line (e.g. 03 895 or: North Shields)
            nfivlines = nfivlines + 1
            IF (nfivlines.GT.dim_fiv) CALL err_msg(
     +      'bufrread: dimension limit exeeded for array fivI/C')
            nvl_fid(ifid) =  nvl_fid(ifid) + 1
C     Read in value field, descriptor for descriptor:
            DO jfid = 1,nd_fid(ifid)
               fmt = fidformat(ifid,jfid)
               IF (fmt(1:1).EQ.'I') THEN
                  READ (inpline(i1:i2),*) fivI(nfivlines,jfid)
                  IF (verbose.GT.2) WRITE(*,*) 'nfivlines,jfid,fivI=',
     +                 nfivlines,jfid,fivI(nfivlines,jfid)
               ELSE IF (fmt(1:1).EQ.'A') THEN
C                 String may include space, therefore i2 may need to be adjusted
                  READ (fmt(2:lenstr(fmt,0)),*) fmt_len
                  i2 = i1 + fmt_len
                  READ (inpline(i1:i2),*) fivC(nfivlines,jfid)
                  IF (verbose.GT.2) WRITE(*,*) 'nfivlines,jfid,fivC=',
     +                 nfivlines,jfid,' ',fivC(nfivlines,jfid)
               ELSE
                  CALL err_msg('Wrong value format in filter file')
               END IF
               CALL advance_word(inpline,inplen,i1,i2)
            END DO
         ENDIF
      END DO filter_loop

 11   CONTINUE
      CLOSE(11)

      IF (verbose.GT.2) WRITE(*,*) 'nfidlines=',nfidlines
      IF (nfivlines.EQ.0) filter = .FALSE.

      END SUBROUTINE read_filter

C     -----------------------------------------------------------------

      subroutine advance_word(str,length,beginp,endp)
C
C     The character variable 'str' contains words of non-blank
C     characters separated by blanks. 'beginp' and 'endp' points to the
C     beginning and end of a word (char indices within 'str'). This
C     subroutine advances these pointers to point to the next word
C     within 'str'. 'length' is the length of 'str'. Initially, 'beginp'
C     and 'endp' is set to 0 (outside this subroutine). The first call
C     to 'advance_word' will then set these pointers to the first word
C     within 'str'. When no more words are found, 'beginp' and 'endp'
C     are again set to 0.
C
      implicit none
      character*(*) str
      integer length,beginp,endp
      integer i1,i2
      i1 = endp+1
      do while (i1 .le. length)
         if (str(i1:i1) .ne. ' ') then
            beginp = i1
            i2 = index(str(i1:length),' ')
            if (i2 .eq. 0) then
               endp = length
               return
            else
               endp = i1 + i2 - 2
               return
            end if
         else
            i1 = i1 + 1
         end if
      end do
      beginp = 0
      endp = 0
      end subroutine advance_word

C     -----------------------------------------------------------------

      SUBROUTINE find_stations(ktdexp,ktdexl,values,cvals,num_subsets,
     +        kxelem,found_list,num_found)
C
C     Return the number of subsets in BUFR message which contain an
C     observation matching one of the criteria in filter file as
C     num_found, the subset numbers in found(list(i)), i=1,...,num_found
C
      IMPLICIT NONE
      INTEGER found_list(10000),num_found
      INTEGER ktdexp(*)         ! array containing expanded data descriptors
      INTEGER ktdexl            ! number of entries in list of expanded data descriptors
      INTEGER kxelem            ! number of data elements for each section 4 report
      CHARACTER*80 cvals(*)     ! Bufr code table or CCITTIA5 Bufr elements entries
      REAL*8 values(*)          ! expanded data values
      INTEGER num_subsets

      INTEGER i1,i2,nsub,ifvl,itd,iv,icv,ifiv
      LOGICAL match             ! Set to true if a value in data matches value in
                                ! filter, for a single descriptor
      LOGICAL int_fid           ! Set to true if filter descriptor is integer type
      LOGICAL char_fid          ! Set to true if filter descriptor is character type

      INCLUDE 'comfilter.f'

      REAL*8 rvind              ! missing value for real*8 data
      PARAMETER (rvind=1.7E38)

      num_found = 0
C     loop through all subsets:
      DO nsub = 1,num_subsets
C     loop through all different conditions:
         ifiv = 0
         DO i1 = 1,nfidlines
C     loop through all filter value lines (for given) condition:
            DO ifvl = 1,nvl_fid(i1)
               ifiv = ifiv + 1
C     loop through all descriptors in condition:
               DO i2 = 1,nd_fid(i1)
                  match = .FALSE.
                  int_fid = .FALSE.
                  char_fid = .FALSE.
                  IF (fidformat(i1,i2)(1:1).EQ.'I') THEN
                     int_fid = .TRUE.
                  ELSE IF (fidformat(i1,i2)(1:1).EQ.'A') THEN
                     char_fid = .TRUE.
                  END IF
C     loop through all data in subset:
                  DO itd = 1,ktdexl
                     IF (ktdexp(itd).EQ.fid(i1,i2)) THEN
                        iv = (nsub-1)*kxelem + itd
                        IF (values(iv).EQ.rvind)
     +                       GOTO 400 ! next condition, no point in checking
                                      ! other filter value lines
                        IF (int_fid) THEN
                           IF (nint(values(iv)).EQ.fivI(ifiv,i2)) THEN
                              match = .TRUE.
                              EXIT ! exit innermost loop
                           END IF
                        ELSE IF (char_fid) THEN
                           icv = nint(values(iv))/1000
                           IF (cvals(icv).EQ.fivC(ifiv,i2)) THEN
                              match = .TRUE.
                              EXIT ! exit innermost loop
                           END IF
                        END IF
                     END IF
                   END DO       ! all data in subset
                   IF (.NOT.match) ! there is one descriptor in condition which
     +                  EXIT       ! subset does not match, so go to next
                                   ! value line
                END DO          ! all descriptors in condition
                IF (match) THEN
                   num_found = num_found + 1
                   found_list(num_found) = nsub
                   GO TO 500    ! next subset
                END IF
            END DO              ! all filter value lines (for given) condition
 400        CONTINUE
         END DO                 ! all different conditions
 500     CONTINUE
      END DO                    ! all subsets

      END SUBROUTINE find_stations

C     -----------------------------------------------------------------

C     Identify pressure, temperature etc and print parameter=value to
C     screen. Note: have seen high resolution data with 7002 HEIGHT OR
C     ALTITUDE and wind profiler data with 007009 GEOPOTENTIAL HEIGHT
C     used as vertical coordinate instead of 7004 PRESSURE (10004 was
C     used for pressure in the first case). Not able to handle that in
C     present code.
      SUBROUTINE print_sounding_values(ksub,kxelem,ktdexl,ktdexp,values,
     +     cvals,rectangle,obstype,verbose)
      IMPLICIT NONE

      INTEGER ksub              ! Input: number of subset currently processed
      INTEGER kxelem            ! Input: expected (max) number of expanded elements
      INTEGER ktdexl            ! Input: number of entries in list of expanded data descriptors
      INTEGER ktdexp(*)         ! Input: array containing expanded data descriptors
      REAL*8 values(*)          ! Input: expanded data values (one subset)
      CHARACTER*80 cvals(*)     ! Input: CCITTIA5 Bufr elements entries (one subset)
      LOGICAL rectangle         ! Input: TRUE if observations are wanted for a rectangle only
      CHARACTER(LEN=*) obstype  ! Input: influence output if set to 'sounding-' or 'soundingh'
      INTEGER verbose           ! Input: verbose level

      REAL*8 rvind              ! missing value for real*8 data
      PARAMETER (rvind=1.7E38)

      REAL*8 II,iii,ix,year,month,day,hour,minute,
     +     longitude,latitude,height,wigos_series
      CHARACTER*5 wigos_issuer,wigos_issueno,missing5
      CHARACTER*9 call_sign,missing9
      CHARACTER*8 flight_number,missing8
      CHARACTER*16 wigos_localid,missing16

      CHARACTER one_bits
      REAL*8 value
      INTEGER idx,cidx,desc,n,maxlevel,numlevels,i,ind,ind2,ind3
      PARAMETER(maxlevel=100000)

      REAL*8 P(maxlevel),D(maxlevel),F(maxlevel),
     +     T(maxlevel),TD(maxlevel),h(maxlevel),
     +     la(maxlevel),lo(maxlevel),tp(maxlevel), ! displacements (lat, lon, time)
     +     wsb(maxlevel),wsa(maxlevel), ! absolute wind shear in 1 km layer below/above
     +     vss(maxlevel)        ! Vertical sounding significance
      LOGICAL highres_data  ! Set to TRUE if containing highres data
      LOGICAL reduce ! Set to TRUE if we should skip levels with
                     ! vertical sounding significance missing or equal
                     ! to 0
      LOGICAL highres_only  ! Set to TRUE if we should skip soundings
                            ! with no high resolution data
      
C     Variables used for geographical filtering av observations
      REAL*8 x1,y1,x2,y2
      COMMON /COM_RECTANGLE/  x1,y1,x2,y2

C     Functions
      INTEGER lenstr
      CHARACTER*9 ctrim  ! length must be >= longest variable ctrim is used for

      IF (obstype.EQ.'sounding-') THEN
         reduce = .TRUE.
      ELSE
         reduce = .FALSE.
      END IF
      IF (obstype.EQ.'soundingh') THEN
         highres_only = .TRUE.
      ELSE
         highres_only = .FALSE.
      END IF

      one_bits = CHAR(255)
      WRITE(missing5,'(5A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits
      WRITE(missing8,'(8A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits,one_bits,one_bits,one_bits
      missing9 = missing8 // one_bits
      missing16 = missing8 // missing8

C     Initialize all parameters to missing values
      call_sign = missing9
      flight_number = missing8
      wigos_issuer = missing5
      wigos_issueno = missing5
      wigos_localid = missing16
      wigos_series = rvind
      II = rvind
      iii= rvind
      year = rvind
      month = rvind
      day = rvind
      hour = rvind
      minute = rvind
      latitude = rvind
      longitude = rvind
      height = rvind
      DO n=1,maxlevel
         P(n) = rvind
         D(n) = rvind
         F(n) = rvind
         T(n) = rvind
         TD(n) = rvind
         h(n) = rvind
         la(n) = rvind
         lo(n) = rvind
         tp(n) = rvind
         vss(n) = rvind
         wsb(n) = rvind
         wsa(n) = rvind
      END DO

C     Loop through all expanded descriptors
      n = 0 ! Numbering the pressure levels
      DO idx=1,ktdexl
         desc = ktdexp(idx)
         value = values(idx + (ksub-1)*kxelem)

C     Nothing to do if value is missing (note that the missing value
C     returned from libbufr might not be exactly equal to rvind)
         IF (ABS(value - rvind)/rvind.LE.0.001) THEN
            CYCLE
         END IF

         IF (desc.EQ.1001) THEN ! WMO block number
            II = value
         ELSE IF (desc.EQ.1002) THEN ! WMO station number
            iii = value
         ELSE IF (desc.EQ.1011.OR.   ! Ship or mobile land station identifier
     +           desc.EQ.1195) THEN  ! Mobil land station identifier
               cidx = int(value/1000)
               call_sign = cvals(cidx) ! CCITTIA5 data
               call_sign = ctrim(call_sign,9,missing9)
         ELSE IF (desc.EQ.1006) THEN ! Aircraft flight number, used for dropsondes
            cidx = int(value/1000)
            flight_number = cvals(cidx) ! CCITTIA5 data
            flight_number = ctrim(flight_number,8,missing8)
         ELSE IF (desc.EQ.1125) THEN ! WIGOS identifier series
            wigos_series = value
         ELSE IF (desc.EQ.1126) THEN ! WIGOS issuer of identifier
            i = NINT(value)
            WRITE(wigos_issuer,'(I5)') i
            wigos_issuer = ADJUSTL(wigos_issuer)
         ELSE IF (desc.EQ.1127) THEN ! WIGOS issue number
            i = NINT(value)
            WRITE(wigos_issueno,'(I5)') i
            wigos_issueno = ADJUSTL(wigos_issueno)
         ELSE IF (desc.EQ.1128) THEN  ! WIGOS local identifier
            cidx = int(value/1000)
            wigos_localid = cvals(cidx) ! CCITTIA5 data
            wigos_localid = ctrim(wigos_localid,16,missing16)
         ELSE IF (desc.EQ.2001) THEN ! Type of station
            ix = value
         ELSE IF (desc.EQ.4001) THEN ! Year
            year = value
         ELSE IF (desc.EQ.4002) THEN ! Month
            month = value
         ELSE IF (desc.EQ.4003) THEN ! Day
            day = value
         ELSE IF (desc.EQ.4004) THEN ! Hour
            hour = value
         ELSE IF (desc.EQ.4005) THEN ! Minute
            minute = value
C     Seen 5002 and 6002 used for each level in high resolution data
         ELSE IF (desc.EQ.5001 .AND. latitude.EQ.rvind) THEN ! Latitude (high accuracy)
            latitude = value
         ELSE IF (desc.EQ.5002 .AND. latitude.EQ.rvind) THEN ! Latitude (coarse accuracy)
            latitude = value
         ELSE IF (desc.EQ.6001 .AND. longitude.EQ.rvind) THEN ! Longitude (high accuracy)
            longitude = value
         ELSE IF (desc.EQ.6002 .AND. longitude.EQ.rvind) THEN ! Longitude (coarse accuracy)
            longitude = value
         ELSE IF (desc.EQ.7001.OR.  ! Height of station
     +           desc.EQ.7030.OR.   ! Height of station ground above mean sea level
     +           desc.EQ.7031.OR.   ! Height of barometer above mean sea level
     +           desc.EQ.7007) THEN ! Height (i.e. of release of sonde above mean sea level)
            IF (height.EQ.rvind) THEN
               height = value
            END IF
         ELSE IF (desc.EQ.4086.AND.n.LT.maxlevel) THEN ! Long time period or displacement [second]
C     In WMO template (309052) descriptors 004086 and 008042 comes BEFORE 7004 pressure
            tp(n+1) = value
         ELSE IF (desc.EQ.8042.AND.n.LT.maxlevel) THEN ! Extended vertical sounding significance
            vss(n+1) = value
         ELSE IF (desc.EQ.7004) THEN ! Pressure
            n = n + 1           ! new level
            IF (n.GT.maxlevel) THEN
               n = maxlevel
               WRITE(*,*) 'Too many levels! Skipping rest of message'
               GOTO 110
            END IF
            P(n) = value
C     All following descriptors come after pressure in 309052 (or Hirlam template)
         ELSE IF (n.GT.0 .AND. n.LE.maxlevel) THEN
            IF (desc.EQ.11001) THEN ! Wind direction
               D(n) = value
            ELSE IF (desc.EQ.11002) THEN ! Wind speed
               F(n) = value
            ELSE IF (desc.EQ.12101) THEN ! Temperature/dry bulb temperature (16 bits)
               T(n) = value
            ELSE IF (desc.EQ.12001) THEN ! Temperature/dry bulb temperature (12 bits)
               T(n) = value
            ELSE IF (desc.EQ.12103) THEN ! Dew-point temperature (16 bits)
               TD(n) = value
            ELSE IF (desc.EQ.12003) THEN ! Dew-point temperature (12 bits)
               TD(n) = value
            ELSE IF (desc.EQ.10003.AND.value.NE.rvind) THEN ! Geopotential
               h(n) = value/9.81
            ELSE IF (desc.EQ.10009) THEN ! Geopotential height
               h(n) = value
            ELSE IF (desc.EQ.5015) THEN ! Latitude displacement
               la(n) = value
            ELSE IF (desc.EQ.6015) THEN ! Longitude displacement
               lo(n) = value
            ELSE IF (desc.EQ.8001 .AND. value.NE.rvind) THEN ! Vertical sounding significance
C     In HIRLAM template (309007) 008001 comes AFTER 7004 pressure
               CALL vss_8001_to_8042(NINT(value),vss(n))
            ELSE IF (desc.EQ.11061) THEN ! Absolute wind shear in 1 km layer below
               wsb(n) = value
            ELSE IF (desc.EQ.11062) THEN ! Absolute wind shear in 1 km layer above
               wsa(n) = value
            END IF
         END IF
      END DO
 110  CONTINUE
      numlevels = n

      IF (rectangle) THEN
         IF (longitude.EQ.rvind .OR. latitude.EQ.rvind
     +        .OR. longitude.LT.x1 .OR. longitude.GT.x2
     +        .OR. latitude.LT.y1 .OR. latitude.GT.y2) RETURN
      END IF

      highres_data = .FALSE.
      DO n=1,numlevels
         IF (tp(n).NE.rvind.OR.la(n).NE.rvind.OR.lo(n).NE.rvind)THEN
            highres_data = .TRUE.
            EXIT
         END IF
      END DO

      IF (highres_only .AND. .NOT. highres_data) THEN
         RETURN
      END IF
         

      IF (II.NE.rvind .AND. iii.NE.rvind) THEN
         WRITE(*,*)
         WRITE(*,'(A,I5.5)') 'wmonr=',NINT(II)*1000 + NINT(iii)
      ELSE IF (wigos_series.NE.rvind .AND. wigos_issuer.NE.missing5
     +        .AND. wigos_issueno.NE.missing5
     +        .AND. wigos_localid.NE.missing16) THEN
         ind = index(wigos_issuer,' ') - 1
         IF (ind.EQ.-1) ind = 5
         ind2 = index(wigos_issueno,' ') - 1
         IF (ind2.EQ.-1) ind2 = 5
         ind3 = index(wigos_localid,' ') - 1
         IF (ind3.EQ.-1) ind3 = 16
         WRITE(*,*)
         WRITE(*,'(A,I1.1,A1,A,A1,A,A1,A)')
     +        'wigosid=',NINT(wigos_series),
     +        '-',wigos_issuer(1:ind),
     +        '-',wigos_issueno(1:ind2),
     +        '-',wigos_localid(1:ind3)
      ELSE IF (call_sign.NE.missing9) THEN
         WRITE(*,*)
         WRITE(*,'(A,A)') 'call_sign=',call_sign(1:lenstr(call_sign,1))
      ELSE IF (flight_number.NE.missing8) THEN
         WRITE(*,*)
         WRITE(*,'(A,A)') 'aircraft=',
     +        flight_number(1:lenstr(flight_number,1))
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*)
            WRITE(*,*) 'Both wmonr, wigosid, call sign and aircraft'
     +           // ' flight number are missing !!!'
         END IF
         RETURN
      END IF

      IF (year.NE.rvind.AND.month.NE.rvind.AND.day.NE.rvind
     +     .AND.hour.NE.rvind.AND.minute.NE.rvind) THEN
         WRITE(*,900) NINT(year),NINT(month),NINT(day),
     +        NINT(hour),NINT(minute)
 900     FORMAT('obstime=',I4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':00')
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*) 'obstime is missing!!!'
            RETURN
         END IF
      ENDIF

C     Extra line included only in bufrdump_netcdf (not in bufrdump)
      IF (highres_data) THEN
         WRITE(*,'(A)') 'resolution=high'
      ELSE
         WRITE(*,'(A)') 'resolution=low'
      END IF
      
      IF (ix.NE.rvind) THEN
         IF (NINT(ix).EQ.0) THEN
            WRITE(*,'(A)') 'type=Automatic'
         ELSE IF (NINT(ix).EQ.1) THEN
            WRITE(*,'(A)') 'type=Manned'
         ELSE IF (NINT(ix).EQ.2) THEN
            WRITE(*,'(A)') 'type=Hybrid'
         END IF
      END IF
      IF (latitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lat=',latitude
      END IF
      IF (longitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lon=',longitude
      END IF
      IF (height.NE.rvind) THEN
         WRITE(*,'(A,I7)') 'height=',NINT(height)
      END IF

      DO n=1,numlevels
         IF (reduce .AND. vss(n).EQ.rvind) CYCLE
         WRITE(*,'(A,I12)') 'n=',n
C     The following 3 parameters are not found in TAC TEMP
         IF (highres_data) THEN
            IF (tp(n).NE.rvind) THEN
               WRITE(*,'(A,I12)') 't=',NINT(tp(n))
            ELSE
               WRITE(*,'(A)') 't='
            END IF
            IF (la(n).NE.rvind) THEN
               WRITE(*,'(A,F11.5)') 'la=',la(n)
            ELSE
               WRITE(*,'(A)') 'la='
            END IF
            IF (lo(n).NE.rvind) THEN
               WRITE(*,'(A,F11.5)') 'lo=',lo(n)
            ELSE
               WRITE(*,'(A)') 'lo='
            END IF
         END IF
C     Choose to display vss even when it is empty
         IF (vss(n).NE.rvind) THEN
            WRITE(*,'(A,F10.1)') 'vss=',vss(n)
         ELSE
            WRITE(*,'(A)') 'vss='
         END IF
         IF (P(n).NE.rvind) THEN
            WRITE(*,'(A,F11.1)') 'PP=',P(n)/100 ! hPa
         ELSE
            WRITE(*,'(A)') 'PP='
         END IF
         IF (h(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'gh=',NINT(h(n))
         ELSE
            WRITE(*,'(A)') 'gh='
         END IF
         IF (D(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'DD=',NINT(D(n))
         ELSE
            WRITE(*,'(A)') 'DD='
         END IF
         IF (F(n).NE.rvind) THEN
            WRITE(*,'(A,F11.1)') 'FF=',F(n)
         ELSE
            WRITE(*,'(A)') 'FF='
         END IF
         IF (T(n).NE.rvind) THEN
            WRITE(*,'(A,F11.1)') 'TT=',T(n)-273.15
         ELSE
            WRITE(*,'(A)') 'TT='
         END IF
         IF (TD(n).NE.rvind) THEN
            WRITE(*,'(A,F11.1)') 'TD=',TD(n)-273.15
         ELSE
            WRITE(*,'(A)') 'TD='
         END IF
         IF (wsb(n).NE.rvind) THEN
            WRITE(*,'(A,F10.1)') 'wsb=',wsb(n)
         END IF
         IF (wsa(n).NE.rvind) THEN
            WRITE(*,'(A,F10.1)') 'wsa=',wsa(n)
         END IF
      END DO

      END SUBROUTINE print_sounding_values

C     -----------------------------------------------------------------

C     Identify height, wind and data quality parameters and print parameter=value to
C     screen for the given heights.
      SUBROUTINE print_radar_profiler_values(ksub,kxelem,ktdexl,ktdexp,
     +     values,cvals,rectangle,verbose)
      IMPLICIT NONE

      INTEGER ksub              ! Input: number of subset currently processed
      INTEGER kxelem            ! Input: expected (max) number of expanded elements
      INTEGER ktdexl            ! Input: number of entries in list of expanded data descriptors
      INTEGER ktdexp(*)         ! Input: array containing expanded data descriptors
      REAL*8 values(*)          ! Input: expanded data values (one subset)
      CHARACTER*80 cvals(*)     ! Input: CCITTIA5 Bufr elements entries (one subset)
      LOGICAL rectangle         ! Input: TRUE if observations are wanted for a rectangle only
      INTEGER verbose           ! Input: verbose level

      REAL*8 rvind              ! missing value for real*8 data
      PARAMETER (rvind=1.7E38)

      REAL*8 II,iii,ix,year,month,day,hour,minute,
     +     longitude,latitude,height
      CHARACTER*9 call_sign,missing9
      CHARACTER one_bits
      REAL*8 value
      INTEGER idx,cidx,desc,n,maxlevel,numlevels,ind
      PARAMETER(maxlevel=1000)

      REAL*8 HH(maxlevel),D(maxlevel),F(maxlevel),
     +     Q1(maxlevel),WC(maxlevel),Q2(maxlevel)

C     Variables used for geographical filtering av observations
      REAL*8 x1,y1,x2,y2
      COMMON /COM_RECTANGLE/  x1,y1,x2,y2

C     Functions
      INTEGER lenstr
      CHARACTER*9 ctrim  ! length must be >= longest variable ctrim is used for

      one_bits = CHAR(255)
      WRITE(missing9,'(9A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits,one_bits,one_bits,one_bits,one_bits

C     Initialize all parameters to missing values
      call_sign = missing9
      II = rvind
      iii= rvind
      year = rvind
      month = rvind
      day = rvind
      hour = rvind
      minute = rvind
      latitude = rvind
      longitude = rvind
      height = rvind
      DO n=1,maxlevel
         HH(n) = rvind
         D(n) = rvind
         F(n) = rvind
         Q1(n) = rvind
         WC(n) = rvind
         Q2(n) = rvind
      END DO

C     Loop through all expanded descriptors
      n = 0 ! Numbering the pressure levels
      DO idx=1,ktdexl
         desc = ktdexp(idx)
         value = values(idx + (ksub-1)*kxelem)

C     Nothing to do if value is missing (note that the missing value
C     returned from libbufr might not be exactly equal to rvind)
         IF (ABS(value - rvind)/rvind.LE.0.001) THEN
            CYCLE
         END IF

         IF (desc.EQ.1001) THEN ! WMO block number
            II = value
         ELSE IF (desc.EQ.1002) THEN ! WMO station number
            iii = value
         ELSE IF (desc.EQ.1011.OR.   ! Ship or mobile land station identifier
     +           desc.EQ.1195) THEN  ! Mobil land station identifier
               cidx = int(value/1000)
               call_sign = cvals(cidx) ! CCITTIA5 data
               call_sign = ctrim(call_sign,9,missing9)
         ELSE IF (desc.EQ.2001) THEN ! Type of station
            ix = value
         ELSE IF (desc.EQ.4001) THEN ! Year
            year = value
         ELSE IF (desc.EQ.4002) THEN ! Month
            month = value
         ELSE IF (desc.EQ.4003) THEN ! Day
            day = value
         ELSE IF (desc.EQ.4004) THEN ! Hour
            hour = value
         ELSE IF (desc.EQ.4005) THEN ! Minute
            minute = value
C     Seen 5002 and 6002 used for each level in high resolution data
         ELSE IF (desc.EQ.5001 .AND. latitude.EQ.rvind) THEN ! Latitude (high accuracy)
            latitude = value
         ELSE IF (desc.EQ.5002 .AND. latitude.EQ.rvind) THEN ! Latitude (coarse accuracy)
            latitude = value
         ELSE IF (desc.EQ.6001 .AND. longitude.EQ.rvind) THEN ! Longitude (high accuracy)
            longitude = value
         ELSE IF (desc.EQ.6002 .AND. longitude.EQ.rvind) THEN ! Longitude (coarse accuracy)
            longitude = value
         ELSE IF (desc.EQ.7001) THEN  ! Height of station
            IF (height.EQ.rvind) THEN
               height = value
            END IF
         ELSE IF (desc.EQ.7007) THEN ! Height
            n = n + 1           ! new level
            IF (n.GT.maxlevel) THEN
               n = maxlevel
               WRITE(*,*) 'Too many levels! Skipping rest of message'
               GOTO 110
            END IF
            HH(n) = value
C     All following descriptors come after 7007 height
         ELSE IF (n.GT.0 .AND. n.LE.maxlevel) THEN
            IF (desc.EQ.11001) THEN ! Wind direction
               D(n) = value
            ELSE IF (desc.EQ.11002) THEN ! Wind speed
               F(n) = value
            ELSE IF (desc.EQ.11006) THEN ! W-component
               WC(n) = value
            ELSE IF (desc.EQ.33002) THEN ! Quality information, included 2 times per level
               IF (ktdexp(idx-1).EQ.11002) THEN
                  Q1(n) = value
               ELSE
                  Q2(n) = value
               END IF
            END IF
         END IF
      END DO
 110  CONTINUE
      numlevels = n

      IF (rectangle) THEN
         IF (longitude.EQ.rvind .OR. latitude.EQ.rvind
     +        .OR. longitude.LT.x1 .OR. longitude.GT.x2
     +        .OR. latitude.LT.y1 .OR. latitude.GT.y2) RETURN
      END IF

      IF (II.NE.rvind .AND. iii.NE.rvind) THEN
         WRITE(*,*)
         WRITE(*,'(A,I5.5)') 'wmonr=',NINT(II)*1000 + NINT(iii)
      ELSE IF (call_sign.NE.missing9) THEN
         WRITE(*,*)
         WRITE(*,'(A,A)') 'call_sign=',call_sign(1:lenstr(call_sign,1))
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*)
            WRITE(*,*) 'Both wmonr and call sign are missing!!!'
         END IF
         RETURN
      END IF

      IF (year.NE.rvind.AND.month.NE.rvind.AND.day.NE.rvind
     +     .AND.hour.NE.rvind.AND.minute.NE.rvind) THEN
         WRITE(*,900) NINT(year),NINT(month),NINT(day),
     +        NINT(hour),NINT(minute)
 900     FORMAT('obstime=',I4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':00')
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*) 'obstime is missing!!!'
            RETURN
         END IF
      ENDIF
      IF (ix.NE.rvind) THEN
         IF (NINT(ix).EQ.0) THEN
            WRITE(*,'(A)') 'type=Automatic'
         ELSE IF (NINT(ix).EQ.1) THEN
            WRITE(*,'(A)') 'type=Manned'
         ELSE IF (NINT(ix).EQ.2) THEN
            WRITE(*,'(A)') 'type=Hybrid'
         END IF
      END IF
      IF (latitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lat=',latitude
      END IF
      IF (longitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lon=',longitude
      END IF
      IF (height.NE.rvind) THEN
         WRITE(*,'(A,I7)') 'height=',NINT(height)
      END IF

      DO n=1,numlevels
         WRITE(*,'(A,I12)') 'n=',n
         IF (HH(n).NE.rvind) THEN
            WRITE(*,'(A,F11.1)') 'HH=',HH(n) ! m
         END IF
         IF (D(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'DD=',NINT(D(n))
         END IF
         IF (F(n).NE.rvind) THEN
            WRITE(*,'(A,F11.1)') 'FF=',F(n)
         END IF
         IF (Q1(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'Q1=',NINT(Q1(n))
         END IF
         IF (WC(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'WC=',NINT(WC(n))
         END IF
         IF (Q2(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'Q2=',NINT(Q2(n))
         END IF
      END DO

      END SUBROUTINE print_radar_profiler_values

C     -----------------------------------------------------------------

      SUBROUTINE print_surface_values(ksub,kxelem,ktdexl,ktdexp,values,
     +     cvals,rectangle,metar,verbose)
C     Identify pressure, temperature etc and print parameter=value to screen
      IMPLICIT NONE

      INTEGER ksub              ! Input: number of subset currently processed
      INTEGER kxelem            ! Input: expected (max) number of expanded elements
      INTEGER ktdexl            ! Input: number of entries in list of expanded data descriptors
      INTEGER ktdexp(*)         ! Input: array containing expanded data descriptors
      REAL*8 values(*)          ! Input: expanded data values (one subset)
      CHARACTER*80 cvals(*)     ! Input: CCITTIA5 Bufr elements entries (one subset)
      LOGICAL rectangle         ! Input: TRUE if observations are wanted for a rectangle only
      LOGICAL metar             ! Input: TRUE if metar (data subcategory 5)
      INTEGER verbose           ! Input: verbose level

      REAL*8 rvind              ! missing value for real*8 data
      PARAMETER (rvind=1.7E38)

      CHARACTER*5 wigos_issuer,wigos_issueno,missing5
      CHARACTER*8 icao_id,missing8
      CHARACTER*9 call_sign,missing9
      CHARACTER*16 wigos_localid,missing16
      CHARACTER*20 name,missing20
      CHARACTER*32 long_name,missing32
C     Parameters defined in Kvalobs
      REAL*8 AA,BI,CH,CI,CL,CM,DD,DG,DG_010,DG_1,DG_X,DI,DW1,DW2,
     +     EE,ES,EV_1,EV_24,FF,FG,FG_010,FG_1,FG_X,FX,FX_1,FX_X,HL,
     +     HW,HW1,HW2,HWA,NH,NN,OT_1,OT_24,PH,PO,PP,PR,PW,PW1,PW2,PWA,
     +     QD,QE,QK,QL,QO,QS,QD_24,QE_24,QK_24,QL_24,QO_24,QS_24,
     +     RR_1,RR_3,RR_6,RR_12,RR_24,RS,SA,SG,SI,SS_24,
     +     TA,TAN_12,TAX_12,TAN,TAX,TD,TGN_12,TW,UU,VV,W1,W2,
     +     WDP1,WHM0,WHMAX,WSPRPT,WTP,WTZ,WW,XIS,ZI,
C     Other parameters
     +     year,month,day,hour,minute,a3,buoy_id5,buoy_id7,ds,
     +     height,hhh,hp,hour_p,II,iii,ix,latitude,longitude,
     +     minute_p,TbTbTb,vert_sign_first,vs,wmo_region_number,
     +     wmo_region_subarea,state_id,national_number,
     +     wigos_series
      REAL*8 vert_sign(4),CC(4),HS(4),NS(4)
      INTEGER idx,cidx
      INTEGER cloud_type_count  ! Will be increased by one for each 020012
                                ! (cloud type) encountered (0 initially).
                                ! Not used for metar
      INTEGER num_cloud_layers  ! Number of individual cloud layers, set
                                ! to value of 031001 (delayed descriptor
                                ! replication factor) if this is met immediately
                                ! after a 020012 descriptor (-1 initially).
                                ! For metar num_cloud_layers is increased
                                ! by one for each new 020011
      LOGICAL bad_cloud_data    ! Set to true if something serious wrong is
                                ! found in cloud data. No more cloud
                                ! data will then be attempted decoded.
      INTEGER cloud_layer       ! Numbers the individual cloud layers
      LOGICAL surface_data      ! Set to false if 007062 ('Depth below sea/water
                                ! surface') is encountered with a value different
                                ! from 0
      LOGICAL time_of_last_position ! Set to true if 008021 time significance is
                                    ! included with value 26
      CHARACTER one_bits
      REAL*8 value
      INTEGER desc,i,mm,hh,ind,ind2,ind3
      INTEGER degree2dir

C     Variables used for geographical filtering av observations
C     (--rectangle option set)
      REAL*8 x1,y1,x2,y2
      COMMON /COM_RECTANGLE/  x1,y1,x2,y2

C     Functions
      CHARACTER*32 ctrim ! length must be >= longest variable ctrim is used for
      INTEGER lenstr

      one_bits = CHAR(255)
      WRITE(missing5,'(5A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits
      WRITE(missing8,'(8A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits,one_bits,one_bits,one_bits
      missing9 = missing8 // one_bits
      missing16 = missing8 // missing8
      missing20 = missing9 // missing9 // one_bits // one_bits
      missing32 = missing16 // missing16

      cloud_type_count = 0
      bad_cloud_data = .FALSE.
      num_cloud_layers = -1
      surface_data = .TRUE.
      time_of_last_position = .FALSE.

C     Initialize all parameters to missing values
      hour_p = rvind
      minute_p = rvind
      icao_id = missing8
      call_sign = missing9
      wigos_issuer = missing5
      wigos_issueno = missing5
      wigos_localid = missing16
      name = missing20
      long_name = missing32
      AA = rvind
      BI = rvind
      CH = rvind
      CI = rvind
      CL = rvind
      CM = rvind
      DD = rvind
      DG = rvind
      DG_010 = rvind
      DG_1 = rvind
      DG_X = rvind
      DI = rvind
      DW1 = rvind
      DW2 = rvind
      EE = rvind
      ES = rvind
      EV_1 = rvind
      EV_24 = rvind
      FF = rvind
      FG = rvind
      FG_010 = rvind
      FG_1 = rvind
      FG_X = rvind
      FX = rvind
      FX_1 = rvind
      FX_X = rvind
      HL = rvind
      HW = rvind
      HW1 = rvind
      HW2 = rvind
      HWA = rvind
      NH = rvind
      NN = rvind
      PO = rvind
      OT_1 = rvind
      OT_24 = rvind
      PH = rvind
      PP = rvind
      PR = rvind
      PR = rvind
      PW = rvind
      PW1 = rvind
      PW2 = rvind
      PWA = rvind
      QD = rvind
      QE = rvind
      QK = rvind
      QL = rvind
      QO = rvind
      QS = rvind
      QD_24 = rvind
      QE_24 = rvind
      QK_24 = rvind
      QL_24 = rvind
      QO_24 = rvind
      QS_24 = rvind
      RR_1 = rvind
      RR_3 = rvind
      RR_6 = rvind
      RR_12 = rvind
      RR_24 = rvind
      RS = rvind
      SA = rvind
      SG = rvind
      SI = rvind
      SS_24 = rvind
      TA = rvind
      TAN_12 = rvind
      TAX_12 = rvind
      TAN = rvind
      TAX = rvind
      TD = rvind
      TGN_12 = rvind
      TW = rvind
      UU = rvind
      VV = rvind
      W1 = rvind
      W2 = rvind
      WDP1 = rvind
      WHM0 = rvind
      WHMAX = rvind
      WSPRPT = rvind
      WTP = rvind
      WTZ = rvind
      WW = rvind
      XIS = rvind
      ZI = rvind
      year = rvind
      month = rvind
      day = rvind
      hour = rvind
      minute = rvind
      latitude = rvind
      longitude = rvind
      height = rvind
      hp = rvind
      vert_sign_first = rvind
      II = rvind
      iii = rvind
      ix = rvind
      a3 = rvind
      hhh = rvind
      ds = rvind
      vs = rvind
      TbTbTb = rvind
      buoy_id5 = rvind
      buoy_id7 = rvind
      wmo_region_number = rvind
      wmo_region_subarea = rvind
      state_id = rvind
      national_number = rvind
      wigos_series = rvind

      DO i=1,4
         vert_sign(i) = rvind
         CC(i) = rvind
         HS(i) = rvind
         NS(i) = rvind
      END DO

C     Loop through all expanded descriptors
      DO idx=1,ktdexl
         desc = ktdexp(idx)
         value = values(idx + (ksub-1)*kxelem)

         IF (ABS(value - rvind)/rvind.LE.0.001) THEN
C     Missing value. Only a few descriptors require a reset of
C     corresponding parameter in this case. Note that the missing value
C     returned from libbufr might not be exactly equal to rvind
            IF (desc.EQ.4024) THEN ! Time period or displacement [hour]
               hour_p = rvind
            ELSE IF (desc.EQ.4025) THEN ! Time period or displacement [minute]
               minute_p = rvind
C     Delayed descriptor replication factor should never be missing
            ELSE IF (desc.EQ.31001) THEN
               IF (idx.GT.1 .AND. ktdexp(idx - 1).EQ.20012) THEN
                  WRITE(*,*) 'WARNING: delayed descriptor replication'
     +                 // ' factor after 020012 undefined!!!'
                  bad_cloud_data = .TRUE.
               ELSE
                  WRITE(*,*) 'WARNING: delayed descriptor replication'
     +                 // ' factor 31001 undefined!!!'
               END IF
C     Some counting needed for clouds even for missing values
            ELSE IF (desc.EQ.20012 .AND. .NOT.bad_cloud_data) THEN ! Cloud type
               cloud_type_count = cloud_type_count + 1
               IF (cloud_type_count.GT.3) THEN
                  cloud_layer = cloud_type_count - 3
               END IF
            END IF
            CYCLE
         END IF

C     Continue the loop for non-missing value. For most variables we
C     choose to not set the parameter if set before, because if a
C     descriptor unexpectedly occurs more than once it is likely that
C     the first occurrence is the 'standard' use of the descriptor,
C     while the later occurrence(s) might for example be due to data
C     required by regional or national reporting practices, added after
C     a standard WMO template
         IF (desc.EQ.4024) THEN ! Time period or displacement [hour]
            hour_p = value
         ELSE IF (desc.EQ.4025) THEN ! Time period or displacement [minute]
            minute_p = value
         ELSE IF (desc.EQ.8021) THEN ! Time significance
            IF (NINT(value).EQ.26) THEN
               time_of_last_position = .TRUE.
            ELSE
               time_of_last_position = .FALSE.
            END IF
         ELSE IF (desc.EQ.1001 .AND. II.EQ.rvind) THEN ! WMO block number
            IF (value.GE.0 .AND. value.LT.100) II = value
         ELSE IF (desc.EQ.1002 .AND. iii.EQ.rvind) THEN ! WMO station number
            IF (value.GE.0 .AND. value.LT.1000) iii = value
         ELSE IF (desc.EQ.1101 .AND. state_id.EQ.rvind) THEN ! WMO member state identifier
            IF (value.GE.0 .AND. value.LT.1000) state_id = value
         ELSE IF (desc.EQ.1102 .AND. national_number.EQ.rvind) THEN ! National station number
            IF (value.GE.0) national_number = value
         ELSE IF (desc.EQ.1015) THEN  ! Station or site name
            cidx = int(value/1000)
            name = cvals(cidx)  ! CCITTIA5 data
            name = ctrim(name,20,missing20)
         ELSE IF (desc.EQ.1019) THEN  ! Long station or site name
            cidx = int(value/1000)
            long_name = cvals(cidx) ! CCITTIA5 data
            long_name = ctrim(long_name,32,missing32)
         ELSE IF (desc.EQ.1125) THEN ! WIGOS identifier series
            wigos_series = value
         ELSE IF (desc.EQ.1126) THEN ! WIGOS issuer of identifier
            i = NINT(value)
            WRITE(wigos_issuer,'(I5)') i
            wigos_issuer = ADJUSTL(wigos_issuer)
         ELSE IF (desc.EQ.1127) THEN ! WIGOS issue number
            i = NINT(value)
            WRITE(wigos_issueno,'(I5)') i
            wigos_issueno = ADJUSTL(wigos_issueno)
         ELSE IF (desc.EQ.1128) THEN  ! WIGOS local identifier
            cidx = int(value/1000)
            wigos_localid = cvals(cidx) ! CCITTIA5 data
            wigos_localid = ctrim(wigos_localid,16,missing16)
         ELSE IF (desc.EQ.2001) THEN ! Type of station
            IF (ix.EQ.rvind) THEN
               ix = value
            END IF
         ELSE IF (desc.EQ.4001) THEN ! Year
            IF (year.EQ.rvind.AND..NOT.time_of_last_position) THEN
               year = value
            END IF
         ELSE IF (desc.EQ.4002) THEN ! Month
            IF (month.EQ.rvind.AND..NOT.time_of_last_position) THEN
               month = value
            END IF
         ELSE IF (desc.EQ.4003) THEN ! Day
            IF (day.EQ.rvind.AND..NOT.time_of_last_position) THEN
               day = value
            END IF
         ELSE IF (desc.EQ.4004) THEN ! Hour
            IF (hour.EQ.rvind.AND..NOT.time_of_last_position) THEN
               hour = value
            END IF
         ELSE IF (desc.EQ.4005) THEN ! Minute
            IF (minute.EQ.rvind.AND..NOT.time_of_last_position) THEN
               minute = value
            END IF
         ELSE IF (desc.EQ.5001.OR.  ! Latitude (high accuracy)
     +           desc.EQ.5002) THEN ! Latitude (coarse accuracy)
            IF (latitude.EQ.rvind) THEN
               latitude = value
            END IF
         ELSE IF (desc.EQ.6001.OR.  ! Longitude (high accuracy)
     +           desc.EQ.6002) THEN ! Longitude (coarse accuracy)
            IF (longitude.EQ.rvind) THEN
               longitude = value
            END IF
         ELSE IF (desc.EQ.7001.OR.  ! Height of station
     +           desc.EQ.7030) THEN ! Height of station ground above mean sea level
            IF (height.EQ.rvind) THEN
               height = value
            END IF
         ELSE IF (desc.EQ.7031) THEN ! Hp
            IF (hp.EQ.rvind) THEN
               hp = value
            END IF
         ELSE IF (desc.EQ.10004) THEN ! Pressure
            IF (PO.EQ.rvind) THEN
               PO = value
            END IF
         ELSE IF (desc.EQ.10051) THEN ! Pressure reduced to mean sea level
            IF (PR.EQ.rvind) THEN
               PR = value
            END IF
         ELSE IF (desc.EQ.10061) THEN ! 3-hour pressure change
            IF (PP.EQ.rvind) THEN
               PP = value
            END IF
         ELSE IF (desc.EQ.10063) THEN ! Characteristic of pressure tendency
            IF (AA.EQ.rvind) THEN
               AA = value
            END IF
         ELSE IF (desc.EQ.11011.OR.  ! Wind direction at 10 m
     +           desc.EQ.11001) THEN ! Wind direction
            IF (DD.EQ.rvind) THEN
               DD = value
            END IF
         ELSE IF (desc.EQ.11012.OR.  ! Wind speed at 10 m
     +           desc.EQ.11002) THEN ! Wind speed
            IF (FF.EQ.rvind) THEN
               FF = value
            END IF
         ELSE IF (desc.EQ.11043) THEN ! Maximum wind gust direction
C     DG is treated same way as FG
            IF (minute_p.NE.rvind .AND. hour.NE.rvind) THEN
               mm = NINT(minute_p)
               hh = NINT(hour)
               IF (mm.EQ.-10) THEN
                  IF (DG_010.EQ.rvind) DG_010 = value
               ELSE IF (mm.GE.-70 .AND. mm.LE.-50) THEN
                  IF (DG_1.EQ.rvind) DG_1 = value
               ELSE IF (mm.EQ.-360 .AND. MOD(hh,6).EQ.0) THEN
                  IF (DG.EQ.rvind) DG = value
               ELSE IF (mm.EQ.-180 .AND. MOD(hh,3).EQ.0
     +                 .AND. MOD(hh,6).NE.0) THEN
                  IF (DG.EQ.rvind) DG = value
               ELSE
C     Actually DG_X is not defined in Kvalobs (but FG_X is!)
                  IF (DG_X.EQ.rvind) DG_X = value
               END IF
            END IF
         ELSE IF (desc.EQ.11041) THEN ! Maximum wind gust speed
            IF (minute_p.NE.rvind .AND. hour.NE.rvind) THEN
               mm = NINT(minute_p)
               hh = NINT(hour)
               IF (mm.EQ.-10) THEN
                  IF (FG_010.EQ.rvind) FG_010 = value
               ELSE IF (mm.GE.-70 .AND. mm.LE.-50) THEN
                  IF (FG_1.EQ.rvind) FG_1 = value
C     For time periods > 1 hour, we choose to decode this as FG for
C     termins 0,6,12,18 if time period is 6 hours, and for termins
C     3,9,15,21 if time period is 3 hours. FG is defined as max wind
C     gust since last synoptic termin (0,6,12,18) and it is unlikely to
C     be reported for other termins than 0,3,6,9...
               ELSE IF (mm.EQ.-360 .AND. MOD(hh,6).EQ.0) THEN
                  IF (FG.EQ.rvind) FG = value
               ELSE IF (mm.EQ.-180 .AND. MOD(hh,3).EQ.0
     +                 .AND. MOD(hh,6).NE.0) THEN
                  IF (FG.EQ.rvind) FG = value
               ELSE
C     All other periods are put in FG_X (arbitrary period)
                  IF (FG_X.EQ.rvind) FG_X = value
               END IF
            END IF
         ELSE IF (desc.EQ.11042) THEN ! Maximum wind speed (10-min mean wind)
            IF (minute_p.NE.rvind .AND. hour.NE.rvind) THEN
C     FX is "Vindhastighet, maks. 10 minutt glidende middel siden
C     forrige hovedobservasjon, m/s". Assume this is reported for
C     termins 0,3,6,9... only
               mm = NINT(minute_p)
               hh = NINT(hour)
               IF ((mm.EQ.-360 .AND. MOD(hh,6).EQ.0)
     +              .OR. (mm.EQ.-180 .AND. MOD(hh,3).EQ.0
     +              .AND. MOD(hh,6).NE.0) .AND. FX.EQ.rvind) THEN
                  IF (FX.EQ.rvind) FX = value
               ELSE IF (mm.GE.-70 .AND. mm.LE.-50) THEN
                  IF (FX_1.EQ.rvind) FX_1 = value
               ELSE
C     All other periods are put in FX_X (arbitrary period)
                  IF (FX_X.EQ.rvind) FX_X = value
               END IF
            END IF
         ELSE IF (desc.EQ.12104.OR. ! Dry bulb temperature at 2m (data width 16 bits)
     +          desc.EQ.12004.OR.   ! Dry bulb temperature at 2m (12 bits)
     +          desc.EQ.12101.OR.   ! Temperature/dry bulb temperature (16 bits)
     +          desc.EQ.12001) THEN ! Temperature/dry bulb temperature (12 bits)
            IF (TA.EQ.rvind) THEN
               TA = value
            END IF
         ELSE IF (desc.EQ.12106.OR.  ! Dew-point temperature at 2m (16 bits)
     +           desc.EQ.12006.OR.   ! Dew-point temperature at 2m (12 bits)
     +           desc.EQ.12103.OR.   ! Dew-point temperature (16 bits)
     +           desc.EQ.12003) THEN ! Dew-point temperature (12 bits)
            IF (TD.EQ.rvind) THEN
               TD = value
            END IF
         ELSE IF (desc.EQ.12113.OR.  ! Ground minimum temperature at 2m (data width 16 bits)
     +           desc.EQ.12013) THEN ! Ground minimum temperature at 2m (12 bits)
            IF (TGN_12.EQ.rvind) THEN
               TGN_12 = value
            END IF
         ELSE IF (desc.EQ.12114.OR.  ! Maximum temperature at 2m, past 12 hours (16 bits)
     +           desc.EQ.12014) THEN ! Maximum temperature at 2m, past 12 hours (12 bits)
            IF (TAX_12.EQ.rvind) THEN
               TAX_12 = value
            END IF
         ELSE IF (desc.EQ.12111) THEN ! Maximum temperature at height and over period specified
            IF (TAX_12.EQ.rvind .AND. idx.GT.2
     +           .AND. ktdexp(idx-1).EQ.4024
     +           .AND. NINT(values(idx-1 + (ksub-1)*kxelem)).EQ.0
     +           .AND. ktdexp(idx-2).EQ.4024) THEN
               IF (NINT(values(idx-2 + (ksub-1)*kxelem)).EQ.-12) THEN
                  IF (TAX_12.EQ.rvind) TAX_12 = value
               ELSE IF (NINT(values(idx-2 + (ksub-1)*kxelem)).EQ.-1)THEN
                  IF (TAX.EQ.rvind) TAX = value
               END IF
            END IF
C     Do we also need to consider 12021 'Maximum temperature at 2m'?
         ELSE IF (desc.EQ.12115.OR.  ! Minimum temperature at 2m, past 12 hours (16 bits)
     +           desc.EQ.12015) THEN ! Minimum temperature at 2m, past 12 hours (12 bits)
            IF (TAN_12.EQ.rvind) THEN
               TAN_12 = value
            END IF
         ELSE IF (desc.EQ.12112) THEN ! Minimum temperature at height and over period specified
            IF (TAN_12.EQ.rvind .AND. idx.GT.2
     +           .AND. ktdexp(idx-1).EQ.4024
     +           .AND. values(idx-1 + (ksub-1)*kxelem).EQ.0
     +           .AND. ktdexp(idx-2).EQ.4024) THEN
               IF (NINT(values(idx-2 + (ksub-1)*kxelem)).EQ.-12) THEN
                  IF (TAN_12.EQ.rvind) TAN_12 = value
               ELSE IF (NINT(values(idx-2 + (ksub-1)*kxelem)).EQ.-1)THEN
                  IF (TAN.EQ.rvind) TAN = value
               END IF
            END IF
C     Do we also need to consider 12022 'Minimum temperature at 2m'?
         ELSE IF (desc.EQ.13003) THEN ! Relative humidity
            IF (UU.EQ.rvind) THEN
               UU = value
            END IF
         ELSE IF (desc.EQ.20001) THEN ! Horizontal visibility
            IF (VV.EQ.rvind) THEN
               VV = value
            END IF
         ELSE IF (desc.EQ.20003) THEN ! Present weather
C     We choose to ignore WW=509, as this value is used in decoded synop
C     when 7 group is missing
            IF (WW.EQ.rvind .AND. NINT(value).NE.509) THEN
               WW = value
            END IF
         ELSE IF (desc.EQ.20004) THEN ! Past weather (1)
            IF (W1.EQ.rvind) THEN
               W1 = value
            END IF
         ELSE IF (desc.EQ.20005) THEN ! Past weather (2)
            IF (W2.EQ.rvind) THEN
               W2 = value
            END IF
         ELSE IF (desc.EQ.20010) THEN ! Cloud cover (total)
            IF (NN.EQ.rvind) THEN
               NN = value
            END IF
         ELSE IF (desc.EQ.31001) THEN
C     Delayed descriptor replication factor; this should be
C     number of cloud layers if previous descriptor is cloud
C     type, according to all WMO recommended templates
C     (but DNMI metar is an exception!)
            IF (ktdexp(idx - 1).EQ.20012) THEN
               IF (NINT(value).LE.4) THEN
                  num_cloud_layers = NINT(value)
               ELSE
                  bad_cloud_data = .TRUE.
               END IF
            END IF
         ELSE IF (desc.EQ.8002 .AND. .NOT.bad_cloud_data) THEN ! Vertical significance (surface observations)
            IF (cloud_type_count.EQ.0) THEN ! First occurrence
               IF (vert_sign_first.EQ.rvind) THEN
                  vert_sign_first = value
               END IF
            ELSE IF (cloud_type_count.LT.3) THEN
               bad_cloud_data = .TRUE. ! There should always be three 020012
                                       ! following first 008002
            ELSE IF (num_cloud_layers.GT.-1) THEN
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LE.num_cloud_layers) THEN
                  vert_sign(cloud_layer) = value
               END IF
            ELSE                ! rdb-files always have 0 or 4 cloud layers
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LT.5) THEN
                  vert_sign(cloud_layer) = value
               END IF
            END IF
         ELSE IF (desc.EQ.20011 .AND. .NOT.bad_cloud_data) THEN ! Cloud amount
            IF (metar) THEN
               IF (num_cloud_layers.GT.-1) THEN
                  num_cloud_layers = num_cloud_layers + 1
               ELSE
                  num_cloud_layers = 1
               END IF
               NS(num_cloud_layers) = value
            ELSE IF (cloud_type_count.EQ.0) THEN ! First occurrence
               IF (NH.EQ.rvind) THEN
                  NH = value
               END IF
            ELSE IF (cloud_type_count.LT.3) THEN
               bad_cloud_data = .TRUE. ! There should always be three 020012
                                       ! following first 008002
            ELSE IF (num_cloud_layers.GT.-1) THEN
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LE.num_cloud_layers) THEN
                  NS(cloud_layer) = value
               END IF
            ELSE                ! rdb-files always have 0 or 4 cloud layers
               cloud_layer = cloud_type_count - 2
               IF (cloud_layer.LT.5) THEN
                  NS(cloud_layer) = value
               END IF
            END IF
         ELSE IF (desc.EQ.20012 .AND. .NOT.bad_cloud_data) THEN ! Cloud type
            IF (metar) THEN
               CC(num_cloud_layers) = value
            ELSE   
               cloud_type_count = cloud_type_count + 1
               IF (cloud_type_count.GT.3) THEN
                  cloud_layer = cloud_type_count - 3
                  IF (num_cloud_layers .GT.-1) THEN
                     IF (value < 10.0 ! Accept one digit values only
     +                    .AND. cloud_layer.LE.num_cloud_layers) THEN
                        CC(cloud_layer) = value
                     END IF
                  ELSE IF (cloud_layer.LT.5) THEN ! rdb-files always have 0 or 4 cloud layers
                     CC(cloud_layer) = value
                  END IF
               ELSE
                  IF (cloud_type_count.EQ.1) THEN
                     IF (CL.EQ.rvind) THEN
                        CL = value
                     END IF
                  ELSE IF (cloud_type_count.EQ.2) THEN
                     IF (CM.EQ.rvind) THEN
                        CM = value
                     END IF
                  ELSE IF (cloud_type_count.EQ.3) THEN
                     IF (CH.EQ.rvind) THEN
                        CH = value
                     END IF
                  END IF
               END IF
            END IF
         ELSE IF (desc.EQ.20013 .AND. .NOT.bad_cloud_data) THEN ! Height of base of cloud
            IF (metar) THEN
               HS(num_cloud_layers) = value
            ELSE IF (cloud_type_count.EQ.0) THEN ! First occurrence
               IF (HL.EQ.rvind) THEN
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
                  HS(cloud_layer) = value
               END IF
            ELSE ! rdb-files always have 0 or 4 cloud layers
               cloud_layer = cloud_type_count - 3
               IF (cloud_layer.LT.5) THEN
                  HS(cloud_layer) = value
               END IF
            END IF
         ELSE IF (desc.EQ.13023) THEN ! Total precipitation past 24 hours
            IF (RR_24.EQ.rvind) THEN
               RR_24 = value
            END IF
         ELSE IF (desc.EQ.13022) THEN ! Total precipitation past 12 hours
            IF (RR_12.EQ.rvind) THEN
               RR_12 = value
            END IF
         ELSE IF (desc.EQ.13021) THEN ! Total precipitation past 6 hours
            IF (RR_6.EQ.rvind) THEN
               RR_6 = value
            END IF
         ELSE IF (desc.EQ.13020) THEN ! Total precipitation past 3 hours
            IF (RR_3.EQ.rvind) THEN
               RR_3 = value
            END IF
         ELSE IF (desc.EQ.13019) THEN ! Total precipitation past 1 hour
            IF (RR_1.EQ.rvind) THEN
               RR_1 = value
            END IF
         ELSE IF (desc.EQ.13011) THEN ! Total precipitation/total water equivalent
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-24) THEN
                  RR_24 = value
               ELSE IF (hh.EQ.-12) THEN
                  RR_12 = value
               ELSE IF (hh.EQ.-6) THEN
                  RR_6 = value
               ELSE IF (hh.EQ.-3) THEN
                  RR_3 = value
               ELSE IF (hh.EQ.-1) THEN
                  RR_1 = value
               END IF
            END IF
         ELSE IF (desc.EQ.13013) THEN ! Total snow depth
C     Don't check for SA.EQ.rvind, because SA might earlier have been set to 0 if
C     EE < 10, which probably means that 20062 has been wrongly encoded, not 13013
            SA = value
         ELSE IF (desc.EQ.20062) THEN ! State of the ground (with or without snow)
            IF (EE.EQ.rvind) THEN
               EE = value
            END IF
            IF (NINT(value).LE.10 .AND. SA.EQ.rvind) THEN
               SA = 0
            END IF
C     Equating 013012 with SS_24 is dubious generally, but this is how
C     SS_24 is encoded in kvalobs for Norwegian avalanche stations (and
C     we have no better kvalobs parameter for depth of fresh snow anyway)
         ELSE IF (desc.EQ.13012) THEN ! Depth of fresh snow
            IF (SS_24.EQ.rvind) THEN
               SS_24 = value
            END IF
         ELSE IF (desc.EQ.14031) THEN ! Total sunshine
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  OT_1 = value
               ELSE IF (hh.EQ.-24) THEN
                  OT_24 = value
               END IF
            END IF
         ELSE IF (desc.EQ.13033) THEN ! Evaporation/evapotranspiration
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  EV_1 = value
               ELSE IF (hh.EQ.-24) THEN
                  EV_24 = value
               END IF
            END IF
         ELSE IF (desc.EQ.14002) THEN ! Long-wave radiation
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  QL = value
               ELSE IF (hh.EQ.-24) THEN
                  QL_24 = value
               END IF
            END IF
         ELSE IF (desc.EQ.14004) THEN ! Short-wave radiation
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  QK = value
               ELSE IF (hh.EQ.-24) THEN
                  QK_24 = value
               END IF
            END IF
         ELSE IF (desc.EQ.14016) THEN ! Net radiation
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  QE = value
               ELSE IF (hh.EQ.-24) THEN
                  QE_24 = value
               END IF
            END IF
         ELSE IF (desc.EQ.14028) THEN ! Global solar radiation
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  QO = value
               ELSE IF (hh.EQ.-24) THEN
                  QO_24 = value
               END IF
            END IF
         ELSE IF (desc.EQ.14029) THEN ! Diffuse solar radiation
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  QD = value
               ELSE IF (hh.EQ.-24) THEN
                  QD_24 = value
               END IF
            END IF
         ELSE IF (desc.EQ.14030) THEN ! Direct solar radiation
            IF (hour_p.NE.rvind) THEN
               hh = NINT(hour_p)
               IF (hh.EQ.-1) THEN
                  QS = value
               ELSE IF (hh.EQ.-24) THEN
                  QS_24 = value
               END IF
            END IF
C     Special for high altitude stations
         ELSE IF (desc.EQ.7004) THEN ! Pressure (location class)
            IF (a3.EQ.rvind) THEN
               a3 = value
            END IF
         ELSE IF (desc.EQ.10009) THEN ! Geopotential height
            IF (hhh.EQ.rvind) THEN
               hhh = value
            END IF
C     Special for ship or marine stations
         ELSE IF (desc.EQ.1011) THEN  ! Ship or mobile land station identifier
            cidx = int(value/1000)
            IF (cidx.GT.0) THEN
               call_sign = cvals(cidx) ! CCITTIA5 data
               call_sign = ctrim(call_sign,9,missing9)
            END IF
         ELSE IF (desc.EQ.1012) THEN ! Direction of motion of moving observing platform
            IF (ds.EQ.rvind) THEN
               ds = value
            END IF
         ELSE IF (desc.EQ.1013) THEN ! Speed of motion of moving observing platform
            IF (vs.EQ.rvind) THEN
               vs = value
            END IF
         ELSE IF (desc.EQ.7062) THEN ! Depth below sea/water surface
C     Some buoy reports starts with depth 1.5 m, others starts with 0 m
C     then 1.5 m and always have same sea/water temperature for these 2
C     levels, so it seems like 0 m should be considered equivalent with 1.5 m
            IF (value.LT.1.6) THEN
               surface_data = .TRUE.
            ELSE
               surface_data = .FALSE.
            END IF
         ELSE IF (desc.EQ.22043.OR.  ! Sea/water temperature (15 bits)
     +           desc.EQ.22042.OR.   ! Sea/water temperature (12 bits)
     +           desc.EQ.22049) THEN ! Sea-surface temperature (15 bits)
            IF (TW.EQ.rvind .AND. surface_data)THEN
               TW = value
            END IF
         ELSE IF (desc.EQ.12102.OR.  ! Wet-bulb temperature (16 bits)
     +           desc.EQ.12005) THEN ! Wet-bulb temperature (12 bits)
            IF (TbTbTb.EQ.rvind) THEN
               TbTbTb = value
            END IF
         ELSE IF (desc.EQ.22011) THEN !  Period of waves (instrumentally measured)
            IF (PWA.EQ.rvind) THEN
               PWA = value
            END IF
         ELSE IF (desc.EQ.22012) THEN !  Period of wind waves (visually measured)
            IF (PW.EQ.rvind) THEN
               PW = value
            END IF
         ELSE IF (desc.EQ.22021) THEN ! Heigth of waves
            IF (HWA.EQ.rvind) THEN
               HWA = value
            END IF
         ELSE IF (desc.EQ.22022) THEN ! Heigth of wind waves
            IF (HW.EQ.rvind) THEN
               HW = value
            END IF
         ELSE IF (desc.EQ.22003) THEN ! Direction of swell waves
            IF (DW1.EQ.rvind) THEN
               DW1 = value
            ELSE
               DW2 = value
            END IF
         ELSE IF (desc.EQ.22013) THEN ! Period of swell waves
            IF (PW1.EQ.rvind) THEN
               PW1 = value
            ELSE
               PW2 = value
            END IF
         ELSE IF (desc.EQ.22023) THEN ! Height of swell waves
            IF (HW1.EQ.rvind) THEN
               HW1 = value
            ELSE
               HW2 = value
            END IF
         ELSE IF (desc.EQ.22070) THEN ! Significant wave height
            IF (WHM0.EQ.rvind) THEN
               WHM0 = value
            END IF
         ELSE IF (desc.EQ.22073) THEN ! Maximum wave height
            IF (WHMAX.EQ.rvind) THEN
               WHMAX = value
            END IF
         ELSE IF (desc.EQ.22071) THEN ! Spectral peak wave period
            IF (WTP.EQ.rvind) THEN
               WTP = value
            END IF
         ELSE IF (desc.EQ.22074) THEN ! Average wave period
            IF (WTZ.EQ.rvind) THEN
               WTZ = value
            END IF
         ELSE IF (desc.EQ.22076) THEN ! Direction from which dominant waves are coming
            IF (WDP1.EQ.rvind) THEN
               WDP1 = value
            END IF
         ELSE IF (desc.EQ.22077) THEN ! Directional spread of dominant wave
            IF (WSPRPT.EQ.rvind) THEN
               WSPRPT = value
            END IF
         ELSE IF (desc.EQ.22061) THEN ! State of the sea
            IF (SG.EQ.rvind) THEN
               SG = value
            END IF
         ELSE IF (desc.EQ.20033) THEN ! Cause of ice accretion
            IF (XIS.EQ.rvind) THEN
               XIS = value
            END IF
         ELSE IF (desc.EQ.20031) THEN ! Ice deposit (thickness)
            IF (ES.EQ.rvind) THEN
               ES = value
            END IF
         ELSE IF (desc.EQ.20032) THEN ! Rate of ice accretion
            IF (RS.EQ.rvind) THEN
               RS = value
            END IF
         ELSE IF (desc.EQ.20034) THEN ! Sea ice concentration
            IF (CI.EQ.rvind) THEN
               CI = value
            END IF
         ELSE IF (desc.EQ.20037) THEN ! Ice development
            IF (SI.EQ.rvind) THEN
               SI = value
            END IF
         ELSE IF (desc.EQ.20035) THEN ! Amount and type of ice
            IF (BI.EQ.rvind) THEN
               BI = value
            END IF
         ELSE IF (desc.EQ.20038) THEN ! Bearing of ice edge
            IF (DI.EQ.rvind) THEN
               DI = value
            END IF
         ELSE IF (desc.EQ.20036) THEN ! Ice situation
            IF (ZI.EQ.rvind) THEN
               ZI = value
            END IF
         ELSE IF (desc.EQ.1005) THEN ! Buoy/platform identifier
            IF (buoy_id5.EQ.rvind) THEN
               buoy_id5 = value
            END IF
         ELSE IF (desc.EQ.1003) THEN ! WMO region number/geographical area
            IF (wmo_region_number.EQ.rvind) THEN
               wmo_region_number = value
            END IF
         ELSE IF (desc.EQ.1020) THEN ! WMO region sub-area
            IF (wmo_region_subarea.EQ.rvind) THEN
               wmo_region_subarea = value
            END IF
         ELSE IF (desc.EQ.1087) THEN ! WMO Marine observing platform extended identifier
            IF (buoy_id7.EQ.rvind) THEN
               buoy_id7 = value
            END IF
C     Special for metar
         ELSE IF (desc.EQ.1063) THEN  ! ICAO location indicator
            cidx = int(value/1000)
            icao_id = cvals(cidx) ! CCITTIA5 data
            icao_id = ctrim(icao_id,8,missing8)
         ELSE IF (desc.EQ.10052) THEN ! Altimeter setting (QNH)
            IF (PH.EQ.rvind) THEN
               PH = value
            END IF
         END IF
      END DO

      IF (rectangle) THEN
         IF (longitude.EQ.rvind .OR. latitude.EQ.rvind
     +        .OR. longitude.LT.x1 .OR. longitude.GT.x2
     +        .OR. latitude.LT.y1 .OR. latitude.GT.y2) RETURN
      END IF

      IF (II.NE.rvind .AND. iii.NE.rvind) THEN
         WRITE(*,*)
         WRITE(*,'(A,I5.5)') 'wmonr=',NINT(II)*1000 + NINT(iii)
      ELSE IF (wigos_series.NE.rvind .AND. wigos_issuer.NE.missing5
     +        .AND. wigos_issueno.NE.missing5
     +        .AND. wigos_localid.NE.missing16) THEN
         ind = index(wigos_issuer,' ') - 1
         IF (ind.EQ.-1) ind = 5
         ind2 = index(wigos_issueno,' ') - 1
         IF (ind2.EQ.-1) ind2 = 5
         ind3 = index(wigos_localid,' ') - 1
         IF (ind3.EQ.-1) ind3 = 16
         WRITE(*,*)
         WRITE(*,'(A,I1.1,A1,A,A1,A,A1,A)')
     +        'wigosid=',NINT(wigos_series),
     +        '-',wigos_issuer(1:ind),
     +        '-',wigos_issueno(1:ind2),
     +        '-',wigos_localid(1:ind3)
      ELSE IF (state_id.NE.rvind .AND. national_number.NE.rvind) THEN
         WRITE(*,*)
         WRITE(*,'(A,I3.3,A1,I10.10)') 'nationalnr=',NINT(state_id),
     +        '_',NINT(national_number)
      ELSE IF (call_sign.NE.missing9) THEN
         ind = index(call_sign,' ') - 1
         IF (ind.EQ.-1) ind = 9
C     Remove trailing NULL characters, which some centres erronously
C     insert
         DO WHILE (IACHAR(call_sign(ind:ind)).EQ.0)
            ind = ind - 1
         END DO
         WRITE(*,*)
         WRITE(*,'(A,A)') 'call_sign=',
     +        call_sign(1:ind)
      ELSE IF (buoy_id7.NE.rvind) THEN
C     New templates introduced in 2014 for data category 1 use 001087
C     WMO Marine observing platform extended identifier, 7 digits
         WRITE(*,*)
         WRITE(*,'(A,I7)') 'buoy_id=',NINT(buoy_id7)
      ELSE IF (buoy_id5.NE.rvind) THEN
         WRITE(*,*)
         IF (wmo_region_number.EQ.rvind
     +        .AND.wmo_region_subarea.EQ.rvind) THEN
C     Old drau files (wrongly) includes wmo_region_number and
C     wmo_region_subarea in 001005 Buoy/platform identifier. Should we
C     expand this to 7 digits by inserting '00'?
            WRITE(*,'(A,I5)') 'buoy_id=',NINT(buoy_id5)
         ELSE IF (wmo_region_number.EQ.rvind
     +        .AND.wmo_region_subarea.NE.rvind) THEN
C     Some BUFR BUOYS on GTS have 'missing' value for wmo_region_number,
C     but not for wmo_region_subarea
            WRITE(*,'(A,I6)') 'buoy_id=',
     +           NINT(wmo_region_subarea)*100000 + NINT(buoy_id5)
         ELSE IF (wmo_region_number.NE.rvind
     +        .AND.wmo_region_subarea.EQ.rvind) THEN
C     Not easy to know how to display this case, but then I have never
C     seen this in practice
            WRITE(*,'(A,I5)') 'buoy_id=',NINT(buoy_id5)
         ELSE IF (buoy_id5.GT.10000.AND.
     +           (NINT(buoy_id5) - MOD(NINT(buoy_id5),1000)).EQ.
     +           NINT(wmo_region_number)*10000
     +           + NINT(wmo_region_subarea)*1000) THEN
C     If first 2 digits of 5 digit buoy_id equals wmo_region_number and
C     wmo_region_subarea respectively, this is almost certainly an
C     encoding error and we choose to show last 3 digits of buoy_id5 only
           WRITE(*,'(A,I7)') 'buoy_id=',NINT(wmo_region_number)*1000000
     +           + NINT(wmo_region_subarea)*100000
     +           + MOD(NINT(buoy_id5),1000)
         ELSE
           WRITE(*,'(A,I7)') 'buoy_id=',NINT(wmo_region_number)*1000000
     +           + NINT(wmo_region_subarea)*100000 + NINT(buoy_id5)
         END IF
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*)
            WRITE(*,*) 'Both wmonr, wigosid, nationalnr, call_sign',
     +           ' and buoy_id are missing!!!'
C     Example: ISRZ47 EGRR has no proper station identification (except
C     station name and position)
         END IF
         RETURN
      END IF

      IF (year.NE.rvind.AND.month.NE.rvind.AND.day.NE.rvind
     +     .AND.hour.NE.rvind.AND.minute.NE.rvind) THEN
         WRITE(*,900) NINT(year),NINT(month),NINT(day),
     +        NINT(hour),NINT(minute)
 900     FORMAT('obstime=',I4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':00')
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*) 'obstime is missing!!!'
            RETURN
         END IF
      ENDIF
      IF (icao_id.NE.missing8) THEN
         WRITE(*,'(A,A)') 'icao_id=',icao_id(1:lenstr(icao_id,1))
      END IF
      IF (name.NE.missing20) THEN
         WRITE(*,'(A,A)') 'name=',name(1:lenstr(name,1))
      END IF
      IF (long_name.NE.missing32) THEN
         WRITE(*,'(A,A)') 'name=',long_name(1:lenstr(long_name,1))
      END IF
      IF (ix.NE.rvind) THEN
         IF (NINT(ix).EQ.0) THEN
            WRITE(*,'(A)') 'type=Automatic'
         ELSE IF (NINT(ix).EQ.1) THEN
            WRITE(*,'(A)') 'type=Manned'
         ELSE IF (NINT(ix).EQ.2) THEN
            WRITE(*,'(A)') 'type=Hybrid'
         END IF
      END IF
      IF (longitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lon=',longitude
      END IF
      IF (latitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lat=',latitude
      END IF
      IF (height.NE.rvind) THEN ! 7030 has scale 1 but 7003 has scale 0 and is more common,
                                ! so display as integer
         WRITE(*,'(A,I7)') 'height=',NINT(height)
      END IF
      IF (hp.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'hp=',hp
      END IF
      IF (vs.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'vs=',vs
      END IF
      IF (ds.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'ds=',NINT(ds)
      END IF
      IF (a3.NE.rvind) THEN     ! Standard isobaric surface for which the
                                ! geopotential is reported, no Kvalobs code exists
         WRITE(*,'(A,I11)') 'a3=',NINT(a3/100) ! hPa
      END IF
      IF (hhh.NE.rvind) THEN    ! Geopotential height, no Kvalobs code exists
         WRITE(*,'(A,F10.1)') 'hhh=',hhh
      END IF
      IF (DD.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'DD=',NINT(DD)
      END IF
      IF (FF.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'FF=',FF
      END IF
      IF (FX.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'FX=',FX
      END IF
      IF (FX_1.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'FX_1=',FX_1
      END IF
      IF (FX_X.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'FX_X=',FX_X
      END IF
      IF (DG.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'DG=',DG
      END IF
      IF (FG.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'FG=',FG
      END IF
      IF (DG_010.NE.rvind) THEN
         WRITE(*,'(A,F7.1)') 'DG_010=',DG_010
      END IF
      IF (FG_010.NE.rvind) THEN
         WRITE(*,'(A,F7.1)') 'FG_010=',FG_010
      END IF
      IF (DG_1.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'DG_1=',DG_1
      END IF
      IF (FG_1.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'FG_1=',FG_1
      END IF
      IF (DG_X.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'DG_X=',DG_X
      END IF
      IF (FG_X.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'FG_X=',FG_X
      END IF
      IF (TA.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'TA=',TA-273.15
      END IF
      IF (TD.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'TD=',TD-273.15
      END IF
      IF (TAX_12.NE.rvind) THEN
         WRITE(*,'(A,F7.1)') 'TAX_12=',TAX_12-273.15
      END IF
      IF (TAN_12.NE.rvind) THEN
         WRITE(*,'(A,F7.1)') 'TAN_12=',TAN_12-273.15
      END IF
      IF (TAX.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'TAX=',TAX-273.15
      END IF
      IF (TAN.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'TAN=',TAN-273.15
      END IF
      IF (TGN_12.NE.rvind) THEN
         WRITE(*,'(A,F7.1)') 'TGN_12=',TGN_12-273.15
      END IF
      IF (TW.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'TW=',TW-273.15
      END IF
      IF (TbTbTb.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'Tb=',TbTbTb-273.15
      END IF
      IF (UU.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'UU=',NINT(UU)
      END IF
      IF (PO.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'PO=',PO/100 ! hPa
      END IF
      IF (PR.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'PR=',PR/100 ! hPa
      END IF
      IF (PH.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'PH=',PH/100 ! hPa
      END IF
      IF (AA.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'AA=',NINT(AA)
      END IF
      IF (PP.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'PP=',PP/100 ! hPa
      END IF
C     Use the BUFR some special values for precipitation, *not* the
C     Kvalobs special values. So 'trace' (less than 0.05 kg/m2), is
C     showed as -0.1 (not 0 as in Kvalobs)
      IF (RR_24.NE.rvind) THEN
         WRITE(*,'(A,F8.1)') 'RR_24=',RR_24
      END IF
      IF (RR_12.NE.rvind) THEN
         WRITE(*,'(A,F8.1)') 'RR_12=',RR_12
      END IF
      IF (RR_6.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'RR_6=',RR_6
      END IF
      IF (RR_3.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'RR_3=',RR_3
      END IF
      IF (RR_1.NE.rvind) THEN
         WRITE(*,'(A,F9.1)') 'RR_1=',RR_1
      END IF
      IF (WW.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'WW=',NINT(WW)
      END IF
      IF (W1.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'W1=',NINT(W1)
      END IF
      IF (W2.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'W2=',NINT(W2)
      END IF
      IF (EE.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'EE=',NINT(EE)
      END IF
C     Use the BUFR some special values for SA, *not* the Kvalobs special values
C     So 'trace' is showed as SA=-1, 'Snow cover not continuous' as SA=-2
C     Note that conversion from synop to BUFR normally will set SA=0 if E < 10
      IF (SA.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'SA=',NINT(SA*100)
      END IF
      IF (SS_24.NE.rvind) THEN
         WRITE(*,'(A,I8)') 'SS_24=',NINT(SS_24*100)
      END IF
      IF (VV.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'VV=',NINT(VV)
      END IF
      IF (NN.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'NN=',NN
      END IF
      IF (HL.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'HL=',NINT(HL)
      END IF
      IF (NH.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'NH=',NINT(NH)
      END IF
      IF (CL.NE.rvind) THEN
         IF (NINT(CL).GE.30.AND.NINT(CL).LT.40) THEN
            WRITE(*,'(A,I11)') 'CL=',NINT(CL)
         END IF
      END IF
      IF (CM.NE.rvind) THEN
         IF (NINT(CM).GE.20.AND.NINT(CM).LT.30) THEN
            WRITE(*,'(A,I11)') 'CM=',NINT(CM)
         END IF
      END IF
      IF (CH.NE.rvind) THEN
         IF (NINT(CH).GE.10.AND.NINT(CH).LT.20) THEN
            WRITE(*,'(A,I11)') 'CH=',NINT(CH)
         END IF
      END IF
C     Cloud layer data
      DO i=1,num_cloud_layers
         IF (NS(i).NE.rvind) THEN
            WRITE(*,'(A,I1,A,I10)') 'NS',i,'=',NINT(NS(i))
         END IF
         IF (CC(i).NE.rvind) THEN
            WRITE(*,'(A,I1,A,I10)') 'CC',i,'=',NINT(CC(i))
         END IF
         IF (HS(i).NE.rvind) THEN
            WRITE(*,'(A,I1,A,I10)') 'HS',i,'=',NINT(HS(i))
         END IF
      END DO
      IF (SG.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'SG=',NINT(SG)
      END IF
      IF (PWA.NE.rvind) THEN
         WRITE(*,'(A,I10)') 'PWA=',NINT(PWA)
      END IF
      IF (PW.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'PW=',NINT(PW)
      END IF
      IF (HWA.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'HWA=',HWA
      END IF
      IF (HW.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'HW=',HW
      END IF
      IF (DW1.NE.rvind) THEN
         WRITE(*,'(A,I10)') 'DW1=',NINT(DW1)
      END IF
      IF (PW1.NE.rvind) THEN
         WRITE(*,'(A,I10)') 'PW1=',NINT(PW1)
      END IF
      IF (HW1.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'HW1=',HW1
      END IF
      IF (DW2.NE.rvind) THEN
         WRITE(*,'(A,I10)') 'DW2=',NINT(DW2)
      END IF
      IF (PW2.NE.rvind) THEN
         WRITE(*,'(A,I10)') 'PW2=',NINT(PW2)
      END IF
      IF (HW2.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'HW2=',HW2
      END IF
      IF (WHM0.NE.rvind) THEN
         WRITE(*,'(A,F9.2)') 'WHM0=',WHM0
      END IF
      IF (WHMAX.NE.rvind) THEN
         WRITE(*,'(A,F8.2)') 'WHMAX=',WHMAX
      END IF
      IF (WDP1.NE.rvind) THEN
         WRITE(*,'(A,I9)') 'WDP1=',NINT(WDP1)
      END IF
      IF (WSPRPT.NE.rvind) THEN
         WRITE(*,'(A,I7)') 'WSPRPT=',NINT(WSPRPT)
      END IF
      IF (WTP.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'WTP=',WTP
      END IF
      IF (WTZ.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'WTZ=',WTZ
      END IF
      IF (XIS.NE.rvind) THEN
         WRITE(*,'(A,F10.1)') 'XIS=',XIS
      END IF
      IF (ES.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'ES=',ES/10
      END IF
      IF (RS.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'RS=',NINT(RS)
      END IF
      IF (CI.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'CI=',NINT(CI)
      END IF
      IF (SI.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'SI=',NINT(SI)
      END IF
      IF (BI.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'BI=',NINT(BI)
      END IF
      IF (DI.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'DI=',degree2dir(DI)
      END IF
      IF (ZI.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'ZI=',NINT(ZI)
      END IF
      IF (OT_1.NE.rvind) THEN
         WRITE(*,'(A,I9)') 'OT_1=',NINT(OT_1)
      END IF
      IF (OT_24.NE.rvind) THEN
         WRITE(*,'(A,I8)') 'OT_24=',NINT(OT_24)
      END IF
      IF (QE.NE.rvind) THEN
         WRITE(*,'(A,F11.2)') 'QE=',QE/3600 ! Wh/m2
      END IF
      IF (QO.NE.rvind) THEN
         WRITE(*,'(A,F11.2)') 'QO=',QO/3600
      END IF
      IF (QL.NE.rvind) THEN
         WRITE(*,'(A,F11.2)') 'QL=',QL/3600
      END IF
      IF (QK.NE.rvind) THEN
         WRITE(*,'(A,F11.2)') 'QK=',QK/3600
      END IF
      IF (QD.NE.rvind) THEN
         WRITE(*,'(A,F11.2)') 'QD=',QD/3600
      END IF
      IF (QS.NE.rvind) THEN
         WRITE(*,'(A,F11.2)') 'QS=',QS/3600
      END IF
      IF (QE_24.NE.rvind) THEN
         WRITE(*,'(A,F8.2)') 'QE_24=',QE_24/3600 ! Wh/m2
      END IF
      IF (QO_24.NE.rvind) THEN
         WRITE(*,'(A,F8.2)') 'QO_24=',QO_24/3600
      END IF
      IF (QL_24.NE.rvind) THEN
         WRITE(*,'(A,F8.2)') 'QL_24=',QL_24/3600
      END IF
      IF (QK_24.NE.rvind) THEN
         WRITE(*,'(A,F8.2)') 'QK_24=',QK_24/3600
      END IF
      IF (QD_24.NE.rvind) THEN
         WRITE(*,'(A,F8.2)') 'QD_24=',QD_24/3600
      END IF
      IF (QS_24.NE.rvind) THEN
         WRITE(*,'(A,F8.2)') 'QS_24=',QS_24/3600
      END IF
      IF (EV_1.NE.rvind) THEN
         WRITE(*,'(A,I9)') 'EV_1=',NINT(EV_1)
      END IF
      IF (EV_24.NE.rvind) THEN
         WRITE(*,'(A,I8)') 'EV_24=',NINT(EV_24)
      END IF

      END SUBROUTINE print_surface_values

C     -----------------------------------------------------------------

      SUBROUTINE print_oceanographic_values(ksub,kxelem,ktdexl,ktdexp,
     +     values,cvals,rectangle,verbose)
C     Identify pressure, temperature etc and print parameter=value to screen
      IMPLICIT NONE

      INTEGER ksub              ! Input: number of subset currently processed
      INTEGER kxelem            ! Input: expected (max) number of expanded elements
      INTEGER ktdexl            ! Input: number of entries in list of expanded data descriptors
      INTEGER ktdexp(*)         ! Input: array containing expanded data descriptors
      REAL*8 values(*)          ! Input: expanded data values (one subset)
      CHARACTER*80 cvals(*)     ! Input: CCITTIA5 Bufr elements entries (one subset)
      LOGICAL rectangle         ! Input: TRUE if observations are wanted for a rectangle only
      INTEGER verbose           ! Input: verbose level

      REAL*8 rvind              ! missing value for real*8 data
      PARAMETER (rvind=1.7E38)

C     Parameters defined in Kvalobs
      REAL*8 HW,PW,TW,
C     Other parameters
     +     year,month,day,hour,minute,
     +     buoy_id,latitude,longitude,sal,
     +     wmo_region_number,wmo_region_subarea
      INTEGER idx,cidx,desc,ind,maxlevel,n,numlevels
      PARAMETER(maxlevel=2000) ! Largest number seen was 499 in a bathy
      CHARACTER*9 call_sign,missing9
      CHARACTER one_bits
      REAL*8 value
      REAL*8 d(maxlevel),s(maxlevel),T(maxlevel),v(maxlevel),z(maxlevel)
      REAL*8 w(maxlevel)

C     Variables used for geographical filtering av observations
C     (--rectangle option set)
      REAL*8 x1,y1,x2,y2
      COMMON /COM_RECTANGLE/  x1,y1,x2,y2

C     Functions
      CHARACTER*9 ctrim ! length must be >= longest variable ctrim is used for

      one_bits = CHAR(255)
      WRITE(missing9,'(9A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits,one_bits,one_bits,one_bits,one_bits

C     Initialize all parameters to missing values
      call_sign = missing9
      HW = rvind
      PW = rvind
      TW = rvind
      year = rvind
      month = rvind
      day = rvind
      hour = rvind
      minute = rvind
      latitude = rvind
      longitude = rvind
      buoy_id = rvind
      sal = rvind
      wmo_region_number = rvind
      wmo_region_subarea = rvind
      DO n=1,maxlevel
         z(n) = rvind
         w(n) = rvind
         T(n) = rvind
         s(n) = rvind
         d(n) = rvind
         v(n) = rvind
      END DO

C     Loop through all expanded descriptors
      n = 0 ! Numbering the subsurface levels
      DO idx=1,ktdexl
         desc = ktdexp(idx)
         value = values(idx + (ksub-1)*kxelem)

         IF (ABS(value - rvind)/rvind.LE.0.001) THEN
C     Missing value, nothing to do
            CYCLE
         END IF

C     Continue the loop for non missing values
         IF (desc.EQ.4001) THEN ! Year
            IF (year.EQ.rvind) THEN
               year = value
           END IF
         ELSE IF (desc.EQ.4002) THEN ! Month
            IF (month.EQ.rvind) THEN
               month = value
            END IF
         ELSE IF (desc.EQ.4003) THEN ! Day
            IF (day.EQ.rvind) THEN
               day = value
            END IF
         ELSE IF (desc.EQ.4004) THEN ! Hour
            IF (hour.EQ.rvind) THEN
               hour = value
            END IF
         ELSE IF (desc.EQ.4005) THEN ! Minute
            IF (minute.EQ.rvind) THEN
               minute = value
            END IF
         ELSE IF (desc.EQ.5001.OR.  ! Latitude (high accuracy)
     +           desc.EQ.5002) THEN ! Latitude (coarse accuracy)
            IF (latitude.EQ.rvind) THEN
               latitude = value
            END IF
         ELSE IF (desc.EQ.6001.OR.  ! Longitude (high accuracy)
     +           desc.EQ.6002) THEN ! Longitude (coarse accuracy)
            IF (longitude.EQ.rvind) THEN
               longitude = value
            END IF
         ELSE IF (desc.EQ.31001) THEN
C     Delayed descriptor replication factor; this should be number of
C     subsurface levels
            IF (value.EQ.rvind) THEN
               WRITE(*,*) 'WARNING: delayed descriptor replication'
     +              // ' factor 31001 undefined!!!'
               RETURN
            END IF
         ELSE IF (desc.EQ.7062) THEN ! Depth below sea/water surface
            n = n + 1           ! new level
            IF (n.GT.maxlevel) THEN
               n = maxlevel
               WRITE(*,*) 'Too many levels! Skipping rest of message'
               GOTO 120
            END IF
            z(n) = value
         ELSE IF (desc.EQ.7065) THEN ! Water pressure
            n = n + 1           ! new level
            IF (n.GT.maxlevel) THEN
               n = maxlevel
               WRITE(*,*) 'Too many levels! Skipping rest of message'
               GOTO 120
            END IF
            w(n) = value
         ELSE IF (desc.EQ.22045.OR.  ! Sea/water temperature (19 bits)
     +           desc.EQ.22043.OR.   ! Sea/water temperature (15 bits)
     +           desc.EQ.22042) THEN ! Sea/water temperature (12 bits)
            IF (n.GT.0 .AND. n.LE.maxlevel) THEN
               t(n) = value
            ELSE IF (n.EQ.0 .AND. TW.EQ.rvind) THEN
               TW = value
            END IF
         ELSE IF (desc.EQ.22064.OR.  ! Salinity [part per thousand] (17 bits)
     +           desc.EQ.22062) THEN ! Salinity [part per thousand] (14 bits)
            IF (n.GT.0 .AND. n.LE.maxlevel) THEN
               s(n) = value
            ELSE IF (n.EQ.0 .AND. sal.EQ.rvind) THEN
               sal = value
            END IF
         ELSE IF (desc.EQ.22004) THEN ! Direction of current
               IF (n.GT.0 .AND. n.LE.maxlevel) THEN
                  d(n) = value
               ELSE IF (n.EQ.0 .AND. verbose.GT.1) THEN
                  WRITE(*,*) 'Find 22004 (dc) before first 7062 (zz)!'
               END IF
         ELSE IF (desc.EQ.22031) THEN ! Speed of current
               IF (n.GT.0 .AND. n.LE.maxlevel) THEN
                  v(n) = value
               ELSE IF (n.EQ.0 .AND. verbose.GT.1) THEN
                  WRITE(*,*) 'Find 22031 (vc) before first 7062 (zz)!'
               END IF
         ELSE IF (desc.EQ.22062) THEN ! Salinity [part per thousand]
            IF (n.GT.0 .AND. n.LE.maxlevel) THEN
               s(n) = value
            ELSE IF (n.EQ.0 .AND. sal.EQ.rvind) THEN
               sal = value
            END IF
         ELSE IF (desc.EQ.22011) THEN !  Period of waves (BUFR doesn't distinguish between PW and PWA)
            IF (PW.EQ.rvind) THEN
               PW = value
            END IF
         ELSE IF (desc.EQ.22021) THEN ! Heigth of waves (BUFR doesn't distinguish between HW and HWA)
            IF (HW.EQ.rvind) THEN
               HW = value
            END IF
         ELSE IF (desc.EQ.1005) THEN ! Buoy/platform identifier
            IF (buoy_id.EQ.rvind) THEN
               buoy_id = value
            END IF
         ELSE IF (desc.EQ.1003) THEN ! WMO region number/geographical area
            IF (wmo_region_number.EQ.rvind) THEN
               wmo_region_number = value
            END IF
         ELSE IF (desc.EQ.1020) THEN ! WMO region sub-area
            IF (wmo_region_subarea.EQ.rvind) THEN
               wmo_region_subarea = value
            END IF
         ELSE IF (desc.EQ.1011) THEN ! Ship or mobile land station identifier
            IF (value.NE.rvind) THEN
               cidx = int(value/1000)
               call_sign = cvals(cidx) ! CCITTIA5 data
               call_sign = ctrim(call_sign,9,missing9)
            END IF
         ELSE IF (desc.EQ.1087) THEN ! WMO marine observing platform extended identifier
            IF (buoy_id.EQ.rvind) THEN
               buoy_id = value
            END IF
         END IF
      END DO
 120  CONTINUE
      numlevels = n

      IF (rectangle) THEN
         IF (longitude.EQ.rvind .OR. latitude.EQ.rvind
     +        .OR. longitude.LT.x1 .OR. longitude.GT.x2
     +        .OR. latitude.LT.y1 .OR. latitude.GT.y2) RETURN
      END IF

      IF (buoy_id.NE.rvind.AND.wmo_region_number.NE.rvind
     +     .AND.wmo_region_subarea.NE.rvind) THEN
         WRITE(*,*)
         IF (buoy_id.LT.1000) THEN
            WRITE(*,'(A,I5)') 'buoy_id=',NINT(wmo_region_number)*10000
     +           + NINT(wmo_region_subarea)*1000 + NINT(buoy_id)
         ELSE
C     This is an error in encoding. We choose to show buoy_id only (and
C     in all cases I have seen of this error, first 2 digits of buoy_id
C     is wmo_region_number and wmo_region_subarea respectively).
            IF (buoy_id.LT.10000) THEN
               WRITE(*,'(A,I4)') 'buoy_id=',NINT(buoy_id)
            ELSE IF (buoy_id.LT.100000) THEN
               WRITE(*,'(A,I5)') 'buoy_id=',NINT(buoy_id)
            ELSE IF (buoy_id.LT.1000000) THEN
               WRITE(*,'(A,I6)') 'buoy_id=',NINT(buoy_id)
            ELSE
               WRITE(*,'(A,I7)') 'buoy_id=',NINT(buoy_id)
            END IF
         END IF
      ELSE IF (buoy_id.NE.rvind.AND.buoy_id.GT.1000) THEN
C     Old drau files (wrongly) includes wmo_region_number and
C     wmo_region_subarea in buoy_id
         WRITE(*,*)
         IF (buoy_id.LT.10000) THEN ! 001005
            WRITE(*,'(A,I5)') 'buoy_id=',NINT(buoy_id)
         ELSE                       ! 001087
            WRITE(*,'(A,I7)') 'buoy_id=',NINT(buoy_id)
         END IF
      ELSE IF (call_sign.NE.missing9) THEN
         ind = index(call_sign,' ') - 1
         IF (ind.EQ.-1) ind = 9
C     Remove trailing NULL characters, which some centres erronously
C     insert
         DO WHILE (IACHAR(call_sign(ind:ind)).EQ.0)
            ind = ind - 1
         END DO
         WRITE(*,*)
         WRITE(*,'(A,A)') 'call_sign=',
     +        call_sign(1:ind)
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*)
            WRITE(*,*) 'Both buoy_id and call_sign are missing!!!'
         END IF
         RETURN
      END IF

      IF (year.NE.rvind.AND.month.NE.rvind.AND.day.NE.rvind
     +     .AND.hour.NE.rvind.AND.minute.NE.rvind) THEN
         WRITE(*,900) NINT(year),NINT(month),NINT(day),
     +        NINT(hour),NINT(minute)
 900     FORMAT('obstime=',I4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':00')
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*) 'obstime is missing!!!'
            RETURN
         END IF
      ENDIF
      IF (longitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lon=',longitude
      END IF
      IF (latitude.NE.rvind) THEN
         WRITE(*,'(A,F10.5)') 'lat=',latitude
      END IF
      IF (PW.NE.rvind) THEN
         WRITE(*,'(A,I11)') 'PW=',NINT(PW)
      END IF
      IF (HW.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'HW=',HW
      END IF
      IF (TW.NE.rvind) THEN
         WRITE(*,'(A,F11.1)') 'TW=',TW-273.15
      END IF
      IF (sal.NE.rvind) THEN
         WRITE(*,'(A,F11.2)') 'ss=',sal
      END IF

      DO n=1,numlevels
C     Do not print level if empty (might happen with last levels in
C     compressed messages, or simply due to bad practice. Setting
C     delayed description replication factor = 1 when ought to be 0 is
C     not quite uncommon)
         IF (z(n).EQ.rvind .AND. t(n).EQ.rvind .AND. s(n).EQ.rvind
     +        .AND. d(n).EQ.rvind .AND. v(n).EQ.rvind
     +        .AND. w(n).EQ.rvind) THEN
            CYCLE
         END IF
         WRITE(*,'(A,I12)') 'n=',n
         IF (z(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'zz=',NINT(z(n))
         END IF
         IF (w(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'wp=',NINT(w(n))
         END IF
         IF (t(n).NE.rvind) THEN
            WRITE(*,'(A,F11.2)') 'tt=',t(n)-273.15
         END IF
         IF (s(n).NE.rvind) THEN
            WRITE(*,'(A,F11.2)') 'ss=',s(n)
         END IF
         IF (d(n).NE.rvind) THEN
            WRITE(*,'(A,I11)') 'dc=',NINT(d(n))
         END IF
         IF (v(n).NE.rvind) THEN
            WRITE(*,'(A,F11.1)') 'vc=',v(n)
         END IF
      END DO

      END SUBROUTINE print_oceanographic_values

C     -----------------------------------------------------------------

      SUBROUTINE print_amdar_values(ksub,kxelem,ktdexl,ktdexp,values,
     +     cvals,rectangle,verbose)
C     Identify pressure, temperature etc and print parameter=value to screen
      IMPLICIT NONE

      INTEGER ksub              ! Input: number of subset currently processed
      INTEGER kxelem            ! Input: expected (max) number of expanded elements
      INTEGER ktdexl            ! Input: number of entries in list of expanded data descriptors
      INTEGER ktdexp(*)         ! Input: array containing expanded data descriptors
      REAL*8 values(*)          ! Input: expanded data values (one subset)
      CHARACTER*80 cvals(*)     ! Input: CCITTIA5 Bufr elements entries (one subset)
      LOGICAL rectangle         ! Input: TRUE if observations are wanted for a rectangle only
      INTEGER verbose           ! Input: verbose level

      REAL*8 rvind              ! missing value for real*8 data
      PARAMETER (rvind=1.7E38)

      CHARACTER*8 aircraft,flight_number,missing8
C     Parameters defined in Kvalobs
      REAL*8 DD,FF,TT,PP,
C     Other parameters
     +     year,month,day,hour,minute,second,latitude,longitude,
     +     flight_level,phase,osn,mixing_ratio
      CHARACTER*3 cphase,missing3
      INTEGER idx,cidx,ind
      CHARACTER one_bits
      REAL*8 value
      INTEGER desc

C     Variables used for geographical filtering av observations
C     (--rectangle option set)
      REAL*8 x1,y1,x2,y2
      COMMON /COM_RECTANGLE/  x1,y1,x2,y2

C     Functions
      INTEGER lenstr
      CHARACTER*8 ctrim ! length must be >= longest variable ctrim is used for
      CHARACTER*3 phase_8004,phase_8009

      one_bits = CHAR(255)
      WRITE(missing3,'(3A)') one_bits,one_bits,one_bits
      WRITE(missing8,'(8A)') one_bits,one_bits,one_bits,one_bits,
     +     one_bits,one_bits,one_bits,one_bits

C     Initialize all parameters to missing values
      aircraft = missing8
      flight_number = missing8
      cphase = missing3
      osn = rvind
      DD = rvind
      FF = rvind
      TT = rvind
      PP = rvind
      year = rvind
      month = rvind
      day = rvind
      hour = rvind
      minute = rvind
      second = rvind
      latitude = rvind
      longitude = rvind
      flight_level = rvind
      phase = rvind
      mixing_ratio = rvind

C     Loop through all expanded descriptors
      DO idx=1,ktdexl
         desc = ktdexp(idx)
         value = values(idx + (ksub-1)*kxelem)

         IF (ABS(value - rvind)/rvind.LE.0.001) THEN
C     Missing value, nothing to do
            CYCLE
         END IF

C     Continue the loop for non missing values
         IF (desc.EQ.1008) THEN ! Aircraft registration number or other identification
            cidx = int(value/1000)
            aircraft = cvals(cidx) ! CCITTIA5 data
            aircraft = ctrim(aircraft,8,missing8)
         ELSE IF (desc.EQ.1006) THEN ! Aircraft flight number
            cidx = int(value/1000)
            flight_number = cvals(cidx) ! CCITTIA5 data
            flight_number = ctrim(flight_number,8,missing8)
         ELSE IF (desc.EQ.1023) THEN ! Observation sequence number
            IF (osn.EQ.rvind) osn = value
         ELSE IF (desc.EQ.4001) THEN ! Year
            IF (year.EQ.rvind) THEN
               year = value
            END IF
         ELSE IF (desc.EQ.4002) THEN ! Month
            IF (month.EQ.rvind) THEN
               month = value
            END IF
         ELSE IF (desc.EQ.4003) THEN ! Day
            IF (day.EQ.rvind) THEN
               day = value
            END IF
         ELSE IF (desc.EQ.4004) THEN ! Hour
            IF (hour.EQ.rvind) THEN
               hour = value
            END IF
         ELSE IF (desc.EQ.4005) THEN ! Minute
            IF (minute.EQ.rvind) THEN
               minute = value
            END IF
         ELSE IF (desc.EQ.4006) THEN ! Second
            IF (second.EQ.rvind) THEN
               second = value
            END IF
         ELSE IF (desc.EQ.5001.OR.  ! Latitude (high accuracy)
     +           desc.EQ.5002) THEN ! Latitude (coarse accuracy)
            IF (latitude.EQ.rvind) THEN
               latitude = value
            END IF
         ELSE IF (desc.EQ.6001.OR.  ! Longitude (high accuracy)
     +           desc.EQ.6002) THEN ! Longitude (coarse accuracy)
            IF (longitude.EQ.rvind) THEN
               longitude = value
            END IF
         ELSE IF (desc.EQ.7010.OR.   ! Flight level
     +           desc.EQ.7002.OR.    ! Height or altitude
     +           desc.EQ.10070) THEN ! Indicated aircraft altitude
            IF (flight_level.EQ.rvind) THEN
               flight_level = value
            END IF
         ELSE IF (desc.EQ.8009.OR.  ! Detailed phase of aircraft flight
     +           desc.EQ.8004) THEN ! Phase of aircraft flight
            IF (phase.EQ.rvind) THEN
               phase = value
               cphase = phase_8009(NINT(phase),missing3)
            END IF
         ELSE IF (desc.EQ.8004) THEN ! Phase of aircraft flight
            IF (phase.EQ.rvind) THEN
               phase = value
               cphase = phase_8004(NINT(phase),missing3)
            END IF
         ELSE IF (desc.EQ.12104.OR. ! Dry bulb temperature at 2m (data width 16 bits)
     +          desc.EQ.12004.OR.   ! Dry bulb temperature at 2m (12 bits)
     +          desc.EQ.12101.OR.   ! Temperature/dry bulb temperature (16 bits)
     +          desc.EQ.12001) THEN ! Temperature/dry bulb temperature (12 bits)
            IF (TT.EQ.rvind) THEN
               TT = value
            END IF
         ELSE IF (desc.EQ.11011.OR.  ! Wind direction at 10 m
     +           desc.EQ.11001) THEN ! Wind direction
            IF (DD.EQ.rvind) THEN
               DD = value
            END IF
         ELSE IF (desc.EQ.11012.OR.  ! Wind speed at 10 m
     +           desc.EQ.11002) THEN ! Wind speed
            IF (FF.EQ.rvind) THEN
               FF = value
            END IF
         ELSE IF (desc.EQ.7004) THEN ! Pressure
            IF (PP.EQ.rvind) THEN
               PP = value
            END IF
         ELSE IF (desc.EQ.13002) THEN ! Mixing ratio
            IF (mixing_ratio.EQ.rvind) THEN
               mixing_ratio = value
            END IF
         END IF
      END DO

      IF (rectangle) THEN
         IF (longitude.EQ.rvind .OR. latitude.EQ.rvind
     +        .OR. longitude.LT.x1 .OR. longitude.GT.x2
     +        .OR. latitude.LT.y1 .OR. latitude.GT.y2) RETURN
      END IF

      IF (aircraft.NE.missing8) THEN
         WRITE(*,*)
         WRITE(*,'(A,A)') 'aircraft=',aircraft(1:lenstr(aircraft,1))
      ELSE IF (flight_number.NE.missing8) THEN
         WRITE(*,*)
         WRITE(*,'(A,A)') 'aircraft=',
     +        flight_number(1:lenstr(flight_number,1))
      ELSE
         IF (verbose .GT. 1) THEN
            WRITE(*,*)
            WRITE(*,*) 'Aircraft (001008/001006) is missing!!!'
         END IF
         RETURN
      END IF

 901  FORMAT('obstime=',I4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2)
 902  FORMAT('obstime=',I4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':00')

      IF (year.NE.rvind.AND.month.NE.rvind.AND.day.NE.rvind
     +     .AND.hour.NE.rvind.AND.minute.NE.rvind) THEN
         IF (second.NE.rvind) THEN
            WRITE(*,901) NINT(year),NINT(month),NINT(day),
     +           NINT(hour),NINT(minute),NINT(second)
         ELSE
            WRITE(*,902) NINT(year),NINT(month),NINT(day),
     +           NINT(hour),NINT(minute)
         END IF
      ELSE IF (verbose .GT. 1) THEN
         WRITE(*,*) 'obstime is missing!!!'
         RETURN
      END IF
      IF (longitude.NE.rvind) THEN
         WRITE(*,'(A,F15.5)') 'lon=',longitude
      END IF
      IF (latitude.NE.rvind) THEN
         WRITE(*,'(A,F15.5)') 'lat=',latitude
      END IF
      IF (osn.NE.rvind) THEN
         WRITE(*,'(A,I12)') 'seqnum=',NINT(osn)
      END IF
      IF (flight_level.NE.rvind) THEN
         WRITE(*,'(A,I6)') 'flight_level=',NINT(flight_level)
      END IF
      IF (phase.NE.rvind) THEN
         WRITE(*,'(A,A3)') 'phase_of_flight=',cphase
      END IF
      IF (DD.NE.rvind) THEN
         WRITE(*,'(A,I16)') 'DD=',NINT(DD)
      END IF
      IF (FF.NE.rvind) THEN
         WRITE(*,'(A,F16.1)') 'FF=',FF
      END IF
      IF (TT.NE.rvind) THEN
         WRITE(*,'(A,F16.1)') 'TT=',TT-273.15
      END IF
      IF (PP.NE.rvind) THEN
         WRITE(*,'(A,F16.1)') 'PP=',PP/100 ! hPa
      END IF
      IF (mixing_ratio.NE.rvind) THEN
         WRITE(*,'(A,F16.2)') 'mr=',mixing_ratio*1000 ! g/kg
      END IF

      END SUBROUTINE print_amdar_values

C     -----------------------------------------------------------------

C     Remove trailing blanks and nulls from string, returning missing if
C     nothing left
      CHARACTER*(*) FUNCTION ctrim(string,dim,missing)
      IMPLICIT NONE
      CHARACTER*(*) string,missing
      INTEGER dim,ind,ascii

      ind = dim
      ascii = IACHAR(string(ind:ind))
      DO WHILE (ind.GT.1 .AND. (ascii.EQ.32 .OR. ascii.EQ.0))
         ind = ind - 1
         ascii = IACHAR(string(ind:ind))
      END DO
      IF (ind.EQ.1) THEN
         ascii = IACHAR(string(1:1))
         IF (ascii.EQ.32 .OR. ascii.EQ.0) THEN
            ctrim = missing
         ELSE
            ctrim = string(1:1)
         END IF
      ELSE IF (ind.GT.1) THEN
         ctrim = string(1:ind)
      ELSE
         ctrim = missing
      END IF

      RETURN
      END FUNCTION ctrim


C     -----------------------------------------------------------------

      CHARACTER*3 FUNCTION phase_8004(phase,missing3)
      IMPLICIT NONE
      INTEGER phase
      CHARACTER*3 missing3,indicator(6)

      DATA indicator/'res','UNS','LVR','LVW','ASC','DES'/
C                      1     2     3     4     5     6

      IF (phase.LE.1 .OR. phase.GT.6) THEN
         phase_8004 = missing3
      ELSE
         phase_8004 = indicator(phase)
      END IF

      RETURN
      END FUNCTION phase_8004

C     -----------------------------------------------------------------

      CHARACTER*3 FUNCTION phase_8009(phase,missing3)
      IMPLICIT NONE
      INTEGER phase
      CHARACTER*3 missing3,indicator(15)

C                      0     1     2     3     4     5     6
      DATA indicator/'LVR','LVW','UNS','LVR','LVW','ASC','DES',
     +     4*'ASC',4*'DES'/
C            7-10    11-14

      IF (phase.LE.0 .OR. phase.GT.14) THEN
         phase_8009 = missing3
      ELSE
         phase_8009 = indicator(phase + 1)
      END IF

      RETURN
      END FUNCTION phase_8009

C     -----------------------------------------------------------------

      INTEGER FUNCTION degree2dir(rd)
      IMPLICIT NONE
      REAL*8 rd
      INTEGER id
      IF (NINT(rd).EQ.0) THEN
         id = 0
      ELSE IF (rd.LT.0 .OR. rd.GT.360) THEN
         id = 9
      ELSE IF (rd.LT.22.5 .OR.rd.GE.337.5) THEN
         id = 8                 ! N
      ELSE IF (rd.LT.67.5) THEN
         id = 1                 ! NE
      ELSE IF (rd.LT.112.5) THEN
         id = 2                 ! E
      ELSE IF (rd.LT.157.5) THEN
         id = 3                 ! SE
      ELSE IF (rd.LT.202.5) THEN
         id = 4                 ! S
      ELSE IF (rd.LT.247.5) THEN
         id = 5                 ! SW
      ELSE IF (rd.LT.292.5) THEN
         id = 6                 ! W
      ELSE IF (rd.LT.337.5) THEN
         id = 7                 ! NW
      END IF

      degree2dir = id
      END FUNCTION degree2dir

C     -----------------------------------------------------------------

      SUBROUTINE vss_8001_to_8042(vss,vss8042)
C     Converts value from flag table 8001 into flag table 8042.  When
C     bit 5 is set in 8001, we cannot know if this is significant level
C     for temperature, relative humididty or both. We choose to mark it
C     as significant level for temperature.
      implicit none
      integer vss,jj
      real*8 vss8042

      vss8042 = 0
      jj = 8
      IF (btest(vss,7-6)) THEN
         vss8042 = vss8042 + 2048 ! significant level, wind
         jj = jj-1
      END IF
      IF (btest(vss,7-5)) THEN ! significant level, temperature and/or
                               ! relative humidity
         vss8042 = vss8042 + 8192 ! significant level, temperature
         jj = jj-1
      END IF
      IF (btest(vss,7-4)) THEN
         vss8042 = vss8042 + 16384 ! maximum wind level
         jj = jj-1
      END IF
      IF (btest(vss,7-3)) THEN
         vss8042 = vss8042 + 32768 ! tropopause level
         jj = jj-1
      END IF
      IF (btest(vss,7-2)) THEN
         vss8042 = vss8042 + 65536 ! standard level
         jj = jj-1
      END IF
      IF (btest(vss,7-1)) THEN
         vss8042 = vss8042 + 131072 ! surface level
         jj = jj-1
      END IF

      END SUBROUTINE vss_8001_to_8042

C     -----------------------------------------------------------------

      integer function lenstr(text,minlen)
c
c  NAME:
c     lenstr
c
c  PURPOSE:
c     Return actual length of text string, excl. trailing blanks.
c
c  SYNOPSIS:
c     integer function lenstr(text,minlen)
c     character*(*) text
c     integer       minlen
c
c  INPUT:
c     text   - text string
c     minlen - the minimum length returned (see NOTES below)
c
c  OUTPUT:
c     lenstr - the string length
c
c  NOTES:
c     Use minlen=1 if you use lenstr to print a text string without
c     trailing blanks. E.g. filenames declared with a large maximum
c     length which hardly ever is longer than one line, but always
c     is printed with trailing blanks on two ore more lines.
c     Example:  write(6,*) filename(1:lenstr(filename,1))
c     (filename(1:0) may abort the program if filename=' ')
c
c-----------------------------------------------------------------------
c  DNMI/FoU  15.03.1995  Anstein Foss
c-----------------------------------------------------------------------
c
      character*(*) text
      integer       k,l,lt,minlen
c
      lt=len(text)
      l=0
      do k=1,lt
        if(text(k:k).ne.' ') l=k
      end do
c
      if(l.lt.minlen) l=minlen
c
      lenstr=l
c
      return
      end
