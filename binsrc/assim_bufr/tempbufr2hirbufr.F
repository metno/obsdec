c  NwpPreprocObs - Free source codes and example scripts for preprocessing
c                  of meteorological observations

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
      PROGRAM tempbufr2hirbufr
      IMPLICIT NONE

      CHARACTER(LEN=80) Usage,argument

      INTEGER jbufl             ! Size of buffer to hold BUFR message
      PARAMETER (jbufl=512000)
      INTEGER ibuff(jbufl)      ! Buffer to hold BUFR message
      INTEGER ibflen            ! Size in BYTES of the array ibuff
      PARAMETER (ibflen=4*jbufl)

      INTEGER verbose              ! Level of verbose output: 0 - 5 (default 0).
                                   ! 0: no output (except possibly error messages.
                                   !    Also, libbufr might display BUFR tables used)
                                   ! 1: warnings
                                   ! 2: message level
                                   ! 3: subset level (simple)
                                   ! 4: subset level (detailed)
                                   ! 5: you probably don't want to use this
      INTEGER kelem             ! expected (max) number of expanded elements
      INTEGER kvals             ! expected (max) number of data values

c     Dimensions of arrays used by libbufr routines are the same as those used in
c     example program decode_bufr.F in bufr_000310/example/

      PARAMETER (kelem = 160000, kvals = 4096000) ! orig values /PD

      INTEGER ksup(9)           ! integer array containing supplementary information
      INTEGER ksec0(3)          ! Bufr section 0 information
      INTEGER ksec1(40)
      INTEGER ksec2(4096)
      INTEGER ksec3(4)
      INTEGER ksec4(2)
      CHARACTER*64 cnames(kelem) ! Bufr Table B element names
      CHARACTER*24 cunits(kelem) ! Bufr Table B units
      CHARACTER*80 cvals(kelem) ! CCITTIA5 Bufr elements entries (one subset)
      REAL values(kvals)      ! expanded data values (one subset)

c     BUSEL variables
      INTEGER ktdlst(kelem)     ! array containing data descriptors in section 3
      INTEGER ktdexp(kelem)     ! array containing expanded data descriptors
      INTEGER ktdlen            ! number of data descriptors in section 3
      INTEGER ktdexl            ! number of entries in list of expanded data descriptors
      integer kdata(kelem)

      integer kxelem
      integer ilen,iret,kerr,bfren_err,ii,jj,iunit1,iunit2
      character*200 bufr_file,bufr_out

      integer obstype,subtype,origcen,nsubsets,edition
      character*80 stat_id

c     Output array
      real, allocatable :: walues(:)

      integer kbuff(10000),kbufl

      integer subtype_ec
      integer nwal
      integer update_seqnr
      integer len_wal,nlev,nlev2,num_data,num_qc
      integer year,month,day,hour,minute

      integer idlev ! input data nr-of-levels
      real diff
      integer iarg,ij,begs_wmo
      logical is_missing

      Usage = '\nUsage: tempbufr2hirbufr <bufr file> <outfile>'
     +     // ' [--verbose n] \n'

      iarg = IARGC()
      IF (iarg.NE.2 .AND. iarg.NE.4) THEN
         WRITE(*,*) Usage
         CALL EXIT(0)
      END IF
      CALL GETARG(1,bufr_file)
      CALL GETARG(2,bufr_out)

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

c     OPEN THE BUFR FILE FOR READING
      CALL PBOPEN(IUNIT1,trim(bufr_file),"R",IRET)
      IF ( IRET .NE. 0 ) THEN
         STOP
      ENDIF

      CALL PBOPEN(IUNIT2,trim(bufr_out),"W",IRET)
  
      DO

 333     CONTINUE

c     GET TO THE NEXT BUFR MESSAGE
         CALL PBBUFR(IUNIT1, IBUFF, IBFLEN, ILEN, IRET)
         IF(IRET.EQ.-1)THEN
            goto 550
         ENDIF
     
          
c     EXPAND ONLY KSEC0,KSEC1 AND KSEC2
         CALL BUS012(ILEN,IBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
         IF(KERR.NE.0) THEN
            WRITE(*,*) 'ERROR CALLING BUS012, KERR=',KERR
            GOTO 333 ! Go to the next BUFR message
         ENDIF 

         edition=ksec1(2)
         obstype=ksec1(6)
         nsubsets=ksup(6)
         origcen=ksec1(3)

         year=ksec1(9)
         month=ksec1(10)
         day=ksec1(11)
         hour=ksec1(12)
         minute=ksec1(13)

         update_seqnr=ksec1(4)
     
         if(edition.eq.3 .or. edition.eq.2)then
            subtype=ksec1(7)
         elseif(edition.eq.4)then
            subtype=ksec1(17) ! might be changed later for temp drop
         endif

c     Print BUFR obstype, subtype, originating center and number of subsets
      IF(verbose.GT.1)THEN
         WRITE(*,'(A23,4(I3,1X))')  'TYPE,SUBTYPE,ORIGCEN,NSUBSETS ',
     &        obstype,subtype,origcen,nsubsets
      ENDIF
         if(obstype.eq.2.and.
     &     (subtype.eq.101.or.subtype.eq.102.or.subtype.eq.103))then
c     This is almost certainly a single subset BUFR message already
c     encoded according to the Hirlam BUFR TEMP template, so we should
c     just reencode the message without doing any changes
            if(nsubsets.ne.1)then
               if(verbose.gt.0)write(*,*) 'nsubsets=',nsubsets,' ????'
            else
               if(verbose.gt.1)write(*,*) 'This is a Hirlam template,'
     &              // ' will be reencoded with no changes'
               CALL reencode_hirlambufr(iunit2,verbose,
     &              ILEN,IBUFF,KELEM,KVALS)
            endif
            goto 333  ! go to the next bufr-message
         endif

         if(obstype.ne.2.or.
     &        (subtype.ne.4.and.subtype.ne.5.and.
     &        subtype.ne.7.and.subtype.ne.14))then
            if(verbose.gt.0)write(*,*) obstype,subtype,
     &           ' not valid obstype,subtype'
            goto 333  ! go to the next bufr-message
         endif

c     Going wrong here only means that station identification might get missing value
         if(subtype.eq.4.or.subtype.eq.14)then
            ! subtype 14 is expected used in 309056 which is radiosonde descent
            subtype_ec=101  ! TEMP LAND (stationid wmonr)
         elseif(subtype.eq.5)then
            subtype_ec=102  ! TEMP SHIP (callsign/mobile land station)
         elseif(subtype.eq.7)then ! expected used in 309053
            subtype_ec=103  ! TEMP DROP (carrier balloon/aircraft identifier)
         endif

         kxelem = kvals/ksup(6)
         IF (kxelem.GT.kelem) kxelem = kelem

c     EXPAND THE WHOLE BUFR     
         CALL BUFREX(ILEN, IBUFF, KSUP, KSEC0, KSEC1, KSEC2,
     &        KSEC3, KSEC4, KXELEM, CNAMES, CUNITS, KVALS, VALUES,
     &        CVALS, KERR)
         IF(KERR.NE.0)THEN
            WRITE(*,*) 'ERROR CALLING BUFREX, KERR=',KERR
            GOTO 333
         ENDIF


         DO JJ=1,KSUP(6)
         
            CALL BUSEL2(JJ,kxelem,ktdlen,ktdlst,ktdexl,ktdexp,
     &           cnames,cunits,kerr)
            IF(KERR.NE.0)THEN
               WRITE(*,*) 'ERROR CALLING BUSEL2, KERR=',KERR
               GOTO 333
            ENDIF

C     Preprocessing not yet able to handle radiosonde descents (309056)
            IF(KTDLST(1).NE.309052.AND.KTDLST(1).NE.309053 .AND.
     &           KTDLST(2).NE.309052.AND.KTDLST(2).NE.309053 .AND.
     &           KTDLST(1).NE.309057)THEN
               WRITE(*,*) 'BUSEL2 GAVE KTDLST(1)=',KTDLST(1),
     &              ' KTDLST(2)=',KTDLST(2)
               WRITE(*,*) '309052/3/7 ARE THE ONLY ACCEPTED TEMPLATES'
               GOTO 333
            ENDIF

c     Be sure to use right template for temp drop, since subtype 14 is also seen used in 309053!
            if(ktdlst(1).eq.309053 .or. ktdlst(2).eq.309053)then
               subtype=7
               subtype_ec=103
            endif


            nwal=ktdexl*3
            allocate(walues(nwal))
            walues(:)=0.0

C     Hack: must find a better way...
            IF(KTDLST(1).NE.301128.AND.is_missing(CVALS(1))) THEN
               stat_id=''
            ELSE IF(KTDLST(1).EQ.301128.AND.is_missing(CVALS(4))) THEN
               stat_id=''
            ELSE IF(KTDLST(1).EQ.301128) THEN
               stat_id=CVALS(4)
               if(verbose.gt.2) write(*,*) 'Station: ',stat_id
            ELSE
               stat_id=CVALS(1)
               if(verbose.gt.2) write(*,*) 'Station: ',stat_id
            ENDIF

            CALL WMO2EC(VALUES,KXELEM,JJ,KTDEXP,KTDEXL,
     &           stat_id,subtype,
     &           walues,nwal,len_wal,
     &           nlev,nlev2,num_data,num_qc,verbose)
            if(nlev.eq.0)then
               if(verbose.gt.0)write(*,*) 'Skipping this observation'
               deallocate(walues)
               goto 333
            endif


            kbuff(:)=0
            CALL ENCODE_BUFR(walues,nwal,len_wal,
     &           nlev,nlev2,num_data,num_qc,
     &           stat_id,subtype_ec,update_seqnr,
     &           year,month,day,hour,minute,
     &           kbuff,kbufl,bfren_err,verbose)
            

            if(bfren_err.eq.0)then
               CALL PBWRITE(IUNIT2,kbuff,kbufl*4,kerr)
            else
               if(verbose.gt.0) write(*,*) 'Skipping this observation'
            endif

            deallocate(walues)

         ENDDO

     
      ENDDO
 550  continue

c     Close files
      CALL PBCLOSE(IUNIT1,IRET)
      if(verbose.gt.1) WRITE(*,*) 'After PBCLOSE, IRET = ', IRET
      IF ( IRET .NE. 0 ) STOP

      CALL PBCLOSE(IUNIT2,IRET)


      END
c==========================================
c sub: WMO2EC
c==========================================
      subroutine wmo2ec(values,kxelem,isub,ktdexp,ktdexl,
     &     stat_id,subtype,
     &     walues,nwal,len_wal,
     &     nlev,nlev2,num_data,num_qc,verbose)
      implicit none
      
      integer kxelem,ktdexl,isub
      real values(kxelem)
      integer ktdexp(ktdexl)
      integer subtype
      character*80 stat_id

      logical levlist(ktdexl) ! list of levels to be chosen from the wmo-bufr
                              ! if nlev>254

      integer ii,jj,verbose

      REAL rvind              ! missing value for real data
      PARAMETER (rvind=1.7E38)

c     base-values
      
      real wmo_block,wmo_stid
      real afn                ! AIRCRAFT FLIGHT NUMBER
      real somlsi             ! SHIP OR MOBILE LAND STATION IDENTIFIER
      real domomop            ! DIRECTION OF MOTION OF MOVING OBSERVING PLATFORM
      real somomop            ! SPEED OF MOTION OF MOVING OBSERVING PLATFORM
      real RT                 ! RADIOSONDE TYPE
      real RCM                ! RADIOSONDE COMPUTATIONAL METHOD
      real year,month,day,hour,minute
      real lat,lon
      real height             ! HEIGHT OF STATION
      real CC                 ! CLOUD COVER (TOTAL)
      real VS                 ! VERTICAL SIGNIFICANCE (SURFACE OBSERVATIONS)
      real CA                 ! CLOUD AMOUNT
      real HOBC               ! HEIGHT OF BASE OF CLOUD
      real CT1,CT2,CT3        ! CLOUD TYPE(S)

      real pressure,gph,temperature,tdew,windir,windsp
      real wsh1,wsh2
      integer evss,vss

      real value
      integer desc

      integer nwal
      real walues(nwal)

c     'parameters per-level'
      integer pplec,pplwmo
      integer ppl2ec,ppl2wmo
      parameter(pplec=7,pplwmo=10)
      parameter(ppl2ec=4,ppl2wmo=7)

      real grav
      parameter(grav=9.81)

      integer begs_ec,begs_wmo
      integer begws_ec,begws_wmo
      integer nlev,nlev2
      integer outlev

      integer beg_dpi,dpi_ind,num_data,num_qc,valind
      integer len_wal
      integer ii_stationid
      real diff

c     initialize base-values to missing values
      wmo_block=rvind
      wmo_stid=rvind
      afn=rvind
      somlsi=rvind
      domomop=rvind
      somomop=rvind
      RT=rvind
      RCM=rvind
      year=rvind
      month=rvind
      day=rvind
      hour=rvind
      minute=rvind
      lat=rvind
      lon=rvind
      height=rvind
      CC=rvind
      VS=rvind
      CA=rvind
      HOBC=rvind
      CT1=rvind
      CT2=rvind
      CT3=rvind
      nlev=0
      nlev2=0

      ii_stationid=0

      do ii=1,ktdexl

         desc = ktdexp(ii)
         value = values(ii+(isub-1)*kxelem)

         diff=abs(value-rvind)/rvind
         if(diff.lt.0.001) value=rvind

         if (desc.eq.1001.or.desc.eq.1006) then 
            ii_stationid = ii
         end if

         if (desc.eq.31001) then
            if (value.ne.rvind) then
               nlev2=int(value)
               begws_wmo=ii
            end if
         endif
      enddo
      
      if(ktdexl.lt.51)then
         if(verbose.gt.0)write(*,*) 'Too short report, ktdexl=',ktdexl
         return
      endif
         
      do ii=ii_stationid,ii_stationid + 51

         desc = ktdexp(ii)
         value = values(ii+(isub-1)*kxelem)

         diff=abs(value-rvind)/rvind
         if(diff.lt.0.001) value=rvind

         if (desc.eq.1001) then
            if (value.ne.rvind) then
               wmo_block=value
            end if
         else if(desc.eq.1002)then
            if (value.ne.rvind) then
               wmo_stid=value
            end if
         else if(desc.eq.1006)then
            if (value.ne.rvind) then
               afn=value
            end if
         else if(desc.eq.1011)then
            if (value.ne.rvind) then
               somlsi=value
            end if
         else if(desc.eq.2011)then
            if (value.ne.rvind) then
               RT=value
            end if
         else if(desc.eq.4001)then
            if (value.ne.rvind) then
               year=value
            end if
         else if(desc.eq.4002)then
            if (value.ne.rvind) then
               month=value
            end if
         else if(desc.eq.4003)then
            if (value.ne.rvind) then
               day=value
            end if
         else if(desc.eq.4004)then
            if (value.ne.rvind) then
               hour=value
            end if
         else if(desc.eq.4005)then
            if (value.ne.rvind) then
               minute=value
            end if
         else if(desc.eq.5001)then
            if (value.ne.rvind) then
               lat=value
            end if
         else if(desc.eq.6001)then
            if (value.ne.rvind) then
               lon=value
            end if
         else if(desc.eq.7030)then ! HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL
            if (value.ne.rvind.and.height.eq.rvind) then
               height=value
            end if
         else if(desc.eq.7031)then ! HEIGHT OF BAROMETER ABOVE MEAN SEA LEVEL
            if (value.ne.rvind.and.height.eq.rvind) then
               height=value
            end if
         else if(desc.eq.7007)then ! HEIGHT
            if (value.ne.rvind.and.height.eq.rvind) then
               height=value
            end if
         else if(desc.eq.8002)then
            if (value.ne.rvind) then
               VS=value
            end if
         else if(desc.eq.20011)then
            if (value.ne.rvind) then
               CA=value
            end if
         else if(desc.eq.20013)then
            if (value.ne.rvind) then
               HOBC=value
            end if
         else if(desc.eq.20012.and.ii.eq.24)then
            if (value.ne.rvind) then
               CT1=value
            end if
         else if(desc.eq.20012.and.ii.eq.25)then
            if (value.ne.rvind) then
               CT2=value
            end if
         else if(desc.eq.20012.and.ii.eq.26)then
            if (value.ne.rvind) then
               CT3=value
            end if
         else if(desc.eq.31002)then
            if (value.ne.rvind) then
               nlev=int(value)
               begs_wmo=ii
            end if
         endif

      enddo

      ! HIRLAM TEMP templates can not handle more levels than 254
      if(nlev.gt.254)then
         if(verbose.gt.2) then
            write(*,*) 'Input TEMP contains' ,nlev,' levels' 
            write(*,*) 'and output templates can not handle nlev>254.'
            write(*,*) 'A subset of levels from the input bufr'//
     &           ' will be selected'
         endif
         call picklevel(values,kxelem,isub,ktdexp,ktdexl,
     &        nlev,begs_wmo,levlist,verbose)


      else
         levlist(1:nlev)=.true.
      endif

      if(nlev.eq.0)then
         if(verbose.gt.0)write(*,*) 'Failed to find number-of-levels'
         return
      endif

c     fill base with values

      if(subtype.eq.4.or.subtype.eq.14)then
         if(verbose.gt.2) write(*,'(a9,i2.2,i3.3)')
     &        'Station: ',int(wmo_block),int(wmo_stid)
         walues(1)=wmo_block
         walues(2)=wmo_stid
         walues(3)=RT
         walues(4)=RCM
         walues(5)=year
         walues(6)=month
         walues(7)=day
         walues(8)=hour
         walues(9)=minute
         walues(10)=lat
         walues(11)=lon
         walues(12)=height
         walues(13)=CC
         walues(14)=VS
         walues(15)=CA
         walues(16)=HOBC
         walues(17)=CT1
         walues(18)=CT2
         walues(19)=CT3
         walues(20)=real(nlev)
         begs_ec=20
      else if(subtype.eq.5)then
         walues(1)=somlsi
         walues(2)=domomop
         walues(3)=somomop
         walues(4)=RT
         walues(5)=RCM
         walues(6)=year
         walues(7)=month
         walues(8)=day
         walues(9)=hour
         walues(10)=minute
         walues(11)=lat
         walues(12)=lon
         walues(13)=height
         walues(14)=CC
         walues(15)=VS
         walues(16)=CA
         walues(17)=HOBC
         walues(18)=CT1
         walues(19)=CT2
         walues(20)=CT3
         walues(21)=real(nlev)
         begs_ec=21
      else if(subtype.eq.7)then
         walues(1)=afn
         walues(2)= rvind  ! domomop not in 309053
         walues(3)= rvind  ! somomop not in 309053
         walues(4)=RT
         walues(5)=RCM
         walues(6)=year
         walues(7)=month
         walues(8)=day
         walues(9)=hour
         walues(10)=minute
         walues(11)=lat
         walues(12)=lon
         walues(13)=height
         walues(14)=CC
         walues(15)=VS
         walues(16)=CA
         walues(17)=HOBC
         walues(18)=CT1
         walues(19)=CT2
         walues(20)=CT3
         walues(21)=real(nlev)
         begs_ec=21
      endif

c     ====================================
c     extract sounding information
c     ====================================


      outlev=0
      do ii=1,nlev

         if(.not.levlist(ii)) cycle
         outlev=outlev+1

         pressure=rvind
         gph=rvind
         evss=0
         temperature=rvind
         tdew=rvind
         windir=rvind
         windsp=rvind
         
         do jj=1,pplwmo
            
            desc=ktdexp(begs_wmo+(ii-1)*pplwmo+jj)
            value=values((isub-1)*kxelem+begs_wmo+(ii-1)*pplwmo+jj)

            diff=abs(value-rvind)/rvind
            if(diff.lt.0.001) value=rvind

            if (desc.eq.7004) then ! PRESSURE
               if (value.ne.rvind) then
                  pressure=value
               end if
            else if (desc.eq.10009) then ! GPH
               if (value.ne.rvind) then
                  gph=value
               end if
            else if (desc.eq.8042) then ! EXTENDED VERTICAL SOUNDING SIGNIFICANCE
               if (value.ne.rvind) then
                  evss=int(value)
               end if
            else if (desc.eq.12101) then ! TEMPERATURE
               if (value.ne.rvind) then
                  temperature=value
               end if
            else if (desc.eq.12103) then ! DEW-POINT TEMPERATURE
               if (value.ne.rvind) then
                  tdew=value
               end if
            else if (desc.eq.11001) then ! WIND DIRECTION
               if (value.ne.rvind) then
                  windir=value
               end if
            else if (desc.eq.11002) then ! WIND SPEED
               if (value.ne.rvind) then
                  windsp=value
               end if
            endif

         enddo

c     convert from EVSS to VSS
c     EVSS is flag table 8042
c     VSS is flag table  8001
         vss=0
         call evss2vss(evss,vss)
c     ------------------------
         
         walues(begs_ec+(outlev-1)*pplec+1)=pressure
         walues(begs_ec+(outlev-1)*pplec+2)=real(vss)
         
         if(gph.ne.rvind.and.gph.gt.-400)then
            walues(begs_ec+(outlev-1)*pplec+3)=real(gph*grav)
         else
            walues(begs_ec+(outlev-1)*pplec+3)=rvind
         endif

         walues(begs_ec+(outlev-1)*pplec+4)=temperature
         walues(begs_ec+(outlev-1)*pplec+5)=tdew
         walues(begs_ec+(outlev-1)*pplec+6)=windir
         walues(begs_ec+(outlev-1)*pplec+7)=windsp
         

      enddo

      if(nlev.gt.254)then
         if(verbose.gt.2) write(*,*) 'Chosen nr of levels= ',outlev
         nlev=outlev

         if(subtype.eq.4.or.subtype.eq.14)then
            walues(20)=real(nlev)
         elseif(subtype.eq.5.or.subtype.eq.7)then
            walues(21)=real(nlev)
         endif

      endif

c     ===========================================
c     extract wind-shear information if available
c     ===========================================
      
      if(nlev2.gt.0)then
         
         begws_ec=begs_ec+(nlev-1)*pplec+pplec+1
         walues(begws_ec)=real(nlev2)
         
         
         do ii=1,nlev2
            
            pressure=rvind
            evss=0
            wsh1=rvind
            wsh2=rvind
            
            do jj=1,ppl2wmo
               
               desc=ktdexp(begws_wmo+(ii-1)*ppl2wmo+jj)
               value=values((isub-1)*kxelem+begws_wmo+(ii-1)*ppl2wmo+jj)

               diff=abs(value-rvind)/rvind
               if(diff.lt.0.001) value=rvind

               if (desc.eq.7004) then ! PRESSURE
                  if (value.ne.rvind) then
                     pressure=value
                  end if
               else if (desc.eq.8042) then ! EXTENDED VERTICAL SOUNDING SIGNIFICANCE
                  if (value.ne.rvind) then
                     evss=int(value)
                  end if
               else if (desc.eq.11061) then ! ABSOLUTE WIND SHEAR IN 1 KM LAYER BELOW
                  if (value.ne.rvind) then
                     wsh1=value
                  end if
               else if (desc.eq.11062) then ! ABSOLUTE WIND SHEAR IN 1 KM LAYER ABOVE
                  if (value.ne.rvind) then
                     wsh2=value
                  end if
               endif
               
            enddo

            vss=0
            call evss2vss(evss,vss)

            walues(begws_ec+(ii-1)*ppl2ec+1)=pressure
            walues(begws_ec+(ii-1)*ppl2ec+2)=real(vss)
            walues(begws_ec+(ii-1)*ppl2ec+3)=wsh1
            walues(begws_ec+(ii-1)*ppl2ec+4)=wsh2

         enddo

      endif


c     Fill 'DATA PRESENT INDICATOR'

      if(nlev2.eq.0)then
         num_data=begs_ec+(nlev-1)*pplec+pplec
      else
         num_data=begws_ec+(nlev2-1)*ppl2ec+ppl2ec
      endif

      beg_dpi=num_data+2
      walues(beg_dpi)=real(num_data)

      num_qc=0
      do ii=1,num_data

         valind=ii
         dpi_ind=beg_dpi+ii

         if(walues(valind).eq.rvind)then
            walues(dpi_ind)=1.0
         else
            num_qc=num_qc+1
         endif


      enddo

c     Fill 'CONFIDENCE' with dummy 70%

      walues(beg_dpi+num_data+1)=82.0 ! IDENTIFICATION OF ORIGINATING/GENERATING CENTRE
      walues(beg_dpi+num_data+2)=1.0  ! GENERATING APPLICATION
      walues(beg_dpi+num_data+3)=real(num_qc) ! EXTENDED DELAYED REPLICATION FACTOR

      do ii=1,num_qc
         walues(beg_dpi+num_data+3+ii)=70.0
      enddo
      len_wal=beg_dpi+num_data+3+num_qc

      if(verbose.gt.4) then
         do ii=1,len_wal
            write(*,*) ii,walues(ii)
         enddo
      endif


      return
      end
c==========================================
c sub: ENCODE_BUFR
c==========================================

      subroutine encode_bufr(walues,nwal,len_wal,
     &     nlev,nlev2,num_data,num_qc,
     &     stat_id,subtype_ec,update_seqnr,
     &     year,month,day,hour,minute,
     &     kbuff,kbufl,kerr,verbose)
      implicit none
      
      integer len_wal,nwal
      real  walues(nwal)
      integer nlev,nlev2,num_data,num_qc
      integer subtype_ec,update_seqnr
      integer year,month,day,hour,minute
      character*80 stat_id
      integer kbuff(10000),kbufl,verbose


      character*80 cwals(nwal)
      INTEGER nsec0(3),nsec1(40),nsec2(4096),nsec3(4),nsec4(2),
     +     ntdlen,ntdlst(16),nelem,nvals
      INTEGER ndlen             ! integer (dimension of kdata array)
      INTEGER ndata(4)          ! integer array containing data needed for data
                                ! descriptor expansion (delayed replication factors)
                                ! which appear in the values array
      INTEGER kerr
      integer ii

      nsec0(1) = 0              ! Length of section 0, will be set by BUFREN
      nsec0(2) = 0              ! Total length of BUFR message, will be set by BUFREN
      nsec0(3) = 3              ! BUFR edition number

      nsec1(1) = 18             ! Length of section 1 (bytes). Must be set by user
      nsec1(2) = 3              ! BUFR edition number
      nsec1(3) = 98             ! Originating centre (98=ECMWF)
      nsec1(4) = update_seqnr
      nsec1(5) = 0              ! No section 2 (optional section)
      nsec1(6) = 2              ! Single level upper air data (other than satellite)
      nsec1(7) = subtype_ec        ! 101 or 102 or 103
      nsec1(8) = 1              ! Version number of local table used
                                ! (and here we really need local tables!)
      nsec1(9) = year           ! Should be year of century in edition 3 (100 for new millennia)
      IF (year.GT.100) nsec1(9) = mod(year,100)
      IF (nsec1(9).EQ.0) nsec1(9) = 100
      nsec1(10) = month
      nsec1(11) = day
      nsec1(12) = hour
      nsec1(13) = minute
      nsec1(14) = 0             ! BUFR Master table used
      nsec1(15) = 13            ! Version number of Master table used
      nsec1(16) = 0             ! Originating sub-centre
      nsec1(17) = 0             ! International subcategory (not used for BUFR edition 3)
      nsec1(18) = 0             ! Second (not used for BUFR edition 3)
      nsec1(19) = 0             ! Local ADP centre information

      nsec2(1) = 0              ! Length of section 2 (bytes). Must be set by user
      
      nsec3(1) = 0              ! Length of section 3, will be set by BUFREN
      nsec3(2) = 0              ! Reserved
      nsec3(3) = 1              ! Number of subsets
      nsec3(4) = 128            ! Observed data, no compression

      nsec4(1) = 0              ! Length of section 4, will be set by BUFREN

      do ii=1,nwal
         cwals(ii)=' '          ! should really be missing value (1 bits) 
      enddo
      
c     Note that the only real difference between 309007, 309194 and
c     309196 is station identification
      if(subtype_ec.eq.101.and.nlev2.eq.0)then
         ntdlen=10
         ntdlst(1)=309007
         ntdlst(2)=222000
         ntdlst(3)=101000
         ntdlst(4)=31002
         ntdlst(5)=31031
         ntdlst(6)=1031
         ntdlst(7)=1032
         ntdlst(8)=101000
         ntdlst(9)=31002
         ntdlst(10)=33007

         ndlen=3
         ndata(1)=nlev
         ndata(2)=num_data
         ndata(3)=num_qc

      elseif(subtype_ec.eq.101.and.nlev2.gt.0)then
         ntdlen=16
         ntdlst(1)=309007
         ntdlst(2)=104000
         ntdlst(3)=31001
         ntdlst(4)=7004
         ntdlst(5)=8001
         ntdlst(6)=11061
         ntdlst(7)=11062
         ntdlst(8)=222000
         ntdlst(9)=101000
         ntdlst(10)=31002
         ntdlst(11)=31031
         ntdlst(12)=1031
         ntdlst(13)=1032
         ntdlst(14)=101000
         ntdlst(15)=31002
         ntdlst(16)=33007

         ndlen=4
         ndata(1)=nlev
         ndata(2)=nlev2
         ndata(3)=num_data
         ndata(4)=num_qc

      elseif(subtype_ec.eq.102.and.nlev2.eq.0)then
         ntdlen=10
         ntdlst(1)=309196
         ntdlst(2)=222000
         ntdlst(3)=101000
         ntdlst(4)=31002
         ntdlst(5)=31031
         ntdlst(6)=1031
         ntdlst(7)=1032
         ntdlst(8)=101000
         ntdlst(9)=31002
         ntdlst(10)=33007

         ndlen=3
         ndata(1)=nlev
         ndata(2)=num_data
         ndata(3)=num_qc

         cwals(1)=stat_id

      elseif(subtype_ec.eq.102.and.nlev2.gt.0)then
         ntdlen=16
         ntdlst(1)=309196
         ntdlst(2)=104000
         ntdlst(3)=31001
         ntdlst(4)=7004
         ntdlst(5)=8001
         ntdlst(6)=11061
         ntdlst(7)=11062
         ntdlst(8)=222000
         ntdlst(9)=101000
         ntdlst(10)=31002
         ntdlst(11)=31031
         ntdlst(12)=1031
         ntdlst(13)=1032
         ntdlst(14)=101000
         ntdlst(15)=31002
         ntdlst(16)=33007

         ndlen=4
         ndata(1)=nlev
         ndata(2)=nlev2
         ndata(3)=num_data
         ndata(4)=num_qc

         cwals(1)=stat_id

      elseif(subtype_ec.eq.103.and.nlev2.eq.0)then
         ntdlen=10
         ntdlst(1)=309194
         ntdlst(2)=222000
         ntdlst(3)=101000
         ntdlst(4)=31002
         ntdlst(5)=31031
         ntdlst(6)=1031
         ntdlst(7)=1032
         ntdlst(8)=101000
         ntdlst(9)=31002
         ntdlst(10)=33007

         ndlen=3
         ndata(1)=nlev
         ndata(2)=num_data
         ndata(3)=num_qc

         cwals(1)=stat_id  ! going from 001006 (8 bytes) in 309053 to
                           ! 001194 in 309194 (9 bytes)

      elseif(subtype_ec.eq.103.and.nlev2.gt.0)then
         ntdlen=16
         ntdlst(1)=309194
         ntdlst(2)=104000
         ntdlst(3)=31001
         ntdlst(4)=7004
         ntdlst(5)=8001
         ntdlst(6)=11061
         ntdlst(7)=11062
         ntdlst(8)=222000
         ntdlst(9)=101000
         ntdlst(10)=31002
         ntdlst(11)=31031
         ntdlst(12)=1031
         ntdlst(13)=1032
         ntdlst(14)=101000
         ntdlst(15)=31002
         ntdlst(16)=33007

         ndlen=4
         ndata(1)=nlev
         ndata(2)=nlev2
         ndata(3)=num_data
         ndata(4)=num_qc

         cwals(1)=stat_id

      endif
      
      CALL BUFREN(nsec0,nsec1,nsec2,nsec3,nsec4,ntdlen,ntdlst,
     &     ndlen,ndata,nwal,len_wal,walues,cwals,kbufl,kbuff,kerr)
      if(kerr.ne.0)then
         write(*,*) 'ERROR IN BUFREN, KERR=',KERR
         return
      else
         if(verbose.gt.1)write(*,*) 'Successfully encoded BUFR-message!'
      endif

      return
      end
c==========================================
c sub: EVSS2VSS
c==========================================
      subroutine evss2vss(evss,vss)
      implicit none

      integer evss,vss
      integer ii

      vss=0

      if(btest(evss,18-1)) vss=ibset(vss,7-1)
      if(btest(evss,18-2)) vss=ibset(vss,7-2)
      if(btest(evss,18-3)) vss=ibset(vss,7-3)
      if(btest(evss,18-4)) vss=ibset(vss,7-4)

      if(btest(evss,18-5)) vss=ibset(vss,7-5)
      if(btest(evss,18-6)) vss=ibset(vss,7-5)

      if(btest(evss,18-7)) vss=ibset(vss,7-6)


      return
      end
c==========================================
c sub: PICKLEVEL
c==========================================
      subroutine picklevel(values,kxelem,isub,ktdexp,ktdexl,
     &     idlev,begs_wmo,levlist,verbose)
      implicit none

      real rvind              ! missing value for real data
      parameter (rvind=1.7E38)

      integer kxelem,ktdexl,isub
      real values(kxelem)
      integer ktdexp(ktdexl)
      integer idlev
      integer begs_wmo
      integer pplwmo
      parameter(pplwmo=10)
      
      integer ii,jj,kk
      integer desc
      real diff,value
      integer evss
      real pressure

      integer newlev,addlev,lincr

      logical levlist(ktdexl)
      integer verbose


      do ii=1,ktdexl
         levlist(ii)=.false.
      enddo

      newlev=0
      do ii=1,idlev

         pressure=rvind
         
         do jj=1,pplwmo
            
            desc=ktdexp(begs_wmo+(ii-1)*pplwmo+jj)
            value=values((isub-1)*kxelem+begs_wmo+(ii-1)*pplwmo+jj)

            diff=abs(value-rvind)/rvind
            if(diff.lt.0.001) value=rvind

            if (desc.eq.7004) then ! PRESSURE
               if (value.ne.rvind) then
                  pressure=value
               end if
            else if (desc.eq.8042) then ! EXTENDED VERTICAL SOUNDING SIGNIFICANCE

               if (value.ne.rvind) then
                  evss=int(value)
                  if(evss.ne.0)then
                     newlev=newlev+1
                     levlist(ii)=.true.

                     if(newlev.eq.254) goto 876

                  endif
               end if

            endif

         enddo

      enddo
 876  continue

      if(verbose.gt.2)
     &     write(*,*) 'EVSS selection gave: ',idlev,'->',newlev

      if(newlev.eq.254) return  ! we're done

      
!     how many more levels can we select:
      addlev=254-newlev
      lincr=int((idlev-newlev)/addlev)


      if(verbose.gt.2) then
         write(*,*) addlev,' levels can be added'
         write(*,*) 'lincr=',lincr
      endif

      jj=0          ! counter for 'available' levels
      kk=0          ! counter for added, selected, levels
      do ii=1,idlev
            
         if(levlist(ii))then
            if(verbose.gt.3) write(*,*) ii,' 1'
         else
            jj=jj+1
            if(mod(jj,lincr).eq.0)then
               kk=kk+1
               levlist(ii)=.true.
               newlev=newlev+1
               if(verbose.gt.3) write(*,*) ii,' 0 -> 1',newlev,kk
               
               if(kk.eq.addlev) goto 554

            else
               if(verbose.gt.3) write(*,*) ii,' 0'
            endif

         endif

      enddo
554   continue

      return
      end


      SUBROUTINE reencode_hirlambufr(iunit2,verbose,
     $     ILEN,IBUFF,KELEM,KVALS)
      IMPLICIT NONE

      INTEGER iunit2            ! Input: unit to write BUFR message to
      INTEGER verbose           ! Input
      INTEGER ilen              ! Input: Length of BUFR message (in words)
      INTEGER ibuff(*)          ! Input: Buffer to hold BUFR message
      INTEGER kelem             ! Input: expected (max) number of expanded elements
      INTEGER kvals             ! Input: expected (max) number of data values

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
c     BUSEL variables
      INTEGER ktdlst(kelem)     ! array containing data descriptors in section 3
      INTEGER ktdexp(kelem)     ! array containing expanded data descriptors
      INTEGER ktdlen            ! number of data descriptors in section 3
      INTEGER ktdexl            ! number of entries in list of expanded data des
      INTEGER kdata(kelem)
      INTEGER kdata_indices(100) ! List of delayed replication indeces
      INTEGER kbuff(10000),kbufl
      integer kerr,ii,kdlen,idx

      CALL BUFREX(ILEN, IBUFF, KSUP, KSEC0, KSEC1, KSEC2,
     &     KSEC3, KSEC4, KELEM, CNAMES, CUNITS, KVALS, VALUES,
     &        CVALS, KERR)
      IF(KERR.NE.0)THEN
         WRITE(*,*) 'ERROR CALLING BUFREX, KERR=',KERR
         RETURN
      ENDIF

c     Must calculate kdata, i.e. replicated descriptors
c     For this we first need the expanded descriptors (ktdexp)
      CALL BUSEL2(1,kelem,ktdlen,ktdlst,ktdexl,ktdexp,
     &     cnames,cunits,kerr)
      IF(KERR.NE.0)THEN
         WRITE(*,*) 'ERROR CALLING BUSEL2, KERR=',KERR
         RETURN
      ENDIF
c     309194 is temp drop, 309195 is temp mobile, 309196 is temp ship
c     The only difference between them is that the first descriptor is
c     001194  CARRIER BALLOON/AIRCRAFT IDENTIFIER
c     001195  MOBIL LAND STATION IDENTIFIER
c     001011  SHIP OR MOBILE LAND STATION IDENTIFIER
c     respectively, but all of them are CCITTIA5 with data width 72
      IF (ktdlst(1).NE.309007.AND.ktdlst(1).NE.309194
     &     .AND.ktdlst(1).NE.309195.AND.ktdlst(1).NE.309196) THEN
         IF(verbose.GT.0) then
            WRITE(*,*) 'BUSEL2 GAVE KTDLST(1)=',KTDLST(1)
            WRITE(*,*) 'EXPECTED 309007 or 309194-96'
         ENDIF
         RETURN
      ENDIF

      idx = 0
      DO ii=1,ktdexl
         IF (ktdexp(ii).EQ.31000 .OR. ktdexp(ii).EQ.31001 .OR.
     &        ktdexp(ii).EQ.31002) THEN
            idx = idx + 1
            kdata_indices(idx) = ii
         END IF
      END DO
      kdlen = idx
      DO ii=1,kdlen
         kdata(ii) = values(kdata_indices(ii))
      ENDDO

      CALL BUFREN(ksec0,ksec1,ksec2,ksec3,ksec4,ktdlen,ktdlst,
     &     kdlen,kdata,kelem,kvals,values,cvals,kbufl,kbuff,kerr)
      IF(KERR.NE.0)THEN
         WRITE(*,*) 'ERROR CALLING BUFREN, KERR=',KERR
      ELSE
         CALL PBWRITE(iunit2,kbuff,kbufl*4,kerr)
      ENDIF

      RETURN
      END SUBROUTINE reencode_hirlambufr


c     Consider character argument as missing value if consisting of
c     spaces only, or one bits only, or first some spaces followed by
c     one bits only
      LOGICAL FUNCTION is_missing(cval)
      IMPLICIT NONE
      CHARACTER*80 cval
      INTEGER II

      is_missing=.TRUE.
      DO II=1,LEN_TRIM(ADJUSTL(cval))
         IF(IACHAR(cval(II:II)).NE.255) is_missing=.FALSE.
      ENDDO

      RETURN
      END FUNCTION is_missing
