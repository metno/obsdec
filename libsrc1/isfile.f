      SUBROUTINE ISFILE(ctype,ctime,cfname,ret_code)
C
C**** *ISFILE*
C
C
C     PURPOSE.
C     --------
C
C         SET UP APPROPRIATE RDB FILE NAME.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ISFILE(CTYPE,CTIME,CFNAME,RET_CODE)*
C
C         INPUT   : CTYPE
C                   CTIME 
cps CTIME = Observation time in format YYYYMMDDhhmm, where MMDDHHmm is fetched 
cps from YYMMJ (YYGGa4) and GGgg/ fields if these exist, else from abbrev. header
c
C         OUTPUT  : CFNAME
C                   RET_CODE
cps ret_code = 0: no error
cps            3: data for future
cps            4: too old date 
cps            5: itime (and therefore ctime) illegal
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. D. DRAGOSAVAC    *ECMWF*       1989.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
C
      CHARACTER*(*) ctype
      CHARACTER*(*) ctime
      CHARACTER*(*) cfname
      CHARACTER*23 CT,CURRENT
CRR
      CHARACTER*24 CORINT
      CHARACTER*3 YMONTH(12)
c
      INTEGER ltype, ltime, ret_code
      DIMENSION NDAYM(12)
C
CRR   DATA YMONTH/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
CRR  1            'AUG','SEP','OCT','NOV','DEC'/
      DATA YMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     1            'Aug','Sep','Oct','Nov','Dec'/
      DATA NDAYM/31,28,31,30,31,30,31,31,30,31,30,31/
C     ------------------------------------------------------------------
C*          1.   CREATE FILE NAME.
C                -----------------
 100  CONTINUE
c
      ret_code= 0
cps   ltype,ltime: number of characters used in ctype (should be 4),ctime (should be 10)
      ltype = INDEX(ctype,' ') - 1
      ltime = INDEX(ctime,' ') - 1
      IF (ltype.le.0.or.ltype.gt.4) ltype = 4
      IF (ltime.le.0.or.ltime.gt.10) ltime = 10
c
cps Transfer observation time (ctime) into CT, which uses a format that 
cps ISTIME understands, i.e. DD-MMM-YYYY HH:MM:SS.CC
c
      read(ctime,'(I4,4(I2))') IYYY,IM,ID,ihh,imm
      WRITE(CT(1:2),'(I2.2)')ID
      CT(3:3)='-'
C
      DO 101 I=1,12
      IF(IM.EQ.I) THEN
         CT(4:6)=YMONTH(I)
      END IF
 101  CONTINUE
C
      CT(7:7)='-'
      WRITE(CT(8:11),'(I4)') IYYY
      CT(12:12)=' '
      WRITE(CT(13:14),'(I2.2)') IHH
      WRITE(CT(16:17),'(I2.2)') IMM
      CT(15:15)=':'
      CT(18:18)=':'
      CT(19:20)='00'
      CT(21:21)='.'
      CT(22:23)='00'
C
cps Then transfer current time (corint) into current, which uses a format that 
cps ISTIME understands, i.e. DD-MMM-YYYY HH:MM:SS.CC
c
CRR  added CORINT-character*24
      CALL FDATE(CORINT)
      current(1:2) = corint(9:10)
      current(3:3) = '-'
      current(4:6) = corint(5:7)
      current(7:7) = '-'
      current(8:11) = corint(21:24)
      current(12:12) = ' '
      current(13:14) = corint(12:13)
      current(15:15) = ':'
      current(16:17) = corint(15:16)
      current(18:18) = ':'
      current(19:20) = corint(18:19)
      current(21:21) = '.'
      current(22:23) = '0'
CRR
c
cps Calculate minutes since epoch for 'now' and obs. time
      CALL ISTIME(CURRENT,NMIN1,IRET)
      CALL ISTIME(CT     ,NMIN0,IRET)
C
cps And then the difference in minutes between these 2 times
      IDIFF=NMIN1-NMIN0
c
      IF(IDIFF.LE.0) THEN
         IF(IDIFF.LT.-25) THEN
            PRINT*,' isfile: WARNING - DATA FOR FUTURE ',CT,
     *           ' diff.mins.=',idiff
            RET_CODE=3
            RETURN
         END IF
      END IF

cpssep02 Changed from 5 to 10 days delay for tesa and bath, to 2 days for
cps      other types (temp, pilo, arep, drau ocea.). Note that this only means
cps      that bufr-files will be written up to 10 (2) days old; for other
cps      types than bathy and tesac, bufr_obs will prevent writing to
cps      obsfile observations with day older than current obsfile.
      if (ctype.eq.'tesa' .or. ctype.eq.'bath') then
         IF (IDIFF.GT.60*24*10) THEN
            PRINT*,' isfile: INFO - TOO OLD DATE ',CT
            RET_CODE=4
            RETURN
         END IF
      elseif(IDIFF.GT.60*24*2) then
         PRINT*,' isfile: INFO - TOO OLD DATE ',CT
         RET_CODE=4
         RETURN
      END IF
 
      itime=ihh*100+imm
      if(itime.gt.2359) then
         print*,' isfile: WARNING - itime > 2359! ',itime
         ret_code=5
         return
      end if
c
      if(itime.ge.0901.and.itime.le.1500) ctime(9:10)='12'
      if(itime.ge.1501.and.itime.le.2100) ctime(9:10)='18'
      if(itime.ge.2101.and.itime.le.2359) then
         id=id+1
         if(id.gt.ndaym(im)) then
            if(im.eq.12) then
               id=1
               im=1
               iyyy=iyyy+1
               go to 200
            end if
            if(im.eq.2) then
               if (mod(iyyy,400).eq.0 .or. 
     *              (mod(iyyy,4).eq.0 .and. mod(iyyy,100).ne.0)) then
                  if(id.le.29) go to 200
                  id=1
                  im=im+1
                  go to 200
               end if
            end if
c
            id=1
            im=im+1
c
c
 200  continue
c
         end if
c
         WRITE(CTIME,'(I4,2(I2.2))') IYYY,IM,ID
         CTIME(9:10)='00'
C
      end if
C
      IF(itime.ge.0000.and.itime.le.0300) ctime(9:10)='00'
      if(itime.ge.0301.and.itime.le.0900) ctime(9:10)='06'
c
      cfname = ctype(:ltype)//ctime(:ltime)
c
c
      RETURN
c
      END
