      SUBROUTINE ISFILE24(ctype,ctime,cfname,ret_code)
C
C**** *ISFILE24*
C
C
C     PURPOSE.
C     --------
C
C         SET UP APPROPRIATE RDB FILE NAME.
Crr        This is for synop files - name for each hour
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ISFILE24(CTYPE,CTIME,CFNAME,RET_CODE)*
C
C         INPUT   : CTYPE
C                   CTIME
C         OUTPUT  : CFNAME
C                   RET_CODE
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
      INCLUDE 'isincl5.f'
c
      CHARACTER*(*) ctype
      CHARACTER*(*) ctime
cpsapr98      CHARACTER*(ip) cfname
      CHARACTER*(*) cfname
      CHARACTER*23 CT,CURRENT
CRR
      CHARACTER*24 CORINT
      CHARACTER*3 YMONTH(12)
c
      INTEGER ltype, ltime, ret_code
      DIMENSION NDAYM(12)
C
      DATA YMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     1            'Aug','Sep','Oct','Nov','Dec'/
      DATA NDAYM/31,28,31,30,31,30,31,31,30,31,30,31/
C     ------------------------------------------------------------------
C*          1.   CREATE FILE NAME.
C                -----------------
 100  CONTINUE
c
      ret_code= 0
      ltype = INDEX(ctype,' ') - 1
      ltime = INDEX(ctime,' ') - 1
      IF (ltype.le.0.or.ltype.gt.4) ltype = 4
cpsapr98      IF (ltime.le.0.or.ltime.gt.8) ltime = 8
      IF (ltime.le.0.or.ltime.gt.10) ltime = 10
c
cpsapr98      read(ctime,'(5(I2.2))') IY,IM,ID,ihh,imm
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
cpsapr98      IYYY=IY+1900
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
CRR   ISTATUS=SYS$ASCTIM(,CURRENT,,)
      CALL ISTIME(CURRENT,NMIN1,IRET)
      CALL ISTIME(CT     ,NMIN0,IRET)
C
      IDIFF=NMIN1-NMIN0
      IF(IDIFF.LE.0) THEN
         IF(IDIFF.LT.-25) THEN
            PRINT*,' isfile24: INFO - DATA FOR FUTURE ',CT,
     *           'diff.mins.=',idiff
            RET_CODE=3
            RETURN
         END IF
      END IF
cpsdec02      IF(IDIFF.GT.7200) THEN           !  5 days delay
      IF(IDIFF.GT.60*24*2) THEN           !  2 days delay
         PRINT*,' isfile24: INFO - TOO OLD DATE ',CT
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
cpsapr98      if(itime.ge.0000.and.itime.le.0031) ctime(7:8)='00'
      if(itime.ge.0000.and.itime.le.0031) ctime(9:10)='00'
      if(itime.ge.0031.and.itime.le.0130) ctime(9:10)='01'
      if(itime.ge.0131.and.itime.le.0230) ctime(9:10)='02'
      if(itime.ge.0231.and.itime.le.0330) ctime(9:10)='03'
      if(itime.ge.0331.and.itime.le.0430) ctime(9:10)='04'
      if(itime.ge.0431.and.itime.le.0530) ctime(9:10)='05'
      if(itime.ge.0531.and.itime.le.0630) ctime(9:10)='06'
      if(itime.ge.0631.and.itime.le.0730) ctime(9:10)='07'
      if(itime.ge.0731.and.itime.le.0830) ctime(9:10)='08'
      if(itime.ge.0831.and.itime.le.0930) ctime(9:10)='09'
      if(itime.ge.0931.and.itime.le.1030) ctime(9:10)='10'
      if(itime.ge.1031.and.itime.le.1130) ctime(9:10)='11'
      if(itime.ge.1131.and.itime.le.1230) ctime(9:10)='12'
      if(itime.ge.1231.and.itime.le.1330) ctime(9:10)='13'
      if(itime.ge.1331.and.itime.le.1430) ctime(9:10)='14'
      if(itime.ge.1431.and.itime.le.1530) ctime(9:10)='15'
      if(itime.ge.1531.and.itime.le.1630) ctime(9:10)='16'
      if(itime.ge.1631.and.itime.le.1730) ctime(9:10)='17'
      if(itime.ge.1731.and.itime.le.1830) ctime(9:10)='18'
      if(itime.ge.1831.and.itime.le.1930) ctime(9:10)='19'
      if(itime.ge.1931.and.itime.le.2030) ctime(9:10)='20'
      if(itime.ge.2031.and.itime.le.2130) ctime(9:10)='21'
      if(itime.ge.2131.and.itime.le.2230) ctime(9:10)='22'
      if(itime.ge.2231.and.itime.le.2330) ctime(9:10)='23'

      if(itime.ge.2331.and.itime.le.2359) then
         id=id+1
         if(id.gt.ndaym(im)) then
            if(im.eq.12) then
               id=1
               im=1
               iyyy=iyyy+1
               go to 200
            end if
            if(im.eq.2) then
cpsapr98               if(mod(iy,4).eq.0) then
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
cpsapr98         WRITE(CTIME,'(3(I2.2))') IY,IM,ID
cpsapr98         CTIME(7:8)='00'
         WRITE(CTIME,'(I4,2(I2.2))') IYYY,IM,ID
         CTIME(9:10)='00'
C
      end if
C
c
      cfname = ctype(:ltype)//ctime(:ltime)
c
c
      RETURN
c
      END
