      SUBROUTINE ISFILE48(ctype,ctime,cfname,ret_code)
C
C**** *ISFILE48*
C
C
C     PURPOSE.
C     --------
C
C         SET UP APPROPRIATE RDB FILE NAME.
Crr        This is for metar files - name for each half-hour
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *ISFILE48(CTYPE,CTIME,CFNAME,RET_CODE)*
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
C          l. lovhoiden     *DNMI*       1995.
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
ccc      print*,'Start isfile48-ctime',ctime
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
         IF(IDIFF.LT.-15) THEN
            PRINT*,' isfile48: WARNING - DATA FOR FUTURE ',CT,
     *           'diff.mins.=',idiff
            RET_CODE=3
            RETURN
         END IF
      END IF
      IF(IDIFF.GT.7200) THEN           !  5 days delay
         PRINT*,' isfile48: WARNING - TO OLD DATE ',CT
         RET_CODE=4
         RETURN
      END IF
 
      itime=ihh*100+imm
      if(itime.gt.2359) then
                           print*,' W file name not defined'
                           ret_code=3
                           return
                        end if
c
cpsapr98      if(itime.ge.0000.and.itime.le.0015) ctime(7:8)='00'
      if(itime.ge.0000.and.itime.le.0015) ctime(9:10)='00'
      if(itime.ge.0016.and.itime.le.0045) ctime(9:10)='01'
c
      if(itime.ge.0046.and.itime.le.0115) ctime(9:10)='02'
      if(itime.ge.0116.and.itime.le.0145) ctime(9:10)='03'
c
      if(itime.ge.0146.and.itime.le.0215) ctime(9:10)='04'
      if(itime.ge.0216.and.itime.le.0245) ctime(9:10)='05'
c
      if(itime.ge.0246.and.itime.le.0315) ctime(9:10)='06'
      if(itime.ge.0316.and.itime.le.0345) ctime(9:10)='07'
c
      if(itime.ge.0346.and.itime.le.0415) ctime(9:10)='08'
      if(itime.ge.0416.and.itime.le.0445) ctime(9:10)='09'
c
      if(itime.ge.0446.and.itime.le.0515) ctime(9:10)='10'
      if(itime.ge.0516.and.itime.le.0545) ctime(9:10)='11'
c
      if(itime.ge.0546.and.itime.le.0615) ctime(9:10)='12'
      if(itime.ge.0616.and.itime.le.0645) ctime(9:10)='13'
c
      if(itime.ge.0646.and.itime.le.0715) ctime(9:10)='14'
      if(itime.ge.0716.and.itime.le.0745) ctime(9:10)='15'
c
      if(itime.ge.0746.and.itime.le.0815) ctime(9:10)='16'
      if(itime.ge.0816.and.itime.le.0845) ctime(9:10)='17'
c
      if(itime.ge.0846.and.itime.le.0945) ctime(9:10)='18'
      if(itime.ge.0916.and.itime.le.0945) ctime(9:10)='19'
c
      if(itime.ge.0946.and.itime.le.1015) ctime(9:10)='20'
      if(itime.ge.1016.and.itime.le.1045) ctime(9:10)='21'
c
      if(itime.ge.1046.and.itime.le.1115) ctime(9:10)='22'
      if(itime.ge.1116.and.itime.le.1145) ctime(9:10)='23'
c
      if(itime.ge.1146.and.itime.le.1215) ctime(9:10)='24'
      if(itime.ge.1216.and.itime.le.1245) ctime(9:10)='25'
c
      if(itime.ge.1246.and.itime.le.1315) ctime(9:10)='26'
      if(itime.ge.1316.and.itime.le.1345) ctime(9:10)='27'
c
      if(itime.ge.1346.and.itime.le.1415) ctime(9:10)='28'
      if(itime.ge.1416.and.itime.le.1445) ctime(9:10)='29'
c
      if(itime.ge.1446.and.itime.le.1515) ctime(9:10)='30'
      if(itime.ge.1516.and.itime.le.1545) ctime(9:10)='31'
c
      if(itime.ge.1546.and.itime.le.1615) ctime(9:10)='32'
      if(itime.ge.1616.and.itime.le.1645) ctime(9:10)='33'
c
      if(itime.ge.1646.and.itime.le.1715) ctime(9:10)='34'
      if(itime.ge.1716.and.itime.le.1745) ctime(9:10)='35'
c
      if(itime.ge.1746.and.itime.le.1815) ctime(9:10)='36'
      if(itime.ge.1816.and.itime.le.1845) ctime(9:10)='37'
c
      if(itime.ge.1846.and.itime.le.1915) ctime(9:10)='38'
      if(itime.ge.1916.and.itime.le.1945) ctime(9:10)='39'
c
      if(itime.ge.1946.and.itime.le.2015) ctime(9:10)='40'
      if(itime.ge.2016.and.itime.le.2045) ctime(9:10)='41'
c
      if(itime.ge.2046.and.itime.le.2115) ctime(9:10)='42'
      if(itime.ge.2116.and.itime.le.2145) ctime(9:10)='43'
c
      if(itime.ge.2146.and.itime.le.2215) ctime(9:10)='44'
      if(itime.ge.2216.and.itime.le.2245) ctime(9:10)='45'
c
      if(itime.ge.2246.and.itime.le.2315) ctime(9:10)='46'
      if(itime.ge.2316.and.itime.le.2345) ctime(9:10)='47'
c
c
      if(itime.ge.2346.and.itime.le.2359) then
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
 200        continue
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
