# makefile for library: libdec.a

FC = gfortran
CC = gcc

LDIR = ../lib
IDIR = ../include1

LIBRARY = $(LDIR)/libdec.a

SOURCES = $(L1) $(L3) $(L4) $(L5) $(L6) $(L7) $(L8) $(L9) $(L10) \
         $(L11) $(L12) $(L13) $(L14) $(L15) $(L16) $(L17) $(L18) $(L19) \
         $(L20) $(L21) $(L22) $(L23) $(L24) $(L25) $(L26) $(L27) $(L28) \
         $(L29) $(L30) $(L31)


.PRECIOUS : $(LIBRARY)

.SUFFIXES: .o .f .c 

# From .f to .o
.f.o:
	$(FC) -c -I$(IDIR) $<

# From .c to .o
.c.o: 
	$(CC) -c -I$(IDIR) $<

L1 = datum.o gbyte_alpha.o istime.o ktompsi.o letfig.o pack.o \
     prtkdec.o prtkint.o unpack.o d2pts.o tvirt.o compos.o \
     pnterp.o diurnal.o asctim.o timediff.o decr_date.o checkdate.o
L3 = getbull.o
L4 = extgrp.o extint.o extval.o prtbull.o \
     nexprt2.o nexsep2.o nextend.o nexteq.o nextfig.o nextlet.o \
     nextmi.o nextprt.o nextsep.o nextval.o  preprt.o presep.o 
L5 = prochdr.o
L6 = remeee.o 
L7 = sec0.o sec2.o sec5.o octnum.o
L8 = settab.o 
L9 = isfile.o isfile24.o isfile8.o isfile48.o
L10= cddff.o preduc.o season.o 
L11= cslt.o 
L12= cuat.o hydro.o 
L13= derita.o 
L14= dplim.o laps.o wshear.o
L15= getlim.o gualim.o 
L16= inshiz.o 
L17= landsea.o 
L18= pinter.o setsf.o
L19= qcairep.o qcdribu.o 
L20= qcbathy.o qcpilot.o qctesac.o
L21= qcsatob.o qctovs.o 
L22= qcsynop.o qcmetar.o
L23= qctemp.o rcbfiles.o
L24= setabs.o setflg.o setgrs.o setint.o upflag.o zcomp.o
L25= shipad.o 
L26= poschk.o 
L27= shippk.o shipup.o    
L28= msysinit.o    
L29= biascor.o 
L30= direct2indexed.o
L31= bswap2.o bswap4.o rlunit.o swapfile.o
 
# Must put this dependency rule after the L's are defined, or else
# 'make' does not work. Don't understand why...
$(LIBRARY): $(SOURCES)
	ar ru $@ $(SOURCES)
	ranlib $@


$(L3) : $(IDIR)/commdb.f
$(L4) : $(IDIR)/parameter.f $(IDIR)/comwork.f
$(L5) : $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/combuff.f \
	$(IDIR)/comindx.f $(IDIR)/comstat.f
$(L6) : $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/comindx.f
$(L7) : $(IDIR)/parameter.f $(IDIR)/compoin.f
$(L8) : $(IDIR)/parameter.f $(IDIR)/comtab.f $(IDIR)/comtabc.f $(IDIR)/comdir.f
$(L9) : $(IDIR)/isincl5.f
$(L10): $(IDIR)/const.f
$(L11): $(IDIR)/const.f $(IDIR)/paramq.f $(IDIR)/ctsf.f
$(L12): $(IDIR)/const.f $(IDIR)/paramq.f $(IDIR)/ctua.f $(IDIR)/conf.f
$(L13): $(IDIR)/const.f $(IDIR)/paramq.f $(IDIR)/ctua.f $(IDIR)/conf.f \
	$(IDIR)/ctsf.f
$(L14): $(IDIR)/const.f $(IDIR)/paramq.f $(IDIR)/ctua.f
$(L15): $(IDIR)/const.f $(IDIR)/paramq.f
$(L16): $(IDIR)/paramsh.f $(IDIR)/comdir.f $(IDIR)/comshiz.f
$(L17): $(IDIR)/paramsh.f $(IDIR)/comlsm.f
$(L18): $(IDIR)/paramq.f
$(L19): $(IDIR)/parameter.f $(IDIR)/paramq.f $(IDIR)/paramsh.f \
	$(IDIR)/comwt.f $(IDIR)/const.f $(IDIR)/comkey.f $(IDIR)/comkeyc.f
$(L20): $(IDIR)/parameter.f $(IDIR)/paramq.f $(IDIR)/paramsh.f \
	$(IDIR)/comwt.f $(IDIR)/const.f $(IDIR)/comkey.f $(IDIR)/comkeyc.f
$(L21): $(IDIR)/paramts.f $(IDIR)/comwt.f $(IDIR)/const.f $(IDIR)/paramq.f
$(L22): $(IDIR)/parameter.f $(IDIR)/comwt.f $(IDIR)/const.f $(IDIR)/paramq.f \
	$(IDIR)/paramsh.f $(IDIR)/comkey.f $(IDIR)/comkeyc.f
$(L23): $(IDIR)/parameter.f $(IDIR)/comwt.f $(IDIR)/const.f $(IDIR)/paramq.f \
        $(IDIR)/paramsh.f $(IDIR)/conf.f $(IDIR)/comkey.f $(IDIR)/comkeyc.f \
	$(IDIR)/comdir.f $(IDIR)/parabias.f 
$(L24): $(IDIR)/const.f $(IDIR)/paramq.f $(IDIR)/conf.f
$(L25): $(IDIR)/paramsh.f $(IDIR)/const.f $(IDIR)/comship.f
$(L26): $(IDIR)/paramsh.f $(IDIR)/const.f $(IDIR)/comship.f $(IDIR)/comdir.f \
        $(IDIR)/compref.f $(IDIR)/comlsm.f
$(L27): $(IDIR)/paramsh.f $(IDIR)/compref.f $(IDIR)/comship.f $(IDIR)/const.f
$(L28): $(IDIR)/commsys.f $(IDIR)/commdb.f $(IDIR)/comdir.f
$(L29): $(IDIR)/parameter.f $(IDIR)/comwt.f $(IDIR)/const.f $(IDIR)/paramq.f \
        $(IDIR)/paramsh.f $(IDIR)/conf.f $(IDIR)/comkey.f $(IDIR)/parabias.f 


clean:
	rm -f *.o
