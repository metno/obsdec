# makefile for library: libdecx.a

FC = gfortran

LDIR = ../lib
IDIR = ../include2

LIBRARY = $(LDIR)/libdecx.a

SOURCES = $(L1) $(L3) $(L4) $(L5) $(L6) $(L7) $(L8) $(L9) \
	 $(L10) $(L11) $(L12) $(L13) $(L14) $(L28) \
         $(L30) $(L32) $(L33)


.PRECIOUS : $(LIBRARY)

.SUFFIXES: .o .F .f

# From .F to .o
.F.o:
	$(FC) -c -D FORTRAN90 -I$(IDIR) $<

# From .f to .o
.f.o:
	$(FC) -c -I$(IDIR) $<

L1 = datum.o gbyte_alpha.o istime.o ktompsi.o letfig.o pack.o \
     prtkdec.o prtkint.o unpack.o d2pts.o compos.o \
     asctim.o timediff.o decr_date.o checkdate.o mardsen.o \
     ic3333.o
L3 = getbull.o
L4 = extgrp.o extint.o extval.o prtbull.o \
     nexprt2.o nexsep2.o nextend.o nexteq.o nextfig.o nextlet.o \
     nextmi.o nextprt.o nextsep.o nextval.o  preprt.o presep.o 
L5 = prochdr.o proctxt.o
L6 = remeee.o procrfb.o
L7 = savbull.o savrep.o
L8 = locstat.o impstat.o
L9 = errsta.o
L10= getnext.o
L11= station.o
L12= initvar.o
L13= stdatm.o
L14= shipheights.o
L28= msysinit.o    
L30= direct2indexed.o
L32= prtvalues.o
L33= writebufr.o

# Must put this dependency rule after the L's are defined, or else
# 'make' does not work. Don't understand why...
$(LIBRARY): $(SOURCES)
	ar ru $@ $(SOURCES)
	ranlib $@

$(L3) : $(IDIR)/commsys.f
$(L4) : $(IDIR)/parameter.f $(IDIR)/comwork.f
$(L5) : $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/combuff.f \
	$(IDIR)/comindx.f   $(IDIR)/comstat.f
$(L6) : $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/comindx.f
$(L7) : $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/comindx.f \
        $(IDIR)/combuff.f   $(IDIR)/commdbc.f $(IDIR)/commsys.f
$(L8) : $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/combuff.f
$(L9) : $(IDIR)/parameter.f $(IDIR)/comwork.f \
	$(IDIR)/comindx.f   $(IDIR)/comstat.f
$(L10): $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/comstat.f \
        $(IDIR)/comwt.f     $(IDIR)/commdbc.f $(IDIR)/commsys.f \
        $(IDIR)/comkey.f
$(L11): $(IDIR)/combuff.f   $(IDIR)/comdir.f
$(L12): $(IDIR)/parameter.f $(IDIR)/comwork.f $(IDIR)/combuff.f \
        $(IDIR)/comstat.f   $(IDIR)/comwt.f   $(IDIR)/commsys.f
$(L13): $(IDIR)/comstd.f    $(IDIR)/comdir.f
$(L14): $(IDIR)/comsh.f     $(IDIR)/comshc.f  $(IDIR)/comdir.f
$(L28): $(IDIR)/commsys.f   $(IDIR)/comdir.f
$(L32): $(IDIR)/parameter.f $(IDIR)/comwt.f
$(L33): $(IDIR)/parameter.f $(IDIR)/comwrite.f $(IDIR)/combufren.f
