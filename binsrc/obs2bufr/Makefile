# Makefile for programs: <obstype>2bufr.F

# Note that compiled program is placed in this directory (src
# directory).

FC = gfortran
FCFLAGS1 = -D FORTRAN90 -D LITTLE_ENDIAN -fallow-invalid-boz

IDIR = ../../include2
LDIR = ../../lib

LBUFRDIR = /usr/local/lib

MOVE = "   NOTE: You must manually move compiled file to BINDIR"

.SUFFIXES: .o .F

# From .F to .o
.F.o: 
	$(FC) -c $(FCFLAGS1) -I $(IDIR) $<

all: syno2bufr drau2bufr

clean: 
	rm -f *.o syno2bufr drau2bufr

# Note: the order of -ldecx -lbufr does matter (or else error messages
# occur). Also, the order of the -L directories might be important (if
# an invalid libbufr.a happens to be in $LDIR)
syno2bufr: syno2bufr.o $(LDIR)/libdecx.a $(LBUFRDIR)/libbufrR64.a
	$(FC) -o syno2bufr syno2bufr.o \
	-L $(LBUFRDIR) -L $(LDIR) -ldecx -lbufrR64 
	@ echo "$(MOVE)"

syno2bufr.o: $(IDIR)/combuff.f $(IDIR)/comdir.f  $(IDIR)/comindx.f \
        $(IDIR)/comkey.f  $(IDIR)/comkeyc.f $(IDIR)/commdbc.f \
        $(IDIR)/commsys.f \
        $(IDIR)/comstat.f $(IDIR)/comstd.f  \
        $(IDIR)/comwork.f \
        $(IDIR)/comwt.f   $(IDIR)/comwtc.f  $(IDIR)/parameter.f \
        $(IDIR)/commsys.f \
	$(IDIR)/comdescr.f $(IDIR)/combufren.f $(IDIR)/comwrite.f

drau2bufr: drau2bufr.o $(LDIR)/libdecx.a $(LBUFRDIR)/libbufrR64.a
	$(FC) -o drau2bufr drau2bufr.o \
	-L $(LBUFRDIR) -L $(LDIR) -ldecx -lbufrR64 
	@ echo "$(MOVE)"

drau2bufr.o: $(IDIR)/combuff.f $(IDIR)/comdir.f  $(IDIR)/comindx.f \
        $(IDIR)/comkey.f  $(IDIR)/comkeyc.f $(IDIR)/commdbc.f \
        $(IDIR)/commsys.f \
        $(IDIR)/comstat.f \
        $(IDIR)/comwork.f \
        $(IDIR)/comwt.f   $(IDIR)/comwtc.f  $(IDIR)/parameter.f \
        $(IDIR)/commsys.f $(IDIR)/commdbc.f $(IDIR)/comsh.f \
	$(IDIR)/comshc.f \
	$(IDIR)/comdescr.f $(IDIR)/combufren.f $(IDIR)/comwrite.f
