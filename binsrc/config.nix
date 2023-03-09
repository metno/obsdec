# include file for make for nix_arep.F, nix_meta.F, nix_pilo.F,
# nix_temp.F

# Note that compiled program is placed in this directory (src
# directory). Must therefore manually be transferred to BDIR.

BDIR = ../bin
IDIR = ../include1
LDIR = ../lib

include config.$(ARCH)

.SUFFIXES: .o .F .c

# From .F to .o
.F.o: 
	$(FC) -c $(FCFLAGS1) -I$(IDIR) $<

# Note: the order of -ldec -lbufr does matter (or else error messages
# occur). Also, the order of the -L directories might be important (if
# an invalid libbufr.a happens to be in $LDIR)
$(BDIR)/$(OBSTYPE): nix_$(OBSTYPE).o $(LDIR)/libdec.a \
		    $(LBUFRDIR)/libbufr.a
	$(FC) $(FCFLAGS2) -o $(OBSTYPE) nix_$(OBSTYPE).o \
	-L $(LBUFRDIR) -L $(LDIR) \
	-ldec -lbufr
	@ echo "$(MOVE)"

