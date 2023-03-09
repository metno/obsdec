# Makefile for bufrdump_netcdf. Note that bufrdump_netcdf.F ought to
# be kept in sync with bufrdump.F in bufr-utils

# Before running bufrdump_netcdf you should probably do
# 	export BUFR_TABLES=/usr/local/lib/bufrtables/

FC = gfortran
FCFLAGS = -fbackslash
LDIR = /usr/local/lib/

.SUFFIXES: .o .F

# FORTRAN files

# From .F to .o
.F.o:
	$(FC) -c $(FCFLAGS) $<

all: bufrdump_netcdf

clean:
	rm -f *.o bufrdump_netcdf

bufrdump_netcdf: bufrdump_netcdf.o comfilter.f
	$(FC) $(FCFLAGS) -o bufrdump_netcdf  $< -L $(LDIR) -lbufr
