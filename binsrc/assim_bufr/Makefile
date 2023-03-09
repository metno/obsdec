# Makefile for some bufr filter programs used in NWP preprocessing

# Before running Bufr-decoding programs you should probably do
# 	export BUFR_TABLES=/usr/local/lib/bufrtables/

CC = gcc
CCFLAGS =
FC = gfortran
FCFLAGS = -g -O2 -fdefault-real-8 -fbackslash

LDIR = /usr/local/lib/
LFLAGS = -Wl,-rpath=$(LDIR)

.SUFFIXES: .o .c .F

# C files

# From .c to .o
.c.o:
	$(CC) -c $(CCFLAGS) $<

# FORTRAN files

# From .F to .o
.F.o:
	$(FC) -c $(FCFLAGS) $<


all: amdarbufr2hirbufr surfacebufr2hirbufr tempbufr2hirbufr

clean:
	rm -f *.o amdarbufr2hirbufr surfacebufr2hirbufr tempbufr2hirbufr

amdarbufr2hirbufr: amdarbufr2hirbufr.F
	$(FC) $(FCFLAGS) -o $@ $< -L $(LDIR) -lbufr

surfacebufr2hirbufr: surfacebufr2hirbufr.F
	$(FC) $(FCFLAGS) -o $@ $< -L $(LDIR) -lbufr

tempbufr2hirbufr: tempbufr2hirbufr.F
	$(FC) $(FCFLAGS) -o $@ $< -L $(LDIR) -lbufr
