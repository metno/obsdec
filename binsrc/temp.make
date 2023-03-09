# Makefile for program nix_temp.f

OBSTYPE = temp

# Override this by compiling with 'make ... ARCH=sgi'
ARCH=linux

include config.nix
 
nix_temp.o: $(IDIR)/combuff.f $(IDIR)/comdir.f  $(IDIR)/comindx.f \
        $(IDIR)/comkey.f  $(IDIR)/comkeyc.f $(IDIR)/commdbc.f \
        $(IDIR)/commsys.f $(IDIR)/compoin.f $(IDIR)/comrec.f \
        $(IDIR)/comstat.f $(IDIR)/comstd.f  \
        $(IDIR)/comtab.f  $(IDIR)/comtabc.f $(IDIR)/comwork.f \
        $(IDIR)/comwt.f   $(IDIR)/comwtc.f  $(IDIR)/parameter.f \
        $(IDIR)/commdb.f  $(IDIR)/commsys.f $(IDIR)/comsh.f \
	$(IDIR)/comshc.f