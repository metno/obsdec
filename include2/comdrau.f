cpssep06 Factored out these variables used only in drau2temp from comwt.f
cps      (and removed NDATA, which we do not need in COMMON areas)
      logical l11,l12,l13,l21,l22,l31,l32,l33,lalalalala
      integer ntl,ncl

      COMMON /COMDRAU/ L11,L12,L13,L21,L22,L31,L32,L33,LaLaLaLaLa,
     *     NTL,NCL

c           L11-L33 - Logical variables, set true if
c           L11: DRIBU SURFACE REPORT
c           L12: DRIBU SUBSURFACE Z,T,S REPORT
c           L13: DRIFTER SUBSURFACE Z,DD,CCC REPORT (CURRENT)
c           L21: SURFACE BATHY
c           L22: OCEANOGRAPHIC BATHY
c           L31: SURFACE TESAC
c           L32: OCEANOGRAPHIC TESAC (Z, T, S)
c           L33: OCEANOGRAPHIC TESAC (Z, DD CCC)
c           NTL - number of subsurface temperature levels
c           NCL - number of subsurface current levels
c           LaLaLaLaLa: True if latitude is reported as LALALALALA, i.e. degrees
c                        and millidegrees, false if reported as LALALALA,
c                        i.e. degreees and minutes
