C     Created nov 2006
c     maxinh = max number of observations (stations) in obsfile = 128*nblship
c              for arep, where nblship is set in set_obs.f
c     maxdat = max number of words in data part in obsfile. Used to be set to
c              32*maxinh, since 32 is the number of words in an arep-obs, and 
c              arepxx.dat used to be the biggest obs-files. Now (2006) tempxx.dat
c              are bigger.
cpsdec05 maxdat=480000 too low for temps, changed to 600000.
cp       maxinnh=15000 will soon be too low for airep, changed to 18000
cpsnov06 maxinnh increased to 20500, maxdat to 656000
cpsfeb07 maxinnh increased to 23100, maxdat to 739200
cpsfeb09 maxinnh increased to 25600, maxdat to 819200


      PARAMETER (maxinh=25600,maxdat=819200,iobsrecl=512,iobsord=1024,
     +  	 misdat=-32767,maxfil=4)
