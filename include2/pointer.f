	 program bb
	 pointer (ptr,v), (ptr2,v2)
	 common /aa/ ptr,ptr2
	 character a*12,v*12,z*1,v2*12
	 data a/'abcdefghijkl'/
	 ptr= loc(a)
	 ptr= ptr+4
	 ptr2=malloc(12)
	 v2=a
	 z=v(1:1)
	 print*,z
	 z=v2(5:5)
	 print*,z
	 call free(ptr2)
	 end




