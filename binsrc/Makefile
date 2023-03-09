# Makefile for decoding programs


default: 
	@ echo "make one of:"
	@ echo "arep meta pilo temp"

clean:
	rm -f *.o arep meta pilo temp

all:	
	make -f arep.make
	make -f meta.make
	make -f pilo.make	
	make -f temp.make

arep: nix_arep.F
	make -f arep.make

meta: nix_meta.F
	make -f meta.make

pilo: nix_pilo.F
	make -f pilo.make	

temp: nix_temp.F
	make -f temp.make
