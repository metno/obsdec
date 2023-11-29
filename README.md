obsdec inneholder all kildekode som trengs for å kompilere opp fortranprogrammene som inngår i ecflowsuite-obsdec/bin, i tillegg til noen få fortranprogrammer som brukes i NWP preprosessering (katalogen binsrc/assim_bufr).

Oversikt over fortranprogrammene:

```
binsrc: arep meta pilo temp
binsrc/obs2bufr: drau2bufr syno2bufr
binsrc/netcdf: bufrdump_netcdf
binsrc/assim_bufr: amdarbufr2hirbufr surfacebufr2hirbufr tempbufr2hirbufr
```

Framgangsmåte ved kompilering:

```
cd libsrc1
make
cd libsrc2
make
```

Dette vil kompilere opp libdec.a og libdecx.a, som legges i katalogen lib. Disse to bibliotekene brukes av fortranprogrammene i binsrc og binsrc/obs2bufr henholdsvis. Se lib/README for mer info.

```
cd binsrc
make all

cd binsrc/obs2bufr
make all

cd binsrc/netcdf
make all

cd binsrc/assim_bufr
make all

```

Rydding i hver av de 4 katalogene over kan gjøres med

```
make clean
```

Merk at de kompilerte programmene i binsrc (og underkataloger) blir lagt i kildekatalogen hvor kompileringen foregår. Kompileringen forutsetter at libbufr.a (fra ECMWF BUFRDC) er installert i /usr/local/lib.

