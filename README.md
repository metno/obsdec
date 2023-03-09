obsdec inneholder all kildekode som trengs for å kompilere opp fortranprogrammene som inngår i ecflowsuite-obsdec/bin, i tillegg til noen få fortranprogrammer brukt i NWP preprosessering (katalogen binsrc/assim_bufr).

Framgangsmåte ved kompilering:

```
cd obsdec/libsrc1
make
cd obsdec/libsrc2
make
```

Dette vil kompilere opp libdec.a og libdecx.a i obsdec/lib som trengs for kompilering av fortranprogrammene i obsdec/binsr og obsdec/binsrc/obs2bufr henholdsvis. Se obsdec/lib/README for bakgrunnen for disse to bibliotekene.

```
cd obsdec/binsrc
make all

cd obsdec/binsrc/obs2bufr
make all

cd obsdec/binsrc/assim_bufr
make all
```

Rydding i hver av de 3 katalogene over kan gjøres med

```
make clean
```

Merk at de kompilerte programmene i binsrc med underkataloger blir lagt i kildekatalogen hvor kompileringen foregår. Kompileringen krever at libbufr.a (fra ECMWF BUFRDC) er installert i /usr/local/lib.
