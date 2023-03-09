obsdec inneholder all kildekode som trengs for å kompilere opp Fortranprogrammene som inngår i ecflowsuite-obsdec/bin, i tillegg til noen få Fortranprogrammer brukt i NWP preprosessering (katalogen binsrc/assim_bufr).

Framgangsmåte ved kompilering:

```
cd obsdec/libsrc1
make
cd obsdec/libsrc2
make
```

Dette vil kompilere opp libdec.a og libdecx.a i obsdec/lib. Se README i denne katalogen for bakgrunnen for disse to bibliotekene.

```
cd obsdec/binsrc
make all

cd obsdec/binsrc/obs2bufr
make

cd obsdec/binsrc/assim_bufr
make
```

Merk at de kompilerte programmene blir lagt i kildekatalogen hvor kompileringen foregår. Kompileringen krever at libbufr.a (fra ECMWF BUFRDC) er installert i /usr/local/lib.