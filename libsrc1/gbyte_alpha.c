
#define GBYTES gbytes_
#define GBYTE  gbyte_
#define SBYTES sbytes_
#define SBYTE  sbyte_

#define SWORD 32
#define MASKSWORD 0x3f

static int onbit[32] = {
  0x00000001,0x00000002,0x00000004,0x00000008,
  0x00000010,0x00000020,0x00000040,0x00000080,
  0x00000100,0x00000200,0x00000400,0x00000800,
  0x00001000,0x00002000,0x00004000,0x00008000,
  0x00010000,0x00020000,0x00040000,0x00080000,
  0x00100000,0x00200000,0x00400000,0x00800000,
  0x01000000,0x02000000,0x04000000,0x08000000,
  0x10000000,0x20000000,0x40000000,0x80000000};

/* 
//  Retrieve or store arbitrary bit-size values from or to SWORD-bit words
//
//  Rewritten April 2000, J.D.Chambers, ECMWF.
//
*/

void GBYTES(
  void *Source,
  void *Destination,
  int *startSkip,
  int *bitsPerValue,
  int *skipBetweenValues,
  int *numberOfValues) {
/*
//  GBYTES: 
//
//   Unpacks values from source to destination.
//
//   Makes an initial skip over bits in source, then skips bits between values.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in source
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
//   skipBetweenValues >= 0    Number of bits to be skipped between values
//
//   numberOfValues >= 0       Number of values to be packed/unpacked
//
*/
unsigned char* source = (unsigned char*) Source;
int * destination = (int *) Destination;
int bit, byte, bitShift, nextValueFirstBit, next, step;

    step = *bitsPerValue + *skipBetweenValues;

    nextValueFirstBit = *startSkip;

    for (next = 0; next < *numberOfValues; ++next) {

      destination[next] = 0;
/*
// Move one bit at a time (!!)
// Uses shifts and masks instead of divides.
// Sets destination bit to 0/1 as source bit is 0/1.
*/
      for ( bit = 0; bit < *bitsPerValue; bit++ ) {
        byte        = ( nextValueFirstBit + bit ) >> 3;
        bitShift    = 7 - ( nextValueFirstBit + bit ) & 0x7;

        destination[next] = (destination[next] << 1) ;
        if( (source[byte] & onbit[bitShift]) ) destination[next] |= 1;
      }

      nextValueFirstBit += step;
    }

}

void GBYTE(
  void *source,
  void *destination,
  int *startSkip,
  int *bitsPerValue) {
/*
//  GBYTE: 
//
//   Unpacks one value from source to destination.
//
//   Makes an initial skip over bits in source.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in source
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
*/
int skipBetweenValues=0, numberOfValues=1;

    GBYTES(source,
           destination,
           startSkip,
           bitsPerValue,
           &skipBetweenValues,
           &numberOfValues);
}


void SBYTES(
  void *Destination,
  void *Source,
  int *startSkip,
  int *bitsPerValue,
  int *skipBetweenValues,
  int *numberOfValues) {
/*
//  SBYTES: 
//
//   Packs values from source to destination.
//
//   Makes an initial skip over bits in source, then skips bits between values.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in destination
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
//   skipBetweenValues >= 0    Number of bits to be skipped between values
//
//   numberOfValues >= 0       Number of values to be packed/unpacked
//
*/
unsigned char* destination = (unsigned char*) Destination;
int * source = (int *) Source;
int bitShift, bitPosition;
int nextValueFirstBit, bpvm1;
int nextDestination, nextBit;
int step, nextValue;
unsigned char setBit0[8] = {0xfe,0xfd,0xfb,0xf7,0xef,0xdf,0xbf,0x7f};
unsigned char setBit1[8] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};

    step = *bitsPerValue + *skipBetweenValues;

    nextValueFirstBit = *startSkip;
    bpvm1 = (*bitsPerValue - 1);

    for( nextValue = 0; nextValue < *numberOfValues; ++nextValue) {

/*
// Move one bit at a time (!!)
// Uses shifts and masks instead of divides.
// Sets destination bit to 0/1 as source bit is 0/1.
*/
      for ( nextBit = 0; nextBit < *bitsPerValue; nextBit++ ) {
        nextDestination = ( nextValueFirstBit + nextBit ) >> 3;
        bitShift    = (bpvm1 - nextBit) & MASKSWORD;
        bitPosition = 7 - (( nextValueFirstBit + nextBit ) & 0x7);

        if( (source[nextValue] & onbit[bitShift]) )
          destination[nextDestination] |= setBit1[bitPosition];
        else
          destination[nextDestination] &= setBit0[bitPosition];
      }
      nextValueFirstBit += step;

    }
}


void SBYTE(
  void *destination,
  void *source,
  int *startSkip,
  int *bitsPerValue) {
/*
//  SBYTE: 
//
//   Packs one value from source to destination.
//
//   Makes an initial skip over bits in source.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in source
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
*/
int skipBetweenValues=0, numberOfValues=1;

    SBYTES(destination,
           source,
           startSkip,
           bitsPerValue,
           &skipBetweenValues,
           &numberOfValues);
}
