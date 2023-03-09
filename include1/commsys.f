C
C      COMMON *COMMSYS* - Values for MSYSn files
C
C
C  - To read data from MSYSn  DATA file from ND-3054.
C  - N.B. A block on ND is a 512 byte record on IBM.
C  - N.B. A block pointer on ND is one less than on IBM.
C  -
C  - First block - word 1 = No. of last block written to file + 1
C  -                        i.e. when writing from Tandem to ND.
C  -               word 2 = 0
C  -               word 3 = Max. no. of blocks on file (constant)
C  -               word 4 = not used
C  -
C  - MSYSBL      - current record (block) number for start of bulletin
C  -
C  - Blocks second to max. are circular.
C  -
C  - Each bulletin starts at the beginning of a block and can continue
C  - over several blocks.
C  -
C  -
C
      COMMON/COMMSYS/MSYSBL,MSYSTOP,MSYSLAST
      COMMON/COMIWD/IWD1,IWD2,IWD3,IWD4
      INTEGER*2 IWD1,IWD2,IWD3,IWD4
C    ---------------------------------------------------------------
C
