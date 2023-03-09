 
c*************************************************************
c*                                                           *
c*    List of declarations that are included in every        *
c*    IS-.FOR file.                                          *
c*    Changes to parameter values must be made in this file  *
c*                                                           *
c*    IM Godfrey,  11 July 1988                              *
c*    M. Dragosavac, May 1990    Modification                *
c*                                                           *
c*                - key definition changed                   *
c*                - rdb software work with 4 files at once.  *
c*************************************************************
c
      PARAMETER (Length_Key = 48)
      PARAMETER (Length_KU =  32)
      PARAMETER (Length_KS = Length_Key - Length_KU)
      PARAMETER (Length_Data = 16320)
      PARAMETER (NMAX   = 340)
      PARAMETER (IFNMAX = 4  )
      PARAMETER (NMAXSEL=11000)
      PARAMETER (Max_Field_Len =  20)
      PARAMETER (Max_No_Conds  =  20)
      PARAMETER (ip = 12)
c
c
c-------------------------------------------------------------
c
      STRUCTURE / KeyList /
CRR     CHARACTER*(Length_KU) List
        CHARACTER*32 List
        INTEGER Number
CRR     CHARACTER*(Length_Key) Full(NMax)
        CHARACTER*48 Full(NMax)
      END STRUCTURE
c
      STRUCTURE / Selection /
        INTEGER Number
CRR     CHARACTER*(Max_Field_Len) Name(Max_No_Conds)
        CHARACTER*20 Name(Max_No_Conds)
        INTEGER Position(Max_No_Conds)
        INTEGER Length(Max_No_Conds)
        CHARACTER*2  Condition(Max_No_Conds)
        LOGICAL      Lbinc(Max_No_Conds)
      END STRUCTURE
c
c-------------------------------------------------------------
c
      RECORD / KeyList / Key
      RECORD / Selection / Field
c
c-------------------------------------------------------------
c
      COMMON / File / File_Name
      COMMON / Keys / Key
      COMMON / Cond / Field
      COMMON / RecSel / No_Select, Key_Index, Last_Key
      common / fkeylc / FULL_LIST(NMAXSEL,ifnmax),
     1                  crdbt(ifnmax),crdbkt(ifnmax),crdbfn(ifnmax)
      common / keyadm / irdb_key_number(ifnmax),irdbfp(ifnmax),
     1                  no_of_files
c
c-------------------------------------------------------------
c
      CHARACTER*(Length_KU) Bulletin_Key
      CHARACTER*(Length_KU) Key_Unique
      CHARACTER*(Length_KS) Key_Supp
      CHARACTER*12 Key_List  /'listofkeys00'/
      CHARACTER*12 key_listnn
c
c-------------------------------------------------------------
c
c      REAL data(Length_Data)
      CHARACTER*(length_data) Bulletin_Data
      character*23 crdbt,crdbkt
      character*16 crdbfn
c
c-------------------------------------------------------------
c
      INTEGER ifptr
      INTEGER iret
      INTEGER status
      INTEGER Key_Index(NMaxSEL), Last_Key
      INTEGER*2         lock_status,
     2                null
      INTEGER*4         lock_id
      COMMON /lock_block/  lock_status,
     2                        null,
     2                        lock_id
c
c-------------------------------------------------------------
      CHARACTER*(length_key) full_key,FULL_LIST
      INTEGER*2 record_len
c
c
c-------------------------------------------------------------
      STRUCTURE /fixed_record/
CRR        CHARACTER*(length_ku) key
           CHARACTER*32 key
           INTEGER*2 rec_len
           BYTE      pointer
       END STRUCTURE    !fixed_record
c
      RECORD /fixed_record/ fixd_rec
