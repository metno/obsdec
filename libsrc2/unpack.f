      SUBROUTINE UNPACK(KBPW,KS,KD,KWPT,KBPT,KSI,IERR)
C                 
C**** *UNPACK*                                                       
C                                                                    
C                                                                    
C     PURPOSE.                                                       
C     --------                                                       
C            PURPOSE OF THIS ROUTINE IS TO UNPACK BIT STRING OF      
C         *KSI* BITS, STARTED AT WORD KWPT OF ARRAY *KS* AFTER       
C         SKIPPING KSK1 BITS. RESULT IS PUT INTO *KD*. AT THE END    
C         POINTERS *KWPT* AND *KBPT* ARE ADJUSTED.                   
C                                                                      
C**   INTERFACE.                                                       
C     ----------                                                       
C                                                                      
C     *CALL* *UNPACK(KBPW,KS,KD,KWPT,KBPT,KSI)* 
C                                                                      
C                                                                      
C            *KBPW*  - NUMBER OF BITS PER COMPUTER WORD.
C            *KWPT*  - WORD POINTER
C            *KS*    - SOURCE(CONTINUOUS BIT STRING OF ARBITRARY LENGTH
C            *KD*    - DESTINATION                                     
C            *KBPT*  - NUMBER OF BITS TO BE SKIPPED                    
C            *KSI*   - NUMBER OF BITS TO BE EXTRACTED.                 
c            *ierr*  - error indicator    
C                                                                      
C     METHOD.                                                          
C     -------                                                          
C                                                                      
C            NONE.                                                     
C                                                                      
C                                                                      
C     EXTERNALS.                                                       
C     ----------                                                       
C                                                                      
C                                                                      
C            *CALL GBYTE(KS,KD,KBPT,KSI)*                              
C                                                                      
C            *KS*    - SOURCE(CONTINUOUS BIT STRING OF ARBITRARY LENGTH
C            *KD*    - DESTINATION.                                    
C            *KSK1*  - NUMBER OF BITS TO BE SKIPPED.                   
C            *KSI*   - NUMBER OF BITS TO BE EXTRACTED.                 
C                                                                      
C                                                                      
C                                                                      
C     REFERENCE.                                                       
C     ----------                                                       
C                                                                      
C          NONE.                                                       
C                                                                      
C     AUTHOR.                                                          
C     -------                                                          
C                                                                      
C          M. D. DRAGOSAVAC    *ECMWF*       09/06/86.                 
C                                                                      
C                                                                      
C     MODIFICATIONS.                                                   
C     --------------                                                   
C                                                                      
C          NONE.                                                       
C                                                                      
C                                                                      
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)                      
C                                                                      
C                                                                     
      DIMENSION KS(*)
C                                                                      
C     -----------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.                                  
C                --------------------                                  
 100  CONTINUE                                                         
C
      if(ksi.gt.KBPW) then
                         ierr=3
                         write(*,9901)
                         return
                      end if
c
      CALL GBYTE(KS,KD,KBPT,KSI)                                       
C                                                                      
C     -----------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.                         
C                -----------------------------                         
 110  CONTINUE                                                         
C                                                                      
      KBPT= KBPT + KSI                                                
C                                                                      
      IF(KBPT.GE.KBPW) THEN                                            
                          KW  = KBPT/ KBPW                             
                          KBPT= KBPT - KW * KBPW                       
                          KWPT= KWPT +KW                               
                       END IF                                          
C
 9901 FORMAT(1H ,'ERROR - NUMBER OF BITS TO BE EXTRACTED
     1 LONGER THAN COMPUTER WORD.'/
     21H ,'        MESSAGE HAS BEEN SKIPPED.')
      RETURN                                                           
      END                                                              
