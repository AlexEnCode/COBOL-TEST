      *     Acronyme COBOL : Common Business Oriented Language 
      *
      *     Prase COBOL : C'est une instruction contenant un verbe 
      *                    et se termine par un point.
      *     Paragraphe COBOL :  Il s'agit d'une division de la 
      *                         logique de procedure.
      *
      *
      *    La Structure Physique du Cobol contient quatre elements:
      *    - l' identification division:
      *        Contient les informations du programme
      *        (ex: nom et auteur)
      *    - l' environment division:
      *       Indique l'environement dans lequel s'execute le programme
      *    - la data division:
      *        contient l'ensemble des donné de traitement
      *    - la working-storage section:
      *        le code procedurale


      ****************************************************************** 
      *                                                                *
      *      Rapport de synthese du document datassur.dat              *
      *                                                                *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. datassur.
       AUTHOR. AlexEnCode.

      ****************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DATASSUR ASSIGN TO 'datassur.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS DATASSUR-STATUS.

           SELECT RAPPORT ASSIGN TO 'rapport.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS RAPPORT-STATUS.
           
      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION.

       FD DATASSUR
           RECORD CONTAINS 125 CHARACTERS
           RECORDING MODE IS F.

       01 FILE-REC-DATASSUR.           
        02 FIELD-REC-DATASSUR        PIC X(125).

       FD RAPPORT
           RECORD CONTAINS 200 CHARACTERS
           RECORDING MODE IS F.    

       01 FILE-WRITE-RAPPORT.           
        02 FIELD-WRITE-RAPPORT        PIC X(200).

      ******************************************************************

       WORKING-STORAGE SECTION.

       01  DATASSUR-STATUS          PIC X(02) VALUE SPACE.
           88 DATASSUR-STATUS-OK    VALUE '00'.        
           88 DATASSUR-STATUS-EOF   VALUE '10'.

       01  RAPPORT-STATUS          PIC X(02) VALUE SPACE.
           88 RAPPORT-STATUS-OK    VALUE '00'.        
           88 RAPPORT-STATUS-EOF   VALUE '10'.

       01  PRT-STARS            PIC X(125)  VALUE ALL "*".
       01  PRT-TITLE            PIC X(19)   VALUE "RAPPORT DE SYNTHESE". 
       01  PRT-IDENTITY         PIC X(15)   VALUE "ID : AlexEnCode".          
       01  PRT-DATE             PIC X(20)   VALUE "DATE".
          
             
       01  PRT-FOOT.
           03 FILLER         PIC X(23) VALUE 'Nombre de ligne lues = '.
           03 PRT-LINE-COUNT PIC 9(03) VALUE ZERO.

       01  WS-DATASSUR-REC.
           03 REC-LGTH     PIC 9(03) VALUE 0.
           03 WS-ASSURANCE-TABLE  OCCURS 1 TO 999 TIMES
               DEPENDING ON REC-LGTH
               INDEXED BY IDX-REC.
              05 WS-DATASSUR-NUMBER    PIC X(08)     VALUE SPACE.
              05 FILLER                PIC X         VALUE '|'.
              05 WS-DATASSUR-CONTRACT  PIC X(14)     VALUE SPACE.
              05 FILLER                PIC X         VALUE '|'.
              05 WS-DATASSUR-CONTRACS  PIC X(14)     VALUE SPACE.
              05 FILLER                PIC X         VALUE '|'.              
              05 WS-DATASSUR-IRP       PIC X(41)     VALUE SPACE.
              05 FILLER                PIC X         VALUE '|'.
              05 WS-DATASSUR-C-TYPE    PIC X(08)     VALUE SPACE.
              05 FILLER                PIC X         VALUE '|'.
              05 WS-DATASSUR-DATE1     PIC 9(08)     VALUE ZERO.
              05 FILLER                PIC X         VALUE '|'.
              05 WS-DATASSUR-DATE1     PIC 9(08)     VALUE ZERO.
              05 FILLER                PIC X         VALUE '|'.              
              05 WS-DATASSUR-SOMME     PIC 999999,99 VALUE ZERO.
              05 FILLER                PIC X(5)      VALUE 'Euros'.

       01  WS-IDX-1         PIC 9(3) VALUE ZERO.
       01  WS-IDX-2         PIC 9(3) VALUE 1.


      ******************************************************************     
       PROCEDURE DIVISION.


           PERFORM 0000-MAIN-START   THRU 0000-MAIN-END. 

      ******************************************************************
       0000-MAIN-START.

           PERFORM 7000-READ-START   THRU 7000-READ-END. 

           PERFORM 7100-SORT-START   THRU 7100-SORT-END. 
           
           PERFORM 7200-WRITE-START  THRU 7200-WRITE-END.

       0000-MAIN-END.
           STOP RUN.

      ****************************************************************** 
       7000-READ-START.
           
           OPEN INPUT DATASSUR.

           DISPLAY "*********".
           DISPLAY WS-ASSURANCE-TABLE(1).
           DISPLAY "*********".           

           PERFORM UNTIL DATASSUR-STATUS-EOF
               ADD 1 TO REC-LGTH
               READ DATASSUR
               MOVE FIELD-REC-DATASSUR
               TO WS-ASSURANCE-TABLE(IDX-REC)
      *         DISPLAY WS-ASSURANCE-TABLE(IDX-REC)
           END-PERFORM.

           DISPLAY "*********".
           DISPLAY WS-ASSURANCE-TABLE(1).
           DISPLAY "*********".

       7000-READ-END.
           CLOSE DATASSUR.
           EXIT.

      ****************************************************************** 
       7100-SORT-START.

            SORT WS-ASSURANCE-TABLE 
            ON ASCENDING KEY WS-DATASSUR-NUMBER.
            
            INITIALIZE WS-IDX-1.
            INITIALIZE WS-IDX-2.

            PERFORM REC-LGTH TIMES
            IF WS-ASSURANCE-TABLE(WS-IDX-1)
            EQUAL WS-ASSURANCE-TABLE(WS-IDX-2)
            INITIALIZE WS-ASSURANCE-TABLE(WS-IDX-2)
            END-IF
            END-PERFORM.

            SORT WS-ASSURANCE-TABLE 
            ON ASCENDING KEY WS-DATASSUR-NUMBER.

       7100-SORT-END.
           EXIT.     

      ****************************************************************** 
       7200-WRITE-START. 
           
           OPEN OUTPUT RAPPORT.
           CLOSE RAPPORT.
           OPEN EXTEND RAPPORT.

           PERFORM 7210-HEAD-WRITE-START   THRU 7210-HEAD-WRITE-END. 

           MOVE REC-LGTH TO PRT-LINE-COUNT.
           INITIALIZE WS-IDX-2. 

           PERFORM PRT-LINE-COUNT TIMES
               INITIALIZE FIELD-WRITE-RAPPORT           
               MOVE WS-ASSURANCE-TABLE(WS-IDX-2) TO FIELD-WRITE-RAPPORT
               WRITE FILE-WRITE-RAPPORT
               DISPLAY WS-ASSURANCE-TABLE(WS-IDX-2)
               ADD 1 TO WS-IDX-2
           END-PERFORM.

           
           PERFORM 7220-FOOT-WRITE-START   THRU 7220-FOOT-WRITE-END. 

       7200-WRITE-END.
           CLOSE RAPPORT.
           EXIT.      

      ****************************************************************** 
       7210-HEAD-WRITE-START.

           MOVE PRT-STARS
                  TO FIELD-WRITE-RAPPORT
           WRITE FILE-WRITE-RAPPORT.            
           MOVE PRT-TITLE            
                  TO FIELD-WRITE-RAPPORT
           WRITE FILE-WRITE-RAPPORT.
           MOVE PRT-IDENTITY         
                  TO FIELD-WRITE-RAPPORT
           WRITE FILE-WRITE-RAPPORT.
           MOVE PRT-DATE             
                  TO FIELD-WRITE-RAPPORT
           WRITE FILE-WRITE-RAPPORT.
           MOVE PRT-STARS
                  TO FIELD-WRITE-RAPPORT
           WRITE FILE-WRITE-RAPPORT. 

       7210-HEAD-WRITE-END.
           EXIT.

      ****************************************************************** 
       7220-FOOT-WRITE-START.       

           INITIALIZE FIELD-WRITE-RAPPORT.   
           MOVE PRT-STARS
                  TO FIELD-WRITE-RAPPORT
           WRITE FILE-WRITE-RAPPORT.                    

           DISPLAY PRT-FOOT.
           MOVE PRT-FOOT
           TO FIELD-WRITE-RAPPORT
           WRITE FILE-WRITE-RAPPORT.

       7220-FOOT-WRITE-END.
           EXIT.

      ******************************************************************
