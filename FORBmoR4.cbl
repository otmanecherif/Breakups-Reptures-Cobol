l       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORBMOR4.
      *-------------------------------------------------------------*
      *                                                             *
      *                UN PROGRAMME DE RUPTURE SIMPLE               *
      *       AVEC UN FICHIER EN ENTREE, UN AFFICHAGE EN SORTIE     *
      *                  ET RUPTURES SUR 3 VARIABLE                 *
      *-------------------------------------------------------------*
       AUTHOR. MOHAMMED.
       DATE-WRITTEN. OCTOBER 2023.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
            C01 IS SAUTP
            CSP IS SAUT0
            DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *     FICHIER COMPTES EN ENTREE
      *
            SELECT MVMNTS-FILE  ASSIGN TO UT-S-MVMNTS
            ORGANIZATION LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE         SECTION.
      *
       FD MVMNTS-FILE
           BLOCK              0         RECORDS
           RECORDING MODE  F
           DATA RECORD    1-MVMNTS
           LABEL    RECORD    STANDARD.
      *
      *     LE FICHIER EN ENTREE QUI CONTIEN LES CLIENTS DES CLIENTS
      *
       COPY MVMNTS.
      *
      *
       WORKING-STORAGE SECTION.
      *
      *     VARIABLE DE FIN DE TRAITEMENT
      *
       01      AXI-FT.
            88 FIN-DE-PROGRAMME                  VALUE '1'.
        05     AXI-FT-R41n             PIC X(01) VALUE SPACE.
            88 FIN-FICHIER-R41n                  VALUE '1'.
      *
      *
      *-------------- VARIABLES SPECIFIQUES ------------------------*
       01      W-SOMMECLI-D             PIC 9(16)V99.
       01      W-SOMMECPT-D             PIC 9(16)V99.
       01      W-SOMMEDATE-D            PIC 9(16)V99.
       01      W-SOMMECLI-C             PIC 9(16)V99.
       01      W-SOMMECPT-C             PIC 9(16)V99.
       01      W-SOMMEDATE-C            PIC 9(16)V99.
       01      W-N-ECRITURE             PIC ZZZZ9.
       01      W-DEBIT                  PIC Z(7)9V99.
       01      W-CREDIT                 PIC Z(7)9V99.
       01      W-TOTAL-JOURNEE-D        PIC Z(7)9V99.
       01      W-TOTAL-COMPTE-D         PIC Z(7)9V99.
       01      W-TOTAL-CLIENT-D         PIC Z(7)9V99.
       01      W-TOTAL-JOURNEE-C        PIC Z(7)9V99.
       01      W-TOTAL-COMPTE-C         PIC Z(7)9V99.
       01      W-TOTAL-CLIENT-C         PIC Z(7)9V99.
       01      W-DATE-FORMAT            PIC X(10).
      *
      *     MEMORISATION DE L’ENREGISTREMENT A TRAITER
      *
       01      MVMNTS.
         05    MV-CLI-CODE              PIC X(5).
         05    MV-COMPTE-NUM            PIC X(6).
         05    MV-ECRITURE-NUM          PIC X(5).
         05    MV-ECRITURE-DATE         PIC X(8).
         05    MV-COMPTE-LIBELLE        PIC X(35).
         05    MV-SENS-ECRITURE         PIC A(1).
         05    MV-MONTANT               PIC 9(16)V99.
         05    FILLER                   PIC X(2).

      *
      *     VARIABLES DE RUPTURE
      *
       01      AXI-RUPT-CLI-CODE        PIC X(001) VALUE 'O'.
          88   RUPTURE-CLI-CODE                    VALUE 'O'.
       01      AXI-NOUV-CLI-CODE        PIC X(001) VALUE 'O'.
          88   NOUVEAU-CLI-CODE                    VALUE 'O'.
      *
       01      AXI-RUPT-COMPTE-NUM      PIC X(001) VALUE 'O'.
          88   RUPTURE-COMPTE-NUM                  VALUE 'O'. 
       01      AXI-NOUV-COMPTE-NUM      PIC X(001) VALUE 'O'.
          88   NOUVEAU-COMPTE-NUM                  VALUE 'O'.
      *
       01      AXI-RUPT-ECRITURE-DATE   PIC X(001) VALUE 'O'.
          88   RUPTURE-ECRITURE-DATE               VALUE 'O'.
       01      AXI-NOUV-ECRITURE-DATE   PIC X(001) VALUE 'O'.
          88   NOUVEAU-ECRITURE-DATE               VALUE 'O'.
      *
      *
       PROCEDURE DIVISION.
      *
      * ------------------> NIVEAU 01 <-----------------------------*
      *
       ENTREE-PROGRAMME.
           PERFORM DEBUT-PROGRAMME        THRU DEBUT-PROGRAMME-FIN
           PERFORM TRAITEMENTS            THRU TRAITEMENTS-FIN
                                         UNTIL FIN-DE-PROGRAMME
           PERFORM FIN-PROGRAMME          THRU FIN-PROGRAMME-FIN.
      *
       DEBUT-PROGRAMME.
           PERFORM DISPLAY-DEBUT-PGM      THRU DISPLAY-DEBUT-PGM-FIN
           PERFORM OUVERTURE-FICHIERS     THRU OUVERTURE-FICHIERS-FIN
           PERFORM LECTURE-FICHIER        THRU LECTURE-FICHIER-FIN.
       DEBUT-PROGRAMME-FIN.
           EXIT.
      *
       TRAITEMENTS.
           PERFORM LECTURE-FICHIER        THRU LECTURE-FICHIER-FIN
           PERFORM CALCUL-RUPTURE         THRU CALCUL-RUPTURE-FIN
           
           IF NOUVEAU-CLI-CODE
              PERFORM NOUVEAU-CLI         THRU NOUVEAU-CLI-FIN
           END-IF
              
           IF NOUVEAU-COMPTE-NUM
              PERFORM NOUVEAU-CPT         THRU NOUVEAU-CPT-FIN
           END-IF
           
           IF NOUVEAU-ECRITURE-DATE
              PERFORM NOUVEAU-DATE        THRU NOUVEAU-DATE-FIN
           END-IF
           
      *              
           PERFORM AFFICHAGE-MVMNT        THRU AFFICHAGE-MVMNT-FIN
           PERFORM CALCUL-SOMME           THRU CALCUL-SOMME-FIN
      *
           IF RUPTURE-ECRITURE-DATE
              PERFORM RUPTURE-DATE        THRU RUPTURE-DATE-FIN
           END-IF
           
           IF RUPTURE-COMPTE-NUM
              PERFORM RUPTURE-CPT         THRU RUPTURE-CPT-FIN
           END-IF
           
           IF RUPTURE-CLI-CODE
              PERFORM RUPTURE-CLI         THRU RUPTURE-CLI-FIN
           END-IF.        
      *     
       TRAITEMENTS-FIN.
           EXIT.
      *
       FIN-PROGRAMME.
           PERFORM FERMETURE-FICHIERS     THRU FERMETURE-FICHIERS-FIN
           PERFORM DISPLAY-FIN-PGM        THRU DISPLAY-FIN-PGM-FIN
           STOP RUN.
       FIN-PROGRAMME-FIN.
           EXIT.
      *
      * ------------------> FIN NIVEAU 01 <-------------------------*
      *
      * ------------------> NIVEAU 99 <-----------------------------*
      *    
       DISPLAY-DEBUT-PGM.
           DISPLAY ' '
           DISPLAY 'DEBUT DU PROGRAMME: '
           DISPLAY ' '.
       DISPLAY-DEBUT-PGM-FIN.
           EXIT.
      *
       OUVERTURE-FICHIERS.
           OPEN     INPUT     MVMNTS-FILE.
       OUVERTURE-FICHIERS-FIN.
           EXIT.
      *
       LECTURE-FICHIER.
           MOVE 1-MVMNTS                    TO MVMNTS
           IF NOT FIN-FICHIER-R41n
              READ MVMNTS-FILE
              AT END
                 MOVE '1'                   TO AXI-FT-R41n
              END-READ
           END-IF.                   
       LECTURE-FICHIER-FIN.
           EXIT.
      *
       AFFICHAGE-MVMNT.
           MOVE MV-ECRITURE-DATE(1:2) TO W-DATE-FORMAT(1:2)
           MOVE '/' TO W-DATE-FORMAT(3:1)
           MOVE MV-ECRITURE-DATE(3:2) TO W-DATE-FORMAT(4:2)
           MOVE '/' TO W-DATE-FORMAT(6:1)
           MOVE MV-ECRITURE-DATE(5:4) TO W-DATE-FORMAT(7:4)
           IF MV-SENS-ECRITURE = 'C'
              MOVE MV-ECRITURE-NUM TO W-N-ECRITURE
              MOVE MV-MONTANT      TO W-CREDIT
              DISPLAY ' ' MV-COMPTE-NUM '    ' W-DATE-FORMAT '       
      -       '  ' W-N-ECRITURE '                      ' W-CREDIT
           ELSE
              MOVE MV-ECRITURE-NUM TO W-N-ECRITURE
              MOVE MV-MONTANT      TO W-DEBIT
              DISPLAY ' ' MV-COMPTE-NUM '    ' W-DATE-FORMAT '       
      -       '  ' W-N-ECRITURE '      ' W-DEBIT.
       AFFICHAGE-MVMNT-FIN.
           EXIT.
      *
       CALCUL-RUPTURE.
           MOVE AXI-RUPT-CLI-CODE           TO AXI-NOUV-CLI-CODE
           MOVE AXI-RUPT-COMPTE-NUM         TO AXI-NOUV-COMPTE-NUM
           MOVE AXI-RUPT-ECRITURE-DATE      TO AXI-NOUV-ECRITURE-DATE

           MOVE 'N'                         TO AXI-RUPT-CLI-CODE
           MOVE 'N'                         TO AXI-RUPT-COMPTE-NUM
           MOVE 'N'                         TO AXI-RUPT-ECRITURE-DATE

           IF FIN-FICHIER-R41n                               OR
              MV-CLI-CODE            NOT =   1-MV-CLI-CODE
              MOVE 'O'                      TO AXI-RUPT-CLI-CODE
              MOVE 'O'                      TO AXI-RUPT-COMPTE-NUM
              MOVE 'O'                      TO AXI-RUPT-ECRITURE-DATE
           END-IF

           IF MV-COMPTE-NUM          NOT =   1-MV-COMPTE-NUM
              MOVE 'O'                      TO AXI-RUPT-COMPTE-NUM
              MOVE 'O'                      TO AXI-RUPT-ECRITURE-DATE
           END-IF

           IF MV-ECRITURE-DATE       NOT =   1-MV-ECRITURE-DATE
              MOVE 'O'                      TO AXI-RUPT-ECRITURE-DATE
           END-IF.
       CALCUL-RUPTURE-FIN.
           EXIT.
      *
       CALCUL-SOMME.
      
           IF MV-SENS-ECRITURE = 'C'
           COMPUTE W-SOMMECPT-C = W-SOMMECPT-C + MV-MONTANT
              COMPUTE W-SOMMECLI-C = W-SOMMECLI-C + MV-MONTANT
              COMPUTE W-SOMMEDATE-C = W-SOMMEDATE-C + MV-MONTANT
           ELSE
              COMPUTE W-SOMMECLI-D = W-SOMMECLI-D + MV-MONTANT
              COMPUTE W-SOMMECPT-D = W-SOMMECPT-D + MV-MONTANT
              COMPUTE W-SOMMEDATE-D = W-SOMMEDATE-D + MV-MONTANT.
       CALCUL-SOMME-FIN.
           EXIT.
      *
       NOUVEAU-CLI.
           MOVE 0 TO W-SOMMECLI-D
           MOVE 0 TO W-SOMMECLI-C
           DISPLAY '                                      '
           DISPLAY '                                      '
           DISPLAY 'CLIENT :' MV-CLI-CODE '  Mr ' MV-COMPTE-LIBELLE 
           DISPLAY '                                      '       
           DISPLAY 
           'N° COMPTE     DATE       N° ECRITURE
      -    '          DEBIT           CREDIT'.
       NOUVEAU-CLI-FIN.
           EXIT.
      *
       NOUVEAU-CPT.
           MOVE 0 TO W-SOMMECPT-D
           MOVE 0 TO W-SOMMECPT-C
           DISPLAY '-----------------------------
      -    '---------------------------------------'.
       NOUVEAU-CPT-FIN.
           EXIT.
      *
       NOUVEAU-DATE.
           MOVE 0 TO W-SOMMEDATE-D
           MOVE 0 TO W-SOMMEDATE-C.
       NOUVEAU-DATE-FIN.
           EXIT.
      *
       RUPTURE-CLI.
           MOVE W-SOMMECLI-D TO W-TOTAL-CLIENT-D
           MOVE W-SOMMECLI-C TO W-TOTAL-CLIENT-C
           DISPLAY '         TOTAL CLIENT  :
      -    '                 ' W-TOTAL-CLIENT-D '   
      -    '   ' W-TOTAL-CLIENT-C.
       RUPTURE-CLI-FIN.
           EXIT.
      *
       RUPTURE-CPT.
           MOVE W-SOMMECPT-D TO W-TOTAL-COMPTE-D
           MOVE W-SOMMECPT-C TO W-TOTAL-COMPTE-C
           DISPLAY '         TOTAL COMPTE  :
      -    '                 ' W-TOTAL-COMPTE-D '  
      -    '    ' W-TOTAL-COMPTE-C.
       RUPTURE-CPT-FIN.
           EXIT.
      *
       RUPTURE-DATE.
           MOVE W-SOMMEDATE-D TO W-TOTAL-JOURNEE-D
           MOVE W-SOMMEDATE-C TO W-TOTAL-JOURNEE-C
           DISPLAY '         TOTAL JOURNEE :
      -    '                 ' W-TOTAL-JOURNEE-D '  
      -    '    ' W-TOTAL-JOURNEE-C.
       RUPTURE-DATE-FIN.
           EXIT.
      *
       FERMETURE-FICHIERS.
           CLOSE    MVMNTS-FILE.
       FERMETURE-FICHIERS-FIN.
           EXIT.
      *
       DISPLAY-FIN-PGM.
           DISPLAY 'FIN DU PROGRAMME: ' .
       DISPLAY-FIN-PGM-FIN.
           EXIT.
