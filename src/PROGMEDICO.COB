       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGMEDICO.
       AUTHOR. AUGUSTO ROCHA NASCIMENTO.

      *----------ENVIRONMENT DIVISION--------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMEDICO ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CRM
           FILE STATUS IS ST-ERRO
           ALTERNATE RECORD KEY IS NOME.
      *-----------DATA DIVISION--------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMEDICO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "REGMEDICO.DAT".

       01 REGMEDICO.
           02 CRM PIC 9(06).
           02 NOME PIC X(30).
           02 ESPECIALIDADE PIC 9(01).
           02 SEXO PIC X(01).
           02 DATA-NASCIMENTO PIC 9(08).
           02 EMAIL PIC X(50).
           02 DDD PIC 9(02).
           02 PHONE PIC 9(09).

      *
      *----------WORKING-STORAGE SECTION-----------
       WORKING-STORAGE SECTION.
       01 WS-DATA-NASCIMENTO.
           02 WS-DIA PIC 9(02) VALUE ZEROS.
           02 WS-MES PIC 9(02) VALUE ZEROS.
           02 WS-ANO PIC 9(04) VALUE ZEROS.

       01 WS-COUNT PIC 9(02) VALUE ZEROS.
       77 WS-LINE PIC 9(02) VALUE ZEROS.
       77 WS-CONCAT PIC X(100) VALUE SPACES.
       77 WS-CLEAR PIC X(100) VALUE SPACES.
       77 WS-OPT PIC X(01) VALUE SPACES.
       77 WS-OPR PIC X(01) VALUE SPACES.
      * WS-OPR:
      * L - LOCALIZAR REGISTRO
      * N - NOVO REGISTRO
      * A - APAGAR EXCLUIR REGISTRO
      * E - EDITAR REGISTRO
       77 WS-ANY PIC X(01) VALUE SPACES.
       01 WS-MSG PIC X(100) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".

       01 TABESPECIALIDADE.
           02 ESPECIALIDADE-PONTEIRO PIC X(20) OCCURS 5 TIMES.

      *-----------SCREEN SECTION-------------------
       SCREEN SECTION.
       01 TELA1.
           02 BLANK SCREEN.
           02 LINE 01 COLUMN 30 VALUE "--------------------------".
           02 LINE 02 COLUMN 30 VALUE "|   CADASTRO DE MEDICOS   |".
           02 LINE 03 COLUMN 30 VALUE "--------------------------".
           02 LINE 05 COLUMN 01 VALUE "CRM:".
           02 LINE 07 COLUMN 01 VALUE "NOME:".
           02 LINE 09 COLUMN 01 VALUE "ESPECIALIDADE:".
           02 LINE 11 COLUMN 01 VALUE "SEXO:".
           02 LINE 13 COLUMN 01 VALUE "DATA DE NASCIMENTO:   /  /    ".
           02 LINE 15 COLUMN 01 VALUE "E-MAIL:".
           02 LINE 17 COLUMN 01 VALUE "TELEFONE:".
           02 LINE 18 COLUMN 04 VALUE "DDD:".
           02 LINE 18 COLUMN 12 VALUE "NUMERO:".
           02 T-CRM LINE 05 COLUMN 20 PIC 9(06) USING CRM HIGHLIGHT.
           02 T-NOME LINE 07 COLUMN 20 PIC X(30) USING NOME.
           02 T-ESPECIALIDADE LINE 09 COLUMN 20 PIC 9(01) 
               USING ESPECIALIDADE.
           02 T-SEXO LINE 11 COLUMN 20 PIC X(01) USING SEXO.
           02 T-DIA LINE 13 COLUMN 21 PIC 9(02) USING WS-DIA.
           02 T-MES LINE 13 COLUMN 24 PIC 9(02) USING WS-MES.
           02 T-ANO LINE 13 COLUMN 27 PIC 9(04) USING WS-ANO.
           02 T-EMAIL LINE 15 COLUMN 20 PIC X(50) USING EMAIL.
           02 T-DDD LINE 18 COLUMN 09 PIC 9(02) USING DDD.
           02 T-PHONE LINE 18 COLUMN 20 PIC 9(09) USING PHONE.
       
       01 TELA-ABERTURA.
           02 BLANK SCREEN.
           02 LINE 01 COLUMN 30 VALUE "--------------------------".
           02 LINE 02 COLUMN 30 VALUE "|   CADASTRO DE MEDICOS   |".
           02 LINE 03 COLUMN 30 VALUE "--------------------------".
           02 LINE 05 COLUMN 01 VALUE "O QUE DESEJA FAZER?".
           02 LINE 06 COLUMN 01 VALUE "(N)OVO REGISTRO".
           02 LINE 07 COLUMN 01 VALUE "(L)OCALIZAR REGISTRO".
           02 LINE 08 COLUMN 01 VALUE "(S)AIR".


      *--------------------------------------------
       PROCEDURE DIVISION.
       SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'.

       INICIO.

           OPEN I-O CADMEDICO
           IF ST-ERRO NOT = "00" THEN
               IF ST-ERRO = "35" THEN
                   OPEN OUTPUT CADMEDICO
                   CLOSE CADMEDICO
                   MOVE "*** ARQUIVO CADMEDICO SENDO CRIADO **"
                       TO WS-MSG
                   PERFORM SHOW-MSG
                   GO TO INICIO
               ELSE
                   MOVE "*** ERRO NA ABERTURA DO ARQUIVO **"
                       TO WS-MSG
                   PERFORM SHOW-MSG
                   GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.
            
           PERFORM CRIA-TABELA-ESPECIALIADES.

       R-ABERTURA.
           DISPLAY TELA-ABERTURA.
           MOVE SPACES TO NOME SEXO EMAIL
           MOVE ZEROS TO CRM ESPECIALIDADE DATA-NASCIMENTO DDD PHONE

           ACCEPT WS-OPR LINE 09 COLUMN 01.
           EVALUATE WS-OPR
               WHEN "L"
               WHEN "l"
                   MOVE "L" TO WS-OPR
                   GO TO R-LOCALIZAR
               WHEN "N"
               WHEN "n"
                   MOVE "N" TO WS-OPR
                   GO TO R-NOVO
               WHEN "S"
               WHEN "s"
                   MOVE "S" TO WS-OPR
                   GO TO ROT-FIM                 
               WHEN OTHER
                   MOVE "OPCAO INVALIDA DIGITE QUALQUER TECLA PARA CONTI
      -            "NUAR" TO WS-MSG
                   PERFORM SHOW-MSG
                   ACCEPT WS-ANY
                   GO TO R-ABERTURA
           END-EVALUATE.

       R-LOCALIZAR.
           DISPLAY TELA1.
           ACCEPT T-CRM.
           READ CADMEDICO.
           EVALUATE ST-ERRO
               WHEN "00"
                   DISPLAY TELA1
                   GO TO R-MODIFICAR-REGISTRO
               WHEN "23"
                   MOVE "REGISTRO NAO ENCONTRADO PRESSIONE QUALQUER TECL
      -            "A PARA CONTINUAR" TO WS-MSG
                   PERFORM SHOW-MSG
                   ACCEPT WS-ANY
                   GO TO R-ABERTURA
               WHEN other
                   DISPLAY ST-ERRO
           END-EVALUATE.

       R-MODIFICAR-REGISTRO.
           MOVE "REGISTRO ENCONTRADO DESEJA (E)DITAR, (A)PAGAR OU (C)ANC
      -         "ELAR ?"
      -         TO WS-MSG
           PERFORM SHOW-MSG
           ACCEPT WS-OPR LINE 24 COLUMN 01
           EVALUATE WS-OPR
               WHEN "E"
               WHEN "e"
                   MOVE "E" TO WS-OPR
                   DISPLAY TELA1
                   GO TO R-NOME
               WHEN "A"
               WHEN "a"
                   MOVE "A" TO WS-OPR
                   DELETE CADMEDICO
                   GO TO R-ABERTURA               
               WHEN "C"
               WHEN "c"
                   MOVE "C" TO WS-OPR
                   GO TO R-ABERTURA
               WHEN other
                   GO TO R-MODIFICAR-REGISTRO
           END-EVALUATE.

       R-NOVO.
           DISPLAY TELA1.
           ACCEPT T-CRM.
           IF cob-crt-status = 2005
              GO TO R-ABERTURA.

           READ CADMEDICO.
           EVALUATE ST-ERRO
               WHEN "00"
                   MOVE "O CRM INFORMADO JA FOI CADASTRADO SISTEMA
      -            "PRESSIONE QUALQUER TECLA PARA CONTINUAR"
      -            TO WS-MSG
                   PERFORM SHOW-MSG
                   ACCEPT WS-ANY
                   GO TO R-ABERTURA
               WHEN "23"
                   MOVE "CADASTRO DE NOVO MEDICO"
                   TO WS-MSG
                   PERFORM SHOW-MSG
               WHEN other
                   DISPLAY ST-ERRO
           END-EVALUATE.


       R-NOME.
           ACCEPT T-NOME.
           IF cob-crt-status = 2005
              GO TO R-NOVO.


       R-ESPECIALIDADE.
           MOVE 0 TO WS-COUNT.
           MOVE 5 TO WS-LINE.
           PERFORM EXIBE-TABELA-ESPECIALIDADE UNTIL WS-COUNT = 5.
           ACCEPT T-ESPECIALIDADE.
           IF cob-crt-status = 2005
              GO TO R-NOME.
           IF ESPECIALIDADE >= 1 AND ESPECIALIDADE <= 5 THEN
               DISPLAY ESPECIALIDADE-PONTEIRO(ESPECIALIDADE)
                   LINE 09 COLUMN 30
           ELSE
               MOVE "NUMERO DE ESPECIALIDADE INVALIDO. CONSULTE TABELA A
      -        "O LADO" TO WS-MSG
               PERFORM SHOW-MSG
               GO TO R-ESPECIALIDADE
           END-IF.

           
       R-SEXO.
           ACCEPT T-SEXO.
           IF cob-crt-status = 2005
              GO TO R-ESPECIALIDADE.
           EVALUATE SEXO
               WHEN "M"
               WHEN "m"
                   MOVE "M" TO SEXO
               WHEN "F"
               WHEN "f"
                   MOVE "F" TO SEXO
               WHEN OTHER
                   MOVE "ERRO - DIGITE (M)ASCULINO OU (F)EMININO"
                   TO WS-MSG
                   PERFORM SHOW-MSG
                   GO TO R-SEXO
           END-EVALUATE.
           
       R-DATA-NASCIMENTO.
           ACCEPT T-DIA.
           ACCEPT T-MES.
           ACCEPT T-ANO.
           IF cob-crt-status = 2005
              GO TO R-SEXO.
           
           IF NOT WS-DIA >= 01 OR NOT WS-DIA <= 30 THEN
               MOVE "FORMATO DE DATA INVALIDO" TO WS-MSG
               PERFORM SHOW-MSG
               GO TO R-DATA-NASCIMENTO.

           IF NOT WS-MES >= 01 OR NOT WS-MES <= 12 THEN
               MOVE "FORMATO DE DATA INVALIDO" TO WS-MSG
               PERFORM SHOW-MSG
               GO TO R-DATA-NASCIMENTO.

           IF NOT WS-ANO >= 1900 THEN
               MOVE "FORMATO DE DATA INVALIDO" TO WS-MSG
               PERFORM SHOW-MSG
               GO TO R-DATA-NASCIMENTO.

           MOVE WS-DATA-NASCIMENTO TO DATA-NASCIMENTO.

           PERFORM CLEAR-MSG.

       R-EMAIL.
           ACCEPT T-EMAIL.
           IF cob-crt-status = 2005
              GO TO R-DATA-NASCIMENTO.
           
       R-DDD.
           ACCEPT T-DDD.
           IF cob-crt-status = 2005
              GO TO R-EMAIL.
           
       R-PHONE.
           ACCEPT T-PHONE.
           IF cob-crt-status = 2005
              GO TO R-DDD.  

       R-INC-OPC.
           MOVE "DADOS OK (S/N) ?: " TO WS-MSG.
           PERFORM SHOW-MSG.
           ACCEPT WS-OPT LINE 24 COLUMN 01.
           IF WS-OPT = "S" OR "s" THEN
               IF WS-OPR = "N" THEN
                   WRITE REGMEDICO
                   GO TO R-ABERTURA
               END-IF
               IF WS-OPR = "E" THEN
                   REWRITE REGMEDICO
                   GO TO R-ABERTURA
               END-IF
           END-IF.

      *
      **********************
      * CARGA DE TABELAS *
      **********************
      *
       CRIA-TABELA-ESPECIALIADES.
           MOVE "CLINICA MEDICA" TO ESPECIALIDADE-PONTEIRO(1).
           MOVE "UROLOGIA" TO ESPECIALIDADE-PONTEIRO(2).
           MOVE "GINICOLOGISTA" TO ESPECIALIDADE-PONTEIRO(3).
           MOVE "PEDIATRIA" TO ESPECIALIDADE-PONTEIRO(4).
           MOVE "CARDIOLOGISTA" TO ESPECIALIDADE-PONTEIRO(5).

       EXIBE-TABELA-ESPECIALIDADE.
           ADD 1 TO WS-COUNT.
           ADD 1 TO WS-LINE
           DISPLAY WS-COUNT LINE WS-LINE COLUMN 70
           DISPLAY " - " LINE WS-LINE COLUMN 75
           DISPLAY ESPECIALIDADE-PONTEIRO(WS-COUNT)
               LINE WS-LINE COLUMN 80.
       

      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       SHOW-MSG.
           DISPLAY WS-MSG AT LINE 23 COLUMN 01.
       
       CLEAR-MSG.
           DISPLAY WS-CLEAR AT LINE 23 COLUMN 01.

      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
      *    DISPLAY ERASE AT 0101
           CLOSE CADMEDICO
           EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
