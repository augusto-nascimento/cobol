       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP001.
       AUTHOR. ANGELO LOTIERZO FILHO.
      ********************************************************
      * MANUTENCAO DO CADASTRO DE MEDICOS  *
      * DATA CRIACAO : 27/07/2020                                *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                    SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01 REGMED.
            03 CRM            PIC 9(06).
            03 NOME         PIC X(30).
            03 ESPEC       PIC 9(02).
            03 SEXO         PIC X(01).
            03 DATANASC.
                05 DIANASC         PIC 99.
                05 MESNASC       PIC 99.
                05 ANONASC       PIC 9(04).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT   PIC 9(06) VALUE ZEROS.
       01 W-MENS   PIC X(50) VALUE SPACES.
       01 W-LIMPA   PIC X(50) VALUE SPACES.
       01 W-SEL         PIC X(01) VALUE SPACES.
       01 W-OPCAO   PIC X(01) VALUE SPACES.
       01 ST-ERRO     PIC X(02) VALUE "00".
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 TXSEXO      PIC X(12) VALUE SPACES.
       01 TXESPEC    PIC X(20) VALUE SPACES.
       01 MENSAGEM.
            03 MENSAGEM1   PIC X(30) VALUE SPACES.
            03 CODIGO           PIC 9(02) VALUE ZEROS.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT001.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                         *** CADASTRO DE".
           05  LINE 03  COLUMN 41 
               VALUE  " MEDICOS ***".
           05  LINE 06  COLUMN 01 
               VALUE  "     CRM             :".
           05  LINE 08  COLUMN 01 
               VALUE  "     NOME            :".
           05  LINE 10  COLUMN 01 
               VALUE  "     ESPECIALIDADE   :".
           05  LINE 12  COLUMN 01 
               VALUE  "     SEXO            :".
           05  LINE 14  COLUMN 01 
               VALUE  "     DATA NASCIMENTO :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCRM
               LINE 06  COLUMN 24  PIC Z99.999
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 08  COLUMN 24  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TESPEC
               LINE 10  COLUMN 24  PIC 99
               USING  ESPEC.
           05  TTXESPC
               LINE 10  COLUMN 27  PIC X(20)
               USING  TXESPEC
               HIGHLIGHT.
           05  TSEXO
               LINE 12  COLUMN 24  PIC X(01)
               USING  SEXO.
           05  TTXSEXO
               LINE 12  COLUMN 26  PIC X(12)
               USING  TXSEXO
               HIGHLIGHT.
           05  TDATANASC
               LINE 14  COLUMN 24  PIC 99.99.9999
               USING  DATANASC
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *------------[ ROTINA DE ABERTURA ARQUIVO ]------------------
       ABRIR-ARQ.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "30"
                    OPEN OUTPUT CADMED
                    CLOSE CADMED
                    GO TO ABRIR-ARQ
                ELSE
                     MOVE "ERRO NA ABERTURA ARQUIVO = " TO MENSAGEM1 
                     MOVE ST-ERRO TO CODIGO
                     MOVE MENSAGEM  TO W-MENS
                     PERFORM ROT-MENS THRU ROT-MENS2
                    GO TO ROT-FIM.
      *--------------------[ INICIALIAR VARIAVEIS ]-----------------
       ROT-INIC.
           MOVE 0 TO CRM DATANASC ESPEC DIANASC
                               MESNASC ANONASC.
           MOVE SPACES TO NOME SEXO TXSEXO TXESPEC.
      *--------------------[ VISUALIZAR TELA INICIAL ]-----------------
       R0.
           DISPLAY SMT001.
      *--------------------[ DIGITAR CAMPO CHAVE  ]-----------------
       R1.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   CLOSE CADMED
                   GO TO ROT-FIM.
           IF CRM = ZEROS
                MOVE "CRM NAO PODE SER ZERO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R1.
      *--------------------[ LER CAMPO  CHAVE  ]-----------------
       LER-ARQ. 
                READ CADMED
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM R3A
                      PERFORM R4A
                      DISPLAY SMT001
                      MOVE "*** MEDICO JA CADASTRAD0 ***" TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS2
                      MOVE "I" TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ CADNOTAS"   TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS2
                      GO TO ROT-FIM
                ELSE
                      MOVE "*** MEDICO NAO ESTA CADASTRAD0 ***" 
                                                        TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS2.
       R2.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.
           IF NOME = SPACES
                MOVE "NOME NAO PODE FICAR EM BRANCO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.
       R3.
           ACCEPT TESPEC.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
       R3A.
              IF ESPEC = 01  MOVE "CLINICA MEDICA" TO TXESPEC
                 ELSE
                   IF ESPEC = 02  MOVE "UROLOGIA " TO TXESPEC 
                       ELSE
                       IF ESPEC = 03  MOVE "GINICOLOGIA " TO TXESPEC
                         ELSE
                           MOVE "ESPECIALIDADE NAO EXISTE" TO W-MENS
                            PERFORM ROT-MENS THRU ROT-MENS2
                            GO TO R3.
           DISPLAY TTXESPC.
       R4.
           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
         R4A.
           IF SEXO = "M"    MOVE "MASCULINO" TO TXSEXO
           ELSE
              IF SEXO = "F"  MOVE "FEMENINO " TO TXSEXO 
                     ELSE
                        MOVE "SEXO  => M = MASCULINO    F=FEMENINO"
                                                             TO W-MENS
                        PERFORM ROT-MENS THRU ROT-MENS2
                       GO TO R4.
           DISPLAY TTXSEXO.
       R5.
           ACCEPT TDATANASC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF DATANASC = ZEROS
                        MOVE "DATA NASC. NAO PODE SER ZEROS" TO W-MENS
                        PERFORM ROT-MENS THRU ROT-MENS2
                        GO TO R5.
           IF W-SEL = "A" 
                    GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "GRAVAR (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO USUARIO ***" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGMED
                IF ST-ERRO = "00" OR "02"
                      MOVE "** DADOS GRAVADOS COM SUCESSO **" TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-INIC.
                IF ST-ERRO = "22"
                      MOVE "*** NOTA JA EXISTENTE ***       " TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-INIC
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE MEDICO"
                                                       TO W-MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *--------------------[ FIM DA ROTINA DE INCLUSAO ]--------------------
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 13)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                                   AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO W-MENS
                DISPLAY (23, 12) W-MENS
                IF W-OPCAO = "A"
                       MOVE "A" TO W-SEL
                       GO TO R2.
                IF W-OPCAO  = "N"
                         GO TO ROT-INIC.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO " TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* INFORMACOES NAO ALTERADAS * " TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO W-MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-INIC.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO MEDICO" TO W-MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *--------------------[ ROTINA DE MENSAGEM ]--------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) W-MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO W-MENS
                   DISPLAY (23, 12) W-MENS.
       ROT-MENS-FIM.
                EXIT.

       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.
       ROT-FIMS.
                STOP RUN.

      *        TABELA DE ST-ERRO  (FILE STATUS)
      *        ==============================
      * 00 Comando executado com sucesso 
      * 02 Comando arquivo c/chave duplicada executado com sucesso 
      * 10 Fim do arquivo 
      * 21 Erro de sequencia na gravação de arquivo sequencial 
      * 22 Chave duplicada não definida.  
      * 23 Chave não encontrada. 
      * 24 Espaço em disco está cheio 
      * 30 Arquivo não encontrado 
      * 91 Erro na estrutura do arquivo 
      * 94 Registro ou arquivo sendo utilizado por outro programa.
      * 95 GERENCIADOR DE ARQUIVO NAO CARREGADO (ISAM) 





