       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR1.
       AUTHOR. ANGELO LOTIERZO FILHO.
      **************************************
      * MANUTENCAO DO XXXXXXXX  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 APELIDO     PIC X(12) VALUE SPACES.
       01 NOME          PIC X(30) VALUE SPACES.
       01 EMAIL          PIC X(30) VALUE SPACES.
       01 DATANASC.
            03 DIA          PIC 99.
            03 MES        PIC 9(02).
            03 ANO.
                05 MIL      PIC 9.
                05 SEC    PIC 9.
                05 DEC    PIC 99.
        01 TELEFONE.
              02 DDD     PIC 99.
              02 NUMT  PIC 9(09).
        01 FACEBOOK PIC X(40).
        01 DATANASC1 PIC 9(08).
        01 MASCARA PIC 99/99/9999.

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAXX.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                           CADASTRO DE A".
           05  LINE 02  COLUMN 41 
               VALUE  "MIGOS".
           05  LINE 05  COLUMN 01 
               VALUE  "    APELIDO         :".
           05  LINE 07  COLUMN 01 
               VALUE  "    NOME            :".
           05  LINE 09  COLUMN 01 
               VALUE  "    EMAIL           :".
           05  LINE 11  COLUMN 01 
               VALUE  "    DATA NASCIMENTO :".
           05  LINE 13  COLUMN 01 
               VALUE  "    TELEFONE        : (  )".
           05  LINE 15  COLUMN 01 
               VALUE  "    FACEBOOK        :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           DISPLAY TELAXX.
           ACCEPT (05, 23) APELIDO
           ACCEPT (07, 23) NOME
           ACCEPT (09, 23) EMAIL
           ACCEPT (11, 23) DATANASC
           MOVE DATANASC TO DATANASC1
           MOVE DATANASC1 TO MASCARA
           DISPLAY (11, 23) MASCARA.
           ACCEPT (13, 24) DDD
           ACCEPT (13, 27) NUMT
           ACCEPT (15, 23) FACEBOOK
           STOP RUN.
