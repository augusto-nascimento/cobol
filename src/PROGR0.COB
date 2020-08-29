       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR0.
      *AUTOR. ANGELO LOTIERZO FILHO.
      *******************************************
      * MANUTENCAO DO PROGRAMA 0  *
      *******************************************
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
       01 APELIDO     PIC X(12) VALUE "ANGELO".
       01 NOME          PIC X(30) VALUE SPACES.
       01 IDADE          PIC 9(03) VALUE ZEROS.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           MOVE "ANGELO LOTIERZO FILHO" TO NOME
           DISPLAY APELIDO
           DISPLAY "OLA MUNDO"   "-----    " NOME.

.          STOP RUN.
