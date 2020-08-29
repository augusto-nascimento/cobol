       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR0A.
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
           DISPLAY "CADASTRO DE AMIGOS" LINE 02 COL 25.
           DISPLAY "APELIDO  :" LINE 06 COL 10.
           DISPLAY "NOME   :" LINE 08 COL 10.
           DISPLAY "IDADE   :" LINE 10 COL 10.
       ROT-APELIDO.
           ACCEPT APELIDO LINE 06 COL 20.
           IF APELIDO = SPACES
               GO TO ROT-APELIDO.
           ACCEPT  NOME LINE 08 COL 20.
           ACCEPT  IDADE LINE 10 COL 20.

.          STOP RUN.
