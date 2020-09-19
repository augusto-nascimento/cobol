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
       01 NOME           PIC X(30) VALUE SPACES.
       01 IDADE          PIC 9(02) VALUE ZEROS.
       01 EMAIL.
            02 EMAIL1    PIC X(15) VALUE SPACES.
            02 EMAIL2    PIC X(15) VALUE SPACES.
       01 DATANASC.
            03 DIA       PIC 99.
            03 MES       PIC 9(02).
            03 ANO.
                05 MIL   PIC 9.
                05 SEC   PIC 9.
                05 DEC   PIC 99.
        01 DATANASC1     PIC 9(08).
        77 MASCARA       PIC 99/99/9999.
        77 MASCAR1       PIC *9/99/9999.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           MOVE "ANGELO LOTIERZO FILHO" TO NOME
           MOVE 64 TO IDADE.

       LABEL1.
           DISPLAY "NOME  : "       LINE 03 COL 10
           DISPLAY  NOME            LINE 03 COL 18
           DISPLAY "IDADE : "       LINE 05 COL 10
           DISPLAY  IDADE           LINE 05 COL 18
           DISPLAY "EMAIL : "       LINE 07 COL 10
           ACCEPT   EMAIL           LINE 07 COL 18.
           DISPLAY "EMAIL : "       LINE 08 COL 10
           DISPLAY  EMAIL           LINE 08 COL 18
           DISPLAY "DATA  : "       LINE 10 COL 10
           ACCEPT DATANASC          LINE 10 COL 18.
           DISPLAY DIA              LINE 11 COL 10
           DISPLAY "/"              LINE 11 COL 12
           DISPLAY MES              LINE 11 COL 13
           DISPLAY "/"              LINE 11 COL 15
           DISPLAY ANO              LINE 11 COL 16
           MOVE DATANASC  TO DATANASC1
           MOVE DATANASC1 TO MASCAR1 MASCARA
           DISPLAY MASCARA          LINE 12 COL 10
           DISPLAY "       "        LINE 12 COL 20
           DISPLAY MASCAR1          LINE 12 COL 27
      *-----ACCEPT para podermos ver a mascara antes do encerramento do programa
           ACCEPT NOME              LINE 13 COL 1.
.          STOP RUN.
