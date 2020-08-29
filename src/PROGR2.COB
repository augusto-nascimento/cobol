       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR2.
      *AUTHOR. ANGELO LOTIERZO FILHO.
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
       01 NOME          PIC X(30) VALUE SPACES.
       01 IDADE          PIC 9(02) VALUE ZEROS.
       01 DATANASC.
            03 DIANASC       PIC 99.
            03 MESNASC     PIC 99.
            03 ANONASC. 
                 05 SECNASC PIC 99.
                 05 DECNASC PIC 99.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           MOVE 0 TO DATANASC
           DISPLAY (10, 20) "NOME : ".
           DISPLAY (12, 20) "IDADE : ".
           DISPLAY (13, 20) "DATA NASC. : ".
       R1.
           ACCEPT (10, 27) NOME.
           IF NOME = SPACES
                   GO TO R1.
       R2.
           ACCEPT (12, 28) IDADE.
           IF IDADE > 70
                MOVE ZEROS TO IDADE
                DISPLAY (12, 28) IDADE
                GO TO R2.
           ACCEPT (13, 34) DATANASC.
           DISPLAY (15, 20) SECNASC.
           STOP RUN.