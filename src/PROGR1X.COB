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
       01 NOME          PIC X(30) VALUE "ANGELO LOTIERZO FILHO".
       01 IDADE          PIC 9(02) 64.
       01 EMAIL          PIC X(30) VALUE SPACES.
       01 DATANASC.
            03 DIA          PIC 99.
            03 MES        PIC 9(02).
            03 ANO.
                05 MIL      PIC 9.
                05 SEC    PIC 9.
                05 DEC    PIC 99.
        01 DATANASC1 PIC 9(08).
        01 MASCARA PIC 99/99/9999.
        01 MASCAR1 PIC *9/99/9999.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           MOVE 1 TO LIN
           MOVE 10 TO COL.
       ROT1.
           IF LIN < 20
                DISPLAY (LIN, COL) NOME "   " IDADE
                ADD 1 TO LIN
                ADD 1 TO COL
                STOP "    "
                GO TO ROT1.
          

.          STOP RUN.
