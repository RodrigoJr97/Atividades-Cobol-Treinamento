      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GERAR-ARQUIVO.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-USUARIO ASSIGN TO
              'C:\Cobol\Atividade\bin\ARQ-USUARIO.CSV'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-USUARIO.
       01 REG-ARQ-USUARIO             PIC X(87).

       WORKING-STORAGE SECTION.
       77 WS-IOF                  PIC X(1)  VALUE SPACE.

       01 WS-ARQ-USUARIO.
           03 WS-EMAIL            PIC X(30).
           03 FILLER              PIC X VALUE ';'.
           03 WS-NAME             PIC X(30).
           03 FILLER              PIC X VALUE ';'.
           03 WS-PASSWORD         PIC X(11).
           03 FILLER              PIC X VALUE ';'.
           03 WS-PHONE            PIC 9(12).
           03 FILLER              PIC X VALUE ';'.

       LINKAGE SECTION.
       01 WS-USUARIO-RECEBIDO.
           03 WS-EMAIL-RECEBIDO            PIC X(30).
           03 WS-NAME-RECEBIDO             PIC X(30).
           03 WS-PASSWORD-RECEBIDO         PIC X(11).
           03 WS-PHONE-RECEBIDO            PIC 9(12).


       PROCEDURE DIVISION USING WS-USUARIO-RECEBIDO.
       MAIN-PROCEDURE.

            OPEN OUTPUT ARQ-USUARIO.

            MOVE 'I' TO WS-IOF

            PERFORM UNTIL WS-IOF = 'F' OR WS-IOF = 'f'

              MOVE WS-EMAIL-RECEBIDO      TO   WS-EMAIL
              MOVE WS-NAME-RECEBIDO       TO   WS-NAME
              MOVE WS-PASSWORD-RECEBIDO   TO   WS-PASSWORD
              MOVE WS-PHONE-RECEBIDO      TO   WS-PHONE

              MOVE WS-ARQ-USUARIO  TO  REG-ARQ-USUARIO

              WRITE REG-ARQ-USUARIO

              DISPLAY 'Tecle F ou f para finalizar execucao.'
              ACCEPT WS-IOF

            END-PERFORM

            CLOSE ARQ-USUARIO

            GOBACK.
       END PROGRAM GERAR-ARQUIVO.
