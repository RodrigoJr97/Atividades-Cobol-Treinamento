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
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ARQ-USUARIO.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-USUARIO.
       01 REG-ARQ-USUARIO             PIC X(119).

       WORKING-STORAGE SECTION.
       01 ARQUIVO-EXISTE          PIC X(1) VALUE 'N'.
       01 FILE-STATUS             PIC X(2).

       01 WS-ARQ-USUARIO.
           03 WS-ID               PIC 9(04).
           03 FILLER              PIC X VALUE ';'.
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
           03 ID-USUARIO-RECEBIDO          PIC 9(04).

       PROCEDURE DIVISION USING WS-USUARIO-RECEBIDO.
       MAIN-PROCEDURE.

             OPEN INPUT ARQ-USUARIO
             IF FILE-STATUS = '00'
               MOVE 'S' TO ARQUIVO-EXISTE
             END-IF
             CLOSE ARQ-USUARIO

             IF ARQUIVO-EXISTE = 'S'
              OPEN EXTEND ARQ-USUARIO
              MOVE 0 TO ID-USUARIO-RECEBIDO
              READ ARQ-USUARIO
                AT END
                   CONTINUE
                NOT AT END
                   MOVE ID-USUARIO-RECEBIDO TO WS-ID
                END-READ
             ELSE
              OPEN OUTPUT ARQ-USUARIO
              MOVE 1 TO ID-USUARIO-RECEBIDO
            END-IF


              MOVE ID-USUARIO-RECEBIDO    TO   WS-ID
              MOVE WS-EMAIL-RECEBIDO      TO   WS-EMAIL
              MOVE WS-NAME-RECEBIDO       TO   WS-NAME
              MOVE WS-PASSWORD-RECEBIDO   TO   WS-PASSWORD
              MOVE WS-PHONE-RECEBIDO      TO   WS-PHONE

              MOVE WS-ARQ-USUARIO  TO  REG-ARQ-USUARIO
              WRITE REG-ARQ-USUARIO


            CLOSE ARQ-USUARIO

            GOBACK.
       END PROGRAM GERAR-ARQUIVO.
