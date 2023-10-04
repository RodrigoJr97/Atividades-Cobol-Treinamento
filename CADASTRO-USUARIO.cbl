      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADASTRO-USUARIO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-USUARIO.
           03 WS-EMAIL            PIC X(30).
           03 WS-NAME             PIC X(30).
           03 WS-PASSWORD         PIC X(11).
           03 WS-PHONE            PIC 9(12).

           03 WS-COUNT            PIC 9(02).
           03 WS-DOMINIO          PIC X(20).
           03 WS-DOMINIO-AUX      PIC X(20).
           03 WS-PRIMEIRO-NOME    PIC X(20).
           03 WS-SEGUNDO-NOME     PIC X(20).
           03 WS-INDICE           PIC 9(02) VALUE 1.
           03 WS-CHARACTER        PIC X(1).
           03 STATUS-VALIDACAO    PIC 9(1)  VALUE 1.

           03 WS-AUX                PIC 9(02).
           03 WS-AUX-LOWER          PIC 9(02).
           03 WS-AUX-UPPER          PIC 9(02).
           03 WS-AUX-NUMERIC        PIC 9(02).

           03  N                    PIC 99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY 'Cadastro Iniciado'

            INITIALISE WS-USUARIO.

            MOVE 'teste@capgemini.com'       TO WS-EMAIL.
            MOVE 'Bruce Waynne'              TO WS-NAME.
            MOVE '12345@Ab'                  TO WS-PASSWORD.
            MOVE 32912341234                 TO WS-PHONE.

            CALL 'VALIDA-DADOS' USING  WS-USUARIO.
            IF STATUS-VALIDACAO = 0
                CALL 'GERAR-ARQUIVO' USING  WS-USUARIO
            END-IF



            STOP RUN.
       END PROGRAM CADASTRO-USUARIO.
