      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA-DADOS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 WS-VALIDACAO-USUARIO.
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
           03 STATUS-VALIDACAO    PIC 9(1)  VALUE ZERO.

           03 WS-AUX                PIC 9(02).
           03 WS-AUX-LOWER          PIC 9(02).
           03 WS-AUX-UPPER          PIC 9(02).
           03 WS-AUX-NUMERIC        PIC 9(02).
           03 WS-AUX-ESPECIAL       PIC 9(02).
           03 N                     PIC 99.

       PROCEDURE DIVISION USING WS-VALIDACAO-USUARIO.
       MAIN-PROCEDURE.

            PERFORM P101-VALIDA-EMAIL.
            PERFORM P102-VALIDA-NOME.
            PERFORM P103-VALIDA-SENHA.
            PERFORM P104-VALIDA-TEL.

            DISPLAY " "
            DISPLAY "--- Validacao de dados Chamado ---"
            DISPLAY " "
            DISPLAY 'WS-EMAIL: '    WS-EMAIL
            DISPLAY 'WS-NAME: '     WS-NAME
            DISPLAY 'WS-PASSWORD: ' WS-PASSWORD
            DISPLAY 'WS-PHONE: '    WS-PHONE

            PERFORM P900-FINALIZA.

       P101-VALIDA-EMAIL.
      * Email é válido (ter pelo menos 10 caracteres, ter pelo menos um
      * caractere antes do @ e pertencer aos domínios “capgemini.com” ou
      * “bradesco.com”

            INSPECT WS-EMAIL TALLYING WS-COUNT
                FOR CHARACTERS BEFORE SPACE

            DISPLAY "Tamanho email: " WS-COUNT

      ************ Valida - email com pelo menos 10 caracteres  ********
            IF WS-COUNT >= 10

      ****************     Valida - somente um @      ******************
                INITIALISE WS-COUNT
                MOVE 0 TO STATUS-VALIDACAO

                INSPECT WS-EMAIL TALLYING WS-COUNT FOR ALL '@'

                IF WS-COUNT <> 1
                 DISPLAY 'ERRO Email: Somente um @.'
                 PERFORM P000-ERRO
                END-IF

                DISPLAY 'count do @: ' WS-COUNT

      *************** Valida - pelo menos um caracter antes do @ **************
                INITIALISE WS-COUNT
                MOVE 0 TO STATUS-VALIDACAO
                INSPECT WS-EMAIL TALLYING WS-COUNT
                                         FOR CHARACTERS BEFORE '@'

                IF WS-COUNT < 1
                  DISPLAY 'ERRO Email: Minimo um caracter antes do @.'
                  PERFORM P000-ERRO
                END-IF

                DISPLAY 'count antes do @: ' WS-COUNT

      *****************        Valida - dominio         ****************
                MOVE 0 TO STATUS-VALIDACAO

                UNSTRING WS-EMAIL DELIMITED BY '@'
                   INTO WS-DOMINIO-AUX WS-DOMINIO
                END-UNSTRING

                IF WS-DOMINIO <> 'capgemini.com' AND
                    WS-DOMINIO <> 'bradesco.com'
                    DISPLAY
                        'ERRO - Dominios: capgemini.com ou bradesco.com'
                    PERFORM P000-ERRO
                END-IF

                DISPLAY 'DOMINIO: ' WS-DOMINIO

            ELSE
                DISPLAY 'Tamanho minimo do email 10 caracteres'
                PERFORM P000-ERRO
            END-IF
            .
       P101-FIM.

       P102-VALIDA-NOME.
      * Nome com pelo menos duas palavras
            MOVE 0 TO STATUS-VALIDACAO

            UNSTRING WS-NAME DELIMITED BY SPACE
               INTO WS-PRIMEIRO-NOME
                    WS-SEGUNDO-NOME
            END-UNSTRING

            IF WS-SEGUNDO-NOME = SPACES
                DISPLAY 'ERRO Nome: Deve conter pelo menos um Sobrenome'
                PERFORM P000-ERRO
            END-IF

            DISPLAY 'Primeiro nome: ' WS-PRIMEIRO-NOME
            DISPLAY 'Segundo nome: ' WS-SEGUNDO-NOME
            .
       P102-FIM.

       P103-VALIDA-SENHA.
      * Senha com pelo menos 8 caracteres, 1 número, 1 letra maiúscula,
      * 1 letra minúscula e um caractere especial

            MOVE 0 TO WS-COUNT
                      WS-AUX
                      WS-AUX-LOWER
                      WS-AUX-UPPER
                      WS-AUX-NUMERIC

            MOVE 0 TO STATUS-VALIDACAO


            INSPECT WS-PASSWORD TALLYING WS-AUX
                    FOR CHARACTERS BEFORE SPACE.

            PERFORM VARYING N FROM 1 BY 1
                    UNTIL N > WS-AUX

              EVALUATE TRUE

                WHEN WS-PASSWORD(N:1) = SPACE
                     CONTINUE

                WHEN WS-PASSWORD(N:1) IS ALPHABETIC-UPPER
                     ADD 1 TO WS-AUX-UPPER

                WHEN WS-PASSWORD(N:1) IS ALPHABETIC-LOWER
                     ADD 1 TO WS-AUX-LOWER

                WHEN WS-PASSWORD(N:1) IS NUMERIC
                     ADD 1 TO WS-AUX-NUMERIC

                WHEN OTHER
                   ADD 1 TO WS-AUX-ESPECIAL

              END-EVALUATE
            END-PERFORM.


             IF WS-AUX >= 8 AND WS-AUX-UPPER > 0
                                 AND WS-AUX-LOWER > 0
                                 AND WS-AUX-NUMERIC > 0
                                 AND WS-AUX-ESPECIAL > 0


               DISPLAY 'SENHA VALIDA!'

             ELSE
               DISPLAY 'WS-AUX: ' WS-AUX
               DISPLAY 'WS-AUX-UPPER: ' WS-AUX-UPPER
               DISPLAY 'WS-AUX-LOWER: ' WS-AUX-LOWER
               DISPLAY 'WS-AUX-NUMERIC: ' WS-AUX-NUMERIC
               DISPLAY 'WS-AUX-NUMERIC: ' WS-AUX-ESPECIAL

               DISPLAY 'Senha deve ter: '
                        'Minimo 8 Caractetes '
                        ' 1-Numero '
                        ' 1-Letra Maiuscula '
                        ' 1-Letra Minuscula '
                        ' 1-Caractere Especial '
               PERFORM P000-ERRO


            DISPLAY "Tamanho senha: " WS-AUX
            .
       P103-FIM.

       P104-VALIDA-TEL.
      * Telefone com no mínimo 11 caracteres e máximo 12 caracteres.
            MOVE 0 TO STATUS-VALIDACAO
                      WS-COUNT
                      WS-AUX

            INSPECT WS-PHONE TALLYING WS-COUNT
                    FOR CHARACTERS AFTER ZEROES

            MOVE WS-PHONE(2:1)  TO  WS-AUX
            DISPLAY 'VALOR AUX: ' WS-AUX

            IF WS-COUNT < 11 OR WS-COUNT > 12 OR WS-AUX = 0
                DISPLAY
                'Telefone deve ter minimo de 11 e maximo 12 caracteres'

                DISPLAY 'COUNT IF: ' WS-COUNT
                DISPLAY 'TELEFONE IF: ' WS-PHONE
                 PERFORM P000-ERRO
            END-IF

            DISPLAY 'Tamanho telefone: ' WS-COUNT

            .
       P104-FIM.

       P000-ERRO.
            MOVE 1 TO STATUS-VALIDACAO
            PERFORM P900-FINALIZA
            .
       P900-FINALIZA.
            GOBACK.
       END PROGRAM VALIDA-DADOS.
