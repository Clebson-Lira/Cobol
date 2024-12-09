IDENTIFICATION DIVISION.
       PROGRAM-ID. CAIXA-ELETRONICO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SALDO PIC 9(5)V99 VALUE 0.
       01 OPCOES PIC 9 VALUE 0.
       01 VALOR PIC 9(5)V99 VALUE 0.
       01 CONTINUAR PIC X VALUE 'S'.

       PROCEDURE DIVISION.
      *> cobol-lint CL002 main-procedure
       MAIN-PROCEDURE.
           PERFORM UNTIL CONTINUAR = 'N'
               DISPLAY 'CAIXA ELETRONICO'
               DISPLAY '1 - VER SALDO'
               DISPLAY '2 - DEPOSITAR'
               DISPLAY '3 - SACAR'
               DISPLAY '4 - SAIR'
               ACCEPT OPCOES
               EVALUATE OPCOES
                   WHEN 1
                       PERFORM VER-SALDO
                   WHEN 2
                       PERFORM DEPOSITAR
                   WHEN 3
                       PERFORM SACAR
                   WHEN 4
                       MOVE 'N' TO CONTINUAR
                   WHEN OTHER
                       DISPLAY 'OPCAO INVALIDA'
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       VER-SALDO.
           DISPLAY 'SEU SALDO E: ' SALDO.

       DEPOSITAR.
           DISPLAY 'DIGITE O VALOR PARA DEPOSITAR:'
           ACCEPT VALOR
           IF VALOR > 0 THEN
               ADD VALOR TO SALDO
               DISPLAY 'DEPOSITO REALIZADO COM SUCESSO. SEU NOVO SALDO E: ' SALDO
           ELSE
               DISPLAY 'VALOR INVALIDO PARA DEPOSITO.'
           END-IF.

       SACAR.
           DISPLAY 'DIGITE O VALOR PARA SACAR:'
           ACCEPT VALOR
           IF VALOR > 0 THEN
               IF VALOR > SALDO THEN
                   DISPLAY 'SALDO INSUFICIENTE.'
               ELSE
                   SUBTRACT VALOR FROM SALDO
                   DISPLAY 'SAQUE REALIZADO COM SUCESSO. SEU NOVO SALDO E: ' SALDO
               END-IF
           ELSE
               DISPLAY 'VALOR INVALIDO PARA SAQUE.'
           END-IF.

         END PROGRAM CAIXA-ELETRONICO.

         STOP RUN.