       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAIXA.
       AUTHOR. "Clebson Lira".
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           77 WS-OPCAO PIC X(2).
           77 WS-DEPOSITO PIC 9(5)V99 VALUE ZERO.
           77 WS-SAQUE PIC 9(5)V99 VALUE ZERO.
           77 WS-SALDO PIC 9(5)V99 VALUE ZERO.
           77 WS-SALDO-FORMATADO PIC ZZZZZ.99.
           77 WS-FIM-EXECUCAO PIC X VALUE "N".
       PROCEDURE DIVISION.
           MAIN-PROCESS.
               PERFORM UNTIL WS-FIM-EXECUCAO = "S"
                   PERFORM P001-MENU1
               END-PERFORM
               DISPLAY "OBRIGADO POR USAR O BANCO LIRA. VOLTE SEMPRE!"
               STOP RUN.

           P001-MENU1.
               PERFORM P000-TITULO
               DISPLAY "1 - DEPOSITO".
               DISPLAY "2 - SAQUE".
               DISPLAY "3 - SALDO".
               DISPLAY "4 - SAIR".
               ACCEPT WS-OPCAO
               EVALUATE WS-OPCAO
                   WHEN "1" 
                       PERFORM P002-DEPOSITO
                   WHEN "2" 
                       PERFORM P003-SAQUE
                   WHEN "3" 
                       PERFORM P004-SALDO
                   WHEN "4" 
                       DISPLAY "SAINDO DO SISTEMA..."
                       MOVE "S" TO WS-FIM-EXECUCAO
                   WHEN OTHER
                       DISPLAY "OPCAO INVALIDA, TENTE NOVAMENTE."
               END-EVALUATE.

           P000-TITULO.
               DISPLAY "-------------------------------".
               DISPLAY "-        BANCO LIRA           -".
               DISPLAY "-------------------------------".

           P002-DEPOSITO.
               PERFORM P000-TITULO
               DISPLAY "VALOR DO DEPOSITO: "
               ACCEPT WS-DEPOSITO
               IF WS-DEPOSITO > 0 THEN
                   COMPUTE WS-SALDO = WS-SALDO + WS-DEPOSITO
                   DISPLAY "DEPOSITO DE R$ " WS-DEPOSITO " REALIZADO COM SUCESSO"
                   MOVE WS-SALDO TO WS-SALDO-FORMATADO
                   DISPLAY "NOVO SALDO: R$ " WS-SALDO-FORMATADO
               ELSE
                   DISPLAY "VALOR DE DEPOSITO INVALIDO"
               END-IF.

           P003-SAQUE.
               PERFORM P000-TITULO
               DISPLAY "VALOR DO SAQUE: "
               ACCEPT WS-SAQUE
               IF WS-SAQUE > 0 THEN
                   IF WS-SAQUE > WS-SALDO THEN
                       DISPLAY "SALDO INSUFICIENTE"
                   ELSE
                       COMPUTE WS-SALDO = WS-SALDO - WS-SAQUE
                       DISPLAY "SAQUE DE R$ " WS-SAQUE " REALIZADO COM SUCESSO"
                       MOVE WS-SALDO TO WS-SALDO-FORMATADO
                       DISPLAY "NOVO SALDO: R$ " WS-SALDO-FORMATADO
                   END-IF
               ELSE
                   DISPLAY "VALOR DE SAQUE INVALIDO"
               END-IF.

           P004-SALDO.
               PERFORM P000-TITULO
               MOVE WS-SALDO TO WS-SALDO-FORMATADO
               DISPLAY "SALDO ATUAL: R$ " WS-SALDO-FORMATADO.
