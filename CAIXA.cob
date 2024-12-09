       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAIXA.
       AUTHOR. "Clebson Lira".
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           77 WS-OPCAO PIC X(2).
           77 WS-DEPOSITO PIC 9(5)V99 VALUE ZERO.
           77 WS-SAQUE PIC 9(5)V99 VALUE ZERO.
           77 WS-SALDO PIC 9(5)V99 VALUE ZERO.
       PROCEDURE DIVISION.
           P001-MENU1.
               DISPLAY "-------------------------------".
               DISPLAY "-        BANCO LIRA           -".
               DISPLAY "-------------------------------".
               DISPLAY "1 - DEPOSITO".
               DISPLAY "2 - SAQUE".
               DISPLAY "3 - SALDO".
               DISPLAY "4 - SAIR".
               ACCEPT WS-OPCAO.
           EVALUATE WS-OPCAO
               WHEN "1" 
                   DISPLAY "-------------------------------"
                   DISPLAY "-        BANCO LIRA           -"
                   DISPLAY "-------------------------------"
                   DISPLAY "VALOR DO DEPOSITO: "
                   ACCEPT WS-DEPOSITO
                   IF WS-DEPOSITO > 0
                   THEN
                       COMPUTE WS-SALDO = WS-SALDO + WS-DEPOSITO
                       DISPLAY "DEPOSITO REALIZADO COM SUCESSO"
                   ELSE
                       DISPLAY "VALOR DE DEPOSITO INVALIDO"
                   END-IF
                   PERFORM P001-MENU1
               WHEN "2" 
                   DISPLAY "-------------------------------"
                   DISPLAY "-        BANCO LIRA           -"
                   DISPLAY "-------------------------------"
                   DISPLAY "VALOR DO SAQUE: "
                   ACCEPT WS-SAQUE
                   IF WS-SAQUE > 0
                   THEN
                       IF WS-SAQUE > WS-SALDO
                       THEN
                           DISPLAY "SALDO INSUFICIENTE"
                       ELSE
                           COMPUTE WS-SALDO = WS-SALDO - WS-SAQUE
                           DISPLAY "SAQUE REALIZADO COM SUCESSO"
                       END-IF
                   ELSE
                       DISPLAY "VALOR DE SAQUE INVALIDO"
                   END-IF
                   PERFORM P001-MENU1
               WHEN "3" 
                   DISPLAY "-------------------------------"
                   DISPLAY "-        BANCO LIRA           -"
                   DISPLAY "-------------------------------"
                   DISPLAY "SALDO ATUAL: R$ " WS-SALDO
                   PERFORM P001-MENU1
               WHEN "4" 
                   DISPLAY "SAINDO DO SISTEMA..."
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA"
                   PERFORM P001-MENU1
           END-EVALUATE.
           STOP RUN.

