      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0303.
       AUTHOR.     JOHNATHAN.
      *================================================================*
      *              C A P G E M I N I - S I S T E M A S               *
      *================================================================*
      *    PROGRAMA....: EXER0303
      *    PROGRAMADOR.: JOHNATHAN
      *    ANALISTA....: ARI BORGES                                *
      *    DATA........: 20/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR UM BALANCELINE GERANDO                *
      *                    2 ARQUIVOS DE SAIDA
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT03103
      *      ARQENT02                                  ENT03203   
      *      ARQSAI01                                  SAI03103
      *      ARQSAI02                                  SAI03203
      *      ARQSAI03                                  SAI03303
      *      ARQSAI04                                  SAI03403
      *----------------------------------------------------------------*
      *    ROTINAS.....:                                               *
      *                                                                *
      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
      *
           SELECT ARQENT01 ASSIGN      TO UT-S-ARQENT01
                      FILE STATUS      IS WRK-FS-ARQENT01.
           
           SELECT ARQENT02 ASSIGN      TO UT-S-ARQENT02
                      FILE STATUS      IS WRK-FS-ARQENT02.
           
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS      IS WRK-FS-ARQSAI01.
           
           SELECT ARQSAI02 ASSIGN       TO UT-S-ARQSAI02
                      FILE STATUS      IS WRK-FS-ARQSAI02.

           SELECT ARQSAI03 ASSIGN       TO UT-S-ARQSAI03
                      FILE STATUS      IS WRK-FS-ARQSAI03.
           
           SELECT ARQSAI04 ASSIGN       TO UT-S-ARQSAI04
                      FILE STATUS      IS WRK-FS-ARQSAI04.

      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================
      *                                                                *
      *----------------------------------------------------------------
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ARQUIVO DOS REGISTROS DE ENTRADA E SAIDA                    *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 72                 *
      *----------------------------------------------------------------*

       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT01             PIC X(56).

       FD  ARQENT02
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT02             PIC X(16).

      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 46                *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(16).

       FD  ARQSAI02
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI02             PIC X(16).

       FD  ARQSAI03
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI03             PIC X(06).

       FD  ARQSAI04
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI04             PIC X(06).
      
      *---------------------------------------------------------------*
      
      *---------------------------------------------------------------*
      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0303 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC  X(008) VALUE 'EXER0303'.
       77 WRK-MASK-QTDREG         PIC  ZZ.ZZ9.
       77 ACU-LIDOS-ARQENT01      PIC  9(005) VALUE ZEROS.
       77 ACU-LIDOS-ARQENT02      PIC  9(005) VALUE ZEROS.
       77 ACU-GRAVA-ARQSAI01      PIC  9(005) VALUE ZEROS.
       77 ACU-GRAVA-ARQSAI02      PIC  9(005) VALUE ZEROS.
       77 ACU-GRAVA-ARQSAI03      PIC  9(005) VALUE ZEROS.
       77 ACU-GRAVA-ARQSAI04      PIC  9(005) VALUE ZEROS.

       01 WRK-CHAVE-CADASTRO.
          03 WRK-AG-CADASTRO      PIC  9(003).
          03 WRK-CTA-CADASTRO     PIC  9(003).
       
       01 WRK-CHAVE-MOVIMENTO.
          03 WRK-AG-MOV           PIC 9(003).
          03 WRK-CTA-MOV          PIC 9(003). 

      *
       77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.
          88 WRK-CN-ARQENT01      VALUE 'ENT03103'.
          88 WRK-CN-ARQENT02      VALUE 'ENT03203'.
          88 WRK-CN-ARQSAI01      VALUE 'SAI03103'.
          88 WRK-CN-ARQSAI02      VALUE 'SAI03203'.
          88 WRK-CN-ARQSAI03      VALUE 'SAI03303'.
          88 WRK-CN-ARQSAI04      VALUE 'SAI03403'.

       77 WRK-COMANDO             PIC  X(005) VALUE SPACES.
          88 WRK-CN-OPEN          VALUE 'OPEN '.
          88 WRK-CN-CLOSE         VALUE 'CLOSE'.
          88 WRK-CN-READ          VALUE 'READ '.
          88 WRK-CN-WRITE         VALUE 'WRITE'. 

       77 WRK-CALE2000            PIC X(008) VALUE 'CALE2000'.

      *----------------------------------------------------------------*
       01 FILLER                  PIC X(050) VALUE
           '* AREA DE COMUNICACAO COM CALE2000 *'.           
           

      *----------------------------------------------------------------
       01 FILLER                  PIC  X(050) VALUE
             'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.
          05 WRK-FS-ARQENT01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-ENT01-OK               VALUE '00'.
             88 WRK-FS-ENT01-FIM              VALUE '10'.
          05 WRK-FS-ARQENT02         PIC X(002) VALUE SPACES.
             88 WRK-FS-ENT02-OK               VALUE '00'.
             88 WRK-FS-ENT02-FIM              VALUE '10'.

      *
           05 WRK-FS-ARQSAI01         PIC  X(002) VALUE SPACES.
               88 WRK-FS-SAI01-OK                  VALUE '00'.
           05 WRK-FS-ARQSAI02         PIC  X(002) VALUE SPACES.
               88 WRK-FS-SAI02-OK                  VALUE '00'.
           05 WRK-FS-ARQSAI03         PIC  X(002) VALUE SPACES.
               88 WRK-FS-SAI03-OK                  VALUE '00'.
           05 WRK-FS-ARQSAI04         PIC  X(002) VALUE SPACES.
               88 WRK-FS-SAI04-OK                  VALUE '00'.

           05 WRK-FS-DISPLAY          PIC X(002) VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
      **** AREA ARQUIVO DE ENTRADA E SAIDA

           COPY ENT03103.
           COPY ENT03203.
           COPY SAI03103.
           COPY SAI03203.
           COPY SAI03303.
           COPY SAI03403.
           
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'EXER0102 - FIM DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.
      *----------------------------------------------------------------
      *
           PERFORM 1000-INICIALIZAR
      *
           PERFORM 3000-PROCESSAR UNTIL (WRK-FS-ENT01-FIM)
                                  AND   (WRK-FS-ENT02-FIM)
      *
           PERFORM 9900-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ROTINA DE INICIALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
      *    
           SET WRK-CN-OPEN                    TO TRUE
           OPEN INPUT ARQENT01
           SET WRK-CN-ARQENT01                TO TRUE
      *
           IF NOT WRK-FS-ENT01-OK
              MOVE WRK-FS-ARQENT01            TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           OPEN INPUT ARQENT02 
           SET WRK-CN-ARQENT02                 TO TRUE

      *
           IF NOT WRK-FS-ENT02-OK
              MOVE WRK-FS-ARQENT02             TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           OPEN OUTPUT ARQSAI01
           SET WRK-FS-ARQSAI01                 TO TRUE
      *
           IF NOT WRK-FS-SAI01-OK
              MOVE WRK-FS-ARQSAI01             TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF
      *
           OPEN OUTPUT ARQSAI02
           SET WRK-FS-ARQSAI02                 TO TRUE
      *
           IF NOT WRK-FS-SAI02-OK
              MOVE WRK-FS-ARQSAI02             TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           OPEN OUTPUT ARQSAI03
           SET WRK-FS-ARQSAI03                 TO TRUE
      *
           IF NOT WRK-FS-SAI03-OK
              MOVE WRK-FS-ARQSAI03             TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           OPEN OUTPUT ARQSAI04
           SET WRK-FS-ARQSAI04                 TO TRUE
      *
           IF NOT WRK-FS-SAI04-OK
              MOVE WRK-FS-ARQSAI04             TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF
              
           
           PERFORM 3800-LER-CADASTRO
           
           IF WRK-FS-ENT01-FIM
              DISPLAY '************************************************'
              DISPLAY '*          ARQUIVO DE ENTRADA 01 VAZIO         *'
              DISPLAY '* PROGRAMA ' WRK-PROGRAMA                        
                                         ' CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9900-FINALIZAR 
           END-IF 

           PERFORM 3850-LER-MOVIMENTO
           
           IF WRK-FS-ENT02-FIM
              DISPLAY '************************************************'
              DISPLAY '*          ARQUIVO DE ENTRADA 02 VAZIO         *'
              DISPLAY '* PROGRAMA ' WRK-PROGRAMA                        
                                         ' CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9900-FINALIZAR 
           END-IF

           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *                     BALANCE LINE                               *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*

           IF (WRK-AG-CADASTRO EQUAL WRK-AG-MOV)
           AND (WRK-CTA-CADASTRO EQUAL WRK-CTA-MOV)
              MOVE ARQENT01-AGENCIA           TO ARQSAI01-AGENCIA
              MOVE ARQENT01-CONTA             TO ARQSAI01-CONTA

              PERFORM 3100-CALCULA-QTDE-DIAS

           IF CALE01-QTDE-DIAS-PERIODO LESS 31
                 PERFORM 3300-LAYOUT-SAIDA1
                 PERFORM 3910-GRAVAR-SAIDA1
              ELSE 
                 PERFORM 3400-LAYOUT-SAIDA2
                 PERFORM 3920-GRAVAR-SAIDA2
              END-IF
              PERFORM 3800-LER-CADASTRO
              PERFORM 3850-LER-MOVIMENTO 
           ELSE 
               IF (WRK-AG-CADASTRO LESS WRK-AG-MOV)
               AND (WRK-CTA-CADASTRO LESS WRK-CTA-MOV)
                   MOVE ARQENT01-AGENCIA          TO ARQSAI02-AGENCIA
                   MOVE ARQENT01-CONTA            TO ARQSAI02-CONTA 
                   PERFORM 3500-LAYOUT-SAIDA3
                   PERFORM 3930-GRAVAR-SAIDA3
                   PERFORM 3800-LER-CADASTRO
               ELSE
                   MOVE ARQENT02-REGISTRO         TO ARQSAI02-REGISTRO
                   PERFORM 3600-LAYOUT-SAIDA4
                   PERFORM 3940-GRAVAR-SAIDA4
                   PERFORM 3850-LER-MOVIMENTO
                END-IF
           END-IF

           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER QTDE DE DIAS CORRIDOS ENTRE DUAS DATAS    *
      *----------------------------------------------------------------*
       3100-CALCULA-QTDE-DIAS SECTION.
      *----------------------------------------------------------------*
           INITIALIZE CALE01-REGISTRO.

           MOVE 'CALE0001'            TO CALE01-ID-BLOCO
           MOVE LENGHT                OF CALE01-REGISTRO
                                      TO CALE01-TAM-BLOCO.
           
           MOVE 'F3'                  TO CALE01-FUNCAO.
           MOVE 'SF3004'              TO CALE01-SUB-FUNCAO.

           MOVE 007                   TO CALE01-FORMATO-ARGUMENTO-INI
           MOVE ARQENT01-DAT-EMP      TO CALE01-VLR-ARGUMENTO-INI.
           
           MOVE 003                   TO CALE01-FORMATO-ARGUMENTO-FINAL.
           MOVE ARQENT02-DAT-PAG      TO CALE01-VLR-ARGUMENTO-FINAL.            

           MOVE 'I'                   TO CALE01-TP-INCL-ARGUMENTO-INI
                                         CALE01-TP-INCL-ARGUMENTO-FINAL.
           MOVE ZEROS                 TO CALE01-COD-IDIOMA
           MOVE ZEROS                 TO CALE01-COD-LOCALIDADE

           MOVE ARQENT01-DAT-EMP      TO CALE01-VLR-ARGUMENTO-INI.
           MOVE ARQENT02-DAT-PAG      TO CALE01-VLR-ARGUMENTO-FINAL. 

           CALL WRK-CALE2000          USING CALE01-REGISTRO.

           EVALUATE CALE01-COD-RETORNO

              WHEN ZEROS
                 IF CALE01-QTDE-DIAS-PERIODO GREATER 30
                   PERFORM 3920-GRAVAR-SAIDA2
                 ELSE 
                   PERFORM 3910-GRAVAR-SAIDA1
              WHEN OTHER 
                 MOVE CALE01-COD-RETORNO TO WRK-FS-DISPLAY
                 DISPLAY 'ERRO NA ROTINA CALE2000'
                 PERFORM 9900-FIM-PROGRAMA
           END-EVALUATE

           . 
      *----------------------------------------------------------------*
       3100-99-FIM.
           EXIT.                                      
      *----------------------------------------------------------------*
      *          PAGOS EM DIA                                          *
      *----------------------------------------------------------------*
       3300-LAYOUT-SAIDA1 SECTION.
      *----------------------------------------------------------------*
           MOVE ARQENT01-REGISTRO      TO  ARQSAI01-REGISTRO

           .
      *----------------------------------------------------------------*
       3300-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *          PAGOS ATRASADO                                        *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3400-LAYOUT-SAIDA2 SECTION.
      *----------------------------------------------------------------*
           MOVE ARQENT01-AGENCIA        TO ARQSAI02-AGENCIA
           MOVE ARQENT01-CONTA          TO ARQSAI02-CONTA
           MOVE ARQENT01-DAT-EMP        TO ARQSAI02-DAT-EMP

           .
      *----------------------------------------------------------------*
       3400-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *          NÃO EFETUARAM PAGAMENTO                               *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3500-LAYOUT-SAIDA3 SECTION.
      *----------------------------------------------------------------*
           MOVE ARQENT01-AGENCIA        TO ARQSAI02-AGENCIA
           MOVE ARQENT01-CONTA          TO ARQSAI02-CONTA
           .
      *----------------------------------------------------------------*
       3500-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *          NÃO CADASTRADOS (INEXISTENTES)                         *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3600-LAYOUT-SAIDA4 SECTION.
      *----------------------------------------------------------------*
           MOVE ARQENT02-AGENCIA        TO ARQSAI02-AGENCIA
           MOVE ARQENT02-CONTA          TO ARQSAI02-CONTA
           .
      *----------------------------------------------------------------*
       3600-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

       3800-LER-CADASTRO SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                     ARQENT01-REGISTRO 
           SET WRK-CN-READ                TO TRUE
           SET WRK-CN-ARQENT01            TO TRUE

           READ ARQENT01 INTO ARQENT01-REGISTRO.
      *
           EVALUATE WRK-FS-ARQENT01                                     
                WHEN '00'                                                
                     ADD 1 TO ACU-LIDOS-ARQENT02    
                WHEN '10'                                                
                     MOVE HIGH-VALUES     TO WRK-CHAVE-CADASTRO                 
                WHEN OTHER                                               
                     MOVE WRK-FS-ARQENT01 TO WRK-FS-DISPLAY              
                     PERFORM 9100-ERROS-ARQUIVOS                         
            END-EVALUATE

           .
      *
      *----------------------------------------------------------------*
       3800-99-FIM.                     
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3850-LER-MOVIMENTO SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                     ARQENT02-REGISTRO 
           SET WRK-CN-READ                TO TRUE
           SET WRK-CN-ARQENT02            TO TRUE

           READ ARQENT02 INTO ARQENT02-REGISTRO.
      *
           EVALUATE WRK-FS-ARQENT02                                     
                WHEN '00'                                                
                     ADD 1 TO ACU-LIDOS-ARQENT02    
                WHEN '10'                                                
                     MOVE HIGH-VALUES     TO ARQENT02-AGENCIA                   
                WHEN OTHER                                               
                     MOVE WRK-FS-ARQENT02 TO WRK-FS-DISPLAY              
                     PERFORM 9100-ERROS-ARQUIVOS                         
            END-EVALUATE                                                

           .
      *
      *----------------------------------------------------------------*
       3850-99-FIM.                     
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------
       3910-GRAVAR-SAIDA1 SECTION.
      *----------------------------------------------------------------*
           
           SET WRK-CN-WRITE        TO TRUE 
           SET WRK-CN-ARQSAI01     TO TRUE

           WRITE FD-ARQSAI01 FROM ARQSAI01-REGISTRO.

           IF NOT WRK-FS-SAI01-OK 
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 
           
           COMPUTE ACU-GRAVA-ARQSAI01 = ACU-GRAVA-ARQSAI01 + 1

           INITIALIZE ARQSAI01-REGISTRO
           .
           
      *----------------------------------------------------------------*
       3910-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------
       3920-GRAVAR-SAIDA2 SECTION.
      *----------------------------------------------------------------*
           
           SET WRK-CN-WRITE        TO TRUE 
           SET WRK-CN-ARQSAI02     TO TRUE

           WRITE FD-ARQSAI02 FROM ARQSAI02-REGISTRO.

           IF NOT WRK-FS-SAI02-OK 
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 
           
           COMPUTE ACU-GRAVA-ARQSAI02 = ACU-GRAVA-ARQSAI02 + 1

           INITIALIZE ARQSAI02-REGISTRO
           .
           
      *----------------------------------------------------------------*
       3920-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------
       3930-GRAVAR-SAIDA3 SECTION.
      *----------------------------------------------------------------*
           
           SET WRK-CN-WRITE        TO TRUE 
           SET WRK-CN-ARQSAI03     TO TRUE

           WRITE FD-ARQSAI03 FROM ARQSAI03-REGISTRO.

           IF NOT WRK-FS-SAI03-OK 
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 
           
           COMPUTE ACU-GRAVA-ARQSAI03 = ACU-GRAVA-ARQSAI03 + 1

           INITIALIZE ARQSAI03-REGISTRO
           .
           
      *----------------------------------------------------------------*
       3930-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------
       3940-GRAVAR-SAIDA4 SECTION.
      *----------------------------------------------------------------*
           
           SET WRK-CN-WRITE        TO TRUE 
           SET WRK-CN-ARQSAI04     TO TRUE

           WRITE FD-ARQSAI04 FROM ARQSAI04-REGISTRO.

           IF NOT WRK-FS-SAI04-OK 
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 
           
           COMPUTE ACU-GRAVA-ARQSAI04 = ACU-GRAVA-ARQSAI04 + 1

           INITIALIZE ARQSAI04-REGISTRO
           .
           
      *----------------------------------------------------------------*
       3940-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------* 
      *----------------------------------------------------------------*
       9100-ERROS-ARQUIVOS SECTION .
      *----------------------------------------------------------------*
  
           DISPLAY '************************************************'
           DISPLAY '*       ERRO EM OPERACAO COM ARQUIVOS          *'
           DISPLAY '* COMANDO: 'WRK-COMANDO'                       *'
           DISPLAY '* ARQUIVO: 'WRK-ARQUIVO'                       *'
           DISPLAY '* FILE-STATUS ENT:' WRK-FS-ARQENT01           '*'
           DISPLAY '* FILE-STATUS SAI:' WRK-FS-ARQSAI01           '*'
           DISPLAY '* FILE-STATUS ENT:' WRK-FS-ARQENT02           '*'
           DISPLAY '* FILE-STATUS SAI:' WRK-FS-ARQSAI02           '*'
           DISPLAY '* FILE-STATUS SAI:' WRK-FS-ARQSAI03           '*'
           DISPLAY '* FILE-STATUS SAI:' WRK-FS-ARQSAI04           '*'
           DISPLAY '* 'WRK-PROGRAMA'  CANCELADO                    *'
           DISPLAY '************************************************'

           PERFORM 9900-FINALIZAR.
      *----------------------------------------------------------------*
       9100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
       9900-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           SET WRK-CN-CLOSE       TO TRUE
           SET WRK-CN-ARQENT01    TO TRUE

           CLOSE ARQENT01
           IF NOT WRK-FS-ENT01-OK
              MOVE WRK-FS-ARQENT01 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           SET WRK-CN-ARQENT02     TO TRUE

           CLOSE ARQENT02
           IF NOT WRK-FS-ENT02-OK
              MOVE WRK-FS-ARQENT02 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           SET WRK-CN-ARQSAI01     TO TRUE

           CLOSE ARQSAI01
           IF NOT WRK-FS-SAI01-OK
              MOVE WRK-FS-ARQSAI01 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           SET WRK-CN-ARQSAI02     TO TRUE

           CLOSE ARQSAI02
           IF NOT WRK-FS-SAI02-OK
              MOVE WRK-FS-ARQSAI02 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           SET WRK-CN-ARQSAI03     TO TRUE

           CLOSE ARQSAI03
           IF NOT WRK-FS-SAI03-OK
              MOVE WRK-FS-ARQSAI03 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           SET WRK-CN-ARQSAI04     TO TRUE

           CLOSE ARQSAI04
           IF NOT WRK-FS-SAI04-OK
              MOVE WRK-FS-ARQSAI04 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           CLOSE ARQENT01
           CLOSE ARQENT02
           CLOSE ARQSAI01
           CLOSE ARQSAI02
           CLOSE ARQSAI03
           CLOSE ARQSAI04
           
           MOVE ACU-LIDOS-ARQENT01 TO WRK-MASK-QTDREG
           DISPLAY '***************************************************'
           DISPLAY '* QTDE DE CADASTROS LIDOS 'WRK-MASK-QTDREG'       *'
           MOVE ACU-LIDOS-ARQENT02 TO WRK-MASK-QTDREG
           DISPLAY '***************************************************'
           DISPLAY '* QTDE DE LIDOS MOVIMENTO: 'WRK-MASK-QTDREG'      *'
           DISPLAY '***************************************************'

           MOVE ACU-GRAVA-ARQSAI01 TO WRK-MASK-QTDREG
           DISPLAY '***************************************************'
           DISPLAY '* QTDE DE GRAVADOS EM DIA 'WRK-MASK-QTDREG'       *'
           MOVE ACU-GRAVA-ARQSAI02 TO WRK-MASK-QTDREG
           DISPLAY '***************************************************'
           DISPLAY '* QTDE DE GRAVADOS PAG ATRASADOS:'WRK-MASK-QTDREG'*'
           DISPLAY '***************************************************'
           MOVE ACU-GRAVA-ARQSAI03 TO WRK-MASK-QTDREG
           DISPLAY '***************************************************'
           DISPLAY '* QTDE DE GRAVADOS QUE N/PAGARAM:'WRK-MASK-QTDREG'*'
           DISPLAY '***************************************************'
           MOVE ACU-GRAVA-ARQSAI04 TO WRK-MASK-QTDREG
           DISPLAY '***************************************************'
           DISPLAY '* QTDE DE GRAVADOS INEXISTENTES:'WRK-MASK-QTDREG' *'
           DISPLAY '***************************************************'
           DISPLAY '***************************************************'
           DISPLAY '* FIM DO PROGRAMA 'WRK-PROGRAMA'                  *'
           DISPLAY '***************************************************'
           

           PERFORM 9900-FIM-PROGRAMA
           .
      *----------------------------------------------------------------*
       9900-FIM-PROGRAMA SECTION.

           STOP RUN.
      *----------------------------------------------------------------*