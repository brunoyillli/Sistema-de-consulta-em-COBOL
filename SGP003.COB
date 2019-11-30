       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGP003.
       AUTHOR. Bruno Mendes Carvalho.
      ****************************************
      *  MANUTENCAO DO CADASTRO DE CLIENTE   *
      ****************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                          DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CPF
                    FILE STATUS IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = NOME
                                                      WITH DUPLICATES.
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CODIGO
                    FILE STATUS IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = LOGRADOURO
                                                      WITH DUPLICATES.

      *
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCLI
             LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS "CADCLI.DAT".
       01 REGCLI.
                03 CPF                 PIC 9(11).
                03 NOME                PIC X(40).
                03 CEP                 PIC 9(08).
                03 NUMERO              PIC 9(04).
                03 COMPLEMENTO         PIC X(12).
                03 TELEFONE            PIC 9(09).
                03 TELEFONE2           PIC 9(09).
                03 DDD                 PIC 9(02).
                03 DDD2                PIC 9(02).
                03 EMAIL               PIC X(40).
                03 SITUACAO            PIC X(01).
       FD CADCEP
             LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CODIGO           PIC X(08).
                03 LOGRADOURO       PIC X(30).
                03 BAIRRO           PIC X(20).
                03 CIDADE           PIC X(20).
                03 UF               PIC X(02).
                03 UFEXTENSO        PIC X(17).
      *
      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 MASC1       PIC ZZZ.ZZ9,99.
       01 MASC2       PIC ZZZZ.ZZZ.ZZ9,99.
       77 W-SEL       PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 IND          PIC 9(02) VALUE ZEROS.
       01 SITUACAOEXTENSO  PIC X(09) VALUE SPACES.
       01 UFCLIEXTENSO  PIC X(17) VALUE SPACES.
      *
       01 TABSITUACAO.
          03 T1   PIC X(10) VALUE "AATIVO    ".
          03 T2   PIC X(10) VALUE "CCANCELADO".
          03 T3   PIC X(10) VALUE "DDELETADO ".
          03 T4   PIC X(10) VALUE "SSUSPENSO ".

       01 TABSITAUX REDEFINES TABSITUACAO.
          03 TABSIT   PIC X(10) OCCURS 4 TIMES.
       01 ALFASIT.
             03 ALFASIT1 PIC X(01).
             03 ALFASIT2 PIC X(09).
      *
       SCREEN SECTION.
       01  TELACLIENTE.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01
               VALUE  "                            CADASTRO DE".
           05  LINE 03  COLUMN 41
               VALUE  "CLIENTE".
           05  LINE 05  COLUMN 01
               VALUE  "  CPF :".
           05  LINE 07  COLUMN 01
               VALUE  "  NOME :".
           05  LINE 09  COLUMN 01
               VALUE  "  CEP :".
           05  LINE 11  COLUMN 01
               VALUE  "  LOGRADOURO :".
           05  LINE 11  COLUMN 41
               VALUE  "     NUM :       COMPL:".
           05  LINE 13  COLUMN 01
               VALUE  "  BAIRRO:                    CIDADE:".
           05  LINE 13  COLUMN 41
               VALUE  "                 UF:".
           05  LINE 15  COLUMN 01
               VALUE  "  TELEFONE:(  )".
           05  LINE 15  COLUMN 41
               VALUE  "     TELEFONE 2:(  )".
           05  LINE 17  COLUMN 01
               VALUE  "  EMAIL :".
           05  LINE 19  COLUMN 01
               VALUE  "  SITUACAO:".
           05  TCPF
               LINE 05  COLUMN 09  PIC 9(11)
               USING  CPF
               HIGHLIGHT.
           05  TNOME
               LINE 07  COLUMN 10  PIC X(40)
               USING  NOME
               HIGHLIGHT.
           05  TCEP
               LINE 09  COLUMN 10  PIC 9(08)
               USING  CEP
               HIGHLIGHT.
           05  TLOGRADOUROCLI
               LINE 11  COLUMN 15  PIC X(30)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TNUMERO
               LINE 11  COLUMN 52  PIC 9(04)
               USING  NUMERO
               HIGHLIGHT.
           05  TCOMPLEMENTO
               LINE 11  COLUMN 64  PIC X(12)
               USING  COMPLEMENTO
               HIGHLIGHT.
           05  TBAIRROCLI
               LINE 13  COLUMN 10  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADECLI
               LINE 13  COLUMN 37  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUFCLI
               LINE 13  COLUMN 61  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TUFCLIEX
               LINE 13  COLUMN 64  PIC X(17)
               USING  UFCLIEXTENSO
               HIGHLIGHT.
           05  TDDD
               LINE 15  COLUMN 13  PIC 9(02)
               USING  DDD
               HIGHLIGHT.
           05  TTELEFONE
               LINE 15  COLUMN 17  PIC 9(09)
               USING  TELEFONE
               HIGHLIGHT.
           05  TDDD2
               LINE 15  COLUMN 58  PIC 9(02)
               USING  DDD2
               HIGHLIGHT.
           05  TTELEFONE2
               LINE 15  COLUMN 62  PIC 9(09)
               USING  TELEFONE2
               HIGHLIGHT.
           05  TEMAIL
               LINE 17  COLUMN 11  PIC X(40)
               USING  EMAIL
               HIGHLIGHT.
           05  TSITUACAO
               LINE 19  COLUMN 13  PIC X(01)
               USING  SITUACAO
               HIGHLIGHT.
           05  TSITUACAOEX
               LINE 19  COLUMN 15  PIC X(09)
               USING  SITUACAOEXTENSO
               HIGHLIGHT.

      *----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCLI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                    OPEN OUTPUT CADCLI
                    CLOSE CADCLI CADCEP
                    MOVE "*** ARQUIVO CADCLI SENDO CRIADO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-OP0
              ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADCLI" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
              ELSE
                    NEXT SENTENCE.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              MOVE "*** ARQUIVO CADCEP NAO FOI ACHADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ROT-FIM.

       INC-001.
              MOVE SPACES TO NOME LOGRADOURO BAIRRO CIDADE
              UF COMPLEMENTO EMAIL SITUACAO SITUACAOEXTENSO.
              MOVE ZEROS TO CPF CEP NUMERO TELEFONE TELEFONE2 DDD2
              DDD.
              DISPLAY TELACLIENTE.
       INC-002.
              ACCEPT TCPF
              ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02
                 CLOSE CADCLI CADCEP
                 GO TO ROT-FIM.
               IF CPF = " "
                  MOVE "*** CPF INVALIDO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INC-002.
       LER-CADCLI001.
               MOVE 0 TO W-SEL
               READ CADCLI
               IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                    DISPLAY TELACLIENTE
                    PERFORM INC-012 THRU INC-012
                    MOVE "*** CPF JA CADASTRADO***" TO MENS
                    MOVE 1 TO W-SEL
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ACE-001
                   ELSE
                     MOVE "ERRO NA LEITURA ARQ. CADCLI" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-003.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-004A.
                MOVE CEP TO CODIGO
                READ CADCEP
                IF ST-ERRO NOT = "00"
                    MOVE " ARQUIVO CADCEP NAO FOI ENCONTRADO " TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-004.
                DISPLAY TELACLIENTE.
       INC-005.
                ACCEPT TNUMERO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TCOMPLEMENTO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-006A.
                ACCEPT TDDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-007.
                ACCEPT TTELEFONE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006A.
       INC-008.
                ACCEPT TDDD2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT TTELEFONE2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-010.
                ACCEPT TEMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
       INC-011.
                ACCEPT TSITUACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010
                MOVE 1 TO IND.
       INC-012.
                MOVE TABSIT(IND) TO ALFASIT
               IF SITUACAO NOT = ALFASIT1
                    IF IND < 5
                         ADD 1 TO IND
                         GO TO INC-012
                    ELSE
                        MOVE SPACES TO SITUACAOEXTENSO
                        MOVE "SITUACAO INVALIDA!" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        MOVE 1 TO IND
                        GO TO INC-011
                ELSE
                    MOVE ALFASIT2 TO SITUACAOEXTENSO.
                DISPLAY TELACLIENTE.
      *
       INC-013.

                IF W-SEL = 1
                  GO TO ALT-OPC.
       INC-OPC.
               MOVE "S" TO W-OPCAO
               DISPLAY (23, 40) "DADOS OK (S/N) : ".
               ACCEPT (23, 57) W-OPCAO WITH UPDATE
               ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02 GO TO INC-001.
               IF W-OPCAO = "N" OR "n"
                  MOVE " * DADOS RECUSADOS PELO OPERADOR * " TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM E N=NAO ****" TO MENS
                   GO TO INC-OPC.
        INC-WR1.
                WRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                        MOVE "*** DADOS GRAVADOS *** " TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-001.
                IF ST-ERRO = "22"
                        MOVE "*** CPF JA EXISTE *** " TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-001
                ELSE
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADCLI" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
      *
      ***********************************************
      *   ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO     *
      ***********************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                        "F1=NOVO REGISTRO F2=ALTERAR  F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                    GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY(23, 40) "EXCLUIR (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM E N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCLI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                IF W-OPCAO = "N" OR "n"
                    MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                    MOVE "* DIGITE APENAS S=SIM e N=NAO *" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                    MOVE "*** REGISTRO ALTERADO *** " TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CPF" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      ********************
      * ROTINS DE FIM    *
      ********************
      *
        ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
        ROT-FIMP.
                EXIT PROGRAM.
        ROT-FIMS.
                STOP RUN.
      *
      ***********************
      *  ROTINA DE MENSAGEM *
      ***********************
      *
        ROT-MENS.
                 MOVE ZEROS TO W-CONT.
        ROT-MENS1.
                 DISPLAY (23, 12) MENS.
        ROT-MENS2.
                 ADD 1 TO W-CONT
                 IF W-CONT < 3000
                    GO TO ROT-MENS2
                 ELSE
                    DISPLAY (23, 12) LIMPA.
        ROT-MENS-FIM.
                 EXIT.
        FIM-ROT-TEMPO.
