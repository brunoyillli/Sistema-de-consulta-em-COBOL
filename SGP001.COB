        IDENTIFICATION DIVISION.
       PROGRAM-ID. SGP001.
       AUTHOR. Bruno Mendes Carvalho.
      ****************************************
      *     MANUTENCAO DO CADASTRO DE CEP    *
      ****************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                          DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CODGO
                    FILE STATUS IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = LOGRADOURO
                                                      WITH DUPLICATES.
      *
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
             LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CODGO           PIC X(08).
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

      *
       01 TABUNIDADEFEDERACAO.
          03 T1   PIC X(19) VALUE "ACACRE             ".
          03 T2   PIC X(19) VALUE "ALALAGOAS          ".
          03 T3   PIC X(19) VALUE "APAMAPA            ".
          03 T4   PIC X(19) VALUE "AMAMAZONAS         ".
          03 T5   PIC X(19) VALUE "BABAHIA            ".
          03 T6   PIC X(19) VALUE "CECEARA            ".
          03 T7   PIC X(19) VALUE "DFDISTRITO FEDERAL ".
          03 T8   PIC X(19) VALUE "ESESPIRITO SANTO   ".
          03 T9   PIC X(19) VALUE "GOGOIAS            ".
          03 T10   PIC X(19) VALUE "MAMARANHAO         ".
          03 T11   PIC X(19) VALUE "MTMATO GROSSO      ".
          03 T12   PIC X(19) VALUE "MSMATO GROSSO SUL  ".
          03 T13   PIC X(19) VALUE "MGMINAS GERAIS     ".
          03 T14   PIC X(19) VALUE "PAPARA             ".
          03 T15   PIC X(19) VALUE "PBPARAIBA          ".
          03 T16   PIC X(19) VALUE "PRPARANA           ".
          03 T17   PIC X(19) VALUE "PEPERNAMBUCO       ".
          03 T18   PIC X(19) VALUE "PIPIAUI            ".
          03 T19   PIC X(19) VALUE "RJRIO DE JANEIRO   ".
          03 T20   PIC X(19) VALUE "RNRIO GRANDE NORTE ".
          03 T21   PIC X(19) VALUE "RSRIO GRANDE SUL   ".
          03 T22   PIC X(19) VALUE "RORONDONIA         ".
          03 T23   PIC X(19) VALUE "RRRORAIMA          ".
          03 T24   PIC X(19) VALUE "SCSANTA CATARINA   ".
          03 T25   PIC X(19) VALUE "SPSAO PAULO        ".
          03 T26   PIC X(19) VALUE "SESERGIPE          ".
          03 T27   PIC X(19) VALUE "TOTOCANTIS         ".


       01 TABUFAUX REDEFINES TABUNIDADEFEDERACAO.
          03 TABUF   PIC X(19) OCCURS 27 TIMES.
       01 ALFATP.
             03 ALFATP1 PIC X(02).
             03 ALFATP2 PIC X(17).
      *


       SCREEN SECTION.
        01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                         CADASTRO DE CEP".
           05  LINE 07  COLUMN 01
               VALUE  "      CODGO".
           05  LINE 09  COLUMN 01
               VALUE  "      LOGRADOURO".
           05  LINE 11  COLUMN 01
               VALUE  "      BAIRRO".
           05  LINE 13  COLUMN 01
               VALUE  "      CIDADE".
           05  LINE 15  COLUMN 01
               VALUE  "      UF".
           05  TCODGO
               LINE 07  COLUMN 22  PIC X(08)
               USING  CODGO
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 09  COLUMN 22  PIC X(30)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 11  COLUMN 22  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 13  COLUMN 22  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 15  COLUMN 22  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TUFEX
               LINE 15  COLUMN 28  PIC X(17)
               USING  UFEXTENSO
               HIGHLIGHT.
      *----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                    OPEN OUTPUT CADCEP
                    CLOSE CADCEP
                    MOVE "*** ARQUIVO CADCEP SENDO CRIADO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-OP0
              ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
              ELSE
                    NEXT SENTENCE.
       INC-001.
              MOVE SPACES TO CODGO LOGRADOURO BAIRRO CIDADE UF
              UFEXTENSO.
              DISPLAY TELACEP.
       INC-002.
              ACCEPT TCODGO
              ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02
                 CLOSE CADCEP
                 GO TO ROT-FIM.
               IF CODGO = " "
                  MOVE "*** CODGOCO INVALIDO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INC-002.
       LER-CADCEP001.
               MOVE 0 TO W-SEL
               READ CADCEP
               IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                    DISPLAY TELACEP
                    PERFORM INC-007 THRU INC-007
                    MOVE "*** CEP JA CADASTRADO***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ACE-001
                   ELSE
                     MOVE "ERRO NA LEITURA ARQ. CADCEP" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-003.
                ACCEPT TLOGRADOURO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                MOVE 1 TO IND.
       INC-007.
               MOVE TABUF(IND) TO ALFATP
               IF UF NOT = ALFATP1
                    IF IND < 27
                         ADD 1 TO IND
                         GO TO INC-007
                    ELSE
                        MOVE SPACES TO UFEXTENSO
                        MOVE "UF INVALIDO!" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        MOVE 1 TO IND
                        GO TO INC-006
                ELSE
                    MOVE ALFATP2 TO UFEXTENSO.
                DISPLAY TELACEP.
      *
       INC-008.

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
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                        MOVE "*** DADOS GRAVADOS *** " TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-001.
                IF ST-ERRO = "22"
                        MOVE "*** CEP JA EXISTE *** " TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-001
                ELSE
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADCEP" TO MENS
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
                DELETE CADCEP RECORD
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
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                    MOVE "*** REGISTRO ALTERADO *** " TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CEP" TO MENS
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
