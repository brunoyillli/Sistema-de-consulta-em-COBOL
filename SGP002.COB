       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGP002.
       AUTHOR. Bruno Mendes Carvalho
      ****************************************
      *   MANUTENCAO DO CADASTRO DE PRODUTO  *
      ****************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                          DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADPRO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CODIGO
                    FILE STATUS IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = DESCRICAO
                                                      WITH DUPLICATES.
      *
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPRO
             LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS "CADPRO.DAT".
       01 REGPRO.
                03 CODIGO           PIC 9(05).
                03 DESCRICAO        PIC X(30).
                03 UNIDADE          PIC X(02).
                03 UNIDEXTENSO      PIC X(18).
                03 PRECOCUSTO       PIC 9(06)V99.
                03 PRECOVENDA       PIC 9(06)V99.
                03 TIPO             PIC 9(01).
                03 STATOS           PIC X(01).
      *
      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 MASC1           PIC ZZZ.ZZ9,99.
       01 MASC2           PIC ZZZZ.ZZZ.ZZ9,99.
       77 W-SEL           PIC 9(01) VALUE ZEROS.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 ST-ERRO         PIC X(02) VALUE "00".
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES.
       01 IND             PIC 9(02) VALUE ZEROS.
       01 CLASSEXTENSO    PIC X(19) VALUE SPACES.
       01 STATOSEXTENSO PIC X(09) VALUE SPACES.

      *
       01 TABUNIDADE.
          03 T1   PIC X(20) VALUE "PCPECA              ".
          03 T2   PIC X(20) VALUE "LTLITRO             ".
          03 T3   PIC X(20) VALUE "LALATA              ".
          03 T4   PIC X(20) VALUE "P2PET 2 LITROS      ".
          03 T5   PIC X(20) VALUE "P6PET 600           ".
          03 T6   PIC X(20) VALUE "CXCAIXA             ".
          03 T7   PIC X(20) VALUE "UNUNIDADE           ".
          03 T8   PIC X(20) VALUE "CPCOPO              ".
          03 T9   PIC X(20) VALUE "DZDUZIA             ".
          03 T10  PIC X(20) VALUE "PSPESO              ".


       01 TABUNAUX REDEFINES TABUNIDADE.
          03 TABUN   PIC X(20) OCCURS 10 TIMES.
       01 ALFAUN.
             03 ALFAUN1 PIC X(02).
             03 ALFAUN2 PIC X(18).

       01 TABCLASSIFICAO.
          03 T1   PIC X(20) VALUE "1ASSADO             ".
          03 T2   PIC X(20) VALUE "2FRITO              ".
          03 T3   PIC X(20) VALUE "3REFRIGERANTE       ".
          03 T4   PIC X(20) VALUE "4CERVEJA            ".
          03 T5   PIC X(20) VALUE "5DOCE               ".
          03 T6   PIC X(20) VALUE "6SALGADO            ".
          03 T7   PIC X(20) VALUE "7SUCO               ".
          03 T8   PIC X(20) VALUE "8CALDO DE CANA      ".
          03 T9   PIC X(20) VALUE "9SALGADINHO         ".

       01 TABCLAUX REDEFINES TABCLASSIFICAO.
          03 TABCL   PIC X(20) OCCURS 9 TIMES.
       01 ALFACL.
             03 ALFACL1 PIC X(01).
             03 ALFACL2 PIC X(19).

       01 TABSTATOS.
          03 T1   PIC X(10) VALUE "AATIVO    ".
          03 T2   PIC X(10) VALUE "CCANCELADO".
          03 T3   PIC X(10) VALUE "DDELETADO ".
          03 T4   PIC X(10) VALUE "SSUSPENSO ".

       01 TABSITAUX REDEFINES TABSTATOS.
          03 TABSIT   PIC X(10) OCCURS 4 TIMES.
       01 ALFASIT.
             03 ALFASIT1 PIC X(01).
             03 ALFASIT2 PIC X(09).
      *


       SCREEN SECTION.
        01  TELAPROD.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01
               VALUE  "                           CADASTRO DE P".
           05  LINE 03  COLUMN 41
               VALUE  "RODUTO".
           05  LINE 05  COLUMN 01
               VALUE  "   CODIGO".
           05  LINE 07  COLUMN 01
               VALUE  "   DESCRICAO".
           05  LINE 09  COLUMN 01
               VALUE  "   UNIDADE".
           05  LINE 11  COLUMN 01
               VALUE  "   PRECO CUSTO".
           05  LINE 13  COLUMN 01
               VALUE  "   PRECO VENDA".
           05  LINE 15  COLUMN 01
               VALUE  "   TIPO".
           05  LINE 17  COLUMN 01
               VALUE  "   STATOS".
           05  TCODIGO
               LINE 05  COLUMN 17  PIC 9(05)
               USING  CODIGO
               HIGHLIGHT.
           05  TDESCRICAO
               LINE 07  COLUMN 17  PIC X(30)
               USING  DESCRICAO
               HIGHLIGHT.
           05  TUNIDADE
               LINE 09  COLUMN 17  PIC X(02)
               USING  UNIDADE
               HIGHLIGHT.
           05  TUNIDEX
               LINE 09  COLUMN 22  PIC X(18)
               USING  UNIDEXTENSO
               HIGHLIGHT.
           05  TPRECOCUSTO
               LINE 11  COLUMN 17  PIC ZZZ.ZZ9,99
               USING  PRECOCUSTO
               HIGHLIGHT.
           05  TPRECOVENDA
               LINE 13  COLUMN 17  PIC ZZZ.ZZ9,99
               USING  PRECOVENDA
               HIGHLIGHT.
           05  TTIPO
               LINE 15  COLUMN 18  PIC 9(01)
               USING TIPO
               HIGHLIGHT.
           05  TCLASSEXT
               LINE 15  COLUMN 23  PIC X(19)
               USING  CLASSEXTENSO
               HIGHLIGHT.
           05  TSTATOS
               LINE 17  COLUMN 17  PIC X(01)
               USING  STATOS
               HIGHLIGHT.
           05  TSTATOSEX
               LINE 17  COLUMN 22  PIC X(09)
               USING  STATOSEXTENSO
               HIGHLIGHT.

      *----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADPRO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                    OPEN OUTPUT CADPRO
                    CLOSE CADPRO
                    MOVE "*** ARQUIVO CADPRO SENDO CRIADO ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO INC-OP0
              ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADPRO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
              ELSE
                    NEXT SENTENCE.
       INC-001.
              MOVE SPACES TO DESCRICAO UNIDADE UNIDEXTENSO
              CLASSEXTENSO STATOSEXTENSO STATOS.
              MOVE ZEROS TO CODIGO PRECOCUSTO PRECOVENDA
              TIPO.
              DISPLAY TELAPROD.
       INC-002.
              ACCEPT TCODIGO
              ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02
                 CLOSE CADPRO
                 GO TO ROT-FIM.
               IF CODIGO = " "
                  MOVE "*** CODIGOCO INVALIDO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INC-002.
       LER-CADPRO001.
               MOVE 0 TO W-SEL
               READ CADPRO
               IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                    DISPLAY TELAPROD
                    PERFORM INC-005 THRU INC-005
                    PERFORM INC-012 THRU INC-012
                    PERFORM INC-009 THRU INC-009
                    PERFORM INC-012 THRU INC-012
                    PERFORM INC-011 THRU INC-011
                    MOVE "*** PRODUTO JA CADASTRADO***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ACE-001
                   ELSE
                     MOVE "ERRO NA LEITURA ARQ. CADPRO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-003.
                ACCEPT TDESCRICAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TUNIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.

       INC-005.
                MOVE TABUN(IND) TO ALFAUN
                IF UNIDADE NOT = ALFAUN1
                    IF IND < 11
                        ADD 1 TO IND
                        GO TO INC-005
                    ELSE
                        MOVE SPACES TO UNIDEXTENSO
                        MOVE "UNIDADE INVALIDA!" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        MOVE 1 TO IND
                        GO TO INC-004
                ELSE
                    MOVE ALFAUN2 TO UNIDEXTENSO.
                    DISPLAY TELAPROD.
       INC-006.
                ACCEPT TPRECOCUSTO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TPRECOVENDA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TTIPO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                MOVE 1 TO IND.
       INC-009.
               MOVE TABCL(IND) TO ALFACL
               IF TIPO NOT = ALFACL1
                    IF IND < 11
                         ADD 1 TO IND
                         GO TO INC-009
                    ELSE
                        MOVE SPACES TO CLASSEXTENSO
                        MOVE "TIPO INVALIDA!" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        MOVE 1 TO IND
                        GO TO INC-008
                ELSE
                    MOVE ALFACL2 TO CLASSEXTENSO.
                DISPLAY TELAPROD.
       INC-010.
                ACCEPT TSTATOS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                MOVE 1 TO IND.
       INC-011.
                MOVE TABSIT(IND) TO ALFASIT
               IF STATOS NOT = ALFASIT1
                    IF IND < 5
                         ADD 1 TO IND
                         GO TO INC-011
                    ELSE
                        MOVE SPACES TO STATOSEXTENSO
                        MOVE "STATOS INVALIDA!" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        MOVE 1 TO IND
                        GO TO INC-010
                ELSE
                    MOVE ALFASIT2 TO STATOSEXTENSO.
                DISPLAY TELAPROD.
       INC-012.
               MOVE 1 TO IND.

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
                WRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                        MOVE "*** DADOS GRAVADOS *** " TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-001.
                IF ST-ERRO = "22"
                        MOVE "*** PRODUTO JA EXISTE *** " TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO INC-001
                ELSE
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADPRO" TO MENS
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
                   MOVE 01 TO W-SEL
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
                DELETE CADPRO RECORD
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
                REWRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                    MOVE "*** REGISTRO ALTERADO *** " TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO" TO MENS
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
