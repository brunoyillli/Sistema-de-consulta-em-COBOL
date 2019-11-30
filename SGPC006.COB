       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGPC006.
       AUTHOR. Bruno Mendes Carvalho.
      *******************************
      *     CONSULTA DE PEDIDOS     *
      *******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT CADPED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PEDIDO
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADPED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPED.DAT".
       01 REGVEN.
                03 PEDIDO.
                     07 OCC00101 PIC 9(06).
                03 CODCLI.
                     07 OCC00102 PIC 9(11).
                03 CODPRODUTO.
                     07 OCC00103 PIC 9(05).
                03 PRECOCUSTO.
                     07 OCC00104  PIC 9(05)V99.
                03 QUANTIDADE.
                     07 OCC00105  PIC 9(05)V99.
                03 VALOR.
                     07 OCC00106  PIC 9(09)V99.
                03 FILLER           PIC X(32).
      *
      *****************************************************************
      *SEQ.*  NOME        *   DENOMINACAO              *TIPO * TAMANHO*
      *****************************************************************
      * 01 * OCC00101     * CODCLI                     *  N  *   11   *
      * 02 * OCC00102     * CODPRODUTO                 *  N  *   05   *
      * 03 * OCC00103     * NUMVENDA                   *  N  *   04   *
      * 04 * OCC00104     * QUANTIDADE                 *  N  *   03   *
      * 05 * OCC00105     * VALORVENDA                 *  N  *   06   *
      * 06 * OCC00106     * LUCRO                      *  N  *   06   *
      * 13 * FILLER       *                            *  N  *   21   *
      *****************************************************************
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 W-CADPED     PIC X(40) VALUE "CADPED.DAT                    ".
       01 CONLIN       PIC 9(03) VALUE 001.
       01 OCW00001     PIC 9(11) VALUE ZEROS.
       01 OCW00002     PIC 9(05) VALUE ZEROS.
       01 OCW00003     PIC 9(04) VALUE ZEROS.
       01 OCW00004     PIC 9(02) VALUE ZEROS.
       01 OCW00005     PIC 9(04) VALUE ZEROS.
       01 OCW00006     PIC 9(04) VALUE ZEROS.
       01 OCW00007     PIC 9(15) VALUE ZEROS.
       01 W-EMPRESA    PIC X(20) VALUE "FATEC ZONA LESTE".
      *-----------------------------------------------------------------
      *
      ******************
      * REPORT SECTION *
      ******************
      *
       01 DET2.
          03 FILLER    PIC X(04)    VALUE SPACES.
          03 DET21     PIC 9(06)    VALUE ZEROS.
          03 FILLER    PIC X(03)    VALUE SPACES.
          03 DET22     PIC 9(11)    VALUE ZEROS.
          03 FILLER    PIC X(05)    VALUE SPACES.
          03 DET23     PIC 9(05)    VALUE ZEROS.
          03 FILLER    PIC X(06)    VALUE SPACES.
          03 DET24     PIC 9(05)    VALUE ZEROS.
          03 FILLER    PIC X(04)    VALUE SPACES.
          03 DET25     PIC ZZ.ZZ9,99 VALUE ZEROS.
          03 FILLER    PIC X(06)    VALUE SPACES.
          03 DET26     PIC ZZ.ZZZ9,99 VALUE ZEROS.
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  TELASGP006.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                         CONSULTA DE PEDI".
           05  LINE 02  COLUMN 41
               VALUE  "DOS".
           05  LINE 04  COLUMN 01
               VALUE  "   PEDIDO     CLIENTE     PRODUTO   PRECO    QU".
           05  LINE 04  COLUMN 41
               VALUE  "ANT     VALOR ".
           05  LINE 05  COLUMN 01
               VALUE  "     -----------   -----     ----      -".
           05  LINE 05  COLUMN 41
               VALUE  "--       ------        ------".


           05  TELACVEN01
               LINE 06  COLUMN 06  PIC 9(11)
               USING  OCW00001.
           05  TELACVEN02
               LINE 06  COLUMN 20  PIC 9(05)
               USING  OCW00002.
           05  TELACVEN03
               LINE 06  COLUMN 30  PIC 9(04)
               USING  OCW00003.
           05  TELACVEN04
               LINE 06  COLUMN 40  PIC 9(03)
               USING  OCW00004.
           05  TELACVEN05
               LINE 06  COLUMN 46  PIC Z.ZZ9,99
               USING  OCW00005.
           05  TELACVEN06
               LINE 06  COLUMN 60  PIC Z.ZZ9,99
               USING  OCW00006.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       INC-OP1.
           OPEN INPUT CADPED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CENTROS DE CUSTOS NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO  " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
           ELSE
               NEXT SENTENCE.
      *
       INC-001.
           DISPLAY  TELASGP006.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA  "
           ACCEPT TELACVEN01
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADPED
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE OCW00001 TO OCC00101
           START CADPED KEY IS NOT LESS PEDIDO INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ CADPED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE ST-ERRO TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE OCC00101 TO DET21
           MOVE OCC00102 TO DET22
           MOVE OCC00103 TO DET23
           MOVE OCC00104 TO DET24
           MOVE OCC00105 TO DET25
           MOVE OCC00106 TO DET26
           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 02) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.
      *
       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELASGP006
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  MOVE ZEROS TO OCW00001
                  MOVE ZEROS TO OCW00002
                  MOVE ZEROS TO OCW00003
                  MOVE ZEROS TO OCW00004
                  MOVE ZEROS TO OCW00005
                  MOVE ZEROS TO OCW00006
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADPED.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *
      *---------------------*** FIM DE PROGRAMA ***--------------------*
