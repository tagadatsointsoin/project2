       IDENTIFICATION DIVISION.                                         CI0107
       PROGRAM-ID.  CI0107P.                                            CI0107
      *AUTHOR.         CDSC WAIVER DETERMINATION.                       CI0107
      *DATE-COMPILED.   09/08/14.                                       CI0107
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1998                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CATS   SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CATS   SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CATS         *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 1998                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0107
       CONFIGURATION SECTION.                                           CI0107
       SOURCE-COMPUTER. IBM-370.                                        CI0107
       OBJECT-COMPUTER. IBM-370.                                        CI0107
       DATA DIVISION.                                                   CI0107
       WORKING-STORAGE SECTION.                                         CI0107
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0107
            10            XW05-XW06.                                    CI0107
            11            XW05-XDBPCB.                                  CI0107
            12            XW05-XDBDNM PICTURE  X(08)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-XSEGLV PICTURE  X(02)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-XRC    PICTURE  X(02)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-XPROPT PICTURE  X(04)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-FILLER PICTURE  S9(5)                    CI0107
                          VALUE                ZERO                     CI0107
                          BINARY.                                       CI0107
            12            XW05-XSEGNM PICTURE  X(08)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0107
                          VALUE                ZERO                     CI0107
                          BINARY.                                       CI0107
            12            XW05-XSEGNB PICTURE  9(05)                    CI0107
                          VALUE                ZERO                     CI0107
                          BINARY.                                       CI0107
            12            XW05-XCOKEY PICTURE  X(70)                    CI0107
                          VALUE                SPACE.                   CI0107
            10            XW05-XW07.                                    CI0107
            11            XW05-XIOPCB.                                  CI0107
            12            XW05-XTERMI PICTURE  X(08)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-FILLER PICTURE  XX                       CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-XRC1   PICTURE  X(02)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-FILLER PICTURE  X(12)                    CI0107
                          VALUE                SPACE.                   CI0107
            12            XW05-XMODNM PICTURE  X(8)                     CI0107
                          VALUE                SPACE.                   CI0107
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0107
                          VALUE                ZERO.                    CI0107
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0107
                          VALUE                ZERO.                    CI0107
            10            XW05-XGU    PICTURE  X(4)                     CI0107
                          VALUE                'GU  '.                  CI0107
            10            XW05-XGHU   PICTURE  X(4)                     CI0107
                          VALUE                'GHU '.                  CI0107
            10            XW05-XGN    PICTURE  X(4)                     CI0107
                          VALUE                'GN  '.                  CI0107
            10            XW05-XGHN   PICTURE  X(4)                     CI0107
                          VALUE                'GHN '.                  CI0107
            10            XW05-XGNP   PICTURE  X(4)                     CI0107
                          VALUE                'GNP '.                  CI0107
            10            XW05-XGHNP  PICTURE  X(4)                     CI0107
                          VALUE                'GHNP'.                  CI0107
            10            XW05-XREPL  PICTURE  XXXX                     CI0107
                          VALUE                'REPL'.                  CI0107
            10            XW05-XISRT  PICTURE  X(4)                     CI0107
                          VALUE                'ISRT'.                  CI0107
            10            XW05-XDLET  PICTURE  X(4)                     CI0107
                          VALUE                'DLET'.                  CI0107
            10            XW05-XOPEN  PICTURE  X(4)                     CI0107
                          VALUE                'OPEN'.                  CI0107
            10            XW05-XCLSE  PICTURE  X(4)                     CI0107
                          VALUE                'CLSE'.                  CI0107
            10            XW05-XCHKP  PICTURE  X(4)                     CI0107
                          VALUE                'CHKP'.                  CI0107
            10            XW05-XXRST  PICTURE  X(4)                     CI0107
                          VALUE                'XRST'.                  CI0107
            10            XW05-XTERM  PICTURE  X(4)                     CI0107
                          VALUE                'TERM'.                  CI0107
            10            XW05-XNFPAC PICTURE  X(13)                    CI0107
                          VALUE                SPACE.                   CI0107
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0107
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0107

      *SS01 SEGMENT (SHARK PRODUCT RULES DATABASE ROOT) FOR 'FROM' ACCT
      *!WF DSP=FR DSL=SS SEL=01 FOR=I DES=1 LEV=1 PLT=FR
       01                 FR01.                                         CI0107
            10            FR01-SS01K.                                   CI0107
            11            FR01-CTIDA  PICTURE  9(3).                    CI0107
            11            FR01-PRCOD  PICTURE  9(5).                    CI0107
            11            FR01-CPRSCN PICTURE  9(9).                    CI0107
            10            FR01-PRCODI PICTURE  9(5).                    CI0107
            10            FR01-PRCODS PICTURE  9(5).                    CI0107
            10            FR01-CLORN1 PICTURE  X(45).                   CI0107
            10            FR01-CLORN2 PICTURE  X(45).                   CI0107
            10            FR01-MFND1  PICTURE  X(22).                   CI0107
            10            FR01-MFND2  PICTURE  X(14).                   CI0107
            10            FR01-MFND3  PICTURE  XXX.                     CI0107
            10            FR01-DS01.                                    CI0107
            11            FR01-NCUSP  PICTURE  X(06).                   CI0107
            11            FR01-CCUSP  PICTURE  XX.                      CI0107
            11            FR01-GECKD  PICTURE  9.                       CI0107
            11            FR01-UCUSP  PICTURE  XX.                      CI0107
            10            FR01-DS22                                     CI0107
                          REDEFINES            FR01-DS01.               CI0107
            11            FR01-CCSIP  PICTURE  X(09).                   CI0107
            11            FR01-CCUSP1 PICTURE  XX.                      CI0107
            10            FR01-DPRDA  PICTURE  9(8).                    CI0107
            10            FR01-DPRDI  PICTURE  9(8).                    CI0107
            10            FR01-DPRDT  PICTURE  9(8).                    CI0107
            10            FR01-CTYFI  PICTURE  9(02).                   CI0107
            10            FR01-GEFYE  PICTURE  9(4).                    CI0107
            10            FR01-APARS  PICTURE  99V9(7)                  CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            FR01-IASSC  PICTURE  X.                       CI0107
            10            FR01-IASSS  PICTURE  X.                       CI0107
            10            FR01-IRPUR  PICTURE  X.                       CI0107
            10            FR01-IRACC  PICTURE  X.                       CI0107
            10            FR01-IDDIV  PICTURE  X.                       CI0107
            10            FR01-IASSD  PICTURE  X.                       CI0107
            10            FR01-IDIVL  PICTURE  X.                       CI0107
            10            FR01-IDPAS  PICTURE  X.                       CI0107
            10            FR01-IDTEX  PICTURE  X.                       CI0107
            10            FR01-IFEDF  PICTURE  X.                       CI0107
            10            FR01-QFOSPD PICTURE  9(2).                    CI0107
            10            FR01-CLDTY  PICTURE  XX.                      CI0107
            10            FR01-QPRCF  PICTURE  9.                       CI0107
            10            FR01-QDMBR  PICTURE  99.                      CI0107
            10            FR01-CMBEM  PICTURE  9.                       CI0107
            10            FR01-AMWIP  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            FR01-AMBIC  PICTURE  9(5)V99                  CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            FR01-ARMSV  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            FR01-QDRVP  PICTURE  99.                      CI0107
            10            FR01-NIDTB  PICTURE  9(5).                    CI0107
            10            FR01-CRTBL  PICTURE  99.                      CI0107
            10            FR01-CSTLD  PICTURE  X.                       CI0107
            10            FR01-IFOFD  PICTURE  X.                       CI0107
            10            FR01-IREFD  PICTURE  X.                       CI0107
            10            FR01-FILLER PICTURE  X(2).                    CI0107
            10            FR01-AEFEE  PICTURE  S9(3)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            FR01-GEMIN                                    CI0107
                          OCCURS       005     TIMES.                   CI0107
            11            FR01-COOBF  PICTURE  9(2).                    CI0107
            11            FR01-AMTRO  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            FR01-AMDER  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            FR01-IFTDY  PICTURE  X.                       CI0107

      *---------------------------------------------------------------- ADU102
      *>>>>> Multiple Informational Messages                            ADU102
      *---------------------------------------------------------------- ADU102
      *      The following element causes subscripts to be generated    ADU102
      *      which are used when adding messages to the PJ11 (MX11)     ADU102
      *      linkage segment.                                           ADU102
      *                                                                 ADU102
       01                 MX11-INDEX      PIC X(01).                    ADU102
      *                                                                 ADU102
      *                                                                 ADU102
       01                 SS01.                                         CI0107
            10            SS01-SS01K.                                   CI0107
            11            SS01-CTIDA  PICTURE  9(3).                    CI0107
            11            SS01-PRCOD  PICTURE  9(5).                    CI0107
            11            SS01-CPRSCN PICTURE  9(9).                    CI0107
            10            SS01-PRCODI PICTURE  9(5).                    CI0107
            10            SS01-PRCODS PICTURE  9(5).                    CI0107
            10            SS01-CLORN1 PICTURE  X(45).                   CI0107
            10            SS01-CLORN2 PICTURE  X(45).                   CI0107
            10            SS01-MFND1  PICTURE  X(22).                   CI0107
            10            SS01-MFND2  PICTURE  X(14).                   CI0107
            10            SS01-MFND3  PICTURE  XXX.                     CI0107
            10            SS01-DS01.                                    CI0107
            11            SS01-NCUSP  PICTURE  X(06).                   CI0107
            11            SS01-CCUSP  PICTURE  XX.                      CI0107
            11            SS01-GECKD  PICTURE  9.                       CI0107
            11            SS01-UCUSP  PICTURE  XX.                      CI0107
            10            SS01-DS22                                     CI0107
                          REDEFINES            SS01-DS01.               CI0107
            11            SS01-CCSIP  PICTURE  X(09).                   CI0107
            11            SS01-CCUSP1 PICTURE  XX.                      CI0107
            10            SS01-DPRDA  PICTURE  9(8).                    CI0107
            10            SS01-DPRDI  PICTURE  9(8).                    CI0107
            10            SS01-DPRDT  PICTURE  9(8).                    CI0107
            10            SS01-CTYFI  PICTURE  9(02).                   CI0107
            10            SS01-GEFYE  PICTURE  9(4).                    CI0107
            10            SS01-APARS  PICTURE  99V9(7)                  CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            SS01-IASSC  PICTURE  X.                       CI0107
            10            SS01-IASSS  PICTURE  X.                       CI0107
            10            SS01-IRPUR  PICTURE  X.                       CI0107
            10            SS01-IRACC  PICTURE  X.                       CI0107
            10            SS01-IDDIV  PICTURE  X.                       CI0107
            10            SS01-IASSD  PICTURE  X.                       CI0107
            10            SS01-IDIVL  PICTURE  X.                       CI0107
            10            SS01-IDPAS  PICTURE  X.                       CI0107
            10            SS01-IDTEX  PICTURE  X.                       CI0107
            10            SS01-IFEDF  PICTURE  X.                       CI0107
            10            SS01-QFOSPD PICTURE  9(2).                    CI0107
            10            SS01-CLDTY  PICTURE  XX.                      CI0107
            10            SS01-QPRCF  PICTURE  9.                       CI0107
            10            SS01-QDMBR  PICTURE  99.                      CI0107
            10            SS01-CMBEM  PICTURE  9.                       CI0107
            10            SS01-AMWIP  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            SS01-AMBIC  PICTURE  9(5)V99                  CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            SS01-ARMSV  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            SS01-QDRVP  PICTURE  99.                      CI0107
            10            SS01-NIDTB  PICTURE  9(5).                    CI0107
            10            SS01-CRTBL  PICTURE  99.                      CI0107
            10            SS01-CSTLD  PICTURE  X.                       CI0107
            10            SS01-IFOFD  PICTURE  X.                       CI0107
            10            SS01-IREFD  PICTURE  X.                       CI0107
            10            SS01-FILLER PICTURE  X(2).                    CI0107
            10            SS01-AEFEE  PICTURE  S9(3)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            SS01-GEMIN                                    CI0107
                          OCCURS       005     TIMES.                   CI0107
            11            SS01-COOBF  PICTURE  9(2).                    CI0107
            11            SS01-AMTRO  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            SS01-AMDER  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            SS01-IFTDY  PICTURE  X.                       CI0107

      *SS01 SEGMENT (SHARK PRODUCT RULES DATABASE ROOT) FOR 'TO' ACCT
      *IN TRANSFER SITUATION
      *!WF DSP=TO DSL=SS SEL=01 FOR=I DES=1 LEV=1 PLT=TO
       01                 TO01.                                         CI0107
            10            TO01-SS01K.                                   CI0107
            11            TO01-CTIDA  PICTURE  9(3).                    CI0107
            11            TO01-PRCOD  PICTURE  9(5).                    CI0107
            11            TO01-CPRSCN PICTURE  9(9).                    CI0107
            10            TO01-PRCODI PICTURE  9(5).                    CI0107
            10            TO01-PRCODS PICTURE  9(5).                    CI0107
            10            TO01-CLORN1 PICTURE  X(45).                   CI0107
            10            TO01-CLORN2 PICTURE  X(45).                   CI0107
            10            TO01-MFND1  PICTURE  X(22).                   CI0107
            10            TO01-MFND2  PICTURE  X(14).                   CI0107
            10            TO01-MFND3  PICTURE  XXX.                     CI0107
            10            TO01-DS01.                                    CI0107
            11            TO01-NCUSP  PICTURE  X(06).                   CI0107
            11            TO01-CCUSP  PICTURE  XX.                      CI0107
            11            TO01-GECKD  PICTURE  9.                       CI0107
            11            TO01-UCUSP  PICTURE  XX.                      CI0107
            10            TO01-DS22                                     CI0107
                          REDEFINES            TO01-DS01.               CI0107
            11            TO01-CCSIP  PICTURE  X(09).                   CI0107
            11            TO01-CCUSP1 PICTURE  XX.                      CI0107
            10            TO01-DPRDA  PICTURE  9(8).                    CI0107
            10            TO01-DPRDI  PICTURE  9(8).                    CI0107
            10            TO01-DPRDT  PICTURE  9(8).                    CI0107
            10            TO01-CTYFI  PICTURE  9(02).                   CI0107
            10            TO01-GEFYE  PICTURE  9(4).                    CI0107
            10            TO01-APARS  PICTURE  99V9(7)                  CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            TO01-IASSC  PICTURE  X.                       CI0107
            10            TO01-IASSS  PICTURE  X.                       CI0107
            10            TO01-IRPUR  PICTURE  X.                       CI0107
            10            TO01-IRACC  PICTURE  X.                       CI0107
            10            TO01-IDDIV  PICTURE  X.                       CI0107
            10            TO01-IASSD  PICTURE  X.                       CI0107
            10            TO01-IDIVL  PICTURE  X.                       CI0107
            10            TO01-IDPAS  PICTURE  X.                       CI0107
            10            TO01-IDTEX  PICTURE  X.                       CI0107
            10            TO01-IFEDF  PICTURE  X.                       CI0107
            10            TO01-QFOSPD PICTURE  9(2).                    CI0107
            10            TO01-CLDTY  PICTURE  XX.                      CI0107
            10            TO01-QPRCF  PICTURE  9.                       CI0107
            10            TO01-QDMBR  PICTURE  99.                      CI0107
            10            TO01-CMBEM  PICTURE  9.                       CI0107
            10            TO01-AMWIP  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            TO01-AMBIC  PICTURE  9(5)V99                  CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            TO01-ARMSV  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            TO01-QDRVP  PICTURE  99.                      CI0107
            10            TO01-NIDTB  PICTURE  9(5).                    CI0107
            10            TO01-CRTBL  PICTURE  99.                      CI0107
            10            TO01-CSTLD  PICTURE  X.                       CI0107
            10            TO01-IFOFD  PICTURE  X.                       CI0107
            10            TO01-IREFD  PICTURE  X.                       CI0107
            10            TO01-FILLER PICTURE  X(2).                    CI0107
            10            TO01-AEFEE  PICTURE  S9(3)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            TO01-GEMIN                                    CI0107
                          OCCURS       005     TIMES.                   CI0107
            11            TO01-COOBF  PICTURE  9(2).                    CI0107
            11            TO01-AMTRO  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            TO01-AMDER  PICTURE  S9(5)V99                 CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            TO01-IFTDY  PICTURE  X.                       CI0107

      ******************************************************************
      **  MISCELLANEOUS WORK AREA                                      *
      ******************************************************************

      *    THIS WORK AREA IS FOR THE DESTINATION ACCOUNT NUMBER
      *    AND EXISTS SO WE DON'T HAVE TO PASS BOTH THE ACCT NUMBER AND
      *    THE ADMIN CODE IN THE LINKAGE AREA.
       01  WORK-CTID01.
      *!WI
           05 WORK-CTIDA1
                        PICTURE 9(3).                                   CI0107
           05 FILLER    PIC X(24).

       01   DEBUT-WSS.                                                  CI0107
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0107
            05   IK     PICTURE X.                                      CI0107
       01  CONSTANTES-PAC.                                              CI0107
           05  FILLER  PICTURE X(87)   VALUE                            CI0107
                     '6015 CAT09/08/14CI0107ADMIN   14:34:48CI0107P AMERCI0107
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0107
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0107
           05  NUGNA   PICTURE X(5).                                    CI0107
           05  APPLI   PICTURE X(3).                                    CI0107
           05  DATGN   PICTURE X(8).                                    CI0107
           05  PROGR   PICTURE X(6).                                    CI0107
           05  CODUTI  PICTURE X(8).                                    CI0107
           05  TIMGN   PICTURE X(8).                                    CI0107
           05  PROGE   PICTURE X(8).                                    CI0107
           05  COBASE  PICTURE X(4).                                    CI0107
           05  DATGNC  PICTURE X(10).                                   CI0107
           05  RELEAS  PICTURE X(7).                                    CI0107
           05  DATGE   PICTURE X(10).                                   CI0107
           05  DATSQ   PICTURE X(10).                                   CI0107
       01  DATCE.                                                       CI0107
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0107
         05  DATOR.                                                     CI0107
           10  DATOA  PICTURE XX.                                       CI0107
           10  DATOM  PICTURE XX.                                       CI0107
           10  DATOJ  PICTURE XX.                                       CI0107
       01   VARIABLES-CONDITIONNELLES.                                  CI0107
            05                  FT      PICTURE X VALUE '0'.            CI0107
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0107
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0107
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0107
            05       5-SS00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0107
       01               S-SS01-SSA.                                     CI0107
            10         S1-SS01-SEGNAM PICTURE X(8)                      CI0107
                                      VALUE 'SS01    '.                 CI0107
            10         S1-SS01-CCOM   PICTURE X VALUE '*'.              CI0107
            10          S-SS01-CCOD   PICTURE X(5)                      CI0107
                                      VALUE '-----'.                    CI0107
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0107
       01            S-SSA01-SSA.                                       CI0107
            11      S1-SSA01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSA01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSA01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSA01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(NCUSP'.                   CI0107
            11       S-SSA01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSA01-NCUSP    PICTURE  X(06).                   CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSB01-SSA.                                       CI0107
            11      S1-SSB01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSB01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSB01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSB01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(CCUSP'.                   CI0107
            11       S-SSB01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSB01-CCUSP    PICTURE  XX.                      CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSC01-SSA.                                       CI0107
            11      S1-SSC01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSC01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSC01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSC01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(GECKD'.                   CI0107
            11       S-SSC01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSC01-GECKD    PICTURE  9.                       CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSD01-SSA.                                       CI0107
            11      S1-SSD01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSD01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSD01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSD01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(UCUSP'.                   CI0107
            11       S-SSD01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSD01-UCUSP    PICTURE  XX.                      CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSE01-SSA.                                       CI0107
            11      S1-SSE01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSE01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSE01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSE01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(PRCOD'.                   CI0107
            11       S-SSE01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSE01-PRCOD    PICTURE  9(5).                    CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSF01-SSA.                                       CI0107
            11      S1-SSF01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSF01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSF01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSF01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(CPRSCN'.                  CI0107
            11       S-SSF01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSF01-CPRSCN   PICTURE  9(9).                    CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSG01-SSA.                                       CI0107
            11      S1-SSG01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSG01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSG01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSG01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(CTIDA'.                   CI0107
            11       S-SSG01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSG01-CTIDA    PICTURE  9(3).                    CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSH01-SSA.                                       CI0107
            11      S1-SSH01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            11      S1-SSH01-CCOM   PICTURE X VALUE '*'.                CI0107
            11       S-SSH01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            11      S1-SSH01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(CCSIP'.                   CI0107
            11       S-SSH01-OPER  PICTURE XX VALUE ' ='.               CI0107
            11       S-SSH01-CCSIP    PICTURE  X(09).                   CI0107
            11  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01            S-SSU01-SSA.                                       CI0107
            10      S1-SSU01-SEGNAM PICTURE X(8)                        CI0107
                                      VALUE 'SS01    '.                 CI0107
            10      S1-SSU01-CCOM   PICTURE X VALUE '*'.                CI0107
            10       S-SSU01-CCOD   PICTURE X(5)                        CI0107
                                      VALUE '-----'.                    CI0107
            10      S1-SSU01-FLDNAM PICTURE X(9)                        CI0107
                                      VALUE '(SS01K'.                   CI0107
            10       S-SSU01-OPER  PICTURE XX VALUE ' ='.               CI0107
            10       S-SSU01-SS01K.                                     CI0107
            11       S-SSU01-CTIDA    PICTURE  9(3).                    CI0107
            11       S-SSU01-PRCOD    PICTURE  9(5).                    CI0107
            11       S-SSU01-CPRSCN   PICTURE  9(9).                    CI0107
            10  FILLER   PICTURE X    VALUE ')'.                        CI0107
       01   ZONES-UTILISATEUR PICTURE X.                                CI0107
       LINKAGE SECTION.                                                 ADU102
      *>>>>>>   DLIUIBII Copybook                                       ADU129
            COPY  DLIUIBII.                                             ADU129
      *                                                                 ADU129
      *>>>>>> Address list of PCB's                                     ADU129
      *                                                                 ADU129
       01   PCB-ADDRESS-LIST.                                           ADU129
      *                                                                 ADU129
      *NOTE: All PCB pointers must be added here using macro ADU015     ADU129
      *      once for each database used                                ADU129
      *                                                                 ADU129
      *NOTE: Following PCB pointers, include PCB masks using ADU015     ADU129
      *      once for each database used                                ADU129
      *                                                                 ADU129
      *-----------------------------------------------------------------ADU129
      ** PCB POINTER FOR SSSP                                           ADU015
            05 PCB-SSSP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR SSSP                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0107
          05              PA00-SUITE.                                   CI0107
            15       FILLER         PICTURE  X(00106).                  CI0107
       01                 PA06  REDEFINES      PA00.                    CI0107
            10            PA06-XDBPCB.                                  CI0107
            11            PA06-XDBDNM PICTURE  X(08).                   CI0107
            11            PA06-XSEGLV PICTURE  X(02).                   CI0107
            11            PA06-XRC    PICTURE  X(02).                   CI0107
            11            PA06-XPROPT PICTURE  X(04).                   CI0107
            11            PA06-FILLER PICTURE  S9(5)                    CI0107
                          BINARY.                                       CI0107
            11            PA06-XSEGNM PICTURE  X(08).                   CI0107
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0107
                          BINARY.                                       CI0107
            11            PA06-XSEGNB PICTURE  9(05)                    CI0107
                          BINARY.                                       CI0107
            11            PA06-XCOKEY PICTURE  X(70).                   CI0107

      *PASS AREA TO/FROM CI0107
      *!WF DSP=CD DSL=PJ SEL=57 FOR=I DES=1 LEV=1 PLT=50
       01                 CD57.                                         CI0107
            10            CD57-C299.                                    CI0107
            11            CD57-CTID.                                    CI0107
            12            CD57-CTIDA  PICTURE  9(3).                    CI0107
            12            CD57-CTIDN.                                   CI0107
            13            CD57-CTIDNP PICTURE  X(13).                   CI0107
            13            CD57-CTIDND PICTURE  9(11).                   CI0107
            10            CD57-PRCOD  PICTURE  9(5).                    CI0107
            10            CD57-PRSCD  PICTURE  X(9).                    CI0107
            10            CD57-CTCCI  PICTURE  X.                       CI0107
            10            CD57-CPAYF  PICTURE  X(2).                    CI0107
            10            CD57-CLTYP  PICTURE  X.                       CI0107
            10            CD57-CLDTH  PICTURE  X.                       CI0107
            10            CD57-CTID01 PICTURE  X(27).                   CI0107
            10            CD57-PRCOD1 PICTURE  9(5).                    CI0107
            10            CD57-CPRSC1 PICTURE  X(9).                    CI0107
            10            CD57-CTCCIA PICTURE  X.                       CI0107
            10            CD57-QCLAGE PICTURE  9(3)V9                   CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            CD57-IDRMD  PICTURE  X.                       CI0107
            10            CD57-FILLER PICTURE  X(95).                   CI0107
            10            CD57-ICDSC  PICTURE  X.                       CI0107
            10            CD57-ICDSU  PICTURE  X.                       CI0107
            10            CD57-CCDSCW PICTURE  9(2).                    CI0107


      *CI0074 PASS AREA (CONTAINS TAX SETTLEMENT CODE)
      *!WF DSP=PJ DSL=PJ SEL=15 FOR=I DES=1 LEV=1 PLT=60
       01                 PJ15.                                         CI0107
            10            PJ15-CT99.                                    CI0107
            11            PJ15-CT99K.                                   CI0107
            12            PJ15-C299.                                    CI0107
            13            PJ15-CTID.                                    CI0107
            14            PJ15-CTIDA  PICTURE  9(3).                    CI0107
            14            PJ15-CTIDN.                                   CI0107
            15            PJ15-CTIDNP PICTURE  X(13).                   CI0107
            15            PJ15-CTIDND PICTURE  9(11).                   CI0107
            10            PJ15-CLID   PICTURE  X(23).                   CI0107
            10            PJ15-CLCUS  PICTURE  99.                      CI0107
            10            PJ15-DCACG  PICTURE  9(8).                    CI0107
            10            PJ15-CAUNIT PICTURE  X(4).                    CI0107
            10            PJ15-GEOPID PICTURE  X(6).                    CI0107
            10            PJ15-CLCUSA PICTURE  XXX.                     CI0107
            10            PJ15-MAPPN  PICTURE  X(10).                   CI0107
            10            PJ15-CUPIQ  PICTURE  X.                       CI0107
            10            PJ15-CTID01 PICTURE  X(27).                   CI0107
            10            PJ15-CTYPE  PICTURE  X.                       CI0107
            10            PJ15-CPAYF  PICTURE  X(2).                    CI0107
            10            PJ15-FILLER PICTURE  X(84).                   CI0107


      *CI0028 PASS AREA (CONTAINS SOURCE ACCT TAX MARKET)
      *!WF DSP=TM DSL=DU SEL=35 FOR=I DES=1 LEV=1 PLT=70
       01                 TM35.                                         CI0107
            10            TM35-C299.                                    CI0107
            11            TM35-CTID.                                    CI0107
            12            TM35-CTIDA  PICTURE  9(3).                    CI0107
            12            TM35-CTIDN.                                   CI0107
            13            TM35-CTIDNP PICTURE  X(13).                   CI0107
            13            TM35-CTIDND PICTURE  9(11).                   CI0107
            10            TM35-DCACG  PICTURE  9(8).                    CI0107
            10            TM35-FILLER PICTURE  X(100).                  CI0107
            10            TM35-CTXMT  PICTURE  9(2).                    CI0107
            10            TM35-CTCUS  PICTURE  999.                     CI0107
            10            TM35-CGRMF  PICTURE  X.                       CI0107
            10            TM35-GRPLC  PICTURE  99.                      CI0107
            10            TM35-GRPLT  PICTURE  99.                      CI0107
            10            TM35-FILLER PICTURE  X(092).                  CI0107


      *CI0019 PASS AREA (CONTAINS SOURCE ACCT GROUP)
      *!WF DSP=FG DSL=DU SEL=15 FOR=I DES=1 LEV=1 PLT=80
       01                 FG15.                                         CI0107
            10            FG15-C299.                                    CI0107
            11            FG15-CTID.                                    CI0107
            12            FG15-CTIDA  PICTURE  9(3).                    CI0107
            12            FG15-CTIDN.                                   CI0107
            13            FG15-CTIDNP PICTURE  X(13).                   CI0107
            13            FG15-CTIDND PICTURE  9(11).                   CI0107
            10            FG15-DCACG  PICTURE  9(8).                    CI0107
            10            FG15-IPOCH  PICTURE  X.                       CI0107
            10            FG15-FILLER PICTURE  X(100).                  CI0107
            10            FG15-DU18                                     CI0107
                          OCCURS       010     TIMES.                   CI0107
            11            FG15-CT10.                                    CI0107
            12            FG15-CT10K.                                   CI0107
            13            FG15-GR98.                                    CI0107
            14            FG15-GRID.                                    CI0107
            15            FG15-GRIDC  PICTURE  9(3).                    CI0107
            15            FG15-GRIDN.                                   CI0107
            16            FG15-GRIDNP PICTURE  99.                      CI0107
            16            FG15-GRIDND PICTURE  9(8).                    CI0107
            12            FG15-GR97                                     CI0107
                          REDEFINES            FG15-CT10K.              CI0107
            13            FG15-GRIDCB PICTURE  9(3).                    CI0107
            13            FG15-FILLER PICTURE  X(10).                   CI0107
            12            FG15-GERSD  PICTURE  9(8).                    CI0107
            12            FG15-GERED  PICTURE  9(8).                    CI0107
            12            FG15-GRCSI  PICTURE  X.                       CI0107
            11            FG15-GR01.                                    CI0107
            12            FG15-GR01K.                                   CI0107
            13            FG15-GR98.                                    CI0107
            14            FG15-GRID.                                    CI0107
            15            FG15-GRIDC  PICTURE  9(3).                    CI0107
            15            FG15-GRIDN.                                   CI0107
            16            FG15-GRIDNP PICTURE  99.                      CI0107
            16            FG15-GRIDND PICTURE  9(8).                    CI0107
            12            FG15-GECKD  PICTURE  9.                       CI0107
            12            FG15-GEMDA  PICTURE  9(8).                    CI0107
            12            FG15-NSEQ4B PICTURE  9(8)                     CI0107
                          BINARY.                                       CI0107
            12            FG15-GRDOR  PICTURE  9(8).                    CI0107
            12            FG15-GRIAD  PICTURE  9(8).                    CI0107
            12            FG15-GECUC  PICTURE  99.                      CI0107
            12            FG15-GRLNG  PICTURE  99.                      CI0107
            12            FG15-GESLC  PICTURE  99.                      CI0107
            12            FG15-AYSIDA PICTURE  9(3).                    CI0107
            12            FG15-AYSID  PICTURE  9(5).                    CI0107
            12            FG15-GRCSD  PICTURE  9(8).                    CI0107
            12            FG15-GRCFD  PICTURE  9(8).                    CI0107
            12            FG15-GRNCL  PICTURE  S9(5)                    CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            FG15-GRNCT  PICTURE  S9(5)                    CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            FG15-GRSFC  PICTURE  99.                      CI0107
            12            FG15-GRCRN  PICTURE  9(3).                    CI0107
            12            FG15-GRCSS  PICTURE  X.                       CI0107
            12            FG15-MKSRC  PICTURE  99                       CI0107
                          OCCURS       010     TIMES.                   CI0107
            12            FG15-NEFPS  PICTURE  X(5).                    CI0107
            12            FG15-DEFPS  PICTURE  9(8).                    CI0107
            12            FG15-DLSRV  PICTURE  9(8).                    CI0107
            12            FG15-CTLNI  PICTURE  X.                       CI0107
            12            FG15-CGRLI  PICTURE  X.                       CI0107
            12            FG15-CAMGR  PICTURE  9(5)                     CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            FG15-CAMGS  PICTURE  9(5)                     CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            FG15-CAMGN  PICTURE  9(3)                     CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            FG15-CGRMF  PICTURE  X.                       CI0107
            12            FG15-FILLER PICTURE  X(08).                   CI0107
            11            FG15-GR07.                                    CI0107
            12            FG15-GEDLA  PICTURE  9(8).                    CI0107
            12            FG15-GRAID  PICTURE  X(12).                   CI0107
            12            FG15-GRPAP  PICTURE  X(14).                   CI0107
            12            FG15-GEPHNX PICTURE  9(4).                    CI0107
            12            FG15-DPLEF  PICTURE  9(8).                    CI0107
            12            FG15-DPLAM  PICTURE  9(8).                    CI0107
            12            FG15-NCPFN  PICTURE  9(6).                    CI0107
            12            FG15-GEFYE  PICTURE  9(4).                    CI0107
            12            FG15-FILLER PICTURE  X(06).                   CI0107
            12            FG15-GRPAN  PICTURE  X(45).                   CI0107
            12            FG15-CGRPA  PICTURE  99.                      CI0107
            12            FG15-IPRTT7 PICTURE  X.                       CI0107
            12            FG15-GRPED  PICTURE  9(8).                    CI0107
            12            FG15-FILLER PICTURE  X(05).                   CI0107
            12            FG15-GRPLC  PICTURE  99.                      CI0107
            12            FG15-GRPLT  PICTURE  99.                      CI0107
            12            FG15-FILLER PICTURE  X(04).                   CI0107
            12            FG15-GEADI  PICTURE  X.                       CI0107
            12            FG15-GRCFA  PICTURE  S9(11)V99                CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            FG15-GECFY  PICTURE  9(4).                    CI0107
            12            FG15-GECFC  PICTURE  99.                      CI0107
            12            FG15-MEMPL  PICTURE  X(20).                   CI0107
            12            FG15-CAUNIT PICTURE  X(4).                    CI0107
            12            FG15-FILLER PICTURE  X(21).                   CI0107
            12            FG15-GRPPP  PICTURE  999.                     CI0107
            12            FG15-CCORT  PICTURE  9(3).                    CI0107
            12            FG15-CIDRP  PICTURE  99.                      CI0107
            12            FG15-CCDWA  PICTURE  9.                       CI0107
            12            FG15-IERSA  PICTURE  X.                       CI0107
            12            FG15-DERSA  PICTURE  9(8).                    CI0107
            12            FG15-FILLER PICTURE  X(04).                   CI0107
            10            FG15-QITEM  PICTURE  9(3).                    CI0107
            10            FG15-XIMAX  PICTURE  S9(4)                    CI0107
                          BINARY.                                       CI0107
            10            FG15-FILLER PICTURE  X(100).                  CI0107


      *CI0019 PASS AREA (CONTAINS DESTINATION ACCT GROUP)
      *!WF DSP=TG DSL=DU SEL=15 FOR=I DES=1 LEV=1 PLT=82
       01                 TG15.                                         CI0107
            10            TG15-C299.                                    CI0107
            11            TG15-CTID.                                    CI0107
            12            TG15-CTIDA  PICTURE  9(3).                    CI0107
            12            TG15-CTIDN.                                   CI0107
            13            TG15-CTIDNP PICTURE  X(13).                   CI0107
            13            TG15-CTIDND PICTURE  9(11).                   CI0107
            10            TG15-DCACG  PICTURE  9(8).                    CI0107
            10            TG15-IPOCH  PICTURE  X.                       CI0107
            10            TG15-FILLER PICTURE  X(100).                  CI0107
            10            TG15-DU18                                     CI0107
                          OCCURS       010     TIMES.                   CI0107
            11            TG15-CT10.                                    CI0107
            12            TG15-CT10K.                                   CI0107
            13            TG15-GR98.                                    CI0107
            14            TG15-GRID.                                    CI0107
            15            TG15-GRIDC  PICTURE  9(3).                    CI0107
            15            TG15-GRIDN.                                   CI0107
            16            TG15-GRIDNP PICTURE  99.                      CI0107
            16            TG15-GRIDND PICTURE  9(8).                    CI0107
            12            TG15-GR97                                     CI0107
                          REDEFINES            TG15-CT10K.              CI0107
            13            TG15-GRIDCB PICTURE  9(3).                    CI0107
            13            TG15-FILLER PICTURE  X(10).                   CI0107
            12            TG15-GERSD  PICTURE  9(8).                    CI0107
            12            TG15-GERED  PICTURE  9(8).                    CI0107
            12            TG15-GRCSI  PICTURE  X.                       CI0107
            11            TG15-GR01.                                    CI0107
            12            TG15-GR01K.                                   CI0107
            13            TG15-GR98.                                    CI0107
            14            TG15-GRID.                                    CI0107
            15            TG15-GRIDC  PICTURE  9(3).                    CI0107
            15            TG15-GRIDN.                                   CI0107
            16            TG15-GRIDNP PICTURE  99.                      CI0107
            16            TG15-GRIDND PICTURE  9(8).                    CI0107
            12            TG15-GECKD  PICTURE  9.                       CI0107
            12            TG15-GEMDA  PICTURE  9(8).                    CI0107
            12            TG15-NSEQ4B PICTURE  9(8)                     CI0107
                          BINARY.                                       CI0107
            12            TG15-GRDOR  PICTURE  9(8).                    CI0107
            12            TG15-GRIAD  PICTURE  9(8).                    CI0107
            12            TG15-GECUC  PICTURE  99.                      CI0107
            12            TG15-GRLNG  PICTURE  99.                      CI0107
            12            TG15-GESLC  PICTURE  99.                      CI0107
            12            TG15-AYSIDA PICTURE  9(3).                    CI0107
            12            TG15-AYSID  PICTURE  9(5).                    CI0107
            12            TG15-GRCSD  PICTURE  9(8).                    CI0107
            12            TG15-GRCFD  PICTURE  9(8).                    CI0107
            12            TG15-GRNCL  PICTURE  S9(5)                    CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            TG15-GRNCT  PICTURE  S9(5)                    CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            TG15-GRSFC  PICTURE  99.                      CI0107
            12            TG15-GRCRN  PICTURE  9(3).                    CI0107
            12            TG15-GRCSS  PICTURE  X.                       CI0107
            12            TG15-MKSRC  PICTURE  99                       CI0107
                          OCCURS       010     TIMES.                   CI0107
            12            TG15-NEFPS  PICTURE  X(5).                    CI0107
            12            TG15-DEFPS  PICTURE  9(8).                    CI0107
            12            TG15-DLSRV  PICTURE  9(8).                    CI0107
            12            TG15-CTLNI  PICTURE  X.                       CI0107
            12            TG15-CGRLI  PICTURE  X.                       CI0107
            12            TG15-CAMGR  PICTURE  9(5)                     CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            TG15-CAMGS  PICTURE  9(5)                     CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            TG15-CAMGN  PICTURE  9(3)                     CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            TG15-CGRMF  PICTURE  X.                       CI0107
            12            TG15-FILLER PICTURE  X(08).                   CI0107
            11            TG15-GR07.                                    CI0107
            12            TG15-GEDLA  PICTURE  9(8).                    CI0107
            12            TG15-GRAID  PICTURE  X(12).                   CI0107
            12            TG15-GRPAP  PICTURE  X(14).                   CI0107
            12            TG15-GEPHNX PICTURE  9(4).                    CI0107
            12            TG15-DPLEF  PICTURE  9(8).                    CI0107
            12            TG15-DPLAM  PICTURE  9(8).                    CI0107
            12            TG15-NCPFN  PICTURE  9(6).                    CI0107
            12            TG15-GEFYE  PICTURE  9(4).                    CI0107
            12            TG15-FILLER PICTURE  X(06).                   CI0107
            12            TG15-GRPAN  PICTURE  X(45).                   CI0107
            12            TG15-CGRPA  PICTURE  99.                      CI0107
            12            TG15-IPRTT7 PICTURE  X.                       CI0107
            12            TG15-GRPED  PICTURE  9(8).                    CI0107
            12            TG15-FILLER PICTURE  X(05).                   CI0107
            12            TG15-GRPLC  PICTURE  99.                      CI0107
            12            TG15-GRPLT  PICTURE  99.                      CI0107
            12            TG15-FILLER PICTURE  X(04).                   CI0107
            12            TG15-GEADI  PICTURE  X.                       CI0107
            12            TG15-GRCFA  PICTURE  S9(11)V99                CI0107
                          COMPUTATIONAL-3.                              CI0107
            12            TG15-GECFY  PICTURE  9(4).                    CI0107
            12            TG15-GECFC  PICTURE  99.                      CI0107
            12            TG15-MEMPL  PICTURE  X(20).                   CI0107
            12            TG15-CAUNIT PICTURE  X(4).                    CI0107
            12            TG15-FILLER PICTURE  X(21).                   CI0107
            12            TG15-GRPPP  PICTURE  999.                     CI0107
            12            TG15-CCORT  PICTURE  9(3).                    CI0107
            12            TG15-CIDRP  PICTURE  99.                      CI0107
            12            TG15-CCDWA  PICTURE  9.                       CI0107
            12            TG15-IERSA  PICTURE  X.                       CI0107
            12            TG15-DERSA  PICTURE  9(8).                    CI0107
            12            TG15-FILLER PICTURE  X(04).                   CI0107
            10            TG15-QITEM  PICTURE  9(3).                    CI0107
            10            TG15-XIMAX  PICTURE  S9(4)                    CI0107
                          BINARY.                                       CI0107
            10            TG15-FILLER PICTURE  X(100).                  CI0107

      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **     TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING ROUTINES. *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0107
            10            DE10-DU11.                                    CI0107
            11            DE10-XFONC  PICTURE  X(4).                    CI0107
            11            DE10-MPSBN  PICTURE  X(8).                    CI0107
            11            DE10-XDBDNM PICTURE  X(08).                   CI0107
            11            DE10-XSEGNM PICTURE  X(08).                   CI0107
            11            DE10-XRC    PICTURE  X(02).                   CI0107
            11            DE10-MSEG   PICTURE  X(08).                   CI0107
            11            DE10-XCOKEY PICTURE  X(70).                   CI0107
            11            DE10-CUIBR  PICTURE  X(01).                   CI0107
            11            DE10-CUIBA  PICTURE  X(01).                   CI0107
            11            DE10-IPBIK  PICTURE  X(1).                    CI0107
            10            DE10-DU03.                                    CI0107
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0107
                          COMPUTATIONAL-3.                              CI0107
            11            DE10-CMSSF  PICTURE  XX.                      CI0107
            11            DE10-DU09.                                    CI0107
            12            DE10-CMESA  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            12            DE10-CMESB  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            12            DE10-CMSST  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            12            DE10-QELLAA PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            12            DE10-TMESS4 PICTURE  X(512).                  CI0107
      *
      *
      *
      *
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0107
          05              MS00-SUITE.                                   CI0107
            15       FILLER         PICTURE  X(00542).                  CI0107
       01                 MS03  REDEFINES      MS00.                    CI0107
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0107
                          COMPUTATIONAL-3.                              CI0107
            10            MS03-CMSSF  PICTURE  XX.                      CI0107
            10            MS03-DU09.                                    CI0107
            11            MS03-CMESA  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            11            MS03-CMESB  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            11            MS03-CMSST  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            11            MS03-QELLAA PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
            11            MS03-TMESS4 PICTURE  X(512).                  CI0107
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0107
            10            MX11-QMSGS  PICTURE  9(03).                   CI0107
            10            MX11-PJ09                                     CI0107
                          OCCURS       025     TIMES.                   CI0107
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0107
                          COMPUTATIONAL-3.                              CI0107
            11            MX11-CMESB  PICTURE  S9(9)                    CI0107
                          BINARY.                                       CI0107
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                CD57
                                PJ15
                                TM35
                                FG15
                                TG15
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0107
      *               *                                   *             CI0107
      *               *INITIALISATIONS                    *             CI0107
      *               *                                   *             CI0107
      *               *************************************.            CI0107
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Module Initializations        *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F02.                                                             lv05
      *                                                                 ADU102
      *N02BA.    NOTE *---> Initialize FT to ensure       *.            ADU102
       F02BA.                                                           lv10
      *     F20 is executed at end of                                   ADU102
      *     processing                                                  ADU102
      *                                                                 ADU102
           MOVE ALL    ZEROS TO FT                                      ADU102
      *---> Update Multiple Message                                     ADU102
      *     Load counter...                                             ADU102
           MOVE        MX11-QMSGS TO IMX11L.                            ADU102
       F02BA-FN. EXIT.
      *N02MA.    NOTE *MISCELLANEOUS INITS                *.
       F02MA.                                                           lv10
      *MOVE DEST CTID TO WORK AREA
           MOVE        CD57-CTID01 TO WORK-CTID01
           MOVE        'N' TO CD57-ICDSC
           CD57-ICDSU
           MOVE        ZERO TO CD57-CCDSCW
      *INIT DB SEGS
           INITIALIZE  FR01
           INITIALIZE  TO01.
       F02MA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESS FOR DB ACCESSES        *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR SSSP                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-SSSP-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0107
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0107
      *               *                                   *             CI0107
      *               *FIN DE TRAITEMENT                  *             CI0107
      *               *                                   *             CI0107
      *               *************************************.            CI0107
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0107
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *EDIT INCOMING PARAMETERS           *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30BA.    NOTE *SOURCE ADMIN MUST BE NUMERIC       *.
       F30BA.    IF    CD57-CTIDA NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30BA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012801 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BA-FN. EXIT.
      *N30CA.    NOTE *SOURCE PRODUCT MUST BE NUMERIC     *.
       F30CA.    IF    CD57-PRCOD NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F30CA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012780 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CA-FN. EXIT.
      *N30DA.    NOTE *DEST. ADMIN MUST BE NUMERIC        *.
       F30DA.    IF    WORK-CTIDA1 NOT NUMERIC                          lv10
                 NEXT SENTENCE ELSE GO TO     F30DA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012800 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DA-FN. EXIT.
      *N30EA.    NOTE *DEST. PRODUCT MUST BE NUMERIC      *.
       F30EA.    IF    CD57-PRCOD1 NOT NUMERIC                          lv10
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012780 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EA-FN. EXIT.
      *N30FA.    NOTE *TAX MARKET MUST BE NON-ZERO        *.
       F30FA.    IF    TM35-CTXMT NOT NUMERIC                           lv10
                 OR    TM35-CTXMT = ZERO
                 NEXT SENTENCE ELSE GO TO     F30FA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013368 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FA-FN. EXIT.
      *N30FM.    NOTE *ROLE TO PLAN MUST BE NUMERIC       *.
       F30FM.    IF    FG15-CIDRP (1) NOT NUMERIC                       lv10
                 NEXT SENTENCE ELSE GO TO     F30FM-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013426 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30FM-FN. EXIT.
      *N30GA.    NOTE *CUSTODIAL ACCOUNTS MUST HAVE       *.
       F30GA.    IF    (CD57-CTCCI = '1'                                lv10
                 OR    FG15-CIDRP (1) = 01)
                 AND   ((PJ15-CLCUS NOT NUMERIC)
                 OR    (PJ15-CLCUS = 0
                 AND   PJ15-CTYPE NOT = 'E'))
                 NEXT SENTENCE ELSE GO TO     F30GA-FN.
      *NON-ZERO TAX SETTLEMENT CODE
      *- EXCHANGE WILL HAVE SETTLEMENT
      *  CODE OF ZERO
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013423 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30GA-FN. EXIT.
       F30-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *FUND SOURCE ACCT:                  *
      *               *                                   *
      *               *************************************.
       F50.      IF    CD57-CTIDA = 002                                 lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *DETERMINE CDSC WAIVER CODE
      *N50EE.    NOTE *GET PRODUCT RULES FOR SOURCE       *.
       F50EE.                                                           lv10
           MOVE        CD57-CTIDA TO S-SSU01-CTIDA
           MOVE        CD57-PRCOD TO S-SSU01-PRCOD
           MOVE        CD57-PRSCD TO S-SSU01-CPRSCN
           PERFORM     F94SS THRU F94SS-FN.
      *N50EF.    NOTE *SS01 FOUND (SOURCE ACCT)           *.
       F50EF.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50EF-FN.
           MOVE        SS01 TO FR01.
       F50EF-900. GO TO F50EH-FN.
       F50EF-FN. EXIT.
      *N50EH.    NOTE *CRITICAL ERROR IF SS01 NOT FOUND   *.
       F50EH.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012355 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50EH-FN. EXIT.
       F50EE-FN. EXIT.
      *N50EO.    NOTE *IF DESTINATION IS A FUND GET       *.
       F50EO.    IF    WORK-CTIDA1 = 002                                lv10
                 NEXT SENTENCE ELSE GO TO     F50EO-FN.
      *PRODUCT RULES
           MOVE        WORK-CTIDA1 TO S-SSU01-CTIDA
           MOVE        CD57-PRCOD1 TO S-SSU01-PRCOD
           MOVE        CD57-CPRSC1 TO S-SSU01-CPRSCN
           PERFORM     F94SS THRU F94SS-FN.
      *N50EP.    NOTE *SS01 FOUND (DEST ACCT)             *.
       F50EP.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50EP-FN.
           MOVE        SS01 TO TO01.
       F50EP-900. GO TO F50ES-FN.
       F50EP-FN. EXIT.
      *N50ES.    NOTE *CRITICAL ERROR IF SS01 NOT FOUND   *.
       F50ES.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012084 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50ES-FN. EXIT.
       F50EO-FN. EXIT.
      *N50FA.    NOTE *SALES LD TYPE = REAR-END LOADED    *.
       F50FA.    IF    FR01-CLDTY = 'CD' OR 'LL'                        lv10
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *N50FC.    NOTE *QUALIFIED SOURCE ACCT              *.
       F50FC.    IF    CD57-CTCCI = '1'                                 lv15
                 OR    FG15-CIDRP (1) = 01
                 NEXT SENTENCE ELSE GO TO     F50FC-FN.
      *(I.E. 'IDS' IS THE CUSTODIAN)
      *N50FE.    NOTE *DEST IS FUND                       *.
       F50FE.    IF    WORK-CTIDA1 = 002                                lv20
                 AND   (TO01-CLDTY = 'CD' OR 'LL')
                 NEXT SENTENCE ELSE GO TO     F50FE-FN.
      *WITH SALES LD TYPE = REAR-END LD
      *N50FF.    NOTE *DEST IS QUALIFIED                  *.
       F50FF.    IF    CD57-CTCCIA = '1'                                lv25
                 OR    TG15-CIDRP (1) = 01
                 NEXT SENTENCE ELSE GO TO     F50FF-FN.
      *CDSC DOES NOT APPLY
               GO TO     F50FA-FN.
       F50FF-900. GO TO F50FG-FN.
       F50FF-FN. EXIT.
      *N50FG.    NOTE *DEST IS NOT QUALIFIED              *.
       F50FG.                                                           lv25
      *IF SETTLEMENT CODE IS SUCH THAT                                  DOT
      *A WAIVER CODE WOULD BE SET BY
      *MACRO ACDSCW, LET IT FALL THRU
      *AND GET THAT WAIVER CODE.
                 IF    PJ15-CLCUS = 4 OR 13 OR 25                       DOT
                       OR 36 OR 45
                 OR    PJ15-CLCUS = 7 OR 21 OR 32
      *DEATH
      *RETIREMENT
           NEXT SENTENCE
                 ELSE
      *CDSC DOES NOT APPLY
               GO TO     F50FA-FN.
       F50FG-FN. EXIT.
       F50FE-FN. EXIT.
       F50FC-900. GO TO F50FK-FN.
       F50FC-FN. EXIT.
      *N50FK.    NOTE *NON-QUALIFIED SOURCE ACCT          *.
       F50FK.         EXIT.                                             lv15
      *N50FL.    NOTE *DEST IS FUND                       *.
       F50FL.    IF    WORK-CTIDA1 = 002                                lv20
                 AND   (TO01-CLDTY = 'CD' OR 'LL')
                 NEXT SENTENCE ELSE GO TO     F50FL-FN.
      *WITH SALES LD TYPE = REAR-END LD
      *CDSC DOES NOT APPLY
               GO TO     F50FA-FN.
       F50FL-FN. EXIT.
       F50FK-FN. EXIT.
      *N50GA.    NOTE *DETERMINE WAIVER CODE              *.
       F50GA.                                                           lv15
      *SET INDICATORS/CODE TO:
      *- 'APPLIES'
      *- 'CAN BE UPDATED'
      *- '9' = VALUE THAT THE WAIVER
      *        CODE CAN BE CHANGED TO
           MOVE        'Y' TO CD57-ICDSC
           CD57-ICDSU
           MOVE        9 TO CD57-CCDSCW.
      *N50HA.    NOTE *DETERMINE CDSC WAIVER CODE         *.            ACDSCW
       F50HA.                                                           lv50
      *********************************                                 ACDSCW
      *THIS CODE IS ONLY NEEDED FOR                                     ACDSCW
      *REAR-END LOADED FUNDS.  PROGRAMS                                 ACDSCW
      *CALLING THIS MACRO NEED TO CHECK                                 ACDSCW
      *THE LOAD TYPE(SS01-CLDTY = 'CD'                                  ACDSCW
      *OR 'LL') AND ONLY PERFORM THIS                                   ACDSCW
      *LOGIC IF THE LOAD TYPE IS 'CD'                                   ACDSCW
      *OR 'LL'.                                                         ACDSCW
      *********************************                                 ACDSCW
      *N50HE.    NOTE *IF CUSTODIAL TRANSFER - PAY CDSC   *.            ACDSCW
       F50HE.    IF    CD57-CPAYF = 'CT'                                lv55
                 NEXT SENTENCE ELSE GO TO     F50HE-FN.                 ACDSCW
      *ASSUME MONEY IS LEAVING AEFA SO                                  ACDSCW
      *CLIENT IS REQUIRED TO PAY CDSC                                   ACDSCW
           MOVE        0 TO CD57-CCDSCW                                 ACDSCW
           MOVE        'N' TO CD57-ICDSU.
       F50HE-900. GO TO F50HG-FN.
       F50HE-FN. EXIT.
      *N50HG.    NOTE *ELSE                               *.            ACDSCW
       F50HG.         EXIT.                                             lv55
      *N50HM.    NOTE *IF EMPLOYER PLAN - WAIVE CDSC      *.            ACDSCW
       F50HM.    IF    TM35-CTXMT = 3                                   lv60
                 OR    4                                                ACDSCW
                 OR    11                                               ACDSCW
                 OR    12                                               ACDSCW
                 NEXT SENTENCE ELSE GO TO     F50HM-FN.                 ACDSCW
      *TAX MARKETS:                                                     ACDSCW
      *     PRIVATE DEFERRED COMP                                       ACDSCW
      *     GOV'T DEFERRED COMP                                         ACDSCW
      *     401K                                                        ACDSCW
      *     TRUSTEED BENEFIT PLAN                                       ACDSCW
           MOVE        4 TO CD57-CCDSCW                                 ACDSCW
           MOVE        'N' TO CD57-ICDSU.
       F50HM-900. GO TO F50HP-FN.
       F50HM-FN. EXIT.
      *N50HP.    NOTE *ELSE                               *.            ACDSCW
       F50HP.         EXIT.                                             lv60
      *N50HS.    NOTE *IF CUSTODIAL ACCOUNT               *.            ACDSCW
       F50HS.    IF    CD57-CTCCI = '1'                                 lv65
                 OR    FG15-CIDRP (1) = 1                               ACDSCW
                 NEXT SENTENCE ELSE GO TO     F50HS-FN.                 ACDSCW
      *N50IA.    NOTE *CHECK TAX SETTLEMENT CODE          *.            ACDSCW
       F50IA.         EXIT.                                             lv70
      *N50IE.    NOTE *WAIVE CDSC ON DEATH                *.            ACDSCW
       F50IE.    IF    PJ15-CLCUS =                                     lv75
                       4                                                ACDSCW
                 OR    13                                               ACDSCW
                 OR    25                                               ACDSCW
                 OR    36                                               ACDSCW
                 OR    45                                               ACDSCW
                 OR    58                                               ACDSCW
                 NEXT SENTENCE ELSE GO TO     F50IE-FN.                 ACDSCW
      *(ALPHA CODES: DTH, DDR, RDH, CDH                                 ACDSCW
      *          AND EDH AND QDH)                                       ACDSCW
           MOVE        2 TO CD57-CCDSCW                                 ACDSCW
           MOVE        'N' TO CD57-ICDSU.
       F50IE-900. GO TO F50IA-FN.
       F50IE-FN. EXIT.
      *N50IH.    NOTE *WAIVE CDSC ON RETIREMENT           *.            ACDSCW
       F50IH.    IF    PJ15-CLCUS =                                     lv75
                       7                                                ACDSCW
                 OR    21                                               ACDSCW
                 OR    32                                               ACDSCW
                 OR    56                                               ACDSCW
                 NEXT SENTENCE ELSE GO TO     F50IH-FN.                 ACDSCW
      *(ALPHA CODES: RET, RTH, CON                                      ACDSCW
      *       AND QRD)                                                  ACDSCW
           MOVE        0 TO CD57-CCDSCW                                 ACDSCW
           MOVE        'N' TO CD57-ICDSU.
                 IF    PJ15-CLCUS = 7                                   DOT
                 AND   CD57-IDRMD = 'Y'
                 AND   CD57-QCLAGE >= 70.5
      *FOR RET AND RMD TRANSACTION,
      *DETERMIN CLIENT AGE
           MOVE        6 TO CD57-CCDSCW.
       F50IH-900. GO TO F50IA-FN.
       F50IH-FN. EXIT.
      *N50IM.    NOTE *CDSC IS CHARGED IF TAX             *.            ACDSCW
       F50IM.                                                           lv75
      *SETTLEMENT CODE IS NOT ONE                                       ACDSCW
      *LISTED ABOVE.                                                    ACDSCW
       F50IM-FN. EXIT.
       F50IA-FN. EXIT.
       F50HS-900. GO TO F50IP-FN.
       F50HS-FN. EXIT.
      *N50IP.    NOTE *ELSE (IF NOT AN AEFA CUSTODIAL     *.            ACDSCW
       F50IP.                                                           lv65
      *ACCOUNT)                                                         ACDSCW
      *N50IR.    NOTE *IF CLIENT IS DEAD AND SELECTED     *.            ACDSCW
       F50IR.    IF    CD57-CLTYP = 'P'                                 lv70
                 AND   CD57-CLDTH = 'Y'                                 ACDSCW
                 AND   (TM35-CTXMT = 1                                  ACDSCW
                 OR    2                                                ACDSCW
                 OR    6                                                ACDSCW
                 OR    8                                                ACDSCW
                 OR    10)                                              ACDSCW
                 NEXT SENTENCE ELSE GO TO     F50IR-FN.                 ACDSCW
      *TAX MARKETS (PERSONAL, FOREIGN,                                  ACDSCW
      *NON-AEFA CUSTODIAL PLAN, IRA,                                    ACDSCW
      *AND 403(B) TSA) - WAIVE CDSC                                     ACDSCW
           MOVE        2 TO CD57-CCDSCW                                 ACDSCW
           MOVE        'N' TO CD57-ICDSU.
       F50IR-FN. EXIT.
       F50IP-FN. EXIT.
       F50HP-FN. EXIT.
       F50HG-FN. EXIT.
       F50HA-FN. EXIT.
       F50GA-FN. EXIT.
       F50FA-FN. EXIT.
       F50-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Normal Termination            *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F79.                                                             lv05
      *     Return to Calling Module                                    ADU102
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N93.      NOTE *************************************.            ADU129
      *               *                                   *             ADU129
      *               *---> Common DL/1 Error Checks      *             ADU129
      *               *                                   *             ADU129
      *               *************************************.            ADU129
       F93.           EXIT.                                             lv05
      *N93EA.    NOTE *---> DL/1 I/O Error Checks         *.            ADU129
       F93EA.                                                           lv10
                 IF    XW05-XRC = '  '                                  DOT
           MOVE        ZERO TO IK                                       ADU129
                 ELSE                                                   ADU129
           MOVE        '1' TO IK                                        ADU129
           DE10-IPBIK                                                   ADU129
           INITIALIZE  DE10-DU03                                        ADU129
           PERFORM     F93UI THRU F93UI-FN                              ADU129
           PERFORM     F93PC THRU F93PC-FN.                             ADU129
       F93EA-FN. EXIT.
      *N93PC.    NOTE *---> DL/1 PCB Check via CI0009P    *.            ADU129
       F93PC.                                                           lv10
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU129
           MOVE        XW05-XRC TO DE10-XRC                             ADU129
           MOVE        XW05-XSEGNM TO DE10-MSEG                         ADU129
           MOVE        XW05-XCOKEY TO DE10-XCOKEY                       ADU129
           MOVE        XW05-XDBDNM TO DE10-XDBDNM                       ADU129
      *                                                                 ADU129
           MOVE        'CI0009P ' TO W-PASS-XPROGR                      ADU129
           CALL        W-PASS-XPROGR                                    ADU129
           USING DFHEIBLK                                               ADU129
           DFHCOMMAREA                                                  ADU129
           DE10                                                         ADU129
           MS03.                                                        ADU129
      *N93PD.    NOTE *---> Exit on severe DL/1 Error     *.            ADU129
       F93PD.    IF    DE10-NMESS2 NOT = ZERO                           lv15
                 NEXT SENTENCE ELSE GO TO     F93PD-FN.                 ADU129
           MOVE                     ALL '1' TO FT GO TO F20.            ADU129
       F93PD-FN. EXIT.
       F93PC-FN. EXIT.
      *N93UI.    NOTE *---> DL/1 User Intrface BLK(UIB)   *.            ADU129
       F93UI.                                                           lv10
      *     via CI0008P                                                 ADU129
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU129
           MOVE        UIBFCTR TO DE10-CUIBR                            ADU129
           MOVE        UIBDLTR TO DE10-CUIBA                            ADU129
      *                                                                 ADU129
           MOVE        'CI0008P ' TO W-PASS-XPROGR                      ADU129
           CALL        W-PASS-XPROGR                                    ADU129
           USING DFHEIBLK                                               ADU129
           DFHCOMMAREA                                                  ADU129
           DE10                                                         ADU129
           MS03.                                                        ADU129
       F93UI-FN. EXIT.
       F93-FN.   EXIT.
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *DL1 CALLS                          *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94SS.    NOTE *CALL GU ON SS01                    *.            ADU026
       F94SS.                                                           lv10
           MOVE        'SSSP' TO DE10-XDBDNM                            ADU026
           MOVE        'SS01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 SS01                                                    ADU026
           S-SSU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94SS-FN. EXIT.
       F94-FN.   EXIT.
      *N98.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Common Performed Routines     *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F98.           EXIT.                                             lv05
      *N98ET.    NOTE *---> Return an Error Message       *.            ADU102
       F98ET.                                                           lv10
      *           AND TERMINATE                                         ADU102
           MOVE        11 TO MS03-CMESB                                 ADU102
      *                                                                 ADU102
      *---> Get Error Message Text                                      ADU102
           PERFORM     F98GM THRU F98GM-FN                              ADU102
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F98ET-FN. EXIT.
      *N98GM.    NOTE *---> Get Error Message Text        *.            ADU102
       F98GM.                                                           lv10
      *                                                                 ADU102
           MOVE        'AP' TO MS03-CMSSF                               ADU102
           CALL        CI0002 USING                                     ADU102
           DFHEIBLK                                                     ADU102
           DFHCOMMAREA                                                  ADU102
           MS03.                                                        ADU102
       F98GM-FN. EXIT.
      *N98IC.    NOTE *---> Return Informational          *.            ADU102
       F98IC.                                                           lv10
      *     Message and Continue                                        ADU102
           MOVE        10 TO MS03-CMESB                                 ADU102
      *---> Get Message Text                                            ADU102
           PERFORM     F98GM THRU F98GM-FN.                             ADU102
       F98IC-FN. EXIT.
      *N98MX.    NOTE *---> Add Informational Message     *.            ADU102
       F98MX.                                                           lv10
      *     to linkage table MX11                                       ADU102
           MOVE        MX11-QMSGS TO IMX11L                             ADU102
           ADD         1 TO IMX11L.                                     ADU102
                 IF    IMX11L > IMX11M                                  DOT
      *---> Maximum number exceeded...                                  ADU102
      *     Return severe error msg...                                  ADU102
           MOVE        012776 TO MS03-NMESS2                            ADU102
           PERFORM     F98ET THRU F98ET-FN.                             ADU102
      *---  Endif  ---                                                  DOT
      *                                                                 ADU102
      *---> Add informational error                                     ADU102
      *     message number to table                                     ADU102
           MOVE        +10 TO MX11-CMESB (IMX11L)                       ADU102
           MOVE        MS03-NMESS2 TO MX11-NMESS2 (IMX11L)              ADU102
           MOVE        IMX11L TO MX11-QMSGS.                            ADU102
       F98MX-FN. EXIT.
       F98-FN.   EXIT.
