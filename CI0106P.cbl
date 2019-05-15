       IDENTIFICATION DIVISION.                                         CI0106
       PROGRAM-ID.  CI0106P.                                            CI0106
      *AUTHOR.         DETR IRA CONTRIBUTION TYPE.                      CI0106
      *DATE-COMPILED.   09/08/14.                                       CI0106
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
       ENVIRONMENT DIVISION.                                            CI0106
       CONFIGURATION SECTION.                                           CI0106
       SOURCE-COMPUTER. IBM-370.                                        CI0106
       OBJECT-COMPUTER. IBM-370.                                        CI0106
       DATA DIVISION.                                                   CI0106
       WORKING-STORAGE SECTION.                                         CI0106
       01               7-CTA09-1-SSA.                                  AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09'.       AAADBL
         05             FILLER          PIC X(01)   VALUE '*'.          AAADBL
         05             7-CTA09-1-CCOD PIC X(05)  VALUE '-----'.        AAADBL
         05             FILLER          PIC X(01)   VALUE '('.          AAADBL
         05             FILLER          PIC X(08)   VALUE 'CT09K'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(03)   VALUE 004.
         05             FILLER          PIC X(01)   VALUE '&'.
         05             FILLER          PIC X(08)   VALUE 'GERED'.
         05             FILLER          PIC X(02)   VALUE ' ='.
         05             FILLER          PIC 9(08)   VALUE ZEROS.
         05             FILLER          PIC X(01)   VALUE ')'.          AAADBL
      * THE FOLLOWING MODULE IS THE DU MESSAGES MODULE (DAR LIBRARY)
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01                 CT01.                                         CI0106
            10            CT01-CT01K.                                   CI0106
            11            CT01-C299.                                    CI0106
            12            CT01-CTID.                                    CI0106
            13            CT01-CTIDA  PICTURE  9(3).                    CI0106
            13            CT01-CTIDN.                                   CI0106
            14            CT01-CTIDNP PICTURE  X(13).                   CI0106
            14            CT01-CTIDND PICTURE  9(11).                   CI0106
            10            CT01-GECKD  PICTURE  9.                       CI0106
            10            CT01-GEMDA  PICTURE  9(8).                    CI0106
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0106
                          BINARY.                                       CI0106
            10            CT01-GECUC  PICTURE  99.                      CI0106
            10            CT01-CTAUL  PICTURE  9(3).                    CI0106
            10            CT01-DIRAC  PICTURE  9(4).                    CI0106
            10            CT01-CTCCI  PICTURE  X.                       CI0106
            10            CT01-CTCUS  PICTURE  999.                     CI0106
            10            CT01-CTEFD  PICTURE  9(8).                    CI0106
            10            CT01-CTIAD  PICTURE  9(8).                    CI0106
            10            CT01-CLCUS  PICTURE  99.                      CI0106
            10            CT01-CAMMB  PICTURE  X(3).                    CI0106
            10            CT01-CKPMM  PICTURE  X.                       CI0106
            10            CT01-CTLAD  PICTURE  9(8).                    CI0106
            10            CT01-IPERS  PICTURE  X.                       CI0106
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0106
                          COMPUTATIONAL-3.                              CI0106
            10            CT01-CTLAT  PICTURE  9(8).                    CI0106
            10            CT01-CTLATC PICTURE  9(6).                    CI0106
            10            CT01-IMEGA  PICTURE  X.                       CI0106
            10            CT01-DIRAB  PICTURE  9(8).                    CI0106
            10            CT01-COLRQ  PICTURE  X.                       CI0106
            10            CT01-ZDA04  PICTURE  X(4).                    CI0106
            10            CT01-CTLPD  PICTURE  9(8).                    CI0106
            10            CT01-CIRASP PICTURE  9.                       CI0106
            10            CT01-CIRATP PICTURE  99.                      CI0106
            10            CT01-DRTHC  PICTURE  9(8).                    CI0106
            10            CT01-CPPTC  PICTURE  X.                       CI0106
            10            CT01-ZDA06  PICTURE  X(6).                    CI0106
            10            CT01-CTACD  PICTURE  9(8).                    CI0106
            10            CT01-CTNLI  PICTURE  X.                       CI0106
            10            CT01-CTRHO  PICTURE  9(8).                    CI0106
            10            CT01-CTSGD  PICTURE  9(8).                    CI0106
            10            CT01-CPATP  PICTURE  X(1).                    CI0106
            10            CT01-IRSTA  PICTURE  X.                       CI0106
            10            CT01-CTSTA  PICTURE  99.                      CI0106
            10            CT01-CTSSC  PICTURE  99.                      CI0106
            10            CT01-PRLIN  PICTURE  9(3).                    CI0106
            10            CT01-PRCOD  PICTURE  9(5).                    CI0106
            10            CT01-PRSCD  PICTURE  X(9).                    CI0106
            10            CT01-CTLNI  PICTURE  X.                       CI0106
            10            CT01-AYSIDA PICTURE  9(3).                    CI0106
            10            CT01-AYSID  PICTURE  9(5).                    CI0106
            10            CT01-CTBMC  PICTURE  99.                      CI0106
            10            CT01-CINAR  PICTURE  99.                      CI0106
            10            CT01-CPHTR  PICTURE  X.                       CI0106
            10            CT01-CDSTR  PICTURE  XX.                      CI0106
            10            CT01-CQACT  PICTURE  999.                     CI0106
            10            CT01-CIRAS  PICTURE  999.                     CI0106
            10            CT01-CIRAT  PICTURE  999.                     CI0106
            10            CT01-CLRAY  PICTURE  9(5).                    CI0106
            10            CT01-CATTP  PICTURE  X.                       CI0106
       01                 CT07.                                         CI0106
            10            CT07-CT07K.                                   CI0106
            11            CT07-C199.                                    CI0106
            12            CT07-CLID.                                    CI0106
            13            CT07-CLIDO  PICTURE  9(3).                    CI0106
            13            CT07-CLIDN.                                   CI0106
            14            CT07-CLIDNP PICTURE  X(12).                   CI0106
            14            CT07-CLIDND PICTURE  9(8).                    CI0106
       01                 CT09.                                         CI0106
            10            CT09-A100.                                    CI0106
            11            CT09-GELL   PICTURE  9(4)                     CI0106
                          BINARY.                                       CI0106
            11            CT09-CT09K.                                   CI0106
            12            CT09-CLCTRC PICTURE  9(3).                    CI0106
            11            CT09-GERSD  PICTURE  9(8).                    CI0106
            11            CT09-GERED  PICTURE  9(8).                    CI0106
            10            CT09-A199.                                    CI0106
            11            CT09-FILLER PICTURE  X(20).                   CI0106
            10            CT09-A101                                     CI0106
                          REDEFINES            CT09-A199.               CI0106
            11            CT09-GECSQ  PICTURE  S9(3)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            11            CT09-CTAXR  PICTURE  X.                       CI0106
            11            CT09-GETAI  PICTURE  X.                       CI0106
            11            CT09-CTLACD PICTURE  9(8).                    CI0106
            11            CT09-GEPCS  PICTURE  S9(3)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            10            CT09-A102                                     CI0106
                          REDEFINES            CT09-A199.               CI0106
            11            CT09-CLPID  PICTURE  9(9).                    CI0106
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW03.                                         CI0106
            10            XW03-XDATG.                                   CI0106
            11            XW03-XDAT1.                                   CI0106
            12            XW03-XDAT19 PICTURE  99                       CI0106
                          VALUE                ZERO.                    CI0106
            11            XW03-XDAT2.                                   CI0106
            12            XW03-XDAT29 PICTURE  99                       CI0106
                          VALUE                ZERO.                    CI0106
            11            XW03-XDAT3.                                   CI0106
            12            XW03-XDAT39 PICTURE  99                       CI0106
                          VALUE                ZERO.                    CI0106
            11            XW03-XDAT4.                                   CI0106
            12            XW03-XDAT49 PICTURE  99                       CI0106
                          VALUE                ZERO.                    CI0106
            10            XW03-XLEAPY PICTURE  99                       CI0106
                          VALUE                ZERO.                    CI0106
            10            XW03-DTGCY  PICTURE  9(4)                     CI0106
                          VALUE                ZERO.                    CI0106
            10            XW03-FILLER                                   CI0106
                          REDEFINES            XW03-DTGCY.              CI0106
            11            XW03-DTGCC  PICTURE  9(2).                    CI0106
            11            XW03-DTGYY  PICTURE  9(2).                    CI0106
       01                 XW05.                                         CI0106
            10            XW05-XW06.                                    CI0106
            11            XW05-XDBPCB.                                  CI0106
            12            XW05-XDBDNM PICTURE  X(08)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-XSEGLV PICTURE  X(02)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-XRC    PICTURE  X(02)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-XPROPT PICTURE  X(04)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-FILLER PICTURE  S9(5)                    CI0106
                          VALUE                ZERO                     CI0106
                          BINARY.                                       CI0106
            12            XW05-XSEGNM PICTURE  X(08)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0106
                          VALUE                ZERO                     CI0106
                          BINARY.                                       CI0106
            12            XW05-XSEGNB PICTURE  9(05)                    CI0106
                          VALUE                ZERO                     CI0106
                          BINARY.                                       CI0106
            12            XW05-XCOKEY PICTURE  X(70)                    CI0106
                          VALUE                SPACE.                   CI0106
            10            XW05-XW07.                                    CI0106
            11            XW05-XIOPCB.                                  CI0106
            12            XW05-XTERMI PICTURE  X(08)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-FILLER PICTURE  XX                       CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-XRC1   PICTURE  X(02)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-FILLER PICTURE  X(12)                    CI0106
                          VALUE                SPACE.                   CI0106
            12            XW05-XMODNM PICTURE  X(8)                     CI0106
                          VALUE                SPACE.                   CI0106
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0106
                          VALUE                ZERO.                    CI0106
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0106
                          VALUE                ZERO.                    CI0106
            10            XW05-XGU    PICTURE  X(4)                     CI0106
                          VALUE                'GU  '.                  CI0106
            10            XW05-XGHU   PICTURE  X(4)                     CI0106
                          VALUE                'GHU '.                  CI0106
            10            XW05-XGN    PICTURE  X(4)                     CI0106
                          VALUE                'GN  '.                  CI0106
            10            XW05-XGHN   PICTURE  X(4)                     CI0106
                          VALUE                'GHN '.                  CI0106
            10            XW05-XGNP   PICTURE  X(4)                     CI0106
                          VALUE                'GNP '.                  CI0106
            10            XW05-XGHNP  PICTURE  X(4)                     CI0106
                          VALUE                'GHNP'.                  CI0106
            10            XW05-XREPL  PICTURE  XXXX                     CI0106
                          VALUE                'REPL'.                  CI0106
            10            XW05-XISRT  PICTURE  X(4)                     CI0106
                          VALUE                'ISRT'.                  CI0106
            10            XW05-XDLET  PICTURE  X(4)                     CI0106
                          VALUE                'DLET'.                  CI0106
            10            XW05-XOPEN  PICTURE  X(4)                     CI0106
                          VALUE                'OPEN'.                  CI0106
            10            XW05-XCLSE  PICTURE  X(4)                     CI0106
                          VALUE                'CLSE'.                  CI0106
            10            XW05-XCHKP  PICTURE  X(4)                     CI0106
                          VALUE                'CHKP'.                  CI0106
            10            XW05-XXRST  PICTURE  X(4)                     CI0106
                          VALUE                'XRST'.                  CI0106
            10            XW05-XTERM  PICTURE  X(4)                     CI0106
                          VALUE                'TERM'.                  CI0106
            10            XW05-XNFPAC PICTURE  X(13)                    CI0106
                          VALUE                SPACE.                   CI0106
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0106
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0106
       01               7-XW03.                                         AADA16
         02  XW03-RETURN            PIC 9(02)  VALUE 1.                 AADA16
      *!WF DSP=XW DSL=XW SEL=03 FOR=I DES=2 LEV=1                       AADA16
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
       01  7-LEVEL-88S.
      *
           05  7-SOURCE-ACCOUNT.
      *!WI
               10  SRCE-CIRAT
                        PICTURE 999.                                    CI0106
      *!WI
               10  SRCE-CIRAS
                        PICTURE 999.                                    CI0106
           05  SOURCE-ACCOUNT REDEFINES 7-SOURCE-ACCOUNT
                              PIC X(06).
               88  SRCE-NON-QUALIFIED        VALUE '000000'.
               88  SRCE-IRA                  VALUE '001001'.
               88  SRCE-IRA-ROLLOVER         VALUE '001002'.
               88  SRCE-IRA-BENEFICIAL       VALUE '001003'.
               88  SRCE-SEP                  VALUE '003001'.
               88  SRCE-SEP-BENEFICIAL       VALUE '003003'.
               88  SRCE-SRA                  VALUE '004001'.
               88  SRCE-ROTH                 VALUE '005001'.
               88  SRCE-ROTH-BENEFICIAL      VALUE '005003'.
               88  SRCE-ROTH-CONVERSION      VALUE '006001'.
               88  SRCE-ROTH-CNV-BENEFICIAL  VALUE '006003'.
               88  SRCE-ED-IRA               VALUE '007001'.
               88  SRCE-PENSION              VALUE '999000'.
      *
           05  7-DESTINATION-ACCOUNT.
      *!WI
               10  DEST-CIRAT
                        PICTURE 999.                                    CI0106
      *!WI
               10  DEST-CIRAS
                        PICTURE 999.                                    CI0106
           05  DESTINATION-ACCOUNT REDEFINES 7-DESTINATION-ACCOUNT
                              PIC X(06).
               88  DEST-NON-QUALIFIED        VALUE '000000'.
               88  DEST-IRA                  VALUE '001001'.
               88  DEST-IRA-ROLLOVER         VALUE '001002'.
               88  DEST-IRA-BENEFICIAL       VALUE '001003'.
               88  DEST-SEP                  VALUE '003001'.
               88  DEST-SEP-BENEFICIAL       VALUE '003003'.
               88  DEST-SRA                  VALUE '004001'.
               88  DEST-ROTH                 VALUE '005001'.
               88  DEST-ROTH-BENEFICIAL      VALUE '005003'.
               88  DEST-ROTH-CONVERSION      VALUE '006001'.
               88  DEST-ROTH-CNV-BENEFICIAL  VALUE '006003'.
               88  DEST-ED-IRA               VALUE '007001'.
      *!WI
       01      SRCE-CQACT
                        PICTURE 999.                                    CI0106
      *!WI
       01      DEST-CQACT
                        PICTURE 999.                                    CI0106

      *  ARE TAXPAYERS THE SAME?
      *
       01  7-SAME-TAXPAYERS    PIC X   VALUE 'N'.
           88 BOTH-ACCTS-SAME-TAXPAYER VALUE 'Y'.
           88 DIFFERENT-TAXPAYERS      VALUE 'N'.
      *
      *!WI
       01  7-SRCE-CLID
                        PICTURE X(23).                                  CI0106
      *!WI
       01  7-DEST-CLID
                        PICTURE X(23).                                  CI0106
       01   DEBUT-WSS.                                                  CI0106
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0106
            05   IK     PICTURE X.                                      CI0106
       01  CONSTANTES-PAC.                                              CI0106
           05  FILLER  PICTURE X(87)   VALUE                            CI0106
                     '6015 CAT09/08/14CI0106ADMIN   14:34:48CI0106P AMERCI0106
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0106
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0106
           05  NUGNA   PICTURE X(5).                                    CI0106
           05  APPLI   PICTURE X(3).                                    CI0106
           05  DATGN   PICTURE X(8).                                    CI0106
           05  PROGR   PICTURE X(6).                                    CI0106
           05  CODUTI  PICTURE X(8).                                    CI0106
           05  TIMGN   PICTURE X(8).                                    CI0106
           05  PROGE   PICTURE X(8).                                    CI0106
           05  COBASE  PICTURE X(4).                                    CI0106
           05  DATGNC  PICTURE X(10).                                   CI0106
           05  RELEAS  PICTURE X(7).                                    CI0106
           05  DATGE   PICTURE X(10).                                   CI0106
           05  DATSQ   PICTURE X(10).                                   CI0106
       01  DATCE.                                                       CI0106
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0106
         05  DATOR.                                                     CI0106
           10  DATOA  PICTURE XX.                                       CI0106
           10  DATOM  PICTURE XX.                                       CI0106
           10  DATOJ  PICTURE XX.                                       CI0106
       01   VARIABLES-CONDITIONNELLES.                                  CI0106
            05                  FT      PICTURE X VALUE '0'.            CI0106
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0106
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0106
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0106
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0106
       01               S-CT01-SSA.                                     CI0106
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0106
                                      VALUE 'CT01    '.                 CI0106
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0106
            10          S-CT01-CCOD   PICTURE X(5)                      CI0106
                                      VALUE '-----'.                    CI0106
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0106
       01            S-CTU01-SSA.                                       CI0106
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0106
                                      VALUE 'CT01    '.                 CI0106
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0106
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0106
                                      VALUE '-----'.                    CI0106
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0106
                                      VALUE '(CT01K'.                   CI0106
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0106
            10       S-CTU01-CT01K.                                     CI0106
            11       S-CTU01-C299.                                      CI0106
            12       S-CTU01-CTID.                                      CI0106
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0106
            13       S-CTU01-CTIDN.                                     CI0106
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0106
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0106
            10  FILLER   PICTURE X    VALUE ')'.                        CI0106
       01               S-CT07-SSA.                                     CI0106
            10         S1-CT07-SEGNAM PICTURE X(8)                      CI0106
                                      VALUE 'CT07    '.                 CI0106
            10         S1-CT07-CCOM   PICTURE X VALUE '*'.              CI0106
            10          S-CT07-CCOD   PICTURE X(5)                      CI0106
                                      VALUE '-----'.                    CI0106
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0106
       01            S-CTU07-SSA.                                       CI0106
            10      S1-CTU07-SEGNAM PICTURE X(8)                        CI0106
                                      VALUE 'CT07    '.                 CI0106
            10      S1-CTU07-CCOM   PICTURE X VALUE '*'.                CI0106
            10       S-CTU07-CCOD   PICTURE X(5)                        CI0106
                                      VALUE '-----'.                    CI0106
            10      S1-CTU07-FLDNAM PICTURE X(9)                        CI0106
                                      VALUE '(CT07K'.                   CI0106
            10       S-CTU07-OPER  PICTURE XX VALUE ' ='.               CI0106
            10       S-CTU07-CT07K.                                     CI0106
            11       S-CTU07-C199.                                      CI0106
            12       S-CTU07-CLID.                                      CI0106
            13       S-CTU07-CLIDO    PICTURE  9(3).                    CI0106
            13       S-CTU07-CLIDN.                                     CI0106
            14       S-CTU07-CLIDNP   PICTURE  X(12).                   CI0106
            14       S-CTU07-CLIDND   PICTURE  9(8).                    CI0106
            10  FILLER   PICTURE X    VALUE ')'.                        CI0106
       01               S-CT09-SSA.                                     CI0106
            10         S1-CT09-SEGNAM PICTURE X(8)                      CI0106
                                      VALUE 'CT09    '.                 CI0106
            10         S1-CT09-CCOM   PICTURE X VALUE '*'.              CI0106
            10          S-CT09-CCOD   PICTURE X(5)                      CI0106
                                      VALUE '-----'.                    CI0106
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0106
       01            S-CTA09-SSA.                                       CI0106
            11      S1-CTA09-SEGNAM PICTURE X(8)                        CI0106
                                      VALUE 'CT09    '.                 CI0106
            11      S1-CTA09-CCOM   PICTURE X VALUE '*'.                CI0106
            11       S-CTA09-CCOD   PICTURE X(5)                        CI0106
                                      VALUE '-----'.                    CI0106
            11      S1-CTA09-FLDNAM PICTURE X(9)                        CI0106
                                      VALUE '(GERED'.                   CI0106
            11       S-CTA09-OPER  PICTURE XX VALUE ' ='.               CI0106
            11       S-CTA09-GERED    PICTURE  9(8).                    CI0106
            11  FILLER   PICTURE X    VALUE ')'.                        CI0106
       01            S-CTB09-SSA.                                       CI0106
            11      S1-CTB09-SEGNAM PICTURE X(8)                        CI0106
                                      VALUE 'CT09    '.                 CI0106
            11      S1-CTB09-CCOM   PICTURE X VALUE '*'.                CI0106
            11       S-CTB09-CCOD   PICTURE X(5)                        CI0106
                                      VALUE '-----'.                    CI0106
            11      S1-CTB09-FLDNAM PICTURE X(9)                        CI0106
                                      VALUE '(GECSQ'.                   CI0106
            11       S-CTB09-OPER  PICTURE XX VALUE ' ='.               CI0106
            11       S-CTB09-GECSQ    PICTURE  S9(3)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            11  FILLER   PICTURE X    VALUE ')'.                        CI0106
       01            S-CTU09-SSA.                                       CI0106
            11      S1-CTU09-SEGNAM PICTURE X(8)                        CI0106
                                      VALUE 'CT09    '.                 CI0106
            11      S1-CTU09-CCOM   PICTURE X VALUE '*'.                CI0106
            11       S-CTU09-CCOD   PICTURE X(5)                        CI0106
                                      VALUE '-----'.                    CI0106
            11      S1-CTU09-FLDNAM PICTURE X(9)                        CI0106
                                      VALUE '(CT09K'.                   CI0106
            11       S-CTU09-OPER  PICTURE XX VALUE ' ='.               CI0106
            11       S-CTU09-CT09K.                                     CI0106
            12       S-CTU09-CLCTRC   PICTURE  9(3).                    CI0106
            11  FILLER   PICTURE X    VALUE ')'.                        CI0106
       01   ZONES-UTILISATEUR PICTURE X.                                CI0106
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
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0106
          05              PA00-SUITE.                                   CI0106
            15       FILLER         PICTURE  X(00106).                  CI0106
       01                 PA06  REDEFINES      PA00.                    CI0106
            10            PA06-XDBPCB.                                  CI0106
            11            PA06-XDBDNM PICTURE  X(08).                   CI0106
            11            PA06-XSEGLV PICTURE  X(02).                   CI0106
            11            PA06-XRC    PICTURE  X(02).                   CI0106
            11            PA06-XPROPT PICTURE  X(04).                   CI0106
            11            PA06-FILLER PICTURE  S9(5)                    CI0106
                          BINARY.                                       CI0106
            11            PA06-XSEGNM PICTURE  X(08).                   CI0106
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0106
                          BINARY.                                       CI0106
            11            PA06-XSEGNB PICTURE  9(05)                    CI0106
                          BINARY.                                       CI0106
            11            PA06-XCOKEY PICTURE  X(70).                   CI0106

      *PASS AREA TO/FROM CI0106
      *!WF DSP=PJ DSL=PJ SEL=53 FOR=I DES=1 LEV=1 PLT=20
       01                 PJ53.                                         CI0106
            10            PJ53-FILLER PICTURE  X(40).                   CI0106
            10            PJ53-DCACG  PICTURE  9(8).                    CI0106
            10            PJ53-CHCR   PICTURE  99.                      CI0106
            10            PJ53-MAPPN  PICTURE  X(10).                   CI0106
            10            PJ53-CTID   PICTURE  X(27)                    CI0106
                          OCCURS       002     TIMES.                   CI0106
            10            PJ53-CMESS  PICTURE  9.                       CI0106
            10            PJ53-CIRAP  PICTURE  XX                       CI0106
                          OCCURS       010     TIMES.                   CI0106
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0019  ACCT GROUPS *
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=DU DSL=DU SEL=15 FOR=I DES=1 LEV=1 PLT=25
       01                 DU15.                                         CI0106
            10            DU15-C299.                                    CI0106
            11            DU15-CTID.                                    CI0106
            12            DU15-CTIDA  PICTURE  9(3).                    CI0106
            12            DU15-CTIDN.                                   CI0106
            13            DU15-CTIDNP PICTURE  X(13).                   CI0106
            13            DU15-CTIDND PICTURE  9(11).                   CI0106
            10            DU15-DCACG  PICTURE  9(8).                    CI0106
            10            DU15-IPOCH  PICTURE  X.                       CI0106
            10            DU15-FILLER PICTURE  X(100).                  CI0106
            10            DU15-DU18                                     CI0106
                          OCCURS       010     TIMES.                   CI0106
            11            DU15-CT10.                                    CI0106
            12            DU15-CT10K.                                   CI0106
            13            DU15-GR98.                                    CI0106
            14            DU15-GRID.                                    CI0106
            15            DU15-GRIDC  PICTURE  9(3).                    CI0106
            15            DU15-GRIDN.                                   CI0106
            16            DU15-GRIDNP PICTURE  99.                      CI0106
            16            DU15-GRIDND PICTURE  9(8).                    CI0106
            12            DU15-GR97                                     CI0106
                          REDEFINES            DU15-CT10K.              CI0106
            13            DU15-GRIDCB PICTURE  9(3).                    CI0106
            13            DU15-FILLER PICTURE  X(10).                   CI0106
            12            DU15-GERSD  PICTURE  9(8).                    CI0106
            12            DU15-GERED  PICTURE  9(8).                    CI0106
            12            DU15-GRCSI  PICTURE  X.                       CI0106
            11            DU15-GR01.                                    CI0106
            12            DU15-GR01K.                                   CI0106
            13            DU15-GR98.                                    CI0106
            14            DU15-GRID.                                    CI0106
            15            DU15-GRIDC  PICTURE  9(3).                    CI0106
            15            DU15-GRIDN.                                   CI0106
            16            DU15-GRIDNP PICTURE  99.                      CI0106
            16            DU15-GRIDND PICTURE  9(8).                    CI0106
            12            DU15-GECKD  PICTURE  9.                       CI0106
            12            DU15-GEMDA  PICTURE  9(8).                    CI0106
            12            DU15-NSEQ4B PICTURE  9(8)                     CI0106
                          BINARY.                                       CI0106
            12            DU15-GRDOR  PICTURE  9(8).                    CI0106
            12            DU15-GRIAD  PICTURE  9(8).                    CI0106
            12            DU15-GECUC  PICTURE  99.                      CI0106
            12            DU15-GRLNG  PICTURE  99.                      CI0106
            12            DU15-GESLC  PICTURE  99.                      CI0106
            12            DU15-AYSIDA PICTURE  9(3).                    CI0106
            12            DU15-AYSID  PICTURE  9(5).                    CI0106
            12            DU15-GRCSD  PICTURE  9(8).                    CI0106
            12            DU15-GRCFD  PICTURE  9(8).                    CI0106
            12            DU15-GRNCL  PICTURE  S9(5)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            12            DU15-GRNCT  PICTURE  S9(5)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            12            DU15-GRSFC  PICTURE  99.                      CI0106
            12            DU15-GRCRN  PICTURE  9(3).                    CI0106
            12            DU15-GRCSS  PICTURE  X.                       CI0106
            12            DU15-MKSRC  PICTURE  99                       CI0106
                          OCCURS       010     TIMES.                   CI0106
            12            DU15-NEFPS  PICTURE  X(5).                    CI0106
            12            DU15-DEFPS  PICTURE  9(8).                    CI0106
            12            DU15-DLSRV  PICTURE  9(8).                    CI0106
            12            DU15-CTLNI  PICTURE  X.                       CI0106
            12            DU15-CGRLI  PICTURE  X.                       CI0106
            12            DU15-CAMGR  PICTURE  9(5)                     CI0106
                          COMPUTATIONAL-3.                              CI0106
            12            DU15-CAMGS  PICTURE  9(5)                     CI0106
                          COMPUTATIONAL-3.                              CI0106
            12            DU15-CAMGN  PICTURE  9(3)                     CI0106
                          COMPUTATIONAL-3.                              CI0106
            12            DU15-CGRMF  PICTURE  X.                       CI0106
            12            DU15-FILLER PICTURE  X(08).                   CI0106
            11            DU15-GR07.                                    CI0106
            12            DU15-GEDLA  PICTURE  9(8).                    CI0106
            12            DU15-GRAID  PICTURE  X(12).                   CI0106
            12            DU15-GRPAP  PICTURE  X(14).                   CI0106
            12            DU15-GEPHNX PICTURE  9(4).                    CI0106
            12            DU15-DPLEF  PICTURE  9(8).                    CI0106
            12            DU15-DPLAM  PICTURE  9(8).                    CI0106
            12            DU15-NCPFN  PICTURE  9(6).                    CI0106
            12            DU15-GEFYE  PICTURE  9(4).                    CI0106
            12            DU15-FILLER PICTURE  X(06).                   CI0106
            12            DU15-GRPAN  PICTURE  X(45).                   CI0106
            12            DU15-CGRPA  PICTURE  99.                      CI0106
            12            DU15-IPRTT7 PICTURE  X.                       CI0106
            12            DU15-GRPED  PICTURE  9(8).                    CI0106
            12            DU15-FILLER PICTURE  X(05).                   CI0106
            12            DU15-GRPLC  PICTURE  99.                      CI0106
            12            DU15-GRPLT  PICTURE  99.                      CI0106
            12            DU15-FILLER PICTURE  X(04).                   CI0106
            12            DU15-GEADI  PICTURE  X.                       CI0106
            12            DU15-GRCFA  PICTURE  S9(11)V99                CI0106
                          COMPUTATIONAL-3.                              CI0106
            12            DU15-GECFY  PICTURE  9(4).                    CI0106
            12            DU15-GECFC  PICTURE  99.                      CI0106
            12            DU15-MEMPL  PICTURE  X(20).                   CI0106
            12            DU15-CAUNIT PICTURE  X(4).                    CI0106
            12            DU15-FILLER PICTURE  X(21).                   CI0106
            12            DU15-GRPPP  PICTURE  999.                     CI0106
            12            DU15-CCORT  PICTURE  9(3).                    CI0106
            12            DU15-CIDRP  PICTURE  99.                      CI0106
            12            DU15-CCDWA  PICTURE  9.                       CI0106
            12            DU15-IERSA  PICTURE  X.                       CI0106
            12            DU15-DERSA  PICTURE  9(8).                    CI0106
            12            DU15-FILLER PICTURE  X(04).                   CI0106
            10            DU15-QITEM  PICTURE  9(3).                    CI0106
            10            DU15-XIMAX  PICTURE  S9(4)                    CI0106
                          BINARY.                                       CI0106
            10            DU15-FILLER PICTURE  X(100).                  CI0106
      *
      *
      *
      *
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0106
          05              DE00-SUITE.                                   CI0106
            15       FILLER         PICTURE  X(00653).                  CI0106
       01                 DE10  REDEFINES      DE00.                    CI0106
            10            DE10-DU11.                                    CI0106
            11            DE10-XFONC  PICTURE  X(4).                    CI0106
            11            DE10-MPSBN  PICTURE  X(8).                    CI0106
            11            DE10-XDBDNM PICTURE  X(08).                   CI0106
            11            DE10-XSEGNM PICTURE  X(08).                   CI0106
            11            DE10-XRC    PICTURE  X(02).                   CI0106
            11            DE10-MSEG   PICTURE  X(08).                   CI0106
            11            DE10-XCOKEY PICTURE  X(70).                   CI0106
            11            DE10-CUIBR  PICTURE  X(01).                   CI0106
            11            DE10-CUIBA  PICTURE  X(01).                   CI0106
            11            DE10-IPBIK  PICTURE  X(1).                    CI0106
            10            DE10-DU03.                                    CI0106
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            11            DE10-CMSSF  PICTURE  XX.                      CI0106
            11            DE10-DU09.                                    CI0106
            12            DE10-CMESA  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            12            DE10-CMESB  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            12            DE10-CMSST  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            12            DE10-QELLAA PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            12            DE10-TMESS4 PICTURE  X(512).                  CI0106
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0106
          05              MS00-SUITE.                                   CI0106
            15       FILLER         PICTURE  X(00542).                  CI0106
       01                 MS03  REDEFINES      MS00.                    CI0106
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            10            MS03-CMSSF  PICTURE  XX.                      CI0106
            10            MS03-DU09.                                    CI0106
            11            MS03-CMESA  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            11            MS03-CMESB  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            11            MS03-CMSST  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            11            MS03-QELLAA PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
            11            MS03-TMESS4 PICTURE  X(512).                  CI0106
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0106
            10            MX11-QMSGS  PICTURE  9(03).                   CI0106
            10            MX11-PJ09                                     CI0106
                          OCCURS       025     TIMES.                   CI0106
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0106
                          COMPUTATIONAL-3.                              CI0106
            11            MX11-CMESB  PICTURE  S9(9)                    CI0106
                          BINARY.                                       CI0106
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ53
                                DU15
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0106
      *               *                                   *             CI0106
      *               *INITIALISATIONS                    *             CI0106
      *               *                                   *             CI0106
      *               *************************************.            CI0106
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
      *N02MA.    NOTE *MISCELLANEOUS INITIALIZATIONS      *.
       F02MA.                                                           lv10
           MOVE        ZERO TO PJ53-CMESS
           MOVE        SPACES TO PJ53-CIRAP (1)
           PJ53-CIRAP (2)
           PJ53-CIRAP (3)
           PJ53-CIRAP (4)
           PJ53-CIRAP (5)
           PJ53-CIRAP (6)
           PJ53-CIRAP (7)
           PJ53-CIRAP (8)
           PJ53-CIRAP (9)
           PJ53-CIRAP (10).
       F02MA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESS FOR CT1P               *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0106
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0106
      *               *                                   *             CI0106
      *               *FIN DE TRAITEMENT                  *             CI0106
      *               *                                   *             CI0106
      *               *************************************.            CI0106
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0106
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT PARMS               *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *********************************
      **  ENSURE PARMS HAVE THE       *
      **  CORRECT CONTENTS BASED ON   *
      **  FIELD CLASS AND CONTENTS    *
      *********************************
      *N35CB.    NOTE *SOURCE ACCT - CK FOR NUMERIC       *.
       F35CB.    IF    PJ53-CTID (1) NOT NUMERIC                        lv10
                 NEXT SENTENCE ELSE GO TO     F35CB-FN.
      *---> Send INVALID DATA Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012449 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CB-FN. EXIT.
      *N35DB.    NOTE *DEST ACCT - CK FOR NUMERIC         *.
       F35DB.    IF    PJ53-CTID (2) NOT NUMERIC                        lv10
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
      *---> Send INVALID DATA Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012449 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DB-FN. EXIT.
      *N35EB.    NOTE *VALIDATE THE CURRENT ACCOUNTING    *.
       F35EB.    IF    PJ53-DCACG NUMERIC                               lv10
                 AND   PJ53-DCACG > ZERO
                 NEXT SENTENCE ELSE GO TO     F35EB-FN.
           MOVE        PJ53-DCACG TO XW03-XDATG
           PERFORM     F91DV THRU F91DV-FN.
      *N35ED.    NOTE *CURRENT ACCOUNTING DATE INVALID    *.
       F35ED.    IF    XW03-RETURN NOT = 1                              lv15
                 NEXT SENTENCE ELSE GO TO     F35ED-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013680 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35ED-FN. EXIT.
       F35EB-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING LOOP               *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *BASED ON IRA TYPE AND STATUS OF
      *THE 2 ACCTS (SOURCE AND DEST),
      *WILL DETERMINE LIST OF POSSIBLE
      *IRA CONTRIBUTION TYPES (CIRAP'S)
      *FOR NON-QUAL DEST ACCTS, CIRAP'S
      *WILL BE SPACES.
      *N40BA.    NOTE *FIRST DETERMINE ACCOUNT TYPES      *.
       F40BA.         EXIT.                                             lv10
      *N40BB.    NOTE *EXAMINE FIRST ACCOUNT SENT         *.
       F40BB.                                                           lv15
      *IF ACCOUNT IS A PENSION ACCT
      *SET ATTRIBUTES FOR PENSION AND
      *CONTINUE
                 IF    DU15-GRPLT (1) = 4 OR 5 OR 6                     DOT
           MOVE        '999000' TO SOURCE-ACCOUNT
               GO TO     F40BB-FN.
      *N40BG.    NOTE *EXAMINE FIRST ACCOUNT SENT IF      *.
       F40BG.                                                           lv20
      *HOUSEHOLD ACCT (NOT PENSION)
      *FIRST READ CT01 FOR 1ST ACCT
           MOVE        PJ53-CTID (1) TO S-CTU01-CT01K
           PERFORM     F94CA THRU F94CA-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
      *---> Send ACCT NOT FND Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BG-FN. EXIT.
      *N40BT.    NOTE *DETERMINE ATTRIBUTES OF 1ST ACCT   *.
       F40BT.                                                           lv20
           MOVE        CT01-CIRAT TO SRCE-CIRAT
           MOVE        CT01-CIRAS TO SRCE-CIRAS
           MOVE        CT01-CQACT TO SRCE-CQACT.
       F40BT-FN. EXIT.
       F40BB-FN. EXIT.
      *N40CB.    NOTE *EXAMINE SECOND ACCOUNT SENT        *.
       F40CB.                                                           lv15
      *FIRST READ CT01 FOR 2ND ACCT
           MOVE        PJ53-CTID (2) TO S-CTU01-CT01K
           PERFORM     F94CA THRU F94CA-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
      *---> Send ACCT NOT FND Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40CD.    NOTE *MOVE ATTRIBUTES OF 2ND ACCT        *.
       F40CD.                                                           lv20
           MOVE        CT01-CIRAT TO DEST-CIRAT
           MOVE        CT01-CIRAS TO DEST-CIRAS
           MOVE        CT01-CQACT TO DEST-CQACT.
       F40CD-FN. EXIT.
       F40CB-FN. EXIT.
      *N40CG.    NOTE *GET & COMPARE TAXPAYER CLID'S      *.
       F40CG.                                                           lv15
           PERFORM     F90 THRU F90-FN.
       F40CG-FN. EXIT.
       F40BA-FN. EXIT.
      *N40FB.    NOTE *NOW, BASED ON THE TWO TYPES        *.
       F40FB.                                                           lv10
      *DETERMINE THE SET OF CONTRIB
      *TYPES TO RETURN.
      *N40GB.    NOTE *IF SOURCE WAS NON-QUALIFIED        *.
       F40GB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '000000'
                 NEXT SENTENCE ELSE GO TO     F40GB-FN.
      *N40GC.    NOTE *BASED ON DESTINATION               *.
       F40GC.         EXIT.                                             lv20
      *N40GD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40GD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40GD-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2).
                 IF    PJ53-MAPPN = 'UD'                                DOT
           MOVE        'SC' TO PJ53-CIRAP (3).
           MOVE        'RO' TO PJ53-CIRAP (4)                           DOT
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GD-900. GO TO F40GC-FN.
       F40GD-FN. EXIT.
      *N40GF.    NOTE *IRA ROLLOVER                       *.
       F40GF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40GF-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GF-900. GO TO F40GC-FN.
       F40GF-FN. EXIT.
      *N40GH.    NOTE *IRA BENEFICIAL                     *.
       F40GH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40GH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GH-900. GO TO F40GC-FN.
       F40GH-FN. EXIT.
      *N40GJ.    NOTE *SEP AND CALLING APPLICATION IS     *.
       F40GJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40GJ-FN.
      *EZTRANS
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GJ-900. GO TO F40GC-FN.
       F40GJ-FN. EXIT.
      *N40GK.    NOTE *SEP AND CALLING APPLICATION IS     *.
       F40GK.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 AND   PJ53-MAPPN = 'FDC'
                 NEXT SENTENCE ELSE GO TO     F40GK-FN.
      *CLIENT VIEWER
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GK-900. GO TO F40GC-FN.
       F40GK-FN. EXIT.
      *N40GL.    NOTE *SRA                                *.
       F40GL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40GL-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GL-900. GO TO F40GC-FN.
       F40GL-FN. EXIT.
      *N40GN.    NOTE *ROTH                               *.
       F40GN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40GN-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GN-900. GO TO F40GC-FN.
       F40GN-FN. EXIT.
      *N40GP.    NOTE *ROTH CONVERSION                    *.
       F40GP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40GP-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GP-900. GO TO F40GC-FN.
       F40GP-FN. EXIT.
      *N40GR.    NOTE *ROTH BENEFICIAL                    *.
       F40GR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40GR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GR-900. GO TO F40GC-FN.
       F40GR-FN. EXIT.
      *N40GT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40GT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40GT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GT-900. GO TO F40GC-FN.
       F40GT-FN. EXIT.
      *N40GV.    NOTE *COVERDELL ESA                      *.
       F40GV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 NEXT SENTENCE ELSE GO TO     F40GV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1).
                 IF    PJ53-DCACG > 20020430                            DOT
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40GV-900. GO TO F40GC-FN.
       F40GV-FN. EXIT.
      *N40GZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40GZ.         EXIT.                                             lv25
      *N40HA.    NOTE *IF TSCA / TSA ACCOUNTS             *.
       F40HA.    IF    PJ53-MAPPN = 'UD'                                lv30
                 AND   SRCE-CQACT = 004
                 AND   DEST-CQACT = (004 OR 002
                       OR 003)
                 NEXT SENTENCE ELSE GO TO     F40HA-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HA-FN. EXIT.
       F40GZ-FN. EXIT.
       F40GC-FN. EXIT.
       F40GB-900. GO TO F40FB-FN.
       F40GB-FN. EXIT.
      *N40HB.    NOTE *IF SOURCE WAS IRA                  *.
       F40HB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40HB-FN.
      *N40HC.    NOTE *BASED ON DESTINATION               *.
       F40HC.         EXIT.                                             lv20
      *N40HD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40HD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40HD-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40HD-900. GO TO F40HC-FN.
       F40HD-FN. EXIT.
      *N40HF.    NOTE *IRA ROLLOVER                       *.
       F40HF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40HF-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'RO' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40HF-900. GO TO F40HC-FN.
       F40HF-FN. EXIT.
      *N40HH.    NOTE *IRA BENEFICIAL                     *.
       F40HH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40HH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HH-900. GO TO F40HC-FN.
       F40HH-FN. EXIT.
      *N40HJ.    NOTE *SEP                                *.
       F40HJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40HJ-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HJ-900. GO TO F40HC-FN.
       F40HJ-FN. EXIT.
      *N40HL.    NOTE *SRA                                *.
       F40HL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40HL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HL-900. GO TO F40HC-FN.
       F40HL-FN. EXIT.
      *N40HN.    NOTE *ROTH                               *.
       F40HN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40HN-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HN-900. GO TO F40HC-FN.
       F40HN-FN. EXIT.
      *N40HP.    NOTE *ROTH CONVERSION                    *.
       F40HP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40HP-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HP-900. GO TO F40HC-FN.
       F40HP-FN. EXIT.
      *N40HR.    NOTE *ROTH BENEFICIAL                    *.
       F40HR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40HR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HR-900. GO TO F40HC-FN.
       F40HR-FN. EXIT.
      *N40HT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40HT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40HT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HT-900. GO TO F40HC-FN.
       F40HT-FN. EXIT.
      *N40HV.    NOTE *COVERDELL ESA                      *.
       F40HV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40HV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HV-900. GO TO F40HC-FN.
       F40HV-FN. EXIT.
      *N40HZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40HZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40HZ-FN. EXIT.
       F40HC-FN. EXIT.
       F40HB-900. GO TO F40FB-FN.
       F40HB-FN. EXIT.
      *N40IB.    NOTE *IF SOURCE WAS IRA ROLLOVER         *.
       F40IB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40IB-FN.
      *N40IC.    NOTE *BASED ON DESTINATION               *.
       F40IC.         EXIT.                                             lv20
      *N40ID.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40ID.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40ID-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1)
      *POSSIBLE CO-MINGLING
           MOVE        1 TO PJ53-CMESS.
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40ID-900. GO TO F40IC-FN.
       F40ID-FN. EXIT.
      *N40IF.    NOTE *IRA ROLLOVER                       *.
       F40IF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40IF-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'RO' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40IF-900. GO TO F40IC-FN.
       F40IF-FN. EXIT.
      *N40IH.    NOTE *IRA BENEFICIAL                     *.
       F40IH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40IH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40IH-900. GO TO F40IC-FN.
       F40IH-FN. EXIT.
      *N40IJ.    NOTE *SEP                                *.
       F40IJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40IJ-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1)
      *POSSIBLE CO-MINGLING
           MOVE        1 TO PJ53-CMESS.
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40IJ-900. GO TO F40IC-FN.
       F40IJ-FN. EXIT.
      *N40IL.    NOTE *SRA                                *.
       F40IL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40IL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40IL-900. GO TO F40IC-FN.
       F40IL-FN. EXIT.
      *N40IN.    NOTE *ROTH                               *.
       F40IN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40IN-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40IN-900. GO TO F40IC-FN.
       F40IN-FN. EXIT.
      *N40IP.    NOTE *ROTH CONVERSION                    *.
       F40IP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40IP-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40IP-900. GO TO F40IC-FN.
       F40IP-FN. EXIT.
      *N40IR.    NOTE *ROTH BENEFICIAL                    *.
       F40IR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40IR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40IR-900. GO TO F40IC-FN.
       F40IR-FN. EXIT.
      *N40IT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40IT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40IT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40IT-900. GO TO F40IC-FN.
       F40IT-FN. EXIT.
      *N40IV.    NOTE *COVERDELL ESA                      *.
       F40IV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 NEXT SENTENCE ELSE GO TO     F40IV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1).
                 IF    PJ53-DCACG > 20020430                            DOT
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40IV-900. GO TO F40IC-FN.
       F40IV-FN. EXIT.
      *N40IZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40IZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40IZ-FN. EXIT.
       F40IC-FN. EXIT.
       F40IB-900. GO TO F40FB-FN.
       F40IB-FN. EXIT.
      *N40JB.    NOTE *IF SOURCE WAS IRA BENEFICIAL       *.
       F40JB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40JB-FN.
      *N40JC.    NOTE *BASED ON DESTINATION               *.
       F40JC.         EXIT.                                             lv20
      *N40JD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40JD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40JD-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2).
                 IF    PJ53-MAPPN = 'UD'                                DOT
           MOVE        'SC' TO PJ53-CIRAP (3).
           MOVE        'RO' TO PJ53-CIRAP (4)                           DOT
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JD-900. GO TO F40JC-FN.
       F40JD-FN. EXIT.
      *N40JF.    NOTE *IRA ROLLOVER                       *.
       F40JF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40JF-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JF-900. GO TO F40JC-FN.
       F40JF-FN. EXIT.
      *N40JH.    NOTE *IRA BENEFICIAL                     *.
       F40JH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40JH-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40JH-900. GO TO F40JC-FN.
       F40JH-FN. EXIT.
      *N40JJ.    NOTE *SEP                                *.
       F40JJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40JJ-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JJ-900. GO TO F40JC-FN.
       F40JJ-FN. EXIT.
      *N40JL.    NOTE *SRA                                *.
       F40JL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40JL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JL-900. GO TO F40JC-FN.
       F40JL-FN. EXIT.
      *N40JN.    NOTE *ROTH                               *.
       F40JN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40JN-FN.
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JN-900. GO TO F40JC-FN.
       F40JN-FN. EXIT.
      *N40JP.    NOTE *ROTH CONVERSION                    *.
       F40JP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40JP-FN.
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JP-900. GO TO F40JC-FN.
       F40JP-FN. EXIT.
      *N40JR.    NOTE *ROTH BENEFICIAL                    *.
       F40JR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40JR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JR-900. GO TO F40JC-FN.
       F40JR-FN. EXIT.
      *N40JT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40JT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40JT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JT-900. GO TO F40JC-FN.
       F40JT-FN. EXIT.
      *N40JV.    NOTE *COVERDELL ESA                      *.
       F40JV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 NEXT SENTENCE ELSE GO TO     F40JV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1).
                 IF    PJ53-DCACG > 20020430                            DOT
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40JV-900. GO TO F40JC-FN.
       F40JV-FN. EXIT.
      *N40JZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40JZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40JZ-FN. EXIT.
       F40JC-FN. EXIT.
       F40JB-900. GO TO F40FB-FN.
       F40JB-FN. EXIT.
      *N40KB.    NOTE *IF SOURCE WAS SEP                  *.
       F40KB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40KB-FN.
      *N40KC.    NOTE *BASED ON DESTINATION               *.
       F40KC.         EXIT.                                             lv20
      *N40KD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40KD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40KD-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40KD-900. GO TO F40KC-FN.
       F40KD-FN. EXIT.
      *N40KF.    NOTE *IRA ROLLOVER                       *.
       F40KF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40KF-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'RO' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40KF-900. GO TO F40KC-FN.
       F40KF-FN. EXIT.
      *N40KH.    NOTE *IRA BENEFICIAL                     *.
       F40KH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40KH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KH-900. GO TO F40KC-FN.
       F40KH-FN. EXIT.
      *N40KJ.    NOTE *SEP                                *.
       F40KJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40KJ-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40KJ-900. GO TO F40KC-FN.
       F40KJ-FN. EXIT.
      *N40KL.    NOTE *SRA                                *.
       F40KL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40KL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KL-900. GO TO F40KC-FN.
       F40KL-FN. EXIT.
      *N40KN.    NOTE *ROTH                               *.
       F40KN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40KN-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KN-900. GO TO F40KC-FN.
       F40KN-FN. EXIT.
      *N40KP.    NOTE *ROTH CONVERSION                    *.
       F40KP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40KP-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KP-900. GO TO F40KC-FN.
       F40KP-FN. EXIT.
      *N40KR.    NOTE *ROTH BENEFICIAL                    *.
       F40KR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40KR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KR-900. GO TO F40KC-FN.
       F40KR-FN. EXIT.
      *N40KT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40KT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40KT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KT-900. GO TO F40KC-FN.
       F40KT-FN. EXIT.
      *N40KV.    NOTE *COVERDELL ESA                      *.
       F40KV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40KV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KV-900. GO TO F40KC-FN.
       F40KV-FN. EXIT.
      *N40KZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40KZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40KZ-FN. EXIT.
       F40KC-FN. EXIT.
       F40KB-900. GO TO F40FB-FN.
       F40KB-FN. EXIT.
      *N40LB.    NOTE *IF SOURCE WAS SRA                  *.
       F40LB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40LB-FN.
      *N40LC.    NOTE *BASED ON DESTINATION               *.
       F40LC.         EXIT.                                             lv20
      *N40LD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40LD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40LD-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'RO' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40LD-900. GO TO F40LC-FN.
       F40LD-FN. EXIT.
      *N40LF.    NOTE *IRA ROLLOVER                       *.
       F40LF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40LF-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'RO' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40LF-900. GO TO F40LC-FN.
       F40LF-FN. EXIT.
      *N40LH.    NOTE *IRA BENEFICIAL                     *.
       F40LH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40LH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LH-900. GO TO F40LC-FN.
       F40LH-FN. EXIT.
      *N40LJ.    NOTE *SEP                                *.
       F40LJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40LJ-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LJ-900. GO TO F40LC-FN.
       F40LJ-FN. EXIT.
      *N40LL.    NOTE *SRA                                *.
       F40LL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40LL-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40LL-900. GO TO F40LC-FN.
       F40LL-FN. EXIT.
      *N40LN.    NOTE *ROTH                               *.
       F40LN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40LN-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LN-900. GO TO F40LC-FN.
       F40LN-FN. EXIT.
      *N40LP.    NOTE *ROTH CONVERSION                    *.
       F40LP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40LP-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LP-900. GO TO F40LC-FN.
       F40LP-FN. EXIT.
      *N40LR.    NOTE *ROTH BENEFICIAL                    *.
       F40LR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40LR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LR-900. GO TO F40LC-FN.
       F40LR-FN. EXIT.
      *N40LT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40LT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40LT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LT-900. GO TO F40LC-FN.
       F40LT-FN. EXIT.
      *N40LV.    NOTE *COVERDELL ESA                      *.
       F40LV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40LV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LV-900. GO TO F40LC-FN.
       F40LV-FN. EXIT.
      *N40LZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40LZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40LZ-FN. EXIT.
       F40LC-FN. EXIT.
       F40LB-900. GO TO F40FB-FN.
       F40LB-FN. EXIT.
      *N40MB.    NOTE *IF SOURCE WAS ROTH CONTRIBUTORY    *.
       F40MB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40MB-FN.
      *N40MC.    NOTE *BASED ON DESTINATION               *.
       F40MC.         EXIT.                                             lv20
      *N40MD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40MD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40MD-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MD-900. GO TO F40MC-FN.
       F40MD-FN. EXIT.
      *N40MF.    NOTE *IRA ROLLOVER                       *.
       F40MF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40MF-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MF-900. GO TO F40MC-FN.
       F40MF-FN. EXIT.
      *N40MH.    NOTE *IRA BENEFICIAL                     *.
       F40MH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40MH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MH-900. GO TO F40MC-FN.
       F40MH-FN. EXIT.
      *N40MJ.    NOTE *SEP                                *.
       F40MJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40MJ-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MJ-900. GO TO F40MC-FN.
       F40MJ-FN. EXIT.
      *N40ML.    NOTE *SRA                                *.
       F40ML.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40ML-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40ML-900. GO TO F40MC-FN.
       F40ML-FN. EXIT.
      *N40MN.    NOTE *ROTH CONTRIBUTORY                  *.
       F40MN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40MN-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40MN-900. GO TO F40MC-FN.
       F40MN-FN. EXIT.
      *N40MP.    NOTE *ROTH CONVERSION                    *.
       F40MP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40MP-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40MP-900. GO TO F40MC-FN.
       F40MP-FN. EXIT.
      *N40MR.    NOTE *ROTH BENEFICIAL                    *.
       F40MR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40MR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MR-900. GO TO F40MC-FN.
       F40MR-FN. EXIT.
      *N40MT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40MT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40MT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MT-900. GO TO F40MC-FN.
       F40MT-FN. EXIT.
      *N40MV.    NOTE *COVERDELL ESA                      *.
       F40MV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40MV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MV-900. GO TO F40MC-FN.
       F40MV-FN. EXIT.
      *N40MZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40MZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40MZ-FN. EXIT.
       F40MC-FN. EXIT.
       F40MB-900. GO TO F40FB-FN.
       F40MB-FN. EXIT.
      *N40NB.    NOTE *IF SOURCE WAS ROTH CONVERSION      *.
       F40NB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40NB-FN.
      *N40NC.    NOTE *BASED ON DESTINATION               *.
       F40NC.         EXIT.                                             lv20
      *N40ND.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40ND.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40ND-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40ND-900. GO TO F40NC-FN.
       F40ND-FN. EXIT.
      *N40NF.    NOTE *IRA ROLLOVER                       *.
       F40NF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40NF-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NF-900. GO TO F40NC-FN.
       F40NF-FN. EXIT.
      *N40NH.    NOTE *IRA BENEFICIAL                     *.
       F40NH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40NH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NH-900. GO TO F40NC-FN.
       F40NH-FN. EXIT.
      *N40NJ.    NOTE *SEP                                *.
       F40NJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40NJ-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NJ-900. GO TO F40NC-FN.
       F40NJ-FN. EXIT.
      *N40NL.    NOTE *SRA                                *.
       F40NL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40NL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NL-900. GO TO F40NC-FN.
       F40NL-FN. EXIT.
      *N40NN.    NOTE *ROTH CONTRIBUTORY                  *.
       F40NN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40NN-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40NN-900. GO TO F40NC-FN.
       F40NN-FN. EXIT.
      *N40NP.    NOTE *ROTH CONVERSION                    *.
       F40NP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40NP-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40NP-900. GO TO F40NC-FN.
       F40NP-FN. EXIT.
      *N40NR.    NOTE *ROTH BENEFICIAL                    *.
       F40NR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40NR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NR-900. GO TO F40NC-FN.
       F40NR-FN. EXIT.
      *N40NT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40NT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40NT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NT-900. GO TO F40NC-FN.
       F40NT-FN. EXIT.
      *N40NV.    NOTE *COVERDELL ESA                      *.
       F40NV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40NV-FN.
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NV-900. GO TO F40NC-FN.
       F40NV-FN. EXIT.
      *N40NZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40NZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40NZ-FN. EXIT.
       F40NC-FN. EXIT.
       F40NB-900. GO TO F40FB-FN.
       F40NB-FN. EXIT.
      *N40OB.    NOTE *IF SOURCE WAS ROTH BENEFICIAL      *.
       F40OB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40OB-FN.
      *N40OC.    NOTE *BASED ON DESTINATION               *.
       F40OC.         EXIT.                                             lv20
      *N40OD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40OD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40OD-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40OD-900. GO TO F40OC-FN.
       F40OD-FN. EXIT.
      *N40OF.    NOTE *IRA ROLLOVER                       *.
       F40OF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40OF-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40OF-900. GO TO F40OC-FN.
       F40OF-FN. EXIT.
      *N40OH.    NOTE *IRA BENEFICIAL                     *.
       F40OH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40OH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40OH-900. GO TO F40OC-FN.
       F40OH-FN. EXIT.
      *N40OJ.    NOTE *SEP                                *.
       F40OJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40OJ-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40OJ-900. GO TO F40OC-FN.
       F40OJ-FN. EXIT.
      *N40OL.    NOTE *SRA                                *.
       F40OL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40OL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40OL-900. GO TO F40OC-FN.
       F40OL-FN. EXIT.
      *N40ON.    NOTE *ROTH                               *.
       F40ON.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40ON-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
                 IF    DIFFERENT-TAXPAYERS                              DOT
                 AND   PJ53-MAPPN = 'UD'
           MOVE        'CU' TO PJ53-CIRAP (1)
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40ON-900. GO TO F40OC-FN.
       F40ON-FN. EXIT.
      *N40OP.    NOTE *ROTH CONVERSION                    *.
       F40OP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40OP-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40OP-900. GO TO F40OC-FN.
       F40OP-FN. EXIT.
      *N40OR.    NOTE *ROTH BENEFICIAL                    *.
       F40OR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40OR-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40OR-900. GO TO F40OC-FN.
       F40OR-FN. EXIT.
      *N40OT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40OT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40OT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40OT-900. GO TO F40OC-FN.
       F40OT-FN. EXIT.
      *N40OV.    NOTE *COVERDELL ESA                      *.
       F40OV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 NEXT SENTENCE ELSE GO TO     F40OV-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'CU' TO PJ53-CIRAP (1).
                 IF    PJ53-DCACG > 20020430                            DOT
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40OV-900. GO TO F40OC-FN.
       F40OV-FN. EXIT.
      *N40OZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40OZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40OZ-FN. EXIT.
       F40OC-FN. EXIT.
       F40OB-900. GO TO F40FB-FN.
       F40OB-FN. EXIT.
      *N40PB.    NOTE *IF SRCE WAS ROTH CNV BENEFICIAL    *.
       F40PB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40PB-FN.
      *N40PC.    NOTE *BASED ON DESTINATION               *.
       F40PC.         EXIT.                                             lv20
      *N40PD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40PD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40PD-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40PD-900. GO TO F40PC-FN.
       F40PD-FN. EXIT.
      *N40PF.    NOTE *IRA ROLLOVER                       *.
       F40PF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40PF-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40PF-900. GO TO F40PC-FN.
       F40PF-FN. EXIT.
      *N40PH.    NOTE *IRA BENEFICIAL                     *.
       F40PH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40PH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40PH-900. GO TO F40PC-FN.
       F40PH-FN. EXIT.
      *N40PJ.    NOTE *SEP                                *.
       F40PJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40PJ-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2)
           MOVE        'SC' TO PJ53-CIRAP (3)
           MOVE        'RO' TO PJ53-CIRAP (4).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40PJ-900. GO TO F40PC-FN.
       F40PJ-FN. EXIT.
      *N40PL.    NOTE *SRA                                *.
       F40PL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40PL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40PL-900. GO TO F40PC-FN.
       F40PL-FN. EXIT.
      *N40PN.    NOTE *ROTH                               *.
       F40PN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40PN-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40PN-900. GO TO F40PC-FN.
       F40PN-FN. EXIT.
      *N40PP.    NOTE *ROTH CONVERSION                    *.
       F40PP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40PP-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'PR' TO PJ53-CIRAP (1)
           MOVE        'CU' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40PP-900. GO TO F40PC-FN.
       F40PP-FN. EXIT.
      *N40PR.    NOTE *ROTH BENEFICIAL                    *.
       F40PR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40PR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40PR-900. GO TO F40PC-FN.
       F40PR-FN. EXIT.
      *N40PT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40PT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40PT-FN.
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40PT-900. GO TO F40PC-FN.
       F40PT-FN. EXIT.
      *N40PV.    NOTE *COVERDELL ESA                      *.
       F40PV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 NEXT SENTENCE ELSE GO TO     F40PV-FN.
                 IF    DIFFERENT-TAXPAYERS                              DOT
           MOVE        'CU' TO PJ53-CIRAP (1).
                 IF    PJ53-DCACG > 20020430                            DOT
           MOVE        'PR' TO PJ53-CIRAP (2).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40PV-900. GO TO F40PC-FN.
       F40PV-FN. EXIT.
      *N40PZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40PZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40PZ-FN. EXIT.
       F40PC-FN. EXIT.
       F40PB-900. GO TO F40FB-FN.
       F40PB-FN. EXIT.
      *N40QB.    NOTE *IF SOURCE COVERDELL ESA            *.
       F40QB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '007001'
                 NEXT SENTENCE ELSE GO TO     F40QB-FN.
      *N40QC.    NOTE *BASED ON DESTINATION               *.
       F40QC.         EXIT.                                             lv20
      *N40QD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40QD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40QD-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QD-900. GO TO F40QC-FN.
       F40QD-FN. EXIT.
      *N40QF.    NOTE *IRA ROLLOVER                       *.
       F40QF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40QF-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QF-900. GO TO F40QC-FN.
       F40QF-FN. EXIT.
      *N40QH.    NOTE *IRA BENEFICIAL                     *.
       F40QH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40QH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QH-900. GO TO F40QC-FN.
       F40QH-FN. EXIT.
      *N40QJ.    NOTE *SEP                                *.
       F40QJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40QJ-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QJ-900. GO TO F40QC-FN.
       F40QJ-FN. EXIT.
      *N40QL.    NOTE *SRA                                *.
       F40QL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40QL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QL-900. GO TO F40QC-FN.
       F40QL-FN. EXIT.
      *N40QN.    NOTE *ROTH                               *.
       F40QN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40QN-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QN-900. GO TO F40QC-FN.
       F40QN-FN. EXIT.
      *N40QP.    NOTE *ROTH CONVERSION                    *.
       F40QP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40QP-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QP-900. GO TO F40QC-FN.
       F40QP-FN. EXIT.
      *N40QR.    NOTE *ROTH BENEFICIAL                    *.
       F40QR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40QR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QR-900. GO TO F40QC-FN.
       F40QR-FN. EXIT.
      *N40QT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40QT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40QT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QT-900. GO TO F40QC-FN.
       F40QT-FN. EXIT.
      *N40QV.    NOTE *COVERDELL ESA AND CALLING APPL     *.
       F40QV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 AND   PJ53-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F40QV-FN.
      *IS EZTRANS
           MOVE        'IT' TO PJ53-CIRAP (1)
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QV-900. GO TO F40QC-FN.
       F40QV-FN. EXIT.
      *N40QX.    NOTE *COVERDELL ESA AND CALLING APPL     *.
       F40QX.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 AND   PJ53-MAPPN = 'FDC'
                 NEXT SENTENCE ELSE GO TO     F40QX-FN.
      *IS CLIENT VIEWER
                 IF    BOTH-ACCTS-SAME-TAXPAYER                         DOT
           MOVE        'IT' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40QX-900. GO TO F40QC-FN.
       F40QX-FN. EXIT.
      *N40QZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40QZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40QZ-FN. EXIT.
       F40QC-FN. EXIT.
       F40QB-900. GO TO F40FB-FN.
       F40QB-FN. EXIT.
      *N40RB.    NOTE *IF SOURCE IS PENSION ACCOUNT       *.
       F40RB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '999000'
                 NEXT SENTENCE ELSE GO TO     F40RB-FN.
      *N40RC.    NOTE *BASED ON DESTINATION               *.
       F40RC.         EXIT.                                             lv20
      *N40RD.    NOTE *INDIVIDUAL IRA DESTINATION         *.
       F40RD.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001001'
                 NEXT SENTENCE ELSE GO TO     F40RD-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RD-900. GO TO F40RC-FN.
       F40RD-FN. EXIT.
      *N40RF.    NOTE *IRA ROLLOVER                       *.
       F40RF.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001002'
                 NEXT SENTENCE ELSE GO TO     F40RF-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RF-900. GO TO F40RC-FN.
       F40RF-FN. EXIT.
      *N40RH.    NOTE *IRA BENEFICIAL                     *.
       F40RH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '001003'
                 NEXT SENTENCE ELSE GO TO     F40RH-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RH-900. GO TO F40RC-FN.
       F40RH-FN. EXIT.
      *N40RJ.    NOTE *SEP                                *.
       F40RJ.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003001'
                 NEXT SENTENCE ELSE GO TO     F40RJ-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RJ-900. GO TO F40RC-FN.
       F40RJ-FN. EXIT.
      *N40RL.    NOTE *SRA                                *.
       F40RL.    IF    DESTINATION-ACCOUNT =                            lv25
                       '004001'
                 NEXT SENTENCE ELSE GO TO     F40RL-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RL-900. GO TO F40RC-FN.
       F40RL-FN. EXIT.
      *N40RN.    NOTE *ROTH                               *.
       F40RN.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005001'
                 NEXT SENTENCE ELSE GO TO     F40RN-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RN-900. GO TO F40RC-FN.
       F40RN-FN. EXIT.
      *N40RP.    NOTE *ROTH CONVERSION                    *.
       F40RP.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006001'
                 NEXT SENTENCE ELSE GO TO     F40RP-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RP-900. GO TO F40RC-FN.
       F40RP-FN. EXIT.
      *N40RR.    NOTE *ROTH BENEFICIAL                    *.
       F40RR.    IF    DESTINATION-ACCOUNT =                            lv25
                       '005003'
                 NEXT SENTENCE ELSE GO TO     F40RR-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RR-900. GO TO F40RC-FN.
       F40RR-FN. EXIT.
      *N40RT.    NOTE *ROTH CONVERSION BENEFICIAL         *.
       F40RT.    IF    DESTINATION-ACCOUNT =                            lv25
                       '006003'
                 NEXT SENTENCE ELSE GO TO     F40RT-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RT-900. GO TO F40RC-FN.
       F40RT-FN. EXIT.
      *N40RV.    NOTE *COVERDELL ESA                      *.
       F40RV.    IF    DESTINATION-ACCOUNT =                            lv25
                       '007001'
                 NEXT SENTENCE ELSE GO TO     F40RV-FN.
      *NOT AVAILABLE AS OPTION
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RV-900. GO TO F40RC-FN.
       F40RV-FN. EXIT.
      *N40RZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40RZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40RZ-FN. EXIT.
       F40RC-FN. EXIT.
       F40RB-900. GO TO F40FB-FN.
       F40RB-FN. EXIT.
      *N40SB.    NOTE *IF SOURCE WAS SEP BENEFICIAL       *.
       F40SB.    IF    SOURCE-ACCOUNT =                                 lv15
                       '003003'
                 NEXT SENTENCE ELSE GO TO     F40SB-FN.
      *N40SC.    NOTE *BASED ON DESTINATION               *.
       F40SC.         EXIT.                                             lv20
      *N40SH.    NOTE *SEP BENEFICIAL AND CALLING         *.
       F40SH.    IF    DESTINATION-ACCOUNT =                            lv25
                       '003003'
                 NEXT SENTENCE ELSE GO TO     F40SH-FN.
      *APPLICATION IS CLIENT VIEWER
                 IF    PJ53-MAPPN = 'FDC'                               DOT
                 AND   BOTH-ACCTS-SAME-TAXPAYER
           MOVE        'IT' TO PJ53-CIRAP (1).
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40SH-900. GO TO F40SC-FN.
       F40SH-FN. EXIT.
      *N40SZ.    NOTE *NONE OF THE ABOVE - EXIT           *.
       F40SZ.                                                           lv25
           MOVE                     ALL '1' TO FT GO TO F20.
       F40SZ-FN. EXIT.
       F40SC-FN. EXIT.
       F40SB-900. GO TO F40FB-FN.
       F40SB-FN. EXIT.
       F40FB-FN. EXIT.
       F40-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Normal Termination            *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F79.                                                             lv05
      *     Return to Calling Module                                    ADU102
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F79-FN.   EXIT.
      *N90.      NOTE *************************************.
      *               *                                   *
      *               *COMPARE TAXPAYERS FOR THE SRCE     *
      *               *                                   *
      *               *************************************.
       F90.                                                             lv05
      *AND DEST ACCOUNTS.
      *N90BB.    NOTE *RETRIEVE SOURCE TAXPAYER           *.
       F90BB.                                                           lv10
           MOVE        PJ53-CTID (1) TO S-CTU01-CT01K
      *PERFORM BOOLEAN READ OF CT09
           PERFORM     F94CB THRU F94CB-FN
      *PULL TAXPAYER FROM KEY FEEDBACK
           MOVE        XW05-XCOKEY (28:23) TO
           7-SRCE-CLID.
                 IF    IK = '1'                                         DOT
      *IF READ FAILED, MOVE ZERO
           MOVE        ZERO TO 7-SRCE-CLID.
       F90BB-FN. EXIT.
      *N90CB.    NOTE *RETRIEVE DESTINATION TAXPAYER      *.
       F90CB.                                                           lv10
           MOVE        PJ53-CTID (2) TO S-CTU01-CT01K
      *PERFORM BOOLEAN READ OF CT09
           PERFORM     F94CB THRU F94CB-FN
      *PULL TAXPAYER FROM KEY FEEDBACK
           MOVE        XW05-XCOKEY (28:23) TO
           7-DEST-CLID.
                 IF    IK = '1'                                         DOT
      *IF READ FAILED, MOVE ZERO
           MOVE        ZERO TO 7-DEST-CLID.
       F90CB-FN. EXIT.
      *N90CG.    NOTE *ARE TAXPAYERS THE SAME?            *.
       F90CG.    IF    7-SRCE-CLID = 7-DEST-CLID                        lv10
                 NEXT SENTENCE ELSE GO TO     F90CG-FN.
           MOVE        'Y' TO 7-SAME-TAXPAYERS.
       F90CG-FN. EXIT.
       F90-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED CODE                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91DV.    NOTE *DATE VALIDATION                    *.            AADA16
       F91DV.                                                           lv10
           MOVE        1 TO XW03-RETURN.                                AADA16
                 IF    XW03-XDATG NOT NUMERIC                           DOT
           MOVE        4 TO XW03-RETURN                                 AADA16
               GO TO     F91DV-FN.                                      AADA16
                 IF    XW03-XDAT3 > '12'                                DOT
                 OR    XW03-XDAT3 = '00'                                AADA16
                 OR    XW03-XDAT4 > '31'                                AADA16
                 OR    XW03-XDAT4 = '00'                                AADA16
           MOVE        5 TO XW03-RETURN                                 AADA16
               GO TO     F91DV-FN.                                      AADA16
                 IF    XW03-XDAT4 > '30'                                DOT
                 AND   (XW03-XDAT3 = '04'                               AADA16
                 OR    XW03-XDAT3 = '06'                                AADA16
                 OR    XW03-XDAT3 = '09'                                AADA16
                 OR    XW03-XDAT3 = '11')                               AADA16
           MOVE        5 TO XW03-RETURN                                 AADA16
               GO TO     F91DV-FN.                                      AADA16
                 IF    XW03-XDAT3 NOT = '02'                            DOT
               GO TO     F91DV-FN.                                      AADA16
                 IF    XW03-XDAT4 > '29'                                DOT
           MOVE        5 TO XW03-RETURN                                 AADA16
               GO TO     F91DV-FN.                                      AADA16
           MOVE        XW03-XDAT29 TO XW03-DTGYY                        DOT
           MOVE        XW03-XDAT19 TO XW03-DTGCC.                       AADA16
                 IF    XW03-DTGYY NOT = ZERO                            DOT
           COMPUTE     XW03-XLEAPY = XW03-DTGCY -                       AADA16
           ((XW03-DTGCY / 4) * 4)                                       AADA16
                 ELSE                                                   AADA16
           COMPUTE     XW03-XLEAPY = (XW03-DTGCY -                      AADA16
           ((XW03-DTGCY / 400) * 400))                                  AADA16
           / 100.                                                       AADA16
                 IF    XW03-XLEAPY NOT = ZERO                           DOT
                 AND   XW03-XDAT4 > '28'                                AADA16
           MOVE        5 TO XW03-RETURN                                 AADA16
               GO TO     F91DV-FN.                                      AADA16
       F91DV-FN. EXIT.
       F91-FN.   EXIT.
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
      *N94CA.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CA-FN. EXIT.
      *N94CB.    NOTE *CALL GU ON CT09                    *.            ADU026
       F94CB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT09' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT09                                                    ADU026
           S-CTU01-SSA S-CT07-SSA                                       ADU026
           7-CTA09-1-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CB-FN. EXIT.
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
