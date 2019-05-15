       IDENTIFICATION DIVISION.                                         CI0113
       PROGRAM-ID.  CI0113P.                                            CI0113
      *AUTHOR.         UD BUILD ACTIVITY.                               CI0113
      *DATE-COMPILED.   09/08/14.                                       CI0113
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
       ENVIRONMENT DIVISION.                                            CI0113
       CONFIGURATION SECTION.                                           CI0113
       SOURCE-COMPUTER. IBM-370.                                        CI0113
       OBJECT-COMPUTER. IBM-370.                                        CI0113
       DATA DIVISION.                                                   CI0113
       WORKING-STORAGE SECTION.                                         CI0113
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0113
            10            XW05-XW06.                                    CI0113
            11            XW05-XDBPCB.                                  CI0113
            12            XW05-XDBDNM PICTURE  X(08)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-XSEGLV PICTURE  X(02)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-XRC    PICTURE  X(02)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-XPROPT PICTURE  X(04)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-FILLER PICTURE  S9(5)                    CI0113
                          VALUE                ZERO                     CI0113
                          BINARY.                                       CI0113
            12            XW05-XSEGNM PICTURE  X(08)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0113
                          VALUE                ZERO                     CI0113
                          BINARY.                                       CI0113
            12            XW05-XSEGNB PICTURE  9(05)                    CI0113
                          VALUE                ZERO                     CI0113
                          BINARY.                                       CI0113
            12            XW05-XCOKEY PICTURE  X(70)                    CI0113
                          VALUE                SPACE.                   CI0113
            10            XW05-XW07.                                    CI0113
            11            XW05-XIOPCB.                                  CI0113
            12            XW05-XTERMI PICTURE  X(08)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-FILLER PICTURE  XX                       CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-XRC1   PICTURE  X(02)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-FILLER PICTURE  X(12)                    CI0113
                          VALUE                SPACE.                   CI0113
            12            XW05-XMODNM PICTURE  X(8)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0113
                          VALUE                ZERO.                    CI0113
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0113
                          VALUE                ZERO.                    CI0113
            10            XW05-XGU    PICTURE  X(4)                     CI0113
                          VALUE                'GU  '.                  CI0113
            10            XW05-XGHU   PICTURE  X(4)                     CI0113
                          VALUE                'GHU '.                  CI0113
            10            XW05-XGN    PICTURE  X(4)                     CI0113
                          VALUE                'GN  '.                  CI0113
            10            XW05-XGHN   PICTURE  X(4)                     CI0113
                          VALUE                'GHN '.                  CI0113
            10            XW05-XGNP   PICTURE  X(4)                     CI0113
                          VALUE                'GNP '.                  CI0113
            10            XW05-XGHNP  PICTURE  X(4)                     CI0113
                          VALUE                'GHNP'.                  CI0113
            10            XW05-XREPL  PICTURE  XXXX                     CI0113
                          VALUE                'REPL'.                  CI0113
            10            XW05-XISRT  PICTURE  X(4)                     CI0113
                          VALUE                'ISRT'.                  CI0113
            10            XW05-XDLET  PICTURE  X(4)                     CI0113
                          VALUE                'DLET'.                  CI0113
            10            XW05-XOPEN  PICTURE  X(4)                     CI0113
                          VALUE                'OPEN'.                  CI0113
            10            XW05-XCLSE  PICTURE  X(4)                     CI0113
                          VALUE                'CLSE'.                  CI0113
            10            XW05-XCHKP  PICTURE  X(4)                     CI0113
                          VALUE                'CHKP'.                  CI0113
            10            XW05-XXRST  PICTURE  X(4)                     CI0113
                          VALUE                'XRST'.                  CI0113
            10            XW05-XTERM  PICTURE  X(4)                     CI0113
                          VALUE                'TERM'.                  CI0113
            10            XW05-XNFPAC PICTURE  X(13)                    CI0113
                          VALUE                SPACE.                   CI0113
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0113
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0113
       01               7-FA00.                                         $MOD10
         05             7-FA00-NTRAC.                                   $MOD10
             15         7-FA00-N1       PIC 9.                          $MOD10
             15         7-FA00-N2       PIC 9.                          $MOD10
             15         7-FA00-N3       PIC 9.                          $MOD10
             15         7-FA00-N4       PIC 9.                          $MOD10
             15         7-FA00-N5       PIC 9.                          $MOD10
             15         7-FA00-N6       PIC 9.                          $MOD10
           10           7-FA00-NTR.                                     $MOD10
             15         7-FA00-N7       PIC 9.                          $MOD10
             15         7-FA00-N8       PIC 9.                          $MOD10
             15         7-FA00-N9       PIC 9.                          $MOD10
             15         7-FA00-N0       PIC 9.                          $MOD10
             15         7-FA00-NN1      PIC 9.                          $MOD10
             15         7-FA00-NN2      PIC 9.                          $MOD10
             15         7-FA00-NN3      PIC 9.                          $MOD10
             15         7-FA00-NN4      PIC 9.                          $MOD10
         05             7-FA00-FILLER.                                  $MOD10
           10           7-FA00-N11      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N22      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N33      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N44      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N55      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N66      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N77      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N88      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N99      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N00      PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N111     PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N222     PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N333     PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-N444     PIC 999  VALUE ZEROES.          $MOD10
           10           7-FA00-TOTAL REDEFINES 7-FA00-N444.             $MOD10
             15         7-FA00-TOT1     PIC 9.                          $MOD10
             15         7-FA00-TOT2     PIC 9.                          $MOD10
             15         7-FA00-TOT3     PIC 9.                          $MOD10
      *!WI pl=FA137                                                     $MOD10
         05             7-FA00-GECKD                                    $MOD10
                        PICTURE 9.                                      CI0113
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
       01                 SB01.                                         CI0113
            10            SB01-R000.                                    CI0113
            11            SB01-GELL   PICTURE  9(4)                     CI0113
                          BINARY.                                       CI0113
            10            SB01-R001.                                    CI0113
            11            SB01-SB01K.                                   CI0113
            12            SB01-CBUPT  PICTURE  9(02).                   CI0113
            12            SB01-CTIDA  PICTURE  9(3).                    CI0113
            12            SB01-FILLER PICTURE  X(52).                   CI0113
            12            SB01-DL13.                                    CI0113
            13            SB01-GEYR   PICTURE  9(4).                    CI0113
            13            SB01-GEMTH  PICTURE  99.                      CI0113
            13            SB01-NDAY   PICTURE  99.                      CI0113
            10            SB01-R002                                     CI0113
                          REDEFINES            SB01-R001.               CI0113
            11            SB01-FILLER PICTURE  X(05).                   CI0113
            11            SB01-PRCOD  PICTURE  9(5).                    CI0113
            11            SB01-PRSCD  PICTURE  X(9).                    CI0113
            11            SB01-CPRSCN                                   CI0113
                          REDEFINES            SB01-PRSCD               CI0113
               PICTURE    9(9).                                         CI0113
            11            SB01-FILLER PICTURE  X(46).                   CI0113
            10            SB01-R003                                     CI0113
                          REDEFINES            SB01-R001.               CI0113
            11            SB01-FILLER PICTURE  X(05).                   CI0113
            11            SB01-PRCODX PICTURE  9(5).                    CI0113
            11            SB01-FILLER PICTURE  X(55).                   CI0113
            10            SB01-R004                                     CI0113
                          REDEFINES            SB01-R001.               CI0113
            11            SB01-FILLER PICTURE  X(19).                   CI0113
            11            SB01-CLABA  PICTURE  9(13).                   CI0113
            11            SB01-CLBAN  PICTURE  X(25).                   CI0113
            11            SB01-FILLER PICTURE  X(08).                   CI0113
            10            SB01-R005                                     CI0113
                          REDEFINES            SB01-R001.               CI0113
            11            SB01-FILLER PICTURE  X(05).                   CI0113
            11            SB01-CLDTY  PICTURE  XX.                      CI0113
            11            SB01-FILLER PICTURE  X(58).                   CI0113
            10            SB01-R999.                                    CI0113
            11            SB01-FILLER PICTURE  X(32).                   CI0113
            10            SB01-R901                                     CI0113
                          REDEFINES            SB01-R999.               CI0113
            11            SB01-DRDIV  PICTURE  9(8).                    CI0113
            11            SB01-DICHK  PICTURE  9(8).                    CI0113
            11            SB01-ADRPS  PICTURE  9(2)V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            SB01-ARSTG  PICTURE  9(2)V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            SB01-ARLTG  PICTURE  9(2)V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            SB01-QNODD  PICTURE  9(4).                    CI0113
            10            SB01-R902                                     CI0113
                          REDEFINES            SB01-R999.               CI0113
            11            SB01-NLCRT  PICTURE  9(12).                   CI0113
            10            SB01-R903                                     CI0113
                          REDEFINES            SB01-R999.               CI0113
            11            SB01-NCKLI  PICTURE  9(12).                   CI0113
            10            SB01-R904                                     CI0113
                          REDEFINES            SB01-R999.               CI0113
            11            SB01-DLACG  PICTURE  9(8).                    CI0113
            11            SB01-DCACG  PICTURE  9(8).                    CI0113
            11            SB01-DNACG  PICTURE  9(8).                    CI0113
            11            SB01-IPRLD  PICTURE  X.                       CI0113
            11            SB01-IPRVS  PICTURE  X.                       CI0113
            11            SB01-IDIVS  PICTURE  X.                       CI0113
            11            SB01-ILAUP  PICTURE  X.                       CI0113
            10            SB01-R905                                     CI0113
                          REDEFINES            SB01-R999.               CI0113
            11            SB01-DPRGL  PICTURE  9(8).                    CI0113
            11            SB01-DFICE  PICTURE  9(8).                    CI0113
            11            SB01-DLAUPL PICTURE  9(8).                    CI0113
            10            SB01-R906                                     CI0113
                          REDEFINES            SB01-R999.               CI0113
            11            SB01-PNAYR  PICTURE  S9(4)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            SB01-PPOYR  PICTURE  S9(4)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            SB01-FILLER PICTURE  X(6).                    CI0113
      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0113
           04    G-TA5B-PARAM.                                          CI0113
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0113
                        VALUE      +154.                                CI0113
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0113
                        VALUE      +001.                                CI0113
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0113
                        VALUE      +017.                                CI0113
             10  G-TA5B-NUAPP  PICTURE 99                               CI0113
                        VALUE       0.                                  CI0113
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0113
                        VALUE 'TA005B'.                                 CI0113
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0113
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0113
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0113
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0113
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0113
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0113
             10  G-TA5B-FILSYS.                                         CI0113
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0113
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0113
           04             TA5B.                                         CI0113
            10            TA5B-GAPSC.                                   CI0113
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0113
                          VALUE                ZERO.                    CI0113
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0113
                          VALUE                ZERO.                    CI0113
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-PRCODX PICTURE  9(5)                     CI0113
                          VALUE                ZERO.                    CI0113
            10            TA5B-PRCSUB PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-PRCAUT PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-PRCBAS PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-PRCSTK PICTURE  XX                       CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IBDUP  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IUSPR  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-GRSFC  PICTURE  99                       CI0113
                          VALUE                ZERO.                    CI0113
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ITERM  PICTURE  X(1)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0113
                          VALUE                ZERO.                    CI0113
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-CFXDM  PICTURE  99                       CI0113
                          VALUE                ZERO.                    CI0113
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-CTNLI  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-CBANK  PICTURE  X(03)                    CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ISYPO  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ISYPP  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ICOPT  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IANPY  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IDSAR  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ICIPT  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IANDS  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IKPMA  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-INMWT  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IVANT  PICTURE  X(1)                     CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ISDAV  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-IUDAV  PICTURE  X                        CI0113
                          VALUE                SPACE.                   CI0113
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0113
                          VALUE                SPACE.                   CI0113
      **                                                                ADUTAB
      ******************************************************************
      ** MISCELLANEOUS WORK FIELDS                                     *
      ******************************************************************
       01  7-WS00-AREA.

      *    FIELD USED TO INDICATE IF TA5B ACCESS WAS SUCCESSFUL
      *    '0' - TA5B ENTRY FOUND
      *    '1' - TA5B ENTRY NOT FOUND
           05  TA5B-IK          PIC X(01).

      *    FIELD USED TO CALCULATE THE WITHHOLDING
      *!WI
           05  7-TEMP-CTWHAT
                        PICTURE S9(7)V99                                CI0113
                          COMPUTATIONAL-3.                              CI0113

      *    FIELD USED TO CALCULATE THE SHARES FOR AN EXPRESS MAIL FEE
      *!WI
           05  7-TEMP-QCSHOW
                        PICTURE S9(9)V999                               CI0113
                          COMPUTATIONAL-3.                              CI0113

      *    FIELD USED TO STORE THE TEXT FOR THE FIRST LINE OF FEE
      *!WI
           05  7-SFEE-GENAL
                        PICTURE X(30)                                   CI0113
              VALUE 'AMERIPRISE FINANCIAL SERVICES '.

       01   DEBUT-WSS.                                                  CI0113
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0113
            05   IK     PICTURE X.                                      CI0113
       01  CONSTANTES-PAC.                                              CI0113
           05  FILLER  PICTURE X(87)   VALUE                            CI0113
                     '6015 CAT09/08/14CI0113ADMIN   14:34:54CI0113P AMERCI0113
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0113
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0113
           05  NUGNA   PICTURE X(5).                                    CI0113
           05  APPLI   PICTURE X(3).                                    CI0113
           05  DATGN   PICTURE X(8).                                    CI0113
           05  PROGR   PICTURE X(6).                                    CI0113
           05  CODUTI  PICTURE X(8).                                    CI0113
           05  TIMGN   PICTURE X(8).                                    CI0113
           05  PROGE   PICTURE X(8).                                    CI0113
           05  COBASE  PICTURE X(4).                                    CI0113
           05  DATGNC  PICTURE X(10).                                   CI0113
           05  RELEAS  PICTURE X(7).                                    CI0113
           05  DATGE   PICTURE X(10).                                   CI0113
           05  DATSQ   PICTURE X(10).                                   CI0113
       01  DATCE.                                                       CI0113
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0113
         05  DATOR.                                                     CI0113
           10  DATOA  PICTURE XX.                                       CI0113
           10  DATOM  PICTURE XX.                                       CI0113
           10  DATOJ  PICTURE XX.                                       CI0113
       01  DAT6.                                                        CI0113
            10 DAT61.                                                   CI0113
            15 DAT619  PICTURE 99.                                      CI0113
            10 DAT62.                                                   CI0113
            15 DAT629  PICTURE 99.                                      CI0113
            10 DAT63   PICTURE XX.                                      CI0113
       01  DAT8.                                                        CI0113
            10 DAT81   PICTURE XX.                                      CI0113
            10 DAT8S1  PICTURE X.                                       CI0113
            10 DAT82   PICTURE XX.                                      CI0113
            10 DAT8S2  PICTURE X.                                       CI0113
            10 DAT83   PICTURE XX.                                      CI0113
       01  DAT8E    REDEFINES    DAT8.                                  CI0113
            10 DAT81E  PICTURE X(4).                                    CI0113
            10 DAT82E  PICTURE XX.                                      CI0113
            10 DAT83E  PICTURE XX.                                      CI0113
       01  DAT6C.                                                       CI0113
            10  DAT61C PICTURE XX.                                      CI0113
            10  DAT62C PICTURE XX.                                      CI0113
            10  DAT63C.                                                 CI0113
             15 DAT63CC PICTURE XX.                                     CI0113
             15 DAT64C  PICTURE XX.                                     CI0113
       01  DAT8C.                                                       CI0113
            10  DAT81C  PICTURE XX.                                     CI0113
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0113
            10  DAT82C  PICTURE XX.                                     CI0113
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0113
            10  DAT83C.                                                 CI0113
             15 DAT83CC PICTURE XX.                                     CI0113
             15 DAT84C  PICTURE XX.                                     CI0113
       01  DATSEP     PICTURE X VALUE '/'.                              CI0113
       01  DATSEW     PICTURE X.                                        CI0113
       01   VARIABLES-CONDITIONNELLES.                                  CI0113
            05                  FT      PICTURE X VALUE '0'.            CI0113
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0113
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0113
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0113
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0113
            05       5-EP00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0113
            05       5-GC00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0113
            05       5-LJ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0113
            05       5-PJ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0113
            05       5-SB00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0113
            05       5-TO00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0113
       01               S-SB01-SSA.                                     CI0113
            10         S1-SB01-SEGNAM PICTURE X(8)                      CI0113
                                      VALUE 'SB01    '.                 CI0113
            10         S1-SB01-CCOM   PICTURE X VALUE '*'.              CI0113
            10          S-SB01-CCOD   PICTURE X(5)                      CI0113
                                      VALUE '-----'.                    CI0113
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0113
       01            S-SBU01-SSA.                                       CI0113
            11      S1-SBU01-SEGNAM PICTURE X(8)                        CI0113
                                      VALUE 'SB01    '.                 CI0113
            11      S1-SBU01-CCOM   PICTURE X VALUE '*'.                CI0113
            11       S-SBU01-CCOD   PICTURE X(5)                        CI0113
                                      VALUE '-----'.                    CI0113
            11      S1-SBU01-FLDNAM PICTURE X(9)                        CI0113
                                      VALUE '(SB01K'.                   CI0113
            11       S-SBU01-OPER  PICTURE XX VALUE ' ='.               CI0113
            11       S-SBU01-SB01K.                                     CI0113
            12       S-SBU01-CBUPT    PICTURE  9(02).                   CI0113
            12       S-SBU01-CTIDA    PICTURE  9(3).                    CI0113
            12       S-SBU01-FILLER   PICTURE  X(52).                   CI0113
            12       S-SBU01-DL13.                                      CI0113
            13       S-SBU01-GEYR     PICTURE  9(4).                    CI0113
            13       S-SBU01-GEMTH    PICTURE  99.                      CI0113
            13       S-SBU01-NDAY     PICTURE  99.                      CI0113
            11  FILLER   PICTURE X    VALUE ')'.                        CI0113
       01   ZONES-UTILISATEUR PICTURE X.                                CI0113
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
      ** PCB POINTER FOR SBUP                                           ADU015
            05 PCB-SBUP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR SBUP                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0113
          05              PB00-SUITE.                                   CI0113
            15       FILLER         PICTURE  X(00106).                  CI0113
       01                 PB06  REDEFINES      PB00.                    CI0113
            10            PB06-XDBPCB.                                  CI0113
            11            PB06-XDBDNM PICTURE  X(08).                   CI0113
            11            PB06-XSEGLV PICTURE  X(02).                   CI0113
            11            PB06-XRC    PICTURE  X(02).                   CI0113
            11            PB06-XPROPT PICTURE  X(04).                   CI0113
            11            PB06-FILLER PICTURE  S9(5)                    CI0113
                          BINARY.                                       CI0113
            11            PB06-XSEGNM PICTURE  X(08).                   CI0113
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0113
                          BINARY.                                       CI0113
            11            PB06-XSEGNB PICTURE  9(05)                    CI0113
                          BINARY.                                       CI0113
            11            PB06-XCOKEY PICTURE  X(70).                   CI0113
       01                 PJ49.                                         CI0113
            10            PJ49-MAPPN  PICTURE  X(10).                   CI0113
            10            PJ49-CHCR   PICTURE  99.                      CI0113
            10            PJ49-CTRTP  PICTURE  X(2).                    CI0113
            10            PJ49-CPORT  PICTURE  X.                       CI0113
            10            PJ49-GEAUN  PICTURE  9(5).                    CI0113
            10            PJ49-GEOPD2 PICTURE  X(8).                    CI0113
            10            PJ49-GETIM  PICTURE  S9(7)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-DCACG  PICTURE  9(8).                    CI0113
            10            PJ49-DEFFT  PICTURE  9(8).                    CI0113
            10            PJ49-ICUST  PICTURE  X.                       CI0113
            10            PJ49-AEDRQ  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-ADBRQ  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-QSHOWQ PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-PACT1  PICTURE  S999V999                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-QNACT  PICTURE  9(3).                    CI0113
            10            PJ49-ADDACT PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-CTWHAT PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-PWHLD  PICTURE  S999V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-IPLIN  PICTURE  X.                       CI0113
            10            PJ49-CLIDNB PICTURE  9(8).                    CI0113
            10            PJ49-CCDSCW PICTURE  9(2).                    CI0113
            10            PJ49-CLCUS  PICTURE  99.                      CI0113
            10            PJ49-CCACT  PICTURE  99.                      CI0113
            10            PJ49-AFEET  PICTURE  S9(5)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-ITERF  PICTURE  X.                       CI0113
            10            PJ49-ATERF  PICTURE  S9(5)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-CLDOB  PICTURE  9(8).                    CI0113
            10            PJ49-CPLTYP PICTURE  X(14).                   CI0113
            10            PJ49-IACFPD PICTURE  X(1).                    CI0113
            10            PJ49-CPAYF  PICTURE  X(2).                    CI0113
            10            PJ49-ITRNB  PICTURE  X.                       CI0113
            10            PJ49-ACOTD  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-PPOTD  PICTURE  S9(3)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-QPSTD  PICTURE  S9(7)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-NTR    PICTURE  9(8).                    CI0113
            10            PJ49-NPBN   PICTURE  X(20).                   CI0113
            10            PJ49-CCBAT  PICTURE  99.                      CI0113
            10            PJ49-CLID4  PICTURE  X(23).                   CI0113
            10            PJ49-GENAL  PICTURE  X(30)                    CI0113
                          OCCURS       002     TIMES.                   CI0113
            10            PJ49-GESAD  PICTURE  X(30)                    CI0113
                          OCCURS       003     TIMES.                   CI0113
            10            PJ49-CTYPE  PICTURE  X.                       CI0113
            10            PJ49-IOWNC  PICTURE  X.                       CI0113
            10            PJ49-CIRAP  PICTURE  XX.                      CI0113
            10            PJ49-CPMTCB PICTURE  X(3).                    CI0113
            10            PJ49-CTCUS  PICTURE  999.                     CI0113
            10            PJ49-IEXML  PICTURE  X.                       CI0113
            10            PJ49-AEXML  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-TPAYB  PICTURE  X(30).                   CI0113
            10            PJ49-NAIRB  PICTURE  X(30).                   CI0113
            10            PJ49-FILLER PICTURE  X(40).                   CI0113
            10            PJ49-IGC01  PICTURE  X(01).                   CI0113
            10            PJ49-IGC03  PICTURE  X(01).                   CI0113
            10            PJ49-IGC04  PICTURE  X(01).                   CI0113
            10            PJ49-IGC06  PICTURE  X(01).                   CI0113
            10            PJ49-ACASH  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            PJ49-FILLER PICTURE  X(40).                   CI0113
       01                 GC01.                                         CI0113
            10            GC01-GC01K.                                   CI0113
            11            GC01-C299.                                    CI0113
            12            GC01-CTID.                                    CI0113
            13            GC01-CTIDA  PICTURE  9(3).                    CI0113
            13            GC01-CTIDN.                                   CI0113
            14            GC01-CTIDNP PICTURE  X(13).                   CI0113
            14            GC01-CTIDND PICTURE  9(11).                   CI0113
            10            GC01-DCAG9L PICTURE  9(8).                    CI0113
            10            GC01-NAASQL PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC01-ICUST  PICTURE  X.                       CI0113
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0113
                          BINARY.                                       CI0113
            10            GC01-PRCOD  PICTURE  9(5).                    CI0113
            10            GC01-PRSCD  PICTURE  X(9).                    CI0113
            10            GC01-FILLER PICTURE  X(8).                    CI0113
       01                 GC03.                                         CI0113
            10            GC03-GELL   PICTURE  9(4)                     CI0113
                          BINARY.                                       CI0113
            10            GC03-GD00.                                    CI0113
            11            GC03-GC03K.                                   CI0113
            12            GC03-DCACG9 PICTURE  9(8).                    CI0113
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CAATY  PICTURE  9(3).                    CI0113
            11            GC03-CVSYS  PICTURE  X(2).                    CI0113
            11            GC03-CACTO  PICTURE  9(3).                    CI0113
            11            GC03-CATRN.                                   CI0113
            12            GC03-CATRF  PICTURE  9(3).                    CI0113
            12            GC03-CATRS  PICTURE  9(3).                    CI0113
            11            GC03-CASTC  PICTURE  99.                      CI0113
            11            GC03-IPULL  PICTURE  X.                       CI0113
            11            GC03-GEAUN  PICTURE  9(5).                    CI0113
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0113
            11            GC03-NBTCH  PICTURE  9(4).                    CI0113
            11            GC03-DEFFT  PICTURE  9(8).                    CI0113
            11            GC03-NSUNT  PICTURE  9(4).                    CI0113
            11            GC03-ITRAN  PICTURE  X.                       CI0113
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0113
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-TTRMS  PICTURE  X(12).                   CI0113
            11            GC03-IDELT  PICTURE  X.                       CI0113
            11            GC03-GEOPDM PICTURE  X(8).                    CI0113
            11            GC03-FILLER PICTURE  X(07).                   CI0113
            10            GC03-GD09.                                    CI0113
            11            GC03-FILLER PICTURE  X(70).                   CI0113
            10            GC03-GD01                                     CI0113
                          REDEFINES            GC03-GD09.               CI0113
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CTRTP  PICTURE  X(2).                    CI0113
            11            GC03-CPORT  PICTURE  X.                       CI0113
            11            GC03-CSCRNU PICTURE  X(4).                    CI0113
            11            GC03-DLAUP  PICTURE  9(8).                    CI0113
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-IWTHH  PICTURE  X.                       CI0113
            11            GC03-NDRFT  PICTURE  9(5).                    CI0113
            11            GC03-IDPAP  PICTURE  X.                       CI0113
            11            GC03-GETIM  PICTURE  S9(7)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QNACT  PICTURE  9(3).                    CI0113
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-IPLIN  PICTURE  X.                       CI0113
            11            GC03-CLIDNB PICTURE  9(8).                    CI0113
            11            GC03-CSLCT  PICTURE  X.                       CI0113
            11            GC03-ITELE  PICTURE  X.                       CI0113
            11            GC03-FILLER PICTURE  X(06).                   CI0113
            10            GC03-GD02                                     CI0113
                          REDEFINES            GC03-GD09.               CI0113
            11            GC03-CSYST  PICTURE  99.                      CI0113
            11            GC03-FILLER PICTURE  X.                       CI0113
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-DTRAC  PICTURE  9(8).                    CI0113
            11            GC03-CTRSO  PICTURE  9(02).                   CI0113
            11            GC03-NTRCE  PICTURE  9(06).                   CI0113
            11            GC03-GECKD1 PICTURE  9.                       CI0113
            11            GC03-CCOLL  PICTURE  X(3).                    CI0113
            11            GC03-CLTDP  PICTURE  X(3).                    CI0113
            11            GC03-PSLLD  PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ISLOR  PICTURE  X.                       CI0113
            11            GC03-ITPAC  PICTURE  X.                       CI0113
            11            GC03-CPMTCA PICTURE  XXX.                     CI0113
            11            GC03-CSERV  PICTURE  X(3).                    CI0113
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-IPLIN1 PICTURE  X.                       CI0113
            11            GC03-INQEX  PICTURE  X.                       CI0113
            11            GC03-CTKRAA PICTURE  X(12).                   CI0113
            11            GC03-CCSMQ  PICTURE  X.                       CI0113
            11            GC03-IVAEX1 PICTURE  X.                       CI0113
            11            GC03-IHPMT  PICTURE  X(1).                    CI0113
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC03-GD03                                     CI0113
                          REDEFINES            GC03-GD09.               CI0113
            11            GC03-CATRNC PICTURE  9(6).                    CI0113
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CTSTR  PICTURE  9(2).                    CI0113
            11            GC03-ICIRA  PICTURE  X.                       CI0113
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CPMTCX PICTURE  XX.                      CI0113
            11            GC03-FILLER PICTURE  X(16).                   CI0113
            10            GC03-GD99.                                    CI0113
            11            GC03-FILLER PICTURE  X(248).                  CI0113
            10            GC03-GD10                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-MROTC  PICTURE  X(7).                    CI0113
            11            GC03-CEDSC  PICTURE  9(1).                    CI0113
            11            GC03-ILPOI  PICTURE  X(1).                    CI0113
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0113
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0113
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0113
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0113
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0113
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0113
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0113
            11            GC03-GD11.                                    CI0113
            12            GC03-FILLER PICTURE  X(219).                  CI0113
            11            GC03-GD12                                     CI0113
                          REDEFINES            GC03-GD11.               CI0113
            12            GC03-CELLO  PICTURE  9(1).                    CI0113
            12            GC03-CECLO  PICTURE  9(1).                    CI0113
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CEPI   PICTURE  X(1).                    CI0113
            12            GC03-CEXTY  PICTURE  X.                       CI0113
            12            GC03-CROPC  PICTURE  9(1).                    CI0113
            12            GC03-CPUTY  PICTURE  9(1).                    CI0113
            12            GC03-IMCII  PICTURE  X(1).                    CI0113
            12            GC03-GEMISC                                   CI0113
                          OCCURS       010     TIMES.                   CI0113
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            13            GC03-CMGLC  PICTURE  9(1).                    CI0113
            13            GC03-NMGLN  PICTURE  9(4).                    CI0113
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-IWRBK  PICTURE  X.                       CI0113
            12            GC03-IFEDX  PICTURE  X.                       CI0113
            12            GC03-ICNTR  PICTURE  X.                       CI0113
            12            GC03-IOCKH  PICTURE  X.                       CI0113
            12            GC03-ICRCK  PICTURE  X.                       CI0113
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ITELR1 PICTURE  X.                       CI0113
            11            GC03-GD13                                     CI0113
                          REDEFINES            GC03-GD11.               CI0113
            12            GC03-DREDO  PICTURE  9(8).                    CI0113
            12            GC03-CATRNR PICTURE  9(6).                    CI0113
            12            GC03-CEVN   PICTURE  9(9).                    CI0113
            12            GC03-ISUSP  PICTURE  X(1).                    CI0113
            11            GC03-GD15                                     CI0113
                          REDEFINES            GC03-GD11.               CI0113
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0113
            12            GC03-CETLB  PICTURE  9(3).                    CI0113
            12            GC03-QTRMC  PICTURE  9(3).                    CI0113
            12            GC03-DEFFTE PICTURE  9(8).                    CI0113
            12            GC03-DEFFTF PICTURE  9(8).                    CI0113
            12            GC03-DEFFTG PICTURE  9(8).                    CI0113
            12            GC03-XZ1A   PICTURE  X.                       CI0113
            12            GC03-XZ1B   PICTURE  X.                       CI0113
            12            GC03-XZ1C   PICTURE  X.                       CI0113
            12            GC03-XZ1D   PICTURE  X.                       CI0113
            12            GC03-XZ1E   PICTURE  X.                       CI0113
            12            GC03-XZ1F   PICTURE  X.                       CI0113
            12            GC03-XZ1G   PICTURE  X.                       CI0113
            12            GC03-XZ1H   PICTURE  X.                       CI0113
            12            GC03-XZ1I   PICTURE  X.                       CI0113
            12            GC03-DEFFTH PICTURE  9(8).                    CI0113
            11            GC03-GD19                                     CI0113
                          REDEFINES            GC03-GD11.               CI0113
            12            GC03-GD11.                                    CI0113
            13            GC03-FILLER PICTURE  X(219).                  CI0113
            10            GC03-GD20                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ISIGV  PICTURE  X.                       CI0113
            11            GC03-IALLF  PICTURE  X.                       CI0113
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CCDSCW PICTURE  9(2).                    CI0113
            11            GC03-IDWRL  PICTURE  X.                       CI0113
            11            GC03-ITELR  PICTURE  X.                       CI0113
            11            GC03-IABIN  PICTURE  X.                       CI0113
            11            GC03-PACT1  PICTURE  S999V999                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-IBFAF  PICTURE  X.                       CI0113
            11            GC03-IFRSA  PICTURE  X.                       CI0113
            11            GC03-ICRCAN PICTURE  X.                       CI0113
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-NDTRC  PICTURE  9(8).                    CI0113
            11            GC03-CAERU  PICTURE  X(4).                    CI0113
            11            GC03-IFDGO  PICTURE  X.                       CI0113
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ISLOR2 PICTURE  X.                       CI0113
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CGDIN  PICTURE  X.                       CI0113
            11            GC03-DGDIN  PICTURE  9(8).                    CI0113
            10            GC03-GD30                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-ISKED  PICTURE  X.                       CI0113
            11            GC03-CENXC  PICTURE  9(2).                    CI0113
            11            GC03-GD31.                                    CI0113
            12            GC03-FILLER PICTURE  X(245).                  CI0113
            11            GC03-GD32                                     CI0113
                          REDEFINES            GC03-GD31.               CI0113
            12            GC03-IABIN1 PICTURE  X.                       CI0113
            12            GC03-CLDOD  PICTURE  9(8).                    CI0113
            12            GC03-NCLAM  PICTURE  9(5)                     CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ISURR  PICTURE  X.                       CI0113
            12            GC03-GEHCD  PICTURE  9(3).                    CI0113
            12            GC03-CRATC  PICTURE  9(4).                    CI0113
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-IWTHH1 PICTURE  X.                       CI0113
            12            GC03-CPAYCL PICTURE  X(2).                    CI0113
            12            GC03-CTSAO  PICTURE  X.                       CI0113
            12            GC03-NCONF  PICTURE  9(08).                   CI0113
            12            GC03-CLID   PICTURE  X(23).                   CI0113
            12            GC03-CARTY  PICTURE  99.                      CI0113
            12            GC03-NARRS  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CARTZ  PICTURE  99.                      CI0113
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CPMTO  PICTURE  X.                       CI0113
            12            GC03-DNPMT  PICTURE  9(8).                    CI0113
            12            GC03-IPCTV  PICTURE  X.                       CI0113
            12            GC03-IMECH  PICTURE  X(01).                   CI0113
            12            GC03-IMVAO  PICTURE  X(1).                    CI0113
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CACTS  PICTURE  X.                       CI0113
            12            GC03-CTSPP  PICTURE  X(1).                    CI0113
            12            GC03-CACT4  PICTURE  X(2).                    CI0113
            12            GC03-IVAEX  PICTURE  X.                       CI0113
            12            GC03-DFPMT  PICTURE  9(8).                    CI0113
            12            GC03-IDEMD  PICTURE  X.                       CI0113
            12            GC03-IOFST  PICTURE  X.                       CI0113
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-DEIRNB PICTURE  9(8).                    CI0113
            12            GC03-DEFFE  PICTURE  9(8).                    CI0113
            12            GC03-DEFFR  PICTURE  9(8).                    CI0113
            12            GC03-ISPUP  PICTURE  X.                       CI0113
            12            GC03-CPNCG  PICTURE  X.                       CI0113
            12            GC03-IEXPU  PICTURE  X.                       CI0113
            12            GC03-IPPCF  PICTURE  X.                       CI0113
            12            GC03-NAAPT  PICTURE  9(2).                    CI0113
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ISWHO  PICTURE  X(1).                    CI0113
            11            GC03-GD33                                     CI0113
                          REDEFINES            GC03-GD31.               CI0113
            12            GC03-CPAYC  PICTURE  X(2).                    CI0113
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CTRTPE PICTURE  X(2).                    CI0113
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CLIDN  PICTURE  X(20).                   CI0113
            12            GC03-DSET01 PICTURE  S9(8)                    CI0113
                          BINARY.                                       CI0113
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0113
                          BINARY.                                       CI0113
            12            GC03-DSET02 PICTURE  S9(8)                    CI0113
                          BINARY.                                       CI0113
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0113
                          BINARY.                                       CI0113
            11            GC03-GD34                                     CI0113
                          REDEFINES            GC03-GD31.               CI0113
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-CLTRM  PICTURE  99.                      CI0113
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-IMECH1 PICTURE  X(01).                   CI0113
            12            GC03-CACT41 PICTURE  X(2).                    CI0113
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-GD39                                     CI0113
                          REDEFINES            GC03-GD31.               CI0113
            12            GC03-GD31.                                    CI0113
            13            GC03-FILLER PICTURE  X(245).                  CI0113
            10            GC03-GD40                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-NTR    PICTURE  9(8).                    CI0113
            11            GC03-NPBNC  PICTURE  X(24).                   CI0113
            11            GC03-CRREV  PICTURE  X(3).                    CI0113
            11            GC03-CSUSL  PICTURE  S9.                      CI0113
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0113
            11            GC03-DCAC92 PICTURE  9(8).                    CI0113
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-GD49.                                    CI0113
            12            GC03-FILLER PICTURE  X(198).                  CI0113
            11            GC03-GD41                                     CI0113
                          REDEFINES            GC03-GD49.               CI0113
            12            GC03-CRREF  PICTURE  9(2).                    CI0113
            12            GC03-CORIR  PICTURE  X(02).                   CI0113
            12            GC03-CIPDB  PICTURE  X(03).                   CI0113
            12            GC03-CPAYH  PICTURE  X(02).                   CI0113
            12            GC03-NAMEX  PICTURE  9(15)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC03-DCHAE  PICTURE  9(4).                    CI0113
            12            GC03-DRQST  PICTURE  S9(8)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-GD42                                     CI0113
                          REDEFINES            GC03-GD49.               CI0113
            12            GC03-CPMTCB PICTURE  X(3).                    CI0113
            10            GC03-GD50                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CSUSL1 PICTURE  S9.                      CI0113
            11            GC03-CRREV1 PICTURE  X(3).                    CI0113
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-DL13.                                    CI0113
            12            GC03-GEYR   PICTURE  9(4).                    CI0113
            12            GC03-GEMTH  PICTURE  99.                      CI0113
            12            GC03-NDAY   PICTURE  99.                      CI0113
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-XZ6A   PICTURE  X(6).                    CI0113
            11            GC03-XZ7    PICTURE  X(7).                    CI0113
            11            GC03-XZ6B   PICTURE  X(6).                    CI0113
            11            GC03-XZ6    PICTURE  X(6).                    CI0113
            11            GC03-XZ6C   PICTURE  X(6).                    CI0113
            11            GC03-XZ20   PICTURE  X(20).                   CI0113
            11            GC03-CATRN1 PICTURE  9(6).                    CI0113
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-XZ5    PICTURE  X(5).                    CI0113
            11            GC03-IREVD  PICTURE  X(1).                    CI0113
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0113
            11            GC03-XZ6D   PICTURE  X(6).                    CI0113
            11            GC03-XZ13   PICTURE  X(13).                   CI0113
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0113
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0113
            11            GC03-DTREN  PICTURE  9(8).                    CI0113
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC03-GD51                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CTXMT  PICTURE  9(2).                    CI0113
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-FILLER PICTURE  X(31).                   CI0113
            10            GC03-GD52                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0113
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CSUSL2 PICTURE  S9.                      CI0113
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-DL22.                                    CI0113
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0113
            12            GC03-GEMTHA PICTURE  99.                      CI0113
            12            GC03-NDAY01 PICTURE  99.                      CI0113
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CWHTP  PICTURE  X(3).                    CI0113
            11            GC03-CWHFR  PICTURE  X(3).                    CI0113
            11            GC03-CATRN7 PICTURE  9(6).                    CI0113
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0113
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0113
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-FILLER PICTURE  X(04).                   CI0113
            11            GC03-CATRN8 PICTURE  9(6).                    CI0113
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CSUSL4 PICTURE  S9.                      CI0113
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC03-GD60                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-GEOPDD PICTURE  X(8)                     CI0113
                          OCCURS       005     TIMES.                   CI0113
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0113
                          OCCURS       005     TIMES.                   CI0113
            11            GC03-GEOPDB PICTURE  X(8).                    CI0113
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0113
            11            GC03-ITELR2 PICTURE  X.                       CI0113
            11            GC03-IPMTA  PICTURE  X.                       CI0113
            11            GC03-CCSMG  PICTURE  X.                       CI0113
            11            GC03-CPLEC  PICTURE  XX.                      CI0113
            11            GC03-CORTYA PICTURE  X(3).                    CI0113
            11            GC03-CACTBC PICTURE  X(1).                    CI0113
            11            GC03-CGSPIA PICTURE  X.                       CI0113
            11            GC03-IPTRDA PICTURE  X(01).                   CI0113
            11            GC03-GCUSPY PICTURE  X(12).                   CI0113
            11            GC03-CPALLA PICTURE  X(1).                    CI0113
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-IFRSAB PICTURE  X.                       CI0113
            11            GC03-DELOI  PICTURE  9(8).                    CI0113
            11            GC03-IAROAA PICTURE  X.                       CI0113
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-ILTINA PICTURE  X.                       CI0113
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC03-CFUNTA PICTURE  X(2).                    CI0113
            11            GC03-CLGND  PICTURE  X.                       CI0113
            11            GC03-CPH3U  PICTURE  X.                       CI0113
            11            GC03-GESTD  PICTURE  9(8).                    CI0113
            11            GC03-GEEND  PICTURE  9(8).                    CI0113
            11            GC03-CPMTF  PICTURE  99.                      CI0113
            11            GC03-CNAVR  PICTURE  X(1).                    CI0113
            10            GC03-GD70                                     CI0113
                          REDEFINES            GC03-GD99.               CI0113
            11            GC03-CMEMO  PICTURE  X(2).                    CI0113
            11            GC03-ALPLDT PICTURE  9(8).                    CI0113
            11            GC03-CTLPD  PICTURE  9(8).                    CI0113
            11            GC03-CPAYCM PICTURE  X(2).                    CI0113
       01                 GC04.                                         CI0113
            10            GC04-CLCUS  PICTURE  99.                      CI0113
            10            GC04-CCACT  PICTURE  99.                      CI0113
            10            GC04-AFEET  PICTURE  S9(5)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC04-ITERF  PICTURE  X.                       CI0113
            10            GC04-ATERF  PICTURE  S9(5)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC04-CLDOB  PICTURE  9(8).                    CI0113
            10            GC04-CPLTYP PICTURE  X(14).                   CI0113
            10            GC04-IACFPD PICTURE  X(1).                    CI0113
            10            GC04-FILLER PICTURE  X(14).                   CI0113
       01                 GC06.                                         CI0113
            10            GC06-GELL   PICTURE  9(4)                     CI0113
                          BINARY.                                       CI0113
            10            GC06-GE00.                                    CI0113
            11            GC06-GC06K.                                   CI0113
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC06-CPITC  PICTURE  99.                      CI0113
            11            GC06-ITRNB  PICTURE  X.                       CI0113
            11            GC06-FILLER PICTURE  X(14).                   CI0113
            10            GC06-GE98.                                    CI0113
            11            GC06-FILLER PICTURE  X(240).                  CI0113
            10            GC06-GE10                                     CI0113
                          REDEFINES            GC06-GE98.               CI0113
            11            GC06-CDELI  PICTURE  9(3).                    CI0113
            11            GC06-CPAYC  PICTURE  X(2).                    CI0113
            11            GC06-ICHKP  PICTURE  X.                       CI0113
            11            GC06-CLTIN  PICTURE  9(12).                   CI0113
            11            GC06-IFHAI  PICTURE  X.                       CI0113
            11            GC06-CDQUA  PICTURE  X(2).                    CI0113
            11            GC06-FILLER PICTURE  X(07).                   CI0113
            11            GC06-GE99.                                    CI0113
            12            GC06-FILLER PICTURE  X(212).                  CI0113
            11            GC06-GE01                                     CI0113
                          REDEFINES            GC06-GE99.               CI0113
            12            GC06-NTR    PICTURE  9(8).                    CI0113
            12            GC06-GECKD  PICTURE  9.                       CI0113
            12            GC06-NPBN   PICTURE  X(20).                   CI0113
            12            GC06-CCBAT  PICTURE  99.                      CI0113
            12            GC06-CLID4  PICTURE  X(23).                   CI0113
            12            GC06-GENAL1 PICTURE  X(30)                    CI0113
                          OCCURS       002     TIMES.                   CI0113
            12            GC06-GESAD1 PICTURE  X(30)                    CI0113
                          OCCURS       003     TIMES.                   CI0113
            11            GC06-GE02                                     CI0113
                          REDEFINES            GC06-GE99.               CI0113
            12            GC06-GENAL  PICTURE  X(30)                    CI0113
                          OCCURS       002     TIMES.                   CI0113
            12            GC06-GESAD  PICTURE  X(30)                    CI0113
                          OCCURS       003     TIMES.                   CI0113
            11            GC06-GE03                                     CI0113
                          REDEFINES            GC06-GE99.               CI0113
            12            GC06-NCHKN  PICTURE  9(11).                   CI0113
            11            GC06-GE04                                     CI0113
                          REDEFINES            GC06-GE99.               CI0113
            12            GC06-CTIDAP PICTURE  9(3).                    CI0113
            12            GC06-PRCOD  PICTURE  9(5).                    CI0113
            12            GC06-TDELI  PICTURE  X(30).                   CI0113
            12            GC06-CINCD  PICTURE  9(02).                   CI0113
            10            GC06-GE20                                     CI0113
                          REDEFINES            GC06-GE98.               CI0113
            11            GC06-C299.                                    CI0113
            12            GC06-CTID.                                    CI0113
            13            GC06-CTIDA  PICTURE  9(3).                    CI0113
            13            GC06-CTIDN.                                   CI0113
            14            GC06-CTIDNP PICTURE  X(13).                   CI0113
            14            GC06-CTIDND PICTURE  9(11).                   CI0113
            11            GC06-DCACG9 PICTURE  9(8).                    CI0113
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            GC06-CIRAP  PICTURE  XX.                      CI0113
            11            GC06-CTYPE  PICTURE  X.                       CI0113
            11            GC06-INACT  PICTURE  X.                       CI0113
            11            GC06-FILLER PICTURE  X(01).                   CI0113
            11            GC06-ITPAC  PICTURE  X.                       CI0113
            11            GC06-ITAXI  PICTURE  X.                       CI0113
            11            GC06-IOWNC  PICTURE  X.                       CI0113
            11            GC06-CDVCD  PICTURE  X(2).                    CI0113
            11            GC06-CTCUS  PICTURE  999.                     CI0113
            11            GC06-CPMTCB PICTURE  X(3).                    CI0113
            11            GC06-CASTC1 PICTURE  99.                      CI0113
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0113
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0113
            11            GC06-CPRTB  PICTURE  X.                       CI0113
            11            GC06-CBRKD  PICTURE  9(4).                    CI0113
            11            GC06-FILLER PICTURE  X(12).                   CI0113
            10            GC06-GE30                                     CI0113
                          REDEFINES            GC06-GE98.               CI0113
            11            GC06-CFIDC  PICTURE  X(5).                    CI0113
            11            GC06-CPHSE  PICTURE  9(2).                    CI0113
            11            GC06-FILLER PICTURE  X(05).                   CI0113
            11            GC06-IABIN  PICTURE  X.                       CI0113
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC06-GE40                                     CI0113
                          REDEFINES            GC06-GE98.               CI0113
            11            GC06-CACCT  PICTURE  X.                       CI0113
            11            GC06-CPAYR  PICTURE  X(2).                    CI0113
            11            GC06-CDELI1 PICTURE  9(3).                    CI0113
            11            GC06-CATRN.                                   CI0113
            12            GC06-CATRF  PICTURE  9(3).                    CI0113
            12            GC06-CATRS  PICTURE  9(3).                    CI0113
            11            GC06-DEFFT  PICTURE  9(8).                    CI0113
            11            GC06-CTYPC  PICTURE  X.                       CI0113
            11            GC06-CIRAPA PICTURE  XX.                      CI0113
            11            GC06-FILLER PICTURE  X(09).                   CI0113
            11            GC06-GE49.                                    CI0113
            12            GC06-FILLER PICTURE  X(208).                  CI0113
            11            GC06-GE41                                     CI0113
                          REDEFINES            GC06-GE49.               CI0113
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0113
            11            GC06-GE42                                     CI0113
                          REDEFINES            GC06-GE49.               CI0113
            12            GC06-CTID1.                                   CI0113
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0113
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0113
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0113
            11            GC06-GE43                                     CI0113
                          REDEFINES            GC06-GE49.               CI0113
            12            GC06-GENAL2 PICTURE  X(30)                    CI0113
                          OCCURS       002     TIMES.                   CI0113
            12            GC06-GESAD2 PICTURE  X(30)                    CI0113
                          OCCURS       003     TIMES.                   CI0113
            11            GC06-GE44                                     CI0113
                          REDEFINES            GC06-GE49.               CI0113
            12            GC06-CTID01.                                  CI0113
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0113
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0113
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0113
            12            GC06-GECKD2 PICTURE  9.                       CI0113
            12            GC06-PACCT  PICTURE  S999V99                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC06-PLOAN  PICTURE  S999V99                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC06-PADPT  PICTURE  S999V99                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            GC06-IPCTL  PICTURE  X.                       CI0113
            12            GC06-IPCTP  PICTURE  X.                       CI0113
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            GC06-GE31                                     CI0113
                          REDEFINES            GC06-GE98.               CI0113
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0113
       01                 EP01.                                         CI0113
            10            EP01-GC01K.                                   CI0113
            11            EP01-C299.                                    CI0113
            12            EP01-CTID.                                    CI0113
            13            EP01-CTIDA  PICTURE  9(3).                    CI0113
            13            EP01-CTIDN.                                   CI0113
            14            EP01-CTIDNP PICTURE  X(13).                   CI0113
            14            EP01-CTIDND PICTURE  9(11).                   CI0113
            10            EP01-DCAG9L PICTURE  9(8).                    CI0113
            10            EP01-NAASQL PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP01-ICUST  PICTURE  X.                       CI0113
            10            EP01-NSEQ4B PICTURE  9(8)                     CI0113
                          BINARY.                                       CI0113
            10            EP01-PRCOD  PICTURE  9(5).                    CI0113
            10            EP01-PRSCD  PICTURE  X(9).                    CI0113
            10            EP01-FILLER PICTURE  X(8).                    CI0113
       01                 EP03.                                         CI0113
            10            EP03-GELL   PICTURE  9(4)                     CI0113
                          BINARY.                                       CI0113
            10            EP03-GD00.                                    CI0113
            11            EP03-GC03K.                                   CI0113
            12            EP03-DCACG9 PICTURE  9(8).                    CI0113
            12            EP03-NAASQ  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CAATY  PICTURE  9(3).                    CI0113
            11            EP03-CVSYS  PICTURE  X(2).                    CI0113
            11            EP03-CACTO  PICTURE  9(3).                    CI0113
            11            EP03-CATRN.                                   CI0113
            12            EP03-CATRF  PICTURE  9(3).                    CI0113
            12            EP03-CATRS  PICTURE  9(3).                    CI0113
            11            EP03-CASTC  PICTURE  99.                      CI0113
            11            EP03-IPULL  PICTURE  X.                       CI0113
            11            EP03-GEAUN  PICTURE  9(5).                    CI0113
            11            EP03-GEOPD2 PICTURE  X(8).                    CI0113
            11            EP03-NBTCH  PICTURE  9(4).                    CI0113
            11            EP03-DEFFT  PICTURE  9(8).                    CI0113
            11            EP03-NSUNT  PICTURE  9(4).                    CI0113
            11            EP03-ITRAN  PICTURE  X.                       CI0113
            11            EP03-DLAUP1 PICTURE  9(8).                    CI0113
            11            EP03-ADRET  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-TTRMS  PICTURE  X(12).                   CI0113
            11            EP03-IDELT  PICTURE  X.                       CI0113
            11            EP03-GEOPDM PICTURE  X(8).                    CI0113
            11            EP03-FILLER PICTURE  X(07).                   CI0113
            10            EP03-GD09.                                    CI0113
            11            EP03-FILLER PICTURE  X(70).                   CI0113
            10            EP03-GD01                                     CI0113
                          REDEFINES            EP03-GD09.               CI0113
            11            EP03-ADBRQ  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CTRTP  PICTURE  X(2).                    CI0113
            11            EP03-CPORT  PICTURE  X.                       CI0113
            11            EP03-CSCRNU PICTURE  X(4).                    CI0113
            11            EP03-DLAUP  PICTURE  9(8).                    CI0113
            11            EP03-CTWHAT PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-PWHLD  PICTURE  S999V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-IWTHH  PICTURE  X.                       CI0113
            11            EP03-NDRFT  PICTURE  9(5).                    CI0113
            11            EP03-IDPAP  PICTURE  X.                       CI0113
            11            EP03-GETIM  PICTURE  S9(7)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QNACT  PICTURE  9(3).                    CI0113
            11            EP03-AEDRQ  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-IPLIN  PICTURE  X.                       CI0113
            11            EP03-CLIDNB PICTURE  9(8).                    CI0113
            11            EP03-CSLCT  PICTURE  X.                       CI0113
            11            EP03-ITELE  PICTURE  X.                       CI0113
            11            EP03-FILLER PICTURE  X(06).                   CI0113
            10            EP03-GD02                                     CI0113
                          REDEFINES            EP03-GD09.               CI0113
            11            EP03-CSYST  PICTURE  99.                      CI0113
            11            EP03-FILLER PICTURE  X.                       CI0113
            11            EP03-ACASH  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-DTRAC  PICTURE  9(8).                    CI0113
            11            EP03-CTRSO  PICTURE  9(02).                   CI0113
            11            EP03-NTRCE  PICTURE  9(06).                   CI0113
            11            EP03-GECKD1 PICTURE  9.                       CI0113
            11            EP03-CCOLL  PICTURE  X(3).                    CI0113
            11            EP03-CLTDP  PICTURE  X(3).                    CI0113
            11            EP03-PSLLD  PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ISLOR  PICTURE  X.                       CI0113
            11            EP03-ITPAC  PICTURE  X.                       CI0113
            11            EP03-CPMTCA PICTURE  XXX.                     CI0113
            11            EP03-CSERV  PICTURE  X(3).                    CI0113
            11            EP03-ACOMO  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-IPLIN1 PICTURE  X.                       CI0113
            11            EP03-INQEX  PICTURE  X.                       CI0113
            11            EP03-CTKRAA PICTURE  X(12).                   CI0113
            11            EP03-CCSMQ  PICTURE  X.                       CI0113
            11            EP03-IVAEX1 PICTURE  X.                       CI0113
            11            EP03-IHPMT  PICTURE  X(1).                    CI0113
            11            EP03-GETIM3 PICTURE  S9(7)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP03-GD03                                     CI0113
                          REDEFINES            EP03-GD09.               CI0113
            11            EP03-CATRNC PICTURE  9(6).                    CI0113
            11            EP03-APRNT1 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QSHOWT PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACINVT PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACOMO7 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QSHOMW PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ATAXT3 PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CTSTR  PICTURE  9(2).                    CI0113
            11            EP03-ICIRA  PICTURE  X.                       CI0113
            11            EP03-GETIM2 PICTURE  S9(7)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CPMTCX PICTURE  XX.                      CI0113
            11            EP03-FILLER PICTURE  X(16).                   CI0113
            10            EP03-GD99.                                    CI0113
            11            EP03-FILLER PICTURE  X(248).                  CI0113
            10            EP03-GD10                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-MROTC  PICTURE  X(7).                    CI0113
            11            EP03-CEDSC  PICTURE  9(1).                    CI0113
            11            EP03-ILPOI  PICTURE  X(1).                    CI0113
            11            EP03-AWRCH  PICTURE  S9(3)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CHCOC1 PICTURE  9(2).                    CI0113
            11            EP03-CHCOC2 PICTURE  9(2).                    CI0113
            11            EP03-CHCOC3 PICTURE  9(2).                    CI0113
            11            EP03-CHCOC4 PICTURE  9(2).                    CI0113
            11            EP03-CMCOC1 PICTURE  9(3).                    CI0113
            11            EP03-CMCOC2 PICTURE  9(3).                    CI0113
            11            EP03-CMCOC3 PICTURE  9(3).                    CI0113
            11            EP03-GD11.                                    CI0113
            12            EP03-FILLER PICTURE  X(219).                  CI0113
            11            EP03-GD12                                     CI0113
                          REDEFINES            EP03-GD11.               CI0113
            12            EP03-CELLO  PICTURE  9(1).                    CI0113
            12            EP03-CECLO  PICTURE  9(1).                    CI0113
            12            EP03-AEXML  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CEPI   PICTURE  X(1).                    CI0113
            12            EP03-CEXTY  PICTURE  X.                       CI0113
            12            EP03-CROPC  PICTURE  9(1).                    CI0113
            12            EP03-CPUTY  PICTURE  9(1).                    CI0113
            12            EP03-IMCII  PICTURE  X(1).                    CI0113
            12            EP03-GEMISC                                   CI0113
                          OCCURS       010     TIMES.                   CI0113
            13            EP03-AMGLA  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            13            EP03-CMGLC  PICTURE  9(1).                    CI0113
            13            EP03-NMGLN  PICTURE  9(4).                    CI0113
            12            EP03-ACTRN  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-IWRBK  PICTURE  X.                       CI0113
            12            EP03-IFEDX  PICTURE  X.                       CI0113
            12            EP03-ICNTR  PICTURE  X.                       CI0113
            12            EP03-IOCKH  PICTURE  X.                       CI0113
            12            EP03-ICRCK  PICTURE  X.                       CI0113
            12            EP03-NHMPN  PICTURE  S9(10)                   CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ITELR1 PICTURE  X.                       CI0113
            11            EP03-GD13                                     CI0113
                          REDEFINES            EP03-GD11.               CI0113
            12            EP03-DREDO  PICTURE  9(8).                    CI0113
            12            EP03-CATRNR PICTURE  9(6).                    CI0113
            12            EP03-CEVN   PICTURE  9(9).                    CI0113
            12            EP03-ISUSP  PICTURE  X(1).                    CI0113
            11            EP03-GD15                                     CI0113
                          REDEFINES            EP03-GD11.               CI0113
            12            EP03-CPUTZ  PICTURE  9(1).                    CI0113
            12            EP03-CETLB  PICTURE  9(3).                    CI0113
            12            EP03-QTRMC  PICTURE  9(3).                    CI0113
            12            EP03-DEFFTE PICTURE  9(8).                    CI0113
            12            EP03-DEFFTF PICTURE  9(8).                    CI0113
            12            EP03-DEFFTG PICTURE  9(8).                    CI0113
            12            EP03-XZ1A   PICTURE  X.                       CI0113
            12            EP03-XZ1B   PICTURE  X.                       CI0113
            12            EP03-XZ1C   PICTURE  X.                       CI0113
            12            EP03-XZ1D   PICTURE  X.                       CI0113
            12            EP03-XZ1E   PICTURE  X.                       CI0113
            12            EP03-XZ1F   PICTURE  X.                       CI0113
            12            EP03-XZ1G   PICTURE  X.                       CI0113
            12            EP03-XZ1H   PICTURE  X.                       CI0113
            12            EP03-XZ1I   PICTURE  X.                       CI0113
            12            EP03-DEFFTH PICTURE  9(8).                    CI0113
            11            EP03-GD19                                     CI0113
                          REDEFINES            EP03-GD11.               CI0113
            12            EP03-GD11.                                    CI0113
            13            EP03-FILLER PICTURE  X(219).                  CI0113
            10            EP03-GD20                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-ADDACT PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ISIGV  PICTURE  X.                       CI0113
            11            EP03-IALLF  PICTURE  X.                       CI0113
            11            EP03-QSHOWQ PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CCDSCW PICTURE  9(2).                    CI0113
            11            EP03-IDWRL  PICTURE  X.                       CI0113
            11            EP03-ITELR  PICTURE  X.                       CI0113
            11            EP03-IABIN  PICTURE  X.                       CI0113
            11            EP03-PACT1  PICTURE  S999V999                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-IBFAF  PICTURE  X.                       CI0113
            11            EP03-IFRSA  PICTURE  X.                       CI0113
            11            EP03-ICRCAN PICTURE  X.                       CI0113
            11            EP03-ACACTV PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-AGFND  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QCSHOW PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QCSHIS PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-NDTRC  PICTURE  9(8).                    CI0113
            11            EP03-CAERU  PICTURE  X(4).                    CI0113
            11            EP03-IFDGO  PICTURE  X.                       CI0113
            11            EP03-PSLLD2 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ISLOR2 PICTURE  X.                       CI0113
            11            EP03-QSFIO  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QSFID  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CGDIN  PICTURE  X.                       CI0113
            11            EP03-DGDIN  PICTURE  9(8).                    CI0113
            10            EP03-GD30                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-ISKED  PICTURE  X.                       CI0113
            11            EP03-CENXC  PICTURE  9(2).                    CI0113
            11            EP03-GD31.                                    CI0113
            12            EP03-FILLER PICTURE  X(245).                  CI0113
            11            EP03-GD32                                     CI0113
                          REDEFINES            EP03-GD31.               CI0113
            12            EP03-IABIN1 PICTURE  X.                       CI0113
            12            EP03-CLDOD  PICTURE  9(8).                    CI0113
            12            EP03-NCLAM  PICTURE  9(5)                     CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ISURR  PICTURE  X.                       CI0113
            12            EP03-GEHCD  PICTURE  9(3).                    CI0113
            12            EP03-CRATC  PICTURE  9(4).                    CI0113
            12            EP03-AMAXD  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ASCHGA PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-APYOM  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-IWTHH1 PICTURE  X.                       CI0113
            12            EP03-CPAYCL PICTURE  X(2).                    CI0113
            12            EP03-CTSAO  PICTURE  X.                       CI0113
            12            EP03-NCONF  PICTURE  9(08).                   CI0113
            12            EP03-CLID   PICTURE  X(23).                   CI0113
            12            EP03-CARTY  PICTURE  99.                      CI0113
            12            EP03-NARRS  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CARTZ  PICTURE  99.                      CI0113
            12            EP03-NAPDS  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CPMTO  PICTURE  X.                       CI0113
            12            EP03-DNPMT  PICTURE  9(8).                    CI0113
            12            EP03-IPCTV  PICTURE  X.                       CI0113
            12            EP03-IMECH  PICTURE  X(01).                   CI0113
            12            EP03-IMVAO  PICTURE  X(1).                    CI0113
            12            EP03-AMVA1  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CACTS  PICTURE  X.                       CI0113
            12            EP03-CTSPP  PICTURE  X(1).                    CI0113
            12            EP03-CACT4  PICTURE  X(2).                    CI0113
            12            EP03-IVAEX  PICTURE  X.                       CI0113
            12            EP03-DFPMT  PICTURE  9(8).                    CI0113
            12            EP03-IDEMD  PICTURE  X.                       CI0113
            12            EP03-IOFST  PICTURE  X.                       CI0113
            12            EP03-AMXLB  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ACULB  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-DEIRNB PICTURE  9(8).                    CI0113
            12            EP03-DEFFE  PICTURE  9(8).                    CI0113
            12            EP03-DEFFR  PICTURE  9(8).                    CI0113
            12            EP03-ISPUP  PICTURE  X.                       CI0113
            12            EP03-CPNCG  PICTURE  X.                       CI0113
            12            EP03-IEXPU  PICTURE  X.                       CI0113
            12            EP03-IPPCF  PICTURE  X.                       CI0113
            12            EP03-NAAPT  PICTURE  9(2).                    CI0113
            12            EP03-PWHLDS PICTURE  S999V9(5)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ISWHO  PICTURE  X(1).                    CI0113
            11            EP03-GD33                                     CI0113
                          REDEFINES            EP03-GD31.               CI0113
            12            EP03-CPAYC  PICTURE  X(2).                    CI0113
            12            EP03-ADBRQX PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ADBRQV PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-APTXR  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CTRTPE PICTURE  X(2).                    CI0113
            12            EP03-NCLAMI PICTURE  S9(9)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CLIDO8 PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CLIDN  PICTURE  X(20).                   CI0113
            12            EP03-DSET01 PICTURE  S9(8)                    CI0113
                          BINARY.                                       CI0113
            12            EP03-CTSET1 PICTURE  S9(6)                    CI0113
                          BINARY.                                       CI0113
            12            EP03-DSET02 PICTURE  S9(8)                    CI0113
                          BINARY.                                       CI0113
            12            EP03-CTSET2 PICTURE  S9(6)                    CI0113
                          BINARY.                                       CI0113
            11            EP03-GD34                                     CI0113
                          REDEFINES            EP03-GD31.               CI0113
            12            EP03-QNOFM  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-CLTRM  PICTURE  99.                      CI0113
            12            EP03-AMXLN  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ALADJ  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ACHK   PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-APRMO  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-IMECH1 PICTURE  X(01).                   CI0113
            12            EP03-CACT41 PICTURE  X(2).                    CI0113
            12            EP03-ACDSCC PICTURE  S9(05)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-ACDSCD PICTURE  S9(05)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-GD39                                     CI0113
                          REDEFINES            EP03-GD31.               CI0113
            12            EP03-GD31.                                    CI0113
            13            EP03-FILLER PICTURE  X(245).                  CI0113
            10            EP03-GD40                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-NTR    PICTURE  9(8).                    CI0113
            11            EP03-NPBNC  PICTURE  X(24).                   CI0113
            11            EP03-CRREV  PICTURE  X(3).                    CI0113
            11            EP03-CSUSL  PICTURE  S9.                      CI0113
            11            EP03-NMGLN1 PICTURE  9(4).                    CI0113
            11            EP03-DCAC92 PICTURE  9(8).                    CI0113
            11            EP03-NAASQ3 PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-GD49.                                    CI0113
            12            EP03-FILLER PICTURE  X(198).                  CI0113
            11            EP03-GD41                                     CI0113
                          REDEFINES            EP03-GD49.               CI0113
            12            EP03-CRREF  PICTURE  9(2).                    CI0113
            12            EP03-CORIR  PICTURE  X(02).                   CI0113
            12            EP03-CIPDB  PICTURE  X(03).                   CI0113
            12            EP03-CPAYH  PICTURE  X(02).                   CI0113
            12            EP03-NAMEX  PICTURE  9(15)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP03-DCHAE  PICTURE  9(4).                    CI0113
            12            EP03-DRQST  PICTURE  S9(8)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-GD42                                     CI0113
                          REDEFINES            EP03-GD49.               CI0113
            12            EP03-CPMTCB PICTURE  X(3).                    CI0113
            10            EP03-GD50                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-ALOAD  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-PSLLD4 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CSUSL1 PICTURE  S9.                      CI0113
            11            EP03-CRREV1 PICTURE  X(3).                    CI0113
            11            EP03-ADDAC  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-DL13.                                    CI0113
            12            EP03-GEYR   PICTURE  9(4).                    CI0113
            12            EP03-GEMTH  PICTURE  99.                      CI0113
            12            EP03-NDAY   PICTURE  99.                      CI0113
            11            EP03-NSEQ3P PICTURE  S9(5)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-XZ6A   PICTURE  X(6).                    CI0113
            11            EP03-XZ7    PICTURE  X(7).                    CI0113
            11            EP03-XZ6B   PICTURE  X(6).                    CI0113
            11            EP03-XZ6    PICTURE  X(6).                    CI0113
            11            EP03-XZ6C   PICTURE  X(6).                    CI0113
            11            EP03-XZ20   PICTURE  X(20).                   CI0113
            11            EP03-CATRN1 PICTURE  9(6).                    CI0113
            11            EP03-ADDAC2 PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ATAXT2 PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACOMOT PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-XZ5    PICTURE  X(5).                    CI0113
            11            EP03-IREVD  PICTURE  X(1).                    CI0113
            11            EP03-ISUSP1 PICTURE  X(1).                    CI0113
            11            EP03-XZ6D   PICTURE  X(6).                    CI0113
            11            EP03-XZ13   PICTURE  X(13).                   CI0113
            11            EP03-CWHTP2 PICTURE  X(3).                    CI0113
            11            EP03-CWHTP3 PICTURE  X(3).                    CI0113
            11            EP03-DTREN  PICTURE  9(8).                    CI0113
            11            EP03-NAASQ1 PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP03-GD51                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-ADOMOT PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACGLT  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACGST  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CTXMT  PICTURE  9(2).                    CI0113
            11            EP03-ALOAD3 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-FILLER PICTURE  X(31).                   CI0113
            10            EP03-GD52                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-DEFFT5 PICTURE  9(8).                    CI0113
            11            EP03-PSLLD5 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CSUSL2 PICTURE  S9.                      CI0113
            11            EP03-ALOAD2 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-DL22.                                    CI0113
            12            EP03-NYEAR1 PICTURE  9(4).                    CI0113
            12            EP03-GEMTHA PICTURE  99.                      CI0113
            12            EP03-NDAY01 PICTURE  99.                      CI0113
            11            EP03-NSEQ3R PICTURE  S9(5)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CWHTP  PICTURE  X(3).                    CI0113
            11            EP03-CWHFR  PICTURE  X(3).                    CI0113
            11            EP03-CATRN7 PICTURE  9(6).                    CI0113
            11            EP03-ATAXT5 PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QSHOT  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACINT3 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CWHTP1 PICTURE  X(3).                    CI0113
            11            EP03-CWHFR1 PICTURE  X(3).                    CI0113
            11            EP03-ACOMO5 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QSHOMU PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACASH1 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-FILLER PICTURE  X(04).                   CI0113
            11            EP03-CATRN8 PICTURE  9(6).                    CI0113
            11            EP03-ALOAD1 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-PSLLD1 PICTURE  S99V999                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-QSHOT1 PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ACINT4 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CSUSL4 PICTURE  S9.                      CI0113
            11            EP03-ACOMO4 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP03-GD60                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-GEOPDD PICTURE  X(8)                     CI0113
                          OCCURS       005     TIMES.                   CI0113
            11            EP03-DLAUP3 PICTURE  9(8)                     CI0113
                          OCCURS       005     TIMES.                   CI0113
            11            EP03-GEOPDB PICTURE  X(8).                    CI0113
            11            EP03-DLAUP4 PICTURE  9(8).                    CI0113
            11            EP03-ITELR2 PICTURE  X.                       CI0113
            11            EP03-IPMTA  PICTURE  X.                       CI0113
            11            EP03-CCSMG  PICTURE  X.                       CI0113
            11            EP03-CPLEC  PICTURE  XX.                      CI0113
            11            EP03-CORTYA PICTURE  X(3).                    CI0113
            11            EP03-CACTBC PICTURE  X(1).                    CI0113
            11            EP03-CGSPIA PICTURE  X.                       CI0113
            11            EP03-IPTRDA PICTURE  X(01).                   CI0113
            11            EP03-GCUSPY PICTURE  X(12).                   CI0113
            11            EP03-CPALLA PICTURE  X(1).                    CI0113
            11            EP03-QSHO5A PICTURE  S9(9)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-IFRSAB PICTURE  X.                       CI0113
            11            EP03-DELOI  PICTURE  9(8).                    CI0113
            11            EP03-IAROAA PICTURE  X.                       CI0113
            11            EP03-ACINVR PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-ILTINA PICTURE  X.                       CI0113
            11            EP03-ALOIDA PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP03-CFUNTA PICTURE  X(2).                    CI0113
            11            EP03-CLGND  PICTURE  X.                       CI0113
            11            EP03-CPH3U  PICTURE  X.                       CI0113
            11            EP03-GESTD  PICTURE  9(8).                    CI0113
            11            EP03-GEEND  PICTURE  9(8).                    CI0113
            11            EP03-CPMTF  PICTURE  99.                      CI0113
            11            EP03-CNAVR  PICTURE  X(1).                    CI0113
            10            EP03-GD70                                     CI0113
                          REDEFINES            EP03-GD99.               CI0113
            11            EP03-CMEMO  PICTURE  X(2).                    CI0113
            11            EP03-ALPLDT PICTURE  9(8).                    CI0113
            11            EP03-CTLPD  PICTURE  9(8).                    CI0113
            11            EP03-CPAYCM PICTURE  X(2).                    CI0113
       01                 EP04.                                         CI0113
            10            EP04-CLCUS  PICTURE  99.                      CI0113
            10            EP04-CCACT  PICTURE  99.                      CI0113
            10            EP04-AFEET  PICTURE  S9(5)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP04-ITERF  PICTURE  X.                       CI0113
            10            EP04-ATERF  PICTURE  S9(5)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP04-CLDOB  PICTURE  9(8).                    CI0113
            10            EP04-CPLTYP PICTURE  X(14).                   CI0113
            10            EP04-IACFPD PICTURE  X(1).                    CI0113
            10            EP04-FILLER PICTURE  X(14).                   CI0113
       01                 EP06.                                         CI0113
            10            EP06-GELL   PICTURE  9(4)                     CI0113
                          BINARY.                                       CI0113
            10            EP06-GE00.                                    CI0113
            11            EP06-GC06K.                                   CI0113
            12            EP06-NPISQ  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP06-ACOTD  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP06-PPOTD  PICTURE  S9(3)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP06-QPSTD  PICTURE  S9(7)V999                CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP06-CPITC  PICTURE  99.                      CI0113
            11            EP06-ITRNB  PICTURE  X.                       CI0113
            11            EP06-FILLER PICTURE  X(14).                   CI0113
            10            EP06-GE98.                                    CI0113
            11            EP06-FILLER PICTURE  X(240).                  CI0113
            10            EP06-GE10                                     CI0113
                          REDEFINES            EP06-GE98.               CI0113
            11            EP06-CDELI  PICTURE  9(3).                    CI0113
            11            EP06-CPAYC  PICTURE  X(2).                    CI0113
            11            EP06-ICHKP  PICTURE  X.                       CI0113
            11            EP06-CLTIN  PICTURE  9(12).                   CI0113
            11            EP06-IFHAI  PICTURE  X.                       CI0113
            11            EP06-CDQUA  PICTURE  X(2).                    CI0113
            11            EP06-FILLER PICTURE  X(07).                   CI0113
            11            EP06-GE99.                                    CI0113
            12            EP06-FILLER PICTURE  X(212).                  CI0113
            11            EP06-GE01                                     CI0113
                          REDEFINES            EP06-GE99.               CI0113
            12            EP06-NTR    PICTURE  9(8).                    CI0113
            12            EP06-GECKD  PICTURE  9.                       CI0113
            12            EP06-NPBN   PICTURE  X(20).                   CI0113
            12            EP06-CCBAT  PICTURE  99.                      CI0113
            12            EP06-CLID4  PICTURE  X(23).                   CI0113
            12            EP06-GENAL1 PICTURE  X(30)                    CI0113
                          OCCURS       002     TIMES.                   CI0113
            12            EP06-GESAD1 PICTURE  X(30)                    CI0113
                          OCCURS       003     TIMES.                   CI0113
            11            EP06-GE02                                     CI0113
                          REDEFINES            EP06-GE99.               CI0113
            12            EP06-GENAL  PICTURE  X(30)                    CI0113
                          OCCURS       002     TIMES.                   CI0113
            12            EP06-GESAD  PICTURE  X(30)                    CI0113
                          OCCURS       003     TIMES.                   CI0113
            11            EP06-GE03                                     CI0113
                          REDEFINES            EP06-GE99.               CI0113
            12            EP06-NCHKN  PICTURE  9(11).                   CI0113
            11            EP06-GE04                                     CI0113
                          REDEFINES            EP06-GE99.               CI0113
            12            EP06-CTIDAP PICTURE  9(3).                    CI0113
            12            EP06-PRCOD  PICTURE  9(5).                    CI0113
            12            EP06-TDELI  PICTURE  X(30).                   CI0113
            12            EP06-CINCD  PICTURE  9(02).                   CI0113
            10            EP06-GE20                                     CI0113
                          REDEFINES            EP06-GE98.               CI0113
            11            EP06-C299.                                    CI0113
            12            EP06-CTID.                                    CI0113
            13            EP06-CTIDA  PICTURE  9(3).                    CI0113
            13            EP06-CTIDN.                                   CI0113
            14            EP06-CTIDNP PICTURE  X(13).                   CI0113
            14            EP06-CTIDND PICTURE  9(11).                   CI0113
            11            EP06-DCACG9 PICTURE  9(8).                    CI0113
            11            EP06-NAASQ  PICTURE  S9(3)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            EP06-CIRAP  PICTURE  XX.                      CI0113
            11            EP06-CTYPE  PICTURE  X.                       CI0113
            11            EP06-INACT  PICTURE  X.                       CI0113
            11            EP06-FILLER PICTURE  X(01).                   CI0113
            11            EP06-ITPAC  PICTURE  X.                       CI0113
            11            EP06-ITAXI  PICTURE  X.                       CI0113
            11            EP06-IOWNC  PICTURE  X.                       CI0113
            11            EP06-CDVCD  PICTURE  X(2).                    CI0113
            11            EP06-CTCUS  PICTURE  999.                     CI0113
            11            EP06-CPMTCB PICTURE  X(3).                    CI0113
            11            EP06-CASTC1 PICTURE  99.                      CI0113
            11            EP06-PRCOD1 PICTURE  9(5).                    CI0113
            11            EP06-CPRSC1 PICTURE  X(9).                    CI0113
            11            EP06-CPRTB  PICTURE  X.                       CI0113
            11            EP06-CBRKD  PICTURE  9(4).                    CI0113
            11            EP06-FILLER PICTURE  X(12).                   CI0113
            10            EP06-GE30                                     CI0113
                          REDEFINES            EP06-GE98.               CI0113
            11            EP06-CFIDC  PICTURE  X(5).                    CI0113
            11            EP06-CPHSE  PICTURE  9(2).                    CI0113
            11            EP06-FILLER PICTURE  X(05).                   CI0113
            11            EP06-IABIN  PICTURE  X.                       CI0113
            11            EP06-PDFND  PICTURE  S999V9(3)                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP06-GE40                                     CI0113
                          REDEFINES            EP06-GE98.               CI0113
            11            EP06-CACCT  PICTURE  X.                       CI0113
            11            EP06-CPAYR  PICTURE  X(2).                    CI0113
            11            EP06-CDELI1 PICTURE  9(3).                    CI0113
            11            EP06-CATRN.                                   CI0113
            12            EP06-CATRF  PICTURE  9(3).                    CI0113
            12            EP06-CATRS  PICTURE  9(3).                    CI0113
            11            EP06-DEFFT  PICTURE  9(8).                    CI0113
            11            EP06-CTYPC  PICTURE  X.                       CI0113
            11            EP06-CIRAPA PICTURE  XX.                      CI0113
            11            EP06-FILLER PICTURE  X(09).                   CI0113
            11            EP06-GE49.                                    CI0113
            12            EP06-FILLER PICTURE  X(208).                  CI0113
            11            EP06-GE41                                     CI0113
                          REDEFINES            EP06-GE49.               CI0113
            12            EP06-NCHKN1 PICTURE  9(6).                    CI0113
            11            EP06-GE42                                     CI0113
                          REDEFINES            EP06-GE49.               CI0113
            12            EP06-CTID1.                                   CI0113
            13            EP06-CTIDA1 PICTURE  9(3).                    CI0113
            13            EP06-CTIDP1 PICTURE  X(13).                   CI0113
            13            EP06-CTIDN1 PICTURE  9(11).                   CI0113
            11            EP06-GE43                                     CI0113
                          REDEFINES            EP06-GE49.               CI0113
            12            EP06-GENAL2 PICTURE  X(30)                    CI0113
                          OCCURS       002     TIMES.                   CI0113
            12            EP06-GESAD2 PICTURE  X(30)                    CI0113
                          OCCURS       003     TIMES.                   CI0113
            11            EP06-GE44                                     CI0113
                          REDEFINES            EP06-GE49.               CI0113
            12            EP06-CTID01.                                  CI0113
            13            EP06-CTIDA6 PICTURE  9(3).                    CI0113
            13            EP06-NTIDP2 PICTURE  X(13).                   CI0113
            13            EP06-CTIDN2 PICTURE  9(11).                   CI0113
            12            EP06-GECKD2 PICTURE  9.                       CI0113
            12            EP06-PACCT  PICTURE  S999V99                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP06-PLOAN  PICTURE  S999V99                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP06-PADPT  PICTURE  S999V99                  CI0113
                          COMPUTATIONAL-3.                              CI0113
            12            EP06-IPCTL  PICTURE  X.                       CI0113
            12            EP06-IPCTP  PICTURE  X.                       CI0113
            12            EP06-CEUNT  PICTURE  S9(5)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            EP06-GE31                                     CI0113
                          REDEFINES            EP06-GE98.               CI0113
            11            EP06-GCUSPZ PICTURE  X(12).                   CI0113
       01                 CT01.                                         CI0113
            10            CT01-CT01K.                                   CI0113
            11            CT01-C299.                                    CI0113
            12            CT01-CTID.                                    CI0113
            13            CT01-CTIDA  PICTURE  9(3).                    CI0113
            13            CT01-CTIDN.                                   CI0113
            14            CT01-CTIDNP PICTURE  X(13).                   CI0113
            14            CT01-CTIDND PICTURE  9(11).                   CI0113
            10            CT01-GECKD  PICTURE  9.                       CI0113
            10            CT01-GEMDA  PICTURE  9(8).                    CI0113
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0113
                          BINARY.                                       CI0113
            10            CT01-GECUC  PICTURE  99.                      CI0113
            10            CT01-CTAUL  PICTURE  9(3).                    CI0113
            10            CT01-DIRAC  PICTURE  9(4).                    CI0113
            10            CT01-CTCCI  PICTURE  X.                       CI0113
            10            CT01-CTCUS  PICTURE  999.                     CI0113
            10            CT01-CTEFD  PICTURE  9(8).                    CI0113
            10            CT01-CTIAD  PICTURE  9(8).                    CI0113
            10            CT01-CLCUS  PICTURE  99.                      CI0113
            10            CT01-CAMMB  PICTURE  X(3).                    CI0113
            10            CT01-CKPMM  PICTURE  X.                       CI0113
            10            CT01-CTLAD  PICTURE  9(8).                    CI0113
            10            CT01-IPERS  PICTURE  X.                       CI0113
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            CT01-CTLAT  PICTURE  9(8).                    CI0113
            10            CT01-CTLATC PICTURE  9(6).                    CI0113
            10            CT01-IMEGA  PICTURE  X.                       CI0113
            10            CT01-DIRAB  PICTURE  9(8).                    CI0113
            10            CT01-COLRQ  PICTURE  X.                       CI0113
            10            CT01-ZDA04  PICTURE  X(4).                    CI0113
            10            CT01-CTLPD  PICTURE  9(8).                    CI0113
            10            CT01-CIRASP PICTURE  9.                       CI0113
            10            CT01-CIRATP PICTURE  99.                      CI0113
            10            CT01-DRTHC  PICTURE  9(8).                    CI0113
            10            CT01-CPPTC  PICTURE  X.                       CI0113
            10            CT01-ZDA06  PICTURE  X(6).                    CI0113
            10            CT01-CTACD  PICTURE  9(8).                    CI0113
            10            CT01-CTNLI  PICTURE  X.                       CI0113
            10            CT01-CTRHO  PICTURE  9(8).                    CI0113
            10            CT01-CTSGD  PICTURE  9(8).                    CI0113
            10            CT01-CPATP  PICTURE  X(1).                    CI0113
            10            CT01-IRSTA  PICTURE  X.                       CI0113
            10            CT01-CTSTA  PICTURE  99.                      CI0113
            10            CT01-CTSSC  PICTURE  99.                      CI0113
            10            CT01-PRLIN  PICTURE  9(3).                    CI0113
            10            CT01-PRCOD  PICTURE  9(5).                    CI0113
            10            CT01-PRSCD  PICTURE  X(9).                    CI0113
            10            CT01-CTLNI  PICTURE  X.                       CI0113
            10            CT01-AYSIDA PICTURE  9(3).                    CI0113
            10            CT01-AYSID  PICTURE  9(5).                    CI0113
            10            CT01-CTBMC  PICTURE  99.                      CI0113
            10            CT01-CINAR  PICTURE  99.                      CI0113
            10            CT01-CPHTR  PICTURE  X.                       CI0113
            10            CT01-CDSTR  PICTURE  XX.                      CI0113
            10            CT01-CQACT  PICTURE  999.                     CI0113
            10            CT01-CIRAS  PICTURE  999.                     CI0113
            10            CT01-CIRAT  PICTURE  999.                     CI0113
            10            CT01-CLRAY  PICTURE  9(5).                    CI0113
            10            CT01-CATTP  PICTURE  X.                       CI0113
       01                 TO01.                                         CI0113
            10            TO01-CT01K.                                   CI0113
            11            TO01-C299.                                    CI0113
            12            TO01-CTID.                                    CI0113
            13            TO01-CTIDA  PICTURE  9(3).                    CI0113
            13            TO01-CTIDN.                                   CI0113
            14            TO01-CTIDNP PICTURE  X(13).                   CI0113
            14            TO01-CTIDND PICTURE  9(11).                   CI0113
            10            TO01-GECKD  PICTURE  9.                       CI0113
            10            TO01-GEMDA  PICTURE  9(8).                    CI0113
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0113
                          BINARY.                                       CI0113
            10            TO01-GECUC  PICTURE  99.                      CI0113
            10            TO01-CTAUL  PICTURE  9(3).                    CI0113
            10            TO01-DIRAC  PICTURE  9(4).                    CI0113
            10            TO01-CTCCI  PICTURE  X.                       CI0113
            10            TO01-CTCUS  PICTURE  999.                     CI0113
            10            TO01-CTEFD  PICTURE  9(8).                    CI0113
            10            TO01-CTIAD  PICTURE  9(8).                    CI0113
            10            TO01-CLCUS  PICTURE  99.                      CI0113
            10            TO01-CAMMB  PICTURE  X(3).                    CI0113
            10            TO01-CKPMM  PICTURE  X.                       CI0113
            10            TO01-CTLAD  PICTURE  9(8).                    CI0113
            10            TO01-IPERS  PICTURE  X.                       CI0113
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            TO01-CTLAT  PICTURE  9(8).                    CI0113
            10            TO01-CTLATC PICTURE  9(6).                    CI0113
            10            TO01-IMEGA  PICTURE  X.                       CI0113
            10            TO01-DIRAB  PICTURE  9(8).                    CI0113
            10            TO01-COLRQ  PICTURE  X.                       CI0113
            10            TO01-ZDA04  PICTURE  X(4).                    CI0113
            10            TO01-CTLPD  PICTURE  9(8).                    CI0113
            10            TO01-CIRASP PICTURE  9.                       CI0113
            10            TO01-CIRATP PICTURE  99.                      CI0113
            10            TO01-DRTHC  PICTURE  9(8).                    CI0113
            10            TO01-CPPTC  PICTURE  X.                       CI0113
            10            TO01-ZDA06  PICTURE  X(6).                    CI0113
            10            TO01-CTACD  PICTURE  9(8).                    CI0113
            10            TO01-CTNLI  PICTURE  X.                       CI0113
            10            TO01-CTRHO  PICTURE  9(8).                    CI0113
            10            TO01-CTSGD  PICTURE  9(8).                    CI0113
            10            TO01-CPATP  PICTURE  X(1).                    CI0113
            10            TO01-IRSTA  PICTURE  X.                       CI0113
            10            TO01-CTSTA  PICTURE  99.                      CI0113
            10            TO01-CTSSC  PICTURE  99.                      CI0113
            10            TO01-PRLIN  PICTURE  9(3).                    CI0113
            10            TO01-PRCOD  PICTURE  9(5).                    CI0113
            10            TO01-PRSCD  PICTURE  X(9).                    CI0113
            10            TO01-CTLNI  PICTURE  X.                       CI0113
            10            TO01-AYSIDA PICTURE  9(3).                    CI0113
            10            TO01-AYSID  PICTURE  9(5).                    CI0113
            10            TO01-CTBMC  PICTURE  99.                      CI0113
            10            TO01-CINAR  PICTURE  99.                      CI0113
            10            TO01-CPHTR  PICTURE  X.                       CI0113
            10            TO01-CDSTR  PICTURE  XX.                      CI0113
            10            TO01-CQACT  PICTURE  999.                     CI0113
            10            TO01-CIRAS  PICTURE  999.                     CI0113
            10            TO01-CIRAT  PICTURE  999.                     CI0113
            10            TO01-CLRAY  PICTURE  9(5).                    CI0113
            10            TO01-CATTP  PICTURE  X.                       CI0113
       01                 LJ52.                                         CI0113
            10            LJ52-AACTV  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-ADDAC  PICTURE  S9(7)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-AGRPV  PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-QSHOW  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-AFAVP  PICTURE  S9(4)V9(3)               CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-DASOF  PICTURE  9(8).                    CI0113
            10            LJ52-QDHGF  PICTURE  9(2).                    CI0113
            10            LJ52-QSHIS  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-CPORT  PICTURE  X.                       CI0113
            10            LJ52-AWDRTP PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-AWDRTC PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-APPAYA PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-APPAYN PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-IGOTYA PICTURE  X.                       CI0113
            10            LJ52-QTYUD1 PICTURE  9(5).                    CI0113
            10            LJ52-QTYUD2 PICTURE  9(5).                    CI0113
            10            LJ52-AGOFD  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-AGOFD1 PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-ANGOF  PICTURE  S9(9)V99                 CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-AATOTI PICTURE  S9(11)V99                CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-QSHOM  PICTURE  S9(10)V999               CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            LJ52-FILLER PICTURE  X(43).                   CI0113

      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0113
            10            DE10-DU11.                                    CI0113
            11            DE10-XFONC  PICTURE  X(4).                    CI0113
            11            DE10-MPSBN  PICTURE  X(8).                    CI0113
            11            DE10-XDBDNM PICTURE  X(08).                   CI0113
            11            DE10-XSEGNM PICTURE  X(08).                   CI0113
            11            DE10-XRC    PICTURE  X(02).                   CI0113
            11            DE10-MSEG   PICTURE  X(08).                   CI0113
            11            DE10-XCOKEY PICTURE  X(70).                   CI0113
            11            DE10-CUIBR  PICTURE  X(01).                   CI0113
            11            DE10-CUIBA  PICTURE  X(01).                   CI0113
            11            DE10-IPBIK  PICTURE  X(1).                    CI0113
            10            DE10-DU03.                                    CI0113
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            DE10-CMSSF  PICTURE  XX.                      CI0113
            11            DE10-DU09.                                    CI0113
            12            DE10-CMESA  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            12            DE10-CMESB  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            12            DE10-CMSST  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            12            DE10-QELLAA PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            12            DE10-TMESS4 PICTURE  X(512).                  CI0113

      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0113
          05              MS00-SUITE.                                   CI0113
            15       FILLER         PICTURE  X(00542).                  CI0113
       01                 MS03  REDEFINES      MS00.                    CI0113
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            10            MS03-CMSSF  PICTURE  XX.                      CI0113
            10            MS03-DU09.                                    CI0113
            11            MS03-CMESA  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            11            MS03-CMESB  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            11            MS03-CMSST  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            11            MS03-QELLAA PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
            11            MS03-TMESS4 PICTURE  X(512).                  CI0113
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0113
            10            MX11-QMSGS  PICTURE  9(03).                   CI0113
            10            MX11-PJ09                                     CI0113
                          OCCURS       025     TIMES.                   CI0113
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0113
                          COMPUTATIONAL-3.                              CI0113
            11            MX11-CMESB  PICTURE  S9(9)                    CI0113
                          BINARY.                                       CI0113
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ49
                                GC01
                                GC03
                                GC04
                                GC06
                                EP01
                                EP03
                                EP04
                                EP06
                                CT01
                                TO01
                                LJ52
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0113
      *               *                                   *             CI0113
      *               *INITIALISATIONS                    *             CI0113
      *               *                                   *             CI0113
      *               *************************************.            CI0113
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
      *N02CA.    NOTE *INITIALIZE LINKAGE SEGMENTS        *.
       F02CA.                                                           lv10
           INITIALIZE  GC01
           GC03
           GC04
           GC06
           EP01
           EP03
           EP04
           EP06.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *INITIALIZE RETURN FIELDS           *.
       F02DA.                                                           lv10
           MOVE        'N' TO PJ49-IGC01
           PJ49-IGC03
           PJ49-IGC04
           PJ49-IGC06
           MOVE        ZERO TO PJ49-ACASH.
       F02DA-FN. EXIT.
      *N02EA.    NOTE *INIT WORK FIELDS THAT AREN'T       *.
       F02EA.                                                           lv10
      *INIT'D ELSEWHERE
           MOVE        ZERO TO 7-TEMP-QCSHOW.
       F02EA-FN. EXIT.
      *N02XA.    NOTE *GET ADDRESSABILITY                 *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR SBUP                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-SBUP-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0113
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0113
      *               *                                   *             CI0113
      *               *FIN DE TRAITEMENT                  *             CI0113
      *               *                                   *             CI0113
      *               *************************************.            CI0113
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0113
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE THE INPUT PARMS           *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30BA.    NOTE *IF SOURCE IS FUNDS                 *.
       F30BA.    IF    CT01-CTIDA NUMERIC                               lv10
                 AND   CT01-CTIDA = 002
                 NEXT SENTENCE ELSE GO TO     F30BA-FN.
      *- CURRENTLY MUST BE FUNDS
       F30BA-900. GO TO F30BG-FN.
       F30BA-FN. EXIT.
      *N30BG.    NOTE *ELSE...  NO OTHER SOURCES YET      *.
       F30BG.                                                           lv10
      *---> Send BAD ADMIN Message                                      ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30BG-FN. EXIT.
      *N30CA.    NOTE *IF SOURCE IS KNOWN                 *.
       F30CA.    IF    PJ49-MAPPN = 'UD        '                        lv10
                 NEXT SENTENCE ELSE GO TO     F30CA-FN.
       F30CA-900. GO TO F30CG-FN.
       F30CA-FN. EXIT.
      *N30CG.    NOTE *ELSE.. DEFAULT TO "UD"             *.
       F30CG.                                                           lv10
           MOVE        'UD        ' TO PJ49-MAPPN.
       F30CG-FN. EXIT.
      *N30CK.    NOTE *IF "HOW" IS VALID                  *.
       F30CK.    IF    PJ49-CHCR NUMERIC                                lv10
                 AND   (PJ49-CHCR = 02
                 OR    PJ49-CHCR = 03)
                 NEXT SENTENCE ELSE GO TO     F30CK-FN.
       F30CK-900. GO TO F30CN-FN.
       F30CK-FN. EXIT.
      *N30CN.    NOTE *ELSE.. MUST BE TELE/WRITTEN        *.
       F30CN.                                                           lv10
      *---> Send BAD HOW CODE Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012053 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CN-FN. EXIT.
      *N30DA.    NOTE *IF DISBURSEMENT REQUEST IS KNOWN   *.
       F30DA.    IF    PJ49-CTRTP = 'S'                                 lv10
                 AND   (PJ49-CPORT = 'F'
                 OR    PJ49-CPORT = 'P')
                 NEXT SENTENCE ELSE GO TO     F30DA-FN.
       F30DA-900. GO TO F30DG-FN.
       F30DA-FN. EXIT.
      *N30DG.    NOTE *ELSE.. EXIT WITH ERROR             *.
       F30DG.                                                           lv10
      *---> Send BAD PORTN ACCT Message                                 ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013388 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DG-FN. EXIT.
      *N30EA.    NOTE *IF DESTINATION TYPE IS KNOWN       *.
       F30EA.    IF    PJ49-CPAYF = 'A '                                lv10
                 OR    PJ49-CPAYF = 'CT'
                 OR    PJ49-CPAYF = 'D '
                 OR    PJ49-CPAYF = 'O '
                 OR    PJ49-CPAYF = 'P '
                 OR    PJ49-CPAYF = 'S '
                 OR    PJ49-CPAYF = 'TR'
                 OR    PJ49-CPAYF = 'W '
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
       F30EA-900. GO TO F30EG-FN.
       F30EA-FN. EXIT.
      *N30EG.    NOTE *ELSE.. EXIT WITH ERROR             *.
       F30EG.                                                           lv10
      *---> Send UNKNOWN DEST Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013319 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30EG-FN. EXIT.
      *N30FA.    NOTE *ENSURE FINAL DESTINATION IS SET    *.
       F30FA.    IF    PJ49-ITRNB = 'N'                                 lv10
                 OR    PJ49-ITRNB = 'Y'
                 NEXT SENTENCE ELSE GO TO     F30FA-FN.
       F30FA-900. GO TO F30FG-FN.
       F30FA-FN. EXIT.
      *N30FG.    NOTE *ELSE.. FORCE TO LAST DESTINATION   *.
       F30FG.                                                           lv10
           MOVE        'Y' TO PJ49-ITRNB.
       F30FG-FN. EXIT.
      *N30TA.    NOTE *ENSURE NUMERIC FIELDS ARE VALID    *.
       F30TA.    IF    PJ49-GEAUN NUMERIC                               lv10
                 AND   PJ49-GETIM NUMERIC
                 AND   PJ49-DCACG NUMERIC
                 AND   PJ49-DEFFT NUMERIC
                 AND   PJ49-ADBRQ NUMERIC
                 AND   PJ49-AEDRQ NUMERIC
                 AND   PJ49-QSHOWQ NUMERIC
                 AND   PJ49-PACT1 NUMERIC
                 AND   PJ49-QNACT NUMERIC
                 AND   PJ49-ADDACT NUMERIC
                 AND   PJ49-CTWHAT NUMERIC
                 AND   PJ49-PWHLD NUMERIC
                 AND   PJ49-CLIDNB NUMERIC
                 AND   PJ49-CCDSCW NUMERIC
                 AND   PJ49-CLCUS NUMERIC
                 AND   PJ49-CCACT NUMERIC
                 AND   PJ49-AFEET NUMERIC
                 AND   PJ49-ATERF NUMERIC
                 AND   PJ49-CLDOB NUMERIC
                 AND   PJ49-ACOTD NUMERIC
                 AND   PJ49-PPOTD NUMERIC
                 AND   PJ49-QPSTD NUMERIC
                 AND   PJ49-NTR NUMERIC
                 AND   PJ49-CCBAT NUMERIC
                 AND   LJ52-AFAVP NUMERIC
                 AND   LJ52-QSHOW NUMERIC
                 AND   LJ52-AATOTI NUMERIC
                 AND   LJ52-AGOFD1 NUMERIC
                 AND   LJ52-QSHIS NUMERIC
                 NEXT SENTENCE ELSE GO TO     F30TA-FN.
      * - THESE FIELDS ARE NUMERIC
      *   SO THEY NEED TO BE VALIDATED
      *   TO ENSURE THEY ARE NUMERIC
      *   SO THAT NO ASRA OCCURS.  IT
      *   IS THE RESPONSIBILITY OF THE
      *   CALLING PROGRAM TO VALIDATE
      *   THESE FIELDS FOR PROPER
      *   VALUES
      *- CI0103 VALUES  ------>
       F30TA-900. GO TO F30TG-FN.
       F30TA-FN. EXIT.
      *N30TG.    NOTE *ELSE.. EXIT WITH ERROR             *.
       F30TG.                                                           lv10
      *---> Send NUM REQUIRED Message                                   ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012309 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30TG-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT THE GC01                    *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      **
      *********************************
      **                              *
      **   THIS ROUTINE IS USED TO    *
      **   FORMAT THE DISBURSEMENT    *
      **   ROOT SEGMENT               *
      **                              *
      *********************************
      **
      *N40DA.    NOTE *MOVE FIELDS FROM INPUT             *.
       F40DA.                                                           lv10
           MOVE        CT01-CTID TO GC01-CTID
           MOVE        PJ49-ICUST TO GC01-ICUST
           COMPUTE     GC01-DCAG9L = 99999999 -
           PJ49-DCACG
           MOVE        CT01-PRCOD TO GC01-PRCOD
           MOVE        CT01-PRSCD TO GC01-PRSCD
           MOVE        +1 TO GC01-NAASQL
           GC01-NSEQ4B.
       F40DA-FN. EXIT.
      *N40ZA.    NOTE *MARK SEGMENT AS FORMATTED          *.
       F40ZA.                                                           lv10
           MOVE        'Y' TO PJ49-IGC01.
       F40ZA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT THE GC03                    *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      **
      *********************************
      **                              *
      **   THIS ROUTINE IS USED TO    *
      **   FORMAT THE DISBURSEMENT    *
      **   ACTIVITY SEGMENT           *
      **                              *
      *********************************
      **
      *N45CA.    NOTE *MOVE CONTROL FIELDS FROM GC01      *.
       F45CA.                                                           lv10
           MOVE        GC01-DCAG9L TO GC03-DCACG9
           MOVE        GC01-NAASQL TO GC03-NAASQ.
       F45CA-FN. EXIT.
      *N45CG.    NOTE *MOVE CONSTANT FIELDS FOR DISB      *.
       F45CG.                                                           lv10
           MOVE        001 TO GC03-CAATY
           MOVE        020 TO GC03-CACTO
           MOVE        '000000' TO GC03-CATRN
           MOVE        01 TO GC03-CASTC
           MOVE        'Y' TO GC03-IPULL
           MOVE        0001 TO GC03-NBTCH
           MOVE        ZERO TO GC03-DLAUP1
           GC03-ADRET
           GC03-NDRFT
           MOVE        SPACES TO GC03-TTRMS
           MOVE        ' ' TO GC03-IDELT
           MOVE        'N' TO GC03-IDPAP
           GC03-IFRSA
           MOVE        SPACES TO GC03-GEOPDM.
       F45CG-FN. EXIT.
      *N45DA.    NOTE *MOVE FIELDS FROM INPUT             *.
       F45DA.                                                           lv10
           MOVE        PJ49-GEAUN TO GC03-GEAUN
           MOVE        PJ49-GEOPD2 TO GC03-GEOPD2
           MOVE        PJ49-DEFFT TO GC03-DEFFT
           MOVE        PJ49-CTRTP TO GC03-CTRTP
           MOVE        PJ49-CPORT TO GC03-CPORT
           MOVE        PJ49-CTWHAT TO GC03-CTWHAT
           MOVE        PJ49-PWHLD TO GC03-PWHLD
           MOVE        PJ49-GETIM TO GC03-GETIM
           MOVE        PJ49-AEDRQ TO GC03-AEDRQ
           MOVE        PJ49-IPLIN TO GC03-IPLIN
           MOVE        PJ49-CLIDNB TO GC03-CLIDNB.
      *N45DC.    NOTE *IF FULL SURRENDER; NOT AMOUNT      *.
       F45DC.    IF    GC03-CPORT = 'F'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45DC-FN.
           MOVE        ZERO TO GC03-ADBRQ.
       F45DC-900. GO TO F45DF-FN.
       F45DC-FN. EXIT.
      *N45DF.    NOTE *ELSE.. STORE AMOUNT                *.
       F45DF.                                                           lv15
           MOVE        PJ49-ADBRQ TO GC03-ADBRQ.
       F45DF-FN. EXIT.
       F45DA-FN. EXIT.
      *N45EA.    NOTE *READ TABLE TA5B TO GET CVSYS       *.
       F45EA.                                                           lv10
           MOVE        CT01-CTIDA TO TA5B-CTIDA
           MOVE        CT01-PRCOD TO TA5B-PRCOD
           MOVE        SPACES TO TA5B-PRSCD
           MOVE        ZERO TO TA5B-IK
           PERFORM     F92TA THRU F92TA-FN.
      *N45ED.    NOTE *TA5B TBL ENTRY FOUND: MOVE CVSYS   *.
       F45ED.    IF    TA5B-IK = '0'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F45ED-FN.
           MOVE        TA5B-CVSYS TO GC03-CVSYS.
       F45ED-900. GO TO F45EG-FN.
       F45ED-FN. EXIT.
      *N45EG.    NOTE *ELSE... SET TO ZERO                *.
       F45EG.                                                           lv15
           MOVE        ZERO TO GC03-CVSYS.
       F45EG-FN. EXIT.
       F45EA-FN. EXIT.
      *N45EM.    NOTE *IF TRANSFER IS INDICATED           *.
       F45EM.    IF    PJ49-CPAYF = 'TR'                                lv10
                 NEXT SENTENCE ELSE GO TO     F45EM-FN.
           MOVE        'Y' TO GC03-ITRAN
           MOVE        PJ49-QNACT TO GC03-QNACT.
       F45EM-900. GO TO F45ET-FN.
       F45EM-FN. EXIT.
      *N45ET.    NOTE *ELSE... SET TO ZERO                *.
       F45ET.                                                           lv10
           MOVE        'N' TO GC03-ITRAN
           MOVE        ZERO TO GC03-QNACT.
       F45ET-FN. EXIT.
      *N45FG.    NOTE *MOVE ZERO TO SUB-UNIT              *.
       F45FG.                                                           lv10
           MOVE        ZERO TO GC03-NSUNT.
       F45FG-FN. EXIT.
      *N45FM.    NOTE *IF WITHHOLDING EXIST; SET SW       *.
       F45FM.    IF    GC03-CTWHAT > ZERO                               lv10
                 OR    GC03-PWHLD > ZERO
                 NEXT SENTENCE ELSE GO TO     F45FM-FN.
           MOVE        'Y' TO GC03-IWTHH.
       F45FM-900. GO TO F45FT-FN.
       F45FM-FN. EXIT.
      *N45FT.    NOTE *ELSE.. NO WITHHOLDING              *.
       F45FT.                                                           lv10
                 IF    PJ49-ICUST = 'N'                                 DOT
      *INITIALISE IWTHH N FOR CUSTODIAL
           MOVE        SPACE TO GC03-IWTHH
                 ELSE
           MOVE        'N' TO GC03-IWTHH.
       F45FT-FN. EXIT.
      *N45JA.    NOTE *IF FUND IS THE SOURCE ACCOUNT      *.
       F45JA.    IF    GC01-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45JA-FN.
           MOVE        +241 TO GC03-GELL
           MOVE        'KD14' TO GC03-CSCRNU
           MOVE        PJ49-ADDACT TO GC03-ADDACT
           MOVE        PJ49-QSHOWQ TO GC03-QSHOWQ
           MOVE        PJ49-CCDSCW TO GC03-CCDSCW
           MOVE        PJ49-PACT1 TO GC03-PACT1
           MOVE        LJ52-AATOTI TO GC03-ACACTV
           MOVE        LJ52-AGOFD1 TO GC03-AGFND
           MOVE        LJ52-QSHIS TO GC03-QCSHIS.
      *N45JD.    NOTE *IF PRICE AVAILABLE; CALC SHARES    *.
       F45JD.    IF    LJ52-AFAVP > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F45JD-FN.
           COMPUTE     GC03-QCSHOW ROUNDED =
           (GC03-ACACTV -
           LJ52-ADDAC) /
           LJ52-AFAVP.
       F45JD-900. GO TO F45JG-FN.
       F45JD-FN. EXIT.
      *N45JG.    NOTE *ELSE.. USE CURRENT SHARES          *.
       F45JG.                                                           lv15
           MOVE        LJ52-QSHOW TO GC03-QCSHOW.
       F45JG-FN. EXIT.
      *N45KK.    NOTE *IF WARNING LETTER SHOULD BE SET    *.
       F45KK.    IF    PJ49-IPLIN = 'N'                                 lv15
                 AND   GC03-ITRAN = 'N'
                 AND   GC03-AEDRQ > 4999.99
                 NEXT SENTENCE ELSE GO TO     F45KK-FN.
           MOVE        'Y' TO GC03-IDWRL.
       F45KK-900. GO TO F45KM-FN.
       F45KK-FN. EXIT.
      *N45KM.    NOTE *ELSE...                            *.
       F45KM.                                                           lv15
           MOVE        'N' TO GC03-IDWRL.
       F45KM-FN. EXIT.
      *N45KO.    NOTE *IF SENT BY PHONE                   *.
       F45KO.    IF    PJ49-CHCR = 03                                   lv15
                 NEXT SENTENCE ELSE GO TO     F45KO-FN.
           MOVE        'Y' TO GC03-ITELR.
       F45KO-900. GO TO F45KQ-FN.
       F45KO-FN. EXIT.
      *N45KQ.    NOTE *ELSE...                            *.
       F45KQ.                                                           lv15
           MOVE        'N' TO GC03-ITELR.
       F45KQ-FN. EXIT.
      *N45LA.    NOTE *IF EXTERNAL; SET TRAN CODE         *.
       F45LA.    IF    GC03-ITRAN = 'N'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45LA-FN.
           MOVE        808 TO GC03-CATRF.
      *N45LE.    NOTE *IF FULL TRANSFER; SET SUB CODE     *.
       F45LE.    IF    GC03-CPORT = 'F'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45LE-FN.
           MOVE        002 TO GC03-CATRS.
       F45LE-900. GO TO F45LG-FN.
       F45LE-FN. EXIT.
      *N45LG.    NOTE *ELSE.. LESS THAN FULL              *.
       F45LG.                                                           lv20
           MOVE        001 TO GC03-CATRS.
       F45LG-FN. EXIT.
       F45LA-FN. EXIT.
      *N45MA.    NOTE *MOVE CONSTANTS TO ACTIVITY         *.
       F45MA.                                                           lv15
           MOVE        'N' TO GC03-ISIGV
           GC03-IALLF
           GC03-IABIN
           MOVE        'B' TO GC03-IBFAF
           MOVE        SPACE TO GC03-ICRCAN
           GC03-CAERU
           MOVE        ZERO TO GC03-NDTRC
           GC03-PSLLD2.
       F45MA-FN. EXIT.
      *N45NA.    NOTE *SET UP SSA FOR SB01 READ           *.
       F45NA.                                                           lv15
      *********************************
      ** SET UP SSA FOR SB01 READ TO  *
      ** GET THE LAST UPDATE DATE     *
      *********************************
      *
           MOVE        LOW-VALUES TO S-SBU01-SB01K
           MOVE        04 TO S-SBU01-CBUPT
           MOVE        CT01-CTIDA TO S-SBU01-CTIDA
           MOVE        ZEROS TO S-SBU01-DL13.
       F45NA-FN. EXIT.
      *N45ND.    NOTE *READ SB01 SEGMENT                  *.
       F45ND.                                                           lv15
      *
           PERFORM     F94SB THRU F94SB-FN.
       F45ND-FN. EXIT.
      *N45NG.    NOTE *CHECK IF SB01 SEGMENT NOT FOUND    *.
       F45NG.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F45NG-FN.
           MOVE        PJ49-DCACG TO GC03-DLAUP.
      *N45NM.    NOTE *IF SEVERE DATABASE ERROR           *.
       F45NM.    IF    DE10-NMESS2 NOT = ZEROS                          lv20
                 NEXT SENTENCE ELSE GO TO     F45NM-FN.
      *---> Send BAD SB01 READ Message                                  ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012019 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45NM-FN. EXIT.
       F45NG-900. GO TO F45NO-FN.
       F45NG-FN. EXIT.
      *N45NO.    NOTE *ELSE .. SET DATE OF LAST UPDATE    *.
       F45NO.         EXIT.                                             lv15
      *N45NT.    NOTE *IF UPDATED; USE CURRENT DATE       *.
       F45NT.    IF    SB01-ILAUP = 'Y'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45NT-FN.
      *!ADS "SB01-DCACG    GC03-DLAUP"
           MOVE        SB01-DCACG                                       CI0113
           TO DAT8E DAT6C                                               CI0113
           MOVE DAT81E TO DAT63C                                        CI0113
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0113
           MOVE   DAT6C TO  GC03-DLAUP.                                 CI0113
       F45NT-900. GO TO F45NV-FN.
       F45NT-FN. EXIT.
      *N45NV.    NOTE *ELSE... USE LAST DATE              *.
       F45NV.                                                           lv20
      *!ADS "SB01-DLACG    GC03-DLAUP"
           MOVE        SB01-DLACG                                       CI0113
           TO DAT8E DAT6C                                               CI0113
           MOVE DAT81E TO DAT63C                                        CI0113
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0113
           MOVE   DAT6C TO  GC03-DLAUP.                                 CI0113
       F45NV-FN. EXIT.
       F45NO-FN. EXIT.
       F45JA-FN. EXIT.
      *N45ZA.    NOTE *MARK SEGMENT AS FORMATTED          *.
       F45ZA.                                                           lv10
           MOVE        'Y' TO PJ49-IGC03.
       F45ZA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT THE GC04                    *
      *               *                                   *
      *               *************************************.
       F50.      IF    PJ49-CLCUS > ZERO                                lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      **
      *********************************
      **                              *
      **   THIS ROUTINE IS USED TO    *
      **   FORMAT THE ACTIVITY        *
      **   CUSTODIAL SEGMENT          *
      **                              *
      *********************************
      **
      *N50DA.    NOTE *MOVE CALCULATED VALUES             *.
       F50DA.                                                           lv10
           MOVE        PJ49-CLCUS TO GC04-CLCUS
           MOVE        PJ49-CCACT TO GC04-CCACT
           MOVE        PJ49-AFEET TO GC04-AFEET
           MOVE        PJ49-ITERF TO GC04-ITERF
           MOVE        PJ49-ATERF TO GC04-ATERF
           MOVE        PJ49-CLDOB TO GC04-CLDOB
           MOVE        PJ49-CPLTYP TO GC04-CPLTYP
           MOVE        PJ49-IACFPD TO GC04-IACFPD.
       F50DA-FN. EXIT.
      *N50ZA.    NOTE *MARK SEGMENT AS FORMATTED          *.
       F50ZA.                                                           lv10
           MOVE        'Y' TO PJ49-IGC04.
       F50ZA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT THE GC06                    *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55CA.    NOTE *SETUP COMMON PORTION               *.
       F55CA.                                                           lv10
           MOVE        001 TO GC06-NPISQ
           MOVE        PJ49-ITRNB TO GC06-ITRNB.
      *N55CD.    NOTE *IF ONE ACCOUNT DESTINATION         *.
       F55CD.    IF    GC03-QNACT < 2                                   lv15
                 NEXT SENTENCE ELSE GO TO     F55CD-FN.
           MOVE        'N' TO GC06-ITRNB.
       F55CD-FN. EXIT.
      *N55CG.    NOTE *IF NOT A TRANSFER                  *.
       F55CG.    IF    GC03-ITRAN = 'N'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55CG-FN.
           MOVE        PJ49-ACOTD TO GC06-ACOTD
           MOVE        PJ49-PPOTD TO GC06-PPOTD
           MOVE        PJ49-QPSTD TO GC06-QPSTD.
       F55CG-900. GO TO F55CQ-FN.
       F55CG-FN. EXIT.
      *N55CQ.    NOTE *ELSE... THIS IS A TRANSFER         *.
       F55CQ.                                                           lv15
           MOVE        PJ49-PPOTD TO GC06-PPOTD.
      *N55CT.    NOTE *IF REMAINING BALANCE TRANSFER      *.
       F55CT.    IF    (GC06-ITRNB = 'Y'                                lv20
                 OR    GC03-QNACT < 2)
                 NEXT SENTENCE ELSE GO TO     F55CT-FN.
           MOVE        +999 TO GC06-NPISQ
           MOVE        ZERO TO GC06-ACOTD
           GC06-QPSTD.
                 IF    GC06-ITRNB = 'Y'                                 DOT
           MOVE        ZERO TO GC06-PPOTD.
       F55CT-FN. EXIT.
       F55CQ-FN. EXIT.
       F55CA-FN. EXIT.
      *N55DA.    NOTE *IF TRANSFER                        *.
       F55DA.    IF    PJ49-CPAYF = 'TR'                                lv10
                 NEXT SENTENCE ELSE GO TO     F55DA-FN.
           MOVE        +123 TO GC06-GELL
           MOVE        02 TO GC06-CPITC
           MOVE        TO01-CTID TO GC06-CTID
           MOVE        GC01-DCAG9L TO GC06-DCACG9
           MOVE        +1 TO GC06-NAASQ
           MOVE        PJ49-CIRAP TO GC06-CIRAP
           MOVE        PJ49-CTYPE TO GC06-CTYPE
           MOVE        SPACE TO GC06-ITPAC
           GC06-ITAXI
           MOVE        PJ49-IOWNC TO GC06-IOWNC
           MOVE        TO01-CDSTR TO GC06-CDVCD
           MOVE        PJ49-CTCUS TO GC06-CTCUS
           MOVE        PJ49-CPMTCB TO GC06-CPMTCB
           MOVE        01 TO GC06-CASTC1
           MOVE        TO01-PRCOD TO GC06-PRCOD1
           MOVE        TO01-PRSCD TO GC06-CPRSC1.
      *N55DG.    NOTE *IF NEW ACCOUNT                     *.
       F55DG.    IF    CT01-CTSTA = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55DG-FN.
           MOVE        'Y' TO GC06-INACT.
       F55DG-900. GO TO F55DL-FN.
       F55DG-FN. EXIT.
      *N55DL.    NOTE *ELSE... NOT A NEW ACCOUNT          *.
       F55DL.                                                           lv15
           MOVE        SPACE TO GC06-INACT.
       F55DL-FN. EXIT.
       F55DA-900. GO TO F55GA-FN.
       F55DA-FN. EXIT.
      *N55GA.    NOTE *ELSE... EXTERNAL PAYOUT            *.
       F55GA.                                                           lv10
           MOVE        01 TO GC06-CPITC
           MOVE        PJ49-CPAYF TO GC06-CPAYC
           MOVE        'Y' TO GC06-ICHKP
           MOVE        ZERO TO GC06-CLTIN
           MOVE        SPACE TO GC06-IFHAI
           MOVE        SPACE TO GC06-CDQUA
           MOVE        SPACE TO GC06-ITRNB.
      *N55HA.    NOTE *IF WIRE OR DIRECT DEPOSIT          *.
       F55HA.    IF    PJ49-CPAYF = 'D'                                 lv15
                 OR    PJ49-CPAYF = 'W'
                 NEXT SENTENCE ELSE GO TO     F55HA-FN.
           MOVE        PJ49-NTR TO GC06-NTR
           7-FA00-NTR
           PERFORM     F92FA THRU F92FA-FN
           MOVE        7-FA00-GECKD TO GC06-GECKD
           MOVE        PJ49-NPBN TO GC06-NPBN
           MOVE        PJ49-CCBAT TO GC06-CCBAT
           MOVE        PJ49-CLID4 TO GC06-CLID4
           MOVE        PJ49-GENAL (1) TO GC06-GENAL1 (1)
           MOVE        PJ49-GENAL (2) TO GC06-GENAL1 (2)
           MOVE        PJ49-GESAD (1) TO GC06-GESAD1 (1)
           MOVE        PJ49-GESAD (2) TO GC06-GESAD1 (2)
           MOVE        PJ49-GESAD (3) TO GC06-GESAD1 (3).
      *N55HE.    NOTE *IF WIRE                            *.
       F55HE.    IF    PJ49-CPAYF = 'W '                                lv20
                 NEXT SENTENCE ELSE GO TO     F55HE-FN.
           MOVE        001 TO GC06-CDELI.
       F55HE-900. GO TO F55HJ-FN.
       F55HE-FN. EXIT.
      *N55HJ.    NOTE *ELSE... DIRECT DEPOSIT             *.
       F55HJ.                                                           lv20
           MOVE        006 TO GC06-CDELI.
       F55HJ-FN. EXIT.
       F55HA-900. GO TO F55IA-FN.
       F55HA-FN. EXIT.
      *N55IA.    NOTE *ELSE... CHECK AND SPECIAL PAYEE    *.
       F55IA.         EXIT.                                             lv15
      *N55JA.    NOTE *IF NOT OWNER                       *.
       F55JA.    IF    PJ49-CPAYF = 'P '                                lv20
                 OR    'CT'
                 OR    'S '
                 NEXT SENTENCE ELSE GO TO     F55JA-FN.
           MOVE        PJ49-GENAL (1) TO GC06-GENAL (1)
           MOVE        PJ49-GENAL (2) TO GC06-GENAL (2)
           MOVE        PJ49-GESAD (1) TO GC06-GESAD (1)
           MOVE        PJ49-GESAD (2) TO GC06-GESAD (2)
           MOVE        PJ49-GESAD (3) TO GC06-GESAD (3).
       F55JA-FN. EXIT.
      *N55JC.    NOTE *SET THE CDELI FIELD                *.
       F55JC.         EXIT.                                             lv20
      *N55JK.    NOTE *IF CHECK TO OWNER                  *.
       F55JK.    IF    PJ49-CPAYF =                                     lv25
                       'A '
                 OR    'O '
                 NEXT SENTENCE ELSE GO TO     F55JK-FN.
           MOVE        003 TO GC06-CDELI.
       F55JK-900. GO TO F55JC-FN.
       F55JK-FN. EXIT.
      *N55JM.    NOTE *IF CUSTODIAL XFER OR SPEC PAYEE    *.
       F55JM.    IF    PJ49-CPAYF =                                     lv25
                       'S '
                 OR    'CT'
                 OR    'P '
                 NEXT SENTENCE ELSE GO TO     F55JM-FN.
           MOVE        004 TO GC06-CDELI.
       F55JM-900. GO TO F55JC-FN.
       F55JM-FN. EXIT.
      *N55JW.    NOTE *ELSE.... DEFAULT TO SPEC PAYEE     *.
       F55JW.                                                           lv25
           MOVE        004 TO GC06-CDELI.
       F55JW-FN. EXIT.
       F55JC-FN. EXIT.
       F55IA-FN. EXIT.
      *N55KA.    NOTE *DETERMINE THE GELL                 *.
       F55KA.         EXIT.                                             lv15
      *N55KC.    NOTE *IF WIRE                            *.
       F55KC.    IF    GC06-CDELI =                                     lv20
                       001
                 NEXT SENTENCE ELSE GO TO     F55KC-FN.
           MOVE        +268 TO GC06-GELL.
       F55KC-900. GO TO F55KA-FN.
       F55KC-FN. EXIT.
      *N55KE.    NOTE *IF CHECK TO OWNER                  *.
       F55KE.    IF    GC06-CDELI =                                     lv20
                       003
                 NEXT SENTENCE ELSE GO TO     F55KE-FN.
           MOVE        +64 TO GC06-GELL.
       F55KE-900. GO TO F55KA-FN.
       F55KE-FN. EXIT.
      *N55KG.    NOTE *IF CHECK TO SPECIAL PAYEE          *.
       F55KG.    IF    GC06-CDELI =                                     lv20
                       004
                 NEXT SENTENCE ELSE GO TO     F55KG-FN.
           MOVE        +214 TO GC06-GELL.
       F55KG-900. GO TO F55KA-FN.
       F55KG-FN. EXIT.
      *N55KI.    NOTE *IF ACH                             *.
       F55KI.    IF    GC06-CDELI =                                     lv20
                       006
                 NEXT SENTENCE ELSE GO TO     F55KI-FN.
           MOVE        +276 TO GC06-GELL.
       F55KI-900. GO TO F55KA-FN.
       F55KI-FN. EXIT.
      *N55KT.    NOTE *UNKNOWN; DEFAULT MAX               *.
       F55KT.                                                           lv20
           MOVE        +276 TO GC06-GELL.
       F55KT-FN. EXIT.
       F55KA-FN. EXIT.
       F55GA-FN. EXIT.
      *N55ZA.    NOTE *MARK SEGMENT AS FORMATTED          *.
       F55ZA.                                                           lv10
           MOVE        'Y' TO PJ49-IGC06.
       F55ZA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *IF EXPRESS MAIL NEEDED             *
      *               *                                   *
      *               *************************************.
       F60.      IF    PJ49-IEXML = 'Y'                                 lv05
                 AND   PJ49-IGC01 = 'Y'
                 AND   PJ49-IGC03 = 'Y'
                 AND   PJ49-IGC06 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *N60DA.    NOTE *CREATE THE MAIL FEE - ROOT         *.
       F60DA.                                                           lv10
      *
           MOVE        GC01 TO EP01
      *
      *SEQUENCE FEE FIRST BY SETTING
      *ORIGINAL ACTIVITY UP BY +1
      *
           ADD         +1 TO GC01-NAASQL.
       F60DA-FN. EXIT.
      *N60GA.    NOTE *CREATE THE MAIL FEE - ACTIVITY     *.
       F60GA.                                                           lv10
      *
           MOVE        GC03 TO EP03
      *
      *SEQUENCE FEE FIRST BY SETTING
      *ORIGINAL ACTIVITY UP BY +1
      *
           MOVE        GC01-NAASQL TO GC03-NAASQ.
      *N60GG.    NOTE *FORMAT GC03 FOR EXP MAIL FEE       *.
       F60GG.                                                           lv15
           MOVE        808001 TO EP03-CATRN
           MOVE        'N' TO EP03-ITRAN
           MOVE        'P' TO EP03-CPORT
           MOVE        ZERO TO EP03-CTWHAT
           EP03-PWHLD.
                 IF    PJ49-ICUST = 'N'                                 DOT
      *INITIALISE IWTHH TO N FOR CUST
           MOVE        SPACE TO EP03-IWTHH
                 ELSE
           MOVE        'N' TO EP03-IWTHH.
           MOVE        ZERO TO EP03-QNACT                               DOT
           EP03-ADDACT
           EP03-QSHOWQ
           EP03-PACT1
           MOVE        PJ49-CCDSCW TO EP03-CCDSCW
           MOVE        'N' TO EP03-IDWRL
           MOVE        PJ49-AEXML TO EP03-ADBRQ
           EP03-AEDRQ.
       F60GG-FN. EXIT.
      *N60GM.    NOTE *CALC ACTIVITY REAL AMOUNT          *.
       F60GM.    IF    GC01-CTIDA = 002                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60GM-FN.
      *********************************
      ** THE ORIGINAL ACTIVITY WILL   *
      ** BE INSERTED SECOND SO WE     *
      ** TO REDUCE THAT ACTIVITY BY   *
      ** THIS ACTIVITIES AMOUNT       *
      *********************************
           COMPUTE     GC03-ACACTV = GC03-ACACTV -
           EP03-ADBRQ
           COMPUTE     GC03-AGFND = GC03-AGFND -
           EP03-ADBRQ.
       F60GM-FN. EXIT.
      *N60GP.    NOTE *CALCUATE SHARE IF SHARE PRICE      *.
       F60GP.    IF    LJ52-AFAVP > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F60GP-FN.
           COMPUTE     7-TEMP-QCSHOW ROUNDED =
           EP03-ADBRQ /
           LJ52-AFAVP.
       F60GP-FN. EXIT.
      *N60GT.    NOTE *CALCULATE OUTSTANDING SHARES       *.
       F60GT.    IF    GC03-QCSHOW > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F60GT-FN.
           COMPUTE     GC03-QCSHOW = GC03-QCSHOW -
           7-TEMP-QCSHOW.
                 IF    GC03-QCSHOW < 0                                  DOT
           MOVE        0 TO GC03-QCSHOW.
       F60GT-FN. EXIT.
      *N60GV.    NOTE *CALCULATE OUTSTANDING CERT SHRS    *.
       F60GV.    IF    GC03-QCSHIS > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F60GV-FN.
           COMPUTE     GC03-QCSHIS = GC03-QCSHIS -
           7-TEMP-QCSHOW.
                 IF    GC03-QCSHIS < 0                                  DOT
           MOVE        0 TO GC03-QCSHIS.
       F60GV-FN. EXIT.
       F60GA-FN. EXIT.
      *N60HA.    NOTE *IF CUSTODIAL ACCT/ DO SETTLEMENT   *.
       F60HA.    IF    GC04-CLCUS > ZERO                                lv10
                 NEXT SENTENCE ELSE GO TO     F60HA-FN.
           MOVE        GC04 TO EP04
           MOVE        ZERO TO EP04-AFEET
           EP04-ATERF
           MOVE        'N' TO EP04-ITERF
           MOVE        99 TO EP04-CLCUS.
       F60HA-FN. EXIT.
      *N60JA.    NOTE *CREATE THE MAIL FEE - DEST         *.
       F60JA.                                                           lv10
           MOVE        +214 TO EP06-GELL
           MOVE        001 TO EP06-NPISQ
           MOVE        PJ49-AEXML TO EP06-ACOTD
           MOVE        100 TO EP06-PPOTD
           MOVE        ZERO TO EP06-QPSTD
           MOVE        01 TO EP06-CPITC
           MOVE        SPACE TO EP06-ITRNB
           MOVE        004 TO EP06-CDELI
           MOVE        'P' TO EP06-CPAYC
           MOVE        'Y' TO EP06-ICHKP
           MOVE        ZERO TO EP06-CLTIN
           MOVE        SPACE TO EP06-IFHAI
           MOVE        SPACE TO EP06-CDQUA
           MOVE        7-SFEE-GENAL TO
           EP06-GENAL (1)
           MOVE        PJ49-TPAYB TO EP06-GENAL (2)
           MOVE        PJ49-NAIRB TO EP06-GESAD (1)
           MOVE        SPACES TO EP06-GESAD (2)
           EP06-GESAD (3).
       F60JA-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *IF TRANSFER; CALC COLLECTION       *
      *               *                                   *
      *               *************************************.
       F65.      IF    PJ49-IGC01 = 'Y'                                 lv05
                 AND   PJ49-IGC03 = 'Y'
                 AND   PJ49-IGC06 = 'Y'
                 AND   GC06-CPITC = 02
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *N65DA.    NOTE *CALCULATE THE ACASH FIELD          *.
       F65DA.                                                           lv10
           MOVE        ZERO TO 7-TEMP-CTWHAT.
      *N65DC.    NOTE *DETERMINE WITHHOLDING              *.
       F65DC.    IF    PJ49-PWHLD > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F65DC-FN.
      * - KD14 DOESN"T ROUND THIS
      *   CALCULATION SO, CI0113 WILL
      *   NOT EITHER
           COMPUTE     7-TEMP-CTWHAT =
           (PJ49-AEDRQ *
           PJ49-PWHLD) /
           100.
       F65DC-900. GO TO F65DG-FN.
       F65DC-FN. EXIT.
      *N65DG.    NOTE *ELSE... MOVE AMOUNT IF ANY         *.
       F65DG.                                                           lv15
           MOVE        PJ49-CTWHAT TO 7-TEMP-CTWHAT.
       F65DG-FN. EXIT.
      *N65DM.    NOTE *REDUCE THE ACASH BY DRAWS          *.
       F65DM.                                                           lv15
           COMPUTE     PJ49-ACASH = PJ49-AEDRQ -
           7-TEMP-CTWHAT -
           GC04-AFEET -
           GC04-ATERF.
       F65DM-FN. EXIT.
      *N65DT.    NOTE *IF AMOUNT IS REDUCED BELOW ZERO    *.
       F65DT.    IF    PJ49-ACASH < ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F65DT-FN.
           MOVE        ZERO TO PJ49-ACASH.
       F65DT-FN. EXIT.
       F65DA-FN. EXIT.
       F65-FN.   EXIT.
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
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *MISCELLANEOUS ROUTINES             *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TA5B       *.
       F92EB.                                                           lv10
           MOVE        '1' TO TA5B-IK.
       F92EB-FN. EXIT.
      *N92FA.    NOTE *ROUTINE FOR ROUTING TRANSIT #      *.
       F92FA.                                                           lv10
      *CHECK DIGIT BY MODULUS 10 METHOD                                 $MOD10
           MOVE        ZEROES TO 7-FA00-FILLER                          $MOD10
           COMPUTE     7-FA00-N11 = 3 * 7-FA00-N1                       $MOD10
           COMPUTE     7-FA00-N22 = 7-FA00-N11 +                        $MOD10
           (7 * 7-FA00-N2)                                              $MOD10
           COMPUTE     7-FA00-N33 = 7-FA00-N22 +                        $MOD10
           (1 * 7-FA00-N3)                                              $MOD10
           COMPUTE     7-FA00-N44 = 7-FA00-N33 +                        $MOD10
           (3 * 7-FA00-N4)                                              $MOD10
           COMPUTE     7-FA00-N55 = 7-FA00-N44 +                        $MOD10
           (7 * 7-FA00-N5)                                              $MOD10
           COMPUTE     7-FA00-N66 = 7-FA00-N55 +                        $MOD10
           (1 * 7-FA00-N6)                                              $MOD10
           COMPUTE     7-FA00-N77 = 7-FA00-N66 +                        $MOD10
           (3 * 7-FA00-N7)                                              $MOD10
           COMPUTE     7-FA00-N88 = 7-FA00-N77 +                        $MOD10
           (7 * 7-FA00-N8)                                              $MOD10
           COMPUTE     7-FA00-N99 = 7-FA00-N88 +                        $MOD10
           (1 * 7-FA00-N9)                                              $MOD10
           COMPUTE     7-FA00-N00 = 7-FA00-N99 +                        $MOD10
           (3 * 7-FA00-N0)                                              $MOD10
           COMPUTE     7-FA00-N111 = 7-FA00-N00 +                       $MOD10
           (7 * 7-FA00-NN1)                                             $MOD10
           COMPUTE     7-FA00-N222 = 7-FA00-N111 +                      $MOD10
           (1 * 7-FA00-NN2)                                             $MOD10
           COMPUTE     7-FA00-N333 = 7-FA00-N222 +                      $MOD10
           (3 * 7-FA00-NN3)                                             $MOD10
           COMPUTE     7-FA00-N444 = 7-FA00-N333 +                      $MOD10
           (7 * 7-FA00-NN4)                                             $MOD10
           COMPUTE     7-FA00-GECKD = 10 - 7-FA00-TOT3.                 $MOD10
      *THE CHECK DIGIT IS IN:                                           $MOD10
      *   7-FA00-GECKD.                                                 $MOD10
       F92FA-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA5B         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA5B-TABFO                             ADUTAB
           COMPUTE     G-TA5B-LTH = 60 + G-TA5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5B)                                ADUTAB
                       LENGTH (G-TA5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5B-TABCR NOT = '00'                          DOT
           PERFORM     F92EB THRU F92EB-FN.                             ADUTAB
       F92TA-FN. EXIT.
       F92-FN.   EXIT.
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
      *N94SB.    NOTE *CALL GU ON SB01                    *.            ADU026
       F94SB.                                                           lv10
           MOVE        'SBUP' TO DE10-XDBDNM                            ADU026
           MOVE        'SB01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 SB01                                                    ADU026
           S-SBU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94SB-FN. EXIT.
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
