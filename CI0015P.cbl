       IDENTIFICATION DIVISION.                                         CI0015
       PROGRAM-ID.  CI0015P.                                            CI0015
      *AUTHOR.         M\M - TRANSFER EDITS MOD.                        CI0015
      *DATE-COMPILED.   09/08/14.                                       CI0015
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1998                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CATS SYSTEM AND ALL INFORMATION RELATING THERETO,    *  ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CATS SYSTEM AND ALL            *  ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CATS       *  ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 1998                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0015
       CONFIGURATION SECTION.                                           CI0015
       SOURCE-COMPUTER. IBM-370.                                        CI0015
       OBJECT-COMPUTER. IBM-370.                                        CI0015
       DATA DIVISION.                                                   CI0015
       WORKING-STORAGE SECTION.                                         CI0015
      *                                                                 AMDU22
      ******************************************************************AMDU22
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR PAYMENT   *AMDU22
      **     EDITS OF AN ACCOUNT NUMBER.                               *AMDU22
      ******************************************************************AMDU22
      *                                                                 AMDU22
      *!WF DSP=PE DSL=DU SEL=22 FOR=I LEV=1                             AMDU22
       01                 PE00.                                         CI0015
          05              PE00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00234).                  CI0015
       01                 PE22  REDEFINES      PE00.                    CI0015
            10            PE22-C299.                                    CI0015
            11            PE22-CTID.                                    CI0015
            12            PE22-CTIDA  PICTURE  9(3).                    CI0015
            12            PE22-CTIDN.                                   CI0015
            13            PE22-CTIDNP PICTURE  X(13).                   CI0015
            13            PE22-CTIDND PICTURE  9(11).                   CI0015
            10            PE22-PRCOD  PICTURE  9(5).                    CI0015
            10            PE22-CTSTA  PICTURE  99.                      CI0015
            10            PE22-MAPPN  PICTURE  X(10).                   CI0015
            10            PE22-FILLER PICTURE  X(088).                  CI0015
            10            PE22-IARRGA PICTURE  X.                       CI0015
            10            PE22-IARLNA PICTURE  X.                       CI0015
            10            PE22-CELBL  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PE22-AMINAL PICTURE  S9(7)V99.                CI0015
            10            PE22-AMAXAL PICTURE  S9(7)V99.                CI0015
            10            PE22-AMIND  PICTURE  S9(7)V99.                CI0015
            10            PE22-AMAXAR PICTURE  S9(7)V99.                CI0015
            10            PE22-IARPSA PICTURE  X.                       CI0015
            10            PE22-CELBA  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PE22-INPAY  PICTURE  X(01).                   CI0015
            10            PE22-AMINAN PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PE22-AMAXAN PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PE22-FILLER PICTURE  X(042).                  CI0015
      *                                                                 AMDU22
      *                                                                 AMDU22
      *                                                                 AMDU22
      *                                                                 AMDU22
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.
       01  CI0018           PIC X(8) VALUE 'CI0018P '.
       01  CI0019           PIC X(8) VALUE 'CI0019P '.
       01  CI0021           PIC X(8) VALUE 'CI0021P '.                  AM0021
       01  CI0135           PIC X(8) VALUE 'CI0135P '.                  AM0135
      ***************************************************************** $CT100
      ** PARAMETER LIST FOR CAT100                                      $CT100
      ***************************************************************** $CT100
      *!WF DSP=CP DSL=CP SEL=9F FOR=I LEV=1                             $CT100
       01                 CP00.                                         CI0015
          05              CP00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00080).                  CI0015
       01                 CP9F  REDEFINES      CP00.                    CI0015
            10            CP9F-CTIDA  PICTURE  9(3).                    CI0015
            10            CP9F-PRCOD  PICTURE  9(5).                    CI0015
            10            CP9F-PRSCD  PICTURE  X(9).                    CI0015
            10            CP9F-FILLER PICTURE  X(20).                   CI0015
            10            CP9F-CTIDA1 PICTURE  9(3).                    CI0015
            10            CP9F-PRCOD1 PICTURE  9(5).                    CI0015
            10            CP9F-CPRSC2 PICTURE  X(9).                    CI0015
            10            CP9F-ISCHA  PICTURE  X.                       CI0015
            10            CP9F-CTRFA  PICTURE  X.                       CI0015
            10            CP9F-FILLER PICTURE  X(24).                   CI0015
                                                                        $CT100
       01               CAT100DY    PIC X(8)    VALUE 'CAT100DY'.       $CT100
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0015
            10            XW05-XW06.                                    CI0015
            11            XW05-XDBPCB.                                  CI0015
            12            XW05-XDBDNM PICTURE  X(08)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-XSEGLV PICTURE  X(02)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-XRC    PICTURE  X(02)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-XPROPT PICTURE  X(04)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-FILLER PICTURE  S9(5)                    CI0015
                          VALUE                ZERO                     CI0015
                          BINARY.                                       CI0015
            12            XW05-XSEGNM PICTURE  X(08)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0015
                          VALUE                ZERO                     CI0015
                          BINARY.                                       CI0015
            12            XW05-XSEGNB PICTURE  9(05)                    CI0015
                          VALUE                ZERO                     CI0015
                          BINARY.                                       CI0015
            12            XW05-XCOKEY PICTURE  X(70)                    CI0015
                          VALUE                SPACE.                   CI0015
            10            XW05-XW07.                                    CI0015
            11            XW05-XIOPCB.                                  CI0015
            12            XW05-XTERMI PICTURE  X(08)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-FILLER PICTURE  XX                       CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-XRC1   PICTURE  X(02)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-FILLER PICTURE  X(12)                    CI0015
                          VALUE                SPACE.                   CI0015
            12            XW05-XMODNM PICTURE  X(8)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0015
                          VALUE                ZERO.                    CI0015
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0015
                          VALUE                ZERO.                    CI0015
            10            XW05-XGU    PICTURE  X(4)                     CI0015
                          VALUE                'GU  '.                  CI0015
            10            XW05-XGHU   PICTURE  X(4)                     CI0015
                          VALUE                'GHU '.                  CI0015
            10            XW05-XGN    PICTURE  X(4)                     CI0015
                          VALUE                'GN  '.                  CI0015
            10            XW05-XGHN   PICTURE  X(4)                     CI0015
                          VALUE                'GHN '.                  CI0015
            10            XW05-XGNP   PICTURE  X(4)                     CI0015
                          VALUE                'GNP '.                  CI0015
            10            XW05-XGHNP  PICTURE  X(4)                     CI0015
                          VALUE                'GHNP'.                  CI0015
            10            XW05-XREPL  PICTURE  XXXX                     CI0015
                          VALUE                'REPL'.                  CI0015
            10            XW05-XISRT  PICTURE  X(4)                     CI0015
                          VALUE                'ISRT'.                  CI0015
            10            XW05-XDLET  PICTURE  X(4)                     CI0015
                          VALUE                'DLET'.                  CI0015
            10            XW05-XOPEN  PICTURE  X(4)                     CI0015
                          VALUE                'OPEN'.                  CI0015
            10            XW05-XCLSE  PICTURE  X(4)                     CI0015
                          VALUE                'CLSE'.                  CI0015
            10            XW05-XCHKP  PICTURE  X(4)                     CI0015
                          VALUE                'CHKP'.                  CI0015
            10            XW05-XXRST  PICTURE  X(4)                     CI0015
                          VALUE                'XRST'.                  CI0015
            10            XW05-XTERM  PICTURE  X(4)                     CI0015
                          VALUE                'TERM'.                  CI0015
            10            XW05-XNFPAC PICTURE  X(13)                    CI0015
                          VALUE                SPACE.                   CI0015
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0015
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0015
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE SS01 SEGMENT FOR THE            *
      **     'FROM' ACCOUNT ID NUMBER                                  *
      ******************************************************************
      *
      *!WF DSP=FF DSL=SS SEL=01 FOR=I LEV=1 PLT=FF
       01                 FF00.                                         CI0015
          05              FF00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00284).                  CI0015
       01                 FF01  REDEFINES      FF00.                    CI0015
            10            FF01-SS01K.                                   CI0015
            11            FF01-CTIDA  PICTURE  9(3).                    CI0015
            11            FF01-PRCOD  PICTURE  9(5).                    CI0015
            11            FF01-CPRSCN PICTURE  9(9).                    CI0015
            10            FF01-PRCODI PICTURE  9(5).                    CI0015
            10            FF01-PRCODS PICTURE  9(5).                    CI0015
            10            FF01-CLORN1 PICTURE  X(45).                   CI0015
            10            FF01-CLORN2 PICTURE  X(45).                   CI0015
            10            FF01-MFND1  PICTURE  X(22).                   CI0015
            10            FF01-MFND2  PICTURE  X(14).                   CI0015
            10            FF01-MFND3  PICTURE  XXX.                     CI0015
            10            FF01-DS01.                                    CI0015
            11            FF01-NCUSP  PICTURE  X(06).                   CI0015
            11            FF01-CCUSP  PICTURE  XX.                      CI0015
            11            FF01-GECKD  PICTURE  9.                       CI0015
            11            FF01-UCUSP  PICTURE  XX.                      CI0015
            10            FF01-DS22                                     CI0015
                          REDEFINES            FF01-DS01.               CI0015
            11            FF01-CCSIP  PICTURE  X(09).                   CI0015
            11            FF01-CCUSP1 PICTURE  XX.                      CI0015
            10            FF01-DPRDA  PICTURE  9(8).                    CI0015
            10            FF01-DPRDI  PICTURE  9(8).                    CI0015
            10            FF01-DPRDT  PICTURE  9(8).                    CI0015
            10            FF01-CTYFI  PICTURE  9(02).                   CI0015
            10            FF01-GEFYE  PICTURE  9(4).                    CI0015
            10            FF01-APARS  PICTURE  99V9(7)                  CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FF01-IASSC  PICTURE  X.                       CI0015
            10            FF01-IASSS  PICTURE  X.                       CI0015
            10            FF01-IRPUR  PICTURE  X.                       CI0015
            10            FF01-IRACC  PICTURE  X.                       CI0015
            10            FF01-IDDIV  PICTURE  X.                       CI0015
            10            FF01-IASSD  PICTURE  X.                       CI0015
            10            FF01-IDIVL  PICTURE  X.                       CI0015
            10            FF01-IDPAS  PICTURE  X.                       CI0015
            10            FF01-IDTEX  PICTURE  X.                       CI0015
            10            FF01-IFEDF  PICTURE  X.                       CI0015
            10            FF01-QFOSPD PICTURE  9(2).                    CI0015
            10            FF01-CLDTY  PICTURE  XX.                      CI0015
            10            FF01-QPRCF  PICTURE  9.                       CI0015
            10            FF01-QDMBR  PICTURE  99.                      CI0015
            10            FF01-CMBEM  PICTURE  9.                       CI0015
            10            FF01-AMWIP  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FF01-AMBIC  PICTURE  9(5)V99                  CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FF01-ARMSV  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FF01-QDRVP  PICTURE  99.                      CI0015
            10            FF01-NIDTB  PICTURE  9(5).                    CI0015
            10            FF01-CRTBL  PICTURE  99.                      CI0015
            10            FF01-CSTLD  PICTURE  X.                       CI0015
            10            FF01-IFOFD  PICTURE  X.                       CI0015
            10            FF01-IREFD  PICTURE  X.                       CI0015
            10            FF01-FILLER PICTURE  X(2).                    CI0015
            10            FF01-AEFEE  PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FF01-GEMIN                                    CI0015
                          OCCURS       005     TIMES.                   CI0015
            11            FF01-COOBF  PICTURE  9(2).                    CI0015
            11            FF01-AMTRO  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FF01-AMDER  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FF01-IFTDY  PICTURE  X.                       CI0015
      *
      *
      *
      *
      ******************************************************************ADUTAB
      **              TABLE GT06 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-GT06-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=GT DSL=TG SEL=06 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-GT06.                                                CI0015
           04    G-GT06-PARAM.                                          CI0015
             10  G-GT06-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0015
                        VALUE      +077.                                CI0015
             10  G-GT06-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0015
                        VALUE      +001.                                CI0015
             10  G-GT06-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0015
                        VALUE      +012.                                CI0015
             10  G-GT06-NUAPP  PICTURE 99                               CI0015
                        VALUE       0.                                  CI0015
             10  G-GT06-NUTAB  PICTURE X(6)                             CI0015
                        VALUE 'TG0006'.                                 CI0015
             10  G-GT06-TABFO  PICTURE XX                 VALUE SPACE.  CI0015
             10  G-GT06-TABCR  PICTURE XX                 VALUE SPACE.  CI0015
             10  G-GT06-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0015
             10  G-GT06-NUSSC  PICTURE X  VALUE   ' '.                  CI0015
             10  G-GT06-NUSSY  PICTURE X                  VALUE SPACE.  CI0015
             10  G-GT06-TRANID PICTURE X(4)               VALUE SPACE.  CI0015
             10  G-GT06-FILSYS.                                         CI0015
             15  G-GT06-USERC  PICTURE X(6)               VALUE SPACE.  CI0015
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0015
           04             GT06.                                         CI0015
            10            GT06-MPLAN  PICTURE  X(12)                    CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAR  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CPENS  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CTSAS  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CTSCA  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CPERS  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC1 PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC3 PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC4 PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC2 PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC5 PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC6 PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            GT06-CIRAC7 PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
      **                                                                ADUTAB
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU071
      *                   MS03                                          ADU071
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU071
      *                   MS03                                          ADU071
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
                                                                        AM0135
      *-----> PCB address list for calling CI0135...                    AM0135
      *                                                                 AM0135
       01                 CI0135-PCB-ADDRESS-LIST.                      AM0135
           05             CI0135-PCB-CH1P-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CCRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CPRP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CBTP-PTR1      POINTER.            AM0135
           05             CI0135-PCB-CA1P-PTR1      POINTER.            AM0135
                                                                        AM0021
      ******************************************************************AM0021
      **     PCB ADDRESS LIST FOR CI0021.  MODULE CI0021 WILL NEED     *AM0021
      **     PCB'S FOR:                                                *AM0021
      **                CERTS ACCOUNT DATABASE(CA1P)                   *AM0021
      **                LIFE MASTER DATABASE(LM1P)                     *AM0021
      ******************************************************************AM0021
                                                                        AM0021
       01  CI0021G-PCB-ADDRESS-LIST.                                    AM0021
           05  CI0021G-PCB-CA1P-PTR1      POINTER.                      AM0021
           05  CI0021G-PCB-LM1P-PTR1      POINTER.                      AM0021
       01                 PJ02.                                         CI0015
            10            PJ02-CTID   PICTURE  X(27).                   CI0015
            10            PJ02-DCACG  PICTURE  9(8).                    CI0015
            10            PJ02-ACCTV8 PICTURE  S9(9)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-AIDOL1 PICTURE  S9(9)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-AUINT1 PICTURE  S9(9)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-CXCSV  PICTURE  S9(7)V9(2)               CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PCIRB5 PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PANYDD PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PCIRA5 PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PANYDF PICTURE  9(3)V99                  CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PCIRCB PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PANYDG PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PPART  PICTURE  9(3)V99                  CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PMRTN  PICTURE  9(3)V99                  CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PMRTEB PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PBRITD PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-CEIAPI PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-CEIRND PICTURE  9(8).                    CI0015
            10            PJ02-CEIT   PICTURE  9(3).                    CI0015
            10            PJ02-DMATUR PICTURE  9(8).                    CI0015
            10            PJ02-AMTUR  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-CELBDT PICTURE  9(8).                    CI0015
            10            PJ02-DTRME  PICTURE  9(8).                    CI0015
            10            PJ02-NBSEI  PICTURE  999V99                   CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-NBSEIC PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-TRPTH  PICTURE  X(30).                   CI0015
            10            PJ02-CELBL  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-ALINT  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-PELIRB PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-ASANP  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-AAPAA  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-IQLIF  PICTURE  X.                       CI0015
            10            PJ02-QMTHAA PICTURE  9(2).                    CI0015
            10            PJ02-QMTHCC PICTURE  9(2).                    CI0015
            10            PJ02-QYEARA PICTURE  9(2).                    CI0015
            10            PJ02-DANNIA PICTURE  9(8).                    CI0015
            10            PJ02-PBONS  PICTURE  9(2).                    CI0015
            10            PJ02-AARQDA PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-AACFA  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-AIEPAA PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-CVSUR  PICTURE  X(30).                   CI0015
            10            PJ02-CPRDA1 PICTURE  9(3).                    CI0015
            10            PJ02-DFYR   PICTURE  9(4).                    CI0015
            10            PJ02-DFYRB  PICTURE  9(4).                    CI0015
            10            PJ02-DVALU  PICTURE  9(8).                    CI0015
            10            PJ02-DNIPM  PICTURE  9(8).                    CI0015
            10            PJ02-CIPFM  PICTURE  S9(3)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-CESLD  PICTURE  9(8).                    CI0015
            10            PJ02-CEHCD  PICTURE  9(3)                     CI0015
                          OCCURS       006     TIMES.                   CI0015
            10            PJ02-CETYPC PICTURE  9(2).                    CI0015
            10            PJ02-CEOTP  PICTURE  9(1).                    CI0015
            10            PJ02-CEIIS  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-DTRME1 PICTURE  9(8).                    CI0015
            10            PJ02-CEFOIM PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-CEIPDA PICTURE  S9(3)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-GECTR  PICTURE  99.                      CI0015
            10            PJ02-GMKTS.                                   CI0015
            11            PJ02-DTRME2 PICTURE  9(8)                     CI0015
                          OCCURS       005     TIMES.                   CI0015
            11            PJ02-DTRME3 PICTURE  9(8)                     CI0015
                          OCCURS       005     TIMES.                   CI0015
            10            PJ02-PRCOD  PICTURE  9(5).                    CI0015
            10            PJ02-CEFOTR PICTURE  S9(3)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            PJ02-DGPED  PICTURE  9(8).                    CI0015
            10            PJ02-DIPED  PICTURE  9(8).                    CI0015
            10            PJ02-FILLER PICTURE  X(27).                   CI0015
       01                 SS00.                                         CI0015
            02            SS01.                                         CI0015
            10            SS01-SS01K.                                   CI0015
            11            SS01-CTIDA  PICTURE  9(3).                    CI0015
            11            SS01-PRCOD  PICTURE  9(5).                    CI0015
            11            SS01-CPRSCN PICTURE  9(9).                    CI0015
            10            SS01-PRCODI PICTURE  9(5).                    CI0015
            10            SS01-PRCODS PICTURE  9(5).                    CI0015
            10            SS01-CLORN1 PICTURE  X(45).                   CI0015
            10            SS01-CLORN2 PICTURE  X(45).                   CI0015
            10            SS01-MFND1  PICTURE  X(22).                   CI0015
            10            SS01-MFND2  PICTURE  X(14).                   CI0015
            10            SS01-MFND3  PICTURE  XXX.                     CI0015
            10            SS01-DS01.                                    CI0015
            11            SS01-NCUSP  PICTURE  X(06).                   CI0015
            11            SS01-CCUSP  PICTURE  XX.                      CI0015
            11            SS01-GECKD  PICTURE  9.                       CI0015
            11            SS01-UCUSP  PICTURE  XX.                      CI0015
            10            SS01-DS22                                     CI0015
                          REDEFINES            SS01-DS01.               CI0015
            11            SS01-CCSIP  PICTURE  X(09).                   CI0015
            11            SS01-CCUSP1 PICTURE  XX.                      CI0015
            10            SS01-DPRDA  PICTURE  9(8).                    CI0015
            10            SS01-DPRDI  PICTURE  9(8).                    CI0015
            10            SS01-DPRDT  PICTURE  9(8).                    CI0015
            10            SS01-CTYFI  PICTURE  9(02).                   CI0015
            10            SS01-GEFYE  PICTURE  9(4).                    CI0015
            10            SS01-APARS  PICTURE  99V9(7)                  CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            SS01-IASSC  PICTURE  X.                       CI0015
            10            SS01-IASSS  PICTURE  X.                       CI0015
            10            SS01-IRPUR  PICTURE  X.                       CI0015
            10            SS01-IRACC  PICTURE  X.                       CI0015
            10            SS01-IDDIV  PICTURE  X.                       CI0015
            10            SS01-IASSD  PICTURE  X.                       CI0015
            10            SS01-IDIVL  PICTURE  X.                       CI0015
            10            SS01-IDPAS  PICTURE  X.                       CI0015
            10            SS01-IDTEX  PICTURE  X.                       CI0015
            10            SS01-IFEDF  PICTURE  X.                       CI0015
            10            SS01-QFOSPD PICTURE  9(2).                    CI0015
            10            SS01-CLDTY  PICTURE  XX.                      CI0015
            10            SS01-QPRCF  PICTURE  9.                       CI0015
            10            SS01-QDMBR  PICTURE  99.                      CI0015
            10            SS01-CMBEM  PICTURE  9.                       CI0015
            10            SS01-AMWIP  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            SS01-AMBIC  PICTURE  9(5)V99                  CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            SS01-ARMSV  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            SS01-QDRVP  PICTURE  99.                      CI0015
            10            SS01-NIDTB  PICTURE  9(5).                    CI0015
            10            SS01-CRTBL  PICTURE  99.                      CI0015
            10            SS01-CSTLD  PICTURE  X.                       CI0015
            10            SS01-IFOFD  PICTURE  X.                       CI0015
            10            SS01-IREFD  PICTURE  X.                       CI0015
            10            SS01-FILLER PICTURE  X(2).                    CI0015
            10            SS01-AEFEE  PICTURE  S9(3)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            SS01-GEMIN                                    CI0015
                          OCCURS       005     TIMES.                   CI0015
            11            SS01-COOBF  PICTURE  9(2).                    CI0015
            11            SS01-AMTRO  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            SS01-AMDER  PICTURE  S9(5)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            SS01-IFTDY  PICTURE  X.                       CI0015
      ******************************************************************ADUTAB
      **              TABLE TB5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TB5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TB DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TB5B.                                                CI0015
           04    G-TB5B-PARAM.                                          CI0015
             10  G-TB5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0015
                        VALUE      +154.                                CI0015
             10  G-TB5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0015
                        VALUE      +001.                                CI0015
             10  G-TB5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0015
                        VALUE      +017.                                CI0015
             10  G-TB5B-NUAPP  PICTURE 99                               CI0015
                        VALUE       0.                                  CI0015
             10  G-TB5B-NUTAB  PICTURE X(6)                             CI0015
                        VALUE 'TA005B'.                                 CI0015
             10  G-TB5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0015
             10  G-TB5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0015
             10  G-TB5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0015
             10  G-TB5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0015
             10  G-TB5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0015
             10  G-TB5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0015
             10  G-TB5B-FILSYS.                                         CI0015
             15  G-TB5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0015
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0015
           04             TB5B.                                         CI0015
            10            TB5B-GAPSC.                                   CI0015
            11            TB5B-CTIDA  PICTURE  9(3)                     CI0015
                          VALUE                ZERO.                    CI0015
            11            TB5B-PRCOD  PICTURE  9(5)                     CI0015
                          VALUE                ZERO.                    CI0015
            11            TB5B-PRSCD  PICTURE  X(9)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-PRCODX PICTURE  9(5)                     CI0015
                          VALUE                ZERO.                    CI0015
            10            TB5B-PRCSUB PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-PRCAUT PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-PRCBAS PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-PRCSTK PICTURE  XX                       CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-PRCPRE PICTURE  X(4)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IBDUP  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IUSPR  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-CVSYS  PICTURE  X(2)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IDTOD  PICTURE  X(1)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-GRSFC  PICTURE  99                       CI0015
                          VALUE                ZERO.                    CI0015
            10            TB5B-ZDA18  PICTURE  X(18)                    CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-CMPCTB PICTURE  X(4)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ITERM  PICTURE  X(1)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-AMFAC  PICTURE  S9(7)                    CI0015
                          VALUE                ZERO.                    CI0015
            10            TB5B-ZDA20  PICTURE  X(20)                    CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-CPRBK  PICTURE  X(3)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-CFXDM  PICTURE  99                       CI0015
                          VALUE                ZERO.                    CI0015
            10            TB5B-NGLCS  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-NDFCS  PICTURE  X(5)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ZDA20  PICTURE  X(20)                    CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-CTNLI  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-CBANK  PICTURE  X(03)                    CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ISYPO  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ISYPP  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ICOPT  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IANPY  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IDSAR  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ICIPT  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IANDS  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IKPMA  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-INMWT  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IVANT  PICTURE  X(1)                     CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ISDAV  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-IUDAV  PICTURE  X                        CI0015
                          VALUE                SPACE.                   CI0015
            10            TB5B-ZDA15  PICTURE  X(15)                    CI0015
                          VALUE                SPACE.                   CI0015
      **                                                                ADUTAB
      ******************************************************************ATG06O
      **             MISCELLANEOUS FIELDS USED BY MACRO ATG06O         *ATG06O
      ******************************************************************ATG06O
                                                                        ATG06O
       01  WORK-FIELDS.                                                 ATG06O
      *        THIS FIELD TELLS IF RULES CONTAINED IN PACBASE TABLE TG06ATG06O
      *        FOR WHETHER A TRANSFER BETWEEN TWO ACCOUNTS IS ALLOWED   ATG06O
      *        WILL BE OVERRIDDEN.                                      ATG06O
      *!WI pl=TG110                                                     ATG06O
           05  WORK-ITG6O                                               ATG06O
                        PICTURE X.                                      CI0015
                                                                        ATG06O
      *        THIS FIELD TELLS IF THE APPLICATION IS USED BY SERVICE   ATG06O
      *        AREAS (WHO ARE ALLOWED TO PROCESS SOME TYPES OF          ATG06O
      *        TRANSACTIONS THAT OTHER AREAS ARE NOT ALLOWED TO PROCESS)ATG06O
      *!WI pl=TG130                                                     ATG06O
           05  WORK-ISEPR                                               ATG06O
                        PICTURE X.                                      CI0015
                                                                        ATG06O
      *        THIS FIELD TELLS THE RESULT OF A TG06 OVERRIDE.  IT IS   ATG06O
      *        USED TO DIRECT PROCESSING AFTER EXECUTING THE MACRO CODE.ATG06O
      *!WI pl=TG150                                                     ATG06O
           05  WORK-CTG6O                                               ATG06O
                        PICTURE 99.                                     CI0015
      *
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************
      *
       01  W-WORK-MISC.
      *!WI
           05  W-FR01-MPLAN
                        PICTURE X(12).                                  CI0015
      *!WI
           05  W-TO01-MPLAN
                        PICTURE X(12).                                  CI0015
           05  W-TG06-CODES.
      *!WI
               10  W-TG06-ITRAN
                        PICTURE X.                                      CI0015
               10  FILLER       PIC X(4).

      *    INDICATES IF TB5B (PACBASE: TA5B) TABLE ENTRY FOUND
      *    '0' = FOUND
      *    '1' = NOT FOUND
           05  TB5B-IK          PIC X.

      *!WI
           05  W-FSRA-GRID
                        PICTURE X(13).                                  CI0015
      *!WI
           05  W-TSRA-GRID
                        PICTURE X(13).                                  CI0015
      *!WI
           05  W-FSRA-QITEM
                        PICTURE 9(3).                                   CI0015
      *!WI
           05  W-TSRA-QITEM
                        PICTURE 9(3).                                   CI0015
      *
      ******************************************************************
      **     WORK VERSION OF CT10 TO BREAK DOWN THE OCCURS PORTION     *
      **     OF THE ARRAY OUT OF CI0019                                *
      ******************************************************************
      *
      *!WF DSP=WS DSL=CT SEL=10 FOR=I LEV=1 PLT=WS
       01                 WS00.                                         CI0015
          05              WS00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00030).                  CI0015
       01                 WS10  REDEFINES      WS00.                    CI0015
            10            WS10-CT10K.                                   CI0015
            11            WS10-GR98.                                    CI0015
            12            WS10-GRID.                                    CI0015
            13            WS10-GRIDC  PICTURE  9(3).                    CI0015
            13            WS10-GRIDN.                                   CI0015
            14            WS10-GRIDNP PICTURE  99.                      CI0015
            14            WS10-GRIDND PICTURE  9(8).                    CI0015
            10            WS10-GR97                                     CI0015
                          REDEFINES            WS10-CT10K.              CI0015
            11            WS10-GRIDCB PICTURE  9(3).                    CI0015
            11            WS10-FILLER PICTURE  X(10).                   CI0015
            10            WS10-GERSD  PICTURE  9(8).                    CI0015
            10            WS10-GERED  PICTURE  9(8).                    CI0015
            10            WS10-GRCSI  PICTURE  X.                       CI0015
      *
      *
      *
       01 7-VANTAGE-LOB.
           05  7-TO-CVALB     PIC X(03)    VALUE SPACES.
           05  7-FROM-CVALB   PIC X(03)    VALUE SPACES.
      *
      *I-CODE FROM CT01.
       01 7-FROM-PRSCD.
           05 FILLER              PIC X(5).
           05 7-FROM-ICODE        PIC X(1).
           05 FILLER              PIC X(3).
       01 7-TO-PRSCD.
           05 FILLER              PIC X(5).
           05 7-TO-ICODE          PIC X(1).
           05 FILLER              PIC X(3).
      *
      *
      *
      **** CLASS OF MUTUAL FUND ACCOUNTS(DESTINATION)*********
      *!WI
       01  WS00-TO-PRSCD
                        PICTURE X(9).                                   CI0015
           88 TO-CLASS-B VALUE '000000002'.
      *!WI
       01  WS00-FR-PRSCD
                        PICTURE X(9).                                   CI0015
           88 FR-CLASS-B  VALUE '000000002'.
       01   DEBUT-WSS.                                                  CI0015
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0015
            05   IK     PICTURE X.                                      CI0015
       01  CONSTANTES-PAC.                                              CI0015
           05  FILLER  PICTURE X(87)   VALUE                            CI0015
                     '6015 CAT09/08/14CI0015ADMIN   14:33:55CI0015P AMERCI0015
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0015
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0015
           05  NUGNA   PICTURE X(5).                                    CI0015
           05  APPLI   PICTURE X(3).                                    CI0015
           05  DATGN   PICTURE X(8).                                    CI0015
           05  PROGR   PICTURE X(6).                                    CI0015
           05  CODUTI  PICTURE X(8).                                    CI0015
           05  TIMGN   PICTURE X(8).                                    CI0015
           05  PROGE   PICTURE X(8).                                    CI0015
           05  COBASE  PICTURE X(4).                                    CI0015
           05  DATGNC  PICTURE X(10).                                   CI0015
           05  RELEAS  PICTURE X(7).                                    CI0015
           05  DATGE   PICTURE X(10).                                   CI0015
           05  DATSQ   PICTURE X(10).                                   CI0015
       01  DATCE.                                                       CI0015
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0015
         05  DATOR.                                                     CI0015
           10  DATOA  PICTURE XX.                                       CI0015
           10  DATOM  PICTURE XX.                                       CI0015
           10  DATOJ  PICTURE XX.                                       CI0015
       01   VARIABLES-CONDITIONNELLES.                                  CI0015
            05                  FT      PICTURE X VALUE '0'.            CI0015
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0015
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0015
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0015
            05       5-PJ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0015
            05       5-SS00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0015
       01               S-SS01-SSA.                                     CI0015
            10         S1-SS01-SEGNAM PICTURE X(8)                      CI0015
                                      VALUE 'SS01    '.                 CI0015
            10         S1-SS01-CCOM   PICTURE X VALUE '*'.              CI0015
            10          S-SS01-CCOD   PICTURE X(5)                      CI0015
                                      VALUE '-----'.                    CI0015
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0015
       01            S-SSA01-SSA.                                       CI0015
            11      S1-SSA01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSA01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSA01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSA01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(NCUSP'.                   CI0015
            11       S-SSA01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSA01-NCUSP    PICTURE  X(06).                   CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSB01-SSA.                                       CI0015
            11      S1-SSB01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSB01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSB01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSB01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(CCUSP'.                   CI0015
            11       S-SSB01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSB01-CCUSP    PICTURE  XX.                      CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSC01-SSA.                                       CI0015
            11      S1-SSC01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSC01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSC01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSC01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(GECKD'.                   CI0015
            11       S-SSC01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSC01-GECKD    PICTURE  9.                       CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSD01-SSA.                                       CI0015
            11      S1-SSD01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSD01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSD01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSD01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(UCUSP'.                   CI0015
            11       S-SSD01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSD01-UCUSP    PICTURE  XX.                      CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSE01-SSA.                                       CI0015
            11      S1-SSE01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSE01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSE01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSE01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(PRCOD'.                   CI0015
            11       S-SSE01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSE01-PRCOD    PICTURE  9(5).                    CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSF01-SSA.                                       CI0015
            11      S1-SSF01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSF01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSF01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSF01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(CPRSCN'.                  CI0015
            11       S-SSF01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSF01-CPRSCN   PICTURE  9(9).                    CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSG01-SSA.                                       CI0015
            11      S1-SSG01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSG01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSG01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSG01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(CTIDA'.                   CI0015
            11       S-SSG01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSG01-CTIDA    PICTURE  9(3).                    CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSH01-SSA.                                       CI0015
            11      S1-SSH01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            11      S1-SSH01-CCOM   PICTURE X VALUE '*'.                CI0015
            11       S-SSH01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            11      S1-SSH01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(CCSIP'.                   CI0015
            11       S-SSH01-OPER  PICTURE XX VALUE ' ='.               CI0015
            11       S-SSH01-CCSIP    PICTURE  X(09).                   CI0015
            11  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01            S-SSU01-SSA.                                       CI0015
            10      S1-SSU01-SEGNAM PICTURE X(8)                        CI0015
                                      VALUE 'SS01    '.                 CI0015
            10      S1-SSU01-CCOM   PICTURE X VALUE '*'.                CI0015
            10       S-SSU01-CCOD   PICTURE X(5)                        CI0015
                                      VALUE '-----'.                    CI0015
            10      S1-SSU01-FLDNAM PICTURE X(9)                        CI0015
                                      VALUE '(SS01K'.                   CI0015
            10       S-SSU01-OPER  PICTURE XX VALUE ' ='.               CI0015
            10       S-SSU01-SS01K.                                     CI0015
            11       S-SSU01-CTIDA    PICTURE  9(3).                    CI0015
            11       S-SSU01-PRCOD    PICTURE  9(5).                    CI0015
            11       S-SSU01-CPRSCN   PICTURE  9(9).                    CI0015
            10  FILLER   PICTURE X    VALUE ')'.                        CI0015
       01   ZONES-UTILISATEUR PICTURE X.                                CI0015
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
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR LM1P                                           ADU015
            05 PCB-LM1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SSPP                                           ADU015
            05 PCB-SSPP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CH1P                                           ADU015
            05 PCB-CH1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CCRP                                           ADU015
            05 PCB-CCRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CPRP                                           ADU015
            05 PCB-CPRP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CBTP                                           ADU015
            05 PCB-CBTP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0015
          05              PA00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PA06  REDEFINES      PA00.                    CI0015
            10            PA06-XDBPCB.                                  CI0015
            11            PA06-XDBDNM PICTURE  X(08).                   CI0015
            11            PA06-XSEGLV PICTURE  X(02).                   CI0015
            11            PA06-XRC    PICTURE  X(02).                   CI0015
            11            PA06-XPROPT PICTURE  X(04).                   CI0015
            11            PA06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PA06-XSEGNM PICTURE  X(08).                   CI0015
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PA06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PA06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0015
          05              PB00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PB06  REDEFINES      PB00.                    CI0015
            10            PB06-XDBPCB.                                  CI0015
            11            PB06-XDBDNM PICTURE  X(08).                   CI0015
            11            PB06-XSEGLV PICTURE  X(02).                   CI0015
            11            PB06-XRC    PICTURE  X(02).                   CI0015
            11            PB06-XPROPT PICTURE  X(04).                   CI0015
            11            PB06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PB06-XSEGNM PICTURE  X(08).                   CI0015
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PB06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PB06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0015
          05              PC00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PC06  REDEFINES      PC00.                    CI0015
            10            PC06-XDBPCB.                                  CI0015
            11            PC06-XDBDNM PICTURE  X(08).                   CI0015
            11            PC06-XSEGLV PICTURE  X(02).                   CI0015
            11            PC06-XRC    PICTURE  X(02).                   CI0015
            11            PC06-XPROPT PICTURE  X(04).                   CI0015
            11            PC06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PC06-XSEGNM PICTURE  X(08).                   CI0015
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PC06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PC06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR LM1P                                             ADU015
      *!WF DSP=PD DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PD00.                                         CI0015
          05              PD00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PD06  REDEFINES      PD00.                    CI0015
            10            PD06-XDBPCB.                                  CI0015
            11            PD06-XDBDNM PICTURE  X(08).                   CI0015
            11            PD06-XSEGLV PICTURE  X(02).                   CI0015
            11            PD06-XRC    PICTURE  X(02).                   CI0015
            11            PD06-XPROPT PICTURE  X(04).                   CI0015
            11            PD06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PD06-XSEGNM PICTURE  X(08).                   CI0015
            11            PD06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PD06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PD06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR SSPP                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0015
          05              PF00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PF06  REDEFINES      PF00.                    CI0015
            10            PF06-XDBPCB.                                  CI0015
            11            PF06-XDBDNM PICTURE  X(08).                   CI0015
            11            PF06-XSEGLV PICTURE  X(02).                   CI0015
            11            PF06-XRC    PICTURE  X(02).                   CI0015
            11            PF06-XPROPT PICTURE  X(04).                   CI0015
            11            PF06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PF06-XSEGNM PICTURE  X(08).                   CI0015
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PF06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PF06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR CH1P                                             ADU015
      *!WF DSP=PG DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PG00.                                         CI0015
          05              PG00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PG06  REDEFINES      PG00.                    CI0015
            10            PG06-XDBPCB.                                  CI0015
            11            PG06-XDBDNM PICTURE  X(08).                   CI0015
            11            PG06-XSEGLV PICTURE  X(02).                   CI0015
            11            PG06-XRC    PICTURE  X(02).                   CI0015
            11            PG06-XPROPT PICTURE  X(04).                   CI0015
            11            PG06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PG06-XSEGNM PICTURE  X(08).                   CI0015
            11            PG06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PG06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PG06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR CCRP                                             ADU015
      *!WF DSP=PH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PH00.                                         CI0015
          05              PH00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PH06  REDEFINES      PH00.                    CI0015
            10            PH06-XDBPCB.                                  CI0015
            11            PH06-XDBDNM PICTURE  X(08).                   CI0015
            11            PH06-XSEGLV PICTURE  X(02).                   CI0015
            11            PH06-XRC    PICTURE  X(02).                   CI0015
            11            PH06-XPROPT PICTURE  X(04).                   CI0015
            11            PH06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PH06-XSEGNM PICTURE  X(08).                   CI0015
            11            PH06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PH06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PH06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR CPRP                                             ADU015
      *!WF DSP=PI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PI00.                                         CI0015
          05              PI00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PI06  REDEFINES      PI00.                    CI0015
            10            PI06-XDBPCB.                                  CI0015
            11            PI06-XDBDNM PICTURE  X(08).                   CI0015
            11            PI06-XSEGLV PICTURE  X(02).                   CI0015
            11            PI06-XRC    PICTURE  X(02).                   CI0015
            11            PI06-XPROPT PICTURE  X(04).                   CI0015
            11            PI06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PI06-XSEGNM PICTURE  X(08).                   CI0015
            11            PI06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PI06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PI06-XCOKEY PICTURE  X(70).                   CI0015
      *** PCB MASK FOR CBTP                                             ADU015
      *!WF DSP=PJ DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PJ00.                                         CI0015
          05              PJ00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00106).                  CI0015
       01                 PJ06  REDEFINES      PJ00.                    CI0015
            10            PJ06-XDBPCB.                                  CI0015
            11            PJ06-XDBDNM PICTURE  X(08).                   CI0015
            11            PJ06-XSEGLV PICTURE  X(02).                   CI0015
            11            PJ06-XRC    PICTURE  X(02).                   CI0015
            11            PJ06-XPROPT PICTURE  X(04).                   CI0015
            11            PJ06-FILLER PICTURE  S9(5)                    CI0015
                          BINARY.                                       CI0015
            11            PJ06-XSEGNM PICTURE  X(08).                   CI0015
            11            PJ06-XKEYLN PICTURE  S9(05)                   CI0015
                          BINARY.                                       CI0015
            11            PJ06-XSEGNB PICTURE  9(05)                    CI0015
                          BINARY.                                       CI0015
            11            PJ06-XCOKEY PICTURE  X(70).                   CI0015
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR           *
      **     PROCESSING OF THE TRANSFER EDIT MODULE(CI0015).           *
      ******************************************************************
      *
      *!WF DSP=TE DSL=DU SEL=17 FOR=I DES=1 LEV=1 PLT=05
       01                 TE17.                                         CI0015
            10            TE17-C299.                                    CI0015
            11            TE17-CTID.                                    CI0015
            12            TE17-CTIDA  PICTURE  9(3).                    CI0015
            12            TE17-CTIDN.                                   CI0015
            13            TE17-CTIDNP PICTURE  X(13).                   CI0015
            13            TE17-CTIDND PICTURE  9(11).                   CI0015
            10            TE17-CTID01.                                  CI0015
            11            TE17-CTIDA1 PICTURE  9(3).                    CI0015
            11            TE17-CTIDN1.                                  CI0015
            12            TE17-CTIDP1 PICTURE  X(13).                   CI0015
            12            TE17-CTIDNA PICTURE  9(11).                   CI0015
            10            TE17-DCACG  PICTURE  9(8).                    CI0015
            10            TE17-ISKED  PICTURE  X.                       CI0015
            10            TE17-CPMTC  PICTURE  99.                      CI0015
            10            TE17-CARTZ  PICTURE  99.                      CI0015
            10            TE17-MAPPN  PICTURE  X(10).                   CI0015
            10            TE17-CHCR   PICTURE  99.                      CI0015
            10            TE17-FILLER PICTURE  X(88).                   CI0015
            10            TE17-ISYPO  PICTURE  X.                       CI0015
            10            TE17-CIRAC  PICTURE  X(5).                    CI0015
            10            TE17-DEFFT  PICTURE  9(8).                    CI0015
            10            TE17-FILLER PICTURE  X(87).                   CI0015
      *
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE "FROM" ACCOUNT CT01 SEGMENT     *
      ******************************************************************
      *
      *!WF DSP=FR DSL=CT SEL=01 FOR=I LEV=1 PLT=10
       01                 FR00.                                         CI0015
          05              FR00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00222).                  CI0015
       01                 FR01  REDEFINES      FR00.                    CI0015
            10            FR01-CT01K.                                   CI0015
            11            FR01-C299.                                    CI0015
            12            FR01-CTID.                                    CI0015
            13            FR01-CTIDA  PICTURE  9(3).                    CI0015
            13            FR01-CTIDN.                                   CI0015
            14            FR01-CTIDNP PICTURE  X(13).                   CI0015
            14            FR01-CTIDND PICTURE  9(11).                   CI0015
            10            FR01-GECKD  PICTURE  9.                       CI0015
            10            FR01-GEMDA  PICTURE  9(8).                    CI0015
            10            FR01-NSEQ4B PICTURE  9(8)                     CI0015
                          BINARY.                                       CI0015
            10            FR01-GECUC  PICTURE  99.                      CI0015
            10            FR01-CTAUL  PICTURE  9(3).                    CI0015
            10            FR01-DIRAC  PICTURE  9(4).                    CI0015
            10            FR01-CTCCI  PICTURE  X.                       CI0015
            10            FR01-CTCUS  PICTURE  999.                     CI0015
            10            FR01-CTEFD  PICTURE  9(8).                    CI0015
            10            FR01-CTIAD  PICTURE  9(8).                    CI0015
            10            FR01-CLCUS  PICTURE  99.                      CI0015
            10            FR01-CAMMB  PICTURE  X(3).                    CI0015
            10            FR01-CKPMM  PICTURE  X.                       CI0015
            10            FR01-CTLAD  PICTURE  9(8).                    CI0015
            10            FR01-IPERS  PICTURE  X.                       CI0015
            10            FR01-AUNCB  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            FR01-CTLAT  PICTURE  9(8).                    CI0015
            10            FR01-CTLATC PICTURE  9(6).                    CI0015
            10            FR01-IMEGA  PICTURE  X.                       CI0015
            10            FR01-DIRAB  PICTURE  9(8).                    CI0015
            10            FR01-COLRQ  PICTURE  X.                       CI0015
            10            FR01-ZDA04  PICTURE  X(4).                    CI0015
            10            FR01-CTLPD  PICTURE  9(8).                    CI0015
            10            FR01-CIRASP PICTURE  9.                       CI0015
            10            FR01-CIRATP PICTURE  99.                      CI0015
            10            FR01-DRTHC  PICTURE  9(8).                    CI0015
            10            FR01-CPPTC  PICTURE  X.                       CI0015
            10            FR01-ZDA06  PICTURE  X(6).                    CI0015
            10            FR01-CTACD  PICTURE  9(8).                    CI0015
            10            FR01-CTNLI  PICTURE  X.                       CI0015
            10            FR01-CTRHO  PICTURE  9(8).                    CI0015
            10            FR01-CTSGD  PICTURE  9(8).                    CI0015
            10            FR01-CPATP  PICTURE  X(1).                    CI0015
            10            FR01-IRSTA  PICTURE  X.                       CI0015
            10            FR01-CTSTA  PICTURE  99.                      CI0015
            10            FR01-CTSSC  PICTURE  99.                      CI0015
            10            FR01-PRLIN  PICTURE  9(3).                    CI0015
            10            FR01-PRCOD  PICTURE  9(5).                    CI0015
            10            FR01-PRSCD  PICTURE  X(9).                    CI0015
            10            FR01-CTLNI  PICTURE  X.                       CI0015
            10            FR01-AYSIDA PICTURE  9(3).                    CI0015
            10            FR01-AYSID  PICTURE  9(5).                    CI0015
            10            FR01-CTBMC  PICTURE  99.                      CI0015
            10            FR01-CINAR  PICTURE  99.                      CI0015
            10            FR01-CPHTR  PICTURE  X.                       CI0015
            10            FR01-CDSTR  PICTURE  XX.                      CI0015
            10            FR01-CQACT  PICTURE  999.                     CI0015
            10            FR01-CIRAS  PICTURE  999.                     CI0015
            10            FR01-CIRAT  PICTURE  999.                     CI0015
            10            FR01-CLRAY  PICTURE  9(5).                    CI0015
            10            FR01-CATTP  PICTURE  X.                       CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0003 (OWNERSHIP)  *
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=FA DSL=DU SEL=04 FOR=I LEV=1 PLT=15
       01                 FA00.                                         CI0015
          05              FA00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00407).                  CI0015
       01                 FA04  REDEFINES      FA00.                    CI0015
            10            FA04-C299.                                    CI0015
            11            FA04-CTID.                                    CI0015
            12            FA04-CTIDA  PICTURE  9(3).                    CI0015
            12            FA04-CTIDN.                                   CI0015
            13            FA04-CTIDNP PICTURE  X(13).                   CI0015
            13            FA04-CTIDND PICTURE  9(11).                   CI0015
            10            FA04-IPOCH  PICTURE  X.                       CI0015
            10            FA04-FILLER PICTURE  X(099).                  CI0015
            10            FA04-CTTLN1 PICTURE  X(30).                   CI0015
            10            FA04-CTTLN2 PICTURE  X(30).                   CI0015
            10            FA04-CTTLN3 PICTURE  X(30).                   CI0015
            10            FA04-CTTBO1 PICTURE  X(45).                   CI0015
            10            FA04-CTTBO2 PICTURE  X(45).                   CI0015
            10            FA04-CTOWN  PICTURE  9(3).                    CI0015
            10            FA04-IUGMA  PICTURE  X.                       CI0015
            10            FA04-FILLER PICTURE  X(096).                  CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0018  ACCT CLIENTS*
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=FC DSL=DU SEL=14 FOR=I LEV=1 PLT=20
       01                 FC00.                                         CI0015
          05              FC00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00917).                  CI0015
       01                 FC14  REDEFINES      FC00.                    CI0015
            10            FC14-C299.                                    CI0015
            11            FC14-CTID.                                    CI0015
            12            FC14-CTIDA  PICTURE  9(3).                    CI0015
            12            FC14-CTIDN.                                   CI0015
            13            FC14-CTIDNP PICTURE  X(13).                   CI0015
            13            FC14-CTIDND PICTURE  9(11).                   CI0015
            10            FC14-DCACG  PICTURE  9(8).                    CI0015
            10            FC14-IPOCH  PICTURE  X.                       CI0015
            10            FC14-FILLER PICTURE  X(100).                  CI0015
            10            FC14-CLID01.                                  CI0015
            11            FC14-CLIDO1 PICTURE  X(3).                    CI0015
            11            FC14-NCLID1.                                  CI0015
            12            FC14-CLIDP1 PICTURE  X(12).                   CI0015
            12            FC14-CLIDNA PICTURE  9(8).                    CI0015
            10            FC14-CLCTR  PICTURE  9(3).                    CI0015
            10            FC14-DU21                                     CI0015
                          OCCURS       025     TIMES.                   CI0015
            11            FC14-C199.                                    CI0015
            12            FC14-CLID.                                    CI0015
            13            FC14-CLIDO  PICTURE  9(3).                    CI0015
            13            FC14-CLIDN.                                   CI0015
            14            FC14-CLIDNP PICTURE  X(12).                   CI0015
            14            FC14-CLIDND PICTURE  9(8).                    CI0015
            11            FC14-CLCTRC PICTURE  9(3).                    CI0015
            10            FC14-QITEM  PICTURE  9(3).                    CI0015
            10            FC14-XIMAX  PICTURE  S9(4)                    CI0015
                          BINARY.                                       CI0015
            10            FC14-CRROL  PICTURE  X.                       CI0015
            10            FC14-FILLER PICTURE  X(099).                  CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0019  ACCT GROUPS *
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=FG DSL=DU SEL=15 FOR=I LEV=1 PLT=25
       01                 FG00.                                         CI0015
          05              FG00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(04181).                  CI0015
       01                 FG15  REDEFINES      FG00.                    CI0015
            10            FG15-C299.                                    CI0015
            11            FG15-CTID.                                    CI0015
            12            FG15-CTIDA  PICTURE  9(3).                    CI0015
            12            FG15-CTIDN.                                   CI0015
            13            FG15-CTIDNP PICTURE  X(13).                   CI0015
            13            FG15-CTIDND PICTURE  9(11).                   CI0015
            10            FG15-DCACG  PICTURE  9(8).                    CI0015
            10            FG15-IPOCH  PICTURE  X.                       CI0015
            10            FG15-FILLER PICTURE  X(100).                  CI0015
            10            FG15-DU18                                     CI0015
                          OCCURS       010     TIMES.                   CI0015
            11            FG15-CT10.                                    CI0015
            12            FG15-CT10K.                                   CI0015
            13            FG15-GR98.                                    CI0015
            14            FG15-GRID.                                    CI0015
            15            FG15-GRIDC  PICTURE  9(3).                    CI0015
            15            FG15-GRIDN.                                   CI0015
            16            FG15-GRIDNP PICTURE  99.                      CI0015
            16            FG15-GRIDND PICTURE  9(8).                    CI0015
            12            FG15-GR97                                     CI0015
                          REDEFINES            FG15-CT10K.              CI0015
            13            FG15-GRIDCB PICTURE  9(3).                    CI0015
            13            FG15-FILLER PICTURE  X(10).                   CI0015
            12            FG15-GERSD  PICTURE  9(8).                    CI0015
            12            FG15-GERED  PICTURE  9(8).                    CI0015
            12            FG15-GRCSI  PICTURE  X.                       CI0015
            11            FG15-GR01.                                    CI0015
            12            FG15-GR01K.                                   CI0015
            13            FG15-GR98.                                    CI0015
            14            FG15-GRID.                                    CI0015
            15            FG15-GRIDC  PICTURE  9(3).                    CI0015
            15            FG15-GRIDN.                                   CI0015
            16            FG15-GRIDNP PICTURE  99.                      CI0015
            16            FG15-GRIDND PICTURE  9(8).                    CI0015
            12            FG15-GECKD  PICTURE  9.                       CI0015
            12            FG15-GEMDA  PICTURE  9(8).                    CI0015
            12            FG15-NSEQ4B PICTURE  9(8)                     CI0015
                          BINARY.                                       CI0015
            12            FG15-GRDOR  PICTURE  9(8).                    CI0015
            12            FG15-GRIAD  PICTURE  9(8).                    CI0015
            12            FG15-GECUC  PICTURE  99.                      CI0015
            12            FG15-GRLNG  PICTURE  99.                      CI0015
            12            FG15-GESLC  PICTURE  99.                      CI0015
            12            FG15-AYSIDA PICTURE  9(3).                    CI0015
            12            FG15-AYSID  PICTURE  9(5).                    CI0015
            12            FG15-GRCSD  PICTURE  9(8).                    CI0015
            12            FG15-GRCFD  PICTURE  9(8).                    CI0015
            12            FG15-GRNCL  PICTURE  S9(5)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            FG15-GRNCT  PICTURE  S9(5)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            FG15-GRSFC  PICTURE  99.                      CI0015
            12            FG15-GRCRN  PICTURE  9(3).                    CI0015
            12            FG15-GRCSS  PICTURE  X.                       CI0015
            12            FG15-MKSRC  PICTURE  99                       CI0015
                          OCCURS       010     TIMES.                   CI0015
            12            FG15-NEFPS  PICTURE  X(5).                    CI0015
            12            FG15-DEFPS  PICTURE  9(8).                    CI0015
            12            FG15-DLSRV  PICTURE  9(8).                    CI0015
            12            FG15-CTLNI  PICTURE  X.                       CI0015
            12            FG15-CGRLI  PICTURE  X.                       CI0015
            12            FG15-CAMGR  PICTURE  9(5)                     CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            FG15-CAMGS  PICTURE  9(5)                     CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            FG15-CAMGN  PICTURE  9(3)                     CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            FG15-CGRMF  PICTURE  X.                       CI0015
            12            FG15-FILLER PICTURE  X(08).                   CI0015
            11            FG15-GR07.                                    CI0015
            12            FG15-GEDLA  PICTURE  9(8).                    CI0015
            12            FG15-GRAID  PICTURE  X(12).                   CI0015
            12            FG15-GRPAP  PICTURE  X(14).                   CI0015
            12            FG15-GEPHNX PICTURE  9(4).                    CI0015
            12            FG15-DPLEF  PICTURE  9(8).                    CI0015
            12            FG15-DPLAM  PICTURE  9(8).                    CI0015
            12            FG15-NCPFN  PICTURE  9(6).                    CI0015
            12            FG15-GEFYE  PICTURE  9(4).                    CI0015
            12            FG15-FILLER PICTURE  X(06).                   CI0015
            12            FG15-GRPAN  PICTURE  X(45).                   CI0015
            12            FG15-CGRPA  PICTURE  99.                      CI0015
            12            FG15-IPRTT7 PICTURE  X.                       CI0015
            12            FG15-GRPED  PICTURE  9(8).                    CI0015
            12            FG15-FILLER PICTURE  X(05).                   CI0015
            12            FG15-GRPLC  PICTURE  99.                      CI0015
            12            FG15-GRPLT  PICTURE  99.                      CI0015
            12            FG15-FILLER PICTURE  X(04).                   CI0015
            12            FG15-GEADI  PICTURE  X.                       CI0015
            12            FG15-GRCFA  PICTURE  S9(11)V99                CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            FG15-GECFY  PICTURE  9(4).                    CI0015
            12            FG15-GECFC  PICTURE  99.                      CI0015
            12            FG15-MEMPL  PICTURE  X(20).                   CI0015
            12            FG15-CAUNIT PICTURE  X(4).                    CI0015
            12            FG15-FILLER PICTURE  X(21).                   CI0015
            12            FG15-GRPPP  PICTURE  999.                     CI0015
            12            FG15-CCORT  PICTURE  9(3).                    CI0015
            12            FG15-CIDRP  PICTURE  99.                      CI0015
            12            FG15-CCDWA  PICTURE  9.                       CI0015
            12            FG15-IERSA  PICTURE  X.                       CI0015
            12            FG15-DERSA  PICTURE  9(8).                    CI0015
            12            FG15-FILLER PICTURE  X(04).                   CI0015
            10            FG15-QITEM  PICTURE  9(3).                    CI0015
            10            FG15-XIMAX  PICTURE  S9(4)                    CI0015
                          BINARY.                                       CI0015
            10            FG15-FILLER PICTURE  X(100).                  CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE  "TO"  ACCOUNT CT01 SEGMENT     *
      ******************************************************************
      *
      *!WF DSP=TO DSL=CT SEL=01 FOR=I LEV=1 PLT=30
       01                 TO00.                                         CI0015
          05              TO00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00222).                  CI0015
       01                 TO01  REDEFINES      TO00.                    CI0015
            10            TO01-CT01K.                                   CI0015
            11            TO01-C299.                                    CI0015
            12            TO01-CTID.                                    CI0015
            13            TO01-CTIDA  PICTURE  9(3).                    CI0015
            13            TO01-CTIDN.                                   CI0015
            14            TO01-CTIDNP PICTURE  X(13).                   CI0015
            14            TO01-CTIDND PICTURE  9(11).                   CI0015
            10            TO01-GECKD  PICTURE  9.                       CI0015
            10            TO01-GEMDA  PICTURE  9(8).                    CI0015
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0015
                          BINARY.                                       CI0015
            10            TO01-GECUC  PICTURE  99.                      CI0015
            10            TO01-CTAUL  PICTURE  9(3).                    CI0015
            10            TO01-DIRAC  PICTURE  9(4).                    CI0015
            10            TO01-CTCCI  PICTURE  X.                       CI0015
            10            TO01-CTCUS  PICTURE  999.                     CI0015
            10            TO01-CTEFD  PICTURE  9(8).                    CI0015
            10            TO01-CTIAD  PICTURE  9(8).                    CI0015
            10            TO01-CLCUS  PICTURE  99.                      CI0015
            10            TO01-CAMMB  PICTURE  X(3).                    CI0015
            10            TO01-CKPMM  PICTURE  X.                       CI0015
            10            TO01-CTLAD  PICTURE  9(8).                    CI0015
            10            TO01-IPERS  PICTURE  X.                       CI0015
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            TO01-CTLAT  PICTURE  9(8).                    CI0015
            10            TO01-CTLATC PICTURE  9(6).                    CI0015
            10            TO01-IMEGA  PICTURE  X.                       CI0015
            10            TO01-DIRAB  PICTURE  9(8).                    CI0015
            10            TO01-COLRQ  PICTURE  X.                       CI0015
            10            TO01-ZDA04  PICTURE  X(4).                    CI0015
            10            TO01-CTLPD  PICTURE  9(8).                    CI0015
            10            TO01-CIRASP PICTURE  9.                       CI0015
            10            TO01-CIRATP PICTURE  99.                      CI0015
            10            TO01-DRTHC  PICTURE  9(8).                    CI0015
            10            TO01-CPPTC  PICTURE  X.                       CI0015
            10            TO01-ZDA06  PICTURE  X(6).                    CI0015
            10            TO01-CTACD  PICTURE  9(8).                    CI0015
            10            TO01-CTNLI  PICTURE  X.                       CI0015
            10            TO01-CTRHO  PICTURE  9(8).                    CI0015
            10            TO01-CTSGD  PICTURE  9(8).                    CI0015
            10            TO01-CPATP  PICTURE  X(1).                    CI0015
            10            TO01-IRSTA  PICTURE  X.                       CI0015
            10            TO01-CTSTA  PICTURE  99.                      CI0015
            10            TO01-CTSSC  PICTURE  99.                      CI0015
            10            TO01-PRLIN  PICTURE  9(3).                    CI0015
            10            TO01-PRCOD  PICTURE  9(5).                    CI0015
            10            TO01-PRSCD  PICTURE  X(9).                    CI0015
            10            TO01-CTLNI  PICTURE  X.                       CI0015
            10            TO01-AYSIDA PICTURE  9(3).                    CI0015
            10            TO01-AYSID  PICTURE  9(5).                    CI0015
            10            TO01-CTBMC  PICTURE  99.                      CI0015
            10            TO01-CINAR  PICTURE  99.                      CI0015
            10            TO01-CPHTR  PICTURE  X.                       CI0015
            10            TO01-CDSTR  PICTURE  XX.                      CI0015
            10            TO01-CQACT  PICTURE  999.                     CI0015
            10            TO01-CIRAS  PICTURE  999.                     CI0015
            10            TO01-CIRAT  PICTURE  999.                     CI0015
            10            TO01-CLRAY  PICTURE  9(5).                    CI0015
            10            TO01-CATTP  PICTURE  X.                       CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0003 (OWNERSHIP)  *
      **     FOR THE  "TO"  ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=TA DSL=DU SEL=04 FOR=I LEV=1 PLT=35
       01                 TA00.                                         CI0015
          05              TA00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00407).                  CI0015
       01                 TA04  REDEFINES      TA00.                    CI0015
            10            TA04-C299.                                    CI0015
            11            TA04-CTID.                                    CI0015
            12            TA04-CTIDA  PICTURE  9(3).                    CI0015
            12            TA04-CTIDN.                                   CI0015
            13            TA04-CTIDNP PICTURE  X(13).                   CI0015
            13            TA04-CTIDND PICTURE  9(11).                   CI0015
            10            TA04-IPOCH  PICTURE  X.                       CI0015
            10            TA04-FILLER PICTURE  X(099).                  CI0015
            10            TA04-CTTLN1 PICTURE  X(30).                   CI0015
            10            TA04-CTTLN2 PICTURE  X(30).                   CI0015
            10            TA04-CTTLN3 PICTURE  X(30).                   CI0015
            10            TA04-CTTBO1 PICTURE  X(45).                   CI0015
            10            TA04-CTTBO2 PICTURE  X(45).                   CI0015
            10            TA04-CTOWN  PICTURE  9(3).                    CI0015
            10            TA04-IUGMA  PICTURE  X.                       CI0015
            10            TA04-FILLER PICTURE  X(096).                  CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0018  ACCT CLIENTS*
      **     FOR THE  "TO"  ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=TC DSL=DU SEL=14 FOR=I LEV=1 PLT=40
       01                 TC00.                                         CI0015
          05              TC00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00917).                  CI0015
       01                 TC14  REDEFINES      TC00.                    CI0015
            10            TC14-C299.                                    CI0015
            11            TC14-CTID.                                    CI0015
            12            TC14-CTIDA  PICTURE  9(3).                    CI0015
            12            TC14-CTIDN.                                   CI0015
            13            TC14-CTIDNP PICTURE  X(13).                   CI0015
            13            TC14-CTIDND PICTURE  9(11).                   CI0015
            10            TC14-DCACG  PICTURE  9(8).                    CI0015
            10            TC14-IPOCH  PICTURE  X.                       CI0015
            10            TC14-FILLER PICTURE  X(100).                  CI0015
            10            TC14-CLID01.                                  CI0015
            11            TC14-CLIDO1 PICTURE  X(3).                    CI0015
            11            TC14-NCLID1.                                  CI0015
            12            TC14-CLIDP1 PICTURE  X(12).                   CI0015
            12            TC14-CLIDNA PICTURE  9(8).                    CI0015
            10            TC14-CLCTR  PICTURE  9(3).                    CI0015
            10            TC14-DU21                                     CI0015
                          OCCURS       025     TIMES.                   CI0015
            11            TC14-C199.                                    CI0015
            12            TC14-CLID.                                    CI0015
            13            TC14-CLIDO  PICTURE  9(3).                    CI0015
            13            TC14-CLIDN.                                   CI0015
            14            TC14-CLIDNP PICTURE  X(12).                   CI0015
            14            TC14-CLIDND PICTURE  9(8).                    CI0015
            11            TC14-CLCTRC PICTURE  9(3).                    CI0015
            10            TC14-QITEM  PICTURE  9(3).                    CI0015
            10            TC14-XIMAX  PICTURE  S9(4)                    CI0015
                          BINARY.                                       CI0015
            10            TC14-CRROL  PICTURE  X.                       CI0015
            10            TC14-FILLER PICTURE  X(099).                  CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0019  ACCT GROUPS *
      **     FOR THE  "TO"  ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=TG DSL=DU SEL=15 FOR=I LEV=1 PLT=45
       01                 TG00.                                         CI0015
          05              TG00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(04181).                  CI0015
       01                 TG15  REDEFINES      TG00.                    CI0015
            10            TG15-C299.                                    CI0015
            11            TG15-CTID.                                    CI0015
            12            TG15-CTIDA  PICTURE  9(3).                    CI0015
            12            TG15-CTIDN.                                   CI0015
            13            TG15-CTIDNP PICTURE  X(13).                   CI0015
            13            TG15-CTIDND PICTURE  9(11).                   CI0015
            10            TG15-DCACG  PICTURE  9(8).                    CI0015
            10            TG15-IPOCH  PICTURE  X.                       CI0015
            10            TG15-FILLER PICTURE  X(100).                  CI0015
            10            TG15-DU18                                     CI0015
                          OCCURS       010     TIMES.                   CI0015
            11            TG15-CT10.                                    CI0015
            12            TG15-CT10K.                                   CI0015
            13            TG15-GR98.                                    CI0015
            14            TG15-GRID.                                    CI0015
            15            TG15-GRIDC  PICTURE  9(3).                    CI0015
            15            TG15-GRIDN.                                   CI0015
            16            TG15-GRIDNP PICTURE  99.                      CI0015
            16            TG15-GRIDND PICTURE  9(8).                    CI0015
            12            TG15-GR97                                     CI0015
                          REDEFINES            TG15-CT10K.              CI0015
            13            TG15-GRIDCB PICTURE  9(3).                    CI0015
            13            TG15-FILLER PICTURE  X(10).                   CI0015
            12            TG15-GERSD  PICTURE  9(8).                    CI0015
            12            TG15-GERED  PICTURE  9(8).                    CI0015
            12            TG15-GRCSI  PICTURE  X.                       CI0015
            11            TG15-GR01.                                    CI0015
            12            TG15-GR01K.                                   CI0015
            13            TG15-GR98.                                    CI0015
            14            TG15-GRID.                                    CI0015
            15            TG15-GRIDC  PICTURE  9(3).                    CI0015
            15            TG15-GRIDN.                                   CI0015
            16            TG15-GRIDNP PICTURE  99.                      CI0015
            16            TG15-GRIDND PICTURE  9(8).                    CI0015
            12            TG15-GECKD  PICTURE  9.                       CI0015
            12            TG15-GEMDA  PICTURE  9(8).                    CI0015
            12            TG15-NSEQ4B PICTURE  9(8)                     CI0015
                          BINARY.                                       CI0015
            12            TG15-GRDOR  PICTURE  9(8).                    CI0015
            12            TG15-GRIAD  PICTURE  9(8).                    CI0015
            12            TG15-GECUC  PICTURE  99.                      CI0015
            12            TG15-GRLNG  PICTURE  99.                      CI0015
            12            TG15-GESLC  PICTURE  99.                      CI0015
            12            TG15-AYSIDA PICTURE  9(3).                    CI0015
            12            TG15-AYSID  PICTURE  9(5).                    CI0015
            12            TG15-GRCSD  PICTURE  9(8).                    CI0015
            12            TG15-GRCFD  PICTURE  9(8).                    CI0015
            12            TG15-GRNCL  PICTURE  S9(5)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            TG15-GRNCT  PICTURE  S9(5)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            TG15-GRSFC  PICTURE  99.                      CI0015
            12            TG15-GRCRN  PICTURE  9(3).                    CI0015
            12            TG15-GRCSS  PICTURE  X.                       CI0015
            12            TG15-MKSRC  PICTURE  99                       CI0015
                          OCCURS       010     TIMES.                   CI0015
            12            TG15-NEFPS  PICTURE  X(5).                    CI0015
            12            TG15-DEFPS  PICTURE  9(8).                    CI0015
            12            TG15-DLSRV  PICTURE  9(8).                    CI0015
            12            TG15-CTLNI  PICTURE  X.                       CI0015
            12            TG15-CGRLI  PICTURE  X.                       CI0015
            12            TG15-CAMGR  PICTURE  9(5)                     CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            TG15-CAMGS  PICTURE  9(5)                     CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            TG15-CAMGN  PICTURE  9(3)                     CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            TG15-CGRMF  PICTURE  X.                       CI0015
            12            TG15-FILLER PICTURE  X(08).                   CI0015
            11            TG15-GR07.                                    CI0015
            12            TG15-GEDLA  PICTURE  9(8).                    CI0015
            12            TG15-GRAID  PICTURE  X(12).                   CI0015
            12            TG15-GRPAP  PICTURE  X(14).                   CI0015
            12            TG15-GEPHNX PICTURE  9(4).                    CI0015
            12            TG15-DPLEF  PICTURE  9(8).                    CI0015
            12            TG15-DPLAM  PICTURE  9(8).                    CI0015
            12            TG15-NCPFN  PICTURE  9(6).                    CI0015
            12            TG15-GEFYE  PICTURE  9(4).                    CI0015
            12            TG15-FILLER PICTURE  X(06).                   CI0015
            12            TG15-GRPAN  PICTURE  X(45).                   CI0015
            12            TG15-CGRPA  PICTURE  99.                      CI0015
            12            TG15-IPRTT7 PICTURE  X.                       CI0015
            12            TG15-GRPED  PICTURE  9(8).                    CI0015
            12            TG15-FILLER PICTURE  X(05).                   CI0015
            12            TG15-GRPLC  PICTURE  99.                      CI0015
            12            TG15-GRPLT  PICTURE  99.                      CI0015
            12            TG15-FILLER PICTURE  X(04).                   CI0015
            12            TG15-GEADI  PICTURE  X.                       CI0015
            12            TG15-GRCFA  PICTURE  S9(11)V99                CI0015
                          COMPUTATIONAL-3.                              CI0015
            12            TG15-GECFY  PICTURE  9(4).                    CI0015
            12            TG15-GECFC  PICTURE  99.                      CI0015
            12            TG15-MEMPL  PICTURE  X(20).                   CI0015
            12            TG15-CAUNIT PICTURE  X(4).                    CI0015
            12            TG15-FILLER PICTURE  X(21).                   CI0015
            12            TG15-GRPPP  PICTURE  999.                     CI0015
            12            TG15-CCORT  PICTURE  9(3).                    CI0015
            12            TG15-CIDRP  PICTURE  99.                      CI0015
            12            TG15-CCDWA  PICTURE  9.                       CI0015
            12            TG15-IERSA  PICTURE  X.                       CI0015
            12            TG15-DERSA  PICTURE  9(8).                    CI0015
            12            TG15-FILLER PICTURE  X(04).                   CI0015
            10            TG15-QITEM  PICTURE  9(3).                    CI0015
            10            TG15-XIMAX  PICTURE  S9(4)                    CI0015
                          BINARY.                                       CI0015
            10            TG15-FILLER PICTURE  X(100).                  CI0015
      *
      *
      *
      *
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0015
            10            DE10-DU11.                                    CI0015
            11            DE10-XFONC  PICTURE  X(4).                    CI0015
            11            DE10-MPSBN  PICTURE  X(8).                    CI0015
            11            DE10-XDBDNM PICTURE  X(08).                   CI0015
            11            DE10-XSEGNM PICTURE  X(08).                   CI0015
            11            DE10-XRC    PICTURE  X(02).                   CI0015
            11            DE10-MSEG   PICTURE  X(08).                   CI0015
            11            DE10-XCOKEY PICTURE  X(70).                   CI0015
            11            DE10-CUIBR  PICTURE  X(01).                   CI0015
            11            DE10-CUIBA  PICTURE  X(01).                   CI0015
            11            DE10-IPBIK  PICTURE  X(1).                    CI0015
            10            DE10-DU03.                                    CI0015
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            11            DE10-CMSSF  PICTURE  XX.                      CI0015
            11            DE10-DU09.                                    CI0015
            12            DE10-CMESA  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            12            DE10-CMESB  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            12            DE10-CMSST  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            12            DE10-QELLAA PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            12            DE10-TMESS4 PICTURE  X(512).                  CI0015
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
       01                 MS00.                                         CI0015
          05              MS00-SUITE.                                   CI0015
            15       FILLER         PICTURE  X(00542).                  CI0015
       01                 MS03  REDEFINES      MS00.                    CI0015
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            10            MS03-CMSSF  PICTURE  XX.                      CI0015
            10            MS03-DU09.                                    CI0015
            11            MS03-CMESA  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            11            MS03-CMESB  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            11            MS03-CMSST  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            11            MS03-QELLAA PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
            11            MS03-TMESS4 PICTURE  X(512).                  CI0015
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0015
            10            MX11-QMSGS  PICTURE  9(03).                   CI0015
            10            MX11-PJ09                                     CI0015
                          OCCURS       025     TIMES.                   CI0015
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0015
                          COMPUTATIONAL-3.                              CI0015
            11            MX11-CMESB  PICTURE  S9(9)                    CI0015
                          BINARY.                                       CI0015
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                TE17
                                FR01
                                FA04
                                FC14
                                FG15
                                TO01
                                TA04
                                TC14
                                TG15
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0015
      *               *                                   *             CI0015
      *               *INITIALISATIONS                    *             CI0015
      *               *                                   *             CI0015
      *               *************************************.            CI0015
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
      *N02CA.    NOTE *INITIALIZE ERROR INDICATOR         *.
       F02CA.                                                           lv10
      *
      *********************************
      ** INITIALIZE THE PAYOUT ALLOWED*
      ** INDICATOR TO TRANSFER IS NOT *
      ** ALLOWED.                     *
      *********************************
      *
           MOVE        'N' TO TE17-ISYPO.
       F02CA-FN. EXIT.
      *N02CB.    NOTE *INITIALIZE                         *.
       F02CB.                                                           lv10
           MOVE        FR01-PRSCD TO WS00-FR-PRSCD
           MOVE        TO01-PRSCD TO WS00-TO-PRSCD.
       F02CB-FN. EXIT.
      *N02DA.    NOTE *INIT FROM ACCT SS01 SAVE AREA      *.
       F02DA.                                                           lv10
           INITIALIZE  FF01.
       F02DA-FN. EXIT.
      *N02FA.    NOTE *SEGMENT RE-INITIALIZATION          *.
       F02FA.                                                           lv10
           INITIALIZE  MS03.
       F02FA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR LM1P                                             DOT
           SET ADDRESS OF PD06 TO                                       ADU015
                PCB-LM1P-PTR1.                                          ADU015
      *SET ADDRESS FOR SSPP                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-SSPP-PTR1.                                          ADU015
      *SET ADDRESS FOR CH1P                                             DOT
           SET ADDRESS OF PG06 TO                                       ADU015
                PCB-CH1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CCRP                                             DOT
           SET ADDRESS OF PH06 TO                                       ADU015
                PCB-CCRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CPRP                                             DOT
           SET ADDRESS OF PI06 TO                                       ADU015
                PCB-CPRP-PTR1.                                          ADU015
      *SET ADDRESS FOR CBTP                                             DOT
           SET ADDRESS OF PJ06 TO                                       ADU015
                PCB-CBTP-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0015
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0015
      *               *                                   *             CI0015
      *               *FIN DE TRAITEMENT                  *             CI0015
      *               *                                   *             CI0015
      *               *************************************.            CI0015
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0015
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *FROM/TO ACCOUNT NUMBER EDITS       *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** EDITS FOR THE FROM AND THE   *
      ** TO ACCOUNT NUMBERS.          *
      *********************************
      *
      *N50BD.    NOTE *READ THE TA5B TABLE                *.
       F50BD.                                                           lv10
      *
      *********************************
      ** READ THE TA5B TABLE FOR THE  *
      ** PRCAUT VALUE TO SEE IF THE   *
      ** 'TO' ACCOUNT IS TERMINATED.  *
      *********************************
           MOVE        TO01-CTIDA TO TB5B-CTIDA
           MOVE        TO01-PRCOD TO TB5B-PRCOD.
                 IF    TO01-CTIDA = 002                                 DOT
           MOVE        TO01-PRSCD TO TB5B-PRSCD
                 ELSE
           MOVE        SPACES TO TB5B-PRSCD.
           MOVE        '0' TO TB5B-IK                                   DOT
           PERFORM     F92TB THRU F92TB-FN.
                 IF    TB5B-IK = '1'                                    DOT
      *TA5B TABLE ENTRY NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012405 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N50BM.    NOTE *TERMINATED 'TO' ACCOUNT            *.
       F50BM.    IF    TB5B-PRCAUT = 'T'                                lv15
                 OR    (TB5B-PRCAUT = 'I'
                 AND   TO01-CTIDA = 021
                 AND   (TO01-PRCOD = 020
                 OR    TO01-PRCOD = 021))
                 NEXT SENTENCE ELSE GO TO     F50BM-FN.
      *********************************
      *IF THE 'TO' ACCOUNT NUMBER IS
      *TERMINATED OR IF IT IS BROKERAGE
      *ACCOUNT WITH PRCOD 20 OR 21
      *AND IS INACTIVE THEN IT IS A
      *INVALID TRANSFER.
      *SEND THE ERROR MESSAGE AND
      *RETURN TO THE CALLING MODULE.
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012404 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BM-FN. EXIT.
       F50BD-FN. EXIT.
      *N50BO.    NOTE *REJECT SHARK FUND 601 CLASS B AS   *.
       F50BO.    IF    TO01-CTIDA = 002                                 lv10
                 AND   TO01-PRCOD = 601
                 AND   TO-CLASS-B
                 NEXT SENTENCE ELSE GO TO     F50BO-FN.
      *DESTINATION ACCOUNT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015609 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BO-FN. EXIT.
      *N50BX.    NOTE *REJECT THE MUTUAL FUND CLASS B     *.
       F50BX.    IF    ((NOT FR-CLASS-B                                 lv10
                 AND   FR01-CTIDA = 002)
                 OR    FR01-CTIDA NOT = 002)
                 AND   (TO01-CTIDA = 002
                 AND   TO-CLASS-B)
                 NEXT SENTENCE ELSE GO TO     F50BX-FN.
      *AS DESTINATION ACCOUNT, WHEN
      *SOURCE ACCOUNT AS: NOT MF ACCT,
      *NOT MF CLASS A/C
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015570 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BX-FN. EXIT.
      *N50DC.    NOTE *IF 'FROM' ACCT IS INACTIVE CERT    *.
       F50DC.    IF    FR01-CTIDA = 001                                 lv10
                 AND   FR01-CTSTA = 03
                 AND   TE17-MAPPN = 'SD'
                 NEXT SENTENCE ELSE GO TO     F50DC-FN.
      * - ERROR - NOT ALLOWED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012840 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50DC-FN. EXIT.
      *N50EA.    NOTE *IF 'TO' ACCT IS PENDING/INACTIVE   *.
       F50EA.    IF    (TO01-CTIDA = 002                                lv10
                 AND   TO01-PRCOD = 102
                 OR    TO01-PRCOD = 106
                 OR    TO01-PRCOD = 107
                 OR    TO01-PRCOD = 108
                 OR    TO01-PRCOD = 124
                 OR    TO01-PRCOD = 125
                 OR    TO01-PRCOD = 126)
                 AND   (TO01-CTSTA = 01
                 OR    TO01-CTSTA = 03)
                 AND   TE17-MAPPN = 'SD'
                 NEXT SENTENCE ELSE GO TO     F50EA-FN.
      *IPS FUND
      *FRF FUND
      *ARC FUND
      *DSV FUND
      *CONTRARIAN EQUITY FUND
      *U.S. EQUITY FUND
      *AND THREADNEEDLE GLOBAL EXTENDED
      *ALPHA FUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50EA-FN. EXIT.
      *N50GA.    NOTE *IF 'TO' ACCT ADMIN NOT A FUND OR   *.
       F50GA.    IF    TO01-CTSTA = 03                                  lv10
                 AND   ((TO01-CTIDA NOT = 002
                 AND   TO01-CTIDA NOT = 021
                 AND   TO01-CTIDA NOT = 133)
                 OR    (TO01-CTIDA = 021
                 AND   (TO01-PRCOD = 00015
                 OR    TO01-PRCOD = 00016
                 OR    TO01-PRCOD = 00022))
                 OR    (TO01-CTIDA = 133
                 AND   (TO01-PRCOD = 00014
                 OR    TO01-PRCOD = 00015
                 OR    TO01-PRCOD = 00016
                 OR    TO01-PRCOD = 00022)))
                 NEXT SENTENCE ELSE GO TO     F50GA-FN.
      *BROK/BETA BROK AND ACCT STATUS
      *IS 'INACTIVE', THEN ERROR OUT
      *********************************
      ** INACTIVE BROKERAGE PRODUCTS  *
      ** ALLOWED AS PART OF PTR#3839  *
      ** EXCEPT FOR SPS PRODUCTS      *
      ** AND DMA PRODUCT              *
      *********************************
      ** INACTIVE BETA BROK PRODUCTS  *
      ** ALLOWED AS PART OF PTR#9812  *
      ** EXCEPT FOR SPS PRODUCTS AND  *
      ** ACTIVE PORTFOLIOS PRODUCTS   *
      *********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012843 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50GA-FN. EXIT.
      *N50GM.    NOTE *IF NO 'TO' ACCT OWNERSHIP FOUND    *.
       F50GM.    IF    TA04-CTTLN1 = SPACES                             lv10
                 AND   TA04-CTTLN2 = SPACES
                 AND   TA04-CTTLN3 = SPACES
                 NEXT SENTENCE ELSE GO TO     F50GM-FN.
      *********************************
      ** SEE IF ANY OWNERSHIP LINES   *
      ** ARE IN THE PASS AREA FROM A  *
      ** CALL TO CI0003 FOR THE 'TO'  *
      ** ACCOUNT.                     *
      *********************************
      **
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013054 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50GM-FN. EXIT.
      *N50HA.    NOTE *'TO' ACCT MUST = CERT/FUND/        *.
       F50HA.    IF    TO01-CTIDA NOT = 001 AND 002                     lv10
                       AND 004
                       AND 005
                       AND 013
                       AND 021
                       AND 133
                 NEXT SENTENCE ELSE GO TO     F50HA-FN.
      *                 LIFE/
      *                 LIFE NY/
      *                 FINANCIAL PLAN/
      *                 BROKERAGE/
      *                 BTEA BROKERAGE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012089 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50HA-FN. EXIT.
      *N50HD.    NOTE *IF DIV/INT DISBURSEMENT TYPE       *.
       F50HD.    IF    (TE17-CARTZ = 05 OR 06)                          lv10
                 AND   TE17-MAPPN = 'SD'
                 NEXT SENTENCE ELSE GO TO     F50HD-FN.
      *N50HH.    NOTE *IF 'TO' ACCT IS MUST-EQUAL LIFE    *.
       F50HH.    IF    (TO01-CTIDA = 004                                lv15
                 AND   (TO01-CTIDND (1:4) = 9000
                 OR    TO01-CTIDND (1:4) = 9100))
                 OR    (TO01-CTIDA = 005
                 AND   (TO01-CTIDND (1:4) = 9700
                 OR    TO01-CTIDND (1:4) = 9800))
                 NEXT SENTENCE ELSE GO TO     F50HH-FN.
      *********************************
      ** NO DIVIDEND OR INTEREST DISB *
      ** CAN BE SET UP TO GO TO A MUST*
      ** EQUAL LIFE PRODUCT.          *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012991 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50HH-FN. EXIT.
       F50HD-FN. EXIT.
      *N50MA.    NOTE *SHARE CLASS EDITS                  *.
       F50MA.         EXIT.                                             lv10
      *N50MC.    NOTE *EDIT FUND DIVIDEND TO FUND TRNSF   *.
       F50MC.    IF    FR01-CTIDA = 002                                 lv15
                 AND   TO01-CTIDA = 002
                 AND   TE17-CARTZ = 05
                 NEXT SENTENCE ELSE GO TO     F50MC-FN.
      *CAT100 IS NOT ACCURATE FOR FUND
      *DIVIDEND.  IT SHOULD ONLY ALLOW
      *SAME CLASS TRANSFERS. 'A' TO 'A'
      *'B' TO 'B' & 'Y' TO 'Y'.
      *-DIVIDENDS (CARTZ=05) ARE EDITED
      * OUTSIDE OF CAT100.
      *
      *N50ME.    NOTE *IF FROM/TO ACCOUNT CLASSES DON'T   *.
       F50ME.    IF    FR01-PRSCD NOT = TO01-PRSCD                      lv20
                 NEXT SENTENCE ELSE GO TO     F50ME-FN.
      *MATCH - ERROR OUT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013128 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50ME-FN. EXIT.
       F50MC-900. GO TO F50MM-FN.
       F50MC-FN. EXIT.
      *N50MM.    NOTE *ENSURE THE ACCTS ARE COMPATABLE    *.
       F50MM.                                                           lv15
      *
      *CHECK THE CAT100 PROGRAM TO MAKE
      *SURE THE FROM ACCOUNT CAN TRANS-
      *FER FUNDS TO THE 'TO' ACCT.
      *THIS IS DONE INITIALLY FOR FUND
      *MULTIPLE PRICING.
      *
      *CI9005 HAS ALREADY SET THE PRSCD
      *TO 000000001 IF IT WAS SPACES ON
      *CONTRACT DB (FOR 'FUNDS' ONLY).
      *ALL OTHER ADMINS SHOULD SET THE
      *PRSCD TO SPACES WHEN READING THE
      *TA5B TABLE.
      *
           MOVE        FR01-CTIDA TO CP9F-CTIDA
           MOVE        FR01-PRCOD TO CP9F-PRCOD
           MOVE        TO01-CTIDA TO CP9F-CTIDA1
           MOVE        TO01-PRCOD TO CP9F-PRCOD1
           MOVE        'N' TO CP9F-CTRFA.
      *N50MN.    NOTE *SET PRSCD FOR TA5B TABLE READ      *.
       F50MN.    IF    FR01-CTIDA = 002                                 lv20
                 NEXT SENTENCE ELSE GO TO     F50MN-FN.
           MOVE        FR01-PRSCD TO CP9F-PRSCD.
       F50MN-900. GO TO F50MP-FN.
       F50MN-FN. EXIT.
      *N50MP.    NOTE *IF NOT FROM FUNDS                  *.
       F50MP.                                                           lv20
           MOVE        SPACES TO CP9F-PRSCD.
       F50MP-FN. EXIT.
      *N50MQ.    NOTE *SET PRSCD FOR TA5B TABLE READ      *.
       F50MQ.    IF    TO01-CTIDA = 002                                 lv20
                 NEXT SENTENCE ELSE GO TO     F50MQ-FN.
           MOVE        TO01-PRSCD TO CP9F-CPRSC2.
       F50MQ-900. GO TO F50MR-FN.
       F50MQ-FN. EXIT.
      *N50MR.    NOTE *IF NOT TO A FUND                   *.
       F50MR.                                                           lv20
           MOVE        SPACES TO CP9F-CPRSC2.
       F50MR-FN. EXIT.
      *N50MS.    NOTE *IF SD - SCHEDULED ACTIVITY         *.
       F50MS.    IF    TE17-MAPPN = 'SD'                                lv20
                 NEXT SENTENCE ELSE GO TO     F50MS-FN.
           MOVE        'Y' TO CP9F-ISCHA.
       F50MS-900. GO TO F50MT-FN.
       F50MS-FN. EXIT.
      *N50MT.    NOTE *IF NOT A SCHEDULED ACTIVITY        *.
       F50MT.                                                           lv20
           MOVE        'N' TO CP9F-ISCHA.
       F50MT-FN. EXIT.
      *N50MU.    NOTE *CALL THE MODULE                    *.
       F50MU.                                                           lv20
      *CALL MONEY MOVEMENT RULES ROUTIN                                 DOT
           CALL        'CAT100' USING CP9F.
       F50MU-FN. EXIT.
      *N50MV.    NOTE *SEND ERROR MSG IF NOT ALLOWED      *.
       F50MV.    IF    CP9F-CTRFA NOT = 'Y'                             lv20
                 NEXT SENTENCE ELSE GO TO     F50MV-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012406 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50MV-FN. EXIT.
       F50MM-FN. EXIT.
       F50MA-FN. EXIT.
      *N50NC.    NOTE *UD SPECIFIC EDIT                   *.
       F50NC.    IF    FR01-CTIDA = 002                                 lv10
                 AND   TO01-CTIDA = 002
                 AND   (TE17-MAPPN = 'UD' OR 'FDC')
                 NEXT SENTENCE ELSE GO TO     F50NC-FN.
      *IF 'FROM' ACCT IS FUND
      *   'TO' ACCT IS FUND
      *N50ND.    NOTE *CLASS TRANSFER CHECK FOR UD        *.
       F50ND.    IF    (FR01-PRSCD = 000000001                          lv15
                 AND   TO01-PRSCD = 000000003)
                 OR    (FR01-PRSCD = 000000003
                 AND   TO01-PRSCD = 000000001)
                 NEXT SENTENCE ELSE GO TO     F50ND-FN.
      *  CLASS A TO Y NOT ALLOWED
      *  CLASS Y TO A NOT ALLOWED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012406 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50ND-FN. EXIT.
       F50NC-FN. EXIT.
      *N50PB.    NOTE *IF 'TO' ACCT CASH CLASS B          *.
       F50PB.    IF    TO01-CTIDA = 002                                 lv10
                 AND   (TO01-PRCOD = 00013
                 OR    TO01-PRCOD = 00167)
                 AND   TO01-PRSCD = '000000002'
                 NEXT SENTENCE ELSE GO TO     F50PB-FN.
      *FUND CASH CLASS B OR RVS GOVT
      *MONEY MKT CLASS B CANNOT ACCEPT
      *TRANSFERS EXCEPT FROM 'B' FUNDS
                 IF    FR01-CTIDA = 002                                 DOT
                 AND   FR01-PRSCD = '000000002'
               GO TO     F50PB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012406 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50PB-FN. EXIT.
      *N50PC.    NOTE *IF 'TO' ACCT CASH CLASS C          *.
       F50PC.    IF    TO01-CTIDA = 002                                 lv10
                 AND   (TO01-PRCOD = 00013
                 OR    TO01-PRCOD = 00167)
                 AND   TO01-PRSCD = '000000006'
                 NEXT SENTENCE ELSE GO TO     F50PC-FN.
      *FUND CASH CLASS C OR RVS GOVT
      *MONEY MKT CLASS C CAN'T ACCEPT
      *TRANSFERS EXCEPT FROM 'C' FUNDS
                 IF    FR01-CTIDA = 002                                 DOT
                 AND   FR01-PRSCD = '000000006'
               GO TO     F50PC-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012406 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50PC-FN. EXIT.
       F50-FN.   EXIT.
      *N52BE.    NOTE *CALL CI0021 - PAYMENT EDITS        *.            AM0021
       F52BE.                                                           lv10
      *********************************                                 AM0021
      *FOR DEST ACCT PASSED IN,                                         AM0021
      *SETS INDICATORS FOR TYPES OF                                     AM0021
      *PAYMENTS ALLOWED.                                                AM0021
      *PE22-IARRGA - REG PMTS ALLOWED                                   AM0021
      *PE22-IARLNA - LOAN REPMTS ALLWD                                  AM0021
      *PE22-INPAY  - NEW PMTS ALLOWED                                   AM0021
      *                                                                 AM0021
      *ALSO SETS MIN & MAX PMT AMTS FOR                                 AM0021
      *EACH OF THE 3 TYPES OF PMTS.                                     AM0021
      *                                                                 AM0021
      *ALSO RETURNS 2 LOAN BALANCE AMTS                                 AM0021
      *********************************                                 AM0021
           MOVE        TE17-CTID01 TO PE22-CTID                         AM0021
           MOVE        TO01-PRCOD TO PE22-PRCOD                         AM0021
           MOVE        TO01-CTSTA TO PE22-CTSTA                         AM0021
           MOVE        TE17-MAPPN TO PE22-MAPPN                         AM0021
           SET CI0021G-PCB-CA1P-PTR1 TO                                 AM0021
                       PCB-CA1P-PTR1                                    AM0021
           SET CI0021G-PCB-LM1P-PTR1 TO                                 AM0021
                       PCB-LM1P-PTR1                                    AM0021
           CALL        CI0021 USING                                     AM0021
           DFHEIBLK                                                     AM0021
           DFHCOMMAREA                                                  AM0021
           DLIUIBII                                                     AM0021
           CI0021G-PCB-ADDRESS-LIST                                     AM0021
           PE22                                                         AM0021
           DE10                                                         AM0021
           MS03                                                         AM0021
           MX11.                                                        AM0021
      *N52BG.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F52BG.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F52BG-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0021 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0021 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F52BG-900. GO TO F52BJ-FN.
       F52BG-FN. EXIT.
      *N52BJ.    NOTE *NO ERRORS                          *.            ADU071
       F52BJ.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F52BJ-FN. EXIT.
       F52BE-FN. EXIT.
      *N52BK.    NOTE *IF SD AND FROM/TO ACCOUNT ID NOT   *.
       F52BK.    IF    TE17-CARTZ = 02                                  lv10
                 AND   FR01-CTID NOT = TO01-CTID
                 AND   FR01-CTIDA = 001
                 AND   (FR01-PRCOD = 00181
                 OR    FR01-PRCOD = 00961)
                 AND   TE17-MAPPN = 'SD'
                 NEXT SENTENCE ELSE GO TO     F52BK-FN.
      *THE SAME AND MARKET STRATEGY,
      *SIGNAL ERROR-ONLY VALID TRANSFER
      *IS INTRA ACCOUNT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013227 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52BK-FN. EXIT.
      *N52BL.    NOTE *IF FROM/TO ACCOUNT ID THE SAME     *.
       F52BL.    IF    FR01-CTID = TO01-CTID                            lv10
                 NEXT SENTENCE ELSE GO TO     F52BL-FN.
      *N52BN.    NOTE *IF 'TO' ACCT IS A CERT             *.
       F52BN.    IF    TO01-CTIDA = 001                                 lv15
                 AND   TE17-MAPPN = 'SD'
                 NEXT SENTENCE ELSE GO TO     F52BN-FN.
      *N52BP.    NOTE *IF MARKET STRATEGY, SAME FROM      *.
       F52BP.    IF    TE17-CARTZ = 02                                  lv17
                 AND   FR01-CTIDA = 001
                 AND   (FR01-PRCOD = 00181
                 OR    FR01-PRCOD = 00961)
                 NEXT SENTENCE ELSE GO TO     F52BP-FN.
      *AND TO IS ALLOWED FOR SD'S
       F52BP-900. GO TO F52BR-FN.
       F52BP-FN. EXIT.
      *N52BR.    NOTE *ELSE                               *.
       F52BR.         EXIT.                                             lv17
      *N52BT.    NOTE *IF CI0021 IND LOAN NOT ALLOWED     *.
       F52BT.    IF    PE22-IARLNA = 'N'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52BT-FN.
      * -CHECK CI0021 TO SEE IF 'LOANS'
      *   ARE EVEN ALLOWED.  CERTS ONLY
      *   ALLOW SAME FROM/TO CTIDS IF A
      *   LOAN PYMT IS BEING MADE.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012990 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52BT-900. GO TO F52CC-FN.
       F52BT-FN. EXIT.
      *N52CC.    NOTE *ELSE -                             *.
       F52CC.         EXIT.                                             lv20
      *N52CF.    NOTE *IF CI0021 IND ALLOWED AND          *.
       F52CF.    IF    PE22-IARLNA = 'Y'                                lv25
                 AND   PE22-CELBL = 0
                 NEXT SENTENCE ELSE GO TO     F52CF-FN.
      * LOAN AMT EQUAL TO ZERO
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012990 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52CF-900. GO TO F52CH-FN.
       F52CF-FN. EXIT.
      *N52CH.    NOTE *ELSE -                             *.
       F52CH.         EXIT.                                             lv25
      *N52CJ.    NOTE *IF CI0021 IND = 'Y' AND            *.
       F52CJ.    IF    PE22-IARLNA = 'Y'                                lv30
                 AND   PE22-CELBL NOT = ZERO
                 NEXT SENTENCE ELSE GO TO     F52CJ-FN.
      *   LOAN AMT NOT EQUAL TO ZERO
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        013155 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F52CJ-FN. EXIT.
       F52CH-FN. EXIT.
       F52CC-FN. EXIT.
       F52BR-FN. EXIT.
       F52BN-FN. EXIT.
      *N52DC.    NOTE *IF 'TO' ACCT IS A FUND             *.
       F52DC.    IF    TO01-CTIDA = 002                                 lv15
                 NEXT SENTENCE ELSE GO TO     F52DC-FN.
      * - TO/FROM MATCH AND ARE 'FUNDS'
      *   THIS IS NOT ALLOWED - ERROR
      *   OUT AND RETURN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012989 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52DC-FN. EXIT.
       F52BL-FN. EXIT.
      *N52GA.    NOTE *EDIT FOR SINGLE PAY 'TO' ACCOUNT   *.
       F52GA.    IF    TO01-CTIDA = 001 OR 004                          lv10
                 OR    TO01-CTIDA = 005
                 NEXT SENTENCE ELSE GO TO     F52GA-FN.
      *(CERTS/LIFE/ANNUITIES)
      *N52GB.    NOTE *NO DISBURSEMENTS TO SINGLE PAY     *.
       F52GB.    IF    TE17-MAPPN = 'SD'                                lv15
                 NEXT SENTENCE ELSE GO TO     F52GB-FN.
      *IN SD
      *N52MC.    NOTE *EVALUATE CI0021 RETURNED FIELDS    *.
       F52MC.                                                           lv20
      *TO SEE IF 'TO' ACCT IS VALID AS
      *AS A DESTINATION.
      *N52ME.    NOTE *IF EITHER REG OR LOAN ALLOWED      *.
       F52ME.    IF    PE22-IARLNA = 'Y'                                lv25
                 OR    PE22-IARRGA = 'Y'
                 NEXT SENTENCE ELSE GO TO     F52ME-FN.
      *-- CONTINUE ON --
      *CAN'T ERROR AT THIS POINT IN THE
      *CATS SD APP; DON'T KNOW PYMT TYP
       F52ME-900. GO TO F52MH-FN.
       F52ME-FN. EXIT.
      *N52MH.    NOTE *ELSE - ERROR                       *.
       F52MH.         EXIT.                                             lv25
      *N52MK.    NOTE *IF SINGLE PAY - ISSUE THAT ERROR   *.
       F52MK.    IF    PE22-IARRGA = 'N'                                lv30
                 NEXT SENTENCE ELSE GO TO     F52MK-FN.
      *- "SINGLE PAY" ERROR
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012992 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52MK-900. GO TO F52MN-FN.
       F52MK-FN. EXIT.
      *N52MN.    NOTE *ELSE JUST ERROR -PYMT NOT ALLOWD   *.
       F52MN.                                                           lv30
      *
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012993 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52MN-FN. EXIT.
       F52MH-FN. EXIT.
       F52MC-FN. EXIT.
       F52GB-FN. EXIT.
      *N52MP.    NOTE *ELSE IF UD AND DISBURSEMENTS       *.
       F52MP.    IF    TE17-MAPPN = 'UD' OR 'FDC'                       lv15
                 NEXT SENTENCE ELSE GO TO     F52MP-FN.
      *TO SINGLE PAY PRODUCTS
      *N52MQ.    NOTE *IF STATUS IS ACTIVE                *.
       F52MQ.    IF    TO01-CTSTA = 02                                  lv20
                 NEXT SENTENCE ELSE GO TO     F52MQ-FN.
      *N52MR.    NOTE *EDIT FOR SINGLE PAY ANNUITY        *.
       F52MR.    IF    TO01-CTIDA = 004 OR 005                          lv25
                 NEXT SENTENCE ELSE GO TO     F52MR-FN.
      *N52MS.    NOTE *IF A LOAN IS ALLOWED BUT           *.
       F52MS.    IF    PE22-IARLNA = 'Y'                                lv30
                 AND   PE22-CELBL = ZERO
                 NEXT SENTENCE ELSE GO TO     F52MS-FN.
      *THERE IS NO LOAN BALANCE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013383 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52MS-FN. EXIT.
      *N52MT.    NOTE *IF A LOAN IS NOT ALLOWED &         *.
       F52MT.    IF    PE22-IARLNA = 'N'                                lv30
                 AND   PE22-IARRGA = 'N'
                 NEXT SENTENCE ELSE GO TO     F52MT-FN.
      *REGULAR PAYMENTS NOT ALLOWED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013383 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52MT-FN. EXIT.
       F52MR-FN. EXIT.
      *N52MU.    NOTE *EDIT FOR SINGLE PAY CERTIFICATES   *.
       F52MU.    IF    TO01-CTIDA = 001                                 lv25
                 AND   PE22-IARRGA = 'N'
                 AND   PE22-INPAY = 'N'
                 NEXT SENTENCE ELSE GO TO     F52MU-FN.
      *CALL CI0135 TO GET GRACE PERIOD
           PERFORM     F96PA THRU F96PA-FN.
      *N52MW.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F52MW.    IF    (MS03-NMESS2 > ZERO                              lv30
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F52MW-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0135 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0135 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F52MW-900. GO TO F52MY-FN.
       F52MW-FN. EXIT.
      *N52MY.    NOTE *NO ERRORS                          *.            ADU071
       F52MY.                                                           lv30
           INITIALIZE  MS03.                                            ADU071
       F52MY-FN. EXIT.
      *N52M1.    NOTE *MAKE SURE ACCT IN GRACE PERIOD     *.
       F52M1.         EXIT.                                             lv30
      *N52M3.    NOTE *FLEX SAVINGS/PREF INVESTORS CERT   *.
       F52M3.    IF    PJ02-CPRDA1 =                                    lv35
                       101
                 NEXT SENTENCE ELSE GO TO     F52M3-FN.
      *TE17-DEFFT IS TXN EFFECTIVE DATE.                                DOT
      *AND SHOULD NOT BE LESS THAN THE.                                 DOT
      *CURRENT ACCOUNTING DATE.                                         DOT
      *PJ02-CELBDT IS THE CURRENT TERM.                                 DOT
      *START DATE.                                                      DOT
      *PJ02-DTRME1 IS THE CURRENT TERM.                                 DOT
      *START DATE + 15 DAYS.                                            DOT
      *THE GRACE PERIOD IS INCLUSIVE.                                   DOT
      *OF THESE TWO DATES.                                              DOT
      *NOT IN GRACE PERIOD - SET ERROR                                  DOT
                 IF    TE17-DEFFT < PJ02-CELBDT                         DOT
                 OR    TE17-DEFFT > PJ02-DTRME1
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013679 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52M3-900. GO TO F52M1-FN.
       F52M3-FN. EXIT.
      *N52M5.    NOTE *STOCK MARKET CERTIFICATE           *.
       F52M5.    IF    PJ02-CPRDA1 =                                    lv35
                       105
                 NEXT SENTENCE ELSE GO TO     F52M5-FN.
      *TE17-DEFFT IS TXN EFFECTIVE DATE.                                DOT
      *AND SHOULD NOT BE LESS THAN THE.                                 DOT
      *CURRENT ACCOUNTING DATE.                                         DOT
      *PJ02-CELBDT IS THE CURRENT TERM.                                 DOT
      *START DATE.                                                      DOT
      *PJ02-DTRME1 IS THE PREVIOUS TERM.                                DOT
      *END DATE.                                                        DOT
      *THE GRACE PERIOD IS EXCLUSIVE.                                   DOT
      *OF THESE TWO DATES.                                              DOT
      *IN GRACE CONTINUE - ELSE ERROR                                   DOT
                 IF    TE17-DEFFT < PJ02-CELBDT                         DOT
                 AND   TE17-DEFFT > PJ02-DTRME1
           CONTINUE
                 ELSE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013679 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52M5-900. GO TO F52M1-FN.
       F52M5-FN. EXIT.
       F52M1-FN. EXIT.
       F52MU-FN. EXIT.
       F52MQ-900. GO TO F52NC-FN.
       F52MQ-FN. EXIT.
      *N52NC.    NOTE *IF STATUS IS NOT ACTIVE            *.
       F52NC.         EXIT.                                             lv20
      *N52NG.    NOTE *IF STATUS IS PENDING               *.
       F52NG.    IF    TO01-CTSTA = 01                                  lv25
                 NEXT SENTENCE ELSE GO TO     F52NG-FN.
      * - DISBURSEMENT ALLOWED FOR UD
       F52NG-900. GO TO F52NI-FN.
       F52NG-FN. EXIT.
      *N52NI.    NOTE *DISBURSEMENT NOT ALLOWED FOR UD    *.
       F52NI.                                                           lv25
      *WHEN STATUS IS - INACTIVE
      *               - SPECIAL
      *               - UNKNOWN
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013382 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52NI-FN. EXIT.
       F52NC-FN. EXIT.
       F52MP-FN. EXIT.
       F52GA-FN. EXIT.
      *N52PA.    NOTE *EDIT FOR ED IRA 'TO' ACCOUNT       *.
       F52PA.    IF    TO01-CIRAT = 007                                 lv10
                 NEXT SENTENCE ELSE GO TO     F52PA-FN.
      *N52PD.    NOTE *IF 'FROM' ACCOUNT IS UGMA-UTMA     *.
       F52PD.    IF    FA04-IUGMA = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F52PD-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013231 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52PD-FN. EXIT.
       F52PA-FN. EXIT.
      *N54.      NOTE *************************************.
      *               *                                   *
      *               *QUALIFIED/NON-QUALIFED ACCT EDIT   *
      *               *                                   *
      *               *************************************.
       F54.                                                             lv05
      *
      *********************************
      ** QUALIFIED/NON-QUALIFIED EDITS*
      *********************************
      *
      *N54BA.    NOTE *'FROM' ACCOUNT GROUP TYPE          *.
       F54BA.                                                           lv10
      *
      *********************************
      ** CASE OF STRUCTURE TO CHECK   *
      ** WHAT TYPE OF GROUP THE 'FROM'*
      ** ACCOUNT NUMBER IS IN.        *
      *********************************
      *
      *N54CA.    NOTE *'FROM' ACCT IN HOUSEHOLD GROUP     *.
       F54CA.    IF    FG15-GRIDCB (1) =                                lv15
                       001
                 NEXT SENTENCE ELSE GO TO     F54CA-FN.
      *N54DA.    NOTE *'FROM' ACCOUNT IS NOT QUALIFIED    *.
       F54DA.    IF    FR01-CQACT < ZERO                                lv20
                 OR    FR01-CQACT = ZERO
                 NEXT SENTENCE ELSE GO TO     F54DA-FN.
      *N54DC.    NOTE *IF SD                              *.
       F54DC.    IF    TE17-MAPPN = 'SD'                                lv25
                 NEXT SENTENCE ELSE GO TO     F54DC-FN.
      *N54DD.    NOTE *IF DIV/INT SD TO QUAL HSHLD ACCT   *.
       F54DD.    IF    (TE17-CARTZ = 05 OR 06)                          lv30
                 AND   TG15-GRIDCB (1) = 001
                 AND   TO01-CQACT > ZEROES
                 NEXT SENTENCE ELSE GO TO     F54DD-FN.
      *********************************
      ** 'FROM' ACCT IS NON-QUALIFIED *
      ** -CAN'T SEND DIV/INT SDS TO   *
      **  QUALIFIED HOUSEHOLD ACCOUNTS*
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012997 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54DD-FN. EXIT.
      *N54DM.    NOTE *IF DIV/INT SD TO QUAL PENSN ACCT   *.
       F54DM.    IF    (TE17-CARTZ = 05 OR 06)                          lv30
                 AND   TG15-GRIDCB (1) = 002
                 AND   TG15-GRPLT (1) > ZERO
                 NEXT SENTENCE ELSE GO TO     F54DM-FN.
      *********************************
      ** 'FROM' ACCT IS NON-QUALIFIED *
      ** -CAN'T SEND DIV/INT SDS TO   *
      **  QUALIFIED PENSION ACCOUNTS. *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012998 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54DM-FN. EXIT.
       F54DC-FN. EXIT.
       F54DA-FN. EXIT.
      *N54EA.    NOTE *'FROM' ACCOUNT IS QUALIFIED        *.
       F54EA.    IF    FR01-CQACT > ZEROS                               lv20
                 NEXT SENTENCE ELSE GO TO     F54EA-FN.
      *
      *********************************
      ** 'FROM' ACCOUNT IS A QUALIFIED*
      ** ACCOUNT.                     *
      *********************************
      *
      *N54EM.    NOTE *'FROM' ACCOUNT IS CERTS/FUNDS      *.
       F54EM.    IF    (FR01-CTIDA = 001 OR 002 OR                      lv25
                       021 OR 133)
                 AND   FR01-CTCCI = '1'
                 NEXT SENTENCE ELSE GO TO     F54EM-FN.
      *OR BROKERAGE OR BETA BROKERAGE
      *********************************
      ** 'FROM' ACCOUNT IS A CERTS OR *
      ** FUNDS OR BROK OR BETA BROK*
      ** AND CUSTODIAN ACCOUNT*
      *********************************
      *N54FA.    NOTE *HOUSEHOLD 'TO' ACCOUNT             *.
       F54FA.    IF    TG15-GRIDCB (1) = 001                            lv30
                 AND   TO01-CTCCI NOT = '1'
                 NEXT SENTENCE ELSE GO TO     F54FA-FN.
      *********************************
      ** 'TO' ACCOUNT NUMBER IS IN    *
      ** HOUSEHOLD GROUP AND IS NOT A *
      ** CUSTODIAN ACCOUNT, THEN GO TO*
      ** PLAN TYPE EDITS.             *
      *********************************
      *
               GO TO     F54-FN.
       F54FA-FN. EXIT.
      *N54GA.    NOTE *PENSION 'TO' ACCOUNT               *.
       F54GA.    IF    TG15-GRIDCB (1) = 002                            lv30
                 AND   TG15-CIDRP (1) NOT = 01
                 NEXT SENTENCE ELSE GO TO     F54GA-FN.
      *********************************
      ** 'TO' ACCOUNT NUMBER IS IN    *
      ** PENSION GROUP AND IS  NOT A  *
      ** CUSTODIAN ACCOUNT, THEN GO TO*
      ** PLAN TYPE EDITS.             *
      *********************************
      *
               GO TO     F54-FN.
       F54GA-FN. EXIT.
       F54EM-FN. EXIT.
      *N54HA.    NOTE *'FROM' ACCOUNT IS LIFE ACCOUNT     *.
       F54HA.    IF    FR01-CTIDA = 004 OR 005                          lv25
                 NEXT SENTENCE ELSE GO TO     F54HA-FN.
      *N54IA.    NOTE *HOUSEHOLD 'TO' ACCOUNT             *.
       F54IA.    IF    TG15-GRIDCB (1) = 001                            lv30
                 AND   TO01-CTCCI NOT = '1'
                 NEXT SENTENCE ELSE GO TO     F54IA-FN.
      *********************************
      ** 'TO' ACCOUNT NUMBER IS IN    *
      ** HOUSEHOLD GROUP AND IS NOT A *
      ** CUSTODIAN ACCOUNT, THEN GO TO*
      ** PLAN TYPE EDITS.             *
      *********************************
      *
               GO TO     F54-FN.
       F54IA-FN. EXIT.
      *N54JA.    NOTE *PENSION 'TO' ACCOUNT               *.
       F54JA.    IF    TG15-GRIDCB (1) = 002                            lv30
                 AND   TG15-CIDRP (1) NOT = 01
                 NEXT SENTENCE ELSE GO TO     F54JA-FN.
      *********************************
      ** 'TO' ACCOUNT NUMBER IS IN    *
      ** PENSION GROUP AND IS  NOT A  *
      ** CUSTODIAN ACCOUNT, THEN GO TO*
      ** PLAN TYPE EDITS.             *
      *********************************
      *
               GO TO     F54-FN.
       F54JA-FN. EXIT.
       F54HA-FN. EXIT.
      *N54KA.    NOTE *CHECK IF NOT SAME QUAL ACCT TYPE   *.
       F54KA.    IF    FR01-CQACT NOT = TO01-CQACT                      lv25
                 NEXT SENTENCE ELSE GO TO     F54KA-FN.
      **
      *********************************
      ** IF THE 'FROM' AND 'TO' ACCT  *
      ** NUMBERS DO NOT HAVE THE SAME *
      ** QUALIFIED ACCOUNT TYPE, GET  *
      ** ERROR MESSAGE TEXT AND RETURN*
      ** TO CALLING MODULE.           *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012999 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54KA-FN. EXIT.
      *N54KD.    NOTE *IF SD                              *.
       F54KD.    IF    TE17-MAPPN = 'SD'                                lv25
                 NEXT SENTENCE ELSE GO TO     F54KD-FN.
      *N54KF.    NOTE *IF DIV/INT FROM A NON-CUST ACCT    *.
       F54KF.    IF    (TE17-CARTZ = 05 OR 06)                          lv30
                 AND   FR01-CTCCI NOT = '1'
                 NEXT SENTENCE ELSE GO TO     F54KF-FN.
      *********************************
      ** IF THE 'FROM' ACCT IS NON-   *
      ** CUSTODIAL & TRYING TO DO A   *
      ** DIVIDEND OR INTEREST SD.     *
      *********************************
      *N54KH.    NOTE *IF 'TO' ACCT CUSTODIAL HOUSEHOLD   *.
       F54KH.    IF    TG15-GRIDCB (1) = 001                            lv35
                 AND   TO01-CTCCI = '1'
                 NEXT SENTENCE ELSE GO TO     F54KH-FN.
      *********************************
      ** IF THE 'TO' ACCT IS CUSTODIAL*
      ** HOUSEHOLD -  ERROR OUT       *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013000 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54KH-FN. EXIT.
       F54KF-FN. EXIT.
      *N54KM.    NOTE *IF 'TO' ACCT IRA & TINS NOT EQUA   *.
       F54KM.    IF    (TE17-CARTZ = 05 OR 06)                          lv30
                 AND   TO01-CQACT = 001
                 AND   FC14-CLID01 NOT =
                       TC14-CLID01
                 NEXT SENTENCE ELSE GO TO     F54KM-FN.
      *********************************
      ** CAN'T GO INTO AN IRA FROM OUT*
      ** SIDE THE PLAN - TAX PAYERS'  *
      ** IDS MUST EQUAL. (DIV OR INT) *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013001 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54KM-FN. EXIT.
       F54KD-FN. EXIT.
       F54EA-FN. EXIT.
       F54CA-900. GO TO F54BA-FN.
       F54CA-FN. EXIT.
      *N54MA.    NOTE *'FROM' ACCOUNT IN PENSION GROUP    *.
       F54MA.    IF    FG15-GRIDCB (1) =                                lv15
                       002
                 NEXT SENTENCE ELSE GO TO     F54MA-FN.
      *N54NA.    NOTE *'FROM' ACCOUNT IS NON-QUALIFIED    *.
       F54NA.    IF    FG15-GRPLT (1) < ZEROS                           lv20
                 OR    FG15-GRPLT (1) = ZERO
                 NEXT SENTENCE ELSE GO TO     F54NA-FN.
      *N54NC.    NOTE *IF SD                              *.
       F54NC.    IF    TE17-MAPPN = 'SD'                                lv25
                 NEXT SENTENCE ELSE GO TO     F54NC-FN.
      *N54NF.    NOTE *IF DIV/INT TO QUALIFIED HSHLD      *.
       F54NF.    IF    (TE17-CARTZ = 05 OR 06)                          lv30
                 AND   TG15-GRIDCB (1) = 001
                 AND   TO01-CQACT > ZERO
                 NEXT SENTENCE ELSE GO TO     F54NF-FN.
      *********************************
      ** CANNOT SEND DIV/INT FROM NON-*
      ** QUALIFIED PENSION TO QUALIFIE*
      ** D HOUSEHOLD ACCOUNT.         *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54NF-FN. EXIT.
      *N54NM.    NOTE *IF DIV/INT TO QUALIFIED PENSION    *.
       F54NM.    IF    (TE17-CARTZ = 05 OR 06)                          lv30
                 AND   TG15-GRIDCB (1) = 002
                 AND   TG15-GRPLT (1) > ZERO
                 NEXT SENTENCE ELSE GO TO     F54NM-FN.
      *********************************
      ** CANNOT SEND DIV/INT FROM NON-*
      ** QUALIFIED PENSION TO QUALIFIE*
      ** D PENSION ACCOUNT.           *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013005 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54NM-FN. EXIT.
       F54NC-FN. EXIT.
       F54NA-FN. EXIT.
      *N54OA.    NOTE *'FROM' ACCOUNT IS QUALIFIED        *.
       F54OA.    IF    FG15-GRPLT (1) > ZEROS                           lv20
                 NEXT SENTENCE ELSE GO TO     F54OA-FN.
      *N54OM.    NOTE *'FROM' ACCT IS CERTS/FUNDS/LIFE    *.
       F54OM.    IF    ((FR01-CTIDA = 001 OR 002                        lv25
                       OR 004 OR 005)
                 AND   TE17-MAPPN = 'SD')
                 OR    (FR01-CTIDA = 002
                 AND   TE17-MAPPN = 'UD')
                 OR    (TE17-MAPPN = 'FDC'
                 AND   (FR01-CTIDA = 001
                 OR    FR01-CTIDA = 002))
                 NEXT SENTENCE ELSE GO TO     F54OM-FN.
      *********************************
      ** 'FROM' ACCOUNT IS A CERTS,   *
      ** FUNDS OR LIFE ACCOUNT.       *
      *********************************
      *
      *N54PA.    NOTE *HOUSEHOLD 'TO' ACCOUNT             *.
       F54PA.    IF    TG15-GRIDCB (1) = 001                            lv30
                 AND   TO01-CTCCI NOT = '1'
                 AND   FG15-CIDRP (1) = 01
                 NEXT SENTENCE ELSE GO TO     F54PA-FN.
      *********************************
      ** 'TO' ACCOUNT NUMBER IS IN    *
      ** HOUSEHOLD GROUP AND IS NOT A *
      ** CUSTODIAN ACCOUNT AND 'FROM' *
      ** ACCOUNT IS CUSTODIAN ACCOUNT,*
      ** THEN GO TO PLAN TYPE EDITS.  *
      *********************************
      *
               GO TO     F54-FN.
       F54PA-FN. EXIT.
      *N54PM.    NOTE *IF DIV/INT FROM NON-CUSTODIAL      *.
       F54PM.    IF    (TE17-CARTZ = 05 OR 06)                          lv30
                 AND   FG15-CIDRP (1) NOT = 01
                 AND   TG15-GRIDCB (1) = 001
                 AND   TO01-CTCCI = '1'
                 AND   TE17-MAPPN = 'SD'
                 NEXT SENTENCE ELSE GO TO     F54PM-FN.
      *********************************
      ** IF DIV/INT FROM NON-CUSTODIAL*
      ** TO A HOUSEHOLD CUSTODIAL -   *
      ** ERROR OUT                    *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013006 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54PM-FN. EXIT.
      *N54QA.    NOTE *PENSION 'TO' ACCOUNT               *.
       F54QA.    IF    TG15-GRIDCB (1) = 002                            lv30
                 AND   TG15-CIDRP (1) NOT = 01
                 AND   FG15-CIDRP (1) = 01
                 NEXT SENTENCE ELSE GO TO     F54QA-FN.
      *********************************
      ** 'TO' ACCOUNT NUMBER IS IN    *
      ** PENSION GROUP AND IS NOT A   *
      ** CUSTODIAN ACCOUNT AND 'FROM' *
      ** ACCOUNT IS CUSTODIAN ACCOUNT,*
      ** THEN GO TO PLAN TYPE EDITS.  *
      *********************************
      *
               GO TO     F54-FN.
       F54QA-FN. EXIT.
       F54OM-FN. EXIT.
      *N54RA.    NOTE *CHECK IF NOT SAME PLAN             *.
       F54RA.    IF    TG15-GRIDCB (1) = 002                            lv25
                 AND   (FG15-GRPLT (1) NOT =
                       TG15-GRPLT (1)
                 OR    FG15-GRPLC (1) NOT =
                       TG15-GRPLC (1))
                 NEXT SENTENCE ELSE GO TO     F54RA-FN.
      *AND TO ACCOUNT IS PENSION
      *********************************
      ** IF THE 'FROM' AND 'TO' ACCT  *
      ** NUMBERS DO NOT HAVE THE SAME *
      ** PLAN TYPE, GET ERROR MESSAGE *
      ** TEXT AND RETURN TO THE       *
      ** CALLING MODULE.              *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013007 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54RA-FN. EXIT.
      *N54SA.    NOTE *CHECK IF DIFFERENT TAXPAYERS       *.
       F54SA.    IF    FC14-CLID01 NOT =                                lv25
                       TC14-CLID01
                 NEXT SENTENCE ELSE GO TO     F54SA-FN.
      *********************************
      ** IF THE 'FROM' AND 'TO' ACCT  *
      ** NUMBERS HAVE DIFFERENT       *
      ** TAXPAYERS, GET ERROR MESSAGE *
      ** TEXT AND RETURN TO THE       *
      ** CALLING MODULE.              *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013008 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54SA-FN. EXIT.
       F54OA-FN. EXIT.
       F54MA-900. GO TO F54BA-FN.
       F54MA-FN. EXIT.
      *N54TA.    NOTE *'FROM' ACCOUNT IS NEITHER          *.
       F54TA.                                                           lv15
      *
      *********************************
      ** 'FROM' ACCOUNT IS NEITHER    *
      ** PENSION OR HOUSEHOLD GROUP   *
      *********************************
      *
      *N54UA.    NOTE *CHECK IF 'TO' ACCOUNT HOUSEHOLD    *.
       F54UA.    IF    TG15-GRIDCB (1) = 001                            lv20
                 AND   TO01-CQACT > ZEROS
                 NEXT SENTENCE ELSE GO TO     F54UA-FN.
      *********************************
      ** IF THE 'TO' ACCOUNT IS IN    *
      ** HOUSEHOLD AND A QUALIFIED    *
      ** ACCOUNT,   GET ERROR MESSAGE *
      ** TEXT AND RETURN TO THE       *
      ** CALLING MODULE.              *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013009 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54UA-FN. EXIT.
      *N54UM.    NOTE *CHECK IF 'TO' ACCOUNT PENSION      *.
       F54UM.    IF    TG15-GRIDCB (1) = 002                            lv20
                 AND   TG15-GRPLT (1) > ZEROS
                 NEXT SENTENCE ELSE GO TO     F54UM-FN.
      *********************************
      ** IF THE 'TO' ACCOUNT IS IN    *
      ** PENSION   AND A QUALIFIED    *
      ** ACCOUNT,   GET ERROR MESSAGE *
      ** TEXT AND RETURN TO THE       *
      ** CALLING MODULE.              *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013010 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54UM-FN. EXIT.
       F54TA-FN. EXIT.
       F54BA-FN. EXIT.
      *N54VA.    NOTE *SRA ACCTS MUST BE IN SAME GROUP    *.
       F54VA.    IF    FR01-CTIDA NOT = 004                             lv10
                 AND   FR01-CTIDA NOT = 005
                 AND   FR01-CIRAT = 004
                 AND   TO01-CIRAT = 004
                 NEXT SENTENCE ELSE GO TO     F54VA-FN.
      ** SKIP TSC EDITS FOR LIFE ACCTS
      *********************************
      ** IF THE 'FROM' ACCOUNT AND THE*
      ** 'TO' ACCOUNT ARE SRA ACCOUNTS*
      ** THEN THEY MUST BE IN THE SAME*
      ** GROUP.  ELSE SEND MESSAGE AND*
      ** RETURN TO CALLING MODULE.    *
      *********************************
      *
           PERFORM     F91BC THRU F91BC-FN.
      *N54WA.    NOTE **                                  *.
       F54WA.    IF    W-FSRA-GRID = ZERO                               lv15
                 OR    W-TSRA-GRID = ZERO
                 OR    (W-FSRA-GRID NOT =
                       W-TSRA-GRID)
                 NEXT SENTENCE ELSE GO TO     F54WA-FN.
      *********************************
      ** IF THE 'FROM' ACCOUNT AND THE*
      ** 'TO' ACCOUNT ARE SRA ACCOUNTS*
      ** THEN THEY MUST BE IN THE SAME*
      ** GROUP.  ELSE SEND MESSAGE AND*
      ** RETURN TO CALLING MODULE.    *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013003 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F54WA-FN. EXIT.
       F54VA-FN. EXIT.
       F54-FN.   EXIT.
      *N56.      NOTE *************************************.
      *               *                                   *
      *               *PLAN TYPE EDITS                    *
      *               *                                   *
      *               *************************************.
       F56.      IF    FR01-CTIDA NOT = 004 AND 005                     lv05
                 NEXT SENTENCE ELSE GO TO     F56-FN.
      *N56AB.    NOTE *SKIP THE PLAN TYPE EDITS FOR       *.
       F56AB.    IF    EIBTRNID (1:4) = ('XE86'                         lv08
                       OR 'XE87' OR 'XF43'
                       OR 'XF45')
                 NEXT SENTENCE ELSE GO TO     F56AB-FN.
      *ONE-TIME AND SPO MMTA
      *TRANSACTIONS IN CLIENT VIEWER
               GO TO     F56-FN.
       F56AB-FN. EXIT.
      *N56BA.    NOTE *PROCESS MACRO AMPLAN ONLY FOR      *.
       F56BA.    IF    TE17-MAPPN = 'SD'                                lv08
                 NEXT SENTENCE ELSE GO TO     F56BA-FN.
      *SD - THIS MACRO FOLLOWS KA60
      * --> PROCESSING IN KA60 TO
      *DETERMINE THE PLAN TYPE IS
      *LITTLE DIFFERENT FROM KD70
      *N56BB.    NOTE *DETERMINE 'FROM' ACCT PLAN TYPE    *.
       F56BB.         EXIT.                                             lv10
      *N56CA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            AMPLAN
       F56CA.                                                           lv15
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** INITIALIZE THE ACCOUNT PLAN  *                                 AMPLAN
      ** TYPE DESCRIPTION TO SPACES   *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        SPACES TO W-FR01-MPLAN.                          AMPLAN
       F56CA-FN. EXIT.
      *N56DA.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            AMPLAN
       F56DA.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 001                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F56DA-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** HOUSEHOLD GROUP, THEN THE    *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-FR01-MPLAN                       AMPLAN
      *
                 IF    FR01-CTCCI NOT = '1'                             DOT
                 AND   7-FROM-ICODE NOT = '5'
                 AND   7-FROM-CVALB NOT = 'IRA'
                 AND   NOT = 'SEP'
      *IF IDS IS NOT THE CUSTODIAN SET
      *PLAN TYPE TO PERSONAL & GET OUT
           MOVE        'PERSONAL' TO W-FR01-MPLAN
               GO TO     F56DA-FN.
      *N56EA.    NOTE *CHECK IF IDS IS CUSTODIAN          *.            AMPLAN
       F56EA.    IF    FR01-CTCCI = '1'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F56EA-FN.                 AMPLAN
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS CUSTODIAN  *                                 AMPLAN
      ** ACCOUNT, CHECK QUALIFIED     *                                 AMPLAN
      ** ACCOUNT CODE AND IRA STATUS  *                                 AMPLAN
      ** CODE.                        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
           MOVE        'IRA ACTIVE' TO W-FR01-MPLAN.                    AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 002                                 AMPLAN
           MOVE        'IRA ROLLOVER' TO W-FR01-MPLAN.                  AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 003                                 AMPLAN
                 AND   (FR01-CIRAT NOT = 005                            AMPLAN
                 AND   FR01-CIRAT NOT = 006)                            AMPLAN
           MOVE        'BENEFICIAL' TO W-FR01-MPLAN.                    AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CIRAT = 004                                 AMPLAN
           MOVE        'SRA' TO W-FR01-MPLAN.                           AMPLAN
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH IRA   ' TO W-FR01-MPLAN.                   AMPLAN
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 003                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH BENF  ' TO W-FR01-MPLAN.                   AMPLAN
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONV  ' TO W-FR01-MPLAN.                   AMPLAN
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 003                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONVBEN' TO W-FR01-MPLAN.                  AMPLAN
                 IF    FR01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-FR01-MPLAN.                           AMPLAN
                 IF    FR01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-FR01-MPLAN.                          AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CIRAT = 007                                 AMPLAN
           MOVE        'EDUC IRA' TO W-FR01-MPLAN.                      AMPLAN
       F56EA-FN. EXIT.
       F56DA-FN. EXIT.
      *N56FA.    NOTE *CHECK IF PENSION GROUP             *.            AMPLAN
       F56FA.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 002                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F56FA-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** PENSION GROUP, THEN THE      *                                 AMPLAN
      ** PLAN TYPE IS PENSION.        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PENSION' TO W-FR01-MPLAN.                       AMPLAN
      *N56GA.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            AMPLAN
       F56GA.    IF    FG15-QITEM > ZEROS                               lv20
                 AND   FG15-CIDRP (1) NOT = 01                          AMPLAN
                 NEXT SENTENCE ELSE GO TO     F56GA-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS NOT A      *                                 AMPLAN
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-FR01-MPLAN.                      AMPLAN
       F56GA-FN. EXIT.
       F56FA-FN. EXIT.
       F56BB-FN. EXIT.
      *N56HA.    NOTE *DETERMINE 'TO' ACCT PLAN TYPE      *.
       F56HA.         EXIT.                                             lv10
      *N56IA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            AMPLAN
       F56IA.                                                           lv15
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** INITIALIZE THE ACCOUNT PLAN  *                                 AMPLAN
      ** TYPE DESCRIPTION TO SPACES   *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        SPACES TO W-TO01-MPLAN                           AMPLAN
      *
           MOVE        TO01-PRSCD TO 7-TO-PRSCD
           MOVE        SPACES TO 7-TO-CVALB.
                 IF    (TO01-CTIDA = 004 OR 005)                        DOT
                 AND   (TO01-PRCOD = 00950
                 OR    = 00951
                 OR    = 00952
                 OR    = 00953
                 OR    = 00954
                 OR    = 00956)
                 AND   TO01-CQACT = 001
                 AND   TO01-CIRAT = 001
           MOVE        'IRA' TO 7-TO-CVALB.
                 IF    (TO01-CTIDA = 004 OR 005)                        DOT
                 AND   (TO01-PRCOD = 00950
                 OR    = 00951
                 OR    = 00952
                 OR    = 00953
                 OR    = 00954
                 OR    = 00956)
                 AND   TO01-CQACT = 001
                 AND   TO01-CIRAT = 003
           MOVE        'SEP' TO 7-TO-CVALB.
       F56IA-FN. EXIT.
      *N56JA.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            AMPLAN
       F56JA.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 001                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F56JA-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** HOUSEHOLD GROUP, THEN THE    *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-TO01-MPLAN                       AMPLAN
      *
                 IF    TO01-CTCCI NOT = '1'                             DOT
                 AND   7-TO-ICODE NOT = '5'
                 AND   7-TO-CVALB NOT = 'IRA'
                 AND   NOT = 'SEP'
      *IF IDS IS NOT THE CUSTODIAN SET
      *PLAN TYPE TO PERSONAL & GET OUT
           MOVE        'PERSONAL' TO W-TO01-MPLAN
               GO TO     F56JA-FN.
      *N56KA.    NOTE *CHECK IF IDS IS CUSTODIAN          *.            AMPLAN
       F56KA.    IF    TO01-CTCCI = '1'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F56KA-FN.                 AMPLAN
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS CUSTODIAN  *                                 AMPLAN
      ** ACCOUNT, CHECK QUALIFIED     *                                 AMPLAN
      ** ACCOUNT CODE AND IRA STATUS  *                                 AMPLAN
      ** CODE.                        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
           MOVE        'IRA ACTIVE' TO W-TO01-MPLAN.                    AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 002                                 AMPLAN
           MOVE        'IRA ROLLOVER' TO W-TO01-MPLAN.                  AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 003                                 AMPLAN
                 AND   (TO01-CIRAT NOT = 005                            AMPLAN
                 AND   TO01-CIRAT NOT = 006)                            AMPLAN
           MOVE        'BENEFICIAL' TO W-TO01-MPLAN.                    AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CIRAT = 004                                 AMPLAN
           MOVE        'SRA' TO W-TO01-MPLAN.                           AMPLAN
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH IRA   ' TO W-TO01-MPLAN.                   AMPLAN
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 003                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH BENF  ' TO W-TO01-MPLAN.                   AMPLAN
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONV  ' TO W-TO01-MPLAN.                   AMPLAN
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 003                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONVBEN' TO W-TO01-MPLAN.                  AMPLAN
                 IF    TO01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-TO01-MPLAN.                           AMPLAN
                 IF    TO01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-TO01-MPLAN.                          AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CIRAT = 007                                 AMPLAN
           MOVE        'EDUC IRA' TO W-TO01-MPLAN.                      AMPLAN
       F56KA-FN. EXIT.
       F56JA-FN. EXIT.
      *N56LA.    NOTE *CHECK IF PENSION GROUP             *.            AMPLAN
       F56LA.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 002                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F56LA-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** PENSION GROUP, THEN THE      *                                 AMPLAN
      ** PLAN TYPE IS PENSION.        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PENSION' TO W-TO01-MPLAN.                       AMPLAN
      *N56MA.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            AMPLAN
       F56MA.    IF    TG15-QITEM > ZEROS                               lv20
                 AND   TG15-CIDRP (1) NOT = 01                          AMPLAN
                 NEXT SENTENCE ELSE GO TO     F56MA-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS NOT A      *                                 AMPLAN
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-TO01-MPLAN.                      AMPLAN
       F56MA-FN. EXIT.
       F56LA-FN. EXIT.
       F56HA-FN. EXIT.
       F56BA-FN. EXIT.
      *N56MD.    NOTE *PROCESS MACRO APLANU FOR UD        *.
       F56MD.    IF    TE17-MAPPN = 'UD' OR 'FDC'                       lv08
                 NEXT SENTENCE ELSE GO TO     F56MD-FN.
      *
           PERFORM     F96BB THRU F96BB-FN.
       F56MD-FN. EXIT.
      *N56NA.    NOTE *RANDOM TABLE READ FOR GT06         *.            ADUTAB
       F56NA.                                                           lv08
           MOVE        W-FR01-MPLAN TO GT06-MPLAN
           MOVE        'R1' TO G-GT06-TABFO                             ADUTAB
           COMPUTE     G-GT06-LTH = 60 + G-GT06-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-GT06-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-GT06)                                ADUTAB
                       LENGTH (G-GT06-LTH)                   END-EXEC.  ADUTAB
                 IF    G-GT06-TABCR NOT = '00'                          DOT
               GO TO     F56-FN.
      *N56OA.    NOTE *SELECT CORRECT PLAN CODES          *.
       F56OA.                                                           lv10
      *
      *********************************
      ** SELECT THE CORRECT PLAN      *
      ** CODES DEPENDING UPON THE 'TO'*
      ** ACCOUNT PLAN TYPE.           *
      *********************************
      *
           MOVE        SPACES TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'IRA ACTIVE'                      DOT
           MOVE        GT06-CIRAC TO W-TG06-CODES.
                 IF    W-TO01-MPLAN =                                   DOT
                       'IRA ROLLOVER'
           MOVE        GT06-CIRAR TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'PENSION'                         DOT
           MOVE        GT06-CPENS TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'TSA'                             DOT
           MOVE        GT06-CTSAS TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'PERSONAL'                        DOT
           MOVE        GT06-CPERS TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'TSCA'                            DOT
           MOVE        GT06-CTSCA TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'SRA'                             DOT
           MOVE        GT06-CIRAC1 TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'BENEFICIAL'                      DOT
           MOVE        GT06-CIRAC2 TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'ROTH IRA'                        DOT
           MOVE        GT06-CIRAC3 TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'ROTH BENF'                       DOT
           MOVE        GT06-CIRAC4 TO W-TG06-CODES.
                 IF    W-TO01-MPLAN =                                   DOT
                       'ROTH CONVBEN'
           MOVE        GT06-CIRAC6 TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'ROTH CONV'                       DOT
           MOVE        GT06-CIRAC5 TO W-TG06-CODES.
                 IF    W-TO01-MPLAN = 'EDUC IRA'                        DOT
           MOVE        GT06-CIRAC7 TO W-TG06-CODES.
      *N56PA.    NOTE *CHECK IF TRANSFER INVALID          *.
       F56PA.    IF    (TE17-MAPPN = 'SD'                               lv15
                 AND   W-TG06-ITRAN = 'N')
                 NEXT SENTENCE ELSE GO TO     F56PA-FN.
      *********************************
      ** IF THE 'FROM' AND 'TO' ACCT  *
      ** NUMBERS DO NOT HAVE THE SAME *
      ** PLAN TYPE, GET ERROR MESSAGE *
      ** TEXT AND RETURN TO THE       *
      ** CALLING MODULE.              *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012096 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F56PA-FN. EXIT.
      *N56RC.    NOTE *PERFORM ATG06O MACRO TO CHECK      *.
       F56RC.    IF    TE17-MAPPN = 'UD' OR 'FDC'                       lv15
                 NEXT SENTENCE ELSE GO TO     F56RC-FN.
      *OVERRIDES FOR UD
           PERFORM     F95 THRU F95-FN.
      *N56RG.    NOTE *IF NOT OVERRIDDEN                  *.
       F56RG.    IF    WORK-ITG6O = 'N'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F56RG-FN.
      *
      *N56RH.    NOTE *CHECK TG06 INDICATOR               *.
       F56RH.    IF    W-TG06-ITRAN = 'N'                               lv25
                 NEXT SENTENCE ELSE GO TO     F56RH-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012096 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F56RH-FN. EXIT.
       F56RG-900. GO TO F56SA-FN.
       F56RG-FN. EXIT.
      *N56SA.    NOTE *IF TG06 IS OVERRIDDEN FOR UD       *.
       F56SA.                                                           lv20
      *
      *N56SC.    NOTE *IF TRANSACTION IS NOT ALLOWED      *.
       F56SC.    IF    WORK-CTG6O = 01                                  lv25
                 NEXT SENTENCE ELSE GO TO     F56SC-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013458 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F56SC-FN. EXIT.
      *N56SD.    NOTE *TRANSACTION NOT ALLOWED UNLESS     *.
       F56SD.    IF    WORK-CTG6O = 03                                  lv25
                 NEXT SENTENCE ELSE GO TO     F56SD-FN.
      *WRITTEN
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013454 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F56SD-FN. EXIT.
      *N56SG.    NOTE *TRANSACTIONS NOT ALLOWED UNLESS    *.
       F56SG.    IF    WORK-CTG6O = 04 OR 05                            lv25
                 NEXT SENTENCE ELSE GO TO     F56SG-FN.
      *PROCESSED BY A SERVICE TEAM OR
      *QUALIFIED PLAN
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013455 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F56SG-FN. EXIT.
       F56SA-FN. EXIT.
       F56RC-FN. EXIT.
       F56OA-FN. EXIT.
      *N56SM.    NOTE *RETURN TG06 CODES TO CI9005        *.
       F56SM.                                                           lv10
      *
      *********************************
      ** MOVE CODES TO LINKAGE AREA   *
      *********************************
      *
           MOVE        W-TG06-CODES TO TE17-CIRAC.
       F56SM-FN. EXIT.
       F56NA-FN. EXIT.
       F56-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *FUND ACCOUNT FURTHER VALIDATION    *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60IC.    NOTE *IS 'FROM' ACCT FUNDS?              *.
       F60IC.    IF    FR01-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F60IC-FN.
      *
      *********************************
      ** IF THE 'FROM' ACCOUNT NUMBER *
      ** IS A FUNDS ACCOUNT, READ THE *
      ** SHARK SUB-PRODUCT DATABASE   *
      ** TO GET THE PRODUCT'S         *
      ** INVESTMENT CODE(DETERMINE IF *
      ** A MONEY MARKET ACCOUNT).     *
      *********************************
      *
      *N60IM.    NOTE *SET UP SSA FOR SS01 READ           *.
       F60IM.                                                           lv15
      *
      *********************************
      ** SET UP THE SSA FOR SS01 READ *
      ** WITH THE CT01 VALUES.        *
      *********************************
      *
           MOVE        FR01-CTIDA TO S-SSU01-CTIDA
           MOVE        FR01-PRCOD TO S-SSU01-PRCOD
           MOVE        FR01-PRSCD TO S-SSU01-CPRSCN.
       F60IM-FN. EXIT.
      *N60IR.    NOTE *READ SS01 SEGMENT                  *.
       F60IR.                                                           lv15
      *
      *********************************
      ** READ THE SS01 SEGMENT FOR THE*
      ** 'FROM' ACCOUNT NUMBER.       *
      *********************************
      *
           PERFORM     F94SS THRU F94SS-FN.
       F60IR-FN. EXIT.
      *N60IU.    NOTE *CHECK IF SS01 SEGMENT NOT FOUND    *.
       F60IU.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F60IU-FN.
      *********************************
      ** IF THE SS01 SEGMENT WAS NOT  *
      ** FOUND, GET ERROR MESSAGE     *
      ** TEXT AND RETURN TO CALLING   *
      ** MODULE.                      *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012355 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60IU-900. GO TO F60JF-FN.
       F60IU-FN. EXIT.
      *N60JF.    NOTE *SS01 SEGMENT FOUND - SAVE IT       *.
       F60JF.                                                           lv15
      *
      *********************************
      *SAVE SS01 IN FF01 FOR LATER EDIT
      *********************************
      *
           MOVE        SS01 TO FF01.
       F60JF-FN. EXIT.
       F60IC-FN. EXIT.
      *N60RP.    NOTE *CHECK IF 'TO' ACCT IS FUNDS ACCT   *.
       F60RP.    IF    TO01-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F60RP-FN.
      *
      *********************************
      ** IF THE 'TO' ACCOUNT NUMBER IS*
      ** A FUNDS ACCOUNT, READ THE    *
      ** SHARK SUB-PRODUCT DATABASE   *
      ** TO GET THE PRODUCT'S         *
      ** INVESTMENT CODE(DETERMINE IF *
      ** A MONEY MARKET ACCOUNT).     *
      *********************************
      *
      *N60TA.    NOTE *SET UP SSA FOR SS01 READ           *.
       F60TA.                                                           lv15
      *
      *********************************
      ** SET UP THE SSA FOR SS01 READ *
      ** WITH THE CT01 VALUES.        *
      *********************************
      *
           MOVE        TO01-CTIDA TO S-SSU01-CTIDA
           MOVE        TO01-PRCOD TO S-SSU01-PRCOD
           MOVE        TO01-PRSCD TO S-SSU01-CPRSCN.
       F60TA-FN. EXIT.
      *N60UA.    NOTE *READ SS01 SEGMENT                  *.
       F60UA.                                                           lv15
      *
      *********************************
      ** READ THE SS01 SEGMENT FOR THE*
      ** 'TO' ACCOUNT NUMBER.         *
      *********************************
      *
           PERFORM     F94SS THRU F94SS-FN.
       F60UA-FN. EXIT.
      *N60VA.    NOTE *CHECK IF SS01 SEGMENT NOT FOUND    *.
       F60VA.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F60VA-FN.
      *********************************
      ** IF THE SS01 SEGMENT WAS NOT  *
      ** FOUND, GET ERROR MESSAGE     *
      ** TEXT AND RETURN TO CALLING   *
      ** MODULE.                      *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012084 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60VA-FN. EXIT.
       F60RP-FN. EXIT.
       F60-FN.   EXIT.
      *N75.      NOTE *************************************.
      *               *                                   *
      *               *VALID TRANSFER                     *
      *               *                                   *
      *               *************************************.
       F75.                                                             lv05
      *
      *********************************
      ** THE ACCOUNT NUMBERS ARE VALID*
      *********************************
      *
      *N75BA.    NOTE *SET TRANSFER INDICATOR TO YES      *.
       F75BA.                                                           lv10
      *
      *********************************
      ** SET THE TRANSFER INDICATOR   *
      ** TO YES AND RETURN TO CALLING *
      ** MODULE.                      *
      *********************************
      *
           MOVE        'Y' TO TE17-ISYPO.
       F75BA-FN. EXIT.
       F75-FN.   EXIT.
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
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *SRA TO SRA EDIT SET-UP             *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BC.    NOTE *SET-UP VARIABLES FOR SEARCHES      *.
       F91BC.                                                           lv10
      *********************************
      ** ARRAYS OF GROUPS ARE PASSED  *
      ** INTO THE PGM FOR BOTH THE    *
      ** FROM & TO ACCOUNT.  SEARCH   *
      ** THRU THEM LOOKING FOR THE SRA*
      ** GROUP ID (WHERE GRIDC = 008).*
      *********************************
      **
           MOVE        +1 TO W-FSRA-QITEM
           MOVE        +1 TO W-TSRA-QITEM
           MOVE        ZERO TO W-FSRA-GRID
           MOVE        ZERO TO W-TSRA-GRID.
      *N91DC.    NOTE *LOOP THRU 'FROM' ACCT GROUPS       *.
       F91DC.    IF    W-FSRA-QITEM NOT >                               lv15
                       FG15-QITEM
                 AND   W-FSRA-GRID = ZERO
                 NEXT SENTENCE ELSE GO TO     F91DC-FN.
      *********************************
      ** LOOK FOR SRA GROUP IN ARRAY  *
      ** OF GROUPS FOR THE FROM ACCT. *
      *********************************
      **
           MOVE        FG15-CT10 (W-FSRA-QITEM) TO WS10.
      *N91DF.    NOTE *IF GROUP ID IS SRA - SAVE IT       *.
       F91DF.    IF    WS10-GRIDC = 008                                 lv20
                 NEXT SENTENCE ELSE GO TO     F91DF-FN.
      *********************************
      ** SAVE THE SRA GROUP ID TO TEMP*
      *********************************
      **
           MOVE        WS10-GRID TO W-FSRA-GRID.
       F91DF-900. GO TO F91DJ-FN.
       F91DF-FN. EXIT.
      *N91DJ.    NOTE *ELSE    -   LOOK FOR NEXT GRID     *.
       F91DJ.                                                           lv20
      *********************************
      ** ADD ONE TO SUBSCRIPT         *
      *********************************
      **
           ADD         +1 TO W-FSRA-QITEM.
       F91DJ-FN. EXIT.
       F91DC-900. GO TO F91DC.
       F91DC-FN. EXIT.
      *N91FC.    NOTE *LOOP THRU 'TO' ACCT GROUPS         *.
       F91FC.    IF    W-TSRA-QITEM NOT >                               lv15
                       TG15-QITEM
                 AND   W-TSRA-GRID = ZERO
                 NEXT SENTENCE ELSE GO TO     F91FC-FN.
      *********************************
      ** LOOK FOR SRA GROUP IN ARRAY  *
      ** OF GROUPS FOR THE 'TO' ACCT. *
      *********************************
      **
           MOVE        TG15-CT10 (W-TSRA-QITEM) TO WS10.
      *N91FF.    NOTE *IF GROUP ID IS SRA - SAVE IT       *.
       F91FF.    IF    WS10-GRIDC = 008                                 lv20
                 NEXT SENTENCE ELSE GO TO     F91FF-FN.
      *********************************
      ** SAVE THE SRA GROUP ID TO TEMP*
      *********************************
      **
           MOVE        WS10-GRID TO W-TSRA-GRID.
       F91FF-900. GO TO F91FJ-FN.
       F91FF-FN. EXIT.
      *N91FJ.    NOTE *ELSE    -   LOOK FOR NEXT GRID     *.
       F91FJ.                                                           lv20
      *********************************
      ** ADD ONE TO SUBSCRIPT         *
      *********************************
      **
           ADD         +1 TO W-TSRA-QITEM.
       F91FJ-FN. EXIT.
       F91FC-900. GO TO F91FC.
       F91FC-FN. EXIT.
       F91BC-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PACBASE TABLE ACCESS ROUTINES      *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TB5B       *.
       F92EB.                                                           lv10
           MOVE        '1' TO TB5B-IK.
       F92EB-FN. EXIT.
      *N92TB.    NOTE *RANDOM TABLE READ FOR TB5B         *.            ADUTAB
       F92TB.                                                           lv10
           MOVE        'R1' TO G-TB5B-TABFO                             ADUTAB
           COMPUTE     G-TB5B-LTH = 60 + G-TB5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TB5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TB5B)                                ADUTAB
                       LENGTH (G-TB5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TB5B-TABCR NOT = '00'                          DOT
           PERFORM     F92EB THRU F92EB-FN.                             ADUTAB
       F92TB-FN. EXIT.
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
      *N94SS.    NOTE *CALL GU ON SS01                    *.            ADU026
       F94SS.                                                           lv10
           MOVE        'SSPP' TO DE10-XDBDNM                            ADU026
           MOVE        'SS01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PF06 SS01                                                    ADU026
           S-SSU01-SSA                                                  ADU026
           MOVE        PF06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94SS-FN. EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *OVERRIDE TG06 TRANSFER RULES       *
      *               *                                   *
      *               *************************************.
       F95.                                                             lv05
      *FOR UD ONLY
      *N95MA.    NOTE *OVERRIDE TG06 TRANSFER RULES       *.            ATG06O
       F95MA.                                                           lv10
      *********************************                                 ATG06O
      *MACRO ATG06O CONTAINS CODE THAT                                  ATG06O
      *CAN BE USED TO OVERRIDE RULES                                    ATG06O
      *FOR ACCOUNT TO ACCOUNT TRANSFERS                                 ATG06O
      *STORED IN PACBASE TABLE TG06.                                    ATG06O
      *                                                                 ATG06O
      *THERE ARE THREE TYPES OF                                         ATG06O
      *OVERRIDES:                                                       ATG06O
      *  1. STANDARD OVERRIDES                                          ATG06O
      *  2. WRITTEN REQUEST OVERRIDES                                   ATG06O
      *  3. SERVICE TEAM OVERRIDES                                      ATG06O
      *                                                                 ATG06O
      *STANDARD OVERRIDES ARE                                           ATG06O
      *COMBINATIONS OF SOURCE AND                                       ATG06O
      *DESTINATION ACCOUNT TYPES THAT                                   ATG06O
      *ARE ALLOWED/NOT ALLOWED.                                         ATG06O
      *                                                                 ATG06O
      *WRITTEN REQUEST OVERRIDES ARE                                    ATG06O
      *COMBINATIONS OF SOURCE AND                                       ATG06O
      *DESTINATION ACCOUNT TYPES THAT                                   ATG06O
      *ARE ALLOWED ONLY IF THE REQUEST                                  ATG06O
      *WAS RECEIVED IN WRITING.                                         ATG06O
      *                                                                 ATG06O
      *SERVICE TEAM OVERRIDES ARE                                       ATG06O
      *COMBINATIONS OF SOURCE AND                                       ATG06O
      *DESTINATION ACCOUNT TYPES THAT                                   ATG06O
      *ARE ALLOWED ONLY IF A SERVICE                                    ATG06O
      *TEAM IS PROCESSING THE                                           ATG06O
      *TRANSACTION AND IF THE REQUEST                                   ATG06O
      *WAS RECEIVED IN WRITING.                                         ATG06O
      *N95MC.    NOTE *MACRO INITIALIZATIONS              *.            ATG06O
       F95MC.                                                           lv15
           MOVE        'N' TO WORK-ITG6O                                ATG06O
           MOVE        ZEROS TO WORK-CTG6O                              ATG06O
           MOVE        'N' TO WORK-ISEPR.                               ATG06O
       F95MC-FN. EXIT.
      *N95MD.    NOTE *EDITS ARE GROUPED BY SOURCE        *.            ATG06O
       F95MD.                                                           lv15
      *ACCOUNT TYPE                                                     ATG06O
      *N95ME.    NOTE *SOURCE ACCOUNT IS ACTIVE IRA       *.            ATG06O
       F95ME.    IF    W-FR01-MPLAN =                                   lv20
                       'IRA ACTIVE'                                     ATG06O
                 AND   FR01-CIRAT NOT = 003                             ATG06O
                 NEXT SENTENCE ELSE GO TO     F95ME-FN.                 ATG06O
      *(AND NOT A SEP)                                                  ATG06O
      *N95MF.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95MF.    IF    W-TO01-MPLAN = 'IRA ROLLOVER'                    lv25
                 NEXT SENTENCE ELSE GO TO     F95MF-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95MF-FN. EXIT.
      *N95MG.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95MG.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95MG-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95MG-FN. EXIT.
      *N95MH.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95MH.    IF    W-TO01-MPLAN =                                   lv25
                       'TSCA'                                           ATG06O
                       OR 'SRA'                                         ATG06O
                       OR 'BENEFICIAL'                                  ATG06O
                       OR 'PENSION'                                     ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MH-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95MH-FN. EXIT.
      *N95MI.    NOTE *OVERRIDES DIFF IN KD70/EZTRANS     *.            ATG06O
       F95MI.    IF    W-TO01-MPLAN = 'ROTH IRA'                        lv25
                       OR 'ROTH CONV'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MI-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED AND IT IS PROCESSED A                                   ATG06O
      *SERVICE TEAM MEMBER(IE. ON KD70)                                 ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
                 AND   WORK-ISEPR = 'Y'                                 ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        05 TO WORK-CTG6O.                                ATG06O
       F95MI-FN. EXIT.
       F95ME-900. GO TO F95MD-FN.
       F95ME-FN. EXIT.
      *N95MJ.    NOTE *SOURCE ACCOUNT IS IRA ROLLOVER     *.            ATG06O
       F95MJ.    IF    W-FR01-MPLAN =                                   lv20
                       'IRA ROLLOVER'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MJ-FN.                 ATG06O
      *N95MK.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95MK.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                 NEXT SENTENCE ELSE GO TO     F95MK-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95MK-FN. EXIT.
      *N95ML.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95ML.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                       OR 'PENSION'                                     ATG06O
                       OR 'TSA'                                         ATG06O
                       OR 'TSCA'                                        ATG06O
                 NEXT SENTENCE ELSE GO TO     F95ML-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95ML-FN. EXIT.
      *N95MM.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95MM.    IF    W-TO01-MPLAN = 'BENEFICIAL'                      lv25
                 NEXT SENTENCE ELSE GO TO     F95MM-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95MM-FN. EXIT.
      *N95MN.    NOTE *OVERRIDES DIFF IN KD70/EZTRANS     *.            ATG06O
       F95MN.    IF    W-TO01-MPLAN = 'ROTH IRA'                        lv25
                       OR 'ROTH CONV'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MN-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED AND IT IS PROCESSED A                                   ATG06O
      *SERVICE TEAM MEMBER(IE. ON KD70)                                 ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
                 AND   WORK-ISEPR = 'Y'                                 ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        05 TO WORK-CTG6O.                                ATG06O
       F95MN-FN. EXIT.
       F95MJ-900. GO TO F95MD-FN.
       F95MJ-FN. EXIT.
      *N95MO.    NOTE *SOURCE ACCOUNT IS SEP              *.            ATG06O
       F95MO.    IF    W-FR01-MPLAN =                                   lv20
                       'IRA ACTIVE'                                     ATG06O
                 AND   FR01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MO-FN.                 ATG06O
      *N95MP.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95MP.    IF    W-TO01-MPLAN = 'IRA ROLLOVER'                    lv25
                 NEXT SENTENCE ELSE GO TO     F95MP-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95MP-FN. EXIT.
      *N95MQ.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95MQ.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95MQ-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95MQ-FN. EXIT.
      *N95MR.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95MR.    IF    W-TO01-MPLAN = 'BENEFICIAL'                      lv25
                       OR 'PENSION'                                     ATG06O
                       OR 'TSA'                                         ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MR-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95MR-FN. EXIT.
      *N95MS.    NOTE *OVERRIDES DIFF IN KD70/EZTRANS     *.            ATG06O
       F95MS.    IF    W-TO01-MPLAN = 'ROTH IRA'                        lv25
                       OR 'ROTH CONV'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MS-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED AND IT IS PROCESSED A                                   ATG06O
      *SERVICE TEAM MEMBER(IE. ON KD70)                                 ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
                 AND   WORK-ISEPR = 'Y'                                 ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        05 TO WORK-CTG6O.                                ATG06O
       F95MS-FN. EXIT.
       F95MO-900. GO TO F95MD-FN.
       F95MO-FN. EXIT.
      *N95MT.    NOTE *SOURCE ACCOUNT IS SRA              *.            ATG06O
       F95MT.    IF    W-FR01-MPLAN =                                   lv20
                       'SRA'                                            ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MT-FN.                 ATG06O
      *N95MU.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95MU.    IF    (W-TO01-MPLAN = 'IRA ACTIVE'                     lv25
                 AND   TO01-CIRAT NOT = 003)                            ATG06O
                 OR    W-TO01-MPLAN = 'IRA ROLLOVER'                    ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MU-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95MU-FN. EXIT.
      *N95MV.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95MV.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95MV-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95MV-FN. EXIT.
      *N95MW.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95MW.    IF    W-TO01-MPLAN = 'PENSION'                         lv25
                       OR 'BENEFICIAL'                                  ATG06O
                 OR    (W-TO01-MPLAN = 'IRA ACTIVE'                     ATG06O
                 AND   TO01-CIRAT = 003)                                ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MW-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95MW-FN. EXIT.
      *N95MX.    NOTE *OVERRIDES DIFF IN KD70/EZTRANS     *.            ATG06O
       F95MX.    IF    W-TO01-MPLAN = 'ROTH IRA'                        lv25
                       OR 'ROTH CONV'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95MX-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED AND IT IS PROCESSED A                                   ATG06O
      *SERVICE TEAM MEMBER(IE. ON KD70)                                 ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
                 AND   WORK-ISEPR = 'Y'                                 ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        05 TO WORK-CTG6O.                                ATG06O
       F95MX-FN. EXIT.
       F95MT-900. GO TO F95MD-FN.
       F95MT-FN. EXIT.
      *N95NA.    NOTE *SOURCE ACCOUNT IS ROTH IRA         *.            ATG06O
       F95NA.    IF    W-FR01-MPLAN =                                   lv20
                       'ROTH IRA'                                       ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NA-FN.                 ATG06O
      *N95NC.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95NC.    IF    W-TO01-MPLAN = 'ROTH CONV'                       lv25
                       OR 'EDUC IRA'                                    ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NC-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95NC-FN. EXIT.
      *N95ND.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95ND.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'ROTH BENF'                                   ATG06O
                       OR 'PENSION'                                     ATG06O
                       OR 'IRA ROLLOVER'                                ATG06O
                 OR    TO01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95ND-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95ND-FN. EXIT.
       F95NA-900. GO TO F95MD-FN.
       F95NA-FN. EXIT.
      *N95NF.    NOTE *SOURCE ACCOUNT IS ROTH             *.            ATG06O
       F95NF.    IF    W-FR01-MPLAN =                                   lv20
                       'ROTH BENF'                                      ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NF-FN.                 ATG06O
      *BENEFICIAL                                                       ATG06O
      *N95NG.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95NG.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'ROTH IRA'                                    ATG06O
                       OR 'ROTH CONV'                                   ATG06O
                 OR    TO01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NG-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95NG-FN. EXIT.
      *N95NH.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95NH.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95NH-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95NH-FN. EXIT.
       F95NF-900. GO TO F95MD-FN.
       F95NF-FN. EXIT.
      *N95NK.    NOTE *SOURCE ACCOUNT IS ROTH             *.            ATG06O
       F95NK.    IF    W-FR01-MPLAN =                                   lv20
                       'ROTH CONV'                                      ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NK-FN.                 ATG06O
      *CONVERTIBLE                                                      ATG06O
      *N95NL.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95NL.    IF    W-TO01-MPLAN = 'ROTH IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95NL-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95NL-FN. EXIT.
      *N95NM.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95NM.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95NM-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95NM-FN. EXIT.
      *N95NN.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95NN.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'IRA ROLLOVER'                                ATG06O
                       OR 'SRA'                                         ATG06O
                       OR 'ROTH CONVBEN'                                ATG06O
                       OR 'PENSION'                                     ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NN-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95NN-FN. EXIT.
       F95NK-900. GO TO F95MD-FN.
       F95NK-FN. EXIT.
      *N95NP.    NOTE *SOURCE ACCOUNT IS ROTH             *.            ATG06O
       F95NP.    IF    W-FR01-MPLAN =                                   lv20
                       'ROTH CONVBEN'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NP-FN.                 ATG06O
      *CONVERTIBLE BENEFICIAL                                           ATG06O
      *N95NQ.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95NQ.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'ROTH IRA'                                    ATG06O
                       OR 'ROTH CONV'                                   ATG06O
                 OR    TO01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NQ-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95NQ-FN. EXIT.
      *N95NR.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95NR.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95NR-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95NR-FN. EXIT.
       F95NP-900. GO TO F95MD-FN.
       F95NP-FN. EXIT.
      *N95NU.    NOTE *SOURCE ACCOUNT IS EDUCATION IRA    *.            ATG06O
       F95NU.    IF    W-FR01-MPLAN =                                   lv20
                       'EDUC IRA'                                       ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NU-FN.                 ATG06O
      *N95NV.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95NV.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'ROTH IRA'                                    ATG06O
                       OR 'ROTH CONV'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95NV-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95NV-FN. EXIT.
       F95NU-900. GO TO F95MD-FN.
       F95NU-FN. EXIT.
      *N95PA.    NOTE *SOURCE ACCOUNT IS BENEFICIAL IRA   *.            ATG06O
       F95PA.    IF    W-FR01-MPLAN =                                   lv20
                       'BENEFICIAL'                                     ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PA-FN.                 ATG06O
      *N95PB.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95PB.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'ROTH IRA'                                    ATG06O
                       OR 'ROTH CONV'                                   ATG06O
                 OR    TO01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PB-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95PB-FN. EXIT.
      *N95PC.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95PC.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95PC-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95PC-FN. EXIT.
      *N95PD.    NOTE *SERVICE TEAM ONLY OVERRIDES        *.            ATG06O
       F95PD.    IF    W-TO01-MPLAN =                                   lv25
                       'IRA ROLLOVER'                                   ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PD-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *SERVICE TEAM IS PROCESSING THE                                   ATG06O
      *TRANSACTION AND HAS RECEIVED THE                                 ATG06O
      *REQUEST IN WRITING                                               ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    WORK-ISEPR = 'Y'                                 DOT
                 AND   TE17-CHCR = 02                                   ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        04 TO WORK-CTG6O.                                ATG06O
       F95PD-FN. EXIT.
       F95PA-900. GO TO F95MD-FN.
       F95PA-FN. EXIT.
      *N95PF.    NOTE *SOURCE ACCOUNT IS PERSONAL         *.            ATG06O
       F95PF.    IF    W-FR01-MPLAN =                                   lv20
                       'PERSONAL'                                       ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PF-FN.                 ATG06O
      *N95PG.    NOTE *STANDARD OVERRIDES                 *.            ATG06O
       F95PG.    IF    W-TO01-MPLAN = 'IRA ROLLOVER'                    lv25
                 NEXT SENTENCE ELSE GO TO     F95PG-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED                                           ATG06O
           MOVE        'Y' TO WORK-ITG6O                                ATG06O
           MOVE        02 TO WORK-CTG6O.                                ATG06O
       F95PG-FN. EXIT.
      *N95PH.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95PH.    IF    W-TO01-MPLAN = 'EDUC IRA'                        lv25
                 NEXT SENTENCE ELSE GO TO     F95PH-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED IF A
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
      *OR A TELEPHONE REQUEST FROM
      *NON-QUALIFIED SOURCE ACCOUNT
      *BELONGS TO HOUSEHOLD GROUP
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
                 OR    (TE17-CHCR = 03
                 AND   FR01-CQACT = ZERO
                 AND   FG15-QITEM > ZEROS
                 AND   FG15-GRIDCB (1) = 001)
      *ALLOW NON-QUAL ACCT IN HOUSEHOLD
      *GROUP CONTRIBUTE TO CESA ACCT
           MOVE        02 TO WORK-CTG6O
                 ELSE
           MOVE        03 TO WORK-CTG6O.
       F95PH-FN. EXIT.
       F95PF-900. GO TO F95MD-FN.
       F95PF-FN. EXIT.
      *N95PK.    NOTE *SOURCE ACCOUNT IS PENSION          *.            ATG06O
       F95PK.    IF    W-FR01-MPLAN =                                   lv20
                       'PENSION'                                        ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PK-FN.                 ATG06O
      *N95PM.    NOTE *WRITTEN/QUALIFIED PLAN OVERRIDES   *.            ATG06O
       F95PM.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'IRA ROLLOVER'                                ATG06O
                 OR    TO01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PM-FN.                 ATG06O
      *MAY ONLY PROCESS IN KD70                                         ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED AND IS BEING PROCESSED                                  ATG06O
      *BY THE QUALIFIED PLAN AREA.  AS                                  ATG06O
      *OF 12/1/98 THESE TRANS WILL ONLY                                 ATG06O
      *BE ALLOWED IN KD70 BECAUSE                                       ATG06O
      *EZTRANS IS UNABLE TO CORRECTLY                                   ATG06O
      *ASSIGN THE TAX SETTLEMENT CODE.                                  ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
                 AND   WORK-ISEPR = 'Y'                                 ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        05 TO WORK-CTG6O.                                ATG06O
       F95PM-FN. EXIT.
       F95PK-900. GO TO F95MD-FN.
       F95PK-FN. EXIT.
      *N95PP.    NOTE *SOURCE ACCOUNT IS TSA              *.            ATG06O
       F95PP.    IF    W-FR01-MPLAN =                                   lv20
                       'TSA'                                            ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PP-FN.                 ATG06O
      *N95PR.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95PR.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'IRA ROLLOVER'                                ATG06O
                       OR 'TSCA'                                        ATG06O
                 OR    TO01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PR-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95PR-FN. EXIT.
       F95PP-900. GO TO F95MD-FN.
       F95PP-FN. EXIT.
      *N95PU.    NOTE *SOURCE ACCOUNT IS TSCA             *.            ATG06O
       F95PU.    IF    W-FR01-MPLAN =                                   lv20
                       'TSCA'                                           ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PU-FN.                 ATG06O
      *N95PW.    NOTE *WRITTEN ONLY OVERRIDES             *.            ATG06O
       F95PW.    IF    W-TO01-MPLAN = 'PERSONAL'                        lv25
                       OR 'TSA'                                         ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PW-FN.                 ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED                                                         ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        03 TO WORK-CTG6O.                                ATG06O
       F95PW-FN. EXIT.
      *N95PY.    NOTE *WRITTEN/QUALIFIED PLAN OVERRIDES   *.            ATG06O
       F95PY.    IF    W-TO01-MPLAN = 'IRA ACTIVE'                      lv25
                       OR 'IRA ROLLOVER'                                ATG06O
                 OR    TO01-CIRAT = 003                                 ATG06O
                 NEXT SENTENCE ELSE GO TO     F95PY-FN.                 ATG06O
      *MAY ONLY PROCESS IN KD70                                         ATG06O
      *FOR THESE DESTINATION ACCT TYPES                                 ATG06O
      *TRANSACTION IS ALLOWED ONLY IF A                                 ATG06O
      *WRITTEN REQUEST HAS BEEN                                         ATG06O
      *RECEIVED AND IS BEING PROCESSED                                  ATG06O
      *BY THE QUALIFIED PLAN AREA.  AS                                  ATG06O
      *OF 12/1/98 THESE TRANS WILL ONLY                                 ATG06O
      *BE ALLOWED IN KD70 BECAUSE                                       ATG06O
      *EZTRANS IS UNABLE TO CORRECTLY                                   ATG06O
      *ASSIGN THE TAX SETTLEMENT CODE.                                  ATG06O
           MOVE        'Y' TO WORK-ITG6O.                               ATG06O
                 IF    TE17-CHCR = 02                                   DOT
                 AND   WORK-ISEPR = 'Y'                                 ATG06O
           MOVE        02 TO WORK-CTG6O                                 ATG06O
                 ELSE                                                   ATG06O
           MOVE        05 TO WORK-CTG6O.                                ATG06O
       F95PY-FN. EXIT.
       F95PU-900. GO TO F95MD-FN.
       F95PU-FN. EXIT.
       F95MD-FN. EXIT.
       F95MA-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96BB.    NOTE *PROCESSING MACRO APLANU FOR UD     *.
       F96BB.                                                           lv10
      *   THIS MACRO MODELS KD70
      *N96CA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            APLANU
       F96CA.                                                           lv15
      *                                                                 APLANU
      *********************************                                 APLANU
      ** INITIALIZE THE ACCOUNT PLAN  *                                 APLANU
      ** TYPE DESCRIPTION TO SPACES   *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        SPACES TO W-FR01-MPLAN.                          APLANU
       F96CA-FN. EXIT.
      *N96DA.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            APLANU
       F96DA.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 001                            APLANU
                 NEXT SENTENCE ELSE GO TO     F96DA-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** HOUSEHOLD GROUP, THEN THE    *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-FR01-MPLAN.                      APLANU
      *N96EA.    NOTE *CHECK IF QUALIFIED                 *.            APLANU
       F96EA.    IF    FR01-CQACT > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F96EA-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS QUALIFIED  *                                 APLANU
      ** CHECK THE IRA TYPE CODE AND  *                                 APLANU
      ** AND IRA STATUS CODE          *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
           MOVE        'IRA ACTIVE' TO W-FR01-MPLAN.                    APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 002                                 APLANU
           MOVE        'IRA ROLLOVER' TO W-FR01-MPLAN.                  APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 003                                 APLANU
                 AND   (FR01-CIRAT NOT = 005                            APLANU
                 AND   FR01-CIRAT NOT = 006)                            APLANU
           MOVE        'BENEFICIAL' TO W-FR01-MPLAN.                    APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CIRAT = 004                                 APLANU
           MOVE        'SRA' TO W-FR01-MPLAN.                           APLANU
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH IRA   ' TO W-FR01-MPLAN.                   APLANU
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 003                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH BENF  ' TO W-FR01-MPLAN.                   APLANU
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONV  ' TO W-FR01-MPLAN.                   APLANU
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 003                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONVBEN' TO W-FR01-MPLAN.                  APLANU
                 IF    FR01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-FR01-MPLAN.                           APLANU
                 IF    FR01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-FR01-MPLAN.                          APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CIRAT = 007                                 APLANU
           MOVE        'EDUC IRA' TO W-FR01-MPLAN.                      APLANU
       F96EA-FN. EXIT.
       F96DA-FN. EXIT.
      *N96FA.    NOTE *CHECK IF PENSION GROUP             *.            APLANU
       F96FA.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 002                            APLANU
                 NEXT SENTENCE ELSE GO TO     F96FA-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** PENSION GROUP, THEN THE      *                                 APLANU
      ** PLAN TYPE IS PENSION.        *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PENSION' TO W-FR01-MPLAN.                       APLANU
      *N96GA.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            APLANU
       F96GA.    IF    FG15-CIDRP (1) NOT = 01                          lv20
                 NEXT SENTENCE ELSE GO TO     F96GA-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS NOT A      *                                 APLANU
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-FR01-MPLAN.                      APLANU
       F96GA-FN. EXIT.
       F96FA-FN. EXIT.
      *N96IA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            APLANU
       F96IA.                                                           lv15
      *                                                                 APLANU
      *********************************                                 APLANU
      ** INITIALIZE THE ACCOUNT PLAN  *                                 APLANU
      ** TYPE DESCRIPTION TO SPACES   *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        SPACES TO W-TO01-MPLAN.                          APLANU
       F96IA-FN. EXIT.
      *N96JA.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            APLANU
       F96JA.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 001                            APLANU
                 NEXT SENTENCE ELSE GO TO     F96JA-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** HOUSEHOLD GROUP, THEN THE    *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-TO01-MPLAN.                      APLANU
      *N96KA.    NOTE *CHECK IF QUALIFIED                 *.            APLANU
       F96KA.    IF    TO01-CQACT > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F96KA-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS QUALIFIED  *                                 APLANU
      ** CHECK THE IRA TYPE CODE AND  *                                 APLANU
      ** AND IRA STATUS CODE          *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
           MOVE        'IRA ACTIVE' TO W-TO01-MPLAN.                    APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 002                                 APLANU
           MOVE        'IRA ROLLOVER' TO W-TO01-MPLAN.                  APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 003                                 APLANU
                 AND   (TO01-CIRAT NOT = 005                            APLANU
                 AND   TO01-CIRAT NOT = 006)                            APLANU
           MOVE        'BENEFICIAL' TO W-TO01-MPLAN.                    APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CIRAT = 004                                 APLANU
           MOVE        'SRA' TO W-TO01-MPLAN.                           APLANU
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH IRA   ' TO W-TO01-MPLAN.                   APLANU
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 003                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH BENF  ' TO W-TO01-MPLAN.                   APLANU
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONV  ' TO W-TO01-MPLAN.                   APLANU
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 003                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONVBEN' TO W-TO01-MPLAN.                  APLANU
                 IF    TO01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-TO01-MPLAN.                           APLANU
                 IF    TO01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-TO01-MPLAN.                          APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CIRAT = 007                                 APLANU
           MOVE        'EDUC IRA' TO W-TO01-MPLAN.                      APLANU
       F96KA-FN. EXIT.
       F96JA-FN. EXIT.
      *N96LA.    NOTE *CHECK IF PENSION GROUP             *.            APLANU
       F96LA.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 002                            APLANU
                 NEXT SENTENCE ELSE GO TO     F96LA-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** PENSION GROUP, THEN THE      *                                 APLANU
      ** PLAN TYPE IS PENSION.        *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PENSION' TO W-TO01-MPLAN.                       APLANU
      *N96MA.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            APLANU
       F96MA.    IF    TG15-CIDRP (1) NOT = 01                          lv20
                 NEXT SENTENCE ELSE GO TO     F96MA-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS NOT A      *                                 APLANU
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-TO01-MPLAN.                      APLANU
       F96MA-FN. EXIT.
       F96LA-FN. EXIT.
       F96BB-FN. EXIT.
      *N96PA.    NOTE *---> Call CI0135                   *.            AM0135
       F96PA.                                                           lv10
      *     Get Cert Account Info                                       AM0135
      *                                                                 AM0135
           INITIALIZE  PJ02                                             AM0135
           DE10-DU03                                                    AM0135
           MOVE        TO01-CTID TO PJ02-CTID                           AM0135
           SET CI0135-PCB-CH1P-PTR1 TO                                  AM0135
                      PCB-CH1P-PTR1                                     AM0135
           SET CI0135-PCB-CCRP-PTR1 TO                                  AM0135
                      PCB-CCRP-PTR1                                     AM0135
           SET CI0135-PCB-CPRP-PTR1 TO                                  AM0135
                      PCB-CPRP-PTR1                                     AM0135
           SET CI0135-PCB-CBTP-PTR1 TO                                  AM0135
                      PCB-CBTP-PTR1                                     AM0135
           SET CI0135-PCB-CA1P-PTR1 TO                                  AM0135
                      PCB-CA1P-PTR1                                     AM0135
           CALL        CI0135 USING                                     AM0135
           DFHEIBLK                                                     AM0135
           DFHCOMMAREA                                                  AM0135
           DLIUIBII                                                     AM0135
           CI0135-PCB-ADDRESS-LIST                                      AM0135
           PJ02                                                         AM0135
           DE10                                                         AM0135
           MS03                                                         AM0135
           MX11.                                                        AM0135
       F96PA-FN. EXIT.
       F96-FN.   EXIT.
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
