       IDENTIFICATION DIVISION.                                         CI0268
       PROGRAM-ID.  CI0268P.                                            CI0268
      *AUTHOR.         INVESTMENT CHANGE CONTROL EDIT.                  CI0268
      *DATE-COMPILED.   09/08/14.                                       CI0268
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
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CI0268
       CONFIGURATION SECTION.                                           CI0268
       SOURCE-COMPUTER. IBM-370.                                        CI0268
       OBJECT-COMPUTER. IBM-370.                                        CI0268
       DATA DIVISION.                                                   CI0268
       WORKING-STORAGE SECTION.                                         CI0268
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0146           PIC X(8) VALUE 'CI0146P '.                  AM0146
       01  CI0500           PIC X(8) VALUE 'CI0500P '.                  AM0500
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0268
            10            I93B-CEADC  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-DACTT  PICTURE  X(10)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-GEOPDC PICTURE  X(8)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-GEOPDB PICTURE  X(8)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-CAEMCE PICTURE  X(8)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-CAEMCD PICTURE  X(8)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-GETIMM PICTURE  X(8)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0268
                          VALUE                ZERO                     CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            I93B-GERTC  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-DXTMST PICTURE  X(26)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0268
                          VALUE                SPACE.                   CI0268
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE NAME OF THE CALLED MODULE.    ACMCTI
      ******************************************************************ACMCTI
       01          CI0361       PIC X(08)                               ACMCTI
                                VALUE 'CI0361P'.                        ACMCTI
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE DATE FOR THE DUMMY DB2 CALL.  ACMCTI
      ******************************************************************ACMCTI
       01          WS00-DATE    PIC X(10)     VALUE SPACES.             ACMCTI
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0268
            10            XW05-XW06.                                    CI0268
            11            XW05-XDBPCB.                                  CI0268
            12            XW05-XDBDNM PICTURE  X(08)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-XSEGLV PICTURE  X(02)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-XRC    PICTURE  X(02)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-XPROPT PICTURE  X(04)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-FILLER PICTURE  S9(5)                    CI0268
                          VALUE                ZERO                     CI0268
                          BINARY.                                       CI0268
            12            XW05-XSEGNM PICTURE  X(08)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0268
                          VALUE                ZERO                     CI0268
                          BINARY.                                       CI0268
            12            XW05-XSEGNB PICTURE  9(05)                    CI0268
                          VALUE                ZERO                     CI0268
                          BINARY.                                       CI0268
            12            XW05-XCOKEY PICTURE  X(70)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            XW05-XW07.                                    CI0268
            11            XW05-XIOPCB.                                  CI0268
            12            XW05-XTERMI PICTURE  X(08)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-FILLER PICTURE  XX                       CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-XRC1   PICTURE  X(02)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-FILLER PICTURE  X(12)                    CI0268
                          VALUE                SPACE.                   CI0268
            12            XW05-XMODNM PICTURE  X(8)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0268
                          VALUE                ZERO.                    CI0268
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0268
                          VALUE                ZERO.                    CI0268
            10            XW05-XGU    PICTURE  X(4)                     CI0268
                          VALUE                'GU  '.                  CI0268
            10            XW05-XGHU   PICTURE  X(4)                     CI0268
                          VALUE                'GHU '.                  CI0268
            10            XW05-XGN    PICTURE  X(4)                     CI0268
                          VALUE                'GN  '.                  CI0268
            10            XW05-XGHN   PICTURE  X(4)                     CI0268
                          VALUE                'GHN '.                  CI0268
            10            XW05-XGNP   PICTURE  X(4)                     CI0268
                          VALUE                'GNP '.                  CI0268
            10            XW05-XGHNP  PICTURE  X(4)                     CI0268
                          VALUE                'GHNP'.                  CI0268
            10            XW05-XREPL  PICTURE  XXXX                     CI0268
                          VALUE                'REPL'.                  CI0268
            10            XW05-XISRT  PICTURE  X(4)                     CI0268
                          VALUE                'ISRT'.                  CI0268
            10            XW05-XDLET  PICTURE  X(4)                     CI0268
                          VALUE                'DLET'.                  CI0268
            10            XW05-XOPEN  PICTURE  X(4)                     CI0268
                          VALUE                'OPEN'.                  CI0268
            10            XW05-XCLSE  PICTURE  X(4)                     CI0268
                          VALUE                'CLSE'.                  CI0268
            10            XW05-XCHKP  PICTURE  X(4)                     CI0268
                          VALUE                'CHKP'.                  CI0268
            10            XW05-XXRST  PICTURE  X(4)                     CI0268
                          VALUE                'XRST'.                  CI0268
            10            XW05-XTERM  PICTURE  X(4)                     CI0268
                          VALUE                'TERM'.                  CI0268
            10            XW05-XNFPAC PICTURE  X(13)                    CI0268
                          VALUE                SPACE.                   CI0268
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0268
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0268
      *!WF DSP=DD DSL=DU SEL=25 FOR=I DES=2 LEV=2                       ADU040
       01                 DD00.                                         CI0268
            02            DD25.                                         CI0268
            10            DD25-XDATG.                                   CI0268
            11            DD25-XDAT1.                                   CI0268
            12            DD25-XDAT19 PICTURE  99                       CI0268
                          VALUE                ZERO.                    CI0268
            11            DD25-XDAT2.                                   CI0268
            12            DD25-XDAT29 PICTURE  99                       CI0268
                          VALUE                ZERO.                    CI0268
            11            DD25-XDAT3.                                   CI0268
            12            DD25-XDAT39 PICTURE  99                       CI0268
                          VALUE                ZERO.                    CI0268
            11            DD25-XDAT4.                                   CI0268
            12            DD25-XDAT49 PICTURE  99                       CI0268
                          VALUE                ZERO.                    CI0268
            10            DD25-XLEAPY PICTURE  99                       CI0268
                          VALUE                ZERO.                    CI0268
      *TABLE TO STORE FROM SUB ACCOUNTS TO COMPARE AGAINST TO SUB
      *ACCOUNTS
      *
       01  WS01-FROM-TAB.
           05  WS01-FROM-CFIDC OCCURS 1 TO 99 DEPENDING ON WS01-NO-FROM.
      *!WI
               10  FR01-CFIDC
                        PICTURE X(5).                                   CI0268
      *
      *
      *TABLE TO STORE TO SUB ACCOUNTS TO COMPARE AGAINST TO SUB
      *ACCOUNTS
       01  WS01-TO-TAB.
           05  WS01-TO-CFIDC OCCURS 1 TO 99 DEPENDING ON WS01-NO-FROM.
      *!WI
               10 TO01-CFIDC
                        PICTURE X(5).                                   CI0268
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
      *                                                                 AM0020
      ******************************************************************AM0020
      **     SEGMENT THAT CONTAINS THE CAMS ACCOUNTING DATES           *AM0020
      ******************************************************************AM0020
      *                                                                 AM0020
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1                             AM0020
       01                 NS00.                                         CI0268
          05              NS00-00.                                      CI0268
            10            NS00-NS00K.                                   CI0268
            11            NS00-PRCSTK PICTURE  XX.                      CI0268
          05              NS00-SUITE.                                   CI0268
            15       FILLER         PICTURE  X(00078).                  CI0268
       01                 NS20  REDEFINES      NS00.                    CI0268
            10       FILLER         PICTURE  X(00002).                  CI0268
            10            NS20-DCACG  PICTURE  9(8).                    CI0268
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            NS20-CCDAT  PICTURE  X(8).                    CI0268
            10            NS20-DCALP  PICTURE  X(12).                   CI0268
            10            NS20-DNACG  PICTURE  9(8).                    CI0268
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            NS20-CNDAT  PICTURE  X(8).                    CI0268
            10            NS20-DNALP  PICTURE  X(12).                   CI0268
            10            NS20-DCACD  PICTURE  X(10).                   CI0268
            10            NS20-FILLER PICTURE  X(4).                    CI0268
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0268
           04    G-TA5B-PARAM.                                          CI0268
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0268
                        VALUE      +154.                                CI0268
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0268
                        VALUE      +001.                                CI0268
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0268
                        VALUE      +017.                                CI0268
             10  G-TA5B-NUAPP  PICTURE 99                               CI0268
                        VALUE       0.                                  CI0268
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0268
                        VALUE 'TA005B'.                                 CI0268
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0268
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0268
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0268
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0268
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0268
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0268
             10  G-TA5B-FILSYS.                                         CI0268
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0268
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0268
           04             TA5B.                                         CI0268
            10            TA5B-GAPSC.                                   CI0268
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0268
                          VALUE                ZERO.                    CI0268
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0268
                          VALUE                ZERO.                    CI0268
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-PRCODX PICTURE  9(5)                     CI0268
                          VALUE                ZERO.                    CI0268
            10            TA5B-PRCSUB PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-PRCAUT PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-PRCBAS PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-PRCSTK PICTURE  XX                       CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IBDUP  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IUSPR  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-GRSFC  PICTURE  99                       CI0268
                          VALUE                ZERO.                    CI0268
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ITERM  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0268
                          VALUE                ZERO.                    CI0268
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-CFXDM  PICTURE  99                       CI0268
                          VALUE                ZERO.                    CI0268
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-CTNLI  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-CBANK  PICTURE  X(03)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ISYPO  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ISYPP  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ICOPT  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IANPY  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IDSAR  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ICIPT  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IANDS  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IKPMA  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-INMWT  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IVANT  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ISDAV  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-IUDAV  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0268
                          VALUE                SPACE.                   CI0268
      **                                                                ADUTAB
      ******************************************************************ADUTAB
      **              TABLE TB8A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TB8A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TB DSL=TA SEL=8A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TB8A.                                                CI0268
           04    G-TB8A-PARAM.                                          CI0268
             10  G-TB8A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0268
                        VALUE      +106.                                CI0268
             10  G-TB8A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0268
                        VALUE      +001.                                CI0268
             10  G-TB8A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0268
                        VALUE      +008.                                CI0268
             10  G-TB8A-NUAPP  PICTURE 99                               CI0268
                        VALUE       0.                                  CI0268
             10  G-TB8A-NUTAB  PICTURE X(6)                             CI0268
                        VALUE 'TA008A'.                                 CI0268
             10  G-TB8A-TABFO  PICTURE XX                 VALUE SPACE.  CI0268
             10  G-TB8A-TABCR  PICTURE XX                 VALUE SPACE.  CI0268
             10  G-TB8A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0268
             10  G-TB8A-NUSSC  PICTURE X  VALUE   ' '.                  CI0268
             10  G-TB8A-NUSSY  PICTURE X                  VALUE SPACE.  CI0268
             10  G-TB8A-TRANID PICTURE X(4)               VALUE SPACE.  CI0268
             10  G-TB8A-FILSYS.                                         CI0268
             15  G-TB8A-USERC  PICTURE X(6)               VALUE SPACE.  CI0268
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0268
           04             TB8A.                                         CI0268
            10            TB8A-GADPR.                                   CI0268
            11            TB8A-CTIDA  PICTURE  9(3)                     CI0268
                          VALUE                ZERO.                    CI0268
            11            TB8A-PRCOD  PICTURE  9(5)                     CI0268
                          VALUE                ZERO.                    CI0268
            10            TB8A-CLIAN  PICTURE  9(02)                    CI0268
                          VALUE                ZERO.                    CI0268
            10            TB8A-CLAST  PICTURE  9(03)                    CI0268
                          VALUE                ZERO.                    CI0268
            10            TB8A-ISMTD  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-ISUBA  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-IVINS  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-IEOIR  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-IDBNL  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-ICHRC  PICTURE  X(1)                     CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-ICHPN  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-IVAPR  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-ICLSF  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-IIULA  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-IINPS  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-IINLN  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-CINPS  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-CINLN  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            TB8A-ZDA79  PICTURE  X(79)                    CI0268
                          VALUE                SPACE.                   CI0268
      **                                                                ADUTAB
      *                                                                 AM0500
      ****************************************************************  AM0500
      ** WORKING STORAGE FOR CI0500                                     AM0500
      ****************************************************************  AM0500
      *                                                                 AM0500
      *!WF DSP=P9 DSL=K9 SEL=49 FOR=I DES=2 LEV=1                       AM0500
       01                 P948.                                         CI0268
            10            P948-CTID   PICTURE  X(27)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            P948-ALPLDT PICTURE  9(8)                     CI0268
                          VALUE                ZERO.                    CI0268
            10            P948-DCACG  PICTURE  9(8)                     CI0268
                          VALUE                ZERO.                    CI0268
            10            P948-MAPPN  PICTURE  X(10)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            P948-ACVPY  PICTURE  S9(7)V99                 CI0268
                          VALUE                ZERO                     CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            P948-ALSURF PICTURE  S9(7)V99                 CI0268
                          VALUE                ZERO                     CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            P948-CSTCVE PICTURE  S9(9)V99                 CI0268
                          VALUE                ZERO                     CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            P948-ALTTDF PICTURE  S9(07)V99                CI0268
                          VALUE                ZERO                     CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            P948-ALYTDF PICTURE  S9(07)V99                CI0268
                          VALUE                ZERO                     CI0268
                          COMPUTATIONAL-3.                              CI0268
       01                 P949.                                         CI0268
            10            P949-CTID   PICTURE  X(27)                    CI0268
                          VALUE                SPACE.                   CI0268
            10            P949-DCACG  PICTURE  9(8)                     CI0268
                          VALUE                ZERO.                    CI0268
            10            P949-GERTC  PICTURE  X                        CI0268
                          VALUE                SPACE.                   CI0268
            10            P949-ADBRQF PICTURE  S9(9)V99                 CI0268
                          VALUE                ZERO                     CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            P949-FILLER PICTURE  X(38)                    CI0268
                          VALUE                SPACE.                   CI0268
      *                                                                 AM0500
      *                                                                 AM0500
      *                                                                 AM0500
      ****************************************************************  AM0500
      ** PCB ADDRESS LIST FOR CI0500.  MODULE CI0500 WILL NEED          AM0500
      ** PCB'S FOR: ARAY AND AR1P                                       AM0500
      ****************************************************************  AM0500
       01                 CI0500-VB-PCB-ADDR-LIST.                      AM0500
         05               CI0500-VB-PCB-ARAY-PTR1       POINTER.        AM0500
         05               CI0500-VB-PCB-AR1P-PTR1       POINTER.        AM0500
      **---------------------------------------------------------------*
      **         WORKING STORAGE TO STORE THE COMPUTED VALUES          *
      **---------------------------------------------------------------*
      *
      *WORKING STORAGE TO STORE THE COMPUTED CASH VALUE
      *!WI
       01                      WA00-ACVPY
                        PICTURE S9(7)V99                                CI0268
                          COMPUTATIONAL-3                               CI0268
                               VALUE ZEROES.
      *
      *WORKING STORAGE TO STORE THE COMPUTED MAXIMUM ACCOUNT VALUE
      *!WI
       01                      WA00-MAX-ACVPY
                        PICTURE S9(7)V99                                CI0268
                          COMPUTATIONAL-3                               CI0268
                               VALUE ZEROES.
      *
      *WORKING STORAGE TO STORE THE TOTAL AMOUNT
      *!WI
       01                      WA00-TOTL-ADBRQF
                        PICTURE S9(9)V99                                CI0268
                          COMPUTATIONAL-3                               CI0268
                               VALUE ZEROES.
      *
      *WORKING STORAGE TO STORE THE ACTUAL ACCOUNT VALUE
      *!WI
       01                      WA00-ACT-AACTV
                        PICTURE S9(11)V99                               CI0268
                          COMPUTATIONAL-3                               CI0268
                               VALUE ZEROES.
      *
      *WORKING STORAGE TO STORE THE TEMPORARY AMOUNT
      *!WI
       01                      WA00-TEMP-AEDRQ
                        PICTURE S9(11)V99                               CI0268
                          COMPUTATIONAL-3                               CI0268
                               VALUE ZEROES.
      *
      *WORKING STORAGE TO STORE THE TEMPORARY PERCENTAGE
      *!WI
       01                      WA00-TEMP-PACT1
                        PICTURE S999V999                                CI0268
                          COMPUTATIONAL-3                               CI0268
                               VALUE ZEROES.
      ******************************************************************AM0146
      **** PCB ADDRESS LIST FOR CI0146                                 *AM0146
      ******************************************************************AM0146
       01                 CI0146-WB-PCB-ADDR-LIST.                      AM0146
           05             CI0146-WB-PCB-CT1P-PTR1      POINTER.         AM0146
      ******************************************************************AM0146
      **** STOP GAP DETAILS       PASS AREA (LINKAGE)  *****************AM0146
      ******************************************************************AM0146
      *                                                                 AM0146
      *!WF DSP=P9 DSL=K9 SEL=48 FOR=I LEV=1                             AM0146
      **---------------------------------------------------------------*
      *        WORKING STORAGE TO STORE THE DATES                      *
      **---------------------------------------------------------------*
      *
      *WORKING STORAGE TO STORE THE NEW END DATE
      *!WI
       01                 WD00-GEEND     VALUE ZEROES
                        PICTURE 9(8).                                   CI0268
      *
      *WORKING STORAGE TO STORE THE COMPUTED NEXT PAYMENT DATE
      *!WI
       01                 WD00-DNPMT     VALUE ZEROES
                        PICTURE 9(8).                                   CI0268
      *
      *SUBSCRIPT TO HANDLE SUB-ACCOUNT TABLE K974
       01  WS01-SUB           PIC 999 VALUE 1.
      *
       01  WS01-NO-FROM       PIC 999 VALUE 0.
      *
       01  WS01-NO-TO         PIC 999 VALUE 0.
      *
       01  WS01-FROM-DOL-IND  PIC X VALUE SPACE.
      *
       01  WS01-FROM-PER-IND  PIC X VALUE SPACE.
      *
       01  WS01-TO-DOL-IND    PIC X VALUE SPACE.
      *
       01  WS01-TO-PER-IND    PIC X VALUE SPACE.
      *
       01  WS01-AA-PACT1      PIC 999V999 VALUE 0.
      *
      *!WI
       01  WS01-FROM-ADBRQ                VALUE 0
                        PICTURE S9(11)V99                               CI0268
                          COMPUTATIONAL-3.                              CI0268
      *
      *!WI
       01  WS01-TO-ADBRQ                  VALUE 0
                        PICTURE S9(11)V99                               CI0268
                          COMPUTATIONAL-3.                              CI0268
      *
      *!WI
       01  WS01-FROM-AEDRQ                VALUE 0
                        PICTURE S9(11)V99                               CI0268
                          COMPUTATIONAL-3.                              CI0268
      *
      *!WI
       01  WS01-TO-PACT1                  VALUE 0
                        PICTURE S999V999                                CI0268
                          COMPUTATIONAL-3.                              CI0268
      *
       01  WS01-FV-VF               PIC XX.
      *
       01  WS01-QSACT               PIC 999 VALUE 0.
      *
      *!WI
       01  WS01-ITRNB               VALUE SPACE
                        PICTURE X.                                      CI0268
      *
       01  WS01-MIN-PER             PIC 9(03)V999 VALUE 0.
      *
      *!WI
       01  WS01-AACTV               VALUE 0
                        PICTURE S9(11)V99                               CI0268
                          COMPUTATIONAL-3.                              CI0268
      *
       01  WS01-TEMP-PER            PIC 9(03)V9(05) VALUE 0.
      *
       01  WS01-ALL-IND             PIC X VALUE 'Y'.
      *
      *!WI
       01  WS00-CPMTF               VALUE ZEROES
                        PICTURE 99.                                     CI0268
      *
      *!WI
       01  WS00-CIRMO               VALUE SPACES
                        PICTURE X(12).                                  CI0268
      *WORKING STORAGE FOR MARKET CLOSE TIME
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0268
       01  WS-GETIMM-RED REDEFINES WS-GETIMM.
           05  WS-HH      PIC XX.
           05  WS-DOT1    PIC X.
           05  WS-MM      PIC XX.
           05  WS-DOT2    PIC X.
           05  WS-SS      PIC XX.
      *!WI
       01  WS-GETIM6
                        PICTURE 9(06).                                  CI0268
       01  WS-MKT-TIME REDEFINES WS-GETIM6.
           05  WS-MKT-HH  PIC 99.
           05  WS-MKT-MM  PIC 99.
           05  WS-MKT-SS  PIC 99.
       01  TA5B-IK          PIC X(01)   VALUE '0'.
      *
       01   DEBUT-WSS.                                                  CI0268
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0268
            05   IK     PICTURE X.                                      CI0268
       01  CONSTANTES-PAC.                                              CI0268
           05  FILLER  PICTURE X(87)   VALUE                            CI0268
                     '6015 CAT09/08/14CI0268ADMIN   14:35:08CI0268P AMERCI0268
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0268
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0268
           05  NUGNA   PICTURE X(5).                                    CI0268
           05  APPLI   PICTURE X(3).                                    CI0268
           05  DATGN   PICTURE X(8).                                    CI0268
           05  PROGR   PICTURE X(6).                                    CI0268
           05  CODUTI  PICTURE X(8).                                    CI0268
           05  TIMGN   PICTURE X(8).                                    CI0268
           05  PROGE   PICTURE X(8).                                    CI0268
           05  COBASE  PICTURE X(4).                                    CI0268
           05  DATGNC  PICTURE X(10).                                   CI0268
           05  RELEAS  PICTURE X(7).                                    CI0268
           05  DATGE   PICTURE X(10).                                   CI0268
           05  DATSQ   PICTURE X(10).                                   CI0268
       01  DATCE.                                                       CI0268
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0268
         05  DATOR.                                                     CI0268
           10  DATOA  PICTURE XX.                                       CI0268
           10  DATOM  PICTURE XX.                                       CI0268
           10  DATOJ  PICTURE XX.                                       CI0268
       01   VARIABLES-CONDITIONNELLES.                                  CI0268
            05                  FT      PICTURE X VALUE '0'.            CI0268
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0268
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0268
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0268
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
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ARAY                                           ADU015
            05 PCB-ARAY-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0268
          05              XA00-SUITE.                                   CI0268
            15       FILLER         PICTURE  X(00106).                  CI0268
       01                 XA06  REDEFINES      XA00.                    CI0268
            10            XA06-XDBPCB.                                  CI0268
            11            XA06-XDBDNM PICTURE  X(08).                   CI0268
            11            XA06-XSEGLV PICTURE  X(02).                   CI0268
            11            XA06-XRC    PICTURE  X(02).                   CI0268
            11            XA06-XPROPT PICTURE  X(04).                   CI0268
            11            XA06-FILLER PICTURE  S9(5)                    CI0268
                          BINARY.                                       CI0268
            11            XA06-XSEGNM PICTURE  X(08).                   CI0268
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0268
                          BINARY.                                       CI0268
            11            XA06-XSEGNB PICTURE  9(05)                    CI0268
                          BINARY.                                       CI0268
            11            XA06-XCOKEY PICTURE  X(70).                   CI0268
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0268
          05              XB00-SUITE.                                   CI0268
            15       FILLER         PICTURE  X(00106).                  CI0268
       01                 XB06  REDEFINES      XB00.                    CI0268
            10            XB06-XDBPCB.                                  CI0268
            11            XB06-XDBDNM PICTURE  X(08).                   CI0268
            11            XB06-XSEGLV PICTURE  X(02).                   CI0268
            11            XB06-XRC    PICTURE  X(02).                   CI0268
            11            XB06-XPROPT PICTURE  X(04).                   CI0268
            11            XB06-FILLER PICTURE  S9(5)                    CI0268
                          BINARY.                                       CI0268
            11            XB06-XSEGNM PICTURE  X(08).                   CI0268
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0268
                          BINARY.                                       CI0268
            11            XB06-XSEGNB PICTURE  9(05)                    CI0268
                          BINARY.                                       CI0268
            11            XB06-XCOKEY PICTURE  X(70).                   CI0268
      *** PCB MASK FOR ARAY                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0268
          05              XC00-SUITE.                                   CI0268
            15       FILLER         PICTURE  X(00106).                  CI0268
       01                 XC06  REDEFINES      XC00.                    CI0268
            10            XC06-XDBPCB.                                  CI0268
            11            XC06-XDBDNM PICTURE  X(08).                   CI0268
            11            XC06-XSEGLV PICTURE  X(02).                   CI0268
            11            XC06-XRC    PICTURE  X(02).                   CI0268
            11            XC06-XPROPT PICTURE  X(04).                   CI0268
            11            XC06-FILLER PICTURE  S9(5)                    CI0268
                          BINARY.                                       CI0268
            11            XC06-XSEGNM PICTURE  X(08).                   CI0268
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0268
                          BINARY.                                       CI0268
            11            XC06-XSEGNB PICTURE  9(05)                    CI0268
                          BINARY.                                       CI0268
            11            XC06-XCOKEY PICTURE  X(70).                   CI0268
      *LINKAGE INPUT FROM THE BROWSER K974
      *!WF DSP=K9 DSL=K9 SEL=74 FOR=E DES=1 LEV=1 PLT=20
       01                 K974.                                         CI0268
            10            K974-MAPPN  PICTURE  X(10).                   CI0268
            10            K974-CAFLOW PICTURE  X(4).                    CI0268
            10            K974-CACTA  PICTURE  X(1).                    CI0268
            10            K974-IDBUP  PICTURE  X.                       CI0268
            10            K974-NSSSI  PICTURE  X(24).                   CI0268
            10            K974-CTID   PICTURE  X(27).                   CI0268
            10            K974-QSACTF PICTURE  9(3).                    CI0268
            10            K974-QSACTT PICTURE  9(3).                    CI0268
            10            K974-FILLER PICTURE  X(100).                  CI0268
            10            K974-K981.                                    CI0268
            11            K974-GRUST                                    CI0268
                          OCCURS       194     TIMES.                   CI0268
            12            K974-CFIDC  PICTURE  X(5).                    CI0268
            12            K974-CACCT  PICTURE  X.                       CI0268
            12            K974-AACTV  PICTURE  S9(11)V99.               CI0268
            12            K974-PFNDV  PICTURE  999V999.                 CI0268
            12            K974-ITRNB  PICTURE  X.                       CI0268
            12            K974-IALLV  PICTURE  X.                       CI0268
            12            K974-ADBRQ  PICTURE  S9(11)V99.               CI0268
            12            K974-PACT1  PICTURE  S999V999.                CI0268
            12            K974-AEDRQ  PICTURE  S9(11)V99.               CI0268
            12            K974-AMAXD  PICTURE  S9(7)V99.                CI0268
            10            K974-NSEQ4  PICTURE  9(4)                     CI0268
                          OCCURS       194     TIMES.                   CI0268
      *
      *LINKAGE FROM ME15 AND MC58 ACCOUNT RULES
      *
      *!WF DSP=QT DSL=QT SEL=58 FOR=I DES=1 LEV=1 PLT=20
       01                 QT58.                                         CI0268
            10            QT58-QT5K.                                    CI0268
            11            QT58-C299.                                    CI0268
            12            QT58-CTID.                                    CI0268
            13            QT58-CTIDA  PICTURE  9(3).                    CI0268
            13            QT58-CTIDN.                                   CI0268
            14            QT58-CTIDNP PICTURE  X(13).                   CI0268
            14            QT58-CTIDND PICTURE  9(11).                   CI0268
            11            QT58-GECKD2 PICTURE  9.                       CI0268
            11            QT58-NSEQ5  PICTURE  9(5).                    CI0268
            11            QT58-CTSTA  PICTURE  99.                      CI0268
            11            QT58-CTSTAL PICTURE  X(10).                   CI0268
            11            QT58-CTOWN  PICTURE  9(3).                    CI0268
            11            QT58-CTTLN1 PICTURE  X(30).                   CI0268
            11            QT58-CTTLN2 PICTURE  X(30).                   CI0268
            11            QT58-CTTLN3 PICTURE  X(30).                   CI0268
            11            QT58-CTTBO1 PICTURE  X(45).                   CI0268
            11            QT58-CTTBO2 PICTURE  X(45).                   CI0268
            11            QT58-CTEFD  PICTURE  9(8).                    CI0268
            11            QT58-CTIAD  PICTURE  9(8).                    CI0268
            11            QT58-CTCUS  PICTURE  999.                     CI0268
            11            QT58-GR98.                                    CI0268
            12            QT58-GRID.                                    CI0268
            13            QT58-GRIDC  PICTURE  9(3).                    CI0268
            13            QT58-GRIDN.                                   CI0268
            14            QT58-GRIDNP PICTURE  99.                      CI0268
            14            QT58-GRIDND PICTURE  9(8).                    CI0268
            11            QT58-CQACT  PICTURE  999.                     CI0268
            11            QT58-CTCCI  PICTURE  X.                       CI0268
            11            QT58-CIRAS  PICTURE  999.                     CI0268
            11            QT58-CIRAT  PICTURE  999.                     CI0268
            11            QT58-IACVD  PICTURE  X.                       CI0268
            11            QT58-FILLER PICTURE  X(4).                    CI0268
            11            QT58-PRCODA PICTURE  X(5).                    CI0268
            11            QT58-PRCMN  PICTURE  X(20).                   CI0268
            11            QT58-MRPLN  PICTURE  X(30).                   CI0268
            11            QT58-CPRDG  PICTURE  9(2).                    CI0268
            11            QT58-CPRDA1 PICTURE  9(3).                    CI0268
            11            QT58-PRSCD  PICTURE  X(9).                    CI0268
            11            QT58-MSP03  PICTURE  X(3).                    CI0268
            11            QT58-CGRLI  PICTURE  X.                       CI0268
            11            QT58-ITERM  PICTURE  X(1).                    CI0268
            11            QT58-IVARP  PICTURE  X.                       CI0268
            11            QT58-DVALU  PICTURE  9(8).                    CI0268
            11            QT58-AACTV  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ACCTVC PICTURE  X(20).                   CI0268
            11            QT58-ITXTI  PICTURE  X.                       CI0268
            11            QT58-ASANP  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ACINV  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CELBL  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-NMESS2 PICTURE  S9(6)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-FILLER PICTURE  X(1).                    CI0268
            11            QT58-PRCLN  PICTURE  X(60).                   CI0268
            11            QT58-GECKD  PICTURE  9.                       CI0268
            11            QT58-MPLNA  PICTURE  X(19).                   CI0268
            11            QT58-CQACTL PICTURE  X(45).                   CI0268
            11            QT58-CRQPA  PICTURE  9(3).                    CI0268
            11            QT58-IVANT  PICTURE  X(1).                    CI0268
            11            QT58-IDBRP  PICTURE  X(1).                    CI0268
            11            QT58-IANPY  PICTURE  X.                       CI0268
            11            QT58-IVARP1 PICTURE  X.                       CI0268
            11            QT58-FILLER PICTURE  X(27).                   CI0268
            11            QT58-NSEQ2A PICTURE  S9(3)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-NSEQ2P PICTURE  S9(3)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-MRPSN  PICTURE  X(12).                   CI0268
            11            QT58-GEHCD  PICTURE  9(3)                     CI0268
                          OCCURS       002     TIMES.                   CI0268
            11            QT58-GEHCSU PICTURE  9(5)                     CI0268
                          OCCURS       002     TIMES.                   CI0268
            11            QT58-PRCSN  PICTURE  X(9).                    CI0268
            11            QT58-CGRMF  PICTURE  X.                       CI0268
            11            QT58-IGFEX  PICTURE  X.                       CI0268
            11            QT58-CLIDP  PICTURE  X(23).                   CI0268
            11            QT58-CLCTRC PICTURE  9(3).                    CI0268
            11            QT58-ADINP  PICTURE  X(20).                   CI0268
            11            QT58-CLCTRA PICTURE  9(3).                    CI0268
            11            QT58-GRPLC  PICTURE  99.                      CI0268
            11            QT58-CIDRP  PICTURE  99.                      CI0268
            11            QT58-FILLER PICTURE  X(01).                   CI0268
            11            QT58-AVMTOT PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AVCSH  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AMARC  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AVLMX  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AVLMN  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-INDRS  PICTURE  X.                       CI0268
            11            QT58-MPRN4  PICTURE  X(35).                   CI0268
            11            QT58-FILLER PICTURE  X(1).                    CI0268
            11            QT58-ACVALM PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-INDRSA PICTURE  X(2).                    CI0268
            11            QT58-DXTMSA PICTURE  X(26).                   CI0268
            11            QT58-NMESS6 PICTURE  S9(6)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-NMESS7 PICTURE  S9(6)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-IBIDSA PICTURE  X.                       CI0268
            11            QT58-IBIDSB PICTURE  X.                       CI0268
            11            QT58-INSPOS PICTURE  X.                       CI0268
            11            QT58-INSPOD PICTURE  X.                       CI0268
            11            QT58-ACBALX PICTURE  X(20).                   CI0268
            11            QT58-AINVMX PICTURE  X(20).                   CI0268
            11            QT58-AMARCX PICTURE  X(20).                   CI0268
            11            QT58-AVMTOX PICTURE  X(20).                   CI0268
            11            QT58-IMNPR  PICTURE  X.                       CI0268
            11            QT58-ISSPL  PICTURE  X.                       CI0268
            11            QT58-AVMTOI PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AVCSHI PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-APOSC  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AVLMXI PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AVLMN1 PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AVLMN2 PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-FILLER PICTURE  X(05).                   CI0268
            10            QT58-QT5A.                                    CI0268
            11            QT58-CLID   PICTURE  X(23).                   CI0268
            11            QT58-GECKD1 PICTURE  9.                       CI0268
            11            QT58-MCLNM  PICTURE  X(40).                   CI0268
            11            QT58-MCLNM2 PICTURE  X(40).                   CI0268
            11            QT58-CLTYP  PICTURE  X.                       CI0268
            11            QT58-CLDOB  PICTURE  9(8).                    CI0268
            11            QT58-CLDTH  PICTURE  X.                       CI0268
            11            QT58-CLTIN  PICTURE  9(12).                   CI0268
            11            QT58-CLTINC PICTURE  9.                       CI0268
            11            QT58-GESAD1 PICTURE  X(30).                   CI0268
            11            QT58-GESAD2 PICTURE  X(30).                   CI0268
            11            QT58-GESAD3 PICTURE  X(30).                   CI0268
            11            QT58-GECIT  PICTURE  X(25).                   CI0268
            11            QT58-GECTRY PICTURE  X(20).                   CI0268
            11            QT58-GEPCD  PICTURE  X(12).                   CI0268
            11            QT58-GEST   PICTURE  X(8).                    CI0268
            11            QT58-GEADS  PICTURE  9.                       CI0268
            11            QT58-GECSD  PICTURE  9(8).                    CI0268
            11            QT58-QCLAGE PICTURE  9(3)V9                   CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-FILLER PICTURE  X(06).                   CI0268
            10            QT58-QT5T.                                    CI0268
            11            QT58-ATFRA  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AGOFD  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-APRMX  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-APRMN  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-IOWNC  PICTURE  X.                       CI0268
            11            QT58-COWNF  PICTURE  X(30).                   CI0268
            11            QT58-CTYPE  PICTURE  X.                       CI0268
            11            QT58-CIRAC  PICTURE  X(5).                    CI0268
            11            QT58-CTXMT  PICTURE  9(2).                    CI0268
            11            QT58-AMIND  PICTURE  S9(7)V99.                CI0268
            11            QT58-AMAXAR PICTURE  S9(7)V99.                CI0268
            11            QT58-QSHOWQ PICTURE  S9(9)V999                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-QSHOW0 PICTURE  S9(10)V999               CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-PPOT1  PICTURE  S9(3)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-PACT1  PICTURE  S999V999                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-IPRTA  PICTURE  X.                       CI0268
            11            QT58-FILLER PICTURE  X.                       CI0268
            11            QT58-CLCUS  PICTURE  99.                      CI0268
            11            QT58-CCDSCW PICTURE  9(2).                    CI0268
            11            QT58-CCACT  PICTURE  99.                      CI0268
            11            QT58-CIRAG.                                   CI0268
            12            QT58-CIRAP  PICTURE  XX                       CI0268
                          OCCURS       010     TIMES.                   CI0268
            11            QT58-ITERF  PICTURE  X.                       CI0268
            11            QT58-IACFPD PICTURE  X(1).                    CI0268
            11            QT58-AFEET  PICTURE  S9(5)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ATERF  PICTURE  S9(5)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CLIDNB PICTURE  9(8).                    CI0268
            11            QT58-ALOAD  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASURR  PICTURE  S9(07)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASHIS  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AMNBL  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-APNAC  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ANGOF  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CPLTYP PICTURE  X(14).                   CI0268
            10            QT58-QT5N.                                    CI0268
            11            QT58-IARRAN PICTURE  X.                       CI0268
            11            QT58-GESTD1 PICTURE  9(8).                    CI0268
            11            QT58-GEEND1 PICTURE  S9(8)                    CI0268
                          BINARY.                                       CI0268
            11            QT58-GESTD  PICTURE  9(8).                    CI0268
            11            QT58-GEEND  PICTURE  9(8).                    CI0268
            11            QT58-NSQ4B2 PICTURE  9(8)                     CI0268
                          BINARY.                                       CI0268
            11            QT58-CDEST  PICTURE  99.                      CI0268
            11            QT58-DEFFT  PICTURE  9(8).                    CI0268
            11            QT58-CPMTF  PICTURE  99.                      CI0268
            11            QT58-CPMTG  PICTURE  99.                      CI0268
            11            QT58-MPMTFL PICTURE  X(24).                   CI0268
            11            QT58-MPMTFE PICTURE  X(24).                   CI0268
            11            QT58-DLAUP  PICTURE  9(8).                    CI0268
            11            QT58-NSEQ4B PICTURE  9(8)                     CI0268
                          BINARY.                                       CI0268
            11            QT58-QSACTF PICTURE  9(3).                    CI0268
            11            QT58-QSACTT PICTURE  9(3).                    CI0268
            11            QT58-CCONF  PICTURE  X(25).                   CI0268
            11            QT58-DCONF  PICTURE  9(8).                    CI0268
            11            QT58-DTIMT  PICTURE  X(8).                    CI0268
            11            QT58-CACTS  PICTURE  X.                       CI0268
            11            QT58-ADBRQ  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-DNPMT  PICTURE  9(8).                    CI0268
            11            QT58-NAPDS  PICTURE  S9(3)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CDEST1 PICTURE  99.                      CI0268
            11            QT58-CLANR1 PICTURE  X(23).                   CI0268
            11            QT58-FILLER PICTURE  X(01).                   CI0268
            10            QT58-FILLER PICTURE  X(600).                  CI0268
            10            QT58-QT5C                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-CESLD  PICTURE  9(8).                    CI0268
            11            QT58-PCIRB5 PICTURE  S9(3)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-PANYDD PICTURE  S9(3)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CEIT   PICTURE  9(3).                    CI0268
            11            QT58-PPART  PICTURE  9(3)V99                  CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-DTRME  PICTURE  9(8).                    CI0268
            11            QT58-CEIRND PICTURE  9(8).                    CI0268
            11            QT58-DANNIA PICTURE  9(8).                    CI0268
            11            QT58-AAPAA  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CELBDT PICTURE  9(8).                    CI0268
            11            QT58-CEIIS  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-DTRME1 PICTURE  9(8).                    CI0268
            11            QT58-GMKTS.                                   CI0268
            12            QT58-DTRME2 PICTURE  9(8)                     CI0268
                          OCCURS       005     TIMES.                   CI0268
            12            QT58-DTRME3 PICTURE  9(8)                     CI0268
                          OCCURS       005     TIMES.                   CI0268
            11            QT58-ALINT  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CEHCD  PICTURE  9(3)                     CI0268
                          OCCURS       006     TIMES.                   CI0268
            11            QT58-CEFOTR PICTURE  S9(3)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-DGPED  PICTURE  9(8).                    CI0268
            11            QT58-DIPED  PICTURE  9(8).                    CI0268
            11            QT58-FILLER PICTURE  X(409).                  CI0268
            10            QT58-QT5F                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-DLAUP2 PICTURE  9(8).                    CI0268
            11            QT58-QSHOW  PICTURE  S9(10)V999               CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AFAVP  PICTURE  S9(4)V9(3)               CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-QSHIS  PICTURE  S9(10)V999               CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-QSHNM  PICTURE  S9(10)V999.              CI0268
            11            QT58-QSHOM  PICTURE  S9(10)V999               CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ADDAC  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-QSHES  PICTURE  S9(10)V999               CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-NDCUS  PICTURE  X(9).                    CI0268
            11            QT58-CSTKR5 PICTURE  X(5).                    CI0268
            11            QT58-NACID  PICTURE  S9(11)                   CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AGOFD2 PICTURE  S9(9)V99.                CI0268
            11            QT58-TCBAT  PICTURE  X(21).                   CI0268
            11            QT58-FILLER PICTURE  X(490).                  CI0268
            10            QT58-QT5L                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-ALDBEN PICTURE  S9(09)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-APREL  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ALMODE PICTURE  99.                      CI0268
            11            QT58-ITMEC  PICTURE  X(1).                    CI0268
            11            QT58-ITAMR  PICTURE  X(1).                    CI0268
            11            QT58-MPMTF  PICTURE  X(14).                   CI0268
            11            QT58-TPLNL  PICTURE  X(30).                   CI0268
            11            QT58-ASBENA PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASBENB PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASBENC PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASBENE PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASBENF PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-GESTNS PICTURE  X(2).                    CI0268
            11            QT58-CTWHPB PICTURE  9(3)V999.                CI0268
            11            QT58-CTWHCB PICTURE  X.                       CI0268
            11            QT58-AMVA1  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASPAM  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ACTCH  PICTURE  S9(07)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AMXLN  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ALFGH  PICTURE  999.                     CI0268
            11            QT58-ALPLNI PICTURE  9.                       CI0268
            11            QT58-ATSA8  PICTURE  S9(07)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CVALB  PICTURE  X(3).                    CI0268
            11            QT58-ASURRN PICTURE  S9(07)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ASURRW PICTURE  S9(07)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ATLTB  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AEARN0 PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ATFPI  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-AEARN1 PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ISELO  PICTURE  X.                       CI0268
            11            QT58-CCLAC  PICTURE  X.                       CI0268
            11            QT58-ALINNO PICTURE  99.                      CI0268
            11            QT58-ALPLNJ PICTURE  9.                       CI0268
            11            QT58-COLPL  PICTURE  9(05).                   CI0268
            11            QT58-ALPLDT PICTURE  9(8).                    CI0268
            11            QT58-ANFMC  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CPNOP  PICTURE  X(2).                    CI0268
            11            QT58-CVSTC  PICTURE  X(4).                    CI0268
            11            QT58-CGMBR  PICTURE  X.                       CI0268
            11            QT58-DWSDT  PICTURE  9(8).                    CI0268
            11            QT58-IRDPH  PICTURE  X.                       CI0268
            11            QT58-DWAIT  PICTURE  9(8).                    CI0268
            11            QT58-IAPGP  PICTURE  X.                       CI0268
            11            QT58-CASTA  PICTURE  X.                       CI0268
            11            QT58-CSSUP2 PICTURE  X.                       CI0268
            11            QT58-CVOMC1 PICTURE  X(1).                    CI0268
            11            QT58-APGBP  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ALDDUE PICTURE  9(08).                   CI0268
            11            QT58-APYMT  PICTURE  S9(9)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ALSURR PICTURE  S9(09)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CESTP  PICTURE  X(03).                   CI0268
            11            QT58-FILLER PICTURE  X(356).                  CI0268
            10            QT58-QT5O                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-NBACT  PICTURE  S9(11)                   CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CTIAC  PICTURE  S9(3)                    CI0268
                          BINARY.                                       CI0268
            11            QT58-CASTT  PICTURE  S99                      CI0268
                          BINARY.                                       CI0268
            11            QT58-CATMI  PICTURE  S9                       CI0268
                          BINARY.                                       CI0268
            11            QT58-IATMR  PICTURE  X(3).                    CI0268
            11            QT58-IBIPI  PICTURE  X.                       CI0268
            11            QT58-CBPST  PICTURE  S99                      CI0268
                          BINARY.                                       CI0268
            11            QT58-TBPST  PICTURE  X(16).                   CI0268
            11            QT58-CODPI  PICTURE  X.                       CI0268
            11            QT58-TODPS  PICTURE  X(9).                    CI0268
            11            QT58-FILLER PICTURE  X(448).                  CI0268
            11            QT58-IBPSD  PICTURE  X.                       CI0268
            11            QT58-FILLER PICTURE  X(107).                  CI0268
            11            QT58-QT5E                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            12            QT58-MPRN4X PICTURE  X(100).                  CI0268
            12            QT58-CCMSH  PICTURE  X(2).                    CI0268
            12            QT58-CPRCS  PICTURE  X(04).                   CI0268
            12            QT58-CURST  PICTURE  X.                       CI0268
            10            QT58-QT5M                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-NAPCN1 PICTURE  X(24).                   CI0268
            11            QT58-FILLER PICTURE  X(576).                  CI0268
            10            QT58-QT5B                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-NAPCN2 PICTURE  X(24).                   CI0268
            11            QT58-CTIDAL PICTURE  X(40).                   CI0268
            11            QT58-NPHNS  PICTURE  X(14).                   CI0268
            11            QT58-FILLER PICTURE  X(522).                  CI0268
            10            QT58-QT5P                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-CFPPT  PICTURE  9(3).                    CI0268
            11            QT58-TTYPP  PICTURE  X(40).                   CI0268
            11            QT58-CPPST  PICTURE  9(3).                    CI0268
            11            QT58-TPPST  PICTURE  X(15).                   CI0268
            11            QT58-APFEEQ PICTURE  S9(7)V99.                CI0268
            11            QT58-APFEEC PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-APFEEP PICTURE  S9(7)V99.                CI0268
            11            QT58-ISVCA  PICTURE  X.                       CI0268
            11            QT58-NSBVS  PICTURE  X(5).                    CI0268
            11            QT58-ICKRV  PICTURE  X.                       CI0268
            11            QT58-PDAMT  PICTURE  S9(03).                  CI0268
            11            QT58-PSTAX  PICTURE  S9(03)V999.              CI0268
            11            QT58-DPCAL  PICTURE  9(8).                    CI0268
            11            QT58-NADVF  PICTURE  X(08).                   CI0268
            11            QT58-DAGUP  PICTURE  9(8).                    CI0268
            11            QT58-AANFEA PICTURE  9(5)V99                  CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-CLIDN7 PICTURE  9(8)                     CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-ARANV  PICTURE  S9(7)V99                 CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            QT58-DRANV  PICTURE  9(8).                    CI0268
            11            QT58-FILLER PICTURE  X(454).                  CI0268
            10            QT58-QT50                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-NANCA  PICTURE  X(30).                   CI0268
            11            QT58-MANCN  PICTURE  X(100).                  CI0268
            11            QT58-AINPTX PICTURE  X(20).                   CI0268
            11            QT58-CTID01 PICTURE  X(27).                   CI0268
            11            QT58-NANCA1 PICTURE  X(04).                   CI0268
            11            QT58-IIVAR  PICTURE  X(1).                    CI0268
            11            QT58-FILLER PICTURE  X(418).                  CI0268
            10            QT58-QT5R                                     CI0268
                          REDEFINES            QT58-FILLER.             CI0268
            11            QT58-NACTJ  PICTURE  X(04).                   CI0268
            11            QT58-NACNO6 PICTURE  X(11).                   CI0268
            11            QT58-FILLER PICTURE  X(585).                  CI0268
            10            QT58-AMAXA  PICTURE  S9(7)V99.                CI0268
            10            QT58-ISAOR  PICTURE  X.                       CI0268
            10            QT58-ISACH  PICTURE  X.                       CI0268
            10            QT58-CERRBA PICTURE  X(02).                   CI0268
            10            QT58-CERRBH PICTURE  X(02).                   CI0268
            10            QT58-IWITHH PICTURE  X.                       CI0268
            10            QT58-CTID20 PICTURE  X(27).                   CI0268
            10            QT58-GECKD3 PICTURE  9.                       CI0268
            10            QT58-DANFC  PICTURE  X(10).                   CI0268
            10            QT58-DAFCN  PICTURE  X(10).                   CI0268
            10            QT58-ISMTA  PICTURE  X.                       CI0268
            10            QT58-CERRBT PICTURE  X(02).                   CI0268
            10            QT58-NPLNI  PICTURE  X(10).                   CI0268
            10            QT58-FILLER PICTURE  X(023).                  CI0268
      *
      *MISCELLANEOUS INFO NEEDED FOR BUIL CI0272
      *!WF DSP=M9 DSL=K9 SEL=77 FOR=I DES=1 LEV=1 PLT=20
       01                 M977.                                         CI0268
            10            M977-ICUST  PICTURE  X.                       CI0268
            10            M977-GEAUN  PICTURE  9(5).                    CI0268
            10            M977-GEOPD2 PICTURE  X(8).                    CI0268
            10            M977-ADBRQ  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            M977-AEDRQ  PICTURE  S9(11)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            M977-ALINNO PICTURE  99.                      CI0268
            10            M977-IANTF  PICTURE  XX.                      CI0268
            10            M977-GETIM  PICTURE  S9(7)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            M977-DEFFT  PICTURE  9(8).                    CI0268
            10            M977-ISUBA1 PICTURE  X.                       CI0268
            10            M977-ISUBA2 PICTURE  X.                       CI0268
            10            M977-ALLNB  PICTURE  S9(07)V99                CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            M977-DCACG  PICTURE  9(8).                    CI0268
            10            M977-CRSNG1 PICTURE  X(02).                   CI0268
            10            M977-CRSNG2 PICTURE  X(02).                   CI0268
            10            M977-IFTDY1 PICTURE  X.                       CI0268
            10            M977-IFXFD  PICTURE  X.                       CI0268
            10            M977-FILLER PICTURE  X(99).                   CI0268
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0268
          05              DE00-SUITE.                                   CI0268
            15       FILLER         PICTURE  X(00653).                  CI0268
       01                 DE10  REDEFINES      DE00.                    CI0268
            10            DE10-DU11.                                    CI0268
            11            DE10-XFONC  PICTURE  X(4).                    CI0268
            11            DE10-MPSBN  PICTURE  X(8).                    CI0268
            11            DE10-XDBDNM PICTURE  X(08).                   CI0268
            11            DE10-XSEGNM PICTURE  X(08).                   CI0268
            11            DE10-XRC    PICTURE  X(02).                   CI0268
            11            DE10-MSEG   PICTURE  X(08).                   CI0268
            11            DE10-XCOKEY PICTURE  X(70).                   CI0268
            11            DE10-CUIBR  PICTURE  X(01).                   CI0268
            11            DE10-CUIBA  PICTURE  X(01).                   CI0268
            11            DE10-IPBIK  PICTURE  X(1).                    CI0268
            10            DE10-DU03.                                    CI0268
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            DE10-CMSSF  PICTURE  XX.                      CI0268
            11            DE10-DU09.                                    CI0268
            12            DE10-CMESA  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            12            DE10-CMESB  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            12            DE10-CMSST  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            12            DE10-QELLAA PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            12            DE10-TMESS4 PICTURE  X(512).                  CI0268
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0268
          05              MS00-SUITE.                                   CI0268
            15       FILLER         PICTURE  X(00542).                  CI0268
       01                 MS03  REDEFINES      MS00.                    CI0268
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            10            MS03-CMSSF  PICTURE  XX.                      CI0268
            10            MS03-DU09.                                    CI0268
            11            MS03-CMESA  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            11            MS03-CMESB  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            11            MS03-CMSST  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            11            MS03-QELLAA PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
            11            MS03-TMESS4 PICTURE  X(512).                  CI0268
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0268
            10            MX11-QMSGS  PICTURE  9(03).                   CI0268
            10            MX11-PJ09                                     CI0268
                          OCCURS       025     TIMES.                   CI0268
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0268
                          COMPUTATIONAL-3.                              CI0268
            11            MX11-CMESB  PICTURE  S9(9)                    CI0268
                          BINARY.                                       CI0268
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                K974
                                QT58
                                M977
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0268
      *               *                                   *             CI0268
      *               *INITIALISATIONS                    *             CI0268
      *               *                                   *             CI0268
      *               *************************************.            CI0268
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
      *N02FG.    NOTE *********************************   *.            ACMCTI
       F02FG.                                                           lv10
      ** SUB-FUNCTION TO PERFORM A    *                                 ACMCTI
      ** DUMMY DB2 CALL.              *                                 ACMCTI
      *********************************                                 ACMCTI
      *SET
      * :WS00-DATE = CURRENT_DATE
      *F93SQ
       F02FG-FN. EXIT.
      *N02SC.    NOTE *SET ADDRESS FOR DATABASES          *.
       F02SC.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ARAY                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-ARAY-PTR1.                                          ADU015
       F02SC-FN. EXIT.
       F02-FN.   EXIT.
      *N03.      NOTE *************************************.
      *               *                                   *
      *               *LOAD MISCELLANEOUS VARIABLES       *
      *               *                                   *
      *               *************************************.
       F03.           EXIT.                                             lv05
      *N03DD.    NOTE *CALC TOTAL NUMBER OF SUB ACCTS     *.
       F03DD.                                                           lv10
           COMPUTE     WS01-QSACT = K974-QSACTF +
           K974-QSACTT.
       F03DD-FN. EXIT.
      *N03FF.    NOTE *INITIALISE WORK FIELDS             *.
       F03FF.                                                           lv10
           MOVE        0 TO M977-ADBRQ
           M977-AEDRQ.
       F03FF-FN. EXIT.
      *N03GF.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F03GF.                                                           lv10
      *                                                                 AM0020
      *********************************                                 AM0020
      ** THIS MODULE WILL READ THE    *                                 AM0020
      ** CAMS ACCOUNTING DATE VSAM    *                                 AM0020
      ** FILE(CAMNMSTK) AND THE CAMS  *                                 AM0020
      ** DATE RECORD.                 *                                 AM0020
      *********************************                                 AM0020
      *                                                                 AM0020
           CALL        CI0020 USING                                     AM0020
           DFHEIBLK                                                     AM0020
           DFHCOMMAREA                                                  AM0020
           NS20                                                         AM0020
           MS03.                                                        AM0020
       F03GF-FN. EXIT.
       F03-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0268
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0268
      *               *                                   *             CI0268
      *               *FIN DE TRAITEMENT                  *             CI0268
      *               *                                   *             CI0268
      *               *************************************.            CI0268
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0268
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *SET THE PRODUCT CODE AND CVSYS     *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35EC.    NOTE *POPULATE THE PRODUCT CODE TO USE   *.
       F35EC.                                                           lv10
      *THE FLAGS
       F35EC-FN. EXIT.
      *N35LF.    NOTE *DETERMINE CVSYS                    *.
       F35LF.                                                           lv10
           MOVE        QT58-CTIDA TO TA5B-CTIDA
           MOVE        QT58-PRCODA TO TA5B-PRCOD
           MOVE        SPACES TO TA5B-PRSCD
           PERFORM     F92TA THRU F92TA-FN.
       F35LF-FN. EXIT.
      *N35OF.    NOTE *DETERMINE PRODUCT TYPE             *.
       F35OF.                                                           lv10
           INITIALIZE  TB8A
           MOVE        QT58-CTIDA TO TB8A-CTIDA
           MOVE        QT58-PRCODA TO TB8A-PRCOD
           PERFORM     F92TB THRU F92TB-FN.
       F35OF-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *EDIT REQUIREMENTS - INV CHANGE     *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BB.    NOTE *READ THROUGH INVESTMENT CHANGE     *.
       F40BB.    IF    K974-CAFLOW = 'IC'                               lv07
                 NEXT SENTENCE ELSE GO TO     F40BB-FN.
      *N40DD.    NOTE *READ THROUGH SUB ACCOUNTS          *.
       F40DD.                       GO TO     F40DD-B.                  lv10
       F40DD-A.
                 IF    WS01-SUB > WS01-QSACT
                                    GO TO     F40DD-FN.
       F40DD-B.       EXIT.
      *N40EJ.    NOTE *EDIT FROM ACCOUNTS                 *.
       F40EJ.    IF    K974-CACCT (WS01-SUB) = 'F'                      lv20
                 NEXT SENTENCE ELSE GO TO     F40EJ-FN.
           ADD         1 TO WS01-NO-FROM
           MOVE        K974-CFIDC (WS01-SUB) TO
           FR01-CFIDC (WS01-NO-FROM).
      *N40FF.    NOTE *IF PERCENT WAS ENTERED             *.
       F40FF.    IF    K974-PACT1 (WS01-SUB) > 0                        lv25
                 NEXT SENTENCE ELSE GO TO     F40FF-FN.
           COMPUTE     K974-AEDRQ (WS01-SUB) ROUNDED
           = K974-AACTV (WS01-SUB)
           * K974-PACT1 (WS01-SUB) / 100
           ADD         K974-AEDRQ (WS01-SUB) TO
           WS01-FROM-AEDRQ
           M977-AEDRQ
           MOVE        'Y' TO WS01-FROM-PER-IND.
                 IF    K974-PACT1 (WS01-SUB) < 100                      DOT
           MOVE        'N' TO WS01-ALL-IND.
                 IF    K974-PACT1 (WS01-SUB) > 100                      DOT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013738 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FF-FN. EXIT.
      *N40FQ.    NOTE *IF $ AMOUNT WAS ENTERED            *.
       F40FQ.    IF    K974-ADBRQ (WS01-SUB) > 0                        lv25
                 NEXT SENTENCE ELSE GO TO     F40FQ-FN.
           MOVE        K974-ADBRQ (WS01-SUB) TO
           K974-AEDRQ (WS01-SUB)
           ADD         K974-ADBRQ (WS01-SUB) TO
           WS01-FROM-ADBRQ
           M977-ADBRQ
           WS01-FROM-AEDRQ
           M977-AEDRQ
           MOVE        'Y' TO WS01-FROM-DOL-IND.
                 IF    K974-ADBRQ (WS01-SUB) >                          DOT
                       K974-AACTV (WS01-SUB)
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013485 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FQ-FN. EXIT.
      *N40FS.    NOTE *IF A TRANSFER IS ALLOWED OUT OF    *.
       F40FS.    IF    M977-ISUBA1 = 'Y'                                lv25
                 AND   K974-AEDRQ (WS01-SUB) NOT <
                       100000
                 NEXT SENTENCE ELSE GO TO     F40FS-FN.
      *A SUB ACCOUNT. CHECK THAT IT
      *NOT > THAN THE MAXIMUM ALLOWED
      *
      *---> Send  Message                                               ADU119
      *      and                                                        ADU119
           MOVE        014601 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40FS-FN. EXIT.
      *N40GE.    NOTE *CHECK FIXED                        *.
       F40GE.    IF    K974-CFIDC (WS01-SUB) = 'FA'                     lv25
                 OR    'FIXED' OR 'FIX'
                 NEXT SENTENCE ELSE GO TO     F40GE-FN.
           MOVE        'FV' TO M977-IANTF.
      *N40GG.    NOTE *IF A TRANSFER IS NOT ALLOWED OUT   *.
       F40GG.    IF    M977-ISUBA1 = 'N'                                lv30
                 NEXT SENTENCE ELSE GO TO     F40GG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013836 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GG-FN. EXIT.
      *N40GM.    NOTE *IF A TRANSFER IS ALLOWED OUT OF    *.
       F40GM.    IF    M977-ISUBA1 = 'Y'                                lv30
                 AND   M977-ALLNB < 0
                 AND   TB8A-CLIAN = 02
                 NEXT SENTENCE ELSE GO TO     F40GM-FN.
      *A FIXED ACCOUNT. CHECK THAT IT
      *NOT > THAN THE MAXIMIM ALLOWED
      *FOR ANNUITIES
           COMPUTE     K974-AMAXD (WS01-SUB) ROUNDED =
           K974-AACTV (WS01-SUB) -
           (M977-ALLNB * 15 / 100 * -1)
      *
                 IF    K974-AEDRQ (WS01-SUB) >                          DOT
                       K974-AMAXD (WS01-SUB)
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013739 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GM-FN. EXIT.
      *N40GO.    NOTE *CALL LATE TRADING MODULE           *.
       F40GO.                                                           lv30
           PERFORM     F99BB THRU F99BB-FN.
       F40GO-FN. EXIT.
      *N40GQ.    NOTE *CHECK IF TIME HAS PASSED 3:00PM    *.
       F40GQ.    IF    M977-IFTDY1 = 'Y'                                lv30
                 AND   M977-GETIM > WS-GETIM6
                 NEXT SENTENCE ELSE GO TO     F40GQ-FN.
      *AND IF IT IS THE 30TH DAY OF THE
      *30 DAY EDIT IN WHICH CASE THE 30
      *DAY EDIT COMES INTO EFFECT
      *BECAUSE IT IS NEXT BUSINESS DAY
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013857 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40GQ-FN. EXIT.
      *N40GT.    NOTE *EDIT AMOUNT FROM FIXED ACCOUNT     *.
       F40GT.    IF    (TB8A-CLAST = 205                                lv30
                 OR    TB8A-CLAST = 202
                 OR    TB8A-CLAST = 203
                 OR    TB8A-CLAST = 204
                 OR    TB8A-CLAST = 206
                 OR    TB8A-CLAST = 207)
                 AND   (QT58-CTEFD > 20030701
                 OR    QT58-CTEFD = 20030701)
                 NEXT SENTENCE ELSE GO TO     F40GT-FN.
      *FOR RAVA2 , RAVA-PLUS ,
      *RAVA-SELECT , NEW-ANN-3, CRAVA
      *NEW-ANN-710 PRODUCTS
      *( WITHIN 30-DAY WINDOW, TRANSFER
      *FROM FIXED WILL BE LIMITED TO
      *30% OF THE BEGINNING OF CONTRACT
      *YEAR FIXED ACCOUNT VALUE OR THE
      *DOLLAR AMOUNT TRANSFERRED IN THE
      *PREVIOUS YEAR, WHICHEVER IS
      *GREATER )
      *N40GW.    NOTE *GET STOPGAP AMTS FROM CI0146       *.
       F40GW.                                                           lv35
           INITIALIZE  P948
           MOVE        QT58-CTID TO P948-CTID
           MOVE        QT58-CTEFD TO P948-ALPLDT
           MOVE        M977-DEFFT TO P948-DCACG
           MOVE        K974-MAPPN TO P948-MAPPN
           PERFORM     F91BB THRU F91BB-FN.
      *N40HB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40HB.    IF    (MS03-NMESS2 > ZERO                              lv40
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40HB-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0146 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0146 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40HB-900. GO TO F40HC-FN.
       F40HB-FN. EXIT.
      *N40HC.    NOTE *NO ERRORS                          *.            ADU071
       F40HC.                                                           lv40
           INITIALIZE  MS03.                                            ADU071
       F40HC-FN. EXIT.
       F40GW-FN. EXIT.
      *N40HH.    NOTE *COMPUTE 30% OF BEGINNING OF        *.
       F40HH.                                                           lv35
      *CONTRACT YEAR TOTAL ACCOUNT
      *VALUE
           COMPUTE     WA00-ACVPY = P948-ACVPY * 0.30.
                 IF    WA00-ACVPY > P948-ALSURF                         DOT
      *CHECK WHETHER THE COMPUTED VALUE
      *IS GREATER THAN THE AMOUNT
      *TRANSFERRED IN PREVIOUS YEAR.
      *IF YES POPULATE IT AS MAX AMT
           COMPUTE     WA00-MAX-ACVPY ROUNDED =
           WA00-ACVPY
                 ELSE
      *ELSE POPULATE PREVIOUS YR AMOUNT
      *AS THE MAXIMUM LIMIT.
           COMPUTE     WA00-MAX-ACVPY ROUNDED =
           P948-ALSURF.
       F40HH-FN. EXIT.
      *N40HK.    NOTE *COMPUTE THE MAXIMUN AMOUNT         *.
       F40HK.                                                           lv35
           COMPUTE     WA00-MAX-ACVPY = WA00-MAX-ACVPY
           - P948-ALTTDF.
       F40HK-FN. EXIT.
      *N40HN.    NOTE *GET DCA AMT FROM CI0500            *.
       F40HN.                                                           lv35
      *>>>>>>>>> CALL  CI0500 <<<<<<<<<
           MOVE        QT58-CTID TO P949-CTID
           MOVE        P948-ALPLDT TO P949-DCACG
           PERFORM     F91CB THRU F91CB-FN.
      *N40HO.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40HO.    IF    (MS03-NMESS2 > ZERO                              lv40
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40HO-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0500 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0500 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40HO-900. GO TO F40HP-FN.
       F40HO-FN. EXIT.
      *N40HP.    NOTE *NO ERRORS                          *.            ADU071
       F40HP.                                                           lv40
           INITIALIZE  MS03.                                            ADU071
       F40HP-FN. EXIT.
       F40HN-FN. EXIT.
      *N40IQ.    NOTE *SUBTRACT THE DCA AMT FROM CI0500   *.
       F40IQ.                                                           lv35
      *COMPUTE THE MAX AMOUNT
           COMPUTE     WA00-MAX-ACVPY = WA00-MAX-ACVPY
           - P949-ADBRQF.
       F40IQ-FN. EXIT.
      *N40IT.    NOTE *CALCULATE THE ACTUAL ACCOUNT       *.
       F40IT.    IF    WS01-FROM-PER-IND = 'Y'                          lv35
                 NEXT SENTENCE ELSE GO TO     F40IT-FN.
      *VALUE IF PERCENT ARE SELECTED
           COMPUTE     WA00-ACT-AACTV ROUNDED =
           K974-AACTV (WS01-SUB) *
           K974-PACT1 (WS01-SUB) / 100.
      *N40IW.    NOTE *IF THE ACTUAL AMOUNT EXCEEDS THE   *.
       F40IW.    IF    WA00-ACT-AACTV >                                 lv40
                       WA00-MAX-ACVPY
                 NEXT SENTENCE ELSE GO TO     F40IW-FN.
      *30% LIMIT, THEN ERROR
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014437 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40IW-FN. EXIT.
       F40IT-FN. EXIT.
      *N40JB.    NOTE *IF DOLLAR IS SELECTED, CHECK THE   *.
       F40JB.    IF    WS01-FROM-DOL-IND = 'Y'                          lv35
                 NEXT SENTENCE ELSE GO TO     F40JB-FN.
      *30% LIMIT
      *N40JD.    NOTE *CHECK IF THE AMOUNT EXCEEDS THE    *.
       F40JD.    IF    K974-AEDRQ (WS01-SUB) >                          lv40
                       WA00-MAX-ACVPY
                 NEXT SENTENCE ELSE GO TO     F40JD-FN.
      *30% LIMIT, IF DOLLAR IS SELECTED
      *AS MODE OF DIBURSEMENT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014437 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40JD-FN. EXIT.
       F40JB-FN. EXIT.
       F40GT-FN. EXIT.
       F40GE-FN. EXIT.
      *N40NB.    NOTE *CHECK MINIMUM                      *.
       F40NB.    IF    K974-AEDRQ (WS01-SUB) < 50                       lv25
                 AND   K974-IALLV (WS01-SUB) = 'N'
                 AND   TB8A-CLIAN = 02
                 NEXT SENTENCE ELSE GO TO     F40NB-FN.
      *FOR ANNUITY
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013740 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40NB-FN. EXIT.
       F40EJ-FN. EXIT.
      *N40NE.    NOTE *PROCESS TO ACCOUNTS                *.
       F40NE.    IF    K974-CACCT (WS01-SUB) = 'T'                      lv20
                 NEXT SENTENCE ELSE GO TO     F40NE-FN.
           ADD         1 TO WS01-NO-TO
           MOVE        K974-CFIDC (WS01-SUB) TO
           TO01-CFIDC (WS01-NO-TO).
      *N40NL.    NOTE *IF $ AMOUNT WAS ENTERED            *.
       F40NL.    IF    K974-ADBRQ (WS01-SUB) > 0                        lv25
                 NEXT SENTENCE ELSE GO TO     F40NL-FN.
           ADD         K974-ADBRQ (WS01-SUB) TO
           WS01-TO-ADBRQ
           MOVE        'Y' TO WS01-TO-DOL-IND.
       F40NL-FN. EXIT.
      *N40OE.    NOTE *IF % WAS ENTERED                   *.
       F40OE.    IF    K974-PACT1 (WS01-SUB) > 0                        lv25
                 NEXT SENTENCE ELSE GO TO     F40OE-FN.
           ADD         K974-PACT1 (WS01-SUB) TO
           WS01-TO-PACT1
           MOVE        'Y' TO WS01-TO-PER-IND.
       F40OE-FN. EXIT.
      *N40OL.    NOTE *CHECK IF BAL OF FUNDS IND - ANN    *.
       F40OL.    IF    K974-ITRNB (WS01-SUB) = 'Y'                      lv25
                 AND   TB8A-CLIAN = 02
                 NEXT SENTENCE ELSE GO TO     F40OL-FN.
           MOVE        'Y' TO WS01-ITRNB.
       F40OL-FN. EXIT.
      *N40OP.    NOTE *CHECK FIXED                        *.
       F40OP.    IF    K974-CFIDC (WS01-SUB) = 'FA'                     lv25
                 OR    'FIXED' OR 'FIX'
                 NEXT SENTENCE ELSE GO TO     F40OP-FN.
           MOVE        'VF' TO M977-IANTF
      *
                 IF    TB8A-CLAST = 208                                 DOT
      *TRANSFERS TO THE FIXED ACCOUNT
      *FOR NEW ANN-C FUND IS RESTRICTED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014593 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN                              ADU119
      *
                 IF    M977-ISUBA2 = 'N'                                DOT
      *CANNOT MOVE MONEY INTO A FIXED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013835 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      *N40OR.    NOTE *TRANSFERS TO THE FIXED ACCOUNT     *.
       F40OR.    IF    (TB8A-CLAST = 205                                lv30
                 OR    TB8A-CLAST = 202
                 OR    TB8A-CLAST = 203)
                 AND   (QT58-CTEFD > 20030701
                 OR    QT58-CTEFD = 20030701)
                 NEXT SENTENCE ELSE GO TO     F40OR-FN.
      *(RAVA-SELECT OR RAVA2 OR CRAVA)
      *MUST BE LIMITED FOR RAVA SELECT
      *AND RAVA ADVANTAGE SUCH THAT
      *AFTER THE TRANSFER, THE FIXED
      *VALUE IS NO MORE THAN 30% OF
      *CURRENT TOTAL ACCOUNT VALUE
      *N40OU.    NOTE *IF PERCENT IS ENTERED, COMPUTE     *.
       F40OU.    IF    WS01-TO-PER-IND = 'Y'                            lv35
                 NEXT SENTENCE ELSE GO TO     F40OU-FN.
      *TOTAL FIXED AMOUNT AFTER
      *TRANSFER.
           COMPUTE     WA00-TEMP-AEDRQ ROUNDED =
           WS01-FROM-AEDRQ *
           K974-PACT1 (WS01-SUB) / 100
           + K974-AACTV (WS01-SUB).
       F40OU-FN. EXIT.
      *N40PB.    NOTE *DISBURSING THROUGH DOLLARS, FIND   *.
       F40PB.    IF    WS01-TO-DOL-IND = 'Y'                            lv35
                 NEXT SENTENCE ELSE GO TO     F40PB-FN.
      *THE PERCENTAGE
           COMPUTE     WA00-TEMP-AEDRQ ROUNDED =
           K974-AEDRQ (WS01-SUB) +
           K974-AACTV (WS01-SUB).
       F40PB-FN. EXIT.
      *N40PC.    NOTE *CALCULATE TOTAL FIXED %            *.
       F40PC.                                                           lv35
           COMPUTE     WA00-TEMP-PACT1 ROUNDED =
           WA00-TEMP-AEDRQ /
           QT58-AACTV * 100.
      *N40PE.    NOTE *IF THE TOTAL PERCENT EXCEEDS 30,   *.
       F40PE.    IF    WA00-TEMP-PACT1 > 30                             lv40
                 NEXT SENTENCE ELSE GO TO     F40PE-FN.
      *THEN ERROR
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014434 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40PE-FN. EXIT.
       F40PC-FN. EXIT.
       F40OR-FN. EXIT.
       F40OP-FN. EXIT.
      *N40RC.    NOTE *IF VANTAGE ACCOUNT AND             *.
       F40RC.    IF    QT58-IVANT = 'Y'                                 lv25
                 AND   K974-CFIDC (WS01-SUB) >=
                       'G01  '
                 AND   K974-CFIDC (WS01-SUB) <=
                       'G10  '
                 AND   (K974-CFIDC (WS01-SUB) <
                       'G1A  '
                 OR    K974-CFIDC (WS01-SUB) >
                       'G1Z  ')
                 NEXT SENTENCE ELSE GO TO     F40RC-FN.
      *GPA DESTINATION, EDIT THAT
      *TO AMOUNT WILL BE AT LEAST $1000
      *N40RG.    NOTE *IF PERCENT IS ENTERED, COMPUTE     *.
       F40RG.    IF    WS01-TO-PER-IND = 'Y'                            lv30
                 NEXT SENTENCE ELSE GO TO     F40RG-FN.
      *TOTAL AMOUNT TO GPA FUND
           COMPUTE     WA00-TEMP-AEDRQ ROUNDED =
           WS01-FROM-AEDRQ *
           K974-PACT1 (WS01-SUB) / 100.
       F40RG-FN. EXIT.
      *N40RI.    NOTE *DISBURSING THROUGH DOLLARS, SET    *.
       F40RI.    IF    WS01-TO-DOL-IND = 'Y'                            lv30
                 NEXT SENTENCE ELSE GO TO     F40RI-FN.
      *TOTAL AMOUNT TO GPA FUND
           COMPUTE     WA00-TEMP-AEDRQ ROUNDED =
           K974-AEDRQ (WS01-SUB).
       F40RI-FN. EXIT.
      *N40RM.    NOTE *ERROR IF NOT AT LEAST $1000        *.
       F40RM.    IF    WA00-TEMP-AEDRQ < 1000                           lv30
                 NEXT SENTENCE ELSE GO TO     F40RM-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014514 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40RM-FN. EXIT.
       F40RC-FN. EXIT.
       F40NE-FN. EXIT.
      *N40XO.    NOTE *INCREMENT SUBSCRIPT                *.
       F40XO.                                                           lv15
           ADD         1 TO WS01-SUB.
       F40XO-FN. EXIT.
       F40DD-900. GO TO F40DD-A.
       F40DD-FN. EXIT.
       F40BB-FN. EXIT.
       F40-FN.   EXIT.
      *N41.      NOTE *************************************.
      *               *                                   *
      *               *FURTHER EDIT REQUIREMENTS          *
      *               *                                   *
      *               *************************************.
       F41.           EXIT.                                             lv05
      *N41BB.    NOTE *READ THROUGH INVESTMENT CHANGE     *.
       F41BB.    IF    K974-CAFLOW = 'IC'                               lv10
                 NEXT SENTENCE ELSE GO TO     F41BB-FN.
      *N41EE.    NOTE *TO CANNOT HAVE $ AND % CHOSEN      *.
       F41EE.    IF    WS01-TO-DOL-IND = 'Y'                            lv15
                 AND   WS01-TO-PER-IND = 'Y'
                 NEXT SENTENCE ELSE GO TO     F41EE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013741 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41EE-FN. EXIT.
      *N41FE.    NOTE *THE DOLLAR AMT CANNOT BE > THAN    *.
       F41FE.    IF    WS01-FROM-ADBRQ >                                lv15
                       QT58-AACTV
                 NEXT SENTENCE ELSE GO TO     F41FE-FN.
      *THE TOTAL ACCOUNT VALUE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013485 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41FE-FN. EXIT.
      *N41GE.    NOTE *IF DOLLAR AMT FOR TO AND FROM      *.
       F41GE.    IF    (WS01-FROM-DOL-IND = 'Y'                         lv15
                 AND   WS01-TO-DOL-IND = 'Y')
                 AND   (WS01-FROM-ADBRQ NOT =
                       WS01-TO-ADBRQ)
                 AND   (TB8A-IVINS = 'Y'
                 OR    QT58-PRCODA = 230)
                 NEXT SENTENCE ELSE GO TO     F41GE-FN.
      *THE AMOUNTS MUST BE EQUAL
      *IF VALID-INSURANCE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013742 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41GE-FN. EXIT.
      *N41HE.    NOTE *IF % CHOSEN IT CANNOT BE > 100%    *.
       F41HE.    IF    WS01-TO-PER-IND = 'Y'                            lv15
                 AND   WS01-TO-PACT1 > 100
                 NEXT SENTENCE ELSE GO TO     F41HE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013743 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41HE-FN. EXIT.
      *N41IE.    NOTE *AN ACCOUNT MUST BE CHOSEN TO       *.
       F41IE.    IF    WS01-ITRNB NOT = 'Y'                             lv15
                 AND   TB8A-CLIAN = 02
                 NEXT SENTENCE ELSE GO TO     F41IE-FN.
      *APPLY THE BALANCE OF FUNDS TO
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013744 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41IE-FN. EXIT.
      *N41LE.    NOTE *TOTAL DISB CANNOT BE < 250         *.
       F41LE.    IF    WS01-FROM-AEDRQ < 250.00                         lv15
                 AND   WS01-ALL-IND = 'N'
                 NEXT SENTENCE ELSE GO TO     F41LE-FN.
      *UNLESS A SUB ACCT IS CLEARED
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013763 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F41LE-FN. EXIT.
       F41BB-FN. EXIT.
       F41-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *EDITS FOR ASSET ALOCATION          *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BB.    NOTE *READ THROUGH INVESTMENT CHANGE     *.
       F50BB.    IF    K974-CAFLOW = 'AA'                               lv10
                 NEXT SENTENCE ELSE GO TO     F50BB-FN.
      *N50BF.    NOTE *SET WHEN FIXED HAS VALUE BUT NO    *.
       F50BF.    IF    M977-IFXFD = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50BF-FN.
      *AMOUNT ASSIGNED TO IT
           MOVE        'FV' TO M977-IANTF.
       F50BF-FN. EXIT.
      *N50CC.    NOTE *ASSET ALLOCATION NOT ALLOWED FOR   *.
       F50CC.    IF    (M977-ALINNO = 0 OR 2)                           lv15
                 AND   TA5B-IVANT = 'N'
                 NEXT SENTENCE ELSE GO TO     F50CC-FN.
      *NON INNOVEST ACCOUNTS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013860 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50CC-FN. EXIT.
      *N50DD.    NOTE *READ THROUGH SUB ACCOUNTS          *.
       F50DD.                       GO TO     F50DD-B.                  lv15
       F50DD-A.
                 IF    WS01-SUB > WS01-QSACT
                                    GO TO     F50DD-FN.
       F50DD-B.       EXIT.
      *N50FE.    NOTE *CHECK % NOT > 100                  *.
       F50FE.    IF    K974-PACT1 (WS01-SUB) > 100                      lv20
                 NEXT SENTENCE ELSE GO TO     F50FE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013745 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50FE-FN. EXIT.
      *N50GE.    NOTE *ADD PERCENTAGES                    *.
       F50GE.                                                           lv20
           ADD         K974-PACT1 (WS01-SUB) TO
           WS01-AA-PACT1.
       F50GE-FN. EXIT.
      *N50IF.    NOTE *FIXED ACCOUNT EDITS                *.
       F50IF.    IF    K974-CFIDC (WS01-SUB) = 'FA'                     lv20
                 OR    'FIXED' OR 'FIX'
                 NEXT SENTENCE ELSE GO TO     F50IF-FN.
           MOVE        'FV' TO M977-IANTF.
      *N50II.    NOTE *IF A TRANSFER IS NOT ALLOWED OUT   *.
       F50II.    IF    M977-ISUBA1 = 'N'                                lv25
                 AND   (K974-PACT1 (WS01-SUB) <
                       K974-PFNDV (WS01-SUB))
                 NEXT SENTENCE ELSE GO TO     F50II-FN.
      *OF THE FIXED ACCOUNT. THE % DISB
      *CANNOT BE GREATER THAN THE
      *ORIGINAL % VALUE OF THE SUB ACCT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013836 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50II-FN. EXIT.
      *N50IL.    NOTE *IF A TRANSFER IS NOT ALLOWED       *.
       F50IL.    IF    M977-ISUBA2 = 'N'                                lv25
                 AND   (K974-PACT1 (WS01-SUB) >
                       K974-PFNDV (WS01-SUB))
                 NEXT SENTENCE ELSE GO TO     F50IL-FN.
      *INTO THE FIXED ACCOUNT. THE %
      *DISB CANNOT BE LESS THAN THE
      *ORIGINAL % VALUE OF THE SUB ACCT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013835 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50IL-FN. EXIT.
      *N50IO.    NOTE *IF A TRANSFER IS ALLOWED OUT OF    *.
       F50IO.    IF    M977-ISUBA1 = 'Y'                                lv25
                 AND   M977-ALLNB < 0
                 NEXT SENTENCE ELSE GO TO     F50IO-FN.
      *A FIXED ACCOUNT. CHECK THAT IT
      *NOT > THAN THE MAXIMIM ALLOWED
      *
           COMPUTE     WS01-TEMP-PER ROUNDED =
           (M977-ALLNB * 15 / 100 * -1) /
           QT58-AACTV * 100
      *
           COMPUTE     WS01-MIN-PER ROUNDED =
           WS01-TEMP-PER * 1.
                 IF    K974-PACT1 (WS01-SUB) <                          DOT
                       WS01-MIN-PER
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013837 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50IO-FN. EXIT.
      *N50IQ.    NOTE *CHECK IF TIME HAS PASSED 3:00PM    *.
       F50IQ.    IF    M977-IFTDY1 = 'Y'                                lv25
                 AND   M977-GETIM > WS-GETIM6
                 NEXT SENTENCE ELSE GO TO     F50IQ-FN.
      *AND IF IT IS THE 30TH DAY OF THE
      *30 DAY EDIT IN WHICH CASE THE 30
      *DAY EDIT COMES INTO EFFECT
      *BECAUSE IT IS NEXT BUSINESS DAY
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013857 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50IQ-FN. EXIT.
       F50IF-FN. EXIT.
      *N50RC.    NOTE *IF VANTAGE ACCOUNT AND             *.
       F50RC.    IF    QT58-IVANT = 'Y'                                 lv20
                 AND   K974-CFIDC (WS01-SUB) >=
                       'G01  '
                 AND   K974-CFIDC (WS01-SUB) <=
                       'G10  '
                 AND   (K974-CFIDC (WS01-SUB) <
                       'G1A  '
                 OR    K974-CFIDC (WS01-SUB) >
                       'G1Z  ')
                 NEXT SENTENCE ELSE GO TO     F50RC-FN.
      *GPA DESTINATION, EDIT THAT
      *TO AMOUNT WILL BE AT LEAST $1000
      *N50RG.    NOTE *PERCENT IS USED, SO COMPUTE        *.
       F50RG.                                                           lv25
      *TOTAL AMOUNT TO GPA FUND
           COMPUTE     WA00-TEMP-AEDRQ ROUNDED =
           QT58-AACTV *
           K974-PACT1 (WS01-SUB) / 100.
       F50RG-FN. EXIT.
      *N50RM.    NOTE *ERROR IF NOT AT LEAST $1000        *.
       F50RM.    IF    WA00-TEMP-AEDRQ < 1000                           lv25
                 NEXT SENTENCE ELSE GO TO     F50RM-FN.
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014514 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50RM-FN. EXIT.
       F50RC-FN. EXIT.
      *N50UF.    NOTE *INCREASE SUBSCRIPT                 *.
       F50UF.                                                           lv20
           ADD         1 TO WS01-SUB.
       F50UF-FN. EXIT.
       F50DD-900. GO TO F50DD-A.
       F50DD-FN. EXIT.
       F50BB-FN. EXIT.
       F50-FN.   EXIT.
      *N51.      NOTE *************************************.
      *               *                                   *
      *               *FURTHER EDITS FOR ASSET ALLOC      *
      *               *                                   *
      *               *************************************.
       F51.           EXIT.                                             lv05
      *N51DD.    NOTE *EDIT FOR ASSET ALLOC ONLY          *.
       F51DD.    IF    K974-CAFLOW = 'AA'                               lv10
                 NEXT SENTENCE ELSE GO TO     F51DD-FN.
      *N51EE.    NOTE *CHECK TOTAL % NOT > 100            *.
       F51EE.    IF    WS01-AA-PACT1 > 100                              lv15
                 NEXT SENTENCE ELSE GO TO     F51EE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013746 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F51EE-FN. EXIT.
       F51DD-FN. EXIT.
       F51-FN.   EXIT.
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
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BB.    NOTE *CALL LIFE DETAIL INFO    MODULE    *.            AM0146
       F91BB.                                                           lv10
      *SET PCB POINTERS                                                 DOT
           SET CI0146-WB-PCB-CT1P-PTR1 TO                               AM0146
                         PCB-CT1P-PTR1.                                 AM0146
      *INITIALIZE OUTPUT PASS AREA                                      DOT
           INITIALIZE  DE10-DU03.                                       AM0146
      *LOAD INPUT PARMS  PASS AREA.                                     DOT
           CALL        CI0146 USING                                     DOT
           DFHEIBLK                                                     AM0146
           DFHCOMMAREA                                                  AM0146
           DLIUIBII                                                     AM0146
           CI0146-WB-PCB-ADDR-LIST                                      AM0146
           P948                                                         AM0146
           DE10                                                         AM0146
           MS03                                                         AM0146
           MX11.                                                        AM0146
       F91BB-FN. EXIT.
      *N91CB.    NOTE *CALL CI0500 MODULE                 *.            AM0500
       F91CB.                                                           lv10
      *                                                                 AM0500
      *********************************                                 AM0500
      ** CALL THE DRIVER PROGRAM FOR  *                                 AM0500
      ** DCA FIXED VALUE VALIDATION   *                                 AM0500
      *********************************.                                AM0500
      *SET PCB POINTERS                                                 DOT
           SET CI0500-VB-PCB-ARAY-PTR1 TO                               AM0500
                         PCB-ARAY-PTR1
           SET CI0500-VB-PCB-AR1P-PTR1 TO                               AM0500
                         PCB-AR1P-PTR1
           INITIALIZE  MX11                                             AM0500
      *                                                                 AM0500
           CALL        CI0500 USING                                     AM0500
           DFHEIBLK                                                     AM0500
           DFHCOMMAREA                                                  AM0500
           DLIUIBII                                                     AM0500
           CI0500-VB-PCB-ADDR-LIST                                      AM0500
           P949                                                         AM0500
           DE10                                                         AM0500
           MS03
           MX11.
       F91CB-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PACTABLE READ                      *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TA5B       *.
       F92EB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012617 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92EB-FN. EXIT.
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
      *N92TB.    NOTE *RANDOM TABLE READ FOR TB8A         *.            ADUTAB
       F92TB.                                                           lv10
           MOVE        'R1' TO G-TB8A-TABFO                             ADUTAB
           COMPUTE     G-TB8A-LTH = 60 + G-TB8A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TB8A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TB8A)                                ADUTAB
                       LENGTH (G-TB8A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TB8A-TABCR NOT = '00'                          DOT
      *NO ERROR OVERRIDE
           INITIALIZE  TB8A.
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
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *DATE CALCULATION AND VALIDATION    *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96TA.    NOTE *DATE VALIDATION                    *.            ADU040
       F96TA.                                                           lv10
                 IF    DD25-XDATG NOT NUMERIC                           DOT
           MOVE        012144 TO MS03-NMESS2                            ADU040
           PERFORM     F98ET THRU F98ET-FN.                             ADU040
                 IF    DD25-XDAT1 > '20'                                DOT
                 OR    DD25-XDAT1 < '18'                                ADU040
           MOVE        012144 TO MS03-NMESS2                            ADU040
           PERFORM     F98ET THRU F98ET-FN.                             ADU040
                 IF    DD25-XDAT3 > '12'                                DOT
                 OR    DD25-XDAT3 = '00'                                ADU040
                 OR    DD25-XDAT4 > '31'                                ADU040
                 OR    DD25-XDAT4 = '00'                                ADU040
           MOVE        012144 TO MS03-NMESS2                            ADU040
           PERFORM     F98ET THRU F98ET-FN.                             ADU040
                 IF    DD25-XDAT4 > '30'                                DOT
                 AND   (DD25-XDAT3 = '04'                               ADU040
                 OR    DD25-XDAT3 = '06'                                ADU040
                 OR    DD25-XDAT3 = '09'                                ADU040
                 OR    DD25-XDAT3 = '11')                               ADU040
           MOVE        012144 TO MS03-NMESS2                            ADU040
           PERFORM     F98ET THRU F98ET-FN.                             ADU040
                 IF    DD25-XDAT3 NOT = '02'                            DOT
               GO TO     F96TA-FN.                                      ADU040
                 IF    DD25-XDAT4 > '29'                                DOT
           MOVE        012144 TO MS03-NMESS2                            ADU040
           PERFORM     F98ET THRU F98ET-FN.                             ADU040
                 IF    DD25-XDAT2 = '00'                                DOT
           MOVE        0 TO DD25-XLEAPY                                 ADU040
                 ELSE                                                   ADU040
           COMPUTE     DD25-XLEAPY = DD25-XDAT29 -                      ADU040
           (DD25-XDAT29 / 4) * 4.                                       ADU040
                 IF    DD25-XLEAPY NOT = ZERO                           DOT
                 AND   DD25-XDAT4 > '28'                                ADU040
           MOVE        012144 TO MS03-NMESS2                            ADU040
           PERFORM     F98ET THRU F98ET-FN.                             ADU040
       F96TA-FN. EXIT.
       F96-FN.   EXIT.
      *N97BB.    NOTE *********************************   *.            ACMCTI
       F97BB.                                                           lv10
      ** THIS SUBFUNCTION CALLS THE   *                                 ACMCTI
      ** ONLINE INQUIRY MODULE        *                                 ACMCTI
      ** (CI0361) TO RETRIEVE THE ROW *                                 ACMCTI
      ** INFORMATION FROM THE DB2     *                                 ACMCTI
      ** TABLE (TBS234) CORRESPONDING *                                 ACMCTI
      ** TO THE KEYS: 'DATE TYPE CODE'*                                 ACMCTI
      ** (ELEMENT: CEADC) AND         *                                 ACMCTI
      ** 'ACCOUNTING DATE'(ELEMENT:   *                                 ACMCTI
      ** DACTT).                      *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** MOVE VALUES TO THE LINKAGE   *                                 ACMCTI
      ** AREA ELEMENTS TO BE PASSED   *                                 ACMCTI
      ** TO THE CALLED MODULE CI0361. *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** CALL THE ONLINE INQUIRY      *                                 ACMCTI
      ** MODULE (CI0361).             *                                 ACMCTI
      *********************************                                 ACMCTI
           CALL        CI0361 USING                                     ACMCTI
           DFHEIBLK                                                     ACMCTI
           DFHCOMMAREA                                                  ACMCTI
           I93B.                                                        ACMCTI
       F97BB-FN. EXIT.
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
      *N99BB.    NOTE *ROUTINE TO CALL THE ONLINE         *.
       F99BB.                                                           lv10
      *LATE TRADING MODULE
      *N99CJ.    NOTE *CALL THE ONLINE INQUIRY MODULE     *.
       F99CJ.                                                           lv15
      *TO GET THE MARKET CLOSE TIME
           INITIALIZE  I93B
           MOVE        NS20-DCACD TO I93B-DACTT
           MOVE        'O' TO I93B-CEADC
           PERFORM     F97BB THRU F97BB-FN.
       F99CJ-FN. EXIT.
      *N99CR.    NOTE *CHECK RETURN CODES                 *.
       F99CR.    IF    I93B-CRTNC = 11111                               lv15
                 OR    I93B-CRTNC = 22222
                 OR    I93B-CRTNC = 100
                 NEXT SENTENCE ELSE GO TO     F99CR-FN.
      *IF INVALID CODE, SEND ERROR MSG
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F99CR-FN. EXIT.
      *N99CU.    NOTE *REFORMAT DATE                      *.
       F99CU.                                                           lv15
           MOVE        I93B-GETIMM TO WS-GETIMM
           MOVE        WS-HH TO WS-MKT-HH
           MOVE        WS-MM TO WS-MKT-MM
           MOVE        WS-SS TO WS-MKT-SS.
       F99CU-FN. EXIT.
       F99BB-FN. EXIT.
