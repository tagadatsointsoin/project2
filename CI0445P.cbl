       IDENTIFICATION DIVISION.                                         CI0445
       PROGRAM-ID.  CI0445P.                                            CI0445
      *AUTHOR.         BUILD  HTML PAGE.                                CI0445
      *DATE-COMPILED.   09/08/14.                                       CI0445
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2007                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE FDC    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE FDC    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE FDC          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2007                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0445
       CONFIGURATION SECTION.                                           CI0445
       SOURCE-COMPUTER. IBM-370.                                        CI0445
       OBJECT-COMPUTER. IBM-370.                                        CI0445
       DATA DIVISION.                                                   CI0445
       WORKING-STORAGE SECTION.                                         CI0445
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102

      ******************************************************************
      **                                                               *
      ** THESE FIELDS WILL BE USED TO FORMAT THE INDIVIDUAL FIELDS USED*
      ** IN THE DETAIL LINES                                           *
      **                                                               *
      ******************************************************************
       01  W-DE00-AREAS.
      *!WS
           05  7-DE00-DCACG
                        PICTURE 99B99B9999.                             CI0445
      *!WS
           05  7-DE00-DNACG
                        PICTURE 99B99B9999.                             CI0445
      *!WS
           05  7-DE00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0445
      *!WS
           05  7-DE00-GEEND
                        PICTURE 99B99B9999.                             CI0445
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0445
            10            I93B-CEADC  PICTURE  X                        CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-DACTT  PICTURE  X(10)                    CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-GEOPDC PICTURE  X(8)                     CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-GEOPDB PICTURE  X(8)                     CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-CAEMCE PICTURE  X(8)                     CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-CAEMCD PICTURE  X(8)                     CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-GETIMM PICTURE  X(8)                     CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0445
                          VALUE                ZERO                     CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            I93B-GERTC  PICTURE  X                        CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-DXTMST PICTURE  X(26)                    CI0445
                          VALUE                SPACE.                   CI0445
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0445
                          VALUE                SPACE.                   CI0445
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
      *HTML BLOB THAT NEEDS TO BE SENT TO WEB AND IMAGE UTILITY

       01 HTML-BLOB    PIC X(50000)  VALUE SPACES.
       01 HTML-LEN     PIC 9(5)      VALUE ZEROS.
       01 HTML-LIMIT   PIC 9(3)      VALUE ZEROS.

       01 HTML-TEXT    PIC X(200)    VALUE SPACES.
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
      *STRING POINTERS USED IN THE PROGRAM

       01  HTML-PT PIC S9(5) VALUE ZEROS.

       01  TEMP-PT PIC S9(5) VALUE ZEROS.
      *---------------  SQL INCLUDE STATEMENTS ---------------------    ADB221
           EXEC SQL     INCLUDE SQLCA             END-EXEC.             ADB221
      *                                                                 ADB221
      *--------------  ERROR HANDLING VARIABLES --------------------    ADB221
       01               7-DB2-FUNCT      PIC X(35) VALUE SPACES.        ADB221
       01               7-SQLR.                                         ADB221
         05             7-SQLR-TEXT-LEN  PIC S9(9) COMP VALUE +80.      ADB221
         05             7-SQLR-MESSAGE.                                 ADB221
           10           7-SQLR-LEN       PIC S9(4) COMP VALUE +960.     ADB221
           10           7-SQLR-TEXT      PIC X(80) OCCURS 12 TIMES.     ADB221
       01               7-DB2-ABEND      PIC 9(4)  VALUE ZERO.          ADB221
       01               7-DB2-ABENDX     REDEFINES 7-DB2-ABEND.         ADB221
           05           7-DB2-FIRST      PIC X.                         ADB221
           05           FILLER           PIC X(3).                      ADB221
       01               7-TEST-SQLCODE   PIC S9(9) COMP.                ADB221
           88           ROW-NOT-FOUND              VALUE +100.          ADB221
           88           DUPLICATE-KEY              VALUE -803.          ADB221
           88           MULTIPLE-ROWS-FOUND        VALUE -811.          ADB221
           88           RESOURCE-NOT-AVAILABLE     VALUE -904.          ADB221
           88           RESOURCE-IN-USE            VALUE -913.          ADB221
       01               7-Q913-COUNT     PIC S9(3) COMP-3               ADB221
                                                   VALUE ZERO.          ADB221
       01               7-Q913-LNGTH     PIC S9(4) COMP                 ADB221
                                                   VALUE +66.           ADB221
      *!WI pl=SQ430                                                     ADB221
       01               7-Q913-TMSGV5                                   ADB221
                        PICTURE X(66)                                   CI0445
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
       01 WC00-CONF-MSG2.
               05 WC00-CONF-MSG21 PIC X(50)
               VALUE 'Your request has been submitted.'.
               05 WC00-CONF-MSG22 PIC X(50)
               VALUE ' Transaction completion varies based on the '.
               05 WC00-CONF-MSG23 PIC X(50)
               VALUE 'transaction type, see Step 3 for details.'.
       01 WC00-CONF-MSG5 PIC X(80)
              VALUE '<li>Allow five business days for processing.</li>'.
       01 WC00-CONF-MSG6.
          05 WC00-CONF-MSG61 PIC X(50)
              VALUE '<li>Allow an additional business day for accounts'.
          05 WC00-CONF-MSG62 PIC X(50)
              VALUE ' with AIMMA cash positions/sweep options.</li>'.
       01 WC00-CONF-MSG6-BROK.
          05 WC00-CONF-MSG61-BROK PIC X(50)
              VALUE '<li>Please note that full redemptions of money'.
          05 WC00-CONF-MSG62-BROK PIC X(50)
              VALUE ' market positions may delay processing by '.
          05 WC00-CONF-MSG63-BROK PIC X(50)
              VALUE '1 business day to allow for interest accrual and'.
          05 WC00-CONF-MSG64-BROK PIC X(50)
              VALUE ' may also result in product fees.</li>'.
       01 WC00-VERF-MSG1 PIC X(35)
               VALUE 'Verify Destination Details'.
       01 WC00-VERF-MSG2 PIC X(35)
               VALUE 'Verify Transaction Details'.
       01 WC00-VERF-MSG4 PIC X(35)
               VALUE 'Destination Details'.
       01 WC00-VERF-MSG5 PIC X(35)
               VALUE 'Transaction Details'.
       01 WC00-VERF-IMPMSG.
          05  WC00-VERF-IMPMSG1 PIC X(50)
               VALUE 'Upon submission, your transaction will be '.
          05  WC00-VERF-IMPMSG2 PIC X(50)
               VALUE 'reviewed for processing.'.
       01 WC00-CONF-IMPMSG.
          05  WC00-CONF-IMPMSG1 PIC X(50)
               VALUE 'Upon submission, your transaction will be '.
          05  WC00-CONF-IMPMSG2 PIC X(50)
               VALUE 'reviewed for processing.'.
       01 WC00-FULL-SUR-MSG1 PIC X(50)
             VALUE 'Full Surrender at End of Term (maturity)'.
       01 WC00-FULL-SUR-MSG2 PIC X(50)
             VALUE 'Term End Date for Market Strategy Certs:'.
       01 WC00-MSG6-SMWT-BROK.
          05 FILLER PIC X(100) VALUE
            '<li>Requests must be submitted by 12:30 PM Central Time '.
          05 FILLER PIC X(100) VALUE
            'to ensure same day processing.</li>'.
       01 WC00-MSG7-SMWT-BROK.
          05 FILLER PIC X(100) VALUE
            '<li>Wire will be received in 2-24 hours depending on '.
          05 FILLER PIC X(100) VALUE
            'receiving bank.</li>'.
       01 WC00-MSG8-SMWT-BROK.
          05 FILLER PIC X(100) VALUE
           '<li>Wire may fail if receiving institution is a pass '.
          05 FILLER PIC X(100) VALUE
           'through bank.</li>'.
       01 WC00-MSG8-MMTA-BROK.
          05 FILLER PIC X(100) VALUE
           '<li>Internal Transfer requests must be submitted by 12:00 '.
          05 FILLER PIC X(100) VALUE
           'Noon Central Time to ensure same day processing.</li>'.
       01 WC00-MES-EXPRESS-BROKDCSO.
          05 FILLER PIC X(100) VALUE
           '<li>Express mail will be delivered next business day if '.
          05 FILLER PIC X(100) VALUE
           'transaction was submitted before 12:30 p.m. Central.</li>'.
       01 WC00-CONF-MSG9.
          05 WC00-CONF-MSG91 PIC X(35)
             VALUE 'Your request has been submitted.'.
          05 WC00-CONF-MSG92 PIC X(45)
             VALUE ' Transaction completion varies based on the '.
          05 WC00-CONF-MSG93 PIC X(40)
             VALUE 'transaction type; see Important Messages'.
          05 WC00-CONF-MSG94 PIC X(20)
             VALUE ' below for details.'.
       01 WC00-MSG9-AMOUNT-BROK.
          05 FILLER PIC X(100) VALUE
            '<LI> Requests to bring account balances below product '.
          05 FILLER PIC X(100) VALUE
            ' minimums will not be processed.</LI>'.
       01               WE00.                                           $CHECK
      *!WI pl=WE110                                                     $CHECK
         05             WE00-CTIDN                                      $CHECK
                        PICTURE X(24)                                   CI0445
                        JUSTIFIED RIGHT.                                $CHECK
      *!WI pl=WE130                                                     $CHECK
         05             WE00-GERTC                                      $CHECK
                        PICTURE X.                                      CI0445
      *!WI
       01 7-CTIDN VALUE SPACES
                        PICTURE X(24).                                  CI0445
       01    WL00-CTTYPG1  PIC X(100)
               VALUE 'Send check to address of record via regular mail'.
       01    WL00-CTTYPG2.
             05   WL00-CTTYPG21   PIC X(50)
                     VALUE 'Internal Transfer to another '.
             05   WL00-CTTYPG22   PIC X(50)
                     VALUE 'Ameriprise Financial Services account'.
       01    WL00-CTTYPG6.
             05   WL00-CTTYPG61   PIC X(50)
                  VALUE 'Direct Deposit (ACH-Out) to an '.
             05   WL00-CTTYPG62   PIC X(50)
                  VALUE 'authorized bank account'.
       01    WL00-CTTYPG3  PIC X(100)
                     VALUE 'Direct Deposit to an existing Bank'.
       01    WL00-CTTYPG4  PIC X(100)
             VALUE  'Send check to address of record via express mail'.
       01    WL00-CTTYPG5  PIC X(100)
             VALUE  'Wire Transfer to an existing Bank'.
       01    WL00-CIRAP1  PIC X(50)
                     VALUE 'Not applicable'.
       01    WL00-CIRAP2  PIC X(50)
                     VALUE 'Current year'.
       01    WL00-CIRAP3  PIC X(50)
                     VALUE 'Rollover'.
       01    WL00-CIRAP4  PIC X(50)
                     VALUE 'Prior year'.
       01    WL00-CIRAP5  PIC X(50)
                     VALUE 'SEP/SRA current year'.
       01    WL00-CACTN1  PIC X(100)
                     VALUE 'Surrender prorata from all Sub-accounts'.
       01    WL00-CACTN2  PIC X(100)
                     VALUE 'Manually Specify Sub Accounts'.
       01    WL00-CACTN3  PIC X(100)
                     VALUE 'Manually specify sub-accounts'.
       01    WL00-CACTN4  PIC X(100)
                     VALUE 'Surrender prorata from all sub-accounts'.
       01    WL00-CACT1  PIC X(100)
                     VALUE 'This is a new systematic distribution'.
       01    WL00-CACT2.
             05    WL00-CACT21  PIC X(50)
                     VALUE 'This is a change to an existing'.
             05    WL00-CACT22  PIC X(50)
                     VALUE '  systematic distribution'.
       01    WL00-CACT3  PIC X(100)
                    VALUE 'Inactivate existing systematic distribution'.
       01    WL00-RIDER1 PIC X(30)
                    VALUE 'Without Living Benefit Rider'.
       01    WL00-RIDER2 PIC X(30)
                    VALUE 'With Living Benefit Rider'.
       01    WL00-CTRN1.
             05 WL00-CTRN11 PIC X(50)
                   VALUE 'Cash from sweep account - Ameriprise Cash,'.
             05 WL00-CTRN12 PIC X(50)
                   VALUE ' Reserve Funds or AIMMA'.
       01    WL00-CTRN2.
             05 WL00-CTRN21 PIC X(50)
                     VALUE 'Value available from margin equity'.
             05 WL00-CTRN22 PIC X(50)
                     VALUE ' (non-qualified account only)'.
       01    WL00-CTRN3  PIC X(100)
                    VALUE 'Proceeds from securities sale'.
       01  WL00-DCSO-AMOUNT.
             05 WL00-AMTAG  PIC X(18) VALUE 'width=230>Amount: '.
             05 WL00-AMTAG2 PIC X(12) VALUE '</td><td><b>'.
             05 WL00-ACOTD2 PIC X(38) VALUE SPACES.
       01  WL00-DCSO-AMOUNT1.
             05 WL00-AMTAG1  PIC X(50)
               VALUE 'colspan=2><b>All Available Cash/Margin '.
             05 WL00-AMTAG2  PIC X(50)
               VALUE 'and/or Trade Proceeds Specified Below'.
       01  WL00-SMEM-METHOD.
             05 WL00-MEEXP1  PIC X(50)
               VALUE 'Other Ameriprise Account'.
             05 WL00-MEEXP2  PIC X(50)
               VALUE 'Credit Card'.
             05 WL00-MEEXP3  PIC X(50)
             VALUE 'Achiever Circle Elite client (UPS/Wire Fee Waived)'.
       01  WL00-SMEM-ADRESS-CHECKB.
             05 WL00-BACBOX  PIC X(30)
               VALUE 'Same as Address on Record     '.
      *MISCELLANEOUS FIELDS
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0445
       01  WS-GETIMN REDEFINES WS-GETIMM.
           05 WS-TIME1-HH   PIC 99.
           05 FILLER       PIC X.
           05 WS-TIME1-MM   PIC 99.
           05 FILLER       PIC X.
           05 WS-TIME1-SS   PIC 99.
       01  WS-GETIMT.
           05 WS-TIME2-HH PIC X(2).
           05 WS-TIME2-MM PIC X(2).
           05 WS-TIME2-SS PIC X(2).
       01 WS00-WORKAREA.
           05 WS00-CTID.
      *!WS
              10 WS00-NCTIDE          VALUE SPACES
                        PICTURE 9999B9999B9999B9999.                    CI0445
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

           05 WS01-CTID.
      *!WS
              10 WS01-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0445
              10 FILLER      PIC X    VALUE SPACE.
              10 WS01-GECKD2 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS01-CTIDA  PIC X(3) VALUE SPACES.

          05 WS00-MSGTXT.
             10 WS00-MSGNUM PIC Z9.
             10 WS00-DOT    PIC X     VALUE '.'.
             10 FILLER      PIC X     VALUE SPACE.
      *!WI
             10 WS00-TMESSC
                        PICTURE X(254).                                 CI0445

      *!WS
          05 WS00-APMTD
                        PICTURE $$,$$$,$$$,$$9.99.                      CI0445

          05 WS00-COUNT    PIC 9(8)    VALUE ZEROS.

      *!WS
          05 WS00-MPMTF                VALUE SPACES
                        PICTURE X(14).                                  CI0445

          05 WS00-PWHLD    PIC ZZ9.99- VALUE ZEROES.
          05 WS00-PWHLD1   PIC ZZ9.99 VALUE ZEROES.
      *!WS
          05 WS00-PACT1
                        PICTURE ZZ9.999-.                               CI0445

      *!WS
          05 WS00-QSHOWQ
                        PICTURE ZZZ,ZZZ,ZZ9.999-                        CI0445
                          BLANK WHEN ZERO.                              CI0445
      ******************************************************************
      *VARIABLE 'TIMER' IS USED TO HOLD THE TRANSACTION RECEIVED TIME.
      ******************************************************************
       01  WS00-TIMER    PIC X(06) VALUE SPACES.
       01  WS00-TIME-ENTERED REDEFINES WS00-TIMER.
              05 WS-TIME-HH   PIC 99.
              05 WS-TIME-MM   PIC 99.
              05 WS-TIME-SS   PIC 99.
      *
       01  WS00-AMPM     PIC X(03) VALUE SPACES.
      *
       01  WS00-CST      PIC X(12) VALUE SPACES.
      *
       01  WS00-CST01    PIC X(05) VALUE SPACES.
      *
      *01  WS00-CST01    PIC X(05) VALUE SPACES.
      ******************************************************************
      *VARIABLES FOR STRING CANCAT TO CREATE SUB-ACCT FND NAME TABLE.
      ******************************************************************
      *!WI
       01 WS00-MFDNME
                        PICTURE X(40).                                  CI0445
      *01 WS00-APMTD.
       01 WS00-SUBACCT-FNDNAME  PIC X(80).
      *!WE
       01 WS00-GEEND VALUE ZEROES
                        PICTURE 9(8).                                   CI0445
       01 WS00-GEENDX REDEFINES WS00-GEEND PIC X(8).
       01 WS00-DATCE  PIC X(8) VALUE ZEROES.
       01   DEBUT-WSS.                                                  CI0445
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0445
            05   IK     PICTURE X.                                      CI0445
       01  CONSTANTES-PAC.                                              CI0445
           05  FILLER  PICTURE X(87)   VALUE                            CI0445
                     '6015 CAT09/08/14CI0445ADMIN   14:35:27CI0445P AMERCI0445
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0445
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0445
           05  NUGNA   PICTURE X(5).                                    CI0445
           05  APPLI   PICTURE X(3).                                    CI0445
           05  DATGN   PICTURE X(8).                                    CI0445
           05  PROGR   PICTURE X(6).                                    CI0445
           05  CODUTI  PICTURE X(8).                                    CI0445
           05  TIMGN   PICTURE X(8).                                    CI0445
           05  PROGE   PICTURE X(8).                                    CI0445
           05  COBASE  PICTURE X(4).                                    CI0445
           05  DATGNC  PICTURE X(10).                                   CI0445
           05  RELEAS  PICTURE X(7).                                    CI0445
           05  DATGE   PICTURE X(10).                                   CI0445
           05  DATSQ   PICTURE X(10).                                   CI0445
       01  DATCE.                                                       CI0445
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0445
         05  DATOR.                                                     CI0445
           10  DATOA  PICTURE XX.                                       CI0445
           10  DATOM  PICTURE XX.                                       CI0445
           10  DATOJ  PICTURE XX.                                       CI0445
       01  DAT6.                                                        CI0445
            10 DAT61.                                                   CI0445
            15 DAT619  PICTURE 99.                                      CI0445
            10 DAT62.                                                   CI0445
            15 DAT629  PICTURE 99.                                      CI0445
            10 DAT63   PICTURE XX.                                      CI0445
       01  DAT8.                                                        CI0445
            10 DAT81   PICTURE XX.                                      CI0445
            10 DAT8S1  PICTURE X.                                       CI0445
            10 DAT82   PICTURE XX.                                      CI0445
            10 DAT8S2  PICTURE X.                                       CI0445
            10 DAT83   PICTURE XX.                                      CI0445
       01  DAT8E    REDEFINES    DAT8.                                  CI0445
            10 DAT81E  PICTURE X(4).                                    CI0445
            10 DAT82E  PICTURE XX.                                      CI0445
            10 DAT83E  PICTURE XX.                                      CI0445
       01  DAT6C.                                                       CI0445
            10  DAT61C PICTURE XX.                                      CI0445
            10  DAT62C PICTURE XX.                                      CI0445
            10  DAT63C.                                                 CI0445
             15 DAT63CC PICTURE XX.                                     CI0445
             15 DAT64C  PICTURE XX.                                     CI0445
       01  DAT8C.                                                       CI0445
            10  DAT81C  PICTURE XX.                                     CI0445
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0445
            10  DAT82C  PICTURE XX.                                     CI0445
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0445
            10  DAT83C.                                                 CI0445
             15 DAT83CC PICTURE XX.                                     CI0445
             15 DAT84C  PICTURE XX.                                     CI0445
       01  DATSEP     PICTURE X VALUE '/'.                              CI0445
       01  DATSEW     PICTURE X.                                        CI0445
       01   VARIABLES-CONDITIONNELLES.                                  CI0445
            05                  FT      PICTURE X VALUE '0'.            CI0445
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0445
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0445
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J70BDR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0445
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT.
      *THIS COPYBOOK CONTAINS THE HEADER, SOURCE ACCOUNT DETAILS AND
      *DESTINATION ACCOUNT DETAILS FOR THE HTML PAGE.
      ******************************************************************
       COPY CI0445C1.
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT
      *THIS COPYBOOOK CONTAINS THE PAYMENT DETAILS AND THE COMPLIANCE
      *QUESTIONS FOR THE HTML PAGE.
      ******************************************************************
       COPY CI0445C2.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0445                *
      ******************************************************************
      *!WF DSP=V2 DSL=V2 SEL=89 FOR=I LEV=1 PLT=80
       01                 V200.                                         CI0445
          05              V200-SUITE.                                   CI0445
            15       FILLER         PICTURE  X(53930).                  CI0445
       01                 V289  REDEFINES      V200.                    CI0445
            10            V289-CTID.                                    CI0445
            11            V289-CTIDA  PICTURE  9(3).                    CI0445
            11            V289-CTIDN.                                   CI0445
            12            V289-CTIDNP PICTURE  X(13).                   CI0445
            12            V289-CTIDND PICTURE  9(11).                   CI0445
            10            V289-GECKD1 PICTURE  9.                       CI0445
            10            V289-CALTN  PICTURE  X(4).                    CI0445
            10            V289-APMTD  PICTURE  S9(11)V99                CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-PWHLD  PICTURE  S999V9(5)                CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-TWITH  PICTURE  X(12).                   CI0445
            10            V289-QITEM  PICTURE  9(3).                    CI0445
            10            V289-QBLCK  PICTURE  9(6).                    CI0445
            10            V289-DCACG  PICTURE  9(8).                    CI0445
            10            V289-DCACD  PICTURE  X(10).                   CI0445
            10            V289-DNPMT  PICTURE  9(8).                    CI0445
            10            V289-GEEND  PICTURE  9(8).                    CI0445
            10            V289-DXTMS2 PICTURE  X(26).                   CI0445
            10            V289-MPLNR2 PICTURE  X(40).                   CI0445
            10            V289-CCONF  PICTURE  X(25).                   CI0445
            10            V289-ATROLL PICTURE  X(25).                   CI0445
            10            V289-GEOPD2 PICTURE  X(8).                    CI0445
            10            V289-CSPRDN PICTURE  X(30).                   CI0445
            10            V289-CTTLN1 PICTURE  X(30).                   CI0445
            10            V289-CTTLN2 PICTURE  X(30).                   CI0445
            10            V289-CTTLN3 PICTURE  X(30).                   CI0445
            10            V289-CTTBO1 PICTURE  X(45).                   CI0445
            10            V289-CTTBO2 PICTURE  X(45).                   CI0445
            10            V289-CTID01.                                  CI0445
            11            V289-CACTID PICTURE  9(3).                    CI0445
            11            V289-CTIDNB.                                  CI0445
            12            V289-CTIDP1 PICTURE  X(13).                   CI0445
            12            V289-CTIDNA PICTURE  9(11).                   CI0445
            10            V289-GECKD2 PICTURE  9.                       CI0445
            10            V289-PRCMN1 PICTURE  X(20).                   CI0445
            10            V289-MPMTT  PICTURE  X(20).                   CI0445
            10            V289-MPMTF  PICTURE  X(14).                   CI0445
            10            V289-CDETY  PICTURE  XX.                      CI0445
            10            V289-CDEST  PICTURE  99.                      CI0445
            10            V289-CACTS  PICTURE  X.                       CI0445
            10            V289-CACT   PICTURE  X(1).                    CI0445
            10            V289-CATLN1 PICTURE  X(30).                   CI0445
            10            V289-CATLN2 PICTURE  X(30).                   CI0445
            10            V289-CATLN3 PICTURE  X(30).                   CI0445
            10            V289-CATBO1 PICTURE  X(45).                   CI0445
            10            V289-CATBO2 PICTURE  X(45).                   CI0445
            10            V289-GESAD1 PICTURE  X(30).                   CI0445
            10            V289-GESAD2 PICTURE  X(30).                   CI0445
            10            V289-GESAD3 PICTURE  X(30).                   CI0445
            10            V289-GECIT  PICTURE  X(25).                   CI0445
            10            V289-GEST   PICTURE  X(8).                    CI0445
            10            V289-GEPCD  PICTURE  X(12).                   CI0445
            10            V289-CLORN  PICTURE  X(45).                   CI0445
            10            V289-NTR    PICTURE  9(8).                    CI0445
            10            V289-GECKD3 PICTURE  9.                       CI0445
            10            V289-NPBN   PICTURE  X(20).                   CI0445
            10            V289-TTBAL  PICTURE  X(15).                   CI0445
            10            V289-MCSIG  PICTURE  X(30).                   CI0445
            10            V289-CPROCM PICTURE  X.                       CI0445
            10            V289-CSLCT  PICTURE  X.                       CI0445
            10            V289-CTRHO  PICTURE  9(8).                    CI0445
            10            V289-GETOD  PICTURE  9(6).                    CI0445
            10            V289-TMESSC PICTURE  X(254)                   CI0445
                          OCCURS       020     TIMES.                   CI0445
            10            V289-QT9D.                                    CI0445
            11            V289-QT9B                                     CI0445
                          OCCURS       233     TIMES.                   CI0445
            12            V289-CHTML  PICTURE  99.                      CI0445
            12            V289-THTML  PICTURE  X(200).                  CI0445
            10            V289-CQACTL PICTURE  X(45).                   CI0445
            10            V289-DAEDTO PICTURE  X(8).                    CI0445
            10            V289-DPTIM  PICTURE  X(8).                    CI0445
            10            V289-NGEOPA PICTURE  X(08).                   CI0445
            10            V289-CTTYPG PICTURE  X(04).                   CI0445
            10            V289-CACTN  PICTURE  X(4).                    CI0445
            10            V289-CTRN   PICTURE  9(03).                   CI0445
            10            V289-CIRAP  PICTURE  XX.                      CI0445
            10            V289-IRMND  PICTURE  X.                       CI0445
            10            V289-ACOTD  PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-AMNBR  PICTURE  S9(5)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-MFDNMH PICTURE  X(30).                   CI0445
            10            V289-MFDNME PICTURE  X(40).                   CI0445
            10            V289-MFDNMF PICTURE  X(80).                   CI0445
            10            V289-MFDNMI PICTURE  X(30).                   CI0445
            10            V289-MFDNMJ PICTURE  X(30).                   CI0445
            10            V289-MFDNMP PICTURE  X(54).                   CI0445
            10            V289-MFDNMS PICTURE  X(30).                   CI0445
            10            V289-MFDNM4 PICTURE  X(40).                   CI0445
            10            V289-MFDNM5 PICTURE  X(40).                   CI0445
            10            V289-MFDNM7 PICTURE  X(30).                   CI0445
            10            V289-ADBCRQ PICTURE  S9(11)V99                CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF1 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF2 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF3 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF4 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF5 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF6 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF7 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF8 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-ADBRF9 PICTURE  S9(9)V99                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-DNPMT1 PICTURE  9(8).                    CI0445
            10            V289-TDE25G PICTURE  X(25).                   CI0445
            10            V289-QSHOWQ PICTURE  S9(9)V999                CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-PACT1  PICTURE  S999V999                 CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            V289-CPORTA PICTURE  X.                       CI0445
            10            V289-DACTG  PICTURE  9(8).                    CI0445
            10            V289-DNACG  PICTURE  9(8).                    CI0445
            10            V289-NGEOR  PICTURE  9(08).                   CI0445
            10            V289-CTID02 PICTURE  X(27).                   CI0445
            10            V289-CAFEE  PICTURE  X.                       CI0445
            10            V289-CCEXD  PICTURE  X(7).                    CI0445
            10            V289-NPRCD  PICTURE  X(16).                   CI0445
            10            V289-CCARC  PICTURE  X(16).                   CI0445
            10            V289-CLNAMA PICTURE  X(30).                   CI0445
            10            V289-CSECU  PICTURE  X(04).                   CI0445
            10            V289-IADRS  PICTURE  X.                       CI0445
            10            V289-CGMBR  PICTURE  X.                       CI0445
            10            V289-CASOP  PICTURE  X.                       CI0445
            10            V289-CCLPR  PICTURE  X.                       CI0445
            10            V289-CCLCH  PICTURE  X.                       CI0445
            10            V289-CCLSU  PICTURE  X.                       CI0445
            10            V289-CCLSB  PICTURE  X.                       CI0445
            10            V289-IWARN  PICTURE  X.                       CI0445
            10            V289-FILLER PICTURE  X(077).                  CI0445
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0445
          05              MS00-SUITE.                                   CI0445
            15       FILLER         PICTURE  X(00542).                  CI0445
       01                 MS03  REDEFINES      MS00.                    CI0445
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0445
                          COMPUTATIONAL-3.                              CI0445
            10            MS03-CMSSF  PICTURE  XX.                      CI0445
            10            MS03-DU09.                                    CI0445
            11            MS03-CMESA  PICTURE  S9(9)                    CI0445
                          BINARY.                                       CI0445
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0445
                          BINARY.                                       CI0445
            11            MS03-CMESB  PICTURE  S9(9)                    CI0445
                          BINARY.                                       CI0445
            11            MS03-CMSST  PICTURE  S9(9)                    CI0445
                          BINARY.                                       CI0445
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0445
                          BINARY.                                       CI0445
            11            MS03-QELLAA PICTURE  S9(9)                    CI0445
                          BINARY.                                       CI0445
            11            MS03-TMESS4 PICTURE  X(512).                  CI0445
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0445
            10            MX11-QMSGS  PICTURE  9(03).                   CI0445
            10            MX11-PJ09                                     CI0445
                          OCCURS       025     TIMES.                   CI0445
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0445
                          COMPUTATIONAL-3.                              CI0445
            11            MX11-CMESB  PICTURE  S9(9)                    CI0445
                          BINARY.                                       CI0445
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V289
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0IAG.    NOTE *********************************   *.            ACMCTI
       F0IAG.                                                           lv10
      ** SUB-FUNCTION TO PERFORM A    *                                 ACMCTI
      ** DUMMY DB2 CALL.              *                                 ACMCTI
      *********************************                                 ACMCTI
           EXEC SQL    SET                                              ACMCTI
                        :WS00-DATE = CURRENT_DATE            END-EXEC.  ACMCTI
           PERFORM     F93SQ THRU F93SQ-FN.                             ACMCTI
       F0IAG-FN. EXIT.
      *N01.      NOTE *************************************.            CI0445
      *               *                                   *             CI0445
      *               *INITIALISATIONS                    *             CI0445
      *               *                                   *             CI0445
      *               *************************************.            CI0445
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
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0445
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0445
      *               *                                   *             CI0445
      *               *FIN DE TRAITEMENT                  *             CI0445
      *               *                                   *             CI0445
      *               *************************************.            CI0445
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0445
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N25.      NOTE *************************************.
      *               *                                   *
      *               *INPUT VALIDATION & CALCULATIONS    *
      *               *                                   *
      *               *************************************.
       F25.           EXIT.                                             lv05
      *N25DB.    NOTE *CALCULATIONS                       *.
       F25DB.         EXIT.                                             lv10
      *N25DD.    NOTE *MOVE LENGTH                        *.
       F25DD.                                                           lv15
      *
           MOVE        LENGTH OF HTML-BLOB TO HTML-LEN.
       F25DD-FN. EXIT.
      *N25DG.    NOTE *CALCULATE THE LOOP COUNT           *.
       F25DG.                                                           lv15
      *
           COMPUTE     HTML-LIMIT = HTML-LEN / 200.
       F25DG-FN. EXIT.
       F25DB-FN. EXIT.
       F25-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *BUILD CONFIRM/VERIFY INFORMATION   *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35AT.    NOTE *MOVE COMMON HEADER INFORMATION     *.
       F35AT.                                                           lv10
           PERFORM     F95DP THRU F95DP-FN
           MOVE        1 TO HTML-PT
           STRING      HTML-COMMON-HDR-TAGS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35AT-FN. EXIT.
      *N35BB.    NOTE *POPULATE HTML PASS AREA            *.
       F35BB.                                                           lv10
                 IF    V289-CALTN = 'BEMM'                              DOT
           MOVE        WC00-CONF-MSG9 TO HTML-CONF-MSG
                 ELSE
           MOVE        WC00-CONF-MSG2 TO HTML-CONF-MSG.
                 IF    V289-CACTS = 'V'                                 DOT
           MOVE        WC00-VERF-IMPMSG TO HTML-IMPMSG
           HTML-IMPMSG1.
                 IF    V289-CACTS = 'S'                                 DOT
           MOVE        WC00-CONF-IMPMSG TO HTML-IMPMSG
           HTML-IMPMSG1.
           PERFORM     F95DN THRU F95DN-FN                              DOT
           MOVE        V289-CTIDND TO WS00-NCTIDE
           MOVE        V289-GECKD1 TO WS00-GECKD1
           MOVE        V289-CTIDA TO WS00-CTIDA
           MOVE        V289-CTTLN1 TO HTML-CTTLN1
      *
           MOVE        V289-CSPRDN TO HTML-PRCMN.
                 IF    V289-CSLCT = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CSLCT
           MOVE        V289-NGEOR TO HTML-NGEOR
                 ELSE
           MOVE        'NO ' TO HTML-CSLCT
           MOVE        'N/A' TO HTML-NGEOR.
                 IF    V289-IRMND = 'Y'                                 DOT
           MOVE        'YES' TO HTML-IRMND
           HTML-IRMND1
                 ELSE
           MOVE        'NO ' TO HTML-IRMND
           HTML-IRMND1.
      *V289-CALTN     HTML-CALTN                                        DOT
           MOVE        V289-CTTLN2 TO HTML-CTTLN2
           MOVE        V289-CTTLN3 TO HTML-CTTLN3
           MOVE        V289-CQACTL TO HTML-CQACTL
           MOVE        V289-CTTBO1 TO HTML-CTTBO1
           MOVE        V289-CTTBO2 TO HTML-CTTBO2
           MOVE        V289-CTTBO2 TO HTML-CTTBO2
           MOVE        V289-GEOPD2 TO HTML-GEOPD2
      *V289-CTTYPG    HTML-CTTYPG
           MOVE        V289-CTTYPG TO HTML-CTTYPG1
           HTML-CTTYPG4
           MOVE        V289-CTID01 TO HTML-CTID01
           HTML-CTID011
           HTML-CTID021
           HTML-CTID013
           MOVE        V289-CIRAP TO HTML-CIRAP
           HTML-CIRAP1
           HTML-CIRAP21
           MOVE        V289-CIRAP TO HTML-CIRAP
           MOVE        V289-CLORN TO HTML-CLORN
           MOVE        V289-NPBN TO HTML-NPBN
           MOVE        V289-CACTN TO HTML-CACTN1
      *               HTML-CACTN
           MOVE        V289-MPLNR2 TO HTML-MPLNR2
           MOVE        V289-ATROLL TO HTML-ATROLL
           MOVE        V289-CCONF TO HTML-CCONF
           MOVE        V289-NGEOPA TO HTML-NGEOPA
           MOVE        V289-MPMTT TO HTML-MPMTT3
           HTML-MPMTT
           MOVE        V289-APMTD TO WS00-APMTD
           MOVE        WS00-APMTD TO WL00-ACOTD2
           MOVE        V289-TDE25G TO HTML-TDE25G
           MOVE        V289-GEEND TO WS00-GEEND.
                 IF    V289-APMTD > ZEROES                              DOT
           MOVE        WL00-DCSO-AMOUNT TO
           HTML-ACOTD2
                 ELSE
           MOVE        WL00-DCSO-AMOUNT1 TO
           HTML-ACOTD2.
           MOVE        V289-CCARC TO HTML-CCARC                         DOT
           HTML-CCARC1
           MOVE        V289-NPRCD TO HTML-NPRCD
           HTML-NPRCD1
           MOVE        V289-CCEXD TO HTML-CCEXD
           HTML-CCEXD1
           MOVE        V289-CSECU TO HTML-CSECU
           HTML-CSECU1
           MOVE        V289-CLNAMA TO HTML-CLNAMA
           HTML-CLNAMA1
           MOVE        V289-GESAD1 TO HTML-GESAD1
           HTML-GESAD3
           MOVE        V289-GESAD2 TO HTML-GESAD2
           HTML-GESAD4.
                 IF    V289-IADRS = 'Y'                                 DOT
           MOVE        WL00-BACBOX TO HTML-GESAD1
           HTML-GESAD3.
                 IF    V289-CCLPR = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLPR.
                 IF    V289-CCLPR = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLPR.
                 IF    V289-CCLPR = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLPR.
                 IF    V289-CCLCH = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLCH.
                 IF    V289-CCLCH = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLCH.
                 IF    V289-CCLCH = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLCH.
                 IF    V289-CCLSU = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLSU.
                 IF    V289-CCLSU = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLSU.
                 IF    V289-CCLSU = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLSU.
                 IF    V289-CCLSB = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLSB.
                 IF    V289-CALTN = 'BEMM'                              DOT
                 AND   V289-CPORTA = 'F'
           MOVE        'Full Surrender' TO
           HTML-ASOPTX.
           MOVE        V289-MCSIG TO HTML-MCSIG                         DOT
           MOVE        V289-TTBAL TO HTML-TTBAL
           STRING      V289-NTR
           '-'
           V289-GECKD3
           DELIMITED BY SIZE
           INTO HTML-NTRX.
                 IF    V289-CALTN = 'BEMM'                              DOT
                 AND   V289-CACTS = 'V'
           MOVE        'Verify Compliance Questions' TO
           HTML-STEPTT.
                 IF    V289-CALTN = 'BEMM'                              DOT
                 AND   V289-CACTS = 'S'
           MOVE        'Compliance Questions' TO
           HTML-STEPTT.
       F35BB-FN. EXIT.
      *N35BC.    NOTE *FORMAT CONTRACT ID                 *.
       F35BC.                                                           lv10
      *
           MOVE        V289-CTIDND TO WS00-NCTIDE
           MOVE        V289-GECKD1 TO WS00-GECKD1
           MOVE        V289-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-CTID.
                 IF    V289-CTTYPG = 'MMTA'                             DOT
           MOVE        V289-CTID01 (12:16) TO WS00-NCTIDE
           MOVE        V289-CTID01 (1:3) TO WS00-CTIDA
           MOVE        V289-CTID01 (4:24) TO 7-CTIDN
           PERFORM     F95CD THRU F95CD-FN
           MOVE        WE00-GERTC TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID01
           HTML-CTID011
           HTML-CTID013
           HTML-CTID021.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   (V289-CALTN = 'ANPS'
                 OR    V289-CALTN = 'BEMM')
                 AND   V289-CAFEE = 'O'
           MOVE        V289-CTID02 (12:16) TO WS00-NCTIDE
           MOVE        V289-CTID02 (1:3) TO WS00-CTIDA
           MOVE        V289-CTID02 (4:24) TO 7-CTIDN
           PERFORM     F95CD THRU F95CD-FN
           MOVE        WE00-GERTC TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID02.
      *N35BD.    NOTE *FORMAT THE CURRENT DATE,           *.
       F35BD.                                                           lv15
      *EFFECTIVE DATE AND SEND TRANS
      *RECEIVED TIME
      *
           STRING      '000'
           V289-DCACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT
           INITIALIZE  I93B
           MOVE        V289-DCACG TO 7-DE00-DCACG
           STRING      V289-DCACG (1:4) '/'
           V289-DCACG (5:2) '/'
           V289-DCACG (7:2) '/'
           DELIMITED BY SIZE INTO
           I93B-DACTT.
      *I93B
           MOVE        V289-DCACD TO I93B-DACTT
           MOVE        'O' TO I93B-CEADC
           PERFORM     F96BB THRU F96BB-FN.
                 IF    I93B-GERTC NOT = 'Y'                             DOT
      *CHECK RETURN CODES
      *IF INVALID CODE, SEND ERROR MSG
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN                              ADU119
                 ELSE
      *MACRO RUN SUCCEED
           MOVE        I93B-GETIMM TO WS-GETIMM
           MOVE FUNCTION CURRENT-DATE(1:8)
                        TO  WS00-DATCE
           MOVE        WS-TIME1-HH TO WS-TIME2-HH
           MOVE        WS-TIME1-MM TO WS-TIME2-MM
           MOVE        WS-TIME1-SS TO WS-TIME2-SS
           MOVE FUNCTION CURRENT-DATE(9:6)
                        TO  WS00-TIMER
      *!ADS "WS00-DATCE     7-DE00-DCACG"
           MOVE        WS00-DATCE                                       CI0445
           TO DAT8E DAT6C                                               CI0445
           MOVE DAT81E TO DAT63C                                        CI0445
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0445
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0445
      *!ADM "7-DE00-DCACG     HTML-DCACG"
           MOVE        7-DE00-DCACG                                     CI0445
           TO DAT8E DAT6C                                               CI0445
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0445
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0445
           MOVE   DAT8C TO  HTML-DCACG.                                 CI0445
                 IF    (WS00-TIMER >= WS-GETIMT                         DOT
                 AND   V289-DCACG <= WS00-DATCE)
      *NEXT ACTG DATE IF AFTER MKT CLS
           STRING      '000' V289-DNACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT.
                 IF    WS00-TIMER (1:2) < 12                            DOT
           MOVE        'AM' TO WS00-AMPM
                 ELSE
           MOVE        'PM' TO WS00-AMPM.
                 IF    WS-TIME-HH > 12                                  DOT
           COMPUTE     WS-TIME-HH = WS-TIME-HH - 12.
                 IF    WS-TIME-HH = 00                                  DOT
           COMPUTE     WS-TIME-HH = WS-TIME-HH + 12.
           MOVE        'Central Time' TO WS00-CST                       DOT
           STRING      WS00-TIMER (1:2) ':'
           WS00-TIMER (3:2) ':'
           WS00-TIMER (5:2) ' '
           WS00-AMPM ' '
           WS00-CST
           DELIMITED BY SIZE INTO
           HTML-TIMER
           STRING      V289-DNPMT1 (1:2) '/'
           V289-DNPMT1 (3:2) '/'
           V289-DNPMT1 (5:4) ' '
           DELIMITED BY SIZE INTO
           HTML-DSTTL2X
           STRING      V289-DACTG (1:2) '/'
           V289-DACTG (3:2) '/'
           V289-DACTG (5:4) ' '
           DELIMITED BY SIZE INTO
           HTML-DACTGX.
       F35BD-FN. EXIT.
      *N35BE.    NOTE *FORMAT THE ORDER RECEIVED DATE,    *.
       F35BE.                                                           lv15
      *EFFECTIVE DATE AND SEND THE
      *ORDER RECEIVED TIME
      *
      *!ADM "V289-DAEDTO     HTML-DRECD"
           MOVE        V289-DAEDTO                                      CI0445
           TO DAT8E DAT6C                                               CI0445
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0445
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0445
           MOVE   DAT8C TO  HTML-DRECD                                  CI0445
      *
           STRING      V289-DPTIM DELIMITED BY SIZE
           ' Central Time'
           DELIMITED BY SIZE
           INTO HTML-TIMER1.
       F35BE-FN. EXIT.
       F35BC-FN. EXIT.
      *N35BF.    NOTE *FORMATING THE OUTPUT FILEDS        *.
       F35BF.                                                           lv10
      *
                 IF    V289-CTTYPG = 'SMTC'                             DOT
           MOVE        WL00-CTTYPG1 TO HTML-CTTYPG1
           HTML-CTTYPG4.
                 IF    V289-CTTYPG = 'MMTA'                             DOT
           MOVE        WL00-CTTYPG2 TO HTML-CTTYPG1
           HTML-CTTYPG4.
                 IF    V289-CTTYPG = 'SMDD'                             DOT
                 AND   V289-CALTN NOT = 'BEMM'
           MOVE        WL00-CTTYPG3 TO HTML-CTTYPG1
           HTML-CTTYPG4.
                 IF    V289-CTTYPG = 'SMDD'                             DOT
                 AND   V289-CALTN = 'BEMM'
           MOVE        WL00-CTTYPG6 TO HTML-CTTYPG1.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
           MOVE        WL00-CTTYPG4 TO HTML-CTTYPG1
           HTML-CTTYPG5
           HTML-CTTYPG4.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   V289-CAFEE = 'O'
           MOVE        WL00-MEEXP1 TO HTML-MEEXP.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   V289-CAFEE = 'C'
           MOVE        WL00-MEEXP2 TO HTML-MEEXP
           HTML-MEEXP1.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   V289-CAFEE = 'A'
           MOVE        WL00-MEEXP3 TO HTML-MEEXP.
                 IF    V289-CTTYPG = 'SMWT'                             DOT
           MOVE        WL00-CTTYPG5 TO HTML-CTTYPG4
           HTML-CTTYPG5.
       F35BF-FN. EXIT.
      *N35BG.    NOTE *FORMATING THE OUTPUT FILEDS        *.
       F35BG.                                                           lv10
      *
                 IF    V289-CIRAP = 'NA'                                DOT
           MOVE        WL00-CIRAP1 TO HTML-CIRAP
           HTML-CIRAP1
           HTML-CIRAPO
           HTML-CIRAP21.
                 IF    V289-CIRAP = 'CU'                                DOT
           MOVE        WL00-CIRAP2 TO HTML-CIRAP
           HTML-CIRAP1
           HTML-CIRAPO
           HTML-CIRAP21.
                 IF    V289-CIRAP = 'RO'                                DOT
           MOVE        WL00-CIRAP3 TO HTML-CIRAP
           HTML-CIRAP1
           HTML-CIRAPO
           HTML-CIRAP21.
                 IF    V289-CIRAP = 'PR'                                DOT
           MOVE        WL00-CIRAP4 TO HTML-CIRAP
           HTML-CIRAP1
           HTML-CIRAPO
           HTML-CIRAP21.
                 IF    V289-CIRAP = 'SC'                                DOT
           MOVE        WL00-CIRAP5 TO HTML-CIRAP
           HTML-CIRAP1
           HTML-CIRAPO
           HTML-CIRAP21.
       F35BG-FN. EXIT.
      *N35BH.    NOTE *FORMATING THE OUTPUT FILEDS        *.
       F35BH.                                                           lv10
      *
                 IF    V289-CACTN = 'SPAS'                              DOT
           MOVE        WL00-CACTN1 TO HTML-CACTN1.
                 IF    V289-CACTN = 'SPAS'                              DOT
                 AND   V289-CALTN = 'BEMM'
           MOVE        WL00-CACTN4 TO HTML-CACTN1.
                 IF    V289-CACTN = 'MSSA'                              DOT
           MOVE        WL00-CACTN2 TO HTML-CACTN1.
                 IF    V289-CACTN = 'MSSA'                              DOT
                 AND   V289-CALTN = 'BEMM'
           MOVE        WL00-CACTN3 TO HTML-CACTN1.
       F35BH-FN. EXIT.
      *N35BI.    NOTE *FORMATING THE OUTPUT FILEDS        *.
       F35BI.    IF    V289-CALTN = 'ANPS'                              lv10
                 NEXT SENTENCE ELSE GO TO     F35BI-FN.
      *
                 IF    V289-CPORTA = 'S'                                DOT
                 OR    V289-CPORTA = 'G'
           MOVE        WL00-RIDER1 TO HTML-RIDER.
                 IF    V289-CPORTA = 'A'                                DOT
                 OR    V289-CPORTA = 'B'
                 OR    V289-CPORTA = 'C'
           MOVE        WL00-RIDER2 TO HTML-RIDER.
       F35BI-FN. EXIT.
      *N35BJ.    NOTE *FORMATING THE OUTPUT FILEDS        *.
       F35BJ.                                                           lv10
      *
                 IF    V289-CTRN (1:1) = '1'                            DOT
           MOVE        WL00-CTRN1 TO HTML-CTRN1.
                 IF    V289-CTRN (2:1) = '1'                            DOT
           MOVE        WL00-CTRN2 TO HTML-CTRN2.
                 IF    V289-CTRN (3:1) = '1'                            DOT
           MOVE        WL00-CTRN3 TO HTML-CTRN3.
       F35BJ-FN. EXIT.
      *N35BK.    NOTE *TRANSACTION IS ANNUITY             *.
       F35BK.    IF    (V289-CALTN = 'ANPS'                             lv10
                 OR    V289-CALTN = 'BEMM')
                 AND   V289-CACTN = 'MSSA'
                 NEXT SENTENCE ELSE GO TO     F35BK-FN.
                 IF    V289-MFDNMH > SPACES                             DOT
           MOVE        V289-MFDNMH TO HTML-MFDNMH
           HTML-MFDNMH1
           MOVE        V289-ADBCRQ TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBCRQX
           HTML-ADBCRQX1.
                 IF    V289-MFDNME > SPACES                             DOT
           MOVE        V289-MFDNME TO HTML-MFDNME
           HTML-MFDNME1
           MOVE        V289-ADBRF1 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF1X
           HTML-ADBRF1Y.
                 IF    V289-MFDNMF > SPACES                             DOT
           MOVE        V289-MFDNMF TO HTML-MFDNMF
           HTML-MFDNMF1
           MOVE        V289-ADBRF2 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF2X
           HTML-ADBRF2Y.
                 IF    V289-MFDNMI > SPACES                             DOT
           MOVE        V289-MFDNMI TO HTML-MFDNMI
           HTML-MFDNMI1
           MOVE        V289-ADBRF3 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF3X
           HTML-ADBRF3Y.
                 IF    V289-MFDNMJ > SPACES                             DOT
           MOVE        V289-MFDNMJ TO HTML-MFDNMJ
           HTML-MFDNMJ1
           MOVE        V289-ADBRF4 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF4X
           HTML-ADBRF4Y.
                 IF    V289-MFDNMP > SPACES                             DOT
           MOVE        V289-MFDNMP TO HTML-MFDNMP
           HTML-MFDNMP1
           MOVE        V289-ADBRF5 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF5X
           HTML-ADBRF5Y.
                 IF    V289-MFDNMS > SPACES                             DOT
           MOVE        V289-MFDNMS TO HTML-MFDNMS
           HTML-MFDNMS1
           MOVE        V289-ADBRF6 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF6X
           HTML-ADBRF6Y.
                 IF    V289-MFDNM4 > SPACES                             DOT
           MOVE        V289-MFDNM4 TO HTML-MFDNM4
           HTML-MFDNM41
           MOVE        V289-ADBRF7 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF7X
           HTML-ADBRF7Y.
                 IF    V289-MFDNM5 > SPACES                             DOT
           MOVE        V289-MFDNM5 TO HTML-MFDNM5
           HTML-MFDNM51
           MOVE        V289-ADBRF8 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF8X
           HTML-ADBRF8Y.
                 IF    V289-MFDNM7 > SPACES                             DOT
           MOVE        V289-MFDNM7 TO HTML-MFDNM7
           HTML-MFDNM71
           MOVE        V289-ADBRF9 TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRF9X
           HTML-ADBRF9Y.
                 IF    V289-IRMND = 'Y'                                 DOT
                 AND   V289-CALTN = 'ANPS'
           MOVE        V289-AMNBR TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-AMNBRX.
       F35BK-FN. EXIT.
      *N35BL.    NOTE *TRANSCATION IS ANNUITY             *.
       F35BL.    IF    V289-CALTN = 'ANPS'                              lv10
                 AND   V289-IRMND = 'Y'
                 NEXT SENTENCE ELSE GO TO     F35BL-FN.
           MOVE        V289-AMNBR TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-AMNBRX.
       F35BL-FN. EXIT.
      *N35BM.    NOTE *STRING CONFIRMATION INFORMATION    *.
       F35BM.    IF    V289-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35BM-FN.
      *
           STRING      HTML-CONFIRM-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BM-FN. EXIT.
      *N35CB.    NOTE *MOVE VERIFY INFORMATION            *.
       F35CB.    IF    V289-CACTS = 'V'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35CB-FN.
       F35CB-FN. EXIT.
      *N35DB.    NOTE *MOVE COMMON INFORMATION            *.
       F35DB.         EXIT.                                             lv10
       F35DB-FN. EXIT.
      *N35DM.    NOTE *STRING COMMOM INFORMATION          *.
       F35DM.                                                           lv10
           PERFORM     F95DM THRU F95DM-FN
           STRING      HTML-COMMON-INFO1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DM-FN. EXIT.
      *N35DP.    NOTE *BUILD OWNERSHIP LINE2              *.
       F35DP.    IF    V289-CTTLN2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DP-FN.
      *
           STRING      V289-CTTLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN2
           STRING      HTML-FROM-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DP-FN. EXIT.
      *N35DR.    NOTE *BUILD OWNERSHIP LINE3              *.
       F35DR.    IF    V289-CTTLN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DR-FN.
      *
           STRING      V289-CTTLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN3
           STRING      HTML-FROM-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DR-FN. EXIT.
      *N35DU.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F35DU.    IF    V289-CTTBO1 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DU-FN.
      *
           STRING      V289-CTTBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
           STRING      HTML-FROM-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DU-FN. EXIT.
      *N35DW.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F35DW.    IF    V289-CTTBO2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DW-FN.
      *
           STRING      V289-CTTBO2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
           STRING      HTML-FROM-BENE-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DW-FN. EXIT.
      *N35EM.    NOTE *STRING COMMOM INFORMATION          *.
       F35EM.                                                           lv10
      *
           STRING      HTML-COMMON-INFO2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35EM-FN. EXIT.
       F35-FN.   EXIT.
      *N40BF.    NOTE *MOVE PAYMENT AMOUNT                *.
       F40BF.                                                           lv10
      *
           MOVE        V289-APMTD TO WS00-APMTD
           STRING      WS00-APMTD
           DELIMITED BY SIZE
           INTO HTML-APMTD
           MOVE        HTML-APMTD TO HTML-APMTD1
           HTML-APMTD3
           HTML-APMTD4
           HTML-APMTD5.
                 IF    V289-CALTN = 'BEMM'                              DOT
                 AND   V289-CASOP = 'A'
           MOVE        'NOT AVAILABLE' TO HTML-APMTD3.
      *N40BN.    NOTE *FORMAT WITHHOLDING PERCENT         *.
       F40BN.    IF    V289-CALTN NOT = 'BEMM'                          lv15
                 NEXT SENTENCE ELSE GO TO     F40BN-FN.
      *
                 IF    V289-TWITH = SPACES                              DOT
           MOVE        V289-PWHLD TO WS00-PWHLD
           STRING      WS00-PWHLD '%'
           DELIMITED BY SIZE
           INTO HTML-CTWHPB
           MOVE        HTML-CTWHPB TO HTML-CTWHPB1
                 ELSE
           MOVE        'N/A' TO HTML-CTWHPB
           HTML-CTWHPB1.
       F40BN-FN. EXIT.
      *N40BQ.    NOTE *FORMAT WITHHOLDING PERCENT         *.
       F40BQ.    IF    V289-CALTN = 'BEMM'                              lv15
                 NEXT SENTENCE ELSE GO TO     F40BQ-FN.
      *FOR BEMM
                 IF    V289-TWITH = SPACES                              DOT
           MOVE        V289-PWHLD TO WS00-PWHLD1
           STRING      WS00-PWHLD1 '%'
           DELIMITED BY SIZE
           INTO HTML-CTWHPB1
                 ELSE
           MOVE        'Do Not Withhold' TO HTML-CTWHPB1.
       F40BQ-FN. EXIT.
       F40BF-FN. EXIT.
      *N45BA.    NOTE *POPULATING VALUES                  *.
       F45BA.                                                           lv10
           MOVE        V289-CTTLN1 TO HTML-CTTLN1.
       F45BA-FN. EXIT.
      *N45BC.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BC.    IF    V289-CALTN = 'CFIA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45BC-FN.
      *
           STRING      HTML-VER-DEST-DETAIL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45BD.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45BD.                                                           lv15
      *
           STRING      HTML-VERIFY-TRANS-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BD-FN. EXIT.
      *N45BE.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BE.    IF    V289-APMTD NUMERIC                               lv15
                 AND   V289-APMTD > ZEROES
                 AND   V289-CTIDA NOT = 001
                 NEXT SENTENCE ELSE GO TO     F45BE-FN.
      *V289-APMTD   WS00-APMTD
      *WS00-APMTD   HTML-APMTD
           STRING      HTML-AMOUNT-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BE-FN. EXIT.
      *N45BF.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BF.                                                           lv15
      *
                 IF    V289-CTIDA = 002                                 DOT
           MOVE        'Mutual Fund: ' TO HTML-CERT-FUND.
                 IF    V289-CTIDA = 001                                 DOT
           MOVE        'Certificate: ' TO HTML-CERT-FUND.
                 IF    V289-CTIDA = (004 OR 005)                        DOT
           MOVE        'Annuity: ' TO HTML-CERT-FUND
           MOVE        'Full Surrender' TO HTML-NAME.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'I'
           MOVE        'Immediate Full Surrender' TO
           HTML-NAME.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'P'
           MOVE        HTML-APMTD TO HTML-APMTD2
           MOVE        'Date:' TO
           HTML-NAME1.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'F'
           MOVE        WC00-FULL-SUR-MSG1 TO
           HTML-NAME
           MOVE        WC00-FULL-SUR-MSG2 TO
           HTML-NAME1.
                 IF    V289-CTIDA = 002                                 DOT
                 AND   V289-CPORTA = 'F'
           MOVE        'Full Redemption' TO
           HTML-NAME.
                 IF    (V289-CTIDA = 002                                DOT
                 AND   V289-CPORTA = 'F')
                 OR    (V289-CTIDA = 001
                 AND   V289-CPORTA NOT = 'P')
                 OR    V289-CTIDA = (004 OR 005)
           STRING      HTML-FULL-REDEM-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'P'
           STRING      HTML-PAMOUNT-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   ((V289-CPORTA = 'F'
                 AND   V289-DACTG NOT = 0)
                 OR    V289-CPORTA = 'P')
           STRING      HTML-DATE-DETAIL-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BF-FN. EXIT.
      *N45BG.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BG.    IF    V289-PACT1 NUMERIC                               lv15
                 AND   V289-PACT1 > ZEROES
                 NEXT SENTENCE ELSE GO TO     F45BG-FN.
           MOVE        V289-PACT1 TO WS00-PACT1
           STRING      WS00-PACT1 '%'
           DELIMITED BY SIZE
           INTO HTML-PACT1X.
                 IF    V289-CTIDA = 001                                 DOT
           MOVE        'Certificate: ' TO HTML-CERT-FUNDP.
                 IF    V289-CTIDA = 002                                 DOT
           MOVE        'Mutual Fund: ' TO HTML-CERT-FUNDP.
           STRING      HTML-PERC-ACCOUNT-CFIA                           DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BG-FN. EXIT.
      *N45BH.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BH.    IF    V289-QSHOWQ NUMERIC                              lv15
                 AND   V289-QSHOWQ > ZEROES
                 NEXT SENTENCE ELSE GO TO     F45BH-FN.
           MOVE        V289-QSHOWQ TO WS00-QSHOWQ
           MOVE        WS00-QSHOWQ TO HTML-QSHOWQX.
                 IF    V289-CTIDA = 001                                 DOT
           MOVE        'Certificate: ' TO HTML-CERT-FUNDS.
                 IF    V289-CTIDA = 002                                 DOT
           MOVE        'Mutual Fund: ' TO HTML-CERT-FUNDS.
           STRING      HTML-NUMB-SHARES-CFIA                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BH-FN. EXIT.
      *N45BJ.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45BJ.    IF    V289-CTIDA NOT =                                 lv15
                       (004 AND 005)
                 NEXT SENTENCE ELSE GO TO     F45BJ-FN.
           STRING      HTML-TRANS-CFIA-CONTD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BJ-FN. EXIT.
      *N45BK.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45BK.    IF    V289-CTIDA = (004 OR 005)                        lv15
                 NEXT SENTENCE ELSE GO TO     F45BK-FN.
      *
           STRING      HTML-TRANS-ANPS-CONTD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BK-FN. EXIT.
       F45BC-FN. EXIT.
      *N45BN.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BN.    IF    V289-CALTN = 'ANPS'                              lv10
                 OR    V289-CALTN = 'BEMM'
                 NEXT SENTENCE ELSE GO TO     F45BN-FN.
           STRING      HTML-DEST-DETAIL-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45BP.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BP.    IF    V289-CTTYPG = 'SMEM'                             lv15
                 NEXT SENTENCE ELSE GO TO     F45BP-FN.
      * CHOOSE BY EXPRESS MAIL
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-DEST-METHOD-ANPS-SMEM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-DEST-METHOD-BEMM-SMEM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45BR.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BR.    IF    V289-CAFEE = 'O'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45BR-FN.
      *FROM OTHER AMERIPRISE ACCOUNT
           STRING      HTML-DEST-ACCOUNT-ANPS-SMEM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BR-FN. EXIT.
      *N45BT.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BT.    IF    V289-CAFEE = 'C'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45BT-FN.
      *CHARGE FROM CREDIT CARD
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-DEST-CCDETAIL-ANPS-SMEM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-DEST-CCDETAIL-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BT-FN. EXIT.
       F45BP-FN. EXIT.
      *N45CE.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45CE.    IF    V289-CTTYPG = 'MMTA'                             lv15
                 NEXT SENTENCE ELSE GO TO     F45CE-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-DEST-ACCOUNT-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-DEST-ACCOUNT-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45CG.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45CG.                                                           lv20
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-DIST-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CG-FN. EXIT.
       F45CE-FN. EXIT.
      *N45CH.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45CH.    IF    V289-CTTYPG = 'SMDD'                             lv15
                 NEXT SENTENCE ELSE GO TO     F45CH-FN.
      *
           STRING      HTML-DEST-BANK-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CH-FN. EXIT.
      *N45CI.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45CI.                                                           lv15
      *
           STRING      HTML-DIST-ANPS-CONTD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CI-FN. EXIT.
      *N45CJ.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CJ.                                                           lv15
      *STRING GENERIC ANPS TRANSACTION
      *INFO WITH SURRENDER TYPE
      *SPECIFIC INFORMATION
      * 'S' -  SPECIFIED AMOUNT
      * 'G' - MAXIMUM AVAILABLE WITHOUT
      *       CLOSING ACCOUNT
           STRING      HTML-TRANS-DETAIL-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-TRANS-ANPS-PARTIAL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'F'                                DOT
           STRING      HTML-TRANS-DETAIL-BEMM-FULL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'P'                                DOT
                 AND   V289-CASOP = 'A'
           STRING      HTML-TRANS-BEMM-PARTIALA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'P'                                DOT
                 AND   V289-CASOP = 'B'
           STRING      HTML-TRANS-BEMM-PARTIALB
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'P'                                DOT
                 AND   V289-CASOP = 'C'
           STRING      HTML-TRANS-BEMM-PARTIALC
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'P'                                DOT
                 AND   V289-CASOP = 'D'
           STRING      HTML-TRANS-BEMM-PARTIALD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'P'                                DOT
           STRING      HTML-AMOUNT-DETAIL-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'A'                                DOT
           STRING      HTML-TRANS-ANPS-PARTIALA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'B'                                DOT
           STRING      HTML-TRANS-ANPS-PARTIALB
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'C'                                DOT
                 AND   V289-IWARN = 'Y'
           STRING      HTML-TRANS-ANPS-PARTIALC
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-TRANS-ANPS-CHECK
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'S'                                DOT
           STRING      HTML-TRANS-ANPS-PARTIALS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'G'                                DOT
           STRING      HTML-TRANS-ANPS-PARTIALG
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45CL.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CL.    IF    V289-IRMND = 'N'                                 lv20
                 AND   V289-CALTN NOT = 'BEMM'
                 NEXT SENTENCE ELSE GO TO     F45CL-FN.
                 IF    V289-CPORTA = 'C'                                DOT
                 AND   V289-IWARN = 'Y'
           STRING      HTML-RMD-TABLE-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-RMD-DETAIL-ANPS                             DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CL-FN. EXIT.
      *N45CN.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CN.    IF    V289-IRMND = 'Y'                                 lv20
                 AND   V289-CALTN NOT = 'BEMM'
                 NEXT SENTENCE ELSE GO TO     F45CN-FN.
                 IF    V289-CPORTA = 'C'                                DOT
                 AND   V289-IWARN = 'Y'
           STRING      HTML-RMD-TABLE-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-RMD-AMOUNT-ANPS                             DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CN-FN. EXIT.
       F45CJ-FN. EXIT.
      *N45CO.    NOTE *SKIP FOR FULL SURRENDER OF BEMM    *.
       F45CO.    IF    V289-CALTN = 'BEMM'                              lv15
                 AND   V289-CPORTA = 'F'
                 NEXT SENTENCE ELSE GO TO     F45CO-FN.
       F45CO-900. GO TO F45CP-FN.
       F45CO-FN. EXIT.
      *N45CP.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CP.                                                           lv15
      *
           STRING      HTML-SUB-ACCOUNT-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45CQ.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CQ.    IF    V289-CACTN = 'MSSA'                              lv20
                 NEXT SENTENCE ELSE GO TO     F45CQ-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT1-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT1-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45CR.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CR.    IF    V289-MFDNME > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CR-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT2-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT2-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CR-FN. EXIT.
      *N45CS.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CS.    IF    V289-MFDNMF > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CS-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT3-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT3-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CS-FN. EXIT.
      *N45CT.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CT.    IF    V289-MFDNMI > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CT-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT4-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT4-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CT-FN. EXIT.
      *N45CV.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CV.    IF    V289-MFDNMJ > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CV-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT5-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT5-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CV-FN. EXIT.
      *N45CW.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CW.    IF    V289-MFDNMP > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CW-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT6-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT6-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CW-FN. EXIT.
      *N45CX.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CX.    IF    V289-MFDNMS > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CX-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT7-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT7-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CX-FN. EXIT.
      *N45CY.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CY.    IF    V289-MFDNM4 > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CY-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT8-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT8-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CY-FN. EXIT.
      *N45CZ.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45CZ.    IF    V289-MFDNM5 > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45CZ-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT9-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT9-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CZ-FN. EXIT.
      *N45DA.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DA.    IF    V289-MFDNM7 > SPACES                             lv25
                 NEXT SENTENCE ELSE GO TO     F45DA-FN.
      *
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-SUB-ACCOUNT10-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'BEMM'                              DOT
           STRING      HTML-SUB-ACCOUNT10-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DA-FN. EXIT.
      *N45DB.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DB.                                                           lv25
      *
           STRING      HTML-SUB-ACCOUNT-ANPS-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DB-FN. EXIT.
       F45CQ-FN. EXIT.
       F45CP-FN. EXIT.
      *N45DC.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DC.                                                           lv15
      *
           STRING      HTML-TRANS-ANPS-CONTD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DC-FN. EXIT.
      *N45DD.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DD.    IF    V289-CALTN = 'BEMM'                              lv15
                 NEXT SENTENCE ELSE GO TO     F45DD-FN.
      *
           STRING      HTML-TRANS-BEMM-CONTD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DD-FN. EXIT.
      *N45DE.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DE.    IF    V289-CALTN = 'BEMM'                              lv15
                 NEXT SENTENCE ELSE GO TO     F45DE-FN.
      *
      *INIT THE VALUE OF WIDTH
                 IF    V289-CTTYPG = 'MMTA'                             DOT
           MOVE        '650' TO HTML-WIDTH
                 ELSE
           MOVE        '49%' TO HTML-WIDTH.
           STRING      HTML-COMP-STEP-BEMM                              DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DE-FN. EXIT.
      *N45DF.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DF.    IF    V289-CALTN = 'BEMM'                              lv15
                 NEXT SENTENCE ELSE GO TO     F45DF-FN.
      *
           STRING      HTML-VERIFY-COMP-QA2-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DF-FN. EXIT.
      *N45DG.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DG.    IF    V289-CALTN = 'BEMM'                              lv15
                 NEXT SENTENCE ELSE GO TO     F45DG-FN.
      *
           STRING      HTML-VERIFY-COMP-QA3-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DG-FN. EXIT.
      *N45DH.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DH.    IF    V289-CALTN = 'BEMM'                              lv15
                 AND   V289-CTTYPG = 'MMTA'
                 NEXT SENTENCE ELSE GO TO     F45DH-FN.
      *FOR MMTA INIT THE VALUE
           STRING      HTML-VERIFY-COMP-QA4-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DH-FN. EXIT.
       F45BN-FN. EXIT.
      *N45DL.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DL.    IF    V289-CALTN = 'DCSO'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45DL-FN.
      *
           STRING      HTML-DEST-DETAIL-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45DM.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DM.    IF    V289-CTTYPG = 'SMDD'                             lv15
                 OR    'SMWT'
                 NEXT SENTENCE ELSE GO TO     F45DM-FN.
           STRING      HTML-DEST-BANK-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DM-FN. EXIT.
      *N45DP.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DP.    IF    V289-CTTYPG = 'MMTA'                             lv15
                 NEXT SENTENCE ELSE GO TO     F45DP-FN.
      *
           STRING      HTML-ACCT-DETAIL-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45DR.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45DR.                                                           lv20
      *
           STRING      HTML-DIST-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DR-FN. EXIT.
       F45DP-FN. EXIT.
      *N45DS.    NOTE *HTML-TABLE-CLOSE-DCSO              *.
       F45DS.                                                           lv15
      *
           STRING      HTML-TABLE-CLOSE-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DS-FN. EXIT.
      *N45DT.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DT.                                                           lv15
      *
           STRING      HTML-TRANS-DETAIL-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DT-FN. EXIT.
      *N45DU.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DU.                                                           lv15
                 IF    V289-APMTD > ZEROES                              DOT
           STRING      HTML-RMD-DETAIL-ANPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *                                                                 DOT
           PERFORM     F95DA THRU F95DA-FN
           STRING      HTML-MCFRH-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DU-FN. EXIT.
      *N45DV.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DV.    IF    V289-CTRN (1:1) = '1'                            lv15
                 NEXT SENTENCE ELSE GO TO     F45DV-FN.
      *
           STRING      HTML-MCFR1-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DV-FN. EXIT.
      *N45DW.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DW.    IF    V289-CTRN (2:1) = '1'                            lv15
                 NEXT SENTENCE ELSE GO TO     F45DW-FN.
      *
           STRING      HTML-MCFR2-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DW-FN. EXIT.
      *N45DX.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DX.    IF    V289-CTRN (3:1) = '1'                            lv15
                 NEXT SENTENCE ELSE GO TO     F45DX-FN.
      *
           STRING      HTML-MCFR3-DCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DX-FN. EXIT.
      *N45DY.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45DY.                                                           lv15
      *
           STRING      HTML-TRANS-ANPS-CONTD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DY-FN. EXIT.
       F45DL-FN. EXIT.
      *N45GA.    NOTE *FOR VERIFY DEST                    *.
       F45GA.    IF    V289-CALTN = 'MFPS'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45GA-FN.
      *
           STRING      HTML-VER-DEST-DETAIL2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTTYPG = 'SMWT'                             DOT
           STRING      HTML-DEST-BANK-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-VER-DEST-DETAIL2-END                        DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45GC.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45GC.                                                           lv15
      *
           STRING      HTML-VERIFY-TRANS-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45GC-FN. EXIT.
      *N45GE.    NOTE *FOR VERIFY DESTINATION/            *.
       F45GE.    IF    V289-APMTD NUMERIC                               lv15
                 AND   V289-APMTD > ZEROES
                 AND   V289-CTIDA NOT = 001
                 NEXT SENTENCE ELSE GO TO     F45GE-FN.
      *TRANSACTION DETAILS
      *V289-APMTD   WS00-APMTD
      *WS00-APMTD   HTML-APMTD
           STRING      HTML-AMOUNT-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45GE-FN. EXIT.
      *N45GF.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45GF.                                                           lv15
      *
                 IF    V289-CTIDA = 002                                 DOT
           MOVE        'Mutual Fund: ' TO HTML-CERT-FUND.
                 IF    V289-CTIDA = 001                                 DOT
           MOVE        'Certificate: ' TO HTML-CERT-FUND.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'I'
           MOVE        'Immediate Full Surrender' TO
           HTML-NAME.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'P'
           MOVE        'Partial amount' TO
           HTML-NAME
           MOVE        'Date:' TO
           HTML-NAME1
           MOVE        HTML-APMTD TO HTML-APMTD2.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'F'
           MOVE        WC00-FULL-SUR-MSG1 TO
           HTML-NAME
           MOVE        WC00-FULL-SUR-MSG2 TO
           HTML-NAME1.
                 IF    V289-CTIDA = 002                                 DOT
                 AND   V289-CPORTA = 'F'
           MOVE        'Full Redemption' TO
           HTML-NAME.
                 IF    (V289-CTIDA = 002                                DOT
                 AND   V289-CPORTA = 'F')
                 OR    (V289-CTIDA = 001
                 AND   V289-CPORTA NOT = 'P')
           STRING      HTML-FULL-REDEM-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   V289-CPORTA = 'P'
           STRING      HTML-PAMOUNT-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTIDA = 001                                 DOT
                 AND   ((V289-CPORTA = 'F'
                 AND   V289-DACTG NOT = 0)
                 OR    V289-CPORTA = 'P')
           STRING      HTML-DATE-DETAIL-CFIA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45GF-FN. EXIT.
      *N45GG.    NOTE *FOR VERIFY AMOUT FOR MF DETAILS    *.
       F45GG.    IF    V289-PACT1 NUMERIC                               lv15
                 AND   V289-PACT1 > ZEROES
                 NEXT SENTENCE ELSE GO TO     F45GG-FN.
           MOVE        V289-PACT1 TO WS00-PACT1
           STRING      WS00-PACT1 '%'
           DELIMITED BY SIZE
           INTO HTML-PACT1X.
                 IF    V289-CTIDA = 001                                 DOT
           MOVE        'Certificate: ' TO HTML-CERT-FUNDP.
                 IF    V289-CTIDA = 002                                 DOT
           MOVE        'Mutual Fund: ' TO HTML-CERT-FUNDP.
           STRING      HTML-PERC-ACCOUNT-CFIA                           DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45GG-FN. EXIT.
      *N45GH.    NOTE *FOR VERIFY NO OF SHARES FOR MF     *.
       F45GH.    IF    V289-QSHOWQ NUMERIC                              lv15
                 AND   V289-QSHOWQ > ZEROES
                 NEXT SENTENCE ELSE GO TO     F45GH-FN.
           MOVE        V289-QSHOWQ TO WS00-QSHOWQ
           MOVE        WS00-QSHOWQ TO HTML-QSHOWQX.
                 IF    V289-CTIDA = 002                                 DOT
           MOVE        'Mutual Fund: ' TO HTML-CERT-FUNDS.
                 IF    V289-CTIDA = 001                                 DOT
           MOVE        'Certificate: ' TO HTML-CERT-FUNDS.
           STRING      HTML-NUMB-SHARES-CFIA                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45GH-FN. EXIT.
      *N45GJ.    NOTE *FOR HTML VERIFY TRANSACTION INFO   *.
       F45GJ.                                                           lv15
      *
           STRING      HTML-TRANS-CFIA-CONTD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45GJ-FN. EXIT.
       F45GA-FN. EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *BUILD INFO MSGS AND COMPLIANCE     *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
      *QUESTIONS FOR VERIFY/CONFIRM
      *N65BB.    NOTE *FOR VERIFY/CONFIRM PAGE            *.
       F65BB.         EXIT.                                             lv10
      *N65BC.    NOTE *LIST IMPLICATION MESSAGES          *.
       F65BC.    IF    V289-CALTN = 'BEMM'                              lv15
                 NEXT SENTENCE ELSE GO TO     F65BC-FN.
      *FOR BEMM (RAVA5) TRANSACTION
           STRING      HTML-IMP-MESSAGE1-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'F'                                DOT
           STRING      HTML-IMP-MES3-FULL-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CPORTA = 'P'                                DOT
           STRING      HTML-IMP-MES3-PARTIAL-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-COMPLETE-TIME1-BEMM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTTYPG = 'MMTA'                             DOT
           STRING      HTML-DEST-ANNUITY-IMP
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-IMP-MES-LAST-BEMM                           DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65BC-900. GO TO F65BE-FN.
       F65BC-FN. EXIT.
      *N65BE.    NOTE *LIST IMPLICATION MESSAGES          *.
       F65BE.                                                           lv15
      *FOR ALL TRANS EXCEPT BEMM(RAVA5)
           PERFORM     F95DO THRU F95DO-FN.
                 IF    V289-CTIDA = 021                                 DOT
                 AND   V289-CALTN = 'DCSO'
                 AND   V289-CTTYPG = ('SMEM'
                 OR    'SMWT' OR 'SMDD' OR 'SMTC')
           MOVE        WC00-MSG9-AMOUNT-BROK TO
           HTML-CONF-MSG9.
                 IF    V289-CALTN = 'ANPS'                              DOT
           STRING      HTML-IMP-MESSAGE11
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-ANPS-MESSAGE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-ANPS-MESSAGE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-ANPS-MESSAGE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-MESSAGE12
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      HTML-IMP-MESSAGE11
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-MESSAGE12
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTIDA = 001                                 DOT
           STRING      HTML-IMP-MESSAGE-CERT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      HTML-IMP-MESSAGE13
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CALTN = 'CFIA'                              DOT
                 AND   V289-CTTYPG = 'MMTA'
                 AND   V289-CGMBR = ('X' OR 'Y')
           STRING      HTML-IMP-MESSAGE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   V289-CALTN NOT = 'ANPS'
                 AND   V289-CALTN NOT = 'DCSO'
                 OR    (V289-CALTN = 'DCSO'
                 AND   V289-CTIDA NOT = 021)
      *STRING MESSAGE FOR EXPRESS MAIL
           STRING      HTML-IMP-MESSAGE-EXPRESS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    (V289-CTTYPG = 'SMEM'                            DOT
                 OR    V289-CTTYPG = 'SMWT'
                 OR    V289-CTTYPG = 'MMTA')
                 AND   V289-CTIDA = 002
      *STRING MESSAGE FOR EXPRESS MAIL
      *,WIRE TRANSFER AND MMTA
           STRING      HTML-IMP-MESSAGE-BIFURCATION
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   V289-CALTN = 'DCSO'
                 AND   V289-CTIDA = 021
           STRING      HTML-IMP-MES-EXPRESS-BROKDCSO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           INITIALIZE  HTML-CONF-MSG7
           MOVE        WC00-MES-EXPRESS-BROKDCSO TO
           HTML-CONF-MSG7.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   V289-CALTN = 'ANPS'
                 AND   V289-CAFEE = 'A'
           STRING      HTML-IMP-MES-EXPRESS-ANPS-A
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTTYPG = 'SMEM'                             DOT
                 AND   V289-CALTN = 'ANPS'
                 AND   V289-CAFEE = ('O' OR 'C')
           STRING      HTML-IMP-MES-EXPRESS-ANPS-OC
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V289-CTTYPG = 'MMTA'                             DOT
           MOVE        HTML-DEST-ANNUITY-IMP TO
           HTML-CONF-MSG10.
           STRING      HTML-IMP-MESSAGE-LAST                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    HTML-CONF-MSG6 = SPACES                          DOT
           PERFORM     F95FA THRU F95FA-FN.
           STRING      HTML-IMP-LAST-END                                DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65BE-FN. EXIT.
       F65BB-FN. EXIT.
       F65-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *POPULATE OUTPUT FIELDS             *
      *               *                                   *
      *               *************************************.
       F70.                                                             lv05
           MOVE        1 TO TALLI.
      *N70BB.    NOTE *UNSTRING HTML BLOB                 *.
       F70BB.                                                           lv10
           MOVE        1 TO HTML-PT.
      *N70BD.    NOTE *LOOP THRU XML-BLOB                 *.
       F70BD.                                                           lv15
           MOVE        1                        TO J70BDR
                                    GO TO     F70BD-B.
       F70BD-A.
           ADD         1                        TO J70BDR.
       F70BD-B.
           IF          J70BDR                   >  HTML-LIMIT
                                    GO TO     F70BD-FN.
      *
           MOVE        HTML-PT TO TEMP-PT
           MOVE        TEMP-PT TO HTML-PT
           UNSTRING    HTML-BLOB  INTO  HTML-TEXT
                      WITH  POINTER HTML-PT.
      *N70BG.    NOTE *MOVE WS VARIABLE INTO THTML        *.
       F70BG.    IF    HTML-TEXT NOT = SPACES                           lv20
                 NEXT SENTENCE ELSE GO TO     F70BG-FN.
      *
           MOVE        01 TO V289-CHTML (TALLI)
           MOVE        HTML-TEXT TO V289-THTML (TALLI)
           ADD         1 TO TALLI.
       F70BG-FN. EXIT.
       F70BD-900. GO TO F70BD-A.
       F70BD-FN. EXIT.
       F70BB-FN. EXIT.
      *N70EB.    NOTE *MOVE END OF FILE MARKER            *.
       F70EB.                                                           lv10
      *
           MOVE        99 TO V289-CHTML (TALLI)
           MOVE        SPACES TO V289-THTML (TALLI).
      *N70EF.    NOTE *CALCULATE THTML LENGTH             *.
       F70EF.                                                           lv15
      *
           COMPUTE     V289-QBLCK = TALLI.
       F70EF-FN. EXIT.
       F70EB-FN. EXIT.
       F70-FN.   EXIT.
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
      *N93SQ.    NOTE *SQL ERROR HANDLING                 *.            ADB221
       F93SQ.                                                           lv10
           MOVE        SQLCODE TO 7-TEST-SQLCODE.                       ADB221
      *N93SR.    NOTE *TEST FOR NORMAL PROCESSING CODE    *.            ADB221
       F93SR.    IF    SQLCODE = +0                                     lv15
                 NEXT SENTENCE ELSE GO TO     F93SR-FN.                 ADB221
           MOVE        '0' TO IK                                        ADB221
           MOVE        ZERO TO 7-Q913-COUNT                             ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SR-FN. EXIT.
      *N93SS.    NOTE *CHECK FOR NON-CRITICAL SQLCODE     *.            ADB221
       F93SS.    IF    SQLCODE = +100                                   lv15
                 OR    SQLCODE = -803                                   ADB221
                 OR    SQLCODE = -811                                   ADB221
                 OR    SQLCODE = -904                                   ADB221
                 NEXT SENTENCE ELSE GO TO     F93SS-FN.                 ADB221
           MOVE        ZERO TO 7-Q913-COUNT                             ADB221
           MOVE        '1' TO IK                                        ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SS-FN. EXIT.
      *N93ST.    NOTE *CHECK FOR RESOURCE-IN-USE          *.            ADB221
       F93ST.    IF    SQLCODE = -913                                   lv15
                 NEXT SENTENCE ELSE GO TO     F93ST-FN.                 ADB221
      *N93SU.    NOTE *CHECK TO SEE IF ATTEMPT RETRY      *.            ADB221
       F93SU.    IF    7-Q913-COUNT < +0                                lv20
                 AND   7-Q913-COUNT < 7-MAXM-RETRY                      ADB221
                 NEXT SENTENCE ELSE GO TO     F93SU-FN.                 ADB221
           ADD         +1 TO 7-Q913-COUNT                               ADB221
           MOVE        '1' TO IK                                        ADB221
               GO TO     F93SQ-FN.                                      ADB221
       F93SU-FN. EXIT.
       F93ST-FN. EXIT.
      *N93SX.    NOTE **** CRITICAL SQLCODE ** ABEND **   *.            ADB221
       F93SX.                                                           lv15
      *COMMENTED OUT UNTIL PROBLEMS                                     ADB221
      *WITH THIS ARE RESOLVED!!!!!!                                     ADB221
      *CAL DSNTIAR FOR TEXT EXPLANATION                                 ADB221
      *CAL 'DSNTIAR' USING SQLCA                                        ADB221
      *                7-SQLR-MESSAGE                                   ADB221
      *                7-SQLR-TEXT-LEN.                                 ADB221
      *FORMAT CICS ABEND CODE AND ABEND                                 ADB221
           MOVE        SQLCODE TO 7-DB2-ABEND.                          ADB221
                 IF    SQLCODE NEGATIVE                                 DOT
           MOVE        '-' TO 7-DB2-FIRST                               ADB221
                 ELSE                                                   ADB221
           MOVE        '+' TO 7-DB2-FIRST.                              ADB221
           EXEC CICS   ABEND ABCODE (7-DB2-ABEND)                       DOT
                       CANCEL                                END-EXEC.  ADB221
       F93SX-FN. EXIT.
       F93SQ-FN. EXIT.
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *STRING CONCAT                      *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94AA.    NOTE *SUB ACCOUNT FUND NAME CONCAT       *.
       F94AA.                                                           lv10
           STRING      '<TR>' DELIMITED BY SIZE
           WS00-MFDNME DELIMITED BY SIZE
           '</TD><TD ALIGN=RIGHT>'
           DELIMITED BY SIZE
           WS00-APMTD DELIMITED BY SIZE
           '</TD></TR>' DELIMITED BY SIZE
           INTO WS00-SUBACCT-FNDNAME.
       F94AA-FN. EXIT.
       F94-FN.   EXIT.
      *N95CD.    NOTE *CALCULATE CHECK DIGIT FOR DEST     *.
       F95CD.                                                           lv10
           MOVE        SPACES TO WE00                                   DOT
           MOVE        7-CTIDN TO                                       $CHECK
           WE00-CTIDN                                                   $CHECK
           CALL        'CMU002DY' USING WE00.                           $CHECK
       F95CD-FN. EXIT.
      *N95DA.    NOTE *POPULATE COPYBOOK MULT-SOURCES     *.
       F95DA.                                                           lv10
                 IF    V289-CTRN (1:1) = '1'                            DOT
                 AND   V289-CTRN (2:1) = '1'
           MOVE        '<TR><TD></TD><TD><BR><B>' TO
           HTML-MCFR2
                 ELSE
           MOVE        SPACES TO HTML-MCFR2.
                 IF    (V289-CTRN (1:1) = '1'                           DOT
                 OR    V289-CTRN (2:1) = '1')
                 AND   V289-CTRN (3:1) = '1'
           MOVE        '<TR><TD></TD><TD><BR><B>' TO
           HTML-MCFR3
                 ELSE
           MOVE        SPACES TO HTML-MCFR3.
       F95DA-FN. EXIT.
      *N95DM.    NOTE *POPULATE STYLE SHEET TAGS          *.
       F95DM.                                                           lv10
                 IF    V289-CACTS = 'V'                                 DOT
      ******************************
           STRING      'style="margin-left:5px;'
           'margin-right:35px;"'
           DELIMITED BY SIZE
           INTO HTML-TAG1.
                 IF    V289-CACTS = 'S'                                 DOT
      ******************************
           MOVE        '</TD></TR></TABLE>' TO
           HTML-TABLE-BOTTOM.
       F95DM-FN. EXIT.
      *N95DN.    NOTE *POPULATE DSCO AMOUNT               *.
       F95DN.                                                           lv10
                 IF    V289-APMTD = ZEROES                              DOT
      ******************************
           MOVE        WL00-DCSO-AMOUNT1 TO
           HTML-ACOTD2.
                 IF    V289-APMTD > ZEROES                              DOT
      ******************************
           MOVE        WL00-DCSO-AMOUNT TO
           HTML-ACOTD2.
       F95DN-FN. EXIT.
      *N95DO.    NOTE *POPULATE DCSO MESSAGE              *.
       F95DO.    IF    V289-CALTN = 'DCSO'                              lv10
                 NEXT SENTENCE ELSE GO TO     F95DO-FN.
           MOVE        WC00-CONF-MSG6-BROK TO
           HTML-CONF-MSG5.
                 IF    V289-CTTYPG = 'MMTA'                             DOT
           MOVE        WC00-MSG8-MMTA-BROK TO
           HTML-CONF-MSG8.
                 IF    V289-CTTYPG = 'SMWT'                             DOT
           MOVE        WC00-MSG8-SMWT-BROK TO
           HTML-CONF-MSG6
           MOVE        WC00-MSG6-SMWT-BROK TO
           HTML-CONF-MSG7
           MOVE        WC00-MSG7-SMWT-BROK TO
           HTML-CONF-MSG8.
       F95DO-FN. EXIT.
      *N95DP.    NOTE *POPULATE DCSO AMOUNT               *.
       F95DP.                                                           lv10
                 IF    V289-CACTS = 'V'                                 DOT
      ******************************
           MOVE        WC00-VERF-MSG1 TO HTML-VERIFY1
           HTML-VERIFY3
           HTML-VERIFY8
           HTML-VERIFY11
           MOVE        WC00-VERF-MSG2 TO HTML-VERIFY2
           HTML-VERIFY4
           HTML-VERIFY5.
                 IF    V289-CACTS = 'S'                                 DOT
      ******************************
           MOVE        WC00-VERF-MSG4 TO HTML-VERIFY1
           HTML-VERIFY3
           HTML-VERIFY8
           HTML-VERIFY11
           MOVE        WC00-VERF-MSG5 TO HTML-VERIFY2
           HTML-VERIFY4
           HTML-VERIFY5.
       F95DP-FN. EXIT.
      *N95FA.    NOTE *POPULATE COMPLETION TIME           *.
       F95FA.                                                           lv10
           STRING      HTML-IMP-COMPLETE-TIME1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-COMPLETE-TIME2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F95FA-FN. EXIT.
      *N96BB.    NOTE *********************************   *.            ACMCTI
       F96BB.                                                           lv10
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
       F96BB-FN. EXIT.
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
