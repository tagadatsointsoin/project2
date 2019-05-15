       IDENTIFICATION DIVISION.                                         CI0446
       PROGRAM-ID.  CI0446P.                                            CI0446
      *AUTHOR.         BUILD  HTML PAGE.                                CI0446
      *DATE-COMPILED.   09/08/14.                                       CI0446
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
       ENVIRONMENT DIVISION.                                            CI0446
       CONFIGURATION SECTION.                                           CI0446
       SOURCE-COMPUTER. IBM-370.                                        CI0446
       OBJECT-COMPUTER. IBM-370.                                        CI0446
       DATA DIVISION.                                                   CI0446
       WORKING-STORAGE SECTION.                                         CI0446
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
                        PICTURE 99B99B9999.                             CI0446
      *!WS
           05  7-DE00-DNACG
                        PICTURE 99B99B9999.                             CI0446
      *!WS
           05  7-DE00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0446
      *!WS
           05  7-DE00-GEEND
                        PICTURE 99B99B9999.                             CI0446
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0446
            10            I93B-CEADC  PICTURE  X                        CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-DACTT  PICTURE  X(10)                    CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-GEOPDC PICTURE  X(8)                     CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-GEOPDB PICTURE  X(8)                     CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-CAEMCE PICTURE  X(8)                     CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-CAEMCD PICTURE  X(8)                     CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-GETIMM PICTURE  X(8)                     CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0446
                          VALUE                ZERO                     CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            I93B-GERTC  PICTURE  X                        CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-DXTMST PICTURE  X(26)                    CI0446
                          VALUE                SPACE.                   CI0446
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0446
                          VALUE                SPACE.                   CI0446
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
       01 SUTBLE-PT PIC S9(5) VALUE ZEROS.
       01  TEMP-PT PIC S9(5) VALUE ZEROS.
        01 SUBACC-INDEX  PIC  9(3)  VALUE ZERO.
      *SUB-ACCOUNT INFO FOR IARB

        01 PX01-SUBACC-TABLE.
          05  PX01-SUBACC   OCCURS 100 TIMES.
            10  WC01-MFND3 PIC X(3) VALUE SPACE.
            10  WC01-MFDNMS PIC X(30) VALUE SPACE.
      *!WI
            10  WC01-PAFDV
                        PICTURE X(4).                                   CI0446
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
                        PICTURE X(66)                                   CI0446
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
       01 WC00-CONF-MSG5 PIC X(80)
              VALUE '<LI>Allow five business days for processing.</LI>'.
       01 WC00-STEP1OT PIC X(35)
               VALUE 'Verify Order Ticket Information'.
       01 WC00-STEP2TT PIC X(35)
               VALUE 'Verify Arrangement Option'.
       01 WC00-STEP3TD PIC X(35)
               VALUE 'Verify Transaction Details'.
       01 WC00-STEP4DD PIC X(35)
               VALUE 'Verify Destination Detail'.
       01 WC00-STEP5WH PIC X(35)
               VALUE 'Verify Withholding'.
       01 WC00-STEP1OT-CONF PIC X(35)
               VALUE 'Order Ticket Information'.
       01 WC00-STEP2TT-CONF PIC X(35)
               VALUE 'Arrangement Option'.
       01 WC00-STEP3TD-CONF PIC X(35)
               VALUE 'Transaction Details'.
       01 WC00-STEP4DD-CONF PIC X(35)
               VALUE 'Destination Detail'.
       01 WC00-STEP5WH-CONF PIC X(35)
               VALUE 'Withholding'.

       01 WC01-BROARR   PIC 9(3) VALUE ZERO.
       01 WC01-DEBRO REDEFINES WC01-BROARR.
           05  WC01-DIVI PIC 9(1).
           05  WC01-PRIN PIC 9(1).
           05  WC01-MATU PIC 9(1).
       01 WC01-INDEX PIC 9(3) VALUE ZERO.
       01 WC01-IMP-MSG1-VER PIC X(35) VALUE
                  'Upon submission, your transaction '.
       01 WC01-IMP-MSG1-CON PIC X(35) VALUE
                  'Upon submission, your transaction '.
       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WD115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0446
      *!WI pl=WD120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0446
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0446
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0446
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0446
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0446
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0446
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0446
       01               WE00.                                           $CHECK
      *!WI pl=WE110                                                     $CHECK
         05             WE00-CTIDN                                      $CHECK
                        PICTURE X(24)                                   CI0446
                        JUSTIFIED RIGHT.                                $CHECK
      *!WI pl=WE130                                                     $CHECK
         05             WE00-GERTC                                      $CHECK
                        PICTURE X.                                      CI0446
      *!WI
       01 7-CTIDN VALUE SPACES
                        PICTURE X(24).                                  CI0446
       01    WL00-CTTYPG1  PIC X(100)
           VALUE 'Send check to address of record via regular mail'.
       01    WL00-CTTYPG2.
             05   WL00-CTTYPG21   PIC X(50)
                     VALUE 'Internal transfer to another '.
             05   WL00-CTTYPG22   PIC X(50)
                     VALUE 'Ameriprise Financial Services account'.
       01    WL00-CTTYPG3  PIC X(100)
           VALUE 'Direct Deposit to an existing Bank'.
       01    WL00-CTTYPG4  PIC X(100)
           VALUE 'Ameriprise Personal Savings'.

      *// FOR IDIS
       01    WL00-CACT1S  PIC X(100)
           VALUE 'Create new income distribution arrangement'.
       01    WL00-CACT2S  PIC X(100)
           VALUE 'Modify existing income distribution arrangement '.
       01    WL00-CACT3S  PIC X(100)
           VALUE 'Inactivate existing income distribution arrangement'.
      *// FOR IARB
       01    WL00-CACT1B  PIC X(100)
           VALUE 'Establish new or modify existing arrangement'.
       01    WL00-CACT2B  PIC X(100)
           VALUE ' Request one time rebalance'.
       01    WL00-CACT3B  PIC X(100)
           VALUE 'Discontinue existing arrangement'.
       01    WL00-SUBD1  PIC X(30)
           VALUE '<br>Subaccount Details:'.
       01    WL00-CTRN1  PIC X(100)
                     VALUE 'Cash From Sweep Account'.
       01    WL00-CTRN2  PIC X(100)
                     VALUE 'Value Available From Margin Equity'.
       01    WL00-CTRN3  PIC X(100)
                    VALUE 'Proceeds From Securities Sale'.
       01    WL00-DCSO-AMOUNT.
             05 WL00-AMTAG  PIC X(12) VALUE 'Amount: <b>'.
             05 WL00-ACOTD2 PIC X(38) VALUE SPACES.
       01  WL00-DCSO-AMOUNT1.
             05 WL00-AMTAG  PIC X(50)
               VALUE '<B>Available cash/margin and/or trade proceeds'.
      *// FOR BETA ANNUITY REBALANCE
       01    WL00-CACT1C  PIC X(100)
           VALUE 'Add Scheduled Annuity Rebalance Arrangement'.
       01    WL00-CACT2C  PIC X(100)
            VALUE 'Modify Scheduled Annuity Rebalance Arrangement'.
       01    WL00-CACT3C  PIC X(100)
            VALUE 'Inactivate Scheduled Annuity Rebalance Arrangement'.
       01    WL00-SUBD2  PIC X(30)
           VALUE '<br>Annuity Rebalance Details:'.
       01    WL00-SUBNAME   PIC X(50)
           VALUE 'Sub-account Name'.
       01    WL00-SUBPCT   PIC X(50)
           VALUE 'Percentage'.
      *MISCELLANEOUS FIELDS
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0446
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
                        PICTURE 9999B9999B9999B9999.                    CI0446
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

           05 WS01-CTID.
      *!WS
              10 WS01-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0446
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
                        PICTURE X(254).                                 CI0446

      *!WS
          05 WS00-APMTD
                        PICTURE $$,$$$,$$$,$$9.99.                      CI0446

          05 WS00-COUNT    PIC 9(8)    VALUE ZEROS.

      *!WS
          05 WS00-MPMTF                VALUE SPACES
                        PICTURE X(14).                                  CI0446

          05 WS00-PWHLD    PIC ZZ9.99- VALUE ZEROES.

      *!WS
          05 WS00-PACT1
                        PICTURE ZZ9.999-.                               CI0446

      *!WS
          05 WS00-QSHOWQ
                        PICTURE ZZZ,ZZZ,ZZ9.999-                        CI0446
                          BLANK WHEN ZERO.                              CI0446

      *!WS
          05 WS00-TARST
                        PICTURE X(10).                                  CI0446
      ******************************************************************
      *VARIABLE 'TIMER' IS USED TO HOLD THE TRANSACTION RECEIVED TIME.
      ******************************************************************
       01  WS00-TIMER    PIC X(06) VALUE SPACES.
       01  WS00-TIME-ENTERED REDEFINES WS00-TIMER.
              05 WS-TIME-HH   PIC 99.
              05 WS-TIME-MM   PIC 99.
              05 WS-TIME-SS   PIC 99.
       01  WS00-AMPM     PIC X(03) VALUE SPACES.
      *
       01  WS00-CST      PIC X(20) VALUE SPACES.
      *
       01  WS00-CST01    PIC X(05) VALUE SPACES.
      ***************************************************************
      *VARIABLES FOR STRING CANCAT TO CREATE SUB-ACCT FND NAME TABLE.
      ***************************************************************
      *!WI
       01 WS00-MFDNME
                        PICTURE X(40).                                  CI0446
      *01 WS00-APMTD.
       01 WS00-SUBACCT-FNDNAME  PIC X(80).
      *!WE
       01 WS00-GEEND VALUE ZEROES
                        PICTURE 9(8).                                   CI0446
       01 WS00-GEENDX REDEFINES WS00-GEEND PIC X(8).
      *!WS
       01 WS00-VOLOI
                        PICTURE ZZZ,ZZZ,ZZ9.99-.                        CI0446
       01 WS00-DATCE  PIC X(8) VALUE ZEROES.
      **----------------------------------------------------------------ANIOBJ
      **  DATE WORK AREA USED BY MACRO AADA58                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
      *!WF DSP=WT DSL=DD SEL=01 FOR=I LEV=1                             ANIOBJ
       01                 WT00.                                         CI0446
          05              WT00-SUITE.                                   CI0446
            15       FILLER         PICTURE  X(00093).                  CI0446
       01                 WT01  REDEFINES      WT00.                    CI0446
            10            WT01-XDAT8.                                   CI0446
            11            WT01-XDATC  PICTURE  XX.                      CI0446
            11            WT01-XDATY  PICTURE  XX.                      CI0446
            11            WT01-XDATM  PICTURE  XX.                      CI0446
            11            WT01-XDATD  PICTURE  XX.                      CI0446
            10            WT01-XDAT8D                                   CI0446
                          REDEFINES            WT01-XDAT8               CI0446
               PICTURE    9(8).                                         CI0446
            10            WT01-XDAT81.                                  CI0446
            11            WT01-XDATM1 PICTURE  XX.                      CI0446
            11            WT01-XDATD1 PICTURE  XX.                      CI0446
            11            WT01-XDATC1 PICTURE  XX.                      CI0446
            11            WT01-XDATY1 PICTURE  XX.                      CI0446
            10            WT01-XDAT80                                   CI0446
                          REDEFINES            WT01-XDAT81              CI0446
               PICTURE    9(8).                                         CI0446
            10            WT01-XDAT62.                                  CI0446
            11            WT01-XDATM2 PICTURE  XX.                      CI0446
            11            WT01-XDATD2 PICTURE  XX.                      CI0446
            11            WT01-XDATY2 PICTURE  XX.                      CI0446
            10            WT01-XDAT69                                   CI0446
                          REDEFINES            WT01-XDAT62              CI0446
               PICTURE    9(6).                                         CI0446
            10            WT01-XDATCU.                                  CI0446
            11            WT01-XDATC9 PICTURE  99.                      CI0446
            11            WT01-XDAYMD.                                  CI0446
            12            WT01-XDATY9 PICTURE  99.                      CI0446
            12            WT01-XDAMD.                                   CI0446
            13            WT01-XDATM9 PICTURE  99.                      CI0446
            13            WT01-XDATD9 PICTURE  99.                      CI0446
            10            WT01-XDAT89 PICTURE  9(8).                    CI0446
            10            WT01-XDAJC  PICTURE  9(7).                    CI0446
            10            WT01-XDAJC1.                                  CI0446
            11            WT01-XDAJC9 PICTURE  99.                      CI0446
            11            WT01-XDAJY  PICTURE  99.                      CI0446
            11            WT01-XDAJN  PICTURE  999.                     CI0446
            10            WT01-XDAB   PICTURE  9(5).                    CI0446
            10            WT01-DD05.                                    CI0446
            11            WT01-XDACT  PICTURE  S9(3)                    CI0446
                          COMPUTATIONAL-3.                              CI0446
            11            WT01-XDACV  PICTURE  S9                       CI0446
                          COMPUTATIONAL-3.                              CI0446
            11            WT01-XDAGP  PICTURE  S9(9)                    CI0446
                          COMPUTATIONAL-3.                              CI0446
            11            WT01-XDAJP  PICTURE  S9(7)                    CI0446
                          COMPUTATIONAL-3.                              CI0446
            11            WT01-XDACV1 PICTURE  S9                       CI0446
                          COMPUTATIONAL-3.                              CI0446
            11            WT01-XDAGP1 PICTURE  S9(9)                    CI0446
                          COMPUTATIONAL-3.                              CI0446
            11            WT01-XDAJP1 PICTURE  S9(7)                    CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            WT01-XW03.                                    CI0446
            11            WT01-XDATG.                                   CI0446
            12            WT01-XDAT1.                                   CI0446
            13            WT01-XDAT19 PICTURE  99.                      CI0446
            12            WT01-XDAT2.                                   CI0446
            13            WT01-XDAT29 PICTURE  99.                      CI0446
            12            WT01-XDAT3.                                   CI0446
            13            WT01-XDAT39 PICTURE  99.                      CI0446
            12            WT01-XDAT4.                                   CI0446
            13            WT01-XDAT49 PICTURE  99.                      CI0446
            11            WT01-XLEAPY PICTURE  99.                      CI0446
            11            WT01-DTGCY  PICTURE  9(4).                    CI0446
            11            WT01-FILLER                                   CI0446
                          REDEFINES            WT01-DTGCY.              CI0446
            12            WT01-DTGCC  PICTURE  9(2).                    CI0446
            12            WT01-DTGYY  PICTURE  9(2).                    CI0446
                                                                        ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      **  WORKING STORAGE FOR CASE DOC DATE                             ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      *!WI pl=WT115                                                     ANIOBJ
       01 7-CSTDTE                                                      ANIOBJ
                        PICTURE X(8).                                   CI0446
       01 FILLER REDEFINES 7-CSTDTE.                                    ANIOBJ
      *!WI pl=WT125                                                     ANIOBJ
          05 7-DTGCY                                                    ANIOBJ
                        PICTURE 9(4).                                   CI0446
      *!WI pl=WT130                                                     ANIOBJ
          05 7-DTGMM                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0446
      *!WI pl=WT135                                                     ANIOBJ
          05 7-DTGDD                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0446
      *                                                                 ANIOBJ
      *!WI pl=WT145                                                     ANIOBJ
       01 7-CASE-DTJUL                                                  ANIOBJ
                        PICTURE 9(7).                                   CI0446
       01 FILLER REDEFINES 7-CASE-DTJUL.                                ANIOBJ
      *!WI pl=WT150                                                     ANIOBJ
          05 7-CASE-DTGCY                                               ANIOBJ
                        PICTURE 9(4).                                   CI0446
      *!WI pl=WT155                                                     ANIOBJ
          05 7-CASE-DTJDD                                               ANIOBJ
                        PICTURE 9(3).                                   CI0446
      *                                                                 ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      ** CASEDOC-ID GENERATED                                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
       01 WT01-NIOBJC.                                                  ANIOBJ
         05 WT01-DTTCY    PIC 9.                                        ANIOBJ
      *!WI pl=WT270                                                     ANIOBJ
         05 WT01-DTJDD                                                  ANIOBJ
                        PICTURE 9(3).                                   CI0446
      *!WI pl=WT280                                                     ANIOBJ
         05 WT01-DTTHH                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0446
      *!WI pl=WT290                                                     ANIOBJ
         05 WT01-DTTMN                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0446
      *!WI pl=WT300                                                     ANIOBJ
         05 WT01-DTTSS                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0446
         05 WT01-DTTNN    PIC 9(3).                                     ANIOBJ
         05 FILLER        PIC X(4)    VALUE '.001'.                     ANIOBJ
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58
       01   DEBUT-WSS.                                                  CI0446
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0446
            05   IK     PICTURE X.                                      CI0446
       01  CONSTANTES-PAC.                                              CI0446
           05  FILLER  PICTURE X(87)   VALUE                            CI0446
                     '6015 CAT09/08/14CI0446ADMIN   14:35:28CI0446P AMERCI0446
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0446
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0446
           05  NUGNA   PICTURE X(5).                                    CI0446
           05  APPLI   PICTURE X(3).                                    CI0446
           05  DATGN   PICTURE X(8).                                    CI0446
           05  PROGR   PICTURE X(6).                                    CI0446
           05  CODUTI  PICTURE X(8).                                    CI0446
           05  TIMGN   PICTURE X(8).                                    CI0446
           05  PROGE   PICTURE X(8).                                    CI0446
           05  COBASE  PICTURE X(4).                                    CI0446
           05  DATGNC  PICTURE X(10).                                   CI0446
           05  RELEAS  PICTURE X(7).                                    CI0446
           05  DATGE   PICTURE X(10).                                   CI0446
           05  DATSQ   PICTURE X(10).                                   CI0446
       01  DATCE.                                                       CI0446
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0446
         05  DATOR.                                                     CI0446
           10  DATOA  PICTURE XX.                                       CI0446
           10  DATOM  PICTURE XX.                                       CI0446
           10  DATOJ  PICTURE XX.                                       CI0446
       01  DAT6.                                                        CI0446
            10 DAT61.                                                   CI0446
            15 DAT619  PICTURE 99.                                      CI0446
            10 DAT62.                                                   CI0446
            15 DAT629  PICTURE 99.                                      CI0446
            10 DAT63   PICTURE XX.                                      CI0446
       01  DAT8.                                                        CI0446
            10 DAT81   PICTURE XX.                                      CI0446
            10 DAT8S1  PICTURE X.                                       CI0446
            10 DAT82   PICTURE XX.                                      CI0446
            10 DAT8S2  PICTURE X.                                       CI0446
            10 DAT83   PICTURE XX.                                      CI0446
       01  DAT8E    REDEFINES    DAT8.                                  CI0446
            10 DAT81E  PICTURE X(4).                                    CI0446
            10 DAT82E  PICTURE XX.                                      CI0446
            10 DAT83E  PICTURE XX.                                      CI0446
       01  DAT6C.                                                       CI0446
            10  DAT61C PICTURE XX.                                      CI0446
            10  DAT62C PICTURE XX.                                      CI0446
            10  DAT63C.                                                 CI0446
             15 DAT63CC PICTURE XX.                                     CI0446
             15 DAT64C  PICTURE XX.                                     CI0446
       01  DAT8C.                                                       CI0446
            10  DAT81C  PICTURE XX.                                     CI0446
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0446
            10  DAT82C  PICTURE XX.                                     CI0446
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0446
            10  DAT83C.                                                 CI0446
             15 DAT83CC PICTURE XX.                                     CI0446
             15 DAT84C  PICTURE XX.                                     CI0446
       01  DATSEP     PICTURE X VALUE '/'.                              CI0446
       01  DATSEW     PICTURE X.                                        CI0446
       01   VARIABLES-CONDITIONNELLES.                                  CI0446
            05                  FT      PICTURE X VALUE '0'.            CI0446
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0446
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0446
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J45DHR PICTURE S9(4) VALUE  ZERO.
            05           J45DLR PICTURE S9(4) VALUE  ZERO.
            05           J70BER PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0446
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT.
      *THIS COPYBOOK CONTAINS THE HEADER, SOURCE ACCOUNT DETAILS AND
      *DESTINATION ACCOUNT DETAILS FOR THE HTML PAGE.
      ******************************************************************
       COPY CI0446C1.
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT
      *THIS COPYBOOOK CONTAINS THE PAYMENT DETAILS AND THE COMPLIANCE
      *QUESTIONS FOR THE HTML PAGE.
      ******************************************************************
       COPY CI0446C2.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0329                *
      ******************************************************************
      *!WF DSP=V2 DSL=V2 SEL=90 FOR=I LEV=1 PLT=80
       01                 V200.                                         CI0446
          05              V200-SUITE.                                   CI0446
            15       FILLER         PICTURE  X(57227).                  CI0446
       01                 V290  REDEFINES      V200.                    CI0446
            10            V290-CTID.                                    CI0446
            11            V290-CTIDA  PICTURE  9(3).                    CI0446
            11            V290-CTIDN.                                   CI0446
            12            V290-CTIDNP PICTURE  X(13).                   CI0446
            12            V290-CTIDND PICTURE  9(11).                   CI0446
            10            V290-GECKD1 PICTURE  9.                       CI0446
            10            V290-CALTN  PICTURE  X(4).                    CI0446
            10            V290-CGMBR  PICTURE  X.                       CI0446
            10            V290-PWHLD  PICTURE  S999V9(5)                CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-TWITH  PICTURE  X(12).                   CI0446
            10            V290-QITEM  PICTURE  9(3).                    CI0446
            10            V290-QBLCK  PICTURE  9(6).                    CI0446
            10            V290-DCACG  PICTURE  9(8).                    CI0446
            10            V290-DNPMT  PICTURE  9(8).                    CI0446
            10            V290-GEEND  PICTURE  9(8).                    CI0446
            10            V290-DXTMS2 PICTURE  X(26).                   CI0446
            10            V290-MPLNR2 PICTURE  X(40).                   CI0446
            10            V290-CCONF  PICTURE  X(25).                   CI0446
            10            V290-ATROLL PICTURE  X(25).                   CI0446
            10            V290-GEOPD2 PICTURE  X(8).                    CI0446
            10            V290-CSPRDN PICTURE  X(30).                   CI0446
            10            V290-CTTLN1 PICTURE  X(30).                   CI0446
            10            V290-CTTLN2 PICTURE  X(30).                   CI0446
            10            V290-CTTLN3 PICTURE  X(30).                   CI0446
            10            V290-CTTBO1 PICTURE  X(45).                   CI0446
            10            V290-CTTBO2 PICTURE  X(45).                   CI0446
            10            V290-CTID01.                                  CI0446
            11            V290-CACTID PICTURE  9(3).                    CI0446
            11            V290-CTIDNB.                                  CI0446
            12            V290-CTIDP1 PICTURE  X(13).                   CI0446
            12            V290-CTIDNA PICTURE  9(11).                   CI0446
            10            V290-GECKD2 PICTURE  9.                       CI0446
            10            V290-PRCMN1 PICTURE  X(20).                   CI0446
            10            V290-MPMTT  PICTURE  X(20).                   CI0446
            10            V290-MPMTFO PICTURE  X(17).                   CI0446
            10            V290-IAIND  PICTURE  X.                       CI0446
            10            V290-CCLPR  PICTURE  X.                       CI0446
            10            V290-CCLCH  PICTURE  X.                       CI0446
            10            V290-CCLSU  PICTURE  X.                       CI0446
            10            V290-CDETY  PICTURE  XX.                      CI0446
            10            V290-CDEST  PICTURE  99.                      CI0446
            10            V290-CACTS  PICTURE  X.                       CI0446
            10            V290-CACT   PICTURE  X(1).                    CI0446
            10            V290-CATLN1 PICTURE  X(30).                   CI0446
            10            V290-CATLN2 PICTURE  X(30).                   CI0446
            10            V290-CATLN3 PICTURE  X(30).                   CI0446
            10            V290-CATBO1 PICTURE  X(45).                   CI0446
            10            V290-CATBO2 PICTURE  X(45).                   CI0446
            10            V290-GESAD1 PICTURE  X(30).                   CI0446
            10            V290-GESAD2 PICTURE  X(30).                   CI0446
            10            V290-GESAD3 PICTURE  X(30).                   CI0446
            10            V290-GECIT  PICTURE  X(25).                   CI0446
            10            V290-GEST   PICTURE  X(8).                    CI0446
            10            V290-GEPCD  PICTURE  X(12).                   CI0446
            10            V290-CLORN  PICTURE  X(45).                   CI0446
            10            V290-NTR    PICTURE  9(8).                    CI0446
            10            V290-GECKD3 PICTURE  9.                       CI0446
            10            V290-NPBN   PICTURE  X(20).                   CI0446
            10            V290-TTBAL  PICTURE  X(15).                   CI0446
            10            V290-MCSIG  PICTURE  X(30).                   CI0446
            10            V290-CPROCM PICTURE  X.                       CI0446
            10            V290-CSLCT  PICTURE  X.                       CI0446
            10            V290-CTRHO  PICTURE  9(8).                    CI0446
            10            V290-GETOD  PICTURE  9(6).                    CI0446
            10            V290-TMESSC PICTURE  X(254)                   CI0446
                          OCCURS       020     TIMES.                   CI0446
            10            V290-QT9D.                                    CI0446
            11            V290-QT9B                                     CI0446
                          OCCURS       233     TIMES.                   CI0446
            12            V290-CHTML  PICTURE  99.                      CI0446
            12            V290-THTML  PICTURE  X(200).                  CI0446
            10            V290-CQACTL PICTURE  X(45).                   CI0446
            10            V290-DAEDTO PICTURE  X(8).                    CI0446
            10            V290-NGEOPA PICTURE  X(08).                   CI0446
            10            V290-CTTYPG PICTURE  X(04).                   CI0446
            10            V290-IRMND  PICTURE  X.                       CI0446
            10            V290-ACOTM  PICTURE  S9(9)V99                 CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-ACOTD  PICTURE  S9(9)V99                 CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-APMTD  PICTURE  S9(11)V99                CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-AMNBR  PICTURE  S9(5)V99                 CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-ADBCRQ PICTURE  S9(11)V99                CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-DNPMT1 PICTURE  9(8).                    CI0446
            10            V290-CPORTA PICTURE  X.                       CI0446
            10            V290-DNACG  PICTURE  9(8).                    CI0446
            10            V290-DACTG  PICTURE  9(8).                    CI0446
            10            V290-CTKSY1 PICTURE  X(06).                   CI0446
            10            V290-CTRTC  PICTURE  X(03).                   CI0446
            10            V290-DPTIM  PICTURE  X(8).                    CI0446
            10            V290-BREPIT PICTURE  X(5).                    CI0446
            10            V290-BROARR PICTURE  9(3).                    CI0446
            10            V290-CEFRY  PICTURE  X.                       CI0446
            10            V290-CTKSY2 PICTURE  X(06).                   CI0446
            10            V290-DIVARR PICTURE  X(2).                    CI0446
            10            V290-DIVIDE PICTURE  X(10).                   CI0446
            10            V290-DNPMTM PICTURE  9(8).                    CI0446
            10            V290-DNPMTX PICTURE  9(8).                    CI0446
            10            V290-GEENDM PICTURE  9(8).                    CI0446
            10            V290-MPMTFM PICTURE  X(17).                   CI0446
            10            V290-UPNMAL PICTURE  X.                       CI0446
            10            V290-VOLOI  PICTURE  S9(09)V99                CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-VOLOIM PICTURE  S9(09)V99                CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-VONSV  PICTURE  X(12).                   CI0446
            10            V290-VOROA  PICTURE  S9(9)V99                 CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-VOROAM PICTURE  S9(9)V99                 CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            V290-TFNDSD PICTURE  X(50).                   CI0446
            10            V290-TFNDSS PICTURE  X(50).                   CI0446
            10            V290-NROWR  PICTURE  9(3).                    CI0446
            10            V290-INDCK  PICTURE  X.                       CI0446
            10            V290-FILLER PICTURE  X(30).                   CI0446
            10            V290-GEENDX PICTURE  9(8).                    CI0446
            10            V290-ST9B.                                    CI0446
            11            V290-ST90                                     CI0446
                          OCCURS       100     TIMES.                   CI0446
            12            V290-MFND3  PICTURE  XXX.                     CI0446
            12            V290-MFDNMS PICTURE  X(30).                   CI0446
            12            V290-PAFDV  PICTURE  X(4).                    CI0446
            10            V290-DCACD  PICTURE  X(10).                   CI0446
            10            V290-DNPMMX PICTURE  9(8).                    CI0446
            10            V290-GEENMX PICTURE  9(8).                    CI0446
            10            V290-NGEOR  PICTURE  9(08).                   CI0446
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0446
          05              MS00-SUITE.                                   CI0446
            15       FILLER         PICTURE  X(00542).                  CI0446
       01                 MS03  REDEFINES      MS00.                    CI0446
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0446
                          COMPUTATIONAL-3.                              CI0446
            10            MS03-CMSSF  PICTURE  XX.                      CI0446
            10            MS03-DU09.                                    CI0446
            11            MS03-CMESA  PICTURE  S9(9)                    CI0446
                          BINARY.                                       CI0446
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0446
                          BINARY.                                       CI0446
            11            MS03-CMESB  PICTURE  S9(9)                    CI0446
                          BINARY.                                       CI0446
            11            MS03-CMSST  PICTURE  S9(9)                    CI0446
                          BINARY.                                       CI0446
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0446
                          BINARY.                                       CI0446
            11            MS03-QELLAA PICTURE  S9(9)                    CI0446
                          BINARY.                                       CI0446
            11            MS03-TMESS4 PICTURE  X(512).                  CI0446
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0446
            10            MX11-QMSGS  PICTURE  9(03).                   CI0446
            10            MX11-PJ09                                     CI0446
                          OCCURS       025     TIMES.                   CI0446
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0446
                          COMPUTATIONAL-3.                              CI0446
            11            MX11-CMESB  PICTURE  S9(9)                    CI0446
                          BINARY.                                       CI0446
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V290
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0I.      NOTE *************************************.            ACMCTI
      *               *                                   *             ACMCTI
      *               *********************************   *             ACMCTI
      *               *                                   *             ACMCTI
      *               *************************************.            ACMCTI
       F0I.                                                             lv05
      ** SUB-FUNCTION TO PERFORM A    *                                 ACMCTI
      ** DUMMY DB2 CALL.              *                                 ACMCTI
      *********************************                                 ACMCTI
           EXEC SQL    SET                                              ACMCTI
                        :WS00-DATE = CURRENT_DATE            END-EXEC.  ACMCTI
           PERFORM     F93SQ THRU F93SQ-FN.                             ACMCTI
       F0I-FN.   EXIT.
      *N01.      NOTE *************************************.            CI0446
      *               *                                   *             CI0446
      *               *INITIALISATIONS                    *             CI0446
      *               *                                   *             CI0446
      *               *************************************.            CI0446
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0446
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0446
      *               *                                   *             CI0446
      *               *FIN DE TRAITEMENT                  *             CI0446
      *               *                                   *             CI0446
      *               *************************************.            CI0446
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0446
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
           MOVE        1 TO HTML-PT
           STRING      HTML-COMMON-HDR-TAGS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35AT-FN. EXIT.
      *N35BB.    NOTE *POPULATE HTML PASS AREA            *.
       F35BB.                                                           lv10
           MOVE        V290-CTIDND TO WS00-NCTIDE
           MOVE        V290-GECKD1 TO WS00-GECKD1
           MOVE        V290-CTIDA TO WS00-CTIDA
           MOVE        V290-CTTLN1 TO HTML-CTTLN1
           MOVE        V290-GEENDX TO WS00-GEENDX
           MOVE        V290-CSPRDN TO HTML-PRCMN.
                 IF    V290-CSLCT = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CSLCT
           MOVE        V290-NGEOR TO HTML-NGEOR.
                 IF    V290-CSLCT = 'N'                                 DOT
           MOVE        'NO ' TO HTML-CSLCT
           MOVE        'N/A' TO HTML-NGEOR.
           MOVE        V290-CTTLN1 TO HTML-CTTLN1                       DOT
           MOVE        V290-CTTLN2 TO HTML-CTTLN2
           MOVE        V290-CTTLN3 TO HTML-CTTLN3
           MOVE        V290-ATROLL TO HTML-ATROLL
           MOVE        V290-CQACTL TO HTML-CQACTL
           MOVE        V290-CTTBO1 TO HTML-CTTBO1
           MOVE        V290-CTTBO2 TO HTML-CTTBO2
           MOVE        V290-CTTBO2 TO HTML-CTTBO2
           MOVE        V290-GEOPD2 TO HTML-GEOPD2
           MOVE        V290-NGEOPA TO HTML-NGEOPA.
                 IF    V290-CACT = 'N'                                  DOT
                 AND   V290-CALTN = 'IDIS'
           MOVE        WL00-CACT1S TO HTML-CACT.
                 IF    V290-CACT = 'M'                                  DOT
                 AND   V290-CALTN = 'IDIS'
           MOVE        WL00-CACT2S TO HTML-CACT.
                 IF    V290-CACT = 'I'                                  DOT
                 AND   V290-CALTN = 'IDIS'
           MOVE        WL00-CACT3S TO HTML-CACT.
                 IF    V290-CACT = 'N'                                  DOT
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        WL00-CACT1B TO HTML-CACT3.
                 IF    V290-CACT = 'O'                                  DOT
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        WL00-CACT2B TO HTML-CACT3.
                 IF    V290-CACT = 'I'                                  DOT
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        WL00-CACT3B TO HTML-CACT3.
           MOVE        V290-CEFRY TO HTML-CEFRY                         DOT
           MOVE        V290-DIVARR TO HTML-DIVARR
           MOVE        V290-MPMTT TO HTML-MPMTT
           HTML-MPMTTA
           MOVE        V290-CTID01 TO HTML-CTID01
           HTML-CTID01A
           MOVE        V290-CLORN TO HTML-CLORN
           MOVE        V290-NPBN TO HTML-NPBN
           MOVE        V290-MPLNR2 TO HTML-MPLNR2
           MOVE        V290-CCONF TO HTML-CCONF
           MOVE        V290-ST9B TO PX01-SUBACC-TABLE
           MOVE        V290-ACOTD TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ACOTD
           MOVE        V290-MPMTFO TO HTML-MPMTF
           HTML-MPMTFCC.
                 IF    V290-CACT = 'M'                                  DOT
                 AND   V290-CALTN = 'AAPS'
           MOVE        'Active' TO HTML-TARSTX.
                 IF    V290-CACT = 'I'                                  DOT
                 AND   V290-CALTN = 'AAPS'
           MOVE        'Inactive' TO HTML-TARSTX.
                 IF    V290-CACT = 'N'                                  DOT
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = 134
           MOVE        WL00-CACT1C TO HTML-CACT3.
                 IF    V290-CACT = 'M'                                  DOT
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = 134
           MOVE        WL00-CACT2C TO HTML-CACT3.
                 IF    V290-CACT = 'I'                                  DOT
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = 134
           MOVE        WL00-CACT3C TO HTML-CACT3.
       F35BB-FN. EXIT.
      *N35BC.    NOTE *FORMAT CONTRACT ID                 *.
       F35BC.                                                           lv10
      *
           MOVE        V290-CTIDND TO WS00-NCTIDE
           MOVE        V290-GECKD1 TO WS00-GECKD1
           MOVE        V290-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-CTID.
                 IF    V290-CTTYPG = 'MMTA'                             DOT
      *
           MOVE        V290-CTID01 (12:16) TO WS00-NCTIDE
           MOVE        V290-CTID01 (1:3) TO WS00-CTIDA
           MOVE        V290-CTID01 (4:24) TO 7-CTIDN
           PERFORM     F95CD THRU F95CD-FN
           MOVE        WE00-GERTC TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID01
           HTML-CTID01A.
      *N35BD.    NOTE *FORMAT THE CURRENT DATE,           *.
       F35BD.                                                           lv15
      *EFFECTIVE DATE AND SEND TRANS
      *RECEIVED TIME
      *
           STRING      '000'
           V290-DCACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT
           INITIALIZE  I93B
           MOVE        V290-DCACG TO 7-DE00-DCACG
           MOVE        V290-DCACD TO I93B-DACTT
           MOVE        'O' TO I93B-CEADC
           PERFORM     F96BB THRU F96BB-FN.
                 IF    I93B-GERTC NOT = 'Y'                             DOT
      *CHECK RETURN CODES
      *IF INVALID CODE, SEND ERROR MSG
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN                              ADU119
      *MACRO RUN SUCCEED
                 ELSE
           MOVE        I93B-GETIMM TO WS-GETIMM
           MOVE FUNCTION CURRENT-DATE(1:8)
                        TO  WS00-DATCE
           MOVE FUNCTION CURRENT-DATE(9:6)
                        TO  WS00-TIMER
           MOVE        WS-TIME1-HH TO WS-TIME2-HH
           MOVE        WS-TIME1-MM TO WS-TIME2-MM
           MOVE        WS-TIME1-SS TO WS-TIME2-SS
      *!ADS "WS00-DATCE     7-DE00-DCACG"
           MOVE        WS00-DATCE                                       CI0446
           TO DAT8E DAT6C                                               CI0446
           MOVE DAT81E TO DAT63C                                        CI0446
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0446
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0446
      *!ADM "7-DE00-DCACG     HTML-DCACG"
           MOVE        7-DE00-DCACG                                     CI0446
           TO DAT8E DAT6C                                               CI0446
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0446
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0446
           MOVE   DAT8C TO  HTML-DCACG.                                 CI0446
                 IF    (WS00-TIMER >= WS-GETIMT                         DOT
                 AND   V290-DCACG <= WS00-DATCE)
      *NEXT ACTG DATE IF AFTER 3PM
           STRING      '000'
           V290-DNACG (7:2)
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
           STRING      V290-DNPMTX (1:2) '/'
           V290-DNPMTX (3:2) '/'
           V290-DNPMTX (5:4) ' '
           DELIMITED BY SIZE INTO
           HTML-DNPMTX
           MOVE        HTML-DNPMTX TO HTML-DNPMTXC
           STRING      WS00-GEENDX (1:2) '/'
           WS00-GEENDX (3:2) '/'
           WS00-GEENDX (5:4) ' '
           DELIMITED BY SIZE INTO
           HTML-GEENDX
           MOVE        HTML-GEENDX TO HTML-GEENDXC.
       F35BD-FN. EXIT.
      *N35BE.    NOTE *FORMAT THE ORDER RECEIVED DATE     *.
       F35BE.                                                           lv15
      *EFFECTIVE DATE AND SEND THE
      *ORDER RECEIVED TIME
      *
      *!ADM "V290-DAEDTO     HTML-DRECD"
           MOVE        V290-DAEDTO                                      CI0446
           TO DAT8E DAT6C                                               CI0446
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0446
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0446
           MOVE   DAT8C TO  HTML-DRECD                                  CI0446
      *
           STRING      V290-DPTIM DELIMITED BY SIZE
           ' Central Time'
           DELIMITED BY SIZE
           INTO HTML-TIMER1.
       F35BE-FN. EXIT.
       F35BC-FN. EXIT.
      *N35BF.    NOTE *FORMATING THE OUTPUT FILEDS        *.
       F35BF.                                                           lv10
      *
                 IF    V290-CTTYPG = 'SMTC'                             DOT
           MOVE        WL00-CTTYPG1 TO HTML-CTTYPG
           HTML-CTTYPGA.
                 IF    V290-CTTYPG = 'MMTA'                             DOT
           MOVE        WL00-CTTYPG2 TO HTML-CTTYPG
           HTML-CTTYPGA.
                 IF    V290-CTTYPG = 'SMDD'                             DOT
           MOVE        WL00-CTTYPG3 TO HTML-CTTYPG
           HTML-CTTYPGA.
                 IF    V290-CTTYPG = 'APS'                              DOT
           MOVE        WL00-CTTYPG4 TO HTML-CTTYPG.
       F35BF-FN. EXIT.
      *N35BH.    NOTE *IMPORTANT MESSAGES STEP NUM        *.
       F35BH.                                                           lv10
      *
                 IF    V290-DIVARR NOT = 'RD'                           DOT
                 OR    V290-CTIDA NOT = 002
           MOVE        '5' TO HTML-STEP.
                 IF    V290-CACT = 'I'                                  DOT
           MOVE        '2' TO HTML-STEP.
                 IF    V290-DIVARR = 'RD'                               DOT
                 AND   V290-CTIDA = 002
           MOVE        '3' TO HTML-STEP.
                 IF    V290-CALTN = 'IARB'                              DOT
                 AND   V290-CACT = 'I'
           MOVE        '2' TO HTML-STEP
           HTML-STEPR.
                 IF    (V290-CACT = 'O'                                 DOT
                 OR    V290-CACT = 'N')
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        '3' TO HTML-STEP.
           MOVE        HTML-STEP TO HTML-STEPTOP.                       DOT
                 IF    V290-CALTN = 'AAPS'                              DOT
           MOVE        '3' TO HTML-STEPM
           MOVE        HTML-STEPM TO HTML-STEPTOP.
                 IF    (V290-CACT = 'N'                                 DOT
                 OR    V290-CACT = 'M')
                 AND   V290-CALTN = 'IARB'
                 AND   V290-CTIDA = 134
           MOVE        '4' TO HTML-STEPR.
                 IF    V290-CALTN = 'IARB'                              DOT
                 AND   V290-CTIDA = 134
           MOVE        HTML-CONFIRM-MSG2 TO
           HTML-CONFIRM-MSG
                 ELSE
           MOVE        HTML-CONFIRM-MSG1 TO
           HTML-CONFIRM-MSG.
       F35BH-FN. EXIT.
      *N35BM.    NOTE *STRING CONFIRMATION INFORMATIO     *.
       F35BM.    IF    V290-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35BM-FN.
           STRING      HTML-CONFIRM-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
      *STRING CONFIRMATION PART2
           STRING      HTML-CONFIRM-INFO-EFF
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BM-FN. EXIT.
      *N35CB.    NOTE *MOVE VERIFY INFORMATION            *.
       F35CB.    IF    V290-CACTS = 'V'                                 lv10
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
       F35DP.    IF    V290-CTTLN2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DP-FN.
      *
           STRING      V290-CTTLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN2
           STRING      HTML-FROM-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DP-FN. EXIT.
      *N35DR.    NOTE *BUILD OWNERSHIP LINE3              *.
       F35DR.    IF    V290-CTTLN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DR-FN.
      *
           STRING      V290-CTTLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN3
           STRING      HTML-FROM-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DR-FN. EXIT.
      *N35DU.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F35DU.    IF    V290-CTTBO1 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DU-FN.
      *
           STRING      V290-CTTBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
           STRING      HTML-FROM-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DU-FN. EXIT.
      *N35DW.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F35DW.    IF    V290-CTTBO2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DW-FN.
      *
           STRING      V290-CTTBO2
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
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT WITHHOLDING PERCENT         *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BN.    NOTE *FORMAT WITHHOLDING PERCENT         *.
       F40BN.                                                           lv10
                 IF    V290-TWITH = SPACES                              DOT
           MOVE        V290-PWHLD TO WS00-PWHLD
           STRING      WS00-PWHLD '%'
           DELIMITED BY SIZE
           INTO HTML-PWHLDX
           MOVE        HTML-PWHLDX TO HTML-PWHLDA
      *HTML-CTWHPB        HTML-PWHLDX
                 ELSE
           MOVE        'N/A' TO HTML-PWHLDX
           HTML-PWHLDA.
       F40BN-FN. EXIT.
       F40-FN.   EXIT.
      *N45BA.    NOTE *POPULATING VALUES                  *.
       F45BA.                                                           lv10
           MOVE        V290-CTTLN1 TO HTML-CTTLN1.
       F45BA-FN. EXIT.
      *N45CB.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45CB.    IF    V290-CALTN = 'IDIS'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45CB-FN.
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        'Verify Transaction Type' TO
           HTML-STEP1TT.
                 IF    V290-CACTS = 'S'                                 DOT
           MOVE        'Transaction Type' TO HTML-STEP1TT.
           STRING      HTML-STEP1-TRAN-TYPE                             DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45CC.    NOTE *//FOR NEW AND MODIFY, HAS TRAN D   *.
       F45CC.    IF    V290-CALTN = 'IDIS'                              lv15
                 AND   V290-CACT NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F45CC-FN.
      *//  FOR INACTIVATE, ONLY TRAN TY
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        'Verify Arrangement Details' TO
           HTML-STEP2TT2.
                 IF    V290-CACTS = 'S'                                 DOT
           MOVE        'Arrangement Details' TO
           HTML-STEP2TT2.
           STRING      HTML-STEP2-TITLE                                 DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45CD.    NOTE *FOR CERTIFICATE                    *.
       F45CD.    IF    V290-CTIDA = '001'                               lv25
                 NEXT SENTENCE ELSE GO TO     F45CD-FN.
                 IF    V290-CEFRY = 'M'                                 DOT
           MOVE        'Monthly' TO HTML-CEFRY.
                 IF    V290-CEFRY = 'Q'                                 DOT
           MOVE        'Quarterly' TO HTML-CEFRY.
                 IF    V290-CEFRY = 'S'                                 DOT
           MOVE        'Semi-annually' TO HTML-CEFRY.
                 IF    V290-CEFRY = 'A'                                 DOT
           MOVE        'Annually' TO HTML-CEFRY.
                 IF    V290-CEFRY = 'E'                                 DOT
           MOVE        'End of Term' TO HTML-CEFRY.
           STRING      HTML-STEP2-CER-IDIS                              DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CD-FN. EXIT.
      *N45CE.    NOTE *FOR MF                             *.
       F45CE.    IF    V290-CTIDA = '002'                               lv25
                 NEXT SENTENCE ELSE GO TO     F45CE-FN.
      *
                 IF    V290-DIVARR = 'CD'                               DOT
           MOVE        'Cash Dividends' TO HTML-DIVARR.
                 IF    V290-DIVARR = 'SD'                               DOT
           MOVE        'Split Dividends' TO HTML-DIVARR.
                 IF    V290-DIVARR = 'RD'                               DOT
           MOVE        'Reinvest Dividends' TO HTML-DIVARR.
           STRING      HTML-STEP2-MF-IDIS                               DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CE-FN. EXIT.
      *N45CH.    NOTE *FOR STEP3 DESTINATION DETAILS      *.
       F45CH.    IF    V290-DIVARR NOT = 'RD'                           lv20
                 OR    V290-CTIDA NOT = 002
                 NEXT SENTENCE ELSE GO TO     F45CH-FN.
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        'Verify Destination Details' TO
           HTML-STEP3TT.
                 IF    V290-CACTS = 'S'                                 DOT
           MOVE        'Destination Details' TO
           HTML-STEP3TT.
                 IF    V290-CTTYPG = 'SMTC'                             DOT
           MOVE        WL00-CTTYPG1 TO HTML-CTTYPG.
                 IF    V290-CTTYPG = 'MMTA'                             DOT
           MOVE        WL00-CTTYPG2 TO HTML-CTTYPG.
                 IF    V290-CTTYPG = 'SMDD'                             DOT
           MOVE        WL00-CTTYPG3 TO HTML-CTTYPG.
                 IF    V290-CTTYPG = 'APS'                              DOT
           MOVE        WL00-CTTYPG4 TO HTML-CTTYPG.
           STRING      HTML-DEST-DETAIL-IDIS                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CTTYPG = 'MMTA'                             DOT
           STRING      HTML-DEST-ACCOUNT-IDIS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CTTYPG = 'SMDD'                             DOT
           STRING      HTML-BANK-DETAIL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-DIST-IDIS-CONTD                             DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45CI.    NOTE *STEP4 WITHHOLDING                  *.
       F45CI.    IF    V290-DIVARR NOT = 'RD'                           lv25
                 OR    V290-CTIDA NOT = 002
                 NEXT SENTENCE ELSE GO TO     F45CI-FN.
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        'Verify Withholding Details' TO
           HTML-STEP4TT
                 ELSE
           MOVE        'Withholding Details' TO
           HTML-STEP4TT.
           STRING      HTML-STEP4-WITHHOLD-IDIS                         DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45CI-FN. EXIT.
       F45CH-FN. EXIT.
       F45CC-FN. EXIT.
       F45CB-FN. EXIT.
      *N45DB.    NOTE *FOR ARRANGEMENT TYPE               *.
       F45DB.    IF    V290-CALTN = 'IARB'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45DB-FN.
      *
                 IF    V290-CACTS = 'V'                                 DOT
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        'Verify Arrangement Option' TO
           HTML-STEP1TT3.
                 IF    V290-CACTS = 'S'                                 DOT
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        'Arrangement Option' TO
           HTML-STEP1TT3.
                 IF    V290-CACTS = 'V'                                 DOT
                 AND   V290-CTIDA = 134
           MOVE        'Verify Request Type' TO
           HTML-STEP1TT3.
                 IF    V290-CACTS = 'S'                                 DOT
                 AND   V290-CTIDA = 134
           MOVE        'Request Type' TO
           HTML-STEP1TT3.
           STRING      HTML-STEP1-TRAN-TYPE-IARB                        DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45DD.    NOTE *//      STEP2 TRAN DETAILS         *.
       F45DD.                                                           lv20
                 IF    V290-CACTS = 'V'                                 DOT
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        'Verify Rebalance Details' TO
           HTML-STEP2TT2.
                 IF    V290-CACTS = 'S'                                 DOT
                 AND   V290-CTIDA = (004 OR 005)
           MOVE        'Rebalance Details' TO
           HTML-STEP2TT2.
                 IF    V290-CACTS = 'V'                                 DOT
                 AND   V290-CTIDA = 134
           MOVE        'Verify Transaction Details' TO
           HTML-STEP2TT2.
                 IF    V290-CACTS = 'S'                                 DOT
                 AND   V290-CTIDA = 134
           MOVE        'Transaction Details' TO
           HTML-STEP2TT2.
                 IF    V290-CTIDA = 134                                 DOT
           MOVE        '234' TO HTML-WIDTH
                 ELSE
           MOVE        '200' TO HTML-WIDTH.
                 IF    V290-CACT NOT = 'I'                              DOT
           STRING      HTML-STEP2-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      '<TABLE WIDTH=984 '
           'STYLE="MARGIN-LEFT:5PX;MARGIN'
           '-RIGHT:35PX;" CELLPADDING=0 '
           'CELLSPACING=0 '
           'BORDER=0> <TBODY>'
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CEFRY = 'M'                                 DOT
           MOVE        'Monthly' TO HTML-MPMTF.
                 IF    V290-CEFRY = 'Q'                                 DOT
           MOVE        'Quarterly' TO HTML-MPMTF.
                 IF    V290-CEFRY = 'S'                                 DOT
           MOVE        'Semi-Annual' TO HTML-MPMTF.
                 IF    V290-CEFRY = 'A'                                 DOT
           MOVE        'Annual' TO HTML-MPMTF.
                 IF    V290-CACT = 'N'                                  DOT
                 OR    V290-CACT = 'M'
           STRING      HTML-STEP2-FREQ-STARTATE-IARB
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-GEENDX = ZEROS                              DOT
           MOVE        SPACES TO HTML-GEENDX.
                 IF    (V290-GEENDX NOT = ZEROS                         DOT
                 OR    V290-CTIDA = 134)
                 AND   V290-CACT NOT = 'I'
           STRING      HTML-END-DATE-IARB
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45DF.    NOTE * SUB-ACCOUNT TABLE                 *.
       F45DF.    IF    V290-CACT NOT = 'I'                              lv25
                 NEXT SENTENCE ELSE GO TO     F45DF-FN.
           MOVE        1 TO WC01-INDEX
           MOVE        1 TO SUTBLE-PT.
      *N45DG.    NOTE *FOR INSURANCE & ANNUITY ACCOUNT    *.
       F45DG.    IF    V290-CTIDA = 004 OR 005                          lv30
                 NEXT SENTENCE ELSE GO TO     F45DG-FN.
           MOVE        WL00-SUBD1 TO HTML-SUBD.
      *N45DH.    NOTE *GENERATE THE SUBACC TABLE          *.
       F45DH.                                                           lv35
           MOVE        1                        TO J45DHR
                                    GO TO     F45DH-B.
       F45DH-A.
           ADD         1                        TO J45DHR.
       F45DH-B.
           IF          J45DHR                   >  V290-NROWR
                                    GO TO     F45DH-FN.
           STRING      '<TR><TD WIDTH=9%><B>'
           DELIMITED BY SIZE
           WC01-MFND3 (WC01-INDEX)
           DELIMITED BY SIZE
           '</B></TD><TD WIDTH=47%><B>'
           DELIMITED BY SIZE
           WC01-MFDNMS (WC01-INDEX)
           DELIMITED BY SIZE
           '</B></TD><TD WIDTH=44%><B>'
           WC01-PAFDV (WC01-INDEX) '%'
           '</b></td></tr>'
           DELIMITED BY SIZE
           INTO HTML-SUBACC
           WITH POINTER SUTBLE-PT
           ADD         1 TO WC01-INDEX.
       F45DH-900. GO TO F45DH-A.
       F45DH-FN. EXIT.
       F45DG-FN. EXIT.
      *N45DJ.    NOTE *GENERATE THE SUBACC TABLE          *.
       F45DJ.    IF    V290-CTIDA = 134                                 lv30
                 NEXT SENTENCE ELSE GO TO     F45DJ-FN.
           MOVE        WL00-SUBD2 TO HTML-SUBD
           STRING      '<TD WIDTH=246>'
           DELIMITED BY SIZE
           WL00-SUBNAME
           DELIMITED BY SIZE
           '</TD><TD>'
           WL00-SUBPCT
           '</TD>'
           DELIMITED BY SIZE
           INTO HTML-SUBACC
           WITH POINTER SUTBLE-PT.
      *N45DL.    NOTE *GENERATE THE SUBACC TABLE          *.
       F45DL.                                                           lv35
           MOVE        1                        TO J45DLR
                                    GO TO     F45DL-B.
       F45DL-A.
           ADD         1                        TO J45DLR.
       F45DL-B.
           IF          J45DLR                   >  V290-NROWR
                                    GO TO     F45DL-FN.
           STRING      '<TR><TD><B>'
           DELIMITED BY SIZE
           WC01-MFDNMS (WC01-INDEX)
           DELIMITED BY SIZE
           '</B></TD><TD><B>'
           WC01-PAFDV (WC01-INDEX) '%'
           '</B></TD></TR>'
           DELIMITED BY SIZE
           INTO HTML-SUBACC
           WITH POINTER SUTBLE-PT
           ADD         1 TO WC01-INDEX.
       F45DL-900. GO TO F45DL-A.
       F45DL-FN. EXIT.
       F45DJ-FN. EXIT.
      *N45DM.    NOTE *F45DM                              *.
       F45DM.                                                           lv30
           STRING      '</TABLE>'
           DELIMITED BY SIZE
           INTO HTML-SUBACC
           WITH POINTER SUTBLE-PT.
       F45DM-FN. EXIT.
       F45DF-FN. EXIT.
      *N45DN.    NOTE *SUBACC DETAILS                     *.
       F45DN.    IF    V290-CACT NOT = 'I'                              lv25
                 NEXT SENTENCE ELSE GO TO     F45DN-FN.
           STRING      HTML-STEP2-SUBACC-DETAIL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45DO.    NOTE *STRING THE NOTE                    *.
       F45DO.    IF    V290-CTIDA = 004 OR 005                          lv30
                 NEXT SENTENCE ELSE GO TO     F45DO-FN.
           STRING      HTML-STEP2-SUBACC-NOTE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DO-FN. EXIT.
      *N45DP.    NOTE *STRING CHECKBOX CHOICE             *.
       F45DP.    IF    V290-CTIDA = 134                                 lv30
                 NEXT SENTENCE ELSE GO TO     F45DP-FN.
                 IF    V290-IAIND = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CHKBOX
                 ELSE
           MOVE        'NO' TO HTML-CHKBOX.
           STRING      HTML-STEP2-SUBACC-CHKBOX                         DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DP-FN. EXIT.
       F45DN-FN. EXIT.
      *N45DV.    NOTE *STRING CHECKBOX CHOICE             *.
       F45DV.    IF    V290-CTIDA = 134                                 lv25
                 AND   V290-CACT NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F45DV-FN.
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        'Verify Compliance Questions' TO
           HTML-STEP3TT1
                 ELSE
           MOVE        'Compliance Questions' TO
           HTML-STEP3TT1.
                 IF    V290-CCLPR = 'Y'                                 DOT
           MOVE        'YES' TO HTML-PROS.
                 IF    V290-CCLPR = 'N'                                 DOT
           MOVE        'NO' TO HTML-PROS.
                 IF    V290-CCLPR = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-PROS.
                 IF    V290-CCLCH = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CHARGE.
                 IF    V290-CCLCH = 'N'                                 DOT
           MOVE        'NO' TO HTML-CHARGE.
                 IF    V290-CCLCH = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CHARGE.
                 IF    V290-CCLSU = 'Y'                                 DOT
           MOVE        'YES' TO HTML-SUIT.
                 IF    V290-CCLSU = 'N'                                 DOT
           MOVE        'NO' TO HTML-SUIT.
                 IF    V290-CCLSU = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-SUIT.
           STRING      HTML-STEP3-TRAN-TYPE-IARB                        DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45DV-FN. EXIT.
       F45DD-FN. EXIT.
       F45DB-FN. EXIT.
      *N45EA.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45EA.    IF    V290-CALTN = 'AAPS'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45EA-FN.
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        'Verify Destination Details' TO
           HTML-STEP1TX.
                 IF    V290-CACTS = 'S'                                 DOT
           MOVE        'Destination Details' TO
           HTML-STEP1TX.
           STRING      HTML-DEST-DETAIL-AAPS                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CTTYPG = 'MMTA'                             DOT
           STRING      HTML-FINA-SERV-ACCOUNT-AAPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CTTYPG = 'SMDD'                             DOT
           STRING      HTML-BANK-DETAIL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-TABLE-END-AAPS                              DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45EA-FN. EXIT.
      *N45EB.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45EB.    IF    V290-CALTN = 'AAPS'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45EB-FN.
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        'Verify Transaction Details' TO
           HTML-STEP2TT2.
                 IF    V290-CACTS = 'S'                                 DOT
           MOVE        'Transaction Details' TO
           HTML-STEP2TT2.
           STRING      HTML-STEP2-TITLE                                 DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CACT = 'M' OR 'I'                           DOT
           STRING      HTML-ACCOUNT-STATUS-AAPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-BASIS-AMOUNT-AAPS                           DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-STEP2-FREQ-STARTATE-AAPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-GEENDX = ZEROES                             DOT
           MOVE        'N/A' TO HTML-GEENDXC.
           STRING      HTML-END-DATE-AAPS                               DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-WITHHOLD-AAPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-TABLE-END-AAPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45EB-FN. EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *BUILD INFO MSGS AND COMPLIANCE     *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
      *QUESTIONS FOR VERIFY/CONFIRM
      *N65BB.    NOTE *FOR VERIFY/CONFIRM PAGE            *.
       F65BB.         EXIT.                                             lv10
      *N65BE.    NOTE *LIST IMPLICATION MESSAGES          *.
       F65BE.    IF    V290-CTIDA NOT = 134                             lv15
                 NEXT SENTENCE ELSE GO TO     F65BE-FN.
      *FOR ALL TRANS EXCEPT RAVA5
                 IF    V290-CACTS = 'V'                                 DOT
           MOVE        WC01-IMP-MSG1-VER TO
           HTML-IMP-MSG01
           HTML-IMP-MSG02
           MOVE        'Read Important Messages' TO
           HTML-STEP5TT
           HTML-STEP5TA.
                 IF    V290-CACTS = 'S'                                 DOT
           MOVE        WC01-IMP-MSG1-CON TO
           HTML-IMP-MSG01
           HTML-IMP-MSG02
           MOVE        'Read Important Messages' TO
           HTML-STEP5TT
           HTML-STEP5TA.
                 IF    V290-CALTN NOT = 'AAPS'                          DOT
           STRING      HTML-IMP-MESSAGE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      HTML-IMP-MESSAGE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CTIDA = 002                                 DOT
                 AND   V290-CALTN = 'IDIS'
           STRING      HTML-IMP-MESSAGE-BIFURCATION
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CGMBR = 'X' OR 'Y'                          DOT
           STRING      HTML-IMP-MESSAGE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V290-CTTYPG = 'MMTA'                             DOT
                 AND   V290-CACT = ('N' OR 'M')
           STRING      HTML-IMP-MESS-MMTA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    (V290-CTIDA = (002 OR 001)                       DOT
                 AND   V290-CALTN = 'IDIS')
                 OR    (V290-CEFRY NOT = SPACE
                 AND   V290-CALTN = 'IARB')
           STRING      HTML-IMP-MESSAGE-THREEDAYS2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    (V290-CEFRY = SPACE                              DOT
                 AND   V290-CALTN = 'IARB')
                 OR    V290-CALTN = 'AAPS'
           MOVE        HTML-IMP-COMPLETE-TIME1 TO
           HTML-CONF-MSG5
           MOVE        HTML-IMP-COMPLETE-TIME2 TO
           HTML-CONF-MSG6.
           STRING      HTML-IMP-MESSAGE-LAST                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65BE-900. GO TO F65CE-FN.
       F65BE-FN. EXIT.
      *N65CE.    NOTE *IMPLICATION MESSAGES FOR RAVA5     *.
       F65CE.                                                           lv15
           STRING      HTML-IMP-MESSAGE-RAVA5
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65CE-FN. EXIT.
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
      *N70BE.    NOTE *LOOP THRU XML-BLOB                 *.
       F70BE.                                                           lv15
           MOVE        1                        TO J70BER
                                    GO TO     F70BE-B.
       F70BE-A.
           ADD         1                        TO J70BER.
       F70BE-B.
           IF          J70BER                   >  HTML-LIMIT
                                    GO TO     F70BE-FN.
      *
           MOVE        HTML-PT TO TEMP-PT
           MOVE        TEMP-PT TO HTML-PT
           UNSTRING     HTML-BLOB  INTO  HTML-TEXT
                      WITH  POINTER HTML-PT.
      *N70BG.    NOTE *MOVE WS VARIABLE INTO THTML        *.
       F70BG.    IF    HTML-TEXT NOT = SPACES                           lv20
                 NEXT SENTENCE ELSE GO TO     F70BG-FN.
      *
           MOVE        01 TO V290-CHTML (TALLI)
           MOVE        HTML-TEXT TO V290-THTML (TALLI)
           ADD         1 TO TALLI.
       F70BG-FN. EXIT.
       F70BE-900. GO TO F70BE-A.
       F70BE-FN. EXIT.
       F70BB-FN. EXIT.
      *N70EB.    NOTE *MOVE END OF FILE MARKER            *.
       F70EB.                                                           lv10
      *
           MOVE        99 TO V290-CHTML (TALLI)
           MOVE        SPACES TO V290-THTML (TALLI).
      *N70EF.    NOTE *CALCULATE THTML LENGTH             *.
       F70EF.                                                           lv15
      *
           COMPUTE     V290-QBLCK = TALLI.
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
      *N92BB.    NOTE *GENERATE SOURCE DOC ID             *.            ANIOBJ
       F92BB.         EXIT.                                             lv10
      *N92CB.    NOTE *UNFORMAT TS                        *.            ANIOBJ
       F92CB.                                                           lv15
           MOVE        V290-DXTMS2 TO 7-DB2-DXTMSA                      ANIOBJ
           MOVE        7-DB2-DTGCY TO 7-DTGCY                           ANIOBJ
           MOVE        7-DB2-DTGMM TO 7-DTGMM                           ANIOBJ
           MOVE        7-DB2-DTGDD TO 7-DTGDD.                          ANIOBJ
       F92CB-FN. EXIT.
      *N92DB.    NOTE *DETERMINE JULIAN DAY               *.            ANIOBJ
       F92DB.                                                           lv15
           MOVE        7-CSTDTE TO WT01-XDAGP                           ANIOBJ
           MOVE        1 TO WT01-XDACT                                  ANIOBJ
           MOVE        1 TO WT01-XDACV                                  ANIOBJ
      *ADD AADA58 HERE.                                                 ANIOBJ
      *CALL MWS100EX - DYNAMIC                                          DOT
           CALL        MWS100EX USING WT01-DD05.                        AADA58
       F92DB-FN. EXIT.
      *N92EB.    NOTE *BUILD DOC ID                       *.            ANIOBJ
       F92EB.                                                           lv15
           MOVE        WT01-XDAJP TO 7-CASE-DTJUL                       ANIOBJ
           MOVE        7-CASE-DTJDD TO WT01-DTJDD                       ANIOBJ
           ADD         366 TO WT01-DTJDD                                ANIOBJ
           MOVE        7-DB2-DTGYY TO WT01-DTTCY                        ANIOBJ
           MOVE        7-DB2-DTTHH TO WT01-DTTHH                        ANIOBJ
           MOVE        7-DB2-DTTMN TO WT01-DTTMN                        ANIOBJ
           MOVE        7-DB2-DTTSS TO WT01-DTTSS                        ANIOBJ
           COMPUTE     WT01-DTTNN = 7-DB2-DTTNN                         ANIOBJ
           / 1000.                                                      ANIOBJ
       F92EB-FN. EXIT.
       F92BB-FN. EXIT.
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
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *MISC CALCULATIONS                  *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95CD.    NOTE *CALCULATE CHECK DIGIT FOR DEST     *.
       F95CD.                                                           lv10
           MOVE        SPACES TO WE00                                   DOT
           MOVE        7-CTIDN TO                                       $CHECK
           WE00-CTIDN                                                   $CHECK
           CALL        'CMU002DY' USING WE00.                           $CHECK
       F95CD-FN. EXIT.
      *N95DM.    NOTE *POPULATE STYLE SHEET TAGS          *.
       F95DM.                                                           lv10
                 IF    V290-CACTS = 'V'                                 DOT
      * *****************************
           STRING      'style="margin-left:5px;'
           'margin-right:35px;"'
           DELIMITED BY SIZE
           INTO HTML-TAG1.
                 IF    V290-CACTS = 'S'                                 DOT
           MOVE        '</TD></TR></TABLE>' TO
           HTML-TABLE-BOTTOM.
       F95DM-FN. EXIT.
       F95-FN.   EXIT.
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
