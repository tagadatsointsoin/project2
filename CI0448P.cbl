       IDENTIFICATION DIVISION.                                         CI0448
       PROGRAM-ID.  CI0448P.                                            CI0448
      *AUTHOR.         BUILD HTML PAGE FOR DCAP/APCP.                   CI0448
      *DATE-COMPILED.   09/08/14.                                       CI0448
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2011                          *ACOPYP
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
      *     COPR. 2011                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0448
       CONFIGURATION SECTION.                                           CI0448
       SOURCE-COMPUTER. IBM-370.                                        CI0448
       OBJECT-COMPUTER. IBM-370.                                        CI0448
       DATA DIVISION.                                                   CI0448
       WORKING-STORAGE SECTION.                                         CI0448
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
                        PICTURE 99B99B9999.                             CI0448
      *!WS
           05  7-DE00-DNACG
                        PICTURE 99B99B9999.                             CI0448
      *!WS
           05  7-DE00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0448
      *!WS
           05  7-DE00-GEEND
                        PICTURE 99B99B9999.                             CI0448
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0448
            10            I93B-CEADC  PICTURE  X                        CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-DACTT  PICTURE  X(10)                    CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-GEOPDC PICTURE  X(8)                     CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-GEOPDB PICTURE  X(8)                     CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-CAEMCE PICTURE  X(8)                     CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-CAEMCD PICTURE  X(8)                     CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-GETIMM PICTURE  X(8)                     CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0448
                          VALUE                ZERO                     CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            I93B-GERTC  PICTURE  X                        CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-DXTMST PICTURE  X(26)                    CI0448
                          VALUE                SPACE.                   CI0448
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0448
                          VALUE                SPACE.                   CI0448
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
                        PICTURE X(66)                                   CI0448
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WD115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0448
      *!WI pl=WD120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0448
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0448
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0448
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0448
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0448
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0448
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0448
       01               WE00.                                           $CHECK
      *!WI pl=WE110                                                     $CHECK
         05             WE00-CTIDN                                      $CHECK
                        PICTURE X(24)                                   CI0448
                        JUSTIFIED RIGHT.                                $CHECK
      *!WI pl=WE130                                                     $CHECK
         05             WE00-GERTC                                      $CHECK
                        PICTURE X.                                      CI0448
      *!WI
       01 7-CTIDN VALUE SPACES
                        PICTURE X(24).                                  CI0448
       01    WL00-CIRAP1  PIC X(50)
                     VALUE 'Not applicable'.
       01    WL00-CIRAP2  PIC X(50)
                     VALUE 'Current year'.
       01    WL00-CIRAP3  PIC X(50)
                     VALUE 'Rollover'.
       01    WL00-CIRAP4  PIC X(50)
                     VALUE 'Prior year'.
       01    WL00-CIRAP5  PIC X(50)
                     VALUE 'SEP/SRA Current year'.
      *MISCELLANEOUS FIELDS
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0448
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
                        PICTURE 9999B9999B9999B9999.                    CI0448
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

           05 WS00-RTN.
      *!WI
              10 WS00-NTR
                        PICTURE 9(8).                                   CI0448
              10 FILLER      PIC X    VALUE '-'.
      *!WI
              10 WS00-GECKD
                        PICTURE 9.                                      CI0448

          05 WS00-MSGTXT.
             10 WS00-MSGNUM PIC Z9.
             10 WS00-DOT    PIC X     VALUE '.'.
             10 FILLER      PIC X     VALUE SPACE.
      *!WI
             10 WS00-TMESSC
                        PICTURE X(254).                                 CI0448
      *!WS
          05 WS00-ACOTD
                        PICTURE ZZZ,ZZZ,ZZ9.99-                         CI0448
                          BLANK WHEN ZERO.                              CI0448
      *!WS
          05 WS00-ADBRQM
                        PICTURE ZZ,ZZZ,ZZZ,ZZZ.99-.                     CI0448
      *!WS
          05 WS00-ADBCRQ
                        PICTURE ZZ,ZZZ,ZZZ,ZZ9.99-.                     CI0448
      *!WS
          05 WS00-ADBRQ
                        PICTURE ZZ,ZZZ,ZZZ,ZZ9.99-.                     CI0448
      *!WS
          05 WS00-ADBRF1
                        PICTURE Z,ZZZ,ZZZ.99.                           CI0448
          05 WS00-PWHLD    PIC ZZ9.99  VALUE ZEROES.

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
      ******************************************************************
      *VARIABLES FOR STRING CANCAT TO CREATE SUB-ACCT FND NAME TABLE.
      ******************************************************************
       01 WS00-DATCE  PIC X(8) VALUE ZEROES.
      **----------------------------------------------------------------ANIOBJ
      **  DATE WORK AREA USED BY MACRO AADA58                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
      *!WF DSP=WT DSL=DD SEL=01 FOR=I LEV=1                             ANIOBJ
       01                 WT00.                                         CI0448
          05              WT00-SUITE.                                   CI0448
            15       FILLER         PICTURE  X(00093).                  CI0448
       01                 WT01  REDEFINES      WT00.                    CI0448
            10            WT01-XDAT8.                                   CI0448
            11            WT01-XDATC  PICTURE  XX.                      CI0448
            11            WT01-XDATY  PICTURE  XX.                      CI0448
            11            WT01-XDATM  PICTURE  XX.                      CI0448
            11            WT01-XDATD  PICTURE  XX.                      CI0448
            10            WT01-XDAT8D                                   CI0448
                          REDEFINES            WT01-XDAT8               CI0448
               PICTURE    9(8).                                         CI0448
            10            WT01-XDAT81.                                  CI0448
            11            WT01-XDATM1 PICTURE  XX.                      CI0448
            11            WT01-XDATD1 PICTURE  XX.                      CI0448
            11            WT01-XDATC1 PICTURE  XX.                      CI0448
            11            WT01-XDATY1 PICTURE  XX.                      CI0448
            10            WT01-XDAT80                                   CI0448
                          REDEFINES            WT01-XDAT81              CI0448
               PICTURE    9(8).                                         CI0448
            10            WT01-XDAT62.                                  CI0448
            11            WT01-XDATM2 PICTURE  XX.                      CI0448
            11            WT01-XDATD2 PICTURE  XX.                      CI0448
            11            WT01-XDATY2 PICTURE  XX.                      CI0448
            10            WT01-XDAT69                                   CI0448
                          REDEFINES            WT01-XDAT62              CI0448
               PICTURE    9(6).                                         CI0448
            10            WT01-XDATCU.                                  CI0448
            11            WT01-XDATC9 PICTURE  99.                      CI0448
            11            WT01-XDAYMD.                                  CI0448
            12            WT01-XDATY9 PICTURE  99.                      CI0448
            12            WT01-XDAMD.                                   CI0448
            13            WT01-XDATM9 PICTURE  99.                      CI0448
            13            WT01-XDATD9 PICTURE  99.                      CI0448
            10            WT01-XDAT89 PICTURE  9(8).                    CI0448
            10            WT01-XDAJC  PICTURE  9(7).                    CI0448
            10            WT01-XDAJC1.                                  CI0448
            11            WT01-XDAJC9 PICTURE  99.                      CI0448
            11            WT01-XDAJY  PICTURE  99.                      CI0448
            11            WT01-XDAJN  PICTURE  999.                     CI0448
            10            WT01-XDAB   PICTURE  9(5).                    CI0448
            10            WT01-DD05.                                    CI0448
            11            WT01-XDACT  PICTURE  S9(3)                    CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            WT01-XDACV  PICTURE  S9                       CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            WT01-XDAGP  PICTURE  S9(9)                    CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            WT01-XDAJP  PICTURE  S9(7)                    CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            WT01-XDACV1 PICTURE  S9                       CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            WT01-XDAGP1 PICTURE  S9(9)                    CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            WT01-XDAJP1 PICTURE  S9(7)                    CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            WT01-XW03.                                    CI0448
            11            WT01-XDATG.                                   CI0448
            12            WT01-XDAT1.                                   CI0448
            13            WT01-XDAT19 PICTURE  99.                      CI0448
            12            WT01-XDAT2.                                   CI0448
            13            WT01-XDAT29 PICTURE  99.                      CI0448
            12            WT01-XDAT3.                                   CI0448
            13            WT01-XDAT39 PICTURE  99.                      CI0448
            12            WT01-XDAT4.                                   CI0448
            13            WT01-XDAT49 PICTURE  99.                      CI0448
            11            WT01-XLEAPY PICTURE  99.                      CI0448
            11            WT01-DTGCY  PICTURE  9(4).                    CI0448
            11            WT01-FILLER                                   CI0448
                          REDEFINES            WT01-DTGCY.              CI0448
            12            WT01-DTGCC  PICTURE  9(2).                    CI0448
            12            WT01-DTGYY  PICTURE  9(2).                    CI0448
                                                                        ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      **  WORKING STORAGE FOR CASE DOC DATE                             ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      *!WI pl=WT115                                                     ANIOBJ
       01 7-CSTDTE                                                      ANIOBJ
                        PICTURE X(8).                                   CI0448
       01 FILLER REDEFINES 7-CSTDTE.                                    ANIOBJ
      *!WI pl=WT125                                                     ANIOBJ
          05 7-DTGCY                                                    ANIOBJ
                        PICTURE 9(4).                                   CI0448
      *!WI pl=WT130                                                     ANIOBJ
          05 7-DTGMM                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0448
      *!WI pl=WT135                                                     ANIOBJ
          05 7-DTGDD                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0448
      *                                                                 ANIOBJ
      *!WI pl=WT145                                                     ANIOBJ
       01 7-CASE-DTJUL                                                  ANIOBJ
                        PICTURE 9(7).                                   CI0448
       01 FILLER REDEFINES 7-CASE-DTJUL.                                ANIOBJ
      *!WI pl=WT150                                                     ANIOBJ
          05 7-CASE-DTGCY                                               ANIOBJ
                        PICTURE 9(4).                                   CI0448
      *!WI pl=WT155                                                     ANIOBJ
          05 7-CASE-DTJDD                                               ANIOBJ
                        PICTURE 9(3).                                   CI0448
      *                                                                 ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      ** CASEDOC-ID GENERATED                                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
       01 WT01-NIOBJC.                                                  ANIOBJ
         05 WT01-DTTCY    PIC 9.                                        ANIOBJ
      *!WI pl=WT270                                                     ANIOBJ
         05 WT01-DTJDD                                                  ANIOBJ
                        PICTURE 9(3).                                   CI0448
      *!WI pl=WT280                                                     ANIOBJ
         05 WT01-DTTHH                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0448
      *!WI pl=WT290                                                     ANIOBJ
         05 WT01-DTTMN                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0448
      *!WI pl=WT300                                                     ANIOBJ
         05 WT01-DTTSS                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0448
         05 WT01-DTTNN    PIC 9(3).                                     ANIOBJ
         05 FILLER        PIC X(4)    VALUE '.001'.                     ANIOBJ
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58
       01   DEBUT-WSS.                                                  CI0448
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0448
            05   IK     PICTURE X.                                      CI0448
       01  CONSTANTES-PAC.                                              CI0448
           05  FILLER  PICTURE X(87)   VALUE                            CI0448
                     '6015 CAT09/08/14CI0448ADMIN   14:35:29CI0448P AMERCI0448
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0448
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0448
           05  NUGNA   PICTURE X(5).                                    CI0448
           05  APPLI   PICTURE X(3).                                    CI0448
           05  DATGN   PICTURE X(8).                                    CI0448
           05  PROGR   PICTURE X(6).                                    CI0448
           05  CODUTI  PICTURE X(8).                                    CI0448
           05  TIMGN   PICTURE X(8).                                    CI0448
           05  PROGE   PICTURE X(8).                                    CI0448
           05  COBASE  PICTURE X(4).                                    CI0448
           05  DATGNC  PICTURE X(10).                                   CI0448
           05  RELEAS  PICTURE X(7).                                    CI0448
           05  DATGE   PICTURE X(10).                                   CI0448
           05  DATSQ   PICTURE X(10).                                   CI0448
       01  DATCE.                                                       CI0448
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0448
         05  DATOR.                                                     CI0448
           10  DATOA  PICTURE XX.                                       CI0448
           10  DATOM  PICTURE XX.                                       CI0448
           10  DATOJ  PICTURE XX.                                       CI0448
       01  DAT6.                                                        CI0448
            10 DAT61.                                                   CI0448
            15 DAT619  PICTURE 99.                                      CI0448
            10 DAT62.                                                   CI0448
            15 DAT629  PICTURE 99.                                      CI0448
            10 DAT63   PICTURE XX.                                      CI0448
       01  DAT8.                                                        CI0448
            10 DAT81   PICTURE XX.                                      CI0448
            10 DAT8S1  PICTURE X.                                       CI0448
            10 DAT82   PICTURE XX.                                      CI0448
            10 DAT8S2  PICTURE X.                                       CI0448
            10 DAT83   PICTURE XX.                                      CI0448
       01  DAT8E    REDEFINES    DAT8.                                  CI0448
            10 DAT81E  PICTURE X(4).                                    CI0448
            10 DAT82E  PICTURE XX.                                      CI0448
            10 DAT83E  PICTURE XX.                                      CI0448
       01  DAT6C.                                                       CI0448
            10  DAT61C PICTURE XX.                                      CI0448
            10  DAT62C PICTURE XX.                                      CI0448
            10  DAT63C.                                                 CI0448
             15 DAT63CC PICTURE XX.                                     CI0448
             15 DAT64C  PICTURE XX.                                     CI0448
       01  DAT8C.                                                       CI0448
            10  DAT81C  PICTURE XX.                                     CI0448
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0448
            10  DAT82C  PICTURE XX.                                     CI0448
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0448
            10  DAT83C.                                                 CI0448
             15 DAT83CC PICTURE XX.                                     CI0448
             15 DAT84C  PICTURE XX.                                     CI0448
       01  DATSEP     PICTURE X VALUE '/'.                              CI0448
       01  DATSEW     PICTURE X.                                        CI0448
       01   VARIABLES-CONDITIONNELLES.                                  CI0448
            05                  FT      PICTURE X VALUE '0'.            CI0448
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0448
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0448
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J45BFR PICTURE S9(4) VALUE  ZERO.
            05           J45BJR PICTURE S9(4) VALUE  ZERO.
            05           J70BDR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0448
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT.
      *THIS COPYBOOK CONTAINS THE HEADER, SOURCE ACCOUNT DETAILS AND
      *DESTINATION ACCOUNT DETAILS FOR THE HTML PAGE.
      ******************************************************************
       COPY CI0448C3.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0448                *
      ******************************************************************
      *!WF DSP=V2 DSL=V2 SEL=85 FOR=I LEV=1 PLT=80
       01                 V200.                                         CI0448
          05              V200-SUITE.                                   CI0448
            15       FILLER         PICTURE  X(06534).                  CI0448
       01                 V285  REDEFINES      V200.                    CI0448
            10            V285-MAPPN  PICTURE  X(10).                   CI0448
            10            V285-NSSSI  PICTURE  X(24).                   CI0448
            10            V285-CALTN  PICTURE  X(4).                    CI0448
            10            V285-CTID.                                    CI0448
            11            V285-CTIDA  PICTURE  9(3).                    CI0448
            11            V285-CTIDN.                                   CI0448
            12            V285-CTIDNP PICTURE  X(13).                   CI0448
            12            V285-CTIDND PICTURE  9(11).                   CI0448
            10            V285-GECKD2 PICTURE  9.                       CI0448
            10            V285-CLID   PICTURE  X(23).                   CI0448
            10            V285-CLTIN  PICTURE  9(12).                   CI0448
            10            V285-IVEUP  PICTURE  X.                       CI0448
            10            V285-PRCOD  PICTURE  9(5).                    CI0448
            10            V285-CTTLN1 PICTURE  X(30).                   CI0448
            10            V285-CTTLN2 PICTURE  X(30).                   CI0448
            10            V285-CTTLN3 PICTURE  X(30).                   CI0448
            10            V285-CTTBO1 PICTURE  X(45).                   CI0448
            10            V285-CTTBO2 PICTURE  X(45).                   CI0448
            10            V285-CSPRDN PICTURE  X(30).                   CI0448
            10            V285-GEOPD2 PICTURE  X(8).                    CI0448
            10            V285-CQACTL PICTURE  X(45).                   CI0448
            10            V285-DAEDTO PICTURE  X(8).                    CI0448
            10            V285-DPTIM  PICTURE  X(8).                    CI0448
            10            V285-CSLCT  PICTURE  X.                       CI0448
            10            V285-NGEOPA PICTURE  X(08).                   CI0448
            10            V285-NGEOR  PICTURE  9(08).                   CI0448
            10            V285-ADBRQM PICTURE  S9(11)V99.               CI0448
            10            V285-IRMND  PICTURE  X.                       CI0448
            10            V285-PWHLD  PICTURE  S999V9(5)                CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            V285-TWITH  PICTURE  X(12).                   CI0448
            10            V285-CCLPR  PICTURE  X.                       CI0448
            10            V285-CCLCH  PICTURE  X.                       CI0448
            10            V285-CCLSU  PICTURE  X.                       CI0448
            10            V285-IAIDE  PICTURE  X.                       CI0448
            10            V285-CTTYPG PICTURE  X(04).                   CI0448
            10            V285-ACOTD  PICTURE  S9(9)V99                 CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            V285-DCACG  PICTURE  9(8).                    CI0448
            10            V285-DCACD  PICTURE  X(10).                   CI0448
            10            V285-DNACG  PICTURE  9(8).                    CI0448
            10            V285-CCONF  PICTURE  X(25).                   CI0448
            10            V285-ATROLL PICTURE  X(25).                   CI0448
            10            V285-DXTMS2 PICTURE  X(26).                   CI0448
            10            V285-MPLNR2 PICTURE  X(40).                   CI0448
            10            V285-NROWR  PICTURE  9(3).                    CI0448
            10            V285-NROWT  PICTURE  9(3).                    CI0448
            10            V285-ST95                                     CI0448
                          OCCURS       003     TIMES.                   CI0448
            11            V285-CIRAP  PICTURE  XX.                      CI0448
            11            V285-MPMTT  PICTURE  X(20).                   CI0448
            11            V285-CTID01 PICTURE  X(27).                   CI0448
            11            V285-ADBCRQ PICTURE  S9(11)V99                CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            V285-ST96                                     CI0448
                          OCCURS       003     TIMES.                   CI0448
            11            V285-CTTYPI PICTURE  X(04).                   CI0448
            11            V285-CLORN  PICTURE  X(45).                   CI0448
            11            V285-NPBN   PICTURE  X(20).                   CI0448
            11            V285-NTR    PICTURE  9(8).                    CI0448
            11            V285-TTBAL  PICTURE  X(15).                   CI0448
            11            V285-MCSIG  PICTURE  X(30).                   CI0448
            11            V285-ADBRQ  PICTURE  S9(11)V99                CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            V285-GECKD  PICTURE  9.                       CI0448
            10            V285-TMESSC PICTURE  X(254)                   CI0448
                          OCCURS       020     TIMES.                   CI0448
            10            V285-CSTRG  PICTURE  X(8).                    CI0448
            10            V285-AACCD3 PICTURE  S9(11)V99                CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            V285-MSMNM  PICTURE  X(50).                   CI0448
            10            V285-IADYP  PICTURE  X.                       CI0448
            10            V285-CTID02.                                  CI0448
            11            V285-CACTID PICTURE  9(3).                    CI0448
            11            V285-CTIDNB.                                  CI0448
            12            V285-CTIDPY PICTURE  X(13).                   CI0448
            12            V285-CTIDNA PICTURE  9(11).                   CI0448
            10            V285-CTIFR  PICTURE  X.                       CI0448
            10            V285-CRITO  PICTURE  X.                       CI0448
            10            V285-CTNOB  PICTURE  X.                       CI0448
            10            V285-CQACT  PICTURE  999.                     CI0448
            10            V285-IWTRI  PICTURE  X.                       CI0448
            10            V285-ADBRF1 PICTURE  S9(9)V99                 CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            V285-NWIRE  PICTURE  99.                      CI0448
            10            V285-CSTRGA PICTURE  X(8).                    CI0448
            10            V285-MPRN4Y PICTURE  X(100).                  CI0448
            10            V285-FILLER PICTURE  X(84).                   CI0448
      ******************************************************************
      ** HTML RETURNED TO CALLING RPC IN 200 BYTE CHUNKS               *
      ******************************************************************
      *
      *!WF DSP=HT DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HT00.                                         CI0448
          05              HT00-SUITE.                                   CI0448
            15       FILLER         PICTURE  X(90906).                  CI0448
       01                 HT93  REDEFINES      HT00.                    CI0448
            10            HT93-QBLCK  PICTURE  9(6).                    CI0448
            10            HT93-QT9O.                                    CI0448
            11            HT93-QT9B                                     CI0448
                          OCCURS       450     TIMES.                   CI0448
            12            HT93-CHTML  PICTURE  99.                      CI0448
            12            HT93-THTML  PICTURE  X(200).                  CI0448
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0448
          05              MS00-SUITE.                                   CI0448
            15       FILLER         PICTURE  X(00542).                  CI0448
       01                 MS03  REDEFINES      MS00.                    CI0448
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0448
                          COMPUTATIONAL-3.                              CI0448
            10            MS03-CMSSF  PICTURE  XX.                      CI0448
            10            MS03-DU09.                                    CI0448
            11            MS03-CMESA  PICTURE  S9(9)                    CI0448
                          BINARY.                                       CI0448
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0448
                          BINARY.                                       CI0448
            11            MS03-CMESB  PICTURE  S9(9)                    CI0448
                          BINARY.                                       CI0448
            11            MS03-CMSST  PICTURE  S9(9)                    CI0448
                          BINARY.                                       CI0448
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0448
                          BINARY.                                       CI0448
            11            MS03-QELLAA PICTURE  S9(9)                    CI0448
                          BINARY.                                       CI0448
            11            MS03-TMESS4 PICTURE  X(512).                  CI0448
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0448
            10            MX11-QMSGS  PICTURE  9(03).                   CI0448
            10            MX11-PJ09                                     CI0448
                          OCCURS       025     TIMES.                   CI0448
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0448
                          COMPUTATIONAL-3.                              CI0448
            11            MX11-CMESB  PICTURE  S9(9)                    CI0448
                          BINARY.                                       CI0448
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V285
                                HT93
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
      *N01.      NOTE *************************************.            CI0448
      *               *                                   *             CI0448
      *               *INITIALISATIONS                    *             CI0448
      *               *                                   *             CI0448
      *               *************************************.            CI0448
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0448
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0448
      *               *                                   *             CI0448
      *               *FIN DE TRAITEMENT                  *             CI0448
      *               *                                   *             CI0448
      *               *************************************.            CI0448
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0448
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
           MOVE        V285-CSPRDN TO HTML-PRCMN.
                 IF    V285-CSLCT = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CSLCT
           MOVE        V285-NGEOR TO HTML-NGEOR
                 ELSE
           MOVE        'NO ' TO HTML-CSLCT
           MOVE        'N/A' TO HTML-NGEOR.
           MOVE        V285-CTTLN1 TO HTML-CTTLN1                       DOT
           MOVE        V285-CTTLN2 TO HTML-CTTLN2
           MOVE        V285-CTTLN3 TO HTML-CTTLN3
           MOVE        V285-CQACTL TO HTML-CQACTL
           MOVE        V285-CTTBO1 TO HTML-CTTBO1
           MOVE        V285-CTTBO2 TO HTML-CTTBO2
           MOVE        V285-GEOPD2 TO HTML-GEOPD2.
                 IF    V285-ADBRQM NUMERIC                              DOT
                 AND   V285-ADBRQM > ZEROES
           MOVE        V285-ADBRQM TO WS00-ADBRQM.
           MOVE        WS00-ADBRQM TO HTML-ADBRQM                       DOT
           MOVE        V285-MPLNR2 TO HTML-MPLNR2
           MOVE        V285-ATROLL TO HTML-ATROLL
           MOVE        V285-CCONF TO HTML-CCONF
           MOVE        V285-NGEOPA TO HTML-NGEOPA
           MOVE        V285-CTID02 (12:16) TO WS00-NCTIDE
           MOVE        V285-CTID02 (1:3) TO WS00-CTIDA
           MOVE        V285-CTID02 (4:24) TO 7-CTIDN
           PERFORM     F95CD THRU F95CD-FN
           MOVE        WE00-GERTC TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID02.
                 IF    V285-CALTN = 'APCP'                              DOT
                 AND   V285-CTIDA = 133
           MOVE        V285-CSTRGA TO HTML-CSTRGA
           MOVE        V285-MPRN4Y TO HTML-MPRN4Y
           MOVE        V285-MSMNM TO HTML-MSMNM.
                 IF    V285-CALTN = 'APCP'                              DOT
                 AND   V285-CTIDA = 021
           MOVE        V285-MSMNM TO HTML-MPRN4X.
                 IF    V285-CTIFR = 'A'                                 DOT
           MOVE        'A. Less than 1 year' TO
           HTML-CTIFR.
                 IF    V285-CTIFR = 'B'                                 DOT
           MOVE        'B. 1-3 years' TO
           HTML-CTIFR.
                 IF    V285-CTIFR = 'C'                                 DOT
           MOVE        'C. 4-7 years' TO
           HTML-CTIFR.
                 IF    V285-CTIFR = 'D'                                 DOT
           MOVE        'D. 8-10 years' TO
           HTML-CTIFR.
                 IF    V285-CTIFR = 'E'                                 DOT
           MOVE        'E. 11+ Years' TO
           HTML-CTIFR.
                 IF    V285-CRITO = 'A'                                 DOT
           MOVE        'A. Conservative' TO
           HTML-CRITO.
                 IF    V285-CRITO = 'B'                                 DOT
           MOVE        'B. Conservative/Moderate' TO
           HTML-CRITO.
                 IF    V285-CRITO = 'C'                                 DOT
           MOVE        'C. Moderate' TO
           HTML-CRITO.
                 IF    V285-CRITO = 'D'                                 DOT
           MOVE        'D. Moderate/Aggressive' TO
           HTML-CRITO.
                 IF    V285-CRITO = 'E'                                 DOT
           MOVE        'E. Aggressive' TO
           HTML-CRITO.
                 IF    V285-CTNOB = 'J'                                 DOT
           MOVE        'J. Growth with income' TO
           HTML-CTNOB.
                 IF    V285-CTNOB = 'I'                                 DOT
           MOVE        'I. Growth' TO
           HTML-CTNOB.
                 IF    V285-CTNOB = 'C'                                 DOT
           MOVE        'C. Income' TO
           HTML-CTNOB.
                 IF    V285-CTNOB = 'H'                                 DOT
           MOVE        'H. Speculation' TO
           HTML-CTNOB.
                 IF    V285-CCLPR = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLPR.
                 IF    V285-CCLPR = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLPR.
                 IF    V285-CCLPR = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLPR.
                 IF    V285-CCLCH = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLCH.
                 IF    V285-CCLCH = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLCH.
                 IF    V285-CCLCH = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLCH.
                 IF    V285-CCLSU = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLSU.
                 IF    V285-CCLSU = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLSU.
                 IF    V285-CCLSU = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLSU.
       F35BB-FN. EXIT.
      *N35BC.    NOTE *FORMAT CONTRACT ID                 *.
       F35BC.                                                           lv10
      *
           MOVE        V285-CTIDND TO WS00-NCTIDE
           MOVE        V285-GECKD2 TO WS00-GECKD1
           MOVE        V285-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-CTID.
      *N35BD.    NOTE *FORMAT THE CURRENT DATE,           *.
       F35BD.                                                           lv15
      *EFFECTIVE DATE AND SEND TRANS
      *RECEIVED TIME
      *
           STRING      '000'
           V285-DCACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT
           INITIALIZE  I93B
           MOVE        V285-DCACG TO 7-DE00-DCACG
           STRING      V285-DCACG (1:4) '/'
           V285-DCACG (5:2) '/'
           V285-DCACG (7:2) '/'
           DELIMITED BY SIZE INTO
           I93B-DACTT.
      *I93B
           MOVE        V285-DCACD TO I93B-DACTT
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
           MOVE        WS00-DATCE                                       CI0448
           TO DAT8E DAT6C                                               CI0448
           MOVE DAT81E TO DAT63C                                        CI0448
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0448
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0448
      *!ADM "7-DE00-DCACG     HTML-DCACG"
           MOVE        7-DE00-DCACG                                     CI0448
           TO DAT8E DAT6C                                               CI0448
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0448
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0448
           MOVE   DAT8C TO  HTML-DCACG.                                 CI0448
                 IF    (WS00-TIMER >= WS-GETIMT                         DOT
                 AND   V285-DCACG <= WS00-DATCE)
      *NEXT ACTG DATE IF AFTER MKT CLS
           STRING      '000' V285-DNACG (7:2)
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
           MOVE        'CENTRAL TIME' TO WS00-CST                       DOT
           STRING      WS00-TIMER (1:2) ':'
           WS00-TIMER (3:2) ':'
           WS00-TIMER (5:2) ' '
           WS00-AMPM ' '
           WS00-CST
           DELIMITED BY SIZE INTO
           HTML-TIMER.
       F35BD-FN. EXIT.
      *N35BE.    NOTE *FORMAT THE ORDER RECEIVED DATE,    *.
       F35BE.                                                           lv15
      *EFFECTIVE DATE AND SEND THE
      *ORDER RECEIVED TIME
      *
      *!ADM "V285-DAEDTO     HTML-DRECD"
           MOVE        V285-DAEDTO                                      CI0448
           TO DAT8E DAT6C                                               CI0448
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0448
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0448
           MOVE   DAT8C TO  HTML-DRECD                                  CI0448
      *
           STRING      V285-DPTIM DELIMITED BY SIZE
           ' CENTRAL TIME'
           DELIMITED BY SIZE
           INTO HTML-TIMER1.
       F35BE-FN. EXIT.
       F35BC-FN. EXIT.
      *N35BM.    NOTE *STRING CONFIRMATION INFORMATION    *.
       F35BM.    IF    V285-IVEUP = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35BM-FN.
      *
           STRING      HTML-CONFIRM-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BM-FN. EXIT.
      *N35CB.    NOTE *MOVE VERIFY INFORMATION            *.
       F35CB.    IF    V285-IVEUP = 'V'                                 lv10
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
       F35DP.    IF    V285-CTTLN2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DP-FN.
      *
           STRING      V285-CTTLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN2
           STRING      HTML-FROM-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DP-FN. EXIT.
      *N35DR.    NOTE *BUILD OWNERSHIP LINE3              *.
       F35DR.    IF    V285-CTTLN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DR-FN.
      *
           STRING      V285-CTTLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN3
           STRING      HTML-FROM-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DR-FN. EXIT.
      *N35DU.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F35DU.    IF    V285-CTTBO1 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DU-FN.
      *
           STRING      V285-CTTBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
           STRING      HTML-FROM-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DU-FN. EXIT.
      *N35DW.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F35DW.    IF    V285-CTTBO2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DW-FN.
      *
           STRING      V285-CTTBO2
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
       F40.                                                             lv05
      *AND RMD
      *N40BN.    NOTE *FORMAT WITHHOLDING PERCENT         *.
       F40BN.    IF    V285-CALTN = 'DCAP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F40BN-FN.
      *AND RMD
                 IF    V285-TWITH = SPACES                              DOT
           MOVE        V285-PWHLD TO WS00-PWHLD
           STRING      WS00-PWHLD '%'
           DELIMITED BY SIZE
           INTO HTML-PWHLD
                 ELSE
           MOVE        'Do Not Withhold' TO HTML-PWHLD.
                 IF    V285-IRMND = 'Y'                                 DOT
           MOVE        'YES' TO HTML-IDRMD
                 ELSE
           MOVE        'NO ' TO HTML-IDRMD.
       F40BN-FN. EXIT.
      *N40BP.    NOTE *FORMAT WITHHOLDING PERCENT         *.
       F40BP.    IF    V285-CALTN = 'APCP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F40BP-FN.
                 IF    V285-TWITH = SPACES                              DOT
           MOVE        V285-PWHLD TO WS00-PWHLD
           STRING      WS00-PWHLD '%'
           DELIMITED BY SIZE
           INTO HTML-PWHLD1
                 ELSE
           MOVE        'Do Not Withhold' TO HTML-PWHLD1.
       F40BP-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *POPULATING VALUES FOR DCAP         *
      *               *                                   *
      *               *************************************.
       F45.      IF    V285-CALTN = 'DCAP'                              lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *N45BC.    NOTE *FOR DESTINATION DETAILS            *.
       F45BC.                                                           lv10
      *
           STRING      HTML-DEST-STEP1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N45BD.    NOTE *FOR ADDRESS OF RECORD INFO         *.
       F45BD.    IF    V285-IAIDE = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45BD-FN.
                 IF    V285-CTTYPG = 'SMTC'                             DOT
           MOVE        DEST-DESC-TEXT (1) TO
           HTML-DEST.
                 IF    V285-CTTYPG = 'SMEM'                             DOT
           MOVE        DEST-DESC-TEXT (2) TO
           HTML-DEST.
           STRING      HTML-DEST-TEXT                                   DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V285-ACOTD NUMERIC                               DOT
                 AND   V285-ACOTD > ZEROES
           MOVE        V285-ACOTD TO WS00-ACOTD.
           MOVE        WS00-ACOTD TO HTML-ACOTD                         DOT
           STRING      HTML-DEST-ADDRESS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BD-FN. EXIT.
      *N45BE.    NOTE *FOR MMTA DETAILS                   *.
       F45BE.    IF    V285-NROWT > 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F45BE-FN.
      *N45BF.    NOTE *LOOP THE OTHER ACCOUNT INFO        *.
       F45BF.                                                           lv20
           MOVE        1                        TO J45BFR
                                    GO TO     F45BF-B.
       F45BF-A.
           ADD         1                        TO J45BFR.
       F45BF-B.
           IF          J45BFR                   >  V285-NROWT
                                    GO TO     F45BF-FN.
           MOVE        DEST-DESC-TEXT (3) TO
           HTML-DEST
           STRING      HTML-DEST-TEXT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
      *
                 IF    V285-CIRAP (J45BFR) = 'NA'                       DOT
           MOVE        WL00-CIRAP1 TO HTML-CIRAP.
                 IF    V285-CIRAP (J45BFR) = 'CU'                       DOT
           MOVE        WL00-CIRAP2 TO HTML-CIRAP.
                 IF    V285-CIRAP (J45BFR) = 'RO'                       DOT
           MOVE        WL00-CIRAP3 TO HTML-CIRAP.
                 IF    V285-CIRAP (J45BFR) = 'PR'                       DOT
           MOVE        WL00-CIRAP4 TO HTML-CIRAP.
                 IF    V285-CIRAP (J45BFR) = 'SC'                       DOT
           MOVE        WL00-CIRAP5 TO HTML-CIRAP.
      *                                                                 DOT
           MOVE        V285-MPMTT (J45BFR) TO HTML-MPMTT
           MOVE        V285-CTID01 (J45BFR) (12:16) TO
           WS00-NCTIDE
           MOVE        V285-CTID01 (J45BFR) (1:3) TO
           WS00-CTIDA
           MOVE        V285-CTID01 (J45BFR) (4:24) TO
           7-CTIDN
           PERFORM     F95CD THRU F95CD-FN
           MOVE        WE00-GERTC TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID01.
                 IF    V285-ADBCRQ (J45BFR) NUMERIC                     DOT
                 AND   V285-ADBCRQ (J45BFR) > ZEROES
           MOVE        V285-ADBCRQ (J45BFR) TO WS00-ADBCRQ.
           MOVE        WS00-ADBCRQ TO HTML-ADBCRQ                       DOT
           STRING      HTML-DEST-OTHERACCT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BF-900. GO TO F45BF-A.
       F45BF-FN. EXIT.
       F45BE-FN. EXIT.
      *N45BH.    NOTE *FOR BANK ACCOUNT DETAILS           *.
       F45BH.    IF    V285-NROWR > ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F45BH-FN.
      *N45BJ.    NOTE *LOOP THE BANK ACCOUNT INFO         *.
       F45BJ.                                                           lv20
           MOVE        1                        TO J45BJR
                                    GO TO     F45BJ-B.
       F45BJ-A.
           ADD         1                        TO J45BJR.
       F45BJ-B.
           IF          J45BJR                   >  V285-NROWR
                                    GO TO     F45BJ-FN.
                 IF    V285-CTTYPI (J45BJR) = 'SMDD'                    DOT
           MOVE        DEST-DESC-TEXT (4) TO
           HTML-DEST.
                 IF    V285-CTTYPI (J45BJR) = 'SMWT'                    DOT
           MOVE        DEST-DESC-TEXT (5) TO
           HTML-DEST.
           STRING      HTML-DEST-TEXT                                   DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           MOVE        V285-CLORN (J45BJR) TO HTML-CLORN
           MOVE        V285-NPBN (J45BJR) TO HTML-NPBN
           MOVE        V285-NTR (J45BJR) TO WS00-NTR
           MOVE        V285-GECKD (J45BJR) TO WS00-GECKD
           MOVE        WS00-RTN TO HTML-NTR
           MOVE        V285-TTBAL (J45BJR) TO HTML-TTBAL
           MOVE        V285-MCSIG (J45BJR) TO HTML-MCSIG.
                 IF    V285-ADBRQ (J45BJR) NUMERIC                      DOT
                 AND   V285-ADBRQ (J45BJR) > ZEROES
           MOVE        V285-ADBRQ (J45BJR) TO WS00-ADBRQ.
           MOVE        WS00-ADBRQ TO HTML-ADBRQ                         DOT
           STRING      HTML-DEST-BANK
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BJ-900. GO TO F45BJ-A.
       F45BJ-FN. EXIT.
       F45BH-FN. EXIT.
      *N45BK.    NOTE *POPULATE VALUE FOR WIRE TRANSFER   *.
       F45BK.    IF    V285-IWTRI = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45BK-FN.
      *INSTRUCTION TRANSACTION DETAILS
           MOVE        DEST-DESC-TEXT (6) TO
           HTML-DEST
           STRING      HTML-DEST-TEXT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V285-ADBRF1 NUMERIC                              DOT
                 AND   V285-ADBRF1 > ZEROES
           MOVE        V285-ADBRF1 TO WS00-ADBRF1
           MOVE        WS00-ADBRF1 TO HTML-ADBRF1.
                 IF    V285-NWIRE > 0                                   DOT
           MOVE        V285-NWIRE TO HTML-NWIRE
                 ELSE
           MOVE        SPACES TO HTML-NWIRE.
           STRING      HTML-DEST-WIRE-INSTR                             DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BK-FN. EXIT.
      *N45BM.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F45BM.                                                           lv15
           STRING      HTML-LAST-TEXT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-DEST-DETAIL-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BM-FN. EXIT.
       F45BC-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *POPULATING VALUES FOR APCP         *
      *               *                                   *
      *               *************************************.
       F50.      IF    V285-CALTN = 'APCP'                              lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BC.    NOTE *FOR TRANSACTION DETAILS            *.
       F50BC.                                                           lv10
                 IF    V285-CTIDA = 021                                 DOT
           STRING      HTML-DEST-STEP1-APCP1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V285-CTIDA = 133                                 DOT
           STRING      HTML-DEST-STEP1-APCP133
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V285-IADYP = 'Y'                                 DOT
                 AND   V285-CTID02 NOT = SPACES
      *STRING THE RECEIVING ACCOUNT
           STRING      HTML-DEST-STEP1-APCP2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V285-IADYP = 'Y'                                 DOT
                 AND   V285-CQACT > 0
                 AND   V285-CTID02 NOT = SPACES
      *STRING THE WITHHOLDING
           STRING      HTML-DEST-STEP1-APCP21
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *STRING THE ACCOUNT SUITABILITY                                   DOT
           STRING      HTML-DEST-STEP1-APCP3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BC-FN. EXIT.
       F50-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *BUILD INFO MSGS AND COMPLIANCE     *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
      *QUESTIONS FOR VERIFY/CONFIRM
      *N65BB.    NOTE *FOR VERIFY/CONFIRM PAGE            *.
       F65BB.                                                           lv10
           STRING      HTML-COMPLIANCE-STEP2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N65CB.    NOTE *FOR DCAP TRANSACTION               *.
       F65CB.    IF    V285-CALTN = 'DCAP'                              lv15
                 NEXT SENTENCE ELSE GO TO     F65CB-FN.
           STRING      HTML-IMP-STEP3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V285-NROWT > 0                                   DOT
           STRING      HTML-DCAP-MMTA-IMP
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-IMP-STEP3-LAST                              DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65CB-FN. EXIT.
      *N65DB.    NOTE *FOR APCP TRANSACTION               *.
       F65DB.    IF    V285-CALTN = 'APCP'                              lv15
                 NEXT SENTENCE ELSE GO TO     F65DB-FN.
           STRING      HTML-IMP-STEP3-APCP
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65DB-FN. EXIT.
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
           MOVE        01 TO HT93-CHTML (TALLI)
           MOVE        HTML-TEXT TO HT93-THTML (TALLI)
           ADD         1 TO TALLI.
       F70BG-FN. EXIT.
       F70BD-900. GO TO F70BD-A.
       F70BD-FN. EXIT.
       F70BB-FN. EXIT.
      *N70EB.    NOTE *MOVE END OF FILE MARKER            *.
       F70EB.                                                           lv10
      *
           MOVE        99 TO HT93-CHTML (TALLI)
           MOVE        SPACES TO HT93-THTML (TALLI).
      *N70EF.    NOTE *CALCULATE THTML LENGTH             *.
       F70EF.                                                           lv15
      *
           COMPUTE     HT93-QBLCK = TALLI.
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
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BB.    NOTE *GENERATE SOURCE DOC ID             *.            ANIOBJ
       F92BB.         EXIT.                                             lv10
      *N92CB.    NOTE *UNFORMAT TS                        *.            ANIOBJ
       F92CB.                                                           lv15
           MOVE        V285-DXTMS2 TO 7-DB2-DXTMSA                      ANIOBJ
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
       F92-FN.   EXIT.
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
                 IF    V285-IVEUP = 'V'                                 DOT
      ******************************
           STRING      'STYLE="MARGIN-LEFT:5PX;'
           'MARGIN-RIGHT:35PX;"'
           DELIMITED BY SIZE
           INTO HTML-TAG1.
                 IF    V285-IVEUP = 'S'                                 DOT
      ******************************
           MOVE        '</TD></TR></TABLE>' TO
           HTML-TABLE-BOTTOM.
       F95DM-FN. EXIT.
      *N95DP.    NOTE *POPULATE STEP TITLE                *.
       F95DP.                                                           lv10
                 IF    V285-IVEUP = 'V'                                 DOT
      ******************************
           MOVE        TITLE-STEP-TEXT (1) TO
           HTML-STEP1T
           HTML-STEP1TA
           HTML-STEP1TB
           MOVE        TITLE-STEP-TEXT (2) TO
           HTML-STEP2T.
                 IF    V285-IVEUP = 'S'                                 DOT
      ******************************
           MOVE        TITLE-STEP-TEXT (3) TO
           HTML-STEP1T
           HTML-STEP1TA
           HTML-STEP1TB
           MOVE        TITLE-STEP-TEXT (4) TO
           HTML-STEP2T.
       F95DP-FN. EXIT.
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
