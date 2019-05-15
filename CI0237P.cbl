       IDENTIFICATION DIVISION.                                         CI0237
       PROGRAM-ID.  CI0237P.                                            CI0237
      *AUTHOR.         BUILD IAT/IDPA/ISMA HTML PAGE.                   CI0237
      *DATE-COMPILED.   09/08/14.                                       CI0237
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
       ENVIRONMENT DIVISION.                                            CI0237
       CONFIGURATION SECTION.                                           CI0237
       SOURCE-COMPUTER. IBM-370.                                        CI0237
       OBJECT-COMPUTER. IBM-370.                                        CI0237
       DATA DIVISION.                                                   CI0237
       WORKING-STORAGE SECTION.                                         CI0237
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
                        PICTURE 99B99B9999.                             CI0237
      *!WS
           05  7-DE00-DNACG
                        PICTURE 99B99B9999.                             CI0237
      *!WS
           05  7-DE00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0237
      *!WS
           05  7-DE00-GEEND
                        PICTURE 99B99B9999.                             CI0237
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0237
            10            I93B-CEADC  PICTURE  X                        CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-DACTT  PICTURE  X(10)                    CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-GEOPDC PICTURE  X(8)                     CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-GEOPDB PICTURE  X(8)                     CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-CAEMCE PICTURE  X(8)                     CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-CAEMCD PICTURE  X(8)                     CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-GETIMM PICTURE  X(8)                     CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0237
                          VALUE                ZERO                     CI0237
                          COMPUTATIONAL-3.                              CI0237
            10            I93B-GERTC  PICTURE  X                        CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-DXTMST PICTURE  X(26)                    CI0237
                          VALUE                SPACE.                   CI0237
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0237
                          VALUE                SPACE.                   CI0237
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
                        PICTURE X(66)                                   CI0237
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      *MISCELLANEOUS FIELDS
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0237
       01  WS-GETIMN REDEFINES WS-GETIMM.
           05 WS-TIME1-HH   PIC 99.
           05 FILLER        PIC X.
           05 WS-TIME1-MM   PIC 99.
           05 FILLER        PIC X.
           05 WS-TIME1-SS   PIC 99.
       01  WS-GETIMT.
           05 WS-TIME2-HH   PIC X(2).
           05 WS-TIME2-MM   PIC X(2).
           05 WS-TIME2-SS   PIC X(2).
       01 WS00-WORKAREA.
           05 WS00-CTID.
      *!WS
              10 WS00-NCTIDE          VALUE SPACES
                        PICTURE 9999B9999B9999B9999.                    CI0237
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

          05 WS00-ADBRQ      PIC $$$,$$$,$$$,$$$.99.
          05 WS00-PERCENT.
              10 WS00-PSUBA  PIC ZZZ.9.
              10 FILLER      PIC X    VALUE '%'.
          05 WS01-PALLO  PIC ZZZ.9.
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
       01   DEBUT-WSS.                                                  CI0237
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0237
            05   IK     PICTURE X.                                      CI0237
       01  CONSTANTES-PAC.                                              CI0237
           05  FILLER  PICTURE X(87)   VALUE                            CI0237
                     '6015 CAT09/08/14CI0237ADMIN   14:35:06CI0237P AMERCI0237
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0237
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0237
           05  NUGNA   PICTURE X(5).                                    CI0237
           05  APPLI   PICTURE X(3).                                    CI0237
           05  DATGN   PICTURE X(8).                                    CI0237
           05  PROGR   PICTURE X(6).                                    CI0237
           05  CODUTI  PICTURE X(8).                                    CI0237
           05  TIMGN   PICTURE X(8).                                    CI0237
           05  PROGE   PICTURE X(8).                                    CI0237
           05  COBASE  PICTURE X(4).                                    CI0237
           05  DATGNC  PICTURE X(10).                                   CI0237
           05  RELEAS  PICTURE X(7).                                    CI0237
           05  DATGE   PICTURE X(10).                                   CI0237
           05  DATSQ   PICTURE X(10).                                   CI0237
       01  DATCE.                                                       CI0237
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0237
         05  DATOR.                                                     CI0237
           10  DATOA  PICTURE XX.                                       CI0237
           10  DATOM  PICTURE XX.                                       CI0237
           10  DATOJ  PICTURE XX.                                       CI0237
       01  DAT6.                                                        CI0237
            10 DAT61.                                                   CI0237
            15 DAT619  PICTURE 99.                                      CI0237
            10 DAT62.                                                   CI0237
            15 DAT629  PICTURE 99.                                      CI0237
            10 DAT63   PICTURE XX.                                      CI0237
       01  DAT8.                                                        CI0237
            10 DAT81   PICTURE XX.                                      CI0237
            10 DAT8S1  PICTURE X.                                       CI0237
            10 DAT82   PICTURE XX.                                      CI0237
            10 DAT8S2  PICTURE X.                                       CI0237
            10 DAT83   PICTURE XX.                                      CI0237
       01  DAT8E    REDEFINES    DAT8.                                  CI0237
            10 DAT81E  PICTURE X(4).                                    CI0237
            10 DAT82E  PICTURE XX.                                      CI0237
            10 DAT83E  PICTURE XX.                                      CI0237
       01  DAT6C.                                                       CI0237
            10  DAT61C PICTURE XX.                                      CI0237
            10  DAT62C PICTURE XX.                                      CI0237
            10  DAT63C.                                                 CI0237
             15 DAT63CC PICTURE XX.                                     CI0237
             15 DAT64C  PICTURE XX.                                     CI0237
       01  DAT8C.                                                       CI0237
            10  DAT81C  PICTURE XX.                                     CI0237
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0237
            10  DAT82C  PICTURE XX.                                     CI0237
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0237
            10  DAT83C.                                                 CI0237
             15 DAT83CC PICTURE XX.                                     CI0237
             15 DAT84C  PICTURE XX.                                     CI0237
       01  DATSEP     PICTURE X VALUE '/'.                              CI0237
       01  DATSEW     PICTURE X.                                        CI0237
       01   VARIABLES-CONDITIONNELLES.                                  CI0237
            05                  FT      PICTURE X VALUE '0'.            CI0237
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0237
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0237
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J70BDR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0237
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT.                            *
      *THIS COPYBOOK CONTAINS THE HEADER, SOURCE ACCOUNT DETAILS AND   *
      *DESTINATION ACCOUNT DETAILS FOR THE HTML PAGE.                  *
      ******************************************************************
       COPY CI0237C1.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0237                *
      ******************************************************************
      *!WF DSP=V2 DSL=V2 SEL=79 FOR=I LEV=1 PLT=80
       01                 V200.                                         CI0237
          05              V200-SUITE.                                   CI0237
            15       FILLER         PICTURE  X(00793).                  CI0237
       01                 V279  REDEFINES      V200.                    CI0237
            10            V279-CTID.                                    CI0237
            11            V279-CTIDA  PICTURE  9(3).                    CI0237
            11            V279-CTIDN.                                   CI0237
            12            V279-CTIDNP PICTURE  X(13).                   CI0237
            12            V279-CTIDND PICTURE  9(11).                   CI0237
            10            V279-GECKD2 PICTURE  9.                       CI0237
            10            V279-CLID   PICTURE  X(23).                   CI0237
            10            V279-CLTIN  PICTURE  9(12).                   CI0237
            10            V279-IVEUP  PICTURE  X.                       CI0237
            10            V279-PRCOD  PICTURE  9(5).                    CI0237
            10            V279-CTTLN1 PICTURE  X(30).                   CI0237
            10            V279-CTTLN2 PICTURE  X(30).                   CI0237
            10            V279-CTTLN3 PICTURE  X(30).                   CI0237
            10            V279-CTTBO1 PICTURE  X(45).                   CI0237
            10            V279-CTTBO2 PICTURE  X(45).                   CI0237
            10            V279-CSPRDN PICTURE  X(30).                   CI0237
            10            V279-GEOPD2 PICTURE  X(8).                    CI0237
            10            V279-CQACTL PICTURE  X(45).                   CI0237
            10            V279-DAEDTO PICTURE  X(8).                    CI0237
            10            V279-DPTIM  PICTURE  X(8).                    CI0237
            10            V279-CSLCT  PICTURE  X.                       CI0237
            10            V279-NGEOPA PICTURE  X(08).                   CI0237
            10            V279-NGEOR  PICTURE  9(08).                   CI0237
            10            V279-CCLPR  PICTURE  X.                       CI0237
            10            V279-CCLCH  PICTURE  X.                       CI0237
            10            V279-CCLSU  PICTURE  X.                       CI0237
            10            V279-CTTYPG PICTURE  X(04).                   CI0237
            10            V279-DCACG  PICTURE  9(8).                    CI0237
            10            V279-DCACD  PICTURE  X(10).                   CI0237
            10            V279-DNACG  PICTURE  9(8).                    CI0237
            10            V279-CCONF  PICTURE  X(25).                   CI0237
            10            V279-ATROLL PICTURE  X(25).                   CI0237
            10            V279-DXTMS2 PICTURE  X(26).                   CI0237
            10            V279-MPLNR2 PICTURE  X(40).                   CI0237
            10            V279-ADBRQ  PICTURE  S9(11)V99                CI0237
                          COMPUTATIONAL-3.                              CI0237
            10            V279-PVFPA1 PICTURE  S9(3)V9(1)               CI0237
                          COMPUTATIONAL-3.                              CI0237
            10            V279-MFDNMS PICTURE  X(30).                   CI0237
            10            V279-PVFPA2 PICTURE  S9(3)V9(1)               CI0237
                          COMPUTATIONAL-3.                              CI0237
            10            V279-MFDNMH PICTURE  X(30).                   CI0237
            10            V279-PVFPA3 PICTURE  S9(3)V9(1)               CI0237
                          COMPUTATIONAL-3.                              CI0237
            10            V279-PVFPA4 PICTURE  S9(3)V9(1)               CI0237
                          COMPUTATIONAL-3.                              CI0237
            10            V279-FILLER PICTURE  X(200).                  CI0237
      ******************************************************************
      ** HTML RETURNED TO CALLING RPC IN 200 BYTE CHUNKS               *
      ******************************************************************
      *
      *!WF DSP=HT DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HT00.                                         CI0237
          05              HT00-SUITE.                                   CI0237
            15       FILLER         PICTURE  X(90906).                  CI0237
       01                 HT93  REDEFINES      HT00.                    CI0237
            10            HT93-QBLCK  PICTURE  9(6).                    CI0237
            10            HT93-QT9O.                                    CI0237
            11            HT93-QT9B                                     CI0237
                          OCCURS       450     TIMES.                   CI0237
            12            HT93-CHTML  PICTURE  99.                      CI0237
            12            HT93-THTML  PICTURE  X(200).                  CI0237
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0237
          05              MS00-SUITE.                                   CI0237
            15       FILLER         PICTURE  X(00542).                  CI0237
       01                 MS03  REDEFINES      MS00.                    CI0237
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0237
                          COMPUTATIONAL-3.                              CI0237
            10            MS03-CMSSF  PICTURE  XX.                      CI0237
            10            MS03-DU09.                                    CI0237
            11            MS03-CMESA  PICTURE  S9(9)                    CI0237
                          BINARY.                                       CI0237
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0237
                          BINARY.                                       CI0237
            11            MS03-CMESB  PICTURE  S9(9)                    CI0237
                          BINARY.                                       CI0237
            11            MS03-CMSST  PICTURE  S9(9)                    CI0237
                          BINARY.                                       CI0237
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0237
                          BINARY.                                       CI0237
            11            MS03-QELLAA PICTURE  S9(9)                    CI0237
                          BINARY.                                       CI0237
            11            MS03-TMESS4 PICTURE  X(512).                  CI0237
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0237
            10            MX11-QMSGS  PICTURE  9(03).                   CI0237
            10            MX11-PJ09                                     CI0237
                          OCCURS       025     TIMES.                   CI0237
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0237
                          COMPUTATIONAL-3.                              CI0237
            11            MX11-CMESB  PICTURE  S9(9)                    CI0237
                          BINARY.                                       CI0237
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V279
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
      *N01.      NOTE *************************************.            CI0237
      *               *                                   *             CI0237
      *               *INITIALISATIONS                    *             CI0237
      *               *                                   *             CI0237
      *               *************************************.            CI0237
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0237
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0237
      *               *                                   *             CI0237
      *               *FIN DE TRAITEMENT                  *             CI0237
      *               *                                   *             CI0237
      *               *************************************.            CI0237
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0237
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
      *N35BB.    NOTE *FORMAT COMMON ACCOUNT DETAILS      *.
       F35BB.                                                           lv10
      *AND COMPLIANCE QUESTIONS.
           MOVE        V279-CTIDND TO WS00-NCTIDE
           MOVE        V279-GECKD2 TO WS00-GECKD1
           MOVE        V279-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-CTID
           MOVE        V279-CSPRDN TO HTML-PRCMN
           MOVE        V279-CTTLN1 TO HTML-CTTLN1
           MOVE        V279-CTTLN2 TO HTML-CTTLN2
           MOVE        V279-CTTLN3 TO HTML-CTTLN3
           MOVE        V279-CQACTL TO HTML-CQACTL
           MOVE        V279-CTTBO1 TO HTML-CTTBO1
           MOVE        V279-CTTBO2 TO HTML-CTTBO2.
                 IF    V279-CCLPR = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLPR.
                 IF    V279-CCLPR = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLPR.
                 IF    V279-CCLPR = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLPR.
                 IF    V279-CCLCH = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLCH.
                 IF    V279-CCLCH = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLCH.
                 IF    V279-CCLCH = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLCH.
                 IF    V279-CCLSU = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLSU.
                 IF    V279-CCLSU = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLSU.
                 IF    V279-CCLSU = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLSU.
       F35BB-FN. EXIT.
      *N35BC.    NOTE *FORMAT ORDER TICKET INFO           *.
       F35BC.    IF    V279-IVEUP = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35BC-FN.
      *FOR SUBMISSION PAGE ONLY
                 IF    V279-CSLCT = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CSLCT
           MOVE        V279-NGEOR TO HTML-NGEOR
                 ELSE
           MOVE        'NO ' TO HTML-CSLCT
           MOVE        'N/A' TO HTML-NGEOR.
           MOVE        V279-GEOPD2 TO HTML-GEOPD2                       DOT
           MOVE        V279-MPLNR2 TO HTML-MPLNR2
           MOVE        V279-ATROLL TO HTML-ATROLL
           MOVE        V279-CCONF TO HTML-CCONF
           MOVE        V279-NGEOPA TO HTML-NGEOPA.
      *N35BD.    NOTE *FORMAT THE CURRENT DATE,           *.
       F35BD.                                                           lv15
      *EFFECTIVE DATE AND SEND TRANS
      *RECEIVED TIME
      *
           STRING      '000'
           V279-DCACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT
           INITIALIZE  I93B
           MOVE        V279-DCACG TO 7-DE00-DCACG
           STRING      V279-DCACG (1:4) '/'
           V279-DCACG (5:2) '/'
           V279-DCACG (7:2) '/'
           DELIMITED BY SIZE INTO
           I93B-DACTT
      *I93B
           MOVE        V279-DCACD TO I93B-DACTT
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
           MOVE        WS00-DATCE                                       CI0237
           TO DAT8E DAT6C                                               CI0237
           MOVE DAT81E TO DAT63C                                        CI0237
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0237
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0237
      *!ADM "7-DE00-DCACG     HTML-DCACG"
           MOVE        7-DE00-DCACG                                     CI0237
           TO DAT8E DAT6C                                               CI0237
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0237
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0237
           MOVE   DAT8C TO  HTML-DCACG.                                 CI0237
                 IF    (WS00-TIMER >= WS-GETIMT                         DOT
                 AND   V279-DCACG <= WS00-DATCE)
      *NEXT ACTG DATE IF AFTER MKT CLS
           STRING      '000' V279-DNACG (7:2)
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
      *!ADM "V279-DAEDTO     HTML-DRECD"
           MOVE        V279-DAEDTO                                      CI0237
           TO DAT8E DAT6C                                               CI0237
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0237
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0237
           MOVE   DAT8C TO  HTML-DRECD                                  CI0237
      *
           STRING      V279-DPTIM DELIMITED BY SIZE
           ' CENTRAL TIME'
           DELIMITED BY SIZE
           INTO HTML-TIMER1.
       F35BE-FN. EXIT.
      *N35BM.    NOTE *STRING CONFIRMATION INFORMATION    *.
       F35BM.                                                           lv15
      *
           STRING      HTML-CONFIRM-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BM-FN. EXIT.
       F35BC-FN. EXIT.
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
       F35DP.    IF    V279-CTTLN2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DP-FN.
      *
           STRING      V279-CTTLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN2
           STRING      HTML-FROM-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DP-FN. EXIT.
      *N35DR.    NOTE *BUILD OWNERSHIP LINE3              *.
       F35DR.    IF    V279-CTTLN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DR-FN.
      *
           STRING      V279-CTTLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN3
           STRING      HTML-FROM-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DR-FN. EXIT.
      *N35DU.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F35DU.    IF    V279-CTTBO1 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DU-FN.
      *
           STRING      V279-CTTBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
           STRING      HTML-FROM-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DU-FN. EXIT.
      *N35DW.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F35DW.    IF    V279-CTTBO2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DW-FN.
      *
           STRING      V279-CTTBO2
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
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *POPULATING VALUES FOR IAT TRAN     *
      *               *                                   *
      *               *************************************.
       F50.      IF    V279-CTTYPG = 'IAT'                              lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BC.    NOTE *FOR STEP1 TITLE                    *.
       F50BC.                                                           lv10
      *
           STRING      HTML-STEP1-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BC-FN. EXIT.
      *N50CD.    NOTE *FOR TRANSACTION DETAILS            *.
       F50CD.                                                           lv10
                 IF    V279-ADBRQ > ZEROES                              DOT
           MOVE        V279-ADBRQ TO WS00-ADBRQ
           MOVE        WS00-ADBRQ TO HTML-VALUE.
                 IF    V279-PVFPA1 > ZEROES                             DOT
           MOVE        V279-PVFPA1 TO WS00-PSUBA
           MOVE        WS00-PERCENT TO HTML-VALUE.
           STRING      HTML-TRAN-DETAIL-IAT                             DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CD-FN. EXIT.
      *N50EE.    NOTE *FOR COMPLIANCE QUESTIONS           *.
       F50EE.                                                           lv10
           STRING      HTML-COMPLIANCE-STEP2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50EE-FN. EXIT.
      *N50FA.    NOTE *FOR STEP3 TITLE                    *.
       F50FA.                                                           lv10
           MOVE        'Read Important Messages' TO
           HTML-STEP3TT
           STRING      HTML-STEP3-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50FA-FN. EXIT.
      *N50FF.    NOTE *FOR IMPORTANT MESSAGES             *.
       F50FF.                                                           lv10
           STRING      HTML-IMP-IAT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50FF-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *POPULATING VALUES FOR IDPA TRAN    *
      *               *                                   *
      *               *************************************.
       F55.      IF    V279-CTTYPG = 'IDPA'                             lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55BC.    NOTE *FOR TRANSACTION DETAILS            *.
       F55BC.                                                           lv10
      *
           STRING      HTML-STEP1-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55BC-FN. EXIT.
      *N55CD.    NOTE *FOR TRANSACTION DETAILS            *.
       F55CD.                                                           lv10
           STRING      HTML-DETAIL-IDPA1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V279-PVFPA2 > ZEROES                             DOT
           MOVE        V279-PVFPA2 TO WS00-PSUBA
           MOVE        WS00-PSUBA TO HTML-PVFPA
           MOVE        V279-MFDNMS TO HTML-MFDNM
           STRING      HTML-TR-SUBACC-IDPA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V279-PVFPA3 > ZEROES                             DOT
           MOVE        V279-PVFPA3 TO WS00-PSUBA
           MOVE        WS00-PSUBA TO HTML-PVFPA
           MOVE        V279-MFDNMH TO HTML-MFDNM
           STRING      HTML-TR-SUBACC-IDPA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-DETAIL-IDPA1-END                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55CD-FN. EXIT.
      *N55EE.    NOTE *FOR COMPLIANCE QUESTIONS           *.
       F55EE.                                                           lv10
           STRING      HTML-COMPLIANCE-STEP2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55EE-FN. EXIT.
      *N55FA.    NOTE *FOR STEP3 TITLE                    *.
       F55FA.                                                           lv10
           MOVE        'Read Important Messages' TO
           HTML-STEP3TT
           STRING      HTML-STEP3-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55FA-FN. EXIT.
      *N55FF.    NOTE *FOR IMPORTANT MESSAGES             *.
       F55FF.                                                           lv10
           STRING      HTML-IMP-IDPA-ISMA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55FF-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *POPULATING VALUES FOR ISMA TRAN    *
      *               *                                   *
      *               *************************************.
       F60.      IF    V279-CTTYPG = 'ISMA'                             lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *N60BC.    NOTE *FOR TRANSACTION DETAILS            *.
       F60BC.                                                           lv10
      *
           STRING      HTML-STEP1-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N60CH.    NOTE *FOR CURRENT MATURITY ALLOCATION    *.
       F60CH.                                                           lv15
           STRING      HTML-DETAIL-ISMA1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           INITIALIZE  WS00-PSUBA WS01-PALLO
           MOVE        V279-PVFPA2 TO WS00-PSUBA
           MOVE        V279-PVFPA3 TO WS01-PALLO
      *INDEXED ACCOUNT
           MOVE        V279-MFDNMH TO HTML-MFDNM1
           MOVE        WS01-PALLO TO HTML-PVFPA1
           STRING      HTML-SUBACC-ISMA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
      *FIXED ACCOUNT
           MOVE        V279-MFDNMS TO HTML-MFDNM1
           MOVE        WS00-PSUBA TO HTML-PVFPA1
           STRING      HTML-SUBACC-ISMA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60CH-FN. EXIT.
      *N60CM.    NOTE *FOR NEW MATURITY ALLOCATION        *.
       F60CM.                                                           lv15
           STRING      HTML-DETAIL-ISMA2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           INITIALIZE  WS00-PSUBA WS01-PALLO
           MOVE        V279-PVFPA4 TO WS00-PSUBA
           COMPUTE     WS01-PALLO = 100 - V279-PVFPA4
      *INDEXED ACCOUNT
           MOVE        V279-MFDNMH TO HTML-MFDNM1
           MOVE        WS00-PSUBA TO HTML-PVFPA1
           STRING      HTML-SUBACC-ISMA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
      *FIXED ACCOUNT
           MOVE        V279-MFDNMS TO HTML-MFDNM1
           MOVE        WS01-PALLO TO HTML-PVFPA1
           STRING      HTML-SUBACC-ISMA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-DETAIL-ISMA-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60CM-FN. EXIT.
       F60BC-FN. EXIT.
      *N60EE.    NOTE *FOR COMPLIANCE QUESTIONS           *.
       F60EE.                                                           lv10
           STRING      HTML-COMPLIANCE-STEP2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60EE-FN. EXIT.
      *N60FA.    NOTE *FOR STEP3 TITLE                    *.
       F60FA.                                                           lv10
           MOVE        'Read Important Messages' TO
           HTML-STEP3TT
           STRING      HTML-STEP3-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60FA-FN. EXIT.
      *N60FF.    NOTE *FOR IMPORTANT MESSAGES             *.
       F60FF.                                                           lv10
           STRING      HTML-IMP-IDPA-ISMA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60FF-FN. EXIT.
       F60-FN.   EXIT.
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
      *N95DM.    NOTE *POPULATE STYLE SHEET TAGS          *.
       F95DM.                                                           lv10
                 IF    V279-IVEUP = 'V'                                 DOT
      ******************************
           STRING      'STYLE="MARGIN-LEFT:5PX;'
           'MARGIN-RIGHT:35PX;"'
           DELIMITED BY SIZE
           INTO HTML-TAG1.
                 IF    V279-IVEUP = 'S'                                 DOT
      ******************************
           MOVE        '</TD></TR></TABLE>' TO
           HTML-TABLE-BOTTOM.
       F95DM-FN. EXIT.
      *N95DP.    NOTE *POPULATE TITLE LINES               *.
       F95DP.                                                           lv10
                 IF    V279-IVEUP = 'V'                                 DOT
      ******************************
           MOVE        TITLE-STEP-TEXT (1) TO
           HTML-STEP1T
           MOVE        TITLE-STEP-TEXT (2) TO
           HTML-STEP2T.
                 IF    V279-IVEUP = 'S'                                 DOT
      ******************************
           MOVE        TITLE-STEP-TEXT (3) TO
           HTML-STEP1T
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
