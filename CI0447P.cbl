       IDENTIFICATION DIVISION.                                         CI0447
       PROGRAM-ID.  CI0447P.                                            CI0447
      *AUTHOR.         BUILD HTML PAGE FOR INPS/INLN.                   CI0447
      *DATE-COMPILED.   09/08/14.                                       CI0447
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2012                          *ACOPYP
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
      *     COPR. 2012                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0447
       CONFIGURATION SECTION.                                           CI0447
       SOURCE-COMPUTER. IBM-370.                                        CI0447
       OBJECT-COMPUTER. IBM-370.                                        CI0447
       DATA DIVISION.                                                   CI0447
       WORKING-STORAGE SECTION.                                         CI0447
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
                        PICTURE 99B99B9999.                             CI0447
      *!WS
           05  7-DE00-DNACG
                        PICTURE 99B99B9999.                             CI0447
      *!WS
           05  7-DE00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0447
      *!WS
           05  7-DE00-GEEND
                        PICTURE 99B99B9999.                             CI0447
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0447
            10            I93B-CEADC  PICTURE  X                        CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-DACTT  PICTURE  X(10)                    CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-GEOPDC PICTURE  X(8)                     CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-GEOPDB PICTURE  X(8)                     CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-CAEMCE PICTURE  X(8)                     CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-CAEMCD PICTURE  X(8)                     CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-GETIMM PICTURE  X(8)                     CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0447
                          VALUE                ZERO                     CI0447
                          COMPUTATIONAL-3.                              CI0447
            10            I93B-GERTC  PICTURE  X                        CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-DXTMST PICTURE  X(26)                    CI0447
                          VALUE                SPACE.                   CI0447
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0447
                          VALUE                SPACE.                   CI0447
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

       01 HTML-PT PIC S9(5) VALUE ZEROS.

       01 TEMP-PT PIC S9(5) VALUE ZEROS.
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
                        PICTURE X(66)                                   CI0447
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      *VERIFY AND SUBMISSION STEP TITLES
       01 WC00-VERF-MSG1 PIC X(35)
               VALUE 'Verify Destination Details'.
       01 WC00-VERF-MSG2 PIC X(35)
               VALUE 'Verify Transaction Details'.
       01 WC00-VERF-MSG3 PIC X(35)
               VALUE 'Verify Compliance Questions'.
       01 WC00-SUB-MSG1 PIC X(35)
               VALUE 'Destination Details'.
       01 WC00-SUB-MSG2 PIC X(35)
               VALUE 'Transaction Details'.
       01 WC00-SUB-MSG3 PIC X(35)
               VALUE 'Compliance Questions'.
       01               WE00.                                           $CHECK
      *!WI pl=WE110                                                     $CHECK
         05             WE00-CTIDN                                      $CHECK
                        PICTURE X(24)                                   CI0447
                        JUSTIFIED RIGHT.                                $CHECK
      *!WI pl=WE130                                                     $CHECK
         05             WE00-GERTC                                      $CHECK
                        PICTURE X.                                      CI0447
      *!WI
       01 7-CTIDN VALUE SPACES
                        PICTURE X(24).                                  CI0447
      *TEXT FOR TRANSACTION TYPES
       01    WL00-CDTYN1  PIC X(70)
               VALUE 'Send check to address of record via regular mail'.
       01    WL00-CDTYN2.
             05   WL00-CDTYN21   PIC X(30)
                     VALUE 'Internal Transfer to another '.
             05   WL00-CDTYN22   PIC X(40)
                     VALUE 'Ameriprise Financial Services account'.
       01    WL00-CDTYN3.
             05   WL00-CDTYN31   PIC X(40)
                     VALUE 'Direct Deposit (ACH-Out) to an '.
             05   WL00-CDTYN32   PIC X(30)
                     VALUE 'authorized bank account'.
       01    WL00-CDTYN4  PIC X(70)
               VALUE 'Send check to address of record via express mail'.

      *TEXT FOR IRA TYPES
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

      *SPECIFIC TEXT FOR SMEM
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
                        PICTURE X(8).                                   CI0447
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
                        PICTURE 9999B9999B9999B9999.                    CI0447
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

          05 WS00-MSGTXT.
             10 WS00-MSGNUM PIC Z9.
             10 WS00-DOT    PIC X     VALUE '.'.
             10 FILLER      PIC X     VALUE SPACE.
      *!WI
             10 WS00-TMESSC
                        PICTURE X(254).                                 CI0447

      *!WS
          05 WS00-APMTD
                        PICTURE $$,$$$,$$$,$$9.99.                      CI0447
          05 WS00-PWHLD    PIC ZZ9.99 VALUE ZEROES.
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
       01  WS00-DATCE    PIC X(8) VALUE ZEROES.
       01   DEBUT-WSS.                                                  CI0447
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0447
            05   IK     PICTURE X.                                      CI0447
       01  CONSTANTES-PAC.                                              CI0447
           05  FILLER  PICTURE X(87)   VALUE                            CI0447
                     '6015 CAT09/08/14CI0447ADMIN   14:35:28CI0447P AMERCI0447
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0447
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0447
           05  NUGNA   PICTURE X(5).                                    CI0447
           05  APPLI   PICTURE X(3).                                    CI0447
           05  DATGN   PICTURE X(8).                                    CI0447
           05  PROGR   PICTURE X(6).                                    CI0447
           05  CODUTI  PICTURE X(8).                                    CI0447
           05  TIMGN   PICTURE X(8).                                    CI0447
           05  PROGE   PICTURE X(8).                                    CI0447
           05  COBASE  PICTURE X(4).                                    CI0447
           05  DATGNC  PICTURE X(10).                                   CI0447
           05  RELEAS  PICTURE X(7).                                    CI0447
           05  DATGE   PICTURE X(10).                                   CI0447
           05  DATSQ   PICTURE X(10).                                   CI0447
       01  DATCE.                                                       CI0447
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0447
         05  DATOR.                                                     CI0447
           10  DATOA  PICTURE XX.                                       CI0447
           10  DATOM  PICTURE XX.                                       CI0447
           10  DATOJ  PICTURE XX.                                       CI0447
       01  DAT6.                                                        CI0447
            10 DAT61.                                                   CI0447
            15 DAT619  PICTURE 99.                                      CI0447
            10 DAT62.                                                   CI0447
            15 DAT629  PICTURE 99.                                      CI0447
            10 DAT63   PICTURE XX.                                      CI0447
       01  DAT8.                                                        CI0447
            10 DAT81   PICTURE XX.                                      CI0447
            10 DAT8S1  PICTURE X.                                       CI0447
            10 DAT82   PICTURE XX.                                      CI0447
            10 DAT8S2  PICTURE X.                                       CI0447
            10 DAT83   PICTURE XX.                                      CI0447
       01  DAT8E    REDEFINES    DAT8.                                  CI0447
            10 DAT81E  PICTURE X(4).                                    CI0447
            10 DAT82E  PICTURE XX.                                      CI0447
            10 DAT83E  PICTURE XX.                                      CI0447
       01  DAT6C.                                                       CI0447
            10  DAT61C PICTURE XX.                                      CI0447
            10  DAT62C PICTURE XX.                                      CI0447
            10  DAT63C.                                                 CI0447
             15 DAT63CC PICTURE XX.                                     CI0447
             15 DAT64C  PICTURE XX.                                     CI0447
       01  DAT8C.                                                       CI0447
            10  DAT81C  PICTURE XX.                                     CI0447
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0447
            10  DAT82C  PICTURE XX.                                     CI0447
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0447
            10  DAT83C.                                                 CI0447
             15 DAT83CC PICTURE XX.                                     CI0447
             15 DAT84C  PICTURE XX.                                     CI0447
       01  DATSEP     PICTURE X VALUE '/'.                              CI0447
       01  DATSEW     PICTURE X.                                        CI0447
       01   VARIABLES-CONDITIONNELLES.                                  CI0447
            05                  FT      PICTURE X VALUE '0'.            CI0447
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0447
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0447
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J70BER PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0447
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT.
      ******************************************************************
       COPY CI0447C3.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0447                *
      ******************************************************************
      *!WF DSP=V2 DSL=V2 SEL=61 FOR=I LEV=1 PLT=80
       01                 V200.                                         CI0447
          05              V200-SUITE.                                   CI0447
            15       FILLER         PICTURE  X(01031).                  CI0447
       01                 V261  REDEFINES      V200.                    CI0447
            10            V261-MAPPN  PICTURE  X(10).                   CI0447
            10            V261-NSSSI  PICTURE  X(24).                   CI0447
            10            V261-CTTYPG PICTURE  X(04).                   CI0447
            10            V261-CTID.                                    CI0447
            11            V261-CTIDA  PICTURE  9(3).                    CI0447
            11            V261-CTIDN.                                   CI0447
            12            V261-CTIDNP PICTURE  X(13).                   CI0447
            12            V261-CTIDND PICTURE  9(11).                   CI0447
            10            V261-GECKD2 PICTURE  9.                       CI0447
            10            V261-IVEUP  PICTURE  X.                       CI0447
            10            V261-CTTLN1 PICTURE  X(30).                   CI0447
            10            V261-CTTLN2 PICTURE  X(30).                   CI0447
            10            V261-CTTLN3 PICTURE  X(30).                   CI0447
            10            V261-CTTBO1 PICTURE  X(45).                   CI0447
            10            V261-CTTBO2 PICTURE  X(45).                   CI0447
            10            V261-PRCMN  PICTURE  X(20).                   CI0447
            10            V261-CQACTL PICTURE  X(45).                   CI0447
            10            V261-DAEDTO PICTURE  X(8).                    CI0447
            10            V261-DPTIM  PICTURE  X(8).                    CI0447
            10            V261-CSLCT  PICTURE  X.                       CI0447
            10            V261-NGEOR  PICTURE  9(08).                   CI0447
            10            V261-NGEOPA PICTURE  X(08).                   CI0447
            10            V261-CDTYN  PICTURE  X(04).                   CI0447
            10            V261-CAFEE  PICTURE  X.                       CI0447
            10            V261-CTID01 PICTURE  X(27).                   CI0447
            10            V261-CCARC  PICTURE  X(16).                   CI0447
            10            V261-NPRCD  PICTURE  X(16).                   CI0447
            10            V261-CCEXD  PICTURE  X(7).                    CI0447
            10            V261-CSECU  PICTURE  X(04).                   CI0447
            10            V261-CLNAMA PICTURE  X(30).                   CI0447
            10            V261-IADRS  PICTURE  X.                       CI0447
            10            V261-GESAD1 PICTURE  X(30).                   CI0447
            10            V261-GESAD2 PICTURE  X(30).                   CI0447
            10            V261-CLORN  PICTURE  X(45).                   CI0447
            10            V261-NTR    PICTURE  9(8).                    CI0447
            10            V261-GECKD3 PICTURE  9.                       CI0447
            10            V261-NPBN   PICTURE  X(20).                   CI0447
            10            V261-TTBAL  PICTURE  X(15).                   CI0447
            10            V261-MCSIG  PICTURE  X(30).                   CI0447
            10            V261-CTID02 PICTURE  X(27).                   CI0447
            10            V261-CIRAP  PICTURE  XX.                      CI0447
            10            V261-MPMTT  PICTURE  X(20).                   CI0447
            10            V261-ADBRQ  PICTURE  S9(11)V99                CI0447
                          COMPUTATIONAL-3.                              CI0447
            10            V261-PWHLD  PICTURE  S999V9(5)                CI0447
                          COMPUTATIONAL-3.                              CI0447
            10            V261-TWITH  PICTURE  X(12).                   CI0447
            10            V261-CCLPR  PICTURE  X.                       CI0447
            10            V261-CCLCH  PICTURE  X.                       CI0447
            10            V261-CCLSU  PICTURE  X.                       CI0447
            10            V261-IFNMV  PICTURE  X.                       CI0447
            10            V261-DCACG  PICTURE  9(8).                    CI0447
            10            V261-DNACG  PICTURE  9(8).                    CI0447
            10            V261-DCACD  PICTURE  X(10).                   CI0447
            10            V261-ATROLL PICTURE  X(25).                   CI0447
            10            V261-MPLNR2 PICTURE  X(40).                   CI0447
            10            V261-CCONF  PICTURE  X(25).                   CI0447
            10            V261-GEOPD2 PICTURE  X(8).                    CI0447
            10            V261-IRTLN  PICTURE  X.                       CI0447
            10            V261-ILNTR  PICTURE  X.                       CI0447
            10            V261-FILLER PICTURE  X(198).                  CI0447
      ******************************************************************
      ** HTML RETURNED TO CALLING RPC IN 200 BYTE CHUNKS               *
      ******************************************************************
      *
      *!WF DSP=HT DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HT00.                                         CI0447
          05              HT00-SUITE.                                   CI0447
            15       FILLER         PICTURE  X(90906).                  CI0447
       01                 HT93  REDEFINES      HT00.                    CI0447
            10            HT93-QBLCK  PICTURE  9(6).                    CI0447
            10            HT93-QT9O.                                    CI0447
            11            HT93-QT9B                                     CI0447
                          OCCURS       450     TIMES.                   CI0447
            12            HT93-CHTML  PICTURE  99.                      CI0447
            12            HT93-THTML  PICTURE  X(200).                  CI0447
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0447
          05              MS00-SUITE.                                   CI0447
            15       FILLER         PICTURE  X(00542).                  CI0447
       01                 MS03  REDEFINES      MS00.                    CI0447
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0447
                          COMPUTATIONAL-3.                              CI0447
            10            MS03-CMSSF  PICTURE  XX.                      CI0447
            10            MS03-DU09.                                    CI0447
            11            MS03-CMESA  PICTURE  S9(9)                    CI0447
                          BINARY.                                       CI0447
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0447
                          BINARY.                                       CI0447
            11            MS03-CMESB  PICTURE  S9(9)                    CI0447
                          BINARY.                                       CI0447
            11            MS03-CMSST  PICTURE  S9(9)                    CI0447
                          BINARY.                                       CI0447
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0447
                          BINARY.                                       CI0447
            11            MS03-QELLAA PICTURE  S9(9)                    CI0447
                          BINARY.                                       CI0447
            11            MS03-TMESS4 PICTURE  X(512).                  CI0447
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0447
            10            MX11-QMSGS  PICTURE  9(03).                   CI0447
            10            MX11-PJ09                                     CI0447
                          OCCURS       025     TIMES.                   CI0447
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0447
                          COMPUTATIONAL-3.                              CI0447
            11            MX11-CMESB  PICTURE  S9(9)                    CI0447
                          BINARY.                                       CI0447
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V261
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
      *N01.      NOTE *************************************.            CI0447
      *               *                                   *             CI0447
      *               *INITIALISATIONS                    *             CI0447
      *               *                                   *             CI0447
      *               *************************************.            CI0447
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0447
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0447
      *               *                                   *             CI0447
      *               *FIN DE TRAITEMENT                  *             CI0447
      *               *                                   *             CI0447
      *               *************************************.            CI0447
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0447
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
           MOVE        V261-PRCMN TO HTML-PRCMN.
                 IF    V261-CSLCT = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CSLCT
           MOVE        V261-NGEOR TO HTML-NGEOR.
                 IF    V261-CSLCT = 'N'                                 DOT
           MOVE        'NO ' TO HTML-CSLCT
           MOVE        'N/A' TO HTML-NGEOR.
           MOVE        V261-CTTLN1 TO HTML-CTTLN1                       DOT
           MOVE        V261-CTTLN2 TO HTML-CTTLN2
           MOVE        V261-CTTLN3 TO HTML-CTTLN3
           MOVE        V261-CQACTL TO HTML-CQACTL
           MOVE        V261-CTTBO1 TO HTML-CTTBO1
           MOVE        V261-CTTBO2 TO HTML-CTTBO2
           MOVE        V261-GEOPD2 TO HTML-GEOPD2
           MOVE        V261-MPLNR2 TO HTML-MPLNR2
           MOVE        V261-ATROLL TO HTML-ATROLL
           MOVE        V261-CCONF TO HTML-CCONF
           MOVE        V261-NGEOPA TO HTML-NGEOPA
           MOVE        V261-CCARC TO HTML-CCARC
           MOVE        V261-NPRCD TO HTML-NPRCD
           MOVE        V261-CCEXD TO HTML-CCEXD
           MOVE        V261-CSECU TO HTML-CSECU
           MOVE        V261-CLNAMA TO HTML-CLNAMA
           MOVE        V261-GESAD1 TO HTML-GESAD1
           MOVE        V261-GESAD2 TO HTML-GESAD2.
                 IF    V261-IADRS = 'Y'                                 DOT
           MOVE        WL00-BACBOX TO HTML-GESAD1.
           MOVE        V261-CLORN TO HTML-CLORN                         DOT
           STRING      V261-NTR
           '-'
           V261-GECKD3
           DELIMITED BY SIZE
           INTO HTML-NTRX
           MOVE        V261-NPBN TO HTML-NPBN
           MOVE        V261-TTBAL TO HTML-TTBAL
           MOVE        V261-MCSIG TO HTML-MCSIG
           MOVE        V261-MPMTT TO HTML-MPMTT
           MOVE        V261-ADBRQ TO WS00-APMTD
           MOVE        WS00-APMTD TO HTML-ADBRQ.
                 IF    V261-CCLPR = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLPR.
                 IF    V261-CCLPR = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLPR.
                 IF    V261-CCLPR = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLPR.
                 IF    V261-CCLCH = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLCH.
                 IF    V261-CCLCH = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLCH.
                 IF    V261-CCLCH = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLCH.
                 IF    V261-CCLSU = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CCLSU.
                 IF    V261-CCLSU = 'N'                                 DOT
           MOVE        'NO' TO HTML-CCLSU.
                 IF    V261-CCLSU = 'Z'                                 DOT
           MOVE        'N/A' TO HTML-CCLSU.
                 IF    V261-IFNMV = 'N'                                 DOT
           MOVE        'NO' TO HTML-IFNMV.
       F35BB-FN. EXIT.
      *N35BC.    NOTE *FORMAT CONTRACT ID                 *.
       F35BC.                                                           lv10
      *
           MOVE        V261-CTID (12:16) TO WS00-NCTIDE
           MOVE        V261-CTIDA TO WS00-CTIDA
           MOVE        V261-GECKD2 TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID.
                 IF    V261-CDTYN = 'SMEM'                              DOT
                 AND   V261-CAFEE = 'O'
           MOVE        V261-CTID01 (12:16) TO WS00-NCTIDE
           MOVE        V261-CTID01 (1:3) TO WS00-CTIDA
           MOVE        V261-CTID01 (4:24) TO 7-CTIDN
           PERFORM     F95CD THRU F95CD-FN
           MOVE        WE00-GERTC TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID01.
                 IF    V261-CDTYN = 'MMTA'                              DOT
           MOVE        V261-CTID02 (12:16) TO WS00-NCTIDE
           MOVE        V261-CTID02 (1:3) TO WS00-CTIDA
           MOVE        V261-CTID02 (4:24) TO 7-CTIDN
           PERFORM     F95CD THRU F95CD-FN
           MOVE        WE00-GERTC TO WS00-GECKD1
           MOVE        WS00-CTID TO HTML-CTID02.
      *N35BD.    NOTE *FORMAT THE CURRENT DATE,           *.
       F35BD.                                                           lv15
      *EFFECTIVE DATE AND SEND TRANS
      *RECEIVED TIME
      *
           STRING      '000'
           V261-DCACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT
           INITIALIZE  I93B
           MOVE        V261-DCACG TO 7-DE00-DCACG
           STRING      V261-DCACG (1:4) '/'
           V261-DCACG (5:2) '/'
           V261-DCACG (7:2) '/'
           DELIMITED BY SIZE INTO
           I93B-DACTT.
           MOVE        V261-DCACD TO I93B-DACTT
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
           MOVE        WS00-DATCE                                       CI0447
           TO DAT8E DAT6C                                               CI0447
           MOVE DAT81E TO DAT63C                                        CI0447
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0447
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0447
      *!ADM "7-DE00-DCACG     HTML-DCACG"
           MOVE        7-DE00-DCACG                                     CI0447
           TO DAT8E DAT6C                                               CI0447
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0447
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0447
           MOVE   DAT8C TO  HTML-DCACG.                                 CI0447
                 IF    (WS00-TIMER >= WS-GETIMT                         DOT
                 AND   V261-DCACG <= WS00-DATCE)
      *NEXT ACTG DATE IF AFTER 3PM
           STRING      '000'
           V261-DNACG (7:2)
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
           WS00-TIMER (3:2) ' '
           WS00-TIMER (5:2) ' '
           WS00-AMPM ' '
           WS00-CST
           DELIMITED BY SIZE INTO
           HTML-TIMER.
       F35BD-FN. EXIT.
      *N35BE.    NOTE *FORMAT THE ORDER RECEIVED DATE     *.
       F35BE.                                                           lv15
      *EFFECTIVE DATE AND SEND THE
      *ORDER RECEIVED TIME
      *
      *!ADM "V261-DAEDTO     HTML-DRECD"
           MOVE        V261-DAEDTO                                      CI0447
           TO DAT8E DAT6C                                               CI0447
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0447
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0447
           MOVE   DAT8C TO  HTML-DRECD                                  CI0447
      *
           STRING      V261-DPTIM DELIMITED BY SIZE
           ' Central Time'
           DELIMITED BY SIZE
           INTO HTML-TIMER1.
       F35BE-FN. EXIT.
       F35BC-FN. EXIT.
      *N35BJ.    NOTE *FORMAT TRANSACTION TYPE FILEDS     *.
       F35BJ.                                                           lv10
                 IF    V261-CDTYN = 'SMTC'                              DOT
           MOVE        WL00-CDTYN1 TO HTML-CDTYN.
                 IF    V261-CDTYN = 'MMTA'                              DOT
           MOVE        WL00-CDTYN2 TO HTML-CDTYN.
                 IF    V261-CDTYN = 'SMDD'                              DOT
           MOVE        WL00-CDTYN3 TO HTML-CDTYN.
                 IF    V261-CDTYN = 'SMEM'                              DOT
           MOVE        WL00-CDTYN4 TO HTML-CDTYN.
                 IF    V261-CDTYN = 'SMEM'                              DOT
                 AND   V261-CAFEE = 'O'
           MOVE        WL00-MEEXP1 TO HTML-MEEXP.
                 IF    V261-CDTYN = 'SMEM'                              DOT
                 AND   V261-CAFEE = 'C'
           MOVE        WL00-MEEXP2 TO HTML-MEEXP.
                 IF    V261-CDTYN = 'SMEM'                              DOT
                 AND   V261-CAFEE = 'A'
           MOVE        WL00-MEEXP3 TO HTML-MEEXP.
       F35BJ-FN. EXIT.
      *N35BM.    NOTE *FORMAT TRA TYPE FIELDS             *.
       F35BM.                                                           lv10
                 IF    V261-CIRAP = 'NA'                                DOT
           MOVE        WL00-CIRAP1 TO HTML-CIRAP.
                 IF    V261-CIRAP = 'CU'                                DOT
           MOVE        WL00-CIRAP2 TO HTML-CIRAP.
                 IF    V261-CIRAP = 'RO'                                DOT
           MOVE        WL00-CIRAP3 TO HTML-CIRAP.
                 IF    V261-CIRAP = 'PR'                                DOT
           MOVE        WL00-CIRAP4 TO HTML-CIRAP.
                 IF    V261-CIRAP = 'SC'                                DOT
           MOVE        WL00-CIRAP5 TO HTML-CIRAP.
       F35BM-FN. EXIT.
      *N35BP.    NOTE *STRING CONFIRMATION INFORMATION    *.
       F35BP.    IF    V261-IVEUP = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35BP-FN.
           STRING      HTML-CONFIRM-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BP-FN. EXIT.
      *N35CB.    NOTE *MOVE VERIFY INFORMATION            *.
       F35CB.    IF    V261-IVEUP = 'V'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35CB-FN.
       F35CB-FN. EXIT.
      *N35DM.    NOTE *STRING COMMOM INFORMATION          *.
       F35DM.                                                           lv10
           PERFORM     F95DM THRU F95DM-FN
           STRING      HTML-COMMON-INFO1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DM-FN. EXIT.
      *N35DP.    NOTE *BUILD OWNERSHIP LINE2              *.
       F35DP.    IF    V261-CTTLN2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DP-FN.
      *
           STRING      V261-CTTLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN2
           STRING      HTML-FROM-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DP-FN. EXIT.
      *N35DR.    NOTE *BUILD OWNERSHIP LINE3              *.
       F35DR.    IF    V261-CTTLN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DR-FN.
      *
           STRING      V261-CTTLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN3
           STRING      HTML-FROM-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DR-FN. EXIT.
      *N35DU.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F35DU.    IF    V261-CTTBO1 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DU-FN.
      *
           STRING      V261-CTTBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
           STRING      HTML-FROM-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DU-FN. EXIT.
      *N35DW.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F35DW.    IF    V261-CTTBO2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DW-FN.
      *
           STRING      V261-CTTBO2
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
      *N40BP.    NOTE *FORMAT WITHHOLDING PERCENT         *.
       F40BP.                                                           lv10
                 IF    V261-TWITH = SPACES                              DOT
           MOVE        V261-PWHLD TO WS00-PWHLD
           STRING      WS00-PWHLD '%'
           DELIMITED BY SIZE
           INTO HTML-CTWHPB
                 ELSE
           MOVE        'Do Not Withhold' TO HTML-CTWHPB.
       F40BP-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *POPULATING VALUES                  *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BC.    NOTE *FOR STEP1 TITLE                    *.
       F50BC.                                                           lv10
      *
           STRING      HTML-STEP1-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BC-FN. EXIT.
      *N50BF.    NOTE *FOR DESTINATION FIELDS             *.
       F50BF.                                                           lv10
      *
           STRING      HTML-DEST-TEXT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BF-FN. EXIT.
      *N50BJ.    NOTE *FOR SMEM DESTINATION               *.
       F50BJ.    IF    V261-CDTYN = 'SMEM'                              lv10
                 NEXT SENTENCE ELSE GO TO     F50BJ-FN.
      *
           STRING      HTML-DEST-METHOD-SMEM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N50BL.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F50BL.    IF    V261-CAFEE = 'O'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50BL-FN.
      *FROM OTHER AMERIPRISE ACCOUNT
           STRING      HTML-DEST-ACCOUNT-SMEM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BL-FN. EXIT.
      *N50BN.    NOTE *FOR VERIFY DESTINATION DETAILS     *.
       F50BN.    IF    V261-CAFEE = 'C'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50BN-FN.
      *CHARGE FROM CREDIT CARD
           STRING      HTML-DEST-CREDIT-SMEM
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BN-FN. EXIT.
       F50BJ-FN. EXIT.
      *N50CA.    NOTE *FOR SMDD DESTINATION               *.
       F50CA.    IF    V261-CDTYN = 'SMDD'                              lv10
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *
           STRING      HTML-DEST-DETAIL-SMDD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CA-FN. EXIT.
      *N50CG.    NOTE *FOR MMTA DESTINATION               *.
       F50CG.    IF    V261-CDTYN = 'MMTA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F50CG-FN.
      *
           STRING      HTML-DEST-DETAIL-MMTA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CG-FN. EXIT.
      *N50CK.    NOTE *END OF DESTINATION DETAILS         *.
       F50CK.                                                           lv10
      *
           STRING      HTML-DEST-DETAIL-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CK-FN. EXIT.
      *N50DA.    NOTE *FOR TRANSACTION DETAILS            *.
       F50DA.                                                           lv10
      *
           STRING      HTML-STEP2-DETAIL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DA-FN. EXIT.
      *N50EA.    NOTE *FOR INSURANCE LOAN TRANSACTIONS    *.
       F50EA.    IF    V261-CTTYPG = 'INLN'                             lv10
                 AND   V261-IRTLN = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50EA-FN.
      *CHECK THE PAYMENT CONVERSION
      *INDICATOR
      *IF REGULAR PREMIUM PAYMENT TO
      *LOAN PAYMENT CONVERSION OPTION
      *IS CHECKED
           STRING      HTML-STEP2-CONVERSION
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V261-ILNTR = 'Y'                                 DOT
      *IF LOAN PAYMENT BACK TO REGULAR
      *PREMIUM PAYMENT CONVERSION
      *OPTION IS CHECKED
           STRING      HTML-STEP2-CONVERSION2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-STEP2-CONVERSION-END                        DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50EA-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *BUILD INFO MSGS AND COMPLIANCE     *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *QUESTIONS
      *N60BA.    NOTE *FOR COMPLIANCE QUESTIONS           *.
       F60BA.                                                           lv10
      *
           STRING      HTML-COMPLIANCE-STEP3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V261-CDTYN = 'MMTA'                              DOT
           STRING      HTML-COMPLIANCE-STEP3-MMTA
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-COMPLIANCE-STEP3-END                        DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60BA-FN. EXIT.
      *N60BP.    NOTE *FOR IMPORTANT MESSAGES             *.
       F60BP.                                                           lv10
      *
           STRING      HTML-IMP-STEP4-HEAD
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V261-CTTYPG = 'INLN'                             DOT
           STRING      HTML-IMP-STEP4-INLN
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V261-CTTYPG = 'INPS'                             DOT
           STRING      HTML-IMP-STEP4-INPS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-IMP-STEP4-CHECK                             DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-IMP-STEP4-COMMON1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V261-CTTYPG = 'INPS'                             DOT
           STRING      HTML-IMP-STEP4-INPS1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-IMP-STEP4-COMMON2                           DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60BP-FN. EXIT.
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
           MOVE        01 TO HT93-CHTML (TALLI)
           MOVE        HTML-TEXT TO HT93-THTML (TALLI)
           ADD         1 TO TALLI.
       F70BG-FN. EXIT.
       F70BE-900. GO TO F70BE-A.
       F70BE-FN. EXIT.
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
      *N93.      NOTE *************************************.
      *               *                                   *
      *               *SQL ERROR HANDLING                 *
      *               *                                   *
      *               *************************************.
       F93.           EXIT.                                             lv05
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
       F93-FN.   EXIT.
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *ADJUST THE MESSAGE AND FORMAT      *
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
                 IF    V261-IVEUP = 'V'                                 DOT
      * *****************************
           STRING      'style="margin-left:5px;'
           'margin-right:35px;"'
           DELIMITED BY SIZE
           INTO HTML-TAG1.
                 IF    V261-IVEUP = 'S'                                 DOT
           MOVE        '</TD></TR></TABLE>' TO
           HTML-TABLE-BOTTOM.
       F95DM-FN. EXIT.
      *N95DP.    NOTE *POPULATE COMMON HEADER             *.
       F95DP.                                                           lv10
                 IF    V261-IVEUP = 'V'                                 DOT
      ******************************
           MOVE        WC00-VERF-MSG1 TO HTML-STEP1T
           MOVE        WC00-VERF-MSG2 TO HTML-STEP2T
           MOVE        WC00-VERF-MSG3 TO HTML-STEP3T.
                 IF    V261-IVEUP = 'S'                                 DOT
      ******************************
           MOVE        WC00-SUB-MSG1 TO HTML-STEP1T
           MOVE        WC00-SUB-MSG2 TO HTML-STEP2T
           MOVE        WC00-SUB-MSG3 TO HTML-STEP3T.
       F95DP-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *CALLED MODULE CI0361               *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
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
