       IDENTIFICATION DIVISION.                                         CI0291
       PROGRAM-ID.  CI0291P.                                            CI0291
      *AUTHOR.         BUILD HTML ARR CONFIRM PAGE.                     CI0291
      *DATE-COMPILED.   09/08/14.                                       CI0291
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2000                          *ACOPYP
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
      *     COPR. 2000                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0291
       CONFIGURATION SECTION.                                           CI0291
       SOURCE-COMPUTER. IBM-370.                                        CI0291
       OBJECT-COMPUTER. IBM-370.                                        CI0291
       DATA DIVISION.                                                   CI0291
       WORKING-STORAGE SECTION.                                         CI0291
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
                        PICTURE 99B99B9999.                             CI0291
      *!WS
           05  7-DE00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0291
      *!WS
           05  7-DE00-GEEND
                        PICTURE 99B99B9999.                             CI0291

      *HTML BLOB THAT NEEDS TO BE SENT TO WEB AND EWF

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
       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WD115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0291
      *!WI pl=WD120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0291
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0291
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0291
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0291
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0291
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0291
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0291
      *** MISCELLANEOUS WORK AREA **

       01  WK00-WORKAREA.
      *!WI
           05 WK00-TX020
                        PICTURE X(20).                                  CI0291
           05 FILLER REDEFINES WK00-TX020.
              10 TX20-TABLE OCCURS 20 TIMES.
      ** INDEX -->        TX20
                 15 TX20-POS PIC X.

           05 WK00-MONTHS.
              10 FILLER PIC X(3) VALUE 'JAN'.
              10 FILLER PIC X(3) VALUE 'FEB'.
              10 FILLER PIC X(3) VALUE 'MAR'.
              10 FILLER PIC X(3) VALUE 'APR'.
              10 FILLER PIC X(3) VALUE 'MAY'.
              10 FILLER PIC X(3) VALUE 'JUN'.
              10 FILLER PIC X(3) VALUE 'JUL'.
              10 FILLER PIC X(3) VALUE 'AUG'.
              10 FILLER PIC X(3) VALUE 'SEP'.
              10 FILLER PIC X(3) VALUE 'OCT'.
              10 FILLER PIC X(3) VALUE 'NOV'.
              10 FILLER PIC X(3) VALUE 'DEC'.
           05 FILLER REDEFINES WK00-MONTHS.
      ** INDEX ----->     MONT
              10 MONTH-TABLE OCCURS 12 TIMES.
                 15 WK00-MON PIC X(3).
      *MISCELLANEOUS FIELDS

       01  WS00-WORKAREA.
           05 WS00-CTID.
              10 WS00-PREFIX PIC X(4) VALUE '0000'.
              10 FILLER      PIC X    VALUE SPACE.
      *!WS
              10 WS00-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0291
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

           05 WS00-CLTINT.
              10 FILLER      PIC X(3) VALUE SPACES.
              10 WS00-TIN    PIC X(9) VALUE SPACES.

           05 WS00-CLID.
              10 FILLER      PIC X(15) VALUE SPACES.
              10 WS00-CLIDND PIC X(8)  VALUE SPACES.

           05 WS00-CLIDPL.
              10 FILLER      PIC X(18) VALUE SPACES.
              10 WS00-CLIDPD PIC X(5)  VALUE SPACES.

      *!WI
           05 WS00-CPCCDE VALUE ZEROS
                        PICTURE 99.                                     CI0291

           05 WS00-TX020  PIC X(60) VALUE SPACES.
      **  INDEX ----->    WX20
           05 FILLER REDEFINES WS00-TX020.
              10 TX020-TABLE OCCURS 20 TIMES.
                 15 WS00-NUM    PIC Z9.
                 15 WS00-COMMA  PIC X.

           05 WS00-MSGTXT.
              10 WS00-MSGNUM PIC Z9.
              10 WS00-DOT    PIC X VALUE '.'.
              10 FILLER      PIC X VALUE SPACE.
      *!WI
              10 WS00-TMESS4
                        PICTURE X(512).                                 CI0291

      *!WI
           05 WS00-CIRMO
                        PICTURE X(12).                                  CI0291
           05 FILLER REDEFINES WS00-CIRMO.
              10 CIRMO-TABLE OCCURS 12 TIMES.
      ** INDEX ----->     FREQ
                 15 FREQ-POS PIC X.

           05 WS00-IRRMON PIC X(30).
           05 FILLER REDEFINES WS00-IRRMON.
              10 IRRMON-TABLE OCCURS 10 TIMES.
                 15 IRR-MON PIC X(3).

      *!WS
           05 WS00-APMT
                        PICTURE ZZ,ZZ9.99.                              CI0291

           05 WS00-COUNT    PIC 9(8) VALUE ZEROS.
      *
      *!WI
       01     WS00-MPMTFL VALUE SPACES
                        PICTURE X(24).                                  CI0291
       01     WS01-TMESSC.
           10 WS01-TMESSC1 PIC X(29) VALUE
                'For record keeping purposes, '.
           10 WS01-TMESSC2 PIC X(25) VALUE
                'this page can be printed.'.
           10 FILLER PIC X(200) VALUE  SPACES.
      ******************************************************************
      *VARIABLE 'TIMER' IS USED TO HOLD THE TRANSACTION RECEIVED TIME.
      ******************************************************************
       01     WS00-TIMER    PIC X(06) VALUE SPACES.
      ******************************************************************
      ** USED TO FORMAT EFFECTIVE DATE BY TRANSACTION
      ******************************************************************
       01 WS00-TS.
           05 WS00-DATE.
      *!WE
               10 WS00-DTGMM
                        PICTURE 9(2).                                   CI0291
               10 FILLER         PIC X    VALUE '/'.
      *!WE
               10 WS00-DTGDD
                        PICTURE 9(2).                                   CI0291
               10 FILLER         PIC X    VALUE '/'.
      *!WE
               10 WS00-DTGCY
                        PICTURE 9(4).                                   CI0291
      ****************************************************************
      ** USED TO FORMAT ORDER TICKET DATE/TIIME BY TRANSACTION
      ****************************************************************
        01 WS01-TS.
           05 WS01-DATE.
      *!WE
              10 WS01-DTGMM
                        PICTURE 9(2).                                   CI0291
              10 FILLER         PIC X    VALUE '/'.
      *!WE
              10 WS01-DTGDD
                        PICTURE 9(2).                                   CI0291
              10 FILLER         PIC X    VALUE '/'.
      *!WE
              10 WS01-DTGCY
                        PICTURE 9(4).                                   CI0291
           05 WS01-TIME.
      *!WE
              10 WS01-DTTHH
                        PICTURE 9(2).                                   CI0291
              10 FILLER         PIC X    VALUE ':'.
      *!WE
              10 WS01-DTTMN
                        PICTURE 9(2).                                   CI0291
              10 FILLER         PIC X    VALUE ':'.
      *!WE
              10 WS01-DTTSS
                        PICTURE 9(2).                                   CI0291
              10 FILLER         PIC X    VALUE ' '.
              10 WS01-AMPM      PIC XX.
              10 FILLER         PIC X(4) VALUE ' CST'.
      *!WI
       01  WS00-DCACG
                        PICTURE 9(8).                                   CI0291
      **----------------------------------------------------------------ANIOBJ
      **  DATE WORK AREA USED BY MACRO AADA58                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
      *!WF DSP=WT DSL=DD SEL=01 FOR=I LEV=1                             ANIOBJ
       01                 WT00.                                         CI0291
          05              WT00-SUITE.                                   CI0291
            15       FILLER         PICTURE  X(00093).                  CI0291
       01                 WT01  REDEFINES      WT00.                    CI0291
            10            WT01-XDAT8.                                   CI0291
            11            WT01-XDATC  PICTURE  XX.                      CI0291
            11            WT01-XDATY  PICTURE  XX.                      CI0291
            11            WT01-XDATM  PICTURE  XX.                      CI0291
            11            WT01-XDATD  PICTURE  XX.                      CI0291
            10            WT01-XDAT8D                                   CI0291
                          REDEFINES            WT01-XDAT8               CI0291
               PICTURE    9(8).                                         CI0291
            10            WT01-XDAT81.                                  CI0291
            11            WT01-XDATM1 PICTURE  XX.                      CI0291
            11            WT01-XDATD1 PICTURE  XX.                      CI0291
            11            WT01-XDATC1 PICTURE  XX.                      CI0291
            11            WT01-XDATY1 PICTURE  XX.                      CI0291
            10            WT01-XDAT80                                   CI0291
                          REDEFINES            WT01-XDAT81              CI0291
               PICTURE    9(8).                                         CI0291
            10            WT01-XDAT62.                                  CI0291
            11            WT01-XDATM2 PICTURE  XX.                      CI0291
            11            WT01-XDATD2 PICTURE  XX.                      CI0291
            11            WT01-XDATY2 PICTURE  XX.                      CI0291
            10            WT01-XDAT69                                   CI0291
                          REDEFINES            WT01-XDAT62              CI0291
               PICTURE    9(6).                                         CI0291
            10            WT01-XDATCU.                                  CI0291
            11            WT01-XDATC9 PICTURE  99.                      CI0291
            11            WT01-XDAYMD.                                  CI0291
            12            WT01-XDATY9 PICTURE  99.                      CI0291
            12            WT01-XDAMD.                                   CI0291
            13            WT01-XDATM9 PICTURE  99.                      CI0291
            13            WT01-XDATD9 PICTURE  99.                      CI0291
            10            WT01-XDAT89 PICTURE  9(8).                    CI0291
            10            WT01-XDAJC  PICTURE  9(7).                    CI0291
            10            WT01-XDAJC1.                                  CI0291
            11            WT01-XDAJC9 PICTURE  99.                      CI0291
            11            WT01-XDAJY  PICTURE  99.                      CI0291
            11            WT01-XDAJN  PICTURE  999.                     CI0291
            10            WT01-XDAB   PICTURE  9(5).                    CI0291
            10            WT01-DD05.                                    CI0291
            11            WT01-XDACT  PICTURE  S9(3)                    CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            WT01-XDACV  PICTURE  S9                       CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            WT01-XDAGP  PICTURE  S9(9)                    CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            WT01-XDAJP  PICTURE  S9(7)                    CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            WT01-XDACV1 PICTURE  S9                       CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            WT01-XDAGP1 PICTURE  S9(9)                    CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            WT01-XDAJP1 PICTURE  S9(7)                    CI0291
                          COMPUTATIONAL-3.                              CI0291
            10            WT01-XW03.                                    CI0291
            11            WT01-XDATG.                                   CI0291
            12            WT01-XDAT1.                                   CI0291
            13            WT01-XDAT19 PICTURE  99.                      CI0291
            12            WT01-XDAT2.                                   CI0291
            13            WT01-XDAT29 PICTURE  99.                      CI0291
            12            WT01-XDAT3.                                   CI0291
            13            WT01-XDAT39 PICTURE  99.                      CI0291
            12            WT01-XDAT4.                                   CI0291
            13            WT01-XDAT49 PICTURE  99.                      CI0291
            11            WT01-XLEAPY PICTURE  99.                      CI0291
            11            WT01-DTGCY  PICTURE  9(4).                    CI0291
            11            WT01-FILLER                                   CI0291
                          REDEFINES            WT01-DTGCY.              CI0291
            12            WT01-DTGCC  PICTURE  9(2).                    CI0291
            12            WT01-DTGYY  PICTURE  9(2).                    CI0291
                                                                        ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      **  WORKING STORAGE FOR CASE DOC DATE                             ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      *!WI pl=WT115                                                     ANIOBJ
       01 7-CSTDTE                                                      ANIOBJ
                        PICTURE X(8).                                   CI0291
       01 FILLER REDEFINES 7-CSTDTE.                                    ANIOBJ
      *!WI pl=WT125                                                     ANIOBJ
          05 7-DTGCY                                                    ANIOBJ
                        PICTURE 9(4).                                   CI0291
      *!WI pl=WT130                                                     ANIOBJ
          05 7-DTGMM                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0291
      *!WI pl=WT135                                                     ANIOBJ
          05 7-DTGDD                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0291
      *                                                                 ANIOBJ
      *!WI pl=WT145                                                     ANIOBJ
       01 7-CASE-DTJUL                                                  ANIOBJ
                        PICTURE 9(7).                                   CI0291
       01 FILLER REDEFINES 7-CASE-DTJUL.                                ANIOBJ
      *!WI pl=WT150                                                     ANIOBJ
          05 7-CASE-DTGCY                                               ANIOBJ
                        PICTURE 9(4).                                   CI0291
      *!WI pl=WT155                                                     ANIOBJ
          05 7-CASE-DTJDD                                               ANIOBJ
                        PICTURE 9(3).                                   CI0291
      *                                                                 ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      ** CASEDOC-ID GENERATED                                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
       01 WT01-NIOBJC.                                                  ANIOBJ
         05 WT01-DTTCY    PIC 9.                                        ANIOBJ
      *!WI pl=WT270                                                     ANIOBJ
         05 WT01-DTJDD                                                  ANIOBJ
                        PICTURE 9(3).                                   CI0291
      *!WI pl=WT280                                                     ANIOBJ
         05 WT01-DTTHH                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0291
      *!WI pl=WT290                                                     ANIOBJ
         05 WT01-DTTMN                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0291
      *!WI pl=WT300                                                     ANIOBJ
         05 WT01-DTTSS                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0291
         05 WT01-DTTNN    PIC 9(3).                                     ANIOBJ
         05 FILLER        PIC X(4)    VALUE '.001'.                     ANIOBJ
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58
       01   DEBUT-WSS.                                                  CI0291
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0291
            05   IK     PICTURE X.                                      CI0291
       01  CONSTANTES-PAC.                                              CI0291
           05  FILLER  PICTURE X(87)   VALUE                            CI0291
                     '6015 CAT09/08/14CI0291ADMIN   14:35:17CI0291P AMERCI0291
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0291
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0291
           05  NUGNA   PICTURE X(5).                                    CI0291
           05  APPLI   PICTURE X(3).                                    CI0291
           05  DATGN   PICTURE X(8).                                    CI0291
           05  PROGR   PICTURE X(6).                                    CI0291
           05  CODUTI  PICTURE X(8).                                    CI0291
           05  TIMGN   PICTURE X(8).                                    CI0291
           05  PROGE   PICTURE X(8).                                    CI0291
           05  COBASE  PICTURE X(4).                                    CI0291
           05  DATGNC  PICTURE X(10).                                   CI0291
           05  RELEAS  PICTURE X(7).                                    CI0291
           05  DATGE   PICTURE X(10).                                   CI0291
           05  DATSQ   PICTURE X(10).                                   CI0291
       01  DATCE.                                                       CI0291
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0291
         05  DATOR.                                                     CI0291
           10  DATOA  PICTURE XX.                                       CI0291
           10  DATOM  PICTURE XX.                                       CI0291
           10  DATOJ  PICTURE XX.                                       CI0291
       01  DAT6.                                                        CI0291
            10 DAT61.                                                   CI0291
            15 DAT619  PICTURE 99.                                      CI0291
            10 DAT62.                                                   CI0291
            15 DAT629  PICTURE 99.                                      CI0291
            10 DAT63   PICTURE XX.                                      CI0291
       01  DAT8.                                                        CI0291
            10 DAT81   PICTURE XX.                                      CI0291
            10 DAT8S1  PICTURE X.                                       CI0291
            10 DAT82   PICTURE XX.                                      CI0291
            10 DAT8S2  PICTURE X.                                       CI0291
            10 DAT83   PICTURE XX.                                      CI0291
       01  DAT8E    REDEFINES    DAT8.                                  CI0291
            10 DAT81E  PICTURE X(4).                                    CI0291
            10 DAT82E  PICTURE XX.                                      CI0291
            10 DAT83E  PICTURE XX.                                      CI0291
       01  DAT6C.                                                       CI0291
            10  DAT61C PICTURE XX.                                      CI0291
            10  DAT62C PICTURE XX.                                      CI0291
            10  DAT63C.                                                 CI0291
             15 DAT63CC PICTURE XX.                                     CI0291
             15 DAT64C  PICTURE XX.                                     CI0291
       01  DAT8C.                                                       CI0291
            10  DAT81C  PICTURE XX.                                     CI0291
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0291
            10  DAT82C  PICTURE XX.                                     CI0291
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0291
            10  DAT83C.                                                 CI0291
             15 DAT83CC PICTURE XX.                                     CI0291
             15 DAT84C  PICTURE XX.                                     CI0291
       01  DATSEP     PICTURE X VALUE '/'.                              CI0291
       01  DATSEW     PICTURE X.                                        CI0291
       01   VARIABLES-CONDITIONNELLES.                                  CI0291
            05                  FT      PICTURE X VALUE '0'.            CI0291
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0291
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0291
            05           IFREQL PICTURE S9(4) VALUE  ZERO.
            05           IFREQR PICTURE S9(4) VALUE  ZERO.
            05           IFREQM PICTURE S9(4) VALUE +0012.
            05           IMONTL PICTURE S9(4) VALUE  ZERO.
            05           IMONTR PICTURE S9(4) VALUE  ZERO.
            05           IMONTM PICTURE S9(4) VALUE +0012.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           ITX20L PICTURE S9(4) VALUE  ZERO.
            05           ITX20R PICTURE S9(4) VALUE  ZERO.
            05           ITX20M PICTURE S9(4) VALUE +0020.
            05           IWX20L PICTURE S9(4) VALUE  ZERO.
            05           IWX20R PICTURE S9(4) VALUE  ZERO.
            05           IWX20M PICTURE S9(4) VALUE +0020.
            05           J40ABR PICTURE S9(4) VALUE  ZERO.
            05           J40IBR PICTURE S9(4) VALUE  ZERO.
            05           J40PDR PICTURE S9(4) VALUE  ZERO.
            05           J50BBR PICTURE S9(4) VALUE  ZERO.
            05           J50IBR PICTURE S9(4) VALUE  ZERO.
            05           J50PDR PICTURE S9(4) VALUE  ZERO.
            05           J55BDR PICTURE S9(4) VALUE  ZERO.
            05           J65GDR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0291
      *COPYBOOK WITH GROUP FLOW HTML TEXT

       COPY CI0291C1.
      *COPYBOOK WITH ACCOUNT FLOW HTML TEXT

       COPY CI0291C2.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0291                *
      ******************************************************************
      *
      *!WF DSP=QT DSL=QT SEL=91 FOR=I LEV=1 PLT=80
       01                 QT00.                                         CI0291
          05              QT00-SUITE.                                   CI0291
            15       FILLER         PICTURE  X(66900).                  CI0291
       01                 QT91  REDEFINES      QT00.                    CI0291
            10            QT91-CIDTPA PICTURE  X.                       CI0291
            10            QT91-CPROCM PICTURE  X.                       CI0291
            10            QT91-MPLNR2 PICTURE  X(40).                   CI0291
            10            QT91-ATROLL PICTURE  X(25).                   CI0291
            10            QT91-GEOPD2 PICTURE  X(8).                    CI0291
            10            QT91-DXTMS2 PICTURE  X(26).                   CI0291
            10            QT91-CCONF  PICTURE  X(25).                   CI0291
            10            QT91-DCACG  PICTURE  9(8).                    CI0291
            10            QT91-CLORN  PICTURE  X(45).                   CI0291
            10            QT91-NTR    PICTURE  9(8).                    CI0291
            10            QT91-GECKD  PICTURE  9.                       CI0291
            10            QT91-NPBN   PICTURE  X(20).                   CI0291
            10            QT91-TTBAL  PICTURE  X(15).                   CI0291
            10            QT91-MCSIG  PICTURE  X(30).                   CI0291
            10            QT91-QT9A                                     CI0291
                          OCCURS       020     TIMES.                   CI0291
            11            QT91-CPCCDE PICTURE  99.                      CI0291
            11            QT91-MFDNM5 PICTURE  X(40).                   CI0291
            11            QT91-CTTLN1 PICTURE  X(30).                   CI0291
            11            QT91-CTTLN2 PICTURE  X(30).                   CI0291
            11            QT91-CTTLN3 PICTURE  X(30).                   CI0291
            11            QT91-CTTBO1 PICTURE  X(45).                   CI0291
            11            QT91-CTTBO2 PICTURE  X(45).                   CI0291
            11            QT91-C299.                                    CI0291
            12            QT91-CTID.                                    CI0291
            13            QT91-CTIDA  PICTURE  9(3).                    CI0291
            13            QT91-CTIDN.                                   CI0291
            14            QT91-CTIDNP PICTURE  X(13).                   CI0291
            14            QT91-CTIDND PICTURE  9(11).                   CI0291
            11            QT91-GECKD1 PICTURE  9.                       CI0291
            11            QT91-MRPSN  PICTURE  X(12).                   CI0291
            11            QT91-CTSTAL PICTURE  X(10).                   CI0291
            11            QT91-APMTL  PICTURE  S9(9)V99                 CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            QT91-MPMTFL PICTURE  X(24).                   CI0291
            11            QT91-CIRMO  PICTURE  X(12).                   CI0291
            11            QT91-DNPMT  PICTURE  9(8).                    CI0291
            11            QT91-MPMTT  PICTURE  X(20).                   CI0291
            11            QT91-DIRAC1 PICTURE  XX.                      CI0291
            11            QT91-GEEND  PICTURE  9(8).                    CI0291
            11            QT91-TDESA  PICTURE  X(10).                   CI0291
            11            QT91-TSECD  PICTURE  X(30).                   CI0291
            11            QT91-CTKRAA PICTURE  X(12).                   CI0291
            11            QT91-TX020  PICTURE  X(20).                   CI0291
            11            QT91-CORTY  PICTURE  X.                       CI0291
            11            QT91-ALOIDD PICTURE  9(9)V99                  CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            QT91-DTLOI  PICTURE  X(10).                   CI0291
            11            QT91-INROA  PICTURE  X(1).                    CI0291
            10            QT91-PRCSN  PICTURE  X(9)                     CI0291
                          OCCURS       020     TIMES.                   CI0291
            10            QT91-QITEM  PICTURE  9(3).                    CI0291
            10            QT91-TMESS4 PICTURE  X(512)                   CI0291
                          OCCURS       020     TIMES.                   CI0291
            10            QT91-CCLPR  PICTURE  X.                       CI0291
            10            QT91-CCLCH  PICTURE  X.                       CI0291
            10            QT91-CCLSU  PICTURE  X.                       CI0291
            10            QT91-QBLCK  PICTURE  9(6).                    CI0291
            10            QT91-QT9C.                                    CI0291
            11            QT91-NBATR  PICTURE  99.                      CI0291
            11            QT91-ISKPP1 PICTURE  X                        CI0291
                          OCCURS       020     TIMES.                   CI0291
            11            QT91-IINSC1 PICTURE  X                        CI0291
                          OCCURS       020     TIMES.                   CI0291
            11            QT91-CLTIN  PICTURE  9(12).                   CI0291
            11            QT91-CLNAMF PICTURE  X(20).                   CI0291
            11            QT91-CLNAMI PICTURE  X.                       CI0291
            11            QT91-CLNAML PICTURE  X(25).                   CI0291
            11            QT91-CLNAMH PICTURE  X(6).                    CI0291
            11            QT91-CLID   PICTURE  X(23).                   CI0291
            11            QT91-GEPHNH PICTURE  X(14).                   CI0291
            11            QT91-CLIDPL PICTURE  X(23).                   CI0291
            11            QT91-CLNMF  PICTURE  X(20).                   CI0291
            11            QT91-CLNML  PICTURE  X(25).                   CI0291
            11            QT91-GEPHNB PICTURE  X(14).                   CI0291
            11            QT91-NSO10  PICTURE  9(5).                    CI0291
            11            QT91-CPCCDF PICTURE  99                       CI0291
                          OCCURS       020     TIMES.                   CI0291
            10            QT91-QT9D.                                    CI0291
            11            QT91-QT9B                                     CI0291
                          OCCURS       233     TIMES.                   CI0291
            12            QT91-CHTML  PICTURE  99.                      CI0291
            12            QT91-THTML  PICTURE  X(200).                  CI0291
            10            QT91-DEFFT  PICTURE  9(8).                    CI0291
            10            QT91-NGEOPA PICTURE  X(08).                   CI0291
            10            QT91-NGEOR  PICTURE  9(08).                   CI0291
            10            QT91-CTRHO  PICTURE  9(8).                    CI0291
            10            QT91-GETOD  PICTURE  9(6).                    CI0291
            10            QT91-CSLCT  PICTURE  X.                       CI0291
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0291
          05              MS00-SUITE.                                   CI0291
            15       FILLER         PICTURE  X(00542).                  CI0291
       01                 MS03  REDEFINES      MS00.                    CI0291
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0291
                          COMPUTATIONAL-3.                              CI0291
            10            MS03-CMSSF  PICTURE  XX.                      CI0291
            10            MS03-DU09.                                    CI0291
            11            MS03-CMESA  PICTURE  S9(9)                    CI0291
                          BINARY.                                       CI0291
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0291
                          BINARY.                                       CI0291
            11            MS03-CMESB  PICTURE  S9(9)                    CI0291
                          BINARY.                                       CI0291
            11            MS03-CMSST  PICTURE  S9(9)                    CI0291
                          BINARY.                                       CI0291
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0291
                          BINARY.                                       CI0291
            11            MS03-QELLAA PICTURE  S9(9)                    CI0291
                          BINARY.                                       CI0291
            11            MS03-TMESS4 PICTURE  X(512).                  CI0291
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0291
            10            MX11-QMSGS  PICTURE  9(03).                   CI0291
            10            MX11-PJ09                                     CI0291
                          OCCURS       025     TIMES.                   CI0291
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0291
                          COMPUTATIONAL-3.                              CI0291
            11            MX11-CMESB  PICTURE  S9(9)                    CI0291
                          BINARY.                                       CI0291
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                QT91
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0291
      *               *                                   *             CI0291
      *               *INITIALISATIONS                    *             CI0291
      *               *                                   *             CI0291
      *               *************************************.            CI0291
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0291
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0291
      *               *                                   *             CI0291
      *               *FIN DE TRAITEMENT                  *             CI0291
      *               *                                   *             CI0291
      *               *************************************.            CI0291
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0291
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
      *N25BB.    NOTE *NUMERIC CHECKS                     *.
       F25BB.    IF    QT91-DCACG NUMERIC                               lv10
                 AND   QT91-NTR NUMERIC
                 AND   QT91-QITEM NUMERIC
                 AND   QT91-QBLCK NUMERIC
                 AND   QT91-CLTIN NUMERIC
                 NEXT SENTENCE ELSE GO TO     F25BB-FN.
       F25BB-900. GO TO F25BD-FN.
       F25BB-FN. EXIT.
      *N25BD.    NOTE *IF ANY OF THEM NOT NUMERIC         *.
       F25BD.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014222 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25BD-FN. EXIT.
      *N25DB.    NOTE *CALCULATIONS                       *.
       F25DB.         EXIT.                                             lv10
      *N25DD.    NOTE *MOVE LENGTH                        *.
       F25DD.                                                           lv15
           MOVE        LENGTH OF HTML-BLOB TO HTML-LEN.
       F25DD-FN. EXIT.
      *N25DG.    NOTE *CALCULATE THE LOOP COUNT           *.
       F25DG.                                                           lv15
           COMPUTE     HTML-LIMIT = HTML-LEN / 200.
       F25DG-FN. EXIT.
       F25DB-FN. EXIT.
       F25-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *BUILD CONFIRMATION INFORMATION     *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35BB.    NOTE *MOVE CONFIRM INFO                  *.
       F35BB.                                                           lv10
           MOVE        QT91-MPLNR2 TO WS91-MPLNR2
           MOVE        QT91-ATROLL TO WS91-ATROLL
           MOVE        QT91-GEOPD2 TO WS91-GEOPD2
           MOVE        QT91-CCONF TO WS91-CCONF
           MOVE        QT91-NGEOPA TO WS91-NGEOPA.
                 IF    QT91-CSLCT = 'Y'                                 DOT
           MOVE        QT91-NGEOR TO WS91-NGEOR
           MOVE        'YES' TO WS91-CSLCT.
                 IF    QT91-CSLCT = 'N'                                 DOT
           MOVE        'N/A' TO WS91-NGEOR
           MOVE        'NO ' TO WS91-CSLCT.
           MOVE        QT91-DEFFT (1:4) TO WS00-DTGCY                   DOT
           MOVE        QT91-DEFFT (5:2) TO WS00-DTGMM
           MOVE        QT91-DEFFT (7:2) TO WS00-DTGDD
           MOVE        WS00-DATE TO WS91-DEFFT.
       F35BB-FN. EXIT.
      *N35BC.    NOTE *SET ORDER TICKET DATE / TIME       *.
       F35BC.                                                           lv10
      ********************************
           MOVE        QT91-CTRHO (1:4) TO WS01-DTGCY
           MOVE        QT91-CTRHO (5:2) TO WS01-DTGMM
           MOVE        QT91-CTRHO (7:2) TO WS01-DTGDD
           MOVE        WS01-DATE TO WS91-CTRHO
           MOVE        QT91-GETOD (1:2) TO WS01-DTTHH
           MOVE        QT91-GETOD (3:2) TO WS01-DTTMN
           MOVE        QT91-GETOD (5:2) TO WS01-DTTSS.
                 IF    WS01-DTTHH < 12                                  DOT
           MOVE        'AM' TO WS01-AMPM
                 ELSE
           MOVE        'PM' TO WS01-AMPM.
                 IF    WS01-DTTHH > 12                                  DOT
           SUBTRACT    12 FROM WS01-DTTHH.
                 IF    WS01-DTTHH = 00                                  DOT
           MOVE        12 TO WS01-DTTHH.
           MOVE        WS01-TIME TO WS91-GETOD.                         DOT
      *N35BD.    NOTE *FORMAT THE CURRENT DATE            *.
       F35BD.                                                           lv15
      *AND SEND THE TRANS RECEIVED TIME
      *********************************
           MOVE FUNCTION CURRENT-DATE(1:8)
                     TO  WS00-DCACG
      *!ADS "WS00-DCACG     7-DE00-DCACG"
           MOVE        WS00-DCACG                                       CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT81E TO DAT63C                                        CI0291
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0291
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0291
      *!ADM "7-DE00-DCACG   WS91-DCACG"
           MOVE        7-DE00-DCACG                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WS91-DCACG                                  CI0291
           MOVE FUNCTION CURRENT-DATE(9:6)
                     TO  WS00-TIMER
      *ASKTIME
      *EIBTIME           WS00-TIMER
           MOVE        WS00-TIMER (1:2) TO WS01-DTTHH
           MOVE        WS00-TIMER (3:2) TO WS01-DTTMN
           MOVE        WS00-TIMER (5:2) TO WS01-DTTSS.
                 IF    WS01-DTTHH < 12                                  DOT
           MOVE        'AM' TO WS01-AMPM
                 ELSE
           MOVE        'PM' TO WS01-AMPM.
                 IF    WS01-DTTHH > 12                                  DOT
           SUBTRACT    12 FROM WS01-DTTHH.
                 IF    WS01-DTTHH = 00                                  DOT
           MOVE        12 TO WS01-DTTHH.
           MOVE        WS01-TIME TO WS91-TIMER.                         DOT
       F35BD-FN. EXIT.
      *N35BG.    NOTE *STRING CONFIRMATION INFORMATION    *.
       F35BG.                                                           lv15
           MOVE        1 TO HTML-PT
           STRING      CONFIRM-INFO DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BG-FN. EXIT.
       F35BC-FN. EXIT.
      *N35CB.    NOTE *BUILD BANK INFORMATION             *.
       F35CB.                                                           lv10
           MOVE        QT91-CLORN TO WS91-CLORN
           MOVE        QT91-NPBN TO WS91-NPBN
           MOVE        QT91-TTBAL TO WS91-TTBAL
           MOVE        QT91-MCSIG TO WS91-MCSIG.
      *N35CD.    NOTE *FORMAT BANK RTN                    *.
       F35CD.                                                           lv15
           MOVE        QT91-NTR TO WS91-RTN
           MOVE        QT91-GECKD TO WS91-GECKD.
       F35CD-FN. EXIT.
      *N35DB.    NOTE *STRING BANK INFORMATION            *.
       F35DB.    IF    QT91-CIDTPA NOT = 'A'                            lv15
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
           STRING      BANK-INFO DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DB-FN. EXIT.
       F35CB-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *BUILD ACCOUNT FLOW CONFIRM PAGE    *
      *               *                                   *
      *               *************************************.
       F40.      IF    QT91-CIDTPA = 'A'                                lv05
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *N40AB.    NOTE *LOOP THRU THE NUMBER OF BA TRANS   *.
       F40AB.                                                           lv10
           MOVE        1                        TO J40ABR
                                    GO TO     F40AB-B.
       F40AB-A.
           ADD         1                        TO J40ABR.
       F40AB-B.
           IF          J40ABR                   >  QT91-NBATR
                                    GO TO     F40AB-FN.
      *N40AF.    NOTE *STRING ACCOUNT INFO FIRST          *.
       F40AF.    IF    J40ABR = 1                                       lv15
                 NEXT SENTENCE ELSE GO TO     F40AF-FN.
           MOVE        QT91-MFDNM5 (J40ABR) TO WK91-MFDNM5.
      *N40BB.    NOTE *BUILD OWNERSHIP LINES              *.
       F40BB.                                                           lv20
           INITIALIZE  OWNER-LINE1 OWNER-LINE2
           INITIALIZE  OWNER-LINE3
           INITIALIZE  BENE-LINE1 BENE-LINE2.
       F40BB-FN. EXIT.
      *N40BD.    NOTE *BUILD ACCOUNT NAME                 *.
       F40BD.                                                           lv20
           MOVE        QT91-CTTLN1 (J40ABR) TO WS91-CTTLN1
           STRING      ACCOUNT-INFO1 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BD-FN. EXIT.
      *N40BF.    NOTE *STRING RETIREMENT PLAN SHORTNAME   *.
       F40BF.    IF    QT91-MRPSN (J40ABR) > SPACES                     lv20
                 AND   QT91-CPCCDE (J40ABR) = 03
                 NEXT SENTENCE ELSE GO TO     F40BF-FN.
           MOVE        QT91-MRPSN (J40ABR) TO WS91-MRPSN
           STRING      MRPSN-LINE DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BF-FN. EXIT.
      *N40BG.    NOTE *BUILD OWNERSHIP LINE #1            *.
       F40BG.                                                           lv20
           MOVE        QT91-CTTLN1 (J40ABR) TO WS91-CTTLN1
           STRING      OWNER-LINE1 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BG-FN. EXIT.
      *N40BI.    NOTE *STRING OWNERSHIP LINE # 2          *.
       F40BI.    IF    QT91-CTTLN2 (J40ABR) > SPACES                    lv20
                 NEXT SENTENCE ELSE GO TO     F40BI-FN.
           MOVE        QT91-CTTLN2 (J40ABR) TO WS91-CTTLN2
           STRING      OWNER-LINE2 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BI-FN. EXIT.
      *N40BL.    NOTE *STRING OWNERSHIP LINE # 3          *.
       F40BL.    IF    QT91-CTTLN3 (J40ABR) > SPACES                    lv20
                 NEXT SENTENCE ELSE GO TO     F40BL-FN.
           MOVE        QT91-CTTLN3 (J40ABR) TO WS91-CTTLN3
           STRING      OWNER-LINE3 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BL-FN. EXIT.
      *N40BP.    NOTE *STRING BENEFICIARY LINE #1         *.
       F40BP.    IF    QT91-CTTBO1 (J40ABR) > SPACES                    lv20
                 NEXT SENTENCE ELSE GO TO     F40BP-FN.
           MOVE        QT91-CTTBO1 (J40ABR) TO WS91-CTTBO1
           STRING      BENE-LINE1 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BP-FN. EXIT.
      *N40BT.    NOTE *STRING BENEFICIARY LINE #2         *.
       F40BT.    IF    QT91-CTTBO2 (J40ABR) > SPACES                    lv20
                 NEXT SENTENCE ELSE GO TO     F40BT-FN.
           MOVE        QT91-CTTBO2 (J40ABR) TO WS91-CTTBO2
           STRING      BENE-LINE2 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BT-FN. EXIT.
      *N40BU.    NOTE *STRING END BOLD TAG                *.
       F40BU.                                                           lv20
           STRING      END-BOLD-TAG DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40BU-FN. EXIT.
      *N40CB.    NOTE *FORMAT CONTRACT ID                 *.
       F40CB.                                                           lv20
      *IF PSA, USE SPECIAL FORMAT
                 IF    QT91-CTIDA (J40ABR) NOT = 040                    DOT
           MOVE        QT91-CTIDND (J40ABR) TO WS00-NCTIDN
           MOVE        QT91-GECKD1 (J40ABR) TO WS00-GECKD1
           MOVE        QT91-CTIDA (J40ABR) TO WS00-CTIDA
           MOVE        WS00-CTID TO WK91-CTID
                 ELSE
           MOVE        QT91-CTIDND (J40ABR) (2:10) TO
           WK91-CTID.
       F40CB-FN. EXIT.
      *N40CD.    NOTE *MOVE ACCOUNT STATUS                *.
       F40CD.                                                           lv20
           MOVE        QT91-CTSTAL (J40ABR) TO WK91-CTSTAL.
       F40CD-FN. EXIT.
      *N40CG.    NOTE *STRING A/C INFO + BANK INFO        *.
       F40CG.                                                           lv20
           STRING      ACCOUNT-INFO2 DELIMITED BY SIZE
           BANK-INFO DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40CG-FN. EXIT.
      *N40DD.    NOTE *FORMAT PRODUCT HEADER              *.
       F40DD.                                                           lv20
           EVALUATE    QT91-CPCCDE (J40ABR)
                 WHEN  01
           MOVE        'Certificates' TO WK91-PTITLE
                 WHEN  02
           MOVE        'Mutual Funds' TO WK91-PTITLE
                 WHEN  03
           MOVE        'Annuities' TO WK91-PTITLE
                 WHEN  04
           MOVE        'Insurance' TO WK91-PTITLE
                 WHEN  05
           MOVE        'Brokerage' TO WK91-PTITLE
                 WHEN  07
           MOVE        'Financial Plans' TO
           WK91-PTITLE
           END-EVALUATE.
       F40DD-FN. EXIT.
      *N40DL.    NOTE *STRING PRODUCT HEADER              *.
       F40DL.                                                           lv20
           STRING      PRODUCT-HEADER DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40DL-FN. EXIT.
       F40AF-FN. EXIT.
      *N40EB.    NOTE *BUILD PAYMENT DETAIL               *.
       F40EB.         EXIT.                                             lv15
      *N40GB.    NOTE *MOVE AMOUNT                        *.
       F40GB.                                                           lv20
           MOVE        QT91-APMTL (J40ABR) TO WK91-APMTL.
       F40GB-FN. EXIT.
      *N40GH.    NOTE *FORMAT FREQUENCY                   *.
       F40GH.         EXIT.                                             lv20
      *N40GL.    NOTE *MOVE FREQUENCY                     *.
       F40GL.                                                           lv25
           MOVE        QT91-MPMTFL (J40ABR) TO WK91-MPMTFL.
       F40GL-FN. EXIT.
      *N40GT.    NOTE *STRING AMOUNT & FREQ INFO          *.
       F40GT.                                                           lv25
           STRING      ACCOUNT-DETAIL01 DELIMITED
           BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40GT-FN. EXIT.
      *N40HB.    NOTE *IN CASE OF IRREGULAR FREQUENCY     *.
       F40HB.    IF    QT91-MPMTFL (J40ABR) =                           lv25
                       'Irregular monthly'
                 NEXT SENTENCE ELSE GO TO     F40HB-FN.
      *N40HD.    NOTE *BUILD IRR FREQ ROW                 *.
       F40HD.                                                           lv30
           MOVE        1 TO IFREQR IMONTR
           MOVE        QT91-CIRMO (J40ABR) TO WS00-CIRMO
           INITIALIZE  WS00-IRRMON.
      *N40IB.    NOTE *LOOP THRU CIRMO                    *.
       F40IB.                                                           lv35
           MOVE        1                        TO J40IBR
                                    GO TO     F40IB-B.
       F40IB-A.
           ADD         1                        TO J40IBR.
       F40IB-B.
           IF          J40IBR                   >  12
                                    GO TO     F40IB-FN.
      *N40ID.    NOTE *CHECK POSITION                     *.
       F40ID.    IF    FREQ-POS (IFREQR) = 'X'                          lv40
                 NEXT SENTENCE ELSE GO TO     F40ID-FN.
           MOVE        WK00-MON (IFREQR) TO IRR-MON (IMONTR)
           ADD         1 TO IFREQR IMONTR.
       F40ID-900. GO TO F40IL-FN.
       F40ID-FN. EXIT.
      *N40IL.    NOTE *BUMP UP INDICATOR FOR NO MATCH     *.
       F40IL.                                                           lv40
           ADD         1 TO IFREQR.
       F40IL-FN. EXIT.
       F40IB-900. GO TO F40IB-A.
       F40IB-FN. EXIT.
      *N40JB.    NOTE *CHECK ROW COUNT                    *.
       F40JB.                                                           lv35
           MOVE        IRR-MON (1) TO IRR-MONTH1
           MOVE        IRR-MON (2) TO IRR-MONTH2
           MOVE        IRR-MON (3) TO IRR-MONTH3
           MOVE        IRR-MON (4) TO IRR-MONTH4
           MOVE        IRR-MON (5) TO IRR-MONTH5
           MOVE        IRR-MON (6) TO IRR-MONTH6
           MOVE        IRR-MON (7) TO IRR-MONTH7
           MOVE        IRR-MON (8) TO IRR-MONTH8
           MOVE        IRR-MON (9) TO IRR-MONTH9
           MOVE        IRR-MON (10) TO IRR-MONTH10.
      *N40JG.    NOTE *STRING THE IRR FREQ TABLE          *.
       F40JG.                                                           lv40
           STRING      IRR-FREQ DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40JG-FN. EXIT.
       F40JB-FN. EXIT.
       F40HD-FN. EXIT.
       F40HB-FN. EXIT.
       F40GH-FN. EXIT.
      *N40LB.    NOTE *REFORMAT THE NEXT PAYMENT DATE     *.
       F40LB.                                                           lv20
      *!ADS "QT91-DNPMT(J40ABR)  7-DE00-DNPMT"
           MOVE        QT91-DNPMT (J40ABR)                              CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT81E TO DAT63C                                        CI0291
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0291
           MOVE   DAT6C TO  7-DE00-DNPMT.                               CI0291
                 IF    QT91-MPMTT (J40ABR) =                            DOT
                       'CASH WITH PURCHASE'
      *!ADM "7-DE00-DNPMT        WK91-DNPMTB"
           MOVE        7-DE00-DNPMT                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WK91-DNPMTB                                 CI0291
                 ELSE
      *!ADM "7-DE00-DNPMT        WK91-DNPMT"
           MOVE        7-DE00-DNPMT                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WK91-DNPMT.                                 CI0291
       F40LB-FN. EXIT.
      *N40LG.    NOTE *MOVE STATUS                        *.
       F40LG.                                                           lv20
                 IF    QT91-MPMTT (J40ABR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        QT91-TDESA (J40ABR) TO WK91-TDESAB
                 ELSE
           MOVE        QT91-TDESA (J40ABR) TO WK91-TDESA.
       F40LG-FN. EXIT.
      *N40LL.    NOTE *MOVE PAYMENT TYPE                  *.
       F40LL.                                                           lv20
                 IF    QT91-MPMTT (J40ABR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        QT91-MPMTT (J40ABR) TO WK91-MPMTTB
                 ELSE
           MOVE        QT91-MPMTT (J40ABR) TO WK91-MPMTT.
       F40LL-FN. EXIT.
      *N40MB.    NOTE *FORMAT IRA PRIOR YEAR              *.
       F40MB.         EXIT.                                             lv20
      *N40MD.    NOTE *IF IRA PRIOR YEAR CODE N/A         *.
       F40MD.    IF    QT91-DIRAC1 (J40ABR) = 99                        lv25
                 NEXT SENTENCE ELSE GO TO     F40MD-FN.
           MOVE        'Not Applicable' TO WK91-DIRAC1.
       F40MD-900. GO TO F40MG-FN.
       F40MD-FN. EXIT.
      *N40MG.    NOTE *IF IRA PRIOR YEAR APPLIES          *.
       F40MG.                                                           lv25
           EVALUATE    TRUE
                 WHEN  QT91-DIRAC1 (J40ABR) = 01
           MOVE        'Jan' TO WK91-DIRAC2
                 WHEN  QT91-DIRAC1 (J40ABR) = 02
           MOVE        'Jan Feb' TO WK91-DIRAC2
                 WHEN  QT91-DIRAC1 (J40ABR) = 03
           MOVE        'Jan Feb Mar' TO WK91-DIRAC2
                 WHEN  QT91-DIRAC1 (J40ABR) = 04
           MOVE        'Jan Feb Mar Apr' TO WK91-DIRAC2
                 WHEN  (QT91-DIRAC1 (J40ABR) = 00
                 AND   QT91-MPMTFL (J40ABR) =
                       'On Demand' OR 'On demand'
                       OR 'ON DEMAND')
           MOVE        'Not Allowed' TO WK91-DIRAC2
                 WHEN  QT91-DIRAC1 (J40ABR) = 00
           MOVE        'None Chosen' TO WK91-DIRAC2
           END-EVALUATE.
       F40MG-FN. EXIT.
       F40MB-FN. EXIT.
      *N40MK.    NOTE *STRING PAYMENT DETAIL              *.
       F40MK.                                                           lv20
                 IF    QT91-MPMTT (J40ABR) =                            DOT
                       'CASH WITH PURCHASE'
           STRING      ACCOUNT-DETAIL02B DELIMITED BY
           SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      ACCOUNT-DETAIL02 DELIMITED BY
           SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40MK-FN. EXIT.
      *N40MN.    NOTE *IF IRA PRIOR YEAR IS N/A           *.
       F40MN.    IF    QT91-DIRAC1 (J40ABR) = 99                        lv20
                 NEXT SENTENCE ELSE GO TO     F40MN-FN.
           STRING      WK91-IRAYR1 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40MN-900. GO TO F40MT-FN.
       F40MN-FN. EXIT.
      *N40MT.    NOTE *IF IRA PRIOR YEAR IS APPLICABLE    *.
       F40MT.                                                           lv20
           STRING      WK91-IRAYR2 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40MT-FN. EXIT.
      *N40OD.    NOTE *FORMAT END DATE                    *.
       F40OD.         EXIT.                                             lv20
      *N40OG.    NOTE *CALCULATE THE END DATE             *.
       F40OG.    IF    QT91-GEEND (J40ABR) > ZERO                       lv25
                 NEXT SENTENCE ELSE GO TO     F40OG-FN.
      *!ADS "QT91-GEEND(J40ABR) 7-DE00-GEEND"
           MOVE        QT91-GEEND (J40ABR)                              CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT81E TO DAT63C                                        CI0291
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0291
           MOVE   DAT6C TO  7-DE00-GEEND.                               CI0291
                 IF    QT91-MPMTT (J40ABR) =                            DOT
                       'CASH WITH PURCHASE'
      *!ADM "7-DE00-GEEND   WK91-GEENDB"
           MOVE        7-DE00-GEEND                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WK91-GEENDB                                 CI0291
                 ELSE
      *!ADM "7-DE00-GEEND   WK91-GEEND"
           MOVE        7-DE00-GEEND                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WK91-GEEND.                                 CI0291
       F40OG-900. GO TO F40OI-FN.
       F40OG-FN. EXIT.
      *N40OI.    NOTE *ELSE... JUST INIT DATE             *.
       F40OI.                                                           lv25
                 IF    QT91-MPMTT (J40ABR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        SPACES TO WK91-GEENDB
                 ELSE
           MOVE        SPACES TO WK91-GEEND.
                 IF    QT91-MPMTFL (J40ABR) =                           DOT
                       'ON DEMAND' OR 'On demand'
                       OR 'On Demand'
      *IF 'ON DEMAND', POPULATE
      *'NOT ALLOWED'
           MOVE        'Not Allowed' TO WK91-GEENDB
           WK91-GEEND.
      *                                                                 DOT
       F40OI-FN. EXIT.
       F40OD-FN. EXIT.
      *N40OJ.    NOTE *INITIALIZE ROA FIELDS              *.
       F40OJ.                                                           lv20
      ********************************
           MOVE        'N/A' TO WK91-CORTYA
           MOVE        'N/A' TO WK91-ALOIDD1
           MOVE        'N/A' TO WK91-DTLOI.
       F40OJ-FN. EXIT.
      *N40OK.    NOTE *MOVE ROA FIELDS                    *.
       F40OK.    IF    QT91-INROA (J40ABR) = 'Y'                        lv20
                 NEXT SENTENCE ELSE GO TO     F40OK-FN.
                 IF    QT91-CORTY (J40ABR) = 'R'                        DOT
      ******MOVE ROA FIELDS***********
           MOVE        'ROA' TO WK91-CORTYA
           MOVE        QT91-ALOIDD (J40ABR) TO WK91-ALOIDD.
                 IF    QT91-CORTY (J40ABR) = 'L'                        DOT
      ******MOVE LOI FIELDS***********
           MOVE        'LOI' TO WK91-CORTYA
           MOVE        QT91-DTLOI (J40ABR) TO WK91-DTLOI
           MOVE        QT91-ALOIDD (J40ABR) TO WK91-ALOIDD.
                 IF    QT91-CORTY (J40ABR) = 'N'                        DOT
           MOVE        'NAV' TO WK91-CORTYA.
      *                                                                 DOT
       F40OK-FN. EXIT.
      *N40OM.    NOTE *FORMAT TICKER SYMBOL & MUTUAL      *.
       F40OM.    IF    QT91-MPMTT (J40ABR) =                            lv20
                       'CASH WITH PURCHASE'
                 NEXT SENTENCE ELSE GO TO     F40OM-FN.
      *FUND DESCRIPTION
           MOVE        QT91-CTKRAA (J40ABR) TO WK91-CTKRAA
           MOVE        QT91-TSECD (J40ABR) TO WK91-TSECD.
       F40OM-FN. EXIT.
       F40EB-FN. EXIT.
      *N40PB.    NOTE *FORMAT IMPLICATION MESSAGE #       *.
       F40PB.                                                           lv15
           MOVE        20 TO ITX20L
           MOVE        1 TO ITX20R IWX20R
           MOVE        QT91-TX020 (J40ABR) TO WK00-TX020
           INITIALIZE  WS00-TX020
      *FORMAT THE FIRST STATIC MESSAGE
      *-"PRINT THE PAGE" MESSAGE
           MOVE        1 TO WS00-NUM (1)
           MOVE        ',' TO WS00-COMMA (1)
           ADD         1 TO IWX20R.
      *N40PD.    NOTE *LOOP THRU TX020 MESSAGE AREA       *.
       F40PD.                                                           lv20
           MOVE        1                        TO J40PDR
                                    GO TO     F40PD-B.
       F40PD-A.
           ADD         1                        TO J40PDR.
       F40PD-B.
           IF          J40PDR                   >  ITX20L
                                    GO TO     F40PD-FN.
      *N40PG.    NOTE *CHECK FOR IMP MESSAGE              *.
       F40PG.    IF    TX20-POS (ITX20R) = 'Y'                          lv25
                 NEXT SENTENCE ELSE GO TO     F40PG-FN.
           COMPUTE     WS00-NUM (IWX20R) = ITX20R + 1
           MOVE        ',' TO WS00-COMMA (IWX20R)
           ADD         1 TO ITX20R IWX20R.
       F40PG-900. GO TO F40PH-FN.
       F40PG-FN. EXIT.
      *N40PH.    NOTE *BUMP UP THE COUNT                  *.
       F40PH.                                                           lv25
           ADD         1 TO ITX20R.
       F40PH-FN. EXIT.
       F40PD-900. GO TO F40PD-A.
       F40PD-FN. EXIT.
      *N40PI.    NOTE *MOVE IMP MSG NUMBERS TO COPYBOOK   *.
       F40PI.                                                           lv20
      *WIPE OUT LAST COMMA
                 IF    IWX20R > 1                                       DOT
           SUBTRACT    1 FROM IWX20R
           MOVE        SPACES TO WS00-COMMA (IWX20R).
                 IF    QT91-MPMTT (J40ABR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        WS00-TX020 TO WK91-IMPNUMB
                 ELSE
           MOVE        WS00-TX020 TO WK91-IMPNUM.
       F40PI-FN. EXIT.
       F40PB-FN. EXIT.
      *N40PP.    NOTE *STRING END DATE,IMPLICATION MSG,   *.
       F40PP.    IF    QT91-MPMTT (J40ABR) =                            lv15
                       'CASH WITH PURCHASE'
                 NEXT SENTENCE ELSE GO TO     F40PP-FN.
      *MUTUAL FUND DESC, TICKER SYMBOL,
      *AND (POSSIBLY) ROA FIELDS
      *****CASH WITH PURCHASE**********
                 IF    QT91-INROA (J40ABR) = 'Y'                        DOT
           MOVE        WK91-GEENDB TO WK91-GEENDC
           MOVE        WK91-CTKRAA TO WK91-CTKRAAC
           MOVE        WK91-TSECD TO WK91-TSECDC
           MOVE        WK91-IMPNUMB TO WK91-IMPNUMC
           STRING      ACCOUNT-DETAIL03C DELIMITED BY
           SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      ACCOUNT-DETAIL03B DELIMITED BY
           SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40PP-900. GO TO F40PQ-FN.
       F40PP-FN. EXIT.
      *N40PQ.    NOTE *********************************   *.
       F40PQ.                                                           lv15
      ****** CASH DEPOSIT OPTION ******
           STRING      ACCOUNT-DETAIL03 DELIMITED BY
           SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40PQ-FN. EXIT.
       F40AB-900. GO TO F40AB-A.
       F40AB-FN. EXIT.
      *N40TB.    NOTE *STRING END TABLE TAG AT END        *.
       F40TB.                                                           lv10
           STRING      END-ACCTTABLE DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F40TB-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *BUILD GROUP/CLIENT CONFIRM PAGE    *
      *               *                                   *
      *               *************************************.
       F50.      IF    QT91-CIDTPA NOT = 'A'                            lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BB.    NOTE *LOOP THRU THE NUMBER OF BA TRANS   *.
       F50BB.                                                           lv10
           MOVE        1                        TO J50BBR
                                    GO TO     F50BB-B.
       F50BB-A.
           ADD         1                        TO J50BBR.
       F50BB-B.
           IF          J50BBR                   >  QT91-NBATR
                                    GO TO     F50BB-FN.
      *N50BC.    NOTE *CHECK CHANGE IN PRODUCT CATEGORY   *.
       F50BC.    IF    QT91-CPCCDE (J50BBR) NOT =                       lv15
                       WS00-CPCCDE
                 NEXT SENTENCE ELSE GO TO     F50BC-FN.
           MOVE        QT91-CPCCDE (J50BBR) TO WS00-CPCCDE.
      *N50BD.    NOTE *FORMAT PRODUCT HEADER              *.
       F50BD.                                                           lv20
           EVALUATE    QT91-CPCCDE (J50BBR)
                 WHEN  01
           MOVE        'Certificates' TO PRODT-TITLE
                 WHEN  02
           MOVE        'Mutual Funds' TO PRODT-TITLE
                 WHEN  03
           MOVE        'Annuities' TO PRODT-TITLE
                 WHEN  04
           MOVE        'Insurance' TO PRODT-TITLE
                 WHEN  05
           MOVE        'Brokerage' TO PRODT-TITLE
                 WHEN  07
           MOVE        'Financial Plans' TO
           PRODT-TITLE
           END-EVALUATE.
       F50BD-FN. EXIT.
      *N50BG.    NOTE *STRING END TABLE TAG               *.
       F50BG.    IF    J50BBR > 1                                       lv20
                 NEXT SENTENCE ELSE GO TO     F50BG-FN.
           STRING      END-TABLE DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BG-FN. EXIT.
      *N50BL.    NOTE *STRING PRODUCT HEADER              *.
       F50BL.                                                           lv20
           STRING      GROUP-HEADER DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BL-FN. EXIT.
       F50BC-FN. EXIT.
      *N50DB.    NOTE *BUILD PAYMENT DETAIL               *.
       F50DB.         EXIT.                                             lv15
      *N50DD.    NOTE *STRING ACCOUNT INFO                *.
       F50DD.                                                           lv20
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        QT91-MFDNM5 (J50BBR) TO WS91-MFDNM5B
                 ELSE
           MOVE        QT91-MFDNM5 (J50BBR) TO WS91-MFDNM5.
       F50DD-FN. EXIT.
      *N50DG.    NOTE *BUILD OWNERSHIP LINES              *.
       F50DG.                                                           lv20
           INITIALIZE  OWNER-LINE1 OWNER-LINE2
           INITIALIZE  OWNER-LINE3
           INITIALIZE  BENE-LINE1 BENE-LINE2.
      *N50DH.    NOTE *STRING ACCOUNT NAME                *.
       F50DH.                                                           lv25
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
           STRING      PAY-DETAIL01B DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      PAY-DETAIL01 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DH-FN. EXIT.
      *N50DJ.    NOTE *STRING RETIREMENT PLAN SHORTNAME   *.
       F50DJ.    IF    QT91-MRPSN (J50BBR) > SPACES                     lv25
                 AND   QT91-CPCCDE (J50BBR) = 03
                 NEXT SENTENCE ELSE GO TO     F50DJ-FN.
           MOVE        QT91-MRPSN (J50BBR) TO WS91-MRPSN
           STRING      MRPSN-LINE DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DJ-FN. EXIT.
      *N50DL.    NOTE *STRING OWNERSHIP LINE #1           *.
       F50DL.                                                           lv25
           MOVE        QT91-CTTLN1 (J50BBR) TO WS91-CTTLN1
           STRING      OWNER-LINE1 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DL-FN. EXIT.
      *N50DN.    NOTE *STRING OWNERSHIP LINE # 2          *.
       F50DN.    IF    QT91-CTTLN2 (J50BBR) > SPACES                    lv25
                 NEXT SENTENCE ELSE GO TO     F50DN-FN.
           MOVE        QT91-CTTLN2 (J50BBR) TO WS91-CTTLN2
           STRING      OWNER-LINE2 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DN-FN. EXIT.
      *N50DP.    NOTE *STRING OWNERSHIP LINE # 3          *.
       F50DP.    IF    QT91-CTTLN3 (J50BBR) > SPACES                    lv25
                 NEXT SENTENCE ELSE GO TO     F50DP-FN.
           MOVE        QT91-CTTLN3 (J50BBR) TO WS91-CTTLN3
           STRING      OWNER-LINE3 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DP-FN. EXIT.
      *N50DR.    NOTE *STRING BENEFICIARY LINE # 1        *.
       F50DR.    IF    QT91-CTTBO1 (J50BBR) > SPACES                    lv25
                 NEXT SENTENCE ELSE GO TO     F50DR-FN.
           MOVE        QT91-CTTBO1 (J50BBR) TO WS91-CTTBO1
           STRING      BENE-LINE1 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DR-FN. EXIT.
      *N50DT.    NOTE *STRING BENEFICIARY LINE # 2        *.
       F50DT.    IF    QT91-CTTBO2 (J50BBR) > SPACES                    lv25
                 NEXT SENTENCE ELSE GO TO     F50DT-FN.
           MOVE        QT91-CTTBO2 (J50BBR) TO WS91-CTTBO2
           STRING      BENE-LINE2 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50DT-FN. EXIT.
       F50DG-FN. EXIT.
      *N50FB.    NOTE *FORMAT CONTRACT ID                 *.
       F50FB.                                                           lv20
      *IF PSA, USE SPECIAL FORMAT
                 IF    QT91-CTIDA (J50BBR) NOT = 040                    DOT
           MOVE        QT91-CTIDND (J50BBR) TO WS00-NCTIDN
           MOVE        QT91-GECKD1 (J50BBR) TO WS00-GECKD1
           MOVE        QT91-CTIDA (J50BBR) TO WS00-CTIDA
           MOVE        WS00-CTID TO WS91-CTID
                 ELSE
           MOVE        QT91-CTIDND (J50BBR) (2:10) TO
           WS91-CTID.
       F50FB-FN. EXIT.
      *N50GB.    NOTE *MOVE AMOUNT                        *.
       F50GB.                                                           lv20
           MOVE        QT91-APMTL (J50BBR) TO WS91-APMTL.
       F50GB-FN. EXIT.
      *N50GH.    NOTE *FORMAT FREQUENCY                   *.
       F50GH.         EXIT.                                             lv20
      *N50GL.    NOTE *MOVE FREQUENCY                     *.
       F50GL.                                                           lv25
           MOVE        QT91-MPMTFL (J50BBR) TO WS91-MPMTFL.
       F50GL-FN. EXIT.
      *N50GT.    NOTE *STRING AMOUNT & FREQ INFO          *.
       F50GT.                                                           lv25
           STRING      PAY-DETAIL02 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50GT-FN. EXIT.
      *N50HB.    NOTE *IN CASE OF IRREGULAR FREQUENCY     *.
       F50HB.    IF    QT91-MPMTFL (J50BBR) =                           lv25
                       'Irregular monthly'
                 NEXT SENTENCE ELSE GO TO     F50HB-FN.
      *N50HD.    NOTE *BUILD IRR FREQ ROW                 *.
       F50HD.                                                           lv30
           MOVE        1 TO IFREQR IMONTR
           MOVE        QT91-CIRMO (J50BBR) TO WS00-CIRMO
           INITIALIZE  WS00-IRRMON.
      *N50IB.    NOTE *LOOP THRU CIRMO                    *.
       F50IB.                                                           lv35
           MOVE        1                        TO J50IBR
                                    GO TO     F50IB-B.
       F50IB-A.
           ADD         1                        TO J50IBR.
       F50IB-B.
           IF          J50IBR                   >  12
                                    GO TO     F50IB-FN.
      *N50ID.    NOTE *CHECK POSITION                     *.
       F50ID.    IF    FREQ-POS (IFREQR) = 'X'                          lv40
                 NEXT SENTENCE ELSE GO TO     F50ID-FN.
           MOVE        WK00-MON (IFREQR) TO IRR-MON (IMONTR)
           ADD         1 TO IFREQR IMONTR.
       F50ID-900. GO TO F50IL-FN.
       F50ID-FN. EXIT.
      *N50IL.    NOTE *BUMP UP INDICATOR FOR NO MATCH     *.
       F50IL.                                                           lv40
           ADD         1 TO IFREQR.
       F50IL-FN. EXIT.
       F50IB-900. GO TO F50IB-A.
       F50IB-FN. EXIT.
      *N50JB.    NOTE *CHECK ROW COUNT                    *.
       F50JB.                                                           lv35
           MOVE        IRR-MON (1) TO IRR-MONTH1
           MOVE        IRR-MON (2) TO IRR-MONTH2
           MOVE        IRR-MON (3) TO IRR-MONTH3
           MOVE        IRR-MON (4) TO IRR-MONTH4
           MOVE        IRR-MON (5) TO IRR-MONTH5
           MOVE        IRR-MON (6) TO IRR-MONTH6
           MOVE        IRR-MON (7) TO IRR-MONTH7
           MOVE        IRR-MON (8) TO IRR-MONTH8
           MOVE        IRR-MON (9) TO IRR-MONTH9
           MOVE        IRR-MON (10) TO IRR-MONTH10.
      *N50JG.    NOTE *STRING THE IRR FREQ TABLE          *.
       F50JG.                                                           lv40
           STRING      IRR-FREQ DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50JG-FN. EXIT.
       F50JB-FN. EXIT.
       F50HD-FN. EXIT.
       F50HB-FN. EXIT.
       F50GH-FN. EXIT.
      *N50LB.    NOTE *REFORMAT THE NEXT PAYMENT DATE     *.
       F50LB.                                                           lv20
      *!ADS "QT91-DNPMT(J50BBR)  7-DE00-DNPMT"
           MOVE        QT91-DNPMT (J50BBR)                              CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT81E TO DAT63C                                        CI0291
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0291
           MOVE   DAT6C TO  7-DE00-DNPMT.                               CI0291
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
      *!ADM "7-DE00-DNPMT        WS91-DNPMTB"
           MOVE        7-DE00-DNPMT                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WS91-DNPMTB                                 CI0291
                 ELSE
      *!ADM "7-DE00-DNPMT        WS91-DNPMT"
           MOVE        7-DE00-DNPMT                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WS91-DNPMT.                                 CI0291
       F50LB-FN. EXIT.
      *N50LG.    NOTE *MOVE STATUS                        *.
       F50LG.                                                           lv20
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        QT91-TDESA (J50BBR) TO WS91-TDESAB
                 ELSE
           MOVE        QT91-TDESA (J50BBR) TO WS91-TDESA.
       F50LG-FN. EXIT.
      *N50LL.    NOTE *MOVE PAYMENT TYPE                  *.
       F50LL.                                                           lv20
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        QT91-MPMTT (J50BBR) TO WS91-MPMTTB
                 ELSE
           MOVE        QT91-MPMTT (J50BBR) TO WS91-MPMTT.
       F50LL-FN. EXIT.
      *N50MB.    NOTE *FORMAT IRA PRIOR YEAR              *.
       F50MB.         EXIT.                                             lv20
      *N50MD.    NOTE *IF IRA PRIOR YEAR CODE N/A         *.
       F50MD.    IF    QT91-DIRAC1 (J50BBR) = 99                        lv25
                 NEXT SENTENCE ELSE GO TO     F50MD-FN.
           MOVE        'Not Applicable' TO WS91-DIRAC1.
       F50MD-900. GO TO F50MG-FN.
       F50MD-FN. EXIT.
      *N50MG.    NOTE *IF IRA PRIOR YEAR APPLIES          *.
       F50MG.                                                           lv25
           EVALUATE    TRUE
                 WHEN  QT91-DIRAC1 (J50BBR) = 01
           MOVE        'Jan' TO WS91-DIRAC2
                 WHEN  QT91-DIRAC1 (J50BBR) = 02
           MOVE        'Jan Feb' TO WS91-DIRAC2
                 WHEN  QT91-DIRAC1 (J50BBR) = 03
           MOVE        'Jan Feb Mar' TO WS91-DIRAC2
                 WHEN  QT91-DIRAC1 (J50BBR) = 04
           MOVE        'Jan Feb Mar Apr' TO WS91-DIRAC2
                 WHEN  (QT91-DIRAC1 (J50BBR) = 00
                 AND   QT91-MPMTFL (J50BBR) =
                       'On Demand' OR 'On demand'
                       OR 'ON DEMAND')
           MOVE        'Not Allowed' TO WS91-DIRAC2
                 WHEN  QT91-DIRAC1 (J50BBR) = 00
           MOVE        'None Chosen' TO WS91-DIRAC2
           END-EVALUATE.
       F50MG-FN. EXIT.
       F50MB-FN. EXIT.
      *N50MK.    NOTE *STRING PAYMENT DETAIL              *.
       F50MK.                                                           lv20
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
           STRING      PAY-DETAIL03B DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      PAY-DETAIL03 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50MK-FN. EXIT.
      *N50MN.    NOTE *IF IRA PRIOR YEAR IS N/A           *.
       F50MN.    IF    QT91-DIRAC1 (J50BBR) = 99                        lv20
                 NEXT SENTENCE ELSE GO TO     F50MN-FN.
           STRING      WS91-IRAYR1 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50MN-900. GO TO F50MT-FN.
       F50MN-FN. EXIT.
      *N50MT.    NOTE *IF IRA PRIOR YEAR IS APPLICABLE    *.
       F50MT.                                                           lv20
           STRING      WS91-IRAYR2 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50MT-FN. EXIT.
      *N50OD.    NOTE *FORMAT END DATE                    *.
       F50OD.         EXIT.                                             lv20
      *N50OG.    NOTE *CALCULATE THE END DATE             *.
       F50OG.    IF    QT91-GEEND (J50BBR) > ZERO                       lv25
                 NEXT SENTENCE ELSE GO TO     F50OG-FN.
      *!ADS "QT91-GEEND(J50BBR) 7-DE00-GEEND"
           MOVE        QT91-GEEND (J50BBR)                              CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT81E TO DAT63C                                        CI0291
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0291
           MOVE   DAT6C TO  7-DE00-GEEND.                               CI0291
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
      *!ADM "7-DE00-GEEND       WS91-GEENDB"
           MOVE        7-DE00-GEEND                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WS91-GEENDB                                 CI0291
                 ELSE
      *!ADM "7-DE00-GEEND       WS91-GEEND"
           MOVE        7-DE00-GEEND                                     CI0291
           TO DAT8E DAT6C                                               CI0291
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0291
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0291
           MOVE   DAT8C TO  WS91-GEEND.                                 CI0291
       F50OG-900. GO TO F50OI-FN.
       F50OG-FN. EXIT.
      *N50OI.    NOTE *ELSE... JUST INIT DATE             *.
       F50OI.                                                           lv25
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        SPACES TO WS91-GEENDB
                 ELSE
           MOVE        SPACES TO WS91-GEEND.
                 IF    QT91-MPMTFL (J50BBR) =                           DOT
                       'On demand' OR 'ON DEMAND'
                       OR 'On Demand'
      *IF 'ON DEMAND', POPULATE
      *'NOT ALLOWED'
           MOVE        'Not Allowed' TO WS91-GEENDB
           WS91-GEEND.
      *                                                                 DOT
       F50OI-FN. EXIT.
       F50OD-FN. EXIT.
      *N50OJ.    NOTE *INITITIALIZE ROA FIELDS            *.
       F50OJ.                                                           lv20
      ********************************
           MOVE        'N/A' TO WS91-CORTYA
           MOVE        'N/A' TO WS91-ALOIDD1
           MOVE        'N/A' TO WS91-DTLOI.
       F50OJ-FN. EXIT.
      *N50OK.    NOTE *MOVE ROA FIELDS                    *.
       F50OK.    IF    QT91-INROA (J50BBR) = 'Y'                        lv20
                 NEXT SENTENCE ELSE GO TO     F50OK-FN.
                 IF    QT91-CORTY (J50BBR) = 'R'                        DOT
      ******MOVE ROA FIELDS***********
           MOVE        'ROA' TO WS91-CORTYA
           MOVE        QT91-ALOIDD (J50BBR) TO WS91-ALOIDD.
                 IF    QT91-CORTY (J50BBR) = 'L'                        DOT
      ******MOVE LOI FIELDS***********
           MOVE        'LOI' TO WS91-CORTYA
           MOVE        QT91-DTLOI (J50BBR) TO WS91-DTLOI
           MOVE        QT91-ALOIDD (J50BBR) TO WS91-ALOIDD.
                 IF    QT91-CORTY (J50BBR) = 'N'                        DOT
           MOVE        'NAV' TO WK91-CORTYA.
      *                                                                 DOT
       F50OK-FN. EXIT.
      *N50OM.    NOTE *FORMAT TICKER SYMBOL AND MUTUAL    *.
       F50OM.    IF    QT91-MPMTT (J50BBR) =                            lv20
                       'CASH WITH PURCHASE'
                 NEXT SENTENCE ELSE GO TO     F50OM-FN.
      *FUND DESCRIPTION
           MOVE        QT91-CTKRAA (J50BBR) TO WS91-CTKRAA
           MOVE        QT91-TSECD (J50BBR) TO WS91-TSECD.
       F50OM-FN. EXIT.
       F50DB-FN. EXIT.
      *N50PB.    NOTE *FORMAT IMPLICATION MESSAGE #       *.
       F50PB.                                                           lv15
           MOVE        20 TO ITX20L
           MOVE        1 TO ITX20R IWX20R
           MOVE        QT91-TX020 (J50BBR) TO WK00-TX020
           INITIALIZE  WS00-TX020
      *FORMAT THE FIRST STATIC MESSAGE
      *-"PRINT THE PAGE" MESSAGE
           MOVE        1 TO WS00-NUM (1)
           MOVE        ',' TO WS00-COMMA (1)
           ADD         1 TO IWX20R.
      *N50PD.    NOTE *LOOP THRU TX020 MESSAGE AREA       *.
       F50PD.                                                           lv20
           MOVE        1                        TO J50PDR
                                    GO TO     F50PD-B.
       F50PD-A.
           ADD         1                        TO J50PDR.
       F50PD-B.
           IF          J50PDR                   >  ITX20L
                                    GO TO     F50PD-FN.
      *N50PG.    NOTE *CHECK FOR IMP MESSAGE              *.
       F50PG.    IF    TX20-POS (ITX20R) = 'Y'                          lv25
                 NEXT SENTENCE ELSE GO TO     F50PG-FN.
           COMPUTE     WS00-NUM (IWX20R) = ITX20R + 1
           MOVE        ',' TO WS00-COMMA (IWX20R)
           ADD         1 TO ITX20R IWX20R.
       F50PG-900. GO TO F50PH-FN.
       F50PG-FN. EXIT.
      *N50PH.    NOTE *BUMP UP THE COUNT                  *.
       F50PH.                                                           lv25
           ADD         1 TO ITX20R.
       F50PH-FN. EXIT.
       F50PD-900. GO TO F50PD-A.
       F50PD-FN. EXIT.
      *N50PI.    NOTE *MOVE IMP MSG NUMBERS TO COPYBOOK   *.
       F50PI.                                                           lv20
      *WIPE OUT LAST COMMA
                 IF    IWX20R > 1                                       DOT
           SUBTRACT    1 FROM IWX20R
           MOVE        SPACES TO WS00-COMMA (IWX20R).
                 IF    QT91-MPMTT (J50BBR) =                            DOT
                       'CASH WITH PURCHASE'
           MOVE        WS00-TX020 TO WS91-IMPNUMB
                 ELSE
           MOVE        WS00-TX020 TO WS91-IMPNUM.
       F50PI-FN. EXIT.
       F50PB-FN. EXIT.
      *N50PP.    NOTE *STRING END DATE,IMPLICATION MSG,   *.
       F50PP.    IF    QT91-MPMTT (J50BBR) =                            lv15
                       'CASH WITH PURCHASE'
                 NEXT SENTENCE ELSE GO TO     F50PP-FN.
      *MUTUAL FUND DESC, TICKER SYMBOL,
      *AND (POSSIBLY) ROA FIELDS
      *****CASH WITH PURCHASE**********
                 IF    QT91-INROA (J50BBR) = 'Y'                        DOT
           MOVE        WS91-GEENDB TO WS91-GEENDC
           MOVE        WS91-CTKRAA TO WS91-CTKRAAC
           MOVE        WS91-TSECD TO WS91-TSECDC
           MOVE        WS91-IMPNUMB TO WS91-IMPNUMC
           STRING      PAY-DETAIL04C DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
                 ELSE
           STRING      PAY-DETAIL04B DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50PP-900. GO TO F50PQ-FN.
       F50PP-FN. EXIT.
      *N50PQ.    NOTE *********************************   *.
       F50PQ.                                                           lv15
      ****** CASH DEPOSIT OPTION ******
           STRING      PAY-DETAIL04 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50PQ-FN. EXIT.
       F50BB-900. GO TO F50BB-A.
       F50BB-FN. EXIT.
      *N50TB.    NOTE *STRING END TABLE TAG AT END        *.
       F50TB.                                                           lv10
           STRING      END-TABLE DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50TB-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *BUILD INFO COMMON TO ALL FLOWS     *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55BB.    NOTE *LIST IMPLICATION MESSAGES          *.
       F55BB.                                                           lv10
           STRING      ADV-MESSAGE DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N55BC.    NOTE *PUT PRINT MESSAGE AT FIRST         *.
       F55BC.                                                           lv15
           MOVE        1 TO WS00-MSGNUM
           MOVE        WS01-TMESSC TO WS00-TMESS4
           MOVE        WS00-MSGTXT TO WS91-MSGTXT
           STRING      IMP-MSG01 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55BC-FN. EXIT.
      *N55BD.    NOTE *LOOP THRU MSG TEXTS TO DISPLAY     *.
       F55BD.                                                           lv15
           MOVE        1                        TO J55BDR
                                    GO TO     F55BD-B.
       F55BD-A.
           ADD         1                        TO J55BDR.
       F55BD-B.
           IF          J55BDR                   >  QT91-QITEM
                                    GO TO     F55BD-FN.
      *N55BG.    NOTE *MOVE MESSAGE TEXT                  *.
       F55BG.                                                           lv20
           COMPUTE     WS00-MSGNUM = J55BDR + 1
           MOVE        QT91-TMESS4 (J55BDR) TO WS00-TMESS4
           MOVE        WS00-MSGTXT TO WS91-MSGTXT.
       F55BG-FN. EXIT.
      *N55BL.    NOTE *STRING IMP MSG TEXT # 1            *.
       F55BL.                                                           lv20
           STRING      IMP-MSG01 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55BL-FN. EXIT.
       F55BD-900. GO TO F55BD-A.
       F55BD-FN. EXIT.
      *N55BT.    NOTE *STRING IMP MSG TEXT # 2            *.
       F55BT.                                                           lv15
           STRING      IMP-MSG02 DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55BT-FN. EXIT.
       F55BB-FN. EXIT.
      *N55DB.    NOTE *FORMAT COMPLIANCE QUESTIONS        *.
       F55DB.                                                           lv10
           INITIALIZE  COMP-QUEST.
      *N55DD.    NOTE *CHECK CLIENT PROSPECTUS IND        *.
       F55DD.                                                           lv15
                 IF    QT91-CCLPR = 'Y'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLPR-YES
                 ELSE
           MOVE        '<TD></TD          ' TO CCLPR-YES.
                 IF    QT91-CCLPR = 'N'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLPR-NO
                 ELSE
           MOVE        '<TD></TD          ' TO CCLPR-NO.
                 IF    QT91-CCLPR = 'Z'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLPR-NA
                 ELSE
           MOVE        '<TD></TD          ' TO CCLPR-NA.
       F55DD-FN. EXIT.
      *N55DG.    NOTE *CHECK CLIENT CHARGES IND           *.
       F55DG.                                                           lv15
                 IF    QT91-CCLCH = 'Y'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLCH-YES
                 ELSE
           MOVE        '<TD></TD          ' TO CCLCH-YES.
                 IF    QT91-CCLCH = 'N'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLCH-NO
                 ELSE
           MOVE        '<TD></TD          ' TO CCLCH-NO.
                 IF    QT91-CCLCH = 'Z'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLCH-NA
                 ELSE
           MOVE        '<TD></TD          ' TO CCLCH-NA.
       F55DG-FN. EXIT.
      *N55DI.    NOTE *CHECK CLIENT SUITABILITY IND       *.
       F55DI.                                                           lv15
                 IF    QT91-CCLSU = 'Y'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLSU-YES
                 ELSE
           MOVE        '<TD></TD          ' TO CCLSU-YES.
                 IF    QT91-CCLSU = 'N'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLSU-NO
                 ELSE
           MOVE        '<TD></TD          ' TO CCLSU-NO.
                 IF    QT91-CCLSU = 'Z'                                 DOT
           MOVE        '<TD><B>X</B       ' TO CCLSU-NA
                 ELSE
           MOVE        '<TD></TD          ' TO CCLSU-NA.
       F55DI-FN. EXIT.
      *N55DK.    NOTE *GENERATE CASE DOC ID               *.
       F55DK.                                                           lv15
           PERFORM     F92BB THRU F92BB-FN
           MOVE        WT01-NIOBJC TO HTML-NIOBJC.
       F55DK-FN. EXIT.
      *N55DL.    NOTE *STRING DIV TAGS ONLY IF ALL        *.
       F55DL.    IF    QT91-CCLPR = 'Z'                                 lv15
                 AND   QT91-CCLCH = 'Z'
                 AND   QT91-CCLSU = 'Z'
                 NEXT SENTENCE ELSE GO TO     F55DL-FN.
      *COMPLIANCE ANSWERS ARE 'N/A'
           STRING      COMP-DIV-TAGS DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55DL-FN. EXIT.
      *N55DM.    NOTE *STRING COMPLIANCE QUESTIONS        *.
       F55DM.                                                           lv15
           STRING      COMP-QUEST DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F55DM-FN. EXIT.
       F55DB-FN. EXIT.
       F55-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *POPULATE OUTPUT FIELDS             *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
           MOVE        1 TO TALLI.
      *N65GB.    NOTE *UNSTRING HTML BLOB                 *.
       F65GB.                                                           lv10
           MOVE        1 TO HTML-PT.
      *N65GD.    NOTE *LOOP THRU XML-BLOB                 *.
       F65GD.                                                           lv15
           MOVE        1                        TO J65GDR
                                    GO TO     F65GD-B.
       F65GD-A.
           ADD         1                        TO J65GDR.
       F65GD-B.
           IF          J65GDR                   >  HTML-LIMIT
                                    GO TO     F65GD-FN.
           MOVE        HTML-PT TO TEMP-PT
           MOVE        TEMP-PT TO HTML-PT
           UNSTRING    HTML-BLOB  INTO  HTML-TEXT
                WITH  POINTER HTML-PT.
      *N65GG.    NOTE *MOVE WS VARIABLE INTO THTML        *.
       F65GG.    IF    HTML-TEXT NOT = SPACES                           lv20
                 NEXT SENTENCE ELSE GO TO     F65GG-FN.
           MOVE        01 TO QT91-CHTML (TALLI)
           MOVE        HTML-TEXT TO QT91-THTML (TALLI)
           ADD         1 TO TALLI.
       F65GG-FN. EXIT.
       F65GD-900. GO TO F65GD-A.
       F65GD-FN. EXIT.
       F65GB-FN. EXIT.
      *N65PB.    NOTE *MOVE END OF FILE MARKER            *.
       F65PB.                                                           lv10
           MOVE        99 TO QT91-CHTML (TALLI)
           MOVE        SPACES TO QT91-THTML (TALLI).
      *N65PF.    NOTE *CALCULATE THTML LENGTH             *.
       F65PF.                                                           lv15
           COMPUTE     QT91-QBLCK = TALLI.
       F65PF-FN. EXIT.
       F65PB-FN. EXIT.
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
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BB.    NOTE *GENERATE SOURCE DOC ID             *.            ANIOBJ
       F92BB.         EXIT.                                             lv10
      *N92CB.    NOTE *UNFORMAT TS                        *.            ANIOBJ
       F92CB.                                                           lv15
           MOVE        QT91-DXTMS2 TO 7-DB2-DXTMSA                      ANIOBJ
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
           MOVE        WT01-XDAJP TO 7-CASE-DTJUL
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
