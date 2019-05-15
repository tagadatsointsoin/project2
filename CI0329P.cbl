       IDENTIFICATION DIVISION.                                         CI0329
       PROGRAM-ID.  CI0329P.                                            CI0329
      *AUTHOR.         BUILD SPO HTML PAGE.                             CI0329
      *DATE-COMPILED.   09/08/14.                                       CI0329
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
       ENVIRONMENT DIVISION.                                            CI0329
       CONFIGURATION SECTION.                                           CI0329
       SOURCE-COMPUTER. IBM-370.                                        CI0329
       OBJECT-COMPUTER. IBM-370.                                        CI0329
       DATA DIVISION.                                                   CI0329
       WORKING-STORAGE SECTION.                                         CI0329
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
                        PICTURE 99B99B9999.                             CI0329
      *!WS
           05  7-DE00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0329
      *!WS
           05  7-DE00-GEEND
                        PICTURE 99B99B9999.                             CI0329
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
       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WD115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0329
      *!WI pl=WD120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0329
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0329
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0329
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0329
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0329
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0329
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0329
      *MISCELLANEOUS FIELDS

       01  WS00-WORKAREA.
           05 WS00-CTID.
              10 WS00-PREFIX PIC X(4) VALUE '0000'.
              10 FILLER      PIC X    VALUE SPACE.
      *!WS
              10 WS00-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0329
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

           05 WS01-CTID.
              10 WS01-PREFIX PIC X(4) VALUE '0000'.
              10 FILLER      PIC X    VALUE SPACE.
      *!WS
              10 WS01-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0329
              10 FILLER      PIC X    VALUE SPACE.
              10 WS01-GECKD2 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS01-CTIDA  PIC X(3) VALUE SPACES.

          05 WS00-MSGTXT.
             10 WS00-MSGNUM PIC Z9.
             10 WS00-DOT    PIC X     VALUE '.'.
             10 FILLER      PIC X     VALUE SPACE.
      *!WI
             10 WS00-TMESS4
                        PICTURE X(512).                                 CI0329

      *!WS
          05 WS00-APMTD
                        PICTURE $$,$$$,$$$,$$9.99.                      CI0329

          05 WS00-COUNT    PIC 9(8)    VALUE ZEROS.

      *!WS
          05 WS00-MPMTF                VALUE SPACES
                        PICTURE X(14).                                  CI0329

          05 WS00-PWHLD    PIC ZZ9.99- VALUE ZEROES.
      ******************************************************************
      *VARIABLE 'TIMER' IS USED TO HOLD THE TRANSACTION RECEIVED TIME.
      ******************************************************************
       01  WS00-TIMER.
      *!WI
           05  WS00-DTTHH
                        PICTURE 9(2).                                   CI0329
      *!WI
           05  WS00-DTTMN
                        PICTURE 9(2).                                   CI0329
      *!WI
           05  WS00-DTTSS
                        PICTURE 9(2).                                   CI0329
      *
       01  WS00-AMPM     PIC X(03) VALUE SPACES.
      *
       01  WS00-CST      PIC X(04) VALUE SPACES.
       01  WS00-DCACG    PIC 9(08) VALUE ZEROES.
      ******************************************************************
      *DISCRIPTION FOR REQUIRED MINIMUM DESTRIBUTION
      ******************************************************************
       01  WS00-TXRMDY      PIC X(03) VALUE 'YES'.
       01  WS00-TXRMDN      PIC X(03) VALUE 'NO '.
       01  WS00-TXRMDA      PIC X(03) VALUE 'N/A'.
      **----------------------------------------------------------------ANIOBJ
      **  DATE WORK AREA USED BY MACRO AADA58                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
      *!WF DSP=WT DSL=DD SEL=01 FOR=I LEV=1                             ANIOBJ
       01                 WT00.                                         CI0329
          05              WT00-SUITE.                                   CI0329
            15       FILLER         PICTURE  X(00093).                  CI0329
       01                 WT01  REDEFINES      WT00.                    CI0329
            10            WT01-XDAT8.                                   CI0329
            11            WT01-XDATC  PICTURE  XX.                      CI0329
            11            WT01-XDATY  PICTURE  XX.                      CI0329
            11            WT01-XDATM  PICTURE  XX.                      CI0329
            11            WT01-XDATD  PICTURE  XX.                      CI0329
            10            WT01-XDAT8D                                   CI0329
                          REDEFINES            WT01-XDAT8               CI0329
               PICTURE    9(8).                                         CI0329
            10            WT01-XDAT81.                                  CI0329
            11            WT01-XDATM1 PICTURE  XX.                      CI0329
            11            WT01-XDATD1 PICTURE  XX.                      CI0329
            11            WT01-XDATC1 PICTURE  XX.                      CI0329
            11            WT01-XDATY1 PICTURE  XX.                      CI0329
            10            WT01-XDAT80                                   CI0329
                          REDEFINES            WT01-XDAT81              CI0329
               PICTURE    9(8).                                         CI0329
            10            WT01-XDAT62.                                  CI0329
            11            WT01-XDATM2 PICTURE  XX.                      CI0329
            11            WT01-XDATD2 PICTURE  XX.                      CI0329
            11            WT01-XDATY2 PICTURE  XX.                      CI0329
            10            WT01-XDAT69                                   CI0329
                          REDEFINES            WT01-XDAT62              CI0329
               PICTURE    9(6).                                         CI0329
            10            WT01-XDATCU.                                  CI0329
            11            WT01-XDATC9 PICTURE  99.                      CI0329
            11            WT01-XDAYMD.                                  CI0329
            12            WT01-XDATY9 PICTURE  99.                      CI0329
            12            WT01-XDAMD.                                   CI0329
            13            WT01-XDATM9 PICTURE  99.                      CI0329
            13            WT01-XDATD9 PICTURE  99.                      CI0329
            10            WT01-XDAT89 PICTURE  9(8).                    CI0329
            10            WT01-XDAJC  PICTURE  9(7).                    CI0329
            10            WT01-XDAJC1.                                  CI0329
            11            WT01-XDAJC9 PICTURE  99.                      CI0329
            11            WT01-XDAJY  PICTURE  99.                      CI0329
            11            WT01-XDAJN  PICTURE  999.                     CI0329
            10            WT01-XDAB   PICTURE  9(5).                    CI0329
            10            WT01-DD05.                                    CI0329
            11            WT01-XDACT  PICTURE  S9(3)                    CI0329
                          COMPUTATIONAL-3.                              CI0329
            11            WT01-XDACV  PICTURE  S9                       CI0329
                          COMPUTATIONAL-3.                              CI0329
            11            WT01-XDAGP  PICTURE  S9(9)                    CI0329
                          COMPUTATIONAL-3.                              CI0329
            11            WT01-XDAJP  PICTURE  S9(7)                    CI0329
                          COMPUTATIONAL-3.                              CI0329
            11            WT01-XDACV1 PICTURE  S9                       CI0329
                          COMPUTATIONAL-3.                              CI0329
            11            WT01-XDAGP1 PICTURE  S9(9)                    CI0329
                          COMPUTATIONAL-3.                              CI0329
            11            WT01-XDAJP1 PICTURE  S9(7)                    CI0329
                          COMPUTATIONAL-3.                              CI0329
            10            WT01-XW03.                                    CI0329
            11            WT01-XDATG.                                   CI0329
            12            WT01-XDAT1.                                   CI0329
            13            WT01-XDAT19 PICTURE  99.                      CI0329
            12            WT01-XDAT2.                                   CI0329
            13            WT01-XDAT29 PICTURE  99.                      CI0329
            12            WT01-XDAT3.                                   CI0329
            13            WT01-XDAT39 PICTURE  99.                      CI0329
            12            WT01-XDAT4.                                   CI0329
            13            WT01-XDAT49 PICTURE  99.                      CI0329
            11            WT01-XLEAPY PICTURE  99.                      CI0329
            11            WT01-DTGCY  PICTURE  9(4).                    CI0329
            11            WT01-FILLER                                   CI0329
                          REDEFINES            WT01-DTGCY.              CI0329
            12            WT01-DTGCC  PICTURE  9(2).                    CI0329
            12            WT01-DTGYY  PICTURE  9(2).                    CI0329
                                                                        ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      **  WORKING STORAGE FOR CASE DOC DATE                             ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      *!WI pl=WT115                                                     ANIOBJ
       01 7-CSTDTE                                                      ANIOBJ
                        PICTURE X(8).                                   CI0329
       01 FILLER REDEFINES 7-CSTDTE.                                    ANIOBJ
      *!WI pl=WT125                                                     ANIOBJ
          05 7-DTGCY                                                    ANIOBJ
                        PICTURE 9(4).                                   CI0329
      *!WI pl=WT130                                                     ANIOBJ
          05 7-DTGMM                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0329
      *!WI pl=WT135                                                     ANIOBJ
          05 7-DTGDD                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0329
      *                                                                 ANIOBJ
      *!WI pl=WT145                                                     ANIOBJ
       01 7-CASE-DTJUL                                                  ANIOBJ
                        PICTURE 9(7).                                   CI0329
       01 FILLER REDEFINES 7-CASE-DTJUL.                                ANIOBJ
      *!WI pl=WT150                                                     ANIOBJ
          05 7-CASE-DTGCY                                               ANIOBJ
                        PICTURE 9(4).                                   CI0329
      *!WI pl=WT155                                                     ANIOBJ
          05 7-CASE-DTJDD                                               ANIOBJ
                        PICTURE 9(3).                                   CI0329
      *                                                                 ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      ** CASEDOC-ID GENERATED                                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
       01 WT01-NIOBJC.                                                  ANIOBJ
         05 WT01-DTTCY    PIC 9.                                        ANIOBJ
      *!WI pl=WT270                                                     ANIOBJ
         05 WT01-DTJDD                                                  ANIOBJ
                        PICTURE 9(3).                                   CI0329
      *!WI pl=WT280                                                     ANIOBJ
         05 WT01-DTTHH                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0329
      *!WI pl=WT290                                                     ANIOBJ
         05 WT01-DTTMN                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0329
      *!WI pl=WT300                                                     ANIOBJ
         05 WT01-DTTSS                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0329
         05 WT01-DTTNN    PIC 9(3).                                     ANIOBJ
         05 FILLER        PIC X(4)    VALUE '.001'.                     ANIOBJ
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58
       01   DEBUT-WSS.                                                  CI0329
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0329
            05   IK     PICTURE X.                                      CI0329
       01  CONSTANTES-PAC.                                              CI0329
           05  FILLER  PICTURE X(87)   VALUE                            CI0329
                     '6015 CAT09/08/14CI0329ADMIN   14:35:21CI0329P AMERCI0329
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0329
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0329
           05  NUGNA   PICTURE X(5).                                    CI0329
           05  APPLI   PICTURE X(3).                                    CI0329
           05  DATGN   PICTURE X(8).                                    CI0329
           05  PROGR   PICTURE X(6).                                    CI0329
           05  CODUTI  PICTURE X(8).                                    CI0329
           05  TIMGN   PICTURE X(8).                                    CI0329
           05  PROGE   PICTURE X(8).                                    CI0329
           05  COBASE  PICTURE X(4).                                    CI0329
           05  DATGNC  PICTURE X(10).                                   CI0329
           05  RELEAS  PICTURE X(7).                                    CI0329
           05  DATGE   PICTURE X(10).                                   CI0329
           05  DATSQ   PICTURE X(10).                                   CI0329
       01  DATCE.                                                       CI0329
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0329
         05  DATOR.                                                     CI0329
           10  DATOA  PICTURE XX.                                       CI0329
           10  DATOM  PICTURE XX.                                       CI0329
           10  DATOJ  PICTURE XX.                                       CI0329
       01  DAT6.                                                        CI0329
            10 DAT61.                                                   CI0329
            15 DAT619  PICTURE 99.                                      CI0329
            10 DAT62.                                                   CI0329
            15 DAT629  PICTURE 99.                                      CI0329
            10 DAT63   PICTURE XX.                                      CI0329
       01  DAT8.                                                        CI0329
            10 DAT81   PICTURE XX.                                      CI0329
            10 DAT8S1  PICTURE X.                                       CI0329
            10 DAT82   PICTURE XX.                                      CI0329
            10 DAT8S2  PICTURE X.                                       CI0329
            10 DAT83   PICTURE XX.                                      CI0329
       01  DAT8E    REDEFINES    DAT8.                                  CI0329
            10 DAT81E  PICTURE X(4).                                    CI0329
            10 DAT82E  PICTURE XX.                                      CI0329
            10 DAT83E  PICTURE XX.                                      CI0329
       01  DAT6C.                                                       CI0329
            10  DAT61C PICTURE XX.                                      CI0329
            10  DAT62C PICTURE XX.                                      CI0329
            10  DAT63C.                                                 CI0329
             15 DAT63CC PICTURE XX.                                     CI0329
             15 DAT64C  PICTURE XX.                                     CI0329
       01  DAT8C.                                                       CI0329
            10  DAT81C  PICTURE XX.                                     CI0329
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0329
            10  DAT82C  PICTURE XX.                                     CI0329
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0329
            10  DAT83C.                                                 CI0329
             15 DAT83CC PICTURE XX.                                     CI0329
             15 DAT84C  PICTURE XX.                                     CI0329
       01  DATSEP     PICTURE X VALUE '/'.                              CI0329
       01  DATSEW     PICTURE X.                                        CI0329
       01   VARIABLES-CONDITIONNELLES.                                  CI0329
            05                  FT      PICTURE X VALUE '0'.            CI0329
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0329
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0329
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J65BNR PICTURE S9(4) VALUE  ZERO.
            05           J70BDR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0329
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT.
      *THIS COPYBOOK CONTAINS THE HEADER, SOURCE ACCOUNT DETAILS AND
      *DESTINATION ACCOUNT DETAILS FOR THE HTML PAGE.
      ******************************************************************
       COPY CI0329C1.
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT
      *THIS COPYBOOOK CONTAINS THE PAYMENT DETAILS AND THE COMPLIANCE
      *QUESTIONS FOR THE HTML PAGE.
      ******************************************************************
       COPY CI0329C2.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0329                *
      ******************************************************************
      *!WF DSP=V2 DSL=V2 SEL=88 FOR=I LEV=1 PLT=80
       01                 V200.                                         CI0329
          05              V200-SUITE.                                   CI0329
            15       FILLER         PICTURE  X(58625).                  CI0329
       01                 V288  REDEFINES      V200.                    CI0329
            10            V288-CTID.                                    CI0329
            11            V288-CTIDA  PICTURE  9(3).                    CI0329
            11            V288-CTIDN.                                   CI0329
            12            V288-CTIDNP PICTURE  X(13).                   CI0329
            12            V288-CTIDND PICTURE  9(11).                   CI0329
            10            V288-GECKD1 PICTURE  9.                       CI0329
            10            V288-APMTD  PICTURE  S9(11)V99                CI0329
                          COMPUTATIONAL-3.                              CI0329
            10            V288-PWHLD  PICTURE  S999V9(5)                CI0329
                          COMPUTATIONAL-3.                              CI0329
            10            V288-TWITH  PICTURE  X(12).                   CI0329
            10            V288-QITEM  PICTURE  9(3).                    CI0329
            10            V288-QBLCK  PICTURE  9(6).                    CI0329
            10            V288-DCACG  PICTURE  9(8).                    CI0329
            10            V288-DNPMT  PICTURE  9(8).                    CI0329
            10            V288-GEEND  PICTURE  9(8).                    CI0329
            10            V288-DXTMS2 PICTURE  X(26).                   CI0329
            10            V288-MPLNR2 PICTURE  X(40).                   CI0329
            10            V288-MRPSN  PICTURE  X(12).                   CI0329
            10            V288-MRPSN3 PICTURE  X(12).                   CI0329
            10            V288-CCONF  PICTURE  X(25).                   CI0329
            10            V288-ATROLL PICTURE  X(25).                   CI0329
            10            V288-GEOPD2 PICTURE  X(8).                    CI0329
            10            V288-NGEOPA PICTURE  X(08).                   CI0329
            10            V288-PRCMN  PICTURE  X(20).                   CI0329
            10            V288-CTTLN1 PICTURE  X(30).                   CI0329
            10            V288-CTTLN2 PICTURE  X(30).                   CI0329
            10            V288-CTTLN3 PICTURE  X(30).                   CI0329
            10            V288-CTTBO1 PICTURE  X(45).                   CI0329
            10            V288-CTTBO2 PICTURE  X(45).                   CI0329
            10            V288-MPRN4  PICTURE  X(35).                   CI0329
            10            V288-CTID01.                                  CI0329
            11            V288-CACTID PICTURE  9(3).                    CI0329
            11            V288-CTIDNB.                                  CI0329
            12            V288-CTIDP1 PICTURE  X(13).                   CI0329
            12            V288-CTIDNA PICTURE  9(11).                   CI0329
            10            V288-GECKD2 PICTURE  9.                       CI0329
            10            V288-PRCMN1 PICTURE  X(20).                   CI0329
            10            V288-MPMTT  PICTURE  X(20).                   CI0329
            10            V288-MPMTF  PICTURE  X(14).                   CI0329
            10            V288-CDETY  PICTURE  XX.                      CI0329
            10            V288-CDEST  PICTURE  99.                      CI0329
            10            V288-CACTS  PICTURE  X.                       CI0329
            10            V288-CATLN1 PICTURE  X(30).                   CI0329
            10            V288-CATLN2 PICTURE  X(30).                   CI0329
            10            V288-CATLN3 PICTURE  X(30).                   CI0329
            10            V288-CATBO1 PICTURE  X(45).                   CI0329
            10            V288-CATBO2 PICTURE  X(45).                   CI0329
            10            V288-MPRN4B PICTURE  X(35).                   CI0329
            10            V288-GESAD1 PICTURE  X(30).                   CI0329
            10            V288-GESAD2 PICTURE  X(30).                   CI0329
            10            V288-GESAD3 PICTURE  X(30).                   CI0329
            10            V288-GECIT  PICTURE  X(25).                   CI0329
            10            V288-GEST   PICTURE  X(8).                    CI0329
            10            V288-GEPCD  PICTURE  X(12).                   CI0329
            10            V288-CLORN  PICTURE  X(45).                   CI0329
            10            V288-NTR    PICTURE  9(8).                    CI0329
            10            V288-GECKD3 PICTURE  9.                       CI0329
            10            V288-NPBN   PICTURE  X(20).                   CI0329
            10            V288-TTBAL  PICTURE  X(15).                   CI0329
            10            V288-MCSIG  PICTURE  X(30).                   CI0329
            10            V288-CPROCM PICTURE  X.                       CI0329
            10            V288-CSLCT  PICTURE  X.                       CI0329
            10            V288-CTRHO  PICTURE  9(8).                    CI0329
            10            V288-GETOD  PICTURE  9(6).                    CI0329
            10            V288-TMESS4 PICTURE  X(512)                   CI0329
                          OCCURS       020     TIMES.                   CI0329
            10            V288-CCLPR  PICTURE  X.                       CI0329
            10            V288-CCLCH  PICTURE  X.                       CI0329
            10            V288-CCLSU  PICTURE  X.                       CI0329
            10            V288-MCLNM5 PICTURE  X(45).                   CI0329
            10            V288-QT9D.                                    CI0329
            11            V288-QT9B                                     CI0329
                          OCCURS       233     TIMES.                   CI0329
            12            V288-CHTML  PICTURE  99.                      CI0329
            12            V288-THTML  PICTURE  X(200).                  CI0329
            10            V288-NGEOR  PICTURE  9(08).                   CI0329
            10            V288-IDRMD  PICTURE  X.                       CI0329
            10            V288-MPRN4X PICTURE  X(100).                  CI0329
            10            V288-MPRN4Y PICTURE  X(100).                  CI0329
            10            V288-CGMBR  PICTURE  X.                       CI0329
            10            V288-IAPGP  PICTURE  X.                       CI0329
            10            V288-DWAIT  PICTURE  9(8).                    CI0329
            10            V288-FILLER PICTURE  X(004).                  CI0329
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0329
          05              MS00-SUITE.                                   CI0329
            15       FILLER         PICTURE  X(00542).                  CI0329
       01                 MS03  REDEFINES      MS00.                    CI0329
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0329
                          COMPUTATIONAL-3.                              CI0329
            10            MS03-CMSSF  PICTURE  XX.                      CI0329
            10            MS03-DU09.                                    CI0329
            11            MS03-CMESA  PICTURE  S9(9)                    CI0329
                          BINARY.                                       CI0329
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0329
                          BINARY.                                       CI0329
            11            MS03-CMESB  PICTURE  S9(9)                    CI0329
                          BINARY.                                       CI0329
            11            MS03-CMSST  PICTURE  S9(9)                    CI0329
                          BINARY.                                       CI0329
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0329
                          BINARY.                                       CI0329
            11            MS03-QELLAA PICTURE  S9(9)                    CI0329
                          BINARY.                                       CI0329
            11            MS03-TMESS4 PICTURE  X(512).                  CI0329
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0329
            10            MX11-QMSGS  PICTURE  9(03).                   CI0329
            10            MX11-PJ09                                     CI0329
                          OCCURS       025     TIMES.                   CI0329
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0329
                          COMPUTATIONAL-3.                              CI0329
            11            MX11-CMESB  PICTURE  S9(9)                    CI0329
                          BINARY.                                       CI0329
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V288
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0329
      *               *                                   *             CI0329
      *               *INITIALISATIONS                    *             CI0329
      *               *                                   *             CI0329
      *               *************************************.            CI0329
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0329
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0329
      *               *                                   *             CI0329
      *               *FIN DE TRAITEMENT                  *             CI0329
      *               *                                   *             CI0329
      *               *************************************.            CI0329
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0329
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
       F25BB.    IF    V288-APMTD NUMERIC                               lv10
                 AND   V288-QITEM NUMERIC
                 AND   V288-QBLCK NUMERIC
                 AND   V288-DCACG NUMERIC
                 AND   V288-GEEND NUMERIC
                 AND   V288-DNPMT NUMERIC
                 AND   V288-CTRHO NUMERIC
                 NEXT SENTENCE ELSE GO TO     F25BB-FN.
       F25BB-900. GO TO F25BD-FN.
       F25BB-FN. EXIT.
      *N25BD.    NOTE *IF ANY OF THEM NOT NUMERIC         *.
       F25BD.                                                           lv10
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014222 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25BD-FN. EXIT.
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
      *
           MOVE        1 TO HTML-PT
           STRING      HTML-COMMON-HDR-TAGS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35AT-FN. EXIT.
      *N35BA.    NOTE *MOVE CONFIRM INFO                  *.
       F35BA.    IF    V288-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35BA-FN.
      *N35BB.    NOTE *MOVE CONFIRM DETAILS               *.
       F35BB.                                                           lv15
      *
           MOVE        V288-MPLNR2 TO HTML-MPLNR2
           MOVE        V288-ATROLL TO HTML-ATROLL
           MOVE        V288-GEOPD2 TO HTML-GEOPD2
           MOVE        V288-NGEOPA TO HTML-GEOPD3
           MOVE        V288-CCONF TO HTML-CCONF.
                 IF    V288-CSLCT = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CSLCT
           MOVE        V288-NGEOR TO HTML-NGEOR
                 ELSE
           MOVE        'NO' TO HTML-CSLCT
           MOVE        'N/A' TO HTML-NGEOR.
       F35BB-FN. EXIT.
      *N35BD.    NOTE *FORMAT THE CURRENT DATE,           *.
       F35BD.                                                           lv15
      *EFFECTIVE DATE AND SEND TRANS
      *RECEIVED TIME
      *
      *!ADS "V288-DCACG      7-DE00-DCACG"
           MOVE        V288-DCACG                                       CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT81E TO DAT63C                                        CI0329
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0329
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0329
      *!ADM "7-DE00-DCACG    HTML-DEFFT"
           MOVE        7-DE00-DCACG                                     CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0329
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0329
           MOVE   DAT8C TO  HTML-DEFFT                                  CI0329
      *
           MOVE FUNCTION CURRENT-DATE(1:8)
                        TO  WS00-DCACG
      *!ADS "WS00-DCACG      7-DE00-DCACG"
           MOVE        WS00-DCACG                                       CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT81E TO DAT63C                                        CI0329
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0329
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0329
      *!ADM "7-DE00-DCACG    HTML-DCACG"
           MOVE        7-DE00-DCACG                                     CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0329
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0329
           MOVE   DAT8C TO  HTML-DCACG                                  CI0329
      *
           MOVE FUNCTION CURRENT-DATE(9:6)
                        TO  WS00-TIMER.
                 IF    WS00-DTTHH (1:2) < 12                            DOT
           MOVE        'AM' TO WS00-AMPM
                 ELSE
           MOVE        'PM' TO WS00-AMPM.
                 IF    WS00-DTTHH (1:2) > 12                            DOT
           SUBTRACT    12 FROM WS00-DTTHH.
                 IF    WS00-DTTHH (1:2) = 00                            DOT
           ADD         12 TO WS00-DTTHH.
           MOVE        'CST' TO WS00-CST                                DOT
           STRING      WS00-DTTHH ':'
           WS00-DTTMN ':'
           WS00-DTTSS ' '
           WS00-AMPM
           WS00-CST
           DELIMITED BY SIZE INTO
           HTML-TIMER.
       F35BD-FN. EXIT.
      *N35BE.    NOTE *FORMAT THE ORDER RECEIVED DATE     *.
       F35BE.                                                           lv15
      *AND SEND THE ORDER RECEIVED TIME
           INITIALIZE  WS00-TIMER
      *!ADS "V288-CTRHO      7-DE00-DCACG"
           MOVE        V288-CTRHO                                       CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT81E TO DAT63C                                        CI0329
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0329
           MOVE   DAT6C TO  7-DE00-DCACG                                CI0329
      *!ADM "7-DE00-DCACG    HTML-DRECD"
           MOVE        7-DE00-DCACG                                     CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0329
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0329
           MOVE   DAT8C TO  HTML-DRECD                                  CI0329
      *
           MOVE        V288-GETOD TO WS00-TIMER.
                 IF    WS00-DTTHH (1:2) < 12                            DOT
           MOVE        'AM' TO WS00-AMPM
                 ELSE
           MOVE        'PM' TO WS00-AMPM.
                 IF    WS00-DTTHH (1:2) > 12                            DOT
           SUBTRACT    12 FROM WS00-DTTHH.
                 IF    WS00-DTTHH = 00                                  DOT
           ADD         12 TO WS00-DTTHH.
           STRING      WS00-DTTHH ':'                                   DOT
           WS00-DTTMN ':'
           WS00-DTTSS ' '
           WS00-AMPM
           WS00-CST
           DELIMITED BY SIZE INTO
           HTML-TIMER1.
       F35BE-FN. EXIT.
      *N35BM.    NOTE *STRING CONFIRMATION INFORMATION    *.
       F35BM.                                                           lv15
      *
           STRING      HTML-CONFIRM-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BM-FN. EXIT.
       F35BA-FN. EXIT.
      *N35CB.    NOTE *MOVE VERIFY INFORMATION            *.
       F35CB.    IF    V288-CACTS = 'V'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35CB-FN.
      *N35CM.    NOTE *STRING VERIFY INFORMATION          *.
       F35CM.                                                           lv15
      *
           STRING      HTML-VERIFY-STEP1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35CM-FN. EXIT.
       F35CB-FN. EXIT.
       F35-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *BUILD SOURCE ACCOUNT DETAILS       *
      *               *                                   *
      *               *************************************.
       F45.           EXIT.                                             lv05
      *N45BB.    NOTE *FOR VERIFY ADD REQUEST             *.
       F45BB.    IF    V288-CACTS = 'V'                                 lv10
                 AND   V288-CPROCM = 'A'
                 NEXT SENTENCE ELSE GO TO     F45BB-FN.
           STRING      HTML-ADD-VERIFY-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BB-FN. EXIT.
      *N45BE.    NOTE *FOR VERIFY MODIFY REQUEST          *.
       F45BE.    IF    V288-CACTS = 'V'                                 lv10
                 AND   V288-CPROCM = 'M'
                 NEXT SENTENCE ELSE GO TO     F45BE-FN.
           STRING      HTML-MOD-VERIFY-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BE-FN. EXIT.
      *N45BH.    NOTE *BUILD PRODUCT NAME                 *.
       F45BH.                                                           lv10
      *
           STRING      V288-PRCMN
           DELIMITED BY LOW-VALUES
           INTO HTML-PRCMN.
       F45BH-FN. EXIT.
      *N45BJ.    NOTE *STRING FROM ACCOUNT INFO           *.
       F45BJ.                                                           lv10
      *
           STRING      HTML-FROM-ACCOUNT-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BJ-FN. EXIT.
      *N45BK.    NOTE *STRING FROM ACCOUNT PLAN INFO      *.
       F45BK.    IF    V288-MRPSN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F45BK-FN.
      *
           STRING      V288-MRPSN3
           DELIMITED BY LOW-VALUES
           INTO HTML-MRPSN3
      *
           STRING      HTML-FROM-PLAN-NAME
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BK-FN. EXIT.
      *N45BL.    NOTE *BUILD OWNERSHIP LINE #1            *.
       F45BL.                                                           lv10
      *
           STRING      V288-CTTLN1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN1
      *
           STRING      HTML-FROM-OWNER-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BL-FN. EXIT.
      *N45BN.    NOTE *BUILD OWNERSHIP LINE #2            *.
       F45BN.    IF    V288-CTTLN2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F45BN-FN.
      *
           STRING      V288-CTTLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN2
      *
           STRING      HTML-FROM-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BN-FN. EXIT.
      *N45BP.    NOTE *BUILD OWNERSHIP LINE #3            *.
       F45BP.    IF    V288-CTTLN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F45BP-FN.
      *
           STRING      V288-CTTLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN3
      *
           STRING      HTML-FROM-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BP-FN. EXIT.
      *N45BR.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F45BR.    IF    V288-CTTBO1 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F45BR-FN.
      *
           STRING      V288-CTTBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
      *
           STRING      HTML-FROM-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BR-FN. EXIT.
      *N45BT.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F45BT.    IF    V288-CTTBO2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F45BT-FN.
      *
           STRING      V288-CTTBO2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO2
      *
           STRING      HTML-FROM-BENE-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45BT-FN. EXIT.
      *N45EB.    NOTE *FORMAT CONTRACT ID                 *.
       F45EB.                                                           lv10
      *
           MOVE        V288-CTIDND TO WS00-NCTIDN
           MOVE        V288-GECKD1 TO WS00-GECKD1
           MOVE        V288-CTIDA TO WS00-CTIDA
      *
           MOVE        WS00-CTID TO HTML-CTID.
       F45EB-FN. EXIT.
      *N45EF.    NOTE *STRING FROM ACCOUNT NUMBER         *.
       F45EF.                                                           lv10
      *
           STRING      HTML-FROM-ACCOUNT-NBR
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45EF-FN. EXIT.
      *N45EI.    NOTE *BUILD STRATEGY NAME FOR BROK       *.
       F45EI.    IF    V288-CTIDA = 021                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45EI-FN.
      *ACCOUNTS
                 IF    V288-MPRN4 > SPACES                              DOT
           STRING      V288-MPRN4
           DELIMITED BY LOW-VALUES
           INTO HTML-MPRN4
      *
           STRING      HTML-FROM-STRATEGY-NAME
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45EI-FN. EXIT.
      *N45EJ.    NOTE *BUILD STRATEGY NAME FOR BETA       *.
       F45EJ.    IF    V288-CTIDA = 133                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45EJ-FN.
      *BROKERAGE ACCOUNTS
                 IF    V288-MPRN4X > SPACES                             DOT
           STRING      V288-MPRN4X
           DELIMITED BY LOW-VALUES
           INTO HTML-MPRN4
      *
           STRING      HTML-FROM-STRATEGY-NAME
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45EJ-FN. EXIT.
      *N45EL.    NOTE *POPULATE END OF FROM ACCOUNT       *.
       F45EL.                                                           lv10
      *TABLE
           STRING      HTML-FROM-ACCOUNT-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F45EL-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *BUILD DESTINATION DETAILS          *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BB.    NOTE *FOR MMTA ARRANGEMENT               *.
       F50BB.    IF    V288-CDETY = '01'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50BB-FN.
      *N50BE.    NOTE *BUILD PRODUCT NAME                 *.
       F50BE.                                                           lv15
      *
           STRING      V288-PRCMN1
           DELIMITED BY LOW-VALUES
           INTO HTML-PRCMN1.
       F50BE-FN. EXIT.
      *N50BG.    NOTE *STRING TO ACCOUNT INFO             *.
       F50BG.                                                           lv15
      *
           STRING      HTML-MMTA-TO-ACCOUNT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BG-FN. EXIT.
      *N50BH.    NOTE *STRING TO ACCOUNT PLAN INFO        *.
       F50BH.    IF    V288-MRPSN > SPACES                              lv15
                 NEXT SENTENCE ELSE GO TO     F50BH-FN.
      *
           STRING      V288-MRPSN
           DELIMITED BY LOW-VALUES
           INTO HTML-MRPSN
      *
           STRING      HTML-TO-PLAN-NAME
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BH-FN. EXIT.
      *N50BI.    NOTE *BUILD OWNERSHIP LINE #1            *.
       F50BI.                                                           lv15
      *
           STRING      V288-CATLN1
           DELIMITED BY LOW-VALUES
           INTO HTML-CATLN1
      *
           STRING      HTML-TO-OWNER-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BI-FN. EXIT.
      *N50BK.    NOTE *BUILD OWNERSHIP LINE #2            *.
       F50BK.    IF    V288-CATLN2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50BK-FN.
      *
           STRING      V288-CATLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CATLN2
      *
           STRING      HTML-TO-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BK-FN. EXIT.
      *N50BM.    NOTE *BUILD OWNERSHIP LINE #3            *.
       F50BM.    IF    V288-CATLN3 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50BM-FN.
      *
           STRING      V288-CATLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CATLN3
      *
           STRING      HTML-TO-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BM-FN. EXIT.
      *N50BO.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F50BO.    IF    V288-CATBO1 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50BO-FN.
      *
           STRING      V288-CATBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CATBO1
      *
           STRING      HTML-TO-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BO-FN. EXIT.
      *N50BQ.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F50BQ.    IF    V288-CATBO2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50BQ-FN.
      *
           STRING      V288-CATBO2
           DELIMITED BY LOW-VALUES
           INTO HTML-CATBO2
      *
           STRING      HTML-TO-BENE-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BQ-FN. EXIT.
      *N50CB.    NOTE *FORMAT CONTRACT ID                 *.
       F50CB.                                                           lv15
      *
           MOVE        V288-CTIDNA TO WS01-NCTIDN
           MOVE        V288-GECKD2 TO WS01-GECKD2
           MOVE        V288-CACTID TO WS01-CTIDA
      *
           MOVE        WS01-CTID TO HTML-CTID1.
       F50CB-FN. EXIT.
      *N50CF.    NOTE *STRING TO ACCOUNT INFO             *.
       F50CF.                                                           lv15
      *
           STRING      HTML-MMTA-TO-ACCOUNT-NBR
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CF-FN. EXIT.
      *N50CH.    NOTE *BUILD STRATEGY NAME FOR BROK       *.
       F50CH.    IF    V288-CACTID = 021                                lv15
                 NEXT SENTENCE ELSE GO TO     F50CH-FN.
      *
                 IF    V288-MPRN4B > SPACES                             DOT
           STRING      V288-MPRN4B
           DELIMITED BY LOW-VALUES
           INTO HTML-MPRN4B
      *
           STRING      HTML-TO-STRATEGY-NAME
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CH-FN. EXIT.
      *N50CI.    NOTE *BUILD STRATEGY NAME FOR BETA       *.
       F50CI.    IF    V288-CACTID = 133                                lv15
                 NEXT SENTENCE ELSE GO TO     F50CI-FN.
      *BROKERAGE
                 IF    V288-MPRN4Y > SPACES                             DOT
           STRING      V288-MPRN4Y
           DELIMITED BY LOW-VALUES
           INTO HTML-MPRN4B
      *
           STRING      HTML-TO-STRATEGY-NAME
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CI-FN. EXIT.
      *N50CJ.    NOTE *POPULATE END OF TO ACCOUNT TABLE   *.
       F50CJ.                                                           lv15
      *
           STRING      HTML-MMTA-TO-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CJ-FN. EXIT.
       F50BB-FN. EXIT.
      *N50EB.    NOTE *FOR ACH ARRANGEMENT                *.
       F50EB.    IF    V288-CDETY = '03'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50EB-FN.
      *N50EE.    NOTE *BUILD BANK NAME                    *.
       F50EE.                                                           lv15
      *
           MOVE        V288-CLORN TO HTML-CLORN.
       F50EE-FN. EXIT.
      *N50EH.    NOTE *BUILD ACCOUNT TYPE                 *.
       F50EH.                                                           lv15
      *
           MOVE        V288-TTBAL TO HTML-TTBAL.
       F50EH-FN. EXIT.
      *N50EK.    NOTE *BUILD CLIENT SIGNATURE             *.
       F50EK.                                                           lv15
      *
           STRING      V288-MCSIG
           DELIMITED BY LOW-VALUES
           INTO HTML-MCSIG.
       F50EK-FN. EXIT.
      *N50EN.    NOTE *FORMAT BANK RTN                    *.
       F50EN.                                                           lv15
      *
           STRING      V288-NTR '-' V288-GECKD3
           DELIMITED BY SIZE
           INTO HTML-RTN.
       F50EN-FN. EXIT.
      *N50EQ.    NOTE *BUILD BANK NUMBER                  *.
       F50EQ.                                                           lv15
      *
           MOVE        V288-NPBN TO HTML-NPBN.
       F50EQ-FN. EXIT.
      *N50ET.    NOTE *STRING ACH-TO ACCOUNT INFO         *.
       F50ET.                                                           lv15
      *
           STRING      HTML-ACH-TO-ACCOUNT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50ET-FN. EXIT.
       F50EB-FN. EXIT.
      *N50HB.    NOTE *FOR AOR OR CHECK TO CLIENT AT      *.
       F50HB.    IF    V288-CDETY = '02' OR '06'                        lv10
                 NEXT SENTENCE ELSE GO TO     F50HB-FN.
      *ALTERNATE ADDRESS
      *N50HD.    NOTE *BUILD AOR ARRANGEMENT              *.
       F50HD.                                                           lv15
      *
           STRING      HTML-AOR-TO-ACCOUNT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50HD-FN. EXIT.
      *N50HE.    NOTE *BUILD DESTINATION ADDR #1          *.
       F50HE.                                                           lv15
      *
           STRING      V288-GESAD1
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD1
      *
           STRING      HTML-TO-ADDR-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50HE-FN. EXIT.
      *N50HG.    NOTE *BUILD DESTINATION ADDR #2          *.
       F50HG.    IF    V288-GESAD2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50HG-FN.
      *
           STRING      V288-GESAD2
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD2
      *
           STRING      HTML-TO-ADDR-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50HG-FN. EXIT.
      *N50HI.    NOTE *BUILD DESTINATION ADDR #3          *.
       F50HI.    IF    V288-GESAD3 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50HI-FN.
      *
           STRING      V288-GESAD3
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD3
      *
           STRING      HTML-TO-ADDR-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50HI-FN. EXIT.
      *N50HL.    NOTE *BUILD CITY NAME AND POSTAL CODE    *.
       F50HL.                                                           lv15
      *
           STRING      V288-GECIT ', '
           V288-GEST ' '
           V288-GEPCD
           DELIMITED BY SIZE INTO
           HTML-GESAD4.
       F50HL-FN. EXIT.
      *N50HR.    NOTE *STRING AOR DETAIL INFO             *.
       F50HR.                                                           lv15
      *
           STRING      HTML-TO-ADDR-LINE4
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50HR-FN. EXIT.
       F50HB-FN. EXIT.
      *N50HS.    NOTE *FOR CHECK TO BANK                  *.
       F50HS.    IF    V288-CDETY = '04'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50HS-FN.
      *N50HU.    NOTE *BUILD BANK NAME                    *.
       F50HU.                                                           lv15
      *
           MOVE        V288-CLORN TO HTML-CLORN1.
       F50HU-FN. EXIT.
      *N50HV.    NOTE *BUILD BANK NUMBER                  *.
       F50HV.                                                           lv15
      *
           MOVE        V288-NPBN TO HTML-NPBN1.
       F50HV-FN. EXIT.
      *N50HX.    NOTE *BUILD CLIENT SIGNATURE             *.
       F50HX.                                                           lv15
      *
           STRING      V288-MCSIG
           DELIMITED BY LOW-VALUES
           INTO HTML-MCSIG1.
       F50HX-FN. EXIT.
      *N50HY.    NOTE *BUILD ACCOUNT TYPE                 *.
       F50HY.                                                           lv15
      *
           MOVE        V288-TTBAL TO HTML-TTBAL1.
       F50HY-FN. EXIT.
      *N50HZ.    NOTE *STRING CHECK TO BANK INFO          *.
       F50HZ.                                                           lv15
      *
           STRING      HTML-CHK-TO-BANK
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50HZ-FN. EXIT.
      *N50IA.    NOTE *BUILD DESTINATION ADDR #1          *.
       F50IA.    IF    V288-GESAD1 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50IA-FN.
      *
           STRING      V288-GESAD1
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD5
      *
           STRING      HTML-TO-ADDR-LINE5
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IA-FN. EXIT.
      *N50IB.    NOTE *BUILD DESTINATION ADDR #2          *.
       F50IB.    IF    V288-GESAD2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50IB-FN.
      *
           STRING      V288-GESAD2
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD6
      *
           STRING      HTML-TO-ADDR-LINE6
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IB-FN. EXIT.
      *N50IC.    NOTE *BUILD DESTINATION ADDR #3          *.
       F50IC.    IF    V288-GESAD3 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50IC-FN.
      *
           STRING      V288-GESAD3
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD7
      *
           STRING      HTML-TO-ADDR-LINE7
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IC-FN. EXIT.
      *N50ID.    NOTE *BUILD CITY NAME AND POSTAL CODE    *.
       F50ID.                                                           lv15
      *
           STRING      V288-GECIT ', '
           V288-GEST ' '
           V288-GEPCD
           DELIMITED BY SIZE INTO
           HTML-GESAD8
           STRING      HTML-TO-ADDR-LINE8
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50ID-FN. EXIT.
       F50HS-FN. EXIT.
      *N50IE.    NOTE *FOR SPECIAL PAYEE ARRNGMNT         *.
       F50IE.    IF    V288-CDETY = '05'                                lv10
                 NEXT SENTENCE ELSE GO TO     F50IE-FN.
      *N50IF.    NOTE *BUILD SPECIAL PAYEE NAME           *.
       F50IF.    IF    V288-MCLNM5 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50IF-FN.
      *
           STRING      V288-MCLNM5
           DELIMITED BY LOW-VALUES
           INTO HTML-MCLNM5
      *
           STRING      HTML-SPL-PAYEE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IF-FN. EXIT.
      *N50IG.    NOTE *BUILD DESTINATION ADDR #1          *.
       F50IG.    IF    V288-GESAD1 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50IG-FN.
      *
           STRING      V288-GESAD1
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD5
      *
           STRING      HTML-TO-ADDR-LINE5
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IG-FN. EXIT.
      *N50IH.    NOTE *BUILD DESTINATION ADDR #2          *.
       F50IH.    IF    V288-GESAD2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50IH-FN.
      *
           STRING      V288-GESAD2
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD6
      *
           STRING      HTML-TO-ADDR-LINE6
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IH-FN. EXIT.
      *N50IJ.    NOTE *BUILD DESTINATION ADDR #3          *.
       F50IJ.    IF    V288-GESAD3 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F50IJ-FN.
      *
           STRING      V288-GESAD3
           DELIMITED BY LOW-VALUES
           INTO HTML-GESAD7
      *
           STRING      HTML-TO-ADDR-LINE7
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IJ-FN. EXIT.
      *N50IK.    NOTE *BUILD CITY NAME AND POSTAL CODE    *.
       F50IK.                                                           lv15
      *
           STRING      V288-GECIT ', '
           V288-GEST ' '
           V288-GEPCD
           DELIMITED BY SIZE INTO
           HTML-GESAD8
           STRING      HTML-TO-ADDR-LINE8
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50IK-FN. EXIT.
       F50IE-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *BUILD PAYMENT DETAILS              *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60BE.    NOTE *MOVE PAYMENT AMOUNT                *.
       F60BE.                                                           lv10
      *
           MOVE        V288-APMTD TO HTML-APMTD
           HTML-APMTD1.
       F60BE-FN. EXIT.
      *N60BH.    NOTE *MOVE PAYMENT TYPE                  *.
       F60BH.                                                           lv10
      *
           MOVE        V288-MPMTT TO HTML-MPMTT
           HTML-MPMTT1.
       F60BH-FN. EXIT.
      *N60BK.    NOTE *MOVE FREQUENCY TEXT                *.
       F60BK.                                                           lv10
      *
           STRING      V288-MPMTF
           DELIMITED BY LOW-VALUES
           INTO HTML-MPMTF
      *
           MOVE        HTML-MPMTF TO HTML-MPMTF1.
       F60BK-FN. EXIT.
      *N60BL.    NOTE *MOVE RMD TEXT                      *.
       F60BL.                                                           lv10
      *
                 IF    V288-IDRMD = 'Y'                                 DOT
           MOVE        WS00-TXRMDY TO HTML-TXRMD.
                 IF    V288-IDRMD = 'N'                                 DOT
           MOVE        WS00-TXRMDN TO HTML-TXRMD.
                 IF    V288-IDRMD = 'A'                                 DOT
           MOVE        WS00-TXRMDA TO HTML-TXRMD.
           MOVE        HTML-TXRMD TO HTML-TXRMD1.                       DOT
      *N60BN.    NOTE *FORMAT WITHHOLDING PERCENT         *.
       F60BN.                                                           lv15
      *
                 IF    V288-TWITH = SPACES                              DOT
           MOVE        V288-PWHLD TO WS00-PWHLD
           STRING      WS00-PWHLD '%'
           DELIMITED BY SIZE
           INTO HTML-CTWHPB
           MOVE        HTML-CTWHPB TO HTML-CTWHPB1
      *
                 ELSE
           MOVE        'N/A' TO HTML-CTWHPB
           HTML-CTWHPB1.
       F60BN-FN. EXIT.
      *N60BQ.    NOTE *REFORMAT THE START DATE            *.
       F60BQ.                                                           lv15
      *
      *!ADS "V288-DNPMT         7-DE00-DNPMT"
           MOVE        V288-DNPMT                                       CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT81E TO DAT63C                                        CI0329
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0329
           MOVE   DAT6C TO  7-DE00-DNPMT                                CI0329
      *!ADM "7-DE00-DNPMT       HTML-DNPMT1
      *HTML-DNPMT2"
           MOVE        7-DE00-DNPMT                                     CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0329
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0329
           MOVE   DAT8C TO  HTML-DNPMT1                                 CI0329
           HTML-DNPMT2.                                                 CI0329
       F60BQ-FN. EXIT.
      *N60BS.    NOTE *REFORMAT THE END DATE              *.
       F60BS.                                                           lv15
      *
                 IF    V288-GEEND > ZEROES                              DOT
      *!ADS "V288-GEEND         7-DE00-GEEND"
           MOVE        V288-GEEND                                       CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT81E TO DAT63C                                        CI0329
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0329
           MOVE   DAT6C TO  7-DE00-GEEND                                CI0329
      *!ADM "7-DE00-GEEND       HTML-GEEND
      *HTML-GEEND1"
           MOVE        7-DE00-GEEND                                     CI0329
           TO DAT8E DAT6C                                               CI0329
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0329
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0329
           MOVE   DAT8C TO  HTML-GEEND                                  CI0329
           HTML-GEEND1                                                  CI0329
                 ELSE
           MOVE        SPACES TO HTML-GEEND
           HTML-GEEND1.
       F60BS-FN. EXIT.
       F60BL-FN. EXIT.
      *N60BV.    NOTE *FOR ADD REQUEST                    *.
       F60BV.    IF    V288-CPROCM = 'A'                                lv10
                 NEXT SENTENCE ELSE GO TO     F60BV-FN.
      *N60BY.    NOTE *STRING PAYMENT DETAIL              *.
       F60BY.                                                           lv15
      *
           STRING      HTML-ADD-PAYMT-DETAIL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60BY-FN. EXIT.
       F60BV-FN. EXIT.
      *N60EB.    NOTE *FOR MODIFY REQUEST                 *.
       F60EB.    IF    V288-CPROCM = 'M'                                lv10
                 NEXT SENTENCE ELSE GO TO     F60EB-FN.
      *N60EC.    NOTE *MOVE PAYMENT STATUS                *.
       F60EC.                                                           lv15
      *
                 IF    V288-CDEST = 01                                  DOT
           MOVE        'ACTIVE' TO HTML-TDESA.
                 IF    V288-CDEST = 03                                  DOT
           MOVE        'INACTIVE' TO HTML-TDESA.
       F60EC-FN. EXIT.
      *N60EF.    NOTE *STRING PAYMENT DETAIL              *.
       F60EF.                                                           lv15
      *
           STRING      HTML-MOD-PAYMT-DETAIL
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F60EF-FN. EXIT.
       F60EB-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *BUILD INFO MSGS AND COMPLIANCE     *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
      *QUESTIONS FOR VERIFY/CONFIRM
      *N65BB.    NOTE *FOR VERIFY PAGE                    *.
       F65BB.    IF    V288-CACTS = 'V'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F65BB-FN.
      *N65BE.    NOTE *LIST IMPLICATION MESSAGES          *.
       F65BE.                                                           lv15
      *
           STRING      HTML-VERF-ADV-MESSAGE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65BE-FN. EXIT.
       F65BB-FN. EXIT.
      *N65BH.    NOTE *FOR CONFIRM PAGE                   *.
       F65BH.    IF    V288-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F65BH-FN.
      *N65BK.    NOTE *LIST IMPLICATION MESSAGES          *.
       F65BK.                                                           lv15
      *
           STRING      HTML-CONF-ADV-MESSAGE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65BK-FN. EXIT.
       F65BH-FN. EXIT.
      *N65BL.    NOTE *FOR ANNUITY SUMMER RIDER           *.
       F65BL.    IF    V288-CTIDA = (004 OR 005)                        lv10
                 AND   V288-CGMBR = ('X' OR 'Y')
                 AND   V288-DCACG > V288-DWAIT
                 NEXT SENTENCE ELSE GO TO     F65BL-FN.
      *AND OUTSIDE WAITING PERIOD
                 IF    V288-IAPGP = 'Y'                                 DOT
      *PERCENTAGE LOCKED
           STRING      HTML-PERC-LOCKED-MESSAGE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-CHECK-SELECT-MESSAGE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    V288-IAPGP = 'N'                                 DOT
      *PERCENTAGE NOT LOCKED
           STRING      HTML-PERC-NOLOCK-MESSAGE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT
           STRING      HTML-CHECK-SELECT-MESSAGE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65BL-FN. EXIT.
      *N65BN.    NOTE *LOOP THRU MSG TEXTS TO DISPLAY     *.
       F65BN.                                                           lv10
           MOVE        1                        TO J65BNR
                                    GO TO     F65BN-B.
       F65BN-A.
           ADD         1                        TO J65BNR.
       F65BN-B.
           IF          J65BNR                   >  V288-QITEM
                                    GO TO     F65BN-FN.
      *N65BQ.    NOTE *MOVE MESSAGE TEXT                  *.
       F65BQ.    IF    V288-TMESS4 (J65BNR) > SPACES                    lv15
                 AND   V288-QITEM NOT > 20
                 NEXT SENTENCE ELSE GO TO     F65BQ-FN.
           MOVE        J65BNR TO WS00-MSGNUM
           MOVE        V288-TMESS4 (J65BNR) TO WS00-TMESS4
           MOVE        WS00-MSGTXT TO HTML-MSGTXT.
      *N65BT.    NOTE *STRING IMP MSG TEXT # 1            *.
       F65BT.                                                           lv20
      *
           STRING      HTML-IMP-MSG01
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65BT-FN. EXIT.
       F65BQ-FN. EXIT.
       F65BN-900. GO TO F65BN-A.
       F65BN-FN. EXIT.
      *N65CB.    NOTE *STRING IMP MSG TEXT # 2            *.
       F65CB.                                                           lv10
      *
           STRING      HTML-IMP-MSG02
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65CB-FN. EXIT.
      *N65CE.    NOTE *FOR VERIFY PAGE                    *.
       F65CE.    IF    V288-CACTS = 'V'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F65CE-FN.
      *N65CH.    NOTE *STRING VERIFY STEP3                *.
       F65CH.                                                           lv15
      *
           STRING      HTML-VERIFY-STEP3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65CH-FN. EXIT.
      *N65CK.    NOTE *STRING COMPLIANCE QUESTIONS        *.
       F65CK.                                                           lv15
      *
           STRING      HTML-VERF-COMP-QUEST
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65CK-FN. EXIT.
      *N65CN.    NOTE *STRING PROCESS REQUEST             *.
       F65CN.                                                           lv15
      *
           STRING      HTML-PROCESS-REQ
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65CN-FN. EXIT.
       F65CE-FN. EXIT.
      *N65CT.    NOTE *FOR CONFIRM PAGE                   *.
       F65CT.    IF    V288-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F65CT-FN.
      *N65DB.    NOTE *FORMAT COMPLIANCE QUESTIONS        *.
       F65DB.                                                           lv15
           INITIALIZE  HTML-CONF-COMP-QUEST.
      *N65DD.    NOTE *CHECK CLIENT PROSPECTUS IND        *.
       F65DD.                                                           lv20
      *
                 IF    V288-CCLPR = 'Y'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLPR-YES
                 ELSE
           MOVE        '<TD>              ' TO CCLPR-YES.
                 IF    V288-CCLPR = 'N'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLPR-NO
                 ELSE
           MOVE        '<TD>              ' TO CCLPR-NO.
                 IF    V288-CCLPR = 'Z'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLPR-NA
                 ELSE
           MOVE        '<TD>              ' TO CCLPR-NA.
       F65DD-FN. EXIT.
      *N65DG.    NOTE *CHECK CLIENT CHARGES IND           *.
       F65DG.                                                           lv20
      *
                 IF    V288-CCLCH = 'Y'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLCH-YES
                 ELSE
           MOVE        '<TD>              ' TO CCLCH-YES.
                 IF    V288-CCLCH = 'N'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLCH-NO
                 ELSE
           MOVE        '<TD>              ' TO CCLCH-NO.
                 IF    V288-CCLCH = 'Z'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLCH-NA
                 ELSE
           MOVE        '<TD>              ' TO CCLCH-NA.
       F65DG-FN. EXIT.
      *N65DI.    NOTE *CHECK CLIENT SUITABILITY IND       *.
       F65DI.                                                           lv20
      *
                 IF    V288-CCLSU = 'Y'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLSU-YES
                 ELSE
           MOVE        '<TD>              ' TO CCLSU-YES.
                 IF    V288-CCLSU = 'N'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLSU-NO
                 ELSE
           MOVE        '<TD>              ' TO CCLSU-NO.
                 IF    V288-CCLSU = 'Z'                                 DOT
           MOVE        '<TD><B>X</B>      ' TO CCLSU-NA
                 ELSE
           MOVE        '<TD>              ' TO CCLSU-NA.
       F65DI-FN. EXIT.
      *N65DL.    NOTE *STRING DIV TAGS ONLY IF ALL        *.
       F65DL.    IF    V288-CCLPR = 'Z'                                 lv20
                 AND   V288-CCLCH = 'Z'
                 AND   V288-CCLSU = 'Z'
                 NEXT SENTENCE ELSE GO TO     F65DL-FN.
      *COMPLIANCE ANSWERS ARE 'N/A'
           STRING      HTML-COMP-DIV-TAGS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65DL-FN. EXIT.
      *N65DQ.    NOTE *STRING COMPLIANCE QUESTIONS        *.
       F65DQ.                                                           lv20
      *
           STRING      HTML-CONF-COMP-QUEST
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F65DQ-FN. EXIT.
       F65DB-FN. EXIT.
       F65CT-FN. EXIT.
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
           MOVE        01 TO V288-CHTML (TALLI)
           MOVE        HTML-TEXT TO V288-THTML (TALLI)
           ADD         1 TO TALLI.
       F70BG-FN. EXIT.
       F70BD-900. GO TO F70BD-A.
       F70BD-FN. EXIT.
       F70BB-FN. EXIT.
      *N70EB.    NOTE *MOVE END OF FILE MARKER            *.
       F70EB.                                                           lv10
      *
           MOVE        99 TO V288-CHTML (TALLI)
           MOVE        SPACES TO V288-THTML (TALLI).
      *N70EF.    NOTE *CALCULATE THTML LENGTH             *.
       F70EF.                                                           lv15
      *
           COMPUTE     V288-QBLCK = TALLI.
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
           MOVE        V288-DXTMS2 TO 7-DB2-DXTMSA                      ANIOBJ
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
