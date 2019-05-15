       IDENTIFICATION DIVISION.                                         CI0280
       PROGRAM-ID.  CI0280P.                                            CI0280
      *AUTHOR.         eFUNDING FA XML BUILD.                           CI0280
      *DATE-COMPILED.   09/08/14.                                       CI0280
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2007                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE OST    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE OST    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE OST          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2007                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0280
       CONFIGURATION SECTION.                                           CI0280
       SOURCE-COMPUTER. IBM-370.                                        CI0280
       OBJECT-COMPUTER. IBM-370.                                        CI0280
       DATA DIVISION.                                                   CI0280
       WORKING-STORAGE SECTION.                                         CI0280
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
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

       01 XML-PT  PIC S9(5) VALUE ZEROS.
       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WD115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0280
      *!WI pl=WD120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0280
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0280
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0280
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0280
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0280
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0280
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0280
      **----------------------------------------------------------------ANIOBJ
      **  DATE WORK AREA USED BY MACRO AADA58                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
      *!WF DSP=WE DSL=DD SEL=01 FOR=I LEV=1                             ANIOBJ
       01                 WE00.                                         CI0280
          05              WE00-SUITE.                                   CI0280
            15       FILLER         PICTURE  X(00093).                  CI0280
       01                 WE01  REDEFINES      WE00.                    CI0280
            10            WE01-XDAT8.                                   CI0280
            11            WE01-XDATC  PICTURE  XX.                      CI0280
            11            WE01-XDATY  PICTURE  XX.                      CI0280
            11            WE01-XDATM  PICTURE  XX.                      CI0280
            11            WE01-XDATD  PICTURE  XX.                      CI0280
            10            WE01-XDAT8D                                   CI0280
                          REDEFINES            WE01-XDAT8               CI0280
               PICTURE    9(8).                                         CI0280
            10            WE01-XDAT81.                                  CI0280
            11            WE01-XDATM1 PICTURE  XX.                      CI0280
            11            WE01-XDATD1 PICTURE  XX.                      CI0280
            11            WE01-XDATC1 PICTURE  XX.                      CI0280
            11            WE01-XDATY1 PICTURE  XX.                      CI0280
            10            WE01-XDAT80                                   CI0280
                          REDEFINES            WE01-XDAT81              CI0280
               PICTURE    9(8).                                         CI0280
            10            WE01-XDAT62.                                  CI0280
            11            WE01-XDATM2 PICTURE  XX.                      CI0280
            11            WE01-XDATD2 PICTURE  XX.                      CI0280
            11            WE01-XDATY2 PICTURE  XX.                      CI0280
            10            WE01-XDAT69                                   CI0280
                          REDEFINES            WE01-XDAT62              CI0280
               PICTURE    9(6).                                         CI0280
            10            WE01-XDATCU.                                  CI0280
            11            WE01-XDATC9 PICTURE  99.                      CI0280
            11            WE01-XDAYMD.                                  CI0280
            12            WE01-XDATY9 PICTURE  99.                      CI0280
            12            WE01-XDAMD.                                   CI0280
            13            WE01-XDATM9 PICTURE  99.                      CI0280
            13            WE01-XDATD9 PICTURE  99.                      CI0280
            10            WE01-XDAT89 PICTURE  9(8).                    CI0280
            10            WE01-XDAJC  PICTURE  9(7).                    CI0280
            10            WE01-XDAJC1.                                  CI0280
            11            WE01-XDAJC9 PICTURE  99.                      CI0280
            11            WE01-XDAJY  PICTURE  99.                      CI0280
            11            WE01-XDAJN  PICTURE  999.                     CI0280
            10            WE01-XDAB   PICTURE  9(5).                    CI0280
            10            WE01-DD05.                                    CI0280
            11            WE01-XDACT  PICTURE  S9(3)                    CI0280
                          COMPUTATIONAL-3.                              CI0280
            11            WE01-XDACV  PICTURE  S9                       CI0280
                          COMPUTATIONAL-3.                              CI0280
            11            WE01-XDAGP  PICTURE  S9(9)                    CI0280
                          COMPUTATIONAL-3.                              CI0280
            11            WE01-XDAJP  PICTURE  S9(7)                    CI0280
                          COMPUTATIONAL-3.                              CI0280
            11            WE01-XDACV1 PICTURE  S9                       CI0280
                          COMPUTATIONAL-3.                              CI0280
            11            WE01-XDAGP1 PICTURE  S9(9)                    CI0280
                          COMPUTATIONAL-3.                              CI0280
            11            WE01-XDAJP1 PICTURE  S9(7)                    CI0280
                          COMPUTATIONAL-3.                              CI0280
            10            WE01-XW03.                                    CI0280
            11            WE01-XDATG.                                   CI0280
            12            WE01-XDAT1.                                   CI0280
            13            WE01-XDAT19 PICTURE  99.                      CI0280
            12            WE01-XDAT2.                                   CI0280
            13            WE01-XDAT29 PICTURE  99.                      CI0280
            12            WE01-XDAT3.                                   CI0280
            13            WE01-XDAT39 PICTURE  99.                      CI0280
            12            WE01-XDAT4.                                   CI0280
            13            WE01-XDAT49 PICTURE  99.                      CI0280
            11            WE01-XLEAPY PICTURE  99.                      CI0280
            11            WE01-DTGCY  PICTURE  9(4).                    CI0280
            11            WE01-FILLER                                   CI0280
                          REDEFINES            WE01-DTGCY.              CI0280
            12            WE01-DTGCC  PICTURE  9(2).                    CI0280
            12            WE01-DTGYY  PICTURE  9(2).                    CI0280
                                                                        ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      **  WORKING STORAGE FOR CASE DOC DATE                             ANIOBJ
      **---------------------------------------------------------------*ANIOBJ
      *!WI pl=WE115                                                     ANIOBJ
       01 7-CSTDTE                                                      ANIOBJ
                        PICTURE X(8).                                   CI0280
       01 FILLER REDEFINES 7-CSTDTE.                                    ANIOBJ
      *!WI pl=WE125                                                     ANIOBJ
          05 7-DTGCY                                                    ANIOBJ
                        PICTURE 9(4).                                   CI0280
      *!WI pl=WE130                                                     ANIOBJ
          05 7-DTGMM                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0280
      *!WI pl=WE135                                                     ANIOBJ
          05 7-DTGDD                                                    ANIOBJ
                        PICTURE 9(2).                                   CI0280
      *                                                                 ANIOBJ
      *!WI pl=WE145                                                     ANIOBJ
       01 7-CASE-DTJUL                                                  ANIOBJ
                        PICTURE 9(7).                                   CI0280
       01 FILLER REDEFINES 7-CASE-DTJUL.                                ANIOBJ
      *!WI pl=WE150                                                     ANIOBJ
          05 7-CASE-DTGCY                                               ANIOBJ
                        PICTURE 9(4).                                   CI0280
      *!WI pl=WE155                                                     ANIOBJ
          05 7-CASE-DTJDD                                               ANIOBJ
                        PICTURE 9(3).                                   CI0280
      *                                                                 ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      ** CASEDOC-ID GENERATED                                           ANIOBJ
      **----------------------------------------------------------------ANIOBJ
      *                                                                 ANIOBJ
       01 WE01-NIOBJC.                                                  ANIOBJ
         05 WE01-DTTCY    PIC 9.                                        ANIOBJ
      *!WI pl=WE270                                                     ANIOBJ
         05 WE01-DTJDD                                                  ANIOBJ
                        PICTURE 9(3).                                   CI0280
      *!WI pl=WE280                                                     ANIOBJ
         05 WE01-DTTHH                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0280
      *!WI pl=WE290                                                     ANIOBJ
         05 WE01-DTTMN                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0280
      *!WI pl=WE300                                                     ANIOBJ
         05 WE01-DTTSS                                                  ANIOBJ
                        PICTURE 9(2).                                   CI0280
         05 WE01-DTTNN    PIC 9(3).                                     ANIOBJ
         05 FILLER        PIC X(4)    VALUE '.001'.                     ANIOBJ
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58
      *MISCELLANEOUS FIELDS
      *
      *WORK AREA FOR ROUTINE TO TRUNCATE TRAILING SPACES
      *IN DATA BETWEEN XML TAGS
       01  WS-XML-TRUNC-TRAILING.
           05  XML-END-TAG        PIC X(80).
           05  XML-SPOS           PIC S9(8) COMP.
           05  XML-EPOS           PIC S9(8) COMP.
       01  XML-LINE               PIC X(80).
       01  XML-LINE-TABLE         REDEFINES XML-LINE.
           05  XML-LINE-CHAR      PIC X(01) OCCURS 80 TIMES.
      *FIELDS USED TO FORMAT TIMESTAMP
      *
       01  WT01-DOC-ID.
          10  WT01-CASE-ID     PIC X(13).
          10  FILLER           PIC X(04).
      *
       01  WT02-DATE-TIME.
            15 WT02-DTGRG      PIC X(10).
            15 FILLER          PIC X VALUE SPACE.
      *!WI
            15 WT02-DTTHH
                        PICTURE 9(2).                                   CI0280
            15 FILLER          PIC X VALUE ':'.
      *!WI
            15 WT02-DTTMN
                        PICTURE 9(2).                                   CI0280
            15 FILLER          PIC X VALUE ':'.
      *!WI
            15 WT02-DTTSS
                        PICTURE 9(2).                                   CI0280
      *
       01  WT03-DATE-TIME.
            15 WT03-DTGRG      PIC X(10).
            15 FILLER          PIC X VALUE SPACE.
      *!WI
            15 WT03-DTTHH
                        PICTURE 9(2).                                   CI0280
            15 FILLER          PIC X VALUE ':'.
      *!WI
            15 WT03-DTTMN
                        PICTURE 9(2).                                   CI0280
            15 FILLER          PIC X VALUE ':'.
      *!WI
            15 WT03-DTTSS
                        PICTURE 9(2).                                   CI0280
       01   DEBUT-WSS.                                                  CI0280
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0280
            05   IK     PICTURE X.                                      CI0280
       01  CONSTANTES-PAC.                                              CI0280
           05  FILLER  PICTURE X(87)   VALUE                            CI0280
                     '6015 CAT09/08/14CI0280ADMIN   14:35:13CI0280P AMERCI0280
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0280
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0280
           05  NUGNA   PICTURE X(5).                                    CI0280
           05  APPLI   PICTURE X(3).                                    CI0280
           05  DATGN   PICTURE X(8).                                    CI0280
           05  PROGR   PICTURE X(6).                                    CI0280
           05  CODUTI  PICTURE X(8).                                    CI0280
           05  TIMGN   PICTURE X(8).                                    CI0280
           05  PROGE   PICTURE X(8).                                    CI0280
           05  COBASE  PICTURE X(4).                                    CI0280
           05  DATGNC  PICTURE X(10).                                   CI0280
           05  RELEAS  PICTURE X(7).                                    CI0280
           05  DATGE   PICTURE X(10).                                   CI0280
           05  DATSQ   PICTURE X(10).                                   CI0280
       01  DATCE.                                                       CI0280
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0280
         05  DATOR.                                                     CI0280
           10  DATOA  PICTURE XX.                                       CI0280
           10  DATOM  PICTURE XX.                                       CI0280
           10  DATOJ  PICTURE XX.                                       CI0280
       01   VARIABLES-CONDITIONNELLES.                                  CI0280
            05                  FT      PICTURE X VALUE '0'.            CI0280
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0280
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0280
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J60CBR PICTURE S9(4) VALUE  ZERO.
            05           J60EBR PICTURE S9(4) VALUE  ZERO.
            05           J60KBR PICTURE S9(4) VALUE  ZERO.
            05           J60MFR PICTURE S9(4) VALUE  ZERO.
            05           J60MIR PICTURE S9(4) VALUE  ZERO.
            05           J60MPR PICTURE S9(4) VALUE  ZERO.
            05           J60ZBR PICTURE S9(4) VALUE  ZERO.
            05           J70CBR PICTURE S9(4) VALUE  ZERO.
            05           J70EBR PICTURE S9(4) VALUE  ZERO.
            05           J85CCR PICTURE S9(4) VALUE  ZERO.
            05           J85CKR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0280
      *COPYBOOK WITH XML TEXT

       COPY CI0280C1.
      *COPYBOOK WITH IMAGE TEXT

       COPY CI0280C2.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE INPUT LINKAGE FOR CI0280          *
      ******************************************************************
      *
      *!WF DSP=V1 DSL=V1 SEL=80 FOR=I LEV=1 PLT=70
       01                 V100.                                         CI0280
          05              V100-SUITE.                                   CI0280
            15       FILLER         PICTURE  X(00162).                  CI0280
       01                 V180  REDEFINES      V100.                    CI0280
            10            V180-CBUSN  PICTURE  X(8).                    CI0280
            10            V180-CPCCDF PICTURE  99.                      CI0280
            10            V180-MCODE  PICTURE  X(30).                   CI0280
            10            V180-MRGNN  PICTURE  X(8).                    CI0280
            10            V180-DXTMSA PICTURE  X(26).                   CI0280
            10            V180-DXTMS2 PICTURE  X(26).                   CI0280
            10            V180-C199.                                    CI0280
            11            V180-CLID.                                    CI0280
            12            V180-CLIDO  PICTURE  9(3).                    CI0280
            12            V180-CLIDN.                                   CI0280
            13            V180-CLIDNP PICTURE  X(12).                   CI0280
            13            V180-CLIDND PICTURE  9(8).                    CI0280
            10            V180-C299.                                    CI0280
            11            V180-CTID.                                    CI0280
            12            V180-CTIDA  PICTURE  9(3).                    CI0280
            12            V180-CTIDN.                                   CI0280
            13            V180-CTIDNP PICTURE  X(13).                   CI0280
            13            V180-CTIDND PICTURE  9(11).                   CI0280
            10            V180-PRCSN  PICTURE  X(9).                    CI0280
            10            V180-QNACT  PICTURE  9(3).                    CI0280
      *
      ******************************************************************
      **         THIS SEGMENT IS THE XML LINKAGE FOR IMAGE UTILITY     *
      ******************************************************************
      *
      *!WF DSP=XM DSL=QT SEL=90 FOR=I LEV=1 PLT=75
       01                 XM00.                                         CI0280
          05              XM00-SUITE.                                   CI0280
            15       FILLER         PICTURE  X(40406).                  CI0280
       01                 XM90  REDEFINES      XM00.                    CI0280
            10            XM90-QBLCK  PICTURE  9(6).                    CI0280
            10            XM90-QT9O.                                    CI0280
            11            XM90-QT9B                                     CI0280
                          OCCURS       200     TIMES.                   CI0280
            12            XM90-CHTML  PICTURE  99.                      CI0280
            12            XM90-THTML  PICTURE  X(200).                  CI0280
      *
      ******************************************************************
      **       THIS SEGMENT IS THE IMAGE LINKAGE FOR IMAGE UTILITY     *
      ******************************************************************
      *
      *!WF DSP=IM DSL=QT SEL=90 FOR=I LEV=1 PLT=80
       01                 IM00.                                         CI0280
          05              IM00-SUITE.                                   CI0280
            15       FILLER         PICTURE  X(40406).                  CI0280
       01                 IM90  REDEFINES      IM00.                    CI0280
            10            IM90-QBLCK  PICTURE  9(6).                    CI0280
            10            IM90-QT9O.                                    CI0280
            11            IM90-QT9B                                     CI0280
                          OCCURS       200     TIMES.                   CI0280
            12            IM90-CHTML  PICTURE  99.                      CI0280
            12            IM90-THTML  PICTURE  X(200).                  CI0280
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0280
          05              MS00-SUITE.                                   CI0280
            15       FILLER         PICTURE  X(00542).                  CI0280
       01                 MS03  REDEFINES      MS00.                    CI0280
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0280
                          COMPUTATIONAL-3.                              CI0280
            10            MS03-CMSSF  PICTURE  XX.                      CI0280
            10            MS03-DU09.                                    CI0280
            11            MS03-CMESA  PICTURE  S9(9)                    CI0280
                          BINARY.                                       CI0280
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0280
                          BINARY.                                       CI0280
            11            MS03-CMESB  PICTURE  S9(9)                    CI0280
                          BINARY.                                       CI0280
            11            MS03-CMSST  PICTURE  S9(9)                    CI0280
                          BINARY.                                       CI0280
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0280
                          BINARY.                                       CI0280
            11            MS03-QELLAA PICTURE  S9(9)                    CI0280
                          BINARY.                                       CI0280
            11            MS03-TMESS4 PICTURE  X(512).                  CI0280
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0280
            10            MX11-QMSGS  PICTURE  9(03).                   CI0280
            10            MX11-PJ09                                     CI0280
                          OCCURS       025     TIMES.                   CI0280
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0280
                          COMPUTATIONAL-3.                              CI0280
            11            MX11-CMESB  PICTURE  S9(9)                    CI0280
                          BINARY.                                       CI0280
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V180
                                XM90
                                IM90
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0280
      *               *                                   *             CI0280
      *               *INITIALISATIONS                    *             CI0280
      *               *                                   *             CI0280
      *               *************************************.            CI0280
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
      *N02DA.    NOTE *---> ALL XML LINES                 *.
       F02DA.                                                           lv10
           INITIALIZE  XML-HEADER-TAGS
           XML-COMMON-TAGS
           XML-DOC-INFO-TAGS
           XML-INDEX1-TAGS
           XML-INDEX2-TAGS
           XML-INDEX8-TAGS
           XML-FOOTER-TAGS
           XML-IMAGE-TAGS
           XML-IMAGE-INFO.
       F02DA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0280
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0280
      *               *                                   *             CI0280
      *               *FIN DE TRAITEMENT                  *             CI0280
      *               *                                   *             CI0280
      *               *************************************.            CI0280
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0280
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *BUILD XML HEADERS                  *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30BB.    NOTE *GENERATE SOURCE DOC ID             *.            ANIOBJ
       F30BB.         EXIT.                                             lv10
      *N30CB.    NOTE *UNFORMAT TS                        *.            ANIOBJ
       F30CB.                                                           lv15
           MOVE        V180-DXTMS2 TO 7-DB2-DXTMSA                      ANIOBJ
           MOVE        7-DB2-DTGCY TO 7-DTGCY                           ANIOBJ
           MOVE        7-DB2-DTGMM TO 7-DTGMM                           ANIOBJ
           MOVE        7-DB2-DTGDD TO 7-DTGDD.                          ANIOBJ
       F30CB-FN. EXIT.
      *N30DB.    NOTE *DETERMINE JULIAN DAY               *.            ANIOBJ
       F30DB.                                                           lv15
           MOVE        7-CSTDTE TO WE01-XDAGP                           ANIOBJ
           MOVE        1 TO WE01-XDACT                                  ANIOBJ
           MOVE        1 TO WE01-XDACV                                  ANIOBJ
      *ADD AADA58 HERE.                                                 ANIOBJ
      *CALL MWS100EX - DYNAMIC                                          DOT
           CALL        MWS100EX USING WE01-DD05.                        AADA58
       F30DB-FN. EXIT.
      *N30EB.    NOTE *BUILD DOC ID                       *.            ANIOBJ
       F30EB.                                                           lv15
           MOVE        WE01-XDAJP TO 7-CASE-DTJUL                       ANIOBJ
           MOVE        7-CASE-DTJDD TO WE01-DTJDD                       ANIOBJ
           ADD         366 TO WE01-DTJDD                                ANIOBJ
           MOVE        7-DB2-DTGYY TO WE01-DTTCY                        ANIOBJ
           MOVE        7-DB2-DTTHH TO WE01-DTTHH                        ANIOBJ
           MOVE        7-DB2-DTTMN TO WE01-DTTMN                        ANIOBJ
           MOVE        7-DB2-DTTSS TO WE01-DTTSS                        ANIOBJ
           COMPUTE     WE01-DTTNN = 7-DB2-DTTNN                         ANIOBJ
           / 1000.                                                      ANIOBJ
       F30EB-FN. EXIT.
       F30BB-FN. EXIT.
      *N30FB.    NOTE *FORMAT CASE / DOC IDS              *.
       F30FB.                                                           lv10
           MOVE        WE01-NIOBJC TO WT01-DOC-ID.
       F30FB-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *BUILD XML LINES                    *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0280C1' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N40BE.    NOTE *BUILD DOC TYPE INFO                *.
       F40BE.                                                           lv10
      *
           MOVE        V180-MCODE TO XML-DOC-TYPE.
       F40BE-FN. EXIT.
      *N40DB.    NOTE *MOVE CASE / SOURCE IDS             *.
       F40DB.                                                           lv10
      *
           MOVE        WT01-DOC-ID TO XML-DOC-ID
           XML-COMP-DCN.
       F40DB-FN. EXIT.
      *N40EB.    NOTE *SET BUSINESS AREA USING REGION     *.
       F40EB.                                                           lv10
                 IF    V180-MRGNN (1:4) = 'CICD'                        DOT
           MOVE        '0112' TO XML-AREA.
                 IF    V180-MRGNN (1:4) = 'CICM'                        DOT
           MOVE        '0112' TO XML-AREA.
                 IF    V180-MRGNN (1:4) = 'CICP'                        DOT
           MOVE        '0110' TO XML-AREA.
       F40EB-FN. EXIT.
      *N40GB.    NOTE *SET SOURCE NAME                    *.
       F40GB.                                                           lv10
      *NO TRAILING SPACES ALLOWED !
           STRING      CN-SOURCE-NAME
           V180-CBUSN
           CN-EO-SOURCE-NAME
           DELIMITED BY SPACES
           INTO XML-SOURCE.
       F40GB-FN. EXIT.
      *N40IB.    NOTE *SET WEB TIME STAMP AS SCAN TS      *.
       F40IB.                                                           lv10
           MOVE        V180-DXTMSA TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGRG TO WT02-DTGRG
           MOVE        7-DB2-DTTHH TO WT02-DTTHH
           MOVE        7-DB2-DTTMN TO WT02-DTTMN
           MOVE        7-DB2-DTTSS TO WT02-DTTSS
           MOVE        WT02-DATE-TIME TO XML-SCAN-TS.
       F40IB-FN. EXIT.
      *N40IE.    NOTE *SET RPC TIME STAMP AS RELA-TS      *.
       F40IE.                                                           lv10
           MOVE        V180-DXTMS2 TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGRG TO WT03-DTGRG
           MOVE        7-DB2-DTTHH TO WT03-DTTHH
           MOVE        7-DB2-DTTMN TO WT03-DTTMN
           MOVE        7-DB2-DTTSS TO WT03-DTTSS
           MOVE        WT03-DATE-TIME TO XML-RELA-TS.
       F40IE-FN. EXIT.
      *N40KB.    NOTE *FORMAT CLIENT INDEX                *.
       F40KB.                                                           lv10
      *
           MOVE        V180-CLIDO TO XML-CLIDO
           MOVE        V180-CLIDN TO XML-CLIDN.
       F40KB-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *BUILD IMAGE INFO                   *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0280C2' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N50BB.    NOTE *COMPUTE XML LENGTH IN THE          *.
       F50BB.                                                           lv10
      *IMAGE MESSAGE RETURNED IN
      *200 BYTE CHUNKS LESS THE 6
      *BYTE LENGTH ITSELF
      *
           COMPUTE     XML-IMAGE-QRECL =
           ((XML-IMAGE-CTR + XML-INFO-CTR)
           * 200) - 6.
       F50BB-FN. EXIT.
      *N50CB.    NOTE *BUILD SOURCE DOC ID                *.
       F50CB.                                                           lv10
           MOVE        WT01-DOC-ID TO XML-IMAGE-DOC-ID.
       F50CB-FN. EXIT.
      *N50EB.    NOTE *SET BUSINESS AREA USING REGION     *.
       F50EB.                                                           lv10
                 IF    V180-MRGNN (1:4) = 'CICD'                        DOT
           MOVE        '0112' TO XML-IMAGE-AREA.
                 IF    V180-MRGNN (1:4) = 'CICM'                        DOT
           MOVE        '0112' TO XML-IMAGE-AREA.
                 IF    V180-MRGNN (1:4) = 'CICP'                        DOT
           MOVE        '0110' TO XML-IMAGE-AREA.
       F50EB-FN. EXIT.
      *N50GB.    NOTE *SET SOURCE NAME                    *.
       F50GB.                                                           lv10
      *NO TRAILING SPACES ALLOWED !
           STRING      CN-SOURCE-NAME
           V180-CBUSN
           CN-EO-SOURCE-NAME
           DELIMITED BY SPACES
           INTO XML-IMAGE-SOURCE.
       F50GB-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *BUILD XML RETURN AREA              *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0280C1' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N60BA.    NOTE *INITIALIZE LINE COUNTER            *.
       F60BA.                                                           lv10
           MOVE        0 TO XML-PT.
       F60BA-FN. EXIT.
      *N60CB.    NOTE *STRING HEADER INTO XML-OUTPUT      *.
       F60CB.                                                           lv10
           MOVE        1                        TO J60CBR
                                    GO TO     F60CB-B.
       F60CB-A.
           ADD         1                        TO J60CBR.
       F60CB-B.
           IF          J60CBR                   >  XML-HEADER-CTR
                                    GO TO     F60CB-FN.
      *N60CD.    NOTE *BUILD INTO XML AREA                *.
       F60CD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-HEADER-LINE (J60CBR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60CD-FN. EXIT.
       F60CB-900. GO TO F60CB-A.
       F60CB-FN. EXIT.
      *N60EB.    NOTE *STRING COMMON INTO XML-OUTPUT      *.
       F60EB.                                                           lv10
           MOVE        1                        TO J60EBR
                                    GO TO     F60EB-B.
       F60EB-A.
           ADD         1                        TO J60EBR.
       F60EB-B.
           IF          J60EBR                   >  XML-COMMON-CTR
                                    GO TO     F60EB-FN.
      *N60ED.    NOTE *BUILD INTO XML AREA                *.
       F60ED.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-COMMON-LINE (J60EBR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60ED-FN. EXIT.
       F60EB-900. GO TO F60EB-A.
       F60EB-FN. EXIT.
      *N60KB.    NOTE *STRING DOC-INFO INTO XML-OUTPUT    *.
       F60KB.                                                           lv10
           MOVE        1                        TO J60KBR
                                    GO TO     F60KB-B.
       F60KB-A.
           ADD         1                        TO J60KBR.
       F60KB-B.
           IF          J60KBR                   >  XML-DOC-INFO-CTR
                                    GO TO     F60KB-FN.
      *N60KD.    NOTE *BUILD INTO XML AREA                *.
       F60KD.                                                           lv15
           ADD         1 TO XML-PT.
                 IF    J60KBR = 4                                       DOT
      *TRUNCATION FOR <DOCTYPENAME>
           MOVE        XML-DOC-INFO-LINE (J60KBR) TO
           XML-LINE
           PERFORM     F85BB THRU F85BB-FN
           MOVE        XML-LINE TO XM90-THTML (XML-PT)
                 ELSE
           MOVE        XML-DOC-INFO-LINE (J60KBR) TO
           XM90-THTML (XML-PT).
           MOVE        01 TO XM90-CHTML (XML-PT).                       DOT
       F60KD-FN. EXIT.
       F60KB-900. GO TO F60KB-A.
       F60KB-FN. EXIT.
      *N60MA.    NOTE *STRING INDEX INTO XML-OUTPUT       *.
       F60MA.                                                           lv10
           ADD         1 TO XML-PT
           MOVE        XML-INDEX-INFO TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60MA-FN. EXIT.
      *N60MB.    NOTE *MOVE THE ACCOUNT DETAILS           *.
       F60MB.         EXIT.                                             lv10
      *N60MD.    NOTE *MOVE ACCOUNT VALUES                *.
       F60MD.                                                           lv15
           MOVE        V180-CTIDA TO XML-CTIDA
           MOVE        V180-CTIDN TO XML-CTIDN
           MOVE        V180-PRCSN TO XML-PRCSN.
       F60MD-FN. EXIT.
      *N60MF.    NOTE *STRING INDEX1 INTO XML-OUTPUT      *.
       F60MF.                                                           lv15
           MOVE        1                        TO J60MFR
                                    GO TO     F60MF-B.
       F60MF-A.
           ADD         1                        TO J60MFR.
       F60MF-B.
           IF          J60MFR                   >  XML-INDEX1-CTR
                                    GO TO     F60MF-FN.
      *N60MG.    NOTE *BUILD INTO XML AREA                *.
       F60MG.                                                           lv20
           ADD         1 TO XML-PT
           MOVE        XML-INDEX1-LINE (J60MFR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60MG-FN. EXIT.
       F60MF-900. GO TO F60MF-A.
       F60MF-FN. EXIT.
       F60MB-FN. EXIT.
      *N60MI.    NOTE *STRING INDEX2 INTO XML-OUTPUT      *.
       F60MI.                                                           lv10
           MOVE        1                        TO J60MIR
                                    GO TO     F60MI-B.
       F60MI-A.
           ADD         1                        TO J60MIR.
       F60MI-B.
           IF          J60MIR                   >  XML-INDEX2-CTR
                                    GO TO     F60MI-FN.
      *N60MK.    NOTE *BUILD INTO XML AREA                *.
       F60MK.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-INDEX2-LINE (J60MIR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60MK-FN. EXIT.
       F60MI-900. GO TO F60MI-A.
       F60MI-FN. EXIT.
      *N60MP.    NOTE *STRING INDEX8 INTO XML-OUTPUT      *.
       F60MP.                                                           lv10
           MOVE        1                        TO J60MPR
                                    GO TO     F60MP-B.
       F60MP-A.
           ADD         1                        TO J60MPR.
       F60MP-B.
           IF          J60MPR                   >  XML-INDEX8-CTR
                                    GO TO     F60MP-FN.
      *N60MQ.    NOTE *BUILD INTO XML AREA                *.
       F60MQ.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-INDEX8-LINE (J60MPR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60MQ-FN. EXIT.
       F60MP-900. GO TO F60MP-A.
       F60MP-FN. EXIT.
      *N60MZ.    NOTE *STRING EO-INDEX INTO XML-OUTPUT    *.
       F60MZ.                                                           lv10
           ADD         1 TO XML-PT
           MOVE        XML-EO-INDEX-INFO TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60MZ-FN. EXIT.
      *N60ZB.    NOTE *STRING FOOTER INTO XML-OUTPUT      *.
       F60ZB.                                                           lv10
           MOVE        1                        TO J60ZBR
                                    GO TO     F60ZB-B.
       F60ZB-A.
           ADD         1                        TO J60ZBR.
       F60ZB-B.
           IF          J60ZBR                   >  XML-FOOTER-CTR
                                    GO TO     F60ZB-FN.
      *N60ZD.    NOTE *BUILD INTO XML AREA                *.
       F60ZD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-FOOTER-LINE (J60ZBR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60ZD-FN. EXIT.
       F60ZB-900. GO TO F60ZB-A.
       F60ZB-FN. EXIT.
      *N60ZO.    NOTE *SEND END OF XML AREA               *.
       F60ZO.                                                           lv10
           ADD         1 TO XML-PT
           MOVE        SPACES TO XM90-THTML (XML-PT)
           MOVE        99 TO XM90-CHTML (XML-PT).
       F60ZO-FN. EXIT.
      *N60ZQ.    NOTE *MOVE  LENGTH OF XML BLOB           *.
       F60ZQ.                                                           lv10
           COMPUTE     XM90-QBLCK = XML-PT.
       F60ZQ-FN. EXIT.
       F60-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *BUILD IMAGE RETURN AREA            *
      *               *                                   *
      *               *************************************.
       F70.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0280C2' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N70BA.    NOTE *INITIALIZE LINE COUNTER            *.
       F70BA.                                                           lv10
           MOVE        0 TO XML-PT.
       F70BA-FN. EXIT.
      *N70CB.    NOTE *STRING IMAGE INTO IM-OUTPUT        *.
       F70CB.                                                           lv10
           MOVE        1                        TO J70CBR
                                    GO TO     F70CB-B.
       F70CB-A.
           ADD         1                        TO J70CBR.
       F70CB-B.
           IF          J70CBR                   >  XML-IMAGE-CTR
                                    GO TO     F70CB-FN.
      *N70CD.    NOTE *BUILD INTO IM AREA                 *.
       F70CD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-IMAGE-LINE (J70CBR) TO
           IM90-THTML (XML-PT)
           MOVE        01 TO IM90-CHTML (XML-PT).
       F70CD-FN. EXIT.
       F70CB-900. GO TO F70CB-A.
       F70CB-FN. EXIT.
      *N70EB.    NOTE *STRING INFO INTO IM-OUTPUT         *.
       F70EB.                                                           lv10
           MOVE        1                        TO J70EBR
                                    GO TO     F70EB-B.
       F70EB-A.
           ADD         1                        TO J70EBR.
       F70EB-B.
           IF          J70EBR                   >  XML-INFO-CTR
                                    GO TO     F70EB-FN.
      *N70ED.    NOTE *BUILD INTO IM AREA                 *.
       F70ED.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-INFO-LINE (J70EBR) TO
           IM90-THTML (XML-PT)
           MOVE        01 TO IM90-CHTML (XML-PT).
       F70ED-FN. EXIT.
       F70EB-900. GO TO F70EB-A.
       F70EB-FN. EXIT.
      *N70ZO.    NOTE *SEND END OF IM AREA                *.
       F70ZO.                                                           lv10
           ADD         1 TO XML-PT
           MOVE        SPACES TO IM90-THTML (XML-PT)
           MOVE        99 TO IM90-CHTML (XML-PT).
       F70ZO-FN. EXIT.
      *N70ZQ.    NOTE *MOVE LENGTH OF IM BLOB             *.
       F70ZQ.                                                           lv10
           COMPUTE     IM90-QBLCK = XML-PT.
       F70ZQ-FN. EXIT.
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
      *N85BB.    NOTE *TRUNCATE TRAILING SPACES FROM      *.
       F85BB.                                                           lv10
      *DATA BETWEEN XML TAGS
           MOVE        SPACES TO XML-END-TAG.
      *N85CC.    NOTE *SAVE OFF END TAG                   *.
       F85CC.                                                           lv15
           MOVE        80                       TO J85CCR
                                    GO TO     F85CC-B.
       F85CC-A.
           SUBTRACT 1                         FROM J85CCR.
       F85CC-B.
           IF          J85CCR                   <  1
                                    GO TO     F85CC-FN.
                 IF    XML-LINE-CHAR (J85CCR) = '<'                     DOT
           MOVE        J85CCR TO XML-SPOS
           COMPUTE     XML-EPOS = 80 - J85CCR + 1
           MOVE        XML-LINE (XML-SPOS:XML-EPOS) TO
           XML-END-TAG
               GO TO     F85CC-FN.
       F85CC-900. GO TO F85CC-A.
       F85CC-FN. EXIT.
      *N85CG.    NOTE *DECREMENT POSITION IN LINE         *.
       F85CG.                                                           lv15
           SUBTRACT    1 FROM J85CCR.
                 IF    J85CCR < 1                                       DOT
               GO TO     F85BB-FN.
       F85CG-FN. EXIT.
      *N85CK.    NOTE *FIND WHERE DATA STARTS             *.
       F85CK.                                                           lv15
           MOVE        J85CCR                   TO J85CKR
                                    GO TO     F85CK-B.
       F85CK-A.
           SUBTRACT 1                         FROM J85CKR.
       F85CK-B.
           IF          J85CKR                   <  1
                                    GO TO     F85CK-FN.
      *AND MOVE END TAG IN
                 IF    XML-LINE-CHAR (J85CKR) > ' '                     DOT
           COMPUTE     XML-SPOS = J85CKR + 1
           COMPUTE     XML-EPOS = 80 - J85CKR
           MOVE        SPACES TO
           XML-LINE (XML-SPOS:XML-EPOS)
           MOVE        XML-END-TAG (1:XML-EPOS) TO
           XML-LINE (XML-SPOS:XML-EPOS)
               GO TO     F85CK-FN.
       F85CK-900. GO TO F85CK-A.
       F85CK-FN. EXIT.
       F85BB-FN. EXIT.
       F9099-ITER-FN.  GO TO F05.
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
