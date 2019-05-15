       IDENTIFICATION DIVISION.                                         CI0444
       PROGRAM-ID.  CI0444P.                                            CI0444
      *AUTHOR.         XML BUILD.                                       CI0444
      *DATE-COMPILED.   09/08/14.                                       CI0444
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
       ENVIRONMENT DIVISION.                                            CI0444
       CONFIGURATION SECTION.                                           CI0444
       SOURCE-COMPUTER. IBM-370.                                        CI0444
       OBJECT-COMPUTER. IBM-370.                                        CI0444
       DATA DIVISION.                                                   CI0444
       WORKING-STORAGE SECTION.                                         CI0444
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0348           PIC X(08)  VALUE 'CI0348P '.                AM0348
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
                        PICTURE 9(2).                                   CI0444
      *!WI pl=WD120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0444
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0444
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0444
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0444
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0444
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0444
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0444
      **----------------------------------------------------------------AM0348
      **  DATE WORKING STORAGE FOR CI0348                               AM0348
      **----------------------------------------------------------------AM0348
      *                                                                 AM0348
      *!WF DSP=LK DSL=V2 SEL=19 FOR=I LEV=1                             AM0348
       01                 LK00.                                         CI0444
          05              LK00-SUITE.                                   CI0444
            15       FILLER         PICTURE  X(00043).                  CI0444
       01                 LK19  REDEFINES      LK00.                    CI0444
            10            LK19-DXTMS2 PICTURE  X(26).                   CI0444
            10            LK19-NIOBJC PICTURE  X(17).                   CI0444
      *                                                                 AM0348
      **---------------------------------------------------------------*AM0348
      **  WORKING STORAGE FOR CASE DOC DATE                             AM0348
      **---------------------------------------------------------------*AM0348
      *!WI pl=WF180                                                     AM0348
       01 7-CSTDTE                                                      AM0348
                        PICTURE X(8).                                   CI0444
       01 FILLER REDEFINES 7-CSTDTE.                                    AM0348
      *!WI pl=WF220                                                     AM0348
          05 7-DTGCY                                                    AM0348
                        PICTURE 9(4).                                   CI0444
      *!WI pl=WF240                                                     AM0348
          05 7-DTGMM                                                    AM0348
                        PICTURE 9(2).                                   CI0444
      *!WI pl=WF260                                                     AM0348
          05 7-DTGDD                                                    AM0348
                        PICTURE 9(2).                                   CI0444
      *                                                                 AM0348
      *!WI pl=WF300                                                     AM0348
       01 7-CASE-DTJUL                                                  AM0348
                        PICTURE 9(7).                                   CI0444
       01 FILLER REDEFINES 7-CASE-DTJUL.                                AM0348
      *!WI pl=WF340                                                     AM0348
          05 7-CASE-DTGCY                                               AM0348
                        PICTURE 9(4).                                   CI0444
      *!WI pl=WF360                                                     AM0348
          05 7-CASE-DTJDD                                               AM0348
                        PICTURE 9(3).                                   CI0444
      *HH & MN & SS TO CALCULATE THE DTJDS                              AM0348
      *!WI pl=WF400                                                     AM0348
       01 WF01-DTTHH                                                    AM0348
                        PICTURE 9(2).                                   CI0444
      *!WI pl=WF420                                                     AM0348
       01 WF01-DTTMN                                                    AM0348
                        PICTURE 9(2).                                   CI0444
      *!WI pl=WF440                                                     AM0348
       01 WF01-DTTSS                                                    AM0348
                        PICTURE 9(2).                                   CI0444
      *DECIMAL WHICH WILL BE CONVERTED TO HEXADECIMAL (15 DIGITS)       AM0348
       01 WF01-DECIMAL.                                                 AM0348
       05 WF01-DTTCY    PIC 9.                                          AM0348
      *!WI pl=WF520                                                     AM0348
          05 WF01-DTJDD                                                 AM0348
                        PICTURE 9(3).                                   CI0444
       05 WF01-DTJDS PIC 9(5) VALUE ZEROES.                             AM0348
      *!WI pl=WF560                                                     AM0348
       05 WF01-DTTNN                                                    AM0348
                        PICTURE 9(6).                                   CI0444
      *DECLARE THE INDEX USED TO CONVERT                                AM0348
       01 WF01-INDEX PIC 9(2) VALUE ZEROES.                             AM0348
      *DECLARE THE QUOTIENT USED TO CONVERT                             AM0348
       01 WF01-QUOTI PIC 9(15) VALUE ZEROES.                            AM0348
      *DECLARE THE REMAINDER USED TO CONVERT                            AM0348
      *!WI pl=WF680                                                     AM0348
       01 WF01-NRDAD                                                    AM0348
                        PICTURE 9(2).                                   CI0444
      *                                                                 AM0348
      **----------------------------------------------------------------AM0348
      ** CASEDOC-ID GENERATED                                           AM0348
      **----------------------------------------------------------------AM0348
      *                                                                 AM0348
       01 WF01-NIOBJC.                                                  AM0348
          05 WF01-HEXT PIC X(13) VALUE ZEROES.                          AM0348
          05 FILLER        PIC X(4)    VALUE '.001'.                    AM0348
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
          05  WT01-CASE-ID     PIC X(13).
          05  FILLER           PIC X(04).
      *
       01  WT02-DATE-TIME.
            05 WT02-DTGRG      PIC X(10).
            05 FILLER          PIC X VALUE SPACE.
      *!WI
            05 WT02-DTTHH
                        PICTURE 9(2).                                   CI0444
            05 FILLER          PIC X VALUE ':'.
      *!WI
            05 WT02-DTTMN
                        PICTURE 9(2).                                   CI0444
            05 FILLER          PIC X VALUE ':'.
      *!WI
            05 WT02-DTTSS
                        PICTURE 9(2).                                   CI0444
      *
        01  WT03-DATE-TIME.
            05 WT03-DTGRG      PIC X(10).
            05 FILLER          PIC X VALUE SPACE.
      *!WI
            05 WT03-DTTHH
                        PICTURE 9(2).                                   CI0444
            05 FILLER          PIC X VALUE ':'.
      *!WI
            05 WT03-DTTMN
                        PICTURE 9(2).                                   CI0444
            05 FILLER          PIC X VALUE ':'.
      *!WI
            05 WT03-DTTSS
                        PICTURE 9(2).                                   CI0444
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU076
      *                   MS03                                          ADU076
       01   DEBUT-WSS.                                                  CI0444
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0444
            05   IK     PICTURE X.                                      CI0444
       01  CONSTANTES-PAC.                                              CI0444
           05  FILLER  PICTURE X(87)   VALUE                            CI0444
                     '6015 CAT09/08/14CI0444ADMIN   14:35:27CI0444P AMERCI0444
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0444
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0444
           05  NUGNA   PICTURE X(5).                                    CI0444
           05  APPLI   PICTURE X(3).                                    CI0444
           05  DATGN   PICTURE X(8).                                    CI0444
           05  PROGR   PICTURE X(6).                                    CI0444
           05  CODUTI  PICTURE X(8).                                    CI0444
           05  TIMGN   PICTURE X(8).                                    CI0444
           05  PROGE   PICTURE X(8).                                    CI0444
           05  COBASE  PICTURE X(4).                                    CI0444
           05  DATGNC  PICTURE X(10).                                   CI0444
           05  RELEAS  PICTURE X(7).                                    CI0444
           05  DATGE   PICTURE X(10).                                   CI0444
           05  DATSQ   PICTURE X(10).                                   CI0444
       01  DATCE.                                                       CI0444
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0444
         05  DATOR.                                                     CI0444
           10  DATOA  PICTURE XX.                                       CI0444
           10  DATOM  PICTURE XX.                                       CI0444
           10  DATOJ  PICTURE XX.                                       CI0444
       01   VARIABLES-CONDITIONNELLES.                                  CI0444
            05                  FT      PICTURE X VALUE '0'.            CI0444
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0444
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0444
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU076
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU076
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU076
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J60CBR PICTURE S9(4) VALUE  ZERO.
            05           J60EBR PICTURE S9(4) VALUE  ZERO.
            05           J60IBR PICTURE S9(4) VALUE  ZERO.
            05           J60KBR PICTURE S9(4) VALUE  ZERO.
            05           J60MFR PICTURE S9(4) VALUE  ZERO.
            05           J60MIR PICTURE S9(4) VALUE  ZERO.
            05           J60MLR PICTURE S9(4) VALUE  ZERO.
            05           J60MPR PICTURE S9(4) VALUE  ZERO.
            05           J60ZBR PICTURE S9(4) VALUE  ZERO.
            05           J65CBR PICTURE S9(4) VALUE  ZERO.
            05           J65EBR PICTURE S9(4) VALUE  ZERO.
            05           J70CBR PICTURE S9(4) VALUE  ZERO.
            05           J70EBR PICTURE S9(4) VALUE  ZERO.
            05           J70GBR PICTURE S9(4) VALUE  ZERO.
            05           J70IBR PICTURE S9(4) VALUE  ZERO.
            05           J70KBR PICTURE S9(4) VALUE  ZERO.
            05           J70ZBR PICTURE S9(4) VALUE  ZERO.
            05           J90CCR PICTURE S9(4) VALUE  ZERO.
            05           J90CKR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0444
      *COPYBOOK WITH XML TEXT

       COPY CI0444C1.
      *COPYBOOK WITH IMAGE TEXT

       COPY CI0444C2.
      *COPYBOOK WITH SSD TEXT

       COPY CI0444C3.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **             THIS SEGMENT IS THE INPUT LINKAGE FOR CI0326      *
      ******************************************************************
      *
      *!WF DSP=V2 DSL=V2 SEL=26 FOR=I LEV=1 PLT=70
       01                 V200.                                         CI0444
          05              V200-SUITE.                                   CI0444
            15       FILLER         PICTURE  X(00337).                  CI0444
       01                 V226  REDEFINES      V200.                    CI0444
            10            V226-CBUSN  PICTURE  X(8).                    CI0444
            10            V226-CPCCDF PICTURE  99.                      CI0444
            10            V226-MCODE  PICTURE  X(30).                   CI0444
            10            V226-MRGNN  PICTURE  X(8).                    CI0444
            10            V226-CCSDSA PICTURE  X(1).                    CI0444
            10            V226-DXTMSA PICTURE  X(26).                   CI0444
            10            V226-DXTMS2 PICTURE  X(26).                   CI0444
            10            V226-CLTIN  PICTURE  9(12).                   CI0444
            10            V226-C199.                                    CI0444
            11            V226-CLID.                                    CI0444
            12            V226-CLIDO  PICTURE  9(3).                    CI0444
            12            V226-CLIDN.                                   CI0444
            13            V226-CLIDNP PICTURE  X(12).                   CI0444
            13            V226-CLIDND PICTURE  9(8).                    CI0444
            10            V226-QNACT  PICTURE  9(3).                    CI0444
            10            V226-C299.                                    CI0444
            11            V226-CTID.                                    CI0444
            12            V226-CTIDA  PICTURE  9(3).                    CI0444
            12            V226-CTIDN.                                   CI0444
            13            V226-CTIDNP PICTURE  X(13).                   CI0444
            13            V226-CTIDND PICTURE  9(11).                   CI0444
            10            V226-PRCSN  PICTURE  X(9).                    CI0444
            10            V226-CLTINV PICTURE  9(12).                   CI0444
            10            V226-CTID01.                                  CI0444
            11            V226-CACTID PICTURE  9(3).                    CI0444
            11            V226-CTIDNB.                                  CI0444
            12            V226-CTIDP1 PICTURE  X(13).                   CI0444
            12            V226-CTIDNA PICTURE  9(11).                   CI0444
            10            V226-CLID01.                                  CI0444
            11            V226-CLIDA  PICTURE  9(3).                    CI0444
            11            V226-CLID1A.                                  CI0444
            12            V226-CLIDP1 PICTURE  X(12).                   CI0444
            12            V226-CLIDNA PICTURE  9(8).                    CI0444
            10            V226-FILLER PICTURE  X(100).                  CI0444
      *
      ******************************************************************
      **         THIS SEGMENT IS THE XML LINKAGE FOR IMAGE UTILITY     *
      ******************************************************************
      *
      *!WF DSP=XM DSL=QT SEL=90 FOR=I LEV=1 PLT=75
       01                 XM00.                                         CI0444
          05              XM00-SUITE.                                   CI0444
            15       FILLER         PICTURE  X(40406).                  CI0444
       01                 XM90  REDEFINES      XM00.                    CI0444
            10            XM90-QBLCK  PICTURE  9(6).                    CI0444
            10            XM90-QT9O.                                    CI0444
            11            XM90-QT9B                                     CI0444
                          OCCURS       200     TIMES.                   CI0444
            12            XM90-CHTML  PICTURE  99.                      CI0444
            12            XM90-THTML  PICTURE  X(200).                  CI0444
      *!WF DSP=IM DSL=QT SEL=90 FOR=I LEV=1 PLT=80
       01                 IM00.                                         CI0444
          05              IM00-SUITE.                                   CI0444
            15       FILLER         PICTURE  X(40406).                  CI0444
       01                 IM90  REDEFINES      IM00.                    CI0444
            10            IM90-QBLCK  PICTURE  9(6).                    CI0444
            10            IM90-QT9O.                                    CI0444
            11            IM90-QT9B                                     CI0444
                          OCCURS       200     TIMES.                   CI0444
            12            IM90-CHTML  PICTURE  99.                      CI0444
            12            IM90-THTML  PICTURE  X(200).                  CI0444
      *
      ******************************************************************
      **       THIS SEGMENT IS THE XML LINKAGE FOR SSD                 *
      ******************************************************************
      *
      *!WF DSP=SD DSL=QT SEL=90 FOR=I LEV=1 PLT=85
       01                 SD00.                                         CI0444
          05              SD00-SUITE.                                   CI0444
            15       FILLER         PICTURE  X(40406).                  CI0444
       01                 SD90  REDEFINES      SD00.                    CI0444
            10            SD90-QBLCK  PICTURE  9(6).                    CI0444
            10            SD90-QT9O.                                    CI0444
            11            SD90-QT9B                                     CI0444
                          OCCURS       200     TIMES.                   CI0444
            12            SD90-CHTML  PICTURE  99.                      CI0444
            12            SD90-THTML  PICTURE  X(200).                  CI0444
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0444
          05              MS00-SUITE.                                   CI0444
            15       FILLER         PICTURE  X(00542).                  CI0444
       01                 MS03  REDEFINES      MS00.                    CI0444
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0444
                          COMPUTATIONAL-3.                              CI0444
            10            MS03-CMSSF  PICTURE  XX.                      CI0444
            10            MS03-DU09.                                    CI0444
            11            MS03-CMESA  PICTURE  S9(9)                    CI0444
                          BINARY.                                       CI0444
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0444
                          BINARY.                                       CI0444
            11            MS03-CMESB  PICTURE  S9(9)                    CI0444
                          BINARY.                                       CI0444
            11            MS03-CMSST  PICTURE  S9(9)                    CI0444
                          BINARY.                                       CI0444
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0444
                          BINARY.                                       CI0444
            11            MS03-QELLAA PICTURE  S9(9)                    CI0444
                          BINARY.                                       CI0444
            11            MS03-TMESS4 PICTURE  X(512).                  CI0444
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0444
            10            MX11-QMSGS  PICTURE  9(03).                   CI0444
            10            MX11-PJ09                                     CI0444
                          OCCURS       025     TIMES.                   CI0444
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0444
                          COMPUTATIONAL-3.                              CI0444
            11            MX11-CMESB  PICTURE  S9(9)                    CI0444
                          BINARY.                                       CI0444
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V226
                                XM90
                                IM90
                                SD90
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0444
      *               *                                   *             CI0444
      *               *INITIALISATIONS                    *             CI0444
      *               *                                   *             CI0444
      *               *************************************.            CI0444
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
      *N02XA.    NOTE *---> ALL XML LINES                 *.
       F02XA.                                                           lv10
           INITIALIZE  XML-HEADER-TAGS
           XML-COMMON-TAGS
           XML-CASE-TAGS
           XML-DOC-INFO-TAGS
           XML-INDEX1-TAGS
           XML-INDEX2-TAGS
           XML-INDEX8-TAGS
           XML-FOOTER-TAGS
           XML-IMAGE-TAGS
           XML-IMAGE-INFO
           SSD-HEADER-TAGS
           SSD-CASE-TAGS
           SSD-USER-TAGS
           SSD-DOCUMENT-TAGS
           SSD-DOC-USER-TAGS
           SSD-FOOTER-TAGS.
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0444
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0444
      *               *                                   *             CI0444
      *               *FIN DE TRAITEMENT                  *             CI0444
      *               *                                   *             CI0444
      *               *************************************.            CI0444
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0444
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
      *N30BB.    NOTE *GET DCN NUMBER FROM CI0348         *.
       F30BB.                                                           lv10
           MOVE        V226-DXTMS2 TO LK19-DXTMS2
           PERFORM     F92BB THRU F92BB-FN.
       F30BB-FN. EXIT.
      *N30FB.    NOTE *FORMAT CASE / DOC IDS              *.
       F30FB.                                                           lv10
           MOVE        LK19-NIOBJC TO WT01-DOC-ID.
       F30FB-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *BUILD XML LINES                    *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *********************************
      **  SEE COPYBOOK 'CI0326C1' FOR *
      **  TAGS AND VALUES             *
      *********************************
      *N40BE.    NOTE *BUILD DOC TYPE INFO                *.
       F40BE.                                                           lv10
      *
           MOVE        V226-MCODE TO XML-DOC-TYPE.
       F40BE-FN. EXIT.
      *N40DB.    NOTE * MOVE CASE / SOURCE IDS            *.
       F40DB.                                                           lv10
      *
           MOVE        WT01-CASE-ID TO XML-CASE-ID
           MOVE        WT01-DOC-ID TO XML-DOC-ID
           XML-COMP-DCN.
       F40DB-FN. EXIT.
      *N40EB.    NOTE * SET BUSINESS AREA USING REGION    *.
       F40EB.                                                           lv10
      *********************************
      *THIS IS REQUIRED FOR TESTING IN
      *VARIOUS REGIONS
      *********************************
                 IF    V226-MRGNN (1:4) = 'CICD'                        DOT
           MOVE        '0112' TO XML-AREA.
                 IF    V226-MRGNN (1:4) = 'CICM'                        DOT
           MOVE        '0112' TO XML-AREA.
                 IF    V226-MRGNN (1:4) = 'CICP'                        DOT
           MOVE        '0110' TO XML-AREA.
       F40EB-FN. EXIT.
      *N40GB.    NOTE *SET SOURCE NAME                    *.
       F40GB.                                                           lv10
      *NO TRAILING SPACES ALLOWED !
           STRING      CN-SOURCE-NAME
           V226-CBUSN
           CN-EO-SOURCE-NAME
           DELIMITED BY SPACES
           INTO XML-SOURCE.
       F40GB-FN. EXIT.
      *N40IB.    NOTE * SET WEB TIME STAMP AS SCAN TS     *.
       F40IB.                                                           lv10
           MOVE        V226-DXTMSA TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGRG TO WT02-DTGRG
           MOVE        7-DB2-DTTHH TO WT02-DTTHH
           MOVE        7-DB2-DTTMN TO WT02-DTTMN
           MOVE        7-DB2-DTTSS TO WT02-DTTSS
           MOVE        WT02-DATE-TIME TO XML-SCAN-TS.
       F40IB-FN. EXIT.
      *N40IE.    NOTE * SET RPC TIME STAMP AS RELA-TS     *.
       F40IE.                                                           lv10
           MOVE        V226-DXTMS2 TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGRG TO WT03-DTGRG
           MOVE        7-DB2-DTTHH TO WT03-DTTHH
           MOVE        7-DB2-DTTMN TO WT03-DTTMN
           MOVE        7-DB2-DTTSS TO WT03-DTTSS
           MOVE        WT03-DATE-TIME TO XML-RELA-TS.
       F40IE-FN. EXIT.
      *N40KB.    NOTE *FORMAT CLIENT INDEX                *.
       F40KB.                                                           lv10
      *
           MOVE        V226-CLIDO TO XML-CLIDO
           MOVE        V226-CLIDN TO XML-CLIDN.
       F40KB-FN. EXIT.
      *N40KE.    NOTE *FORMAT TIN INDEX                   *.
       F40KE.                                                           lv10
      *
           MOVE        V226-CLTIN TO XML-CLTIN.
       F40KE-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *BUILD IMAGE INFO                   *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *********************************
      **  SEE COPYBOOK 'CI0326C2' FOR *
      **  TAGS AND VALUES             *
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
      *
           MOVE        WT01-DOC-ID TO XML-IMAGE-DOC-ID.
       F50CB-FN. EXIT.
      *N50EB.    NOTE *SET BUSINESS AREA USING REGION     *.
       F50EB.                                                           lv10
      *********************************
      *THIS IS REQUIRED FOR TESTING IN
      *VARIOUS REGIONS
      *********************************
                 IF    V226-MRGNN (1:4) = 'CICD'                        DOT
           MOVE        '0112' TO XML-IMAGE-AREA.
                 IF    V226-MRGNN (1:4) = 'CICM'                        DOT
           MOVE        '0112' TO XML-IMAGE-AREA.
                 IF    V226-MRGNN (1:4) = 'CICP'                        DOT
           MOVE        '0110' TO XML-IMAGE-AREA.
       F50EB-FN. EXIT.
      *N50GB.    NOTE *SET SOURCE NAME                    *.
       F50GB.                                                           lv10
      *NO TRAILING SPACES ALLOWED !
           STRING      CN-SOURCE-NAME
           V226-CBUSN
           CN-EO-SOURCE-NAME
           DELIMITED BY SPACES
           INTO XML-IMAGE-SOURCE.
       F50GB-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *BUILD SSD INFO                     *
      *               *                                   *
      *               *************************************.
       F55.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0326C3' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N55BB.    NOTE *BUILD SSD CASE ID / DOC ID         *.
       F55BB.                                                           lv10
           MOVE        WT01-CASE-ID TO SSD-CASE-ID
           MOVE        WT01-DOC-ID TO SSD-DOC-ID
           SSD-DOC-USER-01.
       F55BB-FN. EXIT.
      *N55BE.    NOTE *BUILD SSD DOC TYPE                 *.
       F55BE.                                                           lv10
           MOVE        XML-DOC-TYPE TO SSD-DOC-TYPE.
       F55BE-FN. EXIT.
      *N55EC.    NOTE *BUILD SSD USER DATA 1              *.
       F55EC.                                                           lv10
           MOVE        V226-CTIDN TO SSD-USER-DATA1.
       F55EC-FN. EXIT.
      *N55EE.    NOTE *BUILD SSD USER DATA 2              *.
       F55EE.                                                           lv10
           MOVE        V226-CLTIN TO SSD-USER-DATA2.
       F55EE-FN. EXIT.
      *N55EG.    NOTE *BUILD SSD USER DATA 3              *.
       F55EG.                                                           lv10
           MOVE        V226-CLIDN TO SSD-USER-DATA3.
       F55EG-FN. EXIT.
      *N55GB.    NOTE *SET SOURCE NAME                    *.
       F55GB.                                                           lv10
           MOVE        V226-CBUSN TO SSD-CASE-SYSTEM.
       F55GB-FN. EXIT.
      *N55SB.    NOTE *BUILD SSD RECEIVE DATE TIME        *.
       F55SB.                                                           lv10
           MOVE        WT02-DATE-TIME TO SSD-SCAN-TS
           MOVE        WT02-DATE-TIME TO SSD-SCAN-TS-D.
       F55SB-FN. EXIT.
      *N55SE.    NOTE *BUILD SSD PROCESS DATE TIME        *.
       F55SE.                                                           lv10
           MOVE        WT03-DATE-TIME TO SSD-PROC-TS
           MOVE        WT03-DATE-TIME TO SSD-PROC-TS-D.
       F55SE-FN. EXIT.
      *N55SH.    NOTE *BUILD SSD SEND DATE TIME           *.
       F55SH.                                                           lv10
           MOVE        WT03-DATE-TIME TO SSD-SEND-TS
           MOVE        WT03-DATE-TIME TO SSD-SEND-TS-D.
       F55SH-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *BUILD XML RETURN AREA              *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0326C1' FOR  *
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
      *N60IA.    NOTE *BUILD <CASEINFO> TAG ONLY FOR      *.
       F60IA.    IF    V226-CCSDSA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F60IA-FN.
      *THOSE CASES WHERE IT IS REQUIRED
      *N60IB.    NOTE *STRING CASE INTO XML-OUTPUT        *.
       F60IB.                                                           lv12
           MOVE        1                        TO J60IBR
                                    GO TO     F60IB-B.
       F60IB-A.
           ADD         1                        TO J60IBR.
       F60IB-B.
           IF          J60IBR                   >  XML-CASE-CTR
                                    GO TO     F60IB-FN.
      *N60ID.    NOTE *BUILD INTO XML AREA                *.
       F60ID.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-CASE-LINE (J60IBR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60ID-FN. EXIT.
       F60IB-900. GO TO F60IB-A.
       F60IB-FN. EXIT.
       F60IA-FN. EXIT.
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
           PERFORM     F90BB THRU F90BB-FN
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
           MOVE        V226-CTIDA TO XML-CTIDA
           MOVE        V226-CTIDN TO XML-CTIDN
           MOVE        V226-PRCSN TO XML-PRCSN.
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
      *N60ML.    NOTE *STRING INDEX7 INTO XML-OUTPUT      *.
       F60ML.                                                           lv10
           MOVE        1                        TO J60MLR
                                    GO TO     F60ML-B.
       F60ML-A.
           ADD         1                        TO J60MLR.
       F60ML-B.
           IF          J60MLR                   >  XML-INDEX7-CTR
                                    GO TO     F60ML-FN.
      *N60MM.    NOTE *BUILD INTO XML AREA                *.
       F60MM.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-INDEX7-LINE (J60MLR) TO
           XM90-THTML (XML-PT)
           MOVE        01 TO XM90-CHTML (XML-PT).
       F60MM-FN. EXIT.
       F60ML-900. GO TO F60ML-A.
       F60ML-FN. EXIT.
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
      *N60ZD.    NOTE *STRING EO-INDEX INTO XML-OUTPUT    *.
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
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *BUILD IMAGE RETURN AREA            *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0326C2' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N65BA.    NOTE *INITIALIZE LINE COUNTER            *.
       F65BA.                                                           lv10
           MOVE        0 TO XML-PT.
       F65BA-FN. EXIT.
      *N65CB.    NOTE *STRING IMAGE INTO IM-OUTPUT        *.
       F65CB.                                                           lv10
           MOVE        1                        TO J65CBR
                                    GO TO     F65CB-B.
       F65CB-A.
           ADD         1                        TO J65CBR.
       F65CB-B.
           IF          J65CBR                   >  XML-IMAGE-CTR
                                    GO TO     F65CB-FN.
      *N65CD.    NOTE *BUILD INTO IM AREA                 *.
       F65CD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-IMAGE-LINE (J65CBR) TO
           IM90-THTML (XML-PT)
           MOVE        01 TO IM90-CHTML (XML-PT).
       F65CD-FN. EXIT.
       F65CB-900. GO TO F65CB-A.
       F65CB-FN. EXIT.
      *N65EB.    NOTE *STRING INFO INTO IM-OUTPUT         *.
       F65EB.                                                           lv10
           MOVE        1                        TO J65EBR
                                    GO TO     F65EB-B.
       F65EB-A.
           ADD         1                        TO J65EBR.
       F65EB-B.
           IF          J65EBR                   >  XML-INFO-CTR
                                    GO TO     F65EB-FN.
      *N65ED.    NOTE *BUILD INTO IM AREA                 *.
       F65ED.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        XML-INFO-LINE (J65EBR) TO
           IM90-THTML (XML-PT)
           MOVE        01 TO IM90-CHTML (XML-PT).
       F65ED-FN. EXIT.
       F65EB-900. GO TO F65EB-A.
       F65EB-FN. EXIT.
      *N65ZO.    NOTE *SEND END OF IM AREA                *.
       F65ZO.                                                           lv10
           ADD         1 TO XML-PT
           MOVE        SPACES TO IM90-THTML (XML-PT)
           MOVE        99 TO IM90-CHTML (XML-PT).
       F65ZO-FN. EXIT.
      *N65ZQ.    NOTE *MOVE LENGTH OF IM BLOB             *.
       F65ZQ.                                                           lv10
           COMPUTE     IM90-QBLCK = XML-PT.
       F65ZQ-FN. EXIT.
       F65-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *BUILD SSD RETURN AREA              *
      *               *                                   *
      *               *************************************.
       F70.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0326C3' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N70BA.    NOTE *INITIALIZE LINE COUNTER            *.
       F70BA.                                                           lv10
           MOVE        0 TO XML-PT.
       F70BA-FN. EXIT.
      *N70CB.    NOTE *STRING HEADER INTO SSD-OUTPUT      *.
       F70CB.                                                           lv10
           MOVE        1                        TO J70CBR
                                    GO TO     F70CB-B.
       F70CB-A.
           ADD         1                        TO J70CBR.
       F70CB-B.
           IF          J70CBR                   >  SSD-HEADER-CTR
                                    GO TO     F70CB-FN.
      *N70CD.    NOTE *BUILD INTO SSD AREA                *.
       F70CD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        SSD-HEADER-LINE (J70CBR) TO
           SD90-THTML (XML-PT)
           MOVE        01 TO SD90-CHTML (XML-PT).
       F70CD-FN. EXIT.
       F70CB-900. GO TO F70CB-A.
       F70CB-FN. EXIT.
      *N70EB.    NOTE *STRING CASE INTO SSD-OUTPUT        *.
       F70EB.                                                           lv10
           MOVE        1                        TO J70EBR
                                    GO TO     F70EB-B.
       F70EB-A.
           ADD         1                        TO J70EBR.
       F70EB-B.
           IF          J70EBR                   >  SSD-CASE-CTR
                                    GO TO     F70EB-FN.
      *N70ED.    NOTE *BUILD INTO SSD AREA                *.
       F70ED.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        SSD-CASE-LINE (J70EBR) TO
           SD90-THTML (XML-PT)
           MOVE        01 TO SD90-CHTML (XML-PT).
       F70ED-FN. EXIT.
       F70EB-900. GO TO F70EB-A.
       F70EB-FN. EXIT.
      *N70GB.    NOTE *STRING CASE INTO SSD-OUTPUT        *.
       F70GB.                                                           lv10
           MOVE        1                        TO J70GBR
                                    GO TO     F70GB-B.
       F70GB-A.
           ADD         1                        TO J70GBR.
       F70GB-B.
           IF          J70GBR                   >  SSD-USER-CTR
                                    GO TO     F70GB-FN.
      *N70GD.    NOTE *BUILD INTO SSD AREA                *.
       F70GD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        SSD-USER-LINE (J70GBR) TO
           SD90-THTML (XML-PT)
           MOVE        01 TO SD90-CHTML (XML-PT).
       F70GD-FN. EXIT.
       F70GB-900. GO TO F70GB-A.
       F70GB-FN. EXIT.
      *N70IB.    NOTE *STRING DOC INTO SSD-OUTPUT         *.
       F70IB.                                                           lv10
           MOVE        1                        TO J70IBR
                                    GO TO     F70IB-B.
       F70IB-A.
           ADD         1                        TO J70IBR.
       F70IB-B.
           IF          J70IBR                   >  SSD-DOCUMENT-CTR
                                    GO TO     F70IB-FN.
      *N70ID.    NOTE *BUILD INTO SSD AREA                *.
       F70ID.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        SSD-DOCUMENT-LINE (J70IBR) TO
           SD90-THTML (XML-PT)
           MOVE        01 TO SD90-CHTML (XML-PT).
       F70ID-FN. EXIT.
       F70IB-900. GO TO F70IB-A.
       F70IB-FN. EXIT.
      *N70KB.    NOTE *STRING DOC-USER INTO SSD-OUTPUT    *.
       F70KB.                                                           lv10
           MOVE        1                        TO J70KBR
                                    GO TO     F70KB-B.
       F70KB-A.
           ADD         1                        TO J70KBR.
       F70KB-B.
           IF          J70KBR                   >  SSD-DOC-USER-CTR
                                    GO TO     F70KB-FN.
      *N70KD.    NOTE *BUILD INTO SSD AREA                *.
       F70KD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        SSD-DOC-USER-LINE (J70KBR) TO
           SD90-THTML (XML-PT)
           MOVE        01 TO SD90-CHTML (XML-PT).
       F70KD-FN. EXIT.
       F70KB-900. GO TO F70KB-A.
       F70KB-FN. EXIT.
      *N70ZB.    NOTE *STRING FOOTER INTO SSD-OUTPUT      *.
       F70ZB.                                                           lv10
           MOVE        1                        TO J70ZBR
                                    GO TO     F70ZB-B.
       F70ZB-A.
           ADD         1                        TO J70ZBR.
       F70ZB-B.
           IF          J70ZBR                   >  SSD-FOOTER-CTR
                                    GO TO     F70ZB-FN.
      *N70ZD.    NOTE *BUILD INTO SSD AREA                *.
       F70ZD.                                                           lv15
           ADD         1 TO XML-PT
           MOVE        SSD-FOOTER-LINE (J70ZBR) TO
           SD90-THTML (XML-PT)
           MOVE        01 TO SD90-CHTML (XML-PT).
       F70ZD-FN. EXIT.
       F70ZB-900. GO TO F70ZB-A.
       F70ZB-FN. EXIT.
      *N70ZO.    NOTE *SEND END OF SSD AREA               *.
       F70ZO.                                                           lv10
           ADD         1 TO XML-PT
           MOVE        SPACES TO SD90-THTML (XML-PT)
           MOVE        99 TO SD90-CHTML (XML-PT).
       F70ZO-FN. EXIT.
      *N70ZQ.    NOTE *MOVE  LENGTH OF XML BLOB           *.
       F70ZQ.                                                           lv10
           COMPUTE     SD90-QBLCK = XML-PT.
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
      *N90BB.    NOTE *TRUNCATE TRAILING SPACES FROM      *.
       F90BB.                                                           lv10
      *DATA BETWEEN XML TAGS
           MOVE        SPACES TO XML-END-TAG.
      *N90CC.    NOTE *SAVE OFF END TAG                   *.
       F90CC.                                                           lv15
           MOVE        80                       TO J90CCR
                                    GO TO     F90CC-B.
       F90CC-A.
           SUBTRACT 1                         FROM J90CCR.
       F90CC-B.
           IF          J90CCR                   <  1
                                    GO TO     F90CC-FN.
                 IF    XML-LINE-CHAR (J90CCR) = '<'                     DOT
           MOVE        J90CCR TO XML-SPOS
           COMPUTE     XML-EPOS = 80 - J90CCR + 1
           MOVE        XML-LINE (XML-SPOS:XML-EPOS) TO
           XML-END-TAG
               GO TO     F90CC-FN.
       F90CC-900. GO TO F90CC-A.
       F90CC-FN. EXIT.
      *N90CG.    NOTE *DECREMENT POSITION IN LINE         *.
       F90CG.                                                           lv15
           SUBTRACT    1 FROM J90CCR.
                 IF    J90CCR < 1                                       DOT
               GO TO     F90BB-FN.
       F90CG-FN. EXIT.
      *N90CK.    NOTE *FIND WHERE DATA STARTS             *.
       F90CK.                                                           lv15
           MOVE        J90CCR                   TO J90CKR
                                    GO TO     F90CK-B.
       F90CK-A.
           SUBTRACT 1                         FROM J90CKR.
       F90CK-B.
           IF          J90CKR                   <  1
                                    GO TO     F90CK-FN.
      *AND MOVE END TAG IN
                 IF    XML-LINE-CHAR (J90CKR) > ' '                     DOT
           COMPUTE     XML-SPOS = J90CKR + 1
           COMPUTE     XML-EPOS = 80 - J90CKR
           MOVE        SPACES TO
           XML-LINE (XML-SPOS:XML-EPOS)
           MOVE        XML-END-TAG (1:XML-EPOS) TO
           XML-LINE (XML-SPOS:XML-EPOS)
               GO TO     F90CK-FN.
       F90CK-900. GO TO F90CK-A.
       F90CK-FN. EXIT.
       F90BB-FN. EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N92BB.    NOTE *CALL FDC TRAN DETAIL UPDATE        *.            AM0348
       F92BB.                                                           lv10
      *                                                                 AM0348
      *********************************                                 AM0348
      ** CALL FDC TRAN DETAIL UPDATE  *                                 AM0348
      ** MODULE                       *                                 AM0348
      *********************************                                 AM0348
      *                                                                 AM0348
      *                                                                 AM0348
           CALL        CI0348 USING                                     AM0348
           DFHEIBLK                                                     AM0348
           DFHCOMMAREA                                                  AM0348
           LK19                                                         AM0348
           MS03                                                         AM0348
           MX11.                                                        AM0348
      *N92DB.    NOTE *NON DL1 ERROR                      *.            ADU076
       F92DB.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU076
                 NEXT SENTENCE ELSE GO TO     F92DB-FN.                 ADU076
      *OF A GIVEN SEVERITY                                              ADU076
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU076
           MOVE        CI0348 TO MS03-TMESS4 (IMS03R : 6)               ADU076
           ADD         +7 TO MS03-QELLAA                                ADU076
      *'E'            INT-RETURN-LEVEL
      *+1             INT-RETURN-NUMBER
      *MS03-DU09      INT-MSG-APPL
           MOVE                     ALL '1' TO FT GO TO F20.
       F92DB-FN. EXIT.
       F92BB-FN. EXIT.
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
