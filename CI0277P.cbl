       IDENTIFICATION DIVISION.                                         CI0277
       PROGRAM-ID.  CI0277P.                                            CI0277
      *AUTHOR.         eFUNDING FA HTML BUILD.                          CI0277
      *DATE-COMPILED.   09/08/14.                                       CI0277
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2013                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE FA     SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE FA     SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE FA           *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2013                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0277
       CONFIGURATION SECTION.                                           CI0277
       SOURCE-COMPUTER. IBM-370.                                        CI0277
       OBJECT-COMPUTER. IBM-370.                                        CI0277
       DATA DIVISION.                                                   CI0277
       WORKING-STORAGE SECTION.                                         CI0277
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *COPYBOOK WITH CONSTANTS USED IN HTML

       COPY CI0277CN.
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
       01  LENGTH-FIELDS.                                               AANA15
      * 9LSX FIELDS CONTAIN POINT OF LAST CHARACTER IN NAMES            AANA15
           05 7-NA10-9LSH    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSF    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSM    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSL    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9LSS    PIC S9(4) COMP  VALUE ZERO.                AANA15
      * 9TSPAC CONTAINS TOTAL NUMBER OF BLANKS NEEDED IN NAME.          AANA15
           05 7-NA10-9TSPAC  PIC S9(4) COMP  VALUE ZERO.                AANA15
      * 9STX FIELDS CONTAIN POINT OF FIRST NON-BLANK CHARACTER IN NAMES AANA15
           05 7-NA10-9STH    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STF    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STM    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STL    PIC S9(4) COMP  VALUE ZERO.                AANA15
           05 7-NA10-9STS    PIC S9(4) COMP  VALUE ZERO.                AANA15
       01  7-NA10-CLNAM.                                                AANA15
           05 7-NA10-CLNAMH.                                            AANA15
              10 7-NA10-9NMH      PIC X  OCCURS  6 TIMES.               AANA15
           05 7-NA10-CLNAMF.                                            AANA15
              10 7-NA10-9NMF      PIC X  OCCURS 20 TIMES.               AANA15
           05 7-NA10-CLNAMM.                                            AANA15
              10 7-NA10-9NMM      PIC X  OCCURS 15 TIMES.               AANA15
           05 7-NA10-CLNAML.                                            AANA15
              10 7-NA10-9NML      PIC X  OCCURS 25 TIMES.               AANA15
           05 7-NA10-CLNAMS.                                            AANA15
              10 7-NA10-9NMS      PIC X  OCCURS  4 TIMES.               AANA15
       01  7-NA10-COCLNM1.                                              AANA15
           05 7-NA10-COCLNM PIC X OCCURS 74.                            AANA15
      *FOLLOWING CODES CONTAIN FORMATTING RULES.                        AANA15
       01  7-NA10-9PRINT.                                               AANA15
           05 7-NA10-9PRTH     PIC X.                                   AANA15
           05 7-NA10-9PRTF     PIC X.                                   AANA15
           05 7-NA10-9PRTM     PIC X.                                   AANA15
           05 7-NA10-9PRTL     PIC X.                                   AANA15
           05 7-NA10-9PRTS     PIC X.                                   AANA15
       01  7-NA10-9WKNM.                                                AANA15
           05 7-NA10-9WKNM1 PIC X  OCCURS 30 TIMES.                     AANA15
       01  7-NA10-RETURN       PIC X.
      *STRING POINTERS USED IN THE PROGRAM

       01 HTML-PT PIC S9(8) COMP VALUE ZEROS.
       01 TEMP-PT PIC S9(8) COMP VALUE ZEROS.
      *MISCELLANEOUS FIELDS
       01  WS00-WORKAREA.
          05 WS00-CTID.
      *!WS
          10 WS00-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0277
          10 FILLER      PIC X    VALUE SPACE.
          10 WS00-GECKD  PIC X    VALUE SPACES.
          10 FILLER      PIC X    VALUE SPACE.
          10 WS00-CTIDA  PIC X(3) VALUE SPACES.

          05 WS00-CLID.
      *!WS
          10 WS00-CLIDN2          VALUE SPACES
                        PICTURE 9999B9999.                              CI0277
          10 FILLER      PIC X    VALUE SPACE.
          10 WS00-GECKD2 PIC X    VALUE SPACES.
          10 FILLER      PIC X    VALUE SPACE.
          10 WS00-CLIDO  PIC X(3) VALUE SPACES.

          05 WS00-APMTL     PIC $,$$$,$$$,$$$.99.
      *!WS
          05 WS00-DCACG
                        PICTURE 99B99B9999.                             CI0277
          05 WS00-CTWHA     PIC $$,$$$,$$$.99.
          05 WS00-APRNT     PIC $,$$$,$$$,$$$.99.
          05 WS00-PWHLD     PIC ZZ9.
       01 WS00-WITHHOLD     PIC X    VALUE 'N'.
       01 WS00-CNTB         PIC X    VALUE 'N'.
       01 WS00-DATE.
      *!WE
          05 WS00-DTGMM
                        PICTURE 9(2).                                   CI0277
          05 FILLER         PIC X    VALUE '/'.
      *!WE
          05 WS00-DTGDD
                        PICTURE 9(2).                                   CI0277
          05 FILLER         PIC X    VALUE '/'.
      *!WE
          05 WS00-DTGCY
                        PICTURE 9(4).                                   CI0277
          05 FILLER         PIC X    VALUE ' '.
       01 WS00-TIME.
      *!WE
          05 WS00-DTTHH
                        PICTURE 9(2).                                   CI0277
          05 FILLER         PIC X    VALUE ':'.
      *!WE
          05 WS00-DTTMN
                        PICTURE 9(2).                                   CI0277
          05 FILLER         PIC X    VALUE ':'.
      *!WE
          05 WS00-DTTSS
                        PICTURE 9(2).                                   CI0277
          05 FILLER         PIC X    VALUE ' '.
          05 WS00-AMPM      PIC XX.
          05 FILLER     PIC X(13) VALUE ' Central Time'.
      *!WI
       01  WS00-CLORN1
                        PICTURE X(45).                                  CI0277
      *!WI
       01  WS00-TTBAL
                        PICTURE X(15).                                  CI0277
      *!WI
       01  WS00-NPBN
                        PICTURE X(20).                                  CI0277
      *!WI
       01  WS00-MPRCM
                        PICTURE X(50).                                  CI0277
      *!WI
       01  WS00-MPRCM1
                        PICTURE X(50).                                  CI0277

       01   WS01-PT                PIC 99.
       01   WS-CNT      PIC 9(2) VALUE ZERO.
       01   WS00-YEAR   PIC 9(4) VALUE ZEROES.
       01  WS01-WORKAREA.
          05 WS01-CTID.
      *!WS
          10 WS01-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0277
          10 FILLER      PIC X    VALUE SPACE.
          10 WS01-GECKD  PIC X    VALUE SPACES.
          10 FILLER      PIC X    VALUE SPACE.
          10 WS01-CTIDA  PIC X(3) VALUE SPACES.

       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WT115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0277
      *!WI pl=WT120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0277
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WT130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0277
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WT140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0277
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WT150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0277
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WT160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0277
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WT170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0277
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WT180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0277
      *INDICES ARE SET UP FOR EACH PART OF THE NAME.  USED AS FOLLOWS:  AANA15
      *IXXXXR = USED AS VARIABLE INDEX                                  AANA15
      *IXXXXL = ACTUAL # OF CHARACTERS IN EACH FIELD                    AANA15
      *IXXXXM = MAXIMUM # OF CHARACTERS IN EACH FIELD                   AANA15
      *                 7-NAMH                                          AANA15
      *                 7-NAMF                                          AANA15
      *                 7-NAMM                                          AANA15
      *                 7-NAML                                          AANA15
      *                 7-NAMS                                          AANA15
      *                 7-TOTL                                          AANA15
      *                 7-NMWK                                          AANA15
       01   DEBUT-WSS.                                                  CI0277
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0277
            05   IK     PICTURE X.                                      CI0277
       01  CONSTANTES-PAC.                                              CI0277
           05  FILLER  PICTURE X(87)   VALUE                            CI0277
                     '6015 CAT09/08/14CI0277ADMIN   14:35:12CI0277P AMERCI0277
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0277
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0277
           05  NUGNA   PICTURE X(5).                                    CI0277
           05  APPLI   PICTURE X(3).                                    CI0277
           05  DATGN   PICTURE X(8).                                    CI0277
           05  PROGR   PICTURE X(6).                                    CI0277
           05  CODUTI  PICTURE X(8).                                    CI0277
           05  TIMGN   PICTURE X(8).                                    CI0277
           05  PROGE   PICTURE X(8).                                    CI0277
           05  COBASE  PICTURE X(4).                                    CI0277
           05  DATGNC  PICTURE X(10).                                   CI0277
           05  RELEAS  PICTURE X(7).                                    CI0277
           05  DATGE   PICTURE X(10).                                   CI0277
           05  DATSQ   PICTURE X(10).                                   CI0277
       01  DATCE.                                                       CI0277
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0277
         05  DATOR.                                                     CI0277
           10  DATOA  PICTURE XX.                                       CI0277
           10  DATOM  PICTURE XX.                                       CI0277
           10  DATOJ  PICTURE XX.                                       CI0277
       01   VARIABLES-CONDITIONNELLES.                                  CI0277
            05                  FT      PICTURE X VALUE '0'.            CI0277
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0277
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0277
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           INAMFL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMFR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMFM PICTURE S9(4) VALUE +0020.              AANA15
            05           INAMHL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMHR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMHM PICTURE S9(4) VALUE +0006.              AANA15
            05           INAMLL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMLR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMLM PICTURE S9(4) VALUE +0025.              AANA15
            05           INAMML PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMMR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMMM PICTURE S9(4) VALUE +0015.              AANA15
            05           INAMSL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMSR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INAMSM PICTURE S9(4) VALUE +0004.              AANA15
            05           INMWKL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INMWKR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           INMWKM PICTURE S9(4) VALUE +0025.              AANA15
            05           ITOTLL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           ITOTLR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           ITOTLM PICTURE S9(4) VALUE +0074.              AANA15
            05           J35OER PICTURE S9(4) VALUE  ZERO.
            05           J65CCR PICTURE S9(4) VALUE  ZERO.
            05           J65CVR PICTURE S9(4) VALUE  ZERO.
            05           J65EFR PICTURE S9(4) VALUE  ZERO.
            05           J65FDR PICTURE S9(4) VALUE  ZERO.
            05           J65FJR PICTURE S9(4) VALUE  ZERO.
            05           J65FMR PICTURE S9(4) VALUE  ZERO.
            05           J65FQR PICTURE S9(4) VALUE  ZERO.
            05           J65FSR PICTURE S9(4) VALUE  ZERO.
            05           J65FVR PICTURE S9(4) VALUE  ZERO.
            05           J65GDR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0277
      *COPYBOOK WITH HTML TEXT

       COPY CI0277C1.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE INPUT LINKAGE FOR CI0277          *
      ******************************************************************
      *
      *!WF DSP=LM DSL=V1 SEL=78 FOR=I LEV=1 PLT=75
       01                 LM00.                                         CI0277
          05              LM00-SUITE.                                   CI0277
            15       FILLER         PICTURE  X(00931).                  CI0277
       01                 LM78  REDEFINES      LM00.                    CI0277
            10            LM78-C299.                                    CI0277
            11            LM78-CTID.                                    CI0277
            12            LM78-CTIDA  PICTURE  9(3).                    CI0277
            12            LM78-CTIDN.                                   CI0277
            13            LM78-CTIDNP PICTURE  X(13).                   CI0277
            13            LM78-CTIDND PICTURE  9(11).                   CI0277
            10            LM78-GECKD  PICTURE  9.                       CI0277
            10            LM78-DEFFT  PICTURE  9(8).                    CI0277
            10            LM78-CTWHAT PICTURE  S9(7)V99                 CI0277
                          COMPUTATIONAL-3.                              CI0277
            10            LM78-PWHLD  PICTURE  S999V9(5)                CI0277
                          COMPUTATIONAL-3.                              CI0277
            10            LM78-NPBN   PICTURE  X(20).                   CI0277
            10            LM78-CCBAT  PICTURE  99.                      CI0277
            10            LM78-NTR    PICTURE  9(8).                    CI0277
            10            LM78-GECKD1 PICTURE  9.                       CI0277
            10            LM78-IMQMG  PICTURE  X.                       CI0277
            10            LM78-NIPAD  PICTURE  X(15).                   CI0277
            10            LM78-CLNAM.                                   CI0277
            11            LM78-CLNAMH PICTURE  X(6).                    CI0277
            11            LM78-CLNAMF PICTURE  X(20).                   CI0277
            11            LM78-CLNAMI PICTURE  X.                       CI0277
            11            LM78-CLNAMR PICTURE  X(14).                   CI0277
            11            LM78-CLNAML PICTURE  X(25).                   CI0277
            11            LM78-CLNAMS PICTURE  X(4).                    CI0277
            10            LM78-C199.                                    CI0277
            11            LM78-CLID.                                    CI0277
            12            LM78-CLIDO  PICTURE  9(3).                    CI0277
            12            LM78-CLIDN.                                   CI0277
            13            LM78-CLIDNP PICTURE  X(12).                   CI0277
            13            LM78-CLIDND PICTURE  9(8).                    CI0277
            10            LM78-GECKD2 PICTURE  9.                       CI0277
            10            LM78-CPROCM PICTURE  X.                       CI0277
            10            LM78-CTTLN1 PICTURE  X(30).                   CI0277
            10            LM78-CTTLN2 PICTURE  X(30).                   CI0277
            10            LM78-CTTLN3 PICTURE  X(30).                   CI0277
            10            LM78-CTTBO1 PICTURE  X(45).                   CI0277
            10            LM78-CTTBO2 PICTURE  X(45).                   CI0277
            10            LM78-MPRCM  PICTURE  X(50).                   CI0277
            10            LM78-DXTMSA PICTURE  X(26).                   CI0277
            10            LM78-CUPIQ  PICTURE  X.                       CI0277
            10            LM78-IQACT  PICTURE  X.                       CI0277
            10            LM78-CLORN  PICTURE  X(45).                   CI0277
            10            LM78-APMTL  PICTURE  S9(9)V99                 CI0277
                          COMPUTATIONAL-3.                              CI0277
            10            LM78-MPMTFL PICTURE  X(24).                   CI0277
            10            LM78-ANETTQ PICTURE  S9(9)V99                 CI0277
                          COMPUTATIONAL-3.                              CI0277
            10            LM78-MPRN4  PICTURE  X(35).                   CI0277
            10            LM78-CCONF  PICTURE  X(25).                   CI0277
            10            LM78-CLTYP  PICTURE  X.                       CI0277
            10            LM78-CLORN1 PICTURE  X(45).                   CI0277
            10            LM78-MLNAMR PICTURE  X(51).                   CI0277
            10            LM78-PRCOD  PICTURE  9(5).                    CI0277
            10            LM78-CTID01.                                  CI0277
            11            LM78-CTIDA1 PICTURE  9(3).                    CI0277
            11            LM78-CTIDNB.                                  CI0277
            12            LM78-CTIDP1 PICTURE  X(13).                   CI0277
            12            LM78-NTIDNG PICTURE  9(11).                   CI0277
            10            LM78-GECKD3 PICTURE  9.                       CI0277
            10            LM78-MPRCM1 PICTURE  X(50).                   CI0277
            10            LM78-IQACT4 PICTURE  X.                       CI0277
            10            LM78-CATLN1 PICTURE  X(30).                   CI0277
            10            LM78-CATLN2 PICTURE  X(30).                   CI0277
            10            LM78-CATLN3 PICTURE  X(30).                   CI0277
            10            LM78-CTTYP2 PICTURE  X.                       CI0277
            10            LM78-CIRAP  PICTURE  XX.                      CI0277
            10            LM78-FILLER PICTURE  X(70).                   CI0277
      *
      ******************************************************************
      **         THIS SEGMENT IS THE OUTPUT LINKAGE FOR CI0277         *
      ******************************************************************
      *
      *!WF DSP=HM DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HM00.                                         CI0277
          05              HM00-SUITE.                                   CI0277
            15       FILLER         PICTURE  X(90906).                  CI0277
       01                 HM93  REDEFINES      HM00.                    CI0277
            10            HM93-QBLCK  PICTURE  9(6).                    CI0277
            10            HM93-QT9O.                                    CI0277
            11            HM93-QT9B                                     CI0277
                          OCCURS       450     TIMES.                   CI0277
            12            HM93-CHTML  PICTURE  99.                      CI0277
            12            HM93-THTML  PICTURE  X(200).                  CI0277
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0277
          05              MS00-SUITE.                                   CI0277
            15       FILLER         PICTURE  X(00542).                  CI0277
       01                 MS03  REDEFINES      MS00.                    CI0277
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0277
                          COMPUTATIONAL-3.                              CI0277
            10            MS03-CMSSF  PICTURE  XX.                      CI0277
            10            MS03-DU09.                                    CI0277
            11            MS03-CMESA  PICTURE  S9(9)                    CI0277
                          BINARY.                                       CI0277
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0277
                          BINARY.                                       CI0277
            11            MS03-CMESB  PICTURE  S9(9)                    CI0277
                          BINARY.                                       CI0277
            11            MS03-CMSST  PICTURE  S9(9)                    CI0277
                          BINARY.                                       CI0277
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0277
                          BINARY.                                       CI0277
            11            MS03-QELLAA PICTURE  S9(9)                    CI0277
                          BINARY.                                       CI0277
            11            MS03-TMESS4 PICTURE  X(512).                  CI0277
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0277
            10            MX11-QMSGS  PICTURE  9(03).                   CI0277
            10            MX11-PJ09                                     CI0277
                          OCCURS       025     TIMES.                   CI0277
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0277
                          COMPUTATIONAL-3.                              CI0277
            11            MX11-CMESB  PICTURE  S9(9)                    CI0277
                          BINARY.                                       CI0277
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                LM78
                                HM93
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0277
      *               *                                   *             CI0277
      *               *INITIALISATIONS                    *             CI0277
      *               *                                   *             CI0277
      *               *************************************.            CI0277
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
      *N02CA.    NOTE *---> ALL HTML LINES                *.
       F02CA.                                                           lv10
           INITIALIZE  HTML-HEADER-TAGS
           HTML-CONFIRM-TAGS
           HTML-TRANS-CONFIRM-TAGES
           HTML-TRANS-COMMON-TAGES
           HTML-QUAL-PAYMENT-TAGS
           HTML-MESSAGE-TAGS1
           HTML-EOM-TAGS
           HTML-MESSAGE-TBLCK
           HTML-TRANS-CNTB-TAGS
           HTML-FOOTER-TAGS.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0277
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0277
      *               *                                   *             CI0277
      *               *FIN DE TRAITEMENT                  *             CI0277
      *               *                                   *             CI0277
      *               *************************************.            CI0277
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0277
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *BUILD COMMON HTML TAGS             *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35AE.    NOTE *FORMAT THE SUBMITTED DATE&TIME     *.
       F35AE.                                                           lv10
      *********************************
           MOVE        LM78-DXTMSA TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGCY TO WS00-DTGCY
           MOVE        7-DB2-DTGMM TO WS00-DTGMM
           MOVE        7-DB2-DTGDD TO WS00-DTGDD
           MOVE        7-DB2-DTTHH TO WS00-DTTHH
           MOVE        7-DB2-DTTMN TO WS00-DTTMN
           MOVE        7-DB2-DTTSS TO WS00-DTTSS.
                 IF    WS00-DTTHH < 12                                  DOT
           MOVE        'AM' TO WS00-AMPM
                 ELSE
           MOVE        'PM' TO WS00-AMPM.
                 IF    WS00-DTTHH > 12                                  DOT
           COMPUTE     WS00-DTTHH = WS00-DTTHH - 12.
                 IF    WS00-DTTHH = 00                                  DOT
           COMPUTE     WS00-DTTHH = WS00-DTTHH + 12.
           MOVE        WS00-DATE TO HTML-DS                             DOT
           MOVE        WS00-TIME TO HTML-TS.
       F35AE-FN. EXIT.
      *N35DB.    NOTE *MOVE CONFIRM INFO                  *.
       F35DB.    IF    LM78-CUPIQ = 'U'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
      *N35DC.    NOTE *FORMAT THE CLIENT NAME             *.
       F35DC.                                                           lv15
           PERFORM     F90BA THRU F90BA-FN.
       F35DC-FN. EXIT.
      *N35DD.    NOTE *MOVE CONFIRMATION TITLE / NUMBER   *.
       F35DD.                                                           lv15
      *********************************
           MOVE        CN-CONFIRM-TITLE TO HTML-TITLE
           MOVE        LM78-CCONF TO HTML-CCONF.
       F35DD-FN. EXIT.
      *N35DE.    NOTE *INITIALIZE NUMBER OF MESSAGES      *.
       F35DE.                                                           lv15
           MOVE        1 TO WS-CNT.
       F35DE-FN. EXIT.
      *N35DF.    NOTE *MOVE THE OFF-HOURS NOTE            *.
       F35DF.    IF    LM78-IMQMG = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F35DF-FN.
      *********************************
           MOVE        CN-NOTE-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DF-FN. EXIT.
      *N35DG.    NOTE *FORMAT ADD MESSAGE                 *.
       F35DG.    IF    LM78-CPROCM = 'A'                                lv15
                 NEXT SENTENCE ELSE GO TO     F35DG-FN.
      *********************************
                 IF    LM78-CTTYP2 = 'A'                                DOT
           MOVE        CN-ADD-TMESSA TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
                 IF    LM78-CTTYP2 = 'T'                                DOT
           MOVE        CN-ADD-TMESSA1 TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
      *N35DI.    NOTE *FORMAT MESSAGE FOR:                *.
       F35DI.    IF    LM78-CTTYP2 = 'T'                                lv20
                 AND   ((LM78-CTIDA = 133
                 AND   LM78-CTIDA1 = 001)
                 OR    (LM78-CTIDA = 001
                 AND   LM78-CTIDA1 = 133))
                 AND   LM78-IQACT = 'N'
                 AND   LM78-IQACT4 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F35DI-FN.
      *1. TRANSFER FROM BROKERAGE NQ
      *   TO CASH RESERVE Q 1XTIME
      *2. TRANSFER FROM CASH RESERVE NQ
      *   BROKERAGE RESERVE Q 1X
           MOVE        CN-INTR-TRAN-IRA-TEXT1 TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DI-FN. EXIT.
      *N35DK.    NOTE *FORMAT MESSAGE FOR:                *.
       F35DK.    IF    (LM78-CTTYP2 = 'A'                               lv20
                 AND   LM78-CTIDA = 001
                 AND   LM78-IQACT = 'Y')
                 OR    (LM78-CTTYP2 = 'T'
                 AND   ((LM78-CTIDA = 133
                 AND   LM78-CTIDA1 = 001)
                 OR    (LM78-CTIDA = 001
                 AND   LM78-CTIDA1 = 133))
                 AND   LM78-IQACT = 'Y'
                 AND   LM78-IQACT4 = 'N')
                 NEXT SENTENCE ELSE GO TO     F35DK-FN.
      *1. ACH-OUT CASH RESERVE (1X)
      *   FROM Q ACCOUNT - SOURCE
      *2. TRANSFER FROM BROKERAGE Q
      *   TO CASH RESERVE NQ (1X)
      *3. TRANSFER FROM CASH RESERVE Q
      *   TO BROKERAGE NQ. (1X)
           MOVE        CN-ON-DEMAND-IRA-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DK-FN. EXIT.
      *N35DL.    NOTE *FORMAT MESSAGE FOR:                *.
       F35DL.    IF    LM78-CTTYP2 = 'T'                                lv20
                 AND   ((LM78-CTIDA = 133
                 AND   LM78-CTIDA1 = 001)
                 OR    (LM78-CTIDA = 001
                 AND   LM78-CTIDA1 = 133))
                 AND   LM78-IQACT = 'Y'
                 AND   LM78-IQACT4 = 'Y'
                 NEXT SENTENCE ELSE GO TO     F35DL-FN.
      *1. TRANSFER FROM BROKERAGE Q
      *   TO CASH RESERVE Q 1XTIME
      *2. TRANSFER FROM CASH RESERVE Q
      *   BROKERAGE RESERVE Q 1X
           MOVE        CN-INTR-TRAN-IRA-TEXT2 TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DL-FN. EXIT.
       F35DG-900. GO TO F35DM-FN.
       F35DG-FN. EXIT.
      *N35DM.    NOTE *FORMAT DELETE MESSAGE              *.
       F35DM.                                                           lv15
      *********************************
                 IF    LM78-CTTYP2 = 'A'                                DOT
           MOVE        CN-DEL-TMESSA TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
                 IF    LM78-CTTYP2 = 'T'                                DOT
           MOVE        CN-DEL-TMESSA1 TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DM-FN. EXIT.
      *N35DN.    NOTE *FORMAT CUT-OFF MESSAGES FOR        *.
       F35DN.                                                           lv15
      *CERT/BETA BROK SOURCE ACCOUNT
           MOVE        CN-ON-DEMAND-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DN-FN. EXIT.
      *N35DU.    NOTE *FORMAT THE CLIENT ID               *.
       F35DU.                                                           lv15
      *********************************
      ********************************
      **  FORMAT THE CLIENT ID IN    *
      **  XXXX XXXX X XXX FORMAT     *
      ********************************
           MOVE        LM78-CLIDND TO WS00-CLIDN2
           MOVE        LM78-CLIDO TO WS00-CLIDO
           MOVE        LM78-GECKD2 TO WS00-GECKD2
           MOVE        WS00-CLID TO HTML-CLID.
       F35DU-FN. EXIT.
       F35DB-900. GO TO F35FB-FN.
       F35DB-FN. EXIT.
      *N35FB.    NOTE *MOVE VERIFY INFO                   *.
       F35FB.         EXIT.                                             lv10
      *N35FE.    NOTE *MOVE VERIFY TITLE / MESSAGE        *.
       F35FE.                                                           lv15
      *********************************
           MOVE        CN-VERIFY-TITLE TO HTML-TITLE.
       F35FE-FN. EXIT.
      *N35FG.    NOTE *INITIALIZE NUMBER OF MESSAGES      *.
       F35FG.                                                           lv15
           MOVE        1 TO WS-CNT.
                 IF    LM78-IMQMG = 'Y'                                 DOT
      *MOVE THE CORE-HOURS NOTE
      *********************************
           MOVE        CN-NOTE-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35FG-FN. EXIT.
      *N35FH.    NOTE *VERIFY FOOTER MSG FOR QUAL ACCTS   *.
       F35FH.    IF    LM78-IQACT = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F35FH-FN.
      *QUALIFIED ACCOUNT
      *********************************
           MOVE        CN-VERIFY-FOOTER-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35FH-FN. EXIT.
       F35FB-FN. EXIT.
      *N35MB.    NOTE *BUILD BANK INFORMATION             *.
       F35MB.    IF    LM78-CTTYP2 = 'A'                                lv10
                 NEXT SENTENCE ELSE GO TO     F35MB-FN.
      *********************************
           MOVE        LM78-CLORN TO WS00-CLORN1.
                 IF    LM78-CCBAT = 01                                  DOT
           MOVE        CN-TTBAL-01 TO WS00-TTBAL.
                 IF    LM78-CCBAT = 02                                  DOT
           MOVE        CN-TTBAL-02 TO WS00-TTBAL.
      *N35OC.    NOTE *FORMAT BANK NUMBER                 *.
       F35OC.                                                           lv15
           MOVE        LM78-NPBN TO WS00-NPBN.
       F35OC-FN. EXIT.
      *N35OE.    NOTE *FIND TRAILING NUMERIC              *.
       F35OE.                                                           lv15
           MOVE        20                       TO J35OER
                                    GO TO     F35OE-B.
       F35OE-A.
           SUBTRACT 1                         FROM J35OER.
       F35OE-B.
           IF          J35OER                   <  4
                                    GO TO     F35OE-FN.
                 IF    WS00-NPBN (J35OER:1) NOT =                       DOT
                       ' '
      *********************************
           COMPUTE     WS01-PT = J35OER - 4
               GO TO     F35OE-FN.
       F35OE-900. GO TO F35OE-A.
       F35OE-FN. EXIT.
      *N35OG.    NOTE *MASK LEADING CHARACTERS            *.
       F35OG.                                                           lv15
           MOVE ALL    '*' TO WS00-NPBN (1:WS01-PT).
       F35OG-FN. EXIT.
      *N35PD.    NOTE *FORMAT BANK DETAIL                 *.
       F35PD.                                                           lv15
      *********************************
           STRING      WS00-CLORN1 ' ' WS00-TTBAL
           '<BR>' WS00-NPBN
           DELIMITED BY SIZE
           INTO HTML-TO.
       F35PD-FN. EXIT.
       F35MB-FN. EXIT.
      *N35QB.    NOTE *BUILD TO ACCOUNT INFORMATION       *.
       F35QB.    IF    LM78-CTTYP2 = 'T'                                lv10
                 NEXT SENTENCE ELSE GO TO     F35QB-FN.
      *********************************
      *N35QD.    NOTE *STRING PRODUCT NAME                *.
       F35QD.                                                           lv15
           MOVE        LM78-MPRCM1 TO WS00-MPRCM1.
       F35QD-FN. EXIT.
      *N35QE.    NOTE *FORMAT TO CONTRACT ID              *.
       F35QE.                                                           lv15
      *********************************
           MOVE        LM78-NTIDNG TO WS01-NCTIDN
           MOVE        LM78-GECKD3 TO WS01-GECKD
           MOVE        LM78-CTIDA1 TO WS01-CTIDA.
       F35QE-FN. EXIT.
      *N35QG.    NOTE *FORMAT TO ACCOUNT DETAIL           *.
       F35QG.                                                           lv15
      *********************************
           MOVE        1 TO TEMP-PT.
       F35QG-FN. EXIT.
      *N35RB.    NOTE *STRING PRODUCT CODE                *.
       F35RB.                                                           lv15
           STRING      WS00-MPRCM1
           DELIMITED BY SIZE
           '<BR>' LM78-CATLN1
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
       F35RB-FN. EXIT.
      *N35RD.    NOTE *STRING OWNERSHIP LINE # 2          *.
       F35RD.    IF    LM78-CATLN2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F35RD-FN.
           STRING      '<BR>' LM78-CATLN2
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
       F35RD-FN. EXIT.
      *N35RF.    NOTE *STRING OWNERSHIP LINE # 3          *.
       F35RF.    IF    LM78-CATLN3 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F35RF-FN.
           STRING      '<BR>' LM78-CATLN3
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
       F35RF-FN. EXIT.
      *N35RK.    NOTE *STRING CONTRACT ID                 *.
       F35RK.                                                           lv15
           STRING      '<BR>' WS01-CTID
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
       F35RK-FN. EXIT.
       F35QB-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *BUILD ACCOUNT TAGS                 *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40AF.    NOTE *STRING PRODUCT NAME                *.
       F40AF.                                                           lv10
      *********************************
           MOVE        LM78-MPRCM TO WS00-MPRCM.
       F40AF-FN. EXIT.
      *N40CB.    NOTE *FORMAT CONTRACT ID                 *.
       F40CB.                                                           lv10
      *********************************
           MOVE        LM78-CTIDND TO WS00-NCTIDN
           MOVE        LM78-GECKD TO WS00-GECKD
           MOVE        LM78-CTIDA TO WS00-CTIDA.
       F40CB-FN. EXIT.
      *N40DD.    NOTE *FORMAT ACCOUNT DETAIL              *.
       F40DD.                                                           lv10
      *********************************
           MOVE        1 TO TEMP-PT.
      *N40FB.    NOTE *STRING PRODUCT NAME                *.
       F40FB.                                                           lv15
           STRING      WS00-MPRCM
           DELIMITED BY SIZE
           '<BR>' LM78-CTTLN1
           DELIMITED BY SIZE
           INTO HTML-FROM
           WITH POINTER TEMP-PT.
       F40FB-FN. EXIT.
      *N40FD.    NOTE *STRING OWNERSHIP LINE # 2          *.
       F40FD.    IF    LM78-CTTLN2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F40FD-FN.
           STRING      '<BR>' LM78-CTTLN2
           DELIMITED BY SIZE
           INTO HTML-FROM
           WITH POINTER TEMP-PT.
       F40FD-FN. EXIT.
      *N40FF.    NOTE *STRING OWNERSHIP LINE # 3          *.
       F40FF.    IF    LM78-CTTLN3 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F40FF-FN.
           STRING      '<BR>' LM78-CTTLN3
           DELIMITED BY SIZE
           INTO HTML-FROM
           WITH POINTER TEMP-PT.
       F40FF-FN. EXIT.
      *N40FK.    NOTE *STRING CONTRACT ID                 *.
       F40FK.                                                           lv15
           STRING      '<BR>' WS00-CTID
           DELIMITED BY SIZE
           INTO HTML-FROM
           WITH POINTER TEMP-PT.
       F40FK-FN. EXIT.
       F40DD-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *BUILD PAYMENT DETAIL               *
      *               *                                   *
      *               *************************************.
       F45.           EXIT.                                             lv05
      *N45GB.    NOTE *MOVE AMOUNT                        *.
       F45GB.                                                           lv10
      *********************************
           MOVE        LM78-APMTL TO WS00-APMTL
           MOVE        WS00-APMTL TO HTML-APMTL.
       F45GB-FN. EXIT.
      *N45JB.    NOTE *MOVE FREQUENCY                     *.
       F45JB.                                                           lv10
      *********************************
           MOVE        LM78-MPMTFL TO HTML-MPMTFL.
       F45JB-FN. EXIT.
      *N45JF.    NOTE *MOVE TRANSFER EFFECTIVE DATE       *.
       F45JF.                                                           lv10
      *********************************
           INITIALIZE  WS00-DATE
           MOVE        LM78-DEFFT (1:4) TO WS00-DTGCY
           MOVE        LM78-DEFFT (5:2) TO WS00-DTGMM
           MOVE        LM78-DEFFT (7:2) TO WS00-DTGDD
           MOVE        WS00-DATE TO HTML-DEFFT.
       F45JF-FN. EXIT.
      *N45JI.    NOTE *CALCULATE THE CONTRIBUTION YEAR    *.
       F45JI.    IF    LM78-CTTYP2 = 'T'                                lv10
                 AND   ((LM78-CTIDA = 001
                 AND   LM78-IQACT = 'N'
                 AND   LM78-CTIDA1 = 133
                 AND   LM78-IQACT4 = 'Y')
                 OR    (LM78-CTIDA = 133
                 AND   LM78-IQACT = 'N'
                 AND   LM78-CTIDA1 = 001
                 AND   LM78-IQACT4 = 'Y'))
                 AND   (LM78-CIRAP = 'CU' OR 'PR')
                 NEXT SENTENCE ELSE GO TO     F45JI-FN.
      *WHEN NQ CERTS TO Q BROKERAGE
      *OR NQ BETA BROKERAGE TO Q CERTS
      *TRANSFER
      *THE INPUT CONTRIBUTION CODE
      *SHOULD BE 'CU' OR 'PR'
      *WHEN CONTRIBUTION CODE IS 'CU'
      *THEN THE CONTRIBUTION YEAR EQUAL
      *TO CURRENT YEAR, WHEN IT IS 'PR'
      *THEN THE CONTRIBUTION YEAR EQUAL
      *TO PREVIOUS YEAR.
           INITIALIZE  WS00-DATE
           MOVE        'Y' TO WS00-CNTB
           MOVE        CN-CNTB-YEAR TO HTML-CNTB-YEAR.
                 IF    LM78-CIRAP = 'CU'                                DOT
           MOVE        7-DB2-DTGCY TO HTML-CTYEAR.
                 IF    LM78-CIRAP = 'PR'                                DOT
           MOVE        7-DB2-DTGCY TO WS00-DTGCY
           SUBTRACT    1 FROM WS00-DTGCY
           GIVING WS00-YEAR
           MOVE        WS00-YEAR TO HTML-CTYEAR.
       F45JI-FN. EXIT.
      *N45KB.    NOTE *SET FLAG OF WITHHOLDING FOR        *.
       F45KB.    IF    (LM78-CTTYP2 = 'A'                               lv10
                 AND   LM78-CTIDA = 001
                 AND   LM78-IQACT = 'Y')
                 OR    (LM78-CTTYP2 = 'T'
                 AND   ((LM78-CTIDA = 001
                 AND   LM78-IQACT = 'Y'
                 AND   LM78-CTIDA1 = 133
                 AND   LM78-IQACT4 = 'N')
                 OR    (LM78-CTIDA = 133
                 AND   LM78-IQACT = 'Y'
                 AND   LM78-CTIDA1 = 001
                 AND   LM78-IQACT4 = 'N')))
                 NEXT SENTENCE ELSE GO TO     F45KB-FN.
      *QUALIFIED CERT ACH-OUT TRAN,
      *QUALIFIED CERT TO NON-QUALIFIED
      *BETA BROKERAGE
      *QUALIFIED BETA BROKERAGE TO
      *NON-QUALIFIED CERT
           MOVE        'Y' TO WS00-WITHHOLD.
       F45KB-FN. EXIT.
      *N45LG.    NOTE *MOVE ADDITIONAL DETAILS            *.
       F45LG.    IF    WS00-WITHHOLD = 'Y'                              lv10
                 NEXT SENTENCE ELSE GO TO     F45LG-FN.
      *N45LK.    NOTE *MOVE WITHHOLDING TAGS              *.
       F45LK.                                                           lv15
           MOVE        CN-WITH-PER TO HTML-WITH-PER
           MOVE        CN-WITH-AMT TO HTML-WITH-AMT
           MOVE        CN-NET-AMT TO HTML-NET-AMT.
       F45LK-FN. EXIT.
      *N45LL.    NOTE *MOVE WITHOLDING PERCENT            *.
       F45LL.                                                           lv15
      *********************************
           MOVE        LM78-PWHLD TO WS00-PWHLD
           STRING      WS00-PWHLD '%'
           DELIMITED BY SIZE
           INTO HTML-CTWHPB.
       F45LL-FN. EXIT.
      *N45MB.    NOTE *MOVE WITHHOLDING AMOUNT            *.
       F45MB.                                                           lv15
      *********************************
           MOVE        LM78-CTWHAT TO WS00-CTWHA
           MOVE        WS00-CTWHA TO HTML-CTWHA.
       F45MB-FN. EXIT.
      *N45MD.    NOTE *MOVE NET AMOUNT                    *.
       F45MD.                                                           lv15
      *********************************
           MOVE        LM78-ANETTQ TO WS00-APRNT
           MOVE        WS00-APRNT TO HTML-APRNT.
       F45MD-FN. EXIT.
       F45LG-900. GO TO F45MK-FN.
       F45LG-FN. EXIT.
      *N45MK.    NOTE *IF NON-QUALIFIED ACCOUNT           *.
       F45MK.                                                           lv10
      *********************************
           MOVE        ZEROES TO HTML-CTWHPB
           HTML-CTWHA
           HTML-APRNT.
       F45MK-FN. EXIT.
       F45-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *MOVE HTML TAGS INTO LM78-OUTPUT    *
      *               *                                   *
      *               *************************************.
       F65.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0277C1' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N65BA.    NOTE *INITILIZE LINE COUNTER             *.
       F65BA.                                                           lv10
           MOVE        0 TO HTML-PT.
       F65BA-FN. EXIT.
      *N65CC.    NOTE *STRING BODY INTO HTML OUTPUT       *.
       F65CC.                                                           lv10
           MOVE        1                        TO J65CCR
                                    GO TO     F65CC-B.
       F65CC-A.
           ADD         1                        TO J65CCR.
       F65CC-B.
           IF          J65CCR                   >  HTML-HEADER-CTR
                                    GO TO     F65CC-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-HEADER-LINE (J65CCR) TO
           HM93-THTML (HTML-PT)
           MOVE        03 TO HM93-CHTML (HTML-PT).
       F65CC-900. GO TO F65CC-A.
       F65CC-FN. EXIT.
      *N65CU.    NOTE *STRING HEADER                      *.
       F65CU.         EXIT.                                             lv10
      *N65CV.    NOTE *STRING HEADER INTO HTML-OUTPUT     *.
       F65CV.                                                           lv15
           MOVE        1                        TO J65CVR
                                    GO TO     F65CV-B.
       F65CV-A.
           ADD         1                        TO J65CVR.
       F65CV-B.
           IF          J65CVR                   >  HTML-CONFIRM-CTR
                                    GO TO     F65CV-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-CONFIRM-LINE (J65CVR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65CV-900. GO TO F65CV-A.
       F65CV-FN. EXIT.
       F65CU-FN. EXIT.
      *N65EC.    NOTE *STRING CONFIRMATION SPECIFIC       *.
       F65EC.    IF    LM78-CUPIQ = 'U'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F65EC-FN.
      *N65EF.    NOTE *STRING CONFIRM INTO HTML-OUTPUT    *.
       F65EF.                                                           lv15
           MOVE        1                        TO J65EFR
                                    GO TO     F65EF-B.
       F65EF-A.
           ADD         1                        TO J65EFR.
       F65EF-B.
           IF          J65EFR                   >  HTML-TRANS-CONFIRM-CT
                                    GO TO     F65EF-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TRANS-CONFIRM-LINE (J65EFR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65EF-900. GO TO F65EF-A.
       F65EF-FN. EXIT.
       F65EC-FN. EXIT.
      *N65FC.    NOTE *STRING TRANSACTION COMMON          *.
       F65FC.         EXIT.                                             lv10
      *N65FD.    NOTE *STRING COMMON INTO HTML-OUTPUT     *.
       F65FD.                                                           lv15
           MOVE        1                        TO J65FDR
                                    GO TO     F65FD-B.
       F65FD-A.
           ADD         1                        TO J65FDR.
       F65FD-B.
           IF          J65FDR                   >  HTML-TRANS-COMMON-CTR
                                    GO TO     F65FD-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TRANS-COMMON-LINE (J65FDR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65FD-900. GO TO F65FD-A.
       F65FD-FN. EXIT.
       F65FC-FN. EXIT.
      *N65FI.    NOTE *CONTRIBUTION YEAR                  *.
       F65FI.    IF    WS00-CNTB = 'Y'                                  lv10
                 NEXT SENTENCE ELSE GO TO     F65FI-FN.
      *********************************
      *N65FJ.    NOTE *CONTRIBUTION YEAR                  *.
       F65FJ.                                                           lv15
           MOVE        1                        TO J65FJR
                                    GO TO     F65FJ-B.
       F65FJ-A.
           ADD         1                        TO J65FJR.
       F65FJ-B.
           IF          J65FJR                   >  HTML-TRANS-CNTB-CTR
                                    GO TO     F65FJ-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TRANS-CNTB-LINE (J65FJR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65FJ-900. GO TO F65FJ-A.
       F65FJ-FN. EXIT.
       F65FI-FN. EXIT.
      *N65FL.    NOTE *QUALIFIED PAYMENT DETAILS          *.
       F65FL.    IF    WS00-WITHHOLD = 'Y'                              lv10
                 NEXT SENTENCE ELSE GO TO     F65FL-FN.
      *********************************
      *N65FM.    NOTE *QUALIFIED PAYMENT DETAILS          *.
       F65FM.                                                           lv15
           MOVE        1                        TO J65FMR
                                    GO TO     F65FM-B.
       F65FM-A.
           ADD         1                        TO J65FMR.
       F65FM-B.
           IF          J65FMR                   >  HTML-QUAL-PAYMENT-CTR
                                    GO TO     F65FM-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-QUAL-PAYMENT-LINE (J65FMR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65FM-900. GO TO F65FM-A.
       F65FM-FN. EXIT.
       F65FL-FN. EXIT.
      *N65FO.    NOTE *STRING MESSAGE                     *.
       F65FO.    IF    WS-CNT > 1                                       lv10
                 NEXT SENTENCE ELSE GO TO     F65FO-FN.
      *N65FQ.    NOTE *STRING MESSAGE INTO HTML-OUTPUT    *.
       F65FQ.                                                           lv15
           MOVE        1                        TO J65FQR
                                    GO TO     F65FQ-B.
       F65FQ-A.
           ADD         1                        TO J65FQR.
       F65FQ-B.
           IF          J65FQR                   >  HTML-MESSAGE-CTR1
                                    GO TO     F65FQ-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-MESSAGE-LINE1 (J65FQR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65FQ-900. GO TO F65FQ-A.
       F65FQ-FN. EXIT.
      *N65FS.    NOTE *STRING MESSAGES INTO HTML-OUTPUT   *.
       F65FS.                                                           lv15
           MOVE        1                        TO J65FSR
                                    GO TO     F65FS-B.
       F65FS-A.
           ADD         1                        TO J65FSR.
       F65FS-B.
           IF          J65FSR                   >  20
                                    GO TO     F65FS-FN.
      *N65FT.    NOTE *ONLY MOVE GOOD MESSAGES            *.
       F65FT.    IF    HTML-TMESS1 (J65FSR)                             lv20
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F65FT-FN.
      *
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESSN1 TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS1 (J65FSR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
                 IF    HTML-TMESS2 (J65FSR)                             DOT
                       NOT = SPACES
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS2 (J65FSR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           END-IF.
                 IF    HTML-TMESS3 (J65FSR)                             DOT
                       NOT = SPACES
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS3 (J65FSR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           END-IF.
           ADD         1 TO HTML-PT                                     DOT
           MOVE        HTML-TMESSN2 TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65FT-FN. EXIT.
       F65FS-900. GO TO F65FS-A.
       F65FS-FN. EXIT.
      *N65FV.    NOTE *STRING EOM INTO HTML-OUTPUT        *.
       F65FV.                                                           lv15
           MOVE        1                        TO J65FVR
                                    GO TO     F65FV-B.
       F65FV-A.
           ADD         1                        TO J65FVR.
       F65FV-B.
           IF          J65FVR                   >  HTML-EOM-CTR
                                    GO TO     F65FV-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-EOM-LINE (J65FVR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65FV-900. GO TO F65FV-A.
       F65FV-FN. EXIT.
       F65FO-FN. EXIT.
      *N65GC.    NOTE *STRING FOOTER                      *.
       F65GC.         EXIT.                                             lv10
      *N65GD.    NOTE *STRING FOOTER INTO HTML-OUTPUT     *.
       F65GD.                                                           lv15
           MOVE        1                        TO J65GDR
                                    GO TO     F65GD-B.
       F65GD-A.
           ADD         1                        TO J65GDR.
       F65GD-B.
           IF          J65GDR                   >  HTML-FOOTER-CTR
                                    GO TO     F65GD-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-FOOTER-LINE (J65GDR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65GD-900. GO TO F65GD-A.
       F65GD-FN. EXIT.
       F65GC-FN. EXIT.
      *N65ZO.    NOTE *SEND END OF HTML AREA              *.
       F65ZO.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        SPACES TO HM93-THTML (HTML-PT)
           MOVE        99 TO HM93-CHTML (HTML-PT).
       F65ZO-FN. EXIT.
      *N65ZQ.    NOTE *MOVE LENGTH OF HTML BLOB           *.
       F65ZQ.                                                           lv10
           COMPUTE     HM93-QBLCK = HTML-PT.
       F65ZQ-FN. EXIT.
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
      *N90.      NOTE *************************************.
      *               *                                   *
      *               *NAME PACKER                        *
      *               *                                   *
      *               *************************************.
       F90.           EXIT.                                             lv05
      *N90BA.    NOTE *FORMAT CLIENT NAME                 *.
       F90BA.         EXIT.                                             lv10
      *N90BC.    NOTE *CLIENT IS AN ORGANIZATION          *.
       F90BC.    IF    LM78-CLTYP = 'O'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F90BC-FN.
           STRING      LM78-MLNAMR DELIMITED BY '   '
           ' FOR ' DELIMITED BY SIZE
           LM78-CLORN1 DELIMITED BY SIZE
           INTO HTML-MCLNM.
       F90BC-FN. EXIT.
      *N90BE.    NOTE *CLIENT IS PERSON                   *.
       F90BE.    IF    LM78-CLTYP = 'P'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F90BE-FN.
           PERFORM     F91 THRU F91-FN.
       F90BE-FN. EXIT.
       F90BA-FN. EXIT.
       F90-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *NAEM PACKER                        *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91QA.    NOTE *INITIALIZATION OF WORK AREAS       *.            AANA15
       F91QA.                                                           lv10
           MOVE        ZERO TO INAMHL INAMFL INAMML                     AANA15
           INAMLL INAMSL ITOTLL                                         AANA15
           MOVE        ZERO TO INAMHR INAMFR INAMMR INAMLR              AANA15
           INAMSR ITOTLR                                                AANA15
           MOVE        ZERO TO 7-NA10-9STH 7-NA10-9STF                  AANA15
           7-NA10-9STM 7-NA10-9STL                                      AANA15
           7-NA10-9STS 7-NA10-9TSPAC                                    AANA15
           MOVE        ZERO TO 7-NA10-9LSH 7-NA10-9LSF                  AANA15
           7-NA10-9LSM 7-NA10-9LSL                                      AANA15
           7-NA10-9LSS                                                  AANA15
           MOVE        SPACES TO 7-NA10-RETURN                          AANA15
           MOVE        LM78-CLNAM TO 7-NA10-CLNAM.                      AANA15
      *N91QD.    NOTE *COUNT CHARACTERS IN HONORIFIC      *.            AANA15
       F91QD.    IF    7-NA10-CLNAMH NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F91QD-FN.                 AANA15
           MOVE        INAMHM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMH TO 7-NA10-9WKNM                    AANA15
           PERFORM     F91TC THRU F91TC-FN                              AANA15
           MOVE        INMWKR TO INAMHL.                                AANA15
                 IF    INAMHL > 0                                       DOT
           PERFORM     F91TH THRU F91TH-FN                              AANA15
           MOVE        INAMHL TO 7-NA10-9LSH                            AANA15
           COMPUTE     INAMHL = INAMHL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STH.                           AANA15
       F91QD-FN. EXIT.
      *N91QJ.    NOTE *COUNT CHARACTERS IN FIRST NAME     *.            AANA15
       F91QJ.    IF    7-NA10-CLNAMF NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F91QJ-FN.                 AANA15
           MOVE        INAMFM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMF TO 7-NA10-9WKNM                    AANA15
           PERFORM     F91TC THRU F91TC-FN                              AANA15
           MOVE        INMWKR TO INAMFL.                                AANA15
                 IF    INAMFL > 0                                       DOT
           PERFORM     F91TH THRU F91TH-FN                              AANA15
           MOVE        INAMFL TO 7-NA10-9LSF                            AANA15
           COMPUTE     INAMFL = INAMFL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STF.                           AANA15
       F91QJ-FN. EXIT.
      *N91QO.    NOTE *COUNT CHARACTERS IN MIDDLE NAME    *.            AANA15
       F91QO.    IF    7-NA10-CLNAMM NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F91QO-FN.                 AANA15
           MOVE        INAMMM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMM TO 7-NA10-9WKNM                    AANA15
           PERFORM     F91TC THRU F91TC-FN                              AANA15
           MOVE        INMWKR TO INAMML.                                AANA15
                 IF    INAMML > 0                                       DOT
           PERFORM     F91TH THRU F91TH-FN                              AANA15
           MOVE        INAMML TO 7-NA10-9LSM                            AANA15
           COMPUTE     INAMML = INAMML - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STM.                           AANA15
       F91QO-FN. EXIT.
      *N91QT.    NOTE *COUNT CHARACTERS IN LAST NAME      *.            AANA15
       F91QT.    IF    7-NA10-CLNAML NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F91QT-FN.                 AANA15
           MOVE        INAMLM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAML TO 7-NA10-9WKNM                    AANA15
           PERFORM     F91TC THRU F91TC-FN                              AANA15
           MOVE        INMWKR TO INAMLL.                                AANA15
                 IF    INAMLL > 0                                       DOT
           PERFORM     F91TH THRU F91TH-FN                              AANA15
           MOVE        INAMLL TO 7-NA10-9LSL                            AANA15
           COMPUTE     INAMLL = INAMLL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STL.                           AANA15
       F91QT-FN. EXIT.
      *N91RA.    NOTE *COUNT CHARACTERS IN SUFFIX         *.            AANA15
       F91RA.    IF    7-NA10-CLNAMS NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F91RA-FN.                 AANA15
           MOVE        INAMSM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMS TO 7-NA10-9WKNM                    AANA15
           PERFORM     F91TC THRU F91TC-FN                              AANA15
           MOVE        INMWKR TO INAMSL.                                AANA15
                 IF    INAMSL > 0                                       DOT
           PERFORM     F91TH THRU F91TH-FN                              AANA15
           MOVE        INAMSL TO 7-NA10-9LSS                            AANA15
           COMPUTE     INAMSL = INAMSL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STS.                           AANA15
       F91RA-FN. EXIT.
      *N91RG.    NOTE *COUNT NUMBER OF SPACES NEEDED      *.            AANA15
       F91RG.                                                           lv15
           MOVE        4 TO 7-NA10-9TSPAC.                              AANA15
                 IF    INAMHL = ZERO                                    DOT
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
                 IF    INAMSL = ZERO                                    DOT
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
                 IF    INAMML = ZERO                                    DOT
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
                 IF    INAMFL = ZERO                                    DOT
                 OR    INAMLL = ZERO                                    AANA15
           SUBTRACT    1 FROM 7-NA10-9TSPAC.                            AANA15
       F91RG-FN. EXIT.
      *N91RK.    NOTE *CALCULATE TOTAL NUMBER OF CHRS     *.            AANA15
       F91RK.                                                           lv15
           COMPUTE     ITOTLL = INAMHL +                                AANA15
           INAMFL + INAMML +                                            AANA15
           INAMLL + INAMSL +                                            AANA15
           7-NA10-9TSPAC                                                AANA15
      *IF NO NAME - ERROR                                               AANA15
                 IF    ITOTLL = ZERO                                    DOT
           MOVE        'E' TO 7-NA10-RETURN                             AANA15
               GO TO     F91RK-FN.                                      AANA15
      *PRINT FULL NAME IF SPACE PERMITS                                 AANA15
                 IF    ITOTLL NOT > 70                                  DOT
           MOVE        'HFMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        1 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT HONORIIFIC                                  AANA15
                 IF    INAMHL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMHL - 1.                                                  AANA15
                 IF    ITOTLL NOT > 70                                  DOT
           MOVE        ' FMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        2 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITH MIDDLE INITIAL                                 AANA15
                 IF    INAMML > 1                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMML + 1.                                                  AANA15
                 IF    ITOTLL NOT > 70                                  DOT
           MOVE        ' FILS' TO 7-NA10-9PRINT                         AANA15
           MOVE        3 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH W/O MIDDLE INITIAL                                  AANA15
                 IF    INAMML > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - 2.                             AANA15
                 IF    ITOTLL NOT > 70                                  DOT
           MOVE        ' F LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        4 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITH FIRST INITIAL                                  AANA15
                 IF    INAMFL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMFL + 1.                                                  AANA15
                 IF    ITOTLL NOT > 70                                  DOT
           MOVE        ' I LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        5 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT SUFFIX                                      AANA15
                 IF    INAMSL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - INAMSL - 1.                    AANA15
                 IF    ITOTLL NOT > 70                                  DOT
           MOVE        ' I L ' TO 7-NA10-9PRINT                         AANA15
           MOVE        6 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *PRINT ONLY LAST NAME.                                            AANA15
           MOVE        '   L ' TO 7-NA10-9PRINT                         DOT
           MOVE        7 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
       F91RK-FN. EXIT.
      *N91RO.    NOTE *FORMAT NAME                        *.            AANA15
       F91RO.                                                           lv15
           MOVE        0 TO INMWKR INMWKM                               AANA15
           MOVE        1 TO ITOTLR                                      AANA15
           MOVE        SPACES TO 7-NA10-COCLNM1.                        AANA15
       F91RO-FN. EXIT.
      *N91RS.    NOTE *MOVE HONORIFIC IF POSSIBLE         *.            AANA15
       F91RS.    IF    7-NA10-9PRTH = 'H'                               lv15
                 AND   INAMHL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F91RS-FN.                 AANA15
           MOVE        7-NA10-CLNAMH TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSH TO INMWKM                            AANA15
           MOVE        7-NA10-9STH TO INMWKR                            AANA15
           PERFORM     F91TP THRU F91TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F91RS-FN. EXIT.
      *N91RX.    NOTE *MOVE FIRST NAME IF POSSIBLE        *.            AANA15
       F91RX.    IF    7-NA10-9PRTF = 'F'                               lv15
                 AND   INAMFL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F91RX-FN.                 AANA15
           MOVE        7-NA10-CLNAMF TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSF TO INMWKM                            AANA15
           MOVE        7-NA10-9STF TO INMWKR                            AANA15
           PERFORM     F91TP THRU F91TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F91RX-FN. EXIT.
      *N91RZ.    NOTE *MOVE FIRST INITIAL ?               *.            AANA15
       F91RZ.    IF    7-NA10-9PRTF = 'I'                               lv15
                 AND   INAMFL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F91RZ-FN.                 AANA15
           MOVE        7-NA10-9NMF (7-NA10-9STF) TO                     AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         2 TO ITOTLR.                                     AANA15
       F91RZ-FN. EXIT.
      *N91SB.    NOTE *MOVE MIDDLE NAME ?                 *.            AANA15
       F91SB.    IF    7-NA10-9PRTM = 'M'                               lv15
                 AND   INAMML > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F91SB-FN.                 AANA15
           MOVE        7-NA10-CLNAMM TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSM TO INMWKM                            AANA15
           MOVE        7-NA10-9STM TO INMWKR                            AANA15
           PERFORM     F91TP THRU F91TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F91SB-FN. EXIT.
      *N91SE.    NOTE *MOVE MIDDLE INITIAL                *.            AANA15
       F91SE.    IF    7-NA10-9PRTM = 'I'                               lv15
                 AND   INAMML > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F91SE-FN.                 AANA15
           MOVE        7-NA10-9NMM (7-NA10-9STM) TO                     AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         2 TO ITOTLR.                                     AANA15
       F91SE-FN. EXIT.
      *N91SH.    NOTE *MOVE LAST NAME                     *.            AANA15
       F91SH.    IF    7-NA10-9PRTL = 'L'                               lv15
                 AND   INAMLL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F91SH-FN.                 AANA15
           MOVE        7-NA10-CLNAML TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSL TO INMWKM                            AANA15
           MOVE        7-NA10-9STL TO INMWKR                            AANA15
           PERFORM     F91TP THRU F91TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F91SH-FN. EXIT.
      *N91SL.    NOTE *MOVE SUFFIX ?                      *.            AANA15
       F91SL.    IF    7-NA10-9PRTS = 'S'                               lv15
                 AND   INAMSL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F91SL-FN.                 AANA15
           MOVE        7-NA10-CLNAMS TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSS TO INMWKM                            AANA15
           MOVE        7-NA10-9STS TO INMWKR                            AANA15
           PERFORM     F91TP THRU F91TP-FN.                             AANA15
       F91SL-FN. EXIT.
      *N91SZ.    NOTE *MOVE PACKED NAME TO PASSED PARM    *.            AANA15
       F91SZ.                                                           lv15
           MOVE        7-NA10-COCLNM1 TO                                AANA15
           HTML-MCLNM.                                                  AANA15
       F91SZ-FN. EXIT.
       F91QA-FN. EXIT.
      *N91TC.    NOTE *DECREMENT COUNTER                  *.            AANA15
       F91TC.    IF    INMWKR > 0                                       lv10
                 AND   7-NA10-9WKNM1 (INMWKR)                           AANA15
                       = SPACES                                         AANA15
                 NEXT SENTENCE ELSE GO TO     F91TC-FN.                 AANA15
           SUBTRACT    1 FROM INMWKR.                                   AANA15
       F91TC-900. GO TO F91TC.
       F91TC-FN. EXIT.
      *N91TH.    NOTE *RESET SUBSCRIPT                    *.            AANA15
       F91TH.                                                           lv10
           MOVE        INMWKR TO INMWKM                                 AANA15
           MOVE        1 TO INMWKR                                      AANA15
           PERFORM     F91TL THRU F91TL-FN.                             AANA15
       F91TH-FN. EXIT.
      *N91TL.    NOTE *INCREMENT COUNTER                  *.            AANA15
       F91TL.    IF    INMWKR NOT >                                     lv10
                       INMWKM                                           AANA15
                 AND   7-NA10-9WKNM1 (INMWKR)                           AANA15
                       = SPACES                                         AANA15
                 NEXT SENTENCE ELSE GO TO     F91TL-FN.                 AANA15
           ADD         1 TO INMWKR.                                     AANA15
       F91TL-900. GO TO F91TL.
       F91TL-FN. EXIT.
      *N91TP.    NOTE *SAVE CHARACTERS                    *.            AANA15
       F91TP.    IF    INMWKR NOT > INMWKM                              lv10
                 NEXT SENTENCE ELSE GO TO     F91TP-FN.                 AANA15
           MOVE        7-NA10-9WKNM1 (INMWKR) TO                        AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         1 TO ITOTLR INMWKR.                              AANA15
       F91TP-900. GO TO F91TP.
       F91TP-FN. EXIT.
       F91-FN.   EXIT.
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
