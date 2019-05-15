       IDENTIFICATION DIVISION.                                         CI0293
       PROGRAM-ID.  CI0293P.                                            CI0293
      *AUTHOR.         eFUNDING BA HTML BUILD.                          CI0293
      *DATE-COMPILED.   09/08/14.                                       CI0293
       ENVIRONMENT DIVISION.                                            CI0293
       CONFIGURATION SECTION.                                           CI0293
       SOURCE-COMPUTER. IBM-370.                                        CI0293
       OBJECT-COMPUTER. IBM-370.                                        CI0293
       DATA DIVISION.                                                   CI0293
       WORKING-STORAGE SECTION.                                         CI0293
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *COPYBOOK WITH CONSTANTS USED IN HTML

       COPY CI0293CN.
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

      *    PBN MASK POINTER
      *!WI
       01  WS00-CLORN1
                        PICTURE X(45).                                  CI0293
      *!WI
       01  WS00-TTBAL
                        PICTURE X(15).                                  CI0293
      *!WI
       01  WS00-NPBN
                        PICTURE X(20).                                  CI0293
       01  WS00-RTN               PIC X(10).
      *!WI
       01  WS00-PRCLN
                        PICTURE X(60).                                  CI0293
      *!WI
       01  WS00-MPRN4
                        PICTURE X(35).                                  CI0293
      ***************************************************************
      *MESSAGE FOR TRAD AND ROTH IRA SPECIFIC TO ACHI-IN
      *MESSAGE TEXT TO OVERRIDE MESSAGE # 14148 AND 13517
      ***************************************************************
       01                 WM00-TMESS4.
         05 FILLER PIC X(36)
                           VALUE 'IRAs have certain contribution limit'.
         05 FILLER PIC X(36)
                           VALUE 's, which are based on a number of fa'.
         05 FILLER PIC X(36)
                           VALUE 'ctors, including your age and income'.
         05 FILLER PIC X(36)
                           VALUE '. If you have questions about these '.
         05 FILLER PIC X(36)
                           VALUE 'limits, contact your advisor or call'.
         05 FILLER PIC X(16)
                           VALUE ' 1-800-297-5300.'.
      *MISCELLANEOUS FIELDS

       01  WS00-WORKAREA.
           05 WS00-CTID.
      *!WS
              10 WS00-NCTIDN          VALUE SPACES
                        PICTURE 9999B9999B9999.                         CI0293
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD  PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.

           05 WS00-CLID.
      *!WS
              10 WS00-CLIDN2          VALUE SPACES
                        PICTURE 9999B9999.                              CI0293
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD2 PIC X    VALUE SPACES.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CLIDO  PIC X(3) VALUE SPACES.

           05 WS00-APMT      PIC $,$$$,$$$,$$$.99.
      *!WS
           05 WS00-DCACG
                        PICTURE 99B99B9999.                             CI0293
      *!WS
           05 WS00-DNPMT
                        PICTURE ZZBZZBZZZZ.                             CI0293
      *!WS
           05 WS00-GEEND
                        PICTURE 99B99B9999.                             CI0293

       01 WS00-TS.
      *!WE
           05 WS00-DTGMM
                        PICTURE 9(2).                                   CI0293
           05 FILLER         PIC X    VALUE '/'.
      *!WE
           05 WS00-DTGDD
                        PICTURE 9(2).                                   CI0293
           05 FILLER         PIC X    VALUE '/'.
      *!WE
           05 WS00-DTGCY
                        PICTURE 9(4).                                   CI0293
           05 FILLER         PIC X    VALUE ' '.
      *    05 WS00-DTTHH.
      *    05 FILLER         PIC X    VALUE ':'.
      *    05 WS00-DTTMN.
      *    05 FILLER         PIC X    VALUE ' '.
      *    05 WS00-AMPM      PIC XX.
      *    05 FILLER         PIC X(4) VALUE ' CST'.

      *PBN MASK POINTER
       01   WS01-PT                PIC 99.
       01   WS-CNT      PIC 9(2) VALUE ZERO.
      *USED TO FORMAT THE IRA CONTRIBUTION LIMIT MESSAGE.
       01 WS00-ACLIM   PIC $$,$$$,$$9.99.
      *!WI
       01 WS00-NMESS2
                        PICTURE S9(6)                                   CI0293
                          COMPUTATIONAL-3.                              CI0293
              88 IRA-CNTB-LIMIT-MSG
                        VALUES  013515, 013516, 013610, 013611,
                                013517, 014148, 014149.
       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WT115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0293
      *!WI pl=WT120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0293
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WT130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0293
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WT140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0293
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WT150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0293
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WT160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0293
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WT170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0293
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WT180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0293
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
       01   DEBUT-WSS.                                                  CI0293
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0293
            05   IK     PICTURE X.                                      CI0293
       01  CONSTANTES-PAC.                                              CI0293
           05  FILLER  PICTURE X(87)   VALUE                            CI0293
                     '6015 CAT09/08/14CI0293ADMIN   14:35:17CI0293P AMERCI0293
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0293
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0293
           05  NUGNA   PICTURE X(5).                                    CI0293
           05  APPLI   PICTURE X(3).                                    CI0293
           05  DATGN   PICTURE X(8).                                    CI0293
           05  PROGR   PICTURE X(6).                                    CI0293
           05  CODUTI  PICTURE X(8).                                    CI0293
           05  TIMGN   PICTURE X(8).                                    CI0293
           05  PROGE   PICTURE X(8).                                    CI0293
           05  COBASE  PICTURE X(4).                                    CI0293
           05  DATGNC  PICTURE X(10).                                   CI0293
           05  RELEAS  PICTURE X(7).                                    CI0293
           05  DATGE   PICTURE X(10).                                   CI0293
           05  DATSQ   PICTURE X(10).                                   CI0293
       01  DATCE.                                                       CI0293
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0293
         05  DATOR.                                                     CI0293
           10  DATOA  PICTURE XX.                                       CI0293
           10  DATOM  PICTURE XX.                                       CI0293
           10  DATOJ  PICTURE XX.                                       CI0293
       01  DAT6.                                                        CI0293
            10 DAT61.                                                   CI0293
            15 DAT619  PICTURE 99.                                      CI0293
            10 DAT62.                                                   CI0293
            15 DAT629  PICTURE 99.                                      CI0293
            10 DAT63   PICTURE XX.                                      CI0293
       01  DAT8.                                                        CI0293
            10 DAT81   PICTURE XX.                                      CI0293
            10 DAT8S1  PICTURE X.                                       CI0293
            10 DAT82   PICTURE XX.                                      CI0293
            10 DAT8S2  PICTURE X.                                       CI0293
            10 DAT83   PICTURE XX.                                      CI0293
       01  DAT8E    REDEFINES    DAT8.                                  CI0293
            10 DAT81E  PICTURE X(4).                                    CI0293
            10 DAT82E  PICTURE XX.                                      CI0293
            10 DAT83E  PICTURE XX.                                      CI0293
       01  DAT6C.                                                       CI0293
            10  DAT61C PICTURE XX.                                      CI0293
            10  DAT62C PICTURE XX.                                      CI0293
            10  DAT63C.                                                 CI0293
             15 DAT63CC PICTURE XX.                                     CI0293
             15 DAT64C  PICTURE XX.                                     CI0293
       01  DAT8C.                                                       CI0293
            10  DAT81C  PICTURE XX.                                     CI0293
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0293
            10  DAT82C  PICTURE XX.                                     CI0293
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0293
            10  DAT83C.                                                 CI0293
             15 DAT83CC PICTURE XX.                                     CI0293
             15 DAT84C  PICTURE XX.                                     CI0293
       01  DATSEP     PICTURE X VALUE '/'.                              CI0293
       01  DATSEW     PICTURE X.                                        CI0293
       01   VARIABLES-CONDITIONNELLES.                                  CI0293
            05                  FT      PICTURE X VALUE '0'.            CI0293
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0293
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0293
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
            05           J60CCR PICTURE S9(4) VALUE  ZERO.
            05           J60CVR PICTURE S9(4) VALUE  ZERO.
            05           J60XGR PICTURE S9(4) VALUE  ZERO.
            05           J60YDR PICTURE S9(4) VALUE  ZERO.
            05           J60YGR PICTURE S9(4) VALUE  ZERO.
            05           J60YKR PICTURE S9(4) VALUE  ZERO.
            05           J60YOR PICTURE S9(4) VALUE  ZERO.
            05           J60YPR PICTURE S9(4) VALUE  ZERO.
            05           J60YTR PICTURE S9(4) VALUE  ZERO.
            05           J60ZCR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0293
      *COPYBOOK WITH HTML TEXT

       COPY CI0293C1.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **         THIS SEGMENT IS THE INPUT LINKAGE FOR CI0293          *
      ******************************************************************
      *
      *!WF DSP=LM DSL=QT SEL=9F FOR=I LEV=1 PLT=75
       01                 LM00.                                         CI0293
          05              LM00-SUITE.                                   CI0293
            15       FILLER         PICTURE  X(00848).                  CI0293
       01                 LM9F  REDEFINES      LM00.                    CI0293
            10            LM9F-CUPIQ  PICTURE  X.                       CI0293
            10            LM9F-CPROCM PICTURE  X.                       CI0293
            10            LM9F-IMQMG  PICTURE  X.                       CI0293
            10            LM9F-C199.                                    CI0293
            11            LM9F-CLID.                                    CI0293
            12            LM9F-CLIDO  PICTURE  9(3).                    CI0293
            12            LM9F-CLIDN.                                   CI0293
            13            LM9F-CLIDNP PICTURE  X(12).                   CI0293
            13            LM9F-CLIDND PICTURE  9(8).                    CI0293
            10            LM9F-CCONF  PICTURE  X(25).                   CI0293
            10            LM9F-DXTMSA PICTURE  X(26).                   CI0293
            10            LM9F-C198.                                    CI0293
            11            LM9F-CLNAM.                                   CI0293
            12            LM9F-CLNAMH PICTURE  X(6).                    CI0293
            12            LM9F-CLNAMF PICTURE  X(20).                   CI0293
            12            LM9F-CLNAMM.                                  CI0293
            13            LM9F-CLNAMI PICTURE  X.                       CI0293
            13            LM9F-CLNAMR PICTURE  X(14).                   CI0293
            12            LM9F-CLNAML PICTURE  X(25).                   CI0293
            12            LM9F-CLNAMS PICTURE  X(4).                    CI0293
            10            LM9F-GECKD2 PICTURE  9.                       CI0293
            10            LM9F-CLORN  PICTURE  X(45).                   CI0293
            10            LM9F-NTR    PICTURE  9(8).                    CI0293
            10            LM9F-GECKD1 PICTURE  9.                       CI0293
            10            LM9F-NPBN   PICTURE  X(20).                   CI0293
            10            LM9F-TTBAL  PICTURE  X(15).                   CI0293
            10            LM9F-CPMTF  PICTURE  99.                      CI0293
            10            LM9F-MPMTFL PICTURE  X(24).                   CI0293
            10            LM9F-MPMTT  PICTURE  X(20).                   CI0293
            10            LM9F-APMTF3 PICTURE  9(09)V99.                CI0293
            10            LM9F-DNPMT  PICTURE  9(8).                    CI0293
            10            LM9F-GEEND  PICTURE  9(8).                    CI0293
            10            LM9F-TARST  PICTURE  X(10).                   CI0293
            10            LM9F-NIRACM PICTURE  9(2).                    CI0293
            10            LM9F-DIRAYR PICTURE  9(4).                    CI0293
            10            LM9F-C299.                                    CI0293
            11            LM9F-CTID.                                    CI0293
            12            LM9F-CTIDA  PICTURE  9(3).                    CI0293
            12            LM9F-CTIDN.                                   CI0293
            13            LM9F-CTIDNP PICTURE  X(13).                   CI0293
            13            LM9F-CTIDND PICTURE  9(11).                   CI0293
            10            LM9F-GECKD  PICTURE  9.                       CI0293
            10            LM9F-PRCLN  PICTURE  X(60).                   CI0293
            10            LM9F-CTTLN1 PICTURE  X(30).                   CI0293
            10            LM9F-CTTLN2 PICTURE  X(30).                   CI0293
            10            LM9F-CTTLN3 PICTURE  X(30).                   CI0293
            10            LM9F-CTTBO1 PICTURE  X(45).                   CI0293
            10            LM9F-CTTBO2 PICTURE  X(45).                   CI0293
            10            LM9F-TSECD  PICTURE  X(30).                   CI0293
            10            LM9F-NMESA  PICTURE  9(6)                     CI0293
                          OCCURS       005     TIMES.                   CI0293
            10            LM9F-CLTYP  PICTURE  X.                       CI0293
            10            LM9F-CLORN1 PICTURE  X(45).                   CI0293
            10            LM9F-MLNAMR PICTURE  X(51).                   CI0293
            10            LM9F-ACLIM  PICTURE  S9(7)V9(2).              CI0293
            10            LM9F-FILLER PICTURE  X(59).                   CI0293
            10            LM9F-CQACT  PICTURE  999.                     CI0293
            10            LM9F-DXTMS2 PICTURE  X(26).                   CI0293
      *
      ******************************************************************
      **         THIS SEGMENT IS THE OUTPUT LINKAGE FOR CI0293         *
      ******************************************************************
      *
      *!WF DSP=HM DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HM00.                                         CI0293
          05              HM00-SUITE.                                   CI0293
            15       FILLER         PICTURE  X(90906).                  CI0293
       01                 HM93  REDEFINES      HM00.                    CI0293
            10            HM93-QBLCK  PICTURE  9(6).                    CI0293
            10            HM93-QT9O.                                    CI0293
            11            HM93-QT9B                                     CI0293
                          OCCURS       450     TIMES.                   CI0293
            12            HM93-CHTML  PICTURE  99.                      CI0293
            12            HM93-THTML  PICTURE  X(200).                  CI0293
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0293
          05              MS00-SUITE.                                   CI0293
            15       FILLER         PICTURE  X(00542).                  CI0293
       01                 MS03  REDEFINES      MS00.                    CI0293
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0293
                          COMPUTATIONAL-3.                              CI0293
            10            MS03-CMSSF  PICTURE  XX.                      CI0293
            10            MS03-DU09.                                    CI0293
            11            MS03-CMESA  PICTURE  S9(9)                    CI0293
                          BINARY.                                       CI0293
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0293
                          BINARY.                                       CI0293
            11            MS03-CMESB  PICTURE  S9(9)                    CI0293
                          BINARY.                                       CI0293
            11            MS03-CMSST  PICTURE  S9(9)                    CI0293
                          BINARY.                                       CI0293
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0293
                          BINARY.                                       CI0293
            11            MS03-QELLAA PICTURE  S9(9)                    CI0293
                          BINARY.                                       CI0293
            11            MS03-TMESS4 PICTURE  X(512).                  CI0293
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0293
            10            MX11-QMSGS  PICTURE  9(03).                   CI0293
            10            MX11-PJ09                                     CI0293
                          OCCURS       025     TIMES.                   CI0293
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0293
                          COMPUTATIONAL-3.                              CI0293
            11            MX11-CMESB  PICTURE  S9(9)                    CI0293
                          BINARY.                                       CI0293
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                LM9F
                                HM93
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0293
      *               *                                   *             CI0293
      *               *INITIALISATIONS                    *             CI0293
      *               *                                   *             CI0293
      *               *************************************.            CI0293
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
           HTML-MESSAGE-TAGS1
           HTML-MESSAGE-TBLCK
           HTML-TRANS-ONETIME-TAGES
           HTML-TRANS-RECURR-TAGES.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0293
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0293
      *               *                                   *             CI0293
      *               *FIN DE TRAITEMENT                  *             CI0293
      *               *                                   *             CI0293
      *               *************************************.            CI0293
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0293
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
      *N35AD.    NOTE *FORMAT TRANSFER YEAR               *.
       F35AD.    IF    LM9F-CPMTF = 99                                  lv10
                 NEXT SENTENCE ELSE GO TO     F35AD-FN.
           MOVE        LM9F-DXTMS2 TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGCY TO WS00-DTGCY
           MOVE        WS00-DTGCY TO HTML-YEAR.
       F35AD-FN. EXIT.
      *N35AE.    NOTE *FORMAT THE SUBMITTED DATE          *.
       F35AE.                                                           lv10
           MOVE        LM9F-DXTMSA TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGCY TO WS00-DTGCY
           MOVE        7-DB2-DTGMM TO WS00-DTGMM
           MOVE        7-DB2-DTGDD TO WS00-DTGDD
           MOVE        WS00-TS TO HTML-TS.
       F35AE-FN. EXIT.
      *N35DB.    NOTE *MOVE CONFIRM INFO                  *.
       F35DB.    IF    LM9F-CUPIQ = 'U'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
      *N35DD.    NOTE *MOVE CONFIRMATION TITLE / NUMBER   *.
       F35DD.                                                           lv15
           MOVE        CN-CONFIRM-TITLE TO HTML-TITLE
           MOVE        LM9F-CCONF TO HTML-CCONF.
       F35DD-FN. EXIT.
      *N35DE.    NOTE *INITIALIZE NUMBER OF MESSAGES      *.
       F35DE.                                                           lv15
           MOVE        1 TO WS-CNT.
       F35DE-FN. EXIT.
      *N35DF.    NOTE *FORMAT OFF-HOURS MESSAGES          *.
       F35DF.    IF    LM9F-IMQMG = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F35DF-FN.
           MOVE        CN-OFF-HOURS01 TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT
           MOVE        CN-OFF-HOURS02 TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DF-FN. EXIT.
      *N35DI.    NOTE *FORMAT ADD MESSAGE                 *.
       F35DI.    IF    LM9F-CPROCM = 'A'                                lv15
                 NEXT SENTENCE ELSE GO TO     F35DI-FN.
           MOVE        CN-ADD-TMESSA TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DI-900. GO TO F35DK-FN.
       F35DI-FN. EXIT.
      *N35DK.    NOTE *FORMAT CHANGE MESSAGE              *.
       F35DK.                                                           lv15
           MOVE        CN-MOD-TMESSA TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DK-FN. EXIT.
      *N35DM.    NOTE *FORMAT ON-DEMAND MESSAGES          *.
       F35DM.    IF    LM9F-CPMTF = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F35DM-FN.
           MOVE        CN-ON-DEMAND-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DM-900. GO TO F35DN-FN.
       F35DM-FN. EXIT.
      *N35DN.    NOTE *FORMAT RECURRING MESSAGES          *.
       F35DN.                                                           lv15
           MOVE        CN-RECURRING-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35DN-FN. EXIT.
      *N35DU.    NOTE *FORMAT THE CLIENT DATA             *.
       F35DU.                                                           lv15
      ********************************
      **  FORMAT THE CLIENT ID IN    *
      **  XXXX XXXX X XXX FORMAT     *
      ********************************
           MOVE        LM9F-CLIDND TO WS00-CLIDN2
           MOVE        LM9F-CLIDO TO WS00-CLIDO
           MOVE        LM9F-GECKD2 TO WS00-GECKD2
           MOVE        WS00-CLID TO HTML-CLID.
                 IF    LM9F-CLTYP = 'O'                                 DOT
      *CLIENT IS AN ORGANIZATION
           STRING      LM9F-MLNAMR DELIMITED BY '   '
           ' FOR' DELIMITED BY SIZE
           LM9F-CLORN1 DELIMITED BY SIZE
           INTO HTML-MCLNM1.
                 IF    LM9F-CLTYP = 'P'                                 DOT
      *CLIENT IS PERSON
           PERFORM     F91 THRU F91-FN.
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
      *N35FP.    NOTE *FORMAT OFF-HOURS MESSAGES          *.
       F35FP.    IF    LM9F-IMQMG = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F35FP-FN.
           MOVE        1 TO WS-CNT
           MOVE        CN-VERIFY-OFF TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F35FP-FN. EXIT.
       F35FB-FN. EXIT.
      *N35MB.    NOTE *BUILD BANK INFORMATION             *.
       F35MB.                                                           lv10
           MOVE        LM9F-CLORN TO WS00-CLORN1
           MOVE        LM9F-TTBAL TO WS00-TTBAL.
       F35MB-FN. EXIT.
      *N35MD.    NOTE *FORMAT BANK RTN                    *.
       F35MD.                                                           lv10
           STRING      LM9F-NTR '-' LM9F-GECKD1
           DELIMITED BY SIZE
           INTO WS00-RTN.
       F35MD-FN. EXIT.
      *N35OC.    NOTE *LOAD PBN                           *.
       F35OC.                                                           lv10
           MOVE        LM9F-NPBN TO WS00-NPBN.
       F35OC-FN. EXIT.
      *N35OE.    NOTE *FIND TRAILING NUMERIC              *.
       F35OE.                                                           lv10
           MOVE        20                       TO J35OER
                                    GO TO     F35OE-B.
       F35OE-A.
           SUBTRACT 1                         FROM J35OER.
       F35OE-B.
           IF          J35OER                   <  4
                                    GO TO     F35OE-FN.
                 IF    WS00-NPBN (J35OER:1) NOT =                       DOT
                       ' '
           COMPUTE     WS01-PT = J35OER - 4
               GO TO     F35OE-FN.
       F35OE-900. GO TO F35OE-A.
       F35OE-FN. EXIT.
      *N35OG.    NOTE *MASK LEADING CHARACTERS            *.
       F35OG.                                                           lv10
           MOVE ALL    '*' TO WS00-NPBN (1:WS01-PT).
       F35OG-FN. EXIT.
      *N35PD.    NOTE *FORMAT BANK DETAIL                 *.
       F35PD.                                                           lv10
      *********************************
           STRING      WS00-CLORN1 ' ' WS00-TTBAL
           '<BR>' WS00-NPBN
           DELIMITED BY SIZE
           INTO HTML-FROM.
       F35PD-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *BUILD ACCOUNT TAGS                 *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40AF.    NOTE *STRING ACCOUNT NAME                *.
       F40AF.                                                           lv10
           MOVE        LM9F-PRCLN TO WS00-PRCLN.
       F40AF-FN. EXIT.
      *N40CB.    NOTE *FORMAT CONTRACT ID                 *.
       F40CB.                                                           lv10
           MOVE        LM9F-CTIDND TO WS00-NCTIDN
           MOVE        LM9F-GECKD TO WS00-GECKD
           MOVE        LM9F-CTIDA TO WS00-CTIDA.
       F40CB-FN. EXIT.
      *N40DD.    NOTE *FORMAT ACCOUNT DETAIL              *.
       F40DD.                                                           lv10
      *********************************
           MOVE        1 TO TEMP-PT
           STRING      WS00-PRCLN
           '<BR>' LM9F-CTTLN1
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
      *N40DF.    NOTE *STRING OWNERSHIP LINE # 2          *.
       F40DF.    IF    LM9F-CTTLN2 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F40DF-FN.
           STRING      '<BR>' LM9F-CTTLN2
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
       F40DF-FN. EXIT.
      *N40DH.    NOTE *STRING OWNERSHIP LINE # 3          *.
       F40DH.    IF    LM9F-CTTLN3 > SPACES                             lv15
                 NEXT SENTENCE ELSE GO TO     F40DH-FN.
           STRING      '<BR>' LM9F-CTTLN3
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
       F40DH-FN. EXIT.
      *N40DM.    NOTE *STRING CONTRACT ID                 *.
       F40DM.                                                           lv15
           STRING      '<BR>' WS00-CTID
           DELIMITED BY SIZE
           INTO HTML-TO
           WITH POINTER TEMP-PT.
       F40DM-FN. EXIT.
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
           MOVE        LM9F-APMTF3 TO WS00-APMT
           MOVE        WS00-APMT TO HTML-APMT.
       F45GB-FN. EXIT.
      *N45JB.    NOTE *MOVE FREQUENCY                     *.
       F45JB.                                                           lv10
           MOVE        LM9F-MPMTFL TO HTML-MPMTFL1.
       F45JB-FN. EXIT.
      *N45JF.    NOTE *MOVE TRANSFER TYPE                 *.
       F45JF.                                                           lv10
                 IF    LM9F-CPMTF = 99                                  DOT
           MOVE        'One-time' TO HTML-MPMTFL
                 ELSE
           MOVE        'Recurring' TO HTML-MPMTFL.
       F45JF-FN. EXIT.
      *N45LB.    NOTE *REFORMAT THE NEXT PAYMENT DATE     *.
       F45LB.                                                           lv10
      *!ADS "LM9F-DNPMT        WS00-DNPMT"
           MOVE        LM9F-DNPMT                                       CI0293
           TO DAT8E DAT6C                                               CI0293
           MOVE DAT81E TO DAT63C                                        CI0293
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0293
           MOVE   DAT6C TO  WS00-DNPMT                                  CI0293
      *!ADM "WS00-DNPMT        HTML-DNPMT"
           MOVE        WS00-DNPMT                                       CI0293
           TO DAT8E DAT6C                                               CI0293
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0293
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0293
           MOVE   DAT8C TO  HTML-DNPMT.                                 CI0293
       F45LB-FN. EXIT.
      *N45MD.    NOTE *FORMAT IRA PRIOR YEAR              *.
       F45MD.    IF    LM9F-NIRACM > 00                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45MD-FN.
      *N45ME.    NOTE *FORMAT IRA PRIOR YEAR - RECUR      *.
       F45ME.    IF    LM9F-CPMTF NOT = 99                              lv15
                 NEXT SENTENCE ELSE GO TO     F45ME-FN.
           MOVE        CN-IRA-M1 TO CN-RECURRING-M1.
                 IF    LM9F-NIRACM = 02                                 DOT
           MOVE        CN-IRA-M2 TO CN-RECURRING-M2.
                 IF    LM9F-NIRACM = 03                                 DOT
           MOVE        CN-IRA-M3 TO CN-RECURRING-M2.
                 IF    LM9F-NIRACM = 04                                 DOT
           MOVE        CN-IRA-M4 TO CN-RECURRING-M2.
                 IF    LM9F-NIRACM > 01                                 DOT
           MOVE        CN-IRA-THRU TO CN-RECURRING-TX.
           STRING      CN-RECURRING-IRA1                                DOT
           CN-RECURRING-IRA2
           DELIMITED BY SIZE
           INTO HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F45ME-FN. EXIT.
       F45MD-FN. EXIT.
      *N45MI.    NOTE *FORMAT IRA YEAR - ON-DEMAND        *.
       F45MI.    IF    LM9F-CPMTF = 99                                  lv10
                 NEXT SENTENCE ELSE GO TO     F45MI-FN.
      *N45MJ.    NOTE *FORMAT IRA YEAR - ON-DEMAND        *.
       F45MJ.    IF    LM9F-DIRAYR > 00                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45MJ-FN.
           MOVE        LM9F-DIRAYR TO CN-DIRAYR
           MOVE        CN-ON-DEMAND-IRA-TEXT TO
           HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F45MJ-FN. EXIT.
       F45MI-FN. EXIT.
      *N45OD.    NOTE *FORMAT END DATE                    *.
       F45OD.         EXIT.                                             lv10
      *N45OG.    NOTE *RECURRING                          *.
       F45OG.    IF    LM9F-CPMTF NOT = 99                              lv15
                 NEXT SENTENCE ELSE GO TO     F45OG-FN.
      *N45OH.    NOTE *CALCULATE THE END DATE             *.
       F45OH.    IF    LM9F-GEEND > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F45OH-FN.
      *!ADS "LM9F-GEEND        WS00-GEEND"
           MOVE        LM9F-GEEND                                       CI0293
           TO DAT8E DAT6C                                               CI0293
           MOVE DAT81E TO DAT63C                                        CI0293
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0293
           MOVE   DAT6C TO  WS00-GEEND                                  CI0293
      *!ADM "WS00-GEEND        HTML-GEEND"
           MOVE        WS00-GEEND                                       CI0293
           TO DAT8E DAT6C                                               CI0293
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0293
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0293
           MOVE   DAT8C TO  HTML-GEEND.                                 CI0293
       F45OH-900. GO TO F45OK-FN.
       F45OH-FN. EXIT.
      *N45OK.    NOTE *NO END DATE                        *.
       F45OK.                                                           lv20
           MOVE        CN-NO-END-DATE TO HTML-GEEND.
       F45OK-FN. EXIT.
       F45OG-FN. EXIT.
       F45OD-FN. EXIT.
       F45-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *BUILD MESSAGE INFO                 *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55BC.    NOTE *MOVE MESSAGE TEXT 1                *.
       F55BC.    IF    LM9F-NMESA (1) > 0                               lv10
                 NEXT SENTENCE ELSE GO TO     F55BC-FN.
           MOVE        LM9F-NMESA (1) TO MS03-NMESS2
           WS00-NMESS2
           PERFORM     F98GM THRU F98GM-FN
           PERFORM     F95CA THRU F95CA-FN
           MOVE        MS03-TMESS4 TO HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F55BC-FN. EXIT.
      *N55BE.    NOTE *MOVE MESSAGE TEXT 2                *.
       F55BE.    IF    LM9F-NMESA (2) > 0                               lv10
                 NEXT SENTENCE ELSE GO TO     F55BE-FN.
           MOVE        LM9F-NMESA (2) TO MS03-NMESS2
           WS00-NMESS2
           PERFORM     F98GM THRU F98GM-FN
           PERFORM     F95CA THRU F95CA-FN
           MOVE        MS03-TMESS4 TO HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F55BE-FN. EXIT.
      *N55BG.    NOTE *MOVE MESSAGE TEXT 3                *.
       F55BG.    IF    LM9F-NMESA (3) > 0                               lv10
                 NEXT SENTENCE ELSE GO TO     F55BG-FN.
           MOVE        LM9F-NMESA (3) TO MS03-NMESS2
           WS00-NMESS2
           PERFORM     F98GM THRU F98GM-FN
           PERFORM     F95CA THRU F95CA-FN
           MOVE        MS03-TMESS4 TO HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F55BG-FN. EXIT.
      *N55BI.    NOTE *MOVE MESSAGE TEXT 4                *.
       F55BI.    IF    LM9F-NMESA (4) > 0                               lv10
                 NEXT SENTENCE ELSE GO TO     F55BI-FN.
           MOVE        LM9F-NMESA (4) TO MS03-NMESS2
           WS00-NMESS2
           PERFORM     F98GM THRU F98GM-FN
           PERFORM     F95CA THRU F95CA-FN
           MOVE        MS03-TMESS4 TO HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F55BI-FN. EXIT.
      *N55BK.    NOTE *MOVE MESSAGE TEXT 5                *.
       F55BK.    IF    LM9F-NMESA (5) > 0                               lv10
                 NEXT SENTENCE ELSE GO TO     F55BK-FN.
           MOVE        LM9F-NMESA (5) TO MS03-NMESS2
           WS00-NMESS2
           PERFORM     F98GM THRU F98GM-FN
           PERFORM     F95CA THRU F95CA-FN
           MOVE        MS03-TMESS4 TO HTML-TDTXT1 (WS-CNT)
           ADD         1 TO WS-CNT.
       F55BK-FN. EXIT.
      *N55QA.    NOTE *CLEAN UP MS03                      *.
       F55QA.                                                           lv10
           INITIALIZE  MS03.
       F55QA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *MOVE HTML TAGS INTO LM9F-OUTPUT    *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *********************************
      ** SEE COPYBOOK 'CI0293C1' FOR  *
      ** TAGS AND VALUES              *
      *********************************
      *N60BA.    NOTE *INITIALIZE LINE COUNTER            *.
       F60BA.                                                           lv10
           MOVE        0 TO HTML-PT.
       F60BA-FN. EXIT.
      *N60CC.    NOTE *STRING BODY INTO HTML-OUTPUT       *.
       F60CC.                                                           lv10
           MOVE        1                        TO J60CCR
                                    GO TO     F60CC-B.
       F60CC-A.
           ADD         1                        TO J60CCR.
       F60CC-B.
           IF          J60CCR                   >  HTML-HEADER-CTR
                                    GO TO     F60CC-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-HEADER-LINE (J60CCR) TO
           HM93-THTML (HTML-PT)
           MOVE        03 TO HM93-CHTML (HTML-PT).
       F60CC-900. GO TO F60CC-A.
       F60CC-FN. EXIT.
      *N60CU.    NOTE *STRING HEADER                      *.
       F60CU.         EXIT.                                             lv10
      *N60CV.    NOTE *STRING HEADER INTO HTML-OUTPUT     *.
       F60CV.                                                           lv15
           MOVE        1                        TO J60CVR
                                    GO TO     F60CV-B.
       F60CV-A.
           ADD         1                        TO J60CVR.
       F60CV-B.
           IF          J60CVR                   >  HTML-CONFIRM-CTR
                                    GO TO     F60CV-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-CONFIRM-LINE (J60CVR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60CV-900. GO TO F60CV-A.
       F60CV-FN. EXIT.
       F60CU-FN. EXIT.
      *N60XC.    NOTE *STRING CONFIRMATION SPECIFIC       *.
       F60XC.    IF    LM9F-CUPIQ = 'U'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F60XC-FN.
      *N60XG.    NOTE *STRING CONFIRM INTO HTML-OUTPUT    *.
       F60XG.                                                           lv15
           MOVE        1                        TO J60XGR
                                    GO TO     F60XG-B.
       F60XG-A.
           ADD         1                        TO J60XGR.
       F60XG-B.
           IF          J60XGR                   >  HTML-TRANS-CONFIRM-CT
                                    GO TO     F60XG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TRANS-CONFIRM-LINE (J60XGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60XG-900. GO TO F60XG-A.
       F60XG-FN. EXIT.
       F60XC-FN. EXIT.
      *N60YC.    NOTE *STRING COMMON INTO HTML-OUTPUT     *.
       F60YC.         EXIT.                                             lv10
      *N60YD.    NOTE *STRING COMMON INTO HTML-OUTPUT     *.
       F60YD.                                                           lv15
           MOVE        1                        TO J60YDR
                                    GO TO     F60YD-B.
       F60YD-A.
           ADD         1                        TO J60YDR.
       F60YD-B.
           IF          J60YDR                   >  HTML-TRANS-COMMON-CTR
                                    GO TO     F60YD-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TRANS-COMMON-LINE (J60YDR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60YD-900. GO TO F60YD-A.
       F60YD-FN. EXIT.
       F60YC-FN. EXIT.
      *N60YF.    NOTE *ONE TIME SPECIFIC                  *.
       F60YF.    IF    LM9F-CPMTF = 99                                  lv10
                 NEXT SENTENCE ELSE GO TO     F60YF-FN.
                 IF    LM9F-CQACT = ZEROS                               DOT
      *IF NON-QUALIFIED DESTINATION
      *DON'T SHOW 'TRANSFER IN YEAR'
               GO TO     F60YF-FN.
      *N60YG.    NOTE *STRING COMMON INTO HTML-OUTPUT     *.
       F60YG.                                                           lv15
           MOVE        1                        TO J60YGR
                                    GO TO     F60YG-B.
       F60YG-A.
           ADD         1                        TO J60YGR.
       F60YG-B.
           IF          J60YGR                   >  HTML-TRANS-ONETIME-CT
                                    GO TO     F60YG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TRANS-ONETIME-LINE (J60YGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60YG-900. GO TO F60YG-A.
       F60YG-FN. EXIT.
       F60YF-FN. EXIT.
      *N60YJ.    NOTE *RECURRING SPECIFIC                 *.
       F60YJ.    IF    LM9F-CPMTF NOT = 99                              lv10
                 NEXT SENTENCE ELSE GO TO     F60YJ-FN.
      *N60YK.    NOTE *STRING COMMON INTO HTML-OUTPUT     *.
       F60YK.                                                           lv15
           MOVE        1                        TO J60YKR
                                    GO TO     F60YK-B.
       F60YK-A.
           ADD         1                        TO J60YKR.
       F60YK-B.
           IF          J60YKR                   >  HTML-TRANS-RECURR-CTR
                                    GO TO     F60YK-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TRANS-RECURR-LINE (J60YKR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60YK-900. GO TO F60YK-A.
       F60YK-FN. EXIT.
       F60YJ-FN. EXIT.
      *N60YM.    NOTE *STRING MESSAGE                     *.
       F60YM.    IF    WS-CNT > 1                                       lv10
                 NEXT SENTENCE ELSE GO TO     F60YM-FN.
      *N60YO.    NOTE *STRING MESSAGE INTO HTML-OUTPUT    *.
       F60YO.                                                           lv15
           MOVE        1                        TO J60YOR
                                    GO TO     F60YO-B.
       F60YO-A.
           ADD         1                        TO J60YOR.
       F60YO-B.
           IF          J60YOR                   >  HTML-MESSAGE-CTR1
                                    GO TO     F60YO-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-MESSAGE-LINE1 (J60YOR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60YO-900. GO TO F60YO-A.
       F60YO-FN. EXIT.
      *N60YP.    NOTE *STRING MESSAGES INTO HTML-OUTPUT   *.
       F60YP.                                                           lv15
           MOVE        1                        TO J60YPR
                                    GO TO     F60YP-B.
       F60YP-A.
           ADD         1                        TO J60YPR.
       F60YP-B.
           IF          J60YPR                   >  20
                                    GO TO     F60YP-FN.
      *N60YR.    NOTE *ONLY MOVE GOOD MESSAGES            *.
       F60YR.    IF    HTML-TMESS1 (J60YPR)                             lv20
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F60YR-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESSN1 TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS1 (J60YPR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
                 IF    HTML-TMESS2 (J60YPR)                             DOT
                       NOT = SPACES
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS2 (J60YPR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           END-IF.
                 IF    HTML-TMESS3 (J60YPR)                             DOT
                       NOT = SPACES
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS3 (J60YPR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           END-IF.
           ADD         1 TO HTML-PT                                     DOT
           MOVE        HTML-TMESSN2 TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60YR-FN. EXIT.
       F60YP-900. GO TO F60YP-A.
       F60YP-FN. EXIT.
      *N60YT.    NOTE *STRING EOM INTO HTML-OUTPUT        *.
       F60YT.                                                           lv15
           MOVE        1                        TO J60YTR
                                    GO TO     F60YT-B.
       F60YT-A.
           ADD         1                        TO J60YTR.
       F60YT-B.
           IF          J60YTR                   >  HTML-EOM-CTR
                                    GO TO     F60YT-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-EOM-LINE (J60YTR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60YT-900. GO TO F60YT-A.
       F60YT-FN. EXIT.
       F60YM-FN. EXIT.
      *N60ZA.    NOTE *STRING FOOTER                      *.
       F60ZA.         EXIT.                                             lv10
      *N60ZC.    NOTE *STRING FOOTER INTO HTML-OUTPUT     *.
       F60ZC.                                                           lv15
           MOVE        1                        TO J60ZCR
                                    GO TO     F60ZC-B.
       F60ZC-A.
           ADD         1                        TO J60ZCR.
       F60ZC-B.
           IF          J60ZCR                   >  HTML-FOOTER-CTR
                                    GO TO     F60ZC-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-FOOTER-LINE (J60ZCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60ZC-900. GO TO F60ZC-A.
       F60ZC-FN. EXIT.
       F60ZA-FN. EXIT.
      *N60ZO.    NOTE *SEND END OF HTML AREA              *.
       F60ZO.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        SPACES TO HM93-THTML (HTML-PT)
           MOVE        99 TO HM93-CHTML (HTML-PT).
       F60ZO-FN. EXIT.
      *N60ZQ.    NOTE *MOVE LENGTH OF HTML BLOB           *.
       F60ZQ.                                                           lv10
           COMPUTE     HM93-QBLCK = HTML-PT.
       F60ZQ-FN. EXIT.
       F60-FN.   EXIT.
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
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *NAME PACKER                        *
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
           MOVE        LM9F-CLNAM TO 7-NA10-CLNAM.                      AANA15
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
                 IF    ITOTLL NOT > 40                                  DOT
           MOVE        'HFMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        1 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT HONORIIFIC                                  AANA15
                 IF    INAMHL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMHL - 1.                                                  AANA15
                 IF    ITOTLL NOT > 40                                  DOT
           MOVE        ' FMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        2 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITH MIDDLE INITIAL                                 AANA15
                 IF    INAMML > 1                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMML + 1.                                                  AANA15
                 IF    ITOTLL NOT > 40                                  DOT
           MOVE        ' FILS' TO 7-NA10-9PRINT                         AANA15
           MOVE        3 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH W/O MIDDLE INITIAL                                  AANA15
                 IF    INAMML > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - 2.                             AANA15
                 IF    ITOTLL NOT > 40                                  DOT
           MOVE        ' F LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        4 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITH FIRST INITIAL                                  AANA15
                 IF    INAMFL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMFL + 1.                                                  AANA15
                 IF    ITOTLL NOT > 40                                  DOT
           MOVE        ' I LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        5 TO 7-NA10-RETURN                               AANA15
               GO TO     F91RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT SUFFIX                                      AANA15
                 IF    INAMSL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - INAMSL - 1.                    AANA15
                 IF    ITOTLL NOT > 40                                  DOT
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
           HTML-MCLNM1.                                                 AANA15
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
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F95.           EXIT.                                             lv05
      *N95CA.    NOTE *IRA CONTRIBUTION LIMIT MESSAGE     *.
       F95CA.    IF    IRA-CNTB-LIMIT-MSG                               lv10
                 NEXT SENTENCE ELSE GO TO     F95CA-FN.
      *FORMATTING
           MOVE        LM9F-ACLIM TO WS00-ACLIM
      *FORMAT THE MESSAGE TXT
                 IF    WS00-NMESS2 = 013515                             DOT
                       OR 013611
           MOVE        WS00-ACLIM TO MS03-TMESS4 (155:13).
                 IF    WS00-NMESS2 = 013516                             DOT
                       OR 013610
           MOVE        WS00-ACLIM TO MS03-TMESS4 (146:13).
                 IF    WS00-NMESS2 = 013517                             DOT
                       OR 014148
           MOVE        WS00-ACLIM TO MS03-TMESS4 (44:13).
                 IF    WS00-NMESS2 = 014149                             DOT
           MOVE        WS00-ACLIM TO MS03-TMESS4 (55:13).
      *INITIALIZE THE TEMP VARIABLE                                     DOT
           INITIALIZE  WS00-NMESS2 WS00-ACLIM.
       F95CA-FN. EXIT.
       F95-FN.   EXIT.
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
