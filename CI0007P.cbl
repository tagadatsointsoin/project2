       IDENTIFICATION DIVISION.                                         CI0007
       PROGRAM-ID.  CI0007P.                                            CI0007
      *AUTHOR.         M\M - FORMAT DESTINATION INFO.                   CI0007
      *DATE-COMPILED.   09/08/14.                                       CI0007
       ENVIRONMENT DIVISION.                                            CI0007
       CONFIGURATION SECTION.                                           CI0007
       SOURCE-COMPUTER. IBM-370.                                        CI0007
       OBJECT-COMPUTER. IBM-370.                                        CI0007
       DATA DIVISION.                                                   CI0007
       WORKING-STORAGE SECTION.                                         CI0007
       01  7-ADDR.                                                      AANA20
      *!WI pl=AD030                                                     AANA20
           05  7-ADDR-GESAD                                             AANA20
                        PICTURE X(30)                                   CI0007
                             OCCURS 4.                                  AANA20
           05  7-ADDR-XZ45.                                             AANA20
      *!WI pl=AD040                                                     AANA20
               10  7-ADDR-XZ1                                           AANA20
                        PICTURE X                                       CI0007
                             OCCURS 30.                                 AANA20
           05  7-ADDR-XZ62.                                             AANA20
      *!WI pl=AD060                                                     AANA20
               10  7-ADDR-GECIT                                         AANA20
                        PICTURE X(25).                                  CI0007
               10  7-ADDR-FILLER PIC X VALUE SPACE.                     AANA20
      *!WI pl=AD080                                                     AANA20
               10  7-ADDR-GEST                                          AANA20
                        PICTURE X(8).                                   CI0007
               10  7-ADDR-FILLER PIC X VALUE SPACE.                     AANA20
      *!WI pl=AD100                                                     AANA20
               10  7-ADDR-GEPCD                                         AANA20
                        PICTURE X(12).                                  CI0007
               10  7-ADDR-FILLER PIC X VALUE SPACE.                     AANA20
      *!WI pl=AD120                                                     AANA20
               10  7-ADDR-GECTRY                                        AANA20
                        PICTURE X(20).                                  CI0007
           05  7-ADDR-XZ62A REDEFINES 7-ADDR-XZ62.                      AANA20
      *!WI pl=AD140                                                     AANA20
               10  7-ADDR-XZ1A                                          AANA20
                        PICTURE X                                       CI0007
                             OCCURS 68.                                 AANA20
      *!WI pl=AD210                                                     AANA20
           05  7-ADDR-XCF                                               AANA20
                        PICTURE X.                                      CI0007
      *!WI pl=AD220                                                     AANA20
           05  7-ADDR-XER  VALUE '0'                                    AANA20
                        PICTURE X.                                      CI0007
      *                   IN00                                          AANA20
      *                   RE00                                          AANA20
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
           EJECT                                                        AAOFCT
      ******************************************************************AAOFCT
      ***                                                             **AAOFCT
      ***     WORK AREAS FOR FORMATTING THE CONTRACT ID FOR DISPLAY   **AAOFCT
      ***                                                             **AAOFCT
      ******************************************************************AAOFCT
      ***                                                               AAOFCT
      ***     INPUT FIELDS: APPLICATION PROGRAM FILLS IN 7-INPT-UCTID,  AAOFCT
      ***     7-1ICT-GECKD AND 7-INPT-CTIDA PRIOR TO MACRO'S EXECUTION. AAOFCT
      ***                                                               AAOFCT
       01  7-INPT-CONTRACT.                                             AAOFCT
         05  7-INPT-UCTID.                                              AAOFCT
      *!WI pl=DH070                                                     AAOFCT
           10  7-1ICT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
      *!WI pl=DH075                                                     AAOFCT
           10  7-2ICT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
      *!WI pl=DH080                                                     AAOFCT
           10  7-3ICT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
      *!WI pl=DH085                                                     AAOFCT
           10  7-4ICT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
      *!WI pl=DH090                                                     AAOFCT
           10  7-5ICT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
      *!WI pl=DH095                                                     AAOFCT
           10  7-6ICT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
      *!WI pl=DH100                                                     AAOFCT
         05  7-1ICT-GECKD                                               AAOFCT
                        PICTURE 9.                                      CI0007
      *!WI pl=DH105                                                     AAOFCT
         05  7-INPT-CTIDA                                               AAOFCT
                        PICTURE 9(3).                                   CI0007
           SKIP1                                                        AAOFCT
      ***                                                               AAOFCT
      ***     FORMAT FIELDS:  THE FORMATTED ID IS MOVED TO 7-OUTP-UCTID AAOFCT
      ***                                                               AAOFCT
       01  7-FRMT-UCTID.                                                AAOFCT
      *!WI pl=DH135                                                     AAOFCT
         05    7-1FCT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
         05    FILLER                      PIC X(1) VALUE SPACE.        AAOFCT
         05    7-1FCT-UCTID.                                            AAOFCT
      *!WI pl=DH150                                                     AAOFCT
           10  7-2FCT-XZ4                                               AAOFCT
                        PICTURE X(4).                                   CI0007
           10  FILLER                      PIC X(1) VALUE SPACE.        AAOFCT
           10  7-2FCT-UCTID.                                            AAOFCT
      *!WI pl=DH165                                                     AAOFCT
             15  7-3FCT-XZ4                                             AAOFCT
                        PICTURE X(4).                                   CI0007
             15  FILLER                    PIC X(1) VALUE SPACE.        AAOFCT
      *!WI pl=DH180                                                     AAOFCT
             15  7-4FCT-XZ4                                             AAOFCT
                        PICTURE X(4).                                   CI0007
             15  FILLER                    PIC X(1) VALUE SPACE.        AAOFCT
      *!WI pl=DH195                                                     AAOFCT
             15  7-5FCT-XZ4                                             AAOFCT
                        PICTURE X(4).                                   CI0007
             15  FILLER                    PIC X(1) VALUE SPACE.        AAOFCT
      *!WI pl=DH210                                                     AAOFCT
             15  7-6FCT-XZ4                                             AAOFCT
                        PICTURE X(4).                                   CI0007
             15  FILLER                    PIC X(1) VALUE SPACE.        AAOFCT
      *!WI pl=DH220                                                     AAOFCT
             15  7-1FCT-GECKD                                           AAOFCT
                        PICTURE 9.                                      CI0007
             15  FILLER                    PIC X(1) VALUE SPACE.        AAOFCT
      *!WI pl=DH230                                                     AAOFCT
             15  7-FRMT-CTIDA                                           AAOFCT
                        PICTURE 9(3).                                   CI0007
           SKIP1                                                        AAOFCT
      ***                                                               AAOFCT
      ***    OUTPUT FIELD:  FORMATTED CONTRACT ID, CHECK DIGIT AND ADMINAAOFCT
      ***                                                               AAOFCT
       01  7-OUTP-UCTID                    PIC X(35).                   AAOFCT
      ******************************************************************AAOFCT
      ***                                                             **AAOFCT
      ***END OF WORK AREAS FOR FORMATTING THE CONTRACT ID FOR DISPLAY **AAOFCT
      ***                                                             **AAOFCT
      ******************************************************************AAOFCT
           SKIP3                                                        AAOFCT
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
      *
      ******************************************************************
      **     MISCELLANEOUS WORK FIELDS                                 *
      ******************************************************************
      *
       01  W-WORK-MISC.
           05  W-WORK-1PTR          PIC 999.
      *
       01  WS01-AMEX-TRUST          PIC X(30) VALUE
           'AMERICAN EXPRESS TRUST COMPANY'.
       01  WS01-AMPF-TRUST          PIC X(24) VALUE
           'AMERIPRISE TRUST COMPANY'.
      *
      *
      *
      *
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
       01   DEBUT-WSS.                                                  CI0007
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0007
            05   IK     PICTURE X.                                      CI0007
       01  CONSTANTES-PAC.                                              CI0007
           05  FILLER  PICTURE X(87)   VALUE                            CI0007
                     '6015 CAT09/08/14CI0007ADMIN   14:33:51CI0007P AMERCI0007
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0007
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0007
           05  NUGNA   PICTURE X(5).                                    CI0007
           05  APPLI   PICTURE X(3).                                    CI0007
           05  DATGN   PICTURE X(8).                                    CI0007
           05  PROGR   PICTURE X(6).                                    CI0007
           05  CODUTI  PICTURE X(8).                                    CI0007
           05  TIMGN   PICTURE X(8).                                    CI0007
           05  PROGE   PICTURE X(8).                                    CI0007
           05  COBASE  PICTURE X(4).                                    CI0007
           05  DATGNC  PICTURE X(10).                                   CI0007
           05  RELEAS  PICTURE X(7).                                    CI0007
           05  DATGE   PICTURE X(10).                                   CI0007
           05  DATSQ   PICTURE X(10).                                   CI0007
       01  DATCE.                                                       CI0007
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0007
         05  DATOR.                                                     CI0007
           10  DATOA  PICTURE XX.                                       CI0007
           10  DATOM  PICTURE XX.                                       CI0007
           10  DATOJ  PICTURE XX.                                       CI0007
       01   VARIABLES-CONDITIONNELLES.                                  CI0007
            05                  FT      PICTURE X VALUE '0'.            CI0007
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0007
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0007
            05           IIN00L PICTURE S9(4) VALUE  ZERO.              AANA20
            05           IIN00R PICTURE S9(4) VALUE  ZERO.              AANA20
            05           IIN00M PICTURE S9(4) VALUE +0068.              AANA20
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
            05           IRE00L PICTURE S9(4) VALUE  ZERO.              AANA20
            05           IRE00R PICTURE S9(4) VALUE  ZERO.              AANA20
            05           IRE00M PICTURE S9(4) VALUE +0045.              AANA20
            05           ITOTLL PICTURE S9(4) VALUE  ZERO.              AANA15
            05           ITOTLR PICTURE S9(4) VALUE  ZERO.              AANA15
            05           ITOTLM PICTURE S9(4) VALUE +0074.              AANA15
       01   ZONES-UTILISATEUR PICTURE X.                                CI0007
       LINKAGE SECTION.                                                 ADU002
      *                                                                 AMDU02
      ******************************************************************AMDU02
      **     SEGMENT THAT CONTAINS THE DESTINATION DATA FOR FORMATTING *AMDU02
      **     INTO DESTINATION INFORMATION LINES.                       *AMDU02
      ******************************************************************AMDU02
      *                                                                 AMDU02
      *!WF DSP=DI DSL=DU SEL=02 FOR=I LEV=1                             AMDU02
       01                 DI00.                                         CI0007
          05              DI00-SUITE.                                   CI0007
            15       FILLER         PICTURE  X(00800).                  CI0007
       01                 DI02  REDEFINES      DI00.                    CI0007
            10            DI02-CPAY1  PICTURE  X(2).                    CI0007
            10            DI02-ISHAD  PICTURE  X.                       CI0007
            10            DI02-CTTLN1 PICTURE  X(30).                   CI0007
            10            DI02-CTTLN2 PICTURE  X(30).                   CI0007
            10            DI02-CTTLN3 PICTURE  X(30).                   CI0007
            10            DI02-CL24.                                    CI0007
            11            DI02-GELL   PICTURE  9(4)                     CI0007
                          BINARY.                                       CI0007
            11            DI02-CL24K.                                   CI0007
            12            DI02-GECSQ  PICTURE  S9(3)                    CI0007
                          COMPUTATIONAL-3.                              CI0007
            11            DI02-GECSD  PICTURE  9(8).                    CI0007
            11            DI02-GECED  PICTURE  9(8).                    CI0007
            11            DI02-CREQ2  PICTURE  X.                       CI0007
            11            DI02-FILLER PICTURE  X(4).                    CI0007
            11            DI02-GECTA  PICTURE  X.                       CI0007
            11            DI02-GELCD  PICTURE  9(8).                    CI0007
            11            DI02-GEADS  PICTURE  9.                       CI0007
            11            DI02-GECIT  PICTURE  X(25).                   CI0007
            11            DI02-GECTRY PICTURE  X(20).                   CI0007
            11            DI02-GECTY  PICTURE  9(3).                    CI0007
            11            DI02-GEPCD  PICTURE  X(12).                   CI0007
            11            DI02-GEST   PICTURE  X(8).                    CI0007
            11            DI02-IRESA  PICTURE  X.                       CI0007
            11            DI02-FILLER PICTURE  X(8).                    CI0007
            11            DI02-GESAD  PICTURE  X(30)                    CI0007
                          OCCURS       003     TIMES.                   CI0007
            10            DI02-CLTYP  PICTURE  X.                       CI0007
            10            DI02-CLORN  PICTURE  X(45).                   CI0007
            10            DI02-C198.                                    CI0007
            11            DI02-CLNAM.                                   CI0007
            12            DI02-CLNAMH PICTURE  X(6).                    CI0007
            12            DI02-CLNAMF PICTURE  X(20).                   CI0007
            12            DI02-CLNAMM.                                  CI0007
            13            DI02-CLNAMI PICTURE  X.                       CI0007
            13            DI02-CLNAMR PICTURE  X(14).                   CI0007
            12            DI02-CLNAML PICTURE  X(25).                   CI0007
            12            DI02-CLNAMS PICTURE  X(4).                    CI0007
            10            DI02-TDELI  PICTURE  X(30).                   CI0007
            10            DI02-NPBN   PICTURE  X(20).                   CI0007
            10            DI02-NTR    PICTURE  9(8).                    CI0007
            10            DI02-GECKD1 PICTURE  9.                       CI0007
            10            DI02-CCBAT  PICTURE  99.                      CI0007
            10            DI02-C299.                                    CI0007
            11            DI02-CTID.                                    CI0007
            12            DI02-CTIDA  PICTURE  9(3).                    CI0007
            12            DI02-CTIDN.                                   CI0007
            13            DI02-CTIDNP PICTURE  X(13).                   CI0007
            13            DI02-CTIDND PICTURE  9(11).                   CI0007
            10            DI02-GECKD  PICTURE  9.                       CI0007
            10            DI02-PRCMN  PICTURE  X(20).                   CI0007
            10            DI02-MCSIG  PICTURE  X(30).                   CI0007
            10            DI02-MAPPN  PICTURE  X(10).                   CI0007
            10            DI02-FILLER PICTURE  X(240).                  CI0007
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 AMDU01
      ******************************************************************AMDU01
      **     SEGMENT THAT CONTAINS THE REFORMATTED DESTINATION         *AMDU01
      **     INFORMATION LINES(WHO, WHERE AND HOW).                    *AMDU01
      ******************************************************************AMDU01
      *                                                                 AMDU01
      *!WF DSP=DL DSL=DU SEL=01 FOR=I LEV=1                             AMDU01
       01                 DL00.                                         CI0007
          05              DL00-SUITE.                                   CI0007
            15       FILLER         PICTURE  X(01202).                  CI0007
       01                 DL01  REDEFINES      DL00.                    CI0007
            10            DL01-TWHOL1 PICTURE  X(100).                  CI0007
            10            DL01-TWHOL2 PICTURE  X(100).                  CI0007
            10            DL01-TWHOL3 PICTURE  X(100).                  CI0007
            10            DL01-TWHRL1 PICTURE  X(100).                  CI0007
            10            DL01-TWHRL2 PICTURE  X(100).                  CI0007
            10            DL01-TWHRL3 PICTURE  X(100).                  CI0007
            10            DL01-TWHRL4 PICTURE  X(100).                  CI0007
            10            DL01-TWHRL5 PICTURE  X(100).                  CI0007
            10            DL01-TWHRL6 PICTURE  X(100).                  CI0007
            10            DL01-TWHRL7 PICTURE  X(100).                  CI0007
            10            DL01-THOWL  PICTURE  X(100).                  CI0007
            10            DL01-CPAY1  PICTURE  X(2).                    CI0007
            10            DL01-FILLER PICTURE  X(100).                  CI0007
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 ADU002
      ******************************************************************ADU002
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU002
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU002
      ******************************************************************ADU002
      *                                                                 ADU002
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU002
       01                 MS00.                                         CI0007
          05              MS00-SUITE.                                   CI0007
            15       FILLER         PICTURE  X(00542).                  CI0007
       01                 MS03  REDEFINES      MS00.                    CI0007
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0007
                          COMPUTATIONAL-3.                              CI0007
            10            MS03-CMSSF  PICTURE  XX.                      CI0007
            10            MS03-DU09.                                    CI0007
            11            MS03-CMESA  PICTURE  S9(9)                    CI0007
                          BINARY.                                       CI0007
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0007
                          BINARY.                                       CI0007
            11            MS03-CMESB  PICTURE  S9(9)                    CI0007
                          BINARY.                                       CI0007
            11            MS03-CMSST  PICTURE  S9(9)                    CI0007
                          BINARY.                                       CI0007
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0007
                          BINARY.                                       CI0007
            11            MS03-QELLAA PICTURE  S9(9)                    CI0007
                          BINARY.                                       CI0007
            11            MS03-TMESS4 PICTURE  X(512).                  CI0007
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DI02
                                DL01
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0007
      *               *                                   *             CI0007
      *               *INITIALISATIONS                    *             CI0007
      *               *                                   *             CI0007
      *               *************************************.            CI0007
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.            ADU002
      *               *                                   *             ADU002
      *               *MODULE INITIALIZATIONS             *             ADU002
      *               *                                   *             ADU002
      *               *************************************.            ADU002
       F02.                                                             lv05
      *                                                                 ADU002
      *********************************                                 ADU002
      ** MODULE INITIALIZATIONS       *                                 ADU002
      **  - INITIALIZE FT SWITCH      *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
      *N02BA.    NOTE *INITIALIZE FT SWITCH               *.            ADU002
       F02BA.                                                           lv10
      *                                                                 ADU002
      *********************************                                 ADU002
      ** MOVE ZEROS TO FT SWITCH TO   *                                 ADU002
      ** MAKE SURE THAT FUNCTION 20   *                                 ADU002
      ** GETS EXECUTED AT END OF      *                                 ADU002
      ** PROCESSING.                  *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
           MOVE ALL    ZEROS TO FT.                                     ADU002
       F02BA-FN. EXIT.
      *N02CA.    NOTE *Default App name if not EZ-Trans   *.
       F02CA.    IF    DI02-MAPPN NOT = 'SD'                            lv10
                 AND   DI02-MAPPN NOT = 'UD'
                 NEXT SENTENCE ELSE GO TO     F02CA-FN.
           MOVE        SPACES TO DI02-MAPPN.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0007
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0007
      *               *                                   *             CI0007
      *               *FIN DE TRAITEMENT                  *             CI0007
      *               *                                   *             CI0007
      *               *************************************.            CI0007
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0007
      *N2099.    NOTE *GO BACK TO CALLING MODULE          *.            ADU002
       F2099.                                                           lv10
      *                                                                 ADU002
      *********************************                                 ADU002
      ** GOBACK TO CALLING MODULE     *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
           GOBACK.                                                      ADU002
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.            AMSKDL
      *               *                                   *             AMSKDL
      *               *DEST IS CHECK TO OWNER             *             AMSKDL
      *               *                                   *             AMSKDL
      *               *************************************.            AMSKDL
       F40.      IF    DI02-CPAY1 = 'O '                                lv05
                 NEXT SENTENCE ELSE GO TO     F40-FN.                   AMSKDL
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DESTINATION IS CHECK TO OWNER*
      **  - WHO LINES CONTAIN ACCOUNT *
      **    OWNERSHIP                 *
      **  - WHERE LINES CONTAIN       *
      **    ACCOUNT'S HOME ADDRESS    *
      **  - HOW CONTAINS CHECK        *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N40BA.    NOTE *FORMAT   DELIVERY METHOD           *.            AMSKDL
       F40BA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DELIVERY METHOD IS           *                                 AMSKDL
      ** CHECK                        *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        'CHECK         ' TO                              AMSKDL
           DL01-THOWL.                                                  AMSKDL
       F40BA-FN. EXIT.
      *N40EA.    NOTE *FORMAT   WHO LINES                 *.            AMSKDL
       F40EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N40FA.    NOTE *CHECK IF CUSTODIAL ACCOUNT         *.            AMCOWL
       F40FA.    IF    DI02-CTTLN1 =                                    lv15
                       'IDS TRUST COMPANY'                              AMCOWL
                 OR    'ADVISORY BANK & TRUST'                          AMCOWL
                 OR    'IDS BANK & TRUST'                               AMCOWL
                 OR    WS01-AMEX-TRUST
                 OR    WS01-AMPF-TRUST
                 NEXT SENTENCE ELSE GO TO     F40FA-FN.
      *********************************                                 AMCOWL
      ** CHECK THE FIRST OWNERSHIP    *                                 AMCOWL
      **                              *
      **                              *
      ** LINE TO SEE IF IT IS A       *                                 AMCOWL
      ** CUSTODIAL ACCOUNT.           *                                 AMCOWL
      *********************************                                 AMCOWL
      *                                                                 AMCOWL
      *N40FM.    NOTE *REFORMAT OWNERSHIP LINES           *.            AMCOWL
       F40FM.                                                           lv20
      *                                                                 AMCOWL
      *********************************                                 AMCOWL
      ** STRIP OFF 'C/O' FROM THE     *                                 AMCOWL
      ** SECOND LINE AND MOVE TO FIRST*                                 AMCOWL
      ** LINE.  MOVE THIRD LINE TO    *                                 AMCOWL
      ** SECOND LINE.                 *                                 AMCOWL
      *********************************                                 AMCOWL
      *                                                                 AMCOWL
           MOVE        SPACES TO DI02-CTTLN1                            AMCOWL
           MOVE        5 TO W-WORK-1PTR                                 AMCOWL
           UNSTRING        DI02-CTTLN2                                  AMCOWL
              INTO         DI02-CTTLN1                                  AMCOWL
              WITH POINTER W-WORK-1PTR                                  AMCOWL
           MOVE        DI02-CTTLN3 TO DI02-CTTLN2                       AMCOWL
           MOVE        SPACES TO DI02-CTTLN3.                           AMCOWL
       F40FM-FN. EXIT.
       F40FA-FN. EXIT.
      *N40GA.    NOTE *MOVE ACCOUNT OWNERSHIP LINES       *.            AMOWNL
       F40GA.                                                           lv15
      *                                                                 AMOWNL
      *********************************                                 AMOWNL
      ** MOVE THE ACCOUNT OWNERSHIP   *                                 AMOWNL
      ** LINES INTO THE WHO LINES     *                                 AMOWNL
      *********************************                                 AMOWNL
      *                                                                 AMOWNL
           MOVE        DI02-CTTLN1 TO DL01-TWHOL1                       AMOWNL
           MOVE        DI02-CTTLN2 TO DL01-TWHOL2                       AMOWNL
           MOVE        DI02-CTTLN3 TO DL01-TWHOL3.                      AMOWNL
       F40GA-FN. EXIT.
       F40EA-FN. EXIT.
      *N40MA.    NOTE *FORMAT   WHERE LINES               *.            AMSKDL
       F40MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N40NA.    NOTE *INITIALIZE FOR ADDR FORMATTING     *.            AMADRL
       F40NA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** INITIALIZE FOR FORMATTING    *                                 AMADRL
      ** THE ADDRESS FROM DI02 SEGMENT*                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        DI02-GECIT TO 7-ADDR-GECIT                       AMADRL
           MOVE        DI02-GEST TO 7-ADDR-GEST                         AMADRL
           MOVE        DI02-GEPCD TO 7-ADDR-GEPCD                       AMADRL
           MOVE        DI02-GECTRY TO 7-ADDR-GECTRY                     AMADRL
           MOVE        DI02-GESAD (1) TO 7-ADDR-GESAD (1)               AMADRL
           MOVE        DI02-GESAD (2) TO 7-ADDR-GESAD (2)               AMADRL
           MOVE        DI02-GESAD (3) TO 7-ADDR-GESAD (3).              AMADRL
       F40NA-FN. EXIT.
      *N40OA.    NOTE *FORMAT DI02 SEGMENT INTO ADDRESS   *.            AMADRL
       F40OA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** FORMAT THE CL24 SEGMENT INTO *                                 AMADRL
      ** THE ADDRESS.                 *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           PERFORM     F96 THRU F96-FN.                                 AMADRL
       F40OA-FN. EXIT.
      *N40PA.    NOTE *MOVE FORMATTED ADDR LINES          *.            AMADRL
       F40PA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** MOVE THE FORMATTED ADDRESS   *                                 AMADRL
      ** LINES TO THE WHERE LINES     *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        7-ADDR-GESAD (1) TO DL01-TWHRL1                  AMADRL
           MOVE        7-ADDR-GESAD (2) TO DL01-TWHRL2                  AMADRL
           MOVE        7-ADDR-GESAD (3) TO DL01-TWHRL3                  AMADRL
           MOVE        7-ADDR-GESAD (4) TO DL01-TWHRL4.                 AMADRL
       F40PA-FN. EXIT.
       F40MA-FN. EXIT.
      *N40YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F40YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DI02-CPAY1 TO DL01-CPAY1.                        AMSKDL
       F40YA-FN. EXIT.
      *N40ZA.    NOTE *RETURN TO CALLING MODULE           *.            AMSKDL
       F40ZA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** RETURN TO CALLING MODULE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE                     ALL '1' TO FT GO TO F20.            AMSKDL
       F40ZA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.            AMSKDL
      *               *                                   *             AMSKDL
      *               *DEST IS CHECK TO OWNER AT ALT AD   *             AMSKDL
      *               *                                   *             AMSKDL
      *               *************************************.            AMSKDL
       F45.      IF    DI02-CPAY1 = 'OA'                                lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.                   AMSKDL
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DESTINATION IS CHECK TO OWNER*
      ** AT ALTERNATE ADDRESS         *
      **  - WHO LINES CONTAIN ACCOUNT *
      **    OWNERSHIP                 *
      **  - WHERE LINES CONTAIN       *
      **    SELECTED ADDRESS          *
      **  - HOW CONTAINS CHECK        *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N45BA.    NOTE *FORMAT   DELIVERY METHOD           *.            AMSKDL
       F45BA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DELIVERY METHOD IS           *                                 AMSKDL
      ** CHECK                        *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        'CHECK         ' TO                              AMSKDL
           DL01-THOWL.                                                  AMSKDL
       F45BA-FN. EXIT.
      *N45EA.    NOTE *FORMAT   WHO LINES                 *.            AMSKDL
       F45EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N45ED.    NOTE *Calling program is CATS EZ-Trans   *.
       F45ED.    IF    DI02-MAPPN = 'SD'                                lv15
                 OR    DI02-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F45ED-FN.
      *N45EE.    NOTE *** CHECK IF CUSTODIAL ACCOUNT **   *.
       F45EE.    IF    DI02-CTTLN1 =                                    lv20
                       'IDS TRUST COMPANY'
                 OR    'ADVISORY BANK & TRUST'
                 OR    'IDS BANK & TRUST'
                 OR    WS01-AMEX-TRUST
                 OR    WS01-AMPF-TRUST
                 NEXT SENTENCE ELSE GO TO     F45EE-FN.
      *********************************
      ** CHECK THE FIRST OWNERSHIP    *
      **                              *
      **                              *
      ** LINE TO SEE IF IT IS A       *
      ** CUSTODIAL ACCOUNT.           *
      *********************************
      *
      *N45EF.    NOTE **** REFORMAT OWNERSHIP LINES ***   *.
       F45EF.                                                           lv25
      *
      *********************************
      ** STRIP OFF 'C/O' FROM THE     *
      ** SECOND LINE AND MOVE TO FIRST*
      ** LINE.  MOVE THIRD LINE TO    *
      ** SECOND LINE.                 *
      *********************************
      *
           MOVE        SPACES TO DI02-CTTLN1
           MOVE        5 TO W-WORK-1PTR
           UNSTRING        DI02-CTTLN2
              INTO         DI02-CTTLN1
              WITH POINTER W-WORK-1PTR
           MOVE        DI02-CTTLN3 TO DI02-CTTLN2
           MOVE        SPACES TO DI02-CTTLN3.
       F45EF-FN. EXIT.
       F45EE-FN. EXIT.
       F45ED-FN. EXIT.
      *N45FA.    NOTE *MOVE ACCOUNT OWNERSHIP LINES       *.            AMOWNL
       F45FA.                                                           lv15
      *                                                                 AMOWNL
      *********************************                                 AMOWNL
      ** MOVE THE ACCOUNT OWNERSHIP   *                                 AMOWNL
      ** LINES INTO THE WHO LINES     *                                 AMOWNL
      *********************************                                 AMOWNL
      *                                                                 AMOWNL
           MOVE        DI02-CTTLN1 TO DL01-TWHOL1                       AMOWNL
           MOVE        DI02-CTTLN2 TO DL01-TWHOL2                       AMOWNL
           MOVE        DI02-CTTLN3 TO DL01-TWHOL3.                      AMOWNL
       F45FA-FN. EXIT.
       F45EA-FN. EXIT.
      *N45MA.    NOTE *FORMAT   WHERE LINES               *.            AMSKDL
       F45MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N45NA.    NOTE *INITIALIZE FOR ADDR FORMATTING     *.            AMADRL
       F45NA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** INITIALIZE FOR FORMATTING    *                                 AMADRL
      ** THE ADDRESS FROM DI02 SEGMENT*                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        DI02-GECIT TO 7-ADDR-GECIT                       AMADRL
           MOVE        DI02-GEST TO 7-ADDR-GEST                         AMADRL
           MOVE        DI02-GEPCD TO 7-ADDR-GEPCD                       AMADRL
           MOVE        DI02-GECTRY TO 7-ADDR-GECTRY                     AMADRL
           MOVE        DI02-GESAD (1) TO 7-ADDR-GESAD (1)               AMADRL
           MOVE        DI02-GESAD (2) TO 7-ADDR-GESAD (2)               AMADRL
           MOVE        DI02-GESAD (3) TO 7-ADDR-GESAD (3).              AMADRL
       F45NA-FN. EXIT.
      *N45OA.    NOTE *FORMAT DI02 SEGMENT INTO ADDRESS   *.            AMADRL
       F45OA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** FORMAT THE CL24 SEGMENT INTO *                                 AMADRL
      ** THE ADDRESS.                 *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           PERFORM     F96 THRU F96-FN.                                 AMADRL
       F45OA-FN. EXIT.
      *N45PA.    NOTE *MOVE FORMATTED ADDR LINES          *.            AMADRL
       F45PA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** MOVE THE FORMATTED ADDRESS   *                                 AMADRL
      ** LINES TO THE WHERE LINES     *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        7-ADDR-GESAD (1) TO DL01-TWHRL1                  AMADRL
           MOVE        7-ADDR-GESAD (2) TO DL01-TWHRL2                  AMADRL
           MOVE        7-ADDR-GESAD (3) TO DL01-TWHRL3                  AMADRL
           MOVE        7-ADDR-GESAD (4) TO DL01-TWHRL4.                 AMADRL
       F45PA-FN. EXIT.
       F45MA-FN. EXIT.
      *N45YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F45YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DI02-CPAY1 TO DL01-CPAY1.                        AMSKDL
       F45YA-FN. EXIT.
      *N45ZA.    NOTE *RETURN TO CALLING MODULE           *.            AMSKDL
       F45ZA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** RETURN TO CALLING MODULE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE                     ALL '1' TO FT GO TO F20.            AMSKDL
       F45ZA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.            AMSKDL
      *               *                                   *             AMSKDL
      *               *DEST IS SPECIAL PAYEE              *             AMSKDL
      *               *                                   *             AMSKDL
      *               *************************************.            AMSKDL
       F50.      IF    DI02-CPAY1 = 'S '                                lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.                   AMSKDL
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DESTINATION IS SPECIAL PAYEE *
      **  - WHO LINES CLIENT'S NAME   *
      **    AND MEMO TEXT LINE        *
      **  - WHERE LINES CONTAIN       *
      **    SELECTED ADDRESS          *
      **  - HOW CONTAINS CHECK        *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N50BA.    NOTE *FORMAT   DELIVERY METHOD           *.            AMSKDL
       F50BA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DELIVERY METHOD IS           *                                 AMSKDL
      ** CHECK                        *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        'CHECK         ' TO                              AMSKDL
           DL01-THOWL.                                                  AMSKDL
       F50BA-FN. EXIT.
      *N50EA.    NOTE *FORMAT   WHO LINES                 *.            AMSKDL
       F50EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHO LINES       *                                 AMSKDL
      ** IF THE CLIENT IS A PERSON,   *
      ** FORMAT CLIENT'S NAME AND MOVE*
      ** TO WHO LINE 1.  IF THE CLIENT*
      ** IF THE CLIENT IS AN          *
      ** ORGANIZATION, MOVE THE       *
      ** CLIENT'S NAME TO WHO LINE 1  *
      ** MOVE THE MEMO TEXT LINE TO   *
      ** WHO LINE 2                   *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N50FA.    NOTE *CHECK IF CLIENT IS A PERSON        *.
       F50FA.    IF    DI02-CLTYP = 'P'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *
      *********************************
      ** CLIENT IS A PERSON           *
      *********************************
      *
      *N50GA.    NOTE *FORMAT CLIENT'S NAME               *.
       F50GA.                                                           lv20
      *
      *********************************
      ** PERFORM TO FORMAT CLIENT'S   *
      ** NAME.  THE NAME WILL BE PUT  *
      ** INTO DL01-TWHOL1             *
      *********************************
      *
           PERFORM     F97 THRU F97-FN.
       F50GA-FN. EXIT.
      *N50HA.    NOTE *CHECK IF ERROR IN FORMATTING       *.
       F50HA.    IF    7-NA10-RETURN = 'E'                              lv20
                 NEXT SENTENCE ELSE GO TO     F50HA-FN.
      *
      *********************************
      ** IF THERE WAS AN ERROR IN     *
      ** FORMATTING THE NAME, MOVE    *
      ** 'UNKNOWN' TO WHO LINE 1.     *
      *********************************
      *
           MOVE        'UNKNOWN' TO DL01-TWHOL1.
       F50HA-FN. EXIT.
       F50FA-900. GO TO F50IA-FN.
       F50FA-FN. EXIT.
      *N50IA.    NOTE *CLIENT IS AN ORGANIZATION          *.
       F50IA.                                                           lv15
      *
      *N50JA.    NOTE *MOVE THE ORGANIZATION NAME         *.
       F50JA.                                                           lv20
      *TO WHO LINE 1
      *
           MOVE        DI02-CLORN TO DL01-TWHOL1.
       F50JA-FN. EXIT.
       F50IA-FN. EXIT.
      *N50KA.    NOTE *MOVE MEMO TEXT LINE                *.
       F50KA.                                                           lv15
      *TO WHO LINE 2
      *
           MOVE        DI02-TDELI TO DL01-TWHOL2.
       F50KA-FN. EXIT.
       F50EA-FN. EXIT.
      *N50MA.    NOTE *FORMAT   WHERE LINES               *.            AMSKDL
       F50MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N50NA.    NOTE *INITIALIZE FOR ADDR FORMATTING     *.            AMADRL
       F50NA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** INITIALIZE FOR FORMATTING    *                                 AMADRL
      ** THE ADDRESS FROM DI02 SEGMENT*                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        DI02-GECIT TO 7-ADDR-GECIT                       AMADRL
           MOVE        DI02-GEST TO 7-ADDR-GEST                         AMADRL
           MOVE        DI02-GEPCD TO 7-ADDR-GEPCD                       AMADRL
           MOVE        DI02-GECTRY TO 7-ADDR-GECTRY                     AMADRL
           MOVE        DI02-GESAD (1) TO 7-ADDR-GESAD (1)               AMADRL
           MOVE        DI02-GESAD (2) TO 7-ADDR-GESAD (2)               AMADRL
           MOVE        DI02-GESAD (3) TO 7-ADDR-GESAD (3).              AMADRL
       F50NA-FN. EXIT.
      *N50OA.    NOTE *FORMAT DI02 SEGMENT INTO ADDRESS   *.            AMADRL
       F50OA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** FORMAT THE CL24 SEGMENT INTO *                                 AMADRL
      ** THE ADDRESS.                 *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           PERFORM     F96 THRU F96-FN.                                 AMADRL
       F50OA-FN. EXIT.
      *N50PA.    NOTE *MOVE FORMATTED ADDR LINES          *.            AMADRL
       F50PA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** MOVE THE FORMATTED ADDRESS   *                                 AMADRL
      ** LINES TO THE WHERE LINES     *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        7-ADDR-GESAD (1) TO DL01-TWHRL1                  AMADRL
           MOVE        7-ADDR-GESAD (2) TO DL01-TWHRL2                  AMADRL
           MOVE        7-ADDR-GESAD (3) TO DL01-TWHRL3                  AMADRL
           MOVE        7-ADDR-GESAD (4) TO DL01-TWHRL4.                 AMADRL
       F50PA-FN. EXIT.
       F50MA-FN. EXIT.
      *N50YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F50YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DI02-CPAY1 TO DL01-CPAY1.                        AMSKDL
       F50YA-FN. EXIT.
      *N50ZA.    NOTE *RETURN TO CALLING MODULE           *.            AMSKDL
       F50ZA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** RETURN TO CALLING MODULE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE                     ALL '1' TO FT GO TO F20.            AMSKDL
       F50ZA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.            AMSKDL
      *               *                                   *             AMSKDL
      *               *DEST IS CHECK TO BANK              *             AMSKDL
      *               *                                   *             AMSKDL
      *               *************************************.            AMSKDL
       F55.      IF    DI02-CPAY1 = 'B '                                lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.                   AMSKDL
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DESTINATION IS CHECK TO BANK *
      **  - WHO LINES BANK'S NAME     *
      **    AND BANK ACCT NUMBER      *
      **  - WHERE LINES CONTAIN       *
      **    SELECTED ADDRESS          *
      **  - HOW CONTAINS CHECK        *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N55BA.    NOTE *FORMAT   DELIVERY METHOD           *.            AMSKDL
       F55BA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DELIVERY METHOD IS           *                                 AMSKDL
      ** CHECK                        *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        'CHECK         ' TO                              AMSKDL
           DL01-THOWL.                                                  AMSKDL
       F55BA-FN. EXIT.
      *N55EA.    NOTE *FORMAT   WHO LINES                 *.            AMSKDL
       F55EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N55FA.    NOTE *MOVE THE BANK'S NAME TO            *.
       F55FA.                                                           lv15
      *WHO LINE 1
      *
           MOVE        DI02-CLORN TO DL01-TWHOL1.
       F55FA-FN. EXIT.
      *N55GA.    NOTE *FORMAT CLIENT SIGNATURE FIELD      *.
       F55GA.                                                           lv15
      *TO WHO LINE 2
      *
           MOVE        'A/O:' TO DL01-TWHOL2
           MOVE        6 TO W-WORK-1PTR
           STRING DI02-MCSIG
                  DELIMITED BY SIZE
                  INTO DL01-TWHOL2
                  WITH POINTER W-WORK-1PTR.
       F55GA-FN. EXIT.
      *N55HA.    NOTE *CHECKING ACCOUNT                   *.
       F55HA.    IF    DI02-CCBAT = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55HA-FN.
      *DISPLAY 'CHECKING' ON WHO #3
      *
           MOVE        'CHECKING' TO DL01-TWHOL3
           MOVE        10 TO W-WORK-1PTR.
       F55HA-FN. EXIT.
      *N55IA.    NOTE *SAVINGS ACCOUNT                    *.
       F55IA.    IF    DI02-CCBAT = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55IA-FN.
      *DISPLAY 'SAVINGS' ON WHO #3
      *
           MOVE        'SAVINGS' TO DL01-TWHOL3
           MOVE        9 TO W-WORK-1PTR.
       F55IA-FN. EXIT.
      *N55JA.    NOTE *FORMAT PBN LINE                    *.
       F55JA.         EXIT.                                             lv15
      *N55JC.    NOTE *EZ TRANS MADE THE CALL...          *.
       F55JC.    IF    DI02-MAPPN = 'SD'                                lv20
                 OR    DI02-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F55JC-FN.
      *FORMAT LINE TO INCLUDE # AND
      *BANK ACCOUNT NUMBER
           STRING '# '
                  DELIMITED BY SIZE
                  DI02-NPBN
                  DELIMITED BY SIZE
                  INTO DL01-TWHOL3
                  WITH POINTER W-WORK-1PTR.
       F55JC-900. GO TO F55JH-FN.
       F55JC-FN. EXIT.
      *N55JH.    NOTE *FOR ALL OTHER APPLICATIONS...      *.
       F55JH.                                                           lv20
      *FORMAT LINE TO INCLUDE LITERAL
      *'ACCOUNT' AND BANK ACCOUNT NMBR
           STRING 'ACCOUNT: '
                  DELIMITED BY SIZE
                  DI02-NPBN
                  DELIMITED BY SIZE
                  INTO DL01-TWHOL3
                  WITH POINTER W-WORK-1PTR.
       F55JH-FN. EXIT.
       F55JA-FN. EXIT.
       F55EA-FN. EXIT.
      *N55MA.    NOTE *FORMAT   WHERE LINES               *.            AMSKDL
       F55MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N55NA.    NOTE *INITIALIZE FOR ADDR FORMATTING     *.            AMADRL
       F55NA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** INITIALIZE FOR FORMATTING    *                                 AMADRL
      ** THE ADDRESS FROM DI02 SEGMENT*                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        DI02-GECIT TO 7-ADDR-GECIT                       AMADRL
           MOVE        DI02-GEST TO 7-ADDR-GEST                         AMADRL
           MOVE        DI02-GEPCD TO 7-ADDR-GEPCD                       AMADRL
           MOVE        DI02-GECTRY TO 7-ADDR-GECTRY                     AMADRL
           MOVE        DI02-GESAD (1) TO 7-ADDR-GESAD (1)               AMADRL
           MOVE        DI02-GESAD (2) TO 7-ADDR-GESAD (2)               AMADRL
           MOVE        DI02-GESAD (3) TO 7-ADDR-GESAD (3).              AMADRL
       F55NA-FN. EXIT.
      *N55OA.    NOTE *FORMAT DI02 SEGMENT INTO ADDRESS   *.            AMADRL
       F55OA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** FORMAT THE CL24 SEGMENT INTO *                                 AMADRL
      ** THE ADDRESS.                 *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           PERFORM     F96 THRU F96-FN.                                 AMADRL
       F55OA-FN. EXIT.
      *N55PA.    NOTE *MOVE FORMATTED ADDR LINES          *.            AMADRL
       F55PA.                                                           lv15
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** MOVE THE FORMATTED ADDRESS   *                                 AMADRL
      ** LINES TO THE WHERE LINES     *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        7-ADDR-GESAD (1) TO DL01-TWHRL1                  AMADRL
           MOVE        7-ADDR-GESAD (2) TO DL01-TWHRL2                  AMADRL
           MOVE        7-ADDR-GESAD (3) TO DL01-TWHRL3                  AMADRL
           MOVE        7-ADDR-GESAD (4) TO DL01-TWHRL4.                 AMADRL
       F55PA-FN. EXIT.
       F55MA-FN. EXIT.
      *N55YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F55YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DI02-CPAY1 TO DL01-CPAY1.                        AMSKDL
       F55YA-FN. EXIT.
      *N55ZA.    NOTE *RETURN TO CALLING MODULE           *.            AMSKDL
       F55ZA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** RETURN TO CALLING MODULE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE                     ALL '1' TO FT GO TO F20.            AMSKDL
       F55ZA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.            AMSKDL
      *               *                                   *             AMSKDL
      *               *DEST IS DIRECT DEPOSIT             *             AMSKDL
      *               *                                   *             AMSKDL
      *               *************************************.            AMSKDL
       F60.      IF    DI02-CPAY1 = 'D '                                lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.                   AMSKDL
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DESTINATION IS DIRECT DEPOSIT*
      **  - WHO LINES CLIENT          *
      **    SIGNATURE FIELD           *
      **  - WHERE LINES CONTAIN       *
      **    BANK'S NAME, RTN LINE AND *
      **    PBN LINE                  *
      **  - HOW CONTAINS DIRECT       *
      **    DEPOSIT                   *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N60BA.    NOTE *FORMAT   DELIVERY METHOD           *.            AMSKDL
       F60BA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DELIVERY METHOD IS           *                                 AMSKDL
      ** DIRECT DEPOSIT               *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        'DIRECT DEPOSIT' TO                              AMSKDL
           DL01-THOWL.                                                  AMSKDL
       F60BA-FN. EXIT.
      *N60EA.    NOTE *FORMAT   WHO LINES                 *.            AMSKDL
       F60EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N60FA.    NOTE *MOVE CLIENT SIGNATURE FIELD        *.
       F60FA.                                                           lv15
      *
           MOVE        DI02-MCSIG TO DL01-TWHOL1.
       F60FA-FN. EXIT.
       F60EA-FN. EXIT.
      *N60MA.    NOTE *FORMAT   WHERE LINES               *.            AMSKDL
       F60MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHERE LINES     *                                 AMSKDL
      ** FROM THE BANK'S NAME, RTN    *
      ** AND PBN.                     *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N60NA.    NOTE *MOVE THE BANK'S NAME TO            *.
       F60NA.                                                           lv15
      *WHERE LINE 1
      *
           MOVE        DI02-CLORN TO DL01-TWHRL1.
       F60NA-FN. EXIT.
      *N60OA.    NOTE *BANK ADDRESS TO BE SHOWN           *.
       F60OA.    IF    DI02-ISHAD = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60OA-FN.
      *REFORMAT FOR DISPLAY
      *
      *N60PA.    NOTE *INITIALIZE FOR ADDR FORMATTING     *.            AMADRL
       F60PA.                                                           lv20
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** INITIALIZE FOR FORMATTING    *                                 AMADRL
      ** THE ADDRESS FROM DI02 SEGMENT*                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        DI02-GECIT TO 7-ADDR-GECIT                       AMADRL
           MOVE        DI02-GEST TO 7-ADDR-GEST                         AMADRL
           MOVE        DI02-GEPCD TO 7-ADDR-GEPCD                       AMADRL
           MOVE        DI02-GECTRY TO 7-ADDR-GECTRY                     AMADRL
           MOVE        DI02-GESAD (1) TO 7-ADDR-GESAD (1)               AMADRL
           MOVE        DI02-GESAD (2) TO 7-ADDR-GESAD (2)               AMADRL
           MOVE        DI02-GESAD (3) TO 7-ADDR-GESAD (3).              AMADRL
       F60PA-FN. EXIT.
      *N60QA.    NOTE *FORMAT DI02 SEGMENT INTO ADDRESS   *.            AMADRL
       F60QA.                                                           lv20
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** FORMAT THE CL24 SEGMENT INTO *                                 AMADRL
      ** THE ADDRESS.                 *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           PERFORM     F96 THRU F96-FN.                                 AMADRL
       F60QA-FN. EXIT.
      *N60RA.    NOTE *MOVE FORMATTED ADDR LINES          *.            AMADRL
       F60RA.                                                           lv20
      *                                                                 AMADRL
      *********************************                                 AMADRL
      ** MOVE THE FORMATTED ADDRESS   *                                 AMADRL
      ** LINES TO THE WHERE LINES     *                                 AMADRL
      *********************************                                 AMADRL
      *                                                                 AMADRL
           MOVE        7-ADDR-GESAD (1) TO DL01-TWHRL2                  AMADRL
           MOVE        7-ADDR-GESAD (2) TO DL01-TWHRL3                  AMADRL
           MOVE        7-ADDR-GESAD (3) TO DL01-TWHRL4                  AMADRL
           MOVE        7-ADDR-GESAD (4) TO DL01-TWHRL5.                 AMADRL
       F60RA-FN. EXIT.
       F60OA-FN. EXIT.
      *N60SA.    NOTE *FORMAT RTN AND TRANSIT NUMBER      *.
       F60SA.                                                           lv15
      *TO WHERE LINE 2
      *
           MOVE        'RTN: ' TO DL01-TWHRL6
           MOVE        6 TO W-WORK-1PTR
           STRING DI02-NTR
                  DELIMITED BY SIZE
                  INTO DL01-TWHRL6
                  WITH POINTER W-WORK-1PTR
           MOVE        15 TO W-WORK-1PTR
           STRING DI02-GECKD1
                  DELIMITED BY SIZE
                  INTO DL01-TWHRL6
                  WITH POINTER W-WORK-1PTR.
       F60SA-FN. EXIT.
      *N60TA.    NOTE *CHECKING ACCOUNT                   *.
       F60TA.    IF    DI02-CCBAT = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F60TA-FN.
      *DISPLAY 'CHECKING' WHERE LINE 7
      *
           MOVE        'CHECKING' TO DL01-TWHRL7
           MOVE        10 TO W-WORK-1PTR.
       F60TA-FN. EXIT.
      *N60UA.    NOTE *SAVINGS ACCOUNT                    *.
       F60UA.    IF    DI02-CCBAT = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F60UA-FN.
      *DISPLAY 'SAVINGS' WHERE LINE 7
      *
           MOVE        'SAVINGS' TO DL01-TWHRL7
           MOVE        9 TO W-WORK-1PTR.
       F60UA-FN. EXIT.
      *N60VA.    NOTE *FORMAT PBN LINE                    *.
       F60VA.         EXIT.                                             lv15
      *N60VC.    NOTE *EZ-TRANS MADE THE CALL...          *.
       F60VC.    IF    DI02-MAPPN = 'SD'                                lv20
                 OR    DI02-MAPPN = 'UD'
                 NEXT SENTENCE ELSE GO TO     F60VC-FN.
      *FORMAT WHERE LINE 7 TO INCLUDE
      *# AND BANK ACCOUNT NUMBER
           STRING '# '
                  DELIMITED BY SIZE
                  DI02-NPBN
                  DELIMITED BY SIZE
                  INTO DL01-TWHRL7
                  WITH POINTER W-WORK-1PTR.
       F60VC-900. GO TO F60VH-FN.
       F60VC-FN. EXIT.
      *N60VH.    NOTE *ALL OTHER APPLICATIONS...          *.
       F60VH.                                                           lv20
      *FORMAT WHERE LINE 7 TO INCLUDE
      *'ACCOUNT:' AND BANK ACC NBR
           STRING 'ACCOUNT: '
                  DELIMITED BY SIZE
                  DI02-NPBN
                  DELIMITED BY SIZE
                  INTO DL01-TWHRL7
                  WITH POINTER W-WORK-1PTR.
       F60VH-FN. EXIT.
       F60VA-FN. EXIT.
      *N60WA.    NOTE *FILL IN LINES WITH SPACES          *.
       F60WA.                                                           lv15
      *THAT HAVE THE RTN AND PBN LINES
      *
                 IF    DL01-TWHRL2 = SPACES                             DOT
           MOVE        DL01-TWHRL6 TO DL01-TWHRL2
           MOVE        DL01-TWHRL7 TO DL01-TWHRL3
           MOVE        SPACES TO DL01-TWHRL6
           DL01-TWHRL7
               GO TO     F60WA-FN.
                 IF    DL01-TWHRL3 = SPACES                             DOT
           MOVE        DL01-TWHRL6 TO DL01-TWHRL3
           MOVE        DL01-TWHRL7 TO DL01-TWHRL4
           MOVE        SPACES TO DL01-TWHRL6
           DL01-TWHRL7
               GO TO     F60WA-FN.
                 IF    DL01-TWHRL4 = SPACES                             DOT
           MOVE        DL01-TWHRL6 TO DL01-TWHRL4
           MOVE        DL01-TWHRL7 TO DL01-TWHRL5
           MOVE        SPACES TO DL01-TWHRL6
           DL01-TWHRL7
               GO TO     F60WA-FN.
                 IF    DL01-TWHRL5 = SPACES                             DOT
           MOVE        DL01-TWHRL6 TO DL01-TWHRL5
           MOVE        DL01-TWHRL7 TO DL01-TWHRL6
           MOVE        SPACES TO DL01-TWHRL7.
       F60WA-FN. EXIT.
       F60MA-FN. EXIT.
      *N60YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F60YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DI02-CPAY1 TO DL01-CPAY1.                        AMSKDL
       F60YA-FN. EXIT.
      *N60ZA.    NOTE *RETURN TO CALLING MODULE           *.            AMSKDL
       F60ZA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** RETURN TO CALLING MODULE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE                     ALL '1' TO FT GO TO F20.            AMSKDL
       F60ZA-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.            AMSKDL
      *               *                                   *             AMSKDL
      *               *DEST IS TRANSFER                   *             AMSKDL
      *               *                                   *             AMSKDL
      *               *************************************.            AMSKDL
       F65.      IF    DI02-CPAY1 = 'TR'                                lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.                   AMSKDL
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DESTINATION IS TRANSFER      *
      **  - WHO LINES CONTAIN ACCOUNT *
      **    OWNERSHIP                 *
      **  - WHERE LINES TO ACCOUNT    *
      **    NUMBER AND PRODUCT NAME   *
      **  - HOW CONTAINS TRANSFER     *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N65BA.    NOTE *FORMAT   DELIVERY METHOD           *.            AMSKDL
       F65BA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** DELIVERY METHOD IS           *                                 AMSKDL
      ** TRANSFER                     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        'TRANSFER      ' TO                              AMSKDL
           DL01-THOWL.                                                  AMSKDL
       F65BA-FN. EXIT.
      *N65EA.    NOTE *FORMAT   WHO LINES                 *.            AMSKDL
       F65EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N65FA.    NOTE *MOVE ACCOUNT OWNERSHIP LINES       *.            AMOWNL
       F65FA.                                                           lv15
      *                                                                 AMOWNL
      *********************************                                 AMOWNL
      ** MOVE THE ACCOUNT OWNERSHIP   *                                 AMOWNL
      ** LINES INTO THE WHO LINES     *                                 AMOWNL
      *********************************                                 AMOWNL
      *                                                                 AMOWNL
           MOVE        DI02-CTTLN1 TO DL01-TWHOL1                       AMOWNL
           MOVE        DI02-CTTLN2 TO DL01-TWHOL2                       AMOWNL
           MOVE        DI02-CTTLN3 TO DL01-TWHOL3.                      AMOWNL
       F65FA-FN. EXIT.
       F65EA-FN. EXIT.
      *N65MA.    NOTE *FORMAT   WHERE LINES               *.            AMSKDL
       F65MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** FORMAT   THE WHERE LINES     *                                 AMSKDL
      ** FROM THE 'TO' ACCOUNT NUMBER *
      ** AND THE PRODUCT NAME         *
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N65NA.    NOTE *FORMAT THE ACCOUNT ID NUMBER       *.
       F65NA.                                                           lv15
      *
           MOVE        DI02-CTIDA TO 7-INPT-CTIDA
           MOVE        DI02-GECKD TO 7-1ICT-GECKD
           MOVE        DI02-CTIDN TO 7-INPT-UCTID
           PERFORM     F99 THRU F99-FN
           MOVE        7-OUTP-UCTID TO DL01-TWHRL1.
       F65NA-FN. EXIT.
      *N65OA.    NOTE *PRODUCT NAME IS ON WHERE LINE 2    *.
       F65OA.                                                           lv15
      *
           MOVE        DI02-PRCMN TO DL01-TWHRL2.
       F65OA-FN. EXIT.
       F65MA-FN. EXIT.
      *N65YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F65YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DI02-CPAY1 TO DL01-CPAY1.                        AMSKDL
       F65YA-FN. EXIT.
      *N65ZA.    NOTE *RETURN TO CALLING MODULE           *.            AMSKDL
       F65ZA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** RETURN TO CALLING MODULE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE                     ALL '1' TO FT GO TO F20.            AMSKDL
       F65ZA-FN. EXIT.
       F65-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU002
      *               *                                   *             ADU002
      *               *RETURN TO CALLING MODULE           *             ADU002
      *               *                                   *             ADU002
      *               *************************************.            ADU002
       F79.                                                             lv05
      *                                                                 ADU002
      *********************************                                 ADU002
      ** RETURN TO CALLING MODULE     *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
           MOVE                     ALL '1' TO FT GO TO F20.            ADU002
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT ADDRESS FROM CL24           *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96BD.    NOTE *COMPRESS CITY,STATE,ZIP,COUNTRY    *.            AANA20
       F96BD.                                                           lv10
           MOVE        SPACE TO 7-ADDR-XZ45                             AANA20
           MOVE        '1' TO 7-ADDR-XCF                                AANA20
           MOVE        1 TO IRE00R IIN00R.                              AANA20
                 IF    7-ADDR-GECTRY = 'USA'                            DOT
           MOVE        SPACE TO 7-ADDR-GECTRY.                          AANA20
      *N96BF.    NOTE *LOOP                               *.            AANA20
       F96BF.                       GO TO     F96BF-B.                  lv20
       F96BF-A.
                 IF    IIN00R > IIN00M                                  AANA20
                                    GO TO     F96BF-FN.                 AANA20
       F96BF-B.       EXIT.
      *N96BH.    NOTE *MOVE OF CHARACTER                  *.            AANA20
       F96BH.                                                           lv25
                 IF    7-ADDR-XZ1A (IIN00R) = SPACE                     DOT
                 AND   7-ADDR-XCF = '1'                                 AANA20
               GO TO     F96BH-FN.                                      AANA20
                 IF    IRE00R > 30                                      DOT
               GO TO     F96BF-FN.
           MOVE        7-ADDR-XZ1A (IIN00R) TO                          DOT
           7-ADDR-XZ1 (IRE00R)                                          AANA20
      *BATCH DOES NOT USE ERU
      *BATCH DOES NOT USE ERU.
      *IGNORE LINE 180.                                                 DOT
           ADD         1 TO IRE00R.                                     DOT
                 IF    7-ADDR-XZ1A (IIN00R)                             DOT
                       NOT = SPACE                                      AANA20
           MOVE        '0' TO 7-ADDR-XCF                                AANA20
                 ELSE                                                   AANA20
           MOVE        '1' TO 7-ADDR-XCF.                               AANA20
       F96BH-FN. EXIT.
      *N96BJ.    NOTE *INCREMENT INPUT INDEX              *.            AANA20
       F96BJ.                                                           lv25
                 IF    (IIN00R = 35)                                    DOT
                 AND   (7-ADDR-GECTRY = SPACES)                         AANA20
                 AND   IRE00R < 31
      *1ST, ADD A SECOND SPACE BETWEEN                                  AANA20
      *STATE AND ZIP FOR USA ADDRESSES.                                 AANA20
           MOVE        SPACE TO 7-ADDR-XZ1 (IRE00R)
           ADD         1 TO IRE00R.                                     AANA20
           ADD         1 TO IIN00R.                                     DOT
       F96BJ-FN. EXIT.
       F96BF-900. GO TO F96BF-A.
       F96BF-FN. EXIT.
      *N96BK.    NOTE *MOVE RESULT                        *.            AANA20
       F96BK.                                                           lv20
           MOVE        7-ADDR-XZ45 TO 7-ADDR-GESAD (4).                 AANA20
       F96BK-FN. EXIT.
      *N96CB.    NOTE *FIRST LINE                         *.            AANA30
       F96CB.    IF    7-ADDR-GESAD (1) = SPACE                         lv20
                 OR    7-ADDR-GESAD (1) = LOW-VALUE                     AANA30
                 NEXT SENTENCE ELSE GO TO     F96CB-FN.                 AANA30
                 IF    7-ADDR-GESAD (2) NOT = SPACE                     DOT
                 AND   LOW-VALUE                                        AANA30
           MOVE        7-ADDR-GESAD (2) TO 7-ADDR-GESAD (1)             AANA30
           MOVE        SPACE TO 7-ADDR-GESAD (2)                        AANA30
               GO TO     F96CB-FN.                                      AANA30
                 IF    7-ADDR-GESAD (3) NOT = SPACE                     DOT
                 AND   LOW-VALUE                                        AANA30
           MOVE        7-ADDR-GESAD (3) TO 7-ADDR-GESAD (1)             AANA30
           MOVE        SPACE TO 7-ADDR-GESAD (3)                        AANA30
               GO TO     F96CB-FN.                                      AANA30
                 IF    7-ADDR-GESAD (4) NOT = SPACE                     DOT
                 AND   LOW-VALUE                                        AANA30
           MOVE        7-ADDR-GESAD (4) TO 7-ADDR-GESAD (1)             AANA30
           MOVE        SPACE TO 7-ADDR-GESAD (4)                        AANA30
               GO TO     F96BD-FN.                                      AANA30
       F96CB-FN. EXIT.
      *N96CF.    NOTE *SECOND LINE                        *.            AANA30
       F96CF.    IF    7-ADDR-GESAD (2) = SPACE                         lv20
                 OR    7-ADDR-GESAD (2) = LOW-VALUE                     AANA30
                 NEXT SENTENCE ELSE GO TO     F96CF-FN.                 AANA30
                 IF    7-ADDR-GESAD (3) NOT = SPACE                     DOT
                 AND   LOW-VALUE                                        AANA30
           MOVE        7-ADDR-GESAD (3) TO 7-ADDR-GESAD (2)             AANA30
           MOVE        SPACE TO 7-ADDR-GESAD (3)                        AANA30
               GO TO     F96CF-FN.                                      AANA30
                 IF    7-ADDR-GESAD (4) NOT = SPACE                     DOT
                 AND   LOW-VALUE                                        AANA30
           MOVE        7-ADDR-GESAD (4) TO 7-ADDR-GESAD (2)             AANA30
           MOVE        SPACE TO 7-ADDR-GESAD (4)                        AANA30
               GO TO     F96BD-FN.                                      AANA30
       F96CF-FN. EXIT.
      *N96CM.    NOTE *THIRD LINE                         *.            AANA30
       F96CM.    IF    7-ADDR-GESAD (3) = SPACE                         lv20
                 OR    7-ADDR-GESAD (3) = LOW-VALUE                     AANA30
                 NEXT SENTENCE ELSE GO TO     F96CM-FN.                 AANA30
                 IF    7-ADDR-GESAD (4) NOT = SPACE                     DOT
                 AND   LOW-VALUE                                        AANA30
           MOVE        7-ADDR-GESAD (4) TO 7-ADDR-GESAD (3)             AANA30
           MOVE        SPACE TO 7-ADDR-GESAD (4).                       AANA30
       F96CM-FN. EXIT.
       F96BD-FN. EXIT.
       F96-FN.   EXIT.
      *N97.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT NAME FROM DI02-CLNAM        *
      *               *                                   *
      *               *************************************.
       F97.           EXIT.                                             lv05
      *N97QA.    NOTE *INITIALIZATION OF WORK AREAS       *.            AANA15
       F97QA.                                                           lv10
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
           MOVE        DI02-CLNAM TO 7-NA10-CLNAM.                      AANA15
      *N97QD.    NOTE *COUNT CHARACTERS IN HONORIFIC      *.            AANA15
       F97QD.    IF    7-NA10-CLNAMH NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F97QD-FN.                 AANA15
           MOVE        INAMHM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMH TO 7-NA10-9WKNM                    AANA15
           PERFORM     F97TC THRU F97TC-FN                              AANA15
           MOVE        INMWKR TO INAMHL.                                AANA15
                 IF    INAMHL > 0                                       DOT
           PERFORM     F97TH THRU F97TH-FN                              AANA15
           MOVE        INAMHL TO 7-NA10-9LSH                            AANA15
           COMPUTE     INAMHL = INAMHL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STH.                           AANA15
       F97QD-FN. EXIT.
      *N97QJ.    NOTE *COUNT CHARACTERS IN FIRST NAME     *.            AANA15
       F97QJ.    IF    7-NA10-CLNAMF NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F97QJ-FN.                 AANA15
           MOVE        INAMFM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMF TO 7-NA10-9WKNM                    AANA15
           PERFORM     F97TC THRU F97TC-FN                              AANA15
           MOVE        INMWKR TO INAMFL.                                AANA15
                 IF    INAMFL > 0                                       DOT
           PERFORM     F97TH THRU F97TH-FN                              AANA15
           MOVE        INAMFL TO 7-NA10-9LSF                            AANA15
           COMPUTE     INAMFL = INAMFL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STF.                           AANA15
       F97QJ-FN. EXIT.
      *N97QO.    NOTE *COUNT CHARACTERS IN MIDDLE NAME    *.            AANA15
       F97QO.    IF    7-NA10-CLNAMM NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F97QO-FN.                 AANA15
           MOVE        INAMMM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMM TO 7-NA10-9WKNM                    AANA15
           PERFORM     F97TC THRU F97TC-FN                              AANA15
           MOVE        INMWKR TO INAMML.                                AANA15
                 IF    INAMML > 0                                       DOT
           PERFORM     F97TH THRU F97TH-FN                              AANA15
           MOVE        INAMML TO 7-NA10-9LSM                            AANA15
           COMPUTE     INAMML = INAMML - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STM.                           AANA15
       F97QO-FN. EXIT.
      *N97QT.    NOTE *COUNT CHARACTERS IN LAST NAME      *.            AANA15
       F97QT.    IF    7-NA10-CLNAML NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F97QT-FN.                 AANA15
           MOVE        INAMLM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAML TO 7-NA10-9WKNM                    AANA15
           PERFORM     F97TC THRU F97TC-FN                              AANA15
           MOVE        INMWKR TO INAMLL.                                AANA15
                 IF    INAMLL > 0                                       DOT
           PERFORM     F97TH THRU F97TH-FN                              AANA15
           MOVE        INAMLL TO 7-NA10-9LSL                            AANA15
           COMPUTE     INAMLL = INAMLL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STL.                           AANA15
       F97QT-FN. EXIT.
      *N97RA.    NOTE *COUNT CHARACTERS IN SUFFIX         *.            AANA15
       F97RA.    IF    7-NA10-CLNAMS NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F97RA-FN.                 AANA15
           MOVE        INAMSM TO INMWKR INMWKM                          AANA15
           MOVE        7-NA10-CLNAMS TO 7-NA10-9WKNM                    AANA15
           PERFORM     F97TC THRU F97TC-FN                              AANA15
           MOVE        INMWKR TO INAMSL.                                AANA15
                 IF    INAMSL > 0                                       DOT
           PERFORM     F97TH THRU F97TH-FN                              AANA15
           MOVE        INAMSL TO 7-NA10-9LSS                            AANA15
           COMPUTE     INAMSL = INAMSL - (INMWKR - 1)                   AANA15
           MOVE        INMWKR TO 7-NA10-9STS.                           AANA15
       F97RA-FN. EXIT.
      *N97RG.    NOTE *COUNT NUMBER OF SPACES NEEDED      *.            AANA15
       F97RG.                                                           lv15
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
       F97RG-FN. EXIT.
      *N97RK.    NOTE *CALCULATE TOTAL NUMBER OF CHRS     *.            AANA15
       F97RK.                                                           lv15
           COMPUTE     ITOTLL = INAMHL +                                AANA15
           INAMFL + INAMML +                                            AANA15
           INAMLL + INAMSL +                                            AANA15
           7-NA10-9TSPAC                                                AANA15
      *IF NO NAME - ERROR                                               AANA15
                 IF    ITOTLL = ZERO                                    DOT
           MOVE        'E' TO 7-NA10-RETURN                             AANA15
               GO TO     F97RK-FN.                                      AANA15
      *PRINT FULL NAME IF SPACE PERMITS                                 AANA15
                 IF    ITOTLL NOT > 50                                  DOT
           MOVE        'HFMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        1 TO 7-NA10-RETURN                               AANA15
               GO TO     F97RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT HONORIIFIC                                  AANA15
                 IF    INAMHL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMHL - 1.                                                  AANA15
                 IF    ITOTLL NOT > 50                                  DOT
           MOVE        ' FMLS' TO 7-NA10-9PRINT                         AANA15
           MOVE        2 TO 7-NA10-RETURN                               AANA15
               GO TO     F97RK-FN.                                      AANA15
      *CALC. LENGTH WITH MIDDLE INITIAL                                 AANA15
                 IF    INAMML > 1                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMML + 1.                                                  AANA15
                 IF    ITOTLL NOT > 50                                  DOT
           MOVE        ' FILS' TO 7-NA10-9PRINT                         AANA15
           MOVE        3 TO 7-NA10-RETURN                               AANA15
               GO TO     F97RK-FN.                                      AANA15
      *CALC. LENGTH W/O MIDDLE INITIAL                                  AANA15
                 IF    INAMML > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - 2.                             AANA15
                 IF    ITOTLL NOT > 50                                  DOT
           MOVE        ' F LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        4 TO 7-NA10-RETURN                               AANA15
               GO TO     F97RK-FN.                                      AANA15
      *CALC. LENGTH WITH FIRST INITIAL                                  AANA15
                 IF    INAMFL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL -                                AANA15
           INAMFL + 1.                                                  AANA15
                 IF    ITOTLL NOT > 50                                  DOT
           MOVE        ' I LS' TO 7-NA10-9PRINT                         AANA15
           MOVE        5 TO 7-NA10-RETURN                               AANA15
               GO TO     F97RK-FN.                                      AANA15
      *CALC. LENGTH WITHOUT SUFFIX                                      AANA15
                 IF    INAMSL > 0                                       DOT
           COMPUTE     ITOTLL = ITOTLL - INAMSL - 1.                    AANA15
                 IF    ITOTLL NOT > 50                                  DOT
           MOVE        ' I L ' TO 7-NA10-9PRINT                         AANA15
           MOVE        6 TO 7-NA10-RETURN                               AANA15
               GO TO     F97RK-FN.                                      AANA15
      *PRINT ONLY LAST NAME.                                            AANA15
           MOVE        '   L ' TO 7-NA10-9PRINT                         DOT
           MOVE        7 TO 7-NA10-RETURN                               AANA15
               GO TO     F97RK-FN.                                      AANA15
       F97RK-FN. EXIT.
      *N97RO.    NOTE *FORMAT NAME                        *.            AANA15
       F97RO.                                                           lv15
           MOVE        0 TO INMWKR INMWKM                               AANA15
           MOVE        1 TO ITOTLR                                      AANA15
           MOVE        SPACES TO 7-NA10-COCLNM1.                        AANA15
       F97RO-FN. EXIT.
      *N97RS.    NOTE *MOVE HONORIFIC IF POSSIBLE         *.            AANA15
       F97RS.    IF    7-NA10-9PRTH = 'H'                               lv15
                 AND   INAMHL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F97RS-FN.                 AANA15
           MOVE        7-NA10-CLNAMH TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSH TO INMWKM                            AANA15
           MOVE        7-NA10-9STH TO INMWKR                            AANA15
           PERFORM     F97TP THRU F97TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F97RS-FN. EXIT.
      *N97RX.    NOTE *MOVE FIRST NAME IF POSSIBLE        *.            AANA15
       F97RX.    IF    7-NA10-9PRTF = 'F'                               lv15
                 AND   INAMFL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F97RX-FN.                 AANA15
           MOVE        7-NA10-CLNAMF TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSF TO INMWKM                            AANA15
           MOVE        7-NA10-9STF TO INMWKR                            AANA15
           PERFORM     F97TP THRU F97TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F97RX-FN. EXIT.
      *N97RZ.    NOTE *MOVE FIRST INITIAL ?               *.            AANA15
       F97RZ.    IF    7-NA10-9PRTF = 'I'                               lv15
                 AND   INAMFL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F97RZ-FN.                 AANA15
           MOVE        7-NA10-9NMF (7-NA10-9STF) TO                     AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         2 TO ITOTLR.                                     AANA15
       F97RZ-FN. EXIT.
      *N97SB.    NOTE *MOVE MIDDLE NAME ?                 *.            AANA15
       F97SB.    IF    7-NA10-9PRTM = 'M'                               lv15
                 AND   INAMML > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F97SB-FN.                 AANA15
           MOVE        7-NA10-CLNAMM TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSM TO INMWKM                            AANA15
           MOVE        7-NA10-9STM TO INMWKR                            AANA15
           PERFORM     F97TP THRU F97TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F97SB-FN. EXIT.
      *N97SE.    NOTE *MOVE MIDDLE INITIAL                *.            AANA15
       F97SE.    IF    7-NA10-9PRTM = 'I'                               lv15
                 AND   INAMML > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F97SE-FN.                 AANA15
           MOVE        7-NA10-9NMM (7-NA10-9STM) TO                     AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         2 TO ITOTLR.                                     AANA15
       F97SE-FN. EXIT.
      *N97SH.    NOTE *MOVE LAST NAME                     *.            AANA15
       F97SH.    IF    7-NA10-9PRTL = 'L'                               lv15
                 AND   INAMLL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F97SH-FN.                 AANA15
           MOVE        7-NA10-CLNAML TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSL TO INMWKM                            AANA15
           MOVE        7-NA10-9STL TO INMWKR                            AANA15
           PERFORM     F97TP THRU F97TP-FN                              AANA15
           ADD         1 TO ITOTLR.                                     AANA15
       F97SH-FN. EXIT.
      *N97SL.    NOTE *MOVE SUFFIX ?                      *.            AANA15
       F97SL.    IF    7-NA10-9PRTS = 'S'                               lv15
                 AND   INAMSL > 0                                       AANA15
                 NEXT SENTENCE ELSE GO TO     F97SL-FN.                 AANA15
           MOVE        7-NA10-CLNAMS TO 7-NA10-9WKNM                    AANA15
           MOVE        7-NA10-9LSS TO INMWKM                            AANA15
           MOVE        7-NA10-9STS TO INMWKR                            AANA15
           PERFORM     F97TP THRU F97TP-FN.                             AANA15
       F97SL-FN. EXIT.
      *N97SZ.    NOTE *MOVE PACKED NAME TO PASSED PARM    *.            AANA15
       F97SZ.                                                           lv15
           MOVE        7-NA10-COCLNM1 TO                                AANA15
           DL01-TWHOL1.                                                 AANA15
       F97SZ-FN. EXIT.
       F97QA-FN. EXIT.
      *N97TC.    NOTE *DECREMENT COUNTER                  *.            AANA15
       F97TC.    IF    INMWKR > 0                                       lv10
                 AND   7-NA10-9WKNM1 (INMWKR)                           AANA15
                       = SPACES                                         AANA15
                 NEXT SENTENCE ELSE GO TO     F97TC-FN.                 AANA15
           SUBTRACT    1 FROM INMWKR.                                   AANA15
       F97TC-900. GO TO F97TC.
       F97TC-FN. EXIT.
      *N97TH.    NOTE *RESET SUBSCRIPT                    *.            AANA15
       F97TH.                                                           lv10
           MOVE        INMWKR TO INMWKM                                 AANA15
           MOVE        1 TO INMWKR                                      AANA15
           PERFORM     F97TL THRU F97TL-FN.                             AANA15
       F97TH-FN. EXIT.
      *N97TL.    NOTE *INCREMENT COUNTER                  *.            AANA15
       F97TL.    IF    INMWKR NOT >                                     lv10
                       INMWKM                                           AANA15
                 AND   7-NA10-9WKNM1 (INMWKR)                           AANA15
                       = SPACES                                         AANA15
                 NEXT SENTENCE ELSE GO TO     F97TL-FN.                 AANA15
           ADD         1 TO INMWKR.                                     AANA15
       F97TL-900. GO TO F97TL.
       F97TL-FN. EXIT.
      *N97TP.    NOTE *SAVE CHARACTERS                    *.            AANA15
       F97TP.    IF    INMWKR NOT > INMWKM                              lv10
                 NEXT SENTENCE ELSE GO TO     F97TP-FN.                 AANA15
           MOVE        7-NA10-9WKNM1 (INMWKR) TO                        AANA15
           7-NA10-COCLNM (ITOTLR)                                       AANA15
           ADD         1 TO ITOTLR INMWKR.                              AANA15
       F97TP-900. GO TO F97TP.
       F97TP-FN. EXIT.
       F97-FN.   EXIT.
      *N98.      NOTE *************************************.            ADU002
      *               *                                   *             ADU002
      *               *COMMON PERFORMED ROUTINES          *             ADU002
      *               *                                   *             ADU002
      *               *************************************.            ADU002
       F98.           EXIT.                                             lv05
      *N98ET.    NOTE *ERROR MESSAGE PROCESS              *.            ADU002
       F98ET.                                                           lv10
      *********************************                                 ADU002
      *    SET MESSAGE SEVERITY                                         ADU002
      *********************************                                 ADU002
           MOVE        11 TO MS03-CMESB                                 ADU002
      *********************************                                 ADU002
      *    GET MESSAGE TEXT                                             ADU002
      *********************************                                 ADU002
           PERFORM     F98GM THRU F98GM-FN.                             ADU002
       F98ET-FN. EXIT.
      *N98GM.    NOTE *GET MESSAGE TEXT                   *.            ADU002
       F98GM.                                                           lv10
      *********************************                                 ADU002
      *CALL DATA UTILITY MESSAGE MODULE                                 ADU002
      *********************************                                 ADU002
           MOVE        'AP' TO MS03-CMSSF                               ADU002
           CALL        CI0002 USING                                     ADU002
           DFHEIBLK                                                     ADU002
           DFHCOMMAREA                                                  ADU002
           MS03.                                                        ADU002
       F98GM-FN. EXIT.
      *N98IC.    NOTE *INFO MESSAGE PROCESS               *.            ADU002
       F98IC.                                                           lv10
      *********************************                                 ADU002
      *    SET MESSAGE SEVERITY                                         ADU002
      *********************************                                 ADU002
           MOVE        10 TO MS03-CMESB                                 ADU002
      *********************************                                 ADU002
      *    GET MESSAGE TEXT                                             ADU002
      *********************************                                 ADU002
           PERFORM     F98GM THRU F98GM-FN.                             ADU002
       F98IC-FN. EXIT.
       F98-FN.   EXIT.
      *N99.      NOTE *************************************.
      *               *                                   *
      *               *FORMAT ACCT NUMBER FOR WS          *
      *               *                                   *
      *               *************************************.
       F99.           EXIT.                                             lv05
      *N99NA.    NOTE *FORMAT CONTRACT ID FOR DISPLAY     *.            AAOFCT
       F99NA.                                                           lv10
           MOVE        7-1ICT-GECKD TO 7-1FCT-GECKD                     AAOFCT
           MOVE        7-INPT-CTIDA TO 7-FRMT-CTIDA.                    AAOFCT
      *N99NE.    NOTE *FORMAT 24 CHAR CONTRACT ID         *.            AAOFCT
       F99NE.    IF    7-1ICT-XZ4 NOT = '0000'                          lv15
                 NEXT SENTENCE ELSE GO TO     F99NE-FN.                 AAOFCT
           MOVE        7-1ICT-XZ4 TO 7-1FCT-XZ4                         AAOFCT
           MOVE        7-2ICT-XZ4 TO 7-2FCT-XZ4                         AAOFCT
           MOVE        7-3ICT-XZ4 TO 7-3FCT-XZ4                         AAOFCT
           MOVE        7-4ICT-XZ4 TO 7-4FCT-XZ4                         AAOFCT
           MOVE        7-5ICT-XZ4 TO 7-5FCT-XZ4                         AAOFCT
           MOVE        7-6ICT-XZ4 TO 7-6FCT-XZ4                         AAOFCT
           MOVE        7-FRMT-UCTID TO 7-OUTP-UCTID                     AAOFCT
               GO TO     F99NA-FN.                                      AAOFCT
       F99NE-FN. EXIT.
      *N99NG.    NOTE *FORMAT 20 CHAR CONTRACT ID         *.            AAOFCT
       F99NG.    IF    7-2ICT-XZ4 NOT = '0000'                          lv15
                 NEXT SENTENCE ELSE GO TO     F99NG-FN.                 AAOFCT
           MOVE        7-2ICT-XZ4 TO 7-2FCT-XZ4                         AAOFCT
           MOVE        7-3ICT-XZ4 TO 7-3FCT-XZ4                         AAOFCT
           MOVE        7-4ICT-XZ4 TO 7-4FCT-XZ4                         AAOFCT
           MOVE        7-5ICT-XZ4 TO 7-5FCT-XZ4                         AAOFCT
           MOVE        7-6ICT-XZ4 TO 7-6FCT-XZ4                         AAOFCT
           MOVE        7-1FCT-UCTID TO 7-OUTP-UCTID                     AAOFCT
               GO TO     F99NA-FN.                                      AAOFCT
       F99NG-FN. EXIT.
      *N99NI.    NOTE *FORMAT 16 CHAR CONTRACT ID         *.            AAOFCT
       F99NI.                                                           lv15
           MOVE        7-3ICT-XZ4 TO 7-3FCT-XZ4                         AAOFCT
           MOVE        7-4ICT-XZ4 TO 7-4FCT-XZ4                         AAOFCT
           MOVE        7-5ICT-XZ4 TO 7-5FCT-XZ4                         AAOFCT
           MOVE        7-6ICT-XZ4 TO 7-6FCT-XZ4                         AAOFCT
           MOVE        7-2FCT-UCTID TO 7-OUTP-UCTID                     AAOFCT
               GO TO     F99NA-FN.                                      AAOFCT
       F99NI-FN. EXIT.
       F99NA-FN. EXIT.
       F99-FN.   EXIT.
