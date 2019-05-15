       IDENTIFICATION DIVISION.                                         CI0084
       PROGRAM-ID.  CI0084P.                                            CI0084
      *AUTHOR.         M\M - VALIDATE ARRANGMENT.                       CI0084
      *DATE-COMPILED.   09/08/14.                                       CI0084
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1998                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CATS   SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CATS   SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CATS         *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 1998                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0084
       CONFIGURATION SECTION.                                           CI0084
       SOURCE-COMPUTER. IBM-370.                                        CI0084
       OBJECT-COMPUTER. IBM-370.                                        CI0084
       DATA DIVISION.                                                   CI0084
       WORKING-STORAGE SECTION.                                         CI0084
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *
      ******************************************************************
      **     DATE CONVERSION SEGMENT                                   *
      ******************************************************************
      *
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0084
          05              DD00-SUITE.                                   CI0084
            15       FILLER         PICTURE  X(00093).                  CI0084
       01                 DD01  REDEFINES      DD00.                    CI0084
            10            DD01-XDAT8.                                   CI0084
            11            DD01-XDATC  PICTURE  XX.                      CI0084
            11            DD01-XDATY  PICTURE  XX.                      CI0084
            11            DD01-XDATM  PICTURE  XX.                      CI0084
            11            DD01-XDATD  PICTURE  XX.                      CI0084
            10            DD01-XDAT8D                                   CI0084
                          REDEFINES            DD01-XDAT8               CI0084
               PICTURE    9(8).                                         CI0084
            10            DD01-XDAT81.                                  CI0084
            11            DD01-XDATM1 PICTURE  XX.                      CI0084
            11            DD01-XDATD1 PICTURE  XX.                      CI0084
            11            DD01-XDATC1 PICTURE  XX.                      CI0084
            11            DD01-XDATY1 PICTURE  XX.                      CI0084
            10            DD01-XDAT80                                   CI0084
                          REDEFINES            DD01-XDAT81              CI0084
               PICTURE    9(8).                                         CI0084
            10            DD01-XDAT62.                                  CI0084
            11            DD01-XDATM2 PICTURE  XX.                      CI0084
            11            DD01-XDATD2 PICTURE  XX.                      CI0084
            11            DD01-XDATY2 PICTURE  XX.                      CI0084
            10            DD01-XDAT69                                   CI0084
                          REDEFINES            DD01-XDAT62              CI0084
               PICTURE    9(6).                                         CI0084
            10            DD01-XDATCU.                                  CI0084
            11            DD01-XDATC9 PICTURE  99.                      CI0084
            11            DD01-XDAYMD.                                  CI0084
            12            DD01-XDATY9 PICTURE  99.                      CI0084
            12            DD01-XDAMD.                                   CI0084
            13            DD01-XDATM9 PICTURE  99.                      CI0084
            13            DD01-XDATD9 PICTURE  99.                      CI0084
            10            DD01-XDAT89 PICTURE  9(8).                    CI0084
            10            DD01-XDAJC  PICTURE  9(7).                    CI0084
            10            DD01-XDAJC1.                                  CI0084
            11            DD01-XDAJC9 PICTURE  99.                      CI0084
            11            DD01-XDAJY  PICTURE  99.                      CI0084
            11            DD01-XDAJN  PICTURE  999.                     CI0084
            10            DD01-XDAB   PICTURE  9(5).                    CI0084
            10            DD01-DD05.                                    CI0084
            11            DD01-XDACT  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            DD01-XDACV  PICTURE  S9                       CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            DD01-XDACV1 PICTURE  S9                       CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            DD01-XW03.                                    CI0084
            11            DD01-XDATG.                                   CI0084
            12            DD01-XDAT1.                                   CI0084
            13            DD01-XDAT19 PICTURE  99.                      CI0084
            12            DD01-XDAT2.                                   CI0084
            13            DD01-XDAT29 PICTURE  99.                      CI0084
            12            DD01-XDAT3.                                   CI0084
            13            DD01-XDAT39 PICTURE  99.                      CI0084
            12            DD01-XDAT4.                                   CI0084
            13            DD01-XDAT49 PICTURE  99.                      CI0084
            11            DD01-XLEAPY PICTURE  99.                      CI0084
            11            DD01-DTGCY  PICTURE  9(4).                    CI0084
            11            DD01-FILLER                                   CI0084
                          REDEFINES            DD01-DTGCY.              CI0084
            12            DD01-DTGCC  PICTURE  9(2).                    CI0084
            12            DD01-DTGYY  PICTURE  9(2).                    CI0084
      ** DATE WORK AREA
       01  DEL-ER                 PIC 9(1).
      **------------------MS03
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
      *
       01  W-WK00-AREAS.
      **
      ** THIS AREA WILL BE USED TO ENSURE EACH POSITION OF CIRMO HAS
      ** THE PROPER VALUE IN IT.
      **
           05  W-WK00-CIRMO.
               10  FILLER           PIC X(01).
                   88  JAN                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  FEB                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  MAR                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  APR                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  MAY                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  JUN                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  JUL                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  AUG                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  SEP                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  OCT                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  NOV                     VALUE ' ', 'X'.
               10  FILLER           PIC X(01).
                   88  DEC                     VALUE ' ', 'X'.
      **
      ** THIS FIELD IS USED TO STORE THE NUMBER OF MONTH THAT ARE
      ** MISSING IN CIRMO FOR IRREGULAR SCHEDULES.
      **
           05  W-WK00-COUNT         PIC 9(02).
      **
      ** COMMON HOLDING AREA FOR FREQUENCY
      *!WI
           05  W-CX13-CPMTF
                        PICTURE 99.                                     CI0084
      **
      ** THIS FIELD IS USED TO STORE THE NUMBER OF MONTH THAT ARE
      ** MISSING IN CIRMO FOR IRREGULAR SCHEDULES.
      **
      *!WI
           05  W-DOPDA
                        PICTURE 99.                                     CI0084

      *ESTIMATED DISBURSEMENT AMOUNT
      *!WI
       01   WS10-AEDRQ
                        PICTURE S9(11)V99                               CI0084
                          COMPUTATIONAL-3.                              CI0084

       01   DEBUT-WSS.                                                  CI0084
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0084
            05   IK     PICTURE X.                                      CI0084
       01  CONSTANTES-PAC.                                              CI0084
           05  FILLER  PICTURE X(87)   VALUE                            CI0084
                     '6015 CAT09/08/14CI0084ADMIN   14:34:42CI0084P AMERCI0084
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0084
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0084
           05  NUGNA   PICTURE X(5).                                    CI0084
           05  APPLI   PICTURE X(3).                                    CI0084
           05  DATGN   PICTURE X(8).                                    CI0084
           05  PROGR   PICTURE X(6).                                    CI0084
           05  CODUTI  PICTURE X(8).                                    CI0084
           05  TIMGN   PICTURE X(8).                                    CI0084
           05  PROGE   PICTURE X(8).                                    CI0084
           05  COBASE  PICTURE X(4).                                    CI0084
           05  DATGNC  PICTURE X(10).                                   CI0084
           05  RELEAS  PICTURE X(7).                                    CI0084
           05  DATGE   PICTURE X(10).                                   CI0084
           05  DATSQ   PICTURE X(10).                                   CI0084
       01  DATCE.                                                       CI0084
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0084
         05  DATOR.                                                     CI0084
           10  DATOA  PICTURE XX.                                       CI0084
           10  DATOM  PICTURE XX.                                       CI0084
           10  DATOJ  PICTURE XX.                                       CI0084
       01   VARIABLES-CONDITIONNELLES.                                  CI0084
            05                  FT      PICTURE X VALUE '0'.            CI0084
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0084
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0084
            05           IMS03L PICTURE S9(4) VALUE  ZERO.
            05           IMS03R PICTURE S9(4) VALUE  ZERO.
            05           IMS03M PICTURE S9(4) VALUE +0512.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0084
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0084
            05       5-OX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0084
       01   ZONES-UTILISATEUR PICTURE X.                                CI0084
       LINKAGE SECTION.                                                 ADU102
      *
      *PASS AREA TO CI0084. ALSO SERVES AS PASS AREA TO/FROM CI9006.
      *!WF DSP=PJ DSL=PJ SEL=06 FOR=I LEV=1 PLT=10
       01                 PJ00.                                         CI0084
          05              PJ00-SUITE.                                   CI0084
            15       FILLER         PICTURE  X(00503).                  CI0084
       01                 PJ06  REDEFINES      PJ00.                    CI0084
            10            PJ06-MAPPN  PICTURE  X(10).                   CI0084
            10            PJ06-CHCR   PICTURE  99.                      CI0084
            10            PJ06-CUPIQ  PICTURE  X.                       CI0084
            10            PJ06-COVRC  PICTURE  X.                       CI0084
            10            PJ06-CACTA  PICTURE  X(1).                    CI0084
            10            PJ06-NEIBT  PICTURE  X(7).                    CI0084
            10            PJ06-GESQ2C PICTURE  S99                      CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            PJ06-CX01K.                                   CI0084
            11            PJ06-C199.                                    CI0084
            12            PJ06-CLID.                                    CI0084
            13            PJ06-CLIDO  PICTURE  9(3).                    CI0084
            13            PJ06-CLIDN.                                   CI0084
            14            PJ06-CLIDNP PICTURE  X(12).                   CI0084
            14            PJ06-CLIDND PICTURE  9(8).                    CI0084
            10            PJ06-CX03K.                                   CI0084
            11            PJ06-CARTY  PICTURE  99.                      CI0084
            11            PJ06-NARRS  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            PJ06-CX06K.                                   CI0084
            11            PJ06-C299.                                    CI0084
            12            PJ06-CTID.                                    CI0084
            13            PJ06-CTIDA  PICTURE  9(3).                    CI0084
            13            PJ06-CTIDN.                                   CI0084
            14            PJ06-CTIDNP PICTURE  X(13).                   CI0084
            14            PJ06-CTIDND PICTURE  9(11).                   CI0084
            10            PJ06-CX13K.                                   CI0084
            11            PJ06-CARTZ  PICTURE  99.                      CI0084
            11            PJ06-NAPDS  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            PJ06-CX14K.                                   CI0084
            11            PJ06-NPISQ  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            PJ06-CPAY1  PICTURE  X(2).                    CI0084
            10            PJ06-CPMTF  PICTURE  99.                      CI0084
            10            PJ06-AINPU  PICTURE  S9(9)V99.                CI0084
            10            PJ06-QFOSPD PICTURE  9(2).                    CI0084
            10            PJ06-QSHES  PICTURE  S9(10)V999               CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            PJ06-FILLER PICTURE  X(02).                   CI0084
            10            PJ06-CPAYC  PICTURE  X(2).                    CI0084
            10            PJ06-CFLOW  PICTURE  X.                       CI0084
            10            PJ06-CTID01 PICTURE  X(27).                   CI0084
            10            PJ06-IDELI  PICTURE  X.                       CI0084
            10            PJ06-CDEL1  PICTURE  9(3).                    CI0084
            10            PJ06-NDELS  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            PJ06-OUTPUT.                                  CI0084
            11            PJ06-CTSTA  PICTURE  99.                      CI0084
            11            PJ06-AACTV  PICTURE  S9(11)V99                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-AFAVP  PICTURE  S9(4)V9(3)               CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-CTCUS  PICTURE  999.                     CI0084
            11            PJ06-CTXMTA PICTURE  9(2).                    CI0084
            11            PJ06-INMRC  PICTURE  X(01).                   CI0084
            11            PJ06-CTCUS2 PICTURE  999.                     CI0084
            11            PJ06-CTXMTB PICTURE  9(2).                    CI0084
            11            PJ06-CIRAD  PICTURE  X.                       CI0084
            11            PJ06-IARLNA PICTURE  X.                       CI0084
            11            PJ06-AMINAL PICTURE  S9(7)V99.                CI0084
            11            PJ06-AMAXAL PICTURE  S9(7)V99.                CI0084
            11            PJ06-IARRGA PICTURE  X.                       CI0084
            11            PJ06-AMIND  PICTURE  S9(7)V99.                CI0084
            11            PJ06-AMAXAR PICTURE  S9(7)V99.                CI0084
            11            PJ06-IARPSA PICTURE  X.                       CI0084
            11            PJ06-CELBA  PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-AMAXA  PICTURE  S9(7)V99.                CI0084
            11            PJ06-QSHOW  PICTURE  S9(10)V999               CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-QSHOWQ PICTURE  S9(9)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-PACT1  PICTURE  S999V999                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-PPOT1  PICTURE  S9(3)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-QMTH   PICTURE  9(3).                    CI0084
            11            PJ06-QMTH1  PICTURE  9(3).                    CI0084
            11            PJ06-ALPAGM PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-ALPAGQ PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-ALPAGS PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-ALPAGR PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-ALDDUE PICTURE  9(08).                   CI0084
            11            PJ06-CTSTAC PICTURE  99.                      CI0084
            11            PJ06-IFQAN  PICTURE  X.                       CI0084
            11            PJ06-IFQSA  PICTURE  X.                       CI0084
            11            PJ06-IFQFM  PICTURE  X.                       CI0084
            11            PJ06-IFQQT  PICTURE  X.                       CI0084
            11            PJ06-IFQBM  PICTURE  X.                       CI0084
            11            PJ06-IFQMO  PICTURE  X.                       CI0084
            11            PJ06-IFQBF  PICTURE  X.                       CI0084
            11            PJ06-IFQSM  PICTURE  X.                       CI0084
            11            PJ06-IFQBW  PICTURE  X.                       CI0084
            11            PJ06-IFQWK  PICTURE  X.                       CI0084
            11            PJ06-IFQIR  PICTURE  X.                       CI0084
            11            PJ06-IFQANT PICTURE  X(01).                   CI0084
            11            PJ06-IFQSAT PICTURE  X(01).                   CI0084
            11            PJ06-IFQQTT PICTURE  X(01).                   CI0084
            11            PJ06-IFQBMT PICTURE  X(01).                   CI0084
            11            PJ06-IFQMOT PICTURE  X(01).                   CI0084
            11            PJ06-IFQET  PICTURE  X.                       CI0084
            11            PJ06-DCACG  PICTURE  9(8).                    CI0084
            11            PJ06-DAUTB  PICTURE  9(8).                    CI0084
            11            PJ06-CESLD  PICTURE  9(8).                    CI0084
            11            PJ06-IWTHH  PICTURE  X.                       CI0084
            11            PJ06-IWTHH1 PICTURE  X.                       CI0084
            11            PJ06-CTWTC  PICTURE  9(2).                    CI0084
            11            PJ06-AWITH  PICTURE  S9(11)V99                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-AWITH1 PICTURE  S9(11)V99                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            PJ06-IUGMA  PICTURE  X.                       CI0084
            11            PJ06-CLCUS  PICTURE  99.                      CI0084
            11            PJ06-CDSTR  PICTURE  XX.                      CI0084
            11            PJ06-CADCG  PICTURE  X(1).                    CI0084
            11            PJ06-CPDOM  PICTURE  X(31).                   CI0084
            11            PJ06-CPDOMT PICTURE  X(31).                   CI0084
            11            PJ06-CPDOMA PICTURE  X(31).                   CI0084
            11            PJ06-CPDOMC PICTURE  X(31).                   CI0084
            11            PJ06-CLDOB  PICTURE  9(8).                    CI0084
            11            PJ06-ICRTM  PICTURE  9.                       CI0084
            11            PJ06-CLID01 PICTURE  X(23).                   CI0084
            11            PJ06-IARCDA PICTURE  X.                       CI0084
       01                 CX13.                                         CI0084
            10            CX13-GELL   PICTURE  9(4)                     CI0084
                          BINARY.                                       CI0084
            10            CX13-CY20.                                    CI0084
            11            CX13-CX13K.                                   CI0084
            12            CX13-CARTZ  PICTURE  99.                      CI0084
            12            CX13-NAPDS  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-GESTD  PICTURE  9(8).                    CI0084
            11            CX13-GEEND  PICTURE  9(8).                    CI0084
            11            CX13-DASUQ  PICTURE  9(8).                    CI0084
            11            CX13-CDEST  PICTURE  99.                      CI0084
            11            CX13-IIARR  PICTURE  X.                       CI0084
            11            CX13-DLAUP  PICTURE  9(8).                    CI0084
            11            CX13-GEOPD2 PICTURE  X(8).                    CI0084
            11            CX13-GEAUN  PICTURE  9(5).                    CI0084
            11            CX13-DPCHD  PICTURE  9(8).                    CI0084
            11            CX13-PPOT1  PICTURE  S9(3)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-ACOT1  PICTURE  S9(9)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-QPST1  PICTURE  S9(7)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-FILLER PICTURE  X(03).                   CI0084
            10            CX13-CY96.                                    CI0084
            11            CX13-FILLER PICTURE  X(50).                   CI0084
            10            CX13-CY21                                     CI0084
                          REDEFINES            CX13-CY96.               CI0084
            11            CX13-DNPMT  PICTURE  9(8).                    CI0084
            11            CX13-CPMTF  PICTURE  99.                      CI0084
            11            CX13-ADBRQ  PICTURE  S9(11)V99                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-QSHOWQ PICTURE  S9(9)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-PACT1  PICTURE  S999V999                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-DOPDA  PICTURE  99.                      CI0084
            11            CX13-DNEXE  PICTURE  9(8).                    CI0084
            11            CX13-CIRMO  PICTURE  X(12).                   CI0084
            10            CX13-CY98.                                    CI0084
            11            CX13-FILLER PICTURE  X(120).                  CI0084
            10            CX13-CY25                                     CI0084
                          REDEFINES            CX13-CY98.               CI0084
            11            CX13-COPTC  PICTURE  9(1).                    CI0084
            11            CX13-ILPOI  PICTURE  X(1).                    CI0084
            11            CX13-CATOC  PICTURE  X(1).                    CI0084
            11            CX13-CEOIA  PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-ACOAR  PICTURE  S9(9)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-CEOTR  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-DSTMO  PICTURE  99.                      CI0084
            10            CX13-CY27                                     CI0084
                          REDEFINES            CX13-CY98.               CI0084
            11            CX13-QMTH1  PICTURE  9(3).                    CI0084
            11            CX13-IDRMD  PICTURE  X.                       CI0084
            10            CX13-CY28                                     CI0084
                          REDEFINES            CX13-CY98.               CI0084
            11            CX13-AALLBL PICTURE  S9(8)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-PSURR  PICTURE  S9(3)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-DFPMT  PICTURE  9(8).                    CI0084
            11            CX13-QMTHLA PICTURE  9(3).                    CI0084
            11            CX13-PWHLDS PICTURE  S999V9(5)                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-ISWHO  PICTURE  X(1).                    CI0084
            10            CX13-CY29                                     CI0084
                          REDEFINES            CX13-CY98.               CI0084
            11            CX13-IINDI1 PICTURE  X(1).                    CI0084
            11            CX13-IINDI2 PICTURE  X(1).                    CI0084
            11            CX13-IINDI3 PICTURE  X(1).                    CI0084
            11            CX13-PWHLD5 PICTURE  S999V99                  CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-CCSMQ  PICTURE  X.                       CI0084
            11            CX13-CPLEC  PICTURE  XX.                      CI0084
            11            CX13-IPTRDA PICTURE  X(01).                   CI0084
            11            CX13-GCUSPY PICTURE  X(12).                   CI0084
            11            CX13-ALOIDA PICTURE  S9(11)V99                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            CX13-DELOI  PICTURE  9(8).                    CI0084
            11            CX13-CLGND  PICTURE  X.                       CI0084
            11            CX13-CORTYA PICTURE  X(3).                    CI0084
            11            CX13-CPH3U  PICTURE  X.                       CI0084
            11            CX13-CNAVR  PICTURE  X(1).                    CI0084
            11            CX13-NEXEC  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
       01                 CX14.                                         CI0084
            10            CX14-GELL   PICTURE  9(4)                     CI0084
                          BINARY.                                       CI0084
            10            CX14-CX14K.                                   CI0084
            11            CX14-NPISQ  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            CX14-ACOTD  PICTURE  S9(9)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            CX14-PPOTD  PICTURE  S9(3)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            CX14-QPSTD  PICTURE  S9(7)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            CX14-CPITC  PICTURE  99.                      CI0084
            10            CX14-FILLER PICTURE  X(04).                   CI0084
            10            CX14-CY97.                                    CI0084
            11            CX14-FILLER PICTURE  X(32).                   CI0084
            10            CX14-CY30                                     CI0084
                          REDEFINES            CX14-CY97.               CI0084
            11            CX14-IOWNC  PICTURE  X.                       CI0084
            11            CX14-CTYPE  PICTURE  X.                       CI0084
            11            CX14-C299.                                    CI0084
            12            CX14-CTID.                                    CI0084
            13            CX14-CTIDA  PICTURE  9(3).                    CI0084
            13            CX14-CTIDN.                                   CI0084
            14            CX14-CTIDNP PICTURE  X(13).                   CI0084
            14            CX14-CTIDND PICTURE  9(11).                   CI0084
            11            CX14-CPMTC  PICTURE  99.                      CI0084
            11            CX14-IACSD  PICTURE  X.                       CI0084
            10            CX14-CY31                                     CI0084
                          REDEFINES            CX14-CY97.               CI0084
            11            CX14-FILLER PICTURE  X(2).                    CI0084
            11            CX14-IDELI  PICTURE  X.                       CI0084
            11            CX14-CDEL1  PICTURE  9(3).                    CI0084
            11            CX14-NDELS  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            CX14-CY32                                     CI0084
                          REDEFINES            CX14-CY97.               CI0084
            11            CX14-GCUSPZ PICTURE  X(12).                   CI0084
       01                 OX13.                                         CI0084
            10            OX13-GELL   PICTURE  9(4)                     CI0084
                          BINARY.                                       CI0084
            10            OX13-CY20.                                    CI0084
            11            OX13-CX13K.                                   CI0084
            12            OX13-CARTZ  PICTURE  99.                      CI0084
            12            OX13-NAPDS  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-GESTD  PICTURE  9(8).                    CI0084
            11            OX13-GEEND  PICTURE  9(8).                    CI0084
            11            OX13-DASUQ  PICTURE  9(8).                    CI0084
            11            OX13-CDEST  PICTURE  99.                      CI0084
            11            OX13-IIARR  PICTURE  X.                       CI0084
            11            OX13-DLAUP  PICTURE  9(8).                    CI0084
            11            OX13-GEOPD2 PICTURE  X(8).                    CI0084
            11            OX13-GEAUN  PICTURE  9(5).                    CI0084
            11            OX13-DPCHD  PICTURE  9(8).                    CI0084
            11            OX13-PPOT1  PICTURE  S9(3)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-ACOT1  PICTURE  S9(9)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-QPST1  PICTURE  S9(7)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-FILLER PICTURE  X(03).                   CI0084
            10            OX13-CY96.                                    CI0084
            11            OX13-FILLER PICTURE  X(50).                   CI0084
            10            OX13-CY21                                     CI0084
                          REDEFINES            OX13-CY96.               CI0084
            11            OX13-DNPMT  PICTURE  9(8).                    CI0084
            11            OX13-CPMTF  PICTURE  99.                      CI0084
            11            OX13-ADBRQ  PICTURE  S9(11)V99                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-QSHOWQ PICTURE  S9(9)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-PACT1  PICTURE  S999V999                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-DOPDA  PICTURE  99.                      CI0084
            11            OX13-DNEXE  PICTURE  9(8).                    CI0084
            11            OX13-CIRMO  PICTURE  X(12).                   CI0084
            10            OX13-CY98.                                    CI0084
            11            OX13-FILLER PICTURE  X(120).                  CI0084
            10            OX13-CY25                                     CI0084
                          REDEFINES            OX13-CY98.               CI0084
            11            OX13-COPTC  PICTURE  9(1).                    CI0084
            11            OX13-ILPOI  PICTURE  X(1).                    CI0084
            11            OX13-CATOC  PICTURE  X(1).                    CI0084
            11            OX13-CEOIA  PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-ACOAR  PICTURE  S9(9)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-CEOTR  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-DSTMO  PICTURE  99.                      CI0084
            10            OX13-CY27                                     CI0084
                          REDEFINES            OX13-CY98.               CI0084
            11            OX13-QMTH1  PICTURE  9(3).                    CI0084
            11            OX13-IDRMD  PICTURE  X.                       CI0084
            10            OX13-CY28                                     CI0084
                          REDEFINES            OX13-CY98.               CI0084
            11            OX13-AALLBL PICTURE  S9(8)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-PSURR  PICTURE  S9(3)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-DFPMT  PICTURE  9(8).                    CI0084
            11            OX13-QMTHLA PICTURE  9(3).                    CI0084
            11            OX13-PWHLDS PICTURE  S999V9(5)                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-ISWHO  PICTURE  X(1).                    CI0084
            10            OX13-CY29                                     CI0084
                          REDEFINES            OX13-CY98.               CI0084
            11            OX13-IINDI1 PICTURE  X(1).                    CI0084
            11            OX13-IINDI2 PICTURE  X(1).                    CI0084
            11            OX13-IINDI3 PICTURE  X(1).                    CI0084
            11            OX13-PWHLD5 PICTURE  S999V99                  CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-CCSMQ  PICTURE  X.                       CI0084
            11            OX13-CPLEC  PICTURE  XX.                      CI0084
            11            OX13-IPTRDA PICTURE  X(01).                   CI0084
            11            OX13-GCUSPY PICTURE  X(12).                   CI0084
            11            OX13-ALOIDA PICTURE  S9(11)V99                CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            OX13-DELOI  PICTURE  9(8).                    CI0084
            11            OX13-CLGND  PICTURE  X.                       CI0084
            11            OX13-CORTYA PICTURE  X(3).                    CI0084
            11            OX13-CPH3U  PICTURE  X.                       CI0084
            11            OX13-CNAVR  PICTURE  X(1).                    CI0084
            11            OX13-NEXEC  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
       01                 OX14.                                         CI0084
            10            OX14-GELL   PICTURE  9(4)                     CI0084
                          BINARY.                                       CI0084
            10            OX14-CX14K.                                   CI0084
            11            OX14-NPISQ  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            OX14-ACOTD  PICTURE  S9(9)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            OX14-PPOTD  PICTURE  S9(3)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            OX14-QPSTD  PICTURE  S9(7)V999                CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            OX14-CPITC  PICTURE  99.                      CI0084
            10            OX14-FILLER PICTURE  X(04).                   CI0084
            10            OX14-CY97.                                    CI0084
            11            OX14-FILLER PICTURE  X(32).                   CI0084
            10            OX14-CY30                                     CI0084
                          REDEFINES            OX14-CY97.               CI0084
            11            OX14-IOWNC  PICTURE  X.                       CI0084
            11            OX14-CTYPE  PICTURE  X.                       CI0084
            11            OX14-C299.                                    CI0084
            12            OX14-CTID.                                    CI0084
            13            OX14-CTIDA  PICTURE  9(3).                    CI0084
            13            OX14-CTIDN.                                   CI0084
            14            OX14-CTIDNP PICTURE  X(13).                   CI0084
            14            OX14-CTIDND PICTURE  9(11).                   CI0084
            11            OX14-CPMTC  PICTURE  99.                      CI0084
            11            OX14-IACSD  PICTURE  X.                       CI0084
            10            OX14-CY31                                     CI0084
                          REDEFINES            OX14-CY97.               CI0084
            11            OX14-FILLER PICTURE  X(2).                    CI0084
            11            OX14-IDELI  PICTURE  X.                       CI0084
            11            OX14-CDEL1  PICTURE  9(3).                    CI0084
            11            OX14-NDELS  PICTURE  S9(3)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            OX14-CY32                                     CI0084
                          REDEFINES            OX14-CY97.               CI0084
            11            OX14-GCUSPZ PICTURE  X(12).                   CI0084
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE FROM ACCOUNT CONTRACT SEGMENT   *
      ******************************************************************
      *!WF DSP=FR DSL=CT SEL=01 FOR=I LEV=1 PLT=80
       01                 FR00.                                         CI0084
          05              FR00-SUITE.                                   CI0084
            15       FILLER         PICTURE  X(00222).                  CI0084
       01                 FR01  REDEFINES      FR00.                    CI0084
            10            FR01-CT01K.                                   CI0084
            11            FR01-C299.                                    CI0084
            12            FR01-CTID.                                    CI0084
            13            FR01-CTIDA  PICTURE  9(3).                    CI0084
            13            FR01-CTIDN.                                   CI0084
            14            FR01-CTIDNP PICTURE  X(13).                   CI0084
            14            FR01-CTIDND PICTURE  9(11).                   CI0084
            10            FR01-GECKD  PICTURE  9.                       CI0084
            10            FR01-GEMDA  PICTURE  9(8).                    CI0084
            10            FR01-NSEQ4B PICTURE  9(8)                     CI0084
                          BINARY.                                       CI0084
            10            FR01-GECUC  PICTURE  99.                      CI0084
            10            FR01-CTAUL  PICTURE  9(3).                    CI0084
            10            FR01-DIRAC  PICTURE  9(4).                    CI0084
            10            FR01-CTCCI  PICTURE  X.                       CI0084
            10            FR01-CTCUS  PICTURE  999.                     CI0084
            10            FR01-CTEFD  PICTURE  9(8).                    CI0084
            10            FR01-CTIAD  PICTURE  9(8).                    CI0084
            10            FR01-CLCUS  PICTURE  99.                      CI0084
            10            FR01-CAMMB  PICTURE  X(3).                    CI0084
            10            FR01-CKPMM  PICTURE  X.                       CI0084
            10            FR01-CTLAD  PICTURE  9(8).                    CI0084
            10            FR01-IPERS  PICTURE  X.                       CI0084
            10            FR01-AUNCB  PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            FR01-CTLAT  PICTURE  9(8).                    CI0084
            10            FR01-CTLATC PICTURE  9(6).                    CI0084
            10            FR01-IMEGA  PICTURE  X.                       CI0084
            10            FR01-DIRAB  PICTURE  9(8).                    CI0084
            10            FR01-COLRQ  PICTURE  X.                       CI0084
            10            FR01-ZDA04  PICTURE  X(4).                    CI0084
            10            FR01-CTLPD  PICTURE  9(8).                    CI0084
            10            FR01-CIRASP PICTURE  9.                       CI0084
            10            FR01-CIRATP PICTURE  99.                      CI0084
            10            FR01-DRTHC  PICTURE  9(8).                    CI0084
            10            FR01-CPPTC  PICTURE  X.                       CI0084
            10            FR01-ZDA06  PICTURE  X(6).                    CI0084
            10            FR01-CTACD  PICTURE  9(8).                    CI0084
            10            FR01-CTNLI  PICTURE  X.                       CI0084
            10            FR01-CTRHO  PICTURE  9(8).                    CI0084
            10            FR01-CTSGD  PICTURE  9(8).                    CI0084
            10            FR01-CPATP  PICTURE  X(1).                    CI0084
            10            FR01-IRSTA  PICTURE  X.                       CI0084
            10            FR01-CTSTA  PICTURE  99.                      CI0084
            10            FR01-CTSSC  PICTURE  99.                      CI0084
            10            FR01-PRLIN  PICTURE  9(3).                    CI0084
            10            FR01-PRCOD  PICTURE  9(5).                    CI0084
            10            FR01-PRSCD  PICTURE  X(9).                    CI0084
            10            FR01-CTLNI  PICTURE  X.                       CI0084
            10            FR01-AYSIDA PICTURE  9(3).                    CI0084
            10            FR01-AYSID  PICTURE  9(5).                    CI0084
            10            FR01-CTBMC  PICTURE  99.                      CI0084
            10            FR01-CINAR  PICTURE  99.                      CI0084
            10            FR01-CPHTR  PICTURE  X.                       CI0084
            10            FR01-CDSTR  PICTURE  XX.                      CI0084
            10            FR01-CQACT  PICTURE  999.                     CI0084
            10            FR01-CIRAS  PICTURE  999.                     CI0084
            10            FR01-CIRAT  PICTURE  999.                     CI0084
            10            FR01-CLRAY  PICTURE  9(5).                    CI0084
            10            FR01-CATTP  PICTURE  X.                       CI0084

      ******************************************************************
      **     THIS SEGMENT CONTAINS THE TO ACCOUNT CONTRACT SEGMENT   *
      ******************************************************************
      *!WF DSP=TO DSL=CT SEL=01 FOR=I LEV=1 PLT=80
       01                 TO00.                                         CI0084
          05              TO00-SUITE.                                   CI0084
            15       FILLER         PICTURE  X(00222).                  CI0084
       01                 TO01  REDEFINES      TO00.                    CI0084
            10            TO01-CT01K.                                   CI0084
            11            TO01-C299.                                    CI0084
            12            TO01-CTID.                                    CI0084
            13            TO01-CTIDA  PICTURE  9(3).                    CI0084
            13            TO01-CTIDN.                                   CI0084
            14            TO01-CTIDNP PICTURE  X(13).                   CI0084
            14            TO01-CTIDND PICTURE  9(11).                   CI0084
            10            TO01-GECKD  PICTURE  9.                       CI0084
            10            TO01-GEMDA  PICTURE  9(8).                    CI0084
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0084
                          BINARY.                                       CI0084
            10            TO01-GECUC  PICTURE  99.                      CI0084
            10            TO01-CTAUL  PICTURE  9(3).                    CI0084
            10            TO01-DIRAC  PICTURE  9(4).                    CI0084
            10            TO01-CTCCI  PICTURE  X.                       CI0084
            10            TO01-CTCUS  PICTURE  999.                     CI0084
            10            TO01-CTEFD  PICTURE  9(8).                    CI0084
            10            TO01-CTIAD  PICTURE  9(8).                    CI0084
            10            TO01-CLCUS  PICTURE  99.                      CI0084
            10            TO01-CAMMB  PICTURE  X(3).                    CI0084
            10            TO01-CKPMM  PICTURE  X.                       CI0084
            10            TO01-CTLAD  PICTURE  9(8).                    CI0084
            10            TO01-IPERS  PICTURE  X.                       CI0084
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            TO01-CTLAT  PICTURE  9(8).                    CI0084
            10            TO01-CTLATC PICTURE  9(6).                    CI0084
            10            TO01-IMEGA  PICTURE  X.                       CI0084
            10            TO01-DIRAB  PICTURE  9(8).                    CI0084
            10            TO01-COLRQ  PICTURE  X.                       CI0084
            10            TO01-ZDA04  PICTURE  X(4).                    CI0084
            10            TO01-CTLPD  PICTURE  9(8).                    CI0084
            10            TO01-CIRASP PICTURE  9.                       CI0084
            10            TO01-CIRATP PICTURE  99.                      CI0084
            10            TO01-DRTHC  PICTURE  9(8).                    CI0084
            10            TO01-CPPTC  PICTURE  X.                       CI0084
            10            TO01-ZDA06  PICTURE  X(6).                    CI0084
            10            TO01-CTACD  PICTURE  9(8).                    CI0084
            10            TO01-CTNLI  PICTURE  X.                       CI0084
            10            TO01-CTRHO  PICTURE  9(8).                    CI0084
            10            TO01-CTSGD  PICTURE  9(8).                    CI0084
            10            TO01-CPATP  PICTURE  X(1).                    CI0084
            10            TO01-IRSTA  PICTURE  X.                       CI0084
            10            TO01-CTSTA  PICTURE  99.                      CI0084
            10            TO01-CTSSC  PICTURE  99.                      CI0084
            10            TO01-PRLIN  PICTURE  9(3).                    CI0084
            10            TO01-PRCOD  PICTURE  9(5).                    CI0084
            10            TO01-PRSCD  PICTURE  X(9).                    CI0084
            10            TO01-CTLNI  PICTURE  X.                       CI0084
            10            TO01-AYSIDA PICTURE  9(3).                    CI0084
            10            TO01-AYSID  PICTURE  9(5).                    CI0084
            10            TO01-CTBMC  PICTURE  99.                      CI0084
            10            TO01-CINAR  PICTURE  99.                      CI0084
            10            TO01-CPHTR  PICTURE  X.                       CI0084
            10            TO01-CDSTR  PICTURE  XX.                      CI0084
            10            TO01-CQACT  PICTURE  999.                     CI0084
            10            TO01-CIRAS  PICTURE  999.                     CI0084
            10            TO01-CIRAT  PICTURE  999.                     CI0084
            10            TO01-CLRAY  PICTURE  9(5).                    CI0084
            10            TO01-CATTP  PICTURE  X.                       CI0084
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0084
          05              MS00-SUITE.                                   CI0084
            15       FILLER         PICTURE  X(00542).                  CI0084
       01                 MS03  REDEFINES      MS00.                    CI0084
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            10            MS03-CMSSF  PICTURE  XX.                      CI0084
            10            MS03-DU09.                                    CI0084
            11            MS03-CMESA  PICTURE  S9(9)                    CI0084
                          BINARY.                                       CI0084
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0084
                          BINARY.                                       CI0084
            11            MS03-CMESB  PICTURE  S9(9)                    CI0084
                          BINARY.                                       CI0084
            11            MS03-CMSST  PICTURE  S9(9)                    CI0084
                          BINARY.                                       CI0084
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0084
                          BINARY.                                       CI0084
            11            MS03-QELLAA PICTURE  S9(9)                    CI0084
                          BINARY.                                       CI0084
            11            MS03-TMESS4 PICTURE  X(512).                  CI0084
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0084
            10            MX11-QMSGS  PICTURE  9(03).                   CI0084
            10            MX11-PJ09                                     CI0084
                          OCCURS       025     TIMES.                   CI0084
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0084
                          COMPUTATIONAL-3.                              CI0084
            11            MX11-CMESB  PICTURE  S9(9)                    CI0084
                          BINARY.                                       CI0084
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                PJ06
                                CX13
                                CX14
                                OX13
                                OX14
                                FR01
                                TO01
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0084
      *               *                                   *             CI0084
      *               *INITIALISATIONS                    *             CI0084
      *               *                                   *             CI0084
      *               *************************************.            CI0084
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
      *N03.      NOTE *************************************.
      *               *                                   *
      *               *INITIALISE VALUES                  *
      *               *                                   *
      *               *************************************.
       F03.           EXIT.                                             lv05
      *N03DD.    NOTE *INITIALISE DIBURSEMENT AMOUNT      *.
       F03DD.    IF    CX13-CARTZ = 01 OR 02 OR 10                      lv10
                       OR 14
                 NEXT SENTENCE ELSE GO TO     F03DD-FN.
      *FOR FUND,CERT,BROK AND BETA BROK
      *N03DH.    NOTE *MOVE AMOUNT FIELD                  *.
       F03DH.    IF    CX13-ACOT1 NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F03DH-FN.
                 IF    CX13-ACOT1 > 0                                   DOT
           MOVE        CX13-ACOT1 TO WS10-AEDRQ.
       F03DH-FN. EXIT.
      *N03DJ.    NOTE *CALCULATE AMOUNT IF SHARES         *.
       F03DJ.    IF    CX13-QPST1 NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F03DJ-FN.
                 IF    CX13-QPST1 > 0                                   DOT
      *IF NO OF SHARES > 0 MULTIPLY
      *NO OF SHARES BY SHARE PRICE
           COMPUTE     WS10-AEDRQ =
           CX13-QPST1 * PJ06-AFAVP.
       F03DJ-FN. EXIT.
      *N03DN.    NOTE *CACULATE IF % OF ACCOUNT           *.
       F03DN.    IF    CX13-PPOT1 NUMERIC                               lv15
                 NEXT SENTENCE ELSE GO TO     F03DN-FN.
                 IF    CX13-PPOT1 > 0                                   DOT
      *IF % > 0 MULTIPLY TOT SHARES OF
      *ACCT BY % AND THEN SHARE PRICE
           COMPUTE     WS10-AEDRQ =
           (PJ06-QSHOW * CX13-PPOT1 / 100)
           * PJ06-AFAVP.
       F03DN-FN. EXIT.
       F03DD-FN. EXIT.
       F03-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0084
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0084
      *               *                                   *             CI0084
      *               *FIN DE TRAITEMENT                  *             CI0084
      *               *                                   *             CI0084
      *               *************************************.            CI0084
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0084
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *EDITTING FOR A CX13                *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** THIS AREA WILL BE USED FOR   *
      ** EDITTING THE PASSED CX13 AREA*
      ** FOR VALID FIELDS             *
      *********************************
      *
      *N50DA.    NOTE *VALIDATE CARTZ                     *.
       F50DA.    IF    CX13-CARTZ = 01                                  lv10
                 OR    CX13-CARTZ = 02
                 OR    CX13-CARTZ = 05
                 OR    CX13-CARTZ = 06
                 OR    CX13-CARTZ = 07
                 OR    CX13-CARTZ = 10
                 OR    CX13-CARTZ = 14
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
       F50DA-900. GO TO F50DE-FN.
       F50DA-FN. EXIT.
      *N50DE.    NOTE *ELSE... INVALID CARTZ              *.
       F50DE.                                                           lv10
           MOVE        12035 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50DE-FN. EXIT.
      *N50DI.    NOTE *VALIDATE NAPDS                     *.
       F50DI.    IF    CX13-NAPDS NUMERIC                               lv10
                 AND   CX13-NAPDS > ZERO
                 NEXT SENTENCE ELSE GO TO     F50DI-FN.
       F50DI-900. GO TO F50DL-FN.
       F50DI-FN. EXIT.
      *N50DL.    NOTE *ELSE... INVALID CARTZ              *.
       F50DL.                                                           lv10
           MOVE        12036 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50DL-FN. EXIT.
      *N50EA.    NOTE *VALIDATE GESTD - START DATE        *.
       F50EA.    IF    CX13-GESTD > ZERO                                lv10
                 NEXT SENTENCE ELSE GO TO     F50EA-FN.
           MOVE        CX13-GESTD TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N50EC.    NOTE *IF BAD DATE; ERROR OUT             *.
       F50EC.    IF    DEL-ER NOT = 1                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50EC-FN.
           MOVE        012537 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50EC-FN. EXIT.
      *N50EF.    NOTE *IF AN ADD                          *.
       F50EF.    IF    PJ06-CACTA = 'A'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50EF-FN.
      *N50EH.    NOTE *IF START DATE BEFORE AUTHORIZED    *.
       F50EH.    IF    CX13-GESTD < PJ06-DAUTB                          lv20
                 NEXT SENTENCE ELSE GO TO     F50EH-FN.
           MOVE        012537 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50EH-FN. EXIT.
       F50EF-FN. EXIT.
       F50EA-900. GO TO F50ET-FN.
       F50EA-FN. EXIT.
      *N50ET.    NOTE *ELSE... INVALID START DATE         *.
       F50ET.         EXIT.                                             lv10
      *N50EV.    NOTE *IF NOT A CERT INTEREST             *.
       F50EV.    IF    CX13-CARTZ NOT = 06                              lv15
                 NEXT SENTENCE ELSE GO TO     F50EV-FN.
           MOVE        012537 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50EV-FN. EXIT.
       F50ET-FN. EXIT.
      *N50FA.    NOTE *VALIDATE GEEND - END DATE          *.
       F50FA.    IF    CX13-GEEND NUMERIC                               lv10
                 AND   CX13-GEEND > ZERO
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
           MOVE        CX13-GEEND TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N50FC.    NOTE *IF BAD DATE; ERROR OUT             *.
       F50FC.    IF    DEL-ER NOT = 1                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50FC-FN.
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50FC-FN. EXIT.
      *N50FE.    NOTE *ENDING BEFORE NEXT PAYMENT         *.
       F50FE.    IF    (CX13-CARTZ = 01                                 lv15
                 OR    CX13-CARTZ = 02
                 OR    CX13-CARTZ = 07
                 OR    CX13-CARTZ = 10
                 OR    CX13-CARTZ = 14)
                 AND   CX13-GEEND < CX13-DNPMT
                 AND   PJ06-CACTA NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F50FE-FN.
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50FE-FN. EXIT.
       F50FA-900. GO TO F50FH-FN.
       F50FA-FN. EXIT.
      *N50FH.    NOTE *ELSE...INVALID - END DATE          *.
       F50FH.                                                           lv10
                 IF    CX13-GEEND NOT NUMERIC                           DOT
           MOVE        012205 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50FH-FN. EXIT.
      *N50FM.    NOTE *VALIDATE DASUQ - SETUP DATE        *.
       F50FM.    IF    CX13-DASUQ > ZERO                                lv10
                 NEXT SENTENCE ELSE GO TO     F50FM-FN.
           MOVE        CX13-DASUQ TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N50FR.    NOTE *IF BAD DATE; ERROR OUT             *.
       F50FR.    IF    DEL-ER NOT = 1                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50FR-FN.
           MOVE        012144 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50FR-FN. EXIT.
       F50FM-FN. EXIT.
      *N50GA.    NOTE *ENSURE STATUS IS VALID             *.
       F50GA.    IF    CX13-CDEST = 01                                  lv10
                 OR    CX13-CDEST = 02
                 OR    CX13-CDEST = 03
                 NEXT SENTENCE ELSE GO TO     F50GA-FN.
      *N50GC.    NOTE *IF ARRANGEMENT IS ACTIVE           *.
       F50GC.    IF    CX13-CDEST = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50GC-FN.
      *N50GD.    NOTE *ERROR, IF CONTRACT NOT ACTIVE      *.
       F50GD.    IF    (PJ06-CTSTA NOT = 02                             lv20
                 AND   CX13-CARTZ NOT = 05)
                 AND   (PJ06-CTIDA NOT = 001
                 AND   FR01-PRCOD NOT = 181
                 AND   FR01-PRCOD NOT = 961)
                 NEXT SENTENCE ELSE GO TO     F50GD-FN.
      *AND NOT MARKET STRATEGY
           MOVE        012024 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50GD-FN. EXIT.
       F50GC-FN. EXIT.
      *N50GE.    NOTE *IF CDEST IS PENDING                *.
       F50GE.    IF    CX13-CDEST = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50GE-FN.
      *N50GG.    NOTE *IF CONTRACT STATUS IS OK           *.
       F50GG.    IF    (PJ06-CTIDA = 001                                lv20
                 AND   PJ06-CTSTA = 01)
                 OR    (PJ06-CTIDA = 002
                 AND   (PJ06-CTSTA = 01
                 OR    PJ06-CTSTA = 03))
                 NEXT SENTENCE ELSE GO TO     F50GG-FN.
      *- FUNDS CAN BE INACTIVE OR
      *  PENDING
      *- CERTS CAN BE PENDING ONLY
       F50GG-900. GO TO F50GI-FN.
       F50GG-FN. EXIT.
      *N50GI.    NOTE *ELSE...BAD STATUS                  *.
       F50GI.                                                           lv20
           MOVE        012024 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50GI-FN. EXIT.
       F50GE-FN. EXIT.
      *N50GM.    NOTE *IF CDEST IS INACTIVE               *.
       F50GM.    IF    CX13-CDEST = 03                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50GM-FN.
      *N50GO.    NOTE *IF DOING ADD OR RE-ACTIVATE        *.
       F50GO.    IF    PJ06-CACTA = 'A'                                 lv20
                 OR    PJ06-CACTA = 'R'
                 NEXT SENTENCE ELSE GO TO     F50GO-FN.
           MOVE        012024 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50GO-FN. EXIT.
       F50GM-FN. EXIT.
      *N50GQ.    NOTE *IF CHANGE; STATUS SHOULD BE SAME   *.
       F50GQ.    IF    PJ06-CACTA = 'C'                                 lv15
                 AND   CX13-CDEST NOT = OX13-CDEST
                 NEXT SENTENCE ELSE GO TO     F50GQ-FN.
           MOVE        012024 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50GQ-FN. EXIT.
       F50GA-900. GO TO F50GY-FN.
       F50GA-FN. EXIT.
      *N50GY.    NOTE *ELSE... STATUS IS BAD              *.
       F50GY.                                                           lv10
           MOVE        012024 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50GY-FN. EXIT.
      *N50HA.    NOTE *ENSURE VALID COMPLETENESS          *.
       F50HA.    IF    CX13-IIARR = 'N'                                 lv10
                 OR    CX13-IIARR = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50HA-FN.
       F50HA-900. GO TO F50HG-FN.
       F50HA-FN. EXIT.
      *N50HG.    NOTE *ELSE... COMPLETENESS INVALID       *.
       F50HG.                                                           lv10
           MOVE        012720 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50HG-FN. EXIT.
      *N50IA.    NOTE *VALIDATE AMOUNTS                   *.
       F50IA.    IF    CX13-CARTZ NOT = 06                              lv10
                 NEXT SENTENCE ELSE GO TO     F50IA-FN.
      *N50IC.    NOTE *VALIDATE $, % OR SHARE AMT > 0     *.
       F50IC.    IF    CX13-ACOT1 NUMERIC                               lv15
                 AND   CX13-PPOT1 NUMERIC
                 AND   CX13-QPST1 NUMERIC
                 AND   (CX13-ACOT1 > ZERO
                 OR    CX13-PPOT1 > ZERO
                 OR    CX13-QPST1 > ZERO)
                 NEXT SENTENCE ELSE GO TO     F50IC-FN.
      *AT LEAST ONE OF THESE AMOUNTS
      *SHOULD HAVE A VALUE; IF NOT
      *SOME SORT OF PROBLEMS EXISTS
      *N50IE.    NOTE *VALIDATE $ AMOUNT                  *.
       F50IE.    IF    CX13-ACOT1 > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F50IE-FN.
      *N50IG.    NOTE *IF PCT OR SHARES EXISTS; ERROR     *.
       F50IG.    IF    CX13-PPOT1 > ZERO                                lv25
                 OR    CX13-QPST1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F50IG-FN.
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50IG-FN. EXIT.
       F50IE-FN. EXIT.
      *N50JA.    NOTE *VALIDATE PERCENT AMOUNT            *.
       F50JA.    IF    CX13-PPOT1 > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F50JA-FN.
      *N50JE.    NOTE *IF $'S OR SHRS EXIST; ERROR        *.
       F50JE.    IF    CX13-ACOT1 > ZERO                                lv25
                 OR    CX13-QPST1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F50JE-FN.
           MOVE        012061 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50JE-FN. EXIT.
      *N50JG.    NOTE *IF FUNDS; PERCENTAGE MUST BE 100   *.
       F50JG.    IF    ((CX13-CARTZ = 01                                lv25
                 OR    CX13-CARTZ = 05)
                 AND   CX13-PPOT1 NOT = +100)
                 NEXT SENTENCE ELSE GO TO     F50JG-FN.
           MOVE        012061 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50JG-FN. EXIT.
      *N50JI.    NOTE *NO CERT PARTIALS ALLOWED           *.
       F50JI.    IF    CX13-CARTZ = 02                                  lv25
                 NEXT SENTENCE ELSE GO TO     F50JI-FN.
           MOVE        012061 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50JI-FN. EXIT.
      *N50JM.    NOTE *IF ADD OR CHANGE TO DATE           *.
       F50JM.    IF    PJ06-CACTA = 'A'                                 lv25
                 OR    (PJ06-CACTA NOT = 'A'
                 AND   OX13-DNPMT NOT = CX13-DNPMT)
                 NEXT SENTENCE ELSE GO TO     F50JM-FN.
           MOVE        CX13-DNPMT (7 : 2) TO W-DOPDA.
      *N50JO.    NOTE *IF TERM OR PCT ALREADY EXISTS      *.
       F50JO.    IF    PJ06-CPDOMT (W-DOPDA : 1)                        lv30
                       = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50JO-FN.
           MOVE        012144 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50JO-FN. EXIT.
       F50JM-FN. EXIT.
       F50JA-FN. EXIT.
      *N50KA.    NOTE *VALIDATE SHARES                    *.
       F50KA.    IF    CX13-QPST1 > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F50KA-FN.
      *N50KE.    NOTE *IF DOLLARS OR PERCENT EXISTS       *.
       F50KE.    IF    CX13-ACOT1 > ZERO                                lv25
                 OR    CX13-PPOT1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F50KE-FN.
           MOVE        012067 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50KE-FN. EXIT.
      *N50KG.    NOTE *ONLY FUNDS SP ALLOWED              *.
       F50KG.    IF    CX13-CARTZ NOT = 01                              lv25
                 NEXT SENTENCE ELSE GO TO     F50KG-FN.
           MOVE        012067 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50KG-FN. EXIT.
       F50KA-FN. EXIT.
       F50IC-900. GO TO F50KI-FN.
       F50IC-FN. EXIT.
      *N50KI.    NOTE *ELSE... BAD AMOUNTS                *.
       F50KI.                                                           lv15
           MOVE        013165 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50KI-FN. EXIT.
       F50IA-FN. EXIT.
      *N50KM.    NOTE *IF FUND SP                         *.
       F50KM.    IF    CX13-CARTZ = 01                                  lv10
                 NEXT SENTENCE ELSE GO TO     F50KM-FN.
      *N50KQ.    NOTE *IF TERM SELECTED:; % S/B 100       *.
       F50KQ.    IF    CX13-QMTH1 > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F50KQ-FN.
      *N50KT.    NOTE *IF PERCENTAGE NOT A 100; PROBLEM   *.
       F50KT.    IF    CX13-PPOT1 NOT = +100                            lv20
                 NEXT SENTENCE ELSE GO TO     F50KT-FN.
           MOVE        012221 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50KT-FN. EXIT.
       F50KQ-FN. EXIT.
       F50KM-FN. EXIT.
      *N50LA.    NOTE *IF CERT OR FUND SP OR ANNT SPO     *.
       F50LA.    IF    CX13-CARTZ = 01                                  lv10
                 OR    CX13-CARTZ = 02
                 OR    CX13-CARTZ = 07
                 OR    CX13-CARTZ = 10
                 OR    CX13-CARTZ = 14
                 NEXT SENTENCE ELSE GO TO     F50LA-FN.
      *OR BROK CP OR BETA BROK CP
      *N50LD.    NOTE *VALIDATE REQUESTED AMOUNTS         *.
       F50LD.    IF    CX13-ADBRQ NUMERIC                               lv15
                 AND   CX13-QSHOWQ NUMERIC
                 AND   CX13-PACT1 NUMERIC
                 NEXT SENTENCE ELSE GO TO     F50LD-FN.
      *N50LG.    NOTE *VALIDATE $ AMOUNT                  *.
       F50LG.    IF    CX13-ADBRQ > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F50LG-FN.
      *N50LJ.    NOTE *IF PERCENT OR SHARES EXISTS        *.
       F50LJ.    IF    CX13-QSHOWQ > ZERO                               lv25
                 OR    CX13-PACT1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F50LJ-FN.
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50LJ-FN. EXIT.
       F50LG-FN. EXIT.
      *N50LN.    NOTE *VALIDATE PERCENT AMOUNT            *.
       F50LN.    IF    CX13-PACT1 > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F50LN-FN.
      *N50LP.    NOTE *IF DOLLARS OR SHARES EXISTS        *.
       F50LP.    IF    CX13-ADBRQ > ZERO                                lv25
                 OR    CX13-QSHOWQ > ZERO
                 NEXT SENTENCE ELSE GO TO     F50LP-FN.
           MOVE        012061 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50LP-FN. EXIT.
       F50LN-FN. EXIT.
      *N50MA.    NOTE *VALIDATE SHARES                    *.
       F50MA.    IF    CX13-QSHOWQ > ZERO                               lv20
                 NEXT SENTENCE ELSE GO TO     F50MA-FN.
      *N50ME.    NOTE *IF DOLLARS OR PERCENT EXISTS       *.
       F50ME.    IF    CX13-ADBRQ > ZERO                                lv25
                 OR    CX13-PACT1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F50ME-FN.
           MOVE        012067 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50ME-FN. EXIT.
       F50MA-FN. EXIT.
       F50LD-FN. EXIT.
       F50LA-FN. EXIT.
      *N50MG.    NOTE *CHECK MINIMUMS FOR MARKET          *.
       F50MG.    IF    TO01-CTIDA = 001                                 lv10
                 AND   (TO01-PRCOD = 00181 OR
                       TO01-PRCOD = 00961)
                 AND   PJ06-CPAY1 = 'TR'
                 AND   (CX13-CARTZ = 01 OR 02)
                 NEXT SENTENCE ELSE GO TO     F50MG-FN.
      *STRATEGY
      *N50MJ.    NOTE *CHECK MINIMUM PAYMENT AGAINST      *.
       F50MJ.    IF    WS10-AEDRQ < PJ06-AINPU                          lv15
                 NEXT SENTENCE ELSE GO TO     F50MJ-FN.
      *AMOUNT SET IN CI0067
           MOVE        013691 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50MJ-FN. EXIT.
      *N50PA.    NOTE *VALIDATE DNPMT                     *.
       F50PA.    IF    CX13-DNPMT > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F50PA-FN.
           MOVE        CX13-DNPMT TO DD01-XDATG
           PERFORM     F93DD THRU F93DD-FN.
      *N50PE.    NOTE *IF BAD DATE; ERROR OUT             *.
       F50PE.    IF    DEL-ER NOT = 1                                   lv20
                 NEXT SENTENCE ELSE GO TO     F50PE-FN.
           MOVE        012204 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PE-FN. EXIT.
      *N50PH.    NOTE *IF START DATE BEFORE AUTHORIZED    *.
       F50PH.    IF    CX13-DNPMT < PJ06-DAUTB                          lv20
                 NEXT SENTENCE ELSE GO TO     F50PH-FN.
           MOVE        012204 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PH-FN. EXIT.
      *N50PJ.    NOTE *IF IRREGULAR FREQ; STORE MONTH     *.
       F50PJ.    IF    CX13-CPMTF > 90                                  lv20
                 NEXT SENTENCE ELSE GO TO     F50PJ-FN.
           MOVE        CX13-DNPMT (5 : 2) TO W-DOPDA.
      *N50PM.    NOTE *IF MONTH OF DNPMT IS IRREGULAR     *.
       F50PM.    IF    CX13-CIRMO (W-DOPDA : 1)                         lv25
                       = ' '
                 NEXT SENTENCE ELSE GO TO     F50PM-FN.
           MOVE        012123 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PM-FN. EXIT.
       F50PJ-FN. EXIT.
      *N50PN.    NOTE *IF CERT SD; STORE DAY IN DOPDA     *.
       F50PN.    IF    CX13-CARTZ = 02                                  lv20
                 NEXT SENTENCE ELSE GO TO     F50PN-FN.
           MOVE        CX13-DNPMT (7 : 2) TO W-DOPDA.
      *N50PP.    NOTE *IF ACH SHOULD NOT BE ALLOWED       *.
       F50PP.    IF    (PJ06-CPDOM (W-DOPDA : 1)                        lv25
                       = 'Y'
                 AND   PJ06-CPAY1 = 'D')
                 OR    PJ06-CPDOMA (W-DOPDA : 1)
                       = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50PP-FN.
      *IF ACH PROCESS AND EXISTING
      *ARRANGEMENT OR AN ACH ALREADY
      *EXISTS AND ANY NEW TRAN THEN
      *ERROR
      *
           MOVE        013077 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PP-FN. EXIT.
      *N50PQ.    NOTE *IF MARKET STRATEGY CERTIFICATE     *.
       F50PQ.    IF    PJ06-CTIDA = 001                                 lv25
                 AND   (FR01-PRCOD = 181
                 OR    FR01-PRCOD = 961)
                 AND   PJ06-CPDOM (W-DOPDA : 1)
                       = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50PQ-FN.
      *INTRA-ACCOUNT TRANSFER, ONLY
      *ONE PER DAY
      *
           MOVE        013077 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PQ-FN. EXIT.
      *N50PR.    NOTE *IF DAY ALREADY HAS CHECK           *.
       F50PR.    IF    PJ06-CPDOMC (W-DOPDA : 1)                        lv25
                       = 'Y'
                 AND   PJ06-CDEL1 = 002
                 NEXT SENTENCE ELSE GO TO     F50PR-FN.
           MOVE        013078 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PR-FN. EXIT.
       F50PN-FN. EXIT.
       F50PA-900. GO TO F50PT-FN.
       F50PA-FN. EXIT.
      *N50PT.    NOTE *ELSE... INVALID DNPMT              *.
       F50PT.                                                           lv15
           MOVE        012204 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50PT-FN. EXIT.
      *N50QA.    NOTE *IF FUND; CHECK EXECUTION DATE      *.
       F50QA.    IF    CX13-CARTZ = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50QA-FN.
      *N50QC.    NOTE *ENSURE EXECUTION DATE IS ZERO      *.
       F50QC.    IF    CX13-DNEXE = ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F50QC-FN.
       F50QC-900. GO TO F50QE-FN.
       F50QC-FN. EXIT.
      *N50QE.    NOTE *IF BAD DATE; ERROR OUT             *.
       F50QE.                                                           lv20
           MOVE        012204 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50QE-FN. EXIT.
       F50QA-FN. EXIT.
      *N50QH.    NOTE *IF CERT; CHECK EXECUTION DATE      *.
       F50QH.    IF    CX13-CARTZ = 02 OR 10 OR 07                      lv15
                       OR 14
                 NEXT SENTENCE ELSE GO TO     F50QH-FN.
      *N50QJ.    NOTE *IF EXECUTION MATCHES NEXT PMT      *.
       F50QJ.    IF    CX13-DNEXE = CX13-DNPMT                          lv20
                 NEXT SENTENCE ELSE GO TO     F50QJ-FN.
       F50QJ-900. GO TO F50QM-FN.
       F50QJ-FN. EXIT.
      *N50QM.    NOTE *IF BAD DATE; ERROR OUT             *.
       F50QM.                                                           lv20
           MOVE        012204 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50QM-FN. EXIT.
       F50QH-FN. EXIT.
      *N50TA.    NOTE *EDIT IRREGULAR SCHEDULE FIELD      *.
       F50TA.                                                           lv15
           MOVE        CX13-CIRMO TO W-WK00-CIRMO.
       F50TA-FN. EXIT.
      *N50TF.    NOTE *ENSURE EACH POSITION IS CORRECT    *.
       F50TF.    IF    JAN                                              lv15
                 AND   FEB
                 AND   MAR
                 AND   APR
                 AND   MAY
                 AND   JUN
                 AND   JUL
                 AND   AUG
                 AND   SEP
                 AND   OCT
                 AND   NOV
                 AND   DEC
                 NEXT SENTENCE ELSE GO TO     F50TF-FN.
      ********************************
      **                             *
      ** CIRMO IS A 12 BYTE FIELD    *
      ** THAT HAS "XXXXXXXXXXXX" IN  *
      ** IT WHEN A NORMAL FREQUENCY  *
      ** IS USED.  IF AN IRREGULAR   *
      ** FREQUENCY IS USED, AT LEAST *
      ** ONE OF THESE CHARACTERS     *
      ** WILL BE SPACED OUT.  EACH   *
      ** POSITION REPRESENTS EACH    *
      ** MONTH                       *
      ********************************
       F50TF-900. GO TO F50TH-FN.
       F50TF-FN. EXIT.
      *N50TH.    NOTE *ELSE. ONE OF THE MONTHS IS WRONG   *.
       F50TH.                                                           lv15
           MOVE        012206 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50TH-FN. EXIT.
      *N50TJ.    NOTE *COUNT NUMBER OF MONTHS MISSING     *.
       F50TJ.                                                           lv15
      *   - THIS COUNT WILL BE USED
      *     BELOW
           INSPECT     CX13-CIRMO TALLYING
                W-WK00-COUNT FOR ALL ' '.
       F50TJ-FN. EXIT.
      *N50TL.    NOTE *IF CIRMO IS INCORRECT              *.
       F50TL.    IF    W-WK00-COUNT = 12                                lv15
                 NEXT SENTENCE ELSE GO TO     F50TL-FN.
           MOVE        012121 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50TL-FN. EXIT.
      *N50TN.    NOTE *IF CIRMO SHOULD HAVE AT LEAST 1    *.
       F50TN.    IF    W-WK00-COUNT = 0                                 lv15
                 AND   CX13-CPMTF = 91
                 NEXT SENTENCE ELSE GO TO     F50TN-FN.
           MOVE        012121 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50TN-FN. EXIT.
      *N50TP.    NOTE *IF SOME MONTHS WERE BLANKED OUT    *.
       F50TP.    IF    W-WK00-COUNT > ZERO                              lv15
                 AND   CX13-CPMTF NOT = 91
                 NEXT SENTENCE ELSE GO TO     F50TP-FN.
      *  BUT FREQUENCY IS NOT IRREG
           MOVE        012206 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F50TP-FN. EXIT.
       F50MG-FN. EXIT.
       F50-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *FREQUENCY EDITTING                 *
      *               *                                   *
      *               *************************************.
       F52.           EXIT.                                             lv05
      *N52CA.    NOTE *IF FUND OR CERT SD AND INTEREST    *.
       F52CA.    IF    CX13-CARTZ = 01                                  lv10
                 OR    CX13-CARTZ = 02
                 OR    CX13-CARTZ = 06
                 OR    CX13-CARTZ = 07
                 NEXT SENTENCE ELSE GO TO     F52CA-FN.
      *- THERE IS NO FREQUENCY FOR
      *  A DIVIDEND
      *  ALSO FOR ANNT SP
      *N52CD.    NOTE *IF CERT; CERT INTEREST FREQ        *.
       F52CD.    IF    CX13-CARTZ = 06                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52CD-FN.
           MOVE        PJ06-CPMTF TO W-CX13-CPMTF.
       F52CD-900. GO TO F52CG-FN.
       F52CD-FN. EXIT.
      *N52CG.    NOTE *ELSE... LOAD FREQUENCY TO WORK     *.
       F52CG.                                                           lv15
           MOVE        CX13-CPMTF TO W-CX13-CPMTF.
       F52CG-FN. EXIT.
      *N52IA.    NOTE *DETERMINE THE FREQUENCY            *.
       F52IA.         EXIT.                                             lv15
      *N52ID.    NOTE *IF MONTHLY                         *.
       F52ID.    IF    W-CX13-CPMTF =                                   lv20
                       12
                 NEXT SENTENCE ELSE GO TO     F52ID-FN.
      *- NOTE: THIS IS DONE FIRST
      *        SINCE IT IS THE MOST
      *        COMMON/LIKELY
      *N52IF.    NOTE *IF FUND LIQUIDATE                  *.
       F52IF.    IF    CX13-CARTZ = 01                                  lv25
                 AND   CX13-QMTH1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F52IF-FN.
      *N52IG.    NOTE *IF MONTHLY NOT ALLOWED             *.
       F52IG.    IF    PJ06-IFQMOT = 'N'                                lv30
                 NEXT SENTENCE ELSE GO TO     F52IG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52IG-FN. EXIT.
       F52IF-900. GO TO F52IJ-FN.
       F52IF-FN. EXIT.
      *N52IJ.    NOTE *ELSE.. STANDARD DISBURSEMENT       *.
       F52IJ.         EXIT.                                             lv25
      *N52IT.    NOTE *IF MONTHLY NOT ALLOWED             *.
       F52IT.    IF    PJ06-IFQMO = 'N'                                 lv30
                 NEXT SENTENCE ELSE GO TO     F52IT-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52IT-FN. EXIT.
       F52IJ-FN. EXIT.
       F52ID-900. GO TO F52IA-FN.
       F52ID-FN. EXIT.
      *N52JA.    NOTE *IF ANNUAL                          *.
       F52JA.    IF    W-CX13-CPMTF =                                   lv20
                       01
                 NEXT SENTENCE ELSE GO TO     F52JA-FN.
      *N52JC.    NOTE *IF FUND LIQUIDATE                  *.
       F52JC.    IF    CX13-CARTZ = 01                                  lv25
                 AND   CX13-QMTH1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F52JC-FN.
      *N52JE.    NOTE *IF MONTHLY NOT ALLOWED             *.
       F52JE.    IF    PJ06-IFQANT = 'N'                                lv30
                 NEXT SENTENCE ELSE GO TO     F52JE-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52JE-FN. EXIT.
       F52JC-900. GO TO F52JG-FN.
       F52JC-FN. EXIT.
      *N52JG.    NOTE *ELSE.. STANDARD DISBURSEMENT       *.
       F52JG.         EXIT.                                             lv25
      *N52JJ.    NOTE *IF ANNUAL NOT ALLOWED              *.
       F52JJ.    IF    PJ06-IFQAN = 'N'                                 lv30
                 NEXT SENTENCE ELSE GO TO     F52JJ-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52JJ-FN. EXIT.
       F52JG-FN. EXIT.
       F52JA-900. GO TO F52IA-FN.
       F52JA-FN. EXIT.
      *N52JN.    NOTE *IF SEMI-ANNUAL                     *.
       F52JN.    IF    W-CX13-CPMTF =                                   lv20
                       02
                 NEXT SENTENCE ELSE GO TO     F52JN-FN.
      *N52JP.    NOTE *IF FUND LIQUIDATE                  *.
       F52JP.    IF    CX13-CARTZ = 01                                  lv25
                 AND   CX13-QMTH1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F52JP-FN.
      *N52JR.    NOTE *IF SEMI-ANNUAL NOT ALLOWED         *.
       F52JR.    IF    PJ06-IFQSAT = 'N'                                lv30
                 NEXT SENTENCE ELSE GO TO     F52JR-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52JR-FN. EXIT.
       F52JP-900. GO TO F52JT-FN.
       F52JP-FN. EXIT.
      *N52JT.    NOTE *ELSE... NORMAL                     *.
       F52JT.         EXIT.                                             lv25
      *N52JW.    NOTE *IF SEMI-ANNUAL NOT ALLOWED         *.
       F52JW.    IF    PJ06-IFQSA = 'N'                                 lv30
                 NEXT SENTENCE ELSE GO TO     F52JW-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52JW-FN. EXIT.
       F52JT-FN. EXIT.
       F52JN-900. GO TO F52IA-FN.
       F52JN-FN. EXIT.
      *N52KA.    NOTE *IF EVERY 4 MOS                     *.
       F52KA.    IF    W-CX13-CPMTF =                                   lv20
                       03
                 NEXT SENTENCE ELSE GO TO     F52KA-FN.
      *N52KG.    NOTE *IF EVERY 4 MOS NOT ALLOWED         *.
       F52KG.    IF    PJ06-IFQFM = 'N'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F52KG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52KG-FN. EXIT.
       F52KA-900. GO TO F52IA-FN.
       F52KA-FN. EXIT.
      *N52KK.    NOTE *IF QUARTERLY                       *.
       F52KK.    IF    W-CX13-CPMTF =                                   lv20
                       04
                 NEXT SENTENCE ELSE GO TO     F52KK-FN.
      *N52KM.    NOTE *IF FUND LIQUIDATE                  *.
       F52KM.    IF    CX13-CARTZ = 01                                  lv25
                 AND   CX13-QMTH1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F52KM-FN.
      *N52KO.    NOTE *IF QUARTERLY NOT ALLOWED           *.
       F52KO.    IF    PJ06-IFQQTT = 'N'                                lv30
                 NEXT SENTENCE ELSE GO TO     F52KO-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52KO-FN. EXIT.
       F52KM-900. GO TO F52KQ-FN.
       F52KM-FN. EXIT.
      *N52KQ.    NOTE *ELSE... NORMAL SD                  *.
       F52KQ.         EXIT.                                             lv25
      *N52KS.    NOTE *IF QUARTERLY NOT ALLOWED           *.
       F52KS.    IF    PJ06-IFQQT = 'N'                                 lv30
                 NEXT SENTENCE ELSE GO TO     F52KS-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52KS-FN. EXIT.
       F52KQ-FN. EXIT.
       F52KK-900. GO TO F52IA-FN.
       F52KK-FN. EXIT.
      *N52LA.    NOTE *IF BI-MONTHLY                      *.
       F52LA.    IF    W-CX13-CPMTF =                                   lv20
                       06
                 NEXT SENTENCE ELSE GO TO     F52LA-FN.
      *N52LC.    NOTE *IF FUND LIQUIDATE                  *.
       F52LC.    IF    CX13-CARTZ = 01                                  lv25
                 AND   CX13-QMTH1 > ZERO
                 NEXT SENTENCE ELSE GO TO     F52LC-FN.
      *N52LE.    NOTE *IF QUARTERLY NOT ALLOWED           *.
       F52LE.    IF    PJ06-IFQBMT = 'N'                                lv30
                 NEXT SENTENCE ELSE GO TO     F52LE-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52LE-FN. EXIT.
       F52LC-900. GO TO F52LG-FN.
       F52LC-FN. EXIT.
      *N52LG.    NOTE *ELSE... NORMAL SD                  *.
       F52LG.         EXIT.                                             lv25
      *N52LI.    NOTE *IF QUARTERLY NOT ALLOWED           *.
       F52LI.    IF    PJ06-IFQBM = 'N'                                 lv30
                 NEXT SENTENCE ELSE GO TO     F52LI-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52LI-FN. EXIT.
       F52LG-FN. EXIT.
       F52LA-900. GO TO F52IA-FN.
       F52LA-FN. EXIT.
      *N52LK.    NOTE *IF BI-FORTNIGHTLY                  *.
       F52LK.    IF    W-CX13-CPMTF =                                   lv20
                       13
                 NEXT SENTENCE ELSE GO TO     F52LK-FN.
      *N52LO.    NOTE *IF BI-FORTNIGHTLY NOT ALLOWED      *.
       F52LO.    IF    PJ06-IFQBF = 'N'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F52LO-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52LO-FN. EXIT.
       F52LK-900. GO TO F52IA-FN.
       F52LK-FN. EXIT.
      *N52LQ.    NOTE *IF SEMI-MONTHLY                    *.
       F52LQ.    IF    W-CX13-CPMTF =                                   lv20
                       24
                 NEXT SENTENCE ELSE GO TO     F52LQ-FN.
      *N52LS.    NOTE *IF SEMI-MONTHLY NOT ALLOWED        *.
       F52LS.    IF    PJ06-IFQSM = 'N'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F52LS-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52LS-FN. EXIT.
      *N52LV.    NOTE *IF THE DAY IS 31; DO NOT ALLOW     *.
       F52LV.    IF    CX13-DOPDA = 31                                  lv25
                 NEXT SENTENCE ELSE GO TO     F52LV-FN.
           MOVE        012641 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52LV-FN. EXIT.
       F52LQ-900. GO TO F52IA-FN.
       F52LQ-FN. EXIT.
      *N52MA.    NOTE *IF BI-WEEKLY                       *.
       F52MA.    IF    W-CX13-CPMTF =                                   lv20
                       26
                 NEXT SENTENCE ELSE GO TO     F52MA-FN.
      *N52MG.    NOTE *IF BI-WEEKLY NOT ALLOWED           *.
       F52MG.    IF    PJ06-IFQBW = 'N'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F52MG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52MG-FN. EXIT.
       F52MA-900. GO TO F52IA-FN.
       F52MA-FN. EXIT.
      *N52MK.    NOTE *IF DAILY; NO GOOD; NOT ALLOWED     *.
       F52MK.    IF    W-CX13-CPMTF =                                   lv20
                       31
                 NEXT SENTENCE ELSE GO TO     F52MK-FN.
           MOVE        012119 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52MK-900. GO TO F52IA-FN.
       F52MK-FN. EXIT.
      *N52MS.    NOTE *IF WEEKLY                          *.
       F52MS.    IF    W-CX13-CPMTF =                                   lv20
                       52
                 NEXT SENTENCE ELSE GO TO     F52MS-FN.
      *N52MV.    NOTE *IF WEEKLY NOT ALLOWED              *.
       F52MV.    IF    PJ06-IFQWK = 'N'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F52MV-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52MV-FN. EXIT.
       F52MS-900. GO TO F52IA-FN.
       F52MS-FN. EXIT.
      *N52NA.    NOTE *IF IRREGULAR MONTHLY               *.
       F52NA.    IF    W-CX13-CPMTF =                                   lv20
                       91
                 NEXT SENTENCE ELSE GO TO     F52NA-FN.
      *N52NG.    NOTE *IF IRREGULAR MONTHLY NOT ALLOWED   *.
       F52NG.    IF    PJ06-IFQIR = 'N'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F52NG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52NG-900. GO TO F52NK-FN.
       F52NG-FN. EXIT.
      *N52NK.    NOTE *ELSE... IRREGULAR MONTHLY OK       *.
       F52NK.         EXIT.                                             lv25
      *N52NO.    NOTE *IF UL LOAN; DO NOT ALLOW           *.
       F52NO.    IF    PJ06-CFLOW = 'U'                                 lv30
                 AND   CX14-CPMTC = 01
                 NEXT SENTENCE ELSE GO TO     F52NO-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52NO-FN. EXIT.
       F52NK-FN. EXIT.
       F52NA-900. GO TO F52IA-FN.
       F52NA-FN. EXIT.
      *N52PA.    NOTE *IF END OF TERM; NOT GOOD; ERROR    *.
       F52PA.    IF    W-CX13-CPMTF =                                   lv20
                       98
                 NEXT SENTENCE ELSE GO TO     F52PA-FN.
      *N52PG.    NOTE *IF END OF TERM NOT ALLOWED         *.
       F52PG.    IF    PJ06-IFQET = 'N'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F52PG-FN.
           MOVE        012651 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52PG-FN. EXIT.
       F52PA-900. GO TO F52IA-FN.
       F52PA-FN. EXIT.
      *N52PT.    NOTE *UNKNOWN; ERROR AND GET OUT         *.
       F52PT.                                                           lv20
           MOVE        012118 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F52PT-FN. EXIT.
       F52IA-FN. EXIT.
       F52CA-FN. EXIT.
       F52-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *EDITTING FOR CX14                  *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *
      *********************************
      ** THIS AREA WILL BE USED FOR   *
      ** VALIDATING THE PASSED CX14   *
      ** FIELDS FOR PROPER VALUES     *
      *********************************
      *N60CA.    NOTE *IF DESTINATION TYPE VALID          *.
       F60CA.    IF    CX14-CPITC NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
           MOVE        012029 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60CA-FN. EXIT.
      *N60DA.    NOTE *VALIDATE SEQUENCE NUMBER           *.
       F60DA.    IF    CX14-NPISQ NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
           MOVE        012040 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60DA-FN. EXIT.
      *N60DM.    NOTE *VALIDATE AMOUNTS                   *.
       F60DM.    IF    CX14-ACOTD NUMERIC                               lv10
                 AND   CX14-PPOTD NUMERIC
                 AND   CX14-QPSTD NUMERIC
                 NEXT SENTENCE ELSE GO TO     F60DM-FN.
      *N60DT.    NOTE *IF FUNDS,ANNT SP,BETA BROK CP      *.
       F60DT.    IF    CX13-CARTZ = 01                                  lv15
                 OR    CX13-CARTZ = 05
                 OR    CX13-CARTZ = 07
                 OR    CX13-CARTZ = 10
                 OR    CX13-CARTZ = 14
                 NEXT SENTENCE ELSE GO TO     F60DT-FN.
      *N60DV.    NOTE *CX13 & CX14 SHOULD BALANCE         *.
       F60DV.    IF    CX14-ACOTD NOT = CX13-ACOT1                      lv20
                 AND   CX14-PPOTD NOT = CX13-PPOT1
                 AND   CX14-QPSTD NOT = CX13-QPST1
                 NEXT SENTENCE ELSE GO TO     F60DV-FN.
           MOVE        012055 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60DV-FN. EXIT.
       F60DT-FN. EXIT.
      *N60EC.    NOTE *VALIDATE $ AMOUNT                  *.
       F60EC.    IF    CX14-ACOTD > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F60EC-FN.
      *N60EE.    NOTE *IF PERCENT OR SHARES EXISTS        *.
       F60EE.    IF    CX14-PPOTD > ZERO                                lv20
                 OR    CX14-QPSTD > ZERO
                 NEXT SENTENCE ELSE GO TO     F60EE-FN.
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60EE-FN. EXIT.
      *N60EG.    NOTE *DIVIDENDS AND INTEREST NO $        *.
       F60EG.    IF    CX13-CARTZ = 05                                  lv20
                 OR    CX13-CARTZ = 06
                 NEXT SENTENCE ELSE GO TO     F60EG-FN.
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60EG-FN. EXIT.
      *N60EI.    NOTE *IF DISB EXCEEDS NET ACCT VALUE     *.
       F60EI.    IF    CX13-CARTZ = 01                                  lv20
                 AND   CX14-ACOTD > PJ06-AACTV
                 NEXT SENTENCE ELSE GO TO     F60EI-FN.
      *- AACTV IS GROSS ACCOUNT VALUE
      *  LESS CERT SHRS & LOI ESCROW SH
                 IF    PJ06-ICRTM = '1'                                 DOT
                 AND   PJ06-QSHES > ZERO
      *HAS BOTH CERT SHRS
      *AND LOI ESCROW SHRS
           MOVE        013893 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    PJ06-ICRTM = '1'                                 DOT
      *HAS CERT SHRS
           MOVE        013143 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    PJ06-QSHES > ZERO                                DOT
      *HAS LOI ESCROW SHRS
           MOVE        013890 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60EI-FN. EXIT.
      *N60EK.    NOTE *IF EXTERNAL DESTINATION            *.
       F60EK.    IF    CX14-CPITC = 01                                  lv20
                 NEXT SENTENCE ELSE GO TO     F60EK-FN.
      *N60EM.    NOTE *IF $'S ARE OUTSIDE OF LIMITS       *.
       F60EM.    IF    CX14-ACOTD < PJ06-AMIND                          lv25
                 NEXT SENTENCE ELSE GO TO     F60EM-FN.
      *
           MOVE        012023 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60EM-FN. EXIT.
       F60EK-900. GO TO F60FA-FN.
       F60EK-FN. EXIT.
      *N60FA.    NOTE *ELSE...                            *.
       F60FA.         EXIT.                                             lv20
      *N60FC.    NOTE *IF MUST EQUAL AMTS ZERO OR LOANS   *.
       F60FC.    IF    PJ06-ALPAGR = ZERO                               lv25
                 OR    CX14-CPMTC = 01
                 NEXT SENTENCE ELSE GO TO     F60FC-FN.
      *CHECK $ AMOUNTS
      *N60FE.    NOTE *IF REGULAR ALLOWED AND REGULAR     *.
       F60FE.    IF    PJ06-IARRGA = 'Y'                                lv30
                 AND   CX14-CPMTC = ZERO
                 NEXT SENTENCE ELSE GO TO     F60FE-FN.
      *N60FJ.    NOTE *IF 9090 TYPE LIFE ACCOUNTS         *.
       F60FJ.    IF    PJ06-CFLOW = 'U'                                 lv35
                 AND   CX14-ACOTD < PJ06-AMIND
                 NEXT SENTENCE ELSE GO TO     F60FJ-FN.
           MOVE        012095 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60FJ-FN. EXIT.
       F60FE-FN. EXIT.
      *N60FN.    NOTE *IF LOAN ALLOWED AND LOAN DEST      *.
       F60FN.    IF    PJ06-IARLNA = 'Y'                                lv30
                 AND   CX14-CPMTC = 01
                 NEXT SENTENCE ELSE GO TO     F60FN-FN.
      *N60FR.    NOTE *IF 9090 TYPE LIFE ACCOUNTS         *.
       F60FR.    IF    PJ06-CFLOW = 'U'                                 lv35
                 AND   CX14-ACOTD < PJ06-AMINAL
                 NEXT SENTENCE ELSE GO TO     F60FR-FN.
       F60FR-FN. EXIT.
       F60FN-FN. EXIT.
       F60FC-FN. EXIT.
       F60FA-FN. EXIT.
       F60EC-FN. EXIT.
      *N60JA.    NOTE *VALIDATE PERCENT AMOUNT            *.
       F60JA.    IF    CX14-PPOTD > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F60JA-FN.
      *N60JC.    NOTE *IF DISB EXCEEDS NET %              *.
       F60JC.    IF    CX13-CARTZ = 01                                  lv20
                 AND   CX13-PACT1 > PJ06-PPOT1
                 NEXT SENTENCE ELSE GO TO     F60JC-FN.
      *- PPOT1 WAS ADJUSTED IN F52 OF
      *  CI9006 IF CERT SHRS OR LOI
      *  ESCROW SHRS EXIST
                 IF    PJ06-ICRTM = '1'                                 DOT
                 AND   PJ06-QSHES > ZERO
      *HAS BOTH CERT SHRS
      *AND LOI ESCROW SHRS
           MOVE        013893 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    PJ06-ICRTM = '1'                                 DOT
      *HAS CERT SHRS
           MOVE        013143 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    PJ06-QSHES > ZERO                                DOT
      *HAS LOI ESCROW SHRS
           MOVE        013890 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JC-FN. EXIT.
      *N60JE.    NOTE *IF DOLLARS OR SHARES EXISTS        *.
       F60JE.    IF    CX14-ACOTD > ZERO                                lv20
                 OR    CX14-QPSTD > ZERO
                 NEXT SENTENCE ELSE GO TO     F60JE-FN.
           MOVE        012061 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JE-FN. EXIT.
      *N60JG.    NOTE *IF $'S ARE OUTSIDE OF LIMITS       *.
       F60JG.    IF    ((CX13-CARTZ = 01                                lv20
                 OR    CX13-CARTZ = 05)
                 AND   CX14-PPOTD NOT = +100)
                 NEXT SENTENCE ELSE GO TO     F60JG-FN.
           MOVE        012061 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JG-FN. EXIT.
      *N60JI.    NOTE *NO CERT PARTIALS ALLOWED           *.
       F60JI.    IF    CX13-CARTZ = 02                                  lv20
                 NEXT SENTENCE ELSE GO TO     F60JI-FN.
           MOVE        012061 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JI-FN. EXIT.
       F60JA-FN. EXIT.
      *N60JM.    NOTE *VALIDATE SHARES                    *.
       F60JM.    IF    CX14-QPSTD > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F60JM-FN.
      *N60JO.    NOTE *IF DOLLARS OR PERCENT EXISTS       *.
       F60JO.    IF    CX14-ACOTD > ZERO                                lv20
                 OR    CX14-PPOTD > ZERO
                 NEXT SENTENCE ELSE GO TO     F60JO-FN.
           MOVE        012067 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JO-FN. EXIT.
      *N60JQ.    NOTE *IF DISB EXCEEDS NET SHARES         *.
       F60JQ.    IF    CX13-CARTZ = 01                                  lv20
                 AND   CX14-QPSTD > PJ06-QSHOW
                 NEXT SENTENCE ELSE GO TO     F60JQ-FN.
      *- QSHOW IS SHRS OWNED LESS CERT
      *  SHRS LESS LOI ESCROW SHRS
                 IF    PJ06-ICRTM = '1'                                 DOT
                 AND   PJ06-QSHES > ZERO
      *HAS BOTH CERT SHRS
      *AND LOI ESCROW SHRS
           MOVE        013893 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    PJ06-ICRTM = '1'                                 DOT
      *HAS CERT SHRS
           MOVE        013143 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
                 IF    PJ06-QSHES > ZERO                                DOT
      *HAS LOI ESCROW SHRS
           MOVE        013890 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JQ-FN. EXIT.
      *N60JV.    NOTE *ONLY FUNDS SP IS ALLOWED           *.
       F60JV.    IF    CX13-CARTZ = 02                                  lv20
                 OR    CX13-CARTZ = 05
                 OR    CX13-CARTZ = 06
                 NEXT SENTENCE ELSE GO TO     F60JV-FN.
           MOVE        012067 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60JV-FN. EXIT.
       F60JM-FN. EXIT.
       F60DM-FN. EXIT.
      *N60KA.    NOTE *IF FUND SP                         *.
       F60KA.    IF    CX13-CARTZ = 01                                  lv10
                 NEXT SENTENCE ELSE GO TO     F60KA-FN.
      *N60KD.    NOTE *IF TERM SELECTED:; % S/B 100%      *.
       F60KD.    IF    CX13-QMTH1 > ZERO                                lv15
                 NEXT SENTENCE ELSE GO TO     F60KD-FN.
      *N60KG.    NOTE *IF PERCENTAGE NOT A 100; PROBLEM   *.
       F60KG.    IF    CX14-PPOTD NOT = +100                            lv20
                 NEXT SENTENCE ELSE GO TO     F60KG-FN.
           MOVE        012221 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60KG-FN. EXIT.
       F60KD-FN. EXIT.
       F60KA-FN. EXIT.
      *N60LA.    NOTE *DESTINATION TYPE: PAYEE            *.
       F60LA.    IF    CX14-CPITC = 01                                  lv10
                 NEXT SENTENCE ELSE GO TO     F60LA-FN.
      *N60MA.    NOTE *IF TYPES DON"T MATCH EARLIER       *.
       F60MA.    IF    CX14-IDELI = PJ06-IDELI                          lv15
                 AND   CX14-CDEL1 NUMERIC
                 AND   (CX14-CDEL1 = PJ06-CDEL1
                 OR    (CX14-CDEL1 = ZERO
                 AND   CX14-IDELI = 'Y'))
                 AND   CX14-NDELS NUMERIC
                 AND   CX14-NDELS = PJ06-NDELS
                 NEXT SENTENCE ELSE GO TO     F60MA-FN.
      *IN FUNCTION 50 OF CI9006 IDELI,
      * CDEL1 AND NDELS WERE VALIDATED
      * AS PART OF CI9006 ACCESS
      *SO, AS LONG AS THEY'RE THE SAME
      * AS CI9006, AND THEY SHOULD BE,
      * THEN IT OK
       F60MA-900. GO TO F60MF-FN.
       F60MA-FN. EXIT.
      *N60MF.    NOTE *ELSE... CX14 DOES IS NOT VALID     *.
       F60MF.                                                           lv15
           MOVE        012028 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60MF-FN. EXIT.
       F60LA-900. GO TO F60OA-FN.
       F60LA-FN. EXIT.
      *N60OA.    NOTE *ELSE ... NOT AN EXTERNAL           *.
       F60OA.         EXIT.                                             lv10
      *N60OD.    NOTE *VALIDATE TRANSFER                  *.
       F60OD.    IF    CX14-CPITC = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F60OD-FN.
      *N60OG.    NOTE *IF VALID PAYMENT TYPE              *.
       F60OG.    IF    CX14-CPMTC NUMERIC                               lv20
                 NEXT SENTENCE ELSE GO TO     F60OG-FN.
      *N60OJ.    NOTE *IF REGULAR NOT ALLOWED             *.
       F60OJ.    IF    CX14-CPMTC = 00                                  lv25
                 AND   PJ06-IARRGA NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F60OJ-FN.
           MOVE        012090 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60OJ-FN. EXIT.
      *N60OM.    NOTE *IF LOAN NOT ALLOWED                *.
       F60OM.    IF    CX14-CPMTC = 01                                  lv25
                 AND   PJ06-IARLNA NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F60OM-FN.
           MOVE        012091 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60OM-FN. EXIT.
       F60OG-900. GO TO F60OT-FN.
       F60OG-FN. EXIT.
      *N60OT.    NOTE *ELSE... INVALID TYPE               *.
       F60OT.                                                           lv20
           MOVE        012057 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60OT-FN. EXIT.
      *N60PA.    NOTE *VALIDATE ADMIN                     *.
       F60PA.    IF    CX14-CTIDA NOT = 001                             lv20
                 AND   NOT = 002
                 AND   NOT = 004
                 AND   NOT = 005
                 AND   NOT = 013
                 AND   NOT = 021
                 AND   NOT = 133
                 NEXT SENTENCE ELSE GO TO     F60PA-FN.
           MOVE        012089 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60PA-FN. EXIT.
      *N60QA.    NOTE *IS SIDE FUND FIELD INVALID         *.
       F60QA.    IF    CX14-IACSD NOT = 'Y'                             lv20
                 AND   CX14-IACSD NOT = 'N'
                 NEXT SENTENCE ELSE GO TO     F60QA-FN.
           MOVE        013068 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60QA-FN. EXIT.
      *N60RA.    NOTE *OWNERSHIP SHOULD BE YES OR NO      *.
       F60RA.    IF    CX14-IOWNC NOT = 'N'                             lv20
                 AND   CX14-IOWNC NOT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F60RA-FN.
           MOVE        013069 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60RA-FN. EXIT.
      *N60RG.    NOTE *TRANSFER TYPE SHOULD BE D/E/T      *.
       F60RG.    IF    CX14-CTYPE NOT = 'D'                             lv20
                 AND   CX14-CTYPE NOT = 'E'
                 AND   CX14-CTYPE NOT = 'T'
                 NEXT SENTENCE ELSE GO TO     F60RG-FN.
           MOVE        013070 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60RG-FN. EXIT.
       F60OD-900. GO TO F60SA-FN.
       F60OD-FN. EXIT.
      *N60SA.    NOTE *ERR: ONLY CPITC=01/02 ALLOWED      *.
       F60SA.                                                           lv15
           MOVE        012029 TO MS03-NMESS2
           PERFORM     F98ET THRU F98ET-FN.
       F60SA-FN. EXIT.
       F60OA-FN. EXIT.
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
      *N93.      NOTE *************************************.
      *               *                                   *
      *               *MISCELLANEOUS ROUTINES             *
      *               *                                   *
      *               *************************************.
       F93.           EXIT.                                             lv05
      *N93DD.    NOTE *DATE VALIDATION                    *.            AADA56
       F93DD.                                                           lv10
           MOVE        1 TO DEL-ER.                                     AADA56
                 IF    DD01-XDATG NOT NUMERIC                           DOT
           MOVE        4 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT1 > '99'                                DOT
                 OR    DD01-XDAT1 < '18'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT3 > '12'                                DOT
                 OR    DD01-XDAT3 = '00'                                AADA56
                 OR    DD01-XDAT4 > '31'                                AADA56
                 OR    DD01-XDAT4 = '00'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT4 > '30'                                DOT
                 AND   (DD01-XDAT3 = '04'                               AADA56
                 OR    DD01-XDAT3 = '06'                                AADA56
                 OR    DD01-XDAT3 = '09'                                AADA56
                 OR    DD01-XDAT3 = '11')                               AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT3 NOT = '02'                            DOT
               GO TO     F93DD-FN.                                      AADA56
                 IF    DD01-XDAT4 > '29'                                DOT
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
           MOVE        DD01-XDAT29 TO DD01-DTGYY                        DOT
           MOVE        DD01-XDAT19 TO DD01-DTGCC.                       AADA56
                 IF    DD01-DTGYY NOT = ZERO                            DOT
           COMPUTE     DD01-XLEAPY = DD01-DTGCY -                       AADA56
           ((DD01-DTGCY / 4) * 4)                                       AADA56
                 ELSE                                                   AADA56
           COMPUTE     DD01-XLEAPY = (DD01-DTGCY -                      AADA56
           ((DD01-DTGCY / 400) * 400))                                  AADA56
           / 100.                                                       AADA56
                 IF    DD01-XLEAPY NOT = ZERO                           DOT
                 AND   DD01-XDAT4 > '28'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F93DD-FN.                                      AADA56
       F93DD-FN. EXIT.
       F93-FN.   EXIT.
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
