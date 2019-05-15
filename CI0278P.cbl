       IDENTIFICATION DIVISION.                                         CI0278
       PROGRAM-ID.  CI0278P.                                            CI0278
      *AUTHOR.         ARRANGEMENTS IMPLICATIONS.                       CI0278
      *DATE-COMPILED.   09/08/14.                                       CI0278
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2000                          *ACOPYP
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
      *     COPR. 2000                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0278
       CONFIGURATION SECTION.                                           CI0278
       SOURCE-COMPUTER. IBM-370.                                        CI0278
       OBJECT-COMPUTER. IBM-370.                                        CI0278
       DATA DIVISION.                                                   CI0278
       WORKING-STORAGE SECTION.                                         CI0278
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
      ******************************************************************ADUTAB
      **              TABLE TA8A ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA8A-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=8A FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA8A.                                                CI0278
           04    G-TA8A-PARAM.                                          CI0278
             10  G-TA8A-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0278
                        VALUE      +106.                                CI0278
             10  G-TA8A-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0278
                        VALUE      +001.                                CI0278
             10  G-TA8A-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0278
                        VALUE      +008.                                CI0278
             10  G-TA8A-NUAPP  PICTURE 99                               CI0278
                        VALUE       0.                                  CI0278
             10  G-TA8A-NUTAB  PICTURE X(6)                             CI0278
                        VALUE 'TA008A'.                                 CI0278
             10  G-TA8A-TABFO  PICTURE XX                 VALUE SPACE.  CI0278
             10  G-TA8A-TABCR  PICTURE XX                 VALUE SPACE.  CI0278
             10  G-TA8A-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0278
             10  G-TA8A-NUSSC  PICTURE X  VALUE   ' '.                  CI0278
             10  G-TA8A-NUSSY  PICTURE X                  VALUE SPACE.  CI0278
             10  G-TA8A-TRANID PICTURE X(4)               VALUE SPACE.  CI0278
             10  G-TA8A-FILSYS.                                         CI0278
             15  G-TA8A-USERC  PICTURE X(6)               VALUE SPACE.  CI0278
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0278
           04             TA8A.                                         CI0278
            10            TA8A-GADPR.                                   CI0278
            11            TA8A-CTIDA  PICTURE  9(3)                     CI0278
                          VALUE                ZERO.                    CI0278
            11            TA8A-PRCOD  PICTURE  9(5)                     CI0278
                          VALUE                ZERO.                    CI0278
            10            TA8A-CLIAN  PICTURE  9(02)                    CI0278
                          VALUE                ZERO.                    CI0278
            10            TA8A-CLAST  PICTURE  9(03)                    CI0278
                          VALUE                ZERO.                    CI0278
            10            TA8A-ISMTD  PICTURE  X(1)                     CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-ISUBA  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-IVINS  PICTURE  X(1)                     CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-IEOIR  PICTURE  X(1)                     CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-IDBNL  PICTURE  X(1)                     CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-ICHRC  PICTURE  X(1)                     CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-ICHPN  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-IVAPR  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-ICLSF  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-IIULA  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-IINPS  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-IINLN  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-CINPS  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-CINLN  PICTURE  X                        CI0278
                          VALUE                SPACE.                   CI0278
            10            TA8A-ZDA79  PICTURE  X(79)                    CI0278
                          VALUE                SPACE.                   CI0278
      **                                                                ADUTAB
      *MISCELLANEOUS WORKING STORAGE AREA ***
       01  WS00-WORK-VALUES.
           05  WS00-MAX-MSG                PIC 99 VALUE 20.
           05  WS00-MSG-COUNT              PIC 99.
           05  WS00-NMESS2                 PIC 9(6).
       01  WS01-WORK-VALUES.
           05  W-QT63-APMTL                PIC S9(9)V99 VALUE ZEROES.
       01   DEBUT-WSS.                                                  CI0278
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0278
            05   IK     PICTURE X.                                      CI0278
       01  CONSTANTES-PAC.                                              CI0278
           05  FILLER  PICTURE X(87)   VALUE                            CI0278
                     '6015 CAT09/08/14CI0278ADMIN   14:35:12CI0278P AMERCI0278
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0278
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0278
           05  NUGNA   PICTURE X(5).                                    CI0278
           05  APPLI   PICTURE X(3).                                    CI0278
           05  DATGN   PICTURE X(8).                                    CI0278
           05  PROGR   PICTURE X(6).                                    CI0278
           05  CODUTI  PICTURE X(8).                                    CI0278
           05  TIMGN   PICTURE X(8).                                    CI0278
           05  PROGE   PICTURE X(8).                                    CI0278
           05  COBASE  PICTURE X(4).                                    CI0278
           05  DATGNC  PICTURE X(10).                                   CI0278
           05  RELEAS  PICTURE X(7).                                    CI0278
           05  DATGE   PICTURE X(10).                                   CI0278
           05  DATSQ   PICTURE X(10).                                   CI0278
       01  DATCE.                                                       CI0278
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0278
         05  DATOR.                                                     CI0278
           10  DATOA  PICTURE XX.                                       CI0278
           10  DATOM  PICTURE XX.                                       CI0278
           10  DATOJ  PICTURE XX.                                       CI0278
       01   VARIABLES-CONDITIONNELLES.                                  CI0278
            05                  FT      PICTURE X VALUE '0'.            CI0278
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0278
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0278
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0278
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      ** INPUT SEGMENT FOR CI0278                                      *
      ******************************************************************
      *!WF DSP=QT DSL=QT SEL=63 FOR=I DES=1 LEV=1 PLT=40
       01                 QT63.                                         CI0278
            10            QT63-INPUT.                                   CI0278
            11            QT63-GEMDA  PICTURE  9(8).                    CI0278
            11            QT63-NSEQ4B PICTURE  9(8)                     CI0278
                          BINARY.                                       CI0278
            11            QT63-DCACG  PICTURE  9(8).                    CI0278
            11            QT63-C199.                                    CI0278
            12            QT63-CLID.                                    CI0278
            13            QT63-CLIDO  PICTURE  9(3).                    CI0278
            13            QT63-CLIDN.                                   CI0278
            14            QT63-CLIDNP PICTURE  X(12).                   CI0278
            14            QT63-CLIDND PICTURE  9(8).                    CI0278
            11            QT63-NARRS  PICTURE  S9(3)                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-AACTV  PICTURE  S9(11)V99                CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CTCUS  PICTURE  999.                     CI0278
            11            QT63-CTOWN  PICTURE  9(3).                    CI0278
            11            QT63-CTKRAA PICTURE  X(12).                   CI0278
            11            QT63-AMAXA2 PICTURE  S9(7)V99.                CI0278
            11            QT63-AMAXAL PICTURE  S9(7)V99.                CI0278
            10            QT63-OUTPT1.                                  CI0278
            11            QT63-IARTYA PICTURE  X.                       CI0278
            11            QT63-NMESAA PICTURE  9(6).                    CI0278
            11            QT63-IACHI  PICTURE  X.                       CI0278
            11            QT63-NMESAC PICTURE  9(6).                    CI0278
            11            QT63-IAIND2 PICTURE  X.                       CI0278
            11            QT63-NMESAR PICTURE  9(6).                    CI0278
            11            QT63-IARRGA PICTURE  X.                       CI0278
            11            QT63-NMESA0 PICTURE  9(6).                    CI0278
            11            QT63-IARLNA PICTURE  X.                       CI0278
            11            QT63-NMESA1 PICTURE  9(6).                    CI0278
            11            QT63-IARCDA PICTURE  X.                       CI0278
            11            QT63-NMESA2 PICTURE  9(6).                    CI0278
            11            QT63-IARCPA PICTURE  X.                       CI0278
            11            QT63-NMESA3 PICTURE  9(6).                    CI0278
            11            QT63-IARRG1 PICTURE  X.                       CI0278
            11            QT63-NMESA4 PICTURE  9(6).                    CI0278
            11            QT63-IARLN1 PICTURE  X.                       CI0278
            11            QT63-NMESA5 PICTURE  9(6).                    CI0278
            11            QT63-IARCD1 PICTURE  X.                       CI0278
            11            QT63-NMESA6 PICTURE  9(6).                    CI0278
            11            QT63-IARCP1 PICTURE  X.                       CI0278
            11            QT63-NMESA7 PICTURE  9(6).                    CI0278
            11            QT63-IAINDA PICTURE  X.                       CI0278
            11            QT63-NAPDSK PICTURE  S9(3)                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CDEST1 PICTURE  99.                      CI0278
            11            QT63-CPCCDE PICTURE  99.                      CI0278
            11            QT63-IFQAN  PICTURE  X.                       CI0278
            11            QT63-IFQSA  PICTURE  X.                       CI0278
            11            QT63-IFQQT  PICTURE  X.                       CI0278
            11            QT63-IFQBM  PICTURE  X.                       CI0278
            11            QT63-IFQMO  PICTURE  X.                       CI0278
            11            QT63-IFQSM  PICTURE  X.                       CI0278
            11            QT63-IFQBW  PICTURE  X.                       CI0278
            11            QT63-IFQWK  PICTURE  X.                       CI0278
            11            QT63-IFQOD  PICTURE  X.                       CI0278
            11            QT63-ALMIN  PICTURE  S9(5)                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ALMIN3 PICTURE  S9(5)                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ALPAGQ PICTURE  S9(7)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ALPAGM PICTURE  S9(7)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ALPLDT PICTURE  9(8).                    CI0278
            11            QT63-ALDDUE PICTURE  9(08).                   CI0278
            11            QT63-DGRAC  PICTURE  9(08).                   CI0278
            11            QT63-GESTE  PICTURE  9(8).                    CI0278
            11            QT63-GESTL  PICTURE  9(8).                    CI0278
            11            QT63-GEENE  PICTURE  9(8).                    CI0278
            11            QT63-GEENL  PICTURE  9(8).                    CI0278
            11            QT63-GESTD1 PICTURE  9(8).                    CI0278
            11            QT63-DSKIP  PICTURE  9(8).                    CI0278
            11            QT63-DSKIP1 PICTURE  9(8).                    CI0278
            11            QT63-DSKIP2 PICTURE  9(8).                    CI0278
            11            QT63-DIRAC1 PICTURE  XX.                      CI0278
            11            QT63-CIRAT  PICTURE  999.                     CI0278
            11            QT63-CIRAS  PICTURE  999.                     CI0278
            11            QT63-CQACT  PICTURE  999.                     CI0278
            11            QT63-IERRC  PICTURE  X.                       CI0278
            11            QT63-NMESA  PICTURE  9(6).                    CI0278
            10            QT63-OUTPT2.                                  CI0278
            11            QT63-CPMTG1 PICTURE  99.                      CI0278
            11            QT63-MPMTF1 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL1 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU1 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG2 PICTURE  99.                      CI0278
            11            QT63-MPMTF2 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL2 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU2 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG3 PICTURE  99.                      CI0278
            11            QT63-MPMTF3 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL3 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU3 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG4 PICTURE  99.                      CI0278
            11            QT63-MPMTF4 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL4 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU4 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG5 PICTURE  99.                      CI0278
            11            QT63-MPMTF5 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL5 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU5 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG6 PICTURE  99.                      CI0278
            11            QT63-MPMTF6 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL6 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU6 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG7 PICTURE  99.                      CI0278
            11            QT63-MPMTF7 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL7 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU7 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG8 PICTURE  99.                      CI0278
            11            QT63-MPMTF8 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL8 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU8 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-CPMTG9 PICTURE  99.                      CI0278
            11            QT63-MPMTF9 PICTURE  X(24).                   CI0278
            11            QT63-ACOTL9 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-ACOTU9 PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-MPMTT1 PICTURE  X(20).                   CI0278
            11            QT63-MPMTT2 PICTURE  X(20).                   CI0278
            11            QT63-MPMTT3 PICTURE  X(20).                   CI0278
            11            QT63-MPMTT4 PICTURE  X(20).                   CI0278
            11            QT63-MPMTT5 PICTURE  X(20).                   CI0278
            11            QT63-IABAA  PICTURE  X(01).                   CI0278
            11            QT63-IIBAA  PICTURE  X(01).                   CI0278
            11            QT63-IDBMO  PICTURE  X.                       CI0278
            11            QT63-IDBQT  PICTURE  X.                       CI0278
            11            QT63-IDBSA  PICTURE  X.                       CI0278
            11            QT63-IDBAN  PICTURE  X.                       CI0278
            11            QT63-CPCCD1 PICTURE  9(5).                    CI0278
            11            QT63-PRCOD  PICTURE  9(5).                    CI0278
            11            QT63-OWNOUT PICTURE  X(60).                   CI0278
            11            QT63-TSECD  PICTURE  X(30).                   CI0278
            11            QT63-CSPRP  PICTURE  X(04).                   CI0278
            11            QT63-QSTSO  PICTURE  S999                     CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-QSTSM  PICTURE  S999                     CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-INROA  PICTURE  X(1).                    CI0278
            11            QT63-FILLER PICTURE  X(19).                   CI0278
            10            QT63-LIMITS                                   CI0278
                          REDEFINES            QT63-OUTPT2.             CI0278
            11            QT63-QT6R                                     CI0278
                          OCCURS       009     TIMES.                   CI0278
            12            QT63-CPMTFA PICTURE  X(2).                    CI0278
            12            QT63-MPMTFL PICTURE  X(24).                   CI0278
            12            QT63-ACOTL  PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            12            QT63-ACOTU  PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-MPMTT  PICTURE  X(20)                    CI0278
                          OCCURS       005     TIMES.                   CI0278
            10            QT63-CX06.                                    CI0278
            11            QT63-CX06K.                                   CI0278
            12            QT63-C299.                                    CI0278
            13            QT63-CTID.                                    CI0278
            14            QT63-CTIDA  PICTURE  9(3).                    CI0278
            14            QT63-CTIDN.                                   CI0278
            15            QT63-CTIDNP PICTURE  X(13).                   CI0278
            15            QT63-CTIDND PICTURE  9(11).                   CI0278
            11            QT63-NPECK  PICTURE  9(02).                   CI0278
            11            QT63-FILLER PICTURE  X.                       CI0278
            10            QT63-CX12.                                    CI0278
            11            QT63-CX12K.                                   CI0278
            12            QT63-CPMTC  PICTURE  99.                      CI0278
            12            QT63-NAPDS  PICTURE  S9(3)                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            12            QT63-GESTD  PICTURE  9(8).                    CI0278
            11            QT63-GEEND  PICTURE  9(8).                    CI0278
            11            QT63-CIRMO  PICTURE  X(12).                   CI0278
            11            QT63-CDEST  PICTURE  99.                      CI0278
            11            QT63-APMTL  PICTURE  S9(9)V99                 CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-DNPMT  PICTURE  9(8).                    CI0278
            11            QT63-NIRACM PICTURE  9(2).                    CI0278
            11            QT63-CPMTF  PICTURE  99.                      CI0278
            11            QT63-IPODM  PICTURE  X.                       CI0278
            11            QT63-CLUPD  PICTURE  9(3).                    CI0278
            11            QT63-DLAUP  PICTURE  9(8).                    CI0278
            11            QT63-CWRC   PICTURE  99.                      CI0278
            11            QT63-CHCR   PICTURE  99.                      CI0278
            11            QT63-GEOPD2 PICTURE  X(8).                    CI0278
            11            QT63-GEAUN  PICTURE  9(5).                    CI0278
            11            QT63-DPCHD  PICTURE  9(8).                    CI0278
            11            QT63-DNEXE  PICTURE  9(8).                    CI0278
            11            QT63-CCSMQ  PICTURE  X.                       CI0278
            11            QT63-GCUSPZ PICTURE  X(12).                   CI0278
            11            QT63-CORTY  PICTURE  X.                       CI0278
            11            QT63-CNAVR  PICTURE  X(1).                    CI0278
            11            QT63-DELOI3 PICTURE  9(6).                    CI0278
            11            QT63-ALOIDD PICTURE  9(9)V99                  CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            QT63-FILLER PICTURE  X(5).                    CI0278
       01                 QT78.                                         CI0278
            10            QT78-INPUT.                                   CI0278
            11            QT78-IDBUP  PICTURE  X.                       CI0278
            11            QT78-CPROCM PICTURE  X.                       CI0278
            11            QT78-CDBIL  PICTURE  X(2).                    CI0278
            11            QT78-CSKIP  PICTURE  9.                       CI0278
            11            QT78-IGRAC  PICTURE  X.                       CI0278
            11            QT78-IBACHF PICTURE  X.                       CI0278
            11            QT78-IBACHN PICTURE  X.                       CI0278
            11            QT78-IBACHA PICTURE  X.                       CI0278
            11            QT78-IBACHS PICTURE  X.                       CI0278
            11            QT78-CUPIQ  PICTURE  X.                       CI0278
            10            QT78-OUTPUT.                                  CI0278
            11            QT78-QITEM  PICTURE  9(3).                    CI0278
            11            QT78-NMESS2 PICTURE  S9(6)                    CI0278
                          OCCURS       020     TIMES                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            10            QT78-QCDOB  PICTURE  9(4).                    CI0278
            10            QT78-ACLIM  PICTURE  S9(7)V9(2).              CI0278
      *
      *!WF DSP=QT DSL=QT SEL=78 FOR=I DES=1 LEV=1 PLT=40
      *
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0278
          05              MS00-SUITE.                                   CI0278
            15       FILLER         PICTURE  X(00542).                  CI0278
       01                 MS03  REDEFINES      MS00.                    CI0278
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            10            MS03-CMSSF  PICTURE  XX.                      CI0278
            10            MS03-DU09.                                    CI0278
            11            MS03-CMESA  PICTURE  S9(9)                    CI0278
                          BINARY.                                       CI0278
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0278
                          BINARY.                                       CI0278
            11            MS03-CMESB  PICTURE  S9(9)                    CI0278
                          BINARY.                                       CI0278
            11            MS03-CMSST  PICTURE  S9(9)                    CI0278
                          BINARY.                                       CI0278
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0278
                          BINARY.                                       CI0278
            11            MS03-QELLAA PICTURE  S9(9)                    CI0278
                          BINARY.                                       CI0278
            11            MS03-TMESS4 PICTURE  X(512).                  CI0278
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0278
            10            MX11-QMSGS  PICTURE  9(03).                   CI0278
            10            MX11-PJ09                                     CI0278
                          OCCURS       025     TIMES.                   CI0278
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0278
                          COMPUTATIONAL-3.                              CI0278
            11            MX11-CMESB  PICTURE  S9(9)                    CI0278
                          BINARY.                                       CI0278
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                QT63
                                QT78
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0278
      *               *                                   *             CI0278
      *               *INITIALISATIONS                    *             CI0278
      *               *                                   *             CI0278
      *               *************************************.            CI0278
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
      *N02BC.    NOTE *INITIALIZE WORK AREAS              *.
       F02BC.                                                           lv10
           INITIALIZE  WS00-WORK-VALUES
           MOVE        20 TO WS00-MAX-MSG.
       F02BC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0278
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0278
      *               *                                   *             CI0278
      *               *FIN DE TRAITEMENT                  *             CI0278
      *               *                                   *             CI0278
      *               *************************************.            CI0278
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0278
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *START PROCESSING ERROR CODES       *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40AB.    NOTE *DEFAULT MESSAGE FOR IMPACT OF      *.
       F40AB.    IF    EIBTRNID = 'XC47'                                lv10
                 NEXT SENTENCE ELSE GO TO     F40AB-FN.
      *CHANGING ARRANGEMENTS.
           MOVE        15069 TO WS00-NMESS2                             DOT
           PERFORM     F91BB THRU F91BB-FN.
                 IF    QT78-CPROCM = 'A'                                DOT
                 AND   QT78-IDBUP = 'N'
                 AND   QT78-CUPIQ = 'U'
      *POPULATED IMPLICATION MESSAGE
      *ON THE CONFIRMATION PAGE FOR THE
      *ODC ADD BA TRANSACTION
           MOVE        15520 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
      *N40AC.    NOTE *POPULATE IMPLICATION MESSAGE       *.
       F40AC.    IF    QT63-CPCCD1 = 42000                              lv15
                 AND   QT63-CPMTC = '00'
                 AND   ((QT63-CDEST NOT = 03
                 AND   QT63-CPMTF NOT = 99)
                 OR    (QT63-CPMTF = 99
                 AND   QT63-APMTL NOT = 0))
                 NEXT SENTENCE ELSE GO TO     F40AC-FN.
      *WHEN REQUESTED VALUE IS GREATER
      *OR EQUAL TO $25 AND LESS THAN
      *ACTUAL MINIMUM AMOUNT CALCULATED
      *IN XC47 (CI0225) EXCEPT INAC
      *RECURRING BA AND ON-DEMAND STOP
                 IF    QT63-CPMTF = QT63-CPMTG1                         DOT
                 AND   QT63-APMTL < QT63-ACOTL1
      *MONTHLY
           MOVE        15488 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
                 IF    QT63-CPMTF = QT63-CPMTG2                         DOT
                 AND   QT63-APMTL < QT63-ACOTL2
      *QUARTERLY
           MOVE        15488 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
                 IF    QT63-CPMTF = QT63-CPMTG3                         DOT
                 AND   QT63-APMTL < QT63-ACOTL3
      *ON-DEMAND
           MOVE        15488 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F40AC-FN. EXIT.
       F40AB-FN. EXIT.
      *N40AD.    NOTE *QUALIFIED ACCOUNT TYPE = IRA       *.
       F40AD.    IF    QT63-CQACT = 001                                 lv10
                 AND   QT63-APMTL > 0
                 NEXT SENTENCE ELSE GO TO     F40AD-FN.
      *N40AG.    NOTE *TRANSACTION CODE = ADD             *.
       F40AG.    IF    QT78-CPROCM = 'A'                                lv15
                 OR    QT78-CPROCM = 'M'
                 AND   (QT78-IBACHA = 'Y'
                 OR    QT78-IBACHF = 'Y'
                 OR    QT78-IBACHN = 'Y'
                 OR    QT78-IBACHS = 'Y')
                 AND   QT63-GEEND NOT = QT63-DCACG
                 NEXT SENTENCE ELSE GO TO     F40AG-FN.
      *TRANSACTION CODE = MODIFY
      *AMOUNT  WAS CHANGED
      *FREQUENCY WAS CHANGED
      *NEXT PAYMENT DATE CHANGED
      *STATUS WAS CHANGED
      *AND NOT INACTIVATING
      *N40BB.    NOTE *IRA TYPE CODE SEP                  *.
       F40BB.    IF    QT63-CIRAT = 003                                 lv20
                 NEXT SENTENCE ELSE GO TO     F40BB-FN.
                 IF    QT78-QCDOB < 50                                  DOT
           MOVE        013610 TO WS00-NMESS2
                 ELSE
           MOVE        013516 TO WS00-NMESS2.
           PERFORM     F91BB THRU F91BB-FN.                             DOT
       F40BB-FN. EXIT.
      *N40BD.    NOTE *IRA TYPE CODE SRA                  *.
       F40BD.    IF    QT63-CIRAT = 004                                 lv20
                 NEXT SENTENCE ELSE GO TO     F40BD-FN.
                 IF    QT78-QCDOB < 50                                  DOT
           MOVE        013611 TO WS00-NMESS2
                 ELSE
           MOVE        013515 TO WS00-NMESS2.
           PERFORM     F91BB THRU F91BB-FN.                             DOT
       F40BD-FN. EXIT.
       F40AG-FN. EXIT.
      *N40BH.    NOTE *IRA TYPE CODE = INDIVIDUAL         *.
       F40BH.    IF    QT63-CIRAT = 001                                 lv15
                 AND   QT63-CIRAS = 002
                 AND   QT78-CPROCM = 'A'
                 NEXT SENTENCE ELSE GO TO     F40BH-FN.
      *IRA STATUS CODE = ROLLOVER
      *TRANSACTION CODE = ADD
           MOVE        014150 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F40BH-FN. EXIT.
      *N40BJ.    NOTE *IRA TYPE CODE:  INDIVIDUAL, ROTH   *.
       F40BJ.    IF    (QT63-CIRAT = 001                                lv15
                       OR 005 OR 006 OR 007)
                 NEXT SENTENCE ELSE GO TO     F40BJ-FN.
      *CONTRIBUTORY , ROTH CONVERSION
      *OR COVERDELL ESA
      *N40BL.    NOTE *NOT AN ON-DEMAND TYPE              *.
       F40BL.    IF    QT63-CPMTF NOT = 99                              lv20
                 AND   (QT78-CPROCM = 'A'
                 OR    QT78-CPROCM = 'M'
                 AND   (QT78-IBACHA = 'Y'
                 OR    QT78-IBACHF = 'Y'
                 OR    QT78-IBACHN = 'Y'
                 OR    QT78-IBACHS = 'Y'))
                 AND   QT63-GEEND NOT = QT63-DCACG
                 NEXT SENTENCE ELSE GO TO     F40BL-FN.
      *TRANSACTION CODE = ADD
      *TRANSACTION CODE = MODIFY
      *AMOUNT WAS CHANGED
      *FREQUENCY WAS CHANGED
      *NEXT PAYMENT DATE CHANGED
      *STATUS WAS CHANGED
      *AND BA IS INACTIVATED
      *N40DD.    NOTE *FIGURE THE TOTAL ANNUAL IRA        *.
       F40DD.                                                           lv25
      *CONTRIBUTION BY MULTIPLYING
      *(QT63-APMTL) AMOUNT OF PAYMENT
      *BY (QT63-CPMTF) NUMBER OF
      *   PAYMENTS MADE ANNUALLY
           COMPUTE     W-QT63-APMTL = QT63-APMTL *
           QT63-CPMTF.
       F40DD-FN. EXIT.
      *N40DF.    NOTE *IS THE TOTAL ANNUAL PAYMENTS       *.
       F40DF.    IF    W-QT63-APMTL > QT78-ACLIM                        lv25
                 AND   QT63-CIRAT = 007
                 NEXT SENTENCE ELSE GO TO     F40DF-FN.
      *GREATER THAN LEGALLY ALLOWED
      *FOR COVERDELL ESA
           MOVE        014149 TO WS00-NMESS2.
           PERFORM     F91BB THRU F91BB-FN.                             DOT
       F40DF-FN. EXIT.
      *N40DH.    NOTE *IS THE TOTAL ANNUAL PAYMENTS       *.
       F40DH.    IF    W-QT63-APMTL > QT78-ACLIM                        lv25
                 AND   (QT63-CIRAT = 001 OR 005 OR
                       006)
                 AND   QT63-CIRAS NOT = 003
                 NEXT SENTENCE ELSE GO TO     F40DH-FN.
      *GREATER THAN LEGALLY ALLOWED
      *FOR INDIVIDUAL OR ROTH OR
      *ROTH CONVERSION IRA
                 IF    QT78-QCDOB < 50                                  DOT
           MOVE        014148 TO WS00-NMESS2
                 ELSE
           MOVE        013517 TO WS00-NMESS2.
           PERFORM     F91BB THRU F91BB-FN.                             DOT
       F40DH-FN. EXIT.
       F40BL-FN. EXIT.
       F40BJ-FN. EXIT.
      *N40EC.    NOTE *MESSAGES FOR AN ON DEMAND TYPE     *.
       F40EC.    IF    QT63-CPMTF = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F40EC-FN.
      *N40EE.    NOTE *IRA PRIOR YEAR APPLIES             *.
       F40EE.    IF    QT63-DIRAC1 NOT = 00                             lv20
                 AND   QT63-DIRAC1 NOT = 99
                 NEXT SENTENCE ELSE GO TO     F40EE-FN.
      *N40EF.    NOTE *MESSAGE FOR IRA PRIOR YEAR         *.
       F40EF.    IF    QT63-DIRAC1 < 04                                 lv25
                 NEXT SENTENCE ELSE GO TO     F40EF-FN.
                 IF    QT63-DNPMT (5:2) <=                              DOT
                       QT63-DIRAC1
      *IF JAN, FEB, MARCH
           MOVE        014336 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F40EF-FN. EXIT.
      *N40EG.    NOTE *MESSAGE FOR IRA PRIOR YEAR         *.
       F40EG.    IF    QT63-DIRAC1 = 04                                 lv25
                 NEXT SENTENCE ELSE GO TO     F40EG-FN.
                 IF    QT63-DNPMT (5:4) < 0416                          DOT
      *FOR APRIL ONLY
           MOVE        014336 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F40EG-FN. EXIT.
       F40EE-FN. EXIT.
      *N40EK.    NOTE *MESSAGES FOR OVER CONTRIBUTION     *.
       F40EK.    IF    QT63-APMTL > QT78-ACLIM                          lv20
                 AND   (QT63-CIRAT = 001 OR 005 OR
                       006)
                 AND   QT63-CIRAS NOT = 003
                 NEXT SENTENCE ELSE GO TO     F40EK-FN.
      *IN CASE OF: SRA/ROTH IRA/ROTH
      *CONVERSION IRA
                 IF    QT78-QCDOB < 50                                  DOT
      *CHECK IF AGE LESS THAN 50 YRS
           MOVE        014148 TO WS00-NMESS2
                 ELSE
      *CHECK IF AGE >= 50 AND AMOUNT
           MOVE        013517 TO WS00-NMESS2.
           PERFORM     F91BB THRU F91BB-FN.                             DOT
       F40EK-FN. EXIT.
      *N40EM.    NOTE *MESSAGE FOR OVER CONTIBUTION-      *.
       F40EM.    IF    QT63-APMTL > QT78-ACLIM                          lv20
                 AND   QT63-CIRAT = 007
                 NEXT SENTENCE ELSE GO TO     F40EM-FN.
      *FOR COVERDELL ESA
           MOVE        014149 TO WS00-NMESS2.
           PERFORM     F91BB THRU F91BB-FN.                             DOT
       F40EM-FN. EXIT.
       F40EC-FN. EXIT.
       F40AD-FN. EXIT.
      *N40GA.    NOTE *IF ROA FIELDS ARE POPULATED FOR    *.
       F40GA.    IF    (QT63-INROA NOT = 'Y'                            lv10
                 AND   (QT63-CORTY = 'R'
                 OR    QT63-CORTY = 'L'))
                 OR    QT63-INROA = 'U'
                 NEXT SENTENCE ELSE GO TO     F40GA-FN.
      *A BROKERAGE BA TO NON CLASS A
      *FUND RETURN IMPLICATION MESSAGE
           MOVE        014588 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F40GA-FN. EXIT.
       F40-FN.   EXIT.
      *N41.      NOTE *************************************.
      *               *                                   *
      *               *ACCOUNT STATUS                     *
      *               *                                   *
      *               *************************************.
       F41.           EXIT.                                             lv05
      *N41AG.    NOTE *IF THE ACCOUNT IS A CERT           *.
       F41AG.    IF    QT63-CPCCDE = 01                                 lv10
                 AND   QT63-CDEST = 03
                 AND   QT78-IBACHS = 'Y'
                 AND   ((QT63-PRCOD > 00649
                 AND   < 00658)
                 OR    (QT63-PRCOD > 00979
                 AND   < 00988))
                 NEXT SENTENCE ELSE GO TO     F41AG-FN.
      *ACCOUNT IS INACTIVE
      *STATUS CHANGED
      *INSTALLMENT CERTS
           MOVE        014155 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F41AG-FN. EXIT.
      *N41BB.    NOTE *IF UL INSURANCE                    *.
       F41BB.    IF    QT63-CPCCDE = 04                                 lv10
                 AND   QT78-IBACHS = 'Y'
                 AND   QT63-CDEST = 03
                 NEXT SENTENCE ELSE GO TO     F41BB-FN.
      *STATUS CHANGED
      *ACCOUNT IS INACTIVE
      *N41BD.    NOTE *DIRECT BILL OPTION CODE MONTHLY    *.
       F41BD.    IF    QT78-CDBIL = 'MO'                                lv15
                 AND   QT63-CPMTC = 00
                 NEXT SENTENCE ELSE GO TO     F41BD-FN.
      *PAYMENT TYPE CODE REGULAR
           MOVE        014151 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F41BD-FN. EXIT.
      *N41BF.    NOTE *DIRECT BILL OPTION CODE QUARTER    *.
       F41BF.    IF    QT78-CDBIL = 'QT'                                lv15
                 AND   QT63-CPMTC = 00
                 NEXT SENTENCE ELSE GO TO     F41BF-FN.
      *PAYMENT TYPE CODE REGULAR
           MOVE        014152 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F41BF-FN. EXIT.
      *N41BH.    NOTE *DIRECT BILL OPTION CODE SEMI-ANN   *.
       F41BH.    IF    QT78-CDBIL = 'SA'                                lv15
                 AND   QT63-CPMTC = 00
                 NEXT SENTENCE ELSE GO TO     F41BH-FN.
      *PAYMENT TYPE CODE REGULAR
           MOVE        014153 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F41BH-FN. EXIT.
      *N41BJ.    NOTE *DIRECT BILL OPTION CODE ANNUAL     *.
       F41BJ.    IF    QT78-CDBIL = 'AN'                                lv15
                 AND   QT63-CPMTC = 00
                 NEXT SENTENCE ELSE GO TO     F41BJ-FN.
      *PAYMENT TYPE CODE REGULAR
           MOVE        014154 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F41BJ-FN. EXIT.
       F41BB-FN. EXIT.
       F41-FN.   EXIT.
      *N42.      NOTE *************************************.
      *               *                                   *
      *               *NEXT PAYMENT DATE                  *
      *               *                                   *
      *               *************************************.
       F42.           EXIT.                                             lv05
      *N42AD.    NOTE *NEXT DATE                          *.
       F42AD.         EXIT.                                             lv10
      *N42BB.    NOTE *SKIP PAYMENT CODE                  *.
       F42BB.    IF    QT78-CSKIP = 1 OR 2                              lv15
                 NEXT SENTENCE ELSE GO TO     F42BB-FN.
           MOVE        014159 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F42BB-FN. EXIT.
      *N42BD.    NOTE *SKIP PAYMENT CODE                  *.
       F42BD.    IF    QT78-CSKIP = 3                                   lv15
                 NEXT SENTENCE ELSE GO TO     F42BD-FN.
           MOVE        014160 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F42BD-FN. EXIT.
      *N42BF.    NOTE *GRACE PERIOD INDC                  *.
       F42BF.    IF    QT78-IGRAC = 'Y'                                 lv15
                 AND   QT63-CPCCDE = 04
                 AND   QT63-CDEST NOT = 03
                 NEXT SENTENCE ELSE GO TO     F42BF-FN.
      *PROD CAT CODE : INSURANCE.
      *ACCOUNT IS NOT INACTIVE
           MOVE        014157 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F42BF-FN. EXIT.
       F42AD-FN. EXIT.
      *N42CA.    NOTE *MESSAGE FOR "STOP" OF AN           *.
       F42CA.    IF    QT63-CPMTF = 99                                  lv10
                 AND   QT63-APMTL = 0
                 AND   QT63-DNPMT = 0
                 AND   QT78-CPROCM = 'M'
                 AND   QT78-IBACHA = 'Y'
                 AND   QT78-IBACHN = 'Y'
                 NEXT SENTENCE ELSE GO TO     F42CA-FN.
      *ON-DEMAND.
           MOVE        14337 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F42CA-FN. EXIT.
       F42-FN.   EXIT.
      *N43.      NOTE *************************************.
      *               *                                   *
      *               *TYPE OF PAYMENT                    *
      *               *                                   *
      *               *************************************.
       F43.           EXIT.                                             lv05
      *N43BH.    NOTE *PAYMENT TYPE CODE: LOAN            *.
       F43BH.    IF    QT63-CPMTC = 01                                  lv15
                 AND   (QT63-CPCCDE = 01 OR 04)
                 AND   QT78-CPROCM = 'A'
                 NEXT SENTENCE ELSE GO TO     F43BH-FN.
      *PROD CAT CODE: CERT OR INS
      *TRANSACTION CODE: ADD
           MOVE        014156 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F43BH-FN. EXIT.
       F43-FN.   EXIT.
      *N44.      NOTE *************************************.
      *               *                                   *
      *               *INDEX UL OR ANNUITY ACCOUNT        *
      *               *                                   *
      *               *************************************.
       F44.           EXIT.                                             lv05
      *N44BX.    NOTE *FOR INSURANCE ACCOUNT              *.
       F44BX.    IF    QT63-CPCCDE = 04                                 lv10
                 AND   EIBTRNID = 'XC47'
                 NEXT SENTENCE ELSE GO TO     F44BX-FN.
      *READ TA8A TO GET THE INDICATOR
           INITIALIZE  TA8A
           MOVE        QT63-CTIDA TO TA8A-CTIDA
           MOVE        QT63-PRCOD TO TA8A-PRCOD
           PERFORM     F92TA THRU F92TA-FN.
                 IF    TA8A-IIULA = 'Y'                                 DOT
      *IF THE DESTINATION IS IUL ACCT
           MOVE        15120 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F44BX-FN. EXIT.
      *N44CP.    NOTE *FOR ANNUITY ACCOUNT                *.
       F44CP.    IF    QT63-CPCCDE = 03                                 lv10
                 AND   EIBTRNID = 'XC47'
                 NEXT SENTENCE ELSE GO TO     F44CP-FN.
           MOVE        15653 TO WS00-NMESS2
           PERFORM     F91BB THRU F91BB-FN.
       F44CP-FN. EXIT.
       F44-FN.   EXIT.
      *N78.      NOTE *************************************.
      *               *                                   *
      *               *END OF PROGRAM CLEANUP             *
      *               *                                   *
      *               *************************************.
       F78.           EXIT.                                             lv05
      *N78BB.    NOTE *INITIALIZE MESSAGE AREA            *.
       F78BB.                                                           lv10
           INITIALIZE  MS03.
       F78BB-FN. EXIT.
       F78-FN.   EXIT.
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
      *               *LOAD ERROR MESSAGE ARRAY           *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BB.    NOTE *LOAD ERROR MESSAGE ARRAY           *.
       F91BB.                                                           lv10
      *
           ADD         1 TO WS00-MSG-COUNT
           MOVE        WS00-NMESS2 TO
           QT78-NMESS2 (WS00-MSG-COUNT)
           MOVE        WS00-MSG-COUNT TO QT78-QITEM.
                 IF    WS00-MSG-COUNT                                   DOT
                       > WS00-MAX-MSG
      *MESSAGE ARRAY IS FULL RETURN
           MOVE        014166 TO QT78-NMESS2 (WS00-MAX-MSG)
           MOVE                     ALL '1' TO FT GO TO F20.
       F91BB-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *RANDOM TABLE READ FOR TA8A         *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA8A         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA8A-TABFO                             ADUTAB
           COMPUTE     G-TA8A-LTH = 60 + G-TA8A-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA8A-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA8A)                                ADUTAB
                       LENGTH (G-TA8A-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA8A-TABCR NOT = '00'                          DOT
      *ON ERROR
           INITIALIZE  TA8A.
       F92TA-FN. EXIT.
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
