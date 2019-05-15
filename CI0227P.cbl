       IDENTIFICATION DIVISION.                                         CI0227
       PROGRAM-ID.  CI0227P.                                            CI0227
      *AUTHOR.         UPDATE PENDING ACH IN TABLES.                    CI0227
      *DATE-COMPILED.   09/08/14.                                       CI0227
       ENVIRONMENT DIVISION.                                            CI0227
       CONFIGURATION SECTION.                                           CI0227
       SOURCE-COMPUTER. IBM-370.                                        CI0227
       OBJECT-COMPUTER. IBM-370.                                        CI0227
       DATA DIVISION.                                                   CI0227
       WORKING-STORAGE SECTION.                                         CI0227
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *****************************************************************
      ** HOST VARIABLES FOR DB2 TABLES                                *
      *****************************************************************
      *!WF DSP=CT DSL=CT SEL=1S FOR=I DES=2 LEV=1 PLT=CT
       01                 CT1S.                                         CI0227
            10            CT1S-DXTMS  PICTURE  X(26)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-NUCLI  PICTURE  X(32)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-CARTYA PICTURE  XX                       CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-APMTO  PICTURE  S9(11)V99                CI0227
                          VALUE                ZERO                     CI0227
                          COMPUTATIONAL-3.                              CI0227
            10            CT1S-CASTA  PICTURE  X                        CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-CPMTFA PICTURE  X(2)                     CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-DSETU  PICTURE  X(10)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-DATSD  PICTURE  X(10)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-DTEND  PICTURE  X(10)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-DACPR  PICTURE  X(10)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-GEOPDF PICTURE  X(16)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-DXTMST PICTURE  X(26)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-GEOPDG PICTURE  X(16)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1S-DXTMSD PICTURE  X(26)                    CI0227
                          VALUE                SPACE.                   CI0227
       01                 CT1T.                                         CI0227
            10            CT1T-DXTMS  PICTURE  X(26)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1T-NAGTB  PICTURE  X(8)                     CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1T-CTBAC  PICTURE  X(03)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1T-NPBN   PICTURE  X(20)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1T-IAPRV  PICTURE  X                        CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1T-CPROCM PICTURE  X                        CI0227
                          VALUE                SPACE.                   CI0227
       01                 CT1U.                                         CI0227
            10            CT1U-DXTMS  PICTURE  X(26)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-CNTXC  PICTURE  X(10)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-NNANI  PICTURE  X(40)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-CACCT  PICTURE  X                        CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-CPMTCX PICTURE  XX                       CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-APMTD  PICTURE  S9(11)V99                CI0227
                          VALUE                ZERO                     CI0227
                          COMPUTATIONAL-3.                              CI0227
            10            CT1U-DDATYA PICTURE  XXXX                     CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-DDATM  PICTURE  XX                       CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-GEOPDF PICTURE  X(16)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-DXTMST PICTURE  X(26)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-GEOPDG PICTURE  X(16)                    CI0227
                          VALUE                SPACE.                   CI0227
            10            CT1U-DXTMSD PICTURE  X(26)                    CI0227
                          VALUE                SPACE.                   CI0227
      *!WF DSP=CT DSL=CT SEL=1T FOR=I DES=2 LEV=1 PLT=CT
      *!WF DSP=CT DSL=CT SEL=1U FOR=I DES=2 LEV=1 PLT=CT
      *WORK AREA FOR ADB2DT
       01               7-DTIN-DATE.                                    ADB2DT
      *!WI pl=DT011                                                     ADB2DT
         05             7-DTIN-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0227
      *!WI pl=DT012                                                     ADB2DT
         05             7-DTIN-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0227
      *!WI pl=DT013                                                     ADB2DT
         05             7-DTIN-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0227
      *!WI pl=DT014                                                     ADB2DT
         05             7-DTIN-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0227
       01               7-DTOT-DATE.                                    ADB2DT
      *!WI pl=DT021                                                     ADB2DT
         05             7-DTOT-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0227
      *!WI pl=DT022                                                     ADB2DT
         05             7-DTOT-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0227
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT025                                                     ADB2DT
         05             7-DTOT-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0227
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT028                                                     ADB2DT
         05             7-DTOT-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0227
      *CCYYMMDD DATE IN
      *!WI
       01  DT01-DCACG
                        PICTURE 9(8).                                   CI0227
      *CCYY-MM-DD DATE OUT
      *!WI
       01  DT01-DCACD
                        PICTURE X(10).                                  CI0227
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
                        PICTURE X(66)                                   CI0227
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      ******************************************************************
      **                PROCESSING CONTROL SWITCHES                    *
      ******************************************************************
      *!WI
       01 WS-IAIND.                VALUE 'N'
                        PICTURE X.                                      CI0227
          88 NOT-DUPLICATE-TRAN    VALUE 'N'.
          88 DUPLICATE-TRAN        VALUE 'Y'.
      *!WI
       01 WS-XNULL
                        PICTURE S9(4)                                   CI0227
                          BINARY.                                       CI0227
      ******************************************************************
      *!WI
       01 WS-FIRST-TIME-IAIND      VALUE 'Y'
                        PICTURE X.                                      CI0227
          88 FIRST-TIME            VALUE 'Y'.
          88 NOT-FIRST-TIME        VALUE 'N'.
       01   DEBUT-WSS.                                                  CI0227
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0227
            05   IK     PICTURE X.                                      CI0227
       01  CONSTANTES-PAC.                                              CI0227
           05  FILLER  PICTURE X(87)   VALUE                            CI0227
                     '6015 CAT09/08/14CI0227ADMIN   14:35:05CI0227P AMERCI0227
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0227
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0227
           05  NUGNA   PICTURE X(5).                                    CI0227
           05  APPLI   PICTURE X(3).                                    CI0227
           05  DATGN   PICTURE X(8).                                    CI0227
           05  PROGR   PICTURE X(6).                                    CI0227
           05  CODUTI  PICTURE X(8).                                    CI0227
           05  TIMGN   PICTURE X(8).                                    CI0227
           05  PROGE   PICTURE X(8).                                    CI0227
           05  COBASE  PICTURE X(4).                                    CI0227
           05  DATGNC  PICTURE X(10).                                   CI0227
           05  RELEAS  PICTURE X(7).                                    CI0227
           05  DATGE   PICTURE X(10).                                   CI0227
           05  DATSQ   PICTURE X(10).                                   CI0227
       01  DATCE.                                                       CI0227
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0227
         05  DATOR.                                                     CI0227
           10  DATOA  PICTURE XX.                                       CI0227
           10  DATOM  PICTURE XX.                                       CI0227
           10  DATOJ  PICTURE XX.                                       CI0227
       01   VARIABLES-CONDITIONNELLES.                                  CI0227
            05                  FT      PICTURE X VALUE '0'.            CI0227
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0227
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0227
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0227
       LINKAGE SECTION.                                                 ADU102

      *PASS AREAS FROM CALLING MODULE

      *!WF DSP=QT DSL=QT SEL=92 FOR=I DES=1 LEV=1 PLT=10
       01                 QT6L.                                         CI0227
            10            QT6L-IDBUP  PICTURE  X.                       CI0227
            10            QT6L-CPROCM PICTURE  X.                       CI0227
            10            QT6L-IIDSW  PICTURE  X.                       CI0227
            10            QT6L-CDBIL  PICTURE  X(2).                    CI0227
            10            QT6L-GBACH.                                   CI0227
            11            QT6L-IBACHA PICTURE  X.                       CI0227
            11            QT6L-IBACHN PICTURE  X.                       CI0227
            11            QT6L-IBACHE PICTURE  X.                       CI0227
            11            QT6L-IBACHI PICTURE  X.                       CI0227
            11            QT6L-IBACHS PICTURE  X.                       CI0227
            11            QT6L-IBACHC PICTURE  X.                       CI0227
            11            QT6L-IBACHF PICTURE  X.                       CI0227
            11            QT6L-IBACHT PICTURE  X.                       CI0227
            11            QT6L-IBACHO PICTURE  X.                       CI0227
            11            QT6L-IBACHM PICTURE  X.                       CI0227
            11            QT6L-IBACHD PICTURE  X.                       CI0227
            10            QT6L-GETOD  PICTURE  9(6).                    CI0227
            10            QT6L-GEOPD2 PICTURE  X(8).                    CI0227
            10            QT6L-CTRHO  PICTURE  9(8).                    CI0227
            10            QT6L-MAPPN  PICTURE  X(10).                   CI0227
            10            QT6L-CUPIQ  PICTURE  X.                       CI0227
            10            QT6L-FILLER PICTURE  X(13).                   CI0227
       01                 QT63.                                         CI0227
            10            QT63-INPUT.                                   CI0227
            11            QT63-GEMDA  PICTURE  9(8).                    CI0227
            11            QT63-NSEQ4B PICTURE  9(8)                     CI0227
                          BINARY.                                       CI0227
            11            QT63-DCACG  PICTURE  9(8).                    CI0227
            11            QT63-C199.                                    CI0227
            12            QT63-CLID.                                    CI0227
            13            QT63-CLIDO  PICTURE  9(3).                    CI0227
            13            QT63-CLIDN.                                   CI0227
            14            QT63-CLIDNP PICTURE  X(12).                   CI0227
            14            QT63-CLIDND PICTURE  9(8).                    CI0227
            11            QT63-NARRS  PICTURE  S9(3)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-AACTV  PICTURE  S9(11)V99                CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CTCUS  PICTURE  999.                     CI0227
            11            QT63-CTOWN  PICTURE  9(3).                    CI0227
            11            QT63-CTKRAA PICTURE  X(12).                   CI0227
            11            QT63-AMAXA2 PICTURE  S9(7)V99.                CI0227
            11            QT63-AMAXAL PICTURE  S9(7)V99.                CI0227
            10            QT63-OUTPT1.                                  CI0227
            11            QT63-IARTYA PICTURE  X.                       CI0227
            11            QT63-NMESAA PICTURE  9(6).                    CI0227
            11            QT63-IACHI  PICTURE  X.                       CI0227
            11            QT63-NMESAC PICTURE  9(6).                    CI0227
            11            QT63-IAIND2 PICTURE  X.                       CI0227
            11            QT63-NMESAR PICTURE  9(6).                    CI0227
            11            QT63-IARRGA PICTURE  X.                       CI0227
            11            QT63-NMESA0 PICTURE  9(6).                    CI0227
            11            QT63-IARLNA PICTURE  X.                       CI0227
            11            QT63-NMESA1 PICTURE  9(6).                    CI0227
            11            QT63-IARCDA PICTURE  X.                       CI0227
            11            QT63-NMESA2 PICTURE  9(6).                    CI0227
            11            QT63-IARCPA PICTURE  X.                       CI0227
            11            QT63-NMESA3 PICTURE  9(6).                    CI0227
            11            QT63-IARRG1 PICTURE  X.                       CI0227
            11            QT63-NMESA4 PICTURE  9(6).                    CI0227
            11            QT63-IARLN1 PICTURE  X.                       CI0227
            11            QT63-NMESA5 PICTURE  9(6).                    CI0227
            11            QT63-IARCD1 PICTURE  X.                       CI0227
            11            QT63-NMESA6 PICTURE  9(6).                    CI0227
            11            QT63-IARCP1 PICTURE  X.                       CI0227
            11            QT63-NMESA7 PICTURE  9(6).                    CI0227
            11            QT63-IAINDA PICTURE  X.                       CI0227
            11            QT63-NAPDSK PICTURE  S9(3)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CDEST1 PICTURE  99.                      CI0227
            11            QT63-CPCCDE PICTURE  99.                      CI0227
            11            QT63-IFQAN  PICTURE  X.                       CI0227
            11            QT63-IFQSA  PICTURE  X.                       CI0227
            11            QT63-IFQQT  PICTURE  X.                       CI0227
            11            QT63-IFQBM  PICTURE  X.                       CI0227
            11            QT63-IFQMO  PICTURE  X.                       CI0227
            11            QT63-IFQSM  PICTURE  X.                       CI0227
            11            QT63-IFQBW  PICTURE  X.                       CI0227
            11            QT63-IFQWK  PICTURE  X.                       CI0227
            11            QT63-IFQOD  PICTURE  X.                       CI0227
            11            QT63-ALMIN  PICTURE  S9(5)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ALMIN3 PICTURE  S9(5)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ALPAGQ PICTURE  S9(7)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ALPAGM PICTURE  S9(7)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ALPLDT PICTURE  9(8).                    CI0227
            11            QT63-ALDDUE PICTURE  9(08).                   CI0227
            11            QT63-DGRAC  PICTURE  9(08).                   CI0227
            11            QT63-GESTE  PICTURE  9(8).                    CI0227
            11            QT63-GESTL  PICTURE  9(8).                    CI0227
            11            QT63-GEENE  PICTURE  9(8).                    CI0227
            11            QT63-GEENL  PICTURE  9(8).                    CI0227
            11            QT63-GESTD1 PICTURE  9(8).                    CI0227
            11            QT63-DSKIP  PICTURE  9(8).                    CI0227
            11            QT63-DSKIP1 PICTURE  9(8).                    CI0227
            11            QT63-DSKIP2 PICTURE  9(8).                    CI0227
            11            QT63-DIRAC1 PICTURE  XX.                      CI0227
            11            QT63-CIRAT  PICTURE  999.                     CI0227
            11            QT63-CIRAS  PICTURE  999.                     CI0227
            11            QT63-CQACT  PICTURE  999.                     CI0227
            11            QT63-IERRC  PICTURE  X.                       CI0227
            11            QT63-NMESA  PICTURE  9(6).                    CI0227
            10            QT63-OUTPT2.                                  CI0227
            11            QT63-CPMTG1 PICTURE  99.                      CI0227
            11            QT63-MPMTF1 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL1 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU1 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG2 PICTURE  99.                      CI0227
            11            QT63-MPMTF2 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL2 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU2 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG3 PICTURE  99.                      CI0227
            11            QT63-MPMTF3 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL3 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU3 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG4 PICTURE  99.                      CI0227
            11            QT63-MPMTF4 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL4 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU4 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG5 PICTURE  99.                      CI0227
            11            QT63-MPMTF5 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL5 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU5 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG6 PICTURE  99.                      CI0227
            11            QT63-MPMTF6 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL6 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU6 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG7 PICTURE  99.                      CI0227
            11            QT63-MPMTF7 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL7 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU7 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG8 PICTURE  99.                      CI0227
            11            QT63-MPMTF8 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL8 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU8 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-CPMTG9 PICTURE  99.                      CI0227
            11            QT63-MPMTF9 PICTURE  X(24).                   CI0227
            11            QT63-ACOTL9 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-ACOTU9 PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-MPMTT1 PICTURE  X(20).                   CI0227
            11            QT63-MPMTT2 PICTURE  X(20).                   CI0227
            11            QT63-MPMTT3 PICTURE  X(20).                   CI0227
            11            QT63-MPMTT4 PICTURE  X(20).                   CI0227
            11            QT63-MPMTT5 PICTURE  X(20).                   CI0227
            11            QT63-IABAA  PICTURE  X(01).                   CI0227
            11            QT63-IIBAA  PICTURE  X(01).                   CI0227
            11            QT63-IDBMO  PICTURE  X.                       CI0227
            11            QT63-IDBQT  PICTURE  X.                       CI0227
            11            QT63-IDBSA  PICTURE  X.                       CI0227
            11            QT63-IDBAN  PICTURE  X.                       CI0227
            11            QT63-CPCCD1 PICTURE  9(5).                    CI0227
            11            QT63-PRCOD  PICTURE  9(5).                    CI0227
            11            QT63-OWNOUT PICTURE  X(60).                   CI0227
            11            QT63-TSECD  PICTURE  X(30).                   CI0227
            11            QT63-CSPRP  PICTURE  X(04).                   CI0227
            11            QT63-QSTSO  PICTURE  S999                     CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-QSTSM  PICTURE  S999                     CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-INROA  PICTURE  X(1).                    CI0227
            11            QT63-FILLER PICTURE  X(19).                   CI0227
            10            QT63-LIMITS                                   CI0227
                          REDEFINES            QT63-OUTPT2.             CI0227
            11            QT63-QT6R                                     CI0227
                          OCCURS       009     TIMES.                   CI0227
            12            QT63-CPMTFA PICTURE  X(2).                    CI0227
            12            QT63-MPMTFL PICTURE  X(24).                   CI0227
            12            QT63-ACOTL  PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            12            QT63-ACOTU  PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-MPMTT  PICTURE  X(20)                    CI0227
                          OCCURS       005     TIMES.                   CI0227
            10            QT63-CX06.                                    CI0227
            11            QT63-CX06K.                                   CI0227
            12            QT63-C299.                                    CI0227
            13            QT63-CTID.                                    CI0227
            14            QT63-CTIDA  PICTURE  9(3).                    CI0227
            14            QT63-CTIDN.                                   CI0227
            15            QT63-CTIDNP PICTURE  X(13).                   CI0227
            15            QT63-CTIDND PICTURE  9(11).                   CI0227
            11            QT63-NPECK  PICTURE  9(02).                   CI0227
            11            QT63-FILLER PICTURE  X.                       CI0227
            10            QT63-CX12.                                    CI0227
            11            QT63-CX12K.                                   CI0227
            12            QT63-CPMTC  PICTURE  99.                      CI0227
            12            QT63-NAPDS  PICTURE  S9(3)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            12            QT63-GESTD  PICTURE  9(8).                    CI0227
            11            QT63-GEEND  PICTURE  9(8).                    CI0227
            11            QT63-CIRMO  PICTURE  X(12).                   CI0227
            11            QT63-CDEST  PICTURE  99.                      CI0227
            11            QT63-APMTL  PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-DNPMT  PICTURE  9(8).                    CI0227
            11            QT63-NIRACM PICTURE  9(2).                    CI0227
            11            QT63-CPMTF  PICTURE  99.                      CI0227
            11            QT63-IPODM  PICTURE  X.                       CI0227
            11            QT63-CLUPD  PICTURE  9(3).                    CI0227
            11            QT63-DLAUP  PICTURE  9(8).                    CI0227
            11            QT63-CWRC   PICTURE  99.                      CI0227
            11            QT63-CHCR   PICTURE  99.                      CI0227
            11            QT63-GEOPD2 PICTURE  X(8).                    CI0227
            11            QT63-GEAUN  PICTURE  9(5).                    CI0227
            11            QT63-DPCHD  PICTURE  9(8).                    CI0227
            11            QT63-DNEXE  PICTURE  9(8).                    CI0227
            11            QT63-CCSMQ  PICTURE  X.                       CI0227
            11            QT63-GCUSPZ PICTURE  X(12).                   CI0227
            11            QT63-CORTY  PICTURE  X.                       CI0227
            11            QT63-CNAVR  PICTURE  X(1).                    CI0227
            11            QT63-DELOI3 PICTURE  9(6).                    CI0227
            11            QT63-ALOIDD PICTURE  9(9)V99                  CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            QT63-FILLER PICTURE  X(5).                    CI0227
       01                 QT92.                                         CI0227
            10            QT92-CUPIQ  PICTURE  X.                       CI0227
            10            QT92-CPROCM PICTURE  X.                       CI0227
            10            QT92-IMQMG  PICTURE  X.                       CI0227
            10            QT92-C199.                                    CI0227
            11            QT92-CLID.                                    CI0227
            12            QT92-CLIDO  PICTURE  9(3).                    CI0227
            12            QT92-CLIDN.                                   CI0227
            13            QT92-CLIDNP PICTURE  X(12).                   CI0227
            13            QT92-CLIDND PICTURE  9(8).                    CI0227
            10            QT92-CCONF  PICTURE  X(25).                   CI0227
            10            QT92-NIPAD  PICTURE  X(15).                   CI0227
            10            QT92-CSLCT  PICTURE  X.                       CI0227
            10            QT92-PRBAR  PICTURE  999V999.                 CI0227
            10            QT92-CCSDSA PICTURE  X(1).                    CI0227
            10            QT92-DXTMSA PICTURE  X(26).                   CI0227
            10            QT92-DXTMS2 PICTURE  X(26).                   CI0227
            10            QT92-C198.                                    CI0227
            11            QT92-CLNAM.                                   CI0227
            12            QT92-CLNAMH PICTURE  X(6).                    CI0227
            12            QT92-CLNAMF PICTURE  X(20).                   CI0227
            12            QT92-CLNAMM.                                  CI0227
            13            QT92-CLNAMI PICTURE  X.                       CI0227
            13            QT92-CLNAMR PICTURE  X(14).                   CI0227
            12            QT92-CLNAML PICTURE  X(25).                   CI0227
            12            QT92-CLNAMS PICTURE  X(4).                    CI0227
            10            QT92-GECKD2 PICTURE  9.                       CI0227
            10            QT92-CLTIN  PICTURE  9(12).                   CI0227
            10            QT92-CLID01 PICTURE  X(23).                   CI0227
            10            QT92-NARRSA PICTURE  X(3).                    CI0227
            10            QT92-GECSQ2 PICTURE  9(3).                    CI0227
            10            QT92-NAPDSA PICTURE  X(3).                    CI0227
            10            QT92-CLID4  PICTURE  X(23).                   CI0227
            10            QT92-CLORN  PICTURE  X(45).                   CI0227
            10            QT92-NTR    PICTURE  9(8).                    CI0227
            10            QT92-GECKD1 PICTURE  9.                       CI0227
            10            QT92-NPBN   PICTURE  X(20).                   CI0227
            10            QT92-CCBAT  PICTURE  99.                      CI0227
            10            QT92-TTBAL  PICTURE  X(15).                   CI0227
            10            QT92-MCSIG  PICTURE  X(30).                   CI0227
            10            QT92-CPMTF  PICTURE  99.                      CI0227
            10            QT92-MPMTFL PICTURE  X(24).                   CI0227
            10            QT92-CPMTC  PICTURE  99.                      CI0227
            10            QT92-MPMTT  PICTURE  X(20).                   CI0227
            10            QT92-APMTF3 PICTURE  9(09)V99.                CI0227
            10            QT92-DNPMT  PICTURE  9(8).                    CI0227
            10            QT92-GESTD  PICTURE  9(8).                    CI0227
            10            QT92-GEEND  PICTURE  9(8).                    CI0227
            10            QT92-CDEST  PICTURE  99.                      CI0227
            10            QT92-TARST  PICTURE  X(10).                   CI0227
            10            QT92-NIRACM PICTURE  9(2).                    CI0227
            10            QT92-DIRAYR PICTURE  9(4).                    CI0227
            10            QT92-C299.                                    CI0227
            11            QT92-CTID.                                    CI0227
            12            QT92-CTIDA  PICTURE  9(3).                    CI0227
            12            QT92-CTIDN.                                   CI0227
            13            QT92-CTIDNP PICTURE  X(13).                   CI0227
            13            QT92-CTIDND PICTURE  9(11).                   CI0227
            10            QT92-GECKD  PICTURE  9.                       CI0227
            10            QT92-PRCLN  PICTURE  X(60).                   CI0227
            10            QT92-MRPSN  PICTURE  X(12).                   CI0227
            10            QT92-CTTLN1 PICTURE  X(30).                   CI0227
            10            QT92-CTTLN2 PICTURE  X(30).                   CI0227
            10            QT92-CTTLN3 PICTURE  X(30).                   CI0227
            10            QT92-CTTBO1 PICTURE  X(45).                   CI0227
            10            QT92-CTTBO2 PICTURE  X(45).                   CI0227
            10            QT92-CPCCDE PICTURE  99.                      CI0227
            10            QT92-CPCCDF PICTURE  99.                      CI0227
            10            QT92-GCUSPZ PICTURE  X(12).                   CI0227
            10            QT92-TSECD  PICTURE  X(30).                   CI0227
            10            QT92-PRCODA PICTURE  X(5).                    CI0227
            10            QT92-PRSCD  PICTURE  X(9).                    CI0227
            10            QT92-MRGNN  PICTURE  X(8).                    CI0227
            10            QT92-GEAUN  PICTURE  9(5).                    CI0227
            10            QT92-GEOPD2 PICTURE  X(8).                    CI0227
            10            QT92-CTTYPG PICTURE  X(04).                   CI0227
            10            QT92-FILLER PICTURE  X(52).                   CI0227
            10            QT92-QITEM  PICTURE  9(3).                    CI0227
            10            QT92-NMESA  PICTURE  9(6)                     CI0227
                          OCCURS       005     TIMES.                   CI0227
            10            QT92-NGEOR  PICTURE  9(08).                   CI0227
            10            QT92-FILLER PICTURE  X(56).                   CI0227
      *!WF DSP=QT DSL=QT SEL=63 FOR=I DES=1 LEV=1 PLT=10
      *!WF DSP=QT DSL=QT SEL=6L FOR=I DES=1 LEV=1 PLT=10
      *!WF DSP=CX DSL=CX SEL=12 FOR=I DES=1 LEV=1 PLT=10
       01                 CX12.                                         CI0227
            10            CX12-CX12K.                                   CI0227
            11            CX12-CPMTC  PICTURE  99.                      CI0227
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            CX12-GESTD  PICTURE  9(8).                    CI0227
            10            CX12-GEEND  PICTURE  9(8).                    CI0227
            10            CX12-CIRMO  PICTURE  X(12).                   CI0227
            10            CX12-CDEST  PICTURE  99.                      CI0227
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0227
                          COMPUTATIONAL-3.                              CI0227
            10            CX12-DNPMT  PICTURE  9(8).                    CI0227
            10            CX12-NIRACM PICTURE  9(2).                    CI0227
            10            CX12-CPMTF  PICTURE  99.                      CI0227
            10            CX12-IPODM  PICTURE  X.                       CI0227
            10            CX12-CLUPD  PICTURE  9(3).                    CI0227
            10            CX12-DLAUP  PICTURE  9(8).                    CI0227
            10            CX12-CWRC   PICTURE  99.                      CI0227
            10            CX12-CHCR   PICTURE  99.                      CI0227
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0227
            10            CX12-GEAUN  PICTURE  9(5).                    CI0227
            10            CX12-DPCHD  PICTURE  9(8).                    CI0227
            10            CX12-DNEXE  PICTURE  9(8).                    CI0227
            10            CX12-CCSMQ  PICTURE  X.                       CI0227
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0227
            10            CX12-CORTY  PICTURE  X.                       CI0227
            10            CX12-CNAVR  PICTURE  X(1).                    CI0227
            10            CX12-DELOI3 PICTURE  9(6).                    CI0227
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0227
                          COMPUTATIONAL-3.                              CI0227
            10            CX12-FILLER PICTURE  X(5).                    CI0227

      *RETURN CODE TO CALLING MODULE

       01  LK01.
      *!WI
         05  LK01-GERTC
                        PICTURE X.                                      CI0227
      *!WI
         05  LK01-NMESS2
                        PICTURE S9(6)                                   CI0227
                          COMPUTATIONAL-3.                              CI0227
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0227
          05              MS00-SUITE.                                   CI0227
            15       FILLER         PICTURE  X(00542).                  CI0227
       01                 MS03  REDEFINES      MS00.                    CI0227
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            10            MS03-CMSSF  PICTURE  XX.                      CI0227
            10            MS03-DU09.                                    CI0227
            11            MS03-CMESA  PICTURE  S9(9)                    CI0227
                          BINARY.                                       CI0227
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0227
                          BINARY.                                       CI0227
            11            MS03-CMESB  PICTURE  S9(9)                    CI0227
                          BINARY.                                       CI0227
            11            MS03-CMSST  PICTURE  S9(9)                    CI0227
                          BINARY.                                       CI0227
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0227
                          BINARY.                                       CI0227
            11            MS03-QELLAA PICTURE  S9(9)                    CI0227
                          BINARY.                                       CI0227
            11            MS03-TMESS4 PICTURE  X(512).                  CI0227
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0227
            10            MX11-QMSGS  PICTURE  9(03).                   CI0227
            10            MX11-PJ09                                     CI0227
                          OCCURS       025     TIMES.                   CI0227
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0227
                          COMPUTATIONAL-3.                              CI0227
            11            MX11-CMESB  PICTURE  S9(9)                    CI0227
                          BINARY.                                       CI0227
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                QT92
                                QT63
                                QT6L
                                CX12
                                LK01
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0227
      *               *                                   *             CI0227
      *               *INITIALISATIONS                    *             CI0227
      *               *                                   *             CI0227
      *               *************************************.            CI0227
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0227
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0227
      *               *                                   *             CI0227
      *               *FIN DE TRAITEMENT                  *             CI0227
      *               *                                   *             CI0227
      *               *************************************.            CI0227
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0227
      *N20AB.    NOTE *SET FIRST TIME SWITCH OFF          *.
       F20AB.                                                           lv10
           SET NOT-FIRST-TIME TO TRUE.
       F20AB-FN. EXIT.
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *CONVERT INPUT TO DB VALUES         *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CB.    NOTE *INITIALIZE DB SEGMENTS WS          *.
       F30CB.                                                           lv10
           INITIALIZE  CT1S CT1T CT1U.
       F30CB-FN. EXIT.
      *N30CE.    NOTE *SET ARRANGEMENT ID (KEY)           *.
       F30CE.                                                           lv10
      *
           MOVE        QT92-DXTMS2 TO CT1S-DXTMS
           CT1T-DXTMS
           CT1U-DXTMS.
       F30CE-FN. EXIT.
      *N30EA.    NOTE *SET UP CT1S: MM ARRANGEMENT        *.
       F30EA.                                                           lv10
      *********************************
      *N30EC.    NOTE *MOVE FROM LINKAGE SEGMENT          *.
       F30EC.                                                           lv15
           MOVE        QT92-CLID01 TO CT1S-NUCLI
           MOVE        '01' TO CT1S-CARTYA
           MOVE        QT92-CPMTF TO CT1S-CPMTFA
           MOVE        QT92-DXTMS2 (1:10) TO CT1S-DSETU.
       F30EC-FN. EXIT.
      *N30EE.    NOTE *CONVERT STATUS                     *.
       F30EE.                                                           lv15
                 IF    QT92-CDEST = '01'                                DOT
           MOVE        'Y' TO CT1S-CASTA.
                 IF    QT92-CDEST = '03'                                DOT
           MOVE        'N' TO CT1S-CASTA.
       F30EE-FN. EXIT.
      *N30EH.    NOTE *CONVERT DATES TO DB2 FORMAT        *.
       F30EH.                                                           lv15
      *                                                                 DOT
      *START DATE
           MOVE        QT92-GESTD TO DT01-DCACG
           PERFORM     F91DT THRU F91DT-FN
           MOVE        DT01-DCACD TO CT1S-DATSD.
                 IF    QT92-GEEND > 0                                   DOT
      *END DATE
           MOVE        QT92-GEEND TO DT01-DCACG
           PERFORM     F91DT THRU F91DT-FN
           MOVE        DT01-DCACD TO CT1S-DTEND
                 ELSE
      *DEFAULT TO HIGH-VALUES DATE
           MOVE        '9999-12-31' TO CT1S-DTEND.
      *                                                                 DOT
      *NEXT PAYMENT DATE
           MOVE        QT92-DNPMT TO DT01-DCACG
           PERFORM     F91DT THRU F91DT-FN
           MOVE        DT01-DCACD TO CT1S-DACPR.
       F30EH-FN. EXIT.
       F30EA-FN. EXIT.
      *N30GA.    NOTE *SET UP CT1T: ACH IN ARRANGEMENT    *.
       F30GA.         EXIT.                                             lv10
      *N30GC.    NOTE *MOVE FROM LINKAGE SEGMENT          *.
       F30GC.                                                           lv15
           MOVE        QT92-NTR TO CT1T-NAGTB
           MOVE        QT92-CCBAT TO CT1T-CTBAC
           MOVE        QT92-NPBN TO CT1T-NPBN
           MOVE        'Y' TO CT1T-IAPRV.
       F30GC-FN. EXIT.
      *N30GE.    NOTE *CONVERT CPROCM                     *.
       F30GE.                                                           lv15
                 IF    QT92-CPROCM = 'A'                                DOT
      *ADD
           MOVE        'A' TO CT1T-CPROCM.
                 IF    QT92-CPROCM = 'M'                                DOT
                 AND   QT6L-IBACHS = 'N'
      *MODIFY - NOT STATUS CHANGE
           MOVE        'M' TO CT1T-CPROCM.
                 IF    QT92-CPROCM = 'M'                                DOT
                 AND   QT6L-IBACHS = 'Y'
                 AND   QT92-CDEST = '01'
      *MODIFY - REACTIVATE
           MOVE        'R' TO CT1T-CPROCM.
                 IF    QT92-CPROCM = 'M'                                DOT
                 AND   QT6L-IBACHS = 'Y'
                 AND   QT92-CDEST = '03'
      *MODIFY - INACTIVATE
           MOVE        'I' TO CT1T-CPROCM.
       F30GE-FN. EXIT.
       F30GA-FN. EXIT.
      *N30IA.    NOTE *SET UP CT1U: ARRANGEMENT ACCOUNT   *.
       F30IA.         EXIT.                                             lv10
      *N30IC.    NOTE *MOVE FROM LINKAGE SEGMENT          *.
       F30IC.                                                           lv15
           MOVE        'AMPF' TO CT1U-CNTXC
           MOVE        'T' TO CT1U-CACCT
           MOVE        QT92-CPMTC TO CT1U-CPMTCX
           MOVE        QT92-DIRAYR TO CT1U-DDATYA
           MOVE        QT92-NIRACM TO CT1U-DDATM.
       F30IC-FN. EXIT.
      *N30IE.    NOTE *CONVERT ACCOUNT ID                 *.
       F30IE.                                                           lv15
      *
           STRING      QT92-CTID
           QT92-GCUSPZ
           DELIMITED BY SIZE
           INTO CT1U-NNANI.
       F30IE-FN. EXIT.
      *N30IG.    NOTE *CONVERT ARRANGEMENT AMOUNT         *.
       F30IG.                                                           lv15
      *
                 IF    CT1T-CPROCM = 'A' OR 'R'                         DOT
           COMPUTE     CT1U-APMTD = QT92-APMTF3.
                 IF    CT1T-CPROCM = 'I'                                DOT
           COMPUTE     CT1U-APMTD = 0
           - QT92-APMTF3.
                 IF    CT1T-CPROCM = 'M'                                DOT
           COMPUTE     CT1U-APMTD = QT92-APMTF3
           - CX12-APMTL.
           COMPUTE     CT1S-APMTO = CT1U-APMTD.                         DOT
       F30IG-FN. EXIT.
       F30IA-FN. EXIT.
      *N30ZB.    NOTE *SET CREATE USER ID AND UNIT        *.
       F30ZB.                                                           lv10
      *
           STRING      QT92-GEAUN
           QT92-GEOPD2
           DELIMITED BY SIZE
           INTO CT1S-GEOPDF
      *
           STRING      QT92-GEAUN
           QT92-GEOPD2
           DELIMITED BY SIZE
           INTO CT1U-GEOPDF.
       F30ZB-FN. EXIT.
      *N30ZE.    NOTE *SET CREATE DATES                   *.
       F30ZE.                                                           lv10
      *
           MOVE        QT92-DXTMS2 TO CT1S-DXTMST
           CT1U-DXTMST.
       F30ZE-FN. EXIT.
       F30-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS UPDATE                     *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50CB.    NOTE *CHECK FOR DUPLICATE VALUES         *.
       F50CB.                                                           lv10
      *********************************
           SET NOT-DUPLICATE-TRAN TO TRUE
           PERFORM     F94RC THRU F94RC-FN.
                 IF    DUPLICATE-TRAN                                   DOT
      *DUPLICATE TRANSACTION
           MOVE        'N' TO LK01-GERTC
           MOVE        012639 TO LK01-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50CB-FN. EXIT.
      *N50EB.    NOTE *FIRST TIME CALLED                  *.
       F50EB.    IF    FIRST-TIME                                       lv10
                 NEXT SENTENCE ELSE GO TO     F50EB-FN.
      *********************************
      *N50EE.    NOTE *INSERT CT1S                        *.
       F50EE.                                                           lv15
           PERFORM     F94CC THRU F94CC-FN.
       F50EE-FN. EXIT.
      *N50EG.    NOTE *INSERT CT1T                        *.
       F50EG.                                                           lv15
           PERFORM     F94EC THRU F94EC-FN.
       F50EG-FN. EXIT.
       F50EB-900. GO TO F50EM-FN.
       F50EB-FN. EXIT.
      *N50EM.    NOTE *NOT FIRST TIME CALLED              *.
       F50EM.                                                           lv10
      *********************************
      *N50EO.    NOTE *UPDATE CT1S AMOUNT                 *.
       F50EO.                                                           lv15
           PERFORM     F94CE THRU F94CE-FN.
       F50EO-FN. EXIT.
       F50EM-FN. EXIT.
      *N50GB.    NOTE *EVERY TIME CALLED                  *.
       F50GB.                                                           lv10
      *********************************
      *N50GE.    NOTE *INSERT CT1U                        *.
       F50GE.                                                           lv15
           PERFORM     F94GC THRU F94GC-FN.
       F50GE-FN. EXIT.
       F50GB-FN. EXIT.
       F50-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Normal Termination            *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F79.                                                             lv05
      *     Return to Calling Module                                    ADU102
           MOVE        'Y' TO LK01-GERTC
           MOVE        +0 TO LK01-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91DT.    NOTE *DB2 DATE CONVERSION                *.
       F91DT.                                                           lv10
      *REFORMAT CCYYMMDD USING DASHES                                   ADB2DT
           MOVE        DT01-DCACG TO                                    ADB2DT
           7-DTIN-DATE                                                  ADB2DT
           MOVE        7-DTIN-XDATC TO 7-DTOT-XDATC                     ADB2DT
           MOVE        7-DTIN-XDATY TO 7-DTOT-XDATY                     ADB2DT
           MOVE        7-DTIN-XDATM TO 7-DTOT-XDATM                     ADB2DT
           MOVE        7-DTIN-XDATD TO 7-DTOT-XDATD                     ADB2DT
           MOVE        7-DTOT-DATE TO                                   ADB2DT
           DT01-DCACD.                                                  ADB2DT
       F91DT-FN. EXIT.
       F91-FN.   EXIT.
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
      *N94AD.    NOTE *CHECK DUPLICATE TRANSACTION        *.
       F94AD.    IF    SQLCODE = -803                                   lv10
                 NEXT SENTENCE ELSE GO TO     F94AD-FN.
           MOVE        'N' TO LK01-GERTC
           MOVE        012639 TO LK01-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F94AD-FN. EXIT.
      *N94CC.    NOTE *INSERT CT1S                        *.
       F94CC.                                                           lv10
           MOVE        'F94CC - INSERT' TO 7-DB2-FUNCT                  ADB227
           EXEC SQL    INSERT                                           ADB227
                       INTO
                           CORP.TBCT1S
                       VALUES
                           (:CT1S)                           END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN                              ADB227
           PERFORM     F94AD THRU F94AD-FN.
       F94CC-FN. EXIT.
      *N94CE.    NOTE *UPDATE CT1S AMOUNT                 *.
       F94CE.                                                           lv10
           MOVE        'F94CE - UPDATE' TO 7-DB2-FUNCT                  ADB228
           EXEC SQL    UPDATE                                           ADB228
                           CORP.TBCT1S
                       SET
                           APMTO = APMTO + :CT1S-APMTO
                       WHERE
                           DXTMS = :CT1S-DXTMS               END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB228
       F94CE-FN. EXIT.
      *N94EC.    NOTE *INSERT CT1T                        *.
       F94EC.                                                           lv10
           MOVE        'F94EC - INSERT' TO 7-DB2-FUNCT                  ADB227
           EXEC SQL    INSERT                                           ADB227
                       INTO
                           CORP.TBCT1T
                       VALUES
                           (:CT1T)                           END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN                              ADB227
           PERFORM     F94AD THRU F94AD-FN.
       F94EC-FN. EXIT.
      *N94GC.    NOTE *INSERT CT1U                        *.
       F94GC.                                                           lv10
           MOVE        'F94GC - INSERT' TO 7-DB2-FUNCT                  ADB227
           EXEC SQL    INSERT                                           ADB227
                       INTO
                           CORP.TBCT1U
                       VALUES
                           (:CT1U)                           END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN                              ADB227
           PERFORM     F94AD THRU F94AD-FN.
       F94GC-FN. EXIT.
      *N94RC.    NOTE *CHECK FOR DUPLICATE TRANSACTION    *.
       F94RC.                                                           lv10
           MOVE        'F94RC - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                           IBMREQD
                       INTO
                          :WS-IAIND:WS-XNULL
                       FROM
                           SYSIBM.SYSDUMMY1
                       WHERE EXISTS (
                          SELECT *
                          FROM
                              CORP.TBCT1S A,
                              CORP.TBCT1T B,
                              CORP.TBCT1U C
                          WHERE
                              A.NUCLI  = :CT1S-NUCLI
                          AND A.CARTYA = :CT1S-CARTYA
                          AND A.DATSD  = :CT1S-DATSD
                          AND A.DTEND  = :CT1S-DTEND
                          AND A.DACPR  = :CT1S-DACPR
                          AND A.CASTA  = :CT1S-CASTA
                          AND A.CPMTFA = :CT1S-CPMTFA
                          AND B.DXTMS  = A.DXTMS
                          AND B.NAGTB  = :CT1T-NAGTB
                          AND B.NPBN   = :CT1T-NPBN
                          AND B.CTBAC  = :CT1T-CTBAC
                          AND B.CPROCM = :CT1T-CPROCM
                          AND C.DXTMS  = B.DXTMS
                          AND C.DXTMS  = A.DXTMS
                          AND C.CNTXC  = :CT1U-CNTXC
                          AND C.NNANI  = :CT1U-NNANI
                          AND C.CACCT  = :CT1U-CACCT
                          AND C.CPMTCX = :CT1U-CPMTCX
                          AND C.DDATYA = :CT1U-DDATYA
                                    )                        END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F94RC-FN. EXIT.
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
