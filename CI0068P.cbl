       IDENTIFICATION DIVISION.                                         CI0068
       PROGRAM-ID.  CI0068P.                                            CI0068
      *AUTHOR.         M\M - DETERMINE PAYOUT FREQ.                     CI0068
      *DATE-COMPILED.   09/08/14.                                       CI0068
       ENVIRONMENT DIVISION.                                            CI0068
       CONFIGURATION SECTION.                                           CI0068
       SOURCE-COMPUTER. IBM-370.                                        CI0068
       OBJECT-COMPUTER. IBM-370.                                        CI0068
       DATA DIVISION.                                                   CI0068
       WORKING-STORAGE SECTION.                                         CI0068
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
      **              TABLE TA98 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA98-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=98 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA98.                                                CI0068
           04    G-TA98-PARAM.                                          CI0068
             10  G-TA98-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0068
                        VALUE      +053.                                CI0068
             10  G-TA98-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0068
                        VALUE      +001.                                CI0068
             10  G-TA98-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0068
                        VALUE      +010.                                CI0068
             10  G-TA98-NUAPP  PICTURE 99                               CI0068
                        VALUE       0.                                  CI0068
             10  G-TA98-NUTAB  PICTURE X(6)                             CI0068
                        VALUE 'GCPRAR'.                                 CI0068
             10  G-TA98-TABFO  PICTURE XX                 VALUE SPACE.  CI0068
             10  G-TA98-TABCR  PICTURE XX                 VALUE SPACE.  CI0068
             10  G-TA98-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0068
             10  G-TA98-NUSSC  PICTURE X  VALUE   ' '.                  CI0068
             10  G-TA98-NUSSY  PICTURE X                  VALUE SPACE.  CI0068
             10  G-TA98-TRANID PICTURE X(4)               VALUE SPACE.  CI0068
             10  G-TA98-FILSYS.                                         CI0068
             15  G-TA98-USERC  PICTURE X(6)               VALUE SPACE.  CI0068
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0068
           04             TA98.                                         CI0068
            10            TA98-GCPRAR.                                  CI0068
            11            TA98-CTIDA  PICTURE  9(3)                     CI0068
                          VALUE                ZERO.                    CI0068
            11            TA98-PRCOD  PICTURE  9(5)                     CI0068
                          VALUE                ZERO.                    CI0068
            11            TA98-CARTY  PICTURE  99                       CI0068
                          VALUE                ZERO.                    CI0068
            10            TA98-IARTYA PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IARLNA PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQAN  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQSA  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQFM  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQQT  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQBM  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQMO  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQBF  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQSM  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQBW  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQWK  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQIR  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQIF  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQIS  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQIK  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQIW  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQOD  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IARPSA PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IARRGA PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IFQET  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IMLNA  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-IMPRA  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TA98-ZDA20  PICTURE  X(20)                    CI0068
                          VALUE                SPACE.                   CI0068
      **                                                                ADUTAB
       01  TA98-CF           PIC X(01)  VALUE '0'.
      ******************************************************************ADUTAB
      **              TABLE TB5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TB5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TB DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TB5B.                                                CI0068
           04    G-TB5B-PARAM.                                          CI0068
             10  G-TB5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0068
                        VALUE      +154.                                CI0068
             10  G-TB5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0068
                        VALUE      +001.                                CI0068
             10  G-TB5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0068
                        VALUE      +017.                                CI0068
             10  G-TB5B-NUAPP  PICTURE 99                               CI0068
                        VALUE       0.                                  CI0068
             10  G-TB5B-NUTAB  PICTURE X(6)                             CI0068
                        VALUE 'TA005B'.                                 CI0068
             10  G-TB5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0068
             10  G-TB5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0068
             10  G-TB5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0068
             10  G-TB5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0068
             10  G-TB5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0068
             10  G-TB5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0068
             10  G-TB5B-FILSYS.                                         CI0068
             15  G-TB5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0068
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0068
           04             TB5B.                                         CI0068
            10            TB5B-GAPSC.                                   CI0068
            11            TB5B-CTIDA  PICTURE  9(3)                     CI0068
                          VALUE                ZERO.                    CI0068
            11            TB5B-PRCOD  PICTURE  9(5)                     CI0068
                          VALUE                ZERO.                    CI0068
            11            TB5B-PRSCD  PICTURE  X(9)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-PRCODX PICTURE  9(5)                     CI0068
                          VALUE                ZERO.                    CI0068
            10            TB5B-PRCSUB PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-PRCAUT PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-PRCBAS PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-PRCSTK PICTURE  XX                       CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-PRCPRE PICTURE  X(4)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IBDUP  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IUSPR  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-CVSYS  PICTURE  X(2)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IDTOD  PICTURE  X(1)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-GRSFC  PICTURE  99                       CI0068
                          VALUE                ZERO.                    CI0068
            10            TB5B-ZDA18  PICTURE  X(18)                    CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-CMPCTB PICTURE  X(4)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ITERM  PICTURE  X(1)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-AMFAC  PICTURE  S9(7)                    CI0068
                          VALUE                ZERO.                    CI0068
            10            TB5B-ZDA20  PICTURE  X(20)                    CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-CPRBK  PICTURE  X(3)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-CFXDM  PICTURE  99                       CI0068
                          VALUE                ZERO.                    CI0068
            10            TB5B-NGLCS  PICTURE  X(5)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-NDFCS  PICTURE  X(5)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ZDA20  PICTURE  X(20)                    CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-CTNLI  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-CBANK  PICTURE  X(03)                    CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ISYPO  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ISYPP  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ICOPT  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IANPY  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IDSAR  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ICIPT  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IANDS  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IKPMA  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-INMWT  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IVANT  PICTURE  X(1)                     CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ISDAV  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-IUDAV  PICTURE  X                        CI0068
                          VALUE                SPACE.                   CI0068
            10            TB5B-ZDA15  PICTURE  X(15)                    CI0068
                          VALUE                SPACE.                   CI0068
      **                                                                ADUTAB
       01  TB5B-CF           PIC X(01)  VALUE '0'.
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************
      *
      *!WI
       01  WS01-IFQSA
                        PICTURE X.                                      CI0068
      *!WI
       01  WS01-IFQQT
                        PICTURE X.                                      CI0068
       01   DEBUT-WSS.                                                  CI0068
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0068
            05   IK     PICTURE X.                                      CI0068
       01  CONSTANTES-PAC.                                              CI0068
           05  FILLER  PICTURE X(87)   VALUE                            CI0068
                     '6015 CAT09/08/14CI0068ADMIN   14:34:33CI0068P AMERCI0068
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0068
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0068
           05  NUGNA   PICTURE X(5).                                    CI0068
           05  APPLI   PICTURE X(3).                                    CI0068
           05  DATGN   PICTURE X(8).                                    CI0068
           05  PROGR   PICTURE X(6).                                    CI0068
           05  CODUTI  PICTURE X(8).                                    CI0068
           05  TIMGN   PICTURE X(8).                                    CI0068
           05  PROGE   PICTURE X(8).                                    CI0068
           05  COBASE  PICTURE X(4).                                    CI0068
           05  DATGNC  PICTURE X(10).                                   CI0068
           05  RELEAS  PICTURE X(7).                                    CI0068
           05  DATGE   PICTURE X(10).                                   CI0068
           05  DATSQ   PICTURE X(10).                                   CI0068
       01  DATCE.                                                       CI0068
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0068
         05  DATOR.                                                     CI0068
           10  DATOA  PICTURE XX.                                       CI0068
           10  DATOM  PICTURE XX.                                       CI0068
           10  DATOJ  PICTURE XX.                                       CI0068
       01   VARIABLES-CONDITIONNELLES.                                  CI0068
            05                  FT      PICTURE X VALUE '0'.            CI0068
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0068
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0068
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0068
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      ** THIS SEGMENT CONTAINS THE INPUT/OUTPUT SEGMENT FOR CI0068     *
      ******************************************************************
      *
      *!WF DSP=DS DSL=DU SEL=68 FOR=I LEV=1 PLT=05
       01                 DS00.                                         CI0068
          05              DS00-SUITE.                                   CI0068
            15       FILLER         PICTURE  X(00159).                  CI0068
       01                 DS68  REDEFINES      DS00.                    CI0068
            10            DS68-C299.                                    CI0068
            11            DS68-CTID.                                    CI0068
            12            DS68-CTIDA  PICTURE  9(3).                    CI0068
            12            DS68-CTIDN.                                   CI0068
            13            DS68-CTIDNP PICTURE  X(13).                   CI0068
            13            DS68-CTIDND PICTURE  9(11).                   CI0068
            10            DS68-PRCOD  PICTURE  9(5).                    CI0068
            10            DS68-CARTZ  PICTURE  99.                      CI0068
            10            DS68-ALPAGR PICTURE  S9(7)V99                 CI0068
                          COMPUTATIONAL-3.                              CI0068
            10            DS68-CEIT   PICTURE  9(3).                    CI0068
            10            DS68-IFQSA  PICTURE  X.                       CI0068
            10            DS68-IFQQT  PICTURE  X.                       CI0068
            10            DS68-IFQAN  PICTURE  X.                       CI0068
            10            DS68-IFQFM  PICTURE  X.                       CI0068
            10            DS68-IFQBM  PICTURE  X.                       CI0068
            10            DS68-IFQMO  PICTURE  X.                       CI0068
            10            DS68-IFQBF  PICTURE  X.                       CI0068
            10            DS68-IFQSM  PICTURE  X.                       CI0068
            10            DS68-IFQBW  PICTURE  X.                       CI0068
            10            DS68-IFQWK  PICTURE  X.                       CI0068
            10            DS68-IFQIR  PICTURE  X.                       CI0068
            10            DS68-IFQET  PICTURE  X.                       CI0068
            10            DS68-IFQSAT PICTURE  X(01).                   CI0068
            10            DS68-IFQQTT PICTURE  X(01).                   CI0068
            10            DS68-IFQANT PICTURE  X(01).                   CI0068
            10            DS68-IFQBMT PICTURE  X(01).                   CI0068
            10            DS68-IFQMOT PICTURE  X(01).                   CI0068
            10            DS68-CTID02 PICTURE  X(27).                   CI0068
            10            DS68-PRCODA PICTURE  X(5).                    CI0068
            10            DS68-CUPIQ  PICTURE  X.                       CI0068
            10            DS68-FILLER PICTURE  X(67).                   CI0068
      *
      *
      *
      *
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0068
          05              MS00-SUITE.                                   CI0068
            15       FILLER         PICTURE  X(00542).                  CI0068
       01                 MS03  REDEFINES      MS00.                    CI0068
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0068
                          COMPUTATIONAL-3.                              CI0068
            10            MS03-CMSSF  PICTURE  XX.                      CI0068
            10            MS03-DU09.                                    CI0068
            11            MS03-CMESA  PICTURE  S9(9)                    CI0068
                          BINARY.                                       CI0068
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0068
                          BINARY.                                       CI0068
            11            MS03-CMESB  PICTURE  S9(9)                    CI0068
                          BINARY.                                       CI0068
            11            MS03-CMSST  PICTURE  S9(9)                    CI0068
                          BINARY.                                       CI0068
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0068
                          BINARY.                                       CI0068
            11            MS03-QELLAA PICTURE  S9(9)                    CI0068
                          BINARY.                                       CI0068
            11            MS03-TMESS4 PICTURE  X(512).                  CI0068
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0068
            10            MX11-QMSGS  PICTURE  9(03).                   CI0068
            10            MX11-PJ09                                     CI0068
                          OCCURS       025     TIMES.                   CI0068
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0068
                          COMPUTATIONAL-3.                              CI0068
            11            MX11-CMESB  PICTURE  S9(9)                    CI0068
                          BINARY.                                       CI0068
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DS68
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0068
      *               *                                   *             CI0068
      *               *INITIALISATIONS                    *             CI0068
      *               *                                   *             CI0068
      *               *************************************.            CI0068
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0068
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0068
      *               *                                   *             CI0068
      *               *FIN DE TRAITEMENT                  *             CI0068
      *               *                                   *             CI0068
      *               *************************************.            CI0068
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0068
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *INITILIZATION PROCESSING           *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35BA.    NOTE *INITILIZE OUTPUT PARMS             *.
       F35BA.                                                           lv10
           MOVE        DS68-IFQSA TO WS01-IFQSA
           MOVE        DS68-IFQQT TO WS01-IFQQT
           MOVE        'N' TO DS68-IFQAN
           MOVE        'N' TO DS68-IFQSA
           MOVE        'N' TO DS68-IFQFM
           MOVE        'N' TO DS68-IFQQT
           MOVE        'N' TO DS68-IFQBM
           MOVE        'N' TO DS68-IFQMO
           MOVE        'N' TO DS68-IFQBF
           MOVE        'N' TO DS68-IFQSM
           MOVE        'N' TO DS68-IFQBW
           MOVE        'N' TO DS68-IFQWK
           MOVE        'N' TO DS68-IFQIR
           MOVE        'N' TO DS68-IFQET
           MOVE        'N' TO DS68-IFQANT
           MOVE        'N' TO DS68-IFQSAT
           MOVE        'N' TO DS68-IFQQTT
           MOVE        'N' TO DS68-IFQBMT
           MOVE        'N' TO DS68-IFQMOT.
       F35BA-FN. EXIT.
      *N35CA.    NOTE *DETERMINE INQUIRY/UPDATE PARM      *.
       F35CA.    IF    DS68-CUPIQ NOT = 'U'                             lv10
                 AND   DS68-CUPIQ NOT = 'I'
                 NEXT SENTENCE ELSE GO TO     F35CA-FN.
           MOVE        'I' TO DS68-CUPIQ.
       F35CA-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               ******* SOURCE FREQUENCIES ******   *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BA.    NOTE ******    FUND ACCOUNTS      ****   *.
       F40BA.    IF    DS68-CTIDA = 002                                 lv10
                 AND   DS68-CARTZ NOT = 05
                 NEXT SENTENCE ELSE GO TO     F40BA-FN.
      **
           MOVE        'Y' TO DS68-IFQAN
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQFM
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQBM
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQBF
           MOVE        'Y' TO DS68-IFQSM
           MOVE        'Y' TO DS68-IFQBW
           MOVE        'Y' TO DS68-IFQWK
           MOVE        'Y' TO DS68-IFQIR
           MOVE        'Y' TO DS68-IFQANT
           MOVE        'Y' TO DS68-IFQSAT
           MOVE        'Y' TO DS68-IFQQTT
           MOVE        'Y' TO DS68-IFQBMT
           MOVE        'Y' TO DS68-IFQMOT.
       F40BA-FN. EXIT.
      *N40DA.    NOTE ** IF ACCT IS A CERT & INTEREST *   *.
       F40DA.    IF    DS68-CTIDA = 001                                 lv10
                 AND   DS68-CARTZ = 06
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *********************************
      ** IF THE ACCOUNT IS A CERT AND *
      ** IDSC INTEREST ARRANGEMENTS   *
      *********************************
      *N40EA.    NOTE ** CASE OF STRUCTURE FOR PRCOD  *   *.
       F40EA.                                                           lv15
      *********************************
      ** CASE OF STRUCTURE FOR INPUT  *
      ** PARM PRODUCT CODE DS68-PRCOD *
      *********************************
      *N40EC.    NOTE **** CASH RESERVE PROD. CODES ***   *.
       F40EC.    IF    DS68-PRCOD =                                     lv20
                       00662
                 OR    00972
                 OR    00660
                 OR    00970
                 NEXT SENTENCE ELSE GO TO     F40EC-FN.
      *********************************
      ***** CURRENTLY SOLD PRODUCTS ***
      ** 00662 - NON QUALIFIED        *
      ** 00972 - QUALIFIED            *
      **** NO LONGER SOLD PRODUCTS ****
      ** 00660 - NON QUALIFIED        *
      ** 00970 - QUALIFIED            *
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40EC-900. GO TO F40EA-FN.
       F40EC-FN. EXIT.
      *N40EG.    NOTE ** AMEX BANK INTL CERT MONTHLY **   *.
       F40EG.    IF    DS68-PRCOD =                                     lv20
                       00201
                 OR    00202
                 OR    00203
                 OR    00204
                 OR    00205
                 NEXT SENTENCE ELSE GO TO     F40EG-FN.
      *********************************
      ** AMERICAN EXPRESS BANK        *
      ** INTERNATIONAL CERTIFICATE W/ *
      ** WITH A MONTHLY TERM ONLY     *
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO.
       F40EG-900. GO TO F40EA-FN.
       F40EG-FN. EXIT.
      *N40EK.    NOTE *** AMEX BANK INTL CERT E.O.T. **   *.
       F40EK.    IF    DS68-PRCOD =                                     lv20
                       00301
                 OR    00302
                 OR    00303
                 OR    00304
                 OR    00305
                 NEXT SENTENCE ELSE GO TO     F40EK-FN.
      *********************************
      ** AMERICAN EXPRESS BANK        *
      ** INTERNATIONAL CERTIFICATE W/ *
      ** WITH A END OF TERM ONLY      *
      *********************************
      **
           MOVE        'Y' TO DS68-IFQET.
       F40EK-900. GO TO F40EA-FN.
       F40EK-FN. EXIT.
      *N40EO.    NOTE **** STOCK MARKET CERTIFICATE ***   *.
       F40EO.    IF    DS68-PRCOD =                                     lv20
                       00180
                 OR    00181
                 OR    00960
                 OR    00961
                 OR    00181
                 NEXT SENTENCE ELSE GO TO     F40EO-FN.
      *** MARKET STRATEGY CERTIFICATE**
      *********************************
      **** STOCK MARKET CERTIFICATES **
      *** MARKET STRATEGY CERTIFICATE**
      *********************************
      **
           MOVE        'Y' TO DS68-IFQET.
       F40EO-900. GO TO F40EA-FN.
       F40EO-FN. EXIT.
      *N40ER.    NOTE **** FLEX SAVINGS CERTIFICATE ***   *.
       F40ER.    IF    DS68-PRCOD =                                     lv20
                       00166
                 OR    00973
                 NEXT SENTENCE ELSE GO TO     F40ER-FN.
      *********************************
      **** FLEX SAVINGS CERTIFICATE ***
      **** 00166 - NON QUALIFIED    ***
      **** 00973 - QUALIFIED        ***
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN
           MOVE        'Y' TO DS68-IFQET.
       F40ER-900. GO TO F40EA-FN.
       F40ER-FN. EXIT.
      *N40EU.    NOTE **** FLEX SAVINGS CERTIFICATE ***   *.
       F40EU.    IF    DS68-PRCOD =                                     lv20
                       00165
                 OR    00971
                 NEXT SENTENCE ELSE GO TO     F40EU-FN.
      *********************************
      **** FLEX SAVINGS CERTIFICATE ***
      **** POSSIBLY WITH ODD TERM   ***
      **** 00165 - NON QUALIFIED    ***
      **** 00971 - QUALIFIED        ***
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN
           MOVE        'Y' TO DS68-IFQET.
      *N40EV.    NOTE ******* CHECK FOR ODD TERM ******   *.
       F40EV.    IF    DS68-CEIT = 007                                  lv25
                 OR    011
                 OR    013
                 OR    019
                 OR    025
                 OR    031
                 OR    037
                 NEXT SENTENCE ELSE GO TO     F40EV-FN.
      *********************************
      **** CHECKING FOR ODD TERMS.  ***
      **** IF ODD TERMS RESET SOME  ***
      **** INDICATORS TO 'N'.       ***
      *********************************
      **
           MOVE        'N' TO DS68-IFQQT
           MOVE        'N' TO DS68-IFQSA
           MOVE        'N' TO DS68-IFQAN.
       F40EV-FN. EXIT.
       F40EU-900. GO TO F40EA-FN.
       F40EU-FN. EXIT.
      *N40EY.    NOTE **** DEFAULT SETTINGS FOR CASE **   *.
       F40EY.                                                           lv20
      *********************************
      **** DEFAULT SETTINGS FOR     ***
      **** CASE STATEMENT.          ***
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN
           MOVE        'Y' TO DS68-IFQET.
       F40EY-FN. EXIT.
       F40EA-FN. EXIT.
       F40DA-FN. EXIT.
      *N40HA.    NOTE ** IF ACCOUNT IS A CERT AND SP  *   *.
       F40HA.    IF    DS68-CTIDA = 001                                 lv10
                 AND   DS68-CARTZ = 02
                 NEXT SENTENCE ELSE GO TO     F40HA-FN.
      *********************************
      ** IF ACCOUNT IS A CERTIFICATE  *
      ** AND AN IDSC SYSTEMATIC       *
      ** PARTIAL PAYMENT              *
      *********************************
      *N40IA.    NOTE ** CASE OF STRUCTURE FOR PRCOD  *   *.
       F40IA.                                                           lv15
      *********************************
      ** CASE OF STRUCTURE FOR INPUT  *
      ** PARM PRODUCT CODE DS68-PRCOD *
      *********************************
      *N40IC.    NOTE *** AMEX BANK INTL CERTIFICATE **   *.
       F40IC.    IF    DS68-PRCOD =                                     lv20
                       00201
                 OR    00202
                 OR    00203
                 OR    00204
                 OR    00205
                 OR    00301
                 OR    00302
                 OR    00303
                 OR    00304
                 OR    00305
                 NEXT SENTENCE ELSE GO TO     F40IC-FN.
      *********************************
      ** AMERICAN EXPRESS BANK        *
      ** INTERNATIONAL CERTIFICATE    *
      ** NOT ALLOWED WITH IDSC        *
      ** SYSTEMATIC PARTIAL PAYMENT   *
      ** SCHEDULED DISBURSEMENT.      *
      *********************************
      **
      **
      **
       F40IC-900. GO TO F40IA-FN.
       F40IC-FN. EXIT.
      *N40IE.    NOTE *** MARKET STRATEGY CEETIFICATE**   *.
       F40IE.    IF    DS68-PRCOD =                                     lv20
                       00181
                 OR    00961
                 NEXT SENTENCE ELSE GO TO     F40IE-FN.
      *********************************
      ** 00181 - NON QUALIFIED        *
      ** 00961 - QUALIFIED            *
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQWK
           MOVE        'Y' TO DS68-IFQBM
           MOVE        'Y' TO DS68-IFQFM
           MOVE        'Y' TO DS68-IFQIR.
       F40IE-900. GO TO F40IA-FN.
       F40IE-FN. EXIT.
      *N40IG.    NOTE *** CASH RESERVE 3 CERTIFICATE **   *.
       F40IG.    IF    DS68-PRCOD =                                     lv20
                       00662
                 OR    00972
                 NEXT SENTENCE ELSE GO TO     F40IG-FN.
      *********************************
      *** CASH RESERVE 3 CERTIFICATE **
      ** 00662 - NON QUALIFIED        *
      ** 00972 - QUALIFIED            *
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40IG-900. GO TO F40IA-FN.
       F40IG-FN. EXIT.
      *N40IK.    NOTE **** FLEX SAVINGS CERTIFICATE ***   *.
       F40IK.    IF    DS68-PRCOD =                                     lv20
                       00165
                 OR    00166
                 OR    00971
                 OR    00973
                 NEXT SENTENCE ELSE GO TO     F40IK-FN.
      *********************************
      **** FLEX SAVINGS CERTIFICATE ***
      ** 00165 & 00166 - NON QUALIFIED*
      ** 00971 & 00973 - QUALIFIED    *
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40IK-900. GO TO F40IA-FN.
       F40IK-FN. EXIT.
      *N40IO.    NOTE **PREFERRED INVESTOR CERTIFICATE*   *.
       F40IO.    IF    DS68-PRCOD =                                     lv20
                       00250
                 OR    00950
                 NEXT SENTENCE ELSE GO TO     F40IO-FN.
      *********************************
      **PREFERRED INVESTOR CERTIFICATE*
      ** 00250 - NON QUALIFIED        *
      ** 00950 - QUALIFIED            *
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40IO-900. GO TO F40IA-FN.
       F40IO-FN. EXIT.
      *N40IR.    NOTE **** SERIES D - 1 CERTIFICATE ***   *.
       F40IR.    IF    DS68-PRCOD =                                     lv20
                       00400
                 OR    00990
                 NEXT SENTENCE ELSE GO TO     F40IR-FN.
      *********************************
      **** SERIES D - 1 CERTIFICATE ***
      **** 00400 - NON QUALIFIED    ***
      **** 00990 - QUALIFIED        ***
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40IR-900. GO TO F40IA-FN.
       F40IR-FN. EXIT.
      *N40IU.    NOTE **** CASH RESERVE CERTIFICATE ***   *.
       F40IU.    IF    DS68-PRCOD =                                     lv20
                       00660
                 OR    00970
                 NEXT SENTENCE ELSE GO TO     F40IU-FN.
      *********************************
      **** CASH RESERVE CERTIFICATE ***
      **** 00660 - NON QUALIFIED    ***
      **** 00970 - QUALIFIED        ***
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40IU-900. GO TO F40IA-FN.
       F40IU-FN. EXIT.
      *N40IW.    NOTE **** INSTALLMENT CERTIFICATE ****   *.
       F40IW.    IF    DS68-PRCOD =                                     lv20
                       00650
                 OR    00651
                 OR    00652
                 OR    00653
                 OR    00980
                 OR    00981
                 OR    00982
                 OR    00983
                 NEXT SENTENCE ELSE GO TO     F40IW-FN.
      *********************************
      **** INSTALLMENT CERTIFICATE ****
      ** NON QUALIFIED - 00650, 00651 *
      **                00652 & 00653 *
      ** QUALIFIED     - 00980, 00981 *
      **                00982 & 00983 *
      *********************************
      *
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40IW-900. GO TO F40IA-FN.
       F40IW-FN. EXIT.
      *N40IY.    NOTE **** DEFAULT SETTINGS FOR CASE **   *.
       F40IY.                                                           lv20
      *********************************
      **** DEFAULT SETTINGS FOR     ***
      **** CASE STATEMENT.          ***
      *********************************
      **
           MOVE        'Y' TO DS68-IFQMO
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F40IY-FN. EXIT.
       F40IA-FN. EXIT.
       F40HA-FN. EXIT.
      *N40NA.    NOTE **IF ACCOUNT IS NOT CERT OR FUND*   *.
       F40NA.    IF    DS68-CTIDA NOT = 001                             lv10
                 AND   DS68-CTIDA NOT = 002
                 NEXT SENTENCE ELSE GO TO     F40NA-FN.
      *********************************
      ** IF ACCOUNT IS NOT A MUTUAL   *
      ** FUND AND NOT A CERTIFICATE   *
      ** SEND ERROR TO CALLING PROGRAM*
      *********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012803 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40NA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               ***** DESTINATION FREQUENCIES ***   *
      *               *                                   *
      *               *************************************.
       F45.           EXIT.                                             lv05
      *N45BA.    NOTE ** ANNT/LIFE PREM ANN GROSS AMT *   *.
       F45BA.    IF    DS68-ALPAGR > ZERO                               lv10
                 NEXT SENTENCE ELSE GO TO     F45BA-FN.
      **
           MOVE        'N' TO DS68-IFQFM
           MOVE        'N' TO DS68-IFQBM
           MOVE        'N' TO DS68-IFQBF
           MOVE        'N' TO DS68-IFQSM
           MOVE        'N' TO DS68-IFQBW
           MOVE        'N' TO DS68-IFQWK
           MOVE        'N' TO DS68-IFQIR
           MOVE        'N' TO DS68-IFQANT
           MOVE        'N' TO DS68-IFQSAT
           MOVE        'N' TO DS68-IFQQTT
           MOVE        'N' TO DS68-IFQBMT
           MOVE        'N' TO DS68-IFQMOT.
      *N45CA.    NOTE *IF INQUIRY; SET SA/QT/AN FREQ      *.
       F45CA.    IF    DS68-CUPIQ = 'I'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45CA-FN.
           MOVE        'Y' TO DS68-IFQQT
           MOVE        'Y' TO DS68-IFQSA
           MOVE        'Y' TO DS68-IFQAN.
       F45CA-FN. EXIT.
      *N45EA.    NOTE *IF INPUT QUARTERLY FREQ = N        *.
       F45EA.    IF    WS01-IFQQT = 'N'                                 lv15
                 AND   DS68-IFQQT = 'Y'
                 AND   DS68-CUPIQ = 'U'
                 NEXT SENTENCE ELSE GO TO     F45EA-FN.
      **
           MOVE        'N' TO DS68-IFQQT.
       F45EA-FN. EXIT.
      *N45JA.    NOTE *IF INPUT SEMI-ANNUAL FREQ = N      *.
       F45JA.    IF    WS01-IFQSA = 'N'                                 lv15
                 AND   DS68-IFQSA = 'Y'
                 AND   DS68-CUPIQ = 'U'
                 NEXT SENTENCE ELSE GO TO     F45JA-FN.
           MOVE        'N' TO DS68-IFQSA.
       F45JA-FN. EXIT.
       F45BA-FN. EXIT.
      *N45KA.    NOTE *RANDOM TABLE READ FOR TB5B         *.
       F45KA.                                                           lv10
           MOVE        '1' TO TB5B-CF
           MOVE        DS68-CTID02 (1 : 3) TO
           TB5B-CTIDA
           MOVE        DS68-PRCODA TO TB5B-PRCOD
           MOVE        SPACES TO TB5B-PRSCD
           PERFORM     F92TC THRU F92TC-FN.
       F45KA-FN. EXIT.
      *N45LA.    NOTE *IF DESTINATIONS IS VANTAGE VUL3    *.
       F45LA.    IF    TB5B-CF = '1'                                    lv10
                 AND   TB5B-IVANT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F45LA-FN.
      **
           MOVE        '1' TO TA98-CF
           MOVE        DS68-CTID02 (1 : 3) TO
           TA98-CTIDA
           MOVE        DS68-PRCODA TO TA98-PRCOD
           MOVE        01 TO TA98-CARTY
           PERFORM     F92TA THRU F92TA-FN.
      *N45LD.    NOTE *IF TABLE ENTRY WAS FOUND           *.
       F45LD.    IF    TA98-CF = '1'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F45LD-FN.
                 IF    DS68-IFQAN = 'Y'                                 DOT
           MOVE        TA98-IFQAN TO DS68-IFQAN.
                 IF    DS68-IFQSA = 'Y'                                 DOT
           MOVE        TA98-IFQSA TO DS68-IFQSA.
                 IF    DS68-IFQFM = 'Y'                                 DOT
           MOVE        TA98-IFQFM TO DS68-IFQFM.
                 IF    DS68-IFQQT = 'Y'                                 DOT
           MOVE        TA98-IFQQT TO DS68-IFQQT.
                 IF    DS68-IFQBM = 'Y'                                 DOT
           MOVE        TA98-IFQBM TO DS68-IFQBM.
                 IF    DS68-IFQMO = 'Y'                                 DOT
           MOVE        TA98-IFQMO TO DS68-IFQMO.
                 IF    DS68-IFQBF = 'Y'                                 DOT
           MOVE        TA98-IFQBF TO DS68-IFQBF.
                 IF    DS68-IFQSM = 'Y'                                 DOT
           MOVE        TA98-IFQSM TO DS68-IFQSM.
                 IF    DS68-IFQBW = 'Y'                                 DOT
           MOVE        TA98-IFQBW TO DS68-IFQBW.
                 IF    DS68-IFQWK = 'Y'                                 DOT
           MOVE        TA98-IFQWK TO DS68-IFQWK.
                 IF    DS68-IFQIR = 'Y'                                 DOT
           MOVE        TA98-IFQIR TO DS68-IFQIR.
                 IF    DS68-IFQET = 'Y'                                 DOT
           MOVE        TA98-IFQET TO DS68-IFQET.
                 IF    DS68-IFQANT = 'Y'                                DOT
           MOVE        TA98-IFQAN TO DS68-IFQANT.
                 IF    DS68-IFQSAT = 'Y'                                DOT
           MOVE        TA98-IFQSA TO DS68-IFQSAT.
                 IF    DS68-IFQQTT = 'Y'                                DOT
           MOVE        TA98-IFQQT TO DS68-IFQQTT.
                 IF    DS68-IFQBMT = 'Y'                                DOT
           MOVE        TA98-IFQBM TO DS68-IFQBMT.
                 IF    DS68-IFQMOT = 'Y'                                DOT
           MOVE        TA98-IFQMO TO DS68-IFQMOT.
       F45LD-FN. EXIT.
       F45LA-FN. EXIT.
       F45-FN.   EXIT.
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
      *               *MISCELLANEOUS ROUTINES             *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA98         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA98-TABFO                             ADUTAB
           COMPUTE     G-TA98-LTH = 60 + G-TA98-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA98-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA98)                                ADUTAB
                       LENGTH (G-TA98-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA98-TABCR NOT = '00'                          DOT
           PERFORM     F92TB THRU F92TB-FN.                             ADUTAB
       F92TA-FN. EXIT.
      *N92TB.    NOTE *ERROR ON TABLE READ FOR TA98       *.
       F92TB.                                                           lv10
           MOVE        '0' TO TA98-CF.
       F92TB-FN. EXIT.
      *N92TC.    NOTE *RANDOM TABLE READ FOR TB5B         *.            ADUTAB
       F92TC.                                                           lv10
           MOVE        'R1' TO G-TB5B-TABFO                             ADUTAB
           COMPUTE     G-TB5B-LTH = 60 + G-TB5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TB5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TB5B)                                ADUTAB
                       LENGTH (G-TB5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TB5B-TABCR NOT = '00'                          DOT
           PERFORM     F92TD THRU F92TD-FN.                             ADUTAB
       F92TC-FN. EXIT.
      *N92TD.    NOTE *ERROR ON TABLE READ FOR TA5B       *.
       F92TD.                                                           lv10
           MOVE        '0' TO TB5B-CF.
       F92TD-FN. EXIT.
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
