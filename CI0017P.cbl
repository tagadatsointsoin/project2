       IDENTIFICATION DIVISION.                                         CI0017
       PROGRAM-ID.  CI0017P.                                            CI0017
      *AUTHOR.         M\M - UNFORMAT DEST LINES MOD.                   CI0017
      *DATE-COMPILED.   09/08/14.                                       CI0017
       ENVIRONMENT DIVISION.                                            CI0017
       CONFIGURATION SECTION.                                           CI0017
       SOURCE-COMPUTER. IBM-370.                                        CI0017
       OBJECT-COMPUTER. IBM-370.                                        CI0017
       DATA DIVISION.                                                   CI0017
       WORKING-STORAGE SECTION.                                         CI0017
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
      *
      ******************************************************************
      **     SEGMENT THAT EXTRACTED ADDRESS INFORMATION FROM           *
      **     CMU001DY.                            .                    *
      ******************************************************************
      *
      *!WF DSP=CP DSL=CP SEL=1030 FOR=I LEV=1 PLT=CP
       01                 CP00.                                         CI0017
          05              CP00-SUITE.                                   CI0017
            15       FILLER         PICTURE  X(00238).                  CI0017
       01                 CP10  REDEFINES      CP00.                    CI0017
            10            CP10-GERTC  PICTURE  X.                       CI0017
            10            CP10-CLTYP  PICTURE  X.                       CI0017
            10            CP10-XACT   PICTURE  X.                       CI0017
            10            CP10-CLNAM  PICTURE  X(70).                   CI0017
            10            CP10-CLNAMH PICTURE  X(6).                    CI0017
            10            CP10-CLNAMF PICTURE  X(20).                   CI0017
            10            CP10-CLNAMM.                                  CI0017
            11            CP10-CLNAMI PICTURE  X.                       CI0017
            11            CP10-CLNAMR PICTURE  X(14).                   CI0017
            10            CP10-CLNAML PICTURE  X(25).                   CI0017
            10            CP10-CLNAMS PICTURE  X(4).                    CI0017
            10            CP10-CLORN  PICTURE  X(45).                   CI0017
            10            CP10-CLPHC  PICTURE  9(8)                     CI0017
                          BINARY.                                       CI0017
            10            CP10-CLNAMX PICTURE  X(46).                   CI0017
       01                 CP30  REDEFINES      CP00.                    CI0017
            10            CP30-GERQCD PICTURE  X.                       CI0017
            10            CP30-GERTC  PICTURE  X.                       CI0017
            10            CP30-ALNGTH PICTURE  X(2).                    CI0017
            10            CP30-ALINE  PICTURE  X(65).                   CI0017
            10            CP30-GECIT  PICTURE  X(25).                   CI0017
            10            CP30-GEST   PICTURE  X(8).                    CI0017
            10            CP30-GECTRY PICTURE  X(20).                   CI0017
            10            CP30-GEPCD  PICTURE  X(12).                   CI0017
            10            CP30-FILLER PICTURE  X.                       CI0017
            10       FILLER         PICTURE  X(00103).                  CI0017
      *
      *
      *
      *
           EJECT                                                        AAOFIA
      ******************************************************************AAOFIA
      **                WORK AREA FOR AAOFIA                           *AAOFIA
      ******************************************************************AAOFIA
       SKIP2                                                            AAOFIA
      *AREA USED TO MANIPULATE THE SOURCE FIELD                         AAOFIA
       01                 CT00-UCTID.                                   AAOFIA
          05              CT00-UCTIDX  PIC X  OCCURS 35.                AAOFIA
       01                 CT01-UCTID REDEFINES CT00-UCTID.              AAOFIA
          05              CT01-FILLER  PIC X(32).                       AAOFIA
          05              CT01-CTIDAX  PIC X(03).                       AAOFIA
       01                 CT02-UCTID REDEFINES CT00-UCTID.              AAOFIA
          05              CT02-FILLER  PIC X(34).                       AAOFIA
          05              CT02-GECKDX  PIC X(01).                       AAOFIA
       01                 CT03-UCTID REDEFINES CT00-UCTID.              AAOFIA
          05              CT03-FILLER  PIC X(11).                       AAOFIA
          05              CT03-CTIDNX  PIC X(24).                       AAOFIA
      *SPECIAL INDEX                                                    AAOFIA
       01                 ICT00D       PIC S9(4) COMP.                  AAOFIA
       SKIP2                                                            AAOFIA
      *PASS AREA                                                        AAOFIA
       01               7-CT00-UCTID.                                   AAOFIA
          05            7-CT00-CTID.                                    AAOFIA
             10         7-CT00-CTIDAX     PIC X(03).                    AAOFIA
             10         7-CT00-CTIDNX     PIC X(24).                    AAOFIA
          05            7-CT00-GECKDX     PIC X(01).                    AAOFIA
          05            7-CT00-FILLER     PIC X(07).                    AAOFIA
      *
      ******************************************************************
      **     MISCELLANEOUS WORK FIELDS.                                *
      ******************************************************************
      *
       01  W-WORK-MISC.
      *!WI
           05  W-CP30-ALINE
                        PICTURE X(65).                                  CI0017
           05  W-WORK-1PTR     PIC 999.
      *!WI
           05  W-WORK-XZ1
                        PICTURE X.                                      CI0017
           05  W-WORK-1CTR     PIC 999.
      *!WI
           05  W-DL01-XZ4
                        PICTURE X(4).                                   CI0017
      *
      *
      *
      *
       01   DEBUT-WSS.                                                  CI0017
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0017
            05   IK     PICTURE X.                                      CI0017
       01  CONSTANTES-PAC.                                              CI0017
           05  FILLER  PICTURE X(87)   VALUE                            CI0017
                     '6015 CAT09/08/14CI0017ADMIN   14:33:56CI0017P AMERCI0017
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0017
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0017
           05  NUGNA   PICTURE X(5).                                    CI0017
           05  APPLI   PICTURE X(3).                                    CI0017
           05  DATGN   PICTURE X(8).                                    CI0017
           05  PROGR   PICTURE X(6).                                    CI0017
           05  CODUTI  PICTURE X(8).                                    CI0017
           05  TIMGN   PICTURE X(8).                                    CI0017
           05  PROGE   PICTURE X(8).                                    CI0017
           05  COBASE  PICTURE X(4).                                    CI0017
           05  DATGNC  PICTURE X(10).                                   CI0017
           05  RELEAS  PICTURE X(7).                                    CI0017
           05  DATGE   PICTURE X(10).                                   CI0017
           05  DATSQ   PICTURE X(10).                                   CI0017
       01  DATCE.                                                       CI0017
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0017
         05  DATOR.                                                     CI0017
           10  DATOA  PICTURE XX.                                       CI0017
           10  DATOM  PICTURE XX.                                       CI0017
           10  DATOJ  PICTURE XX.                                       CI0017
       01   VARIABLES-CONDITIONNELLES.                                  CI0017
            05                  FT      PICTURE X VALUE '0'.            CI0017
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0017
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0017
            05           ICT00L PICTURE S9(4) VALUE  ZERO.              AAOFIA
            05           ICT00R PICTURE S9(4) VALUE  ZERO.              AAOFIA
            05           ICT00M PICTURE S9(4) VALUE +0035.              AAOFIA
       01   ZONES-UTILISATEUR PICTURE X.                                CI0017
       LINKAGE SECTION.                                                 ADU002
      *                                                                 AMDU01
      ******************************************************************AMDU01
      **     SEGMENT THAT CONTAINS THE REFORMATTED DESTINATION         *AMDU01
      **     INFORMATION LINES(WHO, WHERE AND HOW).                    *AMDU01
      ******************************************************************AMDU01
      *                                                                 AMDU01
      *!WF DSP=DL DSL=DU SEL=01 FOR=I LEV=1                             AMDU01
       01                 DL00.                                         CI0017
          05              DL00-SUITE.                                   CI0017
            15       FILLER         PICTURE  X(01202).                  CI0017
       01                 DL01  REDEFINES      DL00.                    CI0017
            10            DL01-TWHOL1 PICTURE  X(100).                  CI0017
            10            DL01-TWHOL2 PICTURE  X(100).                  CI0017
            10            DL01-TWHOL3 PICTURE  X(100).                  CI0017
            10            DL01-TWHRL1 PICTURE  X(100).                  CI0017
            10            DL01-TWHRL2 PICTURE  X(100).                  CI0017
            10            DL01-TWHRL3 PICTURE  X(100).                  CI0017
            10            DL01-TWHRL4 PICTURE  X(100).                  CI0017
            10            DL01-TWHRL5 PICTURE  X(100).                  CI0017
            10            DL01-TWHRL6 PICTURE  X(100).                  CI0017
            10            DL01-TWHRL7 PICTURE  X(100).                  CI0017
            10            DL01-THOWL  PICTURE  X(100).                  CI0017
            10            DL01-CPAY1  PICTURE  X(2).                    CI0017
            10            DL01-FILLER PICTURE  X(100).                  CI0017
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 AMDU01
      *                                                                 AMDU02
      ******************************************************************AMDU02
      **     SEGMENT THAT CONTAINS THE DESTINATION DATA FOR FORMATTING *AMDU02
      **     INTO DESTINATION INFORMATION LINES.                       *AMDU02
      ******************************************************************AMDU02
      *                                                                 AMDU02
      *!WF DSP=DI DSL=DU SEL=02 FOR=I LEV=1                             AMDU02
       01                 DI00.                                         CI0017
          05              DI00-SUITE.                                   CI0017
            15       FILLER         PICTURE  X(00800).                  CI0017
       01                 DI02  REDEFINES      DI00.                    CI0017
            10            DI02-CPAY1  PICTURE  X(2).                    CI0017
            10            DI02-ISHAD  PICTURE  X.                       CI0017
            10            DI02-CTTLN1 PICTURE  X(30).                   CI0017
            10            DI02-CTTLN2 PICTURE  X(30).                   CI0017
            10            DI02-CTTLN3 PICTURE  X(30).                   CI0017
            10            DI02-CL24.                                    CI0017
            11            DI02-GELL   PICTURE  9(4)                     CI0017
                          BINARY.                                       CI0017
            11            DI02-CL24K.                                   CI0017
            12            DI02-GECSQ  PICTURE  S9(3)                    CI0017
                          COMPUTATIONAL-3.                              CI0017
            11            DI02-GECSD  PICTURE  9(8).                    CI0017
            11            DI02-GECED  PICTURE  9(8).                    CI0017
            11            DI02-CREQ2  PICTURE  X.                       CI0017
            11            DI02-FILLER PICTURE  X(4).                    CI0017
            11            DI02-GECTA  PICTURE  X.                       CI0017
            11            DI02-GELCD  PICTURE  9(8).                    CI0017
            11            DI02-GEADS  PICTURE  9.                       CI0017
            11            DI02-GECIT  PICTURE  X(25).                   CI0017
            11            DI02-GECTRY PICTURE  X(20).                   CI0017
            11            DI02-GECTY  PICTURE  9(3).                    CI0017
            11            DI02-GEPCD  PICTURE  X(12).                   CI0017
            11            DI02-GEST   PICTURE  X(8).                    CI0017
            11            DI02-IRESA  PICTURE  X.                       CI0017
            11            DI02-FILLER PICTURE  X(8).                    CI0017
            11            DI02-GESAD  PICTURE  X(30)                    CI0017
                          OCCURS       003     TIMES.                   CI0017
            10            DI02-CLTYP  PICTURE  X.                       CI0017
            10            DI02-CLORN  PICTURE  X(45).                   CI0017
            10            DI02-C198.                                    CI0017
            11            DI02-CLNAM.                                   CI0017
            12            DI02-CLNAMH PICTURE  X(6).                    CI0017
            12            DI02-CLNAMF PICTURE  X(20).                   CI0017
            12            DI02-CLNAMM.                                  CI0017
            13            DI02-CLNAMI PICTURE  X.                       CI0017
            13            DI02-CLNAMR PICTURE  X(14).                   CI0017
            12            DI02-CLNAML PICTURE  X(25).                   CI0017
            12            DI02-CLNAMS PICTURE  X(4).                    CI0017
            10            DI02-TDELI  PICTURE  X(30).                   CI0017
            10            DI02-NPBN   PICTURE  X(20).                   CI0017
            10            DI02-NTR    PICTURE  9(8).                    CI0017
            10            DI02-GECKD1 PICTURE  9.                       CI0017
            10            DI02-CCBAT  PICTURE  99.                      CI0017
            10            DI02-C299.                                    CI0017
            11            DI02-CTID.                                    CI0017
            12            DI02-CTIDA  PICTURE  9(3).                    CI0017
            12            DI02-CTIDN.                                   CI0017
            13            DI02-CTIDNP PICTURE  X(13).                   CI0017
            13            DI02-CTIDND PICTURE  9(11).                   CI0017
            10            DI02-GECKD  PICTURE  9.                       CI0017
            10            DI02-PRCMN  PICTURE  X(20).                   CI0017
            10            DI02-MCSIG  PICTURE  X(30).                   CI0017
            10            DI02-MAPPN  PICTURE  X(10).                   CI0017
            10            DI02-FILLER PICTURE  X(240).                  CI0017
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 AMDU02
      *                                                                 ADU002
      ******************************************************************ADU002
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU002
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU002
      ******************************************************************ADU002
      *                                                                 ADU002
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU002
       01                 MS00.                                         CI0017
          05              MS00-SUITE.                                   CI0017
            15       FILLER         PICTURE  X(00542).                  CI0017
       01                 MS03  REDEFINES      MS00.                    CI0017
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0017
                          COMPUTATIONAL-3.                              CI0017
            10            MS03-CMSSF  PICTURE  XX.                      CI0017
            10            MS03-DU09.                                    CI0017
            11            MS03-CMESA  PICTURE  S9(9)                    CI0017
                          BINARY.                                       CI0017
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0017
                          BINARY.                                       CI0017
            11            MS03-CMESB  PICTURE  S9(9)                    CI0017
                          BINARY.                                       CI0017
            11            MS03-CMSST  PICTURE  S9(9)                    CI0017
                          BINARY.                                       CI0017
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0017
                          BINARY.                                       CI0017
            11            MS03-QELLAA PICTURE  S9(9)                    CI0017
                          BINARY.                                       CI0017
            11            MS03-TMESS4 PICTURE  X(512).                  CI0017
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DL01
                                DI02
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0017
      *               *                                   *             CI0017
      *               *INITIALISATIONS                    *             CI0017
      *               *                                   *             CI0017
      *               *************************************.            CI0017
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
      *N02CA.    NOTE *DEFAULT APP NAME IF NOT EZ-TRANS   *.
       F02CA.    IF    DI02-MAPPN NOT = 'SD'                            lv10
                 AND   DI02-MAPPN NOT = 'UD'
                 NEXT SENTENCE ELSE GO TO     F02CA-FN.
           MOVE        SPACES TO DI02-MAPPN.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0017
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0017
      *               *                                   *             CI0017
      *               *FIN DE TRAITEMENT                  *             CI0017
      *               *                                   *             CI0017
      *               *************************************.            CI0017
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0017
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
       F40.      IF    DL01-CPAY1 = 'O '                                lv05
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
      *N40BA.    NOTE *UNFORMAT DELIVERY METHOD           *.            AMSKDL
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
      *N40EA.    NOTE *UNFORMAT WHO LINES                 *.            AMSKDL
       F40EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N40GA.    NOTE *MOVE ACCOUNT OWNERSHIP LINES       *.            AMOWNL
       F40GA.                                                           lv15
      *                                                                 AMOWNL
      *********************************                                 AMOWNL
      ** MOVE THE ACCOUNT OWNERSHIP   *                                 AMOWNL
      ** LINES FROM THE WHO LINES     *                                 AMOWNL
      *********************************                                 AMOWNL
      *                                                                 AMOWNL
           MOVE        DL01-TWHOL1 TO DI02-CTTLN1                       AMOWNL
           MOVE        DL01-TWHOL2 TO DI02-CTTLN2                       AMOWNL
           MOVE        DL01-TWHOL3 TO DI02-CTTLN3.                      AMOWNL
       F40GA-FN. EXIT.
       F40EA-FN. EXIT.
      *N40MA.    NOTE *UNFORMAT WHERE LINES               *.            AMSKDL
       F40MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N40NA.    NOTE *CITY/STATE/ZIP IN WHERE LINE #4    *.            AMUADL
       F40NA.    IF    DL01-TWHRL4 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F40NA-FN.                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #4,   *                                  AMUADL
      ** MOVE WHERE LINE #4, INTO    *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL4 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        DL01-TWHRL3 TO DI02-GESAD (3)                    AMUADL
           GO TO F40NE.                                                 AMUADL
       F40NA-FN. EXIT.
      *N40NB.    NOTE *CITY/STATE/ZIP IN WHERE LINE #3    *.            AMUADL
       F40NB.    IF    DL01-TWHRL3 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F40NB-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #3,   *                                  AMUADL
      ** MOVE WHERE LINE #3 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL3 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F40NE.                                                 AMUADL
       F40NB-FN. EXIT.
      *N40NC.    NOTE *CITY/STATE/ZIP IN WHERE LINE #2    *.            AMUADL
       F40NC.    IF    DL01-TWHRL2 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F40NC-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #2,   *                                  AMUADL
      ** MOVE WHERE LINE #2 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL2 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F40NE.                                                 AMUADL
       F40NC-FN. EXIT.
      *N40ND.    NOTE *CITY/STATE/ZIP IN WHERE LINE #1    *.            AMUADL
       F40ND.    IF    DL01-TWHRL1 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F40ND-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #1,   *                                  AMUADL
      ** MOVE WHERE LINE 1 INTO      *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL1 TO W-CP30-ALINE                      AMUADL
           MOVE        SPACES TO DI02-GESAD (1)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F40NE.                                                 AMUADL
       F40ND-FN. EXIT.
      *N40NE.    NOTE *CALL TO EXTRACT ADDRESS DATA       *.            AMUADL
       F40NE.                                                           lv15
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** CALL MODULE CMU001DY TO      *                                 AMUADL
      ** EXTRACT CITY, STATE, ZIP CODE*                                 AMUADL
      ** AND COUNTRY CP30-ALINE.      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        SPACES TO CP30                                   AMUADL
           MOVE        'U' TO CP30-GERQCD                               AMUADL
           MOVE        W-CP30-ALINE TO CP30-ALINE.                      AMUADL
      *'CMU001DY' USING  CP30.                                          DOT
      *                                                                 DOT
       F40NE-FN. EXIT.
      *N40NF.    NOTE *CHECK IF RETURN CODE IS OK         *.            AMUADL
       F40NF.    IF    CP30-GERTC = SPACES OR ZEROS                     lv15
                 NEXT SENTENCE ELSE GO TO     F40NF-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** MOVE THE EXTRACTED FIELDS    *                                 AMUADL
      ** TO SEGMENT DI02 TO BE        *                                 AMUADL
      ** RETURNED TO THE CALLING      *                                 AMUADL
      ** MODULE.                      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        CP30-GECIT TO DI02-GECIT                         AMUADL
           MOVE        CP30-GEST TO DI02-GEST                           AMUADL
           MOVE        CP30-GEPCD TO DI02-GEPCD                         AMUADL
           MOVE        CP30-GECTRY TO DI02-GECTRY.                      AMUADL
       F40NF-FN. EXIT.
       F40MA-FN. EXIT.
      *N40YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F40YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DL01-CPAY1 TO DI02-CPAY1.                        AMSKDL
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
       F45.      IF    DL01-CPAY1 = 'OA'                                lv05
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
      *N45BA.    NOTE *UNFORMAT DELIVERY METHOD           *.            AMSKDL
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
      *N45EA.    NOTE *UNFORMAT WHO LINES                 *.            AMSKDL
       F45EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N45GA.    NOTE *MOVE ACCOUNT OWNERSHIP LINES       *.            AMOWNL
       F45GA.                                                           lv15
      *                                                                 AMOWNL
      *********************************                                 AMOWNL
      ** MOVE THE ACCOUNT OWNERSHIP   *                                 AMOWNL
      ** LINES FROM THE WHO LINES     *                                 AMOWNL
      *********************************                                 AMOWNL
      *                                                                 AMOWNL
           MOVE        DL01-TWHOL1 TO DI02-CTTLN1                       AMOWNL
           MOVE        DL01-TWHOL2 TO DI02-CTTLN2                       AMOWNL
           MOVE        DL01-TWHOL3 TO DI02-CTTLN3.                      AMOWNL
       F45GA-FN. EXIT.
       F45EA-FN. EXIT.
      *N45MA.    NOTE *UNFORMAT WHERE LINES               *.            AMSKDL
       F45MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N45NA.    NOTE *CITY/STATE/ZIP IN WHERE LINE #4    *.            AMUADL
       F45NA.    IF    DL01-TWHRL4 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F45NA-FN.                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #4,   *                                  AMUADL
      ** MOVE WHERE LINE #4, INTO    *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL4 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        DL01-TWHRL3 TO DI02-GESAD (3)                    AMUADL
           GO TO F45NE.                                                 AMUADL
       F45NA-FN. EXIT.
      *N45NB.    NOTE *CITY/STATE/ZIP IN WHERE LINE #3    *.            AMUADL
       F45NB.    IF    DL01-TWHRL3 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F45NB-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #3,   *                                  AMUADL
      ** MOVE WHERE LINE #3 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL3 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F45NE.                                                 AMUADL
       F45NB-FN. EXIT.
      *N45NC.    NOTE *CITY/STATE/ZIP IN WHERE LINE #2    *.            AMUADL
       F45NC.    IF    DL01-TWHRL2 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F45NC-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #2,   *                                  AMUADL
      ** MOVE WHERE LINE #2 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL2 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F45NE.                                                 AMUADL
       F45NC-FN. EXIT.
      *N45ND.    NOTE *CITY/STATE/ZIP IN WHERE LINE #1    *.            AMUADL
       F45ND.    IF    DL01-TWHRL1 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F45ND-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #1,   *                                  AMUADL
      ** MOVE WHERE LINE 1 INTO      *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL1 TO W-CP30-ALINE                      AMUADL
           MOVE        SPACES TO DI02-GESAD (1)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F45NE.                                                 AMUADL
       F45ND-FN. EXIT.
      *N45NE.    NOTE *CALL TO EXTRACT ADDRESS DATA       *.            AMUADL
       F45NE.                                                           lv15
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** CALL MODULE CMU001DY TO      *                                 AMUADL
      ** EXTRACT CITY, STATE, ZIP CODE*                                 AMUADL
      ** AND COUNTRY CP30-ALINE.      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        SPACES TO CP30                                   AMUADL
           MOVE        'U' TO CP30-GERQCD                               AMUADL
           MOVE        W-CP30-ALINE TO CP30-ALINE.                      AMUADL
      *'CMU001DY' USING  CP30.                                          DOT
      *                                                                 DOT
       F45NE-FN. EXIT.
      *N45NF.    NOTE *CHECK IF RETURN CODE IS OK         *.            AMUADL
       F45NF.    IF    CP30-GERTC = SPACES OR ZEROS                     lv15
                 NEXT SENTENCE ELSE GO TO     F45NF-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** MOVE THE EXTRACTED FIELDS    *                                 AMUADL
      ** TO SEGMENT DI02 TO BE        *                                 AMUADL
      ** RETURNED TO THE CALLING      *                                 AMUADL
      ** MODULE.                      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        CP30-GECIT TO DI02-GECIT                         AMUADL
           MOVE        CP30-GEST TO DI02-GEST                           AMUADL
           MOVE        CP30-GEPCD TO DI02-GEPCD                         AMUADL
           MOVE        CP30-GECTRY TO DI02-GECTRY.                      AMUADL
       F45NF-FN. EXIT.
       F45MA-FN. EXIT.
      *N45YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F45YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DL01-CPAY1 TO DI02-CPAY1.                        AMSKDL
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
       F50.      IF    DL01-CPAY1 = 'S '                                lv05
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
      *N50BA.    NOTE *UNFORMAT DELIVERY METHOD           *.            AMSKDL
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
      *N50EA.    NOTE *UNFORMAT WHO LINES                 *.            AMSKDL
       F50EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N50FA.    NOTE *SET UP CALL TO CMU006DY            *.            AMUNAM
       F50FA.                                                           lv15
      *                                                                 AMUNAM
      *********************************                                 AMUNAM
      ** SET UP CALL TO CMU006DY TO   *                                 AMUNAM
      ** EXTRACT THE CLIENT'S NAME    *                                 AMUNAM
      ** FROM THE WHO LINE.           *                                 AMUNAM
      *********************************                                 AMUNAM
      *                                                                 AMUNAM
           MOVE        SPACES TO CP10                                   AMUNAM
           MOVE        DL01-TWHOL1 TO CP10-CLNAM                        AMUNAM
           CALL        'CMU006DY' USING CP10.                           AMUNAM
       F50FA-FN. EXIT.
      *N50GA.    NOTE *CHECK IF GOOD RETURN CODE          *.            AMUNAM
       F50GA.    IF    CP10-GERTC = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F50GA-FN.                 AMUNAM
      *                                                                 AMUNAM
      *********************************                                 AMUNAM
      ** CHECK IF THERE IS A GOOD     *                                 AMUNAM
      ** RETURN CODE FROM CMU0006DY.  *                                 AMUNAM
      ** IF SO, CHECK CLIENT TYPE TO  *                                 AMUNAM
      ** SEE IF PERSON OR ORGANIZATION*                                 AMUNAM
      ** AND MOVE TO FIELDS.          *                                 AMUNAM
      *********************************                                 AMUNAM
      *                                                                 AMUNAM
           MOVE        CP10-CLTYP TO DI02-CLTYP.                        AMUNAM
                 IF    CP10-CLTYP = 'P'                                 DOT
           MOVE        CP10-CLNAMH TO DI02-CLNAMH                       AMUNAM
           MOVE        CP10-CLNAMF TO DI02-CLNAMF                       AMUNAM
           MOVE        CP10-CLNAMR TO DI02-CLNAMR                       AMUNAM
           MOVE        CP10-CLNAML TO DI02-CLNAML                       AMUNAM
           MOVE        CP10-CLNAMS TO DI02-CLNAMS.                      AMUNAM
                 IF    CP10-CLTYP = 'O'                                 DOT
           MOVE        CP10-CLORN TO DI02-CLORN.                        AMUNAM
      *                                                                 DOT
       F50GA-FN. EXIT.
      *N50HA.    NOTE *UNFORMAT WHO LINE #2               *.
       F50HA.                                                           lv15
      *TO THE MEMO TEXT FIELD
      *
           MOVE        DL01-TWHOL2 TO DI02-TDELI.
       F50HA-FN. EXIT.
       F50EA-FN. EXIT.
      *N50MA.    NOTE *UNFORMAT WHERE LINES               *.            AMSKDL
       F50MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N50NA.    NOTE *CITY/STATE/ZIP IN WHERE LINE #4    *.            AMUADL
       F50NA.    IF    DL01-TWHRL4 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F50NA-FN.                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #4,   *                                  AMUADL
      ** MOVE WHERE LINE #4, INTO    *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL4 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        DL01-TWHRL3 TO DI02-GESAD (3)                    AMUADL
           GO TO F50NE.                                                 AMUADL
       F50NA-FN. EXIT.
      *N50NB.    NOTE *CITY/STATE/ZIP IN WHERE LINE #3    *.            AMUADL
       F50NB.    IF    DL01-TWHRL3 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F50NB-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #3,   *                                  AMUADL
      ** MOVE WHERE LINE #3 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL3 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F50NE.                                                 AMUADL
       F50NB-FN. EXIT.
      *N50NC.    NOTE *CITY/STATE/ZIP IN WHERE LINE #2    *.            AMUADL
       F50NC.    IF    DL01-TWHRL2 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F50NC-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #2,   *                                  AMUADL
      ** MOVE WHERE LINE #2 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL2 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F50NE.                                                 AMUADL
       F50NC-FN. EXIT.
      *N50ND.    NOTE *CITY/STATE/ZIP IN WHERE LINE #1    *.            AMUADL
       F50ND.    IF    DL01-TWHRL1 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F50ND-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #1,   *                                  AMUADL
      ** MOVE WHERE LINE 1 INTO      *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL1 TO W-CP30-ALINE                      AMUADL
           MOVE        SPACES TO DI02-GESAD (1)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F50NE.                                                 AMUADL
       F50ND-FN. EXIT.
      *N50NE.    NOTE *CALL TO EXTRACT ADDRESS DATA       *.            AMUADL
       F50NE.                                                           lv15
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** CALL MODULE CMU001DY TO      *                                 AMUADL
      ** EXTRACT CITY, STATE, ZIP CODE*                                 AMUADL
      ** AND COUNTRY CP30-ALINE.      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        SPACES TO CP30                                   AMUADL
           MOVE        'U' TO CP30-GERQCD                               AMUADL
           MOVE        W-CP30-ALINE TO CP30-ALINE.                      AMUADL
      *'CMU001DY' USING  CP30.                                          DOT
      *                                                                 DOT
       F50NE-FN. EXIT.
      *N50NF.    NOTE *CHECK IF RETURN CODE IS OK         *.            AMUADL
       F50NF.    IF    CP30-GERTC = SPACES OR ZEROS                     lv15
                 NEXT SENTENCE ELSE GO TO     F50NF-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** MOVE THE EXTRACTED FIELDS    *                                 AMUADL
      ** TO SEGMENT DI02 TO BE        *                                 AMUADL
      ** RETURNED TO THE CALLING      *                                 AMUADL
      ** MODULE.                      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        CP30-GECIT TO DI02-GECIT                         AMUADL
           MOVE        CP30-GEST TO DI02-GEST                           AMUADL
           MOVE        CP30-GEPCD TO DI02-GEPCD                         AMUADL
           MOVE        CP30-GECTRY TO DI02-GECTRY.                      AMUADL
       F50NF-FN. EXIT.
       F50MA-FN. EXIT.
      *N50YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F50YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DL01-CPAY1 TO DI02-CPAY1.                        AMSKDL
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
       F55.      IF    DL01-CPAY1 = 'B '                                lv05
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
      *N55BA.    NOTE *UNFORMAT DELIVERY METHOD           *.            AMSKDL
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
      *N55EA.    NOTE *UNFORMAT WHO LINES                 *.            AMSKDL
       F55EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N55FA.    NOTE *SET UP CALL TO CMU006DY            *.            AMUNAM
       F55FA.                                                           lv15
      *                                                                 AMUNAM
      *********************************                                 AMUNAM
      ** SET UP CALL TO CMU006DY TO   *                                 AMUNAM
      ** EXTRACT THE CLIENT'S NAME    *                                 AMUNAM
      ** FROM THE WHO LINE.           *                                 AMUNAM
      *********************************                                 AMUNAM
      *                                                                 AMUNAM
           MOVE        SPACES TO CP10                                   AMUNAM
           MOVE        DL01-TWHOL1 TO CP10-CLNAM                        AMUNAM
           CALL        'CMU006DY' USING CP10.                           AMUNAM
       F55FA-FN. EXIT.
      *N55GA.    NOTE *CHECK IF GOOD RETURN CODE          *.            AMUNAM
       F55GA.    IF    CP10-GERTC = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.                 AMUNAM
      *                                                                 AMUNAM
      *********************************                                 AMUNAM
      ** CHECK IF THERE IS A GOOD     *                                 AMUNAM
      ** RETURN CODE FROM CMU0006DY.  *                                 AMUNAM
      ** IF SO, CHECK CLIENT TYPE TO  *                                 AMUNAM
      ** SEE IF PERSON OR ORGANIZATION*                                 AMUNAM
      ** AND MOVE TO FIELDS.          *                                 AMUNAM
      *********************************                                 AMUNAM
      *                                                                 AMUNAM
           MOVE        CP10-CLTYP TO DI02-CLTYP.                        AMUNAM
                 IF    CP10-CLTYP = 'P'                                 DOT
           MOVE        CP10-CLNAMH TO DI02-CLNAMH                       AMUNAM
           MOVE        CP10-CLNAMF TO DI02-CLNAMF                       AMUNAM
           MOVE        CP10-CLNAMR TO DI02-CLNAMR                       AMUNAM
           MOVE        CP10-CLNAML TO DI02-CLNAML                       AMUNAM
           MOVE        CP10-CLNAMS TO DI02-CLNAMS.                      AMUNAM
                 IF    CP10-CLTYP = 'O'                                 DOT
           MOVE        CP10-CLORN TO DI02-CLORN.                        AMUNAM
      *                                                                 DOT
       F55GA-FN. EXIT.
      *N55HA.    NOTE *UNFORMAT WHO LINE #2               *.
       F55HA.                                                           lv15
      *TO GET CLIENT'S SIGNATURE
      *
           MOVE        6 TO W-WORK-1PTR
           UNSTRING DL01-TWHOL2
                   INTO DI02-MCSIG
                   WITH POINTER W-WORK-1PTR.
       F55HA-FN. EXIT.
      *N55IA.    NOTE *UNFORMAT WHO LINE #3               *.
       F55IA.                                                           lv15
      *TO GET BANK ACC NUMBER AND TYPE
      *(SAVINGS OR CHECKING)
      *
           MOVE        DL01-TWHOL3 TO W-WORK-XZ1.
      *N55IB.    NOTE *SAVINGS ACCOUNT STARTS WITH 'S'    *.
       F55IB.    IF    W-WORK-XZ1 = 'S'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F55IB-FN.
      *
                 IF    DI02-MAPPN = 'SD'                                DOT
                 OR    DI02-MAPPN = 'UD'
      *Calling program is EZ-Trans...
           MOVE        11 TO W-WORK-1PTR
                 ELSE
      *For all other applications...
           MOVE        18 TO W-WORK-1PTR.
      *Endif                                                            DOT
           MOVE        02 TO DI02-CCBAT.
       F55IB-900. GO TO F55IC-FN.
       F55IB-FN. EXIT.
      *N55IC.    NOTE *CHECKING ACCOUNT                   *.
       F55IC.                                                           lv20
      *
                 IF    DI02-MAPPN = 'SD'                                DOT
                 OR    DI02-MAPPN = 'UD'
      *Calling program is EZ-Trans
           MOVE        12 TO W-WORK-1PTR
                 ELSE
      *For all other applications
           MOVE        19 TO W-WORK-1PTR.
      *Endif                                                            DOT
           MOVE        01 TO DI02-CCBAT.
       F55IC-FN. EXIT.
      *N55ID.    NOTE *GET BANK ACCOUNT NUMBER            *.
       F55ID.                                                           lv20
      *FROM 3RD WHO LINE
      *
           UNSTRING DL01-TWHOL3
                   INTO DI02-NPBN
                   WITH POINTER W-WORK-1PTR.
       F55ID-FN. EXIT.
       F55IA-FN. EXIT.
       F55EA-FN. EXIT.
      *N55MA.    NOTE *UNFORMAT WHERE LINES               *.            AMSKDL
       F55MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N55NA.    NOTE *CITY/STATE/ZIP IN WHERE LINE #4    *.            AMUADL
       F55NA.    IF    DL01-TWHRL4 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F55NA-FN.                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #4,   *                                  AMUADL
      ** MOVE WHERE LINE #4, INTO    *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL4 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        DL01-TWHRL3 TO DI02-GESAD (3)                    AMUADL
           GO TO F55NE.                                                 AMUADL
       F55NA-FN. EXIT.
      *N55NB.    NOTE *CITY/STATE/ZIP IN WHERE LINE #3    *.            AMUADL
       F55NB.    IF    DL01-TWHRL3 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F55NB-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #3,   *                                  AMUADL
      ** MOVE WHERE LINE #3 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL3 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (2)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F55NE.                                                 AMUADL
       F55NB-FN. EXIT.
      *N55NC.    NOTE *CITY/STATE/ZIP IN WHERE LINE #2    *.            AMUADL
       F55NC.    IF    DL01-TWHRL2 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F55NC-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #2,   *                                  AMUADL
      ** MOVE WHERE LINE #2 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL2 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL1 TO DI02-GESAD (1)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F55NE.                                                 AMUADL
       F55NC-FN. EXIT.
      *N55ND.    NOTE *CITY/STATE/ZIP IN WHERE LINE #1    *.            AMUADL
       F55ND.    IF    DL01-TWHRL1 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F55ND-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #1,   *                                  AMUADL
      ** MOVE WHERE LINE 1 INTO      *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL1 TO W-CP30-ALINE                      AMUADL
           MOVE        SPACES TO DI02-GESAD (1)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F55NE.                                                 AMUADL
       F55ND-FN. EXIT.
      *N55NE.    NOTE *CALL TO EXTRACT ADDRESS DATA       *.            AMUADL
       F55NE.                                                           lv15
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** CALL MODULE CMU001DY TO      *                                 AMUADL
      ** EXTRACT CITY, STATE, ZIP CODE*                                 AMUADL
      ** AND COUNTRY CP30-ALINE.      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        SPACES TO CP30                                   AMUADL
           MOVE        'U' TO CP30-GERQCD                               AMUADL
           MOVE        W-CP30-ALINE TO CP30-ALINE.                      AMUADL
      *'CMU001DY' USING  CP30.                                          DOT
      *                                                                 DOT
       F55NE-FN. EXIT.
      *N55NF.    NOTE *CHECK IF RETURN CODE IS OK         *.            AMUADL
       F55NF.    IF    CP30-GERTC = SPACES OR ZEROS                     lv15
                 NEXT SENTENCE ELSE GO TO     F55NF-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** MOVE THE EXTRACTED FIELDS    *                                 AMUADL
      ** TO SEGMENT DI02 TO BE        *                                 AMUADL
      ** RETURNED TO THE CALLING      *                                 AMUADL
      ** MODULE.                      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        CP30-GECIT TO DI02-GECIT                         AMUADL
           MOVE        CP30-GEST TO DI02-GEST                           AMUADL
           MOVE        CP30-GEPCD TO DI02-GEPCD                         AMUADL
           MOVE        CP30-GECTRY TO DI02-GECTRY.                      AMUADL
       F55NF-FN. EXIT.
       F55MA-FN. EXIT.
      *N55YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F55YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DL01-CPAY1 TO DI02-CPAY1.                        AMSKDL
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
       F60.      IF    DL01-CPAY1 = 'D '                                lv05
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
      *N60BA.    NOTE *UNFORMAT DELIVERY METHOD           *.            AMSKDL
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
      *N60EA.    NOTE *UNFORMAT WHO LINES                 *.            AMSKDL
       F60EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N60FA.    NOTE *UNFORMAT WHO LINE #1               *.
       F60FA.                                                           lv15
      *TO GET CLIENT'S SIGNATURE
      *
           MOVE        DL01-TWHOL1 TO DI02-MCSIG.
       F60FA-FN. EXIT.
       F60EA-FN. EXIT.
      *N60MA.    NOTE *UNFORMAT WHERE LINES               *.            AMSKDL
       F60MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N60NA.    NOTE *UNFORMAT WHERE LINE #1             *.
       F60NA.                                                           lv15
      *TO GET CLIENT'S NAME
      *
           MOVE        DL01-TWHRL1 TO DI02-CLORN
           MOVE        'O' TO DI02-CLTYP.
       F60NA-FN. EXIT.
      *N60OA.    NOTE *SHUFFLE WHERE LINES                *.
       F60OA.                                                           lv15
      *DEPENDING ON WHERE RTN IS
      *N60PA.    NOTE *WHERE LINE #2 CONTAINS RTN         *.
       F60PA.                                                           lv20
      *
           MOVE        DL01-TWHRL2 TO W-DL01-XZ4.
                 IF    W-DL01-XZ4 = 'RTN:'                              DOT
           MOVE        DL01-TWHRL2 TO DL01-TWHRL6
           MOVE        DL01-TWHRL3 TO DL01-TWHRL7
           MOVE        SPACES TO DL01-TWHRL2
           DL01-TWHRL3
           DL01-TWHRL4
           DL01-TWHRL5
           MOVE        'N' TO DI02-ISHAD
               GO TO     F60OA-FN.
       F60PA-FN. EXIT.
      *N60QA.    NOTE *WHERE LINE #3 CONTAINS RTN         *.
       F60QA.                                                           lv20
           MOVE        DL01-TWHRL3 TO W-DL01-XZ4.
                 IF    W-DL01-XZ4 = 'RTN:'                              DOT
           MOVE        DL01-TWHRL3 TO DL01-TWHRL6
           MOVE        DL01-TWHRL4 TO DL01-TWHRL7
           MOVE        SPACES TO DL01-TWHRL3
           DL01-TWHRL4
           DL01-TWHRL5
           MOVE        'Y' TO DI02-ISHAD
               GO TO     F60OA-FN.
       F60QA-FN. EXIT.
      *N60RA.    NOTE *WHERE LINE #4 CONTAINS RTN         *.
       F60RA.                                                           lv20
      *
           MOVE        DL01-TWHRL4 TO W-DL01-XZ4.
                 IF    W-DL01-XZ4 = 'RTN:'                              DOT
           MOVE        DL01-TWHRL4 TO DL01-TWHRL6
           MOVE        DL01-TWHRL5 TO DL01-TWHRL7
           MOVE        SPACES TO DL01-TWHRL4
           DL01-TWHRL5
           MOVE        'Y' TO DI02-ISHAD
               GO TO     F60OA-FN.
       F60RA-FN. EXIT.
      *N60SA.    NOTE *WHERE LINE #5 CONTAINS RTN         *.
       F60SA.                                                           lv20
      *
           MOVE        DL01-TWHRL5 TO W-DL01-XZ4.
                 IF    W-DL01-XZ4 = 'RTN:'                              DOT
           MOVE        DL01-TWHRL6 TO DL01-TWHRL7
           MOVE        DL01-TWHRL5 TO DL01-TWHRL6
           MOVE        SPACES TO DL01-TWHRL5
           MOVE        'Y' TO DI02-ISHAD
               GO TO     F60OA-FN.
       F60SA-FN. EXIT.
       F60OA-FN. EXIT.
      *N60TA.    NOTE *CITY/STATE/ZIP IN WHERE LINE #5    *.            AMUADL
       F60TA.    IF    DL01-TWHRL5 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F60TA-FN.                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #5,   *                                  AMUADL
      ** MOVE WHERE LINE #5, INTO    *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL5 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL3 TO DI02-GESAD (2)                    AMUADL
           MOVE        DL01-TWHRL4 TO DI02-GESAD (3)                    AMUADL
           GO TO F60TE.                                                 AMUADL
       F60TA-FN. EXIT.
      *N60TB.    NOTE *CITY/STATE/ZIP IN WHERE LINE #4    *.            AMUADL
       F60TB.    IF    DL01-TWHRL4 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F60TB-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #4,   *                                  AMUADL
      ** MOVE WHERE LINE #4 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL4 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (1)                    AMUADL
           MOVE        DL01-TWHRL3 TO DI02-GESAD (2)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F60TE.                                                 AMUADL
       F60TB-FN. EXIT.
      *N60TC.    NOTE *CITY/STATE/ZIP IN WHERE LINE #3    *.            AMUADL
       F60TC.    IF    DL01-TWHRL3 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F60TC-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #3,   *                                  AMUADL
      ** MOVE WHERE LINE #3 INTO     *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL3 TO W-CP30-ALINE                      AMUADL
           MOVE        DL01-TWHRL2 TO DI02-GESAD (1)                    AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F60TE.                                                 AMUADL
       F60TC-FN. EXIT.
      *N60TD.    NOTE *CITY/STATE/ZIP IN WHERE LINE #2    *.            AMUADL
       F60TD.    IF    DL01-TWHRL2 NOT = SPACES                         lv15
                 NEXT SENTENCE ELSE GO TO     F60TD-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** IF THE CITY, STATE AND ZIP   *                                 AMUADL
      ** CODE IS IN WHERE LINE #2,   *                                  AMUADL
      ** MOVE WHERE LINE 2 INTO      *                                  AMUADL
      ** CP30-ALINE FOR EXTRACTING    *                                 AMUADL
      ** OF CITY, STATE, ZIP CODE AND *                                 AMUADL
      ** AND COUNTRY BY CMU0001DY.    *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        DL01-TWHRL2 TO W-CP30-ALINE                      AMUADL
           MOVE        SPACES TO DI02-GESAD (1)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (2)                         AMUADL
           MOVE        SPACES TO DI02-GESAD (3)                         AMUADL
           GO TO F60TE.                                                 AMUADL
       F60TD-FN. EXIT.
      *N60TE.    NOTE *CALL TO EXTRACT ADDRESS DATA       *.            AMUADL
       F60TE.                                                           lv15
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** CALL MODULE CMU001DY TO      *                                 AMUADL
      ** EXTRACT CITY, STATE, ZIP CODE*                                 AMUADL
      ** AND COUNTRY CP30-ALINE.      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        SPACES TO CP30                                   AMUADL
           MOVE        'U' TO CP30-GERQCD                               AMUADL
           MOVE        W-CP30-ALINE TO CP30-ALINE.                      AMUADL
      *'CMU001DY' USING  CP30.                                          DOT
      *                                                                 DOT
       F60TE-FN. EXIT.
      *N60TF.    NOTE *CHECK IF RETURN CODE IS OK         *.            AMUADL
       F60TF.    IF    CP30-GERTC = SPACES OR ZEROS                     lv15
                 NEXT SENTENCE ELSE GO TO     F60TF-FN.                 AMUADL
      *                                                                 AMUADL
      *********************************                                 AMUADL
      ** MOVE THE EXTRACTED FIELDS    *                                 AMUADL
      ** TO SEGMENT DI02 TO BE        *                                 AMUADL
      ** RETURNED TO THE CALLING      *                                 AMUADL
      ** MODULE.                      *                                 AMUADL
      *********************************                                 AMUADL
      *                                                                 AMUADL
           MOVE        CP30-GECIT TO DI02-GECIT                         AMUADL
           MOVE        CP30-GEST TO DI02-GEST                           AMUADL
           MOVE        CP30-GEPCD TO DI02-GEPCD                         AMUADL
           MOVE        CP30-GECTRY TO DI02-GECTRY.                      AMUADL
       F60TF-FN. EXIT.
      *N60UA.    NOTE *UNFORMAT WHERE LINE #6             *.
       F60UA.                                                           lv15
      *FOR BANK'S RTN
      *
           MOVE        6 TO W-WORK-1PTR
           UNSTRING
            DL01-TWHRL6 DELIMITED BY SPACE
            INTO DI02-NTR
                 DI02-GECKD1
            WITH POINTER W-WORK-1PTR.
       F60UA-FN. EXIT.
      *N60VA.    NOTE *UNFORMAT WHERE LINE #7 TO GET      *.
       F60VA.                                                           lv15
      *BANK ACCOUNT NUMBER AND TYPE
      *OF ACCOUNT (SAVINGS OR CHECKING)
      *
           MOVE        DL01-TWHRL7 TO W-WORK-XZ1.
      *N60VB.    NOTE *SAVINGS ACCOUNT STARTS WITH 'S'    *.
       F60VB.    IF    W-WORK-XZ1 = 'S'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F60VB-FN.
                 IF    DI02-MAPPN = 'SD'                                DOT
                 OR    DI02-MAPPN = 'UD'
      *APPLICATION IS EZ-TRANS...
           MOVE        11 TO W-WORK-1PTR
                 ELSE
      *OTHERWISE
           MOVE        18 TO W-WORK-1PTR.
      *ENDIF                                                            DOT
           MOVE        02 TO DI02-CCBAT.
       F60VB-900. GO TO F60VC-FN.
       F60VB-FN. EXIT.
      *N60VC.    NOTE *CHECKING ACCOUNT                   *.
       F60VC.                                                           lv20
                 IF    DI02-MAPPN = 'SD'                                DOT
                 OR    DI02-MAPPN = 'UD'
      *CALLING APPLICATION IS EZ-TRANS
           MOVE        12 TO W-WORK-1PTR
                 ELSE
      *OTHER APPLICATION....
           MOVE        19 TO W-WORK-1PTR.
      *ENDIF                                                            DOT
           MOVE        01 TO DI02-CCBAT.
       F60VC-FN. EXIT.
      *N60VD.    NOTE *GET BANK ACCOUNT NUMBER            *.
       F60VD.                                                           lv20
      *FROM THE SEVENTH WHERE LINE
      *
           UNSTRING DL01-TWHRL7
                   INTO DI02-NPBN
                   WITH POINTER W-WORK-1PTR.
       F60VD-FN. EXIT.
       F60VA-FN. EXIT.
       F60MA-FN. EXIT.
      *N60YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F60YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DL01-CPAY1 TO DI02-CPAY1.                        AMSKDL
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
       F65.      IF    DL01-CPAY1 = 'TR'                                lv05
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
      *N65BA.    NOTE *UNFORMAT DELIVERY METHOD           *.            AMSKDL
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
      *N65EA.    NOTE *UNFORMAT WHO LINES                 *.            AMSKDL
       F65EA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHO LINES       *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N65GA.    NOTE *MOVE ACCOUNT OWNERSHIP LINES       *.            AMOWNL
       F65GA.                                                           lv15
      *                                                                 AMOWNL
      *********************************                                 AMOWNL
      ** MOVE THE ACCOUNT OWNERSHIP   *                                 AMOWNL
      ** LINES FROM THE WHO LINES     *                                 AMOWNL
      *********************************                                 AMOWNL
      *                                                                 AMOWNL
           MOVE        DL01-TWHOL1 TO DI02-CTTLN1                       AMOWNL
           MOVE        DL01-TWHOL2 TO DI02-CTTLN2                       AMOWNL
           MOVE        DL01-TWHOL3 TO DI02-CTTLN3.                      AMOWNL
       F65GA-FN. EXIT.
       F65EA-FN. EXIT.
      *N65MA.    NOTE *UNFORMAT WHERE LINES               *.            AMSKDL
       F65MA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** UNFORMAT THE WHERE LINES     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
      *N65NA.    NOTE *UNFORMAT WHERE LINE #1             *.
       F65NA.                                                           lv15
      *FOR 'TO' ACCOUNT NUMBER
      *
           MOVE        DL01-TWHRL1 TO 7-CT00-UCTID
           PERFORM     F96 THRU F96-FN
           MOVE        7-CT00-CTID TO DI02-CTID
           MOVE        7-CT00-GECKDX TO DI02-GECKD.
       F65NA-FN. EXIT.
      *N65OA.    NOTE *UNFORMAT WHERE LINE #2             *.
       F65OA.                                                           lv15
      *FOR THE PRODUCT CODE
      *
           MOVE        DL01-TWHRL2 TO DI02-PRCMN.
       F65OA-FN. EXIT.
       F65MA-FN. EXIT.
      *N65YA.    NOTE *SET DESTINATION CODE               *.            AMSKDL
       F65YA.                                                           lv10
      *                                                                 AMSKDL
      *********************************                                 AMSKDL
      ** SET THE DESTINATION CODE     *                                 AMSKDL
      *********************************                                 AMSKDL
      *                                                                 AMSKDL
           MOVE        DL01-CPAY1 TO DI02-CPAY1.                        AMSKDL
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
      *               *EXTRACT/FORMAT ACCOUNT ID          *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96ZB.    NOTE *EXTRACT/FORMAT ACCOUNT ID          *.            AAOFIA
       F96ZB.                                                           lv10
           MOVE        7-CT00-UCTID TO CT00-UCTID                       AAOFIA
           MOVE        SPACES TO 7-CT00-UCTID.                          AAOFIA
      *N96ZC.    NOTE *PRE-PROCESS SOURCE FIELD           *.            AAOFIA
       F96ZC.                                                           lv15
      *COUNT THE NUMBER OF SPACES IN                                    AAOFIA
      *THE SOURCE FIELD                                                 AAOFIA
           MOVE        ZEROS TO W-WORK-1CTR
           INSPECT     CT00-UCTID TALLYING W-WORK-1CTR
           FOR ALL SPACE.
      *N96ZD.    NOTE *IF ALL SPACES, ALL DONE            *.
       F96ZD.    IF    W-WORK-1CTR = 00035                              lv20
                 NEXT SENTENCE ELSE GO TO     F96ZD-FN.
      *NO CHARACTERS IN SOURCE                                          AAOFIA
               GO TO     F96ZB-FN.                                      AAOFIA
       F96ZD-FN. EXIT.
      *N96ZE.    NOTE *IF < 7 SPACES, ALL DONE            *.
       F96ZE.    IF    W-WORK-1CTR < 00007                              lv20
                 NEXT SENTENCE ELSE GO TO     F96ZE-FN.
      *TO MANY CHARACTERS IN SOURCE                                     AAOFIA
               GO TO     F96ZB-FN.                                      AAOFIA
       F96ZE-FN. EXIT.
       F96ZC-FN. EXIT.
      *N96ZF.    NOTE *PROCESS ACCT ADM. (CTIDA)          *.            AAOFIA
       F96ZF.                                                           lv15
           MOVE        +35 TO ICT00R ICT00D.                            AAOFIA
      *N96ZG.    NOTE *SET INDEX TO LAST CHARACTER        *.            AAOFIA
       F96ZG.    IF    CT00-UCTIDX (ICT00R) = SPACE                     lv20
                 NEXT SENTENCE ELSE GO TO     F96ZG-FN.                 AAOFIA
           SUBTRACT    +1 FROM ICT00R.                                  AAOFIA
       F96ZG-900. GO TO F96ZG.
       F96ZG-FN. EXIT.
      *N96ZH.    NOTE *REMOVE ANY SPACES ON RIGHT         *.            AAOFIA
       F96ZH.    IF    ICT00R > ZERO                                    lv20
                 AND   < +35                                            AAOFIA
                 NEXT SENTENCE ELSE GO TO     F96ZH-FN.                 AAOFIA
           MOVE        CT00-UCTIDX (ICT00R) TO                          AAOFIA
           CT00-UCTIDX (ICT00D)                                         AAOFIA
           SUBTRACT    +1 FROM ICT00R ICT00D.                           AAOFIA
       F96ZH-900. GO TO F96ZH.
       F96ZH-FN. EXIT.
      *N96ZI.    NOTE *POSITION 32 IS NOT A SPACE         *.            AAOFIA
       F96ZI.    IF    CT00-UCTIDX (32) NOT = SPACE                     lv20
                 NEXT SENTENCE ELSE GO TO     F96ZI-FN.                 AAOFIA
      *CAN'T DO ANYTHING, GET OUT                                       AAOFIA
               GO TO     F96ZB-FN.                                      AAOFIA
       F96ZI-FN. EXIT.
      *N96ZJ.    NOTE *ZERO FILL TO THE LEFT              *.            AAOFIA
       F96ZJ.    IF    ICT00D > ZERO                                    lv20
                 AND   < +35                                            AAOFIA
                 NEXT SENTENCE ELSE GO TO     F96ZJ-FN.                 AAOFIA
           MOVE        ZERO TO CT00-UCTIDX (ICT00D)                     AAOFIA
           SUBTRACT    +1 FROM ICT00D.                                  AAOFIA
       F96ZJ-900. GO TO F96ZJ.
       F96ZJ-FN. EXIT.
      *N96ZK.    NOTE *ISOLATE CTIDA                      *.            AAOFIA
       F96ZK.                                                           lv20
           MOVE        CT01-CTIDAX TO 7-CT00-CTIDAX                     AAOFIA
           MOVE        SPACES TO CT01-CTIDAX.                           AAOFIA
       F96ZK-FN. EXIT.
       F96ZF-FN. EXIT.
      *N96ZN.    NOTE *PROCESS CHECK DIGIT (GECKD)        *.            AAOFIA
       F96ZN.                                                           lv15
           MOVE        +35 TO ICT00R ICT00D.                            AAOFIA
      *N96ZO.    NOTE *SET INDEX TO LAST CHARACTER        *.            AAOFIA
       F96ZO.    IF    CT00-UCTIDX (ICT00R) = SPACE                     lv20
                 NEXT SENTENCE ELSE GO TO     F96ZO-FN.                 AAOFIA
           SUBTRACT    +1 FROM ICT00R.                                  AAOFIA
       F96ZO-900. GO TO F96ZO.
       F96ZO-FN. EXIT.
      *N96ZP.    NOTE *REMOVE ANY SPACES ON RIGHT         *.            AAOFIA
       F96ZP.    IF    ICT00R > ZERO                                    lv20
                 AND   < +35                                            AAOFIA
                 NEXT SENTENCE ELSE GO TO     F96ZP-FN.                 AAOFIA
           MOVE        CT00-UCTIDX (ICT00R) TO                          AAOFIA
           CT00-UCTIDX (ICT00D)                                         AAOFIA
           SUBTRACT    +1 FROM ICT00R ICT00D.                           AAOFIA
       F96ZP-900. GO TO F96ZP.
       F96ZP-FN. EXIT.
      *N96ZQ.    NOTE *ZERO FILL TO THE LEFT              *.            AAOFIA
       F96ZQ.    IF    ICT00D > ZERO                                    lv20
                 NEXT SENTENCE ELSE GO TO     F96ZQ-FN.                 AAOFIA
           MOVE        ZERO TO CT00-UCTIDX (ICT00D)                     AAOFIA
           SUBTRACT    +1 FROM ICT00D.                                  AAOFIA
       F96ZQ-900. GO TO F96ZQ.
       F96ZQ-FN. EXIT.
      *N96ZR.    NOTE *POSITION 34 SPACE, ISOLATE DIGIT   *.            AAOFIA
       F96ZR.    IF    CT00-UCTIDX (34) = SPACE                         lv20
                 NEXT SENTENCE ELSE GO TO     F96ZR-FN.                 AAOFIA
           MOVE        CT02-GECKDX TO 7-CT00-GECKDX                     AAOFIA
           MOVE        SPACE TO CT02-GECKDX.                            AAOFIA
       F96ZR-FN. EXIT.
       F96ZN-FN. EXIT.
      *N96ZS.    NOTE *PROCESS ACCT NUMBER (CTIDN)        *.            AAOFIA
       F96ZS.                                                           lv15
           MOVE        +35 TO ICT00R ICT00D.                            AAOFIA
      *N96ZT.    NOTE *REMOVE ANY REMAINING SPACES        *.            AAOFIA
       F96ZT.    IF    ICT00R > ZERO                                    lv20
                 NEXT SENTENCE ELSE GO TO     F96ZT-FN.                 AAOFIA
      *N96ZU.    NOTE *SKIP ANY IMBEDDED SPACES           *.            AAOFIA
       F96ZU.    IF    CT00-UCTIDX (ICT00R) = SPACE                     lv25
                 NEXT SENTENCE ELSE GO TO     F96ZU-FN.                 AAOFIA
           SUBTRACT    +1 FROM ICT00R.                                  AAOFIA
       F96ZU-900. GO TO F96ZV-FN.
       F96ZU-FN. EXIT.
      *N96ZV.    NOTE *HIT A CHARACTER                    *.            AAOFIA
       F96ZV.                                                           lv25
                 IF    ICT00R < ICT00D                                  DOT
      *SHIFT CHARACTER IF NEEDED                                        AAOFIA
           MOVE        CT00-UCTIDX (ICT00R) TO                          AAOFIA
           CT00-UCTIDX (ICT00D).                                        AAOFIA
           SUBTRACT    +1 FROM ICT00R ICT00D.                           DOT
       F96ZV-FN. EXIT.
       F96ZT-900. GO TO F96ZT.
       F96ZT-FN. EXIT.
      *N96ZW.    NOTE *ZERO FILL TO THE LEFT              *.            AAOFIA
       F96ZW.    IF    ICT00D > ZERO                                    lv20
                 NEXT SENTENCE ELSE GO TO     F96ZW-FN.                 AAOFIA
           MOVE        ZERO TO CT00-UCTIDX (ICT00D)                     AAOFIA
           SUBTRACT    +1 FROM ICT00D.                                  AAOFIA
       F96ZW-900. GO TO F96ZW.
       F96ZW-FN. EXIT.
      *N96ZX.    NOTE *ISOLATE CTIDN                      *.            AAOFIA
       F96ZX.                                                           lv20
           MOVE        CT03-CTIDNX TO 7-CT00-CTIDNX.                    AAOFIA
       F96ZX-FN. EXIT.
       F96ZS-FN. EXIT.
       F96ZB-FN. EXIT.
       F96-FN.   EXIT.
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
