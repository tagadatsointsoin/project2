       IDENTIFICATION DIVISION.                                         CI0079
       PROGRAM-ID.  CI0079P.                                            CI0079
      *AUTHOR.         VALIDATE FREQ FOR TERM ELECTD.                   CI0079
      *DATE-COMPILED.   09/08/14.                                       CI0079
       ENVIRONMENT DIVISION.                                            CI0079
       CONFIGURATION SECTION.                                           CI0079
       SOURCE-COMPUTER. IBM-370.                                        CI0079
       OBJECT-COMPUTER. IBM-370.                                        CI0079
       DATA DIVISION.                                                   CI0079
       WORKING-STORAGE SECTION.                                         CI0079
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
       01  WS01-DENOMINATOR      PIC 9(03) VALUE ZEROS.
       01  WS01-REMAINDER        PIC 9(03) VALUE ZEROS.
       01  WS02-QMTH1A           PIC 9(03) VALUE ZEROS.
       01   DEBUT-WSS.                                                  CI0079
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0079
            05   IK     PICTURE X.                                      CI0079
       01  CONSTANTES-PAC.                                              CI0079
           05  FILLER  PICTURE X(87)   VALUE                            CI0079
                     '6015 CAT09/08/14CI0079ADMIN   14:34:39CI0079P AMERCI0079
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0079
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0079
           05  NUGNA   PICTURE X(5).                                    CI0079
           05  APPLI   PICTURE X(3).                                    CI0079
           05  DATGN   PICTURE X(8).                                    CI0079
           05  PROGR   PICTURE X(6).                                    CI0079
           05  CODUTI  PICTURE X(8).                                    CI0079
           05  TIMGN   PICTURE X(8).                                    CI0079
           05  PROGE   PICTURE X(8).                                    CI0079
           05  COBASE  PICTURE X(4).                                    CI0079
           05  DATGNC  PICTURE X(10).                                   CI0079
           05  RELEAS  PICTURE X(7).                                    CI0079
           05  DATGE   PICTURE X(10).                                   CI0079
           05  DATSQ   PICTURE X(10).                                   CI0079
       01  DATCE.                                                       CI0079
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0079
         05  DATOR.                                                     CI0079
           10  DATOA  PICTURE XX.                                       CI0079
           10  DATOM  PICTURE XX.                                       CI0079
           10  DATOJ  PICTURE XX.                                       CI0079
       01   VARIABLES-CONDITIONNELLES.                                  CI0079
            05                  FT      PICTURE X VALUE '0'.            CI0079
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0079
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0079
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0079
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **     WM23 SEGMENT CONTAINS THE INPUT AND OUTPUT PARAMETERS.    *
      ******************************************************************
      *
      *!WF DSP=WM DSL=WM SEL=28 FOR=I LEV=1 PLT=10
       01                 WM00.                                         CI0079
          05              WM00-SUITE.                                   CI0079
            15       FILLER         PICTURE  X(00008).                  CI0079
       01                 WM28  REDEFINES      WM00.                    CI0079
            10            WM28-QMTH1  PICTURE  9(3).                    CI0079
            10            WM28-CPMTFA PICTURE  X(2).                    CI0079
            10            WM28-QMTH1A PICTURE  9(3).                    CI0079
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
       01                 MS00.                                         CI0079
          05              MS00-SUITE.                                   CI0079
            15       FILLER         PICTURE  X(00542).                  CI0079
       01                 MS03  REDEFINES      MS00.                    CI0079
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0079
                          COMPUTATIONAL-3.                              CI0079
            10            MS03-CMSSF  PICTURE  XX.                      CI0079
            10            MS03-DU09.                                    CI0079
            11            MS03-CMESA  PICTURE  S9(9)                    CI0079
                          BINARY.                                       CI0079
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0079
                          BINARY.                                       CI0079
            11            MS03-CMESB  PICTURE  S9(9)                    CI0079
                          BINARY.                                       CI0079
            11            MS03-CMSST  PICTURE  S9(9)                    CI0079
                          BINARY.                                       CI0079
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0079
                          BINARY.                                       CI0079
            11            MS03-QELLAA PICTURE  S9(9)                    CI0079
                          BINARY.                                       CI0079
            11            MS03-TMESS4 PICTURE  X(512).                  CI0079
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0079
            10            MX11-QMSGS  PICTURE  9(03).                   CI0079
            10            MX11-PJ09                                     CI0079
                          OCCURS       025     TIMES.                   CI0079
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0079
                          COMPUTATIONAL-3.                              CI0079
            11            MX11-CMESB  PICTURE  S9(9)                    CI0079
                          BINARY.                                       CI0079
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                WM28
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0079
      *               *                                   *             CI0079
      *               *INITIALISATIONS                    *             CI0079
      *               *                                   *             CI0079
      *               *************************************.            CI0079
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0079
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0079
      *               *                                   *             CI0079
      *               *FIN DE TRAITEMENT                  *             CI0079
      *               *                                   *             CI0079
      *               *************************************.            CI0079
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0079
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               ****** VALIDATE FREQUENCY *******   *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BA.    NOTE ** VALIDATE PMT FREQ ALPHA CODE *   *.
       F50BA.    IF    WM28-CPMTFA NOT = 'MO'                           lv10
                 AND   'BM'
                 AND   'QT'
                 AND   'SA'
                 AND   'AN'
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      **
      **
      **
      **
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012568 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50BA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               ****** CALCULATE FREQUENCY ******   *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55BA.    NOTE ** CASE STATEMENT TO CALC FREQ  *   *.
       F55BA.         EXIT.                                             lv10
      *N55BB.    NOTE *** CALC TERM WITH ANNUAL FREQ **   *.
       F55BB.    IF    WM28-CPMTFA =                                    lv15
                       'AN'
                 NEXT SENTENCE ELSE GO TO     F55BB-FN.
           DIVIDE      12 INTO WM28-QMTH1
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ** IS REMAINDER = ZEROS         *
           MOVE        WM28-QMTH1 TO WM28-QMTH1A
                 ELSE
      ** FREQUENCY DOESN'T MATCH TERM *
      ** TERM ELECTED ALTERED         *
           COMPUTE     WS02-QMTH1A = 12 -
           WS01-REMAINDER
           COMPUTE     WS02-QMTH1A = WS02-QMTH1A +
           WM28-QMTH1
           MOVE        WS02-QMTH1A TO WM28-QMTH1A
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012773 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
      ******  END OF 99 LEVEL IF  *****                                 DOT
       F55BB-900. GO TO F55BA-FN.
       F55BB-FN. EXIT.
      *N55BC.    NOTE ** CALC TERM WITH SEMI-ANN FREQ *   *.
       F55BC.    IF    WM28-CPMTFA =                                    lv15
                       'SA'
                 NEXT SENTENCE ELSE GO TO     F55BC-FN.
           DIVIDE      6 INTO WM28-QMTH1
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ** IS REMAINDER = ZEROS         *
           MOVE        WM28-QMTH1 TO WM28-QMTH1A
                 ELSE
      ** FREQUENCY DOESN'T MATCH TERM *
      ** TERM ELECTED ALTERED         *
           COMPUTE     WS02-QMTH1A = 6 -
           WS01-REMAINDER
           COMPUTE     WS02-QMTH1A = WS02-QMTH1A +
           WM28-QMTH1
           MOVE        WS02-QMTH1A TO WM28-QMTH1A
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012773 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
      ******  END OF 99 LEVEL IF  *****                                 DOT
       F55BC-900. GO TO F55BA-FN.
       F55BC-FN. EXIT.
      *N55BD.    NOTE ** CALC TERM W / QUARTERLY FREQ *   *.
       F55BD.    IF    WM28-CPMTFA =                                    lv15
                       'QT'
                 NEXT SENTENCE ELSE GO TO     F55BD-FN.
           DIVIDE      3 INTO WM28-QMTH1
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ** IS REMAINDER = ZEROS         *
           MOVE        WM28-QMTH1 TO WM28-QMTH1A
                 ELSE
      ** FREQUENCY DOESN'T MATCH TERM *
      ** TERM ELECTED ALTERED         *
           COMPUTE     WS02-QMTH1A = 3 -
           WS01-REMAINDER
           COMPUTE     WS02-QMTH1A = WS02-QMTH1A +
           WM28-QMTH1
           MOVE        WS02-QMTH1A TO WM28-QMTH1A
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012773 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
      ******  END OF 99 LEVEL IF  *****                                 DOT
       F55BD-900. GO TO F55BA-FN.
       F55BD-FN. EXIT.
      *N55BE.    NOTE ** CALC TERM W / BI-MNTHLY FREQ *   *.
       F55BE.    IF    WM28-CPMTFA =                                    lv15
                       'BM'
                 NEXT SENTENCE ELSE GO TO     F55BE-FN.
           DIVIDE      2 INTO WM28-QMTH1
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ** IS REMAINDER = ZEROS         *
           MOVE        WM28-QMTH1 TO WM28-QMTH1A
                 ELSE
      ** FREQUENCY DOESN'T MATCH TERM *
      ** TERM ELECTED ALTERED         *
           COMPUTE     WS02-QMTH1A = 2 -
           WS01-REMAINDER
           COMPUTE     WS02-QMTH1A = WS02-QMTH1A +
           WM28-QMTH1
           MOVE        WS02-QMTH1A TO WM28-QMTH1A
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012773 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
      ******  END OF 99 LEVEL IF  *****                                 DOT
       F55BE-900. GO TO F55BA-FN.
       F55BE-FN. EXIT.
      *N55BF.    NOTE *** CALC TERM W / MONTHLY FREQ **   *.
       F55BF.                                                           lv15
           MOVE        WM28-QMTH1 TO WM28-QMTH1A.
       F55BF-FN. EXIT.
       F55BA-FN. EXIT.
       F55-FN.   EXIT.
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
