       IDENTIFICATION DIVISION.                                         CI0078
       PROGRAM-ID.  CI0078P.                                            CI0078
      *AUTHOR.         CALCULATE END DATE.                              CI0078
      *DATE-COMPILED.   09/08/14.                                       CI0078
       ENVIRONMENT DIVISION.                                            CI0078
       CONFIGURATION SECTION.                                           CI0078
       SOURCE-COMPUTER. IBM-370.                                        CI0078
       OBJECT-COMPUTER. IBM-370.                                        CI0078
       DATA DIVISION.                                                   CI0078
       WORKING-STORAGE SECTION.                                         CI0078
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      ******************************************************************
      **  DATE WORK AREA USED WITH MACRO AADA56                        *
      ******************************************************************
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0078
          05              DD00-SUITE.                                   CI0078
            15       FILLER         PICTURE  X(00093).                  CI0078
       01                 DD01  REDEFINES      DD00.                    CI0078
            10            DD01-XDAT8.                                   CI0078
            11            DD01-XDATC  PICTURE  XX.                      CI0078
            11            DD01-XDATY  PICTURE  XX.                      CI0078
            11            DD01-XDATM  PICTURE  XX.                      CI0078
            11            DD01-XDATD  PICTURE  XX.                      CI0078
            10            DD01-XDAT8D                                   CI0078
                          REDEFINES            DD01-XDAT8               CI0078
               PICTURE    9(8).                                         CI0078
            10            DD01-XDAT81.                                  CI0078
            11            DD01-XDATM1 PICTURE  XX.                      CI0078
            11            DD01-XDATD1 PICTURE  XX.                      CI0078
            11            DD01-XDATC1 PICTURE  XX.                      CI0078
            11            DD01-XDATY1 PICTURE  XX.                      CI0078
            10            DD01-XDAT80                                   CI0078
                          REDEFINES            DD01-XDAT81              CI0078
               PICTURE    9(8).                                         CI0078
            10            DD01-XDAT62.                                  CI0078
            11            DD01-XDATM2 PICTURE  XX.                      CI0078
            11            DD01-XDATD2 PICTURE  XX.                      CI0078
            11            DD01-XDATY2 PICTURE  XX.                      CI0078
            10            DD01-XDAT69                                   CI0078
                          REDEFINES            DD01-XDAT62              CI0078
               PICTURE    9(6).                                         CI0078
            10            DD01-XDATCU.                                  CI0078
            11            DD01-XDATC9 PICTURE  99.                      CI0078
            11            DD01-XDAYMD.                                  CI0078
            12            DD01-XDATY9 PICTURE  99.                      CI0078
            12            DD01-XDAMD.                                   CI0078
            13            DD01-XDATM9 PICTURE  99.                      CI0078
            13            DD01-XDATD9 PICTURE  99.                      CI0078
            10            DD01-XDAT89 PICTURE  9(8).                    CI0078
            10            DD01-XDAJC  PICTURE  9(7).                    CI0078
            10            DD01-XDAJC1.                                  CI0078
            11            DD01-XDAJC9 PICTURE  99.                      CI0078
            11            DD01-XDAJY  PICTURE  99.                      CI0078
            11            DD01-XDAJN  PICTURE  999.                     CI0078
            10            DD01-XDAB   PICTURE  9(5).                    CI0078
            10            DD01-DD05.                                    CI0078
            11            DD01-XDACT  PICTURE  S9(3)                    CI0078
                          COMPUTATIONAL-3.                              CI0078
            11            DD01-XDACV  PICTURE  S9                       CI0078
                          COMPUTATIONAL-3.                              CI0078
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0078
                          COMPUTATIONAL-3.                              CI0078
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0078
                          COMPUTATIONAL-3.                              CI0078
            11            DD01-XDACV1 PICTURE  S9                       CI0078
                          COMPUTATIONAL-3.                              CI0078
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0078
                          COMPUTATIONAL-3.                              CI0078
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0078
                          COMPUTATIONAL-3.                              CI0078
            10            DD01-XW03.                                    CI0078
            11            DD01-XDATG.                                   CI0078
            12            DD01-XDAT1.                                   CI0078
            13            DD01-XDAT19 PICTURE  99.                      CI0078
            12            DD01-XDAT2.                                   CI0078
            13            DD01-XDAT29 PICTURE  99.                      CI0078
            12            DD01-XDAT3.                                   CI0078
            13            DD01-XDAT39 PICTURE  99.                      CI0078
            12            DD01-XDAT4.                                   CI0078
            13            DD01-XDAT49 PICTURE  99.                      CI0078
            11            DD01-XLEAPY PICTURE  99.                      CI0078
            11            DD01-DTGCY  PICTURE  9(4).                    CI0078
            11            DD01-FILLER                                   CI0078
                          REDEFINES            DD01-DTGCY.              CI0078
            12            DD01-DTGCC  PICTURE  9(2).                    CI0078
            12            DD01-DTGYY  PICTURE  9(2).                    CI0078
      ** DATE WORK AREA
       01  DEL-ER                 PIC 9(1).
       01  DT02-NXT-PMT-DTE.
           05  DT02-NXT-PMT-DTE-MTH      PIC 9(02) VALUE ZEROS.
           05  DT02-NXT-PMT-DTE-DAY      PIC 9(02) VALUE ZEROS.
           05  DT02-NXT-PMT-DTE-CCYY     PIC 9(04) VALUE ZEROS.
           05  DT02-NXT-PMT-DTE-CCYY-R REDEFINES DT02-NXT-PMT-DTE-CCYY.
               10  DT02-NXT-PMT-DTE-CC   PIC 9(02).
               10  DT02-NXT-PMT-DTE-YR   PIC 9(02).
       01  DT03-END-DATE.
           05  DT03-END-DTE-MTH          PIC 9(02) VALUE ZEROS.
           05  DT03-END-DTE-DAY          PIC 9(02) VALUE ZEROS.
           05  DT03-END-DTE-CCYY         PIC 9(04) VALUE ZEROS.
           05  DT03-END-DTE-CCYY-R REDEFINES DT03-END-DTE-CCYY.
               10  DT03-END-DTE-CC       PIC 9(02).
               10  DT03-END-DTE-YR       PIC 9(02).
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
       01  WS01-YRS         PIC 9(03) VALUE ZEROS.
       01  WS01-MTHS        PIC 9(03) VALUE ZEROS.
       01  WS01-LEAP-YR     PIC X(01) VALUE SPACE.
       01  WS01-DENOMINATOR PIC 9(04) VALUE ZEROS.
       01  WS01-REMAINDER   PIC 9(04) VALUE ZEROS.
       01   DEBUT-WSS.                                                  CI0078
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0078
            05   IK     PICTURE X.                                      CI0078
       01  CONSTANTES-PAC.                                              CI0078
           05  FILLER  PICTURE X(87)   VALUE                            CI0078
                     '6015 CAT09/08/14CI0078ADMIN   14:34:39CI0078P AMERCI0078
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0078
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0078
           05  NUGNA   PICTURE X(5).                                    CI0078
           05  APPLI   PICTURE X(3).                                    CI0078
           05  DATGN   PICTURE X(8).                                    CI0078
           05  PROGR   PICTURE X(6).                                    CI0078
           05  CODUTI  PICTURE X(8).                                    CI0078
           05  TIMGN   PICTURE X(8).                                    CI0078
           05  PROGE   PICTURE X(8).                                    CI0078
           05  COBASE  PICTURE X(4).                                    CI0078
           05  DATGNC  PICTURE X(10).                                   CI0078
           05  RELEAS  PICTURE X(7).                                    CI0078
           05  DATGE   PICTURE X(10).                                   CI0078
           05  DATSQ   PICTURE X(10).                                   CI0078
       01  DATCE.                                                       CI0078
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0078
         05  DATOR.                                                     CI0078
           10  DATOA  PICTURE XX.                                       CI0078
           10  DATOM  PICTURE XX.                                       CI0078
           10  DATOJ  PICTURE XX.                                       CI0078
       01   VARIABLES-CONDITIONNELLES.                                  CI0078
            05                  FT      PICTURE X VALUE '0'.            CI0078
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0078
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0078
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0078
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **     WM23 SEGMENT CONTAINS THE INPUT AND OUTPUT PARAMETERS.    *
      ******************************************************************
      *
      *!WF DSP=WM DSL=WM SEL=23 FOR=I LEV=1 PLT=10
       01                 WM00.                                         CI0078
          05              WM00-SUITE.                                   CI0078
            15       FILLER         PICTURE  X(00019).                  CI0078
       01                 WM23  REDEFINES      WM00.                    CI0078
            10            WM23-QMTH1  PICTURE  9(3).                    CI0078
            10            WM23-DNPMT  PICTURE  9(8).                    CI0078
            10            WM23-GEEND  PICTURE  9(8).                    CI0078
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
       01                 MS00.                                         CI0078
          05              MS00-SUITE.                                   CI0078
            15       FILLER         PICTURE  X(00542).                  CI0078
       01                 MS03  REDEFINES      MS00.                    CI0078
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0078
                          COMPUTATIONAL-3.                              CI0078
            10            MS03-CMSSF  PICTURE  XX.                      CI0078
            10            MS03-DU09.                                    CI0078
            11            MS03-CMESA  PICTURE  S9(9)                    CI0078
                          BINARY.                                       CI0078
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0078
                          BINARY.                                       CI0078
            11            MS03-CMESB  PICTURE  S9(9)                    CI0078
                          BINARY.                                       CI0078
            11            MS03-CMSST  PICTURE  S9(9)                    CI0078
                          BINARY.                                       CI0078
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0078
                          BINARY.                                       CI0078
            11            MS03-QELLAA PICTURE  S9(9)                    CI0078
                          BINARY.                                       CI0078
            11            MS03-TMESS4 PICTURE  X(512).                  CI0078
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0078
            10            MX11-QMSGS  PICTURE  9(03).                   CI0078
            10            MX11-PJ09                                     CI0078
                          OCCURS       025     TIMES.                   CI0078
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0078
                          COMPUTATIONAL-3.                              CI0078
            11            MX11-CMESB  PICTURE  S9(9)                    CI0078
                          BINARY.                                       CI0078
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                WM23
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0078
      *               *                                   *             CI0078
      *               *INITIALISATIONS                    *             CI0078
      *               *                                   *             CI0078
      *               *************************************.            CI0078
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
      *N02BD.    NOTE ** INITIALIZE DT02-NXT-PMT-DTE **   *.
       F02BD.                                                           lv10
           MOVE        WM23-DNPMT TO DT02-NXT-PMT-DTE
           MOVE        DT02-NXT-PMT-DTE TO DT03-END-DATE.
       F02BD-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0078
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0078
      *               *                                   *             CI0078
      *               *FIN DE TRAITEMENT                  *             CI0078
      *               *                                   *             CI0078
      *               *************************************.            CI0078
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0078
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *END DATE CALCULATION               *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55CA.    NOTE ****** END DATE CALCULATIONS ****   *.
       F55CA.                                                           lv10
           COMPUTE     WS01-YRS = WM23-QMTH1 / 12
           COMPUTE     DT03-END-DTE-CCYY =
           DT02-NXT-PMT-DTE-CCYY + WS01-YRS
           COMPUTE     WS01-MTHS = WM23-QMTH1
           - (WS01-YRS * 12) - 1
           COMPUTE     DT03-END-DTE-MTH =
           DT02-NXT-PMT-DTE-MTH + WS01-MTHS.
      *N55CD.    NOTE *** DETERMINE IF MONTH OVER 12 **   *.
       F55CD.    IF    DT03-END-DTE-MTH > 12                            lv15
                 NEXT SENTENCE ELSE GO TO     F55CD-FN.
           COMPUTE     WS01-MTHS = DT03-END-DTE-MTH
           / 12
           COMPUTE     DT03-END-DTE-CCYY = WS01-MTHS +
           DT03-END-DTE-CCYY
           COMPUTE     WS01-MTHS = WS01-MTHS * 12
           COMPUTE     DT03-END-DTE-MTH =
           DT03-END-DTE-MTH - WS01-MTHS.
       F55CD-FN. EXIT.
      *N55CG.    NOTE *** DETERMINE IF MONTH = ZEROS **   *.
       F55CG.    IF    DT03-END-DTE-MTH = ZEROS                         lv15
                 NEXT SENTENCE ELSE GO TO     F55CG-FN.
           COMPUTE     DT03-END-DTE-CCYY =
           DT03-END-DTE-CCYY - 1
           MOVE        12 TO DT03-END-DTE-MTH.
       F55CG-FN. EXIT.
      *N55CJ.    NOTE *** MONTHS AND DAY COMPARISONS **   *.
       F55CJ.         EXIT.                                             lv15
      *N55CK.    NOTE ** MTHS OF APR JUN SEP NOV (30) *   *.
       F55CK.    IF    (DT03-END-DTE-MTH = 04                           lv20
                 OR    06
                 OR    09
                 OR    11)
                 AND   DT03-END-DTE-DAY > 30
                 NEXT SENTENCE ELSE GO TO     F55CK-FN.
      **
      **
      **
           MOVE        30 TO DT03-END-DTE-DAY.
       F55CK-FN. EXIT.
      *N55CN.    NOTE *** MONTHS OF FEB (28/29 DAYS( **   *.
       F55CN.    IF    DT03-END-DTE-MTH = 02                            lv20
                 AND   DT03-END-DTE-DAY > 28
                 NEXT SENTENCE ELSE GO TO     F55CN-FN.
      **
      *N55CO.    NOTE ******** LEAP YEAR CHECK 1 ******   *.
       F55CO.                                                           lv25
           DIVIDE      DT03-END-DTE-CCYY INTO 4
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ******  YEAR IS A LEAP YEAR *****
           MOVE        'Y' TO WS01-LEAP-YR
                 ELSE
      ****** YEAR NOT A LEAP YEAR *****
           MOVE        'N' TO WS01-LEAP-YR.
      ******   END OF 99 LEVEL    *****                                 DOT
       F55CO-FN. EXIT.
      *N55CP.    NOTE ******** LEAP YEAR CHECK 2 ******   *.
       F55CP.                                                           lv25
           DIVIDE      DT03-END-DTE-CCYY INTO 100
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ****** YEAR NOT A LEAP YEAR *****
           MOVE        'N' TO WS01-LEAP-YR
                 ELSE
      ******  YEAR IS A LEAP YEAR *****
           MOVE        'Y' TO WS01-LEAP-YR.
      ******   END OF 99 LEVEL    *****                                 DOT
       F55CP-FN. EXIT.
      *N55CQ.    NOTE ******** LEAP YEAR CHECK 3 ******   *.
       F55CQ.                                                           lv25
           DIVIDE      DT03-END-DTE-CCYY INTO 400
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ******  YEAR IS A LEAP YEAR *****
           MOVE        'Y' TO WS01-LEAP-YR
                 ELSE
      ****** YEAR NOT A LEAP YEAR *****
           MOVE        'N' TO WS01-LEAP-YR.
      ******   END OF 99 LEVEL    *****                                 DOT
       F55CQ-FN. EXIT.
      *N55CR.    NOTE ******** LEAP YEAR CHECK 4 ******   *.
       F55CR.    IF    DT03-END-DTE-CCYY >= 3200                        lv25
                 NEXT SENTENCE ELSE GO TO     F55CR-FN.
           DIVIDE      DT03-END-DTE-CCYY INTO 3200
           GIVING WS01-DENOMINATOR
           REMAINDER WS01-REMAINDER.
                 IF    WS01-REMAINDER = ZEROS                           DOT
      ****** YEAR NOT A LEAP YEAR *****
           MOVE        'N' TO WS01-LEAP-YR
                 ELSE
      ******  YEAR IS A LEAP YEAR *****
           MOVE        'Y' TO WS01-LEAP-YR.
      ******   END OF 99 LEVEL    *****                                 DOT
       F55CR-FN. EXIT.
      *N55CS.    NOTE *** MOVE DAY INTO THE END DATE **   *.
       F55CS.                                                           lv25
                 IF    WS01-LEAP-YR = 'Y'                               DOT
           MOVE        29 TO DT03-END-DTE-DAY
                 ELSE
           MOVE        28 TO DT03-END-DTE-DAY.
      ******   END OF 99 LEVEL    *****                                 DOT
       F55CS-FN. EXIT.
       F55CN-FN. EXIT.
       F55CJ-FN. EXIT.
       F55CA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               ****** END DATE VALIDATION ******   *
      *               *                                   *
      *               *************************************.
       F60.           EXIT.                                             lv05
      *N60DA.    NOTE *LOAD DATE VALIDATION MACRO         *.
       F60DA.                                                           lv10
           MOVE        DT03-END-DATE TO DD01-XDATG
           PERFORM     F92DT THRU F92DT-FN.
       F60DA-FN. EXIT.
      *N60DU.    NOTE ******* VALIDATE END DATE *******   *.
       F60DU.    IF    DEL-ER NOT = 1                                   lv10
                 NEXT SENTENCE ELSE GO TO     F60DU-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012205 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F60DU-FN. EXIT.
      *N60EA.    NOTE *** MOVE VALUES TO OUTPUT SEG ***   *.
       F60EA.                                                           lv10
           MOVE        DT03-END-DATE TO WM23-GEEND.
       F60EA-FN. EXIT.
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
      *N92DT.    NOTE *DATE VALIDATION                    *.            AADA56
       F92DT.                                                           lv10
           MOVE        1 TO DEL-ER.                                     AADA56
                 IF    DD01-XDATG NOT NUMERIC                           DOT
           MOVE        4 TO DEL-ER                                      AADA56
               GO TO     F92DT-FN.                                      AADA56
                 IF    DD01-XDAT3 > '99'                                DOT
                 OR    DD01-XDAT3 < '18'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92DT-FN.                                      AADA56
                 IF    DD01-XDAT1 > '12'                                DOT
                 OR    DD01-XDAT1 = '00'                                AADA56
                 OR    DD01-XDAT2 > '31'                                AADA56
                 OR    DD01-XDAT2 = '00'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92DT-FN.                                      AADA56
                 IF    DD01-XDAT2 > '30'                                DOT
                 AND   (DD01-XDAT1 = '04'                               AADA56
                 OR    DD01-XDAT1 = '06'                                AADA56
                 OR    DD01-XDAT1 = '09'                                AADA56
                 OR    DD01-XDAT1 = '11')                               AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92DT-FN.                                      AADA56
                 IF    DD01-XDAT1 NOT = '02'                            DOT
               GO TO     F92DT-FN.                                      AADA56
                 IF    DD01-XDAT2 > '29'                                DOT
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92DT-FN.                                      AADA56
           MOVE        DD01-XDAT49 TO DD01-DTGYY                        DOT
           MOVE        DD01-XDAT39 TO DD01-DTGCC.                       AADA56
                 IF    DD01-DTGYY NOT = ZERO                            DOT
           COMPUTE     DD01-XLEAPY = DD01-DTGCY -                       AADA56
           ((DD01-DTGCY / 4) * 4)                                       AADA56
                 ELSE                                                   AADA56
           COMPUTE     DD01-XLEAPY = (DD01-DTGCY -                      AADA56
           ((DD01-DTGCY / 400) * 400))                                  AADA56
           / 100.                                                       AADA56
                 IF    DD01-XLEAPY NOT = ZERO                           DOT
                 AND   DD01-XDAT2 > '28'                                AADA56
           MOVE        5 TO DEL-ER                                      AADA56
               GO TO     F92DT-FN.                                      AADA56
       F92DT-FN. EXIT.
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
