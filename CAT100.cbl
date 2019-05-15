       IDENTIFICATION DIVISION.                                         CAT100
       PROGRAM-ID.  CAT100.                                             CAT100
      *AUTHOR.         MONEY MOVEMENT RULES.                            CAT100
      *DATE-COMPILED.   09/08/14.                                       CAT100
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1997                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CATS  SYSTEM AND ALL INFORMATION RELATING THERETO,    * ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CATS  SYSTEM AND ALL            * ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CATS        * ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 1997                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CAT100
       CONFIGURATION SECTION.                                           CAT100
       SOURCE-COMPUTER. IBM-370.                                        CAT100
       OBJECT-COMPUTER. IBM-370.                                        CAT100
       DATA DIVISION.                                                   CAT100
       WORKING-STORAGE SECTION.                                         CAT100
       01   DEBUT-WSS.                                                  CAT100
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CAT100
            05   IK     PICTURE X.                                      CAT100
       01  CONSTANTES-PAC.                                              CAT100
           05  FILLER  PICTURE X(87)   VALUE                            CAT100
                     '9999 CAT09/08/14CAT100ADMIN   19:28:28CAT100  BVAPCAT100
      -    '09/08/20143.5 V0419/02/201425/02/2014'.                     CAT100
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CAT100
           05  NUGNA   PICTURE X(5).                                    CAT100
           05  APPLI   PICTURE X(3).                                    CAT100
           05  DATGN   PICTURE X(8).                                    CAT100
           05  PROGR   PICTURE X(6).                                    CAT100
           05  CODUTI  PICTURE X(8).                                    CAT100
           05  TIMGN   PICTURE X(8).                                    CAT100
           05  PROGE   PICTURE X(8).                                    CAT100
           05  COBASE  PICTURE X(4).                                    CAT100
           05  DATGNC  PICTURE X(10).                                   CAT100
           05  RELEAS  PICTURE X(7).                                    CAT100
           05  DATGE   PICTURE X(10).                                   CAT100
           05  DATSQ   PICTURE X(10).                                   CAT100
       01  DATCE.                                                       CAT100
         05  CENTUR   PICTURE XX   VALUE '20'.                          CAT100
         05  DATOR.                                                     CAT100
           10  DATOA  PICTURE XX.                                       CAT100
           10  DATOM  PICTURE XX.                                       CAT100
           10  DATOJ  PICTURE XX.                                       CAT100
       01   VARIABLES-CONDITIONNELLES.                                  CAT100
            05                  FT      PICTURE X VALUE '0'.            CAT100
       01   INDICES  COMPUTATIONAL  SYNC.                               CAT100
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CAT100
       01   ZONES-UTILISATEUR PICTURE X.                                CAT100
       LINKAGE SECTION.                                                 AUSING
      *!WF DSP=K1 DSL=CP SEL=9F FOR=I LEV=1                             AUSING
       01                 K100.                                         CAT100
          05              K100-SUITE.                                   CAT100
            15       FILLER         PICTURE  X(00080).                  CAT100
       01                 K19F  REDEFINES      K100.                    CAT100
            10            K19F-CTIDA  PICTURE  9(3).                    CAT100
            10            K19F-PRCOD  PICTURE  9(5).                    CAT100
            10            K19F-PRSCD  PICTURE  X(9).                    CAT100
            10            K19F-FILLER PICTURE  X(20).                   CAT100
            10            K19F-CTIDA1 PICTURE  9(3).                    CAT100
            10            K19F-PRCOD1 PICTURE  9(5).                    CAT100
            10            K19F-CPRSC2 PICTURE  X(9).                    CAT100
            10            K19F-ISCHA  PICTURE  X.                       CAT100
            10            K19F-CTRFA  PICTURE  X.                       CAT100
            10            K19F-FILLER PICTURE  X(24).                   CAT100
       PROCEDURE DIVISION USING K19F.
      *N01.      NOTE *************************************.            CAT100
      *               *                                   *             CAT100
      *               *INITIALISATIONS                    *             CAT100
      *               *                                   *             CAT100
      *               *************************************.            CAT100
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.
      *               *                                   *
      *               *INITIALIZATIONS                    *
      *               *                                   *
      *               *************************************.
       F02.           EXIT.                                             lv05
      *N02BA.    NOTE *RESET "GOBACK" INDICATOR           *.
       F02BA.                                                           lv10
      *THIS WILL MAKE SURE THE "FT"
      *INDICATOR IS SET TO ZERO WHEN
      *THIS MODULE CALLED MORE THAN
      *ONCE.
           MOVE ALL    '0' TO FT.
       F02BA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CAT100
       F05.           EXIT.
      *N20.      NOTE *************************************.            CAT100
      *               *                                   *             CAT100
      *               *FIN DE TRAITEMENT                  *             CAT100
      *               *                                   *             CAT100
      *               *************************************.            CAT100
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CAT100
       F2099.     GOBACK.
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *INITIALIZATIONS                    *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BA.    NOTE *MAKE SURE OF NUMERICS              *.
       F50BA.                                                           lv10
                 IF    K19F-CTIDA NOT NUMERIC                           DOT
           MOVE        ZERO TO K19F-CTIDA.
                 IF    K19F-PRCOD NOT NUMERIC                           DOT
           MOVE        ZERO TO K19F-PRCOD.
                 IF    K19F-PRSCD NOT NUMERIC                           DOT
           MOVE        ZERO TO K19F-PRSCD.
                 IF    K19F-CTIDA1 NOT NUMERIC                          DOT
           MOVE        ZERO TO K19F-CTIDA1.
                 IF    K19F-PRCOD1 NOT NUMERIC                          DOT
           MOVE        ZERO TO K19F-PRCOD1.
                 IF    K19F-CPRSC2 NOT NUMERIC                          DOT
           MOVE        ZERO TO K19F-CPRSC2.
                 IF    K19F-ISCHA = SPACES                              DOT
           MOVE        'N' TO K19F-ISCHA.
       F50BA-FN. EXIT.
      *N50BF.    NOTE *SET RETURN FIELD DEFAULT 'N'       *.
       F50BF.                                                           lv10
           MOVE        'N' TO K19F-CTRFA.
       F50BF-FN. EXIT.
       F50-FN.   EXIT.
      *N61.      NOTE *************************************.
      *               *                                   *
      *               *'FROM' ACCOUNT = CERTIFICATE       *
      *               *                                   *
      *               *************************************.
       F61.      IF    K19F-CTIDA = 001                                 lv05
                 NEXT SENTENCE ELSE GO TO     F61-FN.
      *N61BB.    NOTE *SCHEDULE ACTIVITY                  *.
       F61BB.    IF    K19F-ISCHA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F61BB-FN.
      *N61CC.    NOTE *'TO' ACCOUNT = FUND                *.
       F61CC.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F61CC-FN.
      *N61CD.    NOTE *'TO' MMK/RCM CLASS B OR CLASS C    *.
       F61CD.    IF    (K19F-PRCOD1 = 00013 OR                          lv20
                       K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F61CD-FN.
           MOVE        'N' TO K19F-CTRFA.
       F61CD-900. GO TO F61CE-FN.
       F61CD-FN. EXIT.
      *N61CE.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F61CE.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F61CE-FN. EXIT.
       F61CC-900. GO TO F61DD-FN.
       F61CC-FN. EXIT.
      *N61DD.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F61DD.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F61DD-FN. EXIT.
       F61BB-900. GO TO F61EE-FN.
       F61BB-FN. EXIT.
      *N61EE.    NOTE *NOT SCHEDULE ACTIVITY              *.
       F61EE.         EXIT.                                             lv10
      *N61FF.    NOTE *'TO' ACCOUNT = FUND                *.
       F61FF.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F61FF-FN.
      *N61GD.    NOTE *'TO' MMK/RCM CLASS B OR CLASS C    *.
       F61GD.    IF    (K19F-PRCOD1 = 00013 OR                          lv20
                       K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F61GD-FN.
           MOVE        'N' TO K19F-CTRFA.
       F61GD-900. GO TO F61GE-FN.
       F61GD-FN. EXIT.
      *N61GE.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F61GE.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F61GE-FN. EXIT.
       F61FF-900. GO TO F61HD-FN.
       F61FF-FN. EXIT.
      *N61HD.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F61HD.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F61HD-FN. EXIT.
       F61EE-FN. EXIT.
       F61-FN.   EXIT.
      *N62.      NOTE *************************************.
      *               *                                   *
      *               *'FROM' ACCOUNT = MUTUAL FUND       *
      *               *                                   *
      *               *************************************.
       F62.      IF    K19F-CTIDA = 002                                 lv05
                 NEXT SENTENCE ELSE GO TO     F62-FN.
      *N62BB.    NOTE *SCHEDULED ACTIVITY                 *.
       F62BB.    IF    K19F-ISCHA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F62BB-FN.
      *N62CC.    NOTE *'TO' ACCOUNT = MUTUAL FUND         *.
       F62CC.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F62CC-FN.
      *N62DD.    NOTE *'FROM' FUND = 'TO' FUND            *.
       F62DD.    IF    K19F-PRCOD = K19F-PRCOD1                         lv20
                 NEXT SENTENCE ELSE GO TO     F62DD-FN.
      *N62DG.    NOTE *'FROM' ACCOUNT CLASS A             *.
       F62DG.    IF    K19F-PRSCD = '000000001'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62DG-FN.
      *N62DH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62DH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62DH-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62DH-FN. EXIT.
      *N62DI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62DI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62DI-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62DI-FN. EXIT.
      *N62DK.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62DK.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62DK-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62DK-FN. EXIT.
      *N62DL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62DL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62DL-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62DL-FN. EXIT.
       F62DG-FN. EXIT.
      *N62EG.    NOTE *'FROM' ACCOUNT CLASS B             *.
       F62EG.    IF    K19F-PRSCD = '000000002'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62EG-FN.
      *N62EH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62EH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62EH-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62EH-FN. EXIT.
      *N62EI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62EI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62EI-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62EI-FN. EXIT.
      *N62EJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62EJ.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62EJ-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62EJ-FN. EXIT.
      *N62EL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62EL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62EL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62EL-FN. EXIT.
       F62EG-FN. EXIT.
      *N62EP.    NOTE *'FROM' ACCOUNT CLASS C             *.
       F62EP.    IF    K19F-PRSCD = '000000006'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62EP-FN.
      *N62EQ.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62EQ.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62EQ-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62EQ-FN. EXIT.
      *N62ER.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62ER.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62ER-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62ER-FN. EXIT.
      *N62ES.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62ES.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62ES-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62ES-FN. EXIT.
      *N62ET.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62ET.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62ET-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62ET-FN. EXIT.
       F62EP-FN. EXIT.
      *N62FG.    NOTE *'FROM' ACCOUNT CLASS Y             *.
       F62FG.    IF    K19F-PRSCD = '000000003'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62FG-FN.
      *N62FH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62FH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62FH-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62FH-FN. EXIT.
      *N62FI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62FI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62FI-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62FI-FN. EXIT.
      *N62FJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62FJ.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62FJ-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62FJ-FN. EXIT.
      *N62FL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62FL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62FL-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62FL-FN. EXIT.
       F62FG-FN. EXIT.
       F62DD-900. GO TO F62GC-FN.
       F62DD-FN. EXIT.
      *N62GC.    NOTE *'FROM' FUND ^= 'TO' FUND           *.
       F62GC.         EXIT.                                             lv20
      *N62GF.    NOTE *'FROM' ACCOUNT CLASS A             *.
       F62GF.    IF    K19F-PRSCD = '000000001'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62GF-FN.
      *N62GG.    NOTE *'FROM' ACCOUNT MONEY MARKET        *.
       F62GG.    IF    K19F-PRCOD = 00013                               lv30
                 OR    K19F-PRCOD = 00167
                 NEXT SENTENCE ELSE GO TO     F62GG-FN.
      *OR CLM GOVT MONEY MARKET FUND
      *N62GH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62GH.    IF    K19F-CPRSC2 = '000000001'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GH-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62GH-FN. EXIT.
      *N62GI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62GI.    IF    K19F-CPRSC2 = '000000002'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GI-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62GI-FN. EXIT.
      *N62GJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62GJ.    IF    K19F-CPRSC2 = '000000006'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GJ-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62GJ-FN. EXIT.
      *N62GL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62GL.    IF    K19F-CPRSC2 = '000000003'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62GL-FN. EXIT.
       F62GG-FN. EXIT.
      *N62GM.    NOTE *'FROM' ACCOUNT TAX-FREE MONEY      *.
       F62GM.    IF    K19F-PRCOD = 00016                               lv30
                 NEXT SENTENCE ELSE GO TO     F62GM-FN.
      *N62GN.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62GN.    IF    K19F-CPRSC2 = '000000001'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GN-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62GN-FN. EXIT.
      *N62GO.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62GO.    IF    K19F-CPRSC2 = '000000002'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GO-FN.
      *N62GP.    NOTE *'TO' ACCOUNT MMK CLASS B           *.
       F62GP.    IF    K19F-PRCOD1 = 00013                              lv40
                 OR    K19F-PRCOD1 = 00167
                 NEXT SENTENCE ELSE GO TO     F62GP-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62GP-900. GO TO F62GQ-FN.
       F62GP-FN. EXIT.
      *N62GQ.    NOTE *'TO' ACCOUNT NOT MMK CLASS B       *.
       F62GQ.                                                           lv40
           MOVE        'Y' TO K19F-CTRFA.
       F62GQ-FN. EXIT.
       F62GO-FN. EXIT.
      *N62GS.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62GS.    IF    K19F-CPRSC2 = '000000006'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GS-FN.
      *N62GT.    NOTE *'TO' ACCOUNT MMK                   *.
       F62GT.    IF    K19F-PRCOD1 = 00013                              lv40
                 OR    K19F-PRCOD1 = 00167
                 NEXT SENTENCE ELSE GO TO     F62GT-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62GT-900. GO TO F62GU-FN.
       F62GT-FN. EXIT.
      *N62GU.    NOTE *'TO' ACCOUNT NOT MMK               *.
       F62GU.                                                           lv40
           MOVE        'Y' TO K19F-CTRFA.
       F62GU-FN. EXIT.
       F62GS-FN. EXIT.
      *N62GW.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62GW.    IF    K19F-CPRSC2 = '000000003'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62GW-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62GW-FN. EXIT.
       F62GM-FN. EXIT.
      *N62G1.    NOTE *'FROM' ACCOUNT NOT MMK OR TFM      *.
       F62G1.    IF    K19F-PRCOD NOT = 00013                           lv30
                 AND   NOT = 00016
                 AND   NOT = 00167
                 NEXT SENTENCE ELSE GO TO     F62G1-FN.
      *OR RCM
      *N62G3.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62G3.    IF    K19F-CPRSC2 = '000000001'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62G3-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62G3-FN. EXIT.
      *N62G5.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62G5.    IF    K19F-CPRSC2 = '000000002'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62G5-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62G5-FN. EXIT.
      *N62G7.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62G7.    IF    K19F-CPRSC2 = '000000006'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62G7-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62G7-FN. EXIT.
      *N62G9.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62G9.    IF    K19F-CPRSC2 = '000000003'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62G9-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62G9-FN. EXIT.
       F62G1-FN. EXIT.
       F62GF-FN. EXIT.
      *N62HG.    NOTE *'FROM' ACCOUNT CLASS B             *.
       F62HG.    IF    K19F-PRSCD = '000000002'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62HG-FN.
      *N62HH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62HH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62HH-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62HH-FN. EXIT.
      *N62HI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62HI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62HI-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62HI-FN. EXIT.
      *N62HJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62HJ.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62HJ-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62HJ-FN. EXIT.
      *N62HL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62HL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62HL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62HL-FN. EXIT.
       F62HG-FN. EXIT.
      *N62IG.    NOTE *'FROM' ACCOUNT CLASS C             *.
       F62IG.    IF    K19F-PRSCD = '000000006'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62IG-FN.
      *N62IH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62IH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62IH-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62IH-FN. EXIT.
      *N62II.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62II.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62II-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62II-FN. EXIT.
      *N62IJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62IJ.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62IJ-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62IJ-FN. EXIT.
      *N62IL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62IL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62IL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62IL-FN. EXIT.
       F62IG-FN. EXIT.
      *N62JG.    NOTE *'FROM' ACCOUNT CLASS Y             *.
       F62JG.    IF    K19F-PRSCD = '000000003'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62JG-FN.
      *N62JH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62JH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62JH-FN.
      *N62JI.    NOTE *'TO' ACCOUNT TAX-FREE MONEY        *.
       F62JI.    IF    K19F-PRCOD1 = 00016                              lv35
                 NEXT SENTENCE ELSE GO TO     F62JI-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62JI-900. GO TO F62JJ-FN.
       F62JI-FN. EXIT.
      *N62JJ.    NOTE *'TO' ACCOUNT ^ TAX-FREE MONEY      *.
       F62JJ.                                                           lv35
           MOVE        'N' TO K19F-CTRFA.
       F62JJ-FN. EXIT.
       F62JH-FN. EXIT.
      *N62JK.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62JK.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62JK-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62JK-FN. EXIT.
      *N62JL.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62JL.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62JL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62JL-FN. EXIT.
      *N62JP.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62JP.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62JP-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62JP-FN. EXIT.
       F62JG-FN. EXIT.
       F62GC-FN. EXIT.
       F62CC-900. GO TO F62KB-FN.
       F62CC-FN. EXIT.
      *N62KB.    NOTE *'TO' ACCOUNT ^= MUTUAL FUND        *.
       F62KB.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F62KB-FN. EXIT.
       F62BB-900. GO TO F62LA-FN.
       F62BB-FN. EXIT.
      *N62LA.    NOTE *NOT SCHEDULED ACTIVITY             *.
       F62LA.         EXIT.                                             lv10
      *N62LC.    NOTE *'TO' ACCOUNT = MUTUAL FUND         *.
       F62LC.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F62LC-FN.
      *N62MD.    NOTE *'FROM' FUND = 'TO' FUND            *.
       F62MD.    IF    K19F-PRCOD = K19F-PRCOD1                         lv20
                 NEXT SENTENCE ELSE GO TO     F62MD-FN.
      *N62MG.    NOTE *'FROM' ACCOUNT CLASS A             *.
       F62MG.    IF    K19F-PRSCD = '000000001'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62MG-FN.
      *N62MH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62MH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62MH-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62MH-FN. EXIT.
      *N62MI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62MI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62MI-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62MI-FN. EXIT.
      *N62MK.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62MK.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62MK-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62MK-FN. EXIT.
      *N62MP.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62MP.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62MP-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62MP-FN. EXIT.
       F62MG-FN. EXIT.
      *N62NG.    NOTE *'FROM' ACCOUNT CLASS B             *.
       F62NG.    IF    K19F-PRSCD = '000000002'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62NG-FN.
      *N62NH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62NH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NH-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62NH-FN. EXIT.
      *N62NI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62NI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NI-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62NI-FN. EXIT.
      *N62NJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62NJ.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NJ-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62NJ-FN. EXIT.
      *N62NL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62NL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62NL-FN. EXIT.
       F62NG-FN. EXIT.
      *N62NQ.    NOTE *'FROM' ACCOUNT CLASS C             *.
       F62NQ.    IF    K19F-PRSCD = '000000006'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62NQ-FN.
      *N62NR.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62NR.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NR-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62NR-FN. EXIT.
      *N62NS.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62NS.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NS-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62NS-FN. EXIT.
      *N62NT.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62NT.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NT-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62NT-FN. EXIT.
      *N62NU.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62NU.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62NU-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62NU-FN. EXIT.
       F62NQ-FN. EXIT.
      *N62OG.    NOTE *'FROM' ACCOUNT CLASS Y             *.
       F62OG.    IF    K19F-PRSCD = '000000003'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62OG-FN.
      *N62OH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62OH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62OH-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62OH-FN. EXIT.
      *N62OI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62OI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62OI-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62OI-FN. EXIT.
      *N62OJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62OJ.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62OJ-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62OJ-FN. EXIT.
      *N62OL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62OL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62OL-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62OL-FN. EXIT.
       F62OG-FN. EXIT.
       F62MD-900. GO TO F62PC-FN.
       F62MD-FN. EXIT.
      *N62PC.    NOTE *'FROM' FUND ^= 'TO' FUND           *.
       F62PC.         EXIT.                                             lv20
      *N62PF.    NOTE *'FROM' ACCOUNT CLASS A             *.
       F62PF.    IF    K19F-PRSCD = '000000001'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62PF-FN.
      *N62PG.    NOTE *'FROM' ACCOUNT MONEY MARKET        *.
       F62PG.    IF    K19F-PRCOD = 00013                               lv30
                 OR    K19F-PRCOD = 00167
                 NEXT SENTENCE ELSE GO TO     F62PG-FN.
      *OR CLM MONEY MARKET FUND
      *N62PH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62PH.    IF    K19F-CPRSC2 = '000000001'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62PH-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62PH-FN. EXIT.
      *N62PI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62PI.    IF    K19F-CPRSC2 = '000000002'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62PI-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62PI-FN. EXIT.
      *N62PJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62PJ.    IF    K19F-CPRSC2 = '000000006'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62PJ-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62PJ-FN. EXIT.
      *N62PL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62PL.    IF    K19F-CPRSC2 = '000000003'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62PL-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62PL-FN. EXIT.
       F62PG-FN. EXIT.
      *N62PM.    NOTE *'FROM' ACCOUNT TAX-FREE MONEY      *.
       F62PM.    IF    K19F-PRCOD = 00016                               lv30
                 NEXT SENTENCE ELSE GO TO     F62PM-FN.
      *N62PN.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62PN.    IF    K19F-CPRSC2 = '000000001'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62PN-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62PN-FN. EXIT.
      *N62PO.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62PO.    IF    K19F-CPRSC2 = '000000002'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62PO-FN.
      *N62PP.    NOTE *'TO' MONEY MARKET ACCOUNT          *.
       F62PP.    IF    K19F-PRCOD1 = 00013                              lv40
                 OR    K19F-PRCOD1 = 00167
                 NEXT SENTENCE ELSE GO TO     F62PP-FN.
      *OR CLM MONEY MARKET FUND
           MOVE        'N' TO K19F-CTRFA.
       F62PP-900. GO TO F62PQ-FN.
       F62PP-FN. EXIT.
      *N62PQ.    NOTE *'TO' ACCOUNT ^= MONEY MARKET       *.
       F62PQ.                                                           lv40
           MOVE        'Y' TO K19F-CTRFA.
       F62PQ-FN. EXIT.
       F62PO-FN. EXIT.
      *N62PT.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62PT.    IF    K19F-CPRSC2 = '000000006'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62PT-FN.
      *N62PU.    NOTE *'TO' MONEY MARKET ACCOUNT          *.
       F62PU.    IF    K19F-PRCOD1 = 00013                              lv40
                 OR    K19F-PRCOD1 = 00167
                 NEXT SENTENCE ELSE GO TO     F62PU-FN.
      *OR GOVT MONEY MARKET FUND
           MOVE        'N' TO K19F-CTRFA.
       F62PU-900. GO TO F62PV-FN.
       F62PU-FN. EXIT.
      *N62PV.    NOTE *'TO' ACCOUNT ^= MONEY MARKET       *.
       F62PV.                                                           lv40
           MOVE        'Y' TO K19F-CTRFA.
       F62PV-FN. EXIT.
       F62PT-FN. EXIT.
      *N62P1.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62P1.    IF    K19F-CPRSC2 = '000000003'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62P1-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62P1-FN. EXIT.
       F62PM-FN. EXIT.
      *N62P2.    NOTE *'FROM' ACCOUNT NOT MMK OR TFM      *.
       F62P2.    IF    K19F-PRCOD NOT = 00013                           lv30
                 AND   NOT = 00016
                 AND   NOT = 00167
                 NEXT SENTENCE ELSE GO TO     F62P2-FN.
      *N62P3.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62P3.    IF    K19F-CPRSC2 = '000000001'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62P3-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62P3-FN. EXIT.
      *N62P4.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62P4.    IF    K19F-CPRSC2 = '000000002'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62P4-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62P4-FN. EXIT.
      *N62P5.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62P5.    IF    K19F-CPRSC2 = '000000006'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62P5-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62P5-FN. EXIT.
      *N62P6.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62P6.    IF    K19F-CPRSC2 = '000000003'                        lv35
                 NEXT SENTENCE ELSE GO TO     F62P6-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62P6-FN. EXIT.
       F62P2-FN. EXIT.
       F62PF-FN. EXIT.
      *N62QG.    NOTE *'FROM' ACCOUNT CLASS B             *.
       F62QG.    IF    K19F-PRSCD = '000000002'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62QG-FN.
      *N62QH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62QH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62QH-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62QH-FN. EXIT.
      *N62QI.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62QI.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62QI-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62QI-FN. EXIT.
      *N62QJ.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62QJ.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62QJ-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62QJ-FN. EXIT.
      *N62QL.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62QL.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62QL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62QL-FN. EXIT.
       F62QG-FN. EXIT.
      *N62RG.    NOTE *'FROM' ACCOUNT CLASS Y             *.
       F62RG.    IF    K19F-PRSCD = '000000003'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62RG-FN.
      *N62RH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62RH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62RH-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62RH-FN. EXIT.
      *N62RK.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62RK.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62RK-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62RK-FN. EXIT.
      *N62RL.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62RL.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62RL-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62RL-FN. EXIT.
      *N62RN.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62RN.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62RN-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62RN-FN. EXIT.
       F62RG-FN. EXIT.
      *N62SG.    NOTE *'FROM' ACCOUNT CLASS C             *.
       F62SG.    IF    K19F-PRSCD = '000000006'                         lv25
                 NEXT SENTENCE ELSE GO TO     F62SG-FN.
      *N62SH.    NOTE *'TO' ACCOUNT CLASS A               *.
       F62SH.    IF    K19F-CPRSC2 = '000000001'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62SH-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62SH-FN. EXIT.
      *N62SK.    NOTE *'TO' ACCOUNT CLASS B               *.
       F62SK.    IF    K19F-CPRSC2 = '000000002'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62SK-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62SK-FN. EXIT.
      *N62SL.    NOTE *'TO' ACCOUNT CLASS C               *.
       F62SL.    IF    K19F-CPRSC2 = '000000006'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62SL-FN.
           MOVE        'Y' TO K19F-CTRFA.
       F62SL-FN. EXIT.
      *N62SN.    NOTE *'TO' ACCOUNT CLASS Y               *.
       F62SN.    IF    K19F-CPRSC2 = '000000003'                        lv30
                 NEXT SENTENCE ELSE GO TO     F62SN-FN.
           MOVE        'N' TO K19F-CTRFA.
       F62SN-FN. EXIT.
       F62SG-FN. EXIT.
       F62PC-FN. EXIT.
       F62LC-900. GO TO F62TU-FN.
       F62LC-FN. EXIT.
      *N62TU.    NOTE *'TO' ACCOUNT ^= MUTUAL FUND        *.
       F62TU.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F62TU-FN. EXIT.
       F62LA-FN. EXIT.
       F62-FN.   EXIT.
      *N63.      NOTE *************************************.
      *               *                                   *
      *               *'FROM' ACCOUNT = BROKERAG          *
      *               *                                   *
      *               *************************************.
       F63.      IF    K19F-CTIDA = 021                                 lv05
                 NEXT SENTENCE ELSE GO TO     F63-FN.
      *N63BB.    NOTE *SCHEDULE ACTIVITY                  *.
       F63BB.    IF    K19F-ISCHA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F63BB-FN.
      *N63CC.    NOTE *'TO' ACCOUNT = FUND                *.
       F63CC.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F63CC-FN.
      *N63CD.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F63CD.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F63CD-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F63CD-900. GO TO F63CE-FN.
       F63CD-FN. EXIT.
      *N63CE.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F63CE.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F63CE-FN. EXIT.
       F63CC-900. GO TO F63DD-FN.
       F63CC-FN. EXIT.
      *N63DD.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F63DD.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F63DD-FN. EXIT.
       F63BB-900. GO TO F63EE-FN.
       F63BB-FN. EXIT.
      *N63EE.    NOTE *NOT SCHEDULE ACTIVITY              *.
       F63EE.         EXIT.                                             lv10
      *N63FF.    NOTE *'TO' ACCOUNT = FUND                *.
       F63FF.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F63FF-FN.
      *N63GD.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F63GD.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F63GD-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F63GD-900. GO TO F63GE-FN.
       F63GD-FN. EXIT.
      *N63GE.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F63GE.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F63GE-FN. EXIT.
       F63FF-900. GO TO F63HD-FN.
       F63FF-FN. EXIT.
      *N63HD.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F63HD.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F63HD-FN. EXIT.
       F63EE-FN. EXIT.
       F63-FN.   EXIT.
      *N64.      NOTE *************************************.
      *               *                                   *
      *               *'FROM' ACCOUNT = LIFE              *
      *               *                                   *
      *               *************************************.
       F64.      IF    K19F-CTIDA = 004                                 lv05
                 NEXT SENTENCE ELSE GO TO     F64-FN.
      *N64BB.    NOTE *SCHEDULE ACTIVITY                  *.
       F64BB.    IF    K19F-ISCHA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F64BB-FN.
      *N64CC.    NOTE *'TO' ACCOUNT = FUND                *.
       F64CC.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F64CC-FN.
      *N64DB.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F64DB.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F64DB-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F64DB-900. GO TO F64DC-FN.
       F64DB-FN. EXIT.
      *N64DC.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F64DC.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F64DC-FN. EXIT.
       F64CC-900. GO TO F64FB-FN.
       F64CC-FN. EXIT.
      *N64FB.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F64FB.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F64FB-FN. EXIT.
       F64BB-900. GO TO F64GB-FN.
       F64BB-FN. EXIT.
      *N64GB.    NOTE *NOT SCHEDULE ACTIVITY              *.
       F64GB.         EXIT.                                             lv10
      *N64HB.    NOTE *'TO' ACCOUNT = FUND                *.
       F64HB.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F64HB-FN.
      *N64IB.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F64IB.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F64IB-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F64IB-900. GO TO F64IC-FN.
       F64IB-FN. EXIT.
      *N64IC.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F64IC.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F64IC-FN. EXIT.
       F64HB-900. GO TO F64JB-FN.
       F64HB-FN. EXIT.
      *N64JB.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F64JB.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F64JB-FN. EXIT.
       F64GB-FN. EXIT.
       F64-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *'FROM' ACCOUNT = LIFE OF NY        *
      *               *                                   *
      *               *************************************.
       F65.      IF    K19F-CTIDA = 005                                 lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *N65BB.    NOTE *SCHEDULE ACTIVITY                  *.
       F65BB.    IF    K19F-ISCHA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F65BB-FN.
      *N65CC.    NOTE *'TO' ACCOUNT = FUND                *.
       F65CC.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F65CC-FN.
      *N65DB.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F65DB.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F65DB-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F65DB-900. GO TO F65DC-FN.
       F65DB-FN. EXIT.
      *N65DC.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F65DC.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F65DC-FN. EXIT.
       F65CC-900. GO TO F65FB-FN.
       F65CC-FN. EXIT.
      *N65FB.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F65FB.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F65FB-FN. EXIT.
       F65BB-900. GO TO F65GB-FN.
       F65BB-FN. EXIT.
      *N65GB.    NOTE *NOT SCHEDULE ACTIVITY              *.
       F65GB.         EXIT.                                             lv10
      *N65HB.    NOTE *'TO' ACCOUNT = FUND                *.
       F65HB.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F65HB-FN.
      *N65IB.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F65IB.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F65IB-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F65IB-900. GO TO F65IC-FN.
       F65IB-FN. EXIT.
      *N65IC.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F65IC.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F65IC-FN. EXIT.
       F65HB-900. GO TO F65JB-FN.
       F65HB-FN. EXIT.
      *N65JB.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F65JB.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F65JB-FN. EXIT.
       F65GB-FN. EXIT.
       F65-FN.   EXIT.
      *N66.      NOTE *************************************.
      *               *                                   *
      *               *'FROM' ACCOUNT = OTHER             *
      *               *                                   *
      *               *************************************.
       F66.      IF    K19F-CTIDA NOT = 001                             lv05
                 AND   K19F-CTIDA NOT = 002
                 AND   K19F-CTIDA NOT = 004
                 AND   K19F-CTIDA NOT = 005
                 AND   K19F-CTIDA NOT = 021
                 AND   K19F-CTIDA NOT = 133
                 NEXT SENTENCE ELSE GO TO     F66-FN.
      *N66BA.    NOTE *SCHEDULE ACTIVITY                  *.
       F66BA.    IF    K19F-ISCHA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F66BA-FN.
      *N66BC.    NOTE *'TO' MMK/RCM CLASS B OR CLASS C    *.
       F66BC.    IF    K19F-CTIDA1 = 002                                lv15
                 AND   (K19F-PRCOD1 = 00013
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F66BC-FN.
           MOVE        'N' TO K19F-CTRFA.
       F66BC-900. GO TO F66BD-FN.
       F66BC-FN. EXIT.
      *N66BD.    NOTE *TO ACCOUNT NOT MMK CLASS B FUND    *.
       F66BD.                                                           lv15
           MOVE        'Y' TO K19F-CTRFA.
       F66BD-FN. EXIT.
       F66BA-900. GO TO F66CA-FN.
       F66BA-FN. EXIT.
      *N66CA.    NOTE *NOT SCHEDULE ACTIVITY              *.
       F66CA.                                                           lv10
           MOVE        'Y' TO K19F-CTRFA.
       F66CA-FN. EXIT.
       F66-FN.   EXIT.
      *N67.      NOTE *************************************.
      *               *                                   *
      *               *'FROM' ACCOUNT = BETA              *
      *               *                                   *
      *               *************************************.
       F67.      IF    K19F-CTIDA = 133                                 lv05
                 NEXT SENTENCE ELSE GO TO     F67-FN.
      *N67BB.    NOTE *SCHEDULE ACTIVITY                  *.
       F67BB.    IF    K19F-ISCHA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F67BB-FN.
      *N67CC.    NOTE *'TO' ACCOUNT = FUND                *.
       F67CC.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F67CC-FN.
      *N67CD.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F67CD.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F67CD-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F67CD-900. GO TO F67CE-FN.
       F67CD-FN. EXIT.
      *N67CE.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F67CE.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F67CE-FN. EXIT.
       F67CC-900. GO TO F67DD-FN.
       F67CC-FN. EXIT.
      *N67DD.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F67DD.                                                           lv15
                 IF    K19F-CTIDA1 = 001                                DOT
                 OR    K19F-CTIDA1 = 004
                 OR    K19F-CTIDA1 = 005
                 OR    K19F-CTIDA1 = 013
                 OR    K19F-CTIDA1 = 021
      *PERMIT ONLY FOR CERTS/LIF/LNY/
      *BROKERAGE/FP
           MOVE        'Y' TO K19F-CTRFA.
       F67DD-FN. EXIT.
       F67BB-900. GO TO F67EE-FN.
       F67BB-FN. EXIT.
      *N67EE.    NOTE *NOT SCHEDULE ACTIVITY              *.
       F67EE.         EXIT.                                             lv10
      *N67FF.    NOTE *'TO' ACCOUNT = FUND                *.
       F67FF.    IF    K19F-CTIDA1 = 002                                lv15
                 NEXT SENTENCE ELSE GO TO     F67FF-FN.
      *N67GD.    NOTE *'TO' MMK CLASS B OR CLASS C        *.
       F67GD.    IF    (K19F-PRCOD1 = 00013                             lv20
                 OR    K19F-PRCOD1 = 00167)
                 AND   (K19F-CPRSC2 = '000000002'
                 OR    = '000000006')
                 NEXT SENTENCE ELSE GO TO     F67GD-FN.
      *OR RCM CLASS B OR CLASS C
           MOVE        'N' TO K19F-CTRFA.
       F67GD-900. GO TO F67GE-FN.
       F67GD-FN. EXIT.
      *N67GE.    NOTE *'TO' OTHER MUTUAL FUND             *.
       F67GE.                                                           lv20
           MOVE        'Y' TO K19F-CTRFA.
       F67GE-FN. EXIT.
       F67FF-900. GO TO F67HD-FN.
       F67FF-FN. EXIT.
      *N67HD.    NOTE *'TO' ACCOUNT NOT = FUND            *.
       F67HD.                                                           lv15
                 IF    K19F-CTIDA1 = 001                                DOT
                 OR    K19F-CTIDA1 = 004
                 OR    K19F-CTIDA1 = 005
                 OR    K19F-CTIDA1 = 013
                 OR    K19F-CTIDA1 = 021
      *PERMIT ONLY FOR CERTS/LIF/LNY/
      *BROKERAGE/FP
           MOVE        'Y' TO K19F-CTRFA.
       F67HD-FN. EXIT.
       F67EE-FN. EXIT.
       F67-FN.   EXIT.
      *N9098.    NOTE *SET "GOBACK" INDICATOR             *.
       F9098.                                                           lv10
      *THIS WILL MAKE SURE ONE PASS OF
      *PROGRAM AND THEN GET OUT
           MOVE ALL    '1' TO FT.
       F9098-FN. EXIT.
       F9099-ITER-FN.  GO TO F05.
