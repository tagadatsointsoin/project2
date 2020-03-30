       IDENTIFICATION DIVISION.                                         CAT200
       PROGRAM-ID.  CAT200.                                             CAT200
      *AUTHOR.         UD TRANSFER/DBL TRANSFER RULES.    gp GP         CAT200
      *DATE-COMPILED.   09/08/14.                                       CAT200
      *************** GP * GP *********************** GP ***************ACOPYP
      *DATE-COMPILED.   09/08/14. GP                                    CAT200
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1999                          *ACOPYP
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
      *     COPR. 1999                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CAT200
       CONFIGURATION SECTION.                                           CAT200
       SOURCE-COMPUTER. IBM-370.                                        CAT200
       OBJECT-COMPUTER. IBM-370.                                        CAT200
       DATA DIVISION.                                                   CAT200
       WORKING-STORAGE SECTION.                                         CAT200
       01   DEBUT-WSS.                                                  CAT200
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CAT200
            05   IK     PICTURE X.                                      CAT200
       01  CONSTANTES-PAC.                                              CAT200
           05  FILLER  PICTURE X(87)   VALUE                            CAT200
                     '9999 CAT09/08/14CAT200ADMIN   19:28:29CAT200  BVAPCAT200
      -    '09/08/20143.5 V0419/02/201425/02/2014'.                     CAT200
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CAT200
           05  NUGNA   PICTURE X(5).                                    CAT200
           05  APPLI   PICTURE X(3).                                    CAT200
           05  DATGN   PICTURE X(8).                                    CAT200
           05  PROGR   PICTURE X(6).                                    CAT200
           05  CODUTI  PICTURE X(8).                                    CAT200
           05  TIMGN   PICTURE X(8).                                    CAT200
           05  PROGE   PICTURE X(8).                                    CAT200
           05  COBASE  PICTURE X(4).                                    CAT200
           05  DATGNC  PICTURE X(10).                                   CAT200
           05  RELEAS  PICTURE X(7).                                    CAT200
           05  DATGE   PICTURE X(10).                                   CAT200
           05  DATSQ   PICTURE X(10).                                   CAT200
       01  DATCE.                                                       CAT200
         05  CENTUR   PICTURE XX   VALUE '20'.                          CAT200
         05  DATOR.                                                     CAT200
           10  DATOA  PICTURE XX.                                       CAT200
           10  DATOM  PICTURE XX.                                       CAT200
           10  DATOJ  PICTURE XX.                                       CAT200
       01   VARIABLES-CONDITIONNELLES.                                  CAT200
            05                  FT      PICTURE X VALUE '0'.            CAT200
       01   INDICES  COMPUTATIONAL  SYNC.                               CAT200
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CAT200
       01   ZONES-UTILISATEUR PICTURE X.                                CAT200
       LINKAGE SECTION.                                                 AUSING
      *!WF DSP=KR DSL=KR SEL=9A FOR=I LEV=1                             AUSING
       01                 KR00.                                         CAT200
          05              KR00-SUITE.                                   CAT200
            15       FILLER         PICTURE  X(00080).                  CAT200
       01                 KR9A  REDEFINES      KR00.                    CAT200
            10            KR9A-MPLAN  PICTURE  X(12).                   CAT200
            10            KR9A-MPLANA PICTURE  X(12).                   CAT200
            10            KR9A-CLCUS  PICTURE  99.                      CAT200
            10            KR9A-CIRAP  PICTURE  XX.                      CAT200
            10            KR9A-CPMTCA PICTURE  XXX.                     CAT200
            10            KR9A-CTRFAA PICTURE  X.                       CAT200
            10            KR9A-FILLER PICTURE  X(48).                   CAT200
       PROCEDURE DIVISION USING KR9A.
      *N01.      NOTE *************************************.            CAT200
      *               *                                   *             CAT200
      *               *INITIALISATIONS                    *             CAT200
      *               *                                   *             CAT200
      *               *************************************.            CAT200
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.
      *               *                                   *
      *               *RESET "GOBACK" INDICATOR           *
      *               *                                   *
      *               *************************************.
       F02.                                                             lv05
      *THIS WILL MAKE SURE THE "FT"
      *INDICATOR IS SET TO ZERO WHEN
      *THIS MODULE CALLED MORE THAN
      *ONCE.
           MOVE ALL    '0' TO FT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CAT200
       F05.           EXIT.
      *N20.      NOTE *************************************.            CAT200
      *               *                                   *             CAT200
      *               *FIN DE TRAITEMENT                  *             CAT200
      *               *                                   *             CAT200
      *               *************************************.            CAT200
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CAT200
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
                 IF    KR9A-CLCUS NOT NUMERIC                           DOT
           MOVE        ZERO TO KR9A-CLCUS.
       F50BA-FN. EXIT.
      *N50FA.    NOTE *SET RETURN FIELD DEFAULT 'N'       *.
       F50FA.                                                           lv10
           MOVE        'N' TO KR9A-CTRFAA.
       F50FA-FN. EXIT.
       F50-FN.   EXIT.
      *N51.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - IRA ACTIVE        *
      *               *                                   *
      *               *************************************.
       F51.      IF    KR9A-MPLAN = 'IRA ACTIVE'                        lv05
                 NEXT SENTENCE ELSE GO TO     F51-FN.
      *N51BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F51BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F51BA-FN.
      *N51BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51BC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15 OR ZERO
                 NEXT SENTENCE ELSE GO TO     F51BC-FN.
      *N51BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F51BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51BE-FN. EXIT.
       F51BC-FN. EXIT.
      *N51BH.    NOTE *DEATH SETTLEMENT CODE              *.
       F51BH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51BH-FN.
      *N51BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51BJ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51BJ-FN. EXIT.
       F51BH-FN. EXIT.
      *N51BN.    NOTE *NON SETTLEMENT CODE                *.
       F51BN.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51BN-FN.
      *N51BP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51BP.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51BP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51BP-FN. EXIT.
       F51BN-FN. EXIT.
      *N51BT.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F51BT.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F51BT-FN.
      *PREMATURE DISTRIBUTION
      *N51BV.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51BV.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F51BV-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51BV-FN. EXIT.
       F51BT-FN. EXIT.
       F51BA-FN. EXIT.
      *N51CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F51CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F51CA-FN.
      *N51CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51CC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F51CC-FN.
      *N51CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F51CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51CE-FN. EXIT.
       F51CC-FN. EXIT.
      *N51CH.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F51CH.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F51CH-FN.
      *PREMATURE DISTRIBUTION
      *N51CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51CJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F51CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51CJ-FN. EXIT.
       F51CH-FN. EXIT.
      *N51CP.    NOTE *NON SETTLEMENT CODE                *.
       F51CP.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51CP-FN.
      *N51CR.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51CR.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51CR-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F51CR-FN. EXIT.
       F51CP-FN. EXIT.
      *N51CT.    NOTE *DEATH SETTLEMENT CODE              *.
       F51CT.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51CT-FN.
      *N51CU.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51CU.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51CU-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51CU-FN. EXIT.
       F51CT-FN. EXIT.
       F51CA-FN. EXIT.
      *N51DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F51DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F51DA-FN.
      *N51DC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51DC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F51DC-FN.
      *N51DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51DE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51DE-FN. EXIT.
       F51DC-FN. EXIT.
       F51DA-FN. EXIT.
      *N51EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F51EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F51EA-FN.
      *N51EC.    NOTE *NON SETTLEMENT CODE                *.
       F51EC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51EC-FN.
      *N51EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51EE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51EE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F51EE-FN. EXIT.
       F51EC-FN. EXIT.
      *N51EH.    NOTE *DEATH SETTLEMENT CODE              *.
       F51EH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51EH-FN.
      *N51EJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51EJ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51EJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51EJ-FN. EXIT.
       F51EH-FN. EXIT.
      *N51EK.    NOTE *RETIREMENT SETTLEMENT CODE OR      *.
       F51EK.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F51EK-FN.
      *PREMATURE DISTRIBUTION
      *N51EL.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51EL.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F51EL-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51EL-FN. EXIT.
       F51EK-FN. EXIT.
       F51EA-FN. EXIT.
      *N51FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F51FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F51FA-FN.
      *N51FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51FC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F51FC-FN.
      *N51FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51FE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51FE-FN. EXIT.
       F51FC-FN. EXIT.
      *N51FH.    NOTE *RECHARACTERIZATION SETTLEMENT CD   *.
       F51FH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F51FH-FN.
      *N51FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51FJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51FJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51FJ-FN. EXIT.
       F51FH-FN. EXIT.
      *N51FN.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F51FN.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F51FN-FN.
      *PREMATURE DISTRIBUTION
      *N51FP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51FP.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F51FP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51FP-FN. EXIT.
       F51FN-FN. EXIT.
      *N51FT.    NOTE *ROTH PRE/RET SETTLEMENT CODE       *.
       F51FT.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F51FT-FN.
      *N51FZ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51FZ.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51FZ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51FZ-FN. EXIT.
       F51FT-FN. EXIT.
       F51FA-FN. EXIT.
      *N51GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F51GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F51GA-FN.
      *N51GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51GC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F51GC-FN.
      *N51GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51GE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51GE-FN. EXIT.
       F51GC-FN. EXIT.
      *N51GH.    NOTE *RECHARACTERIZATION SETTLEMENT CD   *.
       F51GH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F51GH-FN.
      *N51GJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51GJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51GJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51GJ-FN. EXIT.
       F51GH-FN. EXIT.
      *N51GN.    NOTE *ROTH PRE/RET SETTLEMENT CODES      *.
       F51GN.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F51GN-FN.
      *N51GP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51GP.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51GP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51GP-FN. EXIT.
       F51GN-FN. EXIT.
      *N51GT.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F51GT.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F51GT-FN.
      *PREMATURE DISTRIBUTION
      *N51GV.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51GV.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F51GV-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51GV-FN. EXIT.
       F51GT-FN. EXIT.
       F51GA-FN. EXIT.
      *N51HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F51HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F51HA-FN.
      *N51HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51HC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F51HC-FN.
      *N51HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F51HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51HE-FN. EXIT.
       F51HC-FN. EXIT.
      *N51HH.    NOTE *RETIRE SETTLEMENT CODE             *.
       F51HH.    IF    KR9A-CLCUS = 07                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51HH-FN.
      *N51HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51HJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F51HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51HJ-FN. EXIT.
       F51HH-FN. EXIT.
       F51HA-FN. EXIT.
      *N51IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F51IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F51IA-FN.
      *N51IC.    NOTE *NON SETTLEMENT CODE                *.
       F51IC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51IC-FN.
      *N51IE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51IE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F51IE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51IE-FN. EXIT.
       F51IC-FN. EXIT.
      *N51IJ.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51IJ.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F51IJ-FN.
      *N51IL.    NOTE *IRA CONTRIBUTION TYPE              *.
       F51IL.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC' OR 'IT'
                 NEXT SENTENCE ELSE GO TO     F51IL-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51IL-FN. EXIT.
       F51IJ-FN. EXIT.
       F51IA-FN. EXIT.
      *N51JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F51JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F51JA-FN.
      *NOT ALLOWED
       F51JA-FN. EXIT.
      *N51KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F51KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F51KA-FN.
      *NOT ALLOWED
       F51KA-FN. EXIT.
      *N51LA.    NOTE *TO PLAN TYPE - TRUSTEED OR         *.
       F51LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F51LA-FN.
      *CUSTODIAL OR GOV 457
      *N51LC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51LC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15 OR 10
                 NEXT SENTENCE ELSE GO TO     F51LC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51LC-FN. EXIT.
       F51LA-FN. EXIT.
      *N51MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F51MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F51MA-FN.
      *N51MC.    NOTE *PREMATURE OR RETIRE SETL CODE      *.
       F51MC.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F51MC-FN.
      *N51ME.    NOTE *LOAN PAYMENT TYPE                  *.
       F51ME.    IF    KR9A-CPMTCA = 'LON'                              lv20
                 NEXT SENTENCE ELSE GO TO     F51ME-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51ME-FN. EXIT.
       F51MC-FN. EXIT.
      *N51MK.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F51MK.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51MK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51MK-FN. EXIT.
       F51MA-FN. EXIT.
      *N51NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F51NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F51NA-FN.
      *N51NG.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F51NG.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51NG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51NG-FN. EXIT.
       F51NA-FN. EXIT.
      *N51OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F51OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F51OA-FN.
      *N51OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F51OC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F51OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51OC-FN. EXIT.
      *N51OH.    NOTE *PREMATURE OR RETIRE SETTL CODE     *.
       F51OH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F51OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F51OH-FN. EXIT.
      *N51OK.    NOTE *PREMATURE EXCEPTION SETTL CODE     *.
       F51OK.    IF    KR9A-CLCUS = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51OK-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F51OK-FN. EXIT.
      *N51OP.    NOTE *DISABILITY OR DEATH SETTL CODE     *.
       F51OP.    IF    KR9A-CLCUS = 03 OR 04                            lv15
                 NEXT SENTENCE ELSE GO TO     F51OP-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F51OP-FN. EXIT.
      *N51OT.    NOTE *NON SETTLEMENT CODE                *.
       F51OT.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F51OT-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F51OT-FN. EXIT.
       F51OA-FN. EXIT.
       F51-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - IRA ROLLOVER      *
      *               *                                   *
      *               *************************************.
       F52.      IF    KR9A-MPLAN = 'IRA ROLLOVER'                      lv05
                 NEXT SENTENCE ELSE GO TO     F52-FN.
      *N52BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F52BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F52BA-FN.
      *N52BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52BC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F52BC-FN.
      *N52BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F52BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52BE-FN. EXIT.
       F52BC-FN. EXIT.
      *N52BH.    NOTE *NON SETTLEMENT CODE                *.
       F52BH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52BH-FN.
      *N52BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52BJ.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52BJ-FN. EXIT.
       F52BH-FN. EXIT.
      *N52BN.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F52BN.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F52BN-FN.
      *PREMATURE DISTRIBUTION
      *N52BP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52BP.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F52BP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52BP-FN. EXIT.
       F52BN-FN. EXIT.
      *N52BT.    NOTE *DEATH SETTLEMENT CODE              *.
       F52BT.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52BT-FN.
      *N52BU.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52BU.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52BU-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52BU-FN. EXIT.
       F52BT-FN. EXIT.
       F52BA-FN. EXIT.
      *N52CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F52CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F52CA-FN.
      *N52CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52CC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F52CC-FN.
      *N52CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F52CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52CE-FN. EXIT.
       F52CC-FN. EXIT.
      *N52CH.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F52CH.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F52CH-FN.
      *PREMATURE DISTRIBUTION
      *N52CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52CJ.    IF    KR9A-CIRAP = 'SC' OR 'CU'                        lv20
                       OR 'PR'
                 NEXT SENTENCE ELSE GO TO     F52CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52CJ-FN. EXIT.
       F52CH-FN. EXIT.
      *N52CP.    NOTE *DEATH SETTLEMENT CODE              *.
       F52CP.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52CP-FN.
      *N52CR.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52CR.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52CR-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52CR-FN. EXIT.
       F52CP-FN. EXIT.
       F52CA-FN. EXIT.
      *N52DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F52DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F52DA-FN.
      *N52DC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52DC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F52DC-FN.
      *N52DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52DE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52DE-FN. EXIT.
       F52DC-FN. EXIT.
       F52DA-FN. EXIT.
      *N52EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F52EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F52EA-FN.
      *N52EC.    NOTE *DEATH SETTLEMENT CODE              *.
       F52EC.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52EC-FN.
      *N52EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52EE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52EE-FN. EXIT.
       F52EC-FN. EXIT.
      *N52EH.    NOTE *NON SETTLEMENT CODE                *.
       F52EH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52EH-FN.
      *N52EJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52EJ.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52EJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52EJ-FN. EXIT.
       F52EH-FN. EXIT.
       F52EA-FN. EXIT.
      *N52FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F52FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F52FA-FN.
      *N52FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52FC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F52FC-FN.
      *N52FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52FE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52FE-FN. EXIT.
       F52FC-FN. EXIT.
      *N52FH.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F52FH.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F52FH-FN.
      *PREMATURE DISTRIBUTION
      *N52FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52FJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F52FJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52FJ-FN. EXIT.
       F52FH-FN. EXIT.
      *N52FN.    NOTE *RECHARACTERIZATION SETTLEMENT CD   *.
       F52FN.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F52FN-FN.
      *N52FP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52FP.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52FP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52FP-FN. EXIT.
       F52FN-FN. EXIT.
      *N52FS.    NOTE *ROTH PRE/RET SETTLEMENT CODE       *.
       F52FS.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F52FS-FN.
      *N52FW.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52FW.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52FW-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52FW-FN. EXIT.
       F52FS-FN. EXIT.
       F52FA-FN. EXIT.
      *N52GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F52GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F52GA-FN.
      *N52GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52GC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F52GC-FN.
      *N52GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52GE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52GE-FN. EXIT.
       F52GC-FN. EXIT.
      *N52GH.    NOTE *RECHARACTERIZATION SETTLEMENT CD   *.
       F52GH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F52GH-FN.
      *N52GJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52GJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52GJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52GJ-FN. EXIT.
       F52GH-FN. EXIT.
      *N52GN.    NOTE *ROTH PRE/RET SETTLEMENT CODES      *.
       F52GN.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F52GN-FN.
      *N52GP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52GP.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52GP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52GP-FN. EXIT.
       F52GN-FN. EXIT.
      *N52GT.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F52GT.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F52GT-FN.
      *PREMATURE DISTRIBUTION
      *N52GV.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52GV.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F52GV-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52GV-FN. EXIT.
       F52GT-FN. EXIT.
       F52GA-FN. EXIT.
      *N52HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F52HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F52HA-FN.
      *N52HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52HC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F52HC-FN.
      *N52HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F52HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52HE-FN. EXIT.
       F52HC-FN. EXIT.
      *N52HH.    NOTE *RETIREMENT SETTLEMENT,PREMATURE    *.
       F52HH.    IF    KR9A-CLCUS = 07 OR 01 OR 03                      lv15
                       OR 02
                 NEXT SENTENCE ELSE GO TO     F52HH-FN.
      *DISTRIBUTION,DISABILITY OR PRX
      *N52HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52HJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F52HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52HJ-FN. EXIT.
       F52HH-FN. EXIT.
       F52HA-FN. EXIT.
      *N52IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F52IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F52IA-FN.
      *N52IC.    NOTE *NON SETTLEMENT CODE                *.
       F52IC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52IC-FN.
      *N52IE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F52IE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F52IE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52IE-FN. EXIT.
       F52IC-FN. EXIT.
       F52IA-FN. EXIT.
      *N52JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F52JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F52JA-FN.
      *NOT ALLOWED
       F52JA-FN. EXIT.
      *N52KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F52KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F52KA-FN.
      *NOT ALLOWED
       F52KA-FN. EXIT.
      *N52LA.    NOTE *TO PLAN TYPE - TRUSTEED OR         *.
       F52LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F52LA-FN.
      *GOV 457 OR CUSTODIAL
      *N52LC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52LC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15 OR 10
                 NEXT SENTENCE ELSE GO TO     F52LC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52LC-FN. EXIT.
      *N52LG.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F52LG.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52LG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52LG-FN. EXIT.
       F52LA-FN. EXIT.
      *N52MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F52MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F52MA-FN.
      *N52MG.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F52MG.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52MG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52MG-FN. EXIT.
      *N52MK.    NOTE *PREMATURE OR RETIRE SETL CODE      *.
       F52MK.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F52MK-FN.
      *N52MP.    NOTE *LOAN PAYMENT TYPE                  *.
       F52MP.    IF    KR9A-CPMTCA = 'LON'                              lv20
                 NEXT SENTENCE ELSE GO TO     F52MP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52MP-FN. EXIT.
       F52MK-FN. EXIT.
       F52MA-FN. EXIT.
      *N52NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F52NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F52NA-FN.
      *N52NG.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F52NG.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52NG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52NG-FN. EXIT.
       F52NA-FN. EXIT.
      *N52OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F52OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F52OA-FN.
      *N52OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F52OC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F52OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52OC-FN. EXIT.
      *N52OH.    NOTE *RETIRE OR PREMATURE SETTL CODE     *.
       F52OH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F52OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F52OH-FN. EXIT.
      *N52OK.    NOTE *PREMATURE EXCEPTION SETTL CODE     *.
       F52OK.    IF    KR9A-CLCUS = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52OK-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F52OK-FN. EXIT.
      *N52OP.    NOTE *DISABILITY OR DEATH SETTL CODE     *.
       F52OP.    IF    KR9A-CLCUS = 03 OR 04                            lv15
                 NEXT SENTENCE ELSE GO TO     F52OP-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F52OP-FN. EXIT.
      *N52OT.    NOTE *NON SETTLEMENT CODE                *.
       F52OT.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F52OT-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F52OT-FN. EXIT.
       F52OA-FN. EXIT.
       F52-FN.   EXIT.
      *N53.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - SEP IRA           *
      *               *                                   *
      *               *************************************.
       F53.      IF    KR9A-MPLAN = 'SEP'                               lv05
                 NEXT SENTENCE ELSE GO TO     F53-FN.
      *N53BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F53BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F53BA-FN.
      *N53BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53BC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53BC-FN.
      *N53BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F53BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53BE-FN. EXIT.
       F53BC-FN. EXIT.
      *N53BH.    NOTE *NON SETTLEMENT CODE                *.
       F53BH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53BH-FN.
      *N53BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53BJ.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53BJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F53BJ-FN. EXIT.
       F53BH-FN. EXIT.
      *N53BN.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F53BN.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F53BN-FN.
      *PREMATURE DISTRIBUTION
      *N53BP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53BP.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F53BP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53BP-FN. EXIT.
       F53BN-FN. EXIT.
      *N53BT.    NOTE *DEATH SETTLEMENT CODE              *.
       F53BT.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53BT-FN.
      *N53BU.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53BU.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53BU-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53BU-FN. EXIT.
       F53BT-FN. EXIT.
       F53BA-FN. EXIT.
      *N53CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F53CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F53CA-FN.
      *N53CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53CC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53CC-FN.
      *N53CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F53CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53CE-FN. EXIT.
       F53CC-FN. EXIT.
      *N53CH.    NOTE *RETIRE SETTLEMENT CODE             *.
       F53CH.    IF    KR9A-CLCUS = 07                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53CH-FN.
      *N53CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53CJ.    IF    KR9A-CIRAP = 'SC'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53CJ-FN. EXIT.
       F53CH-FN. EXIT.
      *N53CN.    NOTE *DEATH SETTLEMENT CODE              *.
       F53CN.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53CN-FN.
      *N53CP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53CP.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53CP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53CP-FN. EXIT.
       F53CN-FN. EXIT.
      *N53CR.    NOTE *NON SETTLEMENT CODE                *.
       F53CR.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53CR-FN.
      *N53CT.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53CT.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53CT-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F53CT-FN. EXIT.
       F53CR-FN. EXIT.
       F53CA-FN. EXIT.
      *N53DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F53DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F53DA-FN.
      *N53DC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53DC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53DC-FN.
      *N53DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53DE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53DE-FN. EXIT.
       F53DC-FN. EXIT.
       F53DA-FN. EXIT.
      *N53EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F53EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F53EA-FN.
      *N53EC.    NOTE *NON SETTLEMENT CODE                *.
       F53EC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53EC-FN.
      *N53EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53EE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53EE-FN. EXIT.
       F53EC-FN. EXIT.
      *N53EH.    NOTE *DEATH SETTLEMENT CODE              *.
       F53EH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53EH-FN.
      *N53EJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53EJ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53EJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53EJ-FN. EXIT.
       F53EH-FN. EXIT.
      *N53EL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53EL.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53EL-FN.
      *N53EM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53EM.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F53EM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53EM-FN. EXIT.
       F53EL-FN. EXIT.
      *N53EO.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F53EO.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F53EO-FN.
      *PREMATURE DISTRIBUTION
      *N53EP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53EP.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F53EP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53EP-FN. EXIT.
       F53EO-FN. EXIT.
       F53EA-FN. EXIT.
      *N53FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F53FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F53FA-FN.
      *N53FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53FC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53FC-FN.
      *N53FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F53FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53FE-FN. EXIT.
       F53FC-FN. EXIT.
      *N53FH.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F53FH.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F53FH-FN.
      *PREMATURE DISTRIBUTION
      *N53FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53FJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F53FJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53FJ-FN. EXIT.
       F53FH-FN. EXIT.
      *N53FP.    NOTE *RECHARACTERIZATION SETTLEMENT CD   *.
       F53FP.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F53FP-FN.
      *N53FR.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53FR.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53FR-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53FR-FN. EXIT.
       F53FP-FN. EXIT.
      *N53FT.    NOTE *ROTH PRE/RET SETTLEMENT CODE       *.
       F53FT.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F53FT-FN.
      *N53FZ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53FZ.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53FZ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53FZ-FN. EXIT.
       F53FT-FN. EXIT.
       F53FA-FN. EXIT.
      *N53GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F53GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F53GA-FN.
      *N53GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53GC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53GC-FN.
      *N53GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F53GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53GE-FN. EXIT.
       F53GC-FN. EXIT.
      *N53GN.    NOTE *ROTH PRE/RET SETTLEMENT CODES      *.
       F53GN.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F53GN-FN.
      *N53GP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53GP.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53GP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53GP-FN. EXIT.
       F53GN-FN. EXIT.
      *N53GT.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F53GT.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F53GT-FN.
      *PREMATURE DISTRIBUTION
      *N53GV.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53GV.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F53GV-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53GV-FN. EXIT.
       F53GT-FN. EXIT.
      *N53GX.    NOTE *RECHARACTERIZATION SETTL CODE      *.
       F53GX.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F53GX-FN.
      *N53GZ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53GZ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53GZ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53GZ-FN. EXIT.
       F53GX-FN. EXIT.
       F53GA-FN. EXIT.
      *N53HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F53HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F53HA-FN.
      *N53HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53HC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53HC-FN.
      *N53HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F53HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53HE-FN. EXIT.
       F53HC-FN. EXIT.
      *N53HH.    NOTE *RETIRE SETTLEMENT CODE             *.
       F53HH.    IF    KR9A-CLCUS = 07                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53HH-FN.
      *N53HK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53HK.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F53HK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53HK-FN. EXIT.
       F53HH-FN. EXIT.
       F53HA-FN. EXIT.
      *N53IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F53IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F53IA-FN.
      *N53IC.    NOTE *NON SETTLEMENT CODE                *.
       F53IC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53IC-FN.
      *N53IE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F53IE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F53IE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53IE-FN. EXIT.
       F53IC-FN. EXIT.
       F53IA-FN. EXIT.
      *N53JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F53JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F53JA-FN.
      *NOT ALLOWED
       F53JA-FN. EXIT.
      *N53KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F53KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F53KA-FN.
      *NOT ALLOWED
       F53KA-FN. EXIT.
      *N53LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F53LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F53LA-FN.
      *OR GOV 457 OR CUSTODIAL
      *N53LC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53LC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15 OR 10
                 NEXT SENTENCE ELSE GO TO     F53LC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53LC-FN. EXIT.
       F53LA-FN. EXIT.
      *N53MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F53MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F53MA-FN.
      *N53MC.    NOTE *PREMATURE OR RETIRE SETTL CODE     *.
       F53MC.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F53MC-FN.
      *N53MG.    NOTE *LOAN PAYMENT TYPE                  *.
       F53MG.    IF    KR9A-CPMTCA = 'LON'                              lv20
                 NEXT SENTENCE ELSE GO TO     F53MG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53MG-FN. EXIT.
       F53MC-FN. EXIT.
      *N53MK.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F53MK.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53MK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53MK-FN. EXIT.
       F53MA-FN. EXIT.
      *N53NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F53NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F53NA-FN.
      *N53NG.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F53NG.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53NG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53NG-FN. EXIT.
       F53NA-FN. EXIT.
      *N53OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F53OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F53OA-FN.
      *N53OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F53OC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F53OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53OC-FN. EXIT.
      *N53OH.    NOTE *RETIRE OR PREMATURE SETTL CODE     *.
       F53OH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F53OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F53OH-FN. EXIT.
      *N53OK.    NOTE *PREMATURE EXCEPT SETTLEMENT CODE   *.
       F53OK.    IF    KR9A-CLCUS = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53OK-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F53OK-FN. EXIT.
      *N53OP.    NOTE *DISABILITY OR DEATH SETTL CODE     *.
       F53OP.    IF    KR9A-CLCUS = 03 OR 04                            lv15
                 NEXT SENTENCE ELSE GO TO     F53OP-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F53OP-FN. EXIT.
      *N53OT.    NOTE *NON SETTLEMENT CODE                *.
       F53OT.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F53OT-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F53OT-FN. EXIT.
       F53OA-FN. EXIT.
       F53-FN.   EXIT.
      *N54.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - SRA               *
      *               *                                   *
      *               *************************************.
       F54.      IF    KR9A-MPLAN = 'SRA'                               lv05
                 NEXT SENTENCE ELSE GO TO     F54-FN.
      *N54BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F54BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F54BA-FN.
      *N54BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54BC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54BC-FN.
      *N54BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F54BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54BE-FN. EXIT.
       F54BC-FN. EXIT.
      *N54BH.    NOTE *PREMATURE SETTLEMENT CODE          *.
       F54BH.    IF    KR9A-CLCUS = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54BH-FN.
      *N54BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F54BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54BJ-FN. EXIT.
      *N54BK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BK.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54BK-FN.
           MOVE        'N' TO KR9A-CTRFAA.
       F54BK-FN. EXIT.
       F54BH-FN. EXIT.
      *N54BN.    NOTE *RETIRE SETTLEMENT CODE             *.
       F54BN.    IF    KR9A-CLCUS = 07                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54BN-FN.
      *N54BP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BP.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F54BP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54BP-FN. EXIT.
      *N54BQ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BQ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54BQ-FN.
           MOVE        'N' TO KR9A-CTRFAA.
       F54BQ-FN. EXIT.
       F54BN-FN. EXIT.
      *N54BR.    NOTE *NON-TAX REPORTABLE CODE            *.
       F54BR.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54BR-FN.
      *N54BS.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BS.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54BS-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54BS-FN. EXIT.
       F54BR-FN. EXIT.
      *N54BT.    NOTE *SIMPLE SETTLEMENT CODE             *.
       F54BT.    IF    KR9A-CLCUS = 18                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54BT-FN.
      *N54BU.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BU.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54BU-FN.
           MOVE        'N' TO KR9A-CTRFAA.
       F54BU-FN. EXIT.
      *N54BV.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BV.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F54BV-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54BV-FN. EXIT.
       F54BT-FN. EXIT.
      *N54BX.    NOTE *DEATH SETTLEMENT CODE              *.
       F54BX.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54BX-FN.
      *N54BY.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54BY.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54BY-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54BY-FN. EXIT.
       F54BX-FN. EXIT.
       F54BA-FN. EXIT.
      *N54CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F54CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F54CA-FN.
      *N54CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54CC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54CC-FN.
      *N54CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54CE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F54CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54CE-FN. EXIT.
       F54CC-FN. EXIT.
      *N54CH.    NOTE *DEATH SETTLEMENT                   *.
       F54CH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54CH-FN.
      *N54CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54CJ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54CJ-FN. EXIT.
       F54CH-FN. EXIT.
      *N54CL.    NOTE *NON-TAX REPORTABLE CODE            *.
       F54CL.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54CL-FN.
      *N54CM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54CM.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54CM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54CM-FN. EXIT.
       F54CL-FN. EXIT.
      *N54CO.    NOTE *SIMPLE DISTRI, PREMATURE OR        *.
       F54CO.    IF    KR9A-CLCUS = 18 OR 01                            lv15
                       OR 07
                 NEXT SENTENCE ELSE GO TO     F54CO-FN.
      *RETIREMENT DISTRIBUTION CODE
      *N54CP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54CP.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54CP-FN.
           MOVE        'N' TO KR9A-CTRFAA.
       F54CP-FN. EXIT.
      *N54CQ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54CQ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F54CQ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54CQ-FN. EXIT.
       F54CO-FN. EXIT.
       F54CA-FN. EXIT.
      *N54DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F54DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F54DA-FN.
      *N54DC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54DC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54DC-FN.
      *N54DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54DE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54DE-FN. EXIT.
       F54DC-FN. EXIT.
      *N54DH.    NOTE *DEATH SETTLEMENT CODE              *.
       F54DH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54DH-FN.
      *N54DJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54DJ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54DJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54DJ-FN. EXIT.
       F54DH-FN. EXIT.
      *N54DP.    NOTE *NON SETTLEMENT CODE                *.
       F54DP.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54DP-FN.
      *N54DS.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54DS.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54DS-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F54DS-FN. EXIT.
       F54DP-FN. EXIT.
       F54DA-FN. EXIT.
      *N54EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F54EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F54EA-FN.
      *N54EC.    NOTE *DEATH SETTLEMENT CODE              *.
       F54EC.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54EC-FN.
      *N54EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54EE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54EE-FN. EXIT.
       F54EC-FN. EXIT.
      *N54EG.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54EG.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54EG-FN.
      *N54EH.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54EH.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F54EH-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54EH-FN. EXIT.
       F54EG-FN. EXIT.
      *N54EJ.    NOTE *NON-TAX REPORTABLE CODE            *.
       F54EJ.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54EJ-FN.
      *N54EK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54EK.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54EK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54EK-FN. EXIT.
       F54EJ-FN. EXIT.
       F54EA-FN. EXIT.
      *N54FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F54FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F54FA-FN.
      *N54FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54FC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54FC-FN.
      *N54FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F54FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54FE-FN. EXIT.
       F54FC-FN. EXIT.
      *N54FH.    NOTE *RETIRE SETTLEMENT CODE             *.
       F54FH.    IF    KR9A-CLCUS = 07                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54FH-FN.
      *N54FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54FJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F54FJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54FJ-FN. EXIT.
       F54FH-FN. EXIT.
      *N54FT.    NOTE *ROTH PRE/RET SETTLEMENT CODE       *.
       F54FT.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F54FT-FN.
      *N54FZ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54FZ.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54FZ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54FZ-FN. EXIT.
       F54FT-FN. EXIT.
       F54FA-FN. EXIT.
      *N54GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F54GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F54GA-FN.
      *N54GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54GC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54GC-FN.
      *N54GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F54GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54GE-FN. EXIT.
       F54GC-FN. EXIT.
      *N54GN.    NOTE *ROTH PRE/RET SETTLEMENT CODES      *.
       F54GN.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F54GN-FN.
      *N54GP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54GP.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54GP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54GP-FN. EXIT.
       F54GN-FN. EXIT.
      *N54GT.    NOTE *RETIRE SETTLEMENT CODE             *.
       F54GT.    IF    KR9A-CLCUS = 07                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54GT-FN.
      *N54GV.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54GV.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54GV-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54GV-FN. EXIT.
       F54GT-FN. EXIT.
       F54GA-FN. EXIT.
      *N54HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F54HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F54HA-FN.
      *N54HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54HC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54HC-FN.
      *N54HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54HE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54HE-FN. EXIT.
       F54HC-FN. EXIT.
      *N54HH.    NOTE *RETIRE SETTLEMENT CODE             *.
       F54HH.    IF    KR9A-CLCUS = 07                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54HH-FN.
      *N54HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54HJ.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54HJ-FN. EXIT.
       F54HH-FN. EXIT.
       F54HA-FN. EXIT.
      *N54IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F54IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F54IA-FN.
      *N54IC.    NOTE *NON SETTLEMENT CODE                *.
       F54IC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54IC-FN.
      *N54IE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F54IE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F54IE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54IE-FN. EXIT.
       F54IC-FN. EXIT.
       F54IA-FN. EXIT.
      *N54JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F54JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F54JA-FN.
      *NOT ALLOWED
       F54JA-FN. EXIT.
      *N54KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F54KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F54KA-FN.
      *NOT ALLOWED
       F54KA-FN. EXIT.
      *N54LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F54LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F54LA-FN.
      *GOV 457 OR CUSTODIAL
      *N54LC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54LC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15 OR 10
                 NEXT SENTENCE ELSE GO TO     F54LC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54LC-FN. EXIT.
       F54LA-FN. EXIT.
      *N54MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F54MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F54MA-FN.
      *N54MK.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F54MK.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54MK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54MK-FN. EXIT.
       F54MA-FN. EXIT.
      *N54NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F54NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F54NA-FN.
      *N54NG.    NOTE *DIRECT ROLLOVER TO OTHER           *.
       F54NG.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54NG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54NG-FN. EXIT.
       F54NA-FN. EXIT.
      *N54OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F54OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F54OA-FN.
      *N54OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F54OC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F54OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54OC-FN. EXIT.
      *N54OH.    NOTE *PREMATURE OR RETIRE SETTL CODE     *.
       F54OH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F54OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F54OH-FN. EXIT.
      *N54OK.    NOTE *NON SETTLEMENT CODE                *.
       F54OK.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54OK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F54OK-FN. EXIT.
      *N54OP.    NOTE *DISABILITY OR DEATH SETTL CODE     *.
       F54OP.    IF    KR9A-CLCUS = 03 OR 04                            lv15
                 NEXT SENTENCE ELSE GO TO     F54OP-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F54OP-FN. EXIT.
      *N54OT.    NOTE *PREMATURE EXCEPTION SETL CODE      *.
       F54OT.    IF    KR9A-CLCUS = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F54OT-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F54OT-FN. EXIT.
       F54OA-FN. EXIT.
       F54-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - ROTH IRA          *
      *               *                                   *
      *               *************************************.
       F55.      IF    KR9A-MPLAN = 'ROTH IRA'                          lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F55BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F55BA-FN.
      *N55BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55BC.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F55BC-FN.
      *N55BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F55BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55BE-FN. EXIT.
       F55BC-FN. EXIT.
      *N55BH.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F55BH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F55BH-FN.
      *N55BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55BJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55BJ-FN. EXIT.
       F55BH-FN. EXIT.
      *N55BL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55BL.    IF    KR9A-CLCUS = 27 OR 28 OR                         lv15
                       21 OR 22
                 NEXT SENTENCE ELSE GO TO     F55BL-FN.
      *N55BM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55BM.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F55BM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55BM-FN. EXIT.
       F55BL-FN. EXIT.
       F55BA-FN. EXIT.
      *N55CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F55CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F55CA-FN.
      *N55CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55CC.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F55CC-FN.
      *N55CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F55CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55CE-FN. EXIT.
       F55CC-FN. EXIT.
      *N55CH.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F55CH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F55CH-FN.
      *N55CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55CJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55CJ-FN. EXIT.
       F55CH-FN. EXIT.
      *N55CL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55CL.    IF    KR9A-CLCUS = 27 OR 28 OR                         lv15
                       21 OR 22
                 NEXT SENTENCE ELSE GO TO     F55CL-FN.
      *N55CM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55CM.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                       OR 'PR'
                 NEXT SENTENCE ELSE GO TO     F55CM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55CM-FN. EXIT.
       F55CL-FN. EXIT.
       F55CA-FN. EXIT.
      *N55DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F55DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F55DA-FN.
      *N55DC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55DC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F55DC-FN.
      *N55DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55DE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55DE-FN. EXIT.
       F55DC-FN. EXIT.
       F55DA-FN. EXIT.
      *N55EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F55EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F55EA-FN.
      *N55EC.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F55EC.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F55EC-FN.
      *N55EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55EE.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55EE-FN. EXIT.
       F55EC-FN. EXIT.
      *N55EG.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55EG.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F55EG-FN.
      *
      *N55EI.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55EI.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F55EI-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55EI-FN. EXIT.
       F55EG-FN. EXIT.
      *N55EL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55EL.    IF    KR9A-CLCUS = 27 OR 28 OR                         lv15
                       21 OR 22
                 NEXT SENTENCE ELSE GO TO     F55EL-FN.
      *N55EM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55EM.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F55EM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55EM-FN. EXIT.
       F55EL-FN. EXIT.
       F55EA-FN. EXIT.
      *N55FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F55FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F55FA-FN.
      *N55FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55FC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F55FC-FN.
      *N55FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F55FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55FE-FN. EXIT.
       F55FC-FN. EXIT.
      *N55FH.    NOTE *ROTH DEATH SETTLEMENT CODE         *.
       F55FH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F55FH-FN.
      *N55FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55FJ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55FJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55FJ-FN. EXIT.
       F55FH-FN. EXIT.
      *N55FN.    NOTE *NON SETTLEMENT CODE                *.
       F55FN.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55FN-FN.
      *N55FP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55FP.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55FP-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F55FP-FN. EXIT.
       F55FN-FN. EXIT.
       F55FA-FN. EXIT.
      *N55GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F55GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.
      *N55GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55GC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F55GC-FN.
      *N55GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F55GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55GE-FN. EXIT.
       F55GC-FN. EXIT.
      *N55GN.    NOTE *NON SETTLEMENT CODES               *.
       F55GN.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55GN-FN.
      *N55GP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55GP.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55GP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55GP-FN. EXIT.
       F55GN-FN. EXIT.
      *N55GR.    NOTE *ROTH DEATH SETTLEMENT CODE         *.
       F55GR.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F55GR-FN.
      *N55GT.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55GT.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55GT-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55GT-FN. EXIT.
       F55GR-FN. EXIT.
       F55GA-FN. EXIT.
      *N55HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F55HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F55HA-FN.
      *N55HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55HC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F55HC-FN.
      *N55HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F55HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55HE-FN. EXIT.
       F55HC-FN. EXIT.
      *N55HH.    NOTE *ROTH DIST SETTLEMENT CODE          *.
       F55HH.    IF    KR9A-CLCUS = 21 OR 22 OR 24                      lv15
                       OR 56 OR 57
                 NEXT SENTENCE ELSE GO TO     F55HH-FN.
      *N55HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55HJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F55HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55HJ-FN. EXIT.
       F55HH-FN. EXIT.
       F55HA-FN. EXIT.
      *N55IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F55IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F55IA-FN.
      *NOT ALLOWED
       F55IA-FN. EXIT.
      *N55JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F55JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F55JA-FN.
      *N55JC.    NOTE *NON SETTLEMENT CODE                *.
       F55JC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55JC-FN.
      *N55JE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F55JE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F55JE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55JE-FN. EXIT.
       F55JC-FN. EXIT.
       F55JA-FN. EXIT.
      *N55KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F55KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F55KA-FN.
      *NOT ALLOWED
       F55KA-FN. EXIT.
      *N55LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F55LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F55LA-FN.
      *OR GOV 457 OR CUSTODIAL
      *N55LC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55LC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F55LC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55LC-FN. EXIT.
       F55LA-FN. EXIT.
      *N55MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F55MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.
      *N55MC.    NOTE *ROTH PREMATURE OR RETIRE SET CD    *.
       F55MC.    IF    KR9A-CLCUS = 21 OR 22 OR 56                      lv15
                 NEXT SENTENCE ELSE GO TO     F55MC-FN.
      *N55MF.    NOTE *LOAN PAYMENT TYPE                  *.
       F55MF.    IF    KR9A-CPMTCA = 'LON'                              lv20
                 NEXT SENTENCE ELSE GO TO     F55MF-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55MF-FN. EXIT.
       F55MC-FN. EXIT.
       F55MA-FN. EXIT.
      *N55NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F55NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F55NA-FN.
      *NOT ALLOWED
       F55NA-FN. EXIT.
      *N55OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F55OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F55OA-FN.
      *N55OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F55OC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F55OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55OC-FN. EXIT.
      *N55OH.    NOTE *NON SETTLEMENT CODE                *.
       F55OH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55OH-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F55OH-FN. EXIT.
      *N55OL.    NOTE *ROTH DIST SETTLEMENT CODES         *.
       F55OL.    IF    KR9A-CLCUS = 21 OR 22 OR 23                      lv15
                       OR 24 OR 25 OR 56 OR 57 OR
                       58
                 NEXT SENTENCE ELSE GO TO     F55OL-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F55OL-FN. EXIT.
       F55OA-FN. EXIT.
       F55-FN.   EXIT.
      *N56.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - ROTH CONVERSION   *
      *               *                                   *
      *               *************************************.
       F56.      IF    KR9A-MPLAN = 'ROTH CONV'                         lv05
                 NEXT SENTENCE ELSE GO TO     F56-FN.
      *N56BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F56BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F56BA-FN.
      *N56BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56BC.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F56BC-FN.
      *
      *N56BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F56BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56BE-FN. EXIT.
       F56BC-FN. EXIT.
      *N56BH.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F56BH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F56BH-FN.
      *N56BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56BJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56BJ-FN. EXIT.
       F56BH-FN. EXIT.
      *N56BL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56BL.    IF    KR9A-CLCUS = 27 OR 28 OR                         lv15
                       21 OR 22
                 NEXT SENTENCE ELSE GO TO     F56BL-FN.
      *N56BM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56BM.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F56BM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56BM-FN. EXIT.
       F56BL-FN. EXIT.
       F56BA-FN. EXIT.
      *N56CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F56CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F56CA-FN.
      *N56CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56CC.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F56CC-FN.
      *
      *N56CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F56CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56CE-FN. EXIT.
       F56CC-FN. EXIT.
      *N56CH.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F56CH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F56CH-FN.
      *N56CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56CJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56CJ-FN. EXIT.
       F56CH-FN. EXIT.
      *N56CL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56CL.    IF    KR9A-CLCUS = 27 OR 28 OR 21                      lv15
                       OR 22
                 NEXT SENTENCE ELSE GO TO     F56CL-FN.
      *N56CM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56CM.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F56CM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56CM-FN. EXIT.
       F56CL-FN. EXIT.
       F56CA-FN. EXIT.
      *N56DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F56DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F56DA-FN.
      *N56DC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56DC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F56DC-FN.
      *N56DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56DE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56DE-FN. EXIT.
       F56DC-FN. EXIT.
      *N56DH.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F56DH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F56DH-FN.
      *N56DJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56DJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56DJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56DJ-FN. EXIT.
       F56DH-FN. EXIT.
       F56DA-FN. EXIT.
      *N56EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F56EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F56EA-FN.
      *N56EH.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F56EH.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F56EH-FN.
      *N56EJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56EJ.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56EJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56EJ-FN. EXIT.
       F56EH-FN. EXIT.
      *N56EL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56EL.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F56EL-FN.
      *
      *N56EN.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56EN.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F56EN-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56EN-FN. EXIT.
       F56EL-FN. EXIT.
      *N56EP.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56EP.    IF    KR9A-CLCUS = 27 OR 28 OR 21                      lv15
                       OR 22
                 NEXT SENTENCE ELSE GO TO     F56EP-FN.
      *N56EQ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56EQ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F56EQ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56EQ-FN. EXIT.
       F56EP-FN. EXIT.
       F56EA-FN. EXIT.
      *N56FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F56FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F56FA-FN.
      *N56FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56FC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F56FC-FN.
      *N56FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F56FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56FE-FN. EXIT.
       F56FC-FN. EXIT.
      *N56FG.    NOTE *NON REPORTABLE SETTLEMENT CODE     *.
       F56FG.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F56FG-FN.
      *N56FI.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56FI.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56FI-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56FI-FN. EXIT.
       F56FG-FN. EXIT.
       F56FA-FN. EXIT.
      *N56GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F56GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F56GA-FN.
      *N56GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56GC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F56GC-FN.
      *N56GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F56GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56GE-FN. EXIT.
       F56GC-FN. EXIT.
      *N56GH.    NOTE *NON SETTLEMENT CODE                *.
       F56GH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F56GH-FN.
      *N56GJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56GJ.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56GJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F56GJ-FN. EXIT.
       F56GH-FN. EXIT.
      *N56GN.    NOTE *DEATH SETTLEMENT CODE              *.
       F56GN.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F56GN-FN.
      *N56GP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56GP.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56GP-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56GP-FN. EXIT.
       F56GN-FN. EXIT.
       F56GA-FN. EXIT.
      *N56HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F56HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F56HA-FN.
      *N56HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56HC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55 OR 56 OR 57
                 NEXT SENTENCE ELSE GO TO     F56HC-FN.
      *N56HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F56HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56HE-FN. EXIT.
       F56HC-FN. EXIT.
      *N56HH.    NOTE *ROTH DIST SETTLEMENT CODES         *.
       F56HH.    IF    KR9A-CLCUS = 21 OR 22 OR 24                      lv15
                 NEXT SENTENCE ELSE GO TO     F56HH-FN.
      *N56HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56HJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F56HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56HJ-FN. EXIT.
       F56HH-FN. EXIT.
       F56HA-FN. EXIT.
      *N56IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F56IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F56IA-FN.
      *NOT ALLOWED
       F56IA-FN. EXIT.
      *N56JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F56JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F56JA-FN.
      *NOT ALLOWED
       F56JA-FN. EXIT.
      *N56KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F56KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F56KA-FN.
      *N56KC.    NOTE *NON SETTLEMENT CODE                *.
       F56KC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F56KC-FN.
      *N56KE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56KE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F56KE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56KE-FN. EXIT.
       F56KC-FN. EXIT.
      *N56KH.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56KH.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F56KH-FN.
      *N56KL.    NOTE *IRA CONTRIBUTION TYPE              *.
       F56KL.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'IT'
                 NEXT SENTENCE ELSE GO TO     F56KL-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56KL-FN. EXIT.
       F56KH-FN. EXIT.
       F56KA-FN. EXIT.
      *N56LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F56LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F56LA-FN.
      *OR CUSTODIAL OR GOV 457
      *N56LC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56LC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F56LC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56LC-FN. EXIT.
       F56LA-FN. EXIT.
      *N56MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F56MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F56MA-FN.
      *N56MC.    NOTE *ROTH PREMATURE OR RETIRE SET CD    *.
       F56MC.    IF    KR9A-CLCUS = 21 OR 22 OR 56                      lv15
                 NEXT SENTENCE ELSE GO TO     F56MC-FN.
      *N56ME.    NOTE *LOAN PAYMENT TYPE                  *.
       F56ME.    IF    KR9A-CPMTCA = 'LON'                              lv20
                 NEXT SENTENCE ELSE GO TO     F56ME-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56ME-FN. EXIT.
       F56MC-FN. EXIT.
       F56MA-FN. EXIT.
      *N56NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F56NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F56NA-FN.
      *NOT ALLOWED
       F56NA-FN. EXIT.
      *N56OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F56OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F56OA-FN.
      *N56OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F56OC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F56OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56OC-FN. EXIT.
      *N56OF.    NOTE *NON SETTLEMENT CODE                *.
       F56OF.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F56OF-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F56OF-FN. EXIT.
      *N56OH.    NOTE *ROTH DIST SETTLEMENT CODES         *.
       F56OH.    IF    KR9A-CLCUS = 21 OR 22 OR 23                      lv15
                       OR 24 OR 25 OR 56 OR 57 OR
                       58
                 NEXT SENTENCE ELSE GO TO     F56OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F56OH-FN. EXIT.
       F56OA-FN. EXIT.
       F56-FN.   EXIT.
      *N57.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - EDUCATION IRA     *
      *               *                                   *
      *               *************************************.
       F57.      IF    KR9A-MPLAN = 'EDUC IRA'                          lv05
                 NEXT SENTENCE ELSE GO TO     F57-FN.
      *N57BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F57BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F57BA-FN.
      *N57BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57BC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57BC-FN.
      *N57BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F57BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57BE-FN. EXIT.
       F57BC-FN. EXIT.
       F57BA-FN. EXIT.
      *N57CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F57CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F57CA-FN.
      *N57CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57CC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57CC-FN.
      *N57CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57CE.    IF    KR9A-CIRAP = 'SC'                                lv20
                 NEXT SENTENCE ELSE GO TO     F57CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57CE-FN. EXIT.
       F57CC-FN. EXIT.
       F57CA-FN. EXIT.
      *N57DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F57DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F57DA-FN.
      *N57DC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57DC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57DC-FN.
      *N57DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57DE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F57DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57DE-FN. EXIT.
       F57DC-FN. EXIT.
       F57DA-FN. EXIT.
      *N57EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F57EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F57EA-FN.
      *NOT ALLOWED
       F57EA-FN. EXIT.
      *N57FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F57FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F57FA-FN.
      *N57FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57FC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57FC-FN.
      *N57FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F57FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57FE-FN. EXIT.
       F57FC-FN. EXIT.
       F57FA-FN. EXIT.
      *N57GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F57GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F57GA-FN.
      *N57GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57GC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57GC-FN.
      *N57GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F57GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57GE-FN. EXIT.
       F57GC-FN. EXIT.
       F57GA-FN. EXIT.
      *N57HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F57HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F57HA-FN.
      *N57HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57HC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57HC-FN.
      *N57HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57HE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F57HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57HE-FN. EXIT.
       F57HC-FN. EXIT.
      *N57HH.    NOTE *NON SETTLEMENT CODE                *.
       F57HH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F57HH-FN.
      *N57HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57HJ.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F57HJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F57HJ-FN. EXIT.
       F57HH-FN. EXIT.
      *N57HL.    NOTE *CESA, CK TO VALID FAMILY           *.
       F57HL.    IF    KR9A-CLCUS = 59                                  lv15
                 NEXT SENTENCE ELSE GO TO     F57HL-FN.
      *N57HM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57HM.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F57HM-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F57HM-FN. EXIT.
       F57HL-FN. EXIT.
      *N57HP.    NOTE *EDUCATION DEATH SETTLEMENT CODE    *.
       F57HP.    IF    KR9A-CLCUS = 45                                  lv15
                 NEXT SENTENCE ELSE GO TO     F57HP-FN.
      *N57HS.    NOTE *IRA CONTRIBUTION TYPE              *.
       F57HS.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F57HS-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57HS-FN. EXIT.
       F57HP-FN. EXIT.
       F57HA-FN. EXIT.
      *N57IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F57IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F57IA-FN.
      *NOT ALLOWED
       F57IA-FN. EXIT.
      *N57JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F57JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F57JA-FN.
      *NOT ALLOWED
       F57JA-FN. EXIT.
      *N57KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F57KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F57KA-FN.
      *NOT ALLOWED
       F57KA-FN. EXIT.
      *N57LA.    NOTE *TO PLAN TYPE - TRUSTEED OR         *.
       F57LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F57LA-FN.
      *GOV 457 OR CUSTODIAL
      *N57LC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57LC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57LC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57LC-FN. EXIT.
       F57LA-FN. EXIT.
      *N57MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F57MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F57MA-FN.
      *NOT ALLOWED
       F57MA-FN. EXIT.
      *N57NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F57NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F57NA-FN.
      *NOT ALLOWED
       F57NA-FN. EXIT.
      *N57OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F57OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F57OA-FN.
      *N57OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F57OC.    IF    KR9A-CLCUS = 47 OR 48                            lv15
                 NEXT SENTENCE ELSE GO TO     F57OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57OC-FN. EXIT.
      *N57OH.    NOTE *EDUCATION SETTLEMENT CODES         *.
       F57OH.    IF    KR9A-CLCUS = 43 OR 45                            lv15
                 NEXT SENTENCE ELSE GO TO     F57OH-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F57OH-FN. EXIT.
       F57OA-FN. EXIT.
       F57-FN.   EXIT.
      *N58.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - BENEFICIAL        *
      *               *                                   *
      *               *************************************.
       F58.      IF    KR9A-MPLAN = 'BENEFICIAL'                        lv05
                 NEXT SENTENCE ELSE GO TO     F58-FN.
      *N58BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F58BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F58BA-FN.
      *N58BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F58BC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F58BC-FN.
      *N58BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F58BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F58BE-FN. EXIT.
       F58BC-FN. EXIT.
      *N58BH.    NOTE *DEATH SETTLEMENT CODE              *.
       F58BH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58BH-FN.
      *N58BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58BJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F58BJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F58BJ-FN. EXIT.
       F58BH-FN. EXIT.
       F58BA-FN. EXIT.
      *N58CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F58CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F58CA-FN.
      *N58CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F58CC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F58CC-FN.
      *N58CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F58CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F58CE-FN. EXIT.
       F58CC-FN. EXIT.
      *N58CH.    NOTE *DEATH SETTLEMENT CODE              *.
       F58CH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58CH-FN.
      *N58CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58CJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F58CJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F58CJ-FN. EXIT.
       F58CH-FN. EXIT.
       F58CA-FN. EXIT.
      *N58DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F58DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F58DA-FN.
      *NOT ALLOWED
       F58DA-FN. EXIT.
      *N58EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F58EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F58EA-FN.
      *N58EH.    NOTE *DEATH SETTLEMENT CODE              *.
       F58EH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58EH-FN.
      *N58EJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58EJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F58EJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F58EJ-FN. EXIT.
       F58EH-FN. EXIT.
       F58EA-FN. EXIT.
      *N58FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F58FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F58FA-FN.
      *N58FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F58FC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F58FC-FN.
      *N58FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58FE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F58FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F58FE-FN. EXIT.
       F58FC-FN. EXIT.
      *N58FH.    NOTE *DEATH SETTLEMENT CODE              *.
       F58FH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58FH-FN.
      *N58FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58FJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F58FJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F58FJ-FN. EXIT.
       F58FH-FN. EXIT.
       F58FA-FN. EXIT.
      *N58GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F58GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F58GA-FN.
      *N58GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F58GC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F58GC-FN.
      *N58GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58GE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F58GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F58GE-FN. EXIT.
       F58GC-FN. EXIT.
      *N58GH.    NOTE *DEATH SETTLEMENT CODE              *.
       F58GH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58GH-FN.
      *N58GJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58GJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F58GJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F58GJ-FN. EXIT.
       F58GH-FN. EXIT.
       F58GA-FN. EXIT.
      *N58HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F58HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F58HA-FN.
      *N58HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F58HC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F58HC-FN.
      *N58HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F58HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F58HE-FN. EXIT.
       F58HC-FN. EXIT.
      *N58HH.    NOTE *DEATH SETTLEMENT CODE              *.
       F58HH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58HH-FN.
      *N58HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58HJ.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F58HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F58HJ-FN. EXIT.
       F58HH-FN. EXIT.
       F58HA-FN. EXIT.
      *N58IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F58IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F58IA-FN.
      *N58IC.    NOTE *NON SETTLEMENT CODE                *.
       F58IC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58IC-FN.
      *N58IE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F58IE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F58IE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F58IE-FN. EXIT.
       F58IC-FN. EXIT.
       F58IA-FN. EXIT.
      *N58JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F58JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F58JA-FN.
      *NOT ALLOWED
       F58JA-FN. EXIT.
      *N58KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F58KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F58KA-FN.
      *NOT ALLOWED
       F58KA-FN. EXIT.
      *N58LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F58LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F58LA-FN.
      *OR CUSTODIAL OR GOV 457
      *NOT ALLOWED
       F58LA-FN. EXIT.
      *N58MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F58MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F58MA-FN.
      *NOT ALLOWED
       F58MA-FN. EXIT.
      *N58NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F58NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F58NA-FN.
      *NOT ALLOWED
       F58NA-FN. EXIT.
      *N58OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F58OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F58OA-FN.
      *N58OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F58OC.    IF    KR9A-CLCUS = 08 OR 88 OR 87                      lv15
                       OR 14 OR 16 OR 15
                 NEXT SENTENCE ELSE GO TO     F58OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F58OC-FN. EXIT.
      *N58OH.    NOTE *DEATH SETTLEMENT CODE              *.
       F58OH.    IF    KR9A-CLCUS = 04                                  lv15
                 NEXT SENTENCE ELSE GO TO     F58OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F58OH-FN. EXIT.
       F58OA-FN. EXIT.
       F58-FN.   EXIT.
      *N59.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - ROTH BENEFICIAL   *
      *               *                                   *
      *               *************************************.
       F59.      IF    KR9A-MPLAN = 'ROTH BENF'                         lv05
                 NEXT SENTENCE ELSE GO TO     F59-FN.
      *N59BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F59BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F59BA-FN.
      *N59BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59BC.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F59BC-FN.
      *
      *N59BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F59BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59BE-FN. EXIT.
       F59BC-FN. EXIT.
      *N59BH.    NOTE *DEATH SETTLEMENT CODE              *.
       F59BH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F59BH-FN.
      *N59BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59BJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F59BJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F59BJ-FN. EXIT.
       F59BH-FN. EXIT.
      *N59BL.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59BL.    IF    KR9A-CLCUS = 27 OR 28                            lv15
                 NEXT SENTENCE ELSE GO TO     F59BL-FN.
      *
      *N59BM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59BM.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F59BM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59BM-FN. EXIT.
       F59BL-FN. EXIT.
       F59BA-FN. EXIT.
      *N59CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F59CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F59CA-FN.
      *N59CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59CC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F59CC-FN.
      *N59CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F59CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59CE-FN. EXIT.
       F59CC-FN. EXIT.
      *N59CH.    NOTE *DEATH SETTLEMENT CODE              *.
       F59CH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F59CH-FN.
      *N59CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59CJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F59CJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F59CJ-FN. EXIT.
       F59CH-FN. EXIT.
       F59CA-FN. EXIT.
      *N59DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F59DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F59DA-FN.
      *NOT ALLOWED
       F59DA-FN. EXIT.
      *N59EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F59EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F59EA-FN.
      *N59EC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59EC.    IF    KR9A-CLCUS = 54 OR 55                            lv15
                 NEXT SENTENCE ELSE GO TO     F59EC-FN.
      *
      *N59EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59EE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F59EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59EE-FN. EXIT.
       F59EC-FN. EXIT.
      *N59EG.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59EG.    IF    KR9A-CLCUS = 27 OR 28                            lv15
                 NEXT SENTENCE ELSE GO TO     F59EG-FN.
      *
      *N59EH.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59EH.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F59EH-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59EH-FN. EXIT.
       F59EG-FN. EXIT.
       F59EA-FN. EXIT.
      *N59FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F59FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F59FA-FN.
      *N59FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59FC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F59FC-FN.
      *N59FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59FE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F59FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59FE-FN. EXIT.
       F59FC-FN. EXIT.
      *N59FH.    NOTE *DEATH SETTLEMENT CODE              *.
       F59FH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F59FH-FN.
      *N59FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59FJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F59FJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F59FJ-FN. EXIT.
       F59FH-FN. EXIT.
       F59FA-FN. EXIT.
      *N59GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F59GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F59GA-FN.
      *N59GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59GC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F59GC-FN.
      *N59GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59GE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F59GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59GE-FN. EXIT.
       F59GC-FN. EXIT.
      *N59GH.    NOTE *DEATH SETTLEMENT CODE              *.
       F59GH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F59GH-FN.
      *N59GJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59GJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F59GJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F59GJ-FN. EXIT.
       F59GH-FN. EXIT.
       F59GA-FN. EXIT.
      *N59HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F59HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F59HA-FN.
      *N59HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59HC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F59HC-FN.
      *N59HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F59HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59HE-FN. EXIT.
       F59HC-FN. EXIT.
      *N59HH.    NOTE *DEATH SETTLEMENT CODE              *.
       F59HH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F59HH-FN.
      *N59HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59HJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F59HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59HJ-FN. EXIT.
       F59HH-FN. EXIT.
       F59HA-FN. EXIT.
      *N59IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F59IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F59IA-FN.
      *N59IC.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F59IC.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F59IC-FN.
      *N59IE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59IE.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F59IE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59IE-FN. EXIT.
       F59IC-FN. EXIT.
       F59IA-FN. EXIT.
      *N59JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F59JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F59JA-FN.
      *N59JC.    NOTE *NON SETTLEMENT CODE                *.
       F59JC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F59JC-FN.
      *N59JE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F59JE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F59JE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F59JE-FN. EXIT.
       F59JC-FN. EXIT.
       F59JA-FN. EXIT.
      *N59KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F59KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F59KA-FN.
      *NOT ALLOWED
       F59KA-FN. EXIT.
      *N59LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F59LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F59LA-FN.
      *CUSTODIAL OR GOV 457
      *NOT ALLOWED
       F59LA-FN. EXIT.
      *N59MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F59MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F59MA-FN.
      *NOT ALLOWED
       F59MA-FN. EXIT.
      *N59NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F59NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F59NA-FN.
      *NOT ALLOWED
       F59NA-FN. EXIT.
      *N59OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F59OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F59OA-FN.
      *N59OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F59OC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F59OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F59OC-FN. EXIT.
      *N59OH.    NOTE *DEATH SETTLEMENT CODE              *.
       F59OH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F59OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F59OH-FN. EXIT.
       F59OA-FN. EXIT.
       F59-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - ROTH CONV BENF    *
      *               *                                   *
      *               *************************************.
       F60.      IF    KR9A-MPLAN = 'ROTH CONVBEN'                      lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *N60BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F60BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F60BA-FN.
      *N60BC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F60BC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F60BC-FN.
      *N60BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F60BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60BE-FN. EXIT.
       F60BC-FN. EXIT.
      *N60BH.    NOTE *DEATH SETTLEMENT CODE              *.
       F60BH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F60BH-FN.
      *N60BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60BJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F60BJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F60BJ-FN. EXIT.
       F60BH-FN. EXIT.
       F60BA-FN. EXIT.
      *N60CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F60CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
      *N60CC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F60CC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F60CC-FN.
      *N60CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60CE.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F60CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60CE-FN. EXIT.
       F60CC-FN. EXIT.
      *N60CH.    NOTE *DEATH SETTLEMENT CODE              *.
       F60CH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F60CH-FN.
      *N60CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60CJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F60CJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F60CJ-FN. EXIT.
       F60CH-FN. EXIT.
       F60CA-FN. EXIT.
      *N60DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F60DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
      *NOT ALLOWED
       F60DA-FN. EXIT.
      *N60EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F60EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F60EA-FN.
      *
      *N60EC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F60EC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F60EC-FN.
      *N60EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60EE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F60EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60EE-FN. EXIT.
       F60EC-FN. EXIT.
       F60EA-FN. EXIT.
      *N60FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F60FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F60FA-FN.
      *N60FC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F60FC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F60FC-FN.
      *N60FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60FE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F60FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60FE-FN. EXIT.
       F60FC-FN. EXIT.
      *N60FH.    NOTE *DEATH SETTLEMENT CODE              *.
       F60FH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F60FH-FN.
      *N60FJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60FJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F60FJ-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F60FJ-FN. EXIT.
       F60FH-FN. EXIT.
       F60FA-FN. EXIT.
      *N60GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F60GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F60GA-FN.
      *N60GC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F60GC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F60GC-FN.
      *N60GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60GE.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F60GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60GE-FN. EXIT.
       F60GC-FN. EXIT.
      *N60GN.    NOTE *DEATH SETTLEMENT CODE              *.
       F60GN.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F60GN-FN.
      *N60GP.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60GP.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F60GP-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F60GP-FN. EXIT.
       F60GN-FN. EXIT.
       F60GA-FN. EXIT.
      *N60HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F60HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F60HA-FN.
      *N60HC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F60HC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F60HC-FN.
      *N60HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60HE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F60HE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60HE-FN. EXIT.
       F60HC-FN. EXIT.
      *N60HH.    NOTE *DEATH SETTLEMENT CODES             *.
       F60HH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F60HH-FN.
      *N60HJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60HJ.    IF    KR9A-CIRAP = 'CU'                                lv20
                 NEXT SENTENCE ELSE GO TO     F60HJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60HJ-FN. EXIT.
       F60HH-FN. EXIT.
       F60HA-FN. EXIT.
      *N60IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F60IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F60IA-FN.
      *N60IC.    NOTE *RECHARACTERIZATION SETL CODE       *.
       F60IC.    IF    KR9A-CLCUS = 50 OR 53                            lv15
                 NEXT SENTENCE ELSE GO TO     F60IC-FN.
      *N60IE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60IE.    IF    KR9A-CIRAP = 'RE'                                lv20
                 NEXT SENTENCE ELSE GO TO     F60IE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60IE-FN. EXIT.
       F60IC-FN. EXIT.
       F60IA-FN. EXIT.
      *N60JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F60JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F60JA-FN.
      *NOT ALLOWED
       F60JA-FN. EXIT.
      *N60KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F60KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F60KA-FN.
      *N60KC.    NOTE *NON SETTLEMENT CODE                *.
       F60KC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F60KC-FN.
      *N60KE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F60KE.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F60KE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F60KE-FN. EXIT.
       F60KC-FN. EXIT.
       F60KA-FN. EXIT.
      *N60LA.    NOTE *TO PLAN TYPE - TRUSTEED OR         *.
       F60LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F60LA-FN.
      *CUSTODIAL  OR GOV 457
      *NOT ALLOWED
       F60LA-FN. EXIT.
      *N60MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F60MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F60MA-FN.
      *NOT ALLOWED
       F60MA-FN. EXIT.
      *N60NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F60NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F60NA-FN.
      *NOT ALLOWED
       F60NA-FN. EXIT.
      *N60OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F60OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F60OA-FN.
      *N60OC.    NOTE *EXCESS SETTLEMENT CODE             *.
       F60OC.    IF    KR9A-CLCUS = 27 OR 28 OR 54                      lv15
                       OR 55
                 NEXT SENTENCE ELSE GO TO     F60OC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F60OC-FN. EXIT.
      *N60OH.    NOTE *DEATH SETTLEMENT CODES             *.
       F60OH.    IF    KR9A-CLCUS = 25 OR 58                            lv15
                 NEXT SENTENCE ELSE GO TO     F60OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F60OH-FN. EXIT.
       F60OA-FN. EXIT.
       F60-FN.   EXIT.
      *N61.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - PERSONAL          *
      *               *                                   *
      *               *************************************.
       F61.      IF    KR9A-MPLAN = 'PERSONAL'                          lv05
                 NEXT SENTENCE ELSE GO TO     F61-FN.
      *N61BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F61BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F61BA-FN.
      *N61BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F61BE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv15
                       OR 'RO' OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F61BE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61BE-FN. EXIT.
       F61BA-FN. EXIT.
      *N61CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F61CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F61CA-FN.
      *N61CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F61CE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv15
                       OR 'RO' OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F61CE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61CE-FN. EXIT.
       F61CA-FN. EXIT.
      *N61DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F61DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F61DA-FN.
      *N61DE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F61DE.    IF    KR9A-CIRAP = 'CU' OR 'RO'                        lv15
                 NEXT SENTENCE ELSE GO TO     F61DE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61DE-FN. EXIT.
       F61DA-FN. EXIT.
      *N61EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F61EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F61EA-FN.
      *N61EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F61EE.    IF    KR9A-CIRAP = 'RO' OR 'CU'                        lv15
                       OR 'PR' OR 'SC'
                 NEXT SENTENCE ELSE GO TO     F61EE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61EE-FN. EXIT.
       F61EA-FN. EXIT.
      *N61FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F61FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F61FA-FN.
      *N61FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F61FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv15
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F61FE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61FE-FN. EXIT.
       F61FA-FN. EXIT.
      *N61GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F61GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F61GA-FN.
      *N61GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F61GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv15
                       OR 'RO'
                 NEXT SENTENCE ELSE GO TO     F61GE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61GE-FN. EXIT.
       F61GA-FN. EXIT.
      *N61HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F61HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F61HA-FN.
      *N61HE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F61HE.    IF    KR9A-CIRAP = 'CU' OR 'RO'                        lv15
                       OR 'PR'
                 NEXT SENTENCE ELSE GO TO     F61HE-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61HE-FN. EXIT.
       F61HA-FN. EXIT.
      *N61IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F61IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F61IA-FN.
      *NOT ALLOWED
       F61IA-FN. EXIT.
      *N61JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F61JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F61JA-FN.
      *NOT ALLOWED
       F61JA-FN. EXIT.
      *N61KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F61KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F61KA-FN.
      *NOT ALLOWED
       F61KA-FN. EXIT.
      *N61LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F61LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = ' GOV 457'
                 NEXT SENTENCE ELSE GO TO     F61LA-FN.
      *CUSTODIAL OR GOV 457
           MOVE        'Y' TO KR9A-CTRFAA.
       F61LA-FN. EXIT.
      *N61MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F61MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F61MA-FN.
      *N61ME.    NOTE *PAYMENT TYPE                       *.
       F61ME.    IF    KR9A-CPMTCA = 'LON'                              lv15
                 NEXT SENTENCE ELSE GO TO     F61ME-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F61ME-FN. EXIT.
       F61MA-FN. EXIT.
      *N61NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F61NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F61NA-FN.
      *NOT ALLOWED
       F61NA-FN. EXIT.
      *N61OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F61OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F61OA-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F61OA-FN. EXIT.
       F61-FN.   EXIT.
      *N63.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - TSA               *
      *               *                                   *
      *               *************************************.
       F63.      IF    KR9A-MPLAN = 'TSA'                               lv05
                 NEXT SENTENCE ELSE GO TO     F63-FN.
      *N63BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F63BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F63BA-FN.
      *N63BC.    NOTE *DIRECT ROLLOVER SETTLEMENT CODES   *.
       F63BC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F63BC-FN.
      *N63BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63BE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63BE-FN. EXIT.
       F63BC-FN. EXIT.
      *N63BH.    NOTE *PREMATURE/RETIRE SETTLEMENT CODE   *.
       F63BH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F63BH-FN.
      *N63BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63BJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F63BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63BJ-FN. EXIT.
       F63BH-FN. EXIT.
       F63BA-FN. EXIT.
      *N63CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F63CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F63CA-FN.
      *N63CC.    NOTE *DIRECT ROLLOVER SETTLEMENT CODE    *.
       F63CC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F63CC-FN.
      *N63CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63CE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63CE-FN. EXIT.
       F63CC-FN. EXIT.
      *N63CH.    NOTE *PREMATURE/RETIRE SETTLEMENT CODE   *.
       F63CH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F63CH-FN.
      *N63CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63CJ.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F63CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63CJ-FN. EXIT.
       F63CH-FN. EXIT.
       F63CA-FN. EXIT.
      *N63DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F63DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F63DA-FN.
      *NOT ALLOWED
       F63DA-FN. EXIT.
      *N63EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F63EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F63EA-FN.
      *N63EC.    NOTE *DIRECT ROLLOVER SETTLEMENT CODE    *.
       F63EC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F63EC-FN.
      *N63EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63EE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63EE-FN. EXIT.
       F63EC-FN. EXIT.
       F63EA-FN. EXIT.
      *N63FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F63FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F63FA-FN.
      *N63FC.    NOTE *PREMATURE OR RETIRE SETTL CODE     *.
       F63FC.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F63FC-FN.
      *N63FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F63FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63FE-FN. EXIT.
       F63FC-FN. EXIT.
      *N63FI.    NOTE *NON SETTL CODE                     *.
       F63FI.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63FI-FN.
      *N63FK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63FK.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63FK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63FK-FN. EXIT.
       F63FI-FN. EXIT.
      *N63FO.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F63FO.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F63FO-FN.
      *N63FS.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63FS.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63FS-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63FS-FN. EXIT.
       F63FO-FN. EXIT.
       F63FA-FN. EXIT.
      *N63GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F63GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F63GA-FN.
      *N63GC.    NOTE *PREMATURE OR RETIRE SETTL CODE     *.
       F63GC.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F63GC-FN.
      *N63GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F63GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63GE-FN. EXIT.
       F63GC-FN. EXIT.
      *N63GI.    NOTE *NON SETTL CODE                     *.
       F63GI.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63GI-FN.
      *N63GK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63GK.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63GK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63GK-FN. EXIT.
       F63GI-FN. EXIT.
      *N63GO.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F63GO.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F63GO-FN.
      *N63GS.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63GS.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63GS-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63GS-FN. EXIT.
       F63GO-FN. EXIT.
       F63GA-FN. EXIT.
      *N63HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F63HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F63HA-FN.
      *NOT ALLOWED
       F63HA-FN. EXIT.
      *N63IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F63IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F63IA-FN.
      *N63IC.    NOTE *DEATH DIRECT ROLLOVER SETTL CODE   *.
       F63IC.    IF    KR9A-CLCUS = 13                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63IC-FN.
      *N63ID.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63ID.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63ID-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F63ID-FN. EXIT.
       F63IC-FN. EXIT.
      *N63IF.    NOTE *NON SETTLEMENT CODE                *.
       F63IF.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63IF-FN.
      *N63IG.    NOTE *IRA CONTRIBUTION TYPE              *.
       F63IG.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F63IG-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F63IG-FN. EXIT.
       F63IF-FN. EXIT.
       F63IA-FN. EXIT.
      *N63JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F63JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F63JA-FN.
      *NOT ALLOWED
       F63JA-FN. EXIT.
      *N63KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F63KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F63KA-FN.
      *NOT ALLOWED
       F63KA-FN. EXIT.
      *N63LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F63LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F63LA-FN.
      *OR CUSTODIAL OR GOV 457
      *NOT ALLOWED
       F63LA-FN. EXIT.
      *N63MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F63MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F63MA-FN.
      *N63MC.    NOTE *NON SETTLEMENT CODE                *.
       F63MC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63MC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63MC-FN. EXIT.
       F63MA-FN. EXIT.
      *N63NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F63NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F63NA-FN.
      *N63NC.    NOTE *NON SETTLEMENT CODE                *.
       F63NC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63NC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F63NC-FN. EXIT.
       F63NA-FN. EXIT.
      *N63OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F63OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F63OA-FN.
      *N63OH.    NOTE *PREMATURE/RETIRE SETTLEMENT CODE   *.
       F63OH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F63OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F63OH-FN. EXIT.
      *N63OK.    NOTE *PREMATURE EXCEPT SETTLE CODE       *.
       F63OK.    IF    KR9A-CLCUS = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63OK-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F63OK-FN. EXIT.
      *N63OM.    NOTE *DISABILITY OR DEATH SETTL CODE     *.
       F63OM.    IF    KR9A-CLCUS = 03 OR 04                            lv15
                 NEXT SENTENCE ELSE GO TO     F63OM-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F63OM-FN. EXIT.
      *N63OP.    NOTE *LOAN DISTRIBUTION SETTL CODE       *.
       F63OP.    IF    KR9A-CLCUS = 19                                  lv15
                 NEXT SENTENCE ELSE GO TO     F63OP-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F63OP-FN. EXIT.
       F63OA-FN. EXIT.
       F63-FN.   EXIT.
      *N64.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN TYPE - TSCA              *
      *               *                                   *
      *               *************************************.
       F64.      IF    KR9A-MPLAN = 'TSCA'                              lv05
                 NEXT SENTENCE ELSE GO TO     F64-FN.
      *N64BA.    NOTE *TO PLAN TYPE - IRA ACTIVE          *.
       F64BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F64BA-FN.
      *N64BC.    NOTE *DIRECT ROLLOVER SETTLEMENT CODES   *.
       F64BC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F64BC-FN.
      *N64BE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64BE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64BE-FN. EXIT.
       F64BC-FN. EXIT.
      *N64BH.    NOTE *PREMATURE/RETIRE SETTLEMENT CODE   *.
       F64BH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F64BH-FN.
      *N64BJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64BJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F64BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64BJ-FN. EXIT.
       F64BH-FN. EXIT.
       F64BA-FN. EXIT.
      *N64CA.    NOTE *TO PLAN TYPE - SEP                 *.
       F64CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F64CA-FN.
      *N64CC.    NOTE *DIRECT ROLLOVER SETTLEMENT CODE    *.
       F64CC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F64CC-FN.
      *N64CE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64CE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64CE-FN. EXIT.
       F64CC-FN. EXIT.
      *N64CH.    NOTE *PREMATURE/RETIRE SETTLEMENT CODE   *.
       F64CH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F64CH-FN.
      *N64CJ.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64CJ.    IF    KR9A-CIRAP = 'CU' OR 'SC'                        lv20
                 NEXT SENTENCE ELSE GO TO     F64CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64CJ-FN. EXIT.
       F64CH-FN. EXIT.
       F64CA-FN. EXIT.
      *N64DA.    NOTE *TO PLAN TYPE - SRA                 *.
       F64DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F64DA-FN.
      *NOT ALLOWED
       F64DA-FN. EXIT.
      *N64EA.    NOTE *TO PLAN TYPE - IRA ROLLOVER        *.
       F64EA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F64EA-FN.
      *N64EC.    NOTE *DIRECT ROLLOVER SETTLEMENT CODE    *.
       F64EC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F64EC-FN.
      *N64EE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64EE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64EE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64EE-FN. EXIT.
       F64EC-FN. EXIT.
       F64EA-FN. EXIT.
      *N64FA.    NOTE *TO PLAN TYPE - ROTH IRA            *.
       F64FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F64FA-FN.
      *N64FC.    NOTE *PREMATURE OR RETIRE SETTL CODE     *.
       F64FC.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F64FC-FN.
      *N64FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64FE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F64FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64FE-FN. EXIT.
       F64FC-FN. EXIT.
      *N64FI.    NOTE *NON SETTL CODE                     *.
       F64FI.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64FI-FN.
      *N64FK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64FK.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64FK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64FK-FN. EXIT.
       F64FI-FN. EXIT.
      *N64FO.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F64FO.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F64FO-FN.
      *N64FS.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64FS.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64FS-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64FS-FN. EXIT.
       F64FO-FN. EXIT.
       F64FA-FN. EXIT.
      *N64GA.    NOTE *TO PLAN TYPE - ROTH CONVERSION     *.
       F64GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F64GA-FN.
      *N64GC.    NOTE *PREMATURE OR RETIRE SETTL CODE     *.
       F64GC.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F64GC-FN.
      *N64GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64GE.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F64GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64GE-FN. EXIT.
       F64GC-FN. EXIT.
      *N64GI.    NOTE *NON SETTL CODE                     *.
       F64GI.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64GI-FN.
      *N64GK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64GK.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64GK-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64GK-FN. EXIT.
       F64GI-FN. EXIT.
      *N64GO.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F64GO.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F64GO-FN.
      *N64GS.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64GS.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64GS-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64GS-FN. EXIT.
       F64GO-FN. EXIT.
       F64GA-FN. EXIT.
      *N64HA.    NOTE *TO PLAN TYPE - EDUCATION IRA       *.
       F64HA.    IF    KR9A-MPLANA = 'EDUC IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F64HA-FN.
      *NOT ALLOWED
       F64HA-FN. EXIT.
      *N64IA.    NOTE *TO PLAN TYPE - BENEFICIAL IRA      *.
       F64IA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F64IA-FN.
      *N64IC.    NOTE *DEATH DIRECT ROLLOVER SETTL CODE   *.
       F64IC.    IF    KR9A-CLCUS = 13                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64IC-FN.
      *N64ID.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64ID.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64ID-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F64ID-FN. EXIT.
       F64IC-FN. EXIT.
      *N64IF.    NOTE *NON SETTLEMENT CODE                *.
       F64IF.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64IF-FN.
      *N64IG.    NOTE *IRA CONTRIBUTION TYPE              *.
       F64IG.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F64IG-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F64IG-FN. EXIT.
       F64IF-FN. EXIT.
       F64IA-FN. EXIT.
      *N64JA.    NOTE *TO PLAN TYPE - ROTH BENEFICIAL     *.
       F64JA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F64JA-FN.
      *NOT ALLOWED
       F64JA-FN. EXIT.
      *N64KA.    NOTE *TO PLAN TYPE - ROTH CONV BENF      *.
       F64KA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F64KA-FN.
      *NOT ALLOWED
       F64KA-FN. EXIT.
      *N64LA.    NOTE *TO PLAN TYPE - TRUSTEED            *.
       F64LA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 OR    = 'CUSTODIAL'
                 OR    = 'GOV 457'
                 NEXT SENTENCE ELSE GO TO     F64LA-FN.
      *NOT ALLOWED
      *OR CUSTODIAL OR GOV 457
      *N64LG.    NOTE *PREMATURE/RETIRE SETTLEMENT CODE   *.
       F64LG.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64LG-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64LG-FN. EXIT.
       F64LA-FN. EXIT.
      *N64MA.    NOTE *TO PLAN TYPE - TSA                 *.
       F64MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F64MA-FN.
      *N64MC.    NOTE *NON SETTLEMENT CODE                *.
       F64MC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64MC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64MC-FN. EXIT.
       F64MA-FN. EXIT.
      *N64NA.    NOTE *TO PLAN TYPE - TSCA                *.
       F64NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F64NA-FN.
      *N64NC.    NOTE *NON SETTLEMENT CODE                *.
       F64NC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64NC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F64NC-FN. EXIT.
       F64NA-FN. EXIT.
      *N64OA.    NOTE *TO PLAN TYPE - PERSONAL            *.
       F64OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F64OA-FN.
      *N64OH.    NOTE *PREMATURE/RETIRE SETTLEMENT CODE   *.
       F64OH.    IF    KR9A-CLCUS = 01 OR 07                            lv15
                 NEXT SENTENCE ELSE GO TO     F64OH-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F64OH-FN. EXIT.
      *N64OK.    NOTE *PREMATURE EXCEPTION SETTL CODE     *.
       F64OK.    IF    KR9A-CLCUS = 02                                  lv15
                 NEXT SENTENCE ELSE GO TO     F64OK-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F64OK-FN. EXIT.
      *N64OM.    NOTE *DISABILITY OR DEATH SETTL CODE     *.
       F64OM.    IF    KR9A-CLCUS = 03 OR 04                            lv15
                 NEXT SENTENCE ELSE GO TO     F64OM-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F64OM-FN. EXIT.
       F64OA-FN. EXIT.
       F64-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN - TRUSTEED               *
      *               *                                   *
      *               *************************************.
       F65.      IF    KR9A-MPLAN = 'TRUSTEED'                          lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *N65BA.    NOTE *TO PLAN - IRA ACTIVE               *.
       F65BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F65BA-FN.
      *N65BC.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65BC.    IF    KR9A-CIRAP = 'RO'                                lv15
                 NEXT SENTENCE ELSE GO TO     F65BC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65BC-FN. EXIT.
       F65BA-FN. EXIT.
      *N65BE.    NOTE *TO PLAN - IRA ROLLOVER             *.
       F65BE.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F65BE-FN.
      *N65BH.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65BH.    IF    KR9A-CIRAP = 'RO'                                lv15
                 NEXT SENTENCE ELSE GO TO     F65BH-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65BH-FN. EXIT.
       F65BE-FN. EXIT.
      *N65CA.    NOTE *TO PLAN - SEP                      *.
       F65CA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F65CA-FN.
      *N65CC.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65CC.    IF    KR9A-CIRAP = 'RO'                                lv15
                 NEXT SENTENCE ELSE GO TO     F65CC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65CC-FN. EXIT.
       F65CA-FN. EXIT.
      *N65DA.    NOTE *TO PLAN - SRA                      *.
       F65DA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F65DA-FN.
      *NOT ALLOWED
       F65DA-FN. EXIT.
      *N65EA.    NOTE *TO PLAN - BENEFICIAL               *.
       F65EA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F65EA-FN.
      *N65EC.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65EC.    IF    KR9A-CIRAP = 'RO'                                lv15
                 NEXT SENTENCE ELSE GO TO     F65EC-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F65EC-FN. EXIT.
       F65EA-FN. EXIT.
      *N65FA.    NOTE *TO PLAN - ROTH IRA                 *.
       F65FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F65FA-FN.
      *N65FB.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65FB.    IF    KR9A-CIRAP = 'RO' OR 'CV'                        lv15
                 NEXT SENTENCE ELSE GO TO     F65FB-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65FB-FN. EXIT.
       F65FA-FN. EXIT.
      *N65GA.    NOTE *TO PLAN - ROTH BENF                *.
       F65GA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F65GA-FN.
      *N65GC.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65GC.    IF    KR9A-CIRAP = 'RO'                                lv15
                 NEXT SENTENCE ELSE GO TO     F65GC-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F65GC-FN. EXIT.
       F65GA-FN. EXIT.
      *N65HA.    NOTE *TO PLAN - ROTH CONVBEN             *.
       F65HA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F65HA-FN.
      *NOT ALLOWED
       F65HA-FN. EXIT.
      *N65IA.    NOTE *TO PLAN - TRUSTEED                 *.
       F65IA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 NEXT SENTENCE ELSE GO TO     F65IA-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65IA-FN. EXIT.
      *N65JA.    NOTE *TO PLAN - ROTH CONV                *.
       F65JA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F65JA-FN.
      *N65JB.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65JB.    IF    KR9A-CIRAP = 'RO' OR 'CV'                        lv15
                 NEXT SENTENCE ELSE GO TO     F65JB-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65JB-FN. EXIT.
       F65JA-FN. EXIT.
      *N65KA.    NOTE *TO PLAN - CUSTODIAL                *.
       F65KA.    IF    KR9A-MPLANA = 'CUSTODIAL'                        lv10
                 NEXT SENTENCE ELSE GO TO     F65KA-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65KA-FN. EXIT.
      *N65LA.    NOTE *TO PLAN - TSA                      *.
       F65LA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F65LA-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65LA-FN. EXIT.
      *N65MA.    NOTE *TO PLAN - TSCA                     *.
       F65MA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F65MA-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F65MA-FN. EXIT.
      *N65NA.    NOTE *TO PLAN - GOV 457                  *.
       F65NA.    IF    KR9A-MPLANA = 'GOV 457'                          lv10
                 NEXT SENTENCE ELSE GO TO     F65NA-FN.
      *NOT ALLOWED
       F65NA-FN. EXIT.
      *N65OA.    NOTE *TO PLAN - PERSONAL                 *.
       F65OA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F65OA-FN.
      *ALLOWED
           MOVE        'Y' TO KR9A-CTRFAA.
       F65OA-FN. EXIT.
       F65-FN.   EXIT.
      *N66.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN - CUSTODIAL              *
      *               *                                   *
      *               *************************************.
       F66.      IF    KR9A-MPLAN = 'CUSTODIAL'                         lv05
                 NEXT SENTENCE ELSE GO TO     F66-FN.
      *N66BA.    NOTE *TO PLAN - IRA ACTIVE               *.
       F66BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F66BA-FN.
      *N66BC.    NOTE *DIRECT ROLLOVER OR DEATH DIRECT    *.
       F66BC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F66BC-FN.
      *ROLL OVER TO IRA SETTLEMENT CD
      *N66BE.    NOTE *IRA CONTRIBUTION CODE              *.
       F66BE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66BE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66BE-FN. EXIT.
       F66BC-FN. EXIT.
      *N66BH.    NOTE *NON TAX REPORTABLE SETTLEMENT CD   *.
       F66BH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66BH-FN.
      *N66BJ.    NOTE *IRA CONTRIBUTION CODE              *.
       F66BJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F66BJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66BJ-FN. EXIT.
       F66BH-FN. EXIT.
      *N66BM.    NOTE *SETTLEMENT CODE                    *.
       F66BM.    IF    KR9A-CLCUS = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F66BM-FN.
      *N66BO.    NOTE *IRA CONTRIBUTION CODE              *.
       F66BO.    IF    KR9A-CIRAP = 'CU'                                lv20
                 OR    = 'PR'
                 OR    = 'RO'
                 NEXT SENTENCE ELSE GO TO     F66BO-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66BO-FN. EXIT.
       F66BM-FN. EXIT.
       F66BA-FN. EXIT.
      *N66CA.    NOTE *TO PLAN - IRA ROLL OVER            *.
       F66CA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F66CA-FN.
      *N66CC.    NOTE *DIRECT ROLLOVER OR DEATH DIRECT    *.
       F66CC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F66CC-FN.
      *ROLL OVER TO IRA SETTLEMENT CD
      *N66CE.    NOTE *IRA CONTRIBUTION CODE              *.
       F66CE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66CE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66CE-FN. EXIT.
       F66CC-FN. EXIT.
      *N66CH.    NOTE *SETTLEMENT CODE                    *.
       F66CH.    IF    KR9A-CLCUS = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F66CH-FN.
      *N66CJ.    NOTE *IRA CONTRIBUTION CODE              *.
       F66CJ.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66CJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66CJ-FN. EXIT.
       F66CH-FN. EXIT.
       F66CA-FN. EXIT.
      *N66DA.    NOTE *TO PLAN - SEP                      *.
       F66DA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F66DA-FN.
      *N66DC.    NOTE *DIRECT ROLLOVER OR DEATH DIRECT    *.
       F66DC.    IF    KR9A-CLCUS = 10 OR 13                            lv15
                 NEXT SENTENCE ELSE GO TO     F66DC-FN.
      *ROLL OVER TO IRA SETTLEMENT CD
      *N66DE.    NOTE *IRA CONTRIBUTION CODE              *.
       F66DE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66DE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66DE-FN. EXIT.
       F66DC-FN. EXIT.
      *N66DH.    NOTE *NON TAX REPORTABLE SETTLEMENT CD   *.
       F66DH.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66DH-FN.
      *N66DJ.    NOTE *IRA CONTRIBUTION CODE              *.
       F66DJ.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F66DJ-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66DJ-FN. EXIT.
       F66DH-FN. EXIT.
      *N66DM.    NOTE *SETTLEMENT CODE                    *.
       F66DM.    IF    KR9A-CLCUS = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F66DM-FN.
      *N66DO.    NOTE *IRA CONTRIBUTION CODE              *.
       F66DO.    IF    KR9A-CIRAP = 'CU'                                lv20
                 OR    = 'PR'
                 OR    = 'RO'
                 NEXT SENTENCE ELSE GO TO     F66DO-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66DO-FN. EXIT.
       F66DM-FN. EXIT.
       F66DA-FN. EXIT.
      *N66EA.    NOTE *TO PLAN - SRA                      *.
       F66EA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F66EA-FN.
      *NOT ALLOWED
       F66EA-FN. EXIT.
      *N66FA.    NOTE *TO PLAN - ROTH IRA                 *.
       F66FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F66FA-FN.
      *N66FC.    NOTE *NON SETTL CODE                     *.
       F66FC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66FC-FN.
      *N66FE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66FE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66FE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66FE-FN. EXIT.
       F66FC-FN. EXIT.
      *N66FJ.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F66FJ.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F66FJ-FN.
      *N66FM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66FM.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66FM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66FM-FN. EXIT.
       F66FJ-FN. EXIT.
      *N66FR.    NOTE *RETIRE SETTLEMENT CODE OR          *.
       F66FR.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F66FR-FN.
      *PREMATURE DISTRIBUTION
      *N66FT.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66FT.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F66FT-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66FT-FN. EXIT.
       F66FR-FN. EXIT.
       F66FA-FN. EXIT.
      *N66GA.    NOTE *TO PLAN - ROTH CONV                *.
       F66GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F66GA-FN.
      *N66GC.    NOTE *NON SETTL CODE                     *.
       F66GC.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66GC-FN.
      *N66GE.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66GE.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66GE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66GE-FN. EXIT.
       F66GC-FN. EXIT.
      *N66GJ.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F66GJ.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F66GJ-FN.
      *N66GM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66GM.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66GM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66GM-FN. EXIT.
       F66GJ-FN. EXIT.
      *N66GR.    NOTE *PREMATURE OR RETIRE SETL CODE      *.
       F66GR.    IF    KR9A-CLCUS = 07 OR 01                            lv15
                 NEXT SENTENCE ELSE GO TO     F66GR-FN.
      *N66GT.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66GT.    IF    KR9A-CIRAP = 'CU' OR 'PR'                        lv20
                 NEXT SENTENCE ELSE GO TO     F66GT-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66GT-FN. EXIT.
       F66GR-FN. EXIT.
       F66GA-FN. EXIT.
      *N66HA.    NOTE *TO PLAN -  BENEFICIAL              *.
       F66HA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F66HA-FN.
      *N66HC.    NOTE *DEATH DIRECT ROLLOVER SETTL CODE   *.
       F66HC.    IF    KR9A-CLCUS = 13                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66HC-FN.
      *N66HD.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66HD.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66HD-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F66HD-FN. EXIT.
       F66HC-FN. EXIT.
      *N66HF.    NOTE *NON SETTLEMENT CODE                *.
       F66HF.    IF    KR9A-CLCUS = 99                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66HF-FN.
      *N66HG.    NOTE *IRA CONTRIBUTION TYPE              *.
       F66HG.    IF    KR9A-CIRAP = 'IT'                                lv20
                 NEXT SENTENCE ELSE GO TO     F66HG-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F66HG-FN. EXIT.
       F66HF-FN. EXIT.
       F66HA-FN. EXIT.
      *N66IA.    NOTE *TO PLAN -  ROTH BENF               *.
       F66IA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F66IA-FN.
      *NOT ALLOWED
       F66IA-FN. EXIT.
      *N66JA.    NOTE *TO PLAN -  ROTH CONVBEN            *.
       F66JA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F66JA-FN.
      *NOT ALLOWED
       F66JA-FN. EXIT.
      *N66KA.    NOTE *TO PLAN -  TRUSTEED                *.
       F66KA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 NEXT SENTENCE ELSE GO TO     F66KA-FN.
      *N66KC.    NOTE *DIRECT ROLL OVER TO OTHER          *.
       F66KC.    IF    KR9A-CLCUS = 10 OR 99                            lv15
                 NEXT SENTENCE ELSE GO TO     F66KC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66KC-FN. EXIT.
      *N66KE.    NOTE *SETTLEMENT CODE                    *.
       F66KE.    IF    KR9A-CLCUS = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F66KE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66KE-FN. EXIT.
       F66KA-FN. EXIT.
      *N66LA.    NOTE *TO PLAN -  CUSTODIAL               *.
       F66LA.    IF    KR9A-MPLANA = 'CUSTODIAL'                        lv10
                 NEXT SENTENCE ELSE GO TO     F66LA-FN.
      *N66LC.    NOTE *DIRECT ROLL OVER TO OTHER          *.
       F66LC.    IF    KR9A-CLCUS = 10 OR 99                            lv15
                 NEXT SENTENCE ELSE GO TO     F66LC-FN.
      *TAX SHELTERED ANNUITY
      *SETTLEMENT CODE
           MOVE        'O' TO KR9A-CTRFAA.
       F66LC-FN. EXIT.
      *N66LE.    NOTE *SETTLEMENT CODE                    *.
       F66LE.    IF    KR9A-CLCUS = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F66LE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66LE-FN. EXIT.
       F66LA-FN. EXIT.
      *N66MA.    NOTE *TO PLAN -  TSA                     *.
       F66MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F66MA-FN.
      *N66MC.    NOTE *DIRECT ROLL OVER TO OTHER          *.
       F66MC.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66MC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66MC-FN. EXIT.
       F66MA-FN. EXIT.
      *N66NA.    NOTE *TO PLAN -  TSCA                    *.
       F66NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F66NA-FN.
      *N66NC.    NOTE *DIRECT ROLL OVER TO OTHER          *.
       F66NC.    IF    KR9A-CLCUS = 10                                  lv15
                 NEXT SENTENCE ELSE GO TO     F66NC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66NC-FN. EXIT.
       F66NA-FN. EXIT.
      *N66OA.    NOTE *TO PLAN -  GOV 457                 *.
       F66OA.    IF    KR9A-MPLANA = 'GOV 457'                          lv10
                 NEXT SENTENCE ELSE GO TO     F66OA-FN.
      *NOT ALLOWED
       F66OA-FN. EXIT.
      *N66PA.    NOTE *TO PLAN -  PERSONAL                *.
       F66PA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F66PA-FN.
      *N66PC.    NOTE *SETTLEMENT CODE                    *.
       F66PC.    IF    KR9A-CLCUS = 07 OR 01 OR 02                      lv15
                       OR 03 OR 04 OR 99
                 NEXT SENTENCE ELSE GO TO     F66PC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66PC-FN. EXIT.
      *N66PE.    NOTE *SETTLEMENT CODE                    *.
       F66PE.    IF    KR9A-CLCUS = ZEROES                              lv15
                 NEXT SENTENCE ELSE GO TO     F66PE-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F66PE-FN. EXIT.
       F66PA-FN. EXIT.
       F66-FN.   EXIT.
      *N67.      NOTE *************************************.
      *               *                                   *
      *               *FROM PLAN - GOV 457                *
      *               *                                   *
      *               *************************************.
       F67.      IF    KR9A-MPLAN = 'GOV 457'                           lv05
                 NEXT SENTENCE ELSE GO TO     F67-FN.
      *N67BA.    NOTE *TO PLAN - IRA ACTIVE               *.
       F67BA.    IF    KR9A-MPLANA = 'IRA ACTIVE'                       lv10
                 NEXT SENTENCE ELSE GO TO     F67BA-FN.
      *NOT ALLOWED
       F67BA-FN. EXIT.
      *N67CA.    NOTE *TO PLAN - IRA ROLLOVER             *.
       F67CA.    IF    KR9A-MPLANA = 'IRA ROLLOVER'                     lv10
                 NEXT SENTENCE ELSE GO TO     F67CA-FN.
      *NOT ALLOWED
       F67CA-FN. EXIT.
      *N67DA.    NOTE *TO PLAN - SEP                      *.
       F67DA.    IF    KR9A-MPLANA = 'SEP'                              lv10
                 NEXT SENTENCE ELSE GO TO     F67DA-FN.
      *NOT ALLOWED
       F67DA-FN. EXIT.
      *N67EA.    NOTE *TO PLAN - SRA                      *.
       F67EA.    IF    KR9A-MPLANA = 'SRA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F67EA-FN.
      *NOT ALLOWED
       F67EA-FN. EXIT.
      *N67FA.    NOTE *TO PLAN - ROTH IRA                 *.
       F67FA.    IF    KR9A-MPLANA = 'ROTH IRA'                         lv10
                 NEXT SENTENCE ELSE GO TO     F67FA-FN.
      *N67FB.    NOTE *SPACE OR ZERO SETTLEMENT CODES     *.
       F67FB.    IF    KR9A-CLCUS = ZERO OR SPACE                       lv15
                 NEXT SENTENCE ELSE GO TO     F67FB-FN.
      *N67FC.    NOTE *IRA CONTRIBUTION TYPE              *.
       F67FC.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F67FC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F67FC-FN. EXIT.
       F67FB-FN. EXIT.
      *N67FJ.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F67FJ.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F67FJ-FN.
      *N67FM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F67FM.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F67FM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F67FM-FN. EXIT.
       F67FJ-FN. EXIT.
       F67FA-FN. EXIT.
      *N67GA.    NOTE *TO PLAN - ROTH CONV                *.
       F67GA.    IF    KR9A-MPLANA = 'ROTH CONV'                        lv10
                 NEXT SENTENCE ELSE GO TO     F67GA-FN.
      *N67GB.    NOTE *SPACE OR ZERO SETTLEMENT CODES     *.
       F67GB.    IF    KR9A-CLCUS = ZERO OR SPACE                       lv15
                 NEXT SENTENCE ELSE GO TO     F67GB-FN.
      *N67GC.    NOTE *IRA CONTRIBUTION TYPE              *.
       F67GC.    IF    KR9A-CIRAP = 'CV'                                lv20
                 NEXT SENTENCE ELSE GO TO     F67GC-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F67GC-FN. EXIT.
       F67GB-FN. EXIT.
      *N67GJ.    NOTE *ROTH PCV/RCV SETTLEMENT CODES      *.
       F67GJ.    IF    KR9A-CLCUS = 51 OR 52                            lv15
                 NEXT SENTENCE ELSE GO TO     F67GJ-FN.
      *N67GM.    NOTE *IRA CONTRIBUTION TYPE              *.
       F67GM.    IF    KR9A-CIRAP = 'RO'                                lv20
                 NEXT SENTENCE ELSE GO TO     F67GM-FN.
           MOVE        'O' TO KR9A-CTRFAA.
       F67GM-FN. EXIT.
       F67GJ-FN. EXIT.
       F67GA-FN. EXIT.
      *N67HA.    NOTE *TO PLAN - BENEFICIAL               *.
       F67HA.    IF    KR9A-MPLANA = 'BENEFICIAL'                       lv10
                 NEXT SENTENCE ELSE GO TO     F67HA-FN.
      *N67HC.    NOTE *IRA CONTRIBUTION TYPE              *.
       F67HC.    IF    (KR9A-CIRAP = 'RO'                               lv15
                 OR    KR9A-CIRAP = 'IT')
                 NEXT SENTENCE ELSE GO TO     F67HC-FN.
           MOVE        'Y' TO KR9A-CTRFAA.
       F67HC-FN. EXIT.
       F67HA-FN. EXIT.
      *N67IA.    NOTE *TO PLAN - ROTH BENF                *.
       F67IA.    IF    KR9A-MPLANA = 'ROTH BENF'                        lv10
                 NEXT SENTENCE ELSE GO TO     F67IA-FN.
      *NOT ALLOWED
       F67IA-FN. EXIT.
      *N67JA.    NOTE *TO PLAN - ROTH CONVBEN             *.
       F67JA.    IF    KR9A-MPLANA = 'ROTH CONVBEN'                     lv10
                 NEXT SENTENCE ELSE GO TO     F67JA-FN.
      *NOT ALLOWED
       F67JA-FN. EXIT.
      *N67KA.    NOTE *TO PLAN - TRUSTEED                 *.
       F67KA.    IF    KR9A-MPLANA = 'TRUSTEED'                         lv10
                 NEXT SENTENCE ELSE GO TO     F67KA-FN.
      *NOT ALLOWED
       F67KA-FN. EXIT.
      *N67LA.    NOTE *TO PLAN - CUSTODIAL                *.
       F67LA.    IF    KR9A-MPLANA = 'CUSTODIAL'                        lv10
                 NEXT SENTENCE ELSE GO TO     F67LA-FN.
      *NOT ALLOWED
       F67LA-FN. EXIT.
      *N67MA.    NOTE *TO PLAN - TSA                      *.
       F67MA.    IF    KR9A-MPLANA = 'TSA'                              lv10
                 NEXT SENTENCE ELSE GO TO     F67MA-FN.
      *NOT ALLOWED
       F67MA-FN. EXIT.
      *N67NA.    NOTE *TO PLAN - TSCA                     *.
       F67NA.    IF    KR9A-MPLANA = 'TSCA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F67NA-FN.
      *NOT ALLOWED
       F67NA-FN. EXIT.
      *N67OA.    NOTE *TO PLAN - GOV 457                  *.
       F67OA.    IF    KR9A-MPLANA = 'GOV 457'                          lv10
                 NEXT SENTENCE ELSE GO TO     F67OA-FN.
      *NOT ALLOWED
       F67OA-FN. EXIT.
      *N67PA.    NOTE *TO PLAN - PERSONAL                 *.
       F67PA.    IF    KR9A-MPLANA = 'PERSONAL'                         lv10
                 NEXT SENTENCE ELSE GO TO     F67PA-FN.
      *NOT ALLOWED
       F67PA-FN. EXIT.
       F67-FN.   EXIT.
      *N9098.    NOTE *SET "GOBACK" INDICATOR             *.
       F9098.                                                           lv10
      *THIS WILL MAKE SURE ONE PASS OF
      *PROGRAM AND THEN GET OUT
           MOVE ALL    '1' TO FT.
       F9098-FN. EXIT.
       F9099-ITER-FN.  GO TO F05.
