       IDENTIFICATION DIVISION.                                         CI0274
       PROGRAM-ID.  CI0274P.                                            CI0274
      *AUTHOR.         GET ALLOWABLE FREQUENCIES.                       CI0274
      *DATE-COMPILED.   09/08/14.                                       CI0274
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1999                          *ACOPYP
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
      *     COPR. 1999                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CI0274
       CONFIGURATION SECTION.                                           CI0274
       SOURCE-COMPUTER. IBM-370.                                        CI0274
       OBJECT-COMPUTER. IBM-370.                                        CI0274
       DATA DIVISION.                                                   CI0274
       WORKING-STORAGE SECTION.                                         CI0274
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
      ******************************************************************ADUTB2
      **              TABLE TA71 ACCESS FIELDS                         *ADUTB2
      ******************************************************************ADUTB2
      **                                                                ADUTB2
       01  G-TA71-LTH        PIC S9(4) COMP.                            ADUTB2
      **                                                                ADUTB2
      *!WF DSP=TA DSL=TA SEL=71 FOR=I DES=2 LEV=1 ORG=G                 ADUTB2
       01        G-TA71.                                                CI0274
           04    G-TA71-PARAM.                                          CI0274
             10  G-TA71-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0274
                        VALUE      +042.                                CI0274
             10  G-TA71-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0274
                        VALUE      +001.                                CI0274
             10  G-TA71-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0274
                        VALUE      +002.                                CI0274
             10  G-TA71-NUAPP  PICTURE 99                               CI0274
                        VALUE       0.                                  CI0274
             10  G-TA71-NUTAB  PICTURE X(6)                             CI0274
                        VALUE 'CPMTF '.                                 CI0274
             10  G-TA71-TABFO  PICTURE XX                 VALUE SPACE.  CI0274
             10  G-TA71-TABCR  PICTURE XX                 VALUE SPACE.  CI0274
             10  G-TA71-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0274
             10  G-TA71-NUSSC  PICTURE X  VALUE   ' '.                  CI0274
             10  G-TA71-NUSSY  PICTURE X                  VALUE SPACE.  CI0274
             10  G-TA71-TRANID PICTURE X(4)               VALUE SPACE.  CI0274
             10  G-TA71-FILSYS.                                         CI0274
             15  G-TA71-USERC  PICTURE X(6)               VALUE SPACE.  CI0274
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0274
           04             TA71.                                         CI0274
            10            TA71-CPMTF  PICTURE  99                       CI0274
                          VALUE                ZERO.                    CI0274
            10            TA71-MPMTF  PICTURE  X(14)                    CI0274
                          VALUE                SPACE.                   CI0274
            10            TA71-CPMTFA PICTURE  X(2)                     CI0274
                          VALUE                SPACE.                   CI0274
            10            TA71-MPMTFL PICTURE  X(24)                    CI0274
                          VALUE                SPACE.                   CI0274
      **                                                                ADUTB2
      *** WORK FIELDS **
       01 7-ALLOWABLE-FREQ.
      *!WI
          05  7-WS-CPMTF
                        PICTURE 99.                                     CI0274
              88  ANNUITY-FREQ    VALUE 04, 06, 12, 24, 26, 52.
      **
       01 WS-SUB   PIC 9(3) VALUE 0.
       01   DEBUT-WSS.                                                  CI0274
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0274
            05   IK     PICTURE X.                                      CI0274
       01  CONSTANTES-PAC.                                              CI0274
           05  FILLER  PICTURE X(87)   VALUE                            CI0274
                     '6015 CAT09/08/14CI0274ADMIN   14:35:11CI0274P AMERCI0274
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0274
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0274
           05  NUGNA   PICTURE X(5).                                    CI0274
           05  APPLI   PICTURE X(3).                                    CI0274
           05  DATGN   PICTURE X(8).                                    CI0274
           05  PROGR   PICTURE X(6).                                    CI0274
           05  CODUTI  PICTURE X(8).                                    CI0274
           05  TIMGN   PICTURE X(8).                                    CI0274
           05  PROGE   PICTURE X(8).                                    CI0274
           05  COBASE  PICTURE X(4).                                    CI0274
           05  DATGNC  PICTURE X(10).                                   CI0274
           05  RELEAS  PICTURE X(7).                                    CI0274
           05  DATGE   PICTURE X(10).                                   CI0274
           05  DATSQ   PICTURE X(10).                                   CI0274
       01  DATCE.                                                       CI0274
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0274
         05  DATOR.                                                     CI0274
           10  DATOA  PICTURE XX.                                       CI0274
           10  DATOM  PICTURE XX.                                       CI0274
           10  DATOJ  PICTURE XX.                                       CI0274
       01   VARIABLES-CONDITIONNELLES.                                  CI0274
            05                  FT      PICTURE X VALUE '0'.            CI0274
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0274
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0274
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0274
       LINKAGE SECTION.                                                 ADU102
      ******************************************************************
      **    LINKAGE SECTION FROM CALLING MODULE
      ******************************************************************
      **
      *!WF DSP=K9 DSL=K9 SEL=83 FOR=I DES=1 LEV=1 PLT=75
       01                 K983.                                         CI0274
            10            K983-MAPPN  PICTURE  X(10).                   CI0274
            10            K983-CAPPL  PICTURE  X(8).                    CI0274
            10            K983-QITEM  PICTURE  9(3).                    CI0274
            10            K983-GPMTF                                    CI0274
                          OCCURS       020     TIMES.                   CI0274
            11            K983-CPMTF  PICTURE  99.                      CI0274
            11            K983-MPMTF  PICTURE  X(14).                   CI0274
            11            K983-CPMTFA PICTURE  X(2).                    CI0274
            11            K983-MPMTFL PICTURE  X(24).                   CI0274
            10            K983-FILLER PICTURE  X(50).                   CI0274
      **
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0274
          05              MS00-SUITE.                                   CI0274
            15       FILLER         PICTURE  X(00542).                  CI0274
       01                 MS03  REDEFINES      MS00.                    CI0274
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0274
                          COMPUTATIONAL-3.                              CI0274
            10            MS03-CMSSF  PICTURE  XX.                      CI0274
            10            MS03-DU09.                                    CI0274
            11            MS03-CMESA  PICTURE  S9(9)                    CI0274
                          BINARY.                                       CI0274
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0274
                          BINARY.                                       CI0274
            11            MS03-CMESB  PICTURE  S9(9)                    CI0274
                          BINARY.                                       CI0274
            11            MS03-CMSST  PICTURE  S9(9)                    CI0274
                          BINARY.                                       CI0274
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0274
                          BINARY.                                       CI0274
            11            MS03-QELLAA PICTURE  S9(9)                    CI0274
                          BINARY.                                       CI0274
            11            MS03-TMESS4 PICTURE  X(512).                  CI0274
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0274
            10            MX11-QMSGS  PICTURE  9(03).                   CI0274
            10            MX11-PJ09                                     CI0274
                          OCCURS       025     TIMES.                   CI0274
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0274
                          COMPUTATIONAL-3.                              CI0274
            11            MX11-CMESB  PICTURE  S9(9)                    CI0274
                          BINARY.                                       CI0274
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                K983
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0274
      *               *                                   *             CI0274
      *               *INITIALISATIONS                    *             CI0274
      *               *                                   *             CI0274
      *               *************************************.            CI0274
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
      *N02CA.    NOTE *---> INITIALIZE MESSAGE SEGMENT    *.
       F02CA.                                                           lv10
           INITIALIZE  MS03.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *---> INIT OUTPUT & WORK AREA       *.
       F02DA.                                                           lv10
           INITIALIZE  7-WS-CPMTF.
       F02DA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0274
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0274
      *               *                                   *             CI0274
      *               *FIN DE TRAITEMENT                  *             CI0274
      *               *                                   *             CI0274
      *               *************************************.            CI0274
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0274
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *SET UP FREQUECIES LOOP             *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      ** READ CPMTF TABLE SEQUENCIALY *
      ** FOR ALL FREQUENCY CODES THAT *
      ** ARE VALID FOR ANNUITIES, GET *
      ** THE LONG NAME. SEGMENT TA71. *
      *********************************
           MOVE        '0' TO IK
           MOVE        ZEROES TO TA71-CPMTF
           MOVE        +0 TO WS-SUB
      *GET FIRST FREQUENCY
           PERFORM     F92SQ THRU F92SQ-FN.
      *N40BB.    NOTE *FREQUENCY LOOP                     *.
       F40BB.                       GO TO     F40BB-B.                  lv10
       F40BB-A.
                 IF    IK = '1'
                                    GO TO     F40BB-FN.
       F40BB-B.
           MOVE        TA71-CPMTF TO 7-WS-CPMTF.
      *N40BG.    NOTE *SET UP TO TEST REQUEST TYPE        *.
       F40BG.         EXIT.                                             lv15
      *N40BL.    NOTE *REQUEST FOR ANNUITY PRODUCT        *.
       F40BL.    IF    K983-CAPPL =                                     lv20
                       'ANNUITY '
                 OR    'ANNUITYR'
                 NEXT SENTENCE ELSE GO TO     F40BL-FN.
      ***************
      *N40BQ.    NOTE *ALLOWABLE ANNUITY PRODUCT FREQ     *.
       F40BQ.    IF    ANNUITY-FREQ                                     lv25
                 NEXT SENTENCE ELSE GO TO     F40BQ-FN.
           ADD         1 TO WS-SUB
           MOVE        TA71-CPMTF TO K983-CPMTF (WS-SUB)
           MOVE        TA71-MPMTF TO K983-MPMTF (WS-SUB)
           MOVE        TA71-CPMTFA TO K983-CPMTFA (WS-SUB)
           MOVE        TA71-MPMTFL TO K983-MPMTFL (WS-SUB).
       F40BQ-FN. EXIT.
       F40BL-900. GO TO F40BG-FN.
       F40BL-FN. EXIT.
       F40BG-FN. EXIT.
      *N40FB.    NOTE *GET NEXT FREQUENCY                 *.
       F40FB.                                                           lv15
           PERFORM     F92SQ THRU F92SQ-FN.
       F40FB-FN. EXIT.
       F40BB-900. GO TO F40BB-A.
       F40BB-FN. EXIT.
      *N40HB.    NOTE *MOVE FREQ COUNT TO OUTPUT          *.
       F40HB.                                                           lv10
           MOVE        WS-SUB TO K983-QITEM.
       F40HB-FN. EXIT.
       F40-FN.   EXIT.
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
      *N92ER.    NOTE *SET IK ON ERROR                    *.
       F92ER.                                                           lv10
           MOVE        '1' TO IK.
       F92ER-FN. EXIT.
      *N92SQ.    NOTE *SEQUENTIAL TABLE READ FOR TA71     *.            ADUTB2
       F92SQ.                                                           lv10
           MOVE        'L1' TO G-TA71-TABFO                             ADUTB2
           COMPUTE     G-TA71-LTH = 60 + G-TA71-LOZTR                   ADUTB2
           MOVE        'PA01' TO G-TA71-TRANID                          ADUTB2
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTB2
                       COMMAREA (G-TA71)                                ADUTB2
                       LENGTH (G-TA71-LTH)                   END-EXEC.  ADUTB2
                 IF    G-TA71-TABCR NOT = '00'                          DOT
           PERFORM     F92ER THRU F92ER-FN.                             ADUTB2
       F92SQ-FN. EXIT.
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
