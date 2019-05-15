       IDENTIFICATION DIVISION.                                         CI0076
       PROGRAM-ID.  CI0076P.                                            CI0076
      *AUTHOR.         CALC LIFE EXPECT CLIENT ONLY.                    CI0076
      *DATE-COMPILED.   09/08/14.                                       CI0076
       ENVIRONMENT DIVISION.                                            CI0076
       CONFIGURATION SECTION.                                           CI0076
       SOURCE-COMPUTER. IBM-370.                                        CI0076
       OBJECT-COMPUTER. IBM-370.                                        CI0076
       DATA DIVISION.                                                   CI0076
       WORKING-STORAGE SECTION.                                         CI0076
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
      **              TABLE TY03 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TY03-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TY DSL=TY SEL=03 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
      **                                                                ADUTAB
       01   DEBUT-WSS.                                                  CI0076
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0076
            05   IK     PICTURE X.                                      CI0076
       01  CONSTANTES-PAC.                                              CI0076
           05  FILLER  PICTURE X(87)   VALUE                            CI0076
                     '6015 CAT09/08/14CI0076ADMIN   14:34:39CI0076P AMERCI0076
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0076
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0076
           05  NUGNA   PICTURE X(5).                                    CI0076
           05  APPLI   PICTURE X(3).                                    CI0076
           05  DATGN   PICTURE X(8).                                    CI0076
           05  PROGR   PICTURE X(6).                                    CI0076
           05  CODUTI  PICTURE X(8).                                    CI0076
           05  TIMGN   PICTURE X(8).                                    CI0076
           05  PROGE   PICTURE X(8).                                    CI0076
           05  COBASE  PICTURE X(4).                                    CI0076
           05  DATGNC  PICTURE X(10).                                   CI0076
           05  RELEAS  PICTURE X(7).                                    CI0076
           05  DATGE   PICTURE X(10).                                   CI0076
           05  DATSQ   PICTURE X(10).                                   CI0076
       01  DATCE.                                                       CI0076
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0076
         05  DATOR.                                                     CI0076
           10  DATOA  PICTURE XX.                                       CI0076
           10  DATOM  PICTURE XX.                                       CI0076
           10  DATOJ  PICTURE XX.                                       CI0076
       01   VARIABLES-CONDITIONNELLES.                                  CI0076
            05                  FT      PICTURE X VALUE '0'.            CI0076
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0076
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0076
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0076
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **     WM26 SEGMENT CONTAINS THE INPUT AND OUTPUT PARAMETERS.    *
      ******************************************************************
      *
      *!WF DSP=WM DSL=WM SEL=26 FOR=I LEV=1 PLT=10
       01                 WM00.                                         CI0076
          05              WM00-SUITE.                                   CI0076
            15       FILLER         PICTURE  X(00009).                  CI0076
       01                 WM26  REDEFINES      WM00.                    CI0076
            10            WM26-QCAGE  PICTURE  9(3).                    CI0076
            10            WM26-QYRLF  PICTURE  9(03).                   CI0076
            10            WM26-QMTLF  PICTURE  9(03).                   CI0076
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
       01                 MS00.                                         CI0076
          05              MS00-SUITE.                                   CI0076
            15       FILLER         PICTURE  X(00542).                  CI0076
       01                 MS03  REDEFINES      MS00.                    CI0076
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0076
                          COMPUTATIONAL-3.                              CI0076
            10            MS03-CMSSF  PICTURE  XX.                      CI0076
            10            MS03-DU09.                                    CI0076
            11            MS03-CMESA  PICTURE  S9(9)                    CI0076
                          BINARY.                                       CI0076
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0076
                          BINARY.                                       CI0076
            11            MS03-CMESB  PICTURE  S9(9)                    CI0076
                          BINARY.                                       CI0076
            11            MS03-CMSST  PICTURE  S9(9)                    CI0076
                          BINARY.                                       CI0076
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0076
                          BINARY.                                       CI0076
            11            MS03-QELLAA PICTURE  S9(9)                    CI0076
                          BINARY.                                       CI0076
            11            MS03-TMESS4 PICTURE  X(512).                  CI0076
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0076
            10            MX11-QMSGS  PICTURE  9(03).                   CI0076
            10            MX11-PJ09                                     CI0076
                          OCCURS       025     TIMES.                   CI0076
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0076
                          COMPUTATIONAL-3.                              CI0076
            11            MX11-CMESB  PICTURE  S9(9)                    CI0076
                          BINARY.                                       CI0076
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                WM26
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0076
      *               *                                   *             CI0076
      *               *INITIALISATIONS                    *             CI0076
      *               *                                   *             CI0076
      *               *************************************.            CI0076
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
      *N02CA.    NOTE *---> INITIALZE OUTPUT SEG FIELDS   *.
       F02CA.                                                           lv10
           MOVE        ZEROS TO WM26-QYRLF
           MOVE        ZEROS TO WM26-QMTLF.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0076
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0076
      *               *                                   *             CI0076
      *               *FIN DE TRAITEMENT                  *             CI0076
      *               *                                   *             CI0076
      *               *************************************.            CI0076
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0076
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE CLIENT LIFE EXPECTANCY   *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BA.    NOTE **** CLIENT ONLY TABLE ACCESS ***   *.
       F50BA.    IF    WM26-QCAGE > 042                                 lv10
                 AND   WM26-QCAGE < 116
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *********************************
      ** FIND CLIENT AGE ON CLIENT    *
      ** ONLY LIFE EXPECTANCY TABLE   *
      *********************************
      *
           MOVE        WM26-QCAGE TO TY03-QCAGE
           PERFORM     F92TY THRU F92TY-FN
           MOVE        TY03-QYRLF TO WM26-QYRLF
           MOVE        ZEROS TO WM26-QMTLF.
       F50BA-900. GO TO F50DA-FN.
       F50BA-FN. EXIT.
      *N50DA.    NOTE *CLIENT AGE NOT WITH AGE RANGE      *.
       F50DA.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012770 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50DA-FN. EXIT.
       F50-FN.   EXIT.
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
      *N92TY.    NOTE *RANDOM TABLE READ FOR TY03         *.            ADUTAB
       F92TY.                                                           lv10
           MOVE        'R1' TO G-TY03-TABFO                             ADUTAB
           COMPUTE     G-TY03-LTH = 60 + G-TY03-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TY03-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TY03)                                ADUTAB
                       LENGTH (G-TY03-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TY03-TABCR NOT = '00'                          DOT
           PERFORM     F92TZ THRU F92TZ-FN.                             ADUTAB
       F92TY-FN. EXIT.
      *N92TZ.    NOTE *** CLIENT AGE NOT FND ON TABLE *   *.
       F92TZ.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012770 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92TZ-FN. EXIT.
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
