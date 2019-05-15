       IDENTIFICATION DIVISION.                                         CI0290
       PROGRAM-ID.  CI0290P.                                            CI0290
      *AUTHOR.         OST MQ SEND.                                     CI0290
      *DATE-COMPILED.   09/08/14.                                       CI0290
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2000                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE FDC    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE FDC    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE FDC          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2000                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0290
       CONFIGURATION SECTION.                                           CI0290
       SOURCE-COMPUTER. IBM-370.                                        CI0290
       OBJECT-COMPUTER. IBM-370.                                        CI0290
       DATA DIVISION.                                                   CI0290
       WORKING-STORAGE SECTION.                                         CI0290
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *GENERATE INDEX FOR SEND AREA
      *                   SEND
      *STUFF FOR MQ
       01  MQM-OBJECT-DESCRIPTOR.
           COPY CMQODV.
       01  MQM-MESSAGE-DESCRIPTOR.
           COPY CMQMDV.
       01  MQM-PUT-MESSAGE-OPTIONS.
           COPY CMQPMOV.
       01  MQM-CONTROL-FIELDS.
           COPY CMQV.
       01  MQ-VARAIBLES.
       05  W03-OPTIONS                PIC S9(09) BINARY.
       05  W03-HCONN                  PIC S9(09) BINARY VALUE ZERO.
       05  W03-HOBJ-CHKQ-RESP         PIC S9(09) BINARY.
       05  W03-REASON                 PIC S9(09) BINARY.
       05  W03-COMPCODE               PIC S9(09) BINARY.
       05  W03-BUFFLEN                PIC S9(09) BINARY.
       01  M02-ERROR-MESSAGE.
       05  FILLER               PIC X(29) VALUE
           '***ERROR IN MQ PROCESSING - '.
       05  M02-OPERATION         PIC X(30).
       05  FILLER                PIC X(11) VALUE
           ' COMP CODE '.
       05  M02-COMPCODE          PIC Z(8)9.
       05  FILLER                PIC X(08) VALUE
           ' OBJECT '.
       05  M02-OBJECTNAME        PIC X(48).
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
      ******************************************************************
      *MQ PUT BUFFER - MAX 90,000 BYTES
      *AREA WHICH LOADED FROM IN93-THTML AND PUT IN MQ
      ******************************************************************
      *
       01  WQ-SEND-BEGIN.
           05  FILLER            PIC X(17) VALUE '***BEG PUT BUFFER'.
       01  WQ-SEND-AREA.
         05  WQ-SEND-LINE        OCCURS 450.
      *!WI
           10  WQ-SEND-THTML
                        PICTURE X(200).                                 CI0290
       01  WQ-SEND-END.
           05  FILLER            PIC X(17) VALUE '***END PUT BUFFER'.
      *WORK VARIABLES
       01  WS-MQ-Q-NAME           PIC X(40).
      *
       01   DEBUT-WSS.                                                  CI0290
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0290
            05   IK     PICTURE X.                                      CI0290
       01  CONSTANTES-PAC.                                              CI0290
           05  FILLER  PICTURE X(87)   VALUE                            CI0290
                     '6015 CAT09/08/14CI0290ADMIN   14:35:16CI0290P AMERCI0290
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0290
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0290
           05  NUGNA   PICTURE X(5).                                    CI0290
           05  APPLI   PICTURE X(3).                                    CI0290
           05  DATGN   PICTURE X(8).                                    CI0290
           05  PROGR   PICTURE X(6).                                    CI0290
           05  CODUTI  PICTURE X(8).                                    CI0290
           05  TIMGN   PICTURE X(8).                                    CI0290
           05  PROGE   PICTURE X(8).                                    CI0290
           05  COBASE  PICTURE X(4).                                    CI0290
           05  DATGNC  PICTURE X(10).                                   CI0290
           05  RELEAS  PICTURE X(7).                                    CI0290
           05  DATGE   PICTURE X(10).                                   CI0290
           05  DATSQ   PICTURE X(10).                                   CI0290
       01  DATCE.                                                       CI0290
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0290
         05  DATOR.                                                     CI0290
           10  DATOA  PICTURE XX.                                       CI0290
           10  DATOM  PICTURE XX.                                       CI0290
           10  DATOJ  PICTURE XX.                                       CI0290
       01   VARIABLES-CONDITIONNELLES.                                  CI0290
            05                  FT      PICTURE X VALUE '0'.            CI0290
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0290
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0290
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           ISENDL PICTURE S9(4) VALUE  ZERO.
            05           ISENDR PICTURE S9(4) VALUE  ZERO.
            05           ISENDM PICTURE S9(4) VALUE +0450.
            05           J40FFR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0290
       LINKAGE SECTION.                                                 ADU102
      ******************************************************************
      ** C A L L   M O D U L E   I N P U T  A R E A S                  *
      ******************************************************************
      *
      ******************************************************************
      ** COMBINED XML/HTML FROM CALLER                                 *
      ******************************************************************
      *!WF DSP=IN DSL=QT SEL=93 FOR=I LEV=1 PLT=00
       01                 IN00.                                         CI0290
          05              IN00-SUITE.                                   CI0290
            15       FILLER         PICTURE  X(90906).                  CI0290
       01                 IN93  REDEFINES      IN00.                    CI0290
            10            IN93-QBLCK  PICTURE  9(6).                    CI0290
            10            IN93-QT9O.                                    CI0290
            11            IN93-QT9B                                     CI0290
                          OCCURS       450     TIMES.                   CI0290
            12            IN93-CHTML  PICTURE  99.                      CI0290
            12            IN93-THTML  PICTURE  X(200).                  CI0290
      *
      ******************************************************************
      ** DESTINATION QUEUE                                             *
      ******************************************************************
      *
      *!WI
       01  LK-HTML-MDSN
                        PICTURE X(44).                                  CI0290
      *                                                                 ADU102
      ******************************************************************
      ** C A L L   M O D U L E   O U T P U T   A R E A S               *
      ******************************************************************
      *
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0290
          05              MS00-SUITE.                                   CI0290
            15       FILLER         PICTURE  X(00542).                  CI0290
       01                 MS03  REDEFINES      MS00.                    CI0290
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0290
                          COMPUTATIONAL-3.                              CI0290
            10            MS03-CMSSF  PICTURE  XX.                      CI0290
            10            MS03-DU09.                                    CI0290
            11            MS03-CMESA  PICTURE  S9(9)                    CI0290
                          BINARY.                                       CI0290
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0290
                          BINARY.                                       CI0290
            11            MS03-CMESB  PICTURE  S9(9)                    CI0290
                          BINARY.                                       CI0290
            11            MS03-CMSST  PICTURE  S9(9)                    CI0290
                          BINARY.                                       CI0290
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0290
                          BINARY.                                       CI0290
            11            MS03-QELLAA PICTURE  S9(9)                    CI0290
                          BINARY.                                       CI0290
            11            MS03-TMESS4 PICTURE  X(512).                  CI0290
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0290
            10            MX11-QMSGS  PICTURE  9(03).                   CI0290
            10            MX11-PJ09                                     CI0290
                          OCCURS       025     TIMES.                   CI0290
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0290
                          COMPUTATIONAL-3.                              CI0290
            11            MX11-CMESB  PICTURE  S9(9)                    CI0290
                          BINARY.                                       CI0290
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                IN93
                                LK-HTML-MDSN
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0290
      *               *                                   *             CI0290
      *               *INITIALISATIONS                    *             CI0290
      *               *                                   *             CI0290
      *               *************************************.            CI0290
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0290
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0290
      *               *                                   *             CI0290
      *               *FIN DE TRAITEMENT                  *             CI0290
      *               *                                   *             CI0290
      *               *************************************.            CI0290
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0290
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS INPUT PARAMETERS           *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CC.    NOTE *VERIFY THAT LINE COUNT IS VALID    *.
       F30CC.                                                           lv10
      *
                 IF    IN93-QBLCK NOT NUMERIC                           DOT
                 OR    IN93-QBLCK > 450
      *MAX IS 450 AND MUST BE
      *NUMERIC
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012449 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CC-FN. EXIT.
      *N30CE.    NOTE *USE MQ QUEUE NAME SENT BY CALLER   *.
       F30CE.                                                           lv10
      *
           MOVE        LK-HTML-MDSN TO WS-MQ-Q-NAME.
       F30CE-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *PREP FOR MQ PROCESSING             *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40CC.    NOTE *INITIALIZE                         *.
       F40CC.                                                           lv10
           INITIALIZE  WQ-SEND-AREA
           INITIALIZE  ISENDR.
       F40CC-FN. EXIT.
      *N40FF.    NOTE *LOOP THRU INPUT FOR THE NUMBER     *.
       F40FF.                                                           lv10
           MOVE        1                        TO J40FFR
                                    GO TO     F40FF-B.
       F40FF-A.
           ADD         1                        TO J40FFR.
       F40FF-B.
           IF          J40FFR                   >  IN93-QBLCK
                                    GO TO     F40FF-FN.
      *OF LINES SPECIFIED
      *ISENDR COUNTS ONLY LINES
      *DESTINED FOR IUI AND IS USED TO
      *CALC ACTUAL LEN OF MQ PUT BUFFER
      *
                 IF    IN93-CHTML (J40FFR) = '01'                       DOT
                 OR    IN93-CHTML (J40FFR) = '03'
      *MOVE TO OUTPUT AREA IF
      *DESTINATION INCLUDES IUI
           ADD         +1 TO ISENDR
           MOVE        IN93-THTML (J40FFR) TO
           WQ-SEND-THTML (ISENDR).
       F40FF-900. GO TO F40FF-A.
       F40FF-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *MQ PROCESSING                      *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50DD.    NOTE *MQOPEN                             *.
       F50DD.                                                           lv10
           MOVE        MQOT-Q TO MQOD-OBJECTTYPE
           MOVE        WS-MQ-Q-NAME TO MQOD-OBJECTNAME
           COMPUTE     W03-OPTIONS = MQOO-OUTPUT +
           MQOO-FAIL-IF-QUIESCING
           CALL        'MQOPEN' USING W03-HCONN
           MQOD
           W03-OPTIONS
           W03-HOBJ-CHKQ-RESP
           W03-COMPCODE
           W03-REASON.
                 IF    W03-COMPCODE NOT = MQCC-OK                       DOT
      *CHECK RETURN CODE FROM OPEN
           MOVE        'MQOPEN' TO M02-OPERATION
           MOVE        WS-MQ-Q-NAME TO M02-OBJECTNAME
           MOVE        W03-REASON TO M02-COMPCODE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014170 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN                              ADU119
           MOVE        135 TO MS03-QELLAA
           MOVE        M02-ERROR-MESSAGE TO MS03-TMESS4
           MOVE                     ALL '1' TO FT GO TO F20.
       F50DD-FN. EXIT.
      *N50GG.    NOTE *MQPUT                              *.
       F50GG.                                                           lv10
           MOVE        MQMI-NONE TO MQMD-MSGID
           MOVE        MQCI-NONE TO MQMD-CORRELID
           MOVE        W03-HOBJ-CHKQ-RESP TO MQPMO-CONTEXT
           MOVE        MQFMT-STRING TO MQMD-FORMAT
           MOVE        MQCCSI-Q-MGR TO MQMD-CODEDCHARSETID
           MOVE        MQPER-PERSISTENT TO
           MQMD-PERSISTENCE
           COMPUTE     W03-BUFFLEN = ISENDR * 200
           CALL        'MQPUT' USING W03-HCONN
           W03-HOBJ-CHKQ-RESP
           MQMD
           MQPMO
           W03-BUFFLEN
           WQ-SEND-AREA
           W03-COMPCODE
           W03-REASON.
                 IF    W03-COMPCODE NOT = MQCC-OK                       DOT
      *CHECK RETURN CODE FROM PUT
           MOVE        'MQPUT' TO M02-OPERATION
           MOVE        WS-MQ-Q-NAME TO M02-OBJECTNAME
           MOVE        W03-REASON TO M02-COMPCODE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014171 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN                              ADU119
           MOVE        135 TO MS03-QELLAA
           MOVE        M02-ERROR-MESSAGE TO MS03-TMESS4
           MOVE                     ALL '1' TO FT GO TO F20.
       F50GG-FN. EXIT.
      *N50JJ.    NOTE *MQCLOSE                            *.
       F50JJ.                                                           lv10
           CALL        'MQCLOSE' USING W03-HCONN
           W03-HOBJ-CHKQ-RESP
           MQCO-NONE
           W03-COMPCODE
           W03-REASON.
                 IF    W03-COMPCODE NOT = MQCC-OK                       DOT
      *CHECK RETURN CODE FROM CLOSE
           MOVE        'MQCLOSE' TO M02-OPERATION
           MOVE        WS-MQ-Q-NAME TO M02-OBJECTNAME
           MOVE        W03-REASON TO M02-COMPCODE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        014172 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN                              ADU119
           MOVE        135 TO MS03-QELLAA
           MOVE        M02-ERROR-MESSAGE TO MS03-TMESS4
           MOVE                     ALL '1' TO FT GO TO F20.
       F50JJ-FN. EXIT.
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
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *CALLED ROUTINES                    *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
       F91-FN.   EXIT.
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
