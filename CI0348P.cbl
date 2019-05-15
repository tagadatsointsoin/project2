       IDENTIFICATION DIVISION.                                         CI0348
       PROGRAM-ID.  CI0348P.                                            CI0348
      *AUTHOR.         CALCULATE DCN NUMBER.                            CI0348
      *DATE-COMPILED.   09/08/14.                                       CI0348
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2007                          *ACOPYP
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
      *     COPR. 2007                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0348
       CONFIGURATION SECTION.                                           CI0348
       SOURCE-COMPUTER. IBM-370.                                        CI0348
       OBJECT-COMPUTER. IBM-370.                                        CI0348
       DATA DIVISION.                                                   CI0348
       WORKING-STORAGE SECTION.                                         CI0348
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *****************************************************************
      ** HOST VARIABLES FOR DB2 TABLES                                *
      *****************************************************************
      *!WF DSP=FD DSL=FD SEL=06 FOR=I DES=2 LEV=1 PLT=FD
       01                 FD06.                                         CI0348
            10            FD06-XDCNN  PICTURE  X(17)                    CI0348
                          VALUE                SPACE.                   CI0348
            10            FD06-DXTMSA PICTURE  X(26)                    CI0348
                          VALUE                SPACE.                   CI0348
            10            FD06-DXTMSF PICTURE  X(26)                    CI0348
                          VALUE                SPACE.                   CI0348
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
                        PICTURE X(66)                                   CI0348
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
       01 7-DB2-DXTMSA.                                                 AADA84
          05 7-DB2-DTGRG.                                               AADA84
             10 7-DB2-DTGCY.                                            AADA84
      *!WI pl=WD115                                                     AADA84
                15 7-DB2-DTGCC                                          AADA84
                        PICTURE 9(2).                                   CI0348
      *!WI pl=WD120                                                     AADA84
                15 7-DB2-DTGYY                                          AADA84
                        PICTURE 9(2).                                   CI0348
             10 7-DB2-FIL1       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD130                                                     AADA84
             10 7-DB2-DTGMM                                             AADA84
                        PICTURE 9(2).                                   CI0348
             10 7-DB2-FIL2       PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD140                                                     AADA84
             10 7-DB2-DTGDD                                             AADA84
                        PICTURE 9(2).                                   CI0348
          05 7-DB2-FIL3          PIC X  VALUE '-'.                      AADA84
      *!WI pl=WD150                                                     AADA84
          05 7-DB2-DTTHH                                                AADA84
                        PICTURE 9(2).                                   CI0348
          05 7-DB2-FIL4          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD160                                                     AADA84
          05 7-DB2-DTTMN                                                AADA84
                        PICTURE 9(2).                                   CI0348
          05 7-DB2-FIL5          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD170                                                     AADA84
          05 7-DB2-DTTSS                                                AADA84
                        PICTURE 9(2).                                   CI0348
          05 7-DB2-FIL6          PIC X  VALUE '.'.                      AADA84
      *!WI pl=WD180                                                     AADA84
          05 7-DB2-DTTNN                                                AADA84
                        PICTURE 9(6).                                   CI0348
      ******************************************************************
      **                WORKING STORAGE FOR CASE DOC DATE              *
      ******************************************************************
      *!WI
       01   7-CSTDTE
                        PICTURE X(8).                                   CI0348
       01 FILLER REDEFINES 7-CSTDTE.
      *!WI
          05 7-DTGCY
                        PICTURE 9(4).                                   CI0348
      *!WI
          05 7-DTGMM
                        PICTURE 9(2).                                   CI0348
      *!WI
          05 7-DTGDD
                        PICTURE 9(2).                                   CI0348
      *
      *!WI
       01 7-CASE-DTJUL
                        PICTURE 9(7).                                   CI0348
       01 FILLER REDEFINES 7-CASE-DTJUL.
      *!WI
          05 7-CASE-DTGCY
                        PICTURE 9(4).                                   CI0348
      *!WI
          05 7-CASE-DTJDD
                        PICTURE 9(3).                                   CI0348
      *HH & MN & SS TO CALCULATE THE DTJDS
      *!WI
       01 WS01-DTTHH
                        PICTURE 9(2).                                   CI0348
      *!WI
       01 WS01-DTTMN
                        PICTURE 9(2).                                   CI0348
      *!WI
       01 WS01-DTTSS
                        PICTURE 9(2).                                   CI0348
      *DECIMAL WHICH WILL BE CONVERTED TO HEXADECIMAL (15 DIGITS)
       01 WS01-DECIMAL PIC 9(16) VALUE ZEROES.
       01 FILLER REDEFINES WS01-DECIMAL.
          05 WS01-DTTYY    PIC 99.
      *!WI
          05 WS01-DTJDD
                        PICTURE 9(3).                                   CI0348
          05 WS01-DTJDS PIC 9(5).
      *!WI
          05 WS01-DTTNN
                        PICTURE 9(6).                                   CI0348
      *DECLARE THE INDEX USED TO CONVERT
       01 WS01-INDEX PIC 9(2) VALUE ZEROES.
      *DECLARE THE QUOTIENT USED TO CONVERT
       01 WS01-QUOTI PIC 9(15) VALUE ZEROES.
      *DECLARE THE REMAINDER USED TO CONVERT
      *!WI
       01 WS01-NRDAD
                        PICTURE 9(2).                                   CI0348
      *
      **----------------------------------------------------------------
      ** CASEDOC-ID GENERATED
      **----------------------------------------------------------------
      *
       01 WS01-NIOBJC.
          05 WS01-HEXT PIC X(13) VALUE 'OS00000000000'.
          05 FILLER        PIC X(4)    VALUE '.001'.
       01 DCN-INSERT       PIC 9   VALUE 0.
          88 DCN-INSERT-FAILED     VALUE 0.
          88 DCN-INSERT-SUCCESSFUL VALUE 1.
       01 WS-INDEX        PIC 99.
      ******************************************************************
      *DUMMY TIMESTAMP BELOW WILL BE INSERTED IN FD06 SO THAT IN CASE OF
      *MQ PUT FAILURE  ACTUAL TIMESTAMP IS NOT UPDATED BY CI0349. THIS
      *RECORD WILL BE PICKED BY DAILY JOB PKS6202 AND DELETED.
      ******************************************************************
      *!WI
       01 WS00-DXTMSA
                        PICTURE X(26)                                   CI0348
                            VALUE '0001-01-01-00.00.00.000000'.
      *
      *!WF DSP=WS DSL=DD SEL=01 FOR=I LEV=1 PLT=WS
       01                 WS00.                                         CI0348
          05              WS00-SUITE.                                   CI0348
            15       FILLER         PICTURE  X(00093).                  CI0348
       01                 WS01  REDEFINES      WS00.                    CI0348
            10            WS01-XDAT8.                                   CI0348
            11            WS01-XDATC  PICTURE  XX.                      CI0348
            11            WS01-XDATY  PICTURE  XX.                      CI0348
            11            WS01-XDATM  PICTURE  XX.                      CI0348
            11            WS01-XDATD  PICTURE  XX.                      CI0348
            10            WS01-XDAT8D                                   CI0348
                          REDEFINES            WS01-XDAT8               CI0348
               PICTURE    9(8).                                         CI0348
            10            WS01-XDAT81.                                  CI0348
            11            WS01-XDATM1 PICTURE  XX.                      CI0348
            11            WS01-XDATD1 PICTURE  XX.                      CI0348
            11            WS01-XDATC1 PICTURE  XX.                      CI0348
            11            WS01-XDATY1 PICTURE  XX.                      CI0348
            10            WS01-XDAT80                                   CI0348
                          REDEFINES            WS01-XDAT81              CI0348
               PICTURE    9(8).                                         CI0348
            10            WS01-XDAT62.                                  CI0348
            11            WS01-XDATM2 PICTURE  XX.                      CI0348
            11            WS01-XDATD2 PICTURE  XX.                      CI0348
            11            WS01-XDATY2 PICTURE  XX.                      CI0348
            10            WS01-XDAT69                                   CI0348
                          REDEFINES            WS01-XDAT62              CI0348
               PICTURE    9(6).                                         CI0348
            10            WS01-XDATCU.                                  CI0348
            11            WS01-XDATC9 PICTURE  99.                      CI0348
            11            WS01-XDAYMD.                                  CI0348
            12            WS01-XDATY9 PICTURE  99.                      CI0348
            12            WS01-XDAMD.                                   CI0348
            13            WS01-XDATM9 PICTURE  99.                      CI0348
            13            WS01-XDATD9 PICTURE  99.                      CI0348
            10            WS01-XDAT89 PICTURE  9(8).                    CI0348
            10            WS01-XDAJC  PICTURE  9(7).                    CI0348
            10            WS01-XDAJC1.                                  CI0348
            11            WS01-XDAJC9 PICTURE  99.                      CI0348
            11            WS01-XDAJY  PICTURE  99.                      CI0348
            11            WS01-XDAJN  PICTURE  999.                     CI0348
            10            WS01-XDAB   PICTURE  9(5).                    CI0348
            10            WS01-DD05.                                    CI0348
            11            WS01-XDACT  PICTURE  S9(3)                    CI0348
                          COMPUTATIONAL-3.                              CI0348
            11            WS01-XDACV  PICTURE  S9                       CI0348
                          COMPUTATIONAL-3.                              CI0348
            11            WS01-XDAGP  PICTURE  S9(9)                    CI0348
                          COMPUTATIONAL-3.                              CI0348
            11            WS01-XDAJP  PICTURE  S9(7)                    CI0348
                          COMPUTATIONAL-3.                              CI0348
            11            WS01-XDACV1 PICTURE  S9                       CI0348
                          COMPUTATIONAL-3.                              CI0348
            11            WS01-XDAGP1 PICTURE  S9(9)                    CI0348
                          COMPUTATIONAL-3.                              CI0348
            11            WS01-XDAJP1 PICTURE  S9(7)                    CI0348
                          COMPUTATIONAL-3.                              CI0348
            10            WS01-XW03.                                    CI0348
            11            WS01-XDATG.                                   CI0348
            12            WS01-XDAT1.                                   CI0348
            13            WS01-XDAT19 PICTURE  99.                      CI0348
            12            WS01-XDAT2.                                   CI0348
            13            WS01-XDAT29 PICTURE  99.                      CI0348
            12            WS01-XDAT3.                                   CI0348
            13            WS01-XDAT39 PICTURE  99.                      CI0348
            12            WS01-XDAT4.                                   CI0348
            13            WS01-XDAT49 PICTURE  99.                      CI0348
            11            WS01-XLEAPY PICTURE  99.                      CI0348
            11            WS01-DTGCY  PICTURE  9(4).                    CI0348
            11            WS01-FILLER                                   CI0348
                          REDEFINES            WS01-DTGCY.              CI0348
            12            WS01-DTGCC  PICTURE  9(2).                    CI0348
            12            WS01-DTGYY  PICTURE  9(2).                    CI0348
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA58
       01   DEBUT-WSS.                                                  CI0348
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0348
            05   IK     PICTURE X.                                      CI0348
       01  CONSTANTES-PAC.                                              CI0348
           05  FILLER  PICTURE X(87)   VALUE                            CI0348
                     '6015 CAT09/08/14CI0348ADMIN   14:35:21CI0348P AMERCI0348
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0348
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0348
           05  NUGNA   PICTURE X(5).                                    CI0348
           05  APPLI   PICTURE X(3).                                    CI0348
           05  DATGN   PICTURE X(8).                                    CI0348
           05  PROGR   PICTURE X(6).                                    CI0348
           05  CODUTI  PICTURE X(8).                                    CI0348
           05  TIMGN   PICTURE X(8).                                    CI0348
           05  PROGE   PICTURE X(8).                                    CI0348
           05  COBASE  PICTURE X(4).                                    CI0348
           05  DATGNC  PICTURE X(10).                                   CI0348
           05  RELEAS  PICTURE X(7).                                    CI0348
           05  DATGE   PICTURE X(10).                                   CI0348
           05  DATSQ   PICTURE X(10).                                   CI0348
       01  DATCE.                                                       CI0348
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0348
         05  DATOR.                                                     CI0348
           10  DATOA  PICTURE XX.                                       CI0348
           10  DATOM  PICTURE XX.                                       CI0348
           10  DATOJ  PICTURE XX.                                       CI0348
       01   VARIABLES-CONDITIONNELLES.                                  CI0348
            05                  FT      PICTURE X VALUE '0'.            CI0348
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0348
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0348
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0348
       LINKAGE SECTION.                                                 ADU102
      *****************************************************************
      **                PASS AREAS FROM CALLING MODULE
      *****************************************************************
      *!WF DSP=LK DSL=V2 SEL=19 FOR=I DES=1 LEV=1 PLT=10
       01                 LK19.                                         CI0348
            10            LK19-DXTMS2 PICTURE  X(26).                   CI0348
            10            LK19-NIOBJC PICTURE  X(17).                   CI0348
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0348
          05              MS00-SUITE.                                   CI0348
            15       FILLER         PICTURE  X(00542).                  CI0348
       01                 MS03  REDEFINES      MS00.                    CI0348
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0348
                          COMPUTATIONAL-3.                              CI0348
            10            MS03-CMSSF  PICTURE  XX.                      CI0348
            10            MS03-DU09.                                    CI0348
            11            MS03-CMESA  PICTURE  S9(9)                    CI0348
                          BINARY.                                       CI0348
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0348
                          BINARY.                                       CI0348
            11            MS03-CMESB  PICTURE  S9(9)                    CI0348
                          BINARY.                                       CI0348
            11            MS03-CMSST  PICTURE  S9(9)                    CI0348
                          BINARY.                                       CI0348
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0348
                          BINARY.                                       CI0348
            11            MS03-QELLAA PICTURE  S9(9)                    CI0348
                          BINARY.                                       CI0348
            11            MS03-TMESS4 PICTURE  X(512).                  CI0348
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0348
            10            MX11-QMSGS  PICTURE  9(03).                   CI0348
            10            MX11-PJ09                                     CI0348
                          OCCURS       025     TIMES.                   CI0348
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0348
                          COMPUTATIONAL-3.                              CI0348
            11            MX11-CMESB  PICTURE  S9(9)                    CI0348
                          BINARY.                                       CI0348
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                LK19
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0348
      *               *                                   *             CI0348
      *               *INITIALISATIONS                    *             CI0348
      *               *                                   *             CI0348
      *               *************************************.            CI0348
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0348
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0348
      *               *                                   *             CI0348
      *               *FIN DE TRAITEMENT                  *             CI0348
      *               *                                   *             CI0348
      *               *************************************.            CI0348
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0348
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING                    *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BB.    NOTE *INITIALIZATION                     *.
       F40BB.                                                           lv10
           INITIALIZE  FD06
           MOVE        10 TO WS-INDEX.
       F40BB-FN. EXIT.
      *N40BG.    NOTE *CALCULATE AND UPDATE DCN.          *.
       F40BG.    IF    WS-INDEX > 0                                     lv10
                 AND   DCN-INSERT-FAILED
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
      *TRY UNTIL SUCCESFUL OR
      *MAX ATTEMPTS REACHED.
      *CALCULATE DCN NUMBER
           PERFORM     F91 THRU F91-FN
           INITIALIZE  FD06
           MOVE        WS01-NIOBJC TO FD06-XDCNN
           MOVE        WS00-DXTMSA TO FD06-DXTMSA
      *INSERT DCN NUMBER INTO FD06
           PERFORM     F94VD THRU F94VD-FN.
      *N40BI.    NOTE *DCN INSERT SUCCESSFUL              *.
       F40BI.    IF    IK = 0                                           lv15
                 NEXT SENTENCE ELSE GO TO     F40BI-FN.
      *STOP THE LOOP
           MOVE        1 TO DCN-INSERT.
       F40BI-FN. EXIT.
      *N40BK.    NOTE *DCN INSERT NOT SUCCESSFUL          *.
       F40BK.    IF    IK = 1                                           lv15
                 AND   SQLCODE = -803
                 NEXT SENTENCE ELSE GO TO     F40BK-FN.
      *DUE TO DUPLICATE DCN INSERT
           SUBTRACT    1 FROM WS-INDEX
      *GET NEW TIMESTAMP
           PERFORM     F94TS THRU F94TS-FN.
       F40BK-FN. EXIT.
      *N40BP.    NOTE *DCN INSERT NOT SUCCESSFUL          *.
       F40BP.    IF    IK = 1                                           lv15
                 NEXT SENTENCE ELSE GO TO     F40BP-FN.
      *DUE TO OTHER REASONS
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015324 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BP-FN. EXIT.
       F40BG-900. GO TO F40BG.
       F40BG-FN. EXIT.
      *N40BT.    NOTE *DCN INSERT FAILED MAX ATTEMPTS     *.
       F40BT.    IF    WS-INDEX <= 0                                    lv10
                 AND   DCN-INSERT-FAILED
                 NEXT SENTENCE ELSE GO TO     F40BT-FN.
      *DUE TO DUPLICATE DCN
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015324 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BT-FN. EXIT.
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
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *GENERATE SOURCE DOC ID             *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91BG.    NOTE *TIMESTAMP NOT OKAY GET NEW         *.
       F91BG.    IF    (LK19-DXTMS2 = LOW-VALUES                        lv10
                 OR    SPACES)
                 OR    LK19-DXTMS2 (1:4) =
                       ZEROES OR SPACES
                 OR    LK19-DXTMS2 (6:2) =
                       ZEROES OR SPACES
                 OR    LK19-DXTMS2 (9:2) =
                       ZEROES OR SPACES
                 NEXT SENTENCE ELSE GO TO     F91BG-FN.
      *TIMESTAMP
           PERFORM     F94TS THRU F94TS-FN.
       F91BG-FN. EXIT.
      *N91CB.    NOTE *UNFORMAT TS                        *.
       F91CB.                                                           lv10
           MOVE        LK19-DXTMS2 TO 7-DB2-DXTMSA
           MOVE        7-DB2-DTGCY TO 7-DTGCY
           MOVE        7-DB2-DTGMM TO 7-DTGMM
           MOVE        7-DB2-DTGDD TO 7-DTGDD.
       F91CB-FN. EXIT.
      *N91DB.    NOTE *DETERMINE JULIAN DAY               *.
       F91DB.                                                           lv10
           MOVE        7-CSTDTE TO WS01-XDAGP
           MOVE        1 TO WS01-XDACT
           MOVE        1 TO WS01-XDACV
      *ADD AADA58 HERE.
      *CALL MWS100EX - DYNAMIC                                          DOT
           CALL        MWS100EX USING WS01-DD05.                        AADA58
       F91DB-FN. EXIT.
      *N91EB.    NOTE *CALCULATE THE HEXADECIMAL VALUE    *.
       F91EB.                                                           lv10
           MOVE        WS01-XDAJP TO 7-CASE-DTJUL
           MOVE        7-CASE-DTJDD TO WS01-DTJDD
           ADD         366 TO WS01-DTJDD
           MOVE        7-DB2-DTGYY TO WS01-DTTYY
           MOVE        7-DB2-DTTHH TO WS01-DTTHH
           MOVE        7-DB2-DTTMN TO WS01-DTTMN
           MOVE        7-DB2-DTTSS TO WS01-DTTSS
           COMPUTE     WS01-DTJDS = (WS01-DTTHH
           * 3600)
           + (WS01-DTTMN
           * 60)
           + WS01-DTTSS
           MOVE        7-DB2-DTTNN TO WS01-DTTNN
           MOVE        13 TO WS01-INDEX.
      *N91ED.    NOTE *CONVERT THE HEX TO DECIMAL         *.
       F91ED.    IF    (WS01-INDEX > 2)                                 lv15
                 NEXT SENTENCE ELSE GO TO     F91ED-FN.
      *STOP 2 BYTES BEFORE FOR 'OS'
      *PREFIX
           DIVIDE WS01-DECIMAL BY
           30
           GIVING WS01-QUOTI
           REMAINDER WS01-NRDAD
           MOVE        WS01-QUOTI TO WS01-DECIMAL.
      *N91EF.    NOTE *CONVERT THE REMAINDER              *.
       F91EF.         EXIT.                                             lv20
      *N91EH.    NOTE *REMAINDER IS LESS THAN 10          *.
       F91EH.    IF    WS01-NRDAD < 10                                  lv25
                 NEXT SENTENCE ELSE GO TO     F91EH-FN.
           MOVE        WS01-NRDAD (2:1) TO
           WS01-HEXT (WS01-INDEX:1).
       F91EH-900. GO TO F91EJ-FN.
       F91EH-FN. EXIT.
      *N91EJ.    NOTE *REMAINDER IS >= 10                 *.
       F91EJ.                                                           lv25
           EVALUATE    WS01-NRDAD
                 WHEN  10
           MOVE        'A' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  11
           MOVE        'B' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  12
           MOVE        'C' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  13
           MOVE        'D' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  14
           MOVE        'E' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  15
           MOVE        'F' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  16
           MOVE        'G' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  17
           MOVE        'H' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  18
           MOVE        'I' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  19
           MOVE        'J' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  20
           MOVE        'K' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  21
           MOVE        'L' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  22
           MOVE        'M' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  23
           MOVE        'N' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  24
           MOVE        'O' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  25
           MOVE        'P' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  26
           MOVE        'Q' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  27
           MOVE        'R' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  28
           MOVE        'S' TO WS01-HEXT (WS01-INDEX:1)
                 WHEN  29
           MOVE        'T' TO WS01-HEXT (WS01-INDEX:1)
           END-EVALUATE.
       F91EJ-FN. EXIT.
       F91EF-FN. EXIT.
      *N91EM.    NOTE *DECREMENT INDEX                    *.
       F91EM.                                                           lv20
           SUBTRACT    1 FROM WS01-INDEX.
       F91EM-FN. EXIT.
       F91ED-900. GO TO F91ED.
       F91ED-FN. EXIT.
       F91EB-FN. EXIT.
      *N91FB.    NOTE *BUILD DOC ID                       *.
       F91FB.                                                           lv10
           MOVE        WS01-NIOBJC TO LK19-NIOBJC.
       F91FB-FN. EXIT.
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
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *SQL STATEMENTS                     *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94TS.    NOTE *GET DB2 TIMESTAMP                  *.
       F94TS.                                                           lv10
           MOVE        'F94TS - SET' TO 7-DB2-FUNCT                     ADB230
           EXEC SQL    SET                                              ADB230
                       :LK19-DXTMS2 = CURRENT TIMESTAMP      END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB230
      *N94TV.    NOTE *TIMESTAMP CALL NOT SUCCESSFUL      *.
       F94TV.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F94TV-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015324 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F94TV-FN. EXIT.
       F94TS-FN. EXIT.
      *N94VD.    NOTE *INSERT DCN TO FD06                 *.
       F94VD.                                                           lv10
           MOVE        'F94VD - INSERT' TO 7-DB2-FUNCT                  ADB227
           EXEC SQL    INSERT                                           ADB227
                          INTO CORP.TBFD06
                        ( XDCNN,
                          DXTMSA )
                       VALUES
                        ( :FD06-XDCNN,
                          :FD06-DXTMSA )                     END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB227
       F94VD-FN. EXIT.
       F94-FN.   EXIT.
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
