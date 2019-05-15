       IDENTIFICATION DIVISION.                                         ACCMER
       PROGRAM-ID.  ACCMER.                                             ACCMER
      *AUTHOR.         EXTRACTION OF groupbill arrg.                    ACCMER
      *DATE-COMPILED.   09/08/14.                                       ACCMER
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2010     tagada                *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CAMS SYSTEM AND ALL INFORMATION RELATING THERETO,    *  ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CAMS SYSTEM AND ALL            *  ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CAMS       *  ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2010                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN� 55474  *ACOPYP
      ******************************************************************ACOPYP
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT -                                          $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            ACCMER
       CONFIGURATION SECTION.                                           ACCMER
       SOURCE-COMPUTER. IBM-370.                                        ACCMER
       OBJECT-COMPUTER. IBM-370.                                        ACCMER
       INPUT-OUTPUT SECTION.                                            ACCMER
       FILE-CONTROL.                                                    ACCMER
            SELECT     IN-FICHIER    ASSIGN    UT-S-INPUT1.             ACCMER
            SELECT     IP-FICHIER    ASSIGN    UT-S-INPUT2.             ACCMER
            SELECT     OP-FICHIER    ASSIGN    UT-S-OUTPUT.             ACCMER
       DATA DIVISION.                                                   ACCMER
       FILE SECTION.                                                    ACCMER
       FD                 IN-FICHIER                                    ACCMER
            BLOCK              00000 RECORDS                            ACCMER
            RECORDING  F.                                               ACCMER
       01                 IN00.                                         ACCMER
            10            IN00-NAIDC  PICTURE  9(12).                   ACCMER
            10            IN00-CLID   PICTURE  X(23).                   ACCMER
            10            IN00-CLORN  PICTURE  X(45).                   ACCMER
            10            IN00-GESAD1 PICTURE  X(30).                   ACCMER
            10            IN00-GESAD2 PICTURE  X(30).                   ACCMER
            10            IN00-GESAD3 PICTURE  X(30).                   ACCMER
            10            IN00-GESAD4 PICTURE  X(30).                   ACCMER
            10            IN00-TCARR  PICTURE  X(6).                    ACCMER
            10            IN00-CBLTY  PICTURE  X(3).                    ACCMER
            10            IN00-CPMTCA PICTURE  XXX.                     ACCMER
            10            IN00-CTID   PICTURE  X(27).                   ACCMER
            10            IN00-CTSTAU PICTURE  X(10).                   ACCMER
            10            IN00-CQACT6 PICTURE  X(4).                    ACCMER
            10            IN00-CIRAS5 PICTURE  X(15).                   ACCMER
            10            IN00-CRFEE  PICTURE  X(10).                   ACCMER
            10            IN00-CCTET  PICTURE  X(20).                   ACCMER
            10            IN00-MPMTF  PICTURE  X(14).                   ACCMER
            10            IN00-DNPMT1 PICTURE  9(8).                    ACCMER
            10            IN00-DLBPD  PICTURE  9(8).                    ACCMER
            10            IN00-DNBPD  PICTURE  9(8).                    ACCMER
            10            IN00-APMTI  PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            IN00-PPALL  PICTURE  S9(3)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            IN00-AEMAX2 PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            IN00-AEMCT1 PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            IN00-IECNT  PICTURE  X.                       ACCMER
            10            IN00-MBCNM  PICTURE  X(30).                   ACCMER
            10            IN00-NPHNC  PICTURE  X(14).                   ACCMER
            10            IN00-NPHND1 PICTURE  X(14).                   ACCMER
       FD                 IP-FICHIER                                    ACCMER
            BLOCK              00000 RECORDS                            ACCMER
            RECORDING  F.                                               ACCMER
       01                 IP00.                                         ACCMER
            10            IP00-CTID1  PICTURE  X(27).                   ACCMER
            10            IP00-MIPPS  PICTURE  X(4).                    ACCMER
            10            IP00-CT01.                                    ACCMER
            11            IP00-CT01K.                                   ACCMER
            12            IP00-C299.                                    ACCMER
            13            IP00-CTID.                                    ACCMER
            14            IP00-CTIDA  PICTURE  9(3).                    ACCMER
            14            IP00-CTIDN.                                   ACCMER
            15            IP00-CTIDNP PICTURE  X(13).                   ACCMER
            15            IP00-CTIDND PICTURE  9(11).                   ACCMER
            11            IP00-GECKD  PICTURE  9.                       ACCMER
            11            IP00-GEMDA  PICTURE  9(8).                    ACCMER
            11            IP00-NSEQ4B PICTURE  9(8)                     ACCMER
                          BINARY.                                       ACCMER
            11            IP00-GECUC  PICTURE  99.                      ACCMER
            11            IP00-CTAUL  PICTURE  9(3).                    ACCMER
            11            IP00-DIRAC  PICTURE  9(4).                    ACCMER
            11            IP00-CTCCI  PICTURE  X.                       ACCMER
            11            IP00-CTCUS  PICTURE  999.                     ACCMER
            11            IP00-CTEFD  PICTURE  9(8).                    ACCMER
            11            IP00-CTIAD  PICTURE  9(8).                    ACCMER
            11            IP00-CLCUS  PICTURE  99.                      ACCMER
            11            IP00-CAMMB  PICTURE  X(3).                    ACCMER
            11            IP00-CKPMM  PICTURE  X.                       ACCMER
            11            IP00-CTLAD  PICTURE  9(8).                    ACCMER
            11            IP00-IPERS  PICTURE  X.                       ACCMER
            11            IP00-AUNCB  PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            11            IP00-CTLAT  PICTURE  9(8).                    ACCMER
            11            IP00-CTLATC PICTURE  9(6).                    ACCMER
            11            IP00-IMEGA  PICTURE  X.                       ACCMER
            11            IP00-DIRAB  PICTURE  9(8).                    ACCMER
            11            IP00-COLRQ  PICTURE  X.                       ACCMER
            11            IP00-ZDA04  PICTURE  X(4).                    ACCMER
            11            IP00-CTLPD  PICTURE  9(8).                    ACCMER
            11            IP00-CIRASP PICTURE  9.                       ACCMER
            11            IP00-CIRATP PICTURE  99.                      ACCMER
            11            IP00-DRTHC  PICTURE  9(8).                    ACCMER
            11            IP00-CPPTC  PICTURE  X.                       ACCMER
            11            IP00-ZDA06  PICTURE  X(6).                    ACCMER
            11            IP00-CTACD  PICTURE  9(8).                    ACCMER
            11            IP00-CTNLI  PICTURE  X.                       ACCMER
            11            IP00-CTRHO  PICTURE  9(8).                    ACCMER
            11            IP00-CTSGD  PICTURE  9(8).                    ACCMER
            11            IP00-CPATP  PICTURE  X(1).                    ACCMER
            11            IP00-IRSTA  PICTURE  X.                       ACCMER
            11            IP00-CTSTA  PICTURE  99.                      ACCMER
            11            IP00-CTSSC  PICTURE  99.                      ACCMER
            11            IP00-PRLIN  PICTURE  9(3).                    ACCMER
            11            IP00-PRCOD  PICTURE  9(5).                    ACCMER
            11            IP00-PRSCD  PICTURE  X(9).                    ACCMER
            11            IP00-CTLNI  PICTURE  X.                       ACCMER
            11            IP00-AYSIDA PICTURE  9(3).                    ACCMER
            11            IP00-AYSID  PICTURE  9(5).                    ACCMER
            11            IP00-CTBMC  PICTURE  99.                      ACCMER
            11            IP00-CINAR  PICTURE  99.                      ACCMER
            11            IP00-CPHTR  PICTURE  X.                       ACCMER
            11            IP00-CDSTR  PICTURE  XX.                      ACCMER
            11            IP00-CQACT  PICTURE  999.                     ACCMER
            11            IP00-CIRAS  PICTURE  999.                     ACCMER
            11            IP00-CIRAT  PICTURE  999.                     ACCMER
            11            IP00-CLRAY  PICTURE  9(5).                    ACCMER
            11            IP00-CATTP  PICTURE  X.                       ACCMER
       FD                 OP-FICHIER                                    ACCMER
            BLOCK              00000 RECORDS                            ACCMER
            RECORDING  F.                                               ACCMER
       01                 OP00.                                         ACCMER
            10            OP00-NAIDC  PICTURE  9(12).                   ACCMER
            10            OP00-CLID   PICTURE  X(23).                   ACCMER
            10            OP00-CLORN  PICTURE  X(45).                   ACCMER
            10            OP00-GESAD1 PICTURE  X(30).                   ACCMER
            10            OP00-GESAD2 PICTURE  X(30).                   ACCMER
            10            OP00-GESAD3 PICTURE  X(30).                   ACCMER
            10            OP00-GESAD4 PICTURE  X(30).                   ACCMER
            10            OP00-TCARR  PICTURE  X(6).                    ACCMER
            10            OP00-CBLTY  PICTURE  X(3).                    ACCMER
            10            OP00-CPMTCA PICTURE  XXX.                     ACCMER
            10            OP00-CTID   PICTURE  X(27).                   ACCMER
            10            OP00-CTSTAU PICTURE  X(10).                   ACCMER
            10            OP00-CQACT6 PICTURE  X(4).                    ACCMER
            10            OP00-CIRAS5 PICTURE  X(15).                   ACCMER
            10            OP00-CRFEE  PICTURE  X(10).                   ACCMER
            10            OP00-CCTET  PICTURE  X(20).                   ACCMER
            10            OP00-MPMTF  PICTURE  X(14).                   ACCMER
            10            OP00-DNPMT1 PICTURE  9(8).                    ACCMER
            10            OP00-DLBPD  PICTURE  9(8).                    ACCMER
            10            OP00-DNBPD  PICTURE  9(8).                    ACCMER
            10            OP00-APMTI  PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            OP00-PPALL  PICTURE  S9(3)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            OP00-AEMAX2 PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            OP00-AEMCT1 PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10            OP00-IECNT  PICTURE  X.                       ACCMER
            10            OP00-MBCNM  PICTURE  X(30).                   ACCMER
            10            OP00-NPHNC  PICTURE  X(14).                   ACCMER
            10            OP00-NPHND1 PICTURE  X(14).                   ACCMER
       WORKING-STORAGE SECTION.                                         ACCMER
       01  7-TIME-AREAS.                                                AAPR10
           05  7-TIME-1TIME.                                            AAPR10
             08  7-TIME-9TIME.                                          AAPR10
               10  7-TIME-9HH    PIC 99.                                AAPR10
               10  7-TIME-9MM    PIC 99.                                AAPR10
               10  7-TIME-9SS    PIC 99.                                AAPR10
             08  7-TIME-9CC    PIC 99.                                  AAPR10
           05  7-TIME-0TIME.                                            AAPR10
               10  7-TIME-09HH    PIC 99.                               AAPR10
               10  FILLER         PIC X VALUE ':'.                      AAPR10
               10  7-TIME-09MM    PIC 99.                               AAPR10
       01   DEBUT-WSS.                                                  ACCMER
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   ACCMER
            05   IK     PICTURE X.                                      ACCMER
       01  CONSTANTES-PAC.                                              ACCMER
           05  FILLER  PICTURE X(87)   VALUE                            ACCMER
                     '9999 CAT09/08/14ACCMERADMIN   19:28:27ACCMER  BVAPACCMER
      -    '09/08/20143.5 V0419/02/201425/02/2014'.                     ACCMER
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     ACCMER
           05  NUGNA   PICTURE X(5).                                    ACCMER
           05  APPLI   PICTURE X(3).                                    ACCMER
           05  DATGN   PICTURE X(8).                                    ACCMER
           05  PROGR   PICTURE X(6).                                    ACCMER
           05  CODUTI  PICTURE X(8).                                    ACCMER
           05  TIMGN   PICTURE X(8).                                    ACCMER
           05  PROGE   PICTURE X(8).                                    ACCMER
           05  COBASE  PICTURE X(4).                                    ACCMER
           05  DATGNC  PICTURE X(10).                                   ACCMER
           05  RELEAS  PICTURE X(7).                                    ACCMER
           05  DATGE   PICTURE X(10).                                   ACCMER
           05  DATSQ   PICTURE X(10).                                   ACCMER
       01  DATCE.                                                       ACCMER
         05  CENTUR   PICTURE XX   VALUE '20'.                          ACCMER
         05  DATOR.                                                     ACCMER
           10  DATOA  PICTURE XX.                                       ACCMER
           10  DATOM  PICTURE XX.                                       ACCMER
           10  DATOJ  PICTURE XX.                                       ACCMER
       01  DAT6.                                                        ACCMER
            10 DAT61.                                                   ACCMER
            15 DAT619  PICTURE 99.                                      ACCMER
            10 DAT62.                                                   ACCMER
            15 DAT629  PICTURE 99.                                      ACCMER
            10 DAT63   PICTURE XX.                                      ACCMER
       01  DAT8.                                                        ACCMER
            10 DAT81   PICTURE XX.                                      ACCMER
            10 DAT8S1  PICTURE X.                                       ACCMER
            10 DAT82   PICTURE XX.                                      ACCMER
            10 DAT8S2  PICTURE X.                                       ACCMER
            10 DAT83   PICTURE XX.                                      ACCMER
       01  DATSEP     PICTURE X VALUE '/'.                              ACCMER
       01  DATSEW     PICTURE X.                                        ACCMER
       01  DAT-TRANS.                                                   ACCMER
         05  DAT-CTYD   PICTURE XX VALUE '61'.                          ACCMER
       01   VARIABLES-CONDITIONNELLES.                                  ACCMER
            05              RTD.                                        ACCMER
              10            RTD1     PICTURE X VALUE '1'.               ACCMER
            05            NRD        PICTURE 9 VALUE 1.                 ACCMER
            05            NRP        PICTURE 9 VALUE ZERO.              ACCMER
            05              RTP.                                        ACCMER
              10            RTP1     PICTURE X VALUE '1'.               ACCMER
            05            IN-DE.                                        ACCMER
              10          IN-DE1     PICTURE X VALUE '1'.               ACCMER
            05            IN-PE.                                        ACCMER
              10          IN-PE1     PICTURE X VALUE '1'.               ACCMER
            05              VCF.                                        ACCMER
              10          IN-CF.                                        ACCMER
                15        IN-CF1     PICTURE X VALUE '1'.               ACCMER
              10          IP-CF.                                        ACCMER
                15        IP-CF1     PICTURE X VALUE '1'.               ACCMER
            05               FT.                                        ACCMER
              10          IN-FT      PICTURE X VALUE '0'.               ACCMER
              10          IP-FT      PICTURE X VALUE '0'.               ACCMER
            05               FI.                                        ACCMER
              10          IN-FI      PICTURE X VALUE '0'.               ACCMER
       01   INDICES  COMPUTATIONAL  SYNC.                               ACCMER
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              ACCMER
       01   ZONES-CALCUL-CF.                                            ACCMER
           05               IND.                                        ACCMER
            18             TIND1.                                       ACCMER
            19              IND1     PICTURE X(027).                    ACCMER
           05             ININD.                                        ACCMER
            10            ININD1.                                       ACCMER
             15        IN-IN-CTID    PICTURE   X(27).                   ACCMER
           05             IPIND.                                        ACCMER
            10            IPIND1.                                       ACCMER
             15        IP-IN-CTID1   PICTURE   X(27).                   ACCMER
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   ACCMER
            05       5-IN00-CPTENR PICTURE S9(9) VALUE ZERO.            ACCMER
            05       5-IP00-CPTENR PICTURE S9(9) VALUE ZERO.            ACCMER
            05       5-OP00-CPTENR PICTURE S9(9) VALUE ZERO.            ACCMER
       01               1-IN00.                                         ACCMER
            10          1-IN00-NAIDC  PICTURE  9(12).                   ACCMER
            10          1-IN00-CLID   PICTURE  X(23).                   ACCMER
            10          1-IN00-CLORN  PICTURE  X(45).                   ACCMER
            10          1-IN00-GESAD1 PICTURE  X(30).                   ACCMER
            10          1-IN00-GESAD2 PICTURE  X(30).                   ACCMER
            10          1-IN00-GESAD3 PICTURE  X(30).                   ACCMER
            10          1-IN00-GESAD4 PICTURE  X(30).                   ACCMER
            10          1-IN00-TCARR  PICTURE  X(6).                    ACCMER
            10          1-IN00-CBLTY  PICTURE  X(3).                    ACCMER
            10          1-IN00-CPMTCA PICTURE  XXX.                     ACCMER
            10          1-IN00-CTID   PICTURE  X(27).                   ACCMER
            10          1-IN00-CTSTAU PICTURE  X(10).                   ACCMER
            10          1-IN00-CQACT6 PICTURE  X(4).                    ACCMER
            10          1-IN00-CIRAS5 PICTURE  X(15).                   ACCMER
            10          1-IN00-CRFEE  PICTURE  X(10).                   ACCMER
            10          1-IN00-CCTET  PICTURE  X(20).                   ACCMER
            10          1-IN00-MPMTF  PICTURE  X(14).                   ACCMER
            10          1-IN00-DNPMT1 PICTURE  9(8).                    ACCMER
            10          1-IN00-DLBPD  PICTURE  9(8).                    ACCMER
            10          1-IN00-DNBPD  PICTURE  9(8).                    ACCMER
            10          1-IN00-APMTI  PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10          1-IN00-PPALL  PICTURE  S9(3)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10          1-IN00-AEMAX2 PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10          1-IN00-AEMCT1 PICTURE  S9(7)V99                 ACCMER
                          COMPUTATIONAL-3.                              ACCMER
            10          1-IN00-IECNT  PICTURE  X.                       ACCMER
            10          1-IN00-MBCNM  PICTURE  X(30).                   ACCMER
            10          1-IN00-NPHNC  PICTURE  X(14).                   ACCMER
            10          1-IN00-NPHND1 PICTURE  X(14).                   ACCMER
       01   ZONES-UTILISATEUR PICTURE X.                                ACCMER
       PROCEDURE DIVISION.
      *N0BBA.    NOTE *DISPLAY AT THE BEGINNING           *.            AAPR10
       F0BBA.                                                           lv10
      *!ADT                                                             AAPR10
           ACCEPT DATOR FROM DATE                                       ACCMER
           IF DATOA < DAT-CTYD MOVE '20' TO CENTUR END-IF               ACCMER
           DISPLAY     '******************************'                 AAPR10
           DISPLAY     'BEGINNING OF PROGRAM : ' PROGR                  AAPR10
           DISPLAY     'DATE OF EXECUTION    : ' DATCE                  AAPR10
           DISPLAY     'LIBRARY              : ' LIBRA                  AAPR10
           DISPLAY     'SESSION NUMBER       : ' SESSI                  AAPR10
           DISPLAY     'GENERATED ON         : ' DATGN                  AAPR10
           DISPLAY     '   "      BY         : ' USERCO                 AAPR10
           DISPLAY     '------------------------------'                 AAPR10
           ACCEPT      7-TIME-1TIME FROM TIME                           AAPR10
           MOVE        7-TIME-9HH TO 7-TIME-09HH                        AAPR10
           MOVE        7-TIME-9MM TO 7-TIME-09MM                        AAPR10
           DISPLAY     'TIME AT BEG OF JOB   : '                        AAPR50
           7-TIME-9TIME.                                                AAPR50
       F0BBA-FN. EXIT.
      *N01.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *INITIALISATIONS                    *             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
       F01.      EXIT.
      *N01IN.    NOTE *INITIALISATION FICHIER  IN-FICHIER *.            ACCMER
       F01IN.    OPEN INPUT                    IN-FICHIER.
       F01IN-10. READ     IN-FICHIER       AT END
                 MOVE  1 TO                    IN-FI.                   ACCMER
       F01IN-FN. EXIT.
      *N01IP.    NOTE *INITIALISATION FICHIER  IP-FICHIER *.            ACCMER
       F01IP.    OPEN INPUT                    IP-FICHIER.
       F01IP-FN. EXIT.
      *N01OP.    NOTE *INITIALISATION FICHIER  OP-FICHIER *.            ACCMER
       F01OP.    OPEN OUTPUT                   OP-FICHIER.
       F01OP-FN. EXIT.
       F01-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            ACCMER
       F05.
      *N05.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *LECTURE FICHIERS ACCES SEQ. SANS DE*             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
      *N05IP.    NOTE *LECTURE FICHIER         IP  SANS DE*.            ACCMER
       F05IP.    IF         RTD1   = '1'   AND IP-CF1 = '1'
                 NEXT SENTENCE ELSE GO TO     F05IP-FN.                 ACCMER
       F05IP-10. READ     IP-FICHIER       AT END
                 MOVE  1 TO                    IP-FT                    ACCMER
                 MOVE HIGH-VALUE TO            IPIND                    ACCMER
                 GO TO   F05IP-FN.                                      ACCMER
                 MOVE     IP00-CTID1  TO      IP-IN-CTID1.              ACCMER
                 ADD 1 TO 5-IP00-CPTENR.                                ACCMER
       F05IP-FN. EXIT.
       F05-FN.   EXIT.
      *N10.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *LECTURE FICHIERS ACCES SEQ. AVEC DE*             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
       F10.      EXIT.
      *N10IN.    NOTE *LECTURE FICHIER         IN  AVEC DE*.            ACCMER
       F10IN.    IF       IN-CF1      = '1'
                 NEXT SENTENCE ELSE GO TO     F10IN-FN.                 ACCMER
       F10IN-10. MOVE     IN-DE        TO      IN-PE.
                 IF       IN-FI      = '1'                              ACCMER
                 MOVE HIGH-VALUE TO            ININD                    ACCMER
                 MOVE 1 TO  IN-FT     GO TO F10IN-FN.                   ACCMER
                 MOVE     IN00       TO      1-IN00.                    ACCMER
                 MOVE     IN00-CTID   TO      IN-IN-CTID                ACCMER
                 ADD 1 TO 5-IN00-CPTENR.                                ACCMER
                 READ     IN-FICHIER       AT END                       ACCMER
                 MOVE  1 TO                    IN-FI.                   ACCMER
       F10IN-FN. EXIT.
       F10-FN.   EXIT.
      *N20.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *FIN DE TRAITEMENT                  *             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   ACCMER
       F20IN.    CLOSE    IN-FICHIER.
       F20IN-FN. EXIT.
       F20IP.    CLOSE    IP-FICHIER.
       F20IP-FN. EXIT.
       F20OP.    CLOSE    OP-FICHIER.
       F20OP-FN. EXIT.
      *N2096.    NOTE *DISPLAY OF THE COUNTERS            *.            AAPR10
       F2096.                                                           lv10
      *DISPLAY THE COUNTER OF IN00                                      AAPR20
           DISPLAY     'NB OF RECORDS FOR IN00 : '                      AAPR20
           5-IN00-RECCNT                                                AAPR20
      *DISPLAY THE COUNTER OF IP00                                      AAPR20
           DISPLAY     'NB OF RECORDS FOR IP00 : '                      AAPR20
           5-IP00-RECCNT                                                AAPR20
      *DISPLAY THE COUNTER OF OP00                                      AAPR20
           DISPLAY     'NB OF RECORDS FOR OP00 : '                      AAPR20
           5-OP00-RECCNT.                                               AAPR20
       F2096-FN. EXIT.
      *N2097.    NOTE *DISPLAY AT THE END                 *.            AAPR10
       F2097.                                                           lv10
           ACCEPT      7-TIME-1TIME FROM TIME                           AAPR50
           DISPLAY     '------------------------------'                 AAPR10
           DISPLAY     'TIME AT END OF PROG  : '                        AAPR50
           7-TIME-9TIME                                                 AAPR50
           DISPLAY     'END OF PROGRAM       : ' PROGR                  AAPR10
           DISPLAY     '******************************'.                AAPR10
       F2097-FN. EXIT.
       F2099.     GOBACK.
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N22.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *CALCUL DES DERNIERS ENREGISTREMENTS*             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
       F22.      EXIT.
      *N22IN.    NOTE *CALCUL DE SUR FICHIER   IN-FICHIER *.            ACCMER
       F22IN.    MOVE ZERO   TO                IN-DE.
                 IF       IN-FI = '1' GO TO   F22IN-1.                  ACCMER
                 IF       IN00-CTID   NOT =  1-IN00-CTID                ACCMER
                 GO  TO                       F22IN-1.                  ACCMER
                 GO TO   F22IN-FN.                                      ACCMER
       F22IN-1.  MOVE    1  TO                 IN-DE1.
       F22IN-FN. EXIT.
       F22-FN.   EXIT.
      *N24.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *CALCUL CONFIGURATIONS   OCCURRENCES*             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
       F24.      MOVE  ZERO TO VCF  MOVE HIGH-VALUE TO IND.
               IF TIND1 > ININD      MOVE ININD TO IND.                 ACCMER
               IF TIND1 > IPIND      MOVE IPIND TO IND.                 ACCMER
       F24IN.    IF       ININD1     =           IND1
                 MOVE    1  TO                 IN-CF1.                  ACCMER
       F24IN-FN. EXIT.
       F24IP.    IF       IPIND1     =           IND1
                 MOVE    1  TO                 IP-CF1.                  ACCMER
       F24IP-FN. EXIT.
       F24-FN.   EXIT.
      *N26.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *CALCUL DES RUPTURES TOTALES        *             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
       F26.      MOVE RTD TO RTP. MOVE ZERO TO RTD.
                 MOVE NRD TO NRP. MOVE ZERO TO NRD.                     ACCMER
                 IF       IN-CF1    = '0'  OR  IN-DE1 = '1'             ACCMER
                 MOVE          1   TO NRD GO TO F26-1.                  ACCMER
                 GO TO     F26-FN.                                      ACCMER
       F26-1.    MOVE 1 TO  RTD1.
       F26-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *THE PROGRAM MATCHES THE DCA        *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *ARRANGEMENT ACCOUNT AGAINST THE
      *THE XREF FILE TO FILTER THE
      *DCA ARRANGEMENTS ON CONVERTING
      *ACCOUNTS.
      *N40AA.    NOTE *INITIALIZING THE SEGMENT           *.
       F40AA.                                                           lv10
      *********************************
           INITIALIZE  OP00.
       F40AA-FN. EXIT.
      *N40AD.    NOTE *MATCHING                           *.
       F40AD.    IF    IP-CF1 = '1'                                     lv10
                 AND   IN-CF1 = '1'
                 NEXT SENTENCE ELSE GO TO     F40AD-FN.
      *********************************
           MOVE        IN0O TO OP00
           MOVE        IP00-CTID TO OP00-CTID
      *********************************
      *WRITING TO THE OUTPUT FILE
      *********************************
                 IF    IP00-CTSTA = '00'                                DOT
           MOVE        'UNKNOWN' TO OP00-CTSTAU.
                 IF    IP00-CTSTA = '01'                                DOT
           MOVE        'PENDING' TO OP00-CTSTAU.
                 IF    IP00-CTSTA = '02'                                DOT
           MOVE        'ACTIVE' TO OP00-CTSTAU.
                 IF    IP00-CTSTA = '03'                                DOT
           MOVE        'INCATIVE' TO OP00-CTSTAU.
           MOVE        IP00-CQACT TO OP00-CQACT                         DOT
           MOVE        IP00-CIRAS TO OP00-CIRAS
           MOVE        IP00-CIRAT TO OP00-CIRAT
           PERFORM     F90OP THRU F90OP-FN.
       F40AD-FN. EXIT.
      *N40AF.    NOTE *MATCHING                           *.
       F40AF.    IF    IP-CF1 = '0'                                     lv10
                 AND   IN-CF1 = '1'
                 NEXT SENTENCE ELSE GO TO     F40AF-FN.
      *********************************
           MOVE        IP00-CTID TO OP00-CTID
      *********************************
           DISPLAY     1-IN00-CTID.
       F40AF-FN. EXIT.
      *N40AG.    NOTE *READ NEXT RECORD                   *.
       F40AG.                                                           lv10
           GO TO F05.
       F40AG-FN. EXIT.
       F40-FN.   EXIT.
      *N90.      NOTE *************************************.            ACCMER
      *               *                                   *             ACCMER
      *               *          ECRITURES                *             ACCMER
      *               *                                   *             ACCMER
      *               *************************************.            ACCMER
       F90.      EXIT.
      *N90OP.    NOTE *ECRITURE SUR LE FICHIER OP         *.            ACCMER
       F90OP.
                 WRITE    OP00.                                         ACCMER
       F90OP-99. ADD 1 TO 5-OP00-CPTENR.
       F90OP-FN. EXIT.
       F90-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
