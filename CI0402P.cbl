       IDENTIFICATION DIVISION.                                         CI0402
       PROGRAM-ID.  CI0402P.                                            CI0402
      *AUTHOR.         POSITION LOOK UP FOR CATS.                       CI0402
      *DATE-COMPILED.   09/08/14.                                       CI0402
       ENVIRONMENT DIVISION.                                            CI0402
       CONFIGURATION SECTION.                                           CI0402
       SOURCE-COMPUTER. IBM-370.                                        CI0402
       OBJECT-COMPUTER. IBM-370.                                        CI0402
       DATA DIVISION.                                                   CI0402
       WORKING-STORAGE SECTION.                                         CI0402
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
                        PICTURE X(66)                                   CI0402
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      *BEGIN DB2          TB00                                          CI0402
       01                 TB00.                                         CI0402
            10            TB00-NACTN  PICTURE  X(8).                    CI0402
            10            TB00-CAAST  PICTURE  X(3).                    CI0402
            10            TB00-CCSMQ  PICTURE  X.                       CI0402
            10            TB00-GCUSPZ PICTURE  X(12).                   CI0402
            10            TB00-CTKRAA PICTURE  X(12).                   CI0402
            10            TB00-CSECT  PICTURE  X(1).                    CI0402
            10            TB00-AMKTV  PICTURE  S9(16)V99                CI0402
                          COMPUTATIONAL-3.                              CI0402
            10            TB00-CLOCA  PICTURE  XX.                      CI0402
            10            TB00-AFAVPH PICTURE  S9(12)V9(6)              CI0402
                          COMPUTATIONAL-3.                              CI0402
            10            TB00-DMPEN  PICTURE  X(8).                    CI0402
      *END DB2                                                          CI0402
      **---------------------------------------------------------------*
      **             WORKING STOAGE TO STORE THE REGION NAME           *
      **---------------------------------------------------------------*
       01                 WS00-REGION          PIC X(16)
                           VALUE SPACES.
      *
      **---------------------------------------------------------------*
      **             WORKING STOAGE TO STORE THE LOCATION NAME         *
      **---------------------------------------------------------------*
       01                 WS00-LOCATION         PIC X(8)
                           VALUE SPACES.
      *
       01   DEBUT-WSS.                                                  CI0402
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0402
            05   IK     PICTURE X.                                      CI0402
       01  CONSTANTES-PAC.                                              CI0402
           05  FILLER  PICTURE X(87)   VALUE                            CI0402
                     '9999 CAT09/08/14CI0402ADMIN   19:33:53CI0402P BVAPCI0402
      -    '09/08/20143.5 V0419/02/201425/02/2014'.                     CI0402
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0402
           05  NUGNA   PICTURE X(5).                                    CI0402
           05  APPLI   PICTURE X(3).                                    CI0402
           05  DATGN   PICTURE X(8).                                    CI0402
           05  PROGR   PICTURE X(6).                                    CI0402
           05  CODUTI  PICTURE X(8).                                    CI0402
           05  TIMGN   PICTURE X(8).                                    CI0402
           05  PROGE   PICTURE X(8).                                    CI0402
           05  COBASE  PICTURE X(4).                                    CI0402
           05  DATGNC  PICTURE X(10).                                   CI0402
           05  RELEAS  PICTURE X(7).                                    CI0402
           05  DATGE   PICTURE X(10).                                   CI0402
           05  DATSQ   PICTURE X(10).                                   CI0402
       01  DATCE.                                                       CI0402
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0402
         05  DATOR.                                                     CI0402
           10  DATOA  PICTURE XX.                                       CI0402
           10  DATOM  PICTURE XX.                                       CI0402
           10  DATOJ  PICTURE XX.                                       CI0402
       01   VARIABLES-CONDITIONNELLES.                                  CI0402
            05               FT.                                        CI0402
              10          TB-FT      PICTURE X VALUE '0'.               CI0402
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0402
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0402
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0402
            05       5-TB00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0402
       01                VTB00.                                         CI0402
          05             VTB00NACTN    PICTURE S9(4) COMP.              CI0402
          05             VTB00CAAST    PICTURE S9(4) COMP.              CI0402
          05             VTB00CCSMQ    PICTURE S9(4) COMP.              CI0402
          05             VTB00GCUSPZ   PICTURE S9(4) COMP.              CI0402
          05             VTB00CTKRAA   PICTURE S9(4) COMP.              CI0402
          05             VTB00CSECT    PICTURE S9(4) COMP.              CI0402
          05             VTB00AMKTV    PICTURE S9(4) COMP.              CI0402
          05             VTB00CLOCA    PICTURE S9(4) COMP.              CI0402
          05             VTB00AFAVPH   PICTURE S9(4) COMP.              CI0402
          05             VTB00DMPEN    PICTURE S9(4) COMP.              CI0402
       01                VTB00R    REDEFINES  VTB00.                    CI0402
          05             VTB00A        PICTURE S9(4) COMP               CI0402
                         OCCURS                      0010.              CI0402
       01   ZONES-UTILISATEUR PICTURE X.                                CI0402
       LINKAGE SECTION.
      ******************************************************************
      **            INPUT AND OUTPUT SEGMENT
      ******************************************************************
      *!WF DSP=K9 DSL=K9 SEL=15 FOR=I DES=2 LEV=1 PLT=10 id=1
       01                 K915.                                         CI0402
            10            K915-NACTN  PICTURE  X(8)                     CI0402
                          VALUE                SPACE.                   CI0402
            10            K915-GCUSPZ PICTURE  X(12)                    CI0402
                          VALUE                SPACE.                   CI0402
            10            K915-AMKTV  PICTURE  S9(16)V99                CI0402
                          VALUE                ZERO                     CI0402
                          COMPUTATIONAL-3.                              CI0402
            10            K915-GERTC  PICTURE  X                        CI0402
                          VALUE                SPACE.                   CI0402
            10            K915-CSQLRC PICTURE  S9(3)                    CI0402
                          VALUE                ZERO.                    CI0402
            10            K915-AFAVPH PICTURE  S9(12)V9(6)              CI0402
                          VALUE                ZERO                     CI0402
                          COMPUTATIONAL-3.                              CI0402
       PROCEDURE DIVISION USING K915.
      *N01.      NOTE *************************************.            CI0402
      *               *                                   *             CI0402
      *               *INITIALISATIONS                    *             CI0402
      *               *                                   *             CI0402
      *               *************************************.            CI0402
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.
      *               *                                   *
      *               *INITIALIZATIONS                    *
      *               *                                   *
      *               *************************************.
       F02.                                                             lv05
      *
      *N02BC.    NOTE *********************************   *.
       F02BC.                                                           lv10
      ** INITIALIZE FT TO ENSURE F20  *
      ** IS EXECUTED AT THE END OF    *
      ** PROCESSING                   *
      *********************************
      *
           MOVE ALL    ZEROES TO FT.
       F02BC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0402
       F05.
      *N05.      NOTE *************************************.            CI0402
      *               *                                   *             CI0402
      *               *LECTURE FICHIERS ACCES SEQ. SANS DE*             CI0402
      *               *                                   *             CI0402
      *               *************************************.            CI0402
       F05-FN.   EXIT.
      *N20.      NOTE *************************************.            CI0402
      *               *                                   *             CI0402
      *               *FIN DE TRAITEMENT                  *             CI0402
      *               *                                   *             CI0402
      *               *************************************.            CI0402
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0402
      *N2099.    NOTE *********************************   *.
       F2099.                                                           lv10
      **  GO BACK TO CALLING PROGRAM  *
      *********************************
           GOBACK.
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING LOOP               *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *THIS MODULE IS USED FOR POSITION
      *VALIDATION. THIS ACCESSES THE
      *AGGREGATION UDB ACCT_POSITION TO
      *CHECK WHETHER THE ACCOUNT-CUSIP
      *COMBINATION EXISTS OR NOT. BASED
      *ON THE RETURN CODE, FURTHER
      *PROCESSING WILL BE DONE BY THE
      *CALLING PROGRAM
      *N40BC.    NOTE *INITIALIZATIONS                    *.
       F40BC.                                                           lv10
      *
           MOVE        'N' TO K915-GERTC
           INITIALIZE  K915-AMKTV K915-AFAVPH.
       F40BC-FN. EXIT.
      *N40BG.    NOTE *VALIDATE THE ACCOUNT NUMBER, IF    *.
       F40BG.    IF    K915-NACTN NOT NUMERIC                           lv10
                 NEXT SENTENCE ELSE GO TO     F40BG-FN.
      *NOT NUMERIC, GET BACK TO THE
      *CALLING PROGRAM
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F40BG-FN. EXIT.
      *N40BK.    NOTE *POPULATE THE HOST VARIABLES TO     *.
       F40BK.                                                           lv10
      *QUERY THE TABLE
      *
           MOVE        K915-NACTN TO TB00-NACTN
           MOVE        K915-GCUSPZ TO TB00-GCUSPZ
      *.
      *PERFORM THE QUERY                                                DOT
           PERFORM     F95TB THRU F95TB-FN
      *
                 IF    SQLCODE NOT = +0                                 DOT
      *VALIDATE THE SQL CODE
           MOVE                     ALL '1' TO FT GO TO F20.
      *N40BO.    NOTE *POPULATE THE MARKET VALUE          *.
       F40BO.                                                           lv15
           MOVE        'Y' TO K915-GERTC
           MOVE        TB00-AMKTV TO K915-AMKTV
           MOVE        TB00-AFAVPH TO K915-AFAVPH.
       F40BO-FN. EXIT.
       F40BK-FN. EXIT.
       F40-FN.   EXIT.
      *N79.      NOTE *************************************.
      *               *                                   *
      *               *NORMAL TERMINATION                 *
      *               *                                   *
      *               *************************************.
       F79.                                                             lv05
      *RETURN TO THE CALLING PROGRAM
           MOVE                     ALL '1' TO FT GO TO F20.
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N95TB.    NOTE *GET THE MARKET VALUE               *.
      *                                                                 ADB221
       F95TB.                                                           lv10
           MOVE        'F95TB - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                         MARKET_VALUE
                        ,PRICE
                       INTO
                         :TB00-AMKTV
                        ,:TB00-AFAVPH
                       FROM
                         TMQDN00.ACCT_POSITION
                       WHERE
                           ACCT_NBR  = :TB00-NACTN
                       AND CUSIP_NBR = :TB00-GCUSPZ          END-EXEC.
           MOVE        SQLCODE TO K915-CSQLRC.
       F95TB-FN. EXIT.
