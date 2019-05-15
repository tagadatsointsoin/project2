       IDENTIFICATION DIVISION.                                         CI0309
       PROGRAM-ID.  CI0309P.                                            CI0309
      *AUTHOR.         UPDATE PENDING ACH OUT TABLE.                    CI0309
      *DATE-COMPILED.   09/08/14.                                       CI0309
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2007                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE OST    SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE OST    SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE OST          *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 2007                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0309
       CONFIGURATION SECTION.                                           CI0309
       SOURCE-COMPUTER. IBM-370.                                        CI0309
       OBJECT-COMPUTER. IBM-370.                                        CI0309
       DATA DIVISION.                                                   CI0309
       WORKING-STORAGE SECTION.                                         CI0309
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      *****************************************************************
      ** HOST VARIABLES FOR DB2 TABLES                                *
      *****************************************************************
      *!WF DSP=CT DSL=CT SEL=1V FOR=I DES=2 LEV=1 PLT=CT
       01                 CT1V.                                         CI0309
            10            CT1V-CTID   PICTURE  X(27)                    CI0309
                          VALUE                SPACE.                   CI0309
            10            CT1V-NPBN   PICTURE  X(20)                    CI0309
                          VALUE                SPACE.                   CI0309
            10            CT1V-DCACD7 PICTURE  X(10)                    CI0309
                          VALUE                SPACE.                   CI0309
            10            CT1V-CAACTH PICTURE  X                        CI0309
                          VALUE                SPACE.                   CI0309
            10            CT1V-APMTO1 PICTURE  S9(11)V99                CI0309
                          VALUE                ZERO                     CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            CT1V-NTRSN3 PICTURE  S9(8)                    CI0309
                          VALUE                ZERO                     CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            CT1V-CTBAC  PICTURE  X(03)                    CI0309
                          VALUE                SPACE.                   CI0309
            10            CT1V-DXTMST PICTURE  X(26)                    CI0309
                          VALUE                SPACE.                   CI0309
            10            CT1V-GEOPDC PICTURE  X(8)                     CI0309
                          VALUE                SPACE.                   CI0309
       01               7-DTIN-DATE.                                    ADB2DT
      *!WI pl=DT011                                                     ADB2DT
         05             7-DTIN-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0309
      *!WI pl=DT012                                                     ADB2DT
         05             7-DTIN-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0309
      *!WI pl=DT013                                                     ADB2DT
         05             7-DTIN-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0309
      *!WI pl=DT014                                                     ADB2DT
         05             7-DTIN-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0309
       01               7-DTOT-DATE.                                    ADB2DT
      *!WI pl=DT021                                                     ADB2DT
         05             7-DTOT-XDATC                                    ADB2DT
                        PICTURE XX.                                     CI0309
      *!WI pl=DT022                                                     ADB2DT
         05             7-DTOT-XDATY                                    ADB2DT
                        PICTURE XX.                                     CI0309
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT025                                                     ADB2DT
         05             7-DTOT-XDATM                                    ADB2DT
                        PICTURE XX.                                     CI0309
         05             FILLER PIC X                                    ADB2DT
                                            VALUE '-'.                  ADB2DT
      *!WI pl=DT028                                                     ADB2DT
         05             7-DTOT-XDATD                                    ADB2DT
                        PICTURE XX.                                     CI0309
      *CCYYMMDD DATE IN
      *!WI
       01  DT01-DCACG
                        PICTURE 9(8).                                   CI0309
      *CCYY-MM-DD DATE OUT
      *!WI
       01  DT01-DCACD
                        PICTURE X(10).                                  CI0309
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
                        PICTURE X(66)                                   CI0309
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      ******************************************************************
      **                PROCESSING CONTROL SWITCHES                    *
      ******************************************************************
      *!WI
       01 WS-IAIND.                VALUE 'N'
                        PICTURE X.                                      CI0309
          88 NOT-DUPLICATE-TRAN    VALUE 'N'.
          88 DUPLICATE-TRAN        VALUE 'Y'.
      *!WI
       01 WS-XNULL
                        PICTURE S9(4)                                   CI0309
                          BINARY.                                       CI0309
      *!WI
       01 WS-PROCESS-IAIND.        VALUE 'A'
                        PICTURE X.                                      CI0309
          88 ADD-TRAN              VALUE 'A'.
          88 DELETE-TRAN           VALUE 'D'.
       01 WS-APMTA          PIC S9(11)V99  VALUE ZEROES.
       01   DEBUT-WSS.                                                  CI0309
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0309
            05   IK     PICTURE X.                                      CI0309
       01  CONSTANTES-PAC.                                              CI0309
           05  FILLER  PICTURE X(87)   VALUE                            CI0309
                     '6015 CAT09/08/14CI0309ADMIN   14:35:19CI0309P AMERCI0309
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0309
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0309
           05  NUGNA   PICTURE X(5).                                    CI0309
           05  APPLI   PICTURE X(3).                                    CI0309
           05  DATGN   PICTURE X(8).                                    CI0309
           05  PROGR   PICTURE X(6).                                    CI0309
           05  CODUTI  PICTURE X(8).                                    CI0309
           05  TIMGN   PICTURE X(8).                                    CI0309
           05  PROGE   PICTURE X(8).                                    CI0309
           05  COBASE  PICTURE X(4).                                    CI0309
           05  DATGNC  PICTURE X(10).                                   CI0309
           05  RELEAS  PICTURE X(7).                                    CI0309
           05  DATGE   PICTURE X(10).                                   CI0309
           05  DATSQ   PICTURE X(10).                                   CI0309
       01  DATCE.                                                       CI0309
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0309
         05  DATOR.                                                     CI0309
           10  DATOA  PICTURE XX.                                       CI0309
           10  DATOM  PICTURE XX.                                       CI0309
           10  DATOJ  PICTURE XX.                                       CI0309
       01   VARIABLES-CONDITIONNELLES.                                  CI0309
            05                  FT      PICTURE X VALUE '0'.            CI0309
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0309
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0309
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0309
       LINKAGE SECTION.                                                 ADU102
      *****************************************************************
      **                PASS AREAS FROM CALLING MODULE
      *****************************************************************
      *!WF DSP=LK DSL=V1 SEL=70 FOR=I DES=1 LEV=1 PLT=10
       01                 LK70.                                         CI0309
            10            LK70-C299.                                    CI0309
            11            LK70-CTID.                                    CI0309
            12            LK70-CTIDA  PICTURE  9(3).                    CI0309
            12            LK70-CTIDN.                                   CI0309
            13            LK70-CTIDNP PICTURE  X(13).                   CI0309
            13            LK70-CTIDND PICTURE  9(11).                   CI0309
            10            LK70-GECKD  PICTURE  9.                       CI0309
            10            LK70-ICUST  PICTURE  X.                       CI0309
            10            LK70-PRCOD  PICTURE  9(5).                    CI0309
            10            LK70-PRSCD  PICTURE  X(9).                    CI0309
            10            LK70-DCACG9 PICTURE  9(8).                    CI0309
            10            LK70-NAASQ  PICTURE  S9(3)                    CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-CAATY  PICTURE  9(3).                    CI0309
            10            LK70-CACTO  PICTURE  9(3).                    CI0309
            10            LK70-CASTC  PICTURE  99.                      CI0309
            10            LK70-ITRAN  PICTURE  X.                       CI0309
            10            LK70-GEAUN  PICTURE  9(5).                    CI0309
            10            LK70-GEOPD2 PICTURE  X(8).                    CI0309
            10            LK70-DEFFT  PICTURE  9(8).                    CI0309
            10            LK70-CTRTP  PICTURE  X(2).                    CI0309
            10            LK70-CTWHAT PICTURE  S9(7)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-PWHLD  PICTURE  S999V9(5)                CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-GETIM  PICTURE  S9(7)                    CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-ADBRQA PICTURE  S9(9)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-IWTHH  PICTURE  X.                       CI0309
            10            LK70-CLCUS  PICTURE  99.                      CI0309
            10            LK70-CCACT  PICTURE  99.                      CI0309
            10            LK70-AFEET  PICTURE  S9(5)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-ITERF  PICTURE  X.                       CI0309
            10            LK70-ATERF  PICTURE  S9(5)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-CLDOB  PICTURE  9(8).                    CI0309
            10            LK70-CPLTYP PICTURE  X(14).                   CI0309
            10            LK70-IACFPD PICTURE  X(1).                    CI0309
            10            LK70-CDELI  PICTURE  9(3).                    CI0309
            10            LK70-CPAYC  PICTURE  X(2).                    CI0309
            10            LK70-ACOTD  PICTURE  S9(9)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-NPBN   PICTURE  X(20).                   CI0309
            10            LK70-CCBAT  PICTURE  99.                      CI0309
            10            LK70-CLID4.                                   CI0309
            11            LK70-CLIDA  PICTURE  9(3).                    CI0309
            11            LK70-CLIDNP PICTURE  X(12).                   CI0309
            11            LK70-CLIDNA PICTURE  9(8).                    CI0309
            10            LK70-GENAL1 PICTURE  X(30).                   CI0309
            10            LK70-GENAL2 PICTURE  X(30).                   CI0309
            10            LK70-GESAD1 PICTURE  X(30).                   CI0309
            10            LK70-GESAD2 PICTURE  X(30).                   CI0309
            10            LK70-GESAD3 PICTURE  X(30).                   CI0309
            10            LK70-NTR    PICTURE  9(8).                    CI0309
            10            LK70-GECKD1 PICTURE  9.                       CI0309
            10            LK70-IMQMG  PICTURE  X.                       CI0309
            10            LK70-NIPAD  PICTURE  X(15).                   CI0309
            10            LK70-CLNAM.                                   CI0309
            11            LK70-CLNAMH PICTURE  X(6).                    CI0309
            11            LK70-CLNAMF PICTURE  X(20).                   CI0309
            11            LK70-CLNAMI PICTURE  X.                       CI0309
            11            LK70-CLNAMR PICTURE  X(14).                   CI0309
            11            LK70-CLNAML PICTURE  X(25).                   CI0309
            11            LK70-CLNAMS PICTURE  X(4).                    CI0309
            10            LK70-CSLCT  PICTURE  X.                       CI0309
            10            LK70-C199.                                    CI0309
            11            LK70-CLID.                                    CI0309
            12            LK70-CLIDO  PICTURE  9(3).                    CI0309
            12            LK70-CLIDN.                                   CI0309
            13            LK70-CLIDNP PICTURE  X(12).                   CI0309
            13            LK70-CLIDND PICTURE  9(8).                    CI0309
            10            LK70-GECKD2 PICTURE  9.                       CI0309
            10            LK70-CPROCM PICTURE  X.                       CI0309
            10            LK70-NAASQL PICTURE  S9(3)                    CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-NSEQ4B PICTURE  9(8)                     CI0309
                          BINARY.                                       CI0309
            10            LK70-CLTIN  PICTURE  9(12).                   CI0309
            10            LK70-IPULL  PICTURE  X.                       CI0309
            10            LK70-NBTCH  PICTURE  9(4).                    CI0309
            10            LK70-CVSYS  PICTURE  X(2).                    CI0309
            10            LK70-NPISQ  PICTURE  S9(3)                    CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-CPITC  PICTURE  99.                      CI0309
            10            LK70-PPOTD  PICTURE  S9(3)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-ITRNB  PICTURE  X.                       CI0309
            10            LK70-CTTLN1 PICTURE  X(30).                   CI0309
            10            LK70-CTTLN2 PICTURE  X(30).                   CI0309
            10            LK70-CTTLN3 PICTURE  X(30).                   CI0309
            10            LK70-CTTBO1 PICTURE  X(45).                   CI0309
            10            LK70-CTTBO2 PICTURE  X(45).                   CI0309
            10            LK70-PRCMN  PICTURE  X(20).                   CI0309
            10            LK70-TTRTP  PICTURE  X(30).                   CI0309
            10            LK70-DXTMSA PICTURE  X(26).                   CI0309
            10            LK70-DXTMS2 PICTURE  X(26).                   CI0309
            10            LK70-CUPIQ  PICTURE  X.                       CI0309
            10            LK70-IQACT  PICTURE  X.                       CI0309
            10            LK70-MAPPN  PICTURE  X(10).                   CI0309
            10            LK70-CTTYPG PICTURE  X(04).                   CI0309
            10            LK70-CLORN  PICTURE  X(45).                   CI0309
            10            LK70-APMTL  PICTURE  S9(9)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-MPMTFL PICTURE  X(24).                   CI0309
            10            LK70-ANETTQ PICTURE  S9(9)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-TTBAL  PICTURE  X(15).                   CI0309
            10            LK70-MPRN4  PICTURE  X(35).                   CI0309
            10            LK70-CCONF  PICTURE  X(25).                   CI0309
            10            LK70-DCACG  PICTURE  9(8).                    CI0309
            10            LK70-NMESA  PICTURE  9(6).                    CI0309
            10            LK70-MCSIG  PICTURE  X(30).                   CI0309
            10            LK70-IERRC  PICTURE  X.                       CI0309
            10            LK70-AVLMN  PICTURE  S9(7)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-AVLMX  PICTURE  S9(7)V99                 CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-AVCSH  PICTURE  S9(11)V99                CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-ACVALM PICTURE  S9(11)V99                CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-INDRS  PICTURE  X.                       CI0309
            10            LK70-GRID   PICTURE  X(13).                   CI0309
            10            LK70-AACTV  PICTURE  S9(11)V99                CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-CTCUS  PICTURE  999.                     CI0309
            10            LK70-CCDSCW PICTURE  9(2).                    CI0309
            10            LK70-CHCR   PICTURE  99.                      CI0309
            10            LK70-IOWNG  PICTURE  X.                       CI0309
            10            LK70-GECSQ  PICTURE  S9(3)                    CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            LK70-CQACT  PICTURE  999.                     CI0309
            10            LK70-CTOWN  PICTURE  9(3).                    CI0309
            10            LK70-NGEOR  PICTURE  9(08).                   CI0309
            10            LK70-FILLER PICTURE  X(85).                   CI0309
      ******************************************************************
      **               RETURN CODE TO CALLING MODULE
      ******************************************************************
       01  LK01.
      *!WI
       05  LK01-GERTC
                        PICTURE X.                                      CI0309
      *!WI
       05  LK01-NMESS2
                        PICTURE S9(6)                                   CI0309
                          COMPUTATIONAL-3.                              CI0309
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0309
          05              MS00-SUITE.                                   CI0309
            15       FILLER         PICTURE  X(00542).                  CI0309
       01                 MS03  REDEFINES      MS00.                    CI0309
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0309
                          COMPUTATIONAL-3.                              CI0309
            10            MS03-CMSSF  PICTURE  XX.                      CI0309
            10            MS03-DU09.                                    CI0309
            11            MS03-CMESA  PICTURE  S9(9)                    CI0309
                          BINARY.                                       CI0309
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0309
                          BINARY.                                       CI0309
            11            MS03-CMESB  PICTURE  S9(9)                    CI0309
                          BINARY.                                       CI0309
            11            MS03-CMSST  PICTURE  S9(9)                    CI0309
                          BINARY.                                       CI0309
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0309
                          BINARY.                                       CI0309
            11            MS03-QELLAA PICTURE  S9(9)                    CI0309
                          BINARY.                                       CI0309
            11            MS03-TMESS4 PICTURE  X(512).                  CI0309
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0309
            10            MX11-QMSGS  PICTURE  9(03).                   CI0309
            10            MX11-PJ09                                     CI0309
                          OCCURS       025     TIMES.                   CI0309
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0309
                          COMPUTATIONAL-3.                              CI0309
            11            MX11-CMESB  PICTURE  S9(9)                    CI0309
                          BINARY.                                       CI0309
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                LK70
                                LK01
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0309
      *               *                                   *             CI0309
      *               *INITIALISATIONS                    *             CI0309
      *               *                                   *             CI0309
      *               *************************************.            CI0309
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0309
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0309
      *               *                                   *             CI0309
      *               *FIN DE TRAITEMENT                  *             CI0309
      *               *                                   *             CI0309
      *               *************************************.            CI0309
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0309
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *CONVERT INPUT TO DB VALUES         *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CB.    NOTE *INITIALIZE DB SEGMENTS             *.
       F30CB.                                                           lv10
           INITIALIZE  CT1V.
       F30CB-FN. EXIT.
      *N30EA.    NOTE *SET UP CT1V: FA ACTIVITY           *.
       F30EA.                                                           lv10
      *********************************
      *N30EC.    NOTE *MOVE FROM LINKAGE SEGMENT          *.
       F30EC.                                                           lv15
           MOVE        LK70-CTID TO CT1V-CTID
           MOVE        LK70-DXTMSA TO CT1V-DXTMST.
       F30EC-FN. EXIT.
      *N30EH.    NOTE *CONVERT DATES TO DB2 FORMAT        *.
       F30EH.                                                           lv15
      *PROCESSING DATE
           MOVE        LK70-DEFFT TO DT01-DCACG
           PERFORM     F91DT THRU F91DT-FN
           MOVE        DT01-DCACD TO CT1V-DCACD7.
       F30EH-FN. EXIT.
       F30EA-FN. EXIT.
      *N30GA.    NOTE *SET UP CT1V: ACH OUT ARRANGEMENT   *.
       F30GA.         EXIT.                                             lv10
      *N30GC.    NOTE *MOVE BANK DETAILS                  *.
       F30GC.                                                           lv15
           MOVE        LK70-NTR TO CT1V-NTRSN3
           MOVE        LK70-NPBN TO CT1V-NPBN.
                 IF    LK70-CCBAT = '01'                                DOT
           MOVE        'CHK' TO CT1V-CTBAC.
                 IF    LK70-CCBAT = '02'                                DOT
           MOVE        'SAV' TO CT1V-CTBAC.
       F30GC-FN. EXIT.
      *N30GE.    NOTE *MOVE ADD/DELETE TRANSACTION        *.
       F30GE.                                                           lv15
                 IF    LK70-CPROCM = 'A'                                DOT
      *ADD
           MOVE        'A' TO CT1V-CAACTH
           WS-PROCESS-IAIND.
                 IF    LK70-CPROCM = 'D'                                DOT
      *DELETE
           MOVE        'D' TO CT1V-CAACTH
           WS-PROCESS-IAIND.
       F30GE-FN. EXIT.
       F30GA-FN. EXIT.
      *N30IA.    NOTE *SET UP CT1V: ACTIVITY AMOUNT       *.
       F30IA.         EXIT.                                             lv10
      *N30IG.    NOTE *MOVE ACTIVITY AMOUNT               *.
       F30IG.                                                           lv15
                 IF    ADD-TRAN                                         DOT
           COMPUTE     WS-APMTA = LK70-ADBRQA.
                 IF    DELETE-TRAN                                      DOT
           COMPUTE     WS-APMTA = 0
           - LK70-ADBRQA.
           COMPUTE     CT1V-APMTO1 = WS-APMTA.                          DOT
       F30IG-FN. EXIT.
       F30IA-FN. EXIT.
      *N30IK.    NOTE *MOVE CREATE USER ID                *.
       F30IK.                                                           lv10
      *
           MOVE        LK70-GEOPD2 TO CT1V-GEOPDC.
       F30IK-FN. EXIT.
       F30-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS TABLE UPDATE               *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50EB.    NOTE *UPDATE TABLE CT1V FOR ADD OR       *.
       F50EB.                                                           lv10
      *DELETE TRANSACTIONS
      *********************************
      *N50EE.    NOTE *INSERT CT1V                        *.
       F50EE.                                                           lv15
           PERFORM     F94CC THRU F94CC-FN.
       F50EE-FN. EXIT.
      *N50EH.    NOTE *DUPLICATE TRANSACTION              *.
       F50EH.    IF    SQLCODE = -803                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50EH-FN.
           MOVE        'N' TO LK01-GERTC
           MOVE        012639 TO LK01-NMESS2
           MOVE                     ALL '1' TO FT GO TO F20.
       F50EH-FN. EXIT.
      *N50EK.    NOTE *SUCCESSFUL INSERT                  *.
       F50EK.    IF    SQLCODE = +0                                     lv15
                 NEXT SENTENCE ELSE GO TO     F50EK-FN.
           MOVE        'Y' TO LK01-GERTC
           MOVE        ZEROES TO LK01-NMESS2.
       F50EK-FN. EXIT.
       F50EB-FN. EXIT.
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
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91DT.    NOTE *DATE REFORMATTING                  *.
       F91DT.                                                           lv10
      *REFORMAT CCYYMMDD USING DASHES                                   ADB2DT
           MOVE        DT01-DCACG TO                                    ADB2DT
           7-DTIN-DATE                                                  ADB2DT
           MOVE        7-DTIN-XDATC TO 7-DTOT-XDATC                     ADB2DT
           MOVE        7-DTIN-XDATY TO 7-DTOT-XDATY                     ADB2DT
           MOVE        7-DTIN-XDATM TO 7-DTOT-XDATM                     ADB2DT
           MOVE        7-DTIN-XDATD TO 7-DTOT-XDATD                     ADB2DT
           MOVE        7-DTOT-DATE TO                                   ADB2DT
           DT01-DCACD.                                                  ADB2DT
       F91DT-FN. EXIT.
       F91-FN.   EXIT.
      *N93.      NOTE *************************************.
      *               *                                   *
      *               *ERROR HANDLING                     *
      *               *                                   *
      *               *************************************.
       F93.           EXIT.                                             lv05
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
       F93-FN.   EXIT.
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *TABLE - TBCT1V PROCESSING          *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CC.    NOTE *INSERT CT1V                        *.
       F94CC.                                                           lv10
           MOVE        'F94CC - INSERT' TO 7-DB2-FUNCT                  ADB227
           EXEC SQL    INSERT                                           ADB227
                       INTO
                           CORP.TBCT1V
                       VALUES
                           (:CT1V)                           END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB227
       F94CC-FN. EXIT.
      *N94RC.    NOTE *CHECK FOR DUPLICATE TRANSACTION    *.
       F94RC.                                                           lv10
           MOVE        'F94RC - SELECT' TO 7-DB2-FUNCT                  ADB226
           EXEC SQL    SELECT                                           ADB226
                       IBMREQD
                       INTO
                         :WS-IAIND:WS-XNULL
                       FROM
                          SYSIBM.SYSDUMMY1
                       WHERE EXISTS (
                           SELECT *
                           FROM
                               CORP.TBCT1V
                       WHERE
                           CTID   = :CT1V-CTID
                       AND DCACD7 = :CT1V-DCACD7
                       AND NPBN   = :CT1V-NPBN
                       AND CAACTH = :CT1V-CAACTH
                                    )                        END-EXEC.
           PERFORM     F93SQ THRU F93SQ-FN.                             ADB226
       F94RC-FN. EXIT.
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
