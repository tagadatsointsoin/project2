       IDENTIFICATION DIVISION.                                         CI0256
       PROGRAM-ID.  CI0256P.                                            CI0256
      *AUTHOR.         BUILD HTML PAGE FOR CBA - DRV.                   CI0256
      *DATE-COMPILED.   09/08/14.                                       CI0256
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 2013                          *ACOPYP
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
      *     COPR. 2013                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0256
       CONFIGURATION SECTION.                                           CI0256
       SOURCE-COMPUTER. IBM-370.                                        CI0256
       OBJECT-COMPUTER. IBM-370.                                        CI0256
       DATA DIVISION.                                                   CI0256
       WORKING-STORAGE SECTION.                                         CI0256
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0218           PIC X(08)  VALUE 'CI0218P '.                AM0218
      ******************************************************************
      **                                                               *
      ** THESE FIELDS WILL BE USED TO FORMAT THE INDIVIDUAL FIELDS USED*
      ** IN THE DETAIL LINES                                           *
      **                                                               *
      ******************************************************************
       01  W-DE00-AREAS.
      *!WS
           05  W-DE00-DCACG
                        PICTURE 99B99B9999.                             CI0256
      ******************************************************************ACMCTI
      *WORKING STORAGE SEGMENT FOR STORING THE LINKAGE DATA FOR CI0361. ACMCTI
      ******************************************************************ACMCTI
      *!WF DSP=I9 DSL=K9 SEL=3B FOR=I DES=2 LEV=1                       ACMCTI
       01                 I93B.                                         CI0256
            10            I93B-CEADC  PICTURE  X                        CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-DACTT  PICTURE  X(10)                    CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-GEOPDC PICTURE  X(8)                     CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-GEOPDB PICTURE  X(8)                     CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-CAEMCE PICTURE  X(8)                     CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-CAEMCD PICTURE  X(8)                     CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-GETIMM PICTURE  X(8)                     CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-CRTNC  PICTURE  S9(9)                    CI0256
                          VALUE                ZERO                     CI0256
                          COMPUTATIONAL-3.                              CI0256
            10            I93B-GERTC  PICTURE  X                        CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-DXTMST PICTURE  X(26)                    CI0256
                          VALUE                SPACE.                   CI0256
            10            I93B-DXTMS2 PICTURE  X(26)                    CI0256
                          VALUE                SPACE.                   CI0256
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE NAME OF THE CALLED MODULE.    ACMCTI
      ******************************************************************ACMCTI
       01          CI0361       PIC X(08)                               ACMCTI
                                VALUE 'CI0361P'.                        ACMCTI
                                                                        ACMCTI
      ******************************************************************ACMCTI
      *WORKING STORAGE FIELD TO STORE THE DATE FOR THE DUMMY DB2 CALL.  ACMCTI
      ******************************************************************ACMCTI
       01          WS00-DATE    PIC X(10)     VALUE SPACES.             ACMCTI
      *HTML BLOB THAT NEEDS TO BE SENT TO WEB AND IMAGE UTILITY

       01 HTML-BLOB    PIC X(50000)  VALUE SPACES.
       01 HTML-LEN     PIC 9(5)      VALUE ZEROS.
       01 HTML-LIMIT   PIC 9(3)      VALUE ZEROS.
       01 HTML-TEXT    PIC X(200)    VALUE SPACES.
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
      *STRING POINTERS USED IN THE PROGRAM

       01  HTML-PT PIC S9(5) VALUE ZEROS.

       01  TEMP-PT PIC S9(5) VALUE ZEROS.
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
                        PICTURE X(66)                                   CI0256
                           VALUE 'DB2 RESOURCE IN USE. TRY AGAIN '.     ADB221
       01               7-MAXM-RETRY     PIC S9(3) COMP-3               ADB221
                                                   VALUE +001.          ADB221
      ******************************************************************AM0218
      ** WORKING STORAGE SEGMENT FOR CI0218                            *AM0218
      ******************************************************************AM0218
      *                                                                 AM0218
       01   7-TY00-STAGING.                                             AM0218
      *!WI pl=TY210                                                     AM0218
            05  7-TY00-XFUNC                                            AM0218
                        PICTURE X(04).                                  CI0256
      *!WI pl=TY220                                                     AM0218
            05  7-TY00-NPNTRA                                           AM0218
                          POINTER.                                      CI0256

      ******************************************************************
      ** WORKING STORAGE SEGMENT FOR FDC INTERFACE RECORD              *
      ******************************************************************
      *!WF DSP=X1 DSL=VW SEL=01 FOR=I DES=1 LEV=1 PLT=TY
       01                 X101.                                         CI0256
            10            X101-FILLER.                                  CI0256
            11            X101-XFUNC  PICTURE  X(04).                   CI0256
            11            X101-CANUMB PICTURE  X(27).                   CI0256
            10            X101-CENTT  PICTURE  X.                       CI0256
            10            X101-FILLER PICTURE  X(68).                   CI0256
            10            X101-MTQUE  PICTURE  X(08).                   CI0256
            10            X101-FILLER PICTURE  X(04).                   CI0256
            10            X101-FILLER.                                  CI0256
            11            X101-NPNTRB                                   CI0256
                          POINTER.                                      CI0256
            11            X101-NPNTRC                                   CI0256
                          POINTER.                                      CI0256
            11            X101-NPNTRD                                   CI0256
                          POINTER.                                      CI0256
            11            X101-NPNTRE                                   CI0256
                          POINTER.                                      CI0256
            11            X101-NPNTRF                                   CI0256
                          POINTER.                                      CI0256
            11            X101-NPNTRG                                   CI0256
                          POINTER.                                      CI0256
            11            X101-NPNTRH                                   CI0256
                          POINTER.                                      CI0256
            10            X101-DCACG  PICTURE  9(8).                    CI0256
            10            X101-FILLER PICTURE  X(492).                  CI0256
       01                 X158.                                         CI0256
            10            X158-QT5K.                                    CI0256
            11            X158-C299.                                    CI0256
            12            X158-CTID.                                    CI0256
            13            X158-CTIDA  PICTURE  9(3).                    CI0256
            13            X158-CTIDN.                                   CI0256
            14            X158-CTIDNP PICTURE  X(13).                   CI0256
            14            X158-CTIDND PICTURE  9(11).                   CI0256
            11            X158-GECKD2 PICTURE  9.                       CI0256
            11            X158-NSEQ5  PICTURE  9(5).                    CI0256
            11            X158-CTSTA  PICTURE  99.                      CI0256
            11            X158-CTSTAL PICTURE  X(10).                   CI0256
            11            X158-CTOWN  PICTURE  9(3).                    CI0256
            11            X158-CTTLN1 PICTURE  X(30).                   CI0256
            11            X158-CTTLN2 PICTURE  X(30).                   CI0256
            11            X158-CTTLN3 PICTURE  X(30).                   CI0256
            11            X158-CTTBO1 PICTURE  X(45).                   CI0256
            11            X158-CTTBO2 PICTURE  X(45).                   CI0256
            11            X158-CTEFD  PICTURE  9(8).                    CI0256
            11            X158-CTIAD  PICTURE  9(8).                    CI0256
            11            X158-CTCUS  PICTURE  999.                     CI0256
            11            X158-GR98.                                    CI0256
            12            X158-GRID.                                    CI0256
            13            X158-GRIDC  PICTURE  9(3).                    CI0256
            13            X158-GRIDN.                                   CI0256
            14            X158-GRIDNP PICTURE  99.                      CI0256
            14            X158-GRIDND PICTURE  9(8).                    CI0256
            11            X158-CQACT  PICTURE  999.                     CI0256
            11            X158-CTCCI  PICTURE  X.                       CI0256
            11            X158-CIRAS  PICTURE  999.                     CI0256
            11            X158-CIRAT  PICTURE  999.                     CI0256
            11            X158-IACVD  PICTURE  X.                       CI0256
            11            X158-FILLER PICTURE  X(4).                    CI0256
            11            X158-PRCODA PICTURE  X(5).                    CI0256
            11            X158-PRCMN  PICTURE  X(20).                   CI0256
            11            X158-MRPLN  PICTURE  X(30).                   CI0256
            11            X158-CPRDG  PICTURE  9(2).                    CI0256
            11            X158-CPRDA1 PICTURE  9(3).                    CI0256
            11            X158-PRSCD  PICTURE  X(9).                    CI0256
            11            X158-MSP03  PICTURE  X(3).                    CI0256
            11            X158-CGRLI  PICTURE  X.                       CI0256
            11            X158-ITERM  PICTURE  X(1).                    CI0256
            11            X158-IVARP  PICTURE  X.                       CI0256
            11            X158-DVALU  PICTURE  9(8).                    CI0256
            11            X158-AACTV  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ACCTVC PICTURE  X(20).                   CI0256
            11            X158-ITXTI  PICTURE  X.                       CI0256
            11            X158-ASANP  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ACINV  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CELBL  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-NMESS2 PICTURE  S9(6)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-FILLER PICTURE  X(1).                    CI0256
            11            X158-PRCLN  PICTURE  X(60).                   CI0256
            11            X158-GECKD  PICTURE  9.                       CI0256
            11            X158-MPLNA  PICTURE  X(19).                   CI0256
            11            X158-CQACTL PICTURE  X(45).                   CI0256
            11            X158-CRQPA  PICTURE  9(3).                    CI0256
            11            X158-IVANT  PICTURE  X(1).                    CI0256
            11            X158-IDBRP  PICTURE  X(1).                    CI0256
            11            X158-IANPY  PICTURE  X.                       CI0256
            11            X158-IVARP1 PICTURE  X.                       CI0256
            11            X158-FILLER PICTURE  X(27).                   CI0256
            11            X158-NSEQ2A PICTURE  S9(3)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-NSEQ2P PICTURE  S9(3)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-MRPSN  PICTURE  X(12).                   CI0256
            11            X158-GEHCD  PICTURE  9(3)                     CI0256
                          OCCURS       002     TIMES.                   CI0256
            11            X158-GEHCSU PICTURE  9(5)                     CI0256
                          OCCURS       002     TIMES.                   CI0256
            11            X158-PRCSN  PICTURE  X(9).                    CI0256
            11            X158-CGRMF  PICTURE  X.                       CI0256
            11            X158-IGFEX  PICTURE  X.                       CI0256
            11            X158-CLIDP  PICTURE  X(23).                   CI0256
            11            X158-CLCTRC PICTURE  9(3).                    CI0256
            11            X158-ADINP  PICTURE  X(20).                   CI0256
            11            X158-CLCTRA PICTURE  9(3).                    CI0256
            11            X158-GRPLC  PICTURE  99.                      CI0256
            11            X158-CIDRP  PICTURE  99.                      CI0256
            11            X158-FILLER PICTURE  X(01).                   CI0256
            11            X158-AVMTOT PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AVCSH  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AMARC  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AVLMX  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AVLMN  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-INDRS  PICTURE  X.                       CI0256
            11            X158-MPRN4  PICTURE  X(35).                   CI0256
            11            X158-FILLER PICTURE  X(1).                    CI0256
            11            X158-ACVALM PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-INDRSA PICTURE  X(2).                    CI0256
            11            X158-DXTMSA PICTURE  X(26).                   CI0256
            11            X158-NMESS6 PICTURE  S9(6)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-NMESS7 PICTURE  S9(6)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-IBIDSA PICTURE  X.                       CI0256
            11            X158-IBIDSB PICTURE  X.                       CI0256
            11            X158-INSPOS PICTURE  X.                       CI0256
            11            X158-INSPOD PICTURE  X.                       CI0256
            11            X158-ACBALX PICTURE  X(20).                   CI0256
            11            X158-AINVMX PICTURE  X(20).                   CI0256
            11            X158-AMARCX PICTURE  X(20).                   CI0256
            11            X158-AVMTOX PICTURE  X(20).                   CI0256
            11            X158-IMNPR  PICTURE  X.                       CI0256
            11            X158-ISSPL  PICTURE  X.                       CI0256
            11            X158-AVMTOI PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AVCSHI PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-APOSC  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AVLMXI PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AVLMN1 PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AVLMN2 PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-FILLER PICTURE  X(05).                   CI0256
            10            X158-QT5A.                                    CI0256
            11            X158-CLID   PICTURE  X(23).                   CI0256
            11            X158-GECKD1 PICTURE  9.                       CI0256
            11            X158-MCLNM  PICTURE  X(40).                   CI0256
            11            X158-MCLNM2 PICTURE  X(40).                   CI0256
            11            X158-CLTYP  PICTURE  X.                       CI0256
            11            X158-CLDOB  PICTURE  9(8).                    CI0256
            11            X158-CLDTH  PICTURE  X.                       CI0256
            11            X158-CLTIN  PICTURE  9(12).                   CI0256
            11            X158-CLTINC PICTURE  9.                       CI0256
            11            X158-GESAD1 PICTURE  X(30).                   CI0256
            11            X158-GESAD2 PICTURE  X(30).                   CI0256
            11            X158-GESAD3 PICTURE  X(30).                   CI0256
            11            X158-GECIT  PICTURE  X(25).                   CI0256
            11            X158-GECTRY PICTURE  X(20).                   CI0256
            11            X158-GEPCD  PICTURE  X(12).                   CI0256
            11            X158-GEST   PICTURE  X(8).                    CI0256
            11            X158-GEADS  PICTURE  9.                       CI0256
            11            X158-GECSD  PICTURE  9(8).                    CI0256
            11            X158-QCLAGE PICTURE  9(3)V9                   CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-FILLER PICTURE  X(06).                   CI0256
            10            X158-QT5T.                                    CI0256
            11            X158-ATFRA  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AGOFD  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-APRMX  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-APRMN  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-IOWNC  PICTURE  X.                       CI0256
            11            X158-COWNF  PICTURE  X(30).                   CI0256
            11            X158-CTYPE  PICTURE  X.                       CI0256
            11            X158-CIRAC  PICTURE  X(5).                    CI0256
            11            X158-CTXMT  PICTURE  9(2).                    CI0256
            11            X158-AMIND  PICTURE  S9(7)V99.                CI0256
            11            X158-AMAXAR PICTURE  S9(7)V99.                CI0256
            11            X158-QSHOWQ PICTURE  S9(9)V999                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-QSHOW0 PICTURE  S9(10)V999               CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-PPOT1  PICTURE  S9(3)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-PACT1  PICTURE  S999V999                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-IPRTA  PICTURE  X.                       CI0256
            11            X158-FILLER PICTURE  X.                       CI0256
            11            X158-CLCUS  PICTURE  99.                      CI0256
            11            X158-CCDSCW PICTURE  9(2).                    CI0256
            11            X158-CCACT  PICTURE  99.                      CI0256
            11            X158-CIRAG.                                   CI0256
            12            X158-CIRAP  PICTURE  XX                       CI0256
                          OCCURS       010     TIMES.                   CI0256
            11            X158-ITERF  PICTURE  X.                       CI0256
            11            X158-IACFPD PICTURE  X(1).                    CI0256
            11            X158-AFEET  PICTURE  S9(5)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ATERF  PICTURE  S9(5)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CLIDNB PICTURE  9(8).                    CI0256
            11            X158-ALOAD  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASURR  PICTURE  S9(07)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASHIS  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AMNBL  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-APNAC  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ANGOF  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CPLTYP PICTURE  X(14).                   CI0256
            10            X158-QT5N.                                    CI0256
            11            X158-IARRAN PICTURE  X.                       CI0256
            11            X158-GESTD1 PICTURE  9(8).                    CI0256
            11            X158-GEEND1 PICTURE  S9(8)                    CI0256
                          BINARY.                                       CI0256
            11            X158-GESTD  PICTURE  9(8).                    CI0256
            11            X158-GEEND  PICTURE  9(8).                    CI0256
            11            X158-NSQ4B2 PICTURE  9(8)                     CI0256
                          BINARY.                                       CI0256
            11            X158-CDEST  PICTURE  99.                      CI0256
            11            X158-DEFFT  PICTURE  9(8).                    CI0256
            11            X158-CPMTF  PICTURE  99.                      CI0256
            11            X158-CPMTG  PICTURE  99.                      CI0256
            11            X158-MPMTFL PICTURE  X(24).                   CI0256
            11            X158-MPMTFE PICTURE  X(24).                   CI0256
            11            X158-DLAUP  PICTURE  9(8).                    CI0256
            11            X158-NSEQ4B PICTURE  9(8)                     CI0256
                          BINARY.                                       CI0256
            11            X158-QSACTF PICTURE  9(3).                    CI0256
            11            X158-QSACTT PICTURE  9(3).                    CI0256
            11            X158-CCONF  PICTURE  X(25).                   CI0256
            11            X158-DCONF  PICTURE  9(8).                    CI0256
            11            X158-DTIMT  PICTURE  X(8).                    CI0256
            11            X158-CACTS  PICTURE  X.                       CI0256
            11            X158-ADBRQ  PICTURE  S9(11)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-DNPMT  PICTURE  9(8).                    CI0256
            11            X158-NAPDS  PICTURE  S9(3)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CDEST1 PICTURE  99.                      CI0256
            11            X158-CLANR1 PICTURE  X(23).                   CI0256
            11            X158-FILLER PICTURE  X(01).                   CI0256
            10            X158-FILLER PICTURE  X(600).                  CI0256
            10            X158-QT5C                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-CESLD  PICTURE  9(8).                    CI0256
            11            X158-PCIRB5 PICTURE  S9(3)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-PANYDD PICTURE  S9(3)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CEIT   PICTURE  9(3).                    CI0256
            11            X158-PPART  PICTURE  9(3)V99                  CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-DTRME  PICTURE  9(8).                    CI0256
            11            X158-CEIRND PICTURE  9(8).                    CI0256
            11            X158-DANNIA PICTURE  9(8).                    CI0256
            11            X158-AAPAA  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CELBDT PICTURE  9(8).                    CI0256
            11            X158-CEIIS  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-DTRME1 PICTURE  9(8).                    CI0256
            11            X158-GMKTS.                                   CI0256
            12            X158-DTRME2 PICTURE  9(8)                     CI0256
                          OCCURS       005     TIMES.                   CI0256
            12            X158-DTRME3 PICTURE  9(8)                     CI0256
                          OCCURS       005     TIMES.                   CI0256
            11            X158-ALINT  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CEHCD  PICTURE  9(3)                     CI0256
                          OCCURS       006     TIMES.                   CI0256
            11            X158-CEFOTR PICTURE  S9(3)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-DGPED  PICTURE  9(8).                    CI0256
            11            X158-DIPED  PICTURE  9(8).                    CI0256
            11            X158-FILLER PICTURE  X(409).                  CI0256
            10            X158-QT5F                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-DLAUP2 PICTURE  9(8).                    CI0256
            11            X158-QSHOW  PICTURE  S9(10)V999               CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AFAVP  PICTURE  S9(4)V9(3)               CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-QSHIS  PICTURE  S9(10)V999               CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-QSHNM  PICTURE  S9(10)V999.              CI0256
            11            X158-QSHOM  PICTURE  S9(10)V999               CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ADDAC  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-QSHES  PICTURE  S9(10)V999               CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-NDCUS  PICTURE  X(9).                    CI0256
            11            X158-CSTKR5 PICTURE  X(5).                    CI0256
            11            X158-NACID  PICTURE  S9(11)                   CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AGOFD2 PICTURE  S9(9)V99.                CI0256
            11            X158-TCBAT  PICTURE  X(21).                   CI0256
            11            X158-FILLER PICTURE  X(490).                  CI0256
            10            X158-QT5L                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-ALDBEN PICTURE  S9(09)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-APREL  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ALMODE PICTURE  99.                      CI0256
            11            X158-ITMEC  PICTURE  X(1).                    CI0256
            11            X158-ITAMR  PICTURE  X(1).                    CI0256
            11            X158-MPMTF  PICTURE  X(14).                   CI0256
            11            X158-TPLNL  PICTURE  X(30).                   CI0256
            11            X158-ASBENA PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASBENB PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASBENC PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASBENE PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASBENF PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-GESTNS PICTURE  X(2).                    CI0256
            11            X158-CTWHPB PICTURE  9(3)V999.                CI0256
            11            X158-CTWHCB PICTURE  X.                       CI0256
            11            X158-AMVA1  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASPAM  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ACTCH  PICTURE  S9(07)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AMXLN  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ALFGH  PICTURE  999.                     CI0256
            11            X158-ALPLNI PICTURE  9.                       CI0256
            11            X158-ATSA8  PICTURE  S9(07)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CVALB  PICTURE  X(3).                    CI0256
            11            X158-ASURRN PICTURE  S9(07)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ASURRW PICTURE  S9(07)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ATLTB  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AEARN0 PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ATFPI  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-AEARN1 PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ISELO  PICTURE  X.                       CI0256
            11            X158-CCLAC  PICTURE  X.                       CI0256
            11            X158-ALINNO PICTURE  99.                      CI0256
            11            X158-ALPLNJ PICTURE  9.                       CI0256
            11            X158-COLPL  PICTURE  9(05).                   CI0256
            11            X158-ALPLDT PICTURE  9(8).                    CI0256
            11            X158-ANFMC  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CPNOP  PICTURE  X(2).                    CI0256
            11            X158-CVSTC  PICTURE  X(4).                    CI0256
            11            X158-CGMBR  PICTURE  X.                       CI0256
            11            X158-DWSDT  PICTURE  9(8).                    CI0256
            11            X158-IRDPH  PICTURE  X.                       CI0256
            11            X158-DWAIT  PICTURE  9(8).                    CI0256
            11            X158-IAPGP  PICTURE  X.                       CI0256
            11            X158-CASTA  PICTURE  X.                       CI0256
            11            X158-CSSUP2 PICTURE  X.                       CI0256
            11            X158-CVOMC1 PICTURE  X(1).                    CI0256
            11            X158-APGBP  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ALDDUE PICTURE  9(08).                   CI0256
            11            X158-APYMT  PICTURE  S9(9)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ALSURR PICTURE  S9(09)V99                CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CESTP  PICTURE  X(03).                   CI0256
            11            X158-FILLER PICTURE  X(356).                  CI0256
            10            X158-QT5O                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-NBACT  PICTURE  S9(11)                   CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CTIAC  PICTURE  S9(3)                    CI0256
                          BINARY.                                       CI0256
            11            X158-CASTT  PICTURE  S99                      CI0256
                          BINARY.                                       CI0256
            11            X158-CATMI  PICTURE  S9                       CI0256
                          BINARY.                                       CI0256
            11            X158-IATMR  PICTURE  X(3).                    CI0256
            11            X158-IBIPI  PICTURE  X.                       CI0256
            11            X158-CBPST  PICTURE  S99                      CI0256
                          BINARY.                                       CI0256
            11            X158-TBPST  PICTURE  X(16).                   CI0256
            11            X158-CODPI  PICTURE  X.                       CI0256
            11            X158-TODPS  PICTURE  X(9).                    CI0256
            11            X158-FILLER PICTURE  X(448).                  CI0256
            11            X158-IBPSD  PICTURE  X.                       CI0256
            11            X158-FILLER PICTURE  X(107).                  CI0256
            11            X158-QT5E                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            12            X158-MPRN4X PICTURE  X(100).                  CI0256
            12            X158-CCMSH  PICTURE  X(2).                    CI0256
            12            X158-CPRCS  PICTURE  X(04).                   CI0256
            12            X158-CURST  PICTURE  X.                       CI0256
            10            X158-QT5M                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-NAPCN1 PICTURE  X(24).                   CI0256
            11            X158-FILLER PICTURE  X(576).                  CI0256
            10            X158-QT5B                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-NAPCN2 PICTURE  X(24).                   CI0256
            11            X158-CTIDAL PICTURE  X(40).                   CI0256
            11            X158-NPHNS  PICTURE  X(14).                   CI0256
            11            X158-FILLER PICTURE  X(522).                  CI0256
            10            X158-QT5P                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-CFPPT  PICTURE  9(3).                    CI0256
            11            X158-TTYPP  PICTURE  X(40).                   CI0256
            11            X158-CPPST  PICTURE  9(3).                    CI0256
            11            X158-TPPST  PICTURE  X(15).                   CI0256
            11            X158-APFEEQ PICTURE  S9(7)V99.                CI0256
            11            X158-APFEEC PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-APFEEP PICTURE  S9(7)V99.                CI0256
            11            X158-ISVCA  PICTURE  X.                       CI0256
            11            X158-NSBVS  PICTURE  X(5).                    CI0256
            11            X158-ICKRV  PICTURE  X.                       CI0256
            11            X158-PDAMT  PICTURE  S9(03).                  CI0256
            11            X158-PSTAX  PICTURE  S9(03)V999.              CI0256
            11            X158-DPCAL  PICTURE  9(8).                    CI0256
            11            X158-NADVF  PICTURE  X(08).                   CI0256
            11            X158-DAGUP  PICTURE  9(8).                    CI0256
            11            X158-AANFEA PICTURE  9(5)V99                  CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-CLIDN7 PICTURE  9(8)                     CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-ARANV  PICTURE  S9(7)V99                 CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            X158-DRANV  PICTURE  9(8).                    CI0256
            11            X158-FILLER PICTURE  X(454).                  CI0256
            10            X158-QT50                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-NANCA  PICTURE  X(30).                   CI0256
            11            X158-MANCN  PICTURE  X(100).                  CI0256
            11            X158-AINPTX PICTURE  X(20).                   CI0256
            11            X158-CTID01 PICTURE  X(27).                   CI0256
            11            X158-NANCA1 PICTURE  X(04).                   CI0256
            11            X158-IIVAR  PICTURE  X(1).                    CI0256
            11            X158-FILLER PICTURE  X(418).                  CI0256
            10            X158-QT5R                                     CI0256
                          REDEFINES            X158-FILLER.             CI0256
            11            X158-NACTJ  PICTURE  X(04).                   CI0256
            11            X158-NACNO6 PICTURE  X(11).                   CI0256
            11            X158-FILLER PICTURE  X(585).                  CI0256
            10            X158-AMAXA  PICTURE  S9(7)V99.                CI0256
            10            X158-ISAOR  PICTURE  X.                       CI0256
            10            X158-ISACH  PICTURE  X.                       CI0256
            10            X158-CERRBA PICTURE  X(02).                   CI0256
            10            X158-CERRBH PICTURE  X(02).                   CI0256
            10            X158-IWITHH PICTURE  X.                       CI0256
            10            X158-CTID20 PICTURE  X(27).                   CI0256
            10            X158-GECKD3 PICTURE  9.                       CI0256
            10            X158-DANFC  PICTURE  X(10).                   CI0256
            10            X158-DAFCN  PICTURE  X(10).                   CI0256
            10            X158-ISMTA  PICTURE  X.                       CI0256
            10            X158-CERRBT PICTURE  X(02).                   CI0256
            10            X158-NPLNI  PICTURE  X(10).                   CI0256
            10            X158-FILLER PICTURE  X(023).                  CI0256
      *
      ******************************************************************
      ** WORKING STORAGE SEGMENT FOR ACCOUNT TSQ RECORD                *
      ******************************************************************
      *!WF DSP=X1 DSL=QT SEL=58 FOR=I DES=1 LEV=1 PLT=TY
      *MISCELLANEOUS FIELDS
      *!WI
       01  WS-GETIMM
                        PICTURE X(8).                                   CI0256
       01  WS-GETIMN REDEFINES WS-GETIMM.
           05 WS-TIME1-HH   PIC 99.
           05 FILLER        PIC X.
           05 WS-TIME1-MM   PIC 99.
           05 FILLER        PIC X.
           05 WS-TIME1-SS   PIC 99.
       01  WS-GETIMT.
           05 WS-TIME2-HH   PIC X(2).
           05 WS-TIME2-MM   PIC X(2).
           05 WS-TIME2-SS   PIC X(2).
       01  WS00-WORKAREA.
           05 WS00-CTID.
      *!WS
              10 WS00-NCTIDE          VALUE SPACES
                        PICTURE 9999B9999B9999B9999.                    CI0256
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-GECKD1 PIC X    VALUE SPACE.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS00-CTIDA  PIC X(3) VALUE SPACES.
           05 WS01-CTID.
      *!WS
              10 WS01-NCTIDE          VALUE SPACES
                        PICTURE 9999B9999B9999B9999.                    CI0256
              10 FILLER      PIC X    VALUE SPACE.
              10 WS01-GECKD1 PIC X    VALUE SPACE.
              10 FILLER      PIC X    VALUE SPACE.
              10 WS01-CTIDA  PIC X(3) VALUE SPACES.
           05 WS00-RTN.
      *!WI
              10 WS00-NTR
                        PICTURE 9(8).                                   CI0256
              10 FILLER      PIC X    VALUE '-'.
      *!WI
              10 WS00-GECKD
                        PICTURE 9.                                      CI0256
      ******************************************************************
      *VARIABLE 'TIMER' IS USED TO HOLD THE TRANSACTION RECEIVED TIME.
      ******************************************************************
       01  WS00-TIMER    PIC X(06) VALUE SPACES.
       01  WS00-TIME-ENTERED REDEFINES WS00-TIMER.
           05 WS-TIME-HH   PIC 99.
           05 WS-TIME-MM   PIC 99.
           05 WS-TIME-SS   PIC 99.
      *
       01  WS00-AMPM     PIC X(03) VALUE SPACES.
      *
       01  WS00-CST      PIC X(12) VALUE SPACES.
      *
       01 WS00-DATCE  PIC X(8) VALUE ZEROES.
      ******************************************************************
      *THESE FIELDS USED TO INTERACT WITH TSQ UTILITY
      ******************************************************************
      *
       01  WT-TSQ-TYPE-FIELDS.
           05  WT-TYPE-A             PIC X(10)   VALUE 'ACCTLIST  '.
      *
       01   DEBUT-WSS.                                                  CI0256
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0256
            05   IK     PICTURE X.                                      CI0256
       01  CONSTANTES-PAC.                                              CI0256
           05  FILLER  PICTURE X(87)   VALUE                            CI0256
                     '6015 CAT09/08/14CI0256ADMIN   14:35:06CI0256P AMERCI0256
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0256
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0256
           05  NUGNA   PICTURE X(5).                                    CI0256
           05  APPLI   PICTURE X(3).                                    CI0256
           05  DATGN   PICTURE X(8).                                    CI0256
           05  PROGR   PICTURE X(6).                                    CI0256
           05  CODUTI  PICTURE X(8).                                    CI0256
           05  TIMGN   PICTURE X(8).                                    CI0256
           05  PROGE   PICTURE X(8).                                    CI0256
           05  COBASE  PICTURE X(4).                                    CI0256
           05  DATGNC  PICTURE X(10).                                   CI0256
           05  RELEAS  PICTURE X(7).                                    CI0256
           05  DATGE   PICTURE X(10).                                   CI0256
           05  DATSQ   PICTURE X(10).                                   CI0256
       01  DATCE.                                                       CI0256
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0256
         05  DATOR.                                                     CI0256
           10  DATOA  PICTURE XX.                                       CI0256
           10  DATOM  PICTURE XX.                                       CI0256
           10  DATOJ  PICTURE XX.                                       CI0256
       01  DAT6.                                                        CI0256
            10 DAT61.                                                   CI0256
            15 DAT619  PICTURE 99.                                      CI0256
            10 DAT62.                                                   CI0256
            15 DAT629  PICTURE 99.                                      CI0256
            10 DAT63   PICTURE XX.                                      CI0256
       01  DAT8.                                                        CI0256
            10 DAT81   PICTURE XX.                                      CI0256
            10 DAT8S1  PICTURE X.                                       CI0256
            10 DAT82   PICTURE XX.                                      CI0256
            10 DAT8S2  PICTURE X.                                       CI0256
            10 DAT83   PICTURE XX.                                      CI0256
       01  DAT8E    REDEFINES    DAT8.                                  CI0256
            10 DAT81E  PICTURE X(4).                                    CI0256
            10 DAT82E  PICTURE XX.                                      CI0256
            10 DAT83E  PICTURE XX.                                      CI0256
       01  DAT6C.                                                       CI0256
            10  DAT61C PICTURE XX.                                      CI0256
            10  DAT62C PICTURE XX.                                      CI0256
            10  DAT63C.                                                 CI0256
             15 DAT63CC PICTURE XX.                                     CI0256
             15 DAT64C  PICTURE XX.                                     CI0256
       01  DAT8C.                                                       CI0256
            10  DAT81C  PICTURE XX.                                     CI0256
            10  DAT8S1C PICTURE X   VALUE '/'.                          CI0256
            10  DAT82C  PICTURE XX.                                     CI0256
            10  DAT8S2C PICTURE X   VALUE '/'.                          CI0256
            10  DAT83C.                                                 CI0256
             15 DAT83CC PICTURE XX.                                     CI0256
             15 DAT84C  PICTURE XX.                                     CI0256
       01  DATSEP     PICTURE X VALUE '/'.                              CI0256
       01  DATSEW     PICTURE X.                                        CI0256
       01   VARIABLES-CONDITIONNELLES.                                  CI0256
            05                  FT      PICTURE X VALUE '0'.            CI0256
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0256
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0256
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J50DBR PICTURE S9(4) VALUE  ZERO.
            05           J70BDR PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0256
      ******************************************************************
      *COPYBOOK - VERIFY/CONFIRM HTML TEXT.                            *
      *THIS COPYBOOK CONTAINS THE HEADER, SOURCE ACCOUNT DETAILS AND   *
      *DESTINATION ACCOUNT DETAILS FOR THE HTML PAGE.                  *
      ******************************************************************
       COPY CI0256C1.
       LINKAGE SECTION.                                                 ADU102
      ******************************************************************
      **         THIS SEGMENT IS THE LINKAGE FOR CI0256                *
      ******************************************************************
      *!WF DSP=V2 DSL=V2 SEL=71 FOR=I LEV=1 PLT=80
       01                 V200.                                         CI0256
          05              V200-SUITE.                                   CI0256
            15       FILLER         PICTURE  X(01361).                  CI0256
       01                 V271  REDEFINES      V200.                    CI0256
            10            V271-NSSSI  PICTURE  X(24).                   CI0256
            10            V271-CTID.                                    CI0256
            11            V271-CTIDA  PICTURE  9(3).                    CI0256
            11            V271-CTIDN.                                   CI0256
            12            V271-CTIDNP PICTURE  X(13).                   CI0256
            12            V271-CTIDND PICTURE  9(11).                   CI0256
            10            V271-GECKD2 PICTURE  9.                       CI0256
            10            V271-CTTYPG PICTURE  X(04).                   CI0256
            10            V271-IVEUP  PICTURE  X.                       CI0256
            10            V271-CTTLN1 PICTURE  X(30).                   CI0256
            10            V271-CTTLN2 PICTURE  X(30).                   CI0256
            10            V271-CTTLN3 PICTURE  X(30).                   CI0256
            10            V271-CTTBO1 PICTURE  X(45).                   CI0256
            10            V271-CTTBO2 PICTURE  X(45).                   CI0256
            10            V271-CSPRDN PICTURE  X(30).                   CI0256
            10            V271-GEOPD2 PICTURE  X(8).                    CI0256
            10            V271-CQACTL PICTURE  X(45).                   CI0256
            10            V271-DAEDTO PICTURE  X(8).                    CI0256
            10            V271-DPTIM  PICTURE  X(8).                    CI0256
            10            V271-CSLCT  PICTURE  X.                       CI0256
            10            V271-NGEOR  PICTURE  9(08).                   CI0256
            10            V271-NGEOPA PICTURE  X(08).                   CI0256
            10            V271-DCACG  PICTURE  9(8).                    CI0256
            10            V271-DNACG  PICTURE  9(8).                    CI0256
            10            V271-DCACD  PICTURE  X(10).                   CI0256
            10            V271-ATROLL PICTURE  X(25).                   CI0256
            10            V271-MPLNR2 PICTURE  X(40).                   CI0256
            10            V271-CCONF  PICTURE  X(25).                   CI0256
            10            V271-CLORN  PICTURE  X(45).                   CI0256
            10            V271-NTR    PICTURE  9(8).                    CI0256
            10            V271-GECKD  PICTURE  9.                       CI0256
            10            V271-NPBN   PICTURE  X(20).                   CI0256
            10            V271-TTBAL  PICTURE  X(15).                   CI0256
            10            V271-MCSIG  PICTURE  X(30).                   CI0256
            10            V271-MCSIG1 PICTURE  X(30).                   CI0256
            10            V271-NROWT  PICTURE  9(3).                    CI0256
            10            V271-ST98                                     CI0256
                          OCCURS       010     TIMES.                   CI0256
            11            V271-CTID01 PICTURE  X(27).                   CI0256
            11            V271-NSEQ5  PICTURE  9(5).                    CI0256
            11            V271-IAMIT  PICTURE  X.                       CI0256
            11            V271-IAMOT  PICTURE  X.                       CI0256
            11            V271-FILLER PICTURE  X(20).                   CI0256
            10            V271-FILLER PICTURE  X(200).                  CI0256
      ******************************************************************
      ** HTML RETURNED TO CALLING RPC IN 200 BYTE CHUNKS               *
      ******************************************************************
      *
      *!WF DSP=HT DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HT00.                                         CI0256
          05              HT00-SUITE.                                   CI0256
            15       FILLER         PICTURE  X(90906).                  CI0256
       01                 HT93  REDEFINES      HT00.                    CI0256
            10            HT93-QBLCK  PICTURE  9(6).                    CI0256
            10            HT93-QT9O.                                    CI0256
            11            HT93-QT9B                                     CI0256
                          OCCURS       450     TIMES.                   CI0256
            12            HT93-CHTML  PICTURE  99.                      CI0256
            12            HT93-THTML  PICTURE  X(200).                  CI0256
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0256
          05              MS00-SUITE.                                   CI0256
            15       FILLER         PICTURE  X(00542).                  CI0256
       01                 MS03  REDEFINES      MS00.                    CI0256
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            10            MS03-CMSSF  PICTURE  XX.                      CI0256
            10            MS03-DU09.                                    CI0256
            11            MS03-CMESA  PICTURE  S9(9)                    CI0256
                          BINARY.                                       CI0256
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0256
                          BINARY.                                       CI0256
            11            MS03-CMESB  PICTURE  S9(9)                    CI0256
                          BINARY.                                       CI0256
            11            MS03-CMSST  PICTURE  S9(9)                    CI0256
                          BINARY.                                       CI0256
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0256
                          BINARY.                                       CI0256
            11            MS03-QELLAA PICTURE  S9(9)                    CI0256
                          BINARY.                                       CI0256
            11            MS03-TMESS4 PICTURE  X(512).                  CI0256
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0256
            10            MX11-QMSGS  PICTURE  9(03).                   CI0256
            10            MX11-PJ09                                     CI0256
                          OCCURS       025     TIMES.                   CI0256
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0256
                          COMPUTATIONAL-3.                              CI0256
            11            MX11-CMESB  PICTURE  S9(9)                    CI0256
                          BINARY.                                       CI0256
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V271
                                HT93
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0IAG.    NOTE *********************************   *.            ACMCTI
       F0IAG.                                                           lv10
      ** SUB-FUNCTION TO PERFORM A    *                                 ACMCTI
      ** DUMMY DB2 CALL.              *                                 ACMCTI
      *********************************                                 ACMCTI
           EXEC SQL    SET                                              ACMCTI
                        :WS00-DATE = CURRENT_DATE            END-EXEC.  ACMCTI
           PERFORM     F93SQ THRU F93SQ-FN.                             ACMCTI
       F0IAG-FN. EXIT.
      *N01.      NOTE *************************************.            CI0256
      *               *                                   *             CI0256
      *               *INITIALISATIONS                    *             CI0256
      *               *                                   *             CI0256
      *               *************************************.            CI0256
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0256
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0256
      *               *                                   *             CI0256
      *               *FIN DE TRAITEMENT                  *             CI0256
      *               *                                   *             CI0256
      *               *************************************.            CI0256
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0256
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N25.      NOTE *************************************.
      *               *                                   *
      *               *INPUT CALCULATIONS                 *
      *               *                                   *
      *               *************************************.
       F25.           EXIT.                                             lv05
      *N25DB.    NOTE *CALCULATIONS                       *.
       F25DB.         EXIT.                                             lv10
      *N25DD.    NOTE *MOVE LENGTH                        *.
       F25DD.                                                           lv15
      *
           MOVE        LENGTH OF HTML-BLOB TO HTML-LEN.
       F25DD-FN. EXIT.
      *N25DG.    NOTE *CALCULATE THE LOOP COUNT           *.
       F25DG.                                                           lv15
      *
           COMPUTE     HTML-LIMIT = HTML-LEN / 200.
       F25DG-FN. EXIT.
       F25DB-FN. EXIT.
       F25-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *BUILD THE COMMON INFO PORTION OF   *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *THE VERIFY & SUBMIT AND
      *SUBMISSION PAGES.
      *N35AT.    NOTE *MOVE COMMON HEADER INFORMATION     *.
       F35AT.                                                           lv10
           MOVE        1 TO HTML-PT
           STRING      HTML-COMMON-HDR-TAGS
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35AT-FN. EXIT.
      *N35BB.    NOTE *POPULATE HTML PASS AREA            *.
       F35BB.                                                           lv10
           MOVE        V271-CSPRDN TO HTML-PRCMN.
                 IF    V271-CSLCT = 'Y'                                 DOT
           MOVE        'YES' TO HTML-CSLCT
           MOVE        V271-NGEOR TO HTML-NGEOR
                 ELSE
           MOVE        'NO ' TO HTML-CSLCT
           MOVE        'N/A' TO HTML-NGEOR.
           MOVE        V271-CTTLN1 TO HTML-CTTLN1                       DOT
           MOVE        V271-CQACTL TO HTML-CQACTL
           MOVE        V271-GEOPD2 TO HTML-GEOPD2
           MOVE        V271-MPLNR2 TO HTML-MPLNR2
           MOVE        V271-ATROLL TO HTML-ATROLL
           MOVE        V271-CCONF TO HTML-CCONF
           MOVE        V271-NGEOPA TO HTML-NGEOPA.
       F35BB-FN. EXIT.
      *N35BC.    NOTE *FORMAT CONTRACT ID                 *.
       F35BC.                                                           lv10
      *
           MOVE        V271-CTIDND TO WS00-NCTIDE
           MOVE        V271-GECKD2 TO WS00-GECKD1
           MOVE        V271-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-CTID.
      *N35BD.    NOTE *FORMAT THE 'INTERNAL PROCESS       *.
       F35BD.                                                           lv15
      *NO.', 'DATE ENTERED', AND
      *'TIME ENTERED' FIELDS ONLY.
      *
           STRING      '000'
           V271-DCACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT
           INITIALIZE  I93B
           MOVE        V271-DCACD TO I93B-DACTT
           MOVE        'O' TO I93B-CEADC
           PERFORM     F96BB THRU F96BB-FN.
                 IF    I93B-GERTC NOT = 'Y'                             DOT
      *CHECK RETURN CODES
      *IF INVALID CODE, SEND ERROR MSG
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN                              ADU119
                 ELSE
      *MACRO RUN SUCCEED
           MOVE        I93B-GETIMM TO WS-GETIMM
           MOVE FUNCTION CURRENT-DATE(1:8)
                        TO  WS00-DATCE
           MOVE        WS-TIME1-HH TO WS-TIME2-HH
           MOVE        WS-TIME1-MM TO WS-TIME2-MM
           MOVE        WS-TIME1-SS TO WS-TIME2-SS
           MOVE FUNCTION CURRENT-DATE(9:6)
                        TO  WS00-TIMER
      *!ADS "WS00-DATCE     W-DE00-DCACG"
           MOVE        WS00-DATCE                                       CI0256
           TO DAT8E DAT6C                                               CI0256
           MOVE DAT81E TO DAT63C                                        CI0256
           MOVE DAT82E TO DAT61C  MOVE DAT83E TO DAT62C                 CI0256
           MOVE   DAT6C TO  W-DE00-DCACG                                CI0256
      *!ADM "W-DE00-DCACG     HTML-DCACG"
           MOVE        W-DE00-DCACG                                     CI0256
           TO DAT8E DAT6C                                               CI0256
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0256
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0256
           MOVE   DAT8C TO  HTML-DCACG.                                 CI0256
                 IF    (WS00-TIMER >= WS-GETIMT                         DOT
                 AND   V271-DCACG <= WS00-DATCE)
      *NEXT ACTG DATE IF AFTER MKT CLS
           STRING      '000' V271-DNACG (7:2)
           DELIMITED BY SIZE INTO
           HTML-DEFFT.
                 IF    WS00-TIMER (1:2) < 12                            DOT
           MOVE        'AM' TO WS00-AMPM
                 ELSE
           MOVE        'PM' TO WS00-AMPM.
                 IF    WS-TIME-HH > 12                                  DOT
           COMPUTE     WS-TIME-HH = WS-TIME-HH - 12.
                 IF    WS-TIME-HH = 00                                  DOT
           COMPUTE     WS-TIME-HH = WS-TIME-HH + 12.
           MOVE        'Central Time' TO WS00-CST                       DOT
           STRING      WS00-TIMER (1:2) ':'
           WS00-TIMER (3:2) ':'
           WS00-TIMER (5:2) ' '
           WS00-AMPM ' '
           WS00-CST
           DELIMITED BY SIZE INTO
           HTML-TIMER.
       F35BD-FN. EXIT.
      *N35BE.    NOTE *FORMAT 'ORDER RECEIVED DATE' AND   *.
       F35BE.                                                           lv15
      *'ORDER RECEIVED TIME' FIELDS
      *ONLY.
      *
      *!ADM "V271-DAEDTO     HTML-DRECD"
           MOVE        V271-DAEDTO                                      CI0256
           TO DAT8E DAT6C                                               CI0256
           MOVE DAT61C TO DAT81C  MOVE DAT62C TO DAT82C                 CI0256
           MOVE DAT63C TO DAT83C  MOVE DATSEP TO DAT8S1C DAT8S2C        CI0256
           MOVE   DAT8C TO  HTML-DRECD                                  CI0256
      *
           STRING      V271-DPTIM DELIMITED BY SIZE
           ' Central Time'
           DELIMITED BY SIZE
           INTO HTML-TIMER1.
       F35BE-FN. EXIT.
       F35BC-FN. EXIT.
      *N35BM.    NOTE *STRING CONFIRMATION INFORMATION    *.
       F35BM.    IF    V271-IVEUP = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F35BM-FN.
      *
           STRING      HTML-CONFIRM-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35BM-FN. EXIT.
      *N35DB.    NOTE *MOVE COMMON INFORMATION            *.
       F35DB.         EXIT.                                             lv10
       F35DB-FN. EXIT.
      *N35DM.    NOTE *STRING COMMOM INFORMATION          *.
       F35DM.                                                           lv10
           PERFORM     F95DM THRU F95DM-FN
           STRING      HTML-COMMON-INFO1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DM-FN. EXIT.
      *N35DP.    NOTE *BUILD OWNERSHIP LINE2              *.
       F35DP.    IF    V271-CTTLN2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DP-FN.
      *
           STRING      V271-CTTLN2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN2
           STRING      HTML-FROM-OWNER-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DP-FN. EXIT.
      *N35DR.    NOTE *BUILD OWNERSHIP LINE3              *.
       F35DR.    IF    V271-CTTLN3 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DR-FN.
      *
           STRING      V271-CTTLN3
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTLN3
           STRING      HTML-FROM-OWNER-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DR-FN. EXIT.
      *N35DU.    NOTE *BUILD BENEFICIARY LINE #1          *.
       F35DU.    IF    V271-CTTBO1 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DU-FN.
      *
           STRING      V271-CTTBO1
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO1
           STRING      HTML-FROM-BENE-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DU-FN. EXIT.
      *N35DW.    NOTE *BUILD BENEFICIARY LINE #2          *.
       F35DW.    IF    V271-CTTBO2 > SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DW-FN.
      *
           STRING      V271-CTTBO2
           DELIMITED BY LOW-VALUES
           INTO HTML-CTTBO2
           STRING      HTML-FROM-BENE-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35DW-FN. EXIT.
      *N35EM.    NOTE *STRING COMMOM INFORMATION          *.
       F35EM.                                                           lv10
      *
           STRING      HTML-COMMON-INFO2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F35EM-FN. EXIT.
       F35-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *BUILD THE TRANSACTION-SPECIFIC     *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *PORTION OF THE VERIFY & SUBMIT
      *AND SUBMISSION PAGES.
      *N50BC.    NOTE *FOR STEP1 TITLE                    *.
       F50BC.                                                           lv10
      *
           PERFORM     F95DP THRU F95DP-FN
           STRING      HTML-STEP1-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50BC-FN. EXIT.
      *N50CD.    NOTE *FOR BANK ACCOUNT INFO              *.
       F50CD.                                                           lv10
           MOVE        V271-CLORN TO HTML-CLORN
           MOVE        V271-NPBN TO HTML-NPBN
           MOVE        V271-NTR TO WS00-NTR
           MOVE        V271-GECKD TO WS00-GECKD
           MOVE        WS00-RTN TO HTML-NTRX
           MOVE        V271-TTBAL TO HTML-TTBAL
           MOVE        V271-MCSIG TO HTML-MCSIG
           MOVE        V271-MCSIG1 TO HTML-MCSIG1
           STRING      HTML-BANK-ACCOUNT-INFO
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50CD-FN. EXIT.
      *N50CH.    NOTE *FOR TO ACCOUNT INFO TITLE          *.
       F50CH.                                                           lv10
           STRING      HTML-AUTHORIZED-ACCOUNT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
      *N50DA.    NOTE *LOOP TO STRING THE TO ACCOUNT      *.
       F50DA.    IF    V271-NROWT > 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
      *INFO
      *N50DB.    NOTE *LOOP TO MOVE THE ACCOUNT           *.
       F50DB.                                                           lv20
           MOVE        1                        TO J50DBR
                                    GO TO     F50DB-B.
       F50DB-A.
           ADD         1                        TO J50DBR.
       F50DB-B.
           IF          J50DBR                   >  V271-NROWT
                                    GO TO     F50DB-FN.
      *DETAILS, 'ACH-IN SELECTED' AND
      *'ACH-OUT SELECTED' INDICATORS.
      *READ ACCOUNT TSQ TO GET ACCOUNT
      *INFO
           INITIALIZE  X101 X158
           MOVE        'READ' TO 7-TY00-XFUNC
           MOVE        'A' TO X101-CENTT
           MOVE        V271-CTID01 (J50DBR) TO X101-CANUMB
      *PERFORM TSQ CALL
           PERFORM     F91TY THRU F91TY-FN.
      *N50DD.    NOTE *MOVE THE VALUE FROM ACCOUNT TSQ    *.
       F50DD.                                                           lv25
           MOVE        X158-PRCMN TO HTML-PRCMNT
           MOVE        X158-CTTLN1 TO HTML-CTTLN1T
           MOVE        X158-CTTLN2 TO HTML-CTTLN2T
           MOVE        X158-CTTLN3 TO HTML-CTTLN3T
           MOVE        X158-CTTBO1 TO HTML-CTTBO1T
           MOVE        X158-CTTBO2 TO HTML-CTTBO2T.
       F50DD-FN. EXIT.
      *N50DF.    NOTE *FORMAT TO ACCOUNT ID               *.
       F50DF.                                                           lv25
           MOVE        X158-CTIDND TO WS01-NCTIDE
           MOVE        X158-GECKD2 TO WS01-GECKD1
           MOVE        X158-CTIDA TO WS01-CTIDA
           MOVE        WS01-CTID TO HTML-CTID01.
       F50DF-FN. EXIT.
      *N50DG.    NOTE *MOVE VALUE TO 'ACH-IN SELECTED'    *.
       F50DG.                                                           lv25
      *AND 'ACH-OUT SELECTED' INDICATOR
                 IF    V271-IAMIT (J50DBR) = 'Y'                        DOT
           MOVE        'ACH-In' TO HTML-ACHIN
                 ELSE
           MOVE        '&nbsp;' TO HTML-ACHIN.
                 IF    V271-IAMOT (J50DBR) = 'Y'                        DOT
           MOVE        'ACH-Out' TO HTML-ACHOUT
                 ELSE
           MOVE        '&nbsp;' TO HTML-ACHOUT.
       F50DG-FN. EXIT.
      *N50EA.    NOTE *LOOP TO STRING THE ACCOUNT         *.
       F50EA.                                                           lv25
      *DETAILS, 'ACH-IN SELECTED' AND
      *'ACH-OUT SELECTED' INDICATOR.
           STRING      HTML-ACCOUNT-LOOP                                DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    X158-PRCMN > SPACES                              DOT
           STRING      HTML-PRODUCT-NAME
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    X158-CTTLN1 > SPACES                             DOT
           STRING      HTML-OWNERSHIP-LINE1
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    X158-CTTLN2 > SPACES                             DOT
           STRING      HTML-OWNERSHIP-LINE2
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    X158-CTTLN3 > SPACES                             DOT
           STRING      HTML-OWNERSHIP-LINE3
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    X158-CTTBO1 > SPACES                             DOT
           STRING      HTML-OWNERSHIP-LINE4
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
                 IF    X158-CTTBO2 > SPACES                             DOT
           STRING      HTML-OWNERSHIP-LINE5
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-ACCOUNT-LOOP-CON                            DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
           STRING      HTML-TABLE-TR-END                                DOT
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50EA-FN. EXIT.
       F50DB-900. GO TO F50DB-A.
       F50DB-FN. EXIT.
      *N50ED.    NOTE *CONTINUE STRING THE REST INFO      *.
       F50ED.                                                           lv20
           STRING      HTML-ACCOUNT-LOOP-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50ED-FN. EXIT.
      *N50EG.    NOTE *STRING THE END INFORMATION         *.
       F50EG.                                                           lv20
           STRING      HTML-AUTHORIZED-ACCOUNT-END
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50EG-FN. EXIT.
       F50DA-FN. EXIT.
       F50CH-FN. EXIT.
      *N50FA.    NOTE *FOR STEP2 TITLE                    *.
       F50FA.                                                           lv10
           MOVE        'Read Important Messages' TO
           HTML-STEP2TT
           STRING      HTML-STEP2-TITLE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50FA-FN. EXIT.
      *N50FF.    NOTE *FOR IMPORTANT MESSAGES             *.
       F50FF.                                                           lv10
           STRING      HTML-IMP-MESSAGE
           DELIMITED BY SIZE
           INTO HTML-BLOB
           WITH POINTER HTML-PT.
       F50FF-FN. EXIT.
       F50-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *LOOPING THROUGH THE HTML BLOB      *
      *               *                                   *
      *               *************************************.
       F70.                                                             lv05
      *AND UNSTRINGING IT INTO THE
      *LINKAGE SECTION
           MOVE        1 TO TALLI.
      *N70BB.    NOTE *UNSTRING HTML BLOB                 *.
       F70BB.                                                           lv10
           MOVE        1 TO HTML-PT.
      *N70BD.    NOTE *LOOP THRU XML-BLOB                 *.
       F70BD.                                                           lv15
           MOVE        1                        TO J70BDR
                                    GO TO     F70BD-B.
       F70BD-A.
           ADD         1                        TO J70BDR.
       F70BD-B.
           IF          J70BDR                   >  HTML-LIMIT
                                    GO TO     F70BD-FN.
      *
           MOVE        HTML-PT TO TEMP-PT
           MOVE        TEMP-PT TO HTML-PT
           UNSTRING    HTML-BLOB  INTO  HTML-TEXT
                      WITH  POINTER HTML-PT.
      *N70BG.    NOTE *MOVE WS VARIABLE INTO THTML        *.
       F70BG.    IF    HTML-TEXT NOT = SPACES                           lv20
                 NEXT SENTENCE ELSE GO TO     F70BG-FN.
      *
           MOVE        01 TO HT93-CHTML (TALLI)
           MOVE        HTML-TEXT TO HT93-THTML (TALLI)
           ADD         1 TO TALLI.
       F70BG-FN. EXIT.
       F70BD-900. GO TO F70BD-A.
       F70BD-FN. EXIT.
       F70BB-FN. EXIT.
      *N70EB.    NOTE *MOVE END OF FILE MARKER            *.
       F70EB.                                                           lv10
      *
           MOVE        99 TO HT93-CHTML (TALLI)
           MOVE        SPACES TO HT93-THTML (TALLI).
      *N70EF.    NOTE *CALCULATE THTML LENGTH             *.
       F70EF.                                                           lv15
      *FIELDS BEING RETURNED IN LINKAGE
      *SECTION.
           COMPUTE     HT93-QBLCK = TALLI.
       F70EF-FN. EXIT.
       F70EB-FN. EXIT.
       F70-FN.   EXIT.
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
      *N91TY.    NOTE *CALL CI0218 TEMP STORAGE UTIL      *.            AM0218
       F91TY.                                                           lv10
      *                                                                 AM0218
           MOVE        7-TY00-XFUNC TO X101-XFUNC                       AM0218
           SET 7-TY00-NPNTRA TO NULL                                    AM0218
           STRING      'FDCX'                                           AM0218
           V271-NSSSI                                                   AM0218
           DELIMITED BY SIZE                                            AM0218
           INTO X101-MTQUE                                              AM0218
           CALL        CI0218 USING                                     AM0218
           DFHEIBLK                                                     AM0218
           X101                                                         AM0218
           MS03                                                         AM0218
           MX11                                                         AM0218
      *AREAS TO PASS GO BEFORE LINE 940                                 AM0218
                  WT-TYPE-A
                  X158
                  7-TY00-NPNTRA                                         AM0218
      *7-TY00-NPNTRA MUST BE THE LAST                                   AM0218
      *PARM PASSED TO CI0218                                            AM0218
                 IF    MS03-NMESS2 NOT = ZERO                           DOT
      *CHECK FOR ERROR IN CI0218                                        AM0218
           PERFORM     F98ET THRU F98ET-FN.                             AM0218
       F91TY-FN. EXIT.
      *N93.      NOTE *************************************.
      *               *                                   *
      *               *SQL ERROR HANDLING                 *
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
      *N95.      NOTE *************************************.
      *               *                                   *
      *               *SET THE HTML FIELDS BASING ON      *
      *               *                                   *
      *               *************************************.
       F95.                                                             lv05
      *THE REQUEST MODE
      *N95DM.    NOTE *ADJUST THE STYLE BASING ON THE     *.
       F95DM.                                                           lv10
      *REQUEST MODE
                 IF    V271-IVEUP = 'V'                                 DOT
      ******************************
           STRING      'STYLE="MARGIN-LEFT:5PX;'
           'MARGIN-RIGHT:35PX;"'
           DELIMITED BY SIZE
           INTO HTML-TAG1.
                 IF    V271-IVEUP = 'S'                                 DOT
      ******************************
           MOVE        '</TD></TR></TABLE>' TO
           HTML-TABLE-BOTTOM.
       F95DM-FN. EXIT.
      *N95DP.    NOTE *POPULATE TITLE LINES               *.
       F95DP.                                                           lv10
                 IF    V271-IVEUP = 'V'                                 DOT
      ******************************
           MOVE        TITLE-STEP-TEXT (1) TO
           HTML-STEP1T.
                 IF    V271-IVEUP = 'S'                                 DOT
      ******************************
           MOVE        TITLE-STEP-TEXT (2) TO
           HTML-STEP1T.
       F95DP-FN. EXIT.
       F95-FN.   EXIT.
      *N96.      NOTE *************************************.
      *               *                                   *
      *               *CALL THE MARKET CLOSE TIME MOD     *
      *               *                                   *
      *               *************************************.
       F96.           EXIT.                                             lv05
      *N96BB.    NOTE *********************************   *.            ACMCTI
       F96BB.                                                           lv10
      ** THIS SUBFUNCTION CALLS THE   *                                 ACMCTI
      ** ONLINE INQUIRY MODULE        *                                 ACMCTI
      ** (CI0361) TO RETRIEVE THE ROW *                                 ACMCTI
      ** INFORMATION FROM THE DB2     *                                 ACMCTI
      ** TABLE (TBS234) CORRESPONDING *                                 ACMCTI
      ** TO THE KEYS: 'DATE TYPE CODE'*                                 ACMCTI
      ** (ELEMENT: CEADC) AND         *                                 ACMCTI
      ** 'ACCOUNTING DATE'(ELEMENT:   *                                 ACMCTI
      ** DACTT).                      *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** MOVE VALUES TO THE LINKAGE   *                                 ACMCTI
      ** AREA ELEMENTS TO BE PASSED   *                                 ACMCTI
      ** TO THE CALLED MODULE CI0361. *                                 ACMCTI
      *********************************                                 ACMCTI
      *********************************                                 ACMCTI
      ** CALL THE ONLINE INQUIRY      *                                 ACMCTI
      ** MODULE (CI0361).             *                                 ACMCTI
      *********************************                                 ACMCTI
           CALL        CI0361 USING                                     ACMCTI
           DFHEIBLK                                                     ACMCTI
           DFHCOMMAREA                                                  ACMCTI
           I93B.                                                        ACMCTI
       F96BB-FN. EXIT.
       F96-FN.   EXIT.
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
