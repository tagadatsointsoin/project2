       IDENTIFICATION DIVISION.                                         CI0281
       PROGRAM-ID.  CI0281P.                                            CI0281
      *AUTHOR.         OST AA/IC HTML BUILD.                            CI0281
      *DATE-COMPILED.   09/08/14.                                       CI0281
       ENVIRONMENT DIVISION.                                            CI0281
       CONFIGURATION SECTION.                                           CI0281
       SOURCE-COMPUTER. IBM-370.                                        CI0281
       OBJECT-COMPUTER. IBM-370.                                        CI0281
       DATA DIVISION.                                                   CI0281
       WORKING-STORAGE SECTION.                                         CI0281
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      **** GENERATE INDEX USED TO POPULATE MESSAGE TABLE ***************
      *                   HTML

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

       01 HTML-PT PIC S9(8) COMP VALUE ZEROS.
      *MISCELLANEOUS FIELDS

      *!WI
       01  WS-FIRST-TIME-TFLAG        VALUE 'Y'
                        PICTURE X(999).                                 CI0281
           88 WS-FIRST-TIME           VALUE 'Y'.
           88 WS-NOT-FIRST-TIME       VALUE 'N'.
      *
      *!WI
       01  WS-FIRST-FROM-TFLAG        VALUE 'Y'
                        PICTURE X(999).                                 CI0281
           88 WS-FIRST-FROM           VALUE 'Y'.
           88 WS-NOT-FIRST-FROM       VALUE 'N'.
      *
      *!WI
       01  WS-FIRST-TO-TFLAG          VALUE 'Y'
                        PICTURE X(999).                                 CI0281
           88 WS-FIRST-TO             VALUE 'Y'.
           88 WS-NOT-FIRST-TO         VALUE 'N'.
      *
       01  WS00-CTID.
           05 FILLER      PIC X(5) VALUE '0000 '.
      *!WS
           05 WS00-NCTIDN
                        PICTURE 9999B9999B9999.                         CI0281
           05 FILLER      PIC X    VALUE SPACE.
      *!WI
           05 WS00-GECKD
                        PICTURE 9.                                      CI0281
           05 FILLER      PIC X    VALUE SPACE.
      *!WI
           05 WS00-CTIDA
                        PICTURE 9(3).                                   CI0281

       01  WS-AMOUNT.
           05 WS-ADBRQ       PIC $$,$$$,$$$.99.

       01  WS-PERCENT.
           05 WS-PSUBA       PIC ZZZ.999.
           05 FILLER         PIC X    VALUE '%'.
       01   DEBUT-WSS.                                                  CI0281
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0281
            05   IK     PICTURE X.                                      CI0281
       01  CONSTANTES-PAC.                                              CI0281
           05  FILLER  PICTURE X(87)   VALUE                            CI0281
                     '6015 CAT09/08/14CI0281ADMIN   14:35:13CI0281P AMERCI0281
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0281
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0281
           05  NUGNA   PICTURE X(5).                                    CI0281
           05  APPLI   PICTURE X(3).                                    CI0281
           05  DATGN   PICTURE X(8).                                    CI0281
           05  PROGR   PICTURE X(6).                                    CI0281
           05  CODUTI  PICTURE X(8).                                    CI0281
           05  TIMGN   PICTURE X(8).                                    CI0281
           05  PROGE   PICTURE X(8).                                    CI0281
           05  COBASE  PICTURE X(4).                                    CI0281
           05  DATGNC  PICTURE X(10).                                   CI0281
           05  RELEAS  PICTURE X(7).                                    CI0281
           05  DATGE   PICTURE X(10).                                   CI0281
           05  DATSQ   PICTURE X(10).                                   CI0281
       01  DATCE.                                                       CI0281
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0281
         05  DATOR.                                                     CI0281
           10  DATOA  PICTURE XX.                                       CI0281
           10  DATOM  PICTURE XX.                                       CI0281
           10  DATOJ  PICTURE XX.                                       CI0281
       01   VARIABLES-CONDITIONNELLES.                                  CI0281
            05                  FT      PICTURE X VALUE '0'.            CI0281
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0281
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0281
            05           IHTMLL PICTURE S9(4) VALUE  ZERO.
            05           IHTMLR PICTURE S9(4) VALUE  ZERO.
            05           IHTMLM PICTURE S9(4) VALUE +0025.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J60CFR PICTURE S9(4) VALUE  ZERO.
            05           J60DCR PICTURE S9(4) VALUE  ZERO.
            05           J60ECR PICTURE S9(4) VALUE  ZERO.
            05           J60EPR PICTURE S9(4) VALUE  ZERO.
            05           J60PER PICTURE S9(4) VALUE  ZERO.
            05           J60PGR PICTURE S9(4) VALUE  ZERO.
            05           J65CER PICTURE S9(4) VALUE  ZERO.
            05           J65CGR PICTURE S9(4) VALUE  ZERO.
            05           J65EER PICTURE S9(4) VALUE  ZERO.
            05           J65EGR PICTURE S9(4) VALUE  ZERO.
            05           J70CCR PICTURE S9(4) VALUE  ZERO.
            05           J70EDR PICTURE S9(4) VALUE  ZERO.
            05           J70EER PICTURE S9(4) VALUE  ZERO.
            05           J70EIR PICTURE S9(4) VALUE  ZERO.
            05           J75CER PICTURE S9(4) VALUE  ZERO.
            05           J75MCR PICTURE S9(4) VALUE  ZERO.
            05           J75TER PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0281
      *COPYBOOK WITH HTML TEXT

       COPY CI0281C1.

      *COPYBOOK WITH MESSAGE TEXT

       COPY CI0289MM.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **      THESE SEGMENTS ARE THE INPUT LINKAGE FOR CI0281          *
      ******************************************************************
      *
      *!WF DSP=V2 DSL=V2 SEL=49 FOR=I LEV=1 PLT=75
       01                 V200.                                         CI0281
          05              V200-SUITE.                                   CI0281
            15       FILLER         PICTURE  X(00881).                  CI0281
       01                 V249  REDEFINES      V200.                    CI0281
            10            V249-GCOMN.                                   CI0281
            11            V249-MAPPN  PICTURE  X(10).                   CI0281
            11            V249-NSSSI  PICTURE  X(24).                   CI0281
            11            V249-CTTYPG PICTURE  X(04).                   CI0281
            11            V249-DCACG  PICTURE  9(8).                    CI0281
            11            V249-CTSET  PICTURE  9(6).                    CI0281
            11            V249-QNACT  PICTURE  9(3).                    CI0281
            11            V249-CTID   PICTURE  X(27).                   CI0281
            11            V249-GETIM  PICTURE  S9(7)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-CAACT  PICTURE  X(1).                    CI0281
            11            V249-CALSTC PICTURE  X.                       CI0281
            11            V249-CACTS  PICTURE  X.                       CI0281
            11            V249-ADBRQ  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-PACT1  PICTURE  S999V999                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-QSHOWQ PICTURE  S9(9)V999                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-ADBRQ1 PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-PACT1A PICTURE  S999V999                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-QSHO5A PICTURE  S9(9)V999                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-GXREF.                                   CI0281
            12            V249-GACMS.                                   CI0281
            13            V249-XZ30   PICTURE  X(30).                   CI0281
            13            V249-FILLER                                   CI0281
                          REDEFINES            V249-XZ30.               CI0281
            14            V249-IAIND  PICTURE  X                        CI0281
                          OCCURS       030     TIMES.                   CI0281
            12            V249-GMSAC.                                   CI0281
            13            V249-XZ10   PICTURE  X(10).                   CI0281
            13            V249-FILLER                                   CI0281
                          REDEFINES            V249-XZ10.               CI0281
            14            V249-IACCT  PICTURE  X(1)                     CI0281
                          OCCURS       010     TIMES.                   CI0281
            11            V249-NGEOPA PICTURE  X(08).                   CI0281
            11            V249-CTRHO  PICTURE  9(8).                    CI0281
            11            V249-GETOD  PICTURE  9(6).                    CI0281
            11            V249-CSLCT  PICTURE  X.                       CI0281
            11            V249-CCLCH  PICTURE  X.                       CI0281
            11            V249-CCLPR  PICTURE  X.                       CI0281
            11            V249-CCLSU  PICTURE  X.                       CI0281
            11            V249-DXTMS2 PICTURE  X(26).                   CI0281
            11            V249-FILLER PICTURE  X(007).                  CI0281
            11            V249-GOTPT.                                   CI0281
            12            V249-CCONF  PICTURE  X(25).                   CI0281
            12            V249-XDCNN  PICTURE  X(17).                   CI0281
            12            V249-FILLER PICTURE  X(08).                   CI0281
            10            V249-GMSGS.                                   CI0281
            11            V249-NMESS2 PICTURE  S9(6)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-TDTXT1 PICTURE  X(512).                  CI0281
            11            V249-FILLER PICTURE  X(37).                   CI0281
            10            V249-GME87                                    CI0281
                          REDEFINES            V249-GMSGS.              CI0281
            11            V249-CEXTP  PICTURE  X.                       CI0281
            11            V249-AEDRQ  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-PWHLD  PICTURE  S999V9(5)                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-CPORTA PICTURE  X.                       CI0281
            11            V249-TIPUP  PICTURE  X(8).                    CI0281
            11            V249-DEFFT  PICTURE  9(8).                    CI0281
            11            V249-GTOAD.                                   CI0281
            12            V249-CDELIX PICTURE  X(3).                    CI0281
            11            V249-GTOAC.                                   CI0281
            12            V249-CTID01 PICTURE  X(27).                   CI0281
            11            V249-ITRNB  PICTURE  X.                       CI0281
            11            V249-CIRAP  PICTURE  XX.                      CI0281
            11            V249-CEXTP1 PICTURE  X.                       CI0281
            11            V249-ADBRQ2 PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-ATWHDD PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-ATWHDE PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-CTWHPB PICTURE  9(3)V999.                CI0281
            11            V249-ACOTD  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-TDTXTA PICTURE  X(80).                   CI0281
            11            V249-CPAYC  PICTURE  X(2).                    CI0281
            11            V249-CLID   PICTURE  X(23).                   CI0281
            11            V249-GECSQ  PICTURE  S9(3)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-NTR    PICTURE  9(8).                    CI0281
            11            V249-GECKD  PICTURE  9.                       CI0281
            11            V249-NPBN   PICTURE  X(20).                   CI0281
            11            V249-CCBAT  PICTURE  99.                      CI0281
            11            V249-GENAL1 PICTURE  X(30).                   CI0281
            11            V249-GENAL2 PICTURE  X(30).                   CI0281
            11            V249-GESAD1 PICTURE  X(30).                   CI0281
            11            V249-GESAD2 PICTURE  X(30).                   CI0281
            11            V249-GESAD3 PICTURE  X(30).                   CI0281
            10            V249-GME97                                    CI0281
                          REDEFINES            V249-GMSGS.              CI0281
            11            V249-GRID   PICTURE  X(13).                   CI0281
            11            V249-PRCOD  PICTURE  9(5).                    CI0281
            11            V249-CTIDA  PICTURE  9(3).                    CI0281
            11            V249-PRSCD  PICTURE  X(9).                    CI0281
            11            V249-PRCPRE PICTURE  X(4).                    CI0281
            11            V249-CEIT   PICTURE  9(3).                    CI0281
            11            V249-CMPFC  PICTURE  9(3).                    CI0281
            10            V249-GME11                                    CI0281
                          REDEFINES            V249-GMSGS.              CI0281
            11            V249-FILLER PICTURE  X.                       CI0281
            10            V249-GMD49                                    CI0281
                          REDEFINES            V249-GMSGS.              CI0281
            11            V249-CCEIT  PICTURE  X.                       CI0281
            11            V249-CEITX  PICTURE  9(3).                    CI0281
            10            V249-GME13                                    CI0281
                          REDEFINES            V249-GMSGS.              CI0281
            11            V249-CTTYP  PICTURE  X(04).                   CI0281
            10            V249-GMD43                                    CI0281
                          REDEFINES            V249-GMSGS.              CI0281
            11            V249-PERFE  PICTURE  S9(3)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            V249-PNRFE  PICTURE  S9(3)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            V249-GME20                                    CI0281
                          REDEFINES            V249-GMSGS.              CI0281
            11            V249-CPNOP  PICTURE  X(2).                    CI0281
            11            V249-PNPCT  PICTURE  999.                     CI0281
            11            V249-PNRFE1 PICTURE  S9(3)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            V249-MPRN4  PICTURE  X(35).                   CI0281
            10            V249-CTNOB1 PICTURE  X.                       CI0281
            10            V249-CTNOB2 PICTURE  X.                       CI0281
            10            V249-CTNOB3 PICTURE  X.                       CI0281
            10            V249-CRITO  PICTURE  X.                       CI0281
            10            V249-CTIFR  PICTURE  X.                       CI0281
            10            V249-AINVT  PICTURE  S9(11)                   CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            V249-NGEOR  PICTURE  9(08).                   CI0281
      *!WF DSP=FR DSL=QT SEL=58 FOR=I LEV=1 PLT=75
       01                 FR00.                                         CI0281
          05              FR00-SUITE.                                   CI0281
            15       FILLER         PICTURE  X(02300).                  CI0281
       01                 FR58  REDEFINES      FR00.                    CI0281
            10            FR58-QT5K.                                    CI0281
            11            FR58-C299.                                    CI0281
            12            FR58-CTID.                                    CI0281
            13            FR58-CTIDA  PICTURE  9(3).                    CI0281
            13            FR58-CTIDN.                                   CI0281
            14            FR58-CTIDNP PICTURE  X(13).                   CI0281
            14            FR58-CTIDND PICTURE  9(11).                   CI0281
            11            FR58-GECKD2 PICTURE  9.                       CI0281
            11            FR58-NSEQ5  PICTURE  9(5).                    CI0281
            11            FR58-CTSTA  PICTURE  99.                      CI0281
            11            FR58-CTSTAL PICTURE  X(10).                   CI0281
            11            FR58-CTOWN  PICTURE  9(3).                    CI0281
            11            FR58-CTTLN1 PICTURE  X(30).                   CI0281
            11            FR58-CTTLN2 PICTURE  X(30).                   CI0281
            11            FR58-CTTLN3 PICTURE  X(30).                   CI0281
            11            FR58-CTTBO1 PICTURE  X(45).                   CI0281
            11            FR58-CTTBO2 PICTURE  X(45).                   CI0281
            11            FR58-CTEFD  PICTURE  9(8).                    CI0281
            11            FR58-CTIAD  PICTURE  9(8).                    CI0281
            11            FR58-CTCUS  PICTURE  999.                     CI0281
            11            FR58-GR98.                                    CI0281
            12            FR58-GRID.                                    CI0281
            13            FR58-GRIDC  PICTURE  9(3).                    CI0281
            13            FR58-GRIDN.                                   CI0281
            14            FR58-GRIDNP PICTURE  99.                      CI0281
            14            FR58-GRIDND PICTURE  9(8).                    CI0281
            11            FR58-CQACT  PICTURE  999.                     CI0281
            11            FR58-CTCCI  PICTURE  X.                       CI0281
            11            FR58-CIRAS  PICTURE  999.                     CI0281
            11            FR58-CIRAT  PICTURE  999.                     CI0281
            11            FR58-IACVD  PICTURE  X.                       CI0281
            11            FR58-FILLER PICTURE  X(4).                    CI0281
            11            FR58-PRCODA PICTURE  X(5).                    CI0281
            11            FR58-PRCMN  PICTURE  X(20).                   CI0281
            11            FR58-MRPLN  PICTURE  X(30).                   CI0281
            11            FR58-CPRDG  PICTURE  9(2).                    CI0281
            11            FR58-CPRDA1 PICTURE  9(3).                    CI0281
            11            FR58-PRSCD  PICTURE  X(9).                    CI0281
            11            FR58-MSP03  PICTURE  X(3).                    CI0281
            11            FR58-CGRLI  PICTURE  X.                       CI0281
            11            FR58-ITERM  PICTURE  X(1).                    CI0281
            11            FR58-IVARP  PICTURE  X.                       CI0281
            11            FR58-DVALU  PICTURE  9(8).                    CI0281
            11            FR58-AACTV  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ACCTVC PICTURE  X(20).                   CI0281
            11            FR58-ITXTI  PICTURE  X.                       CI0281
            11            FR58-ASANP  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ACINV  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CELBL  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-NMESS2 PICTURE  S9(6)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-FILLER PICTURE  X(1).                    CI0281
            11            FR58-PRCLN  PICTURE  X(60).                   CI0281
            11            FR58-GECKD  PICTURE  9.                       CI0281
            11            FR58-MPLNA  PICTURE  X(19).                   CI0281
            11            FR58-CQACTL PICTURE  X(45).                   CI0281
            11            FR58-CRQPA  PICTURE  9(3).                    CI0281
            11            FR58-IVANT  PICTURE  X(1).                    CI0281
            11            FR58-IDBRP  PICTURE  X(1).                    CI0281
            11            FR58-IANPY  PICTURE  X.                       CI0281
            11            FR58-IVARP1 PICTURE  X.                       CI0281
            11            FR58-FILLER PICTURE  X(27).                   CI0281
            11            FR58-NSEQ2A PICTURE  S9(3)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-NSEQ2P PICTURE  S9(3)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-MRPSN  PICTURE  X(12).                   CI0281
            11            FR58-GEHCD  PICTURE  9(3)                     CI0281
                          OCCURS       002     TIMES.                   CI0281
            11            FR58-GEHCSU PICTURE  9(5)                     CI0281
                          OCCURS       002     TIMES.                   CI0281
            11            FR58-PRCSN  PICTURE  X(9).                    CI0281
            11            FR58-CGRMF  PICTURE  X.                       CI0281
            11            FR58-IGFEX  PICTURE  X.                       CI0281
            11            FR58-CLIDP  PICTURE  X(23).                   CI0281
            11            FR58-CLCTRC PICTURE  9(3).                    CI0281
            11            FR58-ADINP  PICTURE  X(20).                   CI0281
            11            FR58-CLCTRA PICTURE  9(3).                    CI0281
            11            FR58-GRPLC  PICTURE  99.                      CI0281
            11            FR58-CIDRP  PICTURE  99.                      CI0281
            11            FR58-FILLER PICTURE  X(01).                   CI0281
            11            FR58-AVMTOT PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AVCSH  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AMARC  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AVLMX  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AVLMN  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-INDRS  PICTURE  X.                       CI0281
            11            FR58-MPRN4  PICTURE  X(35).                   CI0281
            11            FR58-FILLER PICTURE  X(1).                    CI0281
            11            FR58-ACVALM PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-INDRSA PICTURE  X(2).                    CI0281
            11            FR58-DXTMSA PICTURE  X(26).                   CI0281
            11            FR58-NMESS6 PICTURE  S9(6)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-NMESS7 PICTURE  S9(6)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-IBIDSA PICTURE  X.                       CI0281
            11            FR58-IBIDSB PICTURE  X.                       CI0281
            11            FR58-INSPOS PICTURE  X.                       CI0281
            11            FR58-INSPOD PICTURE  X.                       CI0281
            11            FR58-ACBALX PICTURE  X(20).                   CI0281
            11            FR58-AINVMX PICTURE  X(20).                   CI0281
            11            FR58-AMARCX PICTURE  X(20).                   CI0281
            11            FR58-AVMTOX PICTURE  X(20).                   CI0281
            11            FR58-IMNPR  PICTURE  X.                       CI0281
            11            FR58-ISSPL  PICTURE  X.                       CI0281
            11            FR58-AVMTOI PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AVCSHI PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-APOSC  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AVLMXI PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AVLMN1 PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AVLMN2 PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-FILLER PICTURE  X(05).                   CI0281
            10            FR58-QT5A.                                    CI0281
            11            FR58-CLID   PICTURE  X(23).                   CI0281
            11            FR58-GECKD1 PICTURE  9.                       CI0281
            11            FR58-MCLNM  PICTURE  X(40).                   CI0281
            11            FR58-MCLNM2 PICTURE  X(40).                   CI0281
            11            FR58-CLTYP  PICTURE  X.                       CI0281
            11            FR58-CLDOB  PICTURE  9(8).                    CI0281
            11            FR58-CLDTH  PICTURE  X.                       CI0281
            11            FR58-CLTIN  PICTURE  9(12).                   CI0281
            11            FR58-CLTINC PICTURE  9.                       CI0281
            11            FR58-GESAD1 PICTURE  X(30).                   CI0281
            11            FR58-GESAD2 PICTURE  X(30).                   CI0281
            11            FR58-GESAD3 PICTURE  X(30).                   CI0281
            11            FR58-GECIT  PICTURE  X(25).                   CI0281
            11            FR58-GECTRY PICTURE  X(20).                   CI0281
            11            FR58-GEPCD  PICTURE  X(12).                   CI0281
            11            FR58-GEST   PICTURE  X(8).                    CI0281
            11            FR58-GEADS  PICTURE  9.                       CI0281
            11            FR58-GECSD  PICTURE  9(8).                    CI0281
            11            FR58-QCLAGE PICTURE  9(3)V9                   CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-FILLER PICTURE  X(06).                   CI0281
            10            FR58-QT5T.                                    CI0281
            11            FR58-ATFRA  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AGOFD  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-APRMX  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-APRMN  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-IOWNC  PICTURE  X.                       CI0281
            11            FR58-COWNF  PICTURE  X(30).                   CI0281
            11            FR58-CTYPE  PICTURE  X.                       CI0281
            11            FR58-CIRAC  PICTURE  X(5).                    CI0281
            11            FR58-CTXMT  PICTURE  9(2).                    CI0281
            11            FR58-AMIND  PICTURE  S9(7)V99.                CI0281
            11            FR58-AMAXAR PICTURE  S9(7)V99.                CI0281
            11            FR58-QSHOWQ PICTURE  S9(9)V999                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-QSHOW0 PICTURE  S9(10)V999               CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-PPOT1  PICTURE  S9(3)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-PACT1  PICTURE  S999V999                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-IPRTA  PICTURE  X.                       CI0281
            11            FR58-FILLER PICTURE  X.                       CI0281
            11            FR58-CLCUS  PICTURE  99.                      CI0281
            11            FR58-CCDSCW PICTURE  9(2).                    CI0281
            11            FR58-CCACT  PICTURE  99.                      CI0281
            11            FR58-CIRAG.                                   CI0281
            12            FR58-CIRAP  PICTURE  XX                       CI0281
                          OCCURS       010     TIMES.                   CI0281
            11            FR58-ITERF  PICTURE  X.                       CI0281
            11            FR58-IACFPD PICTURE  X(1).                    CI0281
            11            FR58-AFEET  PICTURE  S9(5)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ATERF  PICTURE  S9(5)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CLIDNB PICTURE  9(8).                    CI0281
            11            FR58-ALOAD  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASURR  PICTURE  S9(07)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASHIS  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AMNBL  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-APNAC  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ANGOF  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CPLTYP PICTURE  X(14).                   CI0281
            10            FR58-QT5N.                                    CI0281
            11            FR58-IARRAN PICTURE  X.                       CI0281
            11            FR58-GESTD1 PICTURE  9(8).                    CI0281
            11            FR58-GEEND1 PICTURE  S9(8)                    CI0281
                          BINARY.                                       CI0281
            11            FR58-GESTD  PICTURE  9(8).                    CI0281
            11            FR58-GEEND  PICTURE  9(8).                    CI0281
            11            FR58-NSQ4B2 PICTURE  9(8)                     CI0281
                          BINARY.                                       CI0281
            11            FR58-CDEST  PICTURE  99.                      CI0281
            11            FR58-DEFFT  PICTURE  9(8).                    CI0281
            11            FR58-CPMTF  PICTURE  99.                      CI0281
            11            FR58-CPMTG  PICTURE  99.                      CI0281
            11            FR58-MPMTFL PICTURE  X(24).                   CI0281
            11            FR58-MPMTFE PICTURE  X(24).                   CI0281
            11            FR58-DLAUP  PICTURE  9(8).                    CI0281
            11            FR58-NSEQ4B PICTURE  9(8)                     CI0281
                          BINARY.                                       CI0281
            11            FR58-QSACTF PICTURE  9(3).                    CI0281
            11            FR58-QSACTT PICTURE  9(3).                    CI0281
            11            FR58-CCONF  PICTURE  X(25).                   CI0281
            11            FR58-DCONF  PICTURE  9(8).                    CI0281
            11            FR58-DTIMT  PICTURE  X(8).                    CI0281
            11            FR58-CACTS  PICTURE  X.                       CI0281
            11            FR58-ADBRQ  PICTURE  S9(11)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-DNPMT  PICTURE  9(8).                    CI0281
            11            FR58-NAPDS  PICTURE  S9(3)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CDEST1 PICTURE  99.                      CI0281
            11            FR58-CLANR1 PICTURE  X(23).                   CI0281
            11            FR58-FILLER PICTURE  X(01).                   CI0281
            10            FR58-FILLER PICTURE  X(600).                  CI0281
            10            FR58-QT5C                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-CESLD  PICTURE  9(8).                    CI0281
            11            FR58-PCIRB5 PICTURE  S9(3)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-PANYDD PICTURE  S9(3)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CEIT   PICTURE  9(3).                    CI0281
            11            FR58-PPART  PICTURE  9(3)V99                  CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-DTRME  PICTURE  9(8).                    CI0281
            11            FR58-CEIRND PICTURE  9(8).                    CI0281
            11            FR58-DANNIA PICTURE  9(8).                    CI0281
            11            FR58-AAPAA  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CELBDT PICTURE  9(8).                    CI0281
            11            FR58-CEIIS  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-DTRME1 PICTURE  9(8).                    CI0281
            11            FR58-GMKTS.                                   CI0281
            12            FR58-DTRME2 PICTURE  9(8)                     CI0281
                          OCCURS       005     TIMES.                   CI0281
            12            FR58-DTRME3 PICTURE  9(8)                     CI0281
                          OCCURS       005     TIMES.                   CI0281
            11            FR58-ALINT  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CEHCD  PICTURE  9(3)                     CI0281
                          OCCURS       006     TIMES.                   CI0281
            11            FR58-CEFOTR PICTURE  S9(3)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-DGPED  PICTURE  9(8).                    CI0281
            11            FR58-DIPED  PICTURE  9(8).                    CI0281
            11            FR58-FILLER PICTURE  X(409).                  CI0281
            10            FR58-QT5F                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-DLAUP2 PICTURE  9(8).                    CI0281
            11            FR58-QSHOW  PICTURE  S9(10)V999               CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AFAVP  PICTURE  S9(4)V9(3)               CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-QSHIS  PICTURE  S9(10)V999               CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-QSHNM  PICTURE  S9(10)V999.              CI0281
            11            FR58-QSHOM  PICTURE  S9(10)V999               CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ADDAC  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-QSHES  PICTURE  S9(10)V999               CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-NDCUS  PICTURE  X(9).                    CI0281
            11            FR58-CSTKR5 PICTURE  X(5).                    CI0281
            11            FR58-NACID  PICTURE  S9(11)                   CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AGOFD2 PICTURE  S9(9)V99.                CI0281
            11            FR58-TCBAT  PICTURE  X(21).                   CI0281
            11            FR58-FILLER PICTURE  X(490).                  CI0281
            10            FR58-QT5L                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-ALDBEN PICTURE  S9(09)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-APREL  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ALMODE PICTURE  99.                      CI0281
            11            FR58-ITMEC  PICTURE  X(1).                    CI0281
            11            FR58-ITAMR  PICTURE  X(1).                    CI0281
            11            FR58-MPMTF  PICTURE  X(14).                   CI0281
            11            FR58-TPLNL  PICTURE  X(30).                   CI0281
            11            FR58-ASBENA PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASBENB PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASBENC PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASBENE PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASBENF PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-GESTNS PICTURE  X(2).                    CI0281
            11            FR58-CTWHPB PICTURE  9(3)V999.                CI0281
            11            FR58-CTWHCB PICTURE  X.                       CI0281
            11            FR58-AMVA1  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASPAM  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ACTCH  PICTURE  S9(07)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AMXLN  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ALFGH  PICTURE  999.                     CI0281
            11            FR58-ALPLNI PICTURE  9.                       CI0281
            11            FR58-ATSA8  PICTURE  S9(07)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CVALB  PICTURE  X(3).                    CI0281
            11            FR58-ASURRN PICTURE  S9(07)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ASURRW PICTURE  S9(07)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ATLTB  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AEARN0 PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ATFPI  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-AEARN1 PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ISELO  PICTURE  X.                       CI0281
            11            FR58-CCLAC  PICTURE  X.                       CI0281
            11            FR58-ALINNO PICTURE  99.                      CI0281
            11            FR58-ALPLNJ PICTURE  9.                       CI0281
            11            FR58-COLPL  PICTURE  9(05).                   CI0281
            11            FR58-ALPLDT PICTURE  9(8).                    CI0281
            11            FR58-ANFMC  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CPNOP  PICTURE  X(2).                    CI0281
            11            FR58-CVSTC  PICTURE  X(4).                    CI0281
            11            FR58-CGMBR  PICTURE  X.                       CI0281
            11            FR58-DWSDT  PICTURE  9(8).                    CI0281
            11            FR58-IRDPH  PICTURE  X.                       CI0281
            11            FR58-DWAIT  PICTURE  9(8).                    CI0281
            11            FR58-IAPGP  PICTURE  X.                       CI0281
            11            FR58-CASTA  PICTURE  X.                       CI0281
            11            FR58-CSSUP2 PICTURE  X.                       CI0281
            11            FR58-CVOMC1 PICTURE  X(1).                    CI0281
            11            FR58-APGBP  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ALDDUE PICTURE  9(08).                   CI0281
            11            FR58-APYMT  PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ALSURR PICTURE  S9(09)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CESTP  PICTURE  X(03).                   CI0281
            11            FR58-FILLER PICTURE  X(356).                  CI0281
            10            FR58-QT5O                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-NBACT  PICTURE  S9(11)                   CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CTIAC  PICTURE  S9(3)                    CI0281
                          BINARY.                                       CI0281
            11            FR58-CASTT  PICTURE  S99                      CI0281
                          BINARY.                                       CI0281
            11            FR58-CATMI  PICTURE  S9                       CI0281
                          BINARY.                                       CI0281
            11            FR58-IATMR  PICTURE  X(3).                    CI0281
            11            FR58-IBIPI  PICTURE  X.                       CI0281
            11            FR58-CBPST  PICTURE  S99                      CI0281
                          BINARY.                                       CI0281
            11            FR58-TBPST  PICTURE  X(16).                   CI0281
            11            FR58-CODPI  PICTURE  X.                       CI0281
            11            FR58-TODPS  PICTURE  X(9).                    CI0281
            11            FR58-FILLER PICTURE  X(448).                  CI0281
            11            FR58-IBPSD  PICTURE  X.                       CI0281
            11            FR58-FILLER PICTURE  X(107).                  CI0281
            11            FR58-QT5E                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            12            FR58-MPRN4X PICTURE  X(100).                  CI0281
            12            FR58-CCMSH  PICTURE  X(2).                    CI0281
            12            FR58-CPRCS  PICTURE  X(04).                   CI0281
            12            FR58-CURST  PICTURE  X.                       CI0281
            10            FR58-QT5M                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-NAPCN1 PICTURE  X(24).                   CI0281
            11            FR58-FILLER PICTURE  X(576).                  CI0281
            10            FR58-QT5B                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-NAPCN2 PICTURE  X(24).                   CI0281
            11            FR58-CTIDAL PICTURE  X(40).                   CI0281
            11            FR58-NPHNS  PICTURE  X(14).                   CI0281
            11            FR58-FILLER PICTURE  X(522).                  CI0281
            10            FR58-QT5P                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-CFPPT  PICTURE  9(3).                    CI0281
            11            FR58-TTYPP  PICTURE  X(40).                   CI0281
            11            FR58-CPPST  PICTURE  9(3).                    CI0281
            11            FR58-TPPST  PICTURE  X(15).                   CI0281
            11            FR58-APFEEQ PICTURE  S9(7)V99.                CI0281
            11            FR58-APFEEC PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-APFEEP PICTURE  S9(7)V99.                CI0281
            11            FR58-ISVCA  PICTURE  X.                       CI0281
            11            FR58-NSBVS  PICTURE  X(5).                    CI0281
            11            FR58-ICKRV  PICTURE  X.                       CI0281
            11            FR58-PDAMT  PICTURE  S9(03).                  CI0281
            11            FR58-PSTAX  PICTURE  S9(03)V999.              CI0281
            11            FR58-DPCAL  PICTURE  9(8).                    CI0281
            11            FR58-NADVF  PICTURE  X(08).                   CI0281
            11            FR58-DAGUP  PICTURE  9(8).                    CI0281
            11            FR58-AANFEA PICTURE  9(5)V99                  CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-CLIDN7 PICTURE  9(8)                     CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-ARANV  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            FR58-DRANV  PICTURE  9(8).                    CI0281
            11            FR58-FILLER PICTURE  X(454).                  CI0281
            10            FR58-QT50                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-NANCA  PICTURE  X(30).                   CI0281
            11            FR58-MANCN  PICTURE  X(100).                  CI0281
            11            FR58-AINPTX PICTURE  X(20).                   CI0281
            11            FR58-CTID01 PICTURE  X(27).                   CI0281
            11            FR58-NANCA1 PICTURE  X(04).                   CI0281
            11            FR58-IIVAR  PICTURE  X(1).                    CI0281
            11            FR58-FILLER PICTURE  X(418).                  CI0281
            10            FR58-QT5R                                     CI0281
                          REDEFINES            FR58-FILLER.             CI0281
            11            FR58-NACTJ  PICTURE  X(04).                   CI0281
            11            FR58-NACNO6 PICTURE  X(11).                   CI0281
            11            FR58-FILLER PICTURE  X(585).                  CI0281
            10            FR58-AMAXA  PICTURE  S9(7)V99.                CI0281
            10            FR58-ISAOR  PICTURE  X.                       CI0281
            10            FR58-ISACH  PICTURE  X.                       CI0281
            10            FR58-CERRBA PICTURE  X(02).                   CI0281
            10            FR58-CERRBH PICTURE  X(02).                   CI0281
            10            FR58-IWITHH PICTURE  X.                       CI0281
            10            FR58-CTID20 PICTURE  X(27).                   CI0281
            10            FR58-GECKD3 PICTURE  9.                       CI0281
            10            FR58-DANFC  PICTURE  X(10).                   CI0281
            10            FR58-DAFCN  PICTURE  X(10).                   CI0281
            10            FR58-ISMTA  PICTURE  X.                       CI0281
            10            FR58-CERRBT PICTURE  X(02).                   CI0281
            10            FR58-NPLNI  PICTURE  X(10).                   CI0281
            10            FR58-FILLER PICTURE  X(023).                  CI0281
      *!WF DSP=K9 DSL=K9 SEL=70 FOR=I LEV=1 PLT=75
       01                 K900.                                         CI0281
          05              K900-SUITE.                                   CI0281
            15       FILLER         PICTURE  X(00274).                  CI0281
       01                 K970  REDEFINES      K900.                    CI0281
            10            K970-MFDNMS PICTURE  X(30).                   CI0281
            10            K970-ALPALC PICTURE  S9(04)V999               CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-PSUBA  PICTURE  S999V999                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-PMDAP  PICTURE  S9(4)V9(5)               CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-CFIDC  PICTURE  X(5).                    CI0281
            10            K970-NSEQ4  PICTURE  9(4).                    CI0281
            10            K970-CACCT  PICTURE  X.                       CI0281
            10            K970-AACTVF PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-AACTVG PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-PSUBA1 PICTURE  S999V999                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-ADBRQA PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-ADBRQC PICTURE  S9(9)V99.                CI0281
            10            K970-ADBRQF PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-ADBRQG PICTURE  S9(9)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-PACT1  PICTURE  S999V999                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-PACT1A PICTURE  S999V999                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-IALLV  PICTURE  X.                       CI0281
            10            K970-ITRNB  PICTURE  X.                       CI0281
            10            K970-AEDRQ1 PICTURE  S9(09)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-AEDRQ2 PICTURE  S9(09)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-PVFPA  PICTURE  S9(3)V9(1)               CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-ISUBA1 PICTURE  X.                       CI0281
            10            K970-ISUBA2 PICTURE  X.                       CI0281
            10            K970-DRSNC1 PICTURE  9(8).                    CI0281
            10            K970-DRSNC2 PICTURE  9(8).                    CI0281
            10            K970-CRSNG1 PICTURE  X(02).                   CI0281
            10            K970-CRSNG2 PICTURE  X(02).                   CI0281
            10            K970-AMAXD  PICTURE  S9(7)V99                 CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-CAMOT4 PICTURE  X(1).                    CI0281
            10            K970-CAMOT5 PICTURE  X(1).                    CI0281
            10            K970-ALLNB  PICTURE  S9(07)V99                CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-ALSHR1 PICTURE  S9(7)V9(6)               CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-ALPRCV PICTURE  S9(3)V9(6)               CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            K970-QSACTF PICTURE  9(3).                    CI0281
            10            K970-QSACTT PICTURE  9(3).                    CI0281
            10            K970-CACCT1 PICTURE  X.                       CI0281
            10            K970-IFTDY1 PICTURE  X.                       CI0281
            10            K970-FILLER PICTURE  X(99).                   CI0281
      *
      ******************************************************************
      **         THIS SEGMENT IS THE OUTPUT LINKAGE FOR CI0281         *
      ******************************************************************
      *
      *!WF DSP=HM DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HM00.                                         CI0281
          05              HM00-SUITE.                                   CI0281
            15       FILLER         PICTURE  X(90906).                  CI0281
       01                 HM93  REDEFINES      HM00.                    CI0281
            10            HM93-QBLCK  PICTURE  9(6).                    CI0281
            10            HM93-QT9O.                                    CI0281
            11            HM93-QT9B                                     CI0281
                          OCCURS       450     TIMES.                   CI0281
            12            HM93-CHTML  PICTURE  99.                      CI0281
            12            HM93-THTML  PICTURE  X(200).                  CI0281
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0281
          05              MS00-SUITE.                                   CI0281
            15       FILLER         PICTURE  X(00542).                  CI0281
       01                 MS03  REDEFINES      MS00.                    CI0281
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            10            MS03-CMSSF  PICTURE  XX.                      CI0281
            10            MS03-DU09.                                    CI0281
            11            MS03-CMESA  PICTURE  S9(9)                    CI0281
                          BINARY.                                       CI0281
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0281
                          BINARY.                                       CI0281
            11            MS03-CMESB  PICTURE  S9(9)                    CI0281
                          BINARY.                                       CI0281
            11            MS03-CMSST  PICTURE  S9(9)                    CI0281
                          BINARY.                                       CI0281
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0281
                          BINARY.                                       CI0281
            11            MS03-QELLAA PICTURE  S9(9)                    CI0281
                          BINARY.                                       CI0281
            11            MS03-TMESS4 PICTURE  X(512).                  CI0281
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0281
            10            MX11-QMSGS  PICTURE  9(03).                   CI0281
            10            MX11-PJ09                                     CI0281
                          OCCURS       025     TIMES.                   CI0281
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0281
                          COMPUTATIONAL-3.                              CI0281
            11            MX11-CMESB  PICTURE  S9(9)                    CI0281
                          BINARY.                                       CI0281
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V249
                                FR58
                                K970
                                HM93
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0281
      *               *                                   *             CI0281
      *               *INITIALISATIONS                    *             CI0281
      *               *                                   *             CI0281
      *               *************************************.            CI0281
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
      *N02CA.    NOTE *---> ALL HTML LINES                *.
       F02CA.    IF    WS-FIRST-TIME                                    lv10
                 NEXT SENTENCE ELSE GO TO     F02CA-FN.
           INITIALIZE  HTML-VERIFY-TAGS
           HTML-PRODUCT-TAGS
           HTML-ACCOUNT-TAGS
           HTML-EO-ACCOUNT
           HTML-REQUEST-TAGS
           HTML-REVIEW-TAGS
           HTML-DETAIL-TAGS
           HTML-EO-DETAIL-TAGS
           HTML-SUBACCT-TAGS
           HTML-MESSAGE1-TAGS
           HTML-IMPLY-TAGS
           HTML-NOTES-TAGS
           HTML-MESSAGE-TAGS
           HTML-MESSAGE-ARRAY
           HTML-MESSAGE-LI
           HTML-EOM-TAGS.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0281
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0281
      *               *                                   *             CI0281
      *               *FIN DE TRAITEMENT                  *             CI0281
      *               *                                   *             CI0281
      *               *************************************.            CI0281
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0281
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *
      *SET FIRST-TIME-FLAG OFF
           SET WS-NOT-FIRST-TIME TO TRUE
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *BUILD COMMON HTML TAGS             *
      *               *                                   *
      *               *************************************.
       F35.      IF    WS-FIRST-TIME                                    lv05
                 NEXT SENTENCE ELSE GO TO     F35-FN.
      *N35DB.    NOTE *FORMAT THE SAT TAGS                *.
       F35DB.    IF    V249-CTTYPG = 'INVC'                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
           MOVE        CN-INVC TO HTML-TRAN-TYPE
           MOVE        4 TO HTML-STEP-R.
       F35DB-FN. EXIT.
      *N35DC.    NOTE *FORMAT THE AALL TAG                *.
       F35DC.    IF    V249-CTTYPG = 'AALL'                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DC-FN.
           MOVE        CN-AALL TO HTML-TRAN-TYPE
           MOVE        3 TO HTML-STEP-R.
       F35DC-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *BUILD ACCOUNT/TRANSACTION TAGS     *
      *               *                                   *
      *               *************************************.
       F40.      IF    WS-FIRST-TIME                                    lv05
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *N40BC.    NOTE *STRING ACCOUNT NAME                *.
       F40BC.                                                           lv10
                 IF    FR58-TPLNL = SPACES                              DOT
           MOVE        FR58-PRCMN TO HTML-PRCMN
                 ELSE
           MOVE        FR58-TPLNL TO HTML-PRCMN.
       F40BC-FN. EXIT.
      *N40BE.    NOTE *STRING QUALIFIED PLAN TYPE         *.
       F40BE.                                                           lv10
           MOVE        FR58-CQACTL TO HTML-CQACTL.
       F40BE-FN. EXIT.
      *N40BG.    NOTE *BUILD OWNERSHIP LINE(S)            *.
       F40BG.                                                           lv10
           MOVE        FR58-CTTLN1 TO HTML-CTTLN1.
                 IF    FR58-CTTLN2 > SPACES                             DOT
           MOVE        FR58-CTTLN2 TO HTML-CTTLN2.
                 IF    FR58-CTTLN3 > SPACES                             DOT
           MOVE        FR58-CTTLN3 TO HTML-CTTLN3.
                 IF    FR58-CTTBO1 > SPACES                             DOT
           MOVE        FR58-CTTBO1 TO HTML-CTTBO1.
                 IF    FR58-CTTBO2 > SPACES                             DOT
           MOVE        FR58-CTTBO2 TO HTML-CTTBO2.
       F40BG-FN. EXIT.
      *N40CB.    NOTE *FORMAT CONTRACT ID                 *.
       F40CB.                                                           lv10
           MOVE        FR58-CTIDND TO WS00-NCTIDN
           MOVE        FR58-GECKD2 TO WS00-GECKD
           MOVE        FR58-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-CTID.
       F40CB-FN. EXIT.
      *N40EC.    NOTE *BUILD REQUEST TYPE                 *.
       F40EC.                                                           lv10
           MOVE        CN-CREATE TO HTML-REQUEST.
       F40EC-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *BUILD SUB-ACCOUNT DETAIL           *
      *               *                                   *
      *               *************************************.
       F45.      IF    V249-CAACT = 'S'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *N45BC.    NOTE *SUB ACCOUNT TRANSFER HEADER        *.
       F45BC.    IF    V249-CTTYPG = 'INVC'                             lv10
                 NEXT SENTENCE ELSE GO TO     F45BC-FN.
      *N45BF.    NOTE *BUILD FROM ACCOUNT TITLE           *.
       F45BF.    IF    K970-CACCT = 'F'                                 lv15
                 AND   WS-FIRST-FROM
                 AND   V249-CACTS = 'E'
                 NEXT SENTENCE ELSE GO TO     F45BF-FN.
      *FOR VERIFY PAGE
           MOVE        CN-STEP-2 TO HTML-STEP
           MOVE        CN-VERIFY-FROM TO HTML-REVIEW
           MOVE        CN-MONEY-FROM TO HTML-TRPTH
           MOVE        CN-SUB-ACCOUNT TO HTML-TRPTH1
           MOVE        CN-AMOUNT-MOVED TO HTML-TRPTH2
           MOVE        CN-DOLLAR-AMOUNT TO HTML-TRPTH3.
       F45BF-FN. EXIT.
      *N45BK.    NOTE *BUILD FROM ACCOUNT TITLE           *.
       F45BK.    IF    K970-CACCT = 'F'                                 lv15
                 AND   V249-CACTS = 'S'
                 NEXT SENTENCE ELSE GO TO     F45BK-FN.
      *FOR CONFIRMATION PAGE
           MOVE        CN-MONEY-FROM TO HTML-TRPTH
           MOVE        CN-SUB-ACCOUNT TO HTML-TRPTH1
           MOVE        CN-AMOUNT-MOVED TO HTML-TRPTH2
           MOVE        CN-DOLLAR-AMOUNT TO HTML-TRPTH3.
       F45BK-FN. EXIT.
      *N45BT.    NOTE *BUILD TO ACCOUNT TITLE             *.
       F45BT.    IF    K970-CACCT = 'T'                                 lv15
                 AND   WS-FIRST-TO
                 AND   V249-CACTS = 'E'
                 NEXT SENTENCE ELSE GO TO     F45BT-FN.
      *FOR VERIFY PAGE
           MOVE        CN-STEP-3 TO HTML-STEP
           MOVE        CN-VERIFY-TO TO HTML-REVIEW
           MOVE        CN-MONEY-TO TO HTML-TRPTH
           MOVE        CN-SUB-ACCOUNT TO HTML-TRPTH1
           MOVE        CN-AMOUNT-APPLIED TO HTML-TRPTH2
           MOVE        CN-DOLLAR-AMOUNT TO HTML-TRPTH3.
       F45BT-FN. EXIT.
      *N45BX.    NOTE *BUILD TO ACCOUNT TITLE             *.
       F45BX.    IF    K970-CACCT = 'T'                                 lv15
                 AND   V249-CACTS = 'S'
                 NEXT SENTENCE ELSE GO TO     F45BX-FN.
      *FOR CONFIRMATION PAGE
           MOVE        CN-MONEY-TO TO HTML-TRPTH
           MOVE        CN-SUB-ACCOUNT TO HTML-TRPTH1
           MOVE        CN-AMOUNT-APPLIED TO HTML-TRPTH2
           MOVE        CN-DOLLAR-AMOUNT TO HTML-TRPTH3.
       F45BX-FN. EXIT.
       F45BC-FN. EXIT.
      *N45CC.    NOTE *ASSET ALLOCATION HEADER            *.
       F45CC.    IF    V249-CTTYPG = 'AALL'                             lv10
                 AND   WS-FIRST-TIME
                 NEXT SENTENCE ELSE GO TO     F45CC-FN.
           MOVE        CN-STEP-2 TO HTML-STEP
           MOVE        CN-REVIEW TO HTML-REVIEW
           MOVE        CN-DETAIL TO HTML-TRPTH
           MOVE        CN-ALLOCATION TO HTML-TRPTH1
           MOVE        CN-ADJUSTED-VALUE TO HTML-TRPTH2
           MOVE        CN-PERCENTAGE TO HTML-TRPTH3
           MOVE        CN-VALUATION-DATE TO HTML-VALUATION.
       F45CC-FN. EXIT.
      *N45EC.    NOTE *BUILD SUB-ACCOUNT LINE             *.
       F45EC.                                                           lv10
           MOVE        K970-MFDNMS TO HTML-MFDNMS.
       F45EC-FN. EXIT.
      *N45GC.    NOTE *SUB ACCOUNT TRANSFER AMOUNTS       *.
       F45GC.    IF    V249-CTTYPG = 'INVC'                             lv10
                 NEXT SENTENCE ELSE GO TO     F45GC-FN.
      *N45GE.    NOTE *BUILD FROM AMOUNTS                 *.
       F45GE.    IF    K970-CACCT = 'F'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45GE-FN.
      *N45GG.    NOTE *BUILD MOVED AMOUNT                 *.
       F45GG.    IF    K970-CAMOT4 = '1'                                lv20
                 NEXT SENTENCE ELSE GO TO     F45GG-FN.
           MOVE        K970-ADBRQF TO WS-ADBRQ
           MOVE        WS-AMOUNT TO HTML-AMOUNT1.
       F45GG-FN. EXIT.
      *N45GI.    NOTE *BUILD MOVED PERCENTAGE             *.
       F45GI.    IF    K970-CAMOT4 = '2'                                lv20
                 NEXT SENTENCE ELSE GO TO     F45GI-FN.
           MOVE        K970-PACT1 TO WS-PSUBA
           MOVE        WS-PERCENT TO HTML-AMOUNT1.
       F45GI-FN. EXIT.
      *N45GL.    NOTE *BUILD MOVE ALL                     *.
       F45GL.    IF    K970-IALLV = 'Y'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45GL-FN.
           MOVE        CN-MOVE-ALL TO HTML-AMOUNT1.
       F45GL-FN. EXIT.
      *N45GN.    NOTE *BUILD ESTIMATED AMOUNT FROM        *.
       F45GN.                                                           lv20
           MOVE        K970-AEDRQ1 TO WS-ADBRQ
           MOVE        WS-AMOUNT TO HTML-AMOUNT2.
       F45GN-FN. EXIT.
       F45GE-FN. EXIT.
      *N45IE.    NOTE *BUILD TO AMOUNTS                   *.
       F45IE.    IF    K970-CACCT = 'T'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F45IE-FN.
      *N45IG.    NOTE *BUILD APPLIED AMOUNT               *.
       F45IG.    IF    K970-CAMOT5 = '1'                                lv20
                 NEXT SENTENCE ELSE GO TO     F45IG-FN.
           MOVE        K970-AEDRQ2 TO WS-ADBRQ
           MOVE        WS-AMOUNT TO HTML-AMOUNT1.
       F45IG-FN. EXIT.
      *N45II.    NOTE *BUILD APPLIED PERCENTAGE           *.
       F45II.    IF    K970-CAMOT5 = '2'                                lv20
                 NEXT SENTENCE ELSE GO TO     F45II-FN.
           MOVE        K970-PSUBA1 TO WS-PSUBA
           MOVE        WS-PERCENT TO HTML-AMOUNT1.
       F45II-FN. EXIT.
      *N45IL.    NOTE *BUILD APPLY BALANCE                *.
       F45IL.    IF    K970-ITRNB = 'Y'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F45IL-FN.
           MOVE        CN-APPLY-BALANCE TO HTML-AMOUNT1.
       F45IL-FN. EXIT.
      *N45IN.    NOTE *BUILD ESTIMATED AMOUNT TO          *.
       F45IN.                                                           lv20
           MOVE        K970-AEDRQ2 TO WS-ADBRQ
           MOVE        WS-AMOUNT TO HTML-AMOUNT2.
       F45IN-FN. EXIT.
       F45IE-FN. EXIT.
       F45GC-FN. EXIT.
      *N45MC.    NOTE *ASSET ALLOCATION AMOUNTS           *.
       F45MC.    IF    V249-CTTYPG = 'AALL'                             lv10
                 NEXT SENTENCE ELSE GO TO     F45MC-FN.
      *N45ME.    NOTE *BUILD ALLOCATION AMOUNT            *.
       F45ME.                                                           lv15
           MOVE        K970-AEDRQ2 TO WS-ADBRQ
           MOVE        WS-AMOUNT TO HTML-AMOUNT1.
       F45ME-FN. EXIT.
      *N45MG.    NOTE *BUILD ALLOCATION PERCENTAGE        *.
       F45MG.                                                           lv15
           MOVE        K970-PSUBA1 TO WS-PSUBA
           MOVE        WS-PERCENT TO HTML-AMOUNT2.
       F45MG-FN. EXIT.
       F45MC-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *BUILD MESSAGE INFO                 *
      *               *                                   *
      *               *************************************.
       F50.      IF    V249-CAACT = 'M'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BC.    NOTE *MOVE MESSAGE TEXT                  *.
       F50BC.    IF    IHTMLL < IHTMLM                                  lv10
                 NEXT SENTENCE ELSE GO TO     F50BC-FN.
           ADD         +1 TO IHTMLL.
      *N50EC.    NOTE *MOVE MESSAGE                       *.
       F50EC.                                                           lv15
           MOVE        V249-TDTXT1 TO HTML-TDTXT1.
       F50EC-FN. EXIT.
      *N50MC.    NOTE *MOVE MESSAGE BLOCK                 *.
       F50MC.                                                           lv15
           MOVE        HTML-MESSAGE-LI TO
           HTML-MESSAGE-TBLCK (IHTMLL).
       F50MC-FN. EXIT.
       F50BC-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *MOVE HTML TAGS INTO HM93-OUTPUT    *
      *               *                                   *
      *               *************************************.
       F60.      IF    WS-FIRST-TIME                                    lv05
                 NEXT SENTENCE ELSE GO TO     F60-FN.
      *N60BA.    NOTE *INITIALIZE LINE COUNTER            *.
       F60BA.                                                           lv10
           MOVE        HM93-QBLCK TO HTML-PT.
       F60BA-FN. EXIT.
      *N60CE.    NOTE *VERIFY TAGS                        *.
       F60CE.    IF    V249-CACTS = 'E'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F60CE-FN.
      *N60CF.    NOTE *STRING VERIFY INTO HTML-OUTPUT     *.
       F60CF.                                                           lv15
           MOVE        1                        TO J60CFR
                                    GO TO     F60CF-B.
       F60CF-A.
           ADD         1                        TO J60CFR.
       F60CF-B.
           IF          J60CFR                   >  HTML-VERIFY-CTR
                                    GO TO     F60CF-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-VERIFY-LINE (J60CFR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60CF-900. GO TO F60CF-A.
       F60CF-FN. EXIT.
       F60CE-FN. EXIT.
      *N60DC.    NOTE *MOVE PRODUCT LINES                 *.
       F60DC.                                                           lv10
           MOVE        1                        TO J60DCR
                                    GO TO     F60DC-B.
       F60DC-A.
           ADD         1                        TO J60DCR.
       F60DC-B.
           IF          J60DCR                   >  HTML-PRODUCT-CTR
                                    GO TO     F60DC-FN.
      *N60DE.    NOTE *STRING PRODUCT INTO HTML-OUTPUT    *.
       F60DE.    IF    HTML-PRODUCT-LINE (J60DCR)                       lv15
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F60DE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-PRODUCT-LINE (J60DCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60DE-FN. EXIT.
       F60DC-900. GO TO F60DC-A.
       F60DC-FN. EXIT.
      *N60EC.    NOTE *MOVE ACCOUNT LINES                 *.
       F60EC.                                                           lv10
           MOVE        1                        TO J60ECR
                                    GO TO     F60EC-B.
       F60EC-A.
           ADD         1                        TO J60ECR.
       F60EC-B.
           IF          J60ECR                   >  HTML-ACCOUNT-CTR
                                    GO TO     F60EC-FN.
      *N60EE.    NOTE *SET ACCOUNT LINES                  *.
       F60EE.    IF    HTML-ACCOUNT-LINE (J60ECR)                       lv15
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F60EE-FN.
           ADD         1 TO HTML-PT
           INITIALIZE  HM93-THTML (HTML-PT)
           STRING      HTML-ACCOUNT-LINE (J60ECR)
           CN-ABREAK DELIMITED BY SIZE
           INTO HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60EE-FN. EXIT.
       F60EC-900. GO TO F60EC-A.
       F60EC-FN. EXIT.
      *N60EN.    NOTE *END OF ACCOUNT LINE                *.
       F60EN.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-ACCOUNT TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60EN-FN. EXIT.
      *N60EP.    NOTE *STRING REQUEST INTO HTML-OUTPUT    *.
       F60EP.                                                           lv10
           MOVE        1                        TO J60EPR
                                    GO TO     F60EP-B.
       F60EP-A.
           ADD         1                        TO J60EPR.
       F60EP-B.
           IF          J60EPR                   >  HTML-REQUEST-CTR
                                    GO TO     F60EP-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REQUEST-LINE (J60EPR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60EP-900. GO TO F60EP-A.
       F60EP-FN. EXIT.
      *N60PC.    NOTE *ASSET ALLOCATION REVIEW LINE       *.
       F60PC.    IF    V249-CTTYPG = 'AALL'                             lv10
                 NEXT SENTENCE ELSE GO TO     F60PC-FN.
      *N60PD.    NOTE *VERIFY TAGS                        *.
       F60PD.    IF    V249-CACTS = 'E'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60PD-FN.
      *N60PE.    NOTE *STRING TITLES INTO HTML-OUTPUT     *.
       F60PE.                                                           lv20
           MOVE        1                        TO J60PER
                                    GO TO     F60PE-B.
       F60PE-A.
           ADD         1                        TO J60PER.
       F60PE-B.
           IF          J60PER                   >  HTML-REVIEW-CTR
                                    GO TO     F60PE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REVIEW-LINE (J60PER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60PE-900. GO TO F60PE-A.
       F60PE-FN. EXIT.
       F60PD-FN. EXIT.
      *N60PG.    NOTE *STRING DETAIL INTO HTML-OUTPUT     *.
       F60PG.                                                           lv15
           MOVE        1                        TO J60PGR
                                    GO TO     F60PG-B.
       F60PG-A.
           ADD         1                        TO J60PGR.
       F60PG-B.
           IF          J60PGR                   >  HTML-DETAIL-CTR
                                    GO TO     F60PG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-DETAIL-LINE (J60PGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60PG-900. GO TO F60PG-A.
       F60PG-FN. EXIT.
       F60PC-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *MOVE TAGS INTO HM93-OUTPUT         *
      *               *                                   *
      *               *************************************.
       F65.      IF    V249-CAACT = 'S'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *N65CA.    NOTE *SUB ACCOUNT TRANSFER REVIEW        *.
       F65CA.    IF    V249-CTTYPG = 'INVC'                             lv10
                 NEXT SENTENCE ELSE GO TO     F65CA-FN.
      *N65CC.    NOTE *FIRST FROM ACCOUNT                 *.
       F65CC.    IF    K970-CACCT = 'F'                                 lv15
                 AND   WS-FIRST-FROM
                 NEXT SENTENCE ELSE GO TO     F65CC-FN.
           SET WS-NOT-FIRST-FROM TO TRUE.
      *N65CE.    NOTE *STRING TITLES INTO HTML-OUTPUT     *.
       F65CE.                                                           lv20
           MOVE        1                        TO J65CER
                                    GO TO     F65CE-B.
       F65CE-A.
           ADD         1                        TO J65CER.
       F65CE-B.
           IF          J65CER                   >  HTML-REVIEW-CTR
                                    GO TO     F65CE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REVIEW-LINE (J65CER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65CE-900. GO TO F65CE-A.
       F65CE-FN. EXIT.
      *N65CG.    NOTE *STRING DETAIL INTO HTML-OUTPUT     *.
       F65CG.                                                           lv20
           MOVE        1                        TO J65CGR
                                    GO TO     F65CG-B.
       F65CG-A.
           ADD         1                        TO J65CGR.
       F65CG-B.
           IF          J65CGR                   >  HTML-DETAIL-CTR
                                    GO TO     F65CG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-DETAIL-LINE (J65CGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65CG-900. GO TO F65CG-A.
       F65CG-FN. EXIT.
       F65CC-FN. EXIT.
      *N65EC.    NOTE *FIRST TO ACCOUNT                   *.
       F65EC.    IF    K970-CACCT = 'T'                                 lv15
                 AND   WS-FIRST-TO
                 NEXT SENTENCE ELSE GO TO     F65EC-FN.
           SET WS-NOT-FIRST-TO TO TRUE.
      *N65EE.    NOTE *STRING TITLES INTO HTML-OUTPUT     *.
       F65EE.                                                           lv20
           MOVE        1                        TO J65EER
                                    GO TO     F65EE-B.
       F65EE-A.
           ADD         1                        TO J65EER.
       F65EE-B.
           IF          J65EER                   >  HTML-REVIEW-CTR
                                    GO TO     F65EE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REVIEW-LINE (J65EER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65EE-900. GO TO F65EE-A.
       F65EE-FN. EXIT.
      *N65EG.    NOTE *STRING DETAIL INTO HTML-OUTPUT     *.
       F65EG.                                                           lv20
           MOVE        1                        TO J65EGR
                                    GO TO     F65EG-B.
       F65EG-A.
           ADD         1                        TO J65EGR.
       F65EG-B.
           IF          J65EGR                   >  HTML-DETAIL-CTR
                                    GO TO     F65EG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-DETAIL-LINE (J65EGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65EG-900. GO TO F65EG-A.
       F65EG-FN. EXIT.
       F65EC-FN. EXIT.
       F65CA-FN. EXIT.
      *N65GE.    NOTE *SUB ACCOUNT LINES                  *.
       F65GE.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        HTML-SUBACCT-TAGS TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65GE-FN. EXIT.
       F65-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *MOVE END TAGS INTO HM93-OUTPUT     *
      *               *                                   *
      *               *************************************.
       F70.      IF    V249-CALSTC = 'Y'                                lv05
                 NEXT SENTENCE ELSE GO TO     F70-FN.
      *N70CC.    NOTE *MOVE END OF DETAIL TAGS            *.
       F70CC.                                                           lv10
           MOVE        1                        TO J70CCR
                                    GO TO     F70CC-B.
       F70CC-A.
           ADD         1                        TO J70CCR.
       F70CC-B.
           IF          J70CCR                   >  HTML-EO-DTL-CTR
                                    GO TO     F70CC-FN.
      *
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-DTL-LINE (J70CCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F70CC-900. GO TO F70CC-A.
       F70CC-FN. EXIT.
      *N70EC.    NOTE *FOR SUBMIT                         *.
       F70EC.    IF    V249-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F70EC-FN.
           MOVE        CN-MESSAGE1-TEXT TO HTML-MESSAGE1
           MOVE        CN-MESSAGE2-TEXT TO HTML-MESSAGE2
           MOVE        CN-MESSAGE3-TEXT TO HTML-MESSAGE3
           MOVE        CN-MESSAGE4-TEXT TO HTML-MESSAGE4.
      *N70ED.    NOTE *MESSAGE TEXT INTO HTML-OUTPUT      *.
       F70ED.                                                           lv15
           MOVE        1                        TO J70EDR
                                    GO TO     F70ED-B.
       F70ED-A.
           ADD         1                        TO J70EDR.
       F70ED-B.
           IF          J70EDR                   >  HTML-MESSAGE1-CTR
                                    GO TO     F70ED-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-MESSAGE1-LINE (J70EDR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F70ED-900. GO TO F70ED-A.
       F70ED-FN. EXIT.
      *N70EE.    NOTE *STRING NOTES INTO HTML-OUTPUT      *.
       F70EE.                                                           lv15
           MOVE        1                        TO J70EER
                                    GO TO     F70EE-B.
       F70EE-A.
           ADD         1                        TO J70EER.
       F70EE-B.
           IF          J70EER                   >  HTML-NOTES-CTR
                                    GO TO     F70EE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-NOTES-LINE (J70EER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F70EE-900. GO TO F70EE-A.
       F70EE-FN. EXIT.
       F70EC-900. GO TO F70EG-FN.
       F70EC-FN. EXIT.
      *N70EG.    NOTE *FOR VERIFY                         *.
       F70EG.         EXIT.                                             lv10
      *N70EI.    NOTE *STRING IMPLY INTO HTML-OUTPUT      *.
       F70EI.                                                           lv15
           MOVE        1                        TO J70EIR
                                    GO TO     F70EI-B.
       F70EI-A.
           ADD         1                        TO J70EIR.
       F70EI-B.
           IF          J70EIR                   >  HTML-IMPLY-CTR
                                    GO TO     F70EI-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-IMPLY-LINE (J70EIR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F70EI-900. GO TO F70EI-A.
       F70EI-FN. EXIT.
       F70EG-FN. EXIT.
       F70-FN.   EXIT.
      *N75.      NOTE *************************************.
      *               *                                   *
      *               *MOVE END TAGS INTO HM93-OUTPUT     *
      *               *                                   *
      *               *************************************.
       F75.      IF    V249-CALSTC = 'Y'                                lv05
                 NEXT SENTENCE ELSE GO TO     F75-FN.
      *N75CE.    NOTE *STRING MESSAGES INTO HTML-OUTPUT   *.
       F75CE.                                                           lv10
           MOVE        1                        TO J75CER
                                    GO TO     F75CE-B.
       F75CE-A.
           ADD         1                        TO J75CER.
       F75CE-B.
           IF          J75CER                   >  HTML-MESSAGE-CTR
                                    GO TO     F75CE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-MESSAGE-LINE (J75CER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F75CE-900. GO TO F75CE-A.
       F75CE-FN. EXIT.
      *N75MC.    NOTE *STRING MESSAGES INTO HTML-OUTPUT   *.
       F75MC.                                                           lv10
           MOVE        1                        TO J75MCR
                                    GO TO     F75MC-B.
       F75MC-A.
           ADD         1                        TO J75MCR.
       F75MC-B.
           IF          J75MCR                   >  IHTMLM
                                    GO TO     F75MC-FN.
      *N75ME.    NOTE *ONLY MOVE GOOD MESSAGES            *.
       F75ME.    IF    HTML-TMESSC (J75MCR)                             lv15
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F75ME-FN.
           ADD         1 TO HTML-PT                                     DOT
           MOVE        HTML-TMESSN1 (J75MCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
      *                                                                 DOT
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS1 (J75MCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
                 IF    HTML-TMESS2 (J75MCR)                             DOT
                       NOT = SPACES
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS2 (J75MCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           END-IF.
                 IF    HTML-TMESS3 (J75MCR)                             DOT
                       NOT = SPACES
           ADD         1 TO HTML-PT
           MOVE        HTML-TMESS3 (J75MCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT)
           END-IF.
           ADD         1 TO HTML-PT                                     DOT
           MOVE        HTML-TMESSN2 (J75MCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F75ME-FN. EXIT.
       F75MC-900. GO TO F75MC-A.
       F75MC-FN. EXIT.
      *N75TE.    NOTE *STRING EOM INTO HTML-OUTPUT        *.
       F75TE.                                                           lv10
           MOVE        1                        TO J75TER
                                    GO TO     F75TE-B.
       F75TE-A.
           ADD         1                        TO J75TER.
       F75TE-B.
           IF          J75TER                   >  HTML-EOM-CTR
                                    GO TO     F75TE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-EOM-LINE (J75TER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F75TE-900. GO TO F75TE-A.
       F75TE-FN. EXIT.
       F75-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU102
      *               *                                   *             ADU102
      *               *---> Normal Termination            *             ADU102
      *               *                                   *             ADU102
      *               *************************************.            ADU102
       F79.                                                             lv05
      *     Return to Calling Module                                    ADU102
      *MOVE LENGTH OF HTML BLOB
           COMPUTE     HM93-QBLCK = HTML-PT
           MOVE                     ALL '1' TO FT GO TO F20.            ADU102
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
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
