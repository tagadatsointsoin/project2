       IDENTIFICATION DIVISION.                                         CI0286
       PROGRAM-ID.  CI0286P.                                            CI0286
      *AUTHOR.         OST CRNA/PIAP HTML BUILD.                        CI0286
      *DATE-COMPILED.   09/08/14.                                       CI0286
       ENVIRONMENT DIVISION.                                            CI0286
       CONFIGURATION SECTION.                                           CI0286
       SOURCE-COMPUTER. IBM-370.                                        CI0286
       OBJECT-COMPUTER. IBM-370.                                        CI0286
       DATA DIVISION.                                                   CI0286
       WORKING-STORAGE SECTION.                                         CI0286
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
                        PICTURE X(999).                                 CI0286
           88 WS-FIRST-TIME           VALUE 'Y'.
           88 WS-NOT-FIRST-TIME       VALUE 'N'.
      *
       01  WS00-CTID.
           05 FILLER      PIC X(5) VALUE '0000 '.
      *!WS
           05 WS00-NCTIDN
                        PICTURE 9999B9999B9999.                         CI0286
           05 FILLER      PIC X    VALUE SPACE.
      *!WI
           05 WS00-GECKD
                        PICTURE 9.                                      CI0286
           05 FILLER      PIC X    VALUE SPACE.
      *!WI
           05 WS00-CTIDA
                        PICTURE 9(3).                                   CI0286
      *
       01  WS00-AINVT     PIC ZZ,ZZZ,ZZZ,ZZ9.
       01   DEBUT-WSS.                                                  CI0286
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0286
            05   IK     PICTURE X.                                      CI0286
       01  CONSTANTES-PAC.                                              CI0286
           05  FILLER  PICTURE X(87)   VALUE                            CI0286
                     '6015 CAT09/08/14CI0286ADMIN   14:35:16CI0286P AMERCI0286
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0286
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0286
           05  NUGNA   PICTURE X(5).                                    CI0286
           05  APPLI   PICTURE X(3).                                    CI0286
           05  DATGN   PICTURE X(8).                                    CI0286
           05  PROGR   PICTURE X(6).                                    CI0286
           05  CODUTI  PICTURE X(8).                                    CI0286
           05  TIMGN   PICTURE X(8).                                    CI0286
           05  PROGE   PICTURE X(8).                                    CI0286
           05  COBASE  PICTURE X(4).                                    CI0286
           05  DATGNC  PICTURE X(10).                                   CI0286
           05  RELEAS  PICTURE X(7).                                    CI0286
           05  DATGE   PICTURE X(10).                                   CI0286
           05  DATSQ   PICTURE X(10).                                   CI0286
       01  DATCE.                                                       CI0286
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0286
         05  DATOR.                                                     CI0286
           10  DATOA  PICTURE XX.                                       CI0286
           10  DATOM  PICTURE XX.                                       CI0286
           10  DATOJ  PICTURE XX.                                       CI0286
       01   VARIABLES-CONDITIONNELLES.                                  CI0286
            05                  FT      PICTURE X VALUE '0'.            CI0286
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0286
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0286
            05           IHTMLL PICTURE S9(4) VALUE  ZERO.
            05           IHTMLR PICTURE S9(4) VALUE  ZERO.
            05           IHTMLM PICTURE S9(4) VALUE +0025.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J60CFR PICTURE S9(4) VALUE  ZERO.
            05           J60DCR PICTURE S9(4) VALUE  ZERO.
            05           J60EFR PICTURE S9(4) VALUE  ZERO.
            05           J65CCR PICTURE S9(4) VALUE  ZERO.
            05           J65ECR PICTURE S9(4) VALUE  ZERO.
            05           J65TCR PICTURE S9(4) VALUE  ZERO.
            05           J65WBR PICTURE S9(4) VALUE  ZERO.
            05           J70EER PICTURE S9(4) VALUE  ZERO.
            05           J70EIR PICTURE S9(4) VALUE  ZERO.
            05           J75CER PICTURE S9(4) VALUE  ZERO.
            05           J75MCR PICTURE S9(4) VALUE  ZERO.
            05           J75TER PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0286
      *COPYBOOK WITH HTML TEXT

       COPY CI0286C1.

      *COPYBOOK WITH MESSAGE TEXT

       COPY CI0289MM.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **      THESE SEGMENTS ARE THE INPUT LINKAGE FOR CI0286          *
      ******************************************************************
      *
      *!WF DSP=V2 DSL=V2 SEL=49 FOR=I LEV=1 PLT=75
       01                 V200.                                         CI0286
          05              V200-SUITE.                                   CI0286
            15       FILLER         PICTURE  X(00881).                  CI0286
       01                 V249  REDEFINES      V200.                    CI0286
            10            V249-GCOMN.                                   CI0286
            11            V249-MAPPN  PICTURE  X(10).                   CI0286
            11            V249-NSSSI  PICTURE  X(24).                   CI0286
            11            V249-CTTYPG PICTURE  X(04).                   CI0286
            11            V249-DCACG  PICTURE  9(8).                    CI0286
            11            V249-CTSET  PICTURE  9(6).                    CI0286
            11            V249-QNACT  PICTURE  9(3).                    CI0286
            11            V249-CTID   PICTURE  X(27).                   CI0286
            11            V249-GETIM  PICTURE  S9(7)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-CAACT  PICTURE  X(1).                    CI0286
            11            V249-CALSTC PICTURE  X.                       CI0286
            11            V249-CACTS  PICTURE  X.                       CI0286
            11            V249-ADBRQ  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-PACT1  PICTURE  S999V999                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-QSHOWQ PICTURE  S9(9)V999                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-ADBRQ1 PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-PACT1A PICTURE  S999V999                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-QSHO5A PICTURE  S9(9)V999                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-GXREF.                                   CI0286
            12            V249-GACMS.                                   CI0286
            13            V249-XZ30   PICTURE  X(30).                   CI0286
            13            V249-FILLER                                   CI0286
                          REDEFINES            V249-XZ30.               CI0286
            14            V249-IAIND  PICTURE  X                        CI0286
                          OCCURS       030     TIMES.                   CI0286
            12            V249-GMSAC.                                   CI0286
            13            V249-XZ10   PICTURE  X(10).                   CI0286
            13            V249-FILLER                                   CI0286
                          REDEFINES            V249-XZ10.               CI0286
            14            V249-IACCT  PICTURE  X(1)                     CI0286
                          OCCURS       010     TIMES.                   CI0286
            11            V249-NGEOPA PICTURE  X(08).                   CI0286
            11            V249-CTRHO  PICTURE  9(8).                    CI0286
            11            V249-GETOD  PICTURE  9(6).                    CI0286
            11            V249-CSLCT  PICTURE  X.                       CI0286
            11            V249-CCLCH  PICTURE  X.                       CI0286
            11            V249-CCLPR  PICTURE  X.                       CI0286
            11            V249-CCLSU  PICTURE  X.                       CI0286
            11            V249-DXTMS2 PICTURE  X(26).                   CI0286
            11            V249-FILLER PICTURE  X(007).                  CI0286
            11            V249-GOTPT.                                   CI0286
            12            V249-CCONF  PICTURE  X(25).                   CI0286
            12            V249-XDCNN  PICTURE  X(17).                   CI0286
            12            V249-FILLER PICTURE  X(08).                   CI0286
            10            V249-GMSGS.                                   CI0286
            11            V249-NMESS2 PICTURE  S9(6)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-TDTXT1 PICTURE  X(512).                  CI0286
            11            V249-FILLER PICTURE  X(37).                   CI0286
            10            V249-GME87                                    CI0286
                          REDEFINES            V249-GMSGS.              CI0286
            11            V249-CEXTP  PICTURE  X.                       CI0286
            11            V249-AEDRQ  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-PWHLD  PICTURE  S999V9(5)                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-CPORTA PICTURE  X.                       CI0286
            11            V249-TIPUP  PICTURE  X(8).                    CI0286
            11            V249-DEFFT  PICTURE  9(8).                    CI0286
            11            V249-GTOAD.                                   CI0286
            12            V249-CDELIX PICTURE  X(3).                    CI0286
            11            V249-GTOAC.                                   CI0286
            12            V249-CTID01 PICTURE  X(27).                   CI0286
            11            V249-ITRNB  PICTURE  X.                       CI0286
            11            V249-CIRAP  PICTURE  XX.                      CI0286
            11            V249-CEXTP1 PICTURE  X.                       CI0286
            11            V249-ADBRQ2 PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-ATWHDD PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-ATWHDE PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-CTWHPB PICTURE  9(3)V999.                CI0286
            11            V249-ACOTD  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-TDTXTA PICTURE  X(80).                   CI0286
            11            V249-CPAYC  PICTURE  X(2).                    CI0286
            11            V249-CLID   PICTURE  X(23).                   CI0286
            11            V249-GECSQ  PICTURE  S9(3)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-NTR    PICTURE  9(8).                    CI0286
            11            V249-GECKD  PICTURE  9.                       CI0286
            11            V249-NPBN   PICTURE  X(20).                   CI0286
            11            V249-CCBAT  PICTURE  99.                      CI0286
            11            V249-GENAL1 PICTURE  X(30).                   CI0286
            11            V249-GENAL2 PICTURE  X(30).                   CI0286
            11            V249-GESAD1 PICTURE  X(30).                   CI0286
            11            V249-GESAD2 PICTURE  X(30).                   CI0286
            11            V249-GESAD3 PICTURE  X(30).                   CI0286
            10            V249-GME97                                    CI0286
                          REDEFINES            V249-GMSGS.              CI0286
            11            V249-GRID   PICTURE  X(13).                   CI0286
            11            V249-PRCOD  PICTURE  9(5).                    CI0286
            11            V249-CTIDA  PICTURE  9(3).                    CI0286
            11            V249-PRSCD  PICTURE  X(9).                    CI0286
            11            V249-PRCPRE PICTURE  X(4).                    CI0286
            11            V249-CEIT   PICTURE  9(3).                    CI0286
            11            V249-CMPFC  PICTURE  9(3).                    CI0286
            10            V249-GME11                                    CI0286
                          REDEFINES            V249-GMSGS.              CI0286
            11            V249-FILLER PICTURE  X.                       CI0286
            10            V249-GMD49                                    CI0286
                          REDEFINES            V249-GMSGS.              CI0286
            11            V249-CCEIT  PICTURE  X.                       CI0286
            11            V249-CEITX  PICTURE  9(3).                    CI0286
            10            V249-GME13                                    CI0286
                          REDEFINES            V249-GMSGS.              CI0286
            11            V249-CTTYP  PICTURE  X(04).                   CI0286
            10            V249-GMD43                                    CI0286
                          REDEFINES            V249-GMSGS.              CI0286
            11            V249-PERFE  PICTURE  S9(3)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            V249-PNRFE  PICTURE  S9(3)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            10            V249-GME20                                    CI0286
                          REDEFINES            V249-GMSGS.              CI0286
            11            V249-CPNOP  PICTURE  X(2).                    CI0286
            11            V249-PNPCT  PICTURE  999.                     CI0286
            11            V249-PNRFE1 PICTURE  S9(3)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            10            V249-MPRN4  PICTURE  X(35).                   CI0286
            10            V249-CTNOB1 PICTURE  X.                       CI0286
            10            V249-CTNOB2 PICTURE  X.                       CI0286
            10            V249-CTNOB3 PICTURE  X.                       CI0286
            10            V249-CRITO  PICTURE  X.                       CI0286
            10            V249-CTIFR  PICTURE  X.                       CI0286
            10            V249-AINVT  PICTURE  S9(11)                   CI0286
                          COMPUTATIONAL-3.                              CI0286
            10            V249-NGEOR  PICTURE  9(08).                   CI0286
      *!WF DSP=TO DSL=QT SEL=58 FOR=I LEV=1 PLT=75
       01                 TO00.                                         CI0286
          05              TO00-SUITE.                                   CI0286
            15       FILLER         PICTURE  X(02300).                  CI0286
       01                 TO58  REDEFINES      TO00.                    CI0286
            10            TO58-QT5K.                                    CI0286
            11            TO58-C299.                                    CI0286
            12            TO58-CTID.                                    CI0286
            13            TO58-CTIDA  PICTURE  9(3).                    CI0286
            13            TO58-CTIDN.                                   CI0286
            14            TO58-CTIDNP PICTURE  X(13).                   CI0286
            14            TO58-CTIDND PICTURE  9(11).                   CI0286
            11            TO58-GECKD2 PICTURE  9.                       CI0286
            11            TO58-NSEQ5  PICTURE  9(5).                    CI0286
            11            TO58-CTSTA  PICTURE  99.                      CI0286
            11            TO58-CTSTAL PICTURE  X(10).                   CI0286
            11            TO58-CTOWN  PICTURE  9(3).                    CI0286
            11            TO58-CTTLN1 PICTURE  X(30).                   CI0286
            11            TO58-CTTLN2 PICTURE  X(30).                   CI0286
            11            TO58-CTTLN3 PICTURE  X(30).                   CI0286
            11            TO58-CTTBO1 PICTURE  X(45).                   CI0286
            11            TO58-CTTBO2 PICTURE  X(45).                   CI0286
            11            TO58-CTEFD  PICTURE  9(8).                    CI0286
            11            TO58-CTIAD  PICTURE  9(8).                    CI0286
            11            TO58-CTCUS  PICTURE  999.                     CI0286
            11            TO58-GR98.                                    CI0286
            12            TO58-GRID.                                    CI0286
            13            TO58-GRIDC  PICTURE  9(3).                    CI0286
            13            TO58-GRIDN.                                   CI0286
            14            TO58-GRIDNP PICTURE  99.                      CI0286
            14            TO58-GRIDND PICTURE  9(8).                    CI0286
            11            TO58-CQACT  PICTURE  999.                     CI0286
            11            TO58-CTCCI  PICTURE  X.                       CI0286
            11            TO58-CIRAS  PICTURE  999.                     CI0286
            11            TO58-CIRAT  PICTURE  999.                     CI0286
            11            TO58-IACVD  PICTURE  X.                       CI0286
            11            TO58-FILLER PICTURE  X(4).                    CI0286
            11            TO58-PRCODA PICTURE  X(5).                    CI0286
            11            TO58-PRCMN  PICTURE  X(20).                   CI0286
            11            TO58-MRPLN  PICTURE  X(30).                   CI0286
            11            TO58-CPRDG  PICTURE  9(2).                    CI0286
            11            TO58-CPRDA1 PICTURE  9(3).                    CI0286
            11            TO58-PRSCD  PICTURE  X(9).                    CI0286
            11            TO58-MSP03  PICTURE  X(3).                    CI0286
            11            TO58-CGRLI  PICTURE  X.                       CI0286
            11            TO58-ITERM  PICTURE  X(1).                    CI0286
            11            TO58-IVARP  PICTURE  X.                       CI0286
            11            TO58-DVALU  PICTURE  9(8).                    CI0286
            11            TO58-AACTV  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ACCTVC PICTURE  X(20).                   CI0286
            11            TO58-ITXTI  PICTURE  X.                       CI0286
            11            TO58-ASANP  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ACINV  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CELBL  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-NMESS2 PICTURE  S9(6)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-FILLER PICTURE  X(1).                    CI0286
            11            TO58-PRCLN  PICTURE  X(60).                   CI0286
            11            TO58-GECKD  PICTURE  9.                       CI0286
            11            TO58-MPLNA  PICTURE  X(19).                   CI0286
            11            TO58-CQACTL PICTURE  X(45).                   CI0286
            11            TO58-CRQPA  PICTURE  9(3).                    CI0286
            11            TO58-IVANT  PICTURE  X(1).                    CI0286
            11            TO58-IDBRP  PICTURE  X(1).                    CI0286
            11            TO58-IANPY  PICTURE  X.                       CI0286
            11            TO58-IVARP1 PICTURE  X.                       CI0286
            11            TO58-FILLER PICTURE  X(27).                   CI0286
            11            TO58-NSEQ2A PICTURE  S9(3)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-NSEQ2P PICTURE  S9(3)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-MRPSN  PICTURE  X(12).                   CI0286
            11            TO58-GEHCD  PICTURE  9(3)                     CI0286
                          OCCURS       002     TIMES.                   CI0286
            11            TO58-GEHCSU PICTURE  9(5)                     CI0286
                          OCCURS       002     TIMES.                   CI0286
            11            TO58-PRCSN  PICTURE  X(9).                    CI0286
            11            TO58-CGRMF  PICTURE  X.                       CI0286
            11            TO58-IGFEX  PICTURE  X.                       CI0286
            11            TO58-CLIDP  PICTURE  X(23).                   CI0286
            11            TO58-CLCTRC PICTURE  9(3).                    CI0286
            11            TO58-ADINP  PICTURE  X(20).                   CI0286
            11            TO58-CLCTRA PICTURE  9(3).                    CI0286
            11            TO58-GRPLC  PICTURE  99.                      CI0286
            11            TO58-CIDRP  PICTURE  99.                      CI0286
            11            TO58-FILLER PICTURE  X(01).                   CI0286
            11            TO58-AVMTOT PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AVCSH  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AMARC  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AVLMX  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AVLMN  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-INDRS  PICTURE  X.                       CI0286
            11            TO58-MPRN4  PICTURE  X(35).                   CI0286
            11            TO58-FILLER PICTURE  X(1).                    CI0286
            11            TO58-ACVALM PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-INDRSA PICTURE  X(2).                    CI0286
            11            TO58-DXTMSA PICTURE  X(26).                   CI0286
            11            TO58-NMESS6 PICTURE  S9(6)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-NMESS7 PICTURE  S9(6)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-IBIDSA PICTURE  X.                       CI0286
            11            TO58-IBIDSB PICTURE  X.                       CI0286
            11            TO58-INSPOS PICTURE  X.                       CI0286
            11            TO58-INSPOD PICTURE  X.                       CI0286
            11            TO58-ACBALX PICTURE  X(20).                   CI0286
            11            TO58-AINVMX PICTURE  X(20).                   CI0286
            11            TO58-AMARCX PICTURE  X(20).                   CI0286
            11            TO58-AVMTOX PICTURE  X(20).                   CI0286
            11            TO58-IMNPR  PICTURE  X.                       CI0286
            11            TO58-ISSPL  PICTURE  X.                       CI0286
            11            TO58-AVMTOI PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AVCSHI PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-APOSC  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AVLMXI PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AVLMN1 PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AVLMN2 PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-FILLER PICTURE  X(05).                   CI0286
            10            TO58-QT5A.                                    CI0286
            11            TO58-CLID   PICTURE  X(23).                   CI0286
            11            TO58-GECKD1 PICTURE  9.                       CI0286
            11            TO58-MCLNM  PICTURE  X(40).                   CI0286
            11            TO58-MCLNM2 PICTURE  X(40).                   CI0286
            11            TO58-CLTYP  PICTURE  X.                       CI0286
            11            TO58-CLDOB  PICTURE  9(8).                    CI0286
            11            TO58-CLDTH  PICTURE  X.                       CI0286
            11            TO58-CLTIN  PICTURE  9(12).                   CI0286
            11            TO58-CLTINC PICTURE  9.                       CI0286
            11            TO58-GESAD1 PICTURE  X(30).                   CI0286
            11            TO58-GESAD2 PICTURE  X(30).                   CI0286
            11            TO58-GESAD3 PICTURE  X(30).                   CI0286
            11            TO58-GECIT  PICTURE  X(25).                   CI0286
            11            TO58-GECTRY PICTURE  X(20).                   CI0286
            11            TO58-GEPCD  PICTURE  X(12).                   CI0286
            11            TO58-GEST   PICTURE  X(8).                    CI0286
            11            TO58-GEADS  PICTURE  9.                       CI0286
            11            TO58-GECSD  PICTURE  9(8).                    CI0286
            11            TO58-QCLAGE PICTURE  9(3)V9                   CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-FILLER PICTURE  X(06).                   CI0286
            10            TO58-QT5T.                                    CI0286
            11            TO58-ATFRA  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AGOFD  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-APRMX  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-APRMN  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-IOWNC  PICTURE  X.                       CI0286
            11            TO58-COWNF  PICTURE  X(30).                   CI0286
            11            TO58-CTYPE  PICTURE  X.                       CI0286
            11            TO58-CIRAC  PICTURE  X(5).                    CI0286
            11            TO58-CTXMT  PICTURE  9(2).                    CI0286
            11            TO58-AMIND  PICTURE  S9(7)V99.                CI0286
            11            TO58-AMAXAR PICTURE  S9(7)V99.                CI0286
            11            TO58-QSHOWQ PICTURE  S9(9)V999                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-QSHOW0 PICTURE  S9(10)V999               CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-PPOT1  PICTURE  S9(3)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-PACT1  PICTURE  S999V999                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-IPRTA  PICTURE  X.                       CI0286
            11            TO58-FILLER PICTURE  X.                       CI0286
            11            TO58-CLCUS  PICTURE  99.                      CI0286
            11            TO58-CCDSCW PICTURE  9(2).                    CI0286
            11            TO58-CCACT  PICTURE  99.                      CI0286
            11            TO58-CIRAG.                                   CI0286
            12            TO58-CIRAP  PICTURE  XX                       CI0286
                          OCCURS       010     TIMES.                   CI0286
            11            TO58-ITERF  PICTURE  X.                       CI0286
            11            TO58-IACFPD PICTURE  X(1).                    CI0286
            11            TO58-AFEET  PICTURE  S9(5)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ATERF  PICTURE  S9(5)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CLIDNB PICTURE  9(8).                    CI0286
            11            TO58-ALOAD  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASURR  PICTURE  S9(07)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASHIS  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AMNBL  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-APNAC  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ANGOF  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CPLTYP PICTURE  X(14).                   CI0286
            10            TO58-QT5N.                                    CI0286
            11            TO58-IARRAN PICTURE  X.                       CI0286
            11            TO58-GESTD1 PICTURE  9(8).                    CI0286
            11            TO58-GEEND1 PICTURE  S9(8)                    CI0286
                          BINARY.                                       CI0286
            11            TO58-GESTD  PICTURE  9(8).                    CI0286
            11            TO58-GEEND  PICTURE  9(8).                    CI0286
            11            TO58-NSQ4B2 PICTURE  9(8)                     CI0286
                          BINARY.                                       CI0286
            11            TO58-CDEST  PICTURE  99.                      CI0286
            11            TO58-DEFFT  PICTURE  9(8).                    CI0286
            11            TO58-CPMTF  PICTURE  99.                      CI0286
            11            TO58-CPMTG  PICTURE  99.                      CI0286
            11            TO58-MPMTFL PICTURE  X(24).                   CI0286
            11            TO58-MPMTFE PICTURE  X(24).                   CI0286
            11            TO58-DLAUP  PICTURE  9(8).                    CI0286
            11            TO58-NSEQ4B PICTURE  9(8)                     CI0286
                          BINARY.                                       CI0286
            11            TO58-QSACTF PICTURE  9(3).                    CI0286
            11            TO58-QSACTT PICTURE  9(3).                    CI0286
            11            TO58-CCONF  PICTURE  X(25).                   CI0286
            11            TO58-DCONF  PICTURE  9(8).                    CI0286
            11            TO58-DTIMT  PICTURE  X(8).                    CI0286
            11            TO58-CACTS  PICTURE  X.                       CI0286
            11            TO58-ADBRQ  PICTURE  S9(11)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-DNPMT  PICTURE  9(8).                    CI0286
            11            TO58-NAPDS  PICTURE  S9(3)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CDEST1 PICTURE  99.                      CI0286
            11            TO58-CLANR1 PICTURE  X(23).                   CI0286
            11            TO58-FILLER PICTURE  X(01).                   CI0286
            10            TO58-FILLER PICTURE  X(600).                  CI0286
            10            TO58-QT5C                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-CESLD  PICTURE  9(8).                    CI0286
            11            TO58-PCIRB5 PICTURE  S9(3)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-PANYDD PICTURE  S9(3)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CEIT   PICTURE  9(3).                    CI0286
            11            TO58-PPART  PICTURE  9(3)V99                  CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-DTRME  PICTURE  9(8).                    CI0286
            11            TO58-CEIRND PICTURE  9(8).                    CI0286
            11            TO58-DANNIA PICTURE  9(8).                    CI0286
            11            TO58-AAPAA  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CELBDT PICTURE  9(8).                    CI0286
            11            TO58-CEIIS  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-DTRME1 PICTURE  9(8).                    CI0286
            11            TO58-GMKTS.                                   CI0286
            12            TO58-DTRME2 PICTURE  9(8)                     CI0286
                          OCCURS       005     TIMES.                   CI0286
            12            TO58-DTRME3 PICTURE  9(8)                     CI0286
                          OCCURS       005     TIMES.                   CI0286
            11            TO58-ALINT  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CEHCD  PICTURE  9(3)                     CI0286
                          OCCURS       006     TIMES.                   CI0286
            11            TO58-CEFOTR PICTURE  S9(3)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-DGPED  PICTURE  9(8).                    CI0286
            11            TO58-DIPED  PICTURE  9(8).                    CI0286
            11            TO58-FILLER PICTURE  X(409).                  CI0286
            10            TO58-QT5F                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-DLAUP2 PICTURE  9(8).                    CI0286
            11            TO58-QSHOW  PICTURE  S9(10)V999               CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AFAVP  PICTURE  S9(4)V9(3)               CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-QSHIS  PICTURE  S9(10)V999               CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-QSHNM  PICTURE  S9(10)V999.              CI0286
            11            TO58-QSHOM  PICTURE  S9(10)V999               CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ADDAC  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-QSHES  PICTURE  S9(10)V999               CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-NDCUS  PICTURE  X(9).                    CI0286
            11            TO58-CSTKR5 PICTURE  X(5).                    CI0286
            11            TO58-NACID  PICTURE  S9(11)                   CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AGOFD2 PICTURE  S9(9)V99.                CI0286
            11            TO58-TCBAT  PICTURE  X(21).                   CI0286
            11            TO58-FILLER PICTURE  X(490).                  CI0286
            10            TO58-QT5L                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-ALDBEN PICTURE  S9(09)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-APREL  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ALMODE PICTURE  99.                      CI0286
            11            TO58-ITMEC  PICTURE  X(1).                    CI0286
            11            TO58-ITAMR  PICTURE  X(1).                    CI0286
            11            TO58-MPMTF  PICTURE  X(14).                   CI0286
            11            TO58-TPLNL  PICTURE  X(30).                   CI0286
            11            TO58-ASBENA PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASBENB PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASBENC PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASBENE PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASBENF PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-GESTNS PICTURE  X(2).                    CI0286
            11            TO58-CTWHPB PICTURE  9(3)V999.                CI0286
            11            TO58-CTWHCB PICTURE  X.                       CI0286
            11            TO58-AMVA1  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASPAM  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ACTCH  PICTURE  S9(07)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AMXLN  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ALFGH  PICTURE  999.                     CI0286
            11            TO58-ALPLNI PICTURE  9.                       CI0286
            11            TO58-ATSA8  PICTURE  S9(07)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CVALB  PICTURE  X(3).                    CI0286
            11            TO58-ASURRN PICTURE  S9(07)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ASURRW PICTURE  S9(07)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ATLTB  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AEARN0 PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ATFPI  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-AEARN1 PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ISELO  PICTURE  X.                       CI0286
            11            TO58-CCLAC  PICTURE  X.                       CI0286
            11            TO58-ALINNO PICTURE  99.                      CI0286
            11            TO58-ALPLNJ PICTURE  9.                       CI0286
            11            TO58-COLPL  PICTURE  9(05).                   CI0286
            11            TO58-ALPLDT PICTURE  9(8).                    CI0286
            11            TO58-ANFMC  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CPNOP  PICTURE  X(2).                    CI0286
            11            TO58-CVSTC  PICTURE  X(4).                    CI0286
            11            TO58-CGMBR  PICTURE  X.                       CI0286
            11            TO58-DWSDT  PICTURE  9(8).                    CI0286
            11            TO58-IRDPH  PICTURE  X.                       CI0286
            11            TO58-DWAIT  PICTURE  9(8).                    CI0286
            11            TO58-IAPGP  PICTURE  X.                       CI0286
            11            TO58-CASTA  PICTURE  X.                       CI0286
            11            TO58-CSSUP2 PICTURE  X.                       CI0286
            11            TO58-CVOMC1 PICTURE  X(1).                    CI0286
            11            TO58-APGBP  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ALDDUE PICTURE  9(08).                   CI0286
            11            TO58-APYMT  PICTURE  S9(9)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ALSURR PICTURE  S9(09)V99                CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CESTP  PICTURE  X(03).                   CI0286
            11            TO58-FILLER PICTURE  X(356).                  CI0286
            10            TO58-QT5O                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-NBACT  PICTURE  S9(11)                   CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CTIAC  PICTURE  S9(3)                    CI0286
                          BINARY.                                       CI0286
            11            TO58-CASTT  PICTURE  S99                      CI0286
                          BINARY.                                       CI0286
            11            TO58-CATMI  PICTURE  S9                       CI0286
                          BINARY.                                       CI0286
            11            TO58-IATMR  PICTURE  X(3).                    CI0286
            11            TO58-IBIPI  PICTURE  X.                       CI0286
            11            TO58-CBPST  PICTURE  S99                      CI0286
                          BINARY.                                       CI0286
            11            TO58-TBPST  PICTURE  X(16).                   CI0286
            11            TO58-CODPI  PICTURE  X.                       CI0286
            11            TO58-TODPS  PICTURE  X(9).                    CI0286
            11            TO58-FILLER PICTURE  X(448).                  CI0286
            11            TO58-IBPSD  PICTURE  X.                       CI0286
            11            TO58-FILLER PICTURE  X(107).                  CI0286
            11            TO58-QT5E                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            12            TO58-MPRN4X PICTURE  X(100).                  CI0286
            12            TO58-CCMSH  PICTURE  X(2).                    CI0286
            12            TO58-CPRCS  PICTURE  X(04).                   CI0286
            12            TO58-CURST  PICTURE  X.                       CI0286
            10            TO58-QT5M                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-NAPCN1 PICTURE  X(24).                   CI0286
            11            TO58-FILLER PICTURE  X(576).                  CI0286
            10            TO58-QT5B                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-NAPCN2 PICTURE  X(24).                   CI0286
            11            TO58-CTIDAL PICTURE  X(40).                   CI0286
            11            TO58-NPHNS  PICTURE  X(14).                   CI0286
            11            TO58-FILLER PICTURE  X(522).                  CI0286
            10            TO58-QT5P                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-CFPPT  PICTURE  9(3).                    CI0286
            11            TO58-TTYPP  PICTURE  X(40).                   CI0286
            11            TO58-CPPST  PICTURE  9(3).                    CI0286
            11            TO58-TPPST  PICTURE  X(15).                   CI0286
            11            TO58-APFEEQ PICTURE  S9(7)V99.                CI0286
            11            TO58-APFEEC PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-APFEEP PICTURE  S9(7)V99.                CI0286
            11            TO58-ISVCA  PICTURE  X.                       CI0286
            11            TO58-NSBVS  PICTURE  X(5).                    CI0286
            11            TO58-ICKRV  PICTURE  X.                       CI0286
            11            TO58-PDAMT  PICTURE  S9(03).                  CI0286
            11            TO58-PSTAX  PICTURE  S9(03)V999.              CI0286
            11            TO58-DPCAL  PICTURE  9(8).                    CI0286
            11            TO58-NADVF  PICTURE  X(08).                   CI0286
            11            TO58-DAGUP  PICTURE  9(8).                    CI0286
            11            TO58-AANFEA PICTURE  9(5)V99                  CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-CLIDN7 PICTURE  9(8)                     CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-ARANV  PICTURE  S9(7)V99                 CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            TO58-DRANV  PICTURE  9(8).                    CI0286
            11            TO58-FILLER PICTURE  X(454).                  CI0286
            10            TO58-QT50                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-NANCA  PICTURE  X(30).                   CI0286
            11            TO58-MANCN  PICTURE  X(100).                  CI0286
            11            TO58-AINPTX PICTURE  X(20).                   CI0286
            11            TO58-CTID01 PICTURE  X(27).                   CI0286
            11            TO58-NANCA1 PICTURE  X(04).                   CI0286
            11            TO58-IIVAR  PICTURE  X(1).                    CI0286
            11            TO58-FILLER PICTURE  X(418).                  CI0286
            10            TO58-QT5R                                     CI0286
                          REDEFINES            TO58-FILLER.             CI0286
            11            TO58-NACTJ  PICTURE  X(04).                   CI0286
            11            TO58-NACNO6 PICTURE  X(11).                   CI0286
            11            TO58-FILLER PICTURE  X(585).                  CI0286
            10            TO58-AMAXA  PICTURE  S9(7)V99.                CI0286
            10            TO58-ISAOR  PICTURE  X.                       CI0286
            10            TO58-ISACH  PICTURE  X.                       CI0286
            10            TO58-CERRBA PICTURE  X(02).                   CI0286
            10            TO58-CERRBH PICTURE  X(02).                   CI0286
            10            TO58-IWITHH PICTURE  X.                       CI0286
            10            TO58-CTID20 PICTURE  X(27).                   CI0286
            10            TO58-GECKD3 PICTURE  9.                       CI0286
            10            TO58-DANFC  PICTURE  X(10).                   CI0286
            10            TO58-DAFCN  PICTURE  X(10).                   CI0286
            10            TO58-ISMTA  PICTURE  X.                       CI0286
            10            TO58-CERRBT PICTURE  X(02).                   CI0286
            10            TO58-NPLNI  PICTURE  X(10).                   CI0286
            10            TO58-FILLER PICTURE  X(023).                  CI0286
      *!WF DSP=DU DSL=DU SEL=70 FOR=I LEV=1 PLT=75
       01                 DU00.                                         CI0286
          05              DU00-SUITE.                                   CI0286
            15       FILLER         PICTURE  X(02229).                  CI0286
       01                 DU70  REDEFINES      DU00.                    CI0286
            10            DU70-CTID1.                                   CI0286
            11            DU70-CTIDA3 PICTURE  S9(3)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            DU70-CTIDN  PICTURE  X(24).                   CI0286
            10            DU70-CLID   PICTURE  X(23).                   CI0286
            10            DU70-CLIDO8 PICTURE  S9(3)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            10            DU70-CLIDN  PICTURE  X(20).                   CI0286
            10            DU70-NRSEQ  PICTURE  9(6).                    CI0286
            10            DU70-NTEXT  PICTURE  9(7).                    CI0286
            10            DU70-TBENE  PICTURE  X(77)                    CI0286
                          OCCURS       025     TIMES.                   CI0286
            10            DU70-NSHID  PICTURE  X(20).                   CI0286
            10            DU70-FILLER PICTURE  X(200).                  CI0286
      *
      ******************************************************************
      **         THIS SEGMENT IS THE OUTPUT LINKAGE FOR CI0286         *
      ******************************************************************
      *
      *!WF DSP=HM DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HM00.                                         CI0286
          05              HM00-SUITE.                                   CI0286
            15       FILLER         PICTURE  X(90906).                  CI0286
       01                 HM93  REDEFINES      HM00.                    CI0286
            10            HM93-QBLCK  PICTURE  9(6).                    CI0286
            10            HM93-QT9O.                                    CI0286
            11            HM93-QT9B                                     CI0286
                          OCCURS       450     TIMES.                   CI0286
            12            HM93-CHTML  PICTURE  99.                      CI0286
            12            HM93-THTML  PICTURE  X(200).                  CI0286
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0286
          05              MS00-SUITE.                                   CI0286
            15       FILLER         PICTURE  X(00542).                  CI0286
       01                 MS03  REDEFINES      MS00.                    CI0286
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            10            MS03-CMSSF  PICTURE  XX.                      CI0286
            10            MS03-DU09.                                    CI0286
            11            MS03-CMESA  PICTURE  S9(9)                    CI0286
                          BINARY.                                       CI0286
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0286
                          BINARY.                                       CI0286
            11            MS03-CMESB  PICTURE  S9(9)                    CI0286
                          BINARY.                                       CI0286
            11            MS03-CMSST  PICTURE  S9(9)                    CI0286
                          BINARY.                                       CI0286
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0286
                          BINARY.                                       CI0286
            11            MS03-QELLAA PICTURE  S9(9)                    CI0286
                          BINARY.                                       CI0286
            11            MS03-TMESS4 PICTURE  X(512).                  CI0286
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0286
            10            MX11-QMSGS  PICTURE  9(03).                   CI0286
            10            MX11-PJ09                                     CI0286
                          OCCURS       025     TIMES.                   CI0286
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0286
                          COMPUTATIONAL-3.                              CI0286
            11            MX11-CMESB  PICTURE  S9(9)                    CI0286
                          BINARY.                                       CI0286
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V249
                                TO58
                                DU70
                                HM93
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0286
      *               *                                   *             CI0286
      *               *INITIALISATIONS                    *             CI0286
      *               *                                   *             CI0286
      *               *************************************.            CI0286
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
           HTML-NEW-ACCT-TAGS
           HTML-ACCOUNT-TAGS
           HTML-EO-ACCOUNT
           HTML-EO-BENEFCRY
           HTML-EO-PRODUCTS
           HTML-IMPLY-TAGS
           HTML-NOTES-TAGS
           HTML-MESSAGE-TAGS
           HTML-MESSAGE-ARRAY
           HTML-MESSAGE-LI
           HTML-EOM-TAGS
           HTML-SUITBITY-TAGS
           HTML-EO-SUITBITY.
       F02CA-FN. EXIT.
      *N02CB.    NOTE *---> EVERY TIME                    *.
       F02CB.                                                           lv10
           INITIALIZE  HTML-NEW-ACCT-TAGS
           HTML-ACCOUNT-TAGS
           HTML-SUITBITY-TAGS.
       F02CB-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0286
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0286
      *               *                                   *             CI0286
      *               *FIN DE TRAITEMENT                  *             CI0286
      *               *                                   *             CI0286
      *               *************************************.            CI0286
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0286
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
      *N35PA.    NOTE *MOVE NEW ACCOUNT TITLE             *.
       F35PA.         EXIT.                                             lv10
      *N35PC.    NOTE *MOVE CERTS TITLE                   *.
       F35PC.    IF    V249-CTIDA = '001'                               lv15
                 NEXT SENTENCE ELSE GO TO     F35PC-FN.
      *
           MOVE        CN-CERT1 TO HTML-PRODUCT1
           MOVE        CN-CERT2 TO HTML-PRODUCT2.
       F35PC-FN. EXIT.
      *N35PF.    NOTE *MOVE FUNDS TITLE                   *.
       F35PF.    IF    V249-CTIDA = '002'                               lv15
                 NEXT SENTENCE ELSE GO TO     F35PF-FN.
      *
           MOVE        CN-FUND1 TO HTML-PRODUCT1
           MOVE        CN-FUND2 TO HTML-PRODUCT2.
       F35PF-FN. EXIT.
       F35PA-FN. EXIT.
       F35-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *BUILD ACCOUNT TAGS                 *
      *               *                                   *
      *               *************************************.
       F45.      IF    V249-CAACT = 'S'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F45-FN.
      *N45BC.    NOTE *STRING ACCOUNT NAME                *.
       F45BC.                                                           lv10
           MOVE        TO58-PRCMN TO HTML-PRCMN.
       F45BC-FN. EXIT.
      *N45BG.    NOTE *BUILD OWNERSHIP LINE(S)            *.
       F45BG.                                                           lv10
           MOVE        TO58-CTTLN1 TO HTML-CTTLN1.
                 IF    TO58-CTTLN2 > SPACES                             DOT
           MOVE        TO58-CTTLN2 TO HTML-CTTLN2.
                 IF    TO58-CTTLN3 > SPACES                             DOT
           MOVE        TO58-CTTLN3 TO HTML-CTTLN3.
                 IF    TO58-CTTBO1 > SPACES                             DOT
           MOVE        TO58-CTTBO1 TO HTML-CTTBO1.
                 IF    TO58-CTTBO2 > SPACES                             DOT
           MOVE        TO58-CTTBO2 TO HTML-CTTBO2.
       F45BG-FN. EXIT.
      *N45BJ.    NOTE *BUILD SUITABILITY INFO - 1         *.
       F45BJ.                                                           lv10
      *
                 IF    V249-AINVT > ZEROES                              DOT
      *MOVE INVESTMENT AMOUNT
           MOVE        V249-AINVT TO WS00-AINVT
           MOVE        WS00-AINVT TO HTML-AINVT.
                 IF    V249-CTIFR = 'A'                                 DOT
      *MOVE TIME FRAME
           MOVE        'Less than 1 year' TO HTML-CTIFR.
                 IF    V249-CTIFR = 'B'                                 DOT
           MOVE        '1 - 3 years' TO HTML-CTIFR.
                 IF    V249-CTIFR = 'C'                                 DOT
           MOVE        '4 - 7 years' TO HTML-CTIFR.
                 IF    V249-CTIFR = 'D'                                 DOT
           MOVE        '8 - 10 years' TO HTML-CTIFR.
                 IF    V249-CTIFR = 'E'                                 DOT
           MOVE        '11+  years' TO
           HTML-CTIFR.
                 IF    V249-CTIFR = ' '                                 DOT
           MOVE        'Unknown' TO HTML-CTIFR.
                 IF    V249-CRITO = 'A'                                 DOT
      *MOVE RISK TOLERANCE
           MOVE        'Conservative' TO HTML-CRITO.
                 IF    V249-CRITO = 'B'                                 DOT
           MOVE        'Conservative/Moderate' TO
           HTML-CRITO.
                 IF    V249-CRITO = 'C'                                 DOT
           MOVE        'Moderate' TO HTML-CRITO.
                 IF    V249-CRITO = 'D'                                 DOT
           MOVE        'Moderate/Aggressive' TO
           HTML-CRITO.
                 IF    V249-CRITO = 'E'                                 DOT
           MOVE        'Aggressive' TO HTML-CRITO.
                 IF    V249-CRITO = ' '                                 DOT
           MOVE        'Unknown' TO HTML-CRITO.
      *N45BM.    NOTE *BUILD SUITABILITY INFO - 2         *.
       F45BM.                                                           lv15
      *
      *MOVE INVESTMENT OBJECTIVE
                 IF    V249-CTNOB1 > SPACES                             DOT
           MOVE        '1.' TO HTML-CTNOB1 (1:2).
                 IF    V249-CTNOB1 = 'A'                                DOT
           MOVE        'Capital Preservation' TO
           HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'B'                                DOT
           MOVE        'Capital Appreciation' TO
           HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'C'                                DOT
           MOVE        'Income' TO HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'D'                                DOT
           MOVE        'Tax Considerations' TO
           HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'E'                                DOT
           MOVE        'Protection' TO HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'F'                                DOT
           MOVE        'Education' TO HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'G'                                DOT
           MOVE        'Estate Planning' TO
           HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'H'                                DOT
           MOVE        'Speculation' TO HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'I'                                DOT
           MOVE        'Growth' TO HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB1 = 'J'                                DOT
           MOVE        'Growth with income' TO
           HTML-CTNOB1 (3:33).
                 IF    V249-CTNOB2 > SPACES                             DOT
           MOVE        '2.' TO HTML-CTNOB2 (1:2).
                 IF    V249-CTNOB2 = 'A'                                DOT
           MOVE        'Capital Preservation' TO
           HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'B'                                DOT
           MOVE        'Capital Appreciation' TO
           HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'C'                                DOT
           MOVE        'Income' TO HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'D'                                DOT
           MOVE        'Tax Considerations' TO
           HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'E'                                DOT
           MOVE        'Protection' TO HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'F'                                DOT
           MOVE        'Education' TO HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'G'                                DOT
           MOVE        'Estate Planning' TO
           HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'H'                                DOT
           MOVE        'Speculation' TO HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'I'                                DOT
           MOVE        'Growth' TO HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB2 = 'J'                                DOT
           MOVE        'Growth with income' TO
           HTML-CTNOB2 (3:33).
                 IF    V249-CTNOB3 > SPACES                             DOT
           MOVE        '3.' TO HTML-CTNOB3 (1:2).
                 IF    V249-CTNOB3 = 'A'                                DOT
           MOVE        'Capital Preservation' TO
           HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'B'                                DOT
           MOVE        'Capital Appreciation' TO
           HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'C'                                DOT
           MOVE        'Income' TO HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'D'                                DOT
           MOVE        'Tax Considerations' TO
           HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'E'                                DOT
           MOVE        'Protection' TO HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'F'                                DOT
           MOVE        'Education' TO HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'G'                                DOT
           MOVE        'Estate Planning' TO
           HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'H'                                DOT
           MOVE        'Speculation' TO HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'I'                                DOT
           MOVE        'Growth' TO HTML-CTNOB3 (3:33).
                 IF    V249-CTNOB3 = 'J'                                DOT
           MOVE        'Growth with income' TO
           HTML-CTNOB3 (3:33).
       F45BM-FN. EXIT.
       F45BJ-FN. EXIT.
      *N45CB.    NOTE *FORMAT CONTRACT ID                 *.
       F45CB.    IF    V249-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45CB-FN.
      *ON SUBMIT ONLY
           MOVE        TO58-CTIDND TO WS00-NCTIDN
           MOVE        TO58-GECKD2 TO WS00-GECKD
           MOVE        TO58-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-CTID.
       F45CB-FN. EXIT.
      *N45CE.    NOTE *FORMAT CERTIFICATE TERM/PART       *.
       F45CE.    IF    V249-CTIDA = '001'                               lv10
                 NEXT SENTENCE ELSE GO TO     F45CE-FN.
      *N45CG.    NOTE *TERM                               *.
       F45CG.    IF    V249-CEIT NOT = 0                                lv15
                 NEXT SENTENCE ELSE GO TO     F45CG-FN.
           MOVE        CN-TERM TO HTML-TERM
           MOVE        V249-CEIT TO HTML-CEIT
           MOVE        CN-MONTHS TO HTML-MONTHS.
       F45CG-FN. EXIT.
      *N45CI.    NOTE *PARTICIPATION                      *.
       F45CI.    IF    V249-CMPFC NOT = 0                               lv15
                 NEXT SENTENCE ELSE GO TO     F45CI-FN.
           MOVE        CN-PARTICIPATE TO HTML-PARTICIPATE.
                 IF    V249-CMPFC = 1                                   DOT
           MOVE        CN-FULL TO HTML-PPART.
                 IF    V249-CMPFC = 2                                   DOT
           MOVE        CN-PARTIAL TO HTML-PPART.
       F45CI-FN. EXIT.
       F45CE-FN. EXIT.
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
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *MOVE NEW ACCT INTO HM93-OUTPUT     *
      *               *                                   *
      *               *************************************.
       F65.      IF    V249-CAACT = 'S'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *N65CC.    NOTE *NEW ACCOUNT LINES                  *.
       F65CC.                                                           lv10
           MOVE        1                        TO J65CCR
                                    GO TO     F65CC-B.
       F65CC-A.
           ADD         1                        TO J65CCR.
       F65CC-B.
           IF          J65CCR                   >  HTML-NEW-ACCT-CTR
                                    GO TO     F65CC-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-NEW-ACCT-LINE (J65CCR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65CC-900. GO TO F65CC-A.
       F65CC-FN. EXIT.
      *N65EC.    NOTE *ACCOUNT DESCRIPTON LINES           *.
       F65EC.                                                           lv10
           MOVE        1                        TO J65ECR
                                    GO TO     F65EC-B.
       F65EC-A.
           ADD         1                        TO J65ECR.
       F65EC-B.
           IF          J65ECR                   >  HTML-ACCOUNT-CTR
                                    GO TO     F65EC-FN.
      *N65EE.    NOTE *ACCOUNT LINES                      *.
       F65EE.    IF    HTML-ACCOUNT-LINE (J65ECR)                       lv15
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F65EE-FN.
           ADD         1 TO HTML-PT
           INITIALIZE  HM93-THTML (HTML-PT)
           STRING      HTML-ACCOUNT-LINE (J65ECR)
           CN-ABREAK DELIMITED BY SIZE
           INTO HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65EE-FN. EXIT.
       F65EC-900. GO TO F65EC-A.
       F65EC-FN. EXIT.
      *N65KN.    NOTE *END OF ACCOUNT LINES               *.
       F65KN.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-ACCOUNT TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65KN-FN. EXIT.
      *N65TC.    NOTE *BENEFICIARY LINES                  *.
       F65TC.                                                           lv10
           MOVE        1                        TO J65TCR
                                    GO TO     F65TC-B.
       F65TC-A.
           ADD         1                        TO J65TCR.
       F65TC-B.
           IF          J65TCR                   >  25
                                    GO TO     F65TC-FN.
      *N65TE.    NOTE *NON BLANK LINES ONLY               *.
       F65TE.    IF    DU70-TBENE (J65TCR)                              lv15
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F65TE-FN.
           ADD         1 TO HTML-PT
           INITIALIZE  HM93-THTML (HTML-PT)
           STRING      DU70-TBENE (J65TCR)
           CN-ABREAK DELIMITED BY SIZE
           INTO HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65TE-FN. EXIT.
       F65TC-900. GO TO F65TC-A.
       F65TC-FN. EXIT.
      *N65TN.    NOTE *END OF BENEFICIARY LINES           *.
       F65TN.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-BENEFCRY TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65TN-FN. EXIT.
      *N65WB.    NOTE *ACCOUNT SUITABILITY                *.
       F65WB.                                                           lv10
           MOVE        1                        TO J65WBR
                                    GO TO     F65WB-B.
       F65WB-A.
           ADD         1                        TO J65WBR.
       F65WB-B.
           IF          J65WBR                   >  HTML-SUITBITY-CTR
                                    GO TO     F65WB-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-SUITBITY-LINE (J65WBR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65WB-900. GO TO F65WB-A.
       F65WB-FN. EXIT.
      *N65WE.    NOTE *END OF ACCT SUITABILITY LINES      *.
       F65WE.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-SUITBITY TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65WE-FN. EXIT.
       F65-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *MOVE REVIEW TAG INTO HM93-OUTPUT   *
      *               *                                   *
      *               *************************************.
       F70.      IF    V249-CALSTC = 'Y'                                lv05
                 NEXT SENTENCE ELSE GO TO     F70-FN.
      *N70CE.    NOTE *END OF NEW ACCOUNTS TABLE          *.
       F70CE.                                                           lv10
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-PRODUCTS TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F70CE-FN. EXIT.
      *N70EC.    NOTE *FOR SUBMIT                         *.
       F70EC.    IF    V249-CACTS = 'S'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F70EC-FN.
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
