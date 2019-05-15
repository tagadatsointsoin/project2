       IDENTIFICATION DIVISION.                                         CI0284
       PROGRAM-ID.  CI0284P.                                            CI0284
      *AUTHOR.         OST M/M HTML BUILD.                              CI0284
      *DATE-COMPILED.   09/08/14.                                       CI0284
       ENVIRONMENT DIVISION.                                            CI0284
       CONFIGURATION SECTION.                                           CI0284
       SOURCE-COMPUTER. IBM-370.                                        CI0284
       OBJECT-COMPUTER. IBM-370.                                        CI0284
       DATA DIVISION.                                                   CI0284
       WORKING-STORAGE SECTION.                                         CI0284
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
                        PICTURE X(999).                                 CI0284
           88 WS-FIRST-TIME           VALUE 'Y'.
           88 WS-NOT-FIRST-TIME       VALUE 'N'.
      *
      *!WI
       01  WS-NSEQA
                        PICTURE S9(3)                                   CI0284
                          COMPUTATIONAL-3.                              CI0284
      *!WI
       01  WS-NSEQB
                        PICTURE 9(3).                                   CI0284
       01  WS00-CTID.
           05 FILLER      PIC X(5) VALUE '0000 '.
      *!WS
           05 WS00-NCTIDN
                        PICTURE 9999B9999B9999.                         CI0284
           05 FILLER      PIC X    VALUE SPACE.
      *!WI
           05 WS00-GECKD
                        PICTURE 9.                                      CI0284
           05 FILLER      PIC X    VALUE SPACE.
      *!WI
           05 WS00-CTIDA
                        PICTURE 9(3).                                   CI0284

       01  WS00-BANK.
           05  WS-ROUTING  PIC X(16).
           05  WS-NTR      PIC 9(8).
           05  WS-DASH     PIC X.
           05  WS-GECKD    PIC 9.
       01  WS-AMOUNT.
           05 WS-ADBRQ2      PIC $$,$$$,$$$.99.
       01  WS-SHARES.
           05 WS-QSHOWQ      PIC ZZZ,ZZ9.999.
           05 FILLER         PIC X(07) VALUE ' Shares'.
       01  WS-PERCENT.
           05 WS-PACT1       PIC ZZZ,ZZ9.

      *!WI
       01  WS-AWITH
                        PICTURE S9(11)V99                               CI0284
                          COMPUTATIONAL-3.                              CI0284

       01  WS-TO-SHARES.
           05 FILLER         PIC X(01) VALUE '('.
           05 WS-TO-QSHOWQ   PIC ZZZ,ZZ9.999.
           05 FILLER         PIC X(08) VALUE ' SHARES)'.

       01  WS-MESSAGE-NBR    PIC XX.
       01  FILLER REDEFINES WS-MESSAGE-NBR.
           05  WS-M-9        PIC 9.
       01  FILLER REDEFINES WS-MESSAGE-NBR.
           05  WS-M-99       PIC 99.
       01  WS-XZ30.
      *!WI
           05 WS-XZ1         OCCURS 30
                        PICTURE X.                                      CI0284
       01  WS-A-HREF-CONSTANTS.
           05 WS-AONE        PIC X     VALUE '1'.
           05 WS-ACOMMA      PIC X     VALUE ','.
           05 WS-A-HREF      PIC X(9)  VALUE '<A HREF=#'.
           05 WS-A-9         PIC 9.
           05 WS-A-99        PIC 99.
           05 WS-ACARAT      PIC X     VALUE '>'.
           05 WS-EO-HREF     PIC X(4)  VALUE '</A>'.
           05 WS-HREF-PTR    PIC 9(4).

       01  WS-ACTCH.
           05 WS-ACTCH-AMT   PIC $$9.99.
           05 FILLER         PIC X    VALUE ')'.
       01  WS-CHARGE.
           05 FILLER           PIC X(31) VALUE
             '<FONT SIZE=-1 COLOR=#FF0000><B>'.
           05 WS-CONTRACT      PIC X(30).
           05 WS-ACTCH2        PIC X(07).
           05 FILLER           PIC X(12) VALUE
             '</B></FONT>'.
       01  WS00-SIGNEE.
           05 WS-SIGNEE      PIC X(11).
           05 WS-GENAL2      PIC X(69).
       01  WS00-BANK-ACCT.
           05 WS-BANK-ACCT   PIC X(21).
           05 WS-NPBN        PIC X(59).
       01  WS00-PWHLDF       PIC S9(3)V999 COMP-3.
       01   DEBUT-WSS.                                                  CI0284
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0284
            05   IK     PICTURE X.                                      CI0284
       01  CONSTANTES-PAC.                                              CI0284
           05  FILLER  PICTURE X(87)   VALUE                            CI0284
                     '6015 CAT09/08/14CI0284ADMIN   14:35:15CI0284P AMERCI0284
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0284
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0284
           05  NUGNA   PICTURE X(5).                                    CI0284
           05  APPLI   PICTURE X(3).                                    CI0284
           05  DATGN   PICTURE X(8).                                    CI0284
           05  PROGR   PICTURE X(6).                                    CI0284
           05  CODUTI  PICTURE X(8).                                    CI0284
           05  TIMGN   PICTURE X(8).                                    CI0284
           05  PROGE   PICTURE X(8).                                    CI0284
           05  COBASE  PICTURE X(4).                                    CI0284
           05  DATGNC  PICTURE X(10).                                   CI0284
           05  RELEAS  PICTURE X(7).                                    CI0284
           05  DATGE   PICTURE X(10).                                   CI0284
           05  DATSQ   PICTURE X(10).                                   CI0284
       01  DATCE.                                                       CI0284
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0284
         05  DATOR.                                                     CI0284
           10  DATOA  PICTURE XX.                                       CI0284
           10  DATOM  PICTURE XX.                                       CI0284
           10  DATOJ  PICTURE XX.                                       CI0284
       01   VARIABLES-CONDITIONNELLES.                                  CI0284
            05                  FT      PICTURE X VALUE '0'.            CI0284
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0284
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0284
            05           IHTMLL PICTURE S9(4) VALUE  ZERO.
            05           IHTMLR PICTURE S9(4) VALUE  ZERO.
            05           IHTMLM PICTURE S9(4) VALUE +0025.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J50KER PICTURE S9(4) VALUE  ZERO.
            05           J60CFR PICTURE S9(4) VALUE  ZERO.
            05           J60CKR PICTURE S9(4) VALUE  ZERO.
            05           J60DFR PICTURE S9(4) VALUE  ZERO.
            05           J60DNR PICTURE S9(4) VALUE  ZERO.
            05           J60DTR PICTURE S9(4) VALUE  ZERO.
            05           J60DVR PICTURE S9(4) VALUE  ZERO.
            05           J60ECR PICTURE S9(4) VALUE  ZERO.
            05           J60ENR PICTURE S9(4) VALUE  ZERO.
            05           J60IAR PICTURE S9(4) VALUE  ZERO.
            05           J60IER PICTURE S9(4) VALUE  ZERO.
            05           J60IKR PICTURE S9(4) VALUE  ZERO.
            05           J60KGR PICTURE S9(4) VALUE  ZERO.
            05           J60KIR PICTURE S9(4) VALUE  ZERO.
            05           J60SIR PICTURE S9(4) VALUE  ZERO.
            05           J65CER PICTURE S9(4) VALUE  ZERO.
            05           J65CIR PICTURE S9(4) VALUE  ZERO.
            05           J65CNR PICTURE S9(4) VALUE  ZERO.
            05           J65EGR PICTURE S9(4) VALUE  ZERO.
            05           J65EKR PICTURE S9(4) VALUE  ZERO.
            05           J65JGR PICTURE S9(4) VALUE  ZERO.
            05           J65JLR PICTURE S9(4) VALUE  ZERO.
            05           J65KKR PICTURE S9(4) VALUE  ZERO.
            05           J65KOR PICTURE S9(4) VALUE  ZERO.
            05           J65KRR PICTURE S9(4) VALUE  ZERO.
            05           J65KVR PICTURE S9(4) VALUE  ZERO.
            05           J65MAR PICTURE S9(4) VALUE  ZERO.
            05           J65MER PICTURE S9(4) VALUE  ZERO.
            05           J65MGR PICTURE S9(4) VALUE  ZERO.
            05           J65MIR PICTURE S9(4) VALUE  ZERO.
            05           J65PFR PICTURE S9(4) VALUE  ZERO.
            05           J65PSR PICTURE S9(4) VALUE  ZERO.
            05           J65RAR PICTURE S9(4) VALUE  ZERO.
            05           J65RER PICTURE S9(4) VALUE  ZERO.
            05           J65SGR PICTURE S9(4) VALUE  ZERO.
            05           J70EER PICTURE S9(4) VALUE  ZERO.
            05           J70EIR PICTURE S9(4) VALUE  ZERO.
            05           J75CER PICTURE S9(4) VALUE  ZERO.
            05           J75MCR PICTURE S9(4) VALUE  ZERO.
            05           J75TER PICTURE S9(4) VALUE  ZERO.
       01   ZONES-UTILISATEUR PICTURE X.                                CI0284
      *COPYBOOK WITH HTML TEXT

       COPY CI0284C1.

      *COPYBOOK WITH MESSAGE TEXT

       COPY CI0289MM.
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **      THESE SEGMENTS ARE THE INPUT LINKAGE FOR CI0284          *
      ******************************************************************
      *
      *!WF DSP=V2 DSL=V2 SEL=49 FOR=I LEV=1 PLT=75
       01                 V200.                                         CI0284
          05              V200-SUITE.                                   CI0284
            15       FILLER         PICTURE  X(00881).                  CI0284
       01                 V249  REDEFINES      V200.                    CI0284
            10            V249-GCOMN.                                   CI0284
            11            V249-MAPPN  PICTURE  X(10).                   CI0284
            11            V249-NSSSI  PICTURE  X(24).                   CI0284
            11            V249-CTTYPG PICTURE  X(04).                   CI0284
            11            V249-DCACG  PICTURE  9(8).                    CI0284
            11            V249-CTSET  PICTURE  9(6).                    CI0284
            11            V249-QNACT  PICTURE  9(3).                    CI0284
            11            V249-CTID   PICTURE  X(27).                   CI0284
            11            V249-GETIM  PICTURE  S9(7)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-CAACT  PICTURE  X(1).                    CI0284
            11            V249-CALSTC PICTURE  X.                       CI0284
            11            V249-CACTS  PICTURE  X.                       CI0284
            11            V249-ADBRQ  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-PACT1  PICTURE  S999V999                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-QSHOWQ PICTURE  S9(9)V999                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-ADBRQ1 PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-PACT1A PICTURE  S999V999                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-QSHO5A PICTURE  S9(9)V999                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-GXREF.                                   CI0284
            12            V249-GACMS.                                   CI0284
            13            V249-XZ30   PICTURE  X(30).                   CI0284
            13            V249-FILLER                                   CI0284
                          REDEFINES            V249-XZ30.               CI0284
            14            V249-IAIND  PICTURE  X                        CI0284
                          OCCURS       030     TIMES.                   CI0284
            12            V249-GMSAC.                                   CI0284
            13            V249-XZ10   PICTURE  X(10).                   CI0284
            13            V249-FILLER                                   CI0284
                          REDEFINES            V249-XZ10.               CI0284
            14            V249-IACCT  PICTURE  X(1)                     CI0284
                          OCCURS       010     TIMES.                   CI0284
            11            V249-NGEOPA PICTURE  X(08).                   CI0284
            11            V249-CTRHO  PICTURE  9(8).                    CI0284
            11            V249-GETOD  PICTURE  9(6).                    CI0284
            11            V249-CSLCT  PICTURE  X.                       CI0284
            11            V249-CCLCH  PICTURE  X.                       CI0284
            11            V249-CCLPR  PICTURE  X.                       CI0284
            11            V249-CCLSU  PICTURE  X.                       CI0284
            11            V249-DXTMS2 PICTURE  X(26).                   CI0284
            11            V249-FILLER PICTURE  X(007).                  CI0284
            11            V249-GOTPT.                                   CI0284
            12            V249-CCONF  PICTURE  X(25).                   CI0284
            12            V249-XDCNN  PICTURE  X(17).                   CI0284
            12            V249-FILLER PICTURE  X(08).                   CI0284
            10            V249-GMSGS.                                   CI0284
            11            V249-NMESS2 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-TDTXT1 PICTURE  X(512).                  CI0284
            11            V249-FILLER PICTURE  X(37).                   CI0284
            10            V249-GME87                                    CI0284
                          REDEFINES            V249-GMSGS.              CI0284
            11            V249-CEXTP  PICTURE  X.                       CI0284
            11            V249-AEDRQ  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-PWHLD  PICTURE  S999V9(5)                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-CPORTA PICTURE  X.                       CI0284
            11            V249-TIPUP  PICTURE  X(8).                    CI0284
            11            V249-DEFFT  PICTURE  9(8).                    CI0284
            11            V249-GTOAD.                                   CI0284
            12            V249-CDELIX PICTURE  X(3).                    CI0284
            11            V249-GTOAC.                                   CI0284
            12            V249-CTID01 PICTURE  X(27).                   CI0284
            11            V249-ITRNB  PICTURE  X.                       CI0284
            11            V249-CIRAP  PICTURE  XX.                      CI0284
            11            V249-CEXTP1 PICTURE  X.                       CI0284
            11            V249-ADBRQ2 PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-ATWHDD PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-ATWHDE PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-CTWHPB PICTURE  9(3)V999.                CI0284
            11            V249-ACOTD  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-TDTXTA PICTURE  X(80).                   CI0284
            11            V249-CPAYC  PICTURE  X(2).                    CI0284
            11            V249-CLID   PICTURE  X(23).                   CI0284
            11            V249-GECSQ  PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-NTR    PICTURE  9(8).                    CI0284
            11            V249-GECKD  PICTURE  9.                       CI0284
            11            V249-NPBN   PICTURE  X(20).                   CI0284
            11            V249-CCBAT  PICTURE  99.                      CI0284
            11            V249-GENAL1 PICTURE  X(30).                   CI0284
            11            V249-GENAL2 PICTURE  X(30).                   CI0284
            11            V249-GESAD1 PICTURE  X(30).                   CI0284
            11            V249-GESAD2 PICTURE  X(30).                   CI0284
            11            V249-GESAD3 PICTURE  X(30).                   CI0284
            10            V249-GME97                                    CI0284
                          REDEFINES            V249-GMSGS.              CI0284
            11            V249-GRID   PICTURE  X(13).                   CI0284
            11            V249-PRCOD  PICTURE  9(5).                    CI0284
            11            V249-CTIDA  PICTURE  9(3).                    CI0284
            11            V249-PRSCD  PICTURE  X(9).                    CI0284
            11            V249-PRCPRE PICTURE  X(4).                    CI0284
            11            V249-CEIT   PICTURE  9(3).                    CI0284
            11            V249-CMPFC  PICTURE  9(3).                    CI0284
            10            V249-GME11                                    CI0284
                          REDEFINES            V249-GMSGS.              CI0284
            11            V249-FILLER PICTURE  X.                       CI0284
            10            V249-GMD49                                    CI0284
                          REDEFINES            V249-GMSGS.              CI0284
            11            V249-CCEIT  PICTURE  X.                       CI0284
            11            V249-CEITX  PICTURE  9(3).                    CI0284
            10            V249-GME13                                    CI0284
                          REDEFINES            V249-GMSGS.              CI0284
            11            V249-CTTYP  PICTURE  X(04).                   CI0284
            10            V249-GMD43                                    CI0284
                          REDEFINES            V249-GMSGS.              CI0284
            11            V249-PERFE  PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            V249-PNRFE  PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            10            V249-GME20                                    CI0284
                          REDEFINES            V249-GMSGS.              CI0284
            11            V249-CPNOP  PICTURE  X(2).                    CI0284
            11            V249-PNPCT  PICTURE  999.                     CI0284
            11            V249-PNRFE1 PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            10            V249-MPRN4  PICTURE  X(35).                   CI0284
            10            V249-CTNOB1 PICTURE  X.                       CI0284
            10            V249-CTNOB2 PICTURE  X.                       CI0284
            10            V249-CTNOB3 PICTURE  X.                       CI0284
            10            V249-CRITO  PICTURE  X.                       CI0284
            10            V249-CTIFR  PICTURE  X.                       CI0284
            10            V249-AINVT  PICTURE  S9(11)                   CI0284
                          COMPUTATIONAL-3.                              CI0284
            10            V249-NGEOR  PICTURE  9(08).                   CI0284
      *!WF DSP=FR DSL=QT SEL=58 FOR=I LEV=1 PLT=75
       01                 FR00.                                         CI0284
          05              FR00-SUITE.                                   CI0284
            15       FILLER         PICTURE  X(02300).                  CI0284
       01                 FR58  REDEFINES      FR00.                    CI0284
            10            FR58-QT5K.                                    CI0284
            11            FR58-C299.                                    CI0284
            12            FR58-CTID.                                    CI0284
            13            FR58-CTIDA  PICTURE  9(3).                    CI0284
            13            FR58-CTIDN.                                   CI0284
            14            FR58-CTIDNP PICTURE  X(13).                   CI0284
            14            FR58-CTIDND PICTURE  9(11).                   CI0284
            11            FR58-GECKD2 PICTURE  9.                       CI0284
            11            FR58-NSEQ5  PICTURE  9(5).                    CI0284
            11            FR58-CTSTA  PICTURE  99.                      CI0284
            11            FR58-CTSTAL PICTURE  X(10).                   CI0284
            11            FR58-CTOWN  PICTURE  9(3).                    CI0284
            11            FR58-CTTLN1 PICTURE  X(30).                   CI0284
            11            FR58-CTTLN2 PICTURE  X(30).                   CI0284
            11            FR58-CTTLN3 PICTURE  X(30).                   CI0284
            11            FR58-CTTBO1 PICTURE  X(45).                   CI0284
            11            FR58-CTTBO2 PICTURE  X(45).                   CI0284
            11            FR58-CTEFD  PICTURE  9(8).                    CI0284
            11            FR58-CTIAD  PICTURE  9(8).                    CI0284
            11            FR58-CTCUS  PICTURE  999.                     CI0284
            11            FR58-GR98.                                    CI0284
            12            FR58-GRID.                                    CI0284
            13            FR58-GRIDC  PICTURE  9(3).                    CI0284
            13            FR58-GRIDN.                                   CI0284
            14            FR58-GRIDNP PICTURE  99.                      CI0284
            14            FR58-GRIDND PICTURE  9(8).                    CI0284
            11            FR58-CQACT  PICTURE  999.                     CI0284
            11            FR58-CTCCI  PICTURE  X.                       CI0284
            11            FR58-CIRAS  PICTURE  999.                     CI0284
            11            FR58-CIRAT  PICTURE  999.                     CI0284
            11            FR58-IACVD  PICTURE  X.                       CI0284
            11            FR58-FILLER PICTURE  X(4).                    CI0284
            11            FR58-PRCODA PICTURE  X(5).                    CI0284
            11            FR58-PRCMN  PICTURE  X(20).                   CI0284
            11            FR58-MRPLN  PICTURE  X(30).                   CI0284
            11            FR58-CPRDG  PICTURE  9(2).                    CI0284
            11            FR58-CPRDA1 PICTURE  9(3).                    CI0284
            11            FR58-PRSCD  PICTURE  X(9).                    CI0284
            11            FR58-MSP03  PICTURE  X(3).                    CI0284
            11            FR58-CGRLI  PICTURE  X.                       CI0284
            11            FR58-ITERM  PICTURE  X(1).                    CI0284
            11            FR58-IVARP  PICTURE  X.                       CI0284
            11            FR58-DVALU  PICTURE  9(8).                    CI0284
            11            FR58-AACTV  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ACCTVC PICTURE  X(20).                   CI0284
            11            FR58-ITXTI  PICTURE  X.                       CI0284
            11            FR58-ASANP  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ACINV  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CELBL  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-NMESS2 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-FILLER PICTURE  X(1).                    CI0284
            11            FR58-PRCLN  PICTURE  X(60).                   CI0284
            11            FR58-GECKD  PICTURE  9.                       CI0284
            11            FR58-MPLNA  PICTURE  X(19).                   CI0284
            11            FR58-CQACTL PICTURE  X(45).                   CI0284
            11            FR58-CRQPA  PICTURE  9(3).                    CI0284
            11            FR58-IVANT  PICTURE  X(1).                    CI0284
            11            FR58-IDBRP  PICTURE  X(1).                    CI0284
            11            FR58-IANPY  PICTURE  X.                       CI0284
            11            FR58-IVARP1 PICTURE  X.                       CI0284
            11            FR58-FILLER PICTURE  X(27).                   CI0284
            11            FR58-NSEQ2A PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-NSEQ2P PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-MRPSN  PICTURE  X(12).                   CI0284
            11            FR58-GEHCD  PICTURE  9(3)                     CI0284
                          OCCURS       002     TIMES.                   CI0284
            11            FR58-GEHCSU PICTURE  9(5)                     CI0284
                          OCCURS       002     TIMES.                   CI0284
            11            FR58-PRCSN  PICTURE  X(9).                    CI0284
            11            FR58-CGRMF  PICTURE  X.                       CI0284
            11            FR58-IGFEX  PICTURE  X.                       CI0284
            11            FR58-CLIDP  PICTURE  X(23).                   CI0284
            11            FR58-CLCTRC PICTURE  9(3).                    CI0284
            11            FR58-ADINP  PICTURE  X(20).                   CI0284
            11            FR58-CLCTRA PICTURE  9(3).                    CI0284
            11            FR58-GRPLC  PICTURE  99.                      CI0284
            11            FR58-CIDRP  PICTURE  99.                      CI0284
            11            FR58-FILLER PICTURE  X(01).                   CI0284
            11            FR58-AVMTOT PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AVCSH  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AMARC  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AVLMX  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AVLMN  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-INDRS  PICTURE  X.                       CI0284
            11            FR58-MPRN4  PICTURE  X(35).                   CI0284
            11            FR58-FILLER PICTURE  X(1).                    CI0284
            11            FR58-ACVALM PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-INDRSA PICTURE  X(2).                    CI0284
            11            FR58-DXTMSA PICTURE  X(26).                   CI0284
            11            FR58-NMESS6 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-NMESS7 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-IBIDSA PICTURE  X.                       CI0284
            11            FR58-IBIDSB PICTURE  X.                       CI0284
            11            FR58-INSPOS PICTURE  X.                       CI0284
            11            FR58-INSPOD PICTURE  X.                       CI0284
            11            FR58-ACBALX PICTURE  X(20).                   CI0284
            11            FR58-AINVMX PICTURE  X(20).                   CI0284
            11            FR58-AMARCX PICTURE  X(20).                   CI0284
            11            FR58-AVMTOX PICTURE  X(20).                   CI0284
            11            FR58-IMNPR  PICTURE  X.                       CI0284
            11            FR58-ISSPL  PICTURE  X.                       CI0284
            11            FR58-AVMTOI PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AVCSHI PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-APOSC  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AVLMXI PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AVLMN1 PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AVLMN2 PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-FILLER PICTURE  X(05).                   CI0284
            10            FR58-QT5A.                                    CI0284
            11            FR58-CLID   PICTURE  X(23).                   CI0284
            11            FR58-GECKD1 PICTURE  9.                       CI0284
            11            FR58-MCLNM  PICTURE  X(40).                   CI0284
            11            FR58-MCLNM2 PICTURE  X(40).                   CI0284
            11            FR58-CLTYP  PICTURE  X.                       CI0284
            11            FR58-CLDOB  PICTURE  9(8).                    CI0284
            11            FR58-CLDTH  PICTURE  X.                       CI0284
            11            FR58-CLTIN  PICTURE  9(12).                   CI0284
            11            FR58-CLTINC PICTURE  9.                       CI0284
            11            FR58-GESAD1 PICTURE  X(30).                   CI0284
            11            FR58-GESAD2 PICTURE  X(30).                   CI0284
            11            FR58-GESAD3 PICTURE  X(30).                   CI0284
            11            FR58-GECIT  PICTURE  X(25).                   CI0284
            11            FR58-GECTRY PICTURE  X(20).                   CI0284
            11            FR58-GEPCD  PICTURE  X(12).                   CI0284
            11            FR58-GEST   PICTURE  X(8).                    CI0284
            11            FR58-GEADS  PICTURE  9.                       CI0284
            11            FR58-GECSD  PICTURE  9(8).                    CI0284
            11            FR58-QCLAGE PICTURE  9(3)V9                   CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-FILLER PICTURE  X(06).                   CI0284
            10            FR58-QT5T.                                    CI0284
            11            FR58-ATFRA  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AGOFD  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-APRMX  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-APRMN  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-IOWNC  PICTURE  X.                       CI0284
            11            FR58-COWNF  PICTURE  X(30).                   CI0284
            11            FR58-CTYPE  PICTURE  X.                       CI0284
            11            FR58-CIRAC  PICTURE  X(5).                    CI0284
            11            FR58-CTXMT  PICTURE  9(2).                    CI0284
            11            FR58-AMIND  PICTURE  S9(7)V99.                CI0284
            11            FR58-AMAXAR PICTURE  S9(7)V99.                CI0284
            11            FR58-QSHOWQ PICTURE  S9(9)V999                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-QSHOW0 PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-PPOT1  PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-PACT1  PICTURE  S999V999                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-IPRTA  PICTURE  X.                       CI0284
            11            FR58-FILLER PICTURE  X.                       CI0284
            11            FR58-CLCUS  PICTURE  99.                      CI0284
            11            FR58-CCDSCW PICTURE  9(2).                    CI0284
            11            FR58-CCACT  PICTURE  99.                      CI0284
            11            FR58-CIRAG.                                   CI0284
            12            FR58-CIRAP  PICTURE  XX                       CI0284
                          OCCURS       010     TIMES.                   CI0284
            11            FR58-ITERF  PICTURE  X.                       CI0284
            11            FR58-IACFPD PICTURE  X(1).                    CI0284
            11            FR58-AFEET  PICTURE  S9(5)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ATERF  PICTURE  S9(5)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CLIDNB PICTURE  9(8).                    CI0284
            11            FR58-ALOAD  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASURR  PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASHIS  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AMNBL  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-APNAC  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ANGOF  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CPLTYP PICTURE  X(14).                   CI0284
            10            FR58-QT5N.                                    CI0284
            11            FR58-IARRAN PICTURE  X.                       CI0284
            11            FR58-GESTD1 PICTURE  9(8).                    CI0284
            11            FR58-GEEND1 PICTURE  S9(8)                    CI0284
                          BINARY.                                       CI0284
            11            FR58-GESTD  PICTURE  9(8).                    CI0284
            11            FR58-GEEND  PICTURE  9(8).                    CI0284
            11            FR58-NSQ4B2 PICTURE  9(8)                     CI0284
                          BINARY.                                       CI0284
            11            FR58-CDEST  PICTURE  99.                      CI0284
            11            FR58-DEFFT  PICTURE  9(8).                    CI0284
            11            FR58-CPMTF  PICTURE  99.                      CI0284
            11            FR58-CPMTG  PICTURE  99.                      CI0284
            11            FR58-MPMTFL PICTURE  X(24).                   CI0284
            11            FR58-MPMTFE PICTURE  X(24).                   CI0284
            11            FR58-DLAUP  PICTURE  9(8).                    CI0284
            11            FR58-NSEQ4B PICTURE  9(8)                     CI0284
                          BINARY.                                       CI0284
            11            FR58-QSACTF PICTURE  9(3).                    CI0284
            11            FR58-QSACTT PICTURE  9(3).                    CI0284
            11            FR58-CCONF  PICTURE  X(25).                   CI0284
            11            FR58-DCONF  PICTURE  9(8).                    CI0284
            11            FR58-DTIMT  PICTURE  X(8).                    CI0284
            11            FR58-CACTS  PICTURE  X.                       CI0284
            11            FR58-ADBRQ  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-DNPMT  PICTURE  9(8).                    CI0284
            11            FR58-NAPDS  PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CDEST1 PICTURE  99.                      CI0284
            11            FR58-CLANR1 PICTURE  X(23).                   CI0284
            11            FR58-FILLER PICTURE  X(01).                   CI0284
            10            FR58-FILLER PICTURE  X(600).                  CI0284
            10            FR58-QT5C                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-CESLD  PICTURE  9(8).                    CI0284
            11            FR58-PCIRB5 PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-PANYDD PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CEIT   PICTURE  9(3).                    CI0284
            11            FR58-PPART  PICTURE  9(3)V99                  CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-DTRME  PICTURE  9(8).                    CI0284
            11            FR58-CEIRND PICTURE  9(8).                    CI0284
            11            FR58-DANNIA PICTURE  9(8).                    CI0284
            11            FR58-AAPAA  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CELBDT PICTURE  9(8).                    CI0284
            11            FR58-CEIIS  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-DTRME1 PICTURE  9(8).                    CI0284
            11            FR58-GMKTS.                                   CI0284
            12            FR58-DTRME2 PICTURE  9(8)                     CI0284
                          OCCURS       005     TIMES.                   CI0284
            12            FR58-DTRME3 PICTURE  9(8)                     CI0284
                          OCCURS       005     TIMES.                   CI0284
            11            FR58-ALINT  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CEHCD  PICTURE  9(3)                     CI0284
                          OCCURS       006     TIMES.                   CI0284
            11            FR58-CEFOTR PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-DGPED  PICTURE  9(8).                    CI0284
            11            FR58-DIPED  PICTURE  9(8).                    CI0284
            11            FR58-FILLER PICTURE  X(409).                  CI0284
            10            FR58-QT5F                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-DLAUP2 PICTURE  9(8).                    CI0284
            11            FR58-QSHOW  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AFAVP  PICTURE  S9(4)V9(3)               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-QSHIS  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-QSHNM  PICTURE  S9(10)V999.              CI0284
            11            FR58-QSHOM  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ADDAC  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-QSHES  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-NDCUS  PICTURE  X(9).                    CI0284
            11            FR58-CSTKR5 PICTURE  X(5).                    CI0284
            11            FR58-NACID  PICTURE  S9(11)                   CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AGOFD2 PICTURE  S9(9)V99.                CI0284
            11            FR58-TCBAT  PICTURE  X(21).                   CI0284
            11            FR58-FILLER PICTURE  X(490).                  CI0284
            10            FR58-QT5L                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-ALDBEN PICTURE  S9(09)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-APREL  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ALMODE PICTURE  99.                      CI0284
            11            FR58-ITMEC  PICTURE  X(1).                    CI0284
            11            FR58-ITAMR  PICTURE  X(1).                    CI0284
            11            FR58-MPMTF  PICTURE  X(14).                   CI0284
            11            FR58-TPLNL  PICTURE  X(30).                   CI0284
            11            FR58-ASBENA PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASBENB PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASBENC PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASBENE PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASBENF PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-GESTNS PICTURE  X(2).                    CI0284
            11            FR58-CTWHPB PICTURE  9(3)V999.                CI0284
            11            FR58-CTWHCB PICTURE  X.                       CI0284
            11            FR58-AMVA1  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASPAM  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ACTCH  PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AMXLN  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ALFGH  PICTURE  999.                     CI0284
            11            FR58-ALPLNI PICTURE  9.                       CI0284
            11            FR58-ATSA8  PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CVALB  PICTURE  X(3).                    CI0284
            11            FR58-ASURRN PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ASURRW PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ATLTB  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AEARN0 PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ATFPI  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-AEARN1 PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ISELO  PICTURE  X.                       CI0284
            11            FR58-CCLAC  PICTURE  X.                       CI0284
            11            FR58-ALINNO PICTURE  99.                      CI0284
            11            FR58-ALPLNJ PICTURE  9.                       CI0284
            11            FR58-COLPL  PICTURE  9(05).                   CI0284
            11            FR58-ALPLDT PICTURE  9(8).                    CI0284
            11            FR58-ANFMC  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CPNOP  PICTURE  X(2).                    CI0284
            11            FR58-CVSTC  PICTURE  X(4).                    CI0284
            11            FR58-CGMBR  PICTURE  X.                       CI0284
            11            FR58-DWSDT  PICTURE  9(8).                    CI0284
            11            FR58-IRDPH  PICTURE  X.                       CI0284
            11            FR58-DWAIT  PICTURE  9(8).                    CI0284
            11            FR58-IAPGP  PICTURE  X.                       CI0284
            11            FR58-CASTA  PICTURE  X.                       CI0284
            11            FR58-CSSUP2 PICTURE  X.                       CI0284
            11            FR58-CVOMC1 PICTURE  X(1).                    CI0284
            11            FR58-APGBP  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ALDDUE PICTURE  9(08).                   CI0284
            11            FR58-APYMT  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ALSURR PICTURE  S9(09)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CESTP  PICTURE  X(03).                   CI0284
            11            FR58-FILLER PICTURE  X(356).                  CI0284
            10            FR58-QT5O                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-NBACT  PICTURE  S9(11)                   CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CTIAC  PICTURE  S9(3)                    CI0284
                          BINARY.                                       CI0284
            11            FR58-CASTT  PICTURE  S99                      CI0284
                          BINARY.                                       CI0284
            11            FR58-CATMI  PICTURE  S9                       CI0284
                          BINARY.                                       CI0284
            11            FR58-IATMR  PICTURE  X(3).                    CI0284
            11            FR58-IBIPI  PICTURE  X.                       CI0284
            11            FR58-CBPST  PICTURE  S99                      CI0284
                          BINARY.                                       CI0284
            11            FR58-TBPST  PICTURE  X(16).                   CI0284
            11            FR58-CODPI  PICTURE  X.                       CI0284
            11            FR58-TODPS  PICTURE  X(9).                    CI0284
            11            FR58-FILLER PICTURE  X(448).                  CI0284
            11            FR58-IBPSD  PICTURE  X.                       CI0284
            11            FR58-FILLER PICTURE  X(107).                  CI0284
            11            FR58-QT5E                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            12            FR58-MPRN4X PICTURE  X(100).                  CI0284
            12            FR58-CCMSH  PICTURE  X(2).                    CI0284
            12            FR58-CPRCS  PICTURE  X(04).                   CI0284
            12            FR58-CURST  PICTURE  X.                       CI0284
            10            FR58-QT5M                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-NAPCN1 PICTURE  X(24).                   CI0284
            11            FR58-FILLER PICTURE  X(576).                  CI0284
            10            FR58-QT5B                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-NAPCN2 PICTURE  X(24).                   CI0284
            11            FR58-CTIDAL PICTURE  X(40).                   CI0284
            11            FR58-NPHNS  PICTURE  X(14).                   CI0284
            11            FR58-FILLER PICTURE  X(522).                  CI0284
            10            FR58-QT5P                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-CFPPT  PICTURE  9(3).                    CI0284
            11            FR58-TTYPP  PICTURE  X(40).                   CI0284
            11            FR58-CPPST  PICTURE  9(3).                    CI0284
            11            FR58-TPPST  PICTURE  X(15).                   CI0284
            11            FR58-APFEEQ PICTURE  S9(7)V99.                CI0284
            11            FR58-APFEEC PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-APFEEP PICTURE  S9(7)V99.                CI0284
            11            FR58-ISVCA  PICTURE  X.                       CI0284
            11            FR58-NSBVS  PICTURE  X(5).                    CI0284
            11            FR58-ICKRV  PICTURE  X.                       CI0284
            11            FR58-PDAMT  PICTURE  S9(03).                  CI0284
            11            FR58-PSTAX  PICTURE  S9(03)V999.              CI0284
            11            FR58-DPCAL  PICTURE  9(8).                    CI0284
            11            FR58-NADVF  PICTURE  X(08).                   CI0284
            11            FR58-DAGUP  PICTURE  9(8).                    CI0284
            11            FR58-AANFEA PICTURE  9(5)V99                  CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-CLIDN7 PICTURE  9(8)                     CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-ARANV  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            FR58-DRANV  PICTURE  9(8).                    CI0284
            11            FR58-FILLER PICTURE  X(454).                  CI0284
            10            FR58-QT50                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-NANCA  PICTURE  X(30).                   CI0284
            11            FR58-MANCN  PICTURE  X(100).                  CI0284
            11            FR58-AINPTX PICTURE  X(20).                   CI0284
            11            FR58-CTID01 PICTURE  X(27).                   CI0284
            11            FR58-NANCA1 PICTURE  X(04).                   CI0284
            11            FR58-IIVAR  PICTURE  X(1).                    CI0284
            11            FR58-FILLER PICTURE  X(418).                  CI0284
            10            FR58-QT5R                                     CI0284
                          REDEFINES            FR58-FILLER.             CI0284
            11            FR58-NACTJ  PICTURE  X(04).                   CI0284
            11            FR58-NACNO6 PICTURE  X(11).                   CI0284
            11            FR58-FILLER PICTURE  X(585).                  CI0284
            10            FR58-AMAXA  PICTURE  S9(7)V99.                CI0284
            10            FR58-ISAOR  PICTURE  X.                       CI0284
            10            FR58-ISACH  PICTURE  X.                       CI0284
            10            FR58-CERRBA PICTURE  X(02).                   CI0284
            10            FR58-CERRBH PICTURE  X(02).                   CI0284
            10            FR58-IWITHH PICTURE  X.                       CI0284
            10            FR58-CTID20 PICTURE  X(27).                   CI0284
            10            FR58-GECKD3 PICTURE  9.                       CI0284
            10            FR58-DANFC  PICTURE  X(10).                   CI0284
            10            FR58-DAFCN  PICTURE  X(10).                   CI0284
            10            FR58-ISMTA  PICTURE  X.                       CI0284
            10            FR58-CERRBT PICTURE  X(02).                   CI0284
            10            FR58-NPLNI  PICTURE  X(10).                   CI0284
            10            FR58-FILLER PICTURE  X(023).                  CI0284
      *!WF DSP=TO DSL=QT SEL=58 FOR=I LEV=1 PLT=75
       01                 TO00.                                         CI0284
          05              TO00-SUITE.                                   CI0284
            15       FILLER         PICTURE  X(02300).                  CI0284
       01                 TO58  REDEFINES      TO00.                    CI0284
            10            TO58-QT5K.                                    CI0284
            11            TO58-C299.                                    CI0284
            12            TO58-CTID.                                    CI0284
            13            TO58-CTIDA  PICTURE  9(3).                    CI0284
            13            TO58-CTIDN.                                   CI0284
            14            TO58-CTIDNP PICTURE  X(13).                   CI0284
            14            TO58-CTIDND PICTURE  9(11).                   CI0284
            11            TO58-GECKD2 PICTURE  9.                       CI0284
            11            TO58-NSEQ5  PICTURE  9(5).                    CI0284
            11            TO58-CTSTA  PICTURE  99.                      CI0284
            11            TO58-CTSTAL PICTURE  X(10).                   CI0284
            11            TO58-CTOWN  PICTURE  9(3).                    CI0284
            11            TO58-CTTLN1 PICTURE  X(30).                   CI0284
            11            TO58-CTTLN2 PICTURE  X(30).                   CI0284
            11            TO58-CTTLN3 PICTURE  X(30).                   CI0284
            11            TO58-CTTBO1 PICTURE  X(45).                   CI0284
            11            TO58-CTTBO2 PICTURE  X(45).                   CI0284
            11            TO58-CTEFD  PICTURE  9(8).                    CI0284
            11            TO58-CTIAD  PICTURE  9(8).                    CI0284
            11            TO58-CTCUS  PICTURE  999.                     CI0284
            11            TO58-GR98.                                    CI0284
            12            TO58-GRID.                                    CI0284
            13            TO58-GRIDC  PICTURE  9(3).                    CI0284
            13            TO58-GRIDN.                                   CI0284
            14            TO58-GRIDNP PICTURE  99.                      CI0284
            14            TO58-GRIDND PICTURE  9(8).                    CI0284
            11            TO58-CQACT  PICTURE  999.                     CI0284
            11            TO58-CTCCI  PICTURE  X.                       CI0284
            11            TO58-CIRAS  PICTURE  999.                     CI0284
            11            TO58-CIRAT  PICTURE  999.                     CI0284
            11            TO58-IACVD  PICTURE  X.                       CI0284
            11            TO58-FILLER PICTURE  X(4).                    CI0284
            11            TO58-PRCODA PICTURE  X(5).                    CI0284
            11            TO58-PRCMN  PICTURE  X(20).                   CI0284
            11            TO58-MRPLN  PICTURE  X(30).                   CI0284
            11            TO58-CPRDG  PICTURE  9(2).                    CI0284
            11            TO58-CPRDA1 PICTURE  9(3).                    CI0284
            11            TO58-PRSCD  PICTURE  X(9).                    CI0284
            11            TO58-MSP03  PICTURE  X(3).                    CI0284
            11            TO58-CGRLI  PICTURE  X.                       CI0284
            11            TO58-ITERM  PICTURE  X(1).                    CI0284
            11            TO58-IVARP  PICTURE  X.                       CI0284
            11            TO58-DVALU  PICTURE  9(8).                    CI0284
            11            TO58-AACTV  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ACCTVC PICTURE  X(20).                   CI0284
            11            TO58-ITXTI  PICTURE  X.                       CI0284
            11            TO58-ASANP  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ACINV  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CELBL  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-NMESS2 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-FILLER PICTURE  X(1).                    CI0284
            11            TO58-PRCLN  PICTURE  X(60).                   CI0284
            11            TO58-GECKD  PICTURE  9.                       CI0284
            11            TO58-MPLNA  PICTURE  X(19).                   CI0284
            11            TO58-CQACTL PICTURE  X(45).                   CI0284
            11            TO58-CRQPA  PICTURE  9(3).                    CI0284
            11            TO58-IVANT  PICTURE  X(1).                    CI0284
            11            TO58-IDBRP  PICTURE  X(1).                    CI0284
            11            TO58-IANPY  PICTURE  X.                       CI0284
            11            TO58-IVARP1 PICTURE  X.                       CI0284
            11            TO58-FILLER PICTURE  X(27).                   CI0284
            11            TO58-NSEQ2A PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-NSEQ2P PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-MRPSN  PICTURE  X(12).                   CI0284
            11            TO58-GEHCD  PICTURE  9(3)                     CI0284
                          OCCURS       002     TIMES.                   CI0284
            11            TO58-GEHCSU PICTURE  9(5)                     CI0284
                          OCCURS       002     TIMES.                   CI0284
            11            TO58-PRCSN  PICTURE  X(9).                    CI0284
            11            TO58-CGRMF  PICTURE  X.                       CI0284
            11            TO58-IGFEX  PICTURE  X.                       CI0284
            11            TO58-CLIDP  PICTURE  X(23).                   CI0284
            11            TO58-CLCTRC PICTURE  9(3).                    CI0284
            11            TO58-ADINP  PICTURE  X(20).                   CI0284
            11            TO58-CLCTRA PICTURE  9(3).                    CI0284
            11            TO58-GRPLC  PICTURE  99.                      CI0284
            11            TO58-CIDRP  PICTURE  99.                      CI0284
            11            TO58-FILLER PICTURE  X(01).                   CI0284
            11            TO58-AVMTOT PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AVCSH  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AMARC  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AVLMX  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AVLMN  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-INDRS  PICTURE  X.                       CI0284
            11            TO58-MPRN4  PICTURE  X(35).                   CI0284
            11            TO58-FILLER PICTURE  X(1).                    CI0284
            11            TO58-ACVALM PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-INDRSA PICTURE  X(2).                    CI0284
            11            TO58-DXTMSA PICTURE  X(26).                   CI0284
            11            TO58-NMESS6 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-NMESS7 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-IBIDSA PICTURE  X.                       CI0284
            11            TO58-IBIDSB PICTURE  X.                       CI0284
            11            TO58-INSPOS PICTURE  X.                       CI0284
            11            TO58-INSPOD PICTURE  X.                       CI0284
            11            TO58-ACBALX PICTURE  X(20).                   CI0284
            11            TO58-AINVMX PICTURE  X(20).                   CI0284
            11            TO58-AMARCX PICTURE  X(20).                   CI0284
            11            TO58-AVMTOX PICTURE  X(20).                   CI0284
            11            TO58-IMNPR  PICTURE  X.                       CI0284
            11            TO58-ISSPL  PICTURE  X.                       CI0284
            11            TO58-AVMTOI PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AVCSHI PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-APOSC  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AVLMXI PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AVLMN1 PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AVLMN2 PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-FILLER PICTURE  X(05).                   CI0284
            10            TO58-QT5A.                                    CI0284
            11            TO58-CLID   PICTURE  X(23).                   CI0284
            11            TO58-GECKD1 PICTURE  9.                       CI0284
            11            TO58-MCLNM  PICTURE  X(40).                   CI0284
            11            TO58-MCLNM2 PICTURE  X(40).                   CI0284
            11            TO58-CLTYP  PICTURE  X.                       CI0284
            11            TO58-CLDOB  PICTURE  9(8).                    CI0284
            11            TO58-CLDTH  PICTURE  X.                       CI0284
            11            TO58-CLTIN  PICTURE  9(12).                   CI0284
            11            TO58-CLTINC PICTURE  9.                       CI0284
            11            TO58-GESAD1 PICTURE  X(30).                   CI0284
            11            TO58-GESAD2 PICTURE  X(30).                   CI0284
            11            TO58-GESAD3 PICTURE  X(30).                   CI0284
            11            TO58-GECIT  PICTURE  X(25).                   CI0284
            11            TO58-GECTRY PICTURE  X(20).                   CI0284
            11            TO58-GEPCD  PICTURE  X(12).                   CI0284
            11            TO58-GEST   PICTURE  X(8).                    CI0284
            11            TO58-GEADS  PICTURE  9.                       CI0284
            11            TO58-GECSD  PICTURE  9(8).                    CI0284
            11            TO58-QCLAGE PICTURE  9(3)V9                   CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-FILLER PICTURE  X(06).                   CI0284
            10            TO58-QT5T.                                    CI0284
            11            TO58-ATFRA  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AGOFD  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-APRMX  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-APRMN  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-IOWNC  PICTURE  X.                       CI0284
            11            TO58-COWNF  PICTURE  X(30).                   CI0284
            11            TO58-CTYPE  PICTURE  X.                       CI0284
            11            TO58-CIRAC  PICTURE  X(5).                    CI0284
            11            TO58-CTXMT  PICTURE  9(2).                    CI0284
            11            TO58-AMIND  PICTURE  S9(7)V99.                CI0284
            11            TO58-AMAXAR PICTURE  S9(7)V99.                CI0284
            11            TO58-QSHOWQ PICTURE  S9(9)V999                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-QSHOW0 PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-PPOT1  PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-PACT1  PICTURE  S999V999                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-IPRTA  PICTURE  X.                       CI0284
            11            TO58-FILLER PICTURE  X.                       CI0284
            11            TO58-CLCUS  PICTURE  99.                      CI0284
            11            TO58-CCDSCW PICTURE  9(2).                    CI0284
            11            TO58-CCACT  PICTURE  99.                      CI0284
            11            TO58-CIRAG.                                   CI0284
            12            TO58-CIRAP  PICTURE  XX                       CI0284
                          OCCURS       010     TIMES.                   CI0284
            11            TO58-ITERF  PICTURE  X.                       CI0284
            11            TO58-IACFPD PICTURE  X(1).                    CI0284
            11            TO58-AFEET  PICTURE  S9(5)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ATERF  PICTURE  S9(5)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CLIDNB PICTURE  9(8).                    CI0284
            11            TO58-ALOAD  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASURR  PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASHIS  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AMNBL  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-APNAC  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ANGOF  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CPLTYP PICTURE  X(14).                   CI0284
            10            TO58-QT5N.                                    CI0284
            11            TO58-IARRAN PICTURE  X.                       CI0284
            11            TO58-GESTD1 PICTURE  9(8).                    CI0284
            11            TO58-GEEND1 PICTURE  S9(8)                    CI0284
                          BINARY.                                       CI0284
            11            TO58-GESTD  PICTURE  9(8).                    CI0284
            11            TO58-GEEND  PICTURE  9(8).                    CI0284
            11            TO58-NSQ4B2 PICTURE  9(8)                     CI0284
                          BINARY.                                       CI0284
            11            TO58-CDEST  PICTURE  99.                      CI0284
            11            TO58-DEFFT  PICTURE  9(8).                    CI0284
            11            TO58-CPMTF  PICTURE  99.                      CI0284
            11            TO58-CPMTG  PICTURE  99.                      CI0284
            11            TO58-MPMTFL PICTURE  X(24).                   CI0284
            11            TO58-MPMTFE PICTURE  X(24).                   CI0284
            11            TO58-DLAUP  PICTURE  9(8).                    CI0284
            11            TO58-NSEQ4B PICTURE  9(8)                     CI0284
                          BINARY.                                       CI0284
            11            TO58-QSACTF PICTURE  9(3).                    CI0284
            11            TO58-QSACTT PICTURE  9(3).                    CI0284
            11            TO58-CCONF  PICTURE  X(25).                   CI0284
            11            TO58-DCONF  PICTURE  9(8).                    CI0284
            11            TO58-DTIMT  PICTURE  X(8).                    CI0284
            11            TO58-CACTS  PICTURE  X.                       CI0284
            11            TO58-ADBRQ  PICTURE  S9(11)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-DNPMT  PICTURE  9(8).                    CI0284
            11            TO58-NAPDS  PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CDEST1 PICTURE  99.                      CI0284
            11            TO58-CLANR1 PICTURE  X(23).                   CI0284
            11            TO58-FILLER PICTURE  X(01).                   CI0284
            10            TO58-FILLER PICTURE  X(600).                  CI0284
            10            TO58-QT5C                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-CESLD  PICTURE  9(8).                    CI0284
            11            TO58-PCIRB5 PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-PANYDD PICTURE  S9(3)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CEIT   PICTURE  9(3).                    CI0284
            11            TO58-PPART  PICTURE  9(3)V99                  CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-DTRME  PICTURE  9(8).                    CI0284
            11            TO58-CEIRND PICTURE  9(8).                    CI0284
            11            TO58-DANNIA PICTURE  9(8).                    CI0284
            11            TO58-AAPAA  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CELBDT PICTURE  9(8).                    CI0284
            11            TO58-CEIIS  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-DTRME1 PICTURE  9(8).                    CI0284
            11            TO58-GMKTS.                                   CI0284
            12            TO58-DTRME2 PICTURE  9(8)                     CI0284
                          OCCURS       005     TIMES.                   CI0284
            12            TO58-DTRME3 PICTURE  9(8)                     CI0284
                          OCCURS       005     TIMES.                   CI0284
            11            TO58-ALINT  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CEHCD  PICTURE  9(3)                     CI0284
                          OCCURS       006     TIMES.                   CI0284
            11            TO58-CEFOTR PICTURE  S9(3)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-DGPED  PICTURE  9(8).                    CI0284
            11            TO58-DIPED  PICTURE  9(8).                    CI0284
            11            TO58-FILLER PICTURE  X(409).                  CI0284
            10            TO58-QT5F                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-DLAUP2 PICTURE  9(8).                    CI0284
            11            TO58-QSHOW  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AFAVP  PICTURE  S9(4)V9(3)               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-QSHIS  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-QSHNM  PICTURE  S9(10)V999.              CI0284
            11            TO58-QSHOM  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ADDAC  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-QSHES  PICTURE  S9(10)V999               CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-NDCUS  PICTURE  X(9).                    CI0284
            11            TO58-CSTKR5 PICTURE  X(5).                    CI0284
            11            TO58-NACID  PICTURE  S9(11)                   CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AGOFD2 PICTURE  S9(9)V99.                CI0284
            11            TO58-TCBAT  PICTURE  X(21).                   CI0284
            11            TO58-FILLER PICTURE  X(490).                  CI0284
            10            TO58-QT5L                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-ALDBEN PICTURE  S9(09)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-APREL  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ALMODE PICTURE  99.                      CI0284
            11            TO58-ITMEC  PICTURE  X(1).                    CI0284
            11            TO58-ITAMR  PICTURE  X(1).                    CI0284
            11            TO58-MPMTF  PICTURE  X(14).                   CI0284
            11            TO58-TPLNL  PICTURE  X(30).                   CI0284
            11            TO58-ASBENA PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASBENB PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASBENC PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASBENE PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASBENF PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-GESTNS PICTURE  X(2).                    CI0284
            11            TO58-CTWHPB PICTURE  9(3)V999.                CI0284
            11            TO58-CTWHCB PICTURE  X.                       CI0284
            11            TO58-AMVA1  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASPAM  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ACTCH  PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AMXLN  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ALFGH  PICTURE  999.                     CI0284
            11            TO58-ALPLNI PICTURE  9.                       CI0284
            11            TO58-ATSA8  PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CVALB  PICTURE  X(3).                    CI0284
            11            TO58-ASURRN PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ASURRW PICTURE  S9(07)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ATLTB  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AEARN0 PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ATFPI  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-AEARN1 PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ISELO  PICTURE  X.                       CI0284
            11            TO58-CCLAC  PICTURE  X.                       CI0284
            11            TO58-ALINNO PICTURE  99.                      CI0284
            11            TO58-ALPLNJ PICTURE  9.                       CI0284
            11            TO58-COLPL  PICTURE  9(05).                   CI0284
            11            TO58-ALPLDT PICTURE  9(8).                    CI0284
            11            TO58-ANFMC  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CPNOP  PICTURE  X(2).                    CI0284
            11            TO58-CVSTC  PICTURE  X(4).                    CI0284
            11            TO58-CGMBR  PICTURE  X.                       CI0284
            11            TO58-DWSDT  PICTURE  9(8).                    CI0284
            11            TO58-IRDPH  PICTURE  X.                       CI0284
            11            TO58-DWAIT  PICTURE  9(8).                    CI0284
            11            TO58-IAPGP  PICTURE  X.                       CI0284
            11            TO58-CASTA  PICTURE  X.                       CI0284
            11            TO58-CSSUP2 PICTURE  X.                       CI0284
            11            TO58-CVOMC1 PICTURE  X(1).                    CI0284
            11            TO58-APGBP  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ALDDUE PICTURE  9(08).                   CI0284
            11            TO58-APYMT  PICTURE  S9(9)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ALSURR PICTURE  S9(09)V99                CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CESTP  PICTURE  X(03).                   CI0284
            11            TO58-FILLER PICTURE  X(356).                  CI0284
            10            TO58-QT5O                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-NBACT  PICTURE  S9(11)                   CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CTIAC  PICTURE  S9(3)                    CI0284
                          BINARY.                                       CI0284
            11            TO58-CASTT  PICTURE  S99                      CI0284
                          BINARY.                                       CI0284
            11            TO58-CATMI  PICTURE  S9                       CI0284
                          BINARY.                                       CI0284
            11            TO58-IATMR  PICTURE  X(3).                    CI0284
            11            TO58-IBIPI  PICTURE  X.                       CI0284
            11            TO58-CBPST  PICTURE  S99                      CI0284
                          BINARY.                                       CI0284
            11            TO58-TBPST  PICTURE  X(16).                   CI0284
            11            TO58-CODPI  PICTURE  X.                       CI0284
            11            TO58-TODPS  PICTURE  X(9).                    CI0284
            11            TO58-FILLER PICTURE  X(448).                  CI0284
            11            TO58-IBPSD  PICTURE  X.                       CI0284
            11            TO58-FILLER PICTURE  X(107).                  CI0284
            11            TO58-QT5E                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            12            TO58-MPRN4X PICTURE  X(100).                  CI0284
            12            TO58-CCMSH  PICTURE  X(2).                    CI0284
            12            TO58-CPRCS  PICTURE  X(04).                   CI0284
            12            TO58-CURST  PICTURE  X.                       CI0284
            10            TO58-QT5M                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-NAPCN1 PICTURE  X(24).                   CI0284
            11            TO58-FILLER PICTURE  X(576).                  CI0284
            10            TO58-QT5B                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-NAPCN2 PICTURE  X(24).                   CI0284
            11            TO58-CTIDAL PICTURE  X(40).                   CI0284
            11            TO58-NPHNS  PICTURE  X(14).                   CI0284
            11            TO58-FILLER PICTURE  X(522).                  CI0284
            10            TO58-QT5P                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-CFPPT  PICTURE  9(3).                    CI0284
            11            TO58-TTYPP  PICTURE  X(40).                   CI0284
            11            TO58-CPPST  PICTURE  9(3).                    CI0284
            11            TO58-TPPST  PICTURE  X(15).                   CI0284
            11            TO58-APFEEQ PICTURE  S9(7)V99.                CI0284
            11            TO58-APFEEC PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-APFEEP PICTURE  S9(7)V99.                CI0284
            11            TO58-ISVCA  PICTURE  X.                       CI0284
            11            TO58-NSBVS  PICTURE  X(5).                    CI0284
            11            TO58-ICKRV  PICTURE  X.                       CI0284
            11            TO58-PDAMT  PICTURE  S9(03).                  CI0284
            11            TO58-PSTAX  PICTURE  S9(03)V999.              CI0284
            11            TO58-DPCAL  PICTURE  9(8).                    CI0284
            11            TO58-NADVF  PICTURE  X(08).                   CI0284
            11            TO58-DAGUP  PICTURE  9(8).                    CI0284
            11            TO58-AANFEA PICTURE  9(5)V99                  CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-CLIDN7 PICTURE  9(8)                     CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-ARANV  PICTURE  S9(7)V99                 CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            TO58-DRANV  PICTURE  9(8).                    CI0284
            11            TO58-FILLER PICTURE  X(454).                  CI0284
            10            TO58-QT50                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-NANCA  PICTURE  X(30).                   CI0284
            11            TO58-MANCN  PICTURE  X(100).                  CI0284
            11            TO58-AINPTX PICTURE  X(20).                   CI0284
            11            TO58-CTID01 PICTURE  X(27).                   CI0284
            11            TO58-NANCA1 PICTURE  X(04).                   CI0284
            11            TO58-IIVAR  PICTURE  X(1).                    CI0284
            11            TO58-FILLER PICTURE  X(418).                  CI0284
            10            TO58-QT5R                                     CI0284
                          REDEFINES            TO58-FILLER.             CI0284
            11            TO58-NACTJ  PICTURE  X(04).                   CI0284
            11            TO58-NACNO6 PICTURE  X(11).                   CI0284
            11            TO58-FILLER PICTURE  X(585).                  CI0284
            10            TO58-AMAXA  PICTURE  S9(7)V99.                CI0284
            10            TO58-ISAOR  PICTURE  X.                       CI0284
            10            TO58-ISACH  PICTURE  X.                       CI0284
            10            TO58-CERRBA PICTURE  X(02).                   CI0284
            10            TO58-CERRBH PICTURE  X(02).                   CI0284
            10            TO58-IWITHH PICTURE  X.                       CI0284
            10            TO58-CTID20 PICTURE  X(27).                   CI0284
            10            TO58-GECKD3 PICTURE  9.                       CI0284
            10            TO58-DANFC  PICTURE  X(10).                   CI0284
            10            TO58-DAFCN  PICTURE  X(10).                   CI0284
            10            TO58-ISMTA  PICTURE  X.                       CI0284
            10            TO58-CERRBT PICTURE  X(02).                   CI0284
            10            TO58-NPLNI  PICTURE  X(10).                   CI0284
            10            TO58-FILLER PICTURE  X(023).                  CI0284
      *
      ******************************************************************
      **         THIS SEGMENT IS THE OUTPUT LINKAGE FOR CI0284         *
      ******************************************************************
      *
      *!WF DSP=HM DSL=QT SEL=93 FOR=I LEV=1 PLT=80
       01                 HM00.                                         CI0284
          05              HM00-SUITE.                                   CI0284
            15       FILLER         PICTURE  X(90906).                  CI0284
       01                 HM93  REDEFINES      HM00.                    CI0284
            10            HM93-QBLCK  PICTURE  9(6).                    CI0284
            10            HM93-QT9O.                                    CI0284
            11            HM93-QT9B                                     CI0284
                          OCCURS       450     TIMES.                   CI0284
            12            HM93-CHTML  PICTURE  99.                      CI0284
            12            HM93-THTML  PICTURE  X(200).                  CI0284
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0284
          05              MS00-SUITE.                                   CI0284
            15       FILLER         PICTURE  X(00542).                  CI0284
       01                 MS03  REDEFINES      MS00.                    CI0284
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            10            MS03-CMSSF  PICTURE  XX.                      CI0284
            10            MS03-DU09.                                    CI0284
            11            MS03-CMESA  PICTURE  S9(9)                    CI0284
                          BINARY.                                       CI0284
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0284
                          BINARY.                                       CI0284
            11            MS03-CMESB  PICTURE  S9(9)                    CI0284
                          BINARY.                                       CI0284
            11            MS03-CMSST  PICTURE  S9(9)                    CI0284
                          BINARY.                                       CI0284
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0284
                          BINARY.                                       CI0284
            11            MS03-QELLAA PICTURE  S9(9)                    CI0284
                          BINARY.                                       CI0284
            11            MS03-TMESS4 PICTURE  X(512).                  CI0284
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0284
            10            MX11-QMSGS  PICTURE  9(03).                   CI0284
            10            MX11-PJ09                                     CI0284
                          OCCURS       025     TIMES.                   CI0284
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0284
                          COMPUTATIONAL-3.                              CI0284
            11            MX11-CMESB  PICTURE  S9(9)                    CI0284
                          BINARY.                                       CI0284
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                V249
                                FR58
                                TO58
                                HM93
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0284
      *               *                                   *             CI0284
      *               *INITIALISATIONS                    *             CI0284
      *               *                                   *             CI0284
      *               *************************************.            CI0284
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
           HTML-FR-PRODUCT-TAGS
           HTML-FR-ACCOUNT-TAGS
           HTML-FR-EO-ACCOUNT-TAGS
           HTML-REDUCE-TAGS
           HTML-WITHHOLD-TAGS
           HTML-FEEAMT-TAGS
           HTML-SURRAMT-TAGS
           HTML-CUSTOD-TAGS
           HTML-SALELOAD-TAGS
           HTML-TO-CLIENT-TAGS
           HTML-TOCLIENT-TAGS
           HTML-TO-ACCT-HDR-TAGS
           HTML-TO-PRODUCT-TAGS
           HTML-TO-ACCOUNT-TAGS
           HTML-TO-EO-ACCOUNT-TAGS
           HTML-REQUEST-TAGS
           HTML-TO-REQUEST-TAGS
           HTML-TO-IRAT-TAGS
           HTML-MOVEMM-TAGS
           HTML-MOVED-TAGS
           HTML-IMPL-TAGS
           HTML-HREF-TAGS
           HTML-IMPLY-TAGS
           HTML-NOTES-TAGS
           HTML-MESSAGE-TAGS
           HTML-MESSAGE-ARRAY
           HTML-MESSAGE-LI
           HTML-EOM-TAGS
           HTML-STRATEGY-TAGS
           INITIALIZE  WS-NSEQA
           WS-NSEQB
           WS-MESSAGE-NBR.
       F02CA-FN. EXIT.
      *N02CB.    NOTE *---> ONLY TO ACCOUNT LINES         *.
       F02CB.                                                           lv10
           INITIALIZE  HTML-WITHHOLD-TAGS
           HTML-TO-PRODUCT-TAGS
           HTML-TO-ACCOUNT-TAGS
           HTML-TO-EO-ACCOUNT-TAGS
           HTML-TO-REQUEST-TAGS
           HTML-TO-IRAT-TAGS
           HTML-MOVEMM-TAGS
           HTML-MOVED-TAGS
           HTML-IMPL-TAGS
           HTML-HREF-TAGS.
       F02CB-FN. EXIT.
      *N02DD.    NOTE *INITIALIZE BANK LINES              *.
       F02DD.    IF    WS-FIRST-TIME                                    lv10
                 NEXT SENTENCE ELSE GO TO     F02DD-FN.
      *AND LINES FROM CI0285
           INITIALIZE  HTML-ACCOUNT-TAGS
           HTML-FR-EO-ACCOUNT-TAGS
           HTML-REQUEST-SURR-TAGS
           HTML-STATE-TAGS
           HTML-FULL-SURRAMT-TAGS
           HTML-MVA-TAGS
           HTML-DESTINATION-TAGS
           HTML-ADDRESS-TAGS
           HTML-BANK-TAGS.
       F02DD-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0284
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0284
      *               *                                   *             CI0284
      *               *FIN DE TRAITEMENT                  *             CI0284
      *               *                                   *             CI0284
      *               *************************************.            CI0284
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0284
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
      *N35DB.    NOTE *FORMAT THE MOVE MONEY TAG          *.
       F35DB.    IF    V249-CTTYPG = 'MMTA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F35DB-FN.
           MOVE        CN-MMTA TO HTML-MOVE-SEND
           MOVE        CN-VERIFY-MOVE TO HTML-VERIFY.
       F35DB-FN. EXIT.
      *N35DC.    NOTE *FORMAT THE SEND MONEY TAG          *.
       F35DC.    IF    V249-CTTYPG = 'SMTC'                             lv10
                 OR    V249-CTTYPG = 'SMDD'
                 NEXT SENTENCE ELSE GO TO     F35DC-FN.
           MOVE        CN-SMTC TO HTML-MOVE-SEND
           MOVE        CN-VERIFY-SEND TO HTML-VERIFY.
       F35DC-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *BUILD FROM ACCOUNT TAGS            *
      *               *                                   *
      *               *************************************.
       F40.      IF    WS-FIRST-TIME                                    lv05
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *N40AH.    NOTE *CHECK IF ADMIN = 004 OR 005        *.
       F40AH.    IF    FR58-CTIDA = 004 OR 005                          lv10
                 NEXT SENTENCE ELSE GO TO     F40AH-FN.
      *N40AM.    NOTE *STRING ACCOUNT NAME                *.
       F40AM.                                                           lv15
                 IF    FR58-TPLNL = SPACES                              DOT
           MOVE        FR58-PRCMN TO HTML-FR-PRCMN
                 ELSE
           MOVE        FR58-TPLNL TO HTML-FR-PRCMN.
       F40AM-FN. EXIT.
       F40AH-900. GO TO F40AZ-FN.
       F40AH-FN. EXIT.
      *N40AZ.    NOTE *ELSE, NOT ADMIN 004 OR 005         *.
       F40AZ.         EXIT.                                             lv10
      *N40BC.    NOTE *STRING ACCOUNT NAME                *.
       F40BC.                                                           lv15
           MOVE        FR58-PRCMN TO HTML-FR-PRCMN.
       F40BC-FN. EXIT.
       F40AZ-FN. EXIT.
      *N40BE.    NOTE *CHECK IF ADMIN = 004 OR 005        *.
       F40BE.    IF    FR58-CTIDA = 004 OR 005                          lv10
                 NEXT SENTENCE ELSE GO TO     F40BE-FN.
      *N40BH.    NOTE *STIRING QUALIFIED PLAN TYPE        *.
       F40BH.                                                           lv15
           MOVE        FR58-CQACTL TO HTML-CQACTL.
       F40BH-FN. EXIT.
      *N40BJ.    NOTE *BUILD OWNERSHIP LINE(S)            *.
       F40BJ.                                                           lv15
           MOVE        FR58-CTTLN1 TO HTML-CTTLN1A.
                 IF    FR58-CTTLN2 > SPACES                             DOT
           MOVE        FR58-CTTLN2 TO HTML-CTTLN2A.
                 IF    FR58-CTTLN3 > SPACES                             DOT
           MOVE        FR58-CTTLN3 TO HTML-CTTLN3A.
                 IF    FR58-CTTBO1 > SPACES                             DOT
           MOVE        FR58-CTTBO1 TO HTML-CTTBO1A.
                 IF    FR58-CTTBO2 > SPACES                             DOT
           MOVE        FR58-CTTBO2 TO HTML-CTTBO2A.
       F40BJ-FN. EXIT.
       F40BE-900. GO TO F40BP-FN.
       F40BE-FN. EXIT.
      *N40BP.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F40BP.         EXIT.                                             lv10
      *N40BS.    NOTE *BUILD OWNERSHIP LINE(S)            *.
       F40BS.                                                           lv15
           MOVE        FR58-CTTLN1 TO HTML-FR-CTTLN1.
                 IF    FR58-CTTLN2 > SPACES                             DOT
           MOVE        FR58-CTTLN2 TO HTML-FR-CTTLN2.
                 IF    FR58-CTTLN3 > SPACES                             DOT
           MOVE        FR58-CTTLN3 TO HTML-FR-CTTLN3.
                 IF    FR58-CTTBO1 > SPACES                             DOT
           MOVE        FR58-CTTBO1 TO HTML-FR-CTTBO1.
                 IF    FR58-CTTBO2 > SPACES                             DOT
           MOVE        FR58-CTTBO2 TO HTML-FR-CTTBO2.
       F40BS-FN. EXIT.
       F40BP-FN. EXIT.
      *N40CB.    NOTE *FORMAT CONTRACT ID                 *.
       F40CB.                                                           lv10
           MOVE        FR58-CTIDND TO WS00-NCTIDN
           MOVE        FR58-GECKD2 TO WS00-GECKD
           MOVE        FR58-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-FR-CTID.
       F40CB-FN. EXIT.
      *N40DB.    NOTE *CHECK IF ADMIN = 4 OR 5            *.
       F40DB.    IF    FR58-CTIDA = 004 OR 005                          lv10
                 NEXT SENTENCE ELSE GO TO     F40DB-FN.
      *N40DF.    NOTE *FORMAT REQUESTED AMOUNT            *.
       F40DF.                                                           lv15
           MOVE        V249-ADBRQ TO HTML-ADBRQ
      *TYPE OF REDEMPTION =
      *FULL SURRENDER
           MOVE        CN-FULL-SURR TO HTML-TTDF6.
                 IF    V249-CTTYPG = 'MMTA'                             DOT
           MOVE        V249-ADBRQ TO HTML-AEDRQ.
       F40DF-FN. EXIT.
       F40DB-900. GO TO F40DJ-FN.
       F40DB-FN. EXIT.
      *N40DJ.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F40DJ.         EXIT.                                             lv10
      *N40EB.    NOTE *FORMAT REQUESTED AMOUNT            *.
       F40EB.                                                           lv15
           EVALUATE    V249-CEXTP
                 WHEN  'D'
           MOVE        V249-ADBRQ TO WS-ADBRQ2
           MOVE        WS-AMOUNT TO HTML-FR-AMOUNT
                 WHEN  'P'
           MOVE        V249-PACT1 TO WS-PACT1
           MOVE        WS-PERCENT TO HTML-FR-AMOUNT
                 WHEN  'S'
           MOVE        V249-QSHOWQ TO WS-QSHOWQ
           MOVE        WS-SHARES TO HTML-FR-AMOUNT
           END-EVALUATE.
       F40EB-FN. EXIT.
      *N40EC.    NOTE *FORMAT REDUCTION AMOUNT            *.
       F40EC.                                                           lv15
                 IF    V249-CTTYPG = 'SMTC'                             DOT
                 OR    V249-CTTYPG = 'SMDD'
           MOVE        V249-AEDRQ TO HTML-AEDRQ.
                 IF    V249-CTTYPG = 'MMTA'                             DOT
           MOVE        V249-ADBRQ TO HTML-AEDRQ.
       F40EC-FN. EXIT.
      *N40NC.    NOTE *FORMAT TRAN TYPE - CERTS           *.
       F40NC.    IF    FR58-CTIDA = '001'                               lv15
                 NEXT SENTENCE ELSE GO TO     F40NC-FN.
      *********************************
      ** SET TYPE OF REDEMPTION CODE  *
      ** FOR A CERTIFICATE            *
      *********************************
      *
                 IF    V249-CPORTA = 'F'                                DOT
           SET CN-002-FULL        TO TRUE.
                 IF    V249-CPORTA = 'S' OR 'P'                         DOT
           SET CN-001-SPECIFIED   TO TRUE.
                 IF    V249-CPORTA = 'M'                                DOT
           SET CN-001-MAX-PARTIAL TO TRUE.
                 IF    V249-CPORTA = 'G'                                DOT
           SET CN-001-GROSSED-UP  TO TRUE.
                 IF    V249-CPORTA = 'R'                                DOT
           SET CN-001-RMD         TO TRUE.
       F40NC-FN. EXIT.
      *N40NF.    NOTE *FORMAT TRAN TYPE - FUNDS           *.
       F40NF.    IF    FR58-CTIDA = '002'                               lv15
                 NEXT SENTENCE ELSE GO TO     F40NF-FN.
      *********************************
      ** SET TYPE OF REDEMPTION CODE  *
      ** FOR A MUTUAL FUND            *
      *********************************
      *
                 IF    V249-CPORTA = 'F'                                DOT
           SET CN-001-FULL        TO TRUE.
                 IF    V249-CPORTA = 'S' OR 'P'                         DOT
           SET CN-002-SPECIFIED   TO TRUE.
                 IF    V249-CPORTA = 'G'                                DOT
           SET CN-002-GROSSED-UP  TO TRUE.
                 IF    V249-CPORTA = 'R'                                DOT
           SET CN-002-RMD         TO TRUE.
                 IF    V249-CPORTA = 'Q'                                DOT
           SET CN-002-FULL-RMD    TO TRUE.
       F40NF-FN. EXIT.
      *N40NS.    NOTE *FORMAT TRAN TYPE - BROKERAGE       *.
       F40NS.    IF    FR58-CTIDA = '021'                               lv15
                 NEXT SENTENCE ELSE GO TO     F40NS-FN.
      *********************************
      ** SET TYPE OF REDEMPTION CODE  *
      ** FOR BROKERAGE ACCOUNT        *
      *********************************
      *
                 IF    V249-CPORTA = 'F'                                DOT
           SET CN-021-FULL        TO TRUE.
                 IF    V249-CPORTA = 'S' OR 'P'                         DOT
           SET CN-021-SPECIFIED   TO TRUE.
       F40NS-FN. EXIT.
       F40DJ-FN. EXIT.
      *N40NX.    NOTE *MOVE DERIVED VALUE                 *.
       F40NX.                                                           lv10
      *
           MOVE        CN-TTDF6 TO HTML-TTDF6.
       F40NX-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *BUILD PAYMENT DETAIL               *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      *
      *N45AN.    NOTE *SEND MONEY TO CLIENT               *.
       F45AN.    IF    (V249-CTTYPG = 'SMTC'                            lv10
                 OR    V249-CTTYPG = 'SMDD')
                 AND   V249-CAACT = 'S'
                 NEXT SENTENCE ELSE GO TO     F45AN-FN.
      *N45BA.    NOTE *CHECK IF ADMIN = 4 OR 5            *.
       F45BA.    IF    FR58-CTIDA = 004 OR 005                          lv15
                 NEXT SENTENCE ELSE GO TO     F45BA-FN.
      *N45BD.    NOTE *FORMAT FEDERAL AMOUNTS             *.
       F45BD.    IF    V249-ATWHDD NOT = 0                              lv20
                 NEXT SENTENCE ELSE GO TO     F45BD-FN.
           COMPUTE     HTML-AWITH = 0 - V249-ATWHDD
           MOVE        V249-PWHLD TO HTML-PWHLD
           MOVE        CN-FED-WITH TO HTML-FED-WITH.
       F45BD-FN. EXIT.
      *N45BJ.    NOTE *FORMAT STATE WITHHOLDING AMTS      *.
       F45BJ.    IF    V249-ATWHDE NOT = 0                              lv20
                 NEXT SENTENCE ELSE GO TO     F45BJ-FN.
           COMPUTE     HTML-ATWHDE = 0 - V249-ATWHDE
           MOVE        V249-CTWHPB TO HTML-PWHLDF
           MOVE        CN-STATE-WITH TO HTML-STATE.
       F45BJ-FN. EXIT.
       F45BA-900. GO TO F45CD-FN.
       F45BA-FN. EXIT.
      *N45CD.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F45CD.         EXIT.                                             lv15
      *N45DE.    NOTE *CALCULATE WITHHOLDING AMOUNT       *.
       F45DE.                                                           lv20
           INITIALIZE  WS-AWITH
           COMPUTE     WS-AWITH ROUNDED = V249-ADBRQ
           * (V249-PWHLD
           / 100).
                 IF    V249-CPORTA = 'G'                                DOT
           COMPUTE     WS-AWITH ROUNDED = V249-AEDRQ
           * (V249-PWHLD
           / 100).
       F45DE-FN. EXIT.
      *N45DG.    NOTE *SET WITHHOLDING AMOUNT TAGS        *.
       F45DG.    IF    WS-AWITH = 0                                     lv20
                 AND   FR58-CQACT > 000
                 NEXT SENTENCE ELSE GO TO     F45DG-FN.
           MOVE        WS-AWITH TO HTML-AWITH
           MOVE        00 TO HTML-PWHLD
           MOVE        CN-FED-WITH TO HTML-FED-WITH.
       F45DG-FN. EXIT.
      *N45DM.    NOTE *SET WITHHOLDING AMOUNT TAGS        *.
       F45DM.    IF    WS-AWITH > 0                                     lv20
                 NEXT SENTENCE ELSE GO TO     F45DM-FN.
           MOVE        WS-AWITH TO HTML-AWITH
           MOVE        V249-PWHLD TO HTML-PWHLD
           MOVE        CN-FED-WITH TO HTML-FED-WITH.
       F45DM-FN. EXIT.
       F45CD-FN. EXIT.
      *N45DS.    NOTE *CHECK IF ADMIN 4 OR 5              *.
       F45DS.    IF    FR58-CTIDA = 4 OR 5                              lv15
                 NEXT SENTENCE ELSE GO TO     F45DS-FN.
      *N45DW.    NOTE *CUSTODIAL FEE & TERMINATION FEE    *.
       F45DW.                                                           lv20
      *IS NOT APPLICABLE FOR ANNUITY
           MOVE        ZERO TO FR58-AFEET
           FR58-ATERF.
       F45DW-FN. EXIT.
       F45DS-900. GO TO F45ED-FN.
       F45DS-FN. EXIT.
      *N45ED.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F45ED.         EXIT.                                             lv15
      *N45EF.    NOTE *MOVE STRATEGY NAME                 *.
       F45EF.    IF    V249-MPRN4 > SPACES                              lv20
                 NEXT SENTENCE ELSE GO TO     F45EF-FN.
           MOVE        V249-MPRN4 TO HTML-MPRN.
       F45EF-FN. EXIT.
      *N45EG.    NOTE *SET FEE AMOUNT TAGS                *.
       F45EG.    IF    FR58-AFEET > 0                                   lv20
                 NEXT SENTENCE ELSE GO TO     F45EG-FN.
           MOVE        FR58-AFEET TO HTML-AFEET
           MOVE        CN-CUSTOD-FEE TO HTML-FEEAMT.
       F45EG-FN. EXIT.
      *N45EK.    NOTE *SET CUSTODIAL FEE TAGS             *.
       F45EK.    IF    FR58-ATERF > 0                                   lv20
                 NEXT SENTENCE ELSE GO TO     F45EK-FN.
           MOVE        FR58-ATERF TO HTML-ATERF
           MOVE        CN-TERMINATION-FEE TO HTML-CUSTOD.
       F45EK-FN. EXIT.
       F45ED-FN. EXIT.
      *N45EL.    NOTE *CHECK ADMIN 4 OR 5                 *.
       F45EL.    IF    FR58-CTIDA = 004 OR 005                          lv15
                 NEXT SENTENCE ELSE GO TO     F45EL-FN.
      *N45EP.    NOTE *SET SURRENDER AMOUNT TAGS          *.
       F45EP.    IF    FR58-ASURRW NOT = 0                              lv20
                 OR    FR58-ACTCH NOT = 0
                 NEXT SENTENCE ELSE GO TO     F45EP-FN.
           MOVE        CN-SURR-CHG TO HTML-FULL-SURRCHG
           COMPUTE     HTML-FULL-ASURR = 0 -
           (FR58-ASURRW).
       F45EP-FN. EXIT.
      *N45EU.    NOTE *SET CONTRACT CHARGE  TAGS          *.
       F45EU.    IF    FR58-ACTCH NOT = 0                               lv20
                 NEXT SENTENCE ELSE GO TO     F45EU-FN.
           MOVE        CN-CONTRACT TO HTML-CONTRACT
           COMPUTE     WS-ACTCH-AMT = 0 - FR58-ACTCH
           MOVE        WS-ACTCH TO HTML-ACTCH.
       F45EU-FN. EXIT.
       F45EL-900. GO TO F45FA-FN.
       F45EL-FN. EXIT.
      *N45FA.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F45FA.         EXIT.                                             lv15
      *N45FF.    NOTE *SET SURRENDER AMOUNT TAGS          *.
       F45FF.    IF    FR58-ASURR > 0                                   lv20
                 NEXT SENTENCE ELSE GO TO     F45FF-FN.
           MOVE        FR58-ASURR TO HTML-ASURR.
                 IF    FR58-CTIDA = 001                                 DOT
                 AND   V249-CACTS = 'E'
      *IF THE SOURCE IS CERTIFICATES
      *AND FOR VERIFY MODE
           MOVE        CN-APROX-SURR-AMT TO HTML-SURRAMT
                 ELSE
      *FOR OTHER PRODUCTS
           MOVE        CN-SURR-AMT TO HTML-SURRAMT.
       F45FF-FN. EXIT.
       F45FA-FN. EXIT.
      *N45FH.    NOTE *CHECK ADMIN 4 OR 5                 *.
       F45FH.    IF    FR58-CTIDA = 4 OR 5                              lv15
                 NEXT SENTENCE ELSE GO TO     F45FH-FN.
      *N45FK.    NOTE *SET SALES LOAD TAGS                *.
       F45FK.    IF    FR58-ALOAD NOT = 0                               lv20
                 NEXT SENTENCE ELSE GO TO     F45FK-FN.
           COMPUTE     HTML-ALOAD = 0 - FR58-ALOAD
           MOVE        CN-SALELOAD TO HTML-SALELOAD.
       F45FK-FN. EXIT.
       F45FH-900. GO TO F45FM-FN.
       F45FH-FN. EXIT.
      *N45FM.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F45FM.         EXIT.                                             lv15
      *N45FQ.    NOTE *SET SALES LOAD TAGS                *.
       F45FQ.    IF    FR58-ALOAD > 0                                   lv20
                 NEXT SENTENCE ELSE GO TO     F45FQ-FN.
           MOVE        FR58-ALOAD TO HTML-ALOAD
           MOVE        CN-SALELOAD TO HTML-SALELOAD.
       F45FQ-FN. EXIT.
       F45FM-FN. EXIT.
      *N45FS.    NOTE *CHECK IF ADMIN 4 OR 5              *.
       F45FS.    IF    FR58-CTIDA = 004 OR 005                          lv15
                 NEXT SENTENCE ELSE GO TO     F45FS-FN.
      *N45FZ.    NOTE *SET MARKET VALUE ADJUSTMENT        *.
       F45FZ.    IF    FR58-AMVA1 NOT = 0                               lv20
                 NEXT SENTENCE ELSE GO TO     F45FZ-FN.
           MOVE        FR58-AMVA1 TO HTML-AMVA1
           MOVE        CN-MVA TO HTML-MVA.
       F45FZ-FN. EXIT.
      *N45HA.    NOTE *AMOUNT OF CHECK                    *.
       F45HA.                                                           lv20
      *********************************
      ** SET AMOUNT TO SEND TO CLIENT *
      *********************************
           MOVE        V249-ACOTD TO HTML-ACOTD.
                 IF    V249-TDTXTA NOT = SPACES                         DOT
           MOVE        V249-TDTXTA TO HTML-TDTXTA.
       F45HA-FN. EXIT.
       F45FS-900. GO TO F45HS-FN.
       F45FS-FN. EXIT.
      *N45HS.    NOTE *NOT ADMIN 4 OR 5                   *.
       F45HS.         EXIT.                                             lv15
      *N45IC.    NOTE *AMOUNT OF CHECK                    *.
       F45IC.                                                           lv20
      *********************************
      ** SET AMOUNT TO SEND TO CLIENT *
      *********************************
           MOVE        V249-ADBRQ1 TO HTML-ACHK.
       F45IC-FN. EXIT.
       F45HS-FN. EXIT.
       F45AN-FN. EXIT.
      *N45KC.    NOTE *SMTC USE ADDRESS FROM FR58         *.
       F45KC.    IF    V249-CTTYPG = 'SMTC'                             lv10
                 NEXT SENTENCE ELSE GO TO     F45KC-FN.
      *********************************
      ** SET ADDRESS LINES            *
      *********************************
                 IF    FR58-GESAD1 NOT = SPACES                         DOT
           MOVE        FR58-GESAD1 TO HTML-GESAD1
           MOVE        CN-ABREAK TO HTML-GESADB1.
                 IF    FR58-GESAD2 NOT = SPACES                         DOT
           MOVE        FR58-GESAD2 TO HTML-GESAD2
           MOVE        CN-ABREAK TO HTML-GESADB2.
                 IF    FR58-GESAD3 NOT = SPACES                         DOT
           MOVE        FR58-GESAD3 TO HTML-GESAD3
           MOVE        CN-ABREAK TO HTML-GESADB3.
           MOVE        FR58-GECIT TO HTML-GECIT                         DOT
           MOVE        FR58-GEST TO HTML-GEST
           MOVE        FR58-GECTRY TO HTML-GECTRY
           MOVE        FR58-GEPCD TO HTML-GEPCD.
       F45KC-FN. EXIT.
      *N45KI.    NOTE *CHECK TO ADDRESS                   *.
       F45KI.    IF    V249-CTTYPG = 'SMTC'                             lv10
                 NEXT SENTENCE ELSE GO TO     F45KI-FN.
      *N45KK.    NOTE *MAIL CODE                          *.
       F45KK.    IF    V249-CDELIX > 0                                  lv15
                 NEXT SENTENCE ELSE GO TO     F45KK-FN.
      *********************************
      ** SET DELIVERY INSTRUCTION TYPE*
      ** TO MAIL FOR SEND MONEY TRAN  *
      *********************************
           MOVE        CN-MAIL TO HTML-TDELI.
       F45KK-FN. EXIT.
       F45KI-FN. EXIT.
      *N45KM.    NOTE *SMDD DIRECT DEPOSIT                *.
       F45KM.    IF    V249-CTTYPG = 'SMDD'                             lv10
                 NEXT SENTENCE ELSE GO TO     F45KM-FN.
      *SET BANK LINES
           MOVE        V249-GENAL1 TO HTML-GENAL1
           MOVE        CN-ABREAK TO HTML-GESADB11
           MOVE        CN-ROUTING TO HTML-ROUTING
           MOVE        CN-DASH TO HTML-DASH
           MOVE        V249-GECKD TO HTML-GECKD
           MOVE        V249-NTR TO HTML-NTR
           MOVE        CN-ABREAK TO HTML-GESADB12
           MOVE        CN-SIGNEE TO HTML-SIGNEE
           MOVE        V249-GENAL2 TO HTML-GENAL2
           MOVE        CN-ABREAK TO HTML-GESADB13
           MOVE        CN-BANK-ACCT TO HTML-BANK-ACCT
           MOVE        V249-NPBN TO HTML-NPBN
           MOVE        CN-ABREAK TO HTML-GESADB14
           MOVE        CN-BANK-ACCT-TYPE TO
           HTML-ACCT-TYPE.
                 IF    V249-CCBAT = 01                                  DOT
           MOVE        CN-CHECKING TO HTML-CHK-SAV.
                 IF    V249-CCBAT = 02                                  DOT
           MOVE        CN-SAVINGS TO HTML-CHK-SAV.
           MOVE        CN-ABREAK TO HTML-GESADB15                       DOT
           MOVE        CN-DIRECT-DEPOSIT TO
           HTML-BANK-TDELI.
       F45KM-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *BUILD TO ACCOUNT                   *
      *               *                                   *
      *               *************************************.
       F50.      IF    V249-CTTYPG = 'MMTA'                             lv05
                 AND   V249-CAACT = 'S'
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50BC.    NOTE *STRING ACCOUNT NAME                *.
       F50BC.                                                           lv10
           ADD         +1 TO WS-NSEQB
           MOVE        WS-NSEQB TO HTML-TO-NSEQB
           MOVE        TO58-PRCMN TO HTML-TO-PRCMN.
       F50BC-FN. EXIT.
      *N50BG.    NOTE *BUILD OWNERSHIP LINE #1            *.
       F50BG.                                                           lv10
           MOVE        TO58-CTTLN1 TO HTML-TO-CTTLN1.
                 IF    TO58-CTTLN2 > SPACES                             DOT
           MOVE        TO58-CTTLN2 TO HTML-TO-CTTLN2.
                 IF    TO58-CTTLN3 > SPACES                             DOT
           MOVE        TO58-CTTLN3 TO HTML-TO-CTTLN3.
                 IF    TO58-CTTBO1 > SPACES                             DOT
           MOVE        TO58-CTTBO1 TO HTML-TO-CTTBO1.
                 IF    TO58-CTTBO2 > SPACES                             DOT
           MOVE        TO58-CTTBO2 TO HTML-TO-CTTBO2.
       F50BG-FN. EXIT.
      *N50CB.    NOTE *FORMAT CONTRACT ID                 *.
       F50CB.                                                           lv10
           MOVE        TO58-CTIDND TO WS00-NCTIDN
           MOVE        TO58-GECKD2 TO WS00-GECKD
           MOVE        TO58-CTIDA TO WS00-CTIDA
           MOVE        WS00-CTID TO HTML-TO-CTID.
       F50CB-FN. EXIT.
      *N50CF.    NOTE *CHECK IF ADMIN = 4 OR 5            *.
       F50CF.    IF    FR58-CTIDA = 004 OR 005                          lv10
                 NEXT SENTENCE ELSE GO TO     F50CF-FN.
      *N50CH.    NOTE *FORMAT FEDERAL AMOUNTS             *.
       F50CH.    IF    V249-ATWHDD NOT = 0                              lv15
                 AND   V249-ITRNB = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50CH-FN.
           COMPUTE     HTML-AWITH = 0 - V249-ATWHDD
           MOVE        V249-PWHLD TO HTML-PWHLD
           MOVE        CN-FED-WITH TO HTML-FED-WITH.
       F50CH-FN. EXIT.
      *N50CJ.    NOTE *FORMAT STATE WITHHOLDING AMTS      *.
       F50CJ.    IF    V249-ATWHDE NOT = 0                              lv15
                 AND   V249-ITRNB = 'Y'
                 NEXT SENTENCE ELSE GO TO     F50CJ-FN.
           COMPUTE     HTML-ATWHDE = 0 - V249-ATWHDE
           MOVE        V249-CTWHPB TO HTML-PWHLDF
           MOVE        CN-STATE-WITH TO HTML-STATE.
       F50CJ-FN. EXIT.
       F50CF-900. GO TO F50EA-FN.
       F50CF-FN. EXIT.
      *N50EA.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F50EA.         EXIT.                                             lv10
      *N50EE.    NOTE *CALCULATE WITHHOLDING AMOUNT       *.
       F50EE.                                                           lv15
           INITIALIZE  WS-AWITH
           COMPUTE     WS-AWITH ROUNDED = V249-ADBRQ2
           * (V249-PWHLD
           / 100).
      *N50EF.    NOTE *SET WITHHOLDING AMOUNT TAGS        *.
       F50EF.    IF    WS-AWITH = 0                                     lv20
                 AND   TO58-CQACT = 000
                 AND   FR58-CQACT > 000
                 NEXT SENTENCE ELSE GO TO     F50EF-FN.
           MOVE        WS-AWITH TO HTML-AWITH
           MOVE        00 TO HTML-PWHLD
           MOVE        CN-FED-WITH TO HTML-FED-WITH.
       F50EF-FN. EXIT.
       F50EE-FN. EXIT.
      *N50EG.    NOTE *SET WITHHOLDING AMOUNT TAGS        *.
       F50EG.    IF    WS-AWITH > 0                                     lv15
                 NEXT SENTENCE ELSE GO TO     F50EG-FN.
           MOVE        WS-AWITH TO HTML-AWITH
           MOVE        V249-PWHLD TO HTML-PWHLD
           MOVE        CN-FED-WITH TO HTML-FED-WITH.
       F50EG-FN. EXIT.
       F50EA-FN. EXIT.
      *N50EH.    NOTE *CHECK IF ADMIN 4 OR 5              *.
       F50EH.    IF    FR58-CTIDA = 4 OR 5                              lv10
                 NEXT SENTENCE ELSE GO TO     F50EH-FN.
      *CUSTODIAL FEE & TERMINATION FEE
      *IS NOT APPLICABLE FOR ANNUITY
           MOVE        ZERO TO FR58-AFEET
           FR58-ATERF.
       F50EH-900. GO TO F50EI-FN.
       F50EH-FN. EXIT.
      *N50EI.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F50EI.         EXIT.                                             lv10
      *N50EJ.    NOTE *SET FEE AMOUNT TAGS                *.
       F50EJ.    IF    FR58-AFEET > 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50EJ-FN.
           MOVE        FR58-AFEET TO HTML-AFEET
           MOVE        CN-FEE-AMT TO HTML-FEEAMT.
       F50EJ-FN. EXIT.
      *N50EK.    NOTE *SET CUSTODIAL FEE TAGS             *.
       F50EK.    IF    FR58-ATERF > 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50EK-FN.
           MOVE        FR58-ATERF TO HTML-ATERF
           MOVE        CN-CUSTOD-FEE TO HTML-CUSTOD.
       F50EK-FN. EXIT.
       F50EI-FN. EXIT.
      *N50EL.    NOTE *CHECK ADMIN 4 OR 5                 *.
       F50EL.    IF    FR58-CTIDA = 004 OR 005                          lv10
                 NEXT SENTENCE ELSE GO TO     F50EL-FN.
                 IF    FR58-ASURRW NOT = 0                              DOT
                 AND   V249-ITRNB = 'Y'
      *SET SURRENDER AMOUNT TAGS
      *ONLY FOR THE B.O.T ACCOUNT
           MOVE        CN-SURR-AMT TO HTML-SURRAMT
           COMPUTE     HTML-ASURR = 0 - FR58-ASURRW.
                 IF    FR58-ACTCH NOT = 0                               DOT
                 AND   V249-ITRNB = 'Y'
      *SET CONTRACT CHARGE  TAGS
           MOVE        CN-CONTRACT TO WS-CONTRACT
           COMPUTE     WS-ACTCH-AMT = 0 - FR58-ACTCH
           MOVE        WS-ACTCH TO WS-ACTCH2
           MOVE        WS-CHARGE TO HTML-CHARGE.
                 IF    FR58-AMVA1 NOT = 0                               DOT
                 AND   V249-ITRNB = 'Y'
      *SET MARKET VALUE ADJUSTMENT
           MOVE        FR58-AMVA1 TO HTML-AMVA1
           MOVE        CN-MVA TO HTML-MVA.
                 IF    V249-TDTXTA NOT = SPACES                         DOT
                 AND   V249-ITRNB = 'Y'
      *SET WITHOLDING REF MESSAGE
           MOVE        V249-TDTXTA TO HTML-TDTXTA2.
       F50EL-900. GO TO F50EM-FN.
       F50EL-FN. EXIT.
      *N50EM.    NOTE *ELSE, NOT ADMIN 4 OR 5             *.
       F50EM.         EXIT.                                             lv10
      *N50EN.    NOTE *SET SURRENDER AMOUNT TAGS          *.
       F50EN.    IF    FR58-ASURR > 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50EN-FN.
           MOVE        FR58-ASURR TO HTML-ASURR.
                 IF    FR58-CTIDA = 001                                 DOT
                 AND   V249-CACTS = 'E'
      *IF THE SOURCE IS CERTIFICATES
      *AND FOR VERIFY MODE
           MOVE        CN-APROX-SURR-AMT TO HTML-SURRAMT
                 ELSE
      *FOR OTHER PRODUCTS
           MOVE        CN-SURR-AMT TO HTML-SURRAMT.
       F50EN-FN. EXIT.
      *N50EO.    NOTE *SET SALES LOAD TAGS                *.
       F50EO.    IF    FR58-ALOAD > 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F50EO-FN.
           MOVE        FR58-ALOAD TO HTML-ALOAD
           MOVE        CN-SALELOAD TO HTML-SALELOAD.
       F50EO-FN. EXIT.
       F50EM-FN. EXIT.
      *N50GB.    NOTE *REQUESTED AMOUNT                   *.
       F50GB.                                                           lv10
           MOVE        V249-ADBRQ2 TO HTML-TO-ADBRQ2.
       F50GB-FN. EXIT.
      *N50GD.    NOTE *CHECK BALANCE OF TRANSFER          *.
       F50GD.    IF    V249-ITRNB = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50GD-FN.
           MOVE        CN-BALANCE TO HTML-TO-AMOUNT
           MOVE        CN-ASTERIKS TO HTML-TO-ASTERK.
                 IF    FR58-CTIDA NOT = '021'                           DOT
           MOVE        CN-BACKEND TO HTML-BACKEND.
       F50GD-900. GO TO F50GF-FN.
       F50GD-FN. EXIT.
      *N50GF.    NOTE *CHECK SHARE AMOUNT                 *.
       F50GF.                                                           lv10
                 IF    V249-CEXTP1 = 'S'                                DOT
           MOVE        V249-QSHO5A TO WS-TO-QSHOWQ
           MOVE        WS-TO-SHARES TO HTML-TO-AMOUNT.
       F50GF-FN. EXIT.
      *N50GM.    NOTE *AMOUNT MOVED                       *.
       F50GM.                                                           lv10
           MOVE        V249-ADBRQ1 TO HTML-TO-ADBRQ1.
       F50GM-FN. EXIT.
      *N50IB.    NOTE *SET IRA CONTRIBUTION TYPE          *.
       F50IB.    IF    V249-CIRAP NOT = SPACES                          lv10
                 NEXT SENTENCE ELSE GO TO     F50IB-FN.
           EVALUATE    V249-CIRAP
                 WHEN  'CU'
           SET CN-CIRAP-CU TO TRUE
                 WHEN  'PR'
           SET CN-CIRAP-PR TO TRUE
                 WHEN  'IT'
           SET CN-CIRAP-IT TO TRUE
                 WHEN  'RO'
           SET CN-CIRAP-RO TO TRUE
                 WHEN  'SC'
           SET CN-CIRAP-SC TO TRUE
                 WHEN  'SP'
           SET CN-CIRAP-SP TO TRUE
           END-EVALUATE.
      *N50IE.    NOTE *MOVE IRA CONTRIBUTION TYPE         *.
       F50IE.                                                           lv15
           MOVE        CN-MIRAP TO HTML-TO-MIRAP.
       F50IE-FN. EXIT.
       F50IB-FN. EXIT.
      *N50KB.    NOTE *SET IMPLICATION MESSAGE TAGS       *.
       F50KB.    IF    V249-XZ30 NOT = ALL '0'                          lv10
                 NEXT SENTENCE ELSE GO TO     F50KB-FN.
      *N50KC.    NOTE *MOVE TO ARRAY                      *.
       F50KC.                                                           lv15
           MOVE        1 TO WS-HREF-PTR
           MOVE        V249-XZ30 TO WS-XZ30
           MOVE        ' ' TO WS-ACOMMA.
       F50KC-FN. EXIT.
      *N50KE.    NOTE *LOOP THRU ARRAY                    *.
       F50KE.                                                           lv15
           MOVE        1                        TO J50KER
                                    GO TO     F50KE-B.
       F50KE-A.
           ADD         1                        TO J50KER.
       F50KE-B.
           IF          J50KER                   >  30
                                    GO TO     F50KE-FN.
      *N50KG.    NOTE *STRING HREF TAGS                   *.
       F50KG.    IF    WS-XZ1 (J50KER) = '1'                            lv20
                 NEXT SENTENCE ELSE GO TO     F50KG-FN.
      *N50KI.    NOTE *STRING HREF TAGS - 1 CHAR          *.
       F50KI.    IF    J50KER < 10                                      lv25
                 NEXT SENTENCE ELSE GO TO     F50KI-FN.
           MOVE        J50KER TO WS-A-9
           STRING      WS-ACOMMA
           WS-A-HREF
           WS-A-9
           WS-ACARAT
           WS-A-9
           WS-EO-HREF DELIMITED BY SIZE
           INTO HTML-HREF
           WITH POINTER WS-HREF-PTR
           MOVE        ',' TO WS-ACOMMA.
       F50KI-900. GO TO F50KM-FN.
       F50KI-FN. EXIT.
      *N50KM.    NOTE *STRING HREF TAGS - 2 CHAR          *.
       F50KM.                                                           lv25
           MOVE        J50KER TO WS-A-99
           STRING      WS-ACOMMA
           WS-A-HREF
           WS-A-99
           WS-ACARAT
           WS-A-99
           WS-EO-HREF DELIMITED BY SIZE
           INTO HTML-HREF
           WITH POINTER WS-HREF-PTR
           MOVE        ',' TO WS-ACOMMA.
       F50KM-FN. EXIT.
       F50KG-FN. EXIT.
       F50KE-900. GO TO F50KE-A.
       F50KE-FN. EXIT.
       F50KB-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *BUILD MESSAGE INFO                 *
      *               *                                   *
      *               *************************************.
       F55.      IF    V249-CAACT = 'M'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F55-FN.
      *N55BC.    NOTE *MOVE MESSAGE TEXT                  *.
       F55BC.    IF    IHTMLL < IHTMLM                                  lv10
                 NEXT SENTENCE ELSE GO TO     F55BC-FN.
           ADD         +1 TO IHTMLL.
      *N55BE.    NOTE *SET XREF-NAME                      *.
       F55BE.                                                           lv15
                 IF    IHTMLL < 10                                      DOT
           MOVE        IHTMLL TO WS-M-9
                 ELSE
           MOVE        IHTMLL TO WS-M-99.
           MOVE        WS-MESSAGE-NBR TO HTML-MESSAGE-NBR.              DOT
       F55BE-FN. EXIT.
      *N55EC.    NOTE *MOVE / SEND MONEY MESSAGES         *.
       F55EC.                                                           lv15
           MOVE        V249-TDTXT1 TO HTML-TDTXT1.
       F55EC-FN. EXIT.
      *N55MC.    NOTE *MOVE MESSAGE BLOCK                 *.
       F55MC.                                                           lv15
           MOVE        HTML-MESSAGE-LI TO
           HTML-MESSAGE-TBLCK (IHTMLL).
       F55MC-FN. EXIT.
       F55BC-FN. EXIT.
       F55-FN.   EXIT.
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
      *N60CC.    NOTE *VERIFY TAGS                        *.
       F60CC.    IF    V249-CACTS = 'E'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F60CC-FN.
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
       F60CC-FN. EXIT.
      *N60CK.    NOTE *FROM PRODUCT DETAILS               *.
       F60CK.                                                           lv10
           MOVE        1                        TO J60CKR
                                    GO TO     F60CK-B.
       F60CK-A.
           ADD         1                        TO J60CKR.
       F60CK-B.
           IF          J60CKR                   >  HTML-FR-PROD-CTR
                                    GO TO     F60CK-FN.
      *N60CL.    NOTE *SET PRODUCT LINES                  *.
       F60CL.                                                           lv15
           ADD         1 TO HTML-PT
           MOVE        HTML-FR-PROD-LINE (J60CKR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60CL-FN. EXIT.
       F60CK-900. GO TO F60CK-A.
       F60CK-FN. EXIT.
      *N60DA.    NOTE *CHECK IF ADMIN = 4 OR 5            *.
       F60DA.    IF    FR58-CTIDA = 004 OR 005                          lv10
                 NEXT SENTENCE ELSE GO TO     F60DA-FN.
      *N60DF.    NOTE *MOVE MONEY FROM THIS ACCOUNT       *.
       F60DF.                                                           lv15
           MOVE        1                        TO J60DFR
                                    GO TO     F60DF-B.
       F60DF-A.
           ADD         1                        TO J60DFR.
       F60DF-B.
           IF          J60DFR                   >  HTML-ACCOUNT-CTR
                                    GO TO     F60DF-FN.
      *N60DI.    NOTE *SET ACCOUNT LINES                  *.
       F60DI.    IF    HTML-ACCOUNT-LINE (J60DFR)                       lv20
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F60DI-FN.
           ADD         1 TO HTML-PT
           INITIALIZE  HM93-THTML (HTML-PT)
           STRING      HTML-ACCOUNT-LINE (J60DFR)
           CN-ABREAK DELIMITED BY SIZE
           INTO HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60DI-FN. EXIT.
       F60DF-900. GO TO F60DF-A.
       F60DF-FN. EXIT.
      *N60DN.    NOTE *END OF ACCOUNT LINES               *.
       F60DN.                                                           lv15
           MOVE        1                        TO J60DNR
                                    GO TO     F60DN-B.
       F60DN-A.
           ADD         1                        TO J60DNR.
       F60DN-B.
           IF          J60DNR                   >  HTML-EO-FR-CTR
                                    GO TO     F60DN-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-FR-LINE (J60DNR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60DN-900. GO TO F60DN-A.
       F60DN-FN. EXIT.
      *N60DT.    NOTE *AMOUNT REQUESTED                   *.
       F60DT.                                                           lv15
           MOVE        1                        TO J60DTR
                                    GO TO     F60DT-B.
       F60DT-A.
           ADD         1                        TO J60DTR.
       F60DT-B.
           IF          J60DTR                   >  HTML-REQUEST-SURR-CTR
                                    GO TO     F60DT-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REQUEST-SURR-LINE (J60DTR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60DT-900. GO TO F60DT-A.
       F60DT-FN. EXIT.
      *N60DU.    NOTE *MOVE ACCOUNT REDUCTION LINES       *.
       F60DU.    IF    V249-CTTYPG = 'MMTA'                             lv15
                 NEXT SENTENCE ELSE GO TO     F60DU-FN.
      *N60DV.    NOTE *MOVE ACCOUNT REDUCTION LINES       *.
       F60DV.                                                           lv20
           MOVE        1                        TO J60DVR
                                    GO TO     F60DV-B.
       F60DV-A.
           ADD         1                        TO J60DVR.
       F60DV-B.
           IF          J60DVR                   >  HTML-REDUCE-CTR
                                    GO TO     F60DV-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REDUCE-LINE (J60DVR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60DV-900. GO TO F60DV-A.
       F60DV-FN. EXIT.
       F60DU-FN. EXIT.
       F60DA-900. GO TO F60EA-FN.
       F60DA-FN. EXIT.
      *N60EA.    NOTE *ELSE, NOT ADMIN 4 AND 5            *.
       F60EA.         EXIT.                                             lv10
      *N60EC.    NOTE *MOVE MONEY FROM THIS ACCOUNT       *.
       F60EC.                                                           lv15
           MOVE        1                        TO J60ECR
                                    GO TO     F60EC-B.
       F60EC-A.
           ADD         1                        TO J60ECR.
       F60EC-B.
           IF          J60ECR                   >  HTML-FR-ACCT-CTR
                                    GO TO     F60EC-FN.
      *N60EE.    NOTE *SET ACCOUNT LINES                  *.
       F60EE.    IF    HTML-FR-ACCT-LINE (J60ECR)                       lv20
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F60EE-FN.
           ADD         1 TO HTML-PT
           INITIALIZE  HM93-THTML (HTML-PT)
           STRING      HTML-FR-ACCT-LINE (J60ECR)
           CN-ABREAK DELIMITED BY SIZE
           INTO HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60EE-FN. EXIT.
       F60EC-900. GO TO F60EC-A.
       F60EC-FN. EXIT.
      *N60EN.    NOTE *END OF ACCOUNT LINES               *.
       F60EN.                                                           lv15
           MOVE        1                        TO J60ENR
                                    GO TO     F60EN-B.
       F60EN-A.
           ADD         1                        TO J60ENR.
       F60EN-B.
           IF          J60ENR                   >  HTML-EO-FR-CTR
                                    GO TO     F60EN-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-FR-LINE (J60ENR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60EN-900. GO TO F60EN-A.
       F60EN-FN. EXIT.
      *N60IA.    NOTE *STRATEGY NAME FOR BROKERAGE        *.
       F60IA.                                                           lv15
           MOVE        1                        TO J60IAR
                                    GO TO     F60IA-B.
       F60IA-A.
           ADD         1                        TO J60IAR.
       F60IA-B.
           IF          J60IAR                   >  HTML-STRATEGY-CTR
                                    GO TO     F60IA-FN.
      *N60ID.    NOTE *POPULATE THE TAGS ONLY IF          *.
       F60ID.    IF    HTML-MPRN NOT = SPACES                           lv20
                 NEXT SENTENCE ELSE GO TO     F60ID-FN.
      *STRATEGY NAME IS NOT SPACES
           ADD         1 TO HTML-PT
           MOVE        HTML-STRATEGY-LINE (J60IAR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60ID-FN. EXIT.
       F60IA-900. GO TO F60IA-A.
       F60IA-FN. EXIT.
      *N60IE.    NOTE *AMOUNT REQUESTED                   *.
       F60IE.                                                           lv15
           MOVE        1                        TO J60IER
                                    GO TO     F60IE-B.
       F60IE-A.
           ADD         1                        TO J60IER.
       F60IE-B.
           IF          J60IER                   >  HTML-REQUEST-CTR
                                    GO TO     F60IE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REQUEST-LINE (J60IER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60IE-900. GO TO F60IE-A.
       F60IE-FN. EXIT.
      *N60IK.    NOTE *MOVE ACCOUNT REDUCTION LINES       *.
       F60IK.                                                           lv15
           MOVE        1                        TO J60IKR
                                    GO TO     F60IK-B.
       F60IK-A.
           ADD         1                        TO J60IKR.
       F60IK-B.
           IF          J60IKR                   >  HTML-REDUCE-CTR
                                    GO TO     F60IK-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-REDUCE-LINE (J60IKR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60IK-900. GO TO F60IK-A.
       F60IK-FN. EXIT.
       F60EA-FN. EXIT.
      *N60KC.    NOTE *MOVE MONEY TAGS                    *.
       F60KC.    IF    V249-CTTYPG = 'MMTA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F60KC-FN.
      *N60KE.    NOTE *FOR VERIFY                         *.
       F60KE.    IF    V249-CACTS = 'E'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60KE-FN.
      *N60KG.    NOTE *VERIFY MOVE MONEY                  *.
       F60KG.                                                           lv20
           MOVE        1                        TO J60KGR
                                    GO TO     F60KG-B.
       F60KG-A.
           ADD         1                        TO J60KGR.
       F60KG-B.
           IF          J60KGR                   >  HTML-MOVEMM-CTR
                                    GO TO     F60KG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-MOVEMM-LINE (J60KGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60KG-900. GO TO F60KG-A.
       F60KG-FN. EXIT.
       F60KE-FN. EXIT.
      *N60KI.    NOTE *TO-ACCOUNT HEADER                  *.
       F60KI.                                                           lv15
           MOVE        1                        TO J60KIR
                                    GO TO     F60KI-B.
       F60KI-A.
           ADD         1                        TO J60KIR.
       F60KI-B.
           IF          J60KIR                   >  HTML-TO-HDR-CTR
                                    GO TO     F60KI-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TO-HDR-LINE (J60KIR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60KI-900. GO TO F60KI-A.
       F60KI-FN. EXIT.
       F60KC-FN. EXIT.
      *N60SH.    NOTE *SURRENDER CHARGE                   *.
       F60SH.    IF    HTML-FULL-SURRCHG NOT = SPACES                   lv10
                 NEXT SENTENCE ELSE GO TO     F60SH-FN.
      *N60SI.    NOTE *SURRENDER CHARGE                   *.
       F60SI.                                                           lv15
           MOVE        1                        TO J60SIR
                                    GO TO     F60SI-B.
       F60SI-A.
           ADD         1                        TO J60SIR.
       F60SI-B.
           IF          J60SIR                   >  HTML-FULL-SURRAMT-CTR
                                    GO TO     F60SI-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-FULL-SURRAMT-LINE (J60SIR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F60SI-900. GO TO F60SI-A.
       F60SI-FN. EXIT.
       F60SH-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *MOVE S/M TAGS INTO HM93-OUTPUT     *
      *               *                                   *
      *               *************************************.
       F65.      IF    V249-CAACT = 'S'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F65-FN.
      *N65CC.    NOTE *MOVE MONEY TAGS                    *.
       F65CC.    IF    V249-CTTYPG = 'MMTA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F65CC-FN.
      *N65CE.    NOTE *TRANSFER MONEY TO THIS PRODUCT     *.
       F65CE.                                                           lv15
           MOVE        1                        TO J65CER
                                    GO TO     F65CE-B.
       F65CE-A.
           ADD         1                        TO J65CER.
       F65CE-B.
           IF          J65CER                   >  HTML-TO-PROD-CTR
                                    GO TO     F65CE-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TO-PROD-LINE (J65CER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65CE-900. GO TO F65CE-A.
       F65CE-FN. EXIT.
      *N65CI.    NOTE *TRANSFER MONEY TO THIS ACCOUNT     *.
       F65CI.                                                           lv15
           MOVE        1                        TO J65CIR
                                    GO TO     F65CI-B.
       F65CI-A.
           ADD         1                        TO J65CIR.
       F65CI-B.
           IF          J65CIR                   >  HTML-TO-ACCT-CTR
                                    GO TO     F65CI-FN.
      *N65CK.    NOTE *TRANSFER MONEY TO THIS ACCOUNT     *.
       F65CK.    IF    HTML-TO-ACCT-LINE (J65CIR)                       lv20
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F65CK-FN.
           ADD         1 TO HTML-PT
           INITIALIZE  HM93-THTML (HTML-PT)
           STRING      HTML-TO-ACCT-LINE (J65CIR)
           CN-ABREAK DELIMITED BY SIZE
           INTO HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65CK-FN. EXIT.
       F65CI-900. GO TO F65CI-A.
       F65CI-FN. EXIT.
      *N65CN.    NOTE *END OF TO ACCOUNT                  *.
       F65CN.                                                           lv15
           MOVE        1                        TO J65CNR
                                    GO TO     F65CN-B.
       F65CN-A.
           ADD         1                        TO J65CNR.
       F65CN-B.
           IF          J65CNR                   >  HTML-EO-TO-CTR
                                    GO TO     F65CN-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-EO-TO-LINE (J65CNR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65CN-900. GO TO F65CN-A.
       F65CN-FN. EXIT.
      *N65EG.    NOTE *AMOUNT REQUESTED THIS ACCOUNT      *.
       F65EG.                                                           lv15
           MOVE        1                        TO J65EGR
                                    GO TO     F65EG-B.
       F65EG-A.
           ADD         1                        TO J65EGR.
       F65EG-B.
           IF          J65EGR                   >  HTML-TO-RQST-CTR
                                    GO TO     F65EG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TO-RQST-LINE (J65EGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65EG-900. GO TO F65EG-A.
       F65EG-FN. EXIT.
      *N65EI.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65EI.    IF    HTML-TO-MIRAP NOT = SPACES                       lv15
                 NEXT SENTENCE ELSE GO TO     F65EI-FN.
      *N65EK.    NOTE *IRA CONTRIBUTION TYPE              *.
       F65EK.                                                           lv20
           MOVE        1                        TO J65EKR
                                    GO TO     F65EK-B.
       F65EK-A.
           ADD         1                        TO J65EKR.
       F65EK-B.
           IF          J65EKR                   >  HTML-TO-IRAT-CTR
                                    GO TO     F65EK-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TO-IRAT-LINE (J65EKR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65EK-900. GO TO F65EK-A.
       F65EK-FN. EXIT.
       F65EI-FN. EXIT.
       F65CC-FN. EXIT.
      *N65JC.    NOTE *TAX WITHHOLDING                    *.
       F65JC.    IF    HTML-FED-WITH NOT = SPACES                       lv10
                 NEXT SENTENCE ELSE GO TO     F65JC-FN.
      *N65JG.    NOTE *MOVE TAX LINES                     *.
       F65JG.                                                           lv15
           MOVE        1                        TO J65JGR
                                    GO TO     F65JG-B.
       F65JG-A.
           ADD         1                        TO J65JGR.
       F65JG-B.
           IF          J65JGR                   >  HTML-WITHHOLD-CTR
                                    GO TO     F65JG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-WITHHOLD-LINE (J65JGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65JG-900. GO TO F65JG-A.
       F65JG-FN. EXIT.
       F65JC-FN. EXIT.
      *N65JI.    NOTE *STATE TAX WITHHOLDING              *.
       F65JI.    IF    HTML-STATE NOT = SPACES                          lv10
                 NEXT SENTENCE ELSE GO TO     F65JI-FN.
      *N65JL.    NOTE *MOVE TAX LINES                     *.
       F65JL.                                                           lv15
           MOVE        1                        TO J65JLR
                                    GO TO     F65JL-B.
       F65JL-A.
           ADD         1                        TO J65JLR.
       F65JL-B.
           IF          J65JLR                   >  HTML-STATE-CTR
                                    GO TO     F65JL-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-STATE-LINE (J65JLR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65JL-900. GO TO F65JL-A.
       F65JL-FN. EXIT.
       F65JI-FN. EXIT.
      *N65KI.    NOTE *FEE AMOUNT                         *.
       F65KI.    IF    HTML-FEEAMT NOT = SPACES                         lv10
                 AND   FR58-CTIDA NOT = 004
                 AND   FR58-CTIDA NOT = 005
                 NEXT SENTENCE ELSE GO TO     F65KI-FN.
      *N65KK.    NOTE *MOVE FEE LINES                     *.
       F65KK.                                                           lv15
           MOVE        1                        TO J65KKR
                                    GO TO     F65KK-B.
       F65KK-A.
           ADD         1                        TO J65KKR.
       F65KK-B.
           IF          J65KKR                   >  HTML-FEEAMT-CTR
                                    GO TO     F65KK-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-FEEAMT-LINE (J65KKR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65KK-900. GO TO F65KK-A.
       F65KK-FN. EXIT.
       F65KI-FN. EXIT.
      *N65KM.    NOTE *CUSTODIAL FEE AMOUNT               *.
       F65KM.    IF    HTML-CUSTOD NOT = SPACES                         lv10
                 AND   FR58-CTIDA NOT = 004
                 AND   FR58-CTIDA NOT = 005
                 NEXT SENTENCE ELSE GO TO     F65KM-FN.
      *N65KO.    NOTE *MOVE CUSTODIAL FEE LINES           *.
       F65KO.                                                           lv15
           MOVE        1                        TO J65KOR
                                    GO TO     F65KO-B.
       F65KO-A.
           ADD         1                        TO J65KOR.
       F65KO-B.
           IF          J65KOR                   >  HTML-CUSTOD-CTR
                                    GO TO     F65KO-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-CUSTOD-LINE (J65KOR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65KO-900. GO TO F65KO-A.
       F65KO-FN. EXIT.
       F65KM-FN. EXIT.
      *N65KQ.    NOTE *SURRENDER FEE AMOUNT               *.
       F65KQ.    IF    HTML-SURRAMT NOT = SPACES                        lv10
                 NEXT SENTENCE ELSE GO TO     F65KQ-FN.
      *N65KR.    NOTE *MOVE SURRENDER FEE LINES           *.
       F65KR.                                                           lv15
           MOVE        1                        TO J65KRR
                                    GO TO     F65KR-B.
       F65KR-A.
           ADD         1                        TO J65KRR.
       F65KR-B.
           IF          J65KRR                   >  HTML-SURRAMT-CTR
                                    GO TO     F65KR-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-SURRAMT-LINE (J65KRR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65KR-900. GO TO F65KR-A.
       F65KR-FN. EXIT.
       F65KQ-FN. EXIT.
      *N65KT.    NOTE *SALES LOAD AMOUNT                  *.
       F65KT.    IF    HTML-SALELOAD NOT = SPACES                       lv10
                 NEXT SENTENCE ELSE GO TO     F65KT-FN.
      *N65KV.    NOTE *MOVE SALES LOAD LINES              *.
       F65KV.                                                           lv15
           MOVE        1                        TO J65KVR
                                    GO TO     F65KV-B.
       F65KV-A.
           ADD         1                        TO J65KVR.
       F65KV-B.
           IF          J65KVR                   >  HTML-SALELOAD-CTR
                                    GO TO     F65KV-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-SALELOAD-LINE (J65KVR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65KV-900. GO TO F65KV-A.
       F65KV-FN. EXIT.
       F65KT-FN. EXIT.
      *N65KX.    NOTE *MARKET VALUE ADJUSTMENT            *.
       F65KX.    IF    HTML-MVA NOT = SPACES                            lv10
                 NEXT SENTENCE ELSE GO TO     F65KX-FN.
      *N65MA.    NOTE *MOVE MVA LINES                     *.
       F65MA.                                                           lv15
           MOVE        1                        TO J65MAR
                                    GO TO     F65MA-B.
       F65MA-A.
           ADD         1                        TO J65MAR.
       F65MA-B.
           IF          J65MAR                   >  HTML-MVA-CTR
                                    GO TO     F65MA-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-MVA-LINE (J65MAR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65MA-900. GO TO F65MA-A.
       F65MA-FN. EXIT.
       F65KX-FN. EXIT.
      *N65MC.    NOTE *MOVE MONEY NET AMOUNT TAGS         *.
       F65MC.    IF    V249-CTTYPG = 'MMTA'                             lv10
                 NEXT SENTENCE ELSE GO TO     F65MC-FN.
           ADD         1 TO WS-NSEQA
           MOVE        WS-NSEQA TO HTML-PRINT-N1
           MOVE        WS-NSEQA TO HTML-PRINT-N2.
      *N65ME.    NOTE *AMOUNT MOVED TO THIS ACCOUNT       *.
       F65ME.                                                           lv15
           MOVE        1                        TO J65MER
                                    GO TO     F65ME-B.
       F65ME-A.
           ADD         1                        TO J65MER.
       F65ME-B.
           IF          J65MER                   >  HTML-MOVED-CTR
                                    GO TO     F65ME-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-MOVED-LINE (J65MER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65ME-900. GO TO F65ME-A.
       F65ME-FN. EXIT.
      *N65MG.    NOTE *IMPLICATION MESSAGES HEADER        *.
       F65MG.                                                           lv15
           MOVE        1                        TO J65MGR
                                    GO TO     F65MG-B.
       F65MG-A.
           ADD         1                        TO J65MGR.
       F65MG-B.
           IF          J65MGR                   >  HTML-IMPL-CTR
                                    GO TO     F65MG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-IMPL-LINE (J65MGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65MG-900. GO TO F65MG-A.
       F65MG-FN. EXIT.
      *N65MI.    NOTE *IMPLICATION MESSAGES XREF          *.
       F65MI.                                                           lv15
           MOVE        1                        TO J65MIR
                                    GO TO     F65MI-B.
       F65MI-A.
           ADD         1                        TO J65MIR.
       F65MI-B.
           IF          J65MIR                   >  HTML-HREF-CTR
                                    GO TO     F65MI-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-HREF-LINE (J65MIR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65MI-900. GO TO F65MI-A.
       F65MI-FN. EXIT.
       F65MC-FN. EXIT.
      *N65PC.    NOTE *SEND MONEY TO CLIENT TAGS          *.
       F65PC.    IF    V249-CTTYPG = 'SMTC'                             lv10
                 OR    V249-CTTYPG = 'SMDD'
                 NEXT SENTENCE ELSE GO TO     F65PC-FN.
      *N65PD.    NOTE *CHECK ADMIN 4 OR 5                 *.
       F65PD.    IF    FR58-CTIDA = 004 OR 005                          lv15
                 NEXT SENTENCE ELSE GO TO     F65PD-FN.
      *N65PF.    NOTE *MOVE TO CLIENT/ADMIN 4 OR 5        *.
       F65PF.                                                           lv20
           MOVE        1                        TO J65PFR
                                    GO TO     F65PF-B.
       F65PF-A.
           ADD         1                        TO J65PFR.
       F65PF-B.
           IF          J65PFR                   >  HTML-TOCLIENT-CTR
                                    GO TO     F65PF-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TOCLIENT-LINE (J65PFR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65PF-900. GO TO F65PF-A.
       F65PF-FN. EXIT.
       F65PD-900. GO TO F65PJ-FN.
       F65PD-FN. EXIT.
      *N65PJ.    NOTE *ELSE, NOT 4 OR 5                   *.
       F65PJ.         EXIT.                                             lv15
      *N65PS.    NOTE *MOVE TO CLIENT LINES               *.
       F65PS.                                                           lv20
           MOVE        1                        TO J65PSR
                                    GO TO     F65PS-B.
       F65PS-A.
           ADD         1                        TO J65PSR.
       F65PS-B.
           IF          J65PSR                   >  HTML-TO-CLIENT-CTR
                                    GO TO     F65PS-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-TO-CLIENT-LINE (J65PSR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65PS-900. GO TO F65PS-A.
       F65PS-FN. EXIT.
       F65PJ-FN. EXIT.
       F65PC-FN. EXIT.
      *N65PV.    NOTE *CHECK IF SEND MONEY                *.
       F65PV.    IF    (V249-CTTYPG = 'SMTC'                            lv10
                 OR    V249-CTTYPG = 'SMDD')
                 AND   V249-CACTS = 'E'
                 NEXT SENTENCE ELSE GO TO     F65PV-FN.
      *N65RA.    NOTE *STRING DEST INTO HTML-OUTPUT       *.
       F65RA.                                                           lv15
           MOVE        1                        TO J65RAR
                                    GO TO     F65RA-B.
       F65RA-A.
           ADD         1                        TO J65RAR.
       F65RA-B.
           IF          J65RAR                   >  HTML-DESTINATION-CTR
                                    GO TO     F65RA-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-DESTINATION-LINE (J65RAR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65RA-900. GO TO F65RA-A.
       F65RA-FN. EXIT.
       F65PV-FN. EXIT.
      *N65RC.    NOTE *CHECK IF SEND MONEY TO CLIENT      *.
       F65RC.    IF    V249-CTTYPG = 'SMTC'                             lv10
                 NEXT SENTENCE ELSE GO TO     F65RC-FN.
      *TO ADDRESS OF RECORD
      *N65RE.    NOTE *MOVE ADDRESS LINES                 *.
       F65RE.                                                           lv15
           MOVE        1                        TO J65RER
                                    GO TO     F65RE-B.
       F65RE-A.
           ADD         1                        TO J65RER.
       F65RE-B.
           IF          J65RER                   >  HTML-ADDRESS-CTR
                                    GO TO     F65RE-FN.
      *N65RG.    NOTE *ONLY GOOD LINES                    *.
       F65RG.    IF    HTML-ADDRESS-LINE (J65RER)                       lv20
                       NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F65RG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-ADDRESS-LINE (J65RER) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65RG-FN. EXIT.
       F65RE-900. GO TO F65RE-A.
       F65RE-FN. EXIT.
       F65RC-FN. EXIT.
      *N65SC.    NOTE *BANK DIRECT DEPOSIT                *.
       F65SC.    IF    V249-CTTYPG = 'SMDD'                             lv10
                 NEXT SENTENCE ELSE GO TO     F65SC-FN.
      *N65SG.    NOTE *BANK LINES                         *.
       F65SG.                                                           lv15
           MOVE        1                        TO J65SGR
                                    GO TO     F65SG-B.
       F65SG-A.
           ADD         1                        TO J65SGR.
       F65SG-B.
           IF          J65SGR                   >  HTML-BANK-CTR
                                    GO TO     F65SG-FN.
           ADD         1 TO HTML-PT
           MOVE        HTML-BANK-LINE (J65SGR) TO
           HM93-THTML (HTML-PT)
           MOVE        01 TO HM93-CHTML (HTML-PT).
       F65SG-900. GO TO F65SG-A.
       F65SG-FN. EXIT.
       F65SC-FN. EXIT.
       F65-FN.   EXIT.
      *N70.      NOTE *************************************.
      *               *                                   *
      *               *MOVE REVIEW TAG INTO HM93-OUTPUT   *
      *               *                                   *
      *               *************************************.
       F70.      IF    V249-CALSTC = 'Y'                                lv05
                 NEXT SENTENCE ELSE GO TO     F70-FN.
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
