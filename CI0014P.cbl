       IDENTIFICATION DIVISION.                                         CI0014
       PROGRAM-ID.  CI0014P.                                            CI0014
      *AUTHOR.         M\M - DIFFERENT OWNERSHIP MOD.                   CI0014
      *DATE-COMPILED.   09/08/14.                                       CI0014
       ENVIRONMENT DIVISION.                                            CI0014
       CONFIGURATION SECTION.                                           CI0014
       SOURCE-COMPUTER. IBM-370.                                        CI0014
       OBJECT-COMPUTER. IBM-370.                                        CI0014
       DATA DIVISION.                                                   CI0014
       WORKING-STORAGE SECTION.                                         CI0014
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01                 CI0219  PIC X(8)  VALUE 'CI0219P '.           AM0219
       01                 DU04.                                         CI0014
            10            DU04-C299.                                    CI0014
            11            DU04-CTID.                                    CI0014
            12            DU04-CTIDA  PICTURE  9(3).                    CI0014
            12            DU04-CTIDN.                                   CI0014
            13            DU04-CTIDNP PICTURE  X(13).                   CI0014
            13            DU04-CTIDND PICTURE  9(11).                   CI0014
            10            DU04-IPOCH  PICTURE  X.                       CI0014
            10            DU04-FILLER PICTURE  X(099).                  CI0014
            10            DU04-CTTLN1 PICTURE  X(30).                   CI0014
            10            DU04-CTTLN2 PICTURE  X(30).                   CI0014
            10            DU04-CTTLN3 PICTURE  X(30).                   CI0014
            10            DU04-CTTBO1 PICTURE  X(45).                   CI0014
            10            DU04-CTTBO2 PICTURE  X(45).                   CI0014
            10            DU04-CTOWN  PICTURE  9(3).                    CI0014
            10            DU04-IUGMA  PICTURE  X.                       CI0014
            10            DU04-FILLER PICTURE  X(096).                  CI0014
       01                 FA19.                                         CI0014
            10            FA19-INPUT.                                   CI0014
            11            FA19-MAPPN  PICTURE  X(10).                   CI0014
            11            FA19-FILLER PICTURE  X(100).                  CI0014
            10            FA19-OUTPUT.                                  CI0014
            11            FA19-CTTLN1 PICTURE  X(30).                   CI0014
            11            FA19-CTTLN2 PICTURE  X(30).                   CI0014
            11            FA19-CTTLN3 PICTURE  X(30).                   CI0014
            11            FA19-CTTBO1 PICTURE  X(45).                   CI0014
            11            FA19-CTTBO2 PICTURE  X(45).                   CI0014
            11            FA19-FILLER PICTURE  X(100).                  CI0014
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU070
      *                   MS03                                          ADU070
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
      **** WORKING STORAGE FOR CI0219 **********************************AM0219
      ****                                                              AM0219
      **** ACCOUNT OWNERSHIP SCRUBBER PASS AREA (LINKAGE) **************AM0219
      *!WF DSP=OS DSL=K1 SEL=19 FOR=I DES=1 LEV=1                       AM0219
       01                 OS19.                                         CI0014
            10            OS19-INPUT.                                   CI0014
            11            OS19-MAPPN  PICTURE  X(10).                   CI0014
            11            OS19-FILLER PICTURE  X(100).                  CI0014
            10            OS19-OUTPUT.                                  CI0014
            11            OS19-CTTLN1 PICTURE  X(30).                   CI0014
            11            OS19-CTTLN2 PICTURE  X(30).                   CI0014
            11            OS19-CTTLN3 PICTURE  X(30).                   CI0014
            11            OS19-CTTBO1 PICTURE  X(45).                   CI0014
            11            OS19-CTTBO2 PICTURE  X(45).                   CI0014
            11            OS19-FILLER PICTURE  X(100).                  CI0014
       01                 TA19.                                         CI0014
            10            TA19-INPUT.                                   CI0014
            11            TA19-MAPPN  PICTURE  X(10).                   CI0014
            11            TA19-FILLER PICTURE  X(100).                  CI0014
            10            TA19-OUTPUT.                                  CI0014
            11            TA19-CTTLN1 PICTURE  X(30).                   CI0014
            11            TA19-CTTLN2 PICTURE  X(30).                   CI0014
            11            TA19-CTTLN3 PICTURE  X(30).                   CI0014
            11            TA19-CTTBO1 PICTURE  X(45).                   CI0014
            11            TA19-CTTBO2 PICTURE  X(45).                   CI0014
            11            TA19-FILLER PICTURE  X(100).                  CI0014

      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************

       01  W-WORK-MISC.
      *!WI
           05  W-FR01-MPLAN
                        PICTURE X(12).                                  CI0014
      *!WI
           05  W-TO01-MPLAN
                        PICTURE X(12).                                  CI0014
      *!WI
           05  W-FC14-QITEM
                        PICTURE 9(3).                                   CI0014
      *!WI
           05  W-TC14-QITEM
                        PICTURE 9(3).                                   CI0014
      *!WI
           05  W-DO13-IOWNC
                        PICTURE X.                                      CI0014
      *!WI
           05  W-FR01-ICUST
                        PICTURE X.                                      CI0014
      *!WI
           05  W-TO01-ICUST
                        PICTURE X.                                      CI0014

      *VANTAGE LINE-OF-BUSINESS CODE (DEST ACCT)
       01  7-TO-CVALB     PIC X(03)    VALUE SPACES.

      *I-CODE FROM CT01.
      *FOR LIFE PRODUCTS, THE PLAN CODE DE-FGH-I-J-KL IS STORED IN
      *THE SUBPRODUCT FIELD.  THE 'I' CODE IS THE TAX CODE.
      *I=5 INDICATES IRA.
       01 7-TO-PRSCD.
           05 FILLER              PIC X(5).
           05 7-TO-ICODE          PIC X(1).
           05 FILLER              PIC X(3).

       01   DEBUT-WSS.                                                  CI0014
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0014
            05   IK     PICTURE X.                                      CI0014
       01  CONSTANTES-PAC.                                              CI0014
           05  FILLER  PICTURE X(87)   VALUE                            CI0014
                     '6015 CAT09/08/14CI0014ADMIN   14:33:52CI0014P AMERCI0014
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0014
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0014
           05  NUGNA   PICTURE X(5).                                    CI0014
           05  APPLI   PICTURE X(3).                                    CI0014
           05  DATGN   PICTURE X(8).                                    CI0014
           05  PROGR   PICTURE X(6).                                    CI0014
           05  CODUTI  PICTURE X(8).                                    CI0014
           05  TIMGN   PICTURE X(8).                                    CI0014
           05  PROGE   PICTURE X(8).                                    CI0014
           05  COBASE  PICTURE X(4).                                    CI0014
           05  DATGNC  PICTURE X(10).                                   CI0014
           05  RELEAS  PICTURE X(7).                                    CI0014
           05  DATGE   PICTURE X(10).                                   CI0014
           05  DATSQ   PICTURE X(10).                                   CI0014
       01  DATCE.                                                       CI0014
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0014
         05  DATOR.                                                     CI0014
           10  DATOA  PICTURE XX.                                       CI0014
           10  DATOM  PICTURE XX.                                       CI0014
           10  DATOJ  PICTURE XX.                                       CI0014
       01   VARIABLES-CONDITIONNELLES.                                  CI0014
            05                  FT      PICTURE X VALUE '0'.            CI0014
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0014
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0014
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0014
            05       5-DU00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0014
            05       5-FA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0014
            05       5-TA00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0014
       01   ZONES-UTILISATEUR PICTURE X.                                CI0014
       LINKAGE SECTION.                                                 ADU102

      *PASS AREA TO/FROM CI0014
      *!WF DSP=DO DSL=DU SEL=13 FOR=I LEV=1 PLT=05
       01                 DO00.                                         CI0014
          05              DO00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00163).                  CI0014
       01                 DO13  REDEFINES      DO00.                    CI0014
            10            DO13-C299.                                    CI0014
            11            DO13-CTID.                                    CI0014
            12            DO13-CTIDA  PICTURE  9(3).                    CI0014
            12            DO13-CTIDN.                                   CI0014
            13            DO13-CTIDNP PICTURE  X(13).                   CI0014
            13            DO13-CTIDND PICTURE  9(11).                   CI0014
            10            DO13-CTID01.                                  CI0014
            11            DO13-CTIDA1 PICTURE  9(3).                    CI0014
            11            DO13-CTIDN1.                                  CI0014
            12            DO13-CTIDP1 PICTURE  X(13).                   CI0014
            12            DO13-CTIDNA PICTURE  9(11).                   CI0014
            10            DO13-DCACG  PICTURE  9(8).                    CI0014
            10            DO13-IOWNC  PICTURE  X.                       CI0014
            10            DO13-MAPPN  PICTURE  X(10).                   CI0014
            10            DO13-FILLER PICTURE  X(90).                   CI0014

      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE "FROM" ACCOUNT CT01 SEGMENT     *
      ******************************************************************
      *
      *!WF DSP=FR DSL=CT SEL=01 FOR=I LEV=1 PLT=10
       01                 FR00.                                         CI0014
          05              FR00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00222).                  CI0014
       01                 FR01  REDEFINES      FR00.                    CI0014
            10            FR01-CT01K.                                   CI0014
            11            FR01-C299.                                    CI0014
            12            FR01-CTID.                                    CI0014
            13            FR01-CTIDA  PICTURE  9(3).                    CI0014
            13            FR01-CTIDN.                                   CI0014
            14            FR01-CTIDNP PICTURE  X(13).                   CI0014
            14            FR01-CTIDND PICTURE  9(11).                   CI0014
            10            FR01-GECKD  PICTURE  9.                       CI0014
            10            FR01-GEMDA  PICTURE  9(8).                    CI0014
            10            FR01-NSEQ4B PICTURE  9(8)                     CI0014
                          BINARY.                                       CI0014
            10            FR01-GECUC  PICTURE  99.                      CI0014
            10            FR01-CTAUL  PICTURE  9(3).                    CI0014
            10            FR01-DIRAC  PICTURE  9(4).                    CI0014
            10            FR01-CTCCI  PICTURE  X.                       CI0014
            10            FR01-CTCUS  PICTURE  999.                     CI0014
            10            FR01-CTEFD  PICTURE  9(8).                    CI0014
            10            FR01-CTIAD  PICTURE  9(8).                    CI0014
            10            FR01-CLCUS  PICTURE  99.                      CI0014
            10            FR01-CAMMB  PICTURE  X(3).                    CI0014
            10            FR01-CKPMM  PICTURE  X.                       CI0014
            10            FR01-CTLAD  PICTURE  9(8).                    CI0014
            10            FR01-IPERS  PICTURE  X.                       CI0014
            10            FR01-AUNCB  PICTURE  S9(7)V99                 CI0014
                          COMPUTATIONAL-3.                              CI0014
            10            FR01-CTLAT  PICTURE  9(8).                    CI0014
            10            FR01-CTLATC PICTURE  9(6).                    CI0014
            10            FR01-IMEGA  PICTURE  X.                       CI0014
            10            FR01-DIRAB  PICTURE  9(8).                    CI0014
            10            FR01-COLRQ  PICTURE  X.                       CI0014
            10            FR01-ZDA04  PICTURE  X(4).                    CI0014
            10            FR01-CTLPD  PICTURE  9(8).                    CI0014
            10            FR01-CIRASP PICTURE  9.                       CI0014
            10            FR01-CIRATP PICTURE  99.                      CI0014
            10            FR01-DRTHC  PICTURE  9(8).                    CI0014
            10            FR01-CPPTC  PICTURE  X.                       CI0014
            10            FR01-ZDA06  PICTURE  X(6).                    CI0014
            10            FR01-CTACD  PICTURE  9(8).                    CI0014
            10            FR01-CTNLI  PICTURE  X.                       CI0014
            10            FR01-CTRHO  PICTURE  9(8).                    CI0014
            10            FR01-CTSGD  PICTURE  9(8).                    CI0014
            10            FR01-CPATP  PICTURE  X(1).                    CI0014
            10            FR01-IRSTA  PICTURE  X.                       CI0014
            10            FR01-CTSTA  PICTURE  99.                      CI0014
            10            FR01-CTSSC  PICTURE  99.                      CI0014
            10            FR01-PRLIN  PICTURE  9(3).                    CI0014
            10            FR01-PRCOD  PICTURE  9(5).                    CI0014
            10            FR01-PRSCD  PICTURE  X(9).                    CI0014
            10            FR01-CTLNI  PICTURE  X.                       CI0014
            10            FR01-AYSIDA PICTURE  9(3).                    CI0014
            10            FR01-AYSID  PICTURE  9(5).                    CI0014
            10            FR01-CTBMC  PICTURE  99.                      CI0014
            10            FR01-CINAR  PICTURE  99.                      CI0014
            10            FR01-CPHTR  PICTURE  X.                       CI0014
            10            FR01-CDSTR  PICTURE  XX.                      CI0014
            10            FR01-CQACT  PICTURE  999.                     CI0014
            10            FR01-CIRAS  PICTURE  999.                     CI0014
            10            FR01-CIRAT  PICTURE  999.                     CI0014
            10            FR01-CLRAY  PICTURE  9(5).                    CI0014
            10            FR01-CATTP  PICTURE  X.                       CI0014
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0003 (OWNERSHIP)  *
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=FA DSL=DU SEL=04 FOR=I LEV=1 PLT=15
       01                 FA00.                                         CI0014
          05              FA00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00407).                  CI0014
       01                 FA04  REDEFINES      FA00.                    CI0014
            10            FA04-C299.                                    CI0014
            11            FA04-CTID.                                    CI0014
            12            FA04-CTIDA  PICTURE  9(3).                    CI0014
            12            FA04-CTIDN.                                   CI0014
            13            FA04-CTIDNP PICTURE  X(13).                   CI0014
            13            FA04-CTIDND PICTURE  9(11).                   CI0014
            10            FA04-IPOCH  PICTURE  X.                       CI0014
            10            FA04-FILLER PICTURE  X(099).                  CI0014
            10            FA04-CTTLN1 PICTURE  X(30).                   CI0014
            10            FA04-CTTLN2 PICTURE  X(30).                   CI0014
            10            FA04-CTTLN3 PICTURE  X(30).                   CI0014
            10            FA04-CTTBO1 PICTURE  X(45).                   CI0014
            10            FA04-CTTBO2 PICTURE  X(45).                   CI0014
            10            FA04-CTOWN  PICTURE  9(3).                    CI0014
            10            FA04-IUGMA  PICTURE  X.                       CI0014
            10            FA04-FILLER PICTURE  X(096).                  CI0014
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0018  ACCT CLIENTS*
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=FC DSL=DU SEL=14 FOR=I LEV=1 PLT=20
       01                 FC00.                                         CI0014
          05              FC00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00917).                  CI0014
       01                 FC14  REDEFINES      FC00.                    CI0014
            10            FC14-C299.                                    CI0014
            11            FC14-CTID.                                    CI0014
            12            FC14-CTIDA  PICTURE  9(3).                    CI0014
            12            FC14-CTIDN.                                   CI0014
            13            FC14-CTIDNP PICTURE  X(13).                   CI0014
            13            FC14-CTIDND PICTURE  9(11).                   CI0014
            10            FC14-DCACG  PICTURE  9(8).                    CI0014
            10            FC14-IPOCH  PICTURE  X.                       CI0014
            10            FC14-FILLER PICTURE  X(100).                  CI0014
            10            FC14-CLID01.                                  CI0014
            11            FC14-CLIDO1 PICTURE  X(3).                    CI0014
            11            FC14-NCLID1.                                  CI0014
            12            FC14-CLIDP1 PICTURE  X(12).                   CI0014
            12            FC14-CLIDNA PICTURE  9(8).                    CI0014
            10            FC14-CLCTR  PICTURE  9(3).                    CI0014
            10            FC14-DU21                                     CI0014
                          OCCURS       025     TIMES.                   CI0014
            11            FC14-C199.                                    CI0014
            12            FC14-CLID.                                    CI0014
            13            FC14-CLIDO  PICTURE  9(3).                    CI0014
            13            FC14-CLIDN.                                   CI0014
            14            FC14-CLIDNP PICTURE  X(12).                   CI0014
            14            FC14-CLIDND PICTURE  9(8).                    CI0014
            11            FC14-CLCTRC PICTURE  9(3).                    CI0014
            10            FC14-QITEM  PICTURE  9(3).                    CI0014
            10            FC14-XIMAX  PICTURE  S9(4)                    CI0014
                          BINARY.                                       CI0014
            10            FC14-CRROL  PICTURE  X.                       CI0014
            10            FC14-FILLER PICTURE  X(099).                  CI0014
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0019  ACCT GROUPS *
      **     FOR THE "FROM" ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=FG DSL=DU SEL=15 FOR=I LEV=1 PLT=25
       01                 FG00.                                         CI0014
          05              FG00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(04181).                  CI0014
       01                 FG15  REDEFINES      FG00.                    CI0014
            10            FG15-C299.                                    CI0014
            11            FG15-CTID.                                    CI0014
            12            FG15-CTIDA  PICTURE  9(3).                    CI0014
            12            FG15-CTIDN.                                   CI0014
            13            FG15-CTIDNP PICTURE  X(13).                   CI0014
            13            FG15-CTIDND PICTURE  9(11).                   CI0014
            10            FG15-DCACG  PICTURE  9(8).                    CI0014
            10            FG15-IPOCH  PICTURE  X.                       CI0014
            10            FG15-FILLER PICTURE  X(100).                  CI0014
            10            FG15-DU18                                     CI0014
                          OCCURS       010     TIMES.                   CI0014
            11            FG15-CT10.                                    CI0014
            12            FG15-CT10K.                                   CI0014
            13            FG15-GR98.                                    CI0014
            14            FG15-GRID.                                    CI0014
            15            FG15-GRIDC  PICTURE  9(3).                    CI0014
            15            FG15-GRIDN.                                   CI0014
            16            FG15-GRIDNP PICTURE  99.                      CI0014
            16            FG15-GRIDND PICTURE  9(8).                    CI0014
            12            FG15-GR97                                     CI0014
                          REDEFINES            FG15-CT10K.              CI0014
            13            FG15-GRIDCB PICTURE  9(3).                    CI0014
            13            FG15-FILLER PICTURE  X(10).                   CI0014
            12            FG15-GERSD  PICTURE  9(8).                    CI0014
            12            FG15-GERED  PICTURE  9(8).                    CI0014
            12            FG15-GRCSI  PICTURE  X.                       CI0014
            11            FG15-GR01.                                    CI0014
            12            FG15-GR01K.                                   CI0014
            13            FG15-GR98.                                    CI0014
            14            FG15-GRID.                                    CI0014
            15            FG15-GRIDC  PICTURE  9(3).                    CI0014
            15            FG15-GRIDN.                                   CI0014
            16            FG15-GRIDNP PICTURE  99.                      CI0014
            16            FG15-GRIDND PICTURE  9(8).                    CI0014
            12            FG15-GECKD  PICTURE  9.                       CI0014
            12            FG15-GEMDA  PICTURE  9(8).                    CI0014
            12            FG15-NSEQ4B PICTURE  9(8)                     CI0014
                          BINARY.                                       CI0014
            12            FG15-GRDOR  PICTURE  9(8).                    CI0014
            12            FG15-GRIAD  PICTURE  9(8).                    CI0014
            12            FG15-GECUC  PICTURE  99.                      CI0014
            12            FG15-GRLNG  PICTURE  99.                      CI0014
            12            FG15-GESLC  PICTURE  99.                      CI0014
            12            FG15-AYSIDA PICTURE  9(3).                    CI0014
            12            FG15-AYSID  PICTURE  9(5).                    CI0014
            12            FG15-GRCSD  PICTURE  9(8).                    CI0014
            12            FG15-GRCFD  PICTURE  9(8).                    CI0014
            12            FG15-GRNCL  PICTURE  S9(5)                    CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            FG15-GRNCT  PICTURE  S9(5)                    CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            FG15-GRSFC  PICTURE  99.                      CI0014
            12            FG15-GRCRN  PICTURE  9(3).                    CI0014
            12            FG15-GRCSS  PICTURE  X.                       CI0014
            12            FG15-MKSRC  PICTURE  99                       CI0014
                          OCCURS       010     TIMES.                   CI0014
            12            FG15-NEFPS  PICTURE  X(5).                    CI0014
            12            FG15-DEFPS  PICTURE  9(8).                    CI0014
            12            FG15-DLSRV  PICTURE  9(8).                    CI0014
            12            FG15-CTLNI  PICTURE  X.                       CI0014
            12            FG15-CGRLI  PICTURE  X.                       CI0014
            12            FG15-CAMGR  PICTURE  9(5)                     CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            FG15-CAMGS  PICTURE  9(5)                     CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            FG15-CAMGN  PICTURE  9(3)                     CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            FG15-CGRMF  PICTURE  X.                       CI0014
            12            FG15-FILLER PICTURE  X(08).                   CI0014
            11            FG15-GR07.                                    CI0014
            12            FG15-GEDLA  PICTURE  9(8).                    CI0014
            12            FG15-GRAID  PICTURE  X(12).                   CI0014
            12            FG15-GRPAP  PICTURE  X(14).                   CI0014
            12            FG15-GEPHNX PICTURE  9(4).                    CI0014
            12            FG15-DPLEF  PICTURE  9(8).                    CI0014
            12            FG15-DPLAM  PICTURE  9(8).                    CI0014
            12            FG15-NCPFN  PICTURE  9(6).                    CI0014
            12            FG15-GEFYE  PICTURE  9(4).                    CI0014
            12            FG15-FILLER PICTURE  X(06).                   CI0014
            12            FG15-GRPAN  PICTURE  X(45).                   CI0014
            12            FG15-CGRPA  PICTURE  99.                      CI0014
            12            FG15-IPRTT7 PICTURE  X.                       CI0014
            12            FG15-GRPED  PICTURE  9(8).                    CI0014
            12            FG15-FILLER PICTURE  X(05).                   CI0014
            12            FG15-GRPLC  PICTURE  99.                      CI0014
            12            FG15-GRPLT  PICTURE  99.                      CI0014
            12            FG15-FILLER PICTURE  X(04).                   CI0014
            12            FG15-GEADI  PICTURE  X.                       CI0014
            12            FG15-GRCFA  PICTURE  S9(11)V99                CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            FG15-GECFY  PICTURE  9(4).                    CI0014
            12            FG15-GECFC  PICTURE  99.                      CI0014
            12            FG15-MEMPL  PICTURE  X(20).                   CI0014
            12            FG15-CAUNIT PICTURE  X(4).                    CI0014
            12            FG15-FILLER PICTURE  X(21).                   CI0014
            12            FG15-GRPPP  PICTURE  999.                     CI0014
            12            FG15-CCORT  PICTURE  9(3).                    CI0014
            12            FG15-CIDRP  PICTURE  99.                      CI0014
            12            FG15-CCDWA  PICTURE  9.                       CI0014
            12            FG15-IERSA  PICTURE  X.                       CI0014
            12            FG15-DERSA  PICTURE  9(8).                    CI0014
            12            FG15-FILLER PICTURE  X(04).                   CI0014
            10            FG15-QITEM  PICTURE  9(3).                    CI0014
            10            FG15-XIMAX  PICTURE  S9(4)                    CI0014
                          BINARY.                                       CI0014
            10            FG15-FILLER PICTURE  X(100).                  CI0014
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE  "TO"  ACCOUNT CT01 SEGMENT     *
      ******************************************************************
      *
      *!WF DSP=TO DSL=CT SEL=01 FOR=I LEV=1 PLT=30
       01                 TO00.                                         CI0014
          05              TO00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00222).                  CI0014
       01                 TO01  REDEFINES      TO00.                    CI0014
            10            TO01-CT01K.                                   CI0014
            11            TO01-C299.                                    CI0014
            12            TO01-CTID.                                    CI0014
            13            TO01-CTIDA  PICTURE  9(3).                    CI0014
            13            TO01-CTIDN.                                   CI0014
            14            TO01-CTIDNP PICTURE  X(13).                   CI0014
            14            TO01-CTIDND PICTURE  9(11).                   CI0014
            10            TO01-GECKD  PICTURE  9.                       CI0014
            10            TO01-GEMDA  PICTURE  9(8).                    CI0014
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0014
                          BINARY.                                       CI0014
            10            TO01-GECUC  PICTURE  99.                      CI0014
            10            TO01-CTAUL  PICTURE  9(3).                    CI0014
            10            TO01-DIRAC  PICTURE  9(4).                    CI0014
            10            TO01-CTCCI  PICTURE  X.                       CI0014
            10            TO01-CTCUS  PICTURE  999.                     CI0014
            10            TO01-CTEFD  PICTURE  9(8).                    CI0014
            10            TO01-CTIAD  PICTURE  9(8).                    CI0014
            10            TO01-CLCUS  PICTURE  99.                      CI0014
            10            TO01-CAMMB  PICTURE  X(3).                    CI0014
            10            TO01-CKPMM  PICTURE  X.                       CI0014
            10            TO01-CTLAD  PICTURE  9(8).                    CI0014
            10            TO01-IPERS  PICTURE  X.                       CI0014
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0014
                          COMPUTATIONAL-3.                              CI0014
            10            TO01-CTLAT  PICTURE  9(8).                    CI0014
            10            TO01-CTLATC PICTURE  9(6).                    CI0014
            10            TO01-IMEGA  PICTURE  X.                       CI0014
            10            TO01-DIRAB  PICTURE  9(8).                    CI0014
            10            TO01-COLRQ  PICTURE  X.                       CI0014
            10            TO01-ZDA04  PICTURE  X(4).                    CI0014
            10            TO01-CTLPD  PICTURE  9(8).                    CI0014
            10            TO01-CIRASP PICTURE  9.                       CI0014
            10            TO01-CIRATP PICTURE  99.                      CI0014
            10            TO01-DRTHC  PICTURE  9(8).                    CI0014
            10            TO01-CPPTC  PICTURE  X.                       CI0014
            10            TO01-ZDA06  PICTURE  X(6).                    CI0014
            10            TO01-CTACD  PICTURE  9(8).                    CI0014
            10            TO01-CTNLI  PICTURE  X.                       CI0014
            10            TO01-CTRHO  PICTURE  9(8).                    CI0014
            10            TO01-CTSGD  PICTURE  9(8).                    CI0014
            10            TO01-CPATP  PICTURE  X(1).                    CI0014
            10            TO01-IRSTA  PICTURE  X.                       CI0014
            10            TO01-CTSTA  PICTURE  99.                      CI0014
            10            TO01-CTSSC  PICTURE  99.                      CI0014
            10            TO01-PRLIN  PICTURE  9(3).                    CI0014
            10            TO01-PRCOD  PICTURE  9(5).                    CI0014
            10            TO01-PRSCD  PICTURE  X(9).                    CI0014
            10            TO01-CTLNI  PICTURE  X.                       CI0014
            10            TO01-AYSIDA PICTURE  9(3).                    CI0014
            10            TO01-AYSID  PICTURE  9(5).                    CI0014
            10            TO01-CTBMC  PICTURE  99.                      CI0014
            10            TO01-CINAR  PICTURE  99.                      CI0014
            10            TO01-CPHTR  PICTURE  X.                       CI0014
            10            TO01-CDSTR  PICTURE  XX.                      CI0014
            10            TO01-CQACT  PICTURE  999.                     CI0014
            10            TO01-CIRAS  PICTURE  999.                     CI0014
            10            TO01-CIRAT  PICTURE  999.                     CI0014
            10            TO01-CLRAY  PICTURE  9(5).                    CI0014
            10            TO01-CATTP  PICTURE  X.                       CI0014
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0003 (OWNERSHIP)  *
      **     FOR THE  "TO"  ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=TA DSL=DU SEL=04 FOR=I LEV=1 PLT=35
       01                 TA00.                                         CI0014
          05              TA00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00407).                  CI0014
       01                 TA04  REDEFINES      TA00.                    CI0014
            10            TA04-C299.                                    CI0014
            11            TA04-CTID.                                    CI0014
            12            TA04-CTIDA  PICTURE  9(3).                    CI0014
            12            TA04-CTIDN.                                   CI0014
            13            TA04-CTIDNP PICTURE  X(13).                   CI0014
            13            TA04-CTIDND PICTURE  9(11).                   CI0014
            10            TA04-IPOCH  PICTURE  X.                       CI0014
            10            TA04-FILLER PICTURE  X(099).                  CI0014
            10            TA04-CTTLN1 PICTURE  X(30).                   CI0014
            10            TA04-CTTLN2 PICTURE  X(30).                   CI0014
            10            TA04-CTTLN3 PICTURE  X(30).                   CI0014
            10            TA04-CTTBO1 PICTURE  X(45).                   CI0014
            10            TA04-CTTBO2 PICTURE  X(45).                   CI0014
            10            TA04-CTOWN  PICTURE  9(3).                    CI0014
            10            TA04-IUGMA  PICTURE  X.                       CI0014
            10            TA04-FILLER PICTURE  X(096).                  CI0014
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0018  ACCT CLIENTS*
      **     FOR THE  "TO"  ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=TC DSL=DU SEL=14 FOR=I LEV=1 PLT=40
       01                 TC00.                                         CI0014
          05              TC00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00917).                  CI0014
       01                 TC14  REDEFINES      TC00.                    CI0014
            10            TC14-C299.                                    CI0014
            11            TC14-CTID.                                    CI0014
            12            TC14-CTIDA  PICTURE  9(3).                    CI0014
            12            TC14-CTIDN.                                   CI0014
            13            TC14-CTIDNP PICTURE  X(13).                   CI0014
            13            TC14-CTIDND PICTURE  9(11).                   CI0014
            10            TC14-DCACG  PICTURE  9(8).                    CI0014
            10            TC14-IPOCH  PICTURE  X.                       CI0014
            10            TC14-FILLER PICTURE  X(100).                  CI0014
            10            TC14-CLID01.                                  CI0014
            11            TC14-CLIDO1 PICTURE  X(3).                    CI0014
            11            TC14-NCLID1.                                  CI0014
            12            TC14-CLIDP1 PICTURE  X(12).                   CI0014
            12            TC14-CLIDNA PICTURE  9(8).                    CI0014
            10            TC14-CLCTR  PICTURE  9(3).                    CI0014
            10            TC14-DU21                                     CI0014
                          OCCURS       025     TIMES.                   CI0014
            11            TC14-C199.                                    CI0014
            12            TC14-CLID.                                    CI0014
            13            TC14-CLIDO  PICTURE  9(3).                    CI0014
            13            TC14-CLIDN.                                   CI0014
            14            TC14-CLIDNP PICTURE  X(12).                   CI0014
            14            TC14-CLIDND PICTURE  9(8).                    CI0014
            11            TC14-CLCTRC PICTURE  9(3).                    CI0014
            10            TC14-QITEM  PICTURE  9(3).                    CI0014
            10            TC14-XIMAX  PICTURE  S9(4)                    CI0014
                          BINARY.                                       CI0014
            10            TC14-CRROL  PICTURE  X.                       CI0014
            10            TC14-FILLER PICTURE  X(099).                  CI0014
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEG CONTAINS INFO FROM A CALL TO CI0019  ACCT GROUPS *
      **     FOR THE  "TO"  ACCOUNT.                                   *
      ******************************************************************
      *
      *!WF DSP=TG DSL=DU SEL=15 FOR=I LEV=1 PLT=45
       01                 TG00.                                         CI0014
          05              TG00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(04181).                  CI0014
       01                 TG15  REDEFINES      TG00.                    CI0014
            10            TG15-C299.                                    CI0014
            11            TG15-CTID.                                    CI0014
            12            TG15-CTIDA  PICTURE  9(3).                    CI0014
            12            TG15-CTIDN.                                   CI0014
            13            TG15-CTIDNP PICTURE  X(13).                   CI0014
            13            TG15-CTIDND PICTURE  9(11).                   CI0014
            10            TG15-DCACG  PICTURE  9(8).                    CI0014
            10            TG15-IPOCH  PICTURE  X.                       CI0014
            10            TG15-FILLER PICTURE  X(100).                  CI0014
            10            TG15-DU18                                     CI0014
                          OCCURS       010     TIMES.                   CI0014
            11            TG15-CT10.                                    CI0014
            12            TG15-CT10K.                                   CI0014
            13            TG15-GR98.                                    CI0014
            14            TG15-GRID.                                    CI0014
            15            TG15-GRIDC  PICTURE  9(3).                    CI0014
            15            TG15-GRIDN.                                   CI0014
            16            TG15-GRIDNP PICTURE  99.                      CI0014
            16            TG15-GRIDND PICTURE  9(8).                    CI0014
            12            TG15-GR97                                     CI0014
                          REDEFINES            TG15-CT10K.              CI0014
            13            TG15-GRIDCB PICTURE  9(3).                    CI0014
            13            TG15-FILLER PICTURE  X(10).                   CI0014
            12            TG15-GERSD  PICTURE  9(8).                    CI0014
            12            TG15-GERED  PICTURE  9(8).                    CI0014
            12            TG15-GRCSI  PICTURE  X.                       CI0014
            11            TG15-GR01.                                    CI0014
            12            TG15-GR01K.                                   CI0014
            13            TG15-GR98.                                    CI0014
            14            TG15-GRID.                                    CI0014
            15            TG15-GRIDC  PICTURE  9(3).                    CI0014
            15            TG15-GRIDN.                                   CI0014
            16            TG15-GRIDNP PICTURE  99.                      CI0014
            16            TG15-GRIDND PICTURE  9(8).                    CI0014
            12            TG15-GECKD  PICTURE  9.                       CI0014
            12            TG15-GEMDA  PICTURE  9(8).                    CI0014
            12            TG15-NSEQ4B PICTURE  9(8)                     CI0014
                          BINARY.                                       CI0014
            12            TG15-GRDOR  PICTURE  9(8).                    CI0014
            12            TG15-GRIAD  PICTURE  9(8).                    CI0014
            12            TG15-GECUC  PICTURE  99.                      CI0014
            12            TG15-GRLNG  PICTURE  99.                      CI0014
            12            TG15-GESLC  PICTURE  99.                      CI0014
            12            TG15-AYSIDA PICTURE  9(3).                    CI0014
            12            TG15-AYSID  PICTURE  9(5).                    CI0014
            12            TG15-GRCSD  PICTURE  9(8).                    CI0014
            12            TG15-GRCFD  PICTURE  9(8).                    CI0014
            12            TG15-GRNCL  PICTURE  S9(5)                    CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            TG15-GRNCT  PICTURE  S9(5)                    CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            TG15-GRSFC  PICTURE  99.                      CI0014
            12            TG15-GRCRN  PICTURE  9(3).                    CI0014
            12            TG15-GRCSS  PICTURE  X.                       CI0014
            12            TG15-MKSRC  PICTURE  99                       CI0014
                          OCCURS       010     TIMES.                   CI0014
            12            TG15-NEFPS  PICTURE  X(5).                    CI0014
            12            TG15-DEFPS  PICTURE  9(8).                    CI0014
            12            TG15-DLSRV  PICTURE  9(8).                    CI0014
            12            TG15-CTLNI  PICTURE  X.                       CI0014
            12            TG15-CGRLI  PICTURE  X.                       CI0014
            12            TG15-CAMGR  PICTURE  9(5)                     CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            TG15-CAMGS  PICTURE  9(5)                     CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            TG15-CAMGN  PICTURE  9(3)                     CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            TG15-CGRMF  PICTURE  X.                       CI0014
            12            TG15-FILLER PICTURE  X(08).                   CI0014
            11            TG15-GR07.                                    CI0014
            12            TG15-GEDLA  PICTURE  9(8).                    CI0014
            12            TG15-GRAID  PICTURE  X(12).                   CI0014
            12            TG15-GRPAP  PICTURE  X(14).                   CI0014
            12            TG15-GEPHNX PICTURE  9(4).                    CI0014
            12            TG15-DPLEF  PICTURE  9(8).                    CI0014
            12            TG15-DPLAM  PICTURE  9(8).                    CI0014
            12            TG15-NCPFN  PICTURE  9(6).                    CI0014
            12            TG15-GEFYE  PICTURE  9(4).                    CI0014
            12            TG15-FILLER PICTURE  X(06).                   CI0014
            12            TG15-GRPAN  PICTURE  X(45).                   CI0014
            12            TG15-CGRPA  PICTURE  99.                      CI0014
            12            TG15-IPRTT7 PICTURE  X.                       CI0014
            12            TG15-GRPED  PICTURE  9(8).                    CI0014
            12            TG15-FILLER PICTURE  X(05).                   CI0014
            12            TG15-GRPLC  PICTURE  99.                      CI0014
            12            TG15-GRPLT  PICTURE  99.                      CI0014
            12            TG15-FILLER PICTURE  X(04).                   CI0014
            12            TG15-GEADI  PICTURE  X.                       CI0014
            12            TG15-GRCFA  PICTURE  S9(11)V99                CI0014
                          COMPUTATIONAL-3.                              CI0014
            12            TG15-GECFY  PICTURE  9(4).                    CI0014
            12            TG15-GECFC  PICTURE  99.                      CI0014
            12            TG15-MEMPL  PICTURE  X(20).                   CI0014
            12            TG15-CAUNIT PICTURE  X(4).                    CI0014
            12            TG15-FILLER PICTURE  X(21).                   CI0014
            12            TG15-GRPPP  PICTURE  999.                     CI0014
            12            TG15-CCORT  PICTURE  9(3).                    CI0014
            12            TG15-CIDRP  PICTURE  99.                      CI0014
            12            TG15-CCDWA  PICTURE  9.                       CI0014
            12            TG15-IERSA  PICTURE  X.                       CI0014
            12            TG15-DERSA  PICTURE  9(8).                    CI0014
            12            TG15-FILLER PICTURE  X(04).                   CI0014
            10            TG15-QITEM  PICTURE  9(3).                    CI0014
            10            TG15-XIMAX  PICTURE  S9(4)                    CI0014
                          BINARY.                                       CI0014
            10            TG15-FILLER PICTURE  X(100).                  CI0014
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
       01                 MS00.                                         CI0014
          05              MS00-SUITE.                                   CI0014
            15       FILLER         PICTURE  X(00542).                  CI0014
       01                 MS03  REDEFINES      MS00.                    CI0014
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0014
                          COMPUTATIONAL-3.                              CI0014
            10            MS03-CMSSF  PICTURE  XX.                      CI0014
            10            MS03-DU09.                                    CI0014
            11            MS03-CMESA  PICTURE  S9(9)                    CI0014
                          BINARY.                                       CI0014
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0014
                          BINARY.                                       CI0014
            11            MS03-CMESB  PICTURE  S9(9)                    CI0014
                          BINARY.                                       CI0014
            11            MS03-CMSST  PICTURE  S9(9)                    CI0014
                          BINARY.                                       CI0014
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0014
                          BINARY.                                       CI0014
            11            MS03-QELLAA PICTURE  S9(9)                    CI0014
                          BINARY.                                       CI0014
            11            MS03-TMESS4 PICTURE  X(512).                  CI0014
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0014
            10            MX11-QMSGS  PICTURE  9(03).                   CI0014
            10            MX11-PJ09                                     CI0014
                          OCCURS       025     TIMES.                   CI0014
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0014
                          COMPUTATIONAL-3.                              CI0014
            11            MX11-CMESB  PICTURE  S9(9)                    CI0014
                          BINARY.                                       CI0014
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DO13
                                FR01
                                FA04
                                FC14
                                FG15
                                TO01
                                TA04
                                TC14
                                TG15
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0014
      *               *                                   *             CI0014
      *               *INITIALISATIONS                    *             CI0014
      *               *                                   *             CI0014
      *               *************************************.            CI0014
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
      *N02CA.    NOTE *INITIALIZE OWNERSHIP INDICATOR     *.
       F02CA.                                                           lv10
      *TO DIFFERENT OWNERSHIP
           MOVE        'N' TO DO13-IOWNC.
       F02CA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0014
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0014
      *               *                                   *             CI0014
      *               *FIN DE TRAITEMENT                  *             CI0014
      *               *                                   *             CI0014
      *               *************************************.            CI0014
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0014
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *COMPARE PLAN TYPES                 *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BA.    NOTE *PROCESS MACRO AMPLAN ONLY FOR      *.
       F50BA.    IF    DO13-MAPPN = 'SD'                                lv08
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *SD - THIS MACRO FOLLOWS KA60
      * --> PROCESSING IN KA60 TO
      *DETERMINE THE PLAN TYPE IS
      *LITTLE DIFFERENT FROM KD70
      *N50BB.    NOTE *SET PLAN TYPE FOR SOURCE ACCT      *.
       F50BB.         EXIT.                                             lv10
      *N50CA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            AMPLAN
       F50CA.                                                           lv15
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** INITIALIZE THE ACCOUNT PLAN  *                                 AMPLAN
      ** TYPE DESCRIPTION TO SPACES   *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        SPACES TO W-FR01-MPLAN.                          AMPLAN
       F50CA-FN. EXIT.
      *N50CB.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            AMPLAN
       F50CB.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 001                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** HOUSEHOLD GROUP, THEN THE    *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-FR01-MPLAN                       AMPLAN
      *
                 IF    FR01-CTCCI NOT = '1'                             DOT
      *IF IDS IS NOT THE CUSTODIAN,
      *LEAVE PLAN TYPE AS PERSONAL
      *& GET OUT
               GO TO     F50CB-FN.
      *N50CC.    NOTE *CHECK IF IDS IS CUSTODIAN          *.            AMPLAN
       F50CC.    IF    FR01-CTCCI = '1'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F50CC-FN.                 AMPLAN
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS CUSTODIAN  *                                 AMPLAN
      ** ACCOUNT, CHECK QUALIFIED     *                                 AMPLAN
      ** ACCOUNT CODE AND IRA STATUS  *                                 AMPLAN
      ** CODE.                        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
           MOVE        'IRA ACTIVE' TO W-FR01-MPLAN.                    AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 002                                 AMPLAN
           MOVE        'IRA ROLLOVER' TO W-FR01-MPLAN.                  AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 003                                 AMPLAN
                 AND   (FR01-CIRAT NOT = 005                            AMPLAN
                 AND   FR01-CIRAT NOT = 006)                            AMPLAN
           MOVE        'BENEFICIAL' TO W-FR01-MPLAN.                    AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CIRAT = 004                                 AMPLAN
           MOVE        'SRA' TO W-FR01-MPLAN.                           AMPLAN
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH IRA   ' TO W-FR01-MPLAN.                   AMPLAN
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 003                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH BENF  ' TO W-FR01-MPLAN.                   AMPLAN
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONV  ' TO W-FR01-MPLAN.                   AMPLAN
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 003                                 AMPLAN
                 AND   FR01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONVBEN' TO W-FR01-MPLAN.                  AMPLAN
                 IF    FR01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-FR01-MPLAN.                           AMPLAN
                 IF    FR01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-FR01-MPLAN.                          AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 AMPLAN
                 AND   FR01-CIRAT = 007                                 AMPLAN
           MOVE        'EDUC IRA' TO W-FR01-MPLAN.                      AMPLAN
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001
                 AND   FR01-CIRAT = 003
      *PUT SEP'S IN SEPARATE PLAN TYPE
           MOVE        'SEP' TO W-FR01-MPLAN.
       F50CC-FN. EXIT.
       F50CB-FN. EXIT.
      *N50CD.    NOTE *CHECK IF PENSION GROUP             *.            AMPLAN
       F50CD.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 002                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F50CD-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** PENSION GROUP, THEN THE      *                                 AMPLAN
      ** PLAN TYPE IS PENSION.        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PENSION' TO W-FR01-MPLAN.                       AMPLAN
      *N50CE.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            AMPLAN
       F50CE.    IF    FG15-QITEM > ZEROS                               lv20
                 AND   FG15-CIDRP (1) NOT = 01                          AMPLAN
                 NEXT SENTENCE ELSE GO TO     F50CE-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS NOT A      *                                 AMPLAN
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-FR01-MPLAN.                      AMPLAN
       F50CE-FN. EXIT.
       F50CD-FN. EXIT.
       F50BB-FN. EXIT.
      *N50DA.    NOTE *SET PLAN TYPE FOR DEST ACCT        *.
       F50DA.         EXIT.                                             lv10
      *N50EA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            AMPLAN
       F50EA.                                                           lv15
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** INITIALIZE THE ACCOUNT PLAN  *                                 AMPLAN
      ** TYPE DESCRIPTION TO SPACES   *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        SPACES TO W-TO01-MPLAN                           AMPLAN
           MOVE        TO01-PRSCD TO 7-TO-PRSCD
      *SET VANTAGE LINE-OF-BUSINESS CD
           MOVE        SPACES TO 7-TO-CVALB.
                 IF    (TO01-CTIDA = 004 OR 005)                        DOT
                 AND   (TO01-PRCOD = 00950
                 OR    = 00951
                 OR    = 00952
                 OR    = 00953
                 OR    = 00954
                 OR    = 00956)
                 AND   TO01-CQACT = 001
                 AND   TO01-CIRAT = 001
           MOVE        'IRA' TO 7-TO-CVALB.
                 IF    (TO01-CTIDA = 004 OR 005)                        DOT
                 AND   (TO01-PRCOD = 00950
                 OR    = 00951
                 OR    = 00952
                 OR    = 00953
                 OR    = 00954
                 OR    = 00956)
                 AND   TO01-CQACT = 001
                 AND   TO01-CIRAT = 003
           MOVE        'SEP' TO 7-TO-CVALB.
       F50EA-FN. EXIT.
      *N50EB.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            AMPLAN
       F50EB.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 001                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F50EB-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** HOUSEHOLD GROUP, THEN THE    *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-TO01-MPLAN                       AMPLAN
      *
                 IF    TO01-CTCCI NOT = '1'                             DOT
                 AND   7-TO-ICODE NOT = '5'
                 AND   7-TO-CVALB NOT = 'IRA'
                 AND   NOT = 'SEP'
      *IF IDS IS NOT THE CUSTODIAN,
      *LEAVE PLAN TYPE AS 'PERSONAL'
      *& GET OUT
               GO TO     F50EB-FN.
      *N50EC.    NOTE *CHECK IF IDS IS CUSTODIAN          *.            AMPLAN
       F50EC.    IF    TO01-CTCCI = '1'                                 lv20
                 NEXT SENTENCE ELSE GO TO     F50EC-FN.                 AMPLAN
      *                                                                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS CUSTODIAN  *                                 AMPLAN
      ** ACCOUNT, CHECK QUALIFIED     *                                 AMPLAN
      ** ACCOUNT CODE AND IRA STATUS  *                                 AMPLAN
      ** CODE.                        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
           MOVE        'IRA ACTIVE' TO W-TO01-MPLAN.                    AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 002                                 AMPLAN
           MOVE        'IRA ROLLOVER' TO W-TO01-MPLAN.                  AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 003                                 AMPLAN
                 AND   (TO01-CIRAT NOT = 005                            AMPLAN
                 AND   TO01-CIRAT NOT = 006)                            AMPLAN
           MOVE        'BENEFICIAL' TO W-TO01-MPLAN.                    AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CIRAT = 004                                 AMPLAN
           MOVE        'SRA' TO W-TO01-MPLAN.                           AMPLAN
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH IRA   ' TO W-TO01-MPLAN.                   AMPLAN
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 003                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH BENF  ' TO W-TO01-MPLAN.                   AMPLAN
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONV  ' TO W-TO01-MPLAN.                   AMPLAN
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 003                                 AMPLAN
                 AND   TO01-CQACT = 001                                 AMPLAN
           MOVE        'ROTH CONVBEN' TO W-TO01-MPLAN.                  AMPLAN
                 IF    TO01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-TO01-MPLAN.                           AMPLAN
                 IF    TO01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-TO01-MPLAN.                          AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 AMPLAN
                 AND   TO01-CIRAT = 007                                 AMPLAN
           MOVE        'EDUC IRA' TO W-TO01-MPLAN.                      AMPLAN
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001
                 AND   TO01-CIRAT = 003
      *PUT SEP'S IN SEPARATE PLAN TYPE
           MOVE        'SEP' TO W-TO01-MPLAN.
       F50EC-FN. EXIT.
       F50EB-FN. EXIT.
      *N50ED.    NOTE *CHECK IF PENSION GROUP             *.            AMPLAN
       F50ED.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 002                            AMPLAN
                 NEXT SENTENCE ELSE GO TO     F50ED-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS WITHIN A   *                                 AMPLAN
      ** PENSION GROUP, THEN THE      *                                 AMPLAN
      ** PLAN TYPE IS PENSION.        *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PENSION' TO W-TO01-MPLAN.                       AMPLAN
      *N50EE.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            AMPLAN
       F50EE.    IF    TG15-QITEM > ZEROS                               lv20
                 AND   TG15-CIDRP (1) NOT = 01                          AMPLAN
                 NEXT SENTENCE ELSE GO TO     F50EE-FN.                 AMPLAN
      *********************************                                 AMPLAN
      ** IF THE ACCOUNT IS NOT A      *                                 AMPLAN
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 AMPLAN
      ** PLAN TYPE IS PERSONAL.       *                                 AMPLAN
      *********************************                                 AMPLAN
      *                                                                 AMPLAN
           MOVE        'PERSONAL' TO W-TO01-MPLAN.                      AMPLAN
       F50EE-FN. EXIT.
       F50ED-FN. EXIT.
       F50DA-FN. EXIT.
       F50BA-FN. EXIT.
      *N50EG.    NOTE *PROCESS ONLY FOR UD                *.
       F50EG.    IF    DO13-MAPPN = 'UD' OR 'FDC'                       lv08
                 NEXT SENTENCE ELSE GO TO     F50EG-FN.
      *
           PERFORM     F92BB THRU F92BB-FN.
       F50EG-FN. EXIT.
      *N50FA.    NOTE *COMPARE PLAN TYPES                 *.
       F50FA.    IF    W-FR01-MPLAN NOT =                               lv08
                       W-TO01-MPLAN
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *********************************
      ** IF THE PLAN TYPES FOR THE 2  *
      ** ACCOUNT NUMBERS ARE NOT THE  *
      ** SAME, RETURN TO THE CALLING  *
      ** MODULE WITH OWNERSHIP        *
      ** INDICATOR SET TO 'N'.        *
      ** (INITIALIZED IN F02CA TO 'N')*
      *********************************
           MOVE                     ALL '1' TO FT GO TO F20.
       F50FA-FN. EXIT.
       F50-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *CMPR CUSTODIAL ACCT INDICATORS     *
      *               *                                   *
      *               *************************************.
       F52.           EXIT.                                             lv05
      *N52BA.    NOTE *INIT CUSTODIAL ACCT INDICATORS     *.
       F52BA.                                                           lv10
           MOVE        'N' TO W-FR01-ICUST
           W-TO01-ICUST.
       F52BA-FN. EXIT.
      *N52BZ.    NOTE *SOURCE ACCT IS IN A GROUP          *.
       F52BZ.    IF    FG15-QITEM > ZEROS                               lv10
                 NEXT SENTENCE ELSE GO TO     F52BZ-FN.
      *N52CA.    NOTE *ACCT IN HH GROUP &                 *.
       F52CA.    IF    (FG15-GRIDCB (1) = 001                           lv15
                 AND   FR01-CTCCI = '1')
                 OR    (FG15-GRIDCB (1) = 002
                 AND   FG15-CIDRP (1) = 01)
                 NEXT SENTENCE ELSE GO TO     F52CA-FN.
      *IDS ROLE TO ACCT IS CUSTODIAN
      *OR
      *ACCT IN PENSION GROUP &
      *IDS ROLE TO PLAN IS CUSTODIAN
           MOVE        'Y' TO W-FR01-ICUST.
       F52CA-FN. EXIT.
       F52BZ-FN. EXIT.
      *N52CZ.    NOTE *DESTINATION ACCT IS IN A GROUP     *.
       F52CZ.    IF    TG15-QITEM > ZEROS                               lv10
                 NEXT SENTENCE ELSE GO TO     F52CZ-FN.
      *N52DA.    NOTE *ACCT IN HH GROUP &                 *.
       F52DA.    IF    (TG15-GRIDCB (1) = 001                           lv15
                 AND   TO01-CTCCI = '1')
                 OR    (TG15-GRIDCB (1) = 002
                 AND   TG15-CIDRP (1) = 01)
                 NEXT SENTENCE ELSE GO TO     F52DA-FN.
      *IDS ROLE TO ACCT IS CUSTODIAN
      *OR
      *ACCT IN PENSION GROUP &
      *IDS ROLE TO PLAN IS CUSTODIAN
           MOVE        'Y' TO W-TO01-ICUST.
       F52DA-FN. EXIT.
       F52CZ-FN. EXIT.
      *N52EA.    NOTE *CUSTODIAL ACCT INDICATORS NOT =    *.
       F52EA.    IF    W-FR01-ICUST NOT =                               lv10
                       W-TO01-ICUST
                 NEXT SENTENCE ELSE GO TO     F52EA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F52EA-FN. EXIT.
       F52-FN.   EXIT.
      *N54.      NOTE *************************************.
      *               *                                   *
      *               *COMPARE OWNERSHIP WORDING CODES    *
      *               *                                   *
      *               *************************************.
       F54.      IF    TA04-CTOWN NOT = ZEROS                           lv05
                 AND   FR01-CTIDA NOT = 004 AND 005
                 AND   TO01-CTIDA NOT = 004 AND 005
                 NEXT SENTENCE ELSE GO TO     F54-FN.
      *& OWNERSHIP LINES
      *(NOT ON LIFE ACCTS)
      *N54DA.    NOTE *OWNERSHIP WORDING CODE NOT EQUAL   *.
       F54DA.    IF    FA04-CTOWN NOT = TA04-CTOWN                      lv10
                 NEXT SENTENCE ELSE GO TO     F54DA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F54DA-FN. EXIT.
      *N54FA.    NOTE *COMPARE OWNERSHIP LINES            *.
       F54FA.    IF    TA04-CTOWN NOT = 001 AND 002                     lv10
                       AND 007
                 NEXT SENTENCE ELSE GO TO     F54FA-FN.
      *(NOT WHEN DEST ACCT IS INDIVID,
      * JOINT TENANCY, OR CUSTODIAN)
      *N54FD.    NOTE *CALL CI0219 TO SCRUB OWN LINES     *.
       F54FD.    IF    DO13-MAPPN = 'FDC'                               lv15
                 NEXT SENTENCE ELSE GO TO     F54FD-FN.
           INITIALIZE  OS19-INPUT
           MOVE        DO13-MAPPN TO OS19-MAPPN.
      *'FROM' ACCOUNT                                                   DOT
           MOVE        FA04 TO DU04
           PERFORM     F91OS THRU F91OS-FN
      *SAVE 'SCRUBBED' OWNERSHIP LINES
           MOVE        OS19 TO FA19.
      *'TO' ACCOUNT                                                     DOT
           MOVE        TA04 TO DU04
           PERFORM     F91OS THRU F91OS-FN
      *SAVE 'SCRUBBED' OWNERSHIP LINES
           MOVE        OS19 TO TA19.
      *MOVE OWNERSHIP LINES FOR COMPARE                                 DOT
           MOVE        FA19-CTTLN1 TO FA04-CTTLN1
           MOVE        FA19-CTTLN2 TO FA04-CTTLN2
           MOVE        FA19-CTTLN3 TO FA04-CTTLN3
           MOVE        FA19-CTTBO1 TO FA04-CTTBO1
           MOVE        FA19-CTTBO2 TO FA04-CTTBO2
           MOVE        TA19-CTTLN1 TO TA04-CTTLN1
           MOVE        TA19-CTTLN2 TO TA04-CTTLN2
           MOVE        TA19-CTTLN3 TO TA04-CTTLN3
           MOVE        TA19-CTTBO1 TO TA04-CTTBO1
           MOVE        TA19-CTTBO2 TO TA04-CTTBO2.
       F54FD-FN. EXIT.
      *N54GA.    NOTE *OWNERSHIP LINE 1 NOT EQUAL         *.
       F54GA.    IF    FA04-CTTLN1 NOT =                                lv15
                       TA04-CTTLN1
                 NEXT SENTENCE ELSE GO TO     F54GA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F54GA-FN. EXIT.
      *N54HA.    NOTE *OWNERSHIP LINE 2 NOT EQUAL         *.
       F54HA.    IF    FA04-CTTLN2 NOT =                                lv15
                       TA04-CTTLN2
                 NEXT SENTENCE ELSE GO TO     F54HA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F54HA-FN. EXIT.
      *N54IA.    NOTE *OWNERSHIP LINE 3 NOT EQUAL         *.
       F54IA.    IF    FA04-CTTLN3 NOT =                                lv15
                       TA04-CTTLN3
                 NEXT SENTENCE ELSE GO TO     F54IA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F54IA-FN. EXIT.
      *N54JA.    NOTE *BENEFICIARY LINE 1 NOT EQUAL       *.
       F54JA.    IF    FA04-CTTBO1 NOT =                                lv15
                       TA04-CTTBO1
                 NEXT SENTENCE ELSE GO TO     F54JA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F54JA-FN. EXIT.
      *N54KA.    NOTE *BENEFICIARY LINE 2 NOT EQUAL       *.
       F54KA.    IF    FA04-CTTBO2 NOT =                                lv15
                       TA04-CTTBO2
                 NEXT SENTENCE ELSE GO TO     F54KA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F54KA-FN. EXIT.
       F54FA-FN. EXIT.
       F54-FN.   EXIT.
      *N56.      NOTE *************************************.
      *               *                                   *
      *               *COMPARE TAXPAYER CLIENT ID'S       *
      *               *                                   *
      *               *************************************.
       F56.           EXIT.                                             lv05
      *N56BA.    NOTE *DIFFERENT TAXPAYERS                *.
       F56BA.    IF    (FC14-CLID01 NOT =                               lv10
                       TC14-CLID01)
                 AND   (FG15-QITEM > ZEROS
                 AND   FG15-GRIDCB (1) = 002)
                 NEXT SENTENCE ELSE GO TO     F56BA-FN.
      *AND SOURCE ACCT IS IN PENS GROUP
           MOVE                     ALL '1' TO FT GO TO F20.
       F56BA-FN. EXIT.
       F56-FN.   EXIT.
      *N58.      NOTE *************************************.
      *               *                                   *
      *               *COMPARE OWNER CLIENT IDS           *
      *               *                                   *
      *               *************************************.
       F58.           EXIT.                                             lv05
      *N58BA.    NOTE *INIT OWNERSHIP INDICATOR TO 'Y'    *.
       F58BA.                                                           lv10
           MOVE        'Y' TO W-DO13-IOWNC.
       F58BA-FN. EXIT.
      *N58CA.    NOTE *NUMBER OF CLIENTS ON 'TO' ACCT     *.
       F58CA.    IF    TC14-QITEM NOT > FC14-QITEM                      lv10
                 NEXT SENTENCE ELSE GO TO     F58CA-FN.
      *=< NUMBER ON 'FROM' ACCT
      *N58DA.    NOTE *INITIALIZE TABLE INDICES TO 1      *.
       F58DA.                                                           lv15
           MOVE        +1 TO W-TC14-QITEM
           W-FC14-QITEM.
       F58DA-FN. EXIT.
      *N58EA.    NOTE *LOOP WHILE MORE OWNER CLIENTS      *.
       F58EA.    IF    W-TC14-QITEM NOT >                               lv15
                       TC14-QITEM
                 NEXT SENTENCE ELSE GO TO     F58EA-FN.
      *IN TABLE
      *N58FA.    NOTE *SAME OWNER FOUND ON SOURCE &       *.
       F58FA.    IF    FC14-CLID (W-FC14-QITEM) =                       lv20
                       TC14-CLID (W-TC14-QITEM)
                 NEXT SENTENCE ELSE GO TO     F58FA-FN.
      *DEST ACCTS
           ADD         +1 TO W-TC14-QITEM
           MOVE        +1 TO W-FC14-QITEM
               GO TO     F58EA-900.
       F58FA-FN. EXIT.
      *N58GA.    NOTE *CHECK IF END OF 'FROM' TABLE       *.
       F58GA.    IF    W-FC14-QITEM = FC14-QITEM                        lv20
                 NEXT SENTENCE ELSE GO TO     F58GA-FN.
           MOVE        'N' TO W-DO13-IOWNC
           MOVE        TC14-QITEM TO W-TC14-QITEM
           ADD         +1 TO W-TC14-QITEM
               GO TO     F58EA-900.
       F58GA-FN. EXIT.
      *N58HA.    NOTE *BUMP UP 'FROM' TABLE INDEX         *.
       F58HA.                                                           lv20
           ADD         +1 TO W-FC14-QITEM.
       F58HA-FN. EXIT.
       F58EA-900. GO TO F58EA.
       F58EA-FN. EXIT.
       F58CA-900. GO TO F58IA-FN.
       F58CA-FN. EXIT.
      *N58IA.    NOTE *NUMBER OF CLIENTS ON 'TO' ACCT     *.
       F58IA.                                                           lv10
      *> NUMBER ON 'FROM' ACCT
      *N58JA.    NOTE *INITIALIZE TABLE INDICES TO 1      *.
       F58JA.                                                           lv15
           MOVE        +1 TO W-TC14-QITEM
           W-FC14-QITEM.
       F58JA-FN. EXIT.
      *N58KA.    NOTE *LOOP WHILE MORE OWNER CLIENTS      *.
       F58KA.    IF    W-FC14-QITEM NOT >                               lv15
                       FC14-QITEM
                 NEXT SENTENCE ELSE GO TO     F58KA-FN.
      *IN TABLE
      *N58LA.    NOTE *SAME OWNER FOUND ON SOURCE &       *.
       F58LA.    IF    FC14-CLID (W-FC14-QITEM) =                       lv20
                       TC14-CLID (W-TC14-QITEM)
                 NEXT SENTENCE ELSE GO TO     F58LA-FN.
      *DEST ACCTS
           ADD         +1 TO W-FC14-QITEM
           MOVE        +1 TO W-TC14-QITEM
               GO TO     F58KA-900.
       F58LA-FN. EXIT.
      *N58MA.    NOTE *CHECK IF END OF 'TO' TABLE         *.
       F58MA.    IF    W-TC14-QITEM = TC14-QITEM                        lv20
                 NEXT SENTENCE ELSE GO TO     F58MA-FN.
           MOVE        'N' TO W-DO13-IOWNC
           MOVE        FC14-QITEM TO W-FC14-QITEM
           ADD         +1 TO W-FC14-QITEM
               GO TO     F58KA-900.
       F58MA-FN. EXIT.
      *N58NA.    NOTE *BUMP UP 'TO' TABLE INDEX           *.
       F58NA.                                                           lv20
           ADD         +1 TO W-TC14-QITEM.
       F58NA-FN. EXIT.
       F58KA-900. GO TO F58KA.
       F58KA-FN. EXIT.
       F58IA-FN. EXIT.
      *N58ZA.    NOTE *NOT SAME OWNERS                    *.
       F58ZA.    IF    W-DO13-IOWNC = 'N'                               lv10
                 NEXT SENTENCE ELSE GO TO     F58ZA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F58ZA-FN. EXIT.
       F58-FN.   EXIT.
      *N62.      NOTE *************************************.
      *               *                                   *
      *               *COMPARE GROUP CATEGORIES, ID'S     *
      *               *                                   *
      *               *************************************.
       F62.           EXIT.                                             lv05
      *N62BA.    NOTE *MUST BE IN SAME GROUP CATEGORY     *.
       F62BA.    IF    (FG15-QITEM > ZEROS)                             lv10
                 AND   (TG15-QITEM > ZEROS)
                 AND   (FG15-GRIDCB (1) NOT =
                       TG15-GRIDCB (1))
                 NEXT SENTENCE ELSE GO TO     F62BA-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F62BA-FN. EXIT.
      *N62BG.    NOTE *BOTH ACCTS ARE SRA'S               *.
       F62BG.    IF    FR01-CIRAT = 004                                 lv10
                 AND   TO01-CIRAT = 004
                 AND   FG15-CT10K (1) NOT =
                       TG15-CT10K (1)
                 NEXT SENTENCE ELSE GO TO     F62BG-FN.
      *MUST HAVE SAME GROUP ID
           MOVE                     ALL '1' TO FT GO TO F20.
       F62BG-FN. EXIT.
       F62-FN.   EXIT.
      *N75.      NOTE *************************************.
      *               *                                   *
      *               *ACCOUNTS HAVE SAME OWNERSHIP       *
      *               *                                   *
      *               *************************************.
       F75.                                                             lv05
           MOVE        'Y' TO DO13-IOWNC.
       F75-FN.   EXIT.
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
      *               *PERFORMED CODE                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91OS.    NOTE *'SCRUB' OWNERSHIP LINES            *.            AM0219
       F91OS.                                                           lv10
           INITIALIZE  OS19-OUTPUT MS03                                 AM0219
           CALL        CI0219 USING                                     AM0219
           DFHEIBLK                                                     AM0219
           DFHCOMMAREA                                                  AM0219
           OS19                                                         AM0219
           DU04                                                         AM0219
           MS03                                                         AM0219
           MX11.                                                        AM0219
      *N91OT.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F91OT.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F91OT-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0219 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F91OT-900. GO TO F91OU-FN.
       F91OT-FN. EXIT.
      *N91OU.    NOTE *NO ERRORS                          *.            ADU070
       F91OU.                                                           lv15
           INITIALIZE  MS03.                                            ADU070
       F91OU-FN. EXIT.
       F91OS-FN. EXIT.
       F91-FN.   EXIT.
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *PERFORMED ROUTINES                 *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92BB.    NOTE *PROCESSING MACRO APLANU FOR UD     *.
       F92BB.                                                           lv10
      *    THIS MACRO MODELS KD70
      *N92CA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            APLANU
       F92CA.                                                           lv15
      *                                                                 APLANU
      *********************************                                 APLANU
      ** INITIALIZE THE ACCOUNT PLAN  *                                 APLANU
      ** TYPE DESCRIPTION TO SPACES   *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        SPACES TO W-FR01-MPLAN.                          APLANU
       F92CA-FN. EXIT.
      *N92CB.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            APLANU
       F92CB.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 001                            APLANU
                 NEXT SENTENCE ELSE GO TO     F92CB-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** HOUSEHOLD GROUP, THEN THE    *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-FR01-MPLAN.                      APLANU
      *N92CC.    NOTE *CHECK IF QUALIFIED                 *.            APLANU
       F92CC.    IF    FR01-CQACT > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F92CC-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS QUALIFIED  *                                 APLANU
      ** CHECK THE IRA TYPE CODE AND  *                                 APLANU
      ** AND IRA STATUS CODE          *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
           MOVE        'IRA ACTIVE' TO W-FR01-MPLAN.                    APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 002                                 APLANU
           MOVE        'IRA ROLLOVER' TO W-FR01-MPLAN.                  APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 003                                 APLANU
                 AND   (FR01-CIRAT NOT = 005                            APLANU
                 AND   FR01-CIRAT NOT = 006)                            APLANU
           MOVE        'BENEFICIAL' TO W-FR01-MPLAN.                    APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CIRAT = 004                                 APLANU
           MOVE        'SRA' TO W-FR01-MPLAN.                           APLANU
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH IRA   ' TO W-FR01-MPLAN.                   APLANU
                 IF    FR01-CIRAT = 005                                 DOT
                 AND   FR01-CIRAS = 003                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH BENF  ' TO W-FR01-MPLAN.                   APLANU
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONV  ' TO W-FR01-MPLAN.                   APLANU
                 IF    FR01-CIRAT = 006                                 DOT
                 AND   FR01-CIRAS = 003                                 APLANU
                 AND   FR01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONVBEN' TO W-FR01-MPLAN.                  APLANU
                 IF    FR01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-FR01-MPLAN.                           APLANU
                 IF    FR01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-FR01-MPLAN.                          APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001                                 APLANU
                 AND   FR01-CIRAT = 007                                 APLANU
           MOVE        'EDUC IRA' TO W-FR01-MPLAN.                      APLANU
                 IF    FR01-CQACT = 001                                 DOT
                 AND   FR01-CIRAS = 001
                 AND   FR01-CIRAT = 003
      *PUT SEP'S IN SEPARATE PLAN TYPE
           MOVE        'SEP' TO W-FR01-MPLAN.
       F92CC-FN. EXIT.
       F92CB-FN. EXIT.
      *N92CD.    NOTE *CHECK IF PENSION GROUP             *.            APLANU
       F92CD.    IF    FG15-QITEM > ZEROS                               lv15
                 AND   FG15-GRIDCB (1) = 002                            APLANU
                 NEXT SENTENCE ELSE GO TO     F92CD-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** PENSION GROUP, THEN THE      *                                 APLANU
      ** PLAN TYPE IS PENSION.        *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PENSION' TO W-FR01-MPLAN.                       APLANU
      *N92CE.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            APLANU
       F92CE.    IF    FG15-CIDRP (1) NOT = 01                          lv20
                 NEXT SENTENCE ELSE GO TO     F92CE-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS NOT A      *                                 APLANU
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-FR01-MPLAN.                      APLANU
       F92CE-FN. EXIT.
       F92CD-FN. EXIT.
      *N92EA.    NOTE *INITIALIZE ACCOUNT PLAN TYPE       *.            APLANU
       F92EA.                                                           lv15
      *                                                                 APLANU
      *********************************                                 APLANU
      ** INITIALIZE THE ACCOUNT PLAN  *                                 APLANU
      ** TYPE DESCRIPTION TO SPACES   *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        SPACES TO W-TO01-MPLAN.                          APLANU
       F92EA-FN. EXIT.
      *N92EB.    NOTE *CHECK IF HOUSEHOLD GROUP           *.            APLANU
       F92EB.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 001                            APLANU
                 NEXT SENTENCE ELSE GO TO     F92EB-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** HOUSEHOLD GROUP, THEN THE    *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-TO01-MPLAN.                      APLANU
      *N92EC.    NOTE *CHECK IF QUALIFIED                 *.            APLANU
       F92EC.    IF    TO01-CQACT > ZERO                                lv20
                 NEXT SENTENCE ELSE GO TO     F92EC-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS QUALIFIED  *                                 APLANU
      ** CHECK THE IRA TYPE CODE AND  *                                 APLANU
      ** AND IRA STATUS CODE          *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
           MOVE        'IRA ACTIVE' TO W-TO01-MPLAN.                    APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 002                                 APLANU
           MOVE        'IRA ROLLOVER' TO W-TO01-MPLAN.                  APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 003                                 APLANU
                 AND   (TO01-CIRAT NOT = 005                            APLANU
                 AND   TO01-CIRAT NOT = 006)                            APLANU
           MOVE        'BENEFICIAL' TO W-TO01-MPLAN.                    APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CIRAT = 004                                 APLANU
           MOVE        'SRA' TO W-TO01-MPLAN.                           APLANU
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH IRA   ' TO W-TO01-MPLAN.                   APLANU
                 IF    TO01-CIRAT = 005                                 DOT
                 AND   TO01-CIRAS = 003                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH BENF  ' TO W-TO01-MPLAN.                   APLANU
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONV  ' TO W-TO01-MPLAN.                   APLANU
                 IF    TO01-CIRAT = 006                                 DOT
                 AND   TO01-CIRAS = 003                                 APLANU
                 AND   TO01-CQACT = 001                                 APLANU
           MOVE        'ROTH CONVBEN' TO W-TO01-MPLAN.                  APLANU
                 IF    TO01-CQACT = 002 OR 003                          DOT
           MOVE        'TSA' TO W-TO01-MPLAN.                           APLANU
                 IF    TO01-CQACT = 004                                 DOT
           MOVE        'TSCA' TO W-TO01-MPLAN.                          APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001                                 APLANU
                 AND   TO01-CIRAT = 007                                 APLANU
           MOVE        'EDUC IRA' TO W-TO01-MPLAN.                      APLANU
                 IF    TO01-CQACT = 001                                 DOT
                 AND   TO01-CIRAS = 001
                 AND   TO01-CIRAT = 003
      *PUT SEP'S IN SEPARATE PLAN TYPE
           MOVE        'SEP' TO W-TO01-MPLAN.
       F92EC-FN. EXIT.
       F92EB-FN. EXIT.
      *N92ED.    NOTE *CHECK IF PENSION GROUP             *.            APLANU
       F92ED.    IF    TG15-QITEM > ZEROS                               lv15
                 AND   TG15-GRIDCB (1) = 002                            APLANU
                 NEXT SENTENCE ELSE GO TO     F92ED-FN.                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS WITHIN A   *                                 APLANU
      ** PENSION GROUP, THEN THE      *                                 APLANU
      ** PLAN TYPE IS PENSION.        *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PENSION' TO W-TO01-MPLAN.                       APLANU
      *N92EE.    NOTE *CHECK IF ACCT IS NOT CUSTODIAN     *.            APLANU
       F92EE.    IF    TG15-CIDRP (1) NOT = 01                          lv20
                 NEXT SENTENCE ELSE GO TO     F92EE-FN.                 APLANU
      *                                                                 APLANU
      *********************************                                 APLANU
      ** IF THE ACCOUNT IS NOT A      *                                 APLANU
      ** CUSTODIAN ACCOUNT, THEN THE  *                                 APLANU
      ** PLAN TYPE IS PERSONAL.       *                                 APLANU
      *********************************                                 APLANU
      *                                                                 APLANU
           MOVE        'PERSONAL' TO W-TO01-MPLAN.                      APLANU
       F92EE-FN. EXIT.
       F92ED-FN. EXIT.
       F92BB-FN. EXIT.
       F92-FN.   EXIT.
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
