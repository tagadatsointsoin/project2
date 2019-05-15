       IDENTIFICATION DIVISION.                                         CI0016
       PROGRAM-ID.  CI0016P.                                            CI0016
      *AUTHOR.         M\M - DETR TRANSFER TYPE MOD.                    CI0016
      *DATE-COMPILED.   09/08/14.                                       CI0016
       ENVIRONMENT DIVISION.                                            CI0016
       CONFIGURATION SECTION.                                           CI0016
       SOURCE-COMPUTER. IBM-370.                                        CI0016
       OBJECT-COMPUTER. IBM-370.                                        CI0016
       DATA DIVISION.                                                   CI0016
       WORKING-STORAGE SECTION.                                         CI0016
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
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
      *
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************
      *
       01  W-WORK-MISC.
      *!WI
           05  W-FR01-ICUST
                        PICTURE X.                                      CI0016
      *!WI
           05  W-TO01-ICUST
                        PICTURE X.                                      CI0016
           05  W-WORK-1PTR         PIC 999.
      *
       01  WS01-AMEX-TRUST          PIC X(30) VALUE
           'AMERICAN EXPRESS TRUST COMPANY'.
       01  WS01-AMPF-TRUST          PIC X(24) VALUE
           'AMERIPRISE TRUST COMPANY'.
      *
      *
      *
      *
       01   DEBUT-WSS.                                                  CI0016
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0016
            05   IK     PICTURE X.                                      CI0016
       01  CONSTANTES-PAC.                                              CI0016
           05  FILLER  PICTURE X(87)   VALUE                            CI0016
                     '6015 CAT09/08/14CI0016ADMIN   14:33:56CI0016P AMERCI0016
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0016
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0016
           05  NUGNA   PICTURE X(5).                                    CI0016
           05  APPLI   PICTURE X(3).                                    CI0016
           05  DATGN   PICTURE X(8).                                    CI0016
           05  PROGR   PICTURE X(6).                                    CI0016
           05  CODUTI  PICTURE X(8).                                    CI0016
           05  TIMGN   PICTURE X(8).                                    CI0016
           05  PROGE   PICTURE X(8).                                    CI0016
           05  COBASE  PICTURE X(4).                                    CI0016
           05  DATGNC  PICTURE X(10).                                   CI0016
           05  RELEAS  PICTURE X(7).                                    CI0016
           05  DATGE   PICTURE X(10).                                   CI0016
           05  DATSQ   PICTURE X(10).                                   CI0016
       01  DATCE.                                                       CI0016
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0016
         05  DATOR.                                                     CI0016
           10  DATOA  PICTURE XX.                                       CI0016
           10  DATOM  PICTURE XX.                                       CI0016
           10  DATOJ  PICTURE XX.                                       CI0016
       01   VARIABLES-CONDITIONNELLES.                                  CI0016
            05                  FT      PICTURE X VALUE '0'.            CI0016
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0016
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0016
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0016
       LINKAGE SECTION.                                                 ADU102
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO            *
      **     DETERMINE THE TRANSFER TYPE BETWEEN 2 ACCOUNTS NUMBERS    *
      ******************************************************************
      *
      *!WF DSP=TD DSL=DU SEL=16 FOR=I LEV=1 PLT=05
       01                 TD00.                                         CI0016
          05              TD00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00265).                  CI0016
       01                 TD16  REDEFINES      TD00.                    CI0016
            10            TD16-C299.                                    CI0016
            11            TD16-CTID.                                    CI0016
            12            TD16-CTIDA  PICTURE  9(3).                    CI0016
            12            TD16-CTIDN.                                   CI0016
            13            TD16-CTIDNP PICTURE  X(13).                   CI0016
            13            TD16-CTIDND PICTURE  9(11).                   CI0016
            10            TD16-CTID01.                                  CI0016
            11            TD16-CTIDA1 PICTURE  9(3).                    CI0016
            11            TD16-CTIDN1.                                  CI0016
            12            TD16-CTIDP1 PICTURE  X(13).                   CI0016
            12            TD16-CTIDNA PICTURE  9(11).                   CI0016
            10            TD16-DCACG  PICTURE  9(8).                    CI0016
            10            TD16-IOWNC  PICTURE  X.                       CI0016
            10            TD16-IPOCH  PICTURE  X.                       CI0016
            10            TD16-FILLER PICTURE  X(100).                  CI0016
            10            TD16-CTYPE  PICTURE  X.                       CI0016
            10            TD16-FILLER PICTURE  X(100).                  CI0016
      *
      *
      *
      *
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE "FROM" ACCOUNT CT01 SEGMENT     *
      ******************************************************************
      *
      *!WF DSP=FR DSL=CT SEL=01 FOR=I LEV=1 PLT=10
       01                 FR00.                                         CI0016
          05              FR00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00222).                  CI0016
       01                 FR01  REDEFINES      FR00.                    CI0016
            10            FR01-CT01K.                                   CI0016
            11            FR01-C299.                                    CI0016
            12            FR01-CTID.                                    CI0016
            13            FR01-CTIDA  PICTURE  9(3).                    CI0016
            13            FR01-CTIDN.                                   CI0016
            14            FR01-CTIDNP PICTURE  X(13).                   CI0016
            14            FR01-CTIDND PICTURE  9(11).                   CI0016
            10            FR01-GECKD  PICTURE  9.                       CI0016
            10            FR01-GEMDA  PICTURE  9(8).                    CI0016
            10            FR01-NSEQ4B PICTURE  9(8)                     CI0016
                          BINARY.                                       CI0016
            10            FR01-GECUC  PICTURE  99.                      CI0016
            10            FR01-CTAUL  PICTURE  9(3).                    CI0016
            10            FR01-DIRAC  PICTURE  9(4).                    CI0016
            10            FR01-CTCCI  PICTURE  X.                       CI0016
            10            FR01-CTCUS  PICTURE  999.                     CI0016
            10            FR01-CTEFD  PICTURE  9(8).                    CI0016
            10            FR01-CTIAD  PICTURE  9(8).                    CI0016
            10            FR01-CLCUS  PICTURE  99.                      CI0016
            10            FR01-CAMMB  PICTURE  X(3).                    CI0016
            10            FR01-CKPMM  PICTURE  X.                       CI0016
            10            FR01-CTLAD  PICTURE  9(8).                    CI0016
            10            FR01-IPERS  PICTURE  X.                       CI0016
            10            FR01-AUNCB  PICTURE  S9(7)V99                 CI0016
                          COMPUTATIONAL-3.                              CI0016
            10            FR01-CTLAT  PICTURE  9(8).                    CI0016
            10            FR01-CTLATC PICTURE  9(6).                    CI0016
            10            FR01-IMEGA  PICTURE  X.                       CI0016
            10            FR01-DIRAB  PICTURE  9(8).                    CI0016
            10            FR01-COLRQ  PICTURE  X.                       CI0016
            10            FR01-ZDA04  PICTURE  X(4).                    CI0016
            10            FR01-CTLPD  PICTURE  9(8).                    CI0016
            10            FR01-CIRASP PICTURE  9.                       CI0016
            10            FR01-CIRATP PICTURE  99.                      CI0016
            10            FR01-DRTHC  PICTURE  9(8).                    CI0016
            10            FR01-CPPTC  PICTURE  X.                       CI0016
            10            FR01-ZDA06  PICTURE  X(6).                    CI0016
            10            FR01-CTACD  PICTURE  9(8).                    CI0016
            10            FR01-CTNLI  PICTURE  X.                       CI0016
            10            FR01-CTRHO  PICTURE  9(8).                    CI0016
            10            FR01-CTSGD  PICTURE  9(8).                    CI0016
            10            FR01-CPATP  PICTURE  X(1).                    CI0016
            10            FR01-IRSTA  PICTURE  X.                       CI0016
            10            FR01-CTSTA  PICTURE  99.                      CI0016
            10            FR01-CTSSC  PICTURE  99.                      CI0016
            10            FR01-PRLIN  PICTURE  9(3).                    CI0016
            10            FR01-PRCOD  PICTURE  9(5).                    CI0016
            10            FR01-PRSCD  PICTURE  X(9).                    CI0016
            10            FR01-CTLNI  PICTURE  X.                       CI0016
            10            FR01-AYSIDA PICTURE  9(3).                    CI0016
            10            FR01-AYSID  PICTURE  9(5).                    CI0016
            10            FR01-CTBMC  PICTURE  99.                      CI0016
            10            FR01-CINAR  PICTURE  99.                      CI0016
            10            FR01-CPHTR  PICTURE  X.                       CI0016
            10            FR01-CDSTR  PICTURE  XX.                      CI0016
            10            FR01-CQACT  PICTURE  999.                     CI0016
            10            FR01-CIRAS  PICTURE  999.                     CI0016
            10            FR01-CIRAT  PICTURE  999.                     CI0016
            10            FR01-CLRAY  PICTURE  9(5).                    CI0016
            10            FR01-CATTP  PICTURE  X.                       CI0016
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
       01                 FA00.                                         CI0016
          05              FA00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00407).                  CI0016
       01                 FA04  REDEFINES      FA00.                    CI0016
            10            FA04-C299.                                    CI0016
            11            FA04-CTID.                                    CI0016
            12            FA04-CTIDA  PICTURE  9(3).                    CI0016
            12            FA04-CTIDN.                                   CI0016
            13            FA04-CTIDNP PICTURE  X(13).                   CI0016
            13            FA04-CTIDND PICTURE  9(11).                   CI0016
            10            FA04-IPOCH  PICTURE  X.                       CI0016
            10            FA04-FILLER PICTURE  X(099).                  CI0016
            10            FA04-CTTLN1 PICTURE  X(30).                   CI0016
            10            FA04-CTTLN2 PICTURE  X(30).                   CI0016
            10            FA04-CTTLN3 PICTURE  X(30).                   CI0016
            10            FA04-CTTBO1 PICTURE  X(45).                   CI0016
            10            FA04-CTTBO2 PICTURE  X(45).                   CI0016
            10            FA04-CTOWN  PICTURE  9(3).                    CI0016
            10            FA04-IUGMA  PICTURE  X.                       CI0016
            10            FA04-FILLER PICTURE  X(096).                  CI0016
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
       01                 FC00.                                         CI0016
          05              FC00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00917).                  CI0016
       01                 FC14  REDEFINES      FC00.                    CI0016
            10            FC14-C299.                                    CI0016
            11            FC14-CTID.                                    CI0016
            12            FC14-CTIDA  PICTURE  9(3).                    CI0016
            12            FC14-CTIDN.                                   CI0016
            13            FC14-CTIDNP PICTURE  X(13).                   CI0016
            13            FC14-CTIDND PICTURE  9(11).                   CI0016
            10            FC14-DCACG  PICTURE  9(8).                    CI0016
            10            FC14-IPOCH  PICTURE  X.                       CI0016
            10            FC14-FILLER PICTURE  X(100).                  CI0016
            10            FC14-CLID01.                                  CI0016
            11            FC14-CLIDO1 PICTURE  X(3).                    CI0016
            11            FC14-NCLID1.                                  CI0016
            12            FC14-CLIDP1 PICTURE  X(12).                   CI0016
            12            FC14-CLIDNA PICTURE  9(8).                    CI0016
            10            FC14-CLCTR  PICTURE  9(3).                    CI0016
            10            FC14-DU21                                     CI0016
                          OCCURS       025     TIMES.                   CI0016
            11            FC14-C199.                                    CI0016
            12            FC14-CLID.                                    CI0016
            13            FC14-CLIDO  PICTURE  9(3).                    CI0016
            13            FC14-CLIDN.                                   CI0016
            14            FC14-CLIDNP PICTURE  X(12).                   CI0016
            14            FC14-CLIDND PICTURE  9(8).                    CI0016
            11            FC14-CLCTRC PICTURE  9(3).                    CI0016
            10            FC14-QITEM  PICTURE  9(3).                    CI0016
            10            FC14-XIMAX  PICTURE  S9(4)                    CI0016
                          BINARY.                                       CI0016
            10            FC14-CRROL  PICTURE  X.                       CI0016
            10            FC14-FILLER PICTURE  X(099).                  CI0016
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
       01                 FG00.                                         CI0016
          05              FG00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(04181).                  CI0016
       01                 FG15  REDEFINES      FG00.                    CI0016
            10            FG15-C299.                                    CI0016
            11            FG15-CTID.                                    CI0016
            12            FG15-CTIDA  PICTURE  9(3).                    CI0016
            12            FG15-CTIDN.                                   CI0016
            13            FG15-CTIDNP PICTURE  X(13).                   CI0016
            13            FG15-CTIDND PICTURE  9(11).                   CI0016
            10            FG15-DCACG  PICTURE  9(8).                    CI0016
            10            FG15-IPOCH  PICTURE  X.                       CI0016
            10            FG15-FILLER PICTURE  X(100).                  CI0016
            10            FG15-DU18                                     CI0016
                          OCCURS       010     TIMES.                   CI0016
            11            FG15-CT10.                                    CI0016
            12            FG15-CT10K.                                   CI0016
            13            FG15-GR98.                                    CI0016
            14            FG15-GRID.                                    CI0016
            15            FG15-GRIDC  PICTURE  9(3).                    CI0016
            15            FG15-GRIDN.                                   CI0016
            16            FG15-GRIDNP PICTURE  99.                      CI0016
            16            FG15-GRIDND PICTURE  9(8).                    CI0016
            12            FG15-GR97                                     CI0016
                          REDEFINES            FG15-CT10K.              CI0016
            13            FG15-GRIDCB PICTURE  9(3).                    CI0016
            13            FG15-FILLER PICTURE  X(10).                   CI0016
            12            FG15-GERSD  PICTURE  9(8).                    CI0016
            12            FG15-GERED  PICTURE  9(8).                    CI0016
            12            FG15-GRCSI  PICTURE  X.                       CI0016
            11            FG15-GR01.                                    CI0016
            12            FG15-GR01K.                                   CI0016
            13            FG15-GR98.                                    CI0016
            14            FG15-GRID.                                    CI0016
            15            FG15-GRIDC  PICTURE  9(3).                    CI0016
            15            FG15-GRIDN.                                   CI0016
            16            FG15-GRIDNP PICTURE  99.                      CI0016
            16            FG15-GRIDND PICTURE  9(8).                    CI0016
            12            FG15-GECKD  PICTURE  9.                       CI0016
            12            FG15-GEMDA  PICTURE  9(8).                    CI0016
            12            FG15-NSEQ4B PICTURE  9(8)                     CI0016
                          BINARY.                                       CI0016
            12            FG15-GRDOR  PICTURE  9(8).                    CI0016
            12            FG15-GRIAD  PICTURE  9(8).                    CI0016
            12            FG15-GECUC  PICTURE  99.                      CI0016
            12            FG15-GRLNG  PICTURE  99.                      CI0016
            12            FG15-GESLC  PICTURE  99.                      CI0016
            12            FG15-AYSIDA PICTURE  9(3).                    CI0016
            12            FG15-AYSID  PICTURE  9(5).                    CI0016
            12            FG15-GRCSD  PICTURE  9(8).                    CI0016
            12            FG15-GRCFD  PICTURE  9(8).                    CI0016
            12            FG15-GRNCL  PICTURE  S9(5)                    CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            FG15-GRNCT  PICTURE  S9(5)                    CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            FG15-GRSFC  PICTURE  99.                      CI0016
            12            FG15-GRCRN  PICTURE  9(3).                    CI0016
            12            FG15-GRCSS  PICTURE  X.                       CI0016
            12            FG15-MKSRC  PICTURE  99                       CI0016
                          OCCURS       010     TIMES.                   CI0016
            12            FG15-NEFPS  PICTURE  X(5).                    CI0016
            12            FG15-DEFPS  PICTURE  9(8).                    CI0016
            12            FG15-DLSRV  PICTURE  9(8).                    CI0016
            12            FG15-CTLNI  PICTURE  X.                       CI0016
            12            FG15-CGRLI  PICTURE  X.                       CI0016
            12            FG15-CAMGR  PICTURE  9(5)                     CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            FG15-CAMGS  PICTURE  9(5)                     CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            FG15-CAMGN  PICTURE  9(3)                     CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            FG15-CGRMF  PICTURE  X.                       CI0016
            12            FG15-FILLER PICTURE  X(08).                   CI0016
            11            FG15-GR07.                                    CI0016
            12            FG15-GEDLA  PICTURE  9(8).                    CI0016
            12            FG15-GRAID  PICTURE  X(12).                   CI0016
            12            FG15-GRPAP  PICTURE  X(14).                   CI0016
            12            FG15-GEPHNX PICTURE  9(4).                    CI0016
            12            FG15-DPLEF  PICTURE  9(8).                    CI0016
            12            FG15-DPLAM  PICTURE  9(8).                    CI0016
            12            FG15-NCPFN  PICTURE  9(6).                    CI0016
            12            FG15-GEFYE  PICTURE  9(4).                    CI0016
            12            FG15-FILLER PICTURE  X(06).                   CI0016
            12            FG15-GRPAN  PICTURE  X(45).                   CI0016
            12            FG15-CGRPA  PICTURE  99.                      CI0016
            12            FG15-IPRTT7 PICTURE  X.                       CI0016
            12            FG15-GRPED  PICTURE  9(8).                    CI0016
            12            FG15-FILLER PICTURE  X(05).                   CI0016
            12            FG15-GRPLC  PICTURE  99.                      CI0016
            12            FG15-GRPLT  PICTURE  99.                      CI0016
            12            FG15-FILLER PICTURE  X(04).                   CI0016
            12            FG15-GEADI  PICTURE  X.                       CI0016
            12            FG15-GRCFA  PICTURE  S9(11)V99                CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            FG15-GECFY  PICTURE  9(4).                    CI0016
            12            FG15-GECFC  PICTURE  99.                      CI0016
            12            FG15-MEMPL  PICTURE  X(20).                   CI0016
            12            FG15-CAUNIT PICTURE  X(4).                    CI0016
            12            FG15-FILLER PICTURE  X(21).                   CI0016
            12            FG15-GRPPP  PICTURE  999.                     CI0016
            12            FG15-CCORT  PICTURE  9(3).                    CI0016
            12            FG15-CIDRP  PICTURE  99.                      CI0016
            12            FG15-CCDWA  PICTURE  9.                       CI0016
            12            FG15-IERSA  PICTURE  X.                       CI0016
            12            FG15-DERSA  PICTURE  9(8).                    CI0016
            12            FG15-FILLER PICTURE  X(04).                   CI0016
            10            FG15-QITEM  PICTURE  9(3).                    CI0016
            10            FG15-XIMAX  PICTURE  S9(4)                    CI0016
                          BINARY.                                       CI0016
            10            FG15-FILLER PICTURE  X(100).                  CI0016
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
       01                 TO00.                                         CI0016
          05              TO00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00222).                  CI0016
       01                 TO01  REDEFINES      TO00.                    CI0016
            10            TO01-CT01K.                                   CI0016
            11            TO01-C299.                                    CI0016
            12            TO01-CTID.                                    CI0016
            13            TO01-CTIDA  PICTURE  9(3).                    CI0016
            13            TO01-CTIDN.                                   CI0016
            14            TO01-CTIDNP PICTURE  X(13).                   CI0016
            14            TO01-CTIDND PICTURE  9(11).                   CI0016
            10            TO01-GECKD  PICTURE  9.                       CI0016
            10            TO01-GEMDA  PICTURE  9(8).                    CI0016
            10            TO01-NSEQ4B PICTURE  9(8)                     CI0016
                          BINARY.                                       CI0016
            10            TO01-GECUC  PICTURE  99.                      CI0016
            10            TO01-CTAUL  PICTURE  9(3).                    CI0016
            10            TO01-DIRAC  PICTURE  9(4).                    CI0016
            10            TO01-CTCCI  PICTURE  X.                       CI0016
            10            TO01-CTCUS  PICTURE  999.                     CI0016
            10            TO01-CTEFD  PICTURE  9(8).                    CI0016
            10            TO01-CTIAD  PICTURE  9(8).                    CI0016
            10            TO01-CLCUS  PICTURE  99.                      CI0016
            10            TO01-CAMMB  PICTURE  X(3).                    CI0016
            10            TO01-CKPMM  PICTURE  X.                       CI0016
            10            TO01-CTLAD  PICTURE  9(8).                    CI0016
            10            TO01-IPERS  PICTURE  X.                       CI0016
            10            TO01-AUNCB  PICTURE  S9(7)V99                 CI0016
                          COMPUTATIONAL-3.                              CI0016
            10            TO01-CTLAT  PICTURE  9(8).                    CI0016
            10            TO01-CTLATC PICTURE  9(6).                    CI0016
            10            TO01-IMEGA  PICTURE  X.                       CI0016
            10            TO01-DIRAB  PICTURE  9(8).                    CI0016
            10            TO01-COLRQ  PICTURE  X.                       CI0016
            10            TO01-ZDA04  PICTURE  X(4).                    CI0016
            10            TO01-CTLPD  PICTURE  9(8).                    CI0016
            10            TO01-CIRASP PICTURE  9.                       CI0016
            10            TO01-CIRATP PICTURE  99.                      CI0016
            10            TO01-DRTHC  PICTURE  9(8).                    CI0016
            10            TO01-CPPTC  PICTURE  X.                       CI0016
            10            TO01-ZDA06  PICTURE  X(6).                    CI0016
            10            TO01-CTACD  PICTURE  9(8).                    CI0016
            10            TO01-CTNLI  PICTURE  X.                       CI0016
            10            TO01-CTRHO  PICTURE  9(8).                    CI0016
            10            TO01-CTSGD  PICTURE  9(8).                    CI0016
            10            TO01-CPATP  PICTURE  X(1).                    CI0016
            10            TO01-IRSTA  PICTURE  X.                       CI0016
            10            TO01-CTSTA  PICTURE  99.                      CI0016
            10            TO01-CTSSC  PICTURE  99.                      CI0016
            10            TO01-PRLIN  PICTURE  9(3).                    CI0016
            10            TO01-PRCOD  PICTURE  9(5).                    CI0016
            10            TO01-PRSCD  PICTURE  X(9).                    CI0016
            10            TO01-CTLNI  PICTURE  X.                       CI0016
            10            TO01-AYSIDA PICTURE  9(3).                    CI0016
            10            TO01-AYSID  PICTURE  9(5).                    CI0016
            10            TO01-CTBMC  PICTURE  99.                      CI0016
            10            TO01-CINAR  PICTURE  99.                      CI0016
            10            TO01-CPHTR  PICTURE  X.                       CI0016
            10            TO01-CDSTR  PICTURE  XX.                      CI0016
            10            TO01-CQACT  PICTURE  999.                     CI0016
            10            TO01-CIRAS  PICTURE  999.                     CI0016
            10            TO01-CIRAT  PICTURE  999.                     CI0016
            10            TO01-CLRAY  PICTURE  9(5).                    CI0016
            10            TO01-CATTP  PICTURE  X.                       CI0016
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
       01                 TA00.                                         CI0016
          05              TA00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00407).                  CI0016
       01                 TA04  REDEFINES      TA00.                    CI0016
            10            TA04-C299.                                    CI0016
            11            TA04-CTID.                                    CI0016
            12            TA04-CTIDA  PICTURE  9(3).                    CI0016
            12            TA04-CTIDN.                                   CI0016
            13            TA04-CTIDNP PICTURE  X(13).                   CI0016
            13            TA04-CTIDND PICTURE  9(11).                   CI0016
            10            TA04-IPOCH  PICTURE  X.                       CI0016
            10            TA04-FILLER PICTURE  X(099).                  CI0016
            10            TA04-CTTLN1 PICTURE  X(30).                   CI0016
            10            TA04-CTTLN2 PICTURE  X(30).                   CI0016
            10            TA04-CTTLN3 PICTURE  X(30).                   CI0016
            10            TA04-CTTBO1 PICTURE  X(45).                   CI0016
            10            TA04-CTTBO2 PICTURE  X(45).                   CI0016
            10            TA04-CTOWN  PICTURE  9(3).                    CI0016
            10            TA04-IUGMA  PICTURE  X.                       CI0016
            10            TA04-FILLER PICTURE  X(096).                  CI0016
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
       01                 TC00.                                         CI0016
          05              TC00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00917).                  CI0016
       01                 TC14  REDEFINES      TC00.                    CI0016
            10            TC14-C299.                                    CI0016
            11            TC14-CTID.                                    CI0016
            12            TC14-CTIDA  PICTURE  9(3).                    CI0016
            12            TC14-CTIDN.                                   CI0016
            13            TC14-CTIDNP PICTURE  X(13).                   CI0016
            13            TC14-CTIDND PICTURE  9(11).                   CI0016
            10            TC14-DCACG  PICTURE  9(8).                    CI0016
            10            TC14-IPOCH  PICTURE  X.                       CI0016
            10            TC14-FILLER PICTURE  X(100).                  CI0016
            10            TC14-CLID01.                                  CI0016
            11            TC14-CLIDO1 PICTURE  X(3).                    CI0016
            11            TC14-NCLID1.                                  CI0016
            12            TC14-CLIDP1 PICTURE  X(12).                   CI0016
            12            TC14-CLIDNA PICTURE  9(8).                    CI0016
            10            TC14-CLCTR  PICTURE  9(3).                    CI0016
            10            TC14-DU21                                     CI0016
                          OCCURS       025     TIMES.                   CI0016
            11            TC14-C199.                                    CI0016
            12            TC14-CLID.                                    CI0016
            13            TC14-CLIDO  PICTURE  9(3).                    CI0016
            13            TC14-CLIDN.                                   CI0016
            14            TC14-CLIDNP PICTURE  X(12).                   CI0016
            14            TC14-CLIDND PICTURE  9(8).                    CI0016
            11            TC14-CLCTRC PICTURE  9(3).                    CI0016
            10            TC14-QITEM  PICTURE  9(3).                    CI0016
            10            TC14-XIMAX  PICTURE  S9(4)                    CI0016
                          BINARY.                                       CI0016
            10            TC14-CRROL  PICTURE  X.                       CI0016
            10            TC14-FILLER PICTURE  X(099).                  CI0016
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
       01                 TG00.                                         CI0016
          05              TG00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(04181).                  CI0016
       01                 TG15  REDEFINES      TG00.                    CI0016
            10            TG15-C299.                                    CI0016
            11            TG15-CTID.                                    CI0016
            12            TG15-CTIDA  PICTURE  9(3).                    CI0016
            12            TG15-CTIDN.                                   CI0016
            13            TG15-CTIDNP PICTURE  X(13).                   CI0016
            13            TG15-CTIDND PICTURE  9(11).                   CI0016
            10            TG15-DCACG  PICTURE  9(8).                    CI0016
            10            TG15-IPOCH  PICTURE  X.                       CI0016
            10            TG15-FILLER PICTURE  X(100).                  CI0016
            10            TG15-DU18                                     CI0016
                          OCCURS       010     TIMES.                   CI0016
            11            TG15-CT10.                                    CI0016
            12            TG15-CT10K.                                   CI0016
            13            TG15-GR98.                                    CI0016
            14            TG15-GRID.                                    CI0016
            15            TG15-GRIDC  PICTURE  9(3).                    CI0016
            15            TG15-GRIDN.                                   CI0016
            16            TG15-GRIDNP PICTURE  99.                      CI0016
            16            TG15-GRIDND PICTURE  9(8).                    CI0016
            12            TG15-GR97                                     CI0016
                          REDEFINES            TG15-CT10K.              CI0016
            13            TG15-GRIDCB PICTURE  9(3).                    CI0016
            13            TG15-FILLER PICTURE  X(10).                   CI0016
            12            TG15-GERSD  PICTURE  9(8).                    CI0016
            12            TG15-GERED  PICTURE  9(8).                    CI0016
            12            TG15-GRCSI  PICTURE  X.                       CI0016
            11            TG15-GR01.                                    CI0016
            12            TG15-GR01K.                                   CI0016
            13            TG15-GR98.                                    CI0016
            14            TG15-GRID.                                    CI0016
            15            TG15-GRIDC  PICTURE  9(3).                    CI0016
            15            TG15-GRIDN.                                   CI0016
            16            TG15-GRIDNP PICTURE  99.                      CI0016
            16            TG15-GRIDND PICTURE  9(8).                    CI0016
            12            TG15-GECKD  PICTURE  9.                       CI0016
            12            TG15-GEMDA  PICTURE  9(8).                    CI0016
            12            TG15-NSEQ4B PICTURE  9(8)                     CI0016
                          BINARY.                                       CI0016
            12            TG15-GRDOR  PICTURE  9(8).                    CI0016
            12            TG15-GRIAD  PICTURE  9(8).                    CI0016
            12            TG15-GECUC  PICTURE  99.                      CI0016
            12            TG15-GRLNG  PICTURE  99.                      CI0016
            12            TG15-GESLC  PICTURE  99.                      CI0016
            12            TG15-AYSIDA PICTURE  9(3).                    CI0016
            12            TG15-AYSID  PICTURE  9(5).                    CI0016
            12            TG15-GRCSD  PICTURE  9(8).                    CI0016
            12            TG15-GRCFD  PICTURE  9(8).                    CI0016
            12            TG15-GRNCL  PICTURE  S9(5)                    CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            TG15-GRNCT  PICTURE  S9(5)                    CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            TG15-GRSFC  PICTURE  99.                      CI0016
            12            TG15-GRCRN  PICTURE  9(3).                    CI0016
            12            TG15-GRCSS  PICTURE  X.                       CI0016
            12            TG15-MKSRC  PICTURE  99                       CI0016
                          OCCURS       010     TIMES.                   CI0016
            12            TG15-NEFPS  PICTURE  X(5).                    CI0016
            12            TG15-DEFPS  PICTURE  9(8).                    CI0016
            12            TG15-DLSRV  PICTURE  9(8).                    CI0016
            12            TG15-CTLNI  PICTURE  X.                       CI0016
            12            TG15-CGRLI  PICTURE  X.                       CI0016
            12            TG15-CAMGR  PICTURE  9(5)                     CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            TG15-CAMGS  PICTURE  9(5)                     CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            TG15-CAMGN  PICTURE  9(3)                     CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            TG15-CGRMF  PICTURE  X.                       CI0016
            12            TG15-FILLER PICTURE  X(08).                   CI0016
            11            TG15-GR07.                                    CI0016
            12            TG15-GEDLA  PICTURE  9(8).                    CI0016
            12            TG15-GRAID  PICTURE  X(12).                   CI0016
            12            TG15-GRPAP  PICTURE  X(14).                   CI0016
            12            TG15-GEPHNX PICTURE  9(4).                    CI0016
            12            TG15-DPLEF  PICTURE  9(8).                    CI0016
            12            TG15-DPLAM  PICTURE  9(8).                    CI0016
            12            TG15-NCPFN  PICTURE  9(6).                    CI0016
            12            TG15-GEFYE  PICTURE  9(4).                    CI0016
            12            TG15-FILLER PICTURE  X(06).                   CI0016
            12            TG15-GRPAN  PICTURE  X(45).                   CI0016
            12            TG15-CGRPA  PICTURE  99.                      CI0016
            12            TG15-IPRTT7 PICTURE  X.                       CI0016
            12            TG15-GRPED  PICTURE  9(8).                    CI0016
            12            TG15-FILLER PICTURE  X(05).                   CI0016
            12            TG15-GRPLC  PICTURE  99.                      CI0016
            12            TG15-GRPLT  PICTURE  99.                      CI0016
            12            TG15-FILLER PICTURE  X(04).                   CI0016
            12            TG15-GEADI  PICTURE  X.                       CI0016
            12            TG15-GRCFA  PICTURE  S9(11)V99                CI0016
                          COMPUTATIONAL-3.                              CI0016
            12            TG15-GECFY  PICTURE  9(4).                    CI0016
            12            TG15-GECFC  PICTURE  99.                      CI0016
            12            TG15-MEMPL  PICTURE  X(20).                   CI0016
            12            TG15-CAUNIT PICTURE  X(4).                    CI0016
            12            TG15-FILLER PICTURE  X(21).                   CI0016
            12            TG15-GRPPP  PICTURE  999.                     CI0016
            12            TG15-CCORT  PICTURE  9(3).                    CI0016
            12            TG15-CIDRP  PICTURE  99.                      CI0016
            12            TG15-CCDWA  PICTURE  9.                       CI0016
            12            TG15-IERSA  PICTURE  X.                       CI0016
            12            TG15-DERSA  PICTURE  9(8).                    CI0016
            12            TG15-FILLER PICTURE  X(04).                   CI0016
            10            TG15-QITEM  PICTURE  9(3).                    CI0016
            10            TG15-XIMAX  PICTURE  S9(4)                    CI0016
                          BINARY.                                       CI0016
            10            TG15-FILLER PICTURE  X(100).                  CI0016
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
       01                 MS00.                                         CI0016
          05              MS00-SUITE.                                   CI0016
            15       FILLER         PICTURE  X(00542).                  CI0016
       01                 MS03  REDEFINES      MS00.                    CI0016
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0016
                          COMPUTATIONAL-3.                              CI0016
            10            MS03-CMSSF  PICTURE  XX.                      CI0016
            10            MS03-DU09.                                    CI0016
            11            MS03-CMESA  PICTURE  S9(9)                    CI0016
                          BINARY.                                       CI0016
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0016
                          BINARY.                                       CI0016
            11            MS03-CMESB  PICTURE  S9(9)                    CI0016
                          BINARY.                                       CI0016
            11            MS03-CMSST  PICTURE  S9(9)                    CI0016
                          BINARY.                                       CI0016
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0016
                          BINARY.                                       CI0016
            11            MS03-QELLAA PICTURE  S9(9)                    CI0016
                          BINARY.                                       CI0016
            11            MS03-TMESS4 PICTURE  X(512).                  CI0016
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0016
            10            MX11-QMSGS  PICTURE  9(03).                   CI0016
            10            MX11-PJ09                                     CI0016
                          OCCURS       025     TIMES.                   CI0016
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0016
                          COMPUTATIONAL-3.                              CI0016
            11            MX11-CMESB  PICTURE  S9(9)                    CI0016
                          BINARY.                                       CI0016
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                TD16
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
      *N01.      NOTE *************************************.            CI0016
      *               *                                   *             CI0016
      *               *INITIALISATIONS                    *             CI0016
      *               *                                   *             CI0016
      *               *************************************.            CI0016
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0016
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0016
      *               *                                   *             CI0016
      *               *FIN DE TRAITEMENT                  *             CI0016
      *               *                                   *             CI0016
      *               *************************************.            CI0016
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0016
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE IF TRANSFER              *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** DETERMINE IF THE TRANSFER    *
      ** TYPE IS TRANSFER.            *
      ** TRANSFER OCCURS WHEN THE     *
      ** ADMIN CODES FOR THE 2 ACCOUNT*
      ** NUMBERS ARE THE SAME AND THE *
      ** PRODUCT CODES FOR THE 2      *
      ** ACCOUNT NUMBERS ARE THE      *
      ** SAME.                        *
      *********************************
      *
      *N50BA.    NOTE *CHECK IF SAME ADMIN CODE           *.
       F50BA.    IF    FR01-CTIDA = TO01-CTIDA                          lv10
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *
      *********************************
      ** CHECK IF THE 2 ACCOUNT       *
      ** NUMBERS HAVE THE SAME ADMIN  *
      ** CODE.                        *
      *********************************
      *
      *N50CA.    NOTE *CHECK IF SAME PRODUCT CODE         *.
       F50CA.    IF    FR01-PRCOD = TO01-PRCOD                          lv15
                 NEXT SENTENCE ELSE GO TO     F50CA-FN.
      *
      *********************************
      ** CHECK IF THE 2 ACCOUNT       *
      ** NUMBERS HAVE THE SAME PRODUCT*
      ** CODE.                        *
      *********************************
      *
      *N50DA.    NOTE *TRANSFER TYPE IS A TRANSFER        *.
       F50DA.                                                           lv20
      *
      *********************************
      ** THE TRANSFER TYPE IS A       *
      ** TRANSFER.                    *
      *********************************
      *
           MOVE        'T' TO TD16-CTYPE.
       F50DA-FN. EXIT.
      *N50ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F50ZA.                                                           lv20
      *
      *********************************
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F50ZA-FN. EXIT.
       F50CA-FN. EXIT.
       F50BA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE IF DOUBLE TRANSFER       *
      *               *                                   *
      *               *************************************.
       F55.                                                             lv05
      *
      *********************************
      ** DETERMINE IF THE TRANSFER    *
      ** TYPE IS A DOUBLE TRANSFER.   *
      ** DOUBLE TRANSFER OCCURS WHEN  *
      ** ADMIN CODES FOR THE 2 ACCOUNT*
      ** NUMBERS ARE NOT EQUAL OR THE *
      ** PRODUCT CODES FOR THE 2      *
      ** ACCOUNT NUMBERS ARE NOT      *
      ** EQUAL AND DIFFERENT OWNERSHIP*
      ** EXCEPTION:                   *
      **  IF THE 'FROM' ACCOUNT IS A  *
      **  CERTS OR FUNDS AND THE 'TO' *
      **  ACCOUNT IS CERTS,FUNDS OR   *
      **  LIFE AND THE SAME TAXPAYER  *
      **  CLIENT ID AND 'FROM' ACCT   *
      **  IS CUSTODIAN AND 'TO' ACCT  *
      **  IS NOT CUSTODIAN AND 'FROM' *
      **  ACCT IS IRA AND 1ST         *
      **  OWNERSHIP LINES ARE THE     *
      **  SAME, CHANGE FROM A DOUBLE  *
      **  TRANSFER TO AN EXCHANGE.    *
      *********************************
      *
      *N55BA.    NOTE *CHECK IF DIFF ADMIN, PROD CODE     *.
       F55BA.    IF    FR01-CTIDA NOT = TO01-CTIDA                      lv10
                 OR    FR01-PRCOD NOT = TO01-PRCOD
                 NEXT SENTENCE ELSE GO TO     F55BA-FN.
      *********************************
      ** CHECK IF THE 2 ACCOUNT       *
      ** NUMBERS HAVE DIFFERENT ADMIN *
      ** OR PRODUCT CODES             *
      *********************************
      *
      *N55CA.    NOTE *CHECK IF DIFFERENT OWNERSHIP       *.
       F55CA.    IF    TD16-IOWNC = 'N'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F55CA-FN.
      *
      *********************************
      ** CHECK IF THE 2 ACCOUNT       *
      ** NUMBERS HAVE DIFFERENT       *
      ** OWNERSHIPS.                  *
      *********************************
      *
      *N55DA.    NOTE *TRANSFER TYPE IS A DOUBLE TFR      *.
       F55DA.                                                           lv20
      *
      *********************************
      ** THE TRANSFER TYPE IS A       *
      ** DOUBLE TRANSFER.             *
      *********************************
      *
           MOVE        'D' TO TD16-CTYPE.
       F55DA-FN. EXIT.
      *N55KA.    NOTE *CHANGE TO AN EXCHANGE?             *.
       F55KA.                                                           lv20
      *
      *********************************
      ** CHECK TO SEE IF NEED TO      *
      ** CHANGE THE DOUBLE TRANSFER   *
      ** TO AN EXCHANGE.              *
      *********************************
      *
      *N55LA.    NOTE *'FROM' ACCT CERTS OR FUNDS         *.
       F55LA.    IF    FR01-CTIDA = 001 OR 002                          lv25
                 NEXT SENTENCE ELSE GO TO     F55LA-FN.
      *
      *********************************
      ** 'FROM' ACCOUNT IS A CERTS OR *
      ** A FUNDS ACCOUNT.             *
      *********************************
      *
      *N55MA.    NOTE *'TO' ACCT CERTS,FUNDS OR LIFE      *.
       F55MA.    IF    TO01-CTIDA = 001 OR 002 OR                       lv30
                       004 OR 005
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.
      *********************************
      ** 'TO' ACCOUNT IS CERTS, FUNDS *
      ** OR A LIFE ACCOUNT            *
      *********************************
      *
      *N55NA.    NOTE *TAXPAYER CLIENTS THE SAME          *.
       F55NA.    IF    FC14-CLID01 = TC14-CLID01                        lv35
                 NEXT SENTENCE ELSE GO TO     F55NA-FN.
      *
      *********************************
      ** CHECK IF THE TAXPAYER CLIENTS*
      ** ARE THE SAME BETWEEN THE     *
      ** 'FROM' AND 'TO' ACCOUNT.     *
      *********************************
      *
      *N55OA.    NOTE *DETERMINE CUST FOR FROM/TO ACCT    *.
       F55OA.                                                           lv40
      *
      *********************************
      ** DETERMINE IF IDS IS THE      *
      ** CUSTODIAN FOR BOTH THE 'FROM'*
      ** AND 'TO' ACCOUNTS            *
      *********************************
      *
      *N55OC.    NOTE *INITIALIZE INDICATORS              *.
       F55OC.                                                           lv45
      *
      *********************************
      ** INITIALIZE THE CUSTODIAN     *
      ** INDICATORS FOR BOTH THE      *
      ** 'FROM' AND 'TO' ACCOUNTS     *
      *********************************
      *
           MOVE        'N' TO W-FR01-ICUST
           W-TO01-ICUST.
       F55OC-FN. EXIT.
      *N55OF.    NOTE *IDS CUSTODIAN FOR FROM ACCOUNT     *.
       F55OF.    IF    (FG15-QITEM > ZEROS)                             lv45
                 AND   (FG15-GRIDCB (1) = 001
                 AND   FR01-CTCCI = '1')
                 OR    (FG15-GRIDCB (1) = 002
                 AND   FG15-CIDRP (1) = 01)
                 NEXT SENTENCE ELSE GO TO     F55OF-FN.
      *********************************
      ** IF GROUPS WERE FOUND FOR THE *
      ** 'FROM' ACCOUNT IN CI0019 AND *
      ** HOUSEHOLD GROUP AND IDS ROLE *
      ** TO ACCOUNT CODE IS CUSTODIAN *
      ** OR PENSION GROUP AND IDS ROLE*
      ** TO PLAN CODE IS CUSTODIAN,   *
      ** THEN IDS IS CUSTODIAN FOR    *
      ** 'FROM' ACCOUNT.              *
      *********************************
      *
           MOVE        'Y' TO W-FR01-ICUST.
       F55OF-FN. EXIT.
      *N55OT.    NOTE *IDS CUSTODIAN FOR TO ACCOUNT       *.
       F55OT.    IF    (TG15-QITEM > ZEROS)                             lv45
                 AND   (TG15-GRIDCB (1) = 001
                 AND   TO01-CTCCI = '1')
                 OR    (TG15-GRIDCB (1) = 002
                 AND   TG15-CIDRP (1) = 01)
                 NEXT SENTENCE ELSE GO TO     F55OT-FN.
      *********************************
      ** IF GROUPS WERE FOUND FOR THE *
      ** 'TO' ACCOUNT IN CI0019 AND   *
      ** HOUSEHOLD GROUP AND IDS ROLE *
      ** TO ACCOUNT CODE IS CUSTODIAN *
      ** OR PENSION GROUP AND IDS ROLE*
      ** TO PLAN CODE IS CUSTODIAN,   *
      ** THEN IDS IS CUSTODIAN FOR    *
      ** 'TO' ACCOUNT.                *
      *********************************
      *
           MOVE        'Y' TO W-TO01-ICUST.
       F55OT-FN. EXIT.
       F55OA-FN. EXIT.
      *N55PA.    NOTE *CHECK FROM/TO ACCT FOR CUSTODIAN   *.
       F55PA.    IF    W-FR01-ICUST = 'Y'                               lv40
                 AND   W-TO01-ICUST = 'N'
                 NEXT SENTENCE ELSE GO TO     F55PA-FN.
      *********************************
      ** CHECK TO SEE IF IDS IS       *
      ** CUSTODIAN FOR THE 'FROM'     *
      ** ACCOUNT BUT NOT CUSTODIAN FOR*
      ** THE 'TO' ACCOUNT.            *
      *********************************
      *
      *N55QA.    NOTE *CHECK IF 'FROM' ACCT IS IRA        *.
       F55QA.    IF    FR01-CQACT = 001                                 lv45
                 AND   TO01-CQACT = 001
                 NEXT SENTENCE ELSE GO TO     F55QA-FN.
      *********************************
      ** CHECK TO SEE IF 'FROM'       *
      ** ACCOUNT IS AN IRA.           *
      *********************************
      *
      *N55RA.    NOTE *CHECK FROM/TO ONWERSHIP CODE       *.
       F55RA.    IF    FA04-CTOWN NOT = 001 AND 002                     lv50
                 AND   TA04-CTOWN NOT = 001 AND 002
                       AND 007
                 NEXT SENTENCE ELSE GO TO     F55RA-FN.
      *********************************
      ** CHECK FROM/TO OWNERSHIP CODE *
      *********************************
      *
      *N55SA.    NOTE *CHECK IF CUSTODIAL ACCOUNT         *.            AMCOWL
       F55SA.    IF    FA04-CTTLN1 =                                    lv55
                       'IDS TRUST COMPANY'                              AMCOWL
                 OR    'ADVISORY BANK & TRUST'                          AMCOWL
                 OR    'IDS BANK & TRUST'                               AMCOWL
                 OR    WS01-AMEX-TRUST
                 OR    WS01-AMPF-TRUST
                 NEXT SENTENCE ELSE GO TO     F55SA-FN.
      *********************************                                 AMCOWL
      ** CHECK THE FIRST OWNERSHIP    *                                 AMCOWL
      **                              *
      **                              *
      ** LINE TO SEE IF IT IS A       *                                 AMCOWL
      ** CUSTODIAL ACCOUNT.           *                                 AMCOWL
      *********************************                                 AMCOWL
      *                                                                 AMCOWL
      *N55SM.    NOTE *REFORMAT OWNERSHIP LINES           *.            AMCOWL
       F55SM.                                                           lv60
      *                                                                 AMCOWL
      *********************************                                 AMCOWL
      ** STRIP OFF 'C/O' FROM THE     *                                 AMCOWL
      ** SECOND LINE AND MOVE TO FIRST*                                 AMCOWL
      ** LINE.  MOVE THIRD LINE TO    *                                 AMCOWL
      ** SECOND LINE.                 *                                 AMCOWL
      *********************************                                 AMCOWL
      *                                                                 AMCOWL
           MOVE        SPACES TO FA04-CTTLN1                            AMCOWL
           MOVE        5 TO W-WORK-1PTR                                 AMCOWL
           UNSTRING        FA04-CTTLN2                                  AMCOWL
              INTO         FA04-CTTLN1                                  AMCOWL
              WITH POINTER W-WORK-1PTR                                  AMCOWL
           MOVE        FA04-CTTLN3 TO FA04-CTTLN2                       AMCOWL
           MOVE        SPACES TO FA04-CTTLN3.                           AMCOWL
       F55SM-FN. EXIT.
       F55SA-FN. EXIT.
      *N55TA.    NOTE *CHECK OWNERSHIP LINE IS THE SAME   *.
       F55TA.    IF    FA04-CTTLN1 = TA04-CTTLN1                        lv55
                 NEXT SENTENCE ELSE GO TO     F55TA-FN.
      *
      *********************************
      ** CHECK IF THE FIRST OWNERSHIP *
      ** LINES ARE THE SAME.          *
      *********************************
      *
      *N55UA.    NOTE *CHANGE DOUBLE TRANSFER TO EXCHGE   *.
       F55UA.                                                           lv60
      *
      *********************************
      ** CHANGE THE DOUBLE TRANSFER   *
      ** TO AN EXCHANGE.              *
      *********************************
      *
           MOVE        'E' TO TD16-CTYPE.
       F55UA-FN. EXIT.
       F55TA-FN. EXIT.
       F55RA-FN. EXIT.
       F55QA-FN. EXIT.
       F55PA-FN. EXIT.
       F55NA-FN. EXIT.
       F55MA-FN. EXIT.
       F55LA-FN. EXIT.
       F55KA-FN. EXIT.
      *N55ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F55ZA.                                                           lv20
      *
      *********************************
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F55ZA-FN. EXIT.
       F55CA-FN. EXIT.
       F55BA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE IF AN EXCHANGE           *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *
      *********************************
      ** DETERMINE IF THE TRANSFER    *
      ** TYPE IS AN EXCHANGE. EXCHANGE*
      ** OCCURS WHEN ADMIN CODES FOR  *
      ** THE 2 ACCOUNT NUMBERS ARE    *
      ** NOT EQUAL OR THE PRODUCT     *
      ** CODES FOR THE 2 ACCOUNT      *
      ** NUMBERS ARE NOT EQUAL AND    *
      ** THE SAME OWNERSHIP.          *
      *********************************
      *
      *N60BA.    NOTE *CHECK IF DIFF ADMIN, PROD CODE     *.
       F60BA.    IF    FR01-CTIDA NOT = TO01-CTIDA                      lv10
                 OR    FR01-PRCOD NOT = TO01-PRCOD
                 NEXT SENTENCE ELSE GO TO     F60BA-FN.
      *********************************
      ** CHECK IF THE 2 ACCOUNT       *
      ** NUMBERS HAVE DIFFERENT ADMIN *
      ** OR PRODUCT CODES             *
      *********************************
      *
      *N60CA.    NOTE *CHECK IF THE SAME OWNERSHIPP       *.
       F60CA.    IF    TD16-IOWNC = 'Y'                                 lv15
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
      *
      *********************************
      ** CHECK IF THE 2 ACCOUNT       *
      ** NUMBERS HAVE THE OWNERSHIP   *
      *********************************
      *
      *N60DA.    NOTE *TRANSFER TYPE IS AN EXCHANGE       *.
       F60DA.                                                           lv20
      *
      *********************************
      ** THE TRANSFER TYPE IS AN      *
      ** EXCHANGE.                    *
      *********************************
      *
           MOVE        'E' TO TD16-CTYPE.
       F60DA-FN. EXIT.
      *N60ZA.    NOTE *RETURN TO CALLING MODULE           *.
       F60ZA.                                                           lv20
      *
      *********************************
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F60ZA-FN. EXIT.
       F60CA-FN. EXIT.
       F60BA-FN. EXIT.
       F60-FN.   EXIT.
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
