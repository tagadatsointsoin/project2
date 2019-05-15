       IDENTIFICATION DIVISION.                                         CI0027
       PROGRAM-ID.  CI0027P.                                            CI0027
      *AUTHOR.         M\M - GET ACCT CT22 SEGMENTS.                    CI0027
      *DATE-COMPILED.   09/08/14.                                       CI0027
       ENVIRONMENT DIVISION.                                            CI0027
       CONFIGURATION SECTION.                                           CI0027
       SOURCE-COMPUTER. IBM-370.                                        CI0027
       OBJECT-COMPUTER. IBM-370.                                        CI0027
       DATA DIVISION.                                                   CI0027
       WORKING-STORAGE SECTION.                                         CI0027
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
       01                 CT00.                                         CI0027
            02            CT01.                                         CI0027
            10            CT01-CT01K.                                   CI0027
            11            CT01-C299.                                    CI0027
            12            CT01-CTID.                                    CI0027
            13            CT01-CTIDA  PICTURE  9(3).                    CI0027
            13            CT01-CTIDN.                                   CI0027
            14            CT01-CTIDNP PICTURE  X(13).                   CI0027
            14            CT01-CTIDND PICTURE  9(11).                   CI0027
            10            CT01-GECKD  PICTURE  9.                       CI0027
            10            CT01-GEMDA  PICTURE  9(8).                    CI0027
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0027
                          BINARY.                                       CI0027
            10            CT01-GECUC  PICTURE  99.                      CI0027
            10            CT01-CTAUL  PICTURE  9(3).                    CI0027
            10            CT01-DIRAC  PICTURE  9(4).                    CI0027
            10            CT01-CTCCI  PICTURE  X.                       CI0027
            10            CT01-CTCUS  PICTURE  999.                     CI0027
            10            CT01-CTEFD  PICTURE  9(8).                    CI0027
            10            CT01-CTIAD  PICTURE  9(8).                    CI0027
            10            CT01-CLCUS  PICTURE  99.                      CI0027
            10            CT01-CAMMB  PICTURE  X(3).                    CI0027
            10            CT01-CKPMM  PICTURE  X.                       CI0027
            10            CT01-CTLAD  PICTURE  9(8).                    CI0027
            10            CT01-IPERS  PICTURE  X.                       CI0027
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0027
                          COMPUTATIONAL-3.                              CI0027
            10            CT01-CTLAT  PICTURE  9(8).                    CI0027
            10            CT01-CTLATC PICTURE  9(6).                    CI0027
            10            CT01-IMEGA  PICTURE  X.                       CI0027
            10            CT01-DIRAB  PICTURE  9(8).                    CI0027
            10            CT01-COLRQ  PICTURE  X.                       CI0027
            10            CT01-ZDA04  PICTURE  X(4).                    CI0027
            10            CT01-CTLPD  PICTURE  9(8).                    CI0027
            10            CT01-CIRASP PICTURE  9.                       CI0027
            10            CT01-CIRATP PICTURE  99.                      CI0027
            10            CT01-DRTHC  PICTURE  9(8).                    CI0027
            10            CT01-CPPTC  PICTURE  X.                       CI0027
            10            CT01-ZDA06  PICTURE  X(6).                    CI0027
            10            CT01-CTACD  PICTURE  9(8).                    CI0027
            10            CT01-CTNLI  PICTURE  X.                       CI0027
            10            CT01-CTRHO  PICTURE  9(8).                    CI0027
            10            CT01-CTSGD  PICTURE  9(8).                    CI0027
            10            CT01-CPATP  PICTURE  X(1).                    CI0027
            10            CT01-IRSTA  PICTURE  X.                       CI0027
            10            CT01-CTSTA  PICTURE  99.                      CI0027
            10            CT01-CTSSC  PICTURE  99.                      CI0027
            10            CT01-PRLIN  PICTURE  9(3).                    CI0027
            10            CT01-PRCOD  PICTURE  9(5).                    CI0027
            10            CT01-PRSCD  PICTURE  X(9).                    CI0027
            10            CT01-CTLNI  PICTURE  X.                       CI0027
            10            CT01-AYSIDA PICTURE  9(3).                    CI0027
            10            CT01-AYSID  PICTURE  9(5).                    CI0027
            10            CT01-CTBMC  PICTURE  99.                      CI0027
            10            CT01-CINAR  PICTURE  99.                      CI0027
            10            CT01-CPHTR  PICTURE  X.                       CI0027
            10            CT01-CDSTR  PICTURE  XX.                      CI0027
            10            CT01-CQACT  PICTURE  999.                     CI0027
            10            CT01-CIRAS  PICTURE  999.                     CI0027
            10            CT01-CIRAT  PICTURE  999.                     CI0027
            10            CT01-CLRAY  PICTURE  9(5).                    CI0027
            10            CT01-CATTP  PICTURE  X.                       CI0027
            02            CT22.                                         CI0027
            10            CT22-CT22K.                                   CI0027
            11            CT22-CGVEN  PICTURE  X(2).                    CI0027
            11            CT22-CTWHC  PICTURE  9(2).                    CI0027
            10            CT22-CFCNTY PICTURE  X(2).                    CI0027
            10            CT22-DLAUP  PICTURE  9(8).                    CI0027
            10            CT22-CTWTC  PICTURE  9(2).                    CI0027
            10            CT22-CTWHAT PICTURE  S9(7)V99                 CI0027
                          COMPUTATIONAL-3.                              CI0027
            10            CT22-CTWHP  PICTURE  9(3)V99                  CI0027
                          COMPUTATIONAL-3.                              CI0027
            10            CT22-FILLER PICTURE  X(06).                   CI0027
      ******************************************************************ADU029
      ***  STORAGE AREAS FOR DL1                                        ADU029
      ******************************************************************ADU029
      ***  DL1 FUNCTIONS                                                ADU029
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU029
       01                 XW05.                                         CI0027
            10            XW05-XW06.                                    CI0027
            11            XW05-XDBPCB.                                  CI0027
            12            XW05-XDBDNM PICTURE  X(08)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-XSEGLV PICTURE  X(02)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-XRC    PICTURE  X(02)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-XPROPT PICTURE  X(04)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-FILLER PICTURE  S9(5)                    CI0027
                          VALUE                ZERO                     CI0027
                          BINARY.                                       CI0027
            12            XW05-XSEGNM PICTURE  X(08)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0027
                          VALUE                ZERO                     CI0027
                          BINARY.                                       CI0027
            12            XW05-XSEGNB PICTURE  9(05)                    CI0027
                          VALUE                ZERO                     CI0027
                          BINARY.                                       CI0027
            12            XW05-XCOKEY PICTURE  X(70)                    CI0027
                          VALUE                SPACE.                   CI0027
            10            XW05-XW07.                                    CI0027
            11            XW05-XIOPCB.                                  CI0027
            12            XW05-XTERMI PICTURE  X(08)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-FILLER PICTURE  XX                       CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-XRC1   PICTURE  X(02)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-FILLER PICTURE  X(12)                    CI0027
                          VALUE                SPACE.                   CI0027
            12            XW05-XMODNM PICTURE  X(8)                     CI0027
                          VALUE                SPACE.                   CI0027
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0027
                          VALUE                ZERO.                    CI0027
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0027
                          VALUE                ZERO.                    CI0027
            10            XW05-XGU    PICTURE  X(4)                     CI0027
                          VALUE                'GU  '.                  CI0027
            10            XW05-XGHU   PICTURE  X(4)                     CI0027
                          VALUE                'GHU '.                  CI0027
            10            XW05-XGN    PICTURE  X(4)                     CI0027
                          VALUE                'GN  '.                  CI0027
            10            XW05-XGHN   PICTURE  X(4)                     CI0027
                          VALUE                'GHN '.                  CI0027
            10            XW05-XGNP   PICTURE  X(4)                     CI0027
                          VALUE                'GNP '.                  CI0027
            10            XW05-XGHNP  PICTURE  X(4)                     CI0027
                          VALUE                'GHNP'.                  CI0027
            10            XW05-XREPL  PICTURE  XXXX                     CI0027
                          VALUE                'REPL'.                  CI0027
            10            XW05-XISRT  PICTURE  X(4)                     CI0027
                          VALUE                'ISRT'.                  CI0027
            10            XW05-XDLET  PICTURE  X(4)                     CI0027
                          VALUE                'DLET'.                  CI0027
            10            XW05-XOPEN  PICTURE  X(4)                     CI0027
                          VALUE                'OPEN'.                  CI0027
            10            XW05-XCLSE  PICTURE  X(4)                     CI0027
                          VALUE                'CLSE'.                  CI0027
            10            XW05-XCHKP  PICTURE  X(4)                     CI0027
                          VALUE                'CHKP'.                  CI0027
            10            XW05-XXRST  PICTURE  X(4)                     CI0027
                          VALUE                'XRST'.                  CI0027
            10            XW05-XTERM  PICTURE  X(4)                     CI0027
                          VALUE                'TERM'.                  CI0027
            10            XW05-XNFPAC PICTURE  X(13)                    CI0027
                          VALUE                SPACE.                   CI0027
      *!WI pl=DL200                                                     ADU029
       01  DL01-KFPCB                                                   ADU029
                        PICTURE X(04)                                   CI0027
              VALUE 'PCB '.                                             ADU029
      ***  SAVE AREA FOR DL1 FUNCTION VALUE - USED FOR ERROR PROCESSING ADU029
       01  SV01-FUNC     PIC X(4).                                      ADU029
      ******************************************************************ADU029
      *** USED WHEN DYNAMICALLY CALLING PCB & UIB ERROR CHECKING MODULESADU029
      ***    (CI0008P - UIB ERROR CHECK    CI0009P - PCB ERROR CHECK)   ADU029
      ******************************************************************ADU029
      *!WI pl=DN100                                                     ADU029
       01  W-PASS-XPROGR                                                ADU029
                        PICTURE X(8).                                   CI0027
       01   DEBUT-WSS.                                                  CI0027
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0027
            05   IK     PICTURE X.                                      CI0027
       01  CONSTANTES-PAC.                                              CI0027
           05  FILLER  PICTURE X(87)   VALUE                            CI0027
                     '6015 CAT09/08/14CI0027ADMIN   14:34:19CI0027P AMERCI0027
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0027
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0027
           05  NUGNA   PICTURE X(5).                                    CI0027
           05  APPLI   PICTURE X(3).                                    CI0027
           05  DATGN   PICTURE X(8).                                    CI0027
           05  PROGR   PICTURE X(6).                                    CI0027
           05  CODUTI  PICTURE X(8).                                    CI0027
           05  TIMGN   PICTURE X(8).                                    CI0027
           05  PROGE   PICTURE X(8).                                    CI0027
           05  COBASE  PICTURE X(4).                                    CI0027
           05  DATGNC  PICTURE X(10).                                   CI0027
           05  RELEAS  PICTURE X(7).                                    CI0027
           05  DATGE   PICTURE X(10).                                   CI0027
           05  DATSQ   PICTURE X(10).                                   CI0027
       01  DATCE.                                                       CI0027
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0027
         05  DATOR.                                                     CI0027
           10  DATOA  PICTURE XX.                                       CI0027
           10  DATOM  PICTURE XX.                                       CI0027
           10  DATOJ  PICTURE XX.                                       CI0027
       01   VARIABLES-CONDITIONNELLES.                                  CI0027
            05                  FT      PICTURE X VALUE '0'.            CI0027
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0027
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0027
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0027
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0027
       01               S-CT01-SSA.                                     CI0027
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0027
                                      VALUE 'CT01    '.                 CI0027
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0027
            10          S-CT01-CCOD   PICTURE X(5)                      CI0027
                                      VALUE '-----'.                    CI0027
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0027
       01            S-CTU01-SSA.                                       CI0027
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0027
                                      VALUE 'CT01    '.                 CI0027
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0027
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0027
                                      VALUE '-----'.                    CI0027
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0027
                                      VALUE '(CT01K'.                   CI0027
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0027
            10       S-CTU01-CT01K.                                     CI0027
            11       S-CTU01-C299.                                      CI0027
            12       S-CTU01-CTID.                                      CI0027
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0027
            13       S-CTU01-CTIDN.                                     CI0027
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0027
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0027
            10  FILLER   PICTURE X    VALUE ')'.                        CI0027
       01               S-CT22-SSA.                                     CI0027
            10         S1-CT22-SEGNAM PICTURE X(8)                      CI0027
                                      VALUE 'CT22    '.                 CI0027
            10         S1-CT22-CCOM   PICTURE X VALUE '*'.              CI0027
            10          S-CT22-CCOD   PICTURE X(5)                      CI0027
                                      VALUE '-----'.                    CI0027
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0027
       01            S-CTU22-SSA.                                       CI0027
            10      S1-CTU22-SEGNAM PICTURE X(8)                        CI0027
                                      VALUE 'CT22    '.                 CI0027
            10      S1-CTU22-CCOM   PICTURE X VALUE '*'.                CI0027
            10       S-CTU22-CCOD   PICTURE X(5)                        CI0027
                                      VALUE '-----'.                    CI0027
            10      S1-CTU22-FLDNAM PICTURE X(9)                        CI0027
                                      VALUE '(CT22K'.                   CI0027
            10       S-CTU22-OPER  PICTURE XX VALUE ' ='.               CI0027
            10       S-CTU22-CT22K.                                     CI0027
            11       S-CTU22-CGVEN    PICTURE  X(2).                    CI0027
            11       S-CTU22-CTWHC    PICTURE  9(2).                    CI0027
            10  FILLER   PICTURE X    VALUE ')'.                        CI0027
       01   ZONES-UTILISATEUR PICTURE X.                                CI0027
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=S1 DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 S100.                                         CI0027
          05              S100-SUITE.                                   CI0027
            15       FILLER         PICTURE  X(00541).                  CI0027
       01                 S134  REDEFINES      S100.                    CI0027
            10            S134-C299.                                    CI0027
            11            S134-CTID.                                    CI0027
            12            S134-CTIDA  PICTURE  9(3).                    CI0027
            12            S134-CTIDN.                                   CI0027
            13            S134-CTIDNP PICTURE  X(13).                   CI0027
            13            S134-CTIDND PICTURE  9(11).                   CI0027
            10            S134-DCACG  PICTURE  9(8).                    CI0027
            10            S134-IPOCH  PICTURE  X.                       CI0027
            10            S134-FILLER PICTURE  X(100).                  CI0027
            10            S134-CT22                                     CI0027
                          OCCURS       010     TIMES.                   CI0027
            11            S134-CT22K.                                   CI0027
            12            S134-CGVEN  PICTURE  X(2).                    CI0027
            12            S134-CTWHC  PICTURE  9(2).                    CI0027
            11            S134-CFCNTY PICTURE  X(2).                    CI0027
            11            S134-DLAUP  PICTURE  9(8).                    CI0027
            11            S134-CTWTC  PICTURE  9(2).                    CI0027
            11            S134-CTWHAT PICTURE  S9(7)V99                 CI0027
                          COMPUTATIONAL-3.                              CI0027
            11            S134-CTWHP  PICTURE  9(3)V99                  CI0027
                          COMPUTATIONAL-3.                              CI0027
            11            S134-FILLER PICTURE  X(06).                   CI0027
            10            S134-QITEM  PICTURE  9(3).                    CI0027
            10            S134-XIMAX  PICTURE  S9(4)                    CI0027
                          BINARY.                                       CI0027
            10            S134-CTXMT  PICTURE  9(2).                    CI0027
            10            S134-CTCUS  PICTURE  999.                     CI0027
            10            S134-FILLER PICTURE  X(95).                   CI0027
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=S2 DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 S200.                                         CI0027
          05              S200-SUITE.                                   CI0027
            15       FILLER         PICTURE  X(00541).                  CI0027
       01                 S234  REDEFINES      S200.                    CI0027
            10            S234-C299.                                    CI0027
            11            S234-CTID.                                    CI0027
            12            S234-CTIDA  PICTURE  9(3).                    CI0027
            12            S234-CTIDN.                                   CI0027
            13            S234-CTIDNP PICTURE  X(13).                   CI0027
            13            S234-CTIDND PICTURE  9(11).                   CI0027
            10            S234-DCACG  PICTURE  9(8).                    CI0027
            10            S234-IPOCH  PICTURE  X.                       CI0027
            10            S234-FILLER PICTURE  X(100).                  CI0027
            10            S234-CT22                                     CI0027
                          OCCURS       010     TIMES.                   CI0027
            11            S234-CT22K.                                   CI0027
            12            S234-CGVEN  PICTURE  X(2).                    CI0027
            12            S234-CTWHC  PICTURE  9(2).                    CI0027
            11            S234-CFCNTY PICTURE  X(2).                    CI0027
            11            S234-DLAUP  PICTURE  9(8).                    CI0027
            11            S234-CTWTC  PICTURE  9(2).                    CI0027
            11            S234-CTWHAT PICTURE  S9(7)V99                 CI0027
                          COMPUTATIONAL-3.                              CI0027
            11            S234-CTWHP  PICTURE  9(3)V99                  CI0027
                          COMPUTATIONAL-3.                              CI0027
            11            S234-FILLER PICTURE  X(06).                   CI0027
            10            S234-QITEM  PICTURE  9(3).                    CI0027
            10            S234-XIMAX  PICTURE  S9(4)                    CI0027
                          BINARY.                                       CI0027
            10            S234-CTXMT  PICTURE  9(2).                    CI0027
            10            S234-CTCUS  PICTURE  999.                     CI0027
            10            S234-FILLER PICTURE  X(95).                   CI0027
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
       LINKAGE SECTION.                                                 ADU002
      ***  DLIUIBII COPYBOOK                                            ADU029
            COPY  DLIUIBII.                                             ADU029
      *                                                                 ADU029
      *** ADDRESS LIST OF PCB'S                                         ADU029
       01   PCB-ADDRESS-LIST.                                           ADU029
      ** ALL PCB POINTERS MUST BE ADDED HERE USING MACRO ADU015         ADU029
      **  ONCE FOR EACH DATABASE USED                                   ADU029
      *                                                                 ADU029
      ** FOLLOWING ALL PCB POINTERS, INCLUDE PCB MASKS (USE MACRO       ADU029
      **  ADU015) FOR EACH DATABASE USED                                ADU029
      ******************************************************************ADU029
      ******************************************************************ADU029
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0027
          05              PA00-SUITE.                                   CI0027
            15       FILLER         PICTURE  X(00106).                  CI0027
       01                 PA06  REDEFINES      PA00.                    CI0027
            10            PA06-XDBPCB.                                  CI0027
            11            PA06-XDBDNM PICTURE  X(08).                   CI0027
            11            PA06-XSEGLV PICTURE  X(02).                   CI0027
            11            PA06-XRC    PICTURE  X(02).                   CI0027
            11            PA06-XPROPT PICTURE  X(04).                   CI0027
            11            PA06-FILLER PICTURE  S9(5)                    CI0027
                          BINARY.                                       CI0027
            11            PA06-XSEGNM PICTURE  X(08).                   CI0027
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0027
                          BINARY.                                       CI0027
            11            PA06-XSEGNB PICTURE  9(05)                    CI0027
                          BINARY.                                       CI0027
            11            PA06-XCOKEY PICTURE  X(70).                   CI0027
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=AW DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 AW00.                                         CI0027
          05              AW00-SUITE.                                   CI0027
            15       FILLER         PICTURE  X(00541).                  CI0027
       01                 AW34  REDEFINES      AW00.                    CI0027
            10            AW34-C299.                                    CI0027
            11            AW34-CTID.                                    CI0027
            12            AW34-CTIDA  PICTURE  9(3).                    CI0027
            12            AW34-CTIDN.                                   CI0027
            13            AW34-CTIDNP PICTURE  X(13).                   CI0027
            13            AW34-CTIDND PICTURE  9(11).                   CI0027
            10            AW34-DCACG  PICTURE  9(8).                    CI0027
            10            AW34-IPOCH  PICTURE  X.                       CI0027
            10            AW34-FILLER PICTURE  X(100).                  CI0027
            10            AW34-CT22                                     CI0027
                          OCCURS       010     TIMES.                   CI0027
            11            AW34-CT22K.                                   CI0027
            12            AW34-CGVEN  PICTURE  X(2).                    CI0027
            12            AW34-CTWHC  PICTURE  9(2).                    CI0027
            11            AW34-CFCNTY PICTURE  X(2).                    CI0027
            11            AW34-DLAUP  PICTURE  9(8).                    CI0027
            11            AW34-CTWTC  PICTURE  9(2).                    CI0027
            11            AW34-CTWHAT PICTURE  S9(7)V99                 CI0027
                          COMPUTATIONAL-3.                              CI0027
            11            AW34-CTWHP  PICTURE  9(3)V99                  CI0027
                          COMPUTATIONAL-3.                              CI0027
            11            AW34-FILLER PICTURE  X(06).                   CI0027
            10            AW34-QITEM  PICTURE  9(3).                    CI0027
            10            AW34-XIMAX  PICTURE  S9(4)                    CI0027
                          BINARY.                                       CI0027
            10            AW34-CTXMT  PICTURE  9(2).                    CI0027
            10            AW34-CTCUS  PICTURE  999.                     CI0027
            10            AW34-FILLER PICTURE  X(95).                   CI0027
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0027
          05              DE00-SUITE.                                   CI0027
            15       FILLER         PICTURE  X(00653).                  CI0027
       01                 DE10  REDEFINES      DE00.                    CI0027
            10            DE10-DU11.                                    CI0027
            11            DE10-XFONC  PICTURE  X(4).                    CI0027
            11            DE10-MPSBN  PICTURE  X(8).                    CI0027
            11            DE10-XDBDNM PICTURE  X(08).                   CI0027
            11            DE10-XSEGNM PICTURE  X(08).                   CI0027
            11            DE10-XRC    PICTURE  X(02).                   CI0027
            11            DE10-MSEG   PICTURE  X(08).                   CI0027
            11            DE10-XCOKEY PICTURE  X(70).                   CI0027
            11            DE10-CUIBR  PICTURE  X(01).                   CI0027
            11            DE10-CUIBA  PICTURE  X(01).                   CI0027
            11            DE10-IPBIK  PICTURE  X(1).                    CI0027
            10            DE10-DU03.                                    CI0027
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0027
                          COMPUTATIONAL-3.                              CI0027
            11            DE10-CMSSF  PICTURE  XX.                      CI0027
            11            DE10-DU09.                                    CI0027
            12            DE10-CMESA  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            12            DE10-CMESB  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            12            DE10-CMSST  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            12            DE10-QELLAA PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            12            DE10-TMESS4 PICTURE  X(512).                  CI0027
      *                                                                 AMDU10
      *                                                                 AMDU10
      *                                                                 AMDU10
      *                                                                 AMDU10
      *                                                                 ADU002
      ******************************************************************ADU002
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU002
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU002
      ******************************************************************ADU002
      *                                                                 ADU002
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU002
       01                 MS00.                                         CI0027
          05              MS00-SUITE.                                   CI0027
            15       FILLER         PICTURE  X(00542).                  CI0027
       01                 MS03  REDEFINES      MS00.                    CI0027
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0027
                          COMPUTATIONAL-3.                              CI0027
            10            MS03-CMSSF  PICTURE  XX.                      CI0027
            10            MS03-DU09.                                    CI0027
            11            MS03-CMESA  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            11            MS03-CMESB  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            11            MS03-CMSST  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            11            MS03-QELLAA PICTURE  S9(9)                    CI0027
                          BINARY.                                       CI0027
            11            MS03-TMESS4 PICTURE  X(512).                  CI0027
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                AW34
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0027
      *               *                                   *             CI0027
      *               *INITIALISATIONS                    *             CI0027
      *               *                                   *             CI0027
      *               *************************************.            CI0027
       F01.      EXIT.
       F01-FN.   EXIT.
      *N02.      NOTE *************************************.            ADU002
      *               *                                   *             ADU002
      *               *MODULE INITIALIZATIONS             *             ADU002
      *               *                                   *             ADU002
      *               *************************************.            ADU002
       F02.                                                             lv05
      *                                                                 ADU002
      *********************************                                 ADU002
      ** MODULE INITIALIZATIONS       *                                 ADU002
      **  - INITIALIZE FT SWITCH      *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
      *N02BA.    NOTE *INITIALIZE FT SWITCH               *.            ADU002
       F02BA.                                                           lv10
      *                                                                 ADU002
      *********************************                                 ADU002
      ** MOVE ZEROS TO FT SWITCH TO   *                                 ADU002
      ** MAKE SURE THAT FUNCTION 20   *                                 ADU002
      ** GETS EXECUTED AT END OF      *                                 ADU002
      ** PROCESSING.                  *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
           MOVE ALL    ZEROS TO FT.                                     ADU002
       F02BA-FN. EXIT.
      *N02CA.    NOTE *INIT NUMBER OF CT22 SEGS PASSED    *.
       F02CA.                                                           lv10
      *
      *********************************
      ** INITIALIZE THE NUMBER OF CT22*
      ** SEGMENTS THAT WILL BE PASSED *
      ** BACK TO THE CALLING MODULE   *
      *********************************
      *
           MOVE        ZEROS TO AW34-QITEM.
       F02CA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *
      *********************************
      ** SET ADDRESSES FOR DATABASES  *
      *********************************
      *.
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0027
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0027
      *               *                                   *             CI0027
      *               *FIN DE TRAITEMENT                  *             CI0027
      *               *                                   *             CI0027
      *               *************************************.            CI0027
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0027
      *N2099.    NOTE *GO BACK TO CALLING MODULE          *.            ADU002
       F2099.                                                           lv10
      *                                                                 ADU002
      *********************************                                 ADU002
      ** GOBACK TO CALLING MODULE     *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
           GOBACK.                                                      ADU002
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF ACCT IS ONE OF BUFFERS    *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      ** FOR EFFICIENCY, THE LAST TWO *
      ** ACCOUNT NUMBERS THAT WERE    *
      ** ACCESSED WILL BE STORED IN   *
      ** SAVE AREAS.                  *
      *********************************
      *
      *N40BA.    NOTE *CHECK FIRST SAVE AREA              *.
       F40BA.    IF    S134-CTID = AW34-CTID                            lv10
                 NEXT SENTENCE ELSE GO TO     F40BA-FN.
      *
      *********************************
      ** CHECK IF THE ACCOUNT NUMBERS *
      ** MATCH BETWEEN WHAT THE       *
      ** CALLING MODULE WANTS AND     *
      ** WHAT IS IN THE FIRST SAVE    *
      ** AREA.                        *
      *********************************
      *
      *N40CA.    NOTE *ACCOUNT NUMBERS MATCH              *.
       F40CA.                                                           lv15
      *
      *********************************
      ** THE ACCOUNT NUMBER ARE THE   *
      ** SAME, PASS THE FIRST SAVED   *
      ** AREA BACK TO THE CALLING     *
      ** MODULE.                      *
      *********************************
      *
           MOVE        S134 TO AW34.
       F40CA-FN. EXIT.
      *N40DA.    NOTE *RETURN TO CALLING MODULE           *.
       F40DA.                                                           lv15
      *
      *********************************
      ** RETURN TO THE CALLING        *
      ** MODULE.                      *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F40DA-FN. EXIT.
       F40BA-FN. EXIT.
      *N40EA.    NOTE *CHECK SECOND SAVE AREA             *.
       F40EA.    IF    S234-CTID = AW34-CTID                            lv10
                 NEXT SENTENCE ELSE GO TO     F40EA-FN.
      *
      *********************************
      ** CHECK IF THE ACCOUNT NUMBERS *
      ** MATCH BETWEEN WHAT THE       *
      ** CALLING MODULE WANTS AND     *
      ** WHAT IS IN THE SECOND SAVE   *
      ** AREA.                        *
      *********************************
      *
      *N40FA.    NOTE *ACCOUNT NUMBERS MATCH              *.
       F40FA.                                                           lv15
      *
      *********************************
      ** THE ACCOUNT NUMBER ARE THE   *
      ** SAME, PASS THE SECOND SAVED  *
      ** AREA BACK TO THE CALLING     *
      ** MODULE.                      *
      *********************************
      *
           MOVE        S234 TO AW34.
       F40FA-FN. EXIT.
      *N40GA.    NOTE *RETURN TO CALLING MODULE           *.
       F40GA.                                                           lv15
      *
      *********************************
      ** RETURN TO THE CALLING        *
      ** MODULE.                      *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F40GA-FN. EXIT.
       F40EA-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *GET CT22 SEGMENTS FOR ACCOUNT      *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** READ CONTRACT DATABASE       *
      ** TO GET THE CT22 SEGMENTS     *
      ** FOR THE ACCOUNT.             *
      *********************************
      *
      *N50AA.    NOTE *SET UP SSA FOR CT01 READ           *.
       F50AA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CT01 READ     *
      ** USING ACCOUNT ID NUMBER      *
      ** PASSED FROM CALLING MODULE   *
      *********************************
      *
           MOVE        AW34-CTID TO S-CTU01-CTID.
       F50AA-FN. EXIT.
      *N50BA.    NOTE *CHECK IF CT01 NEEDS TO BE READ     *.
       F50BA.    IF    AW34-IPOCH = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50BA-FN.
      *
      *********************************
      ** IF THE CALLING MODULE HAS    *
      ** ALREADY ESTABLISHED POSITION *
      ** AT THE CT01 SEGMENT DO NOT   *
      ** RE-READ.                     *
      *********************************
      *
      *N50CA.    NOTE *READ CT01 SEGMENT                  *.
       F50CA.                                                           lv15
      *
      *********************************
      ** READ CT01 SEGMENT            *
      *********************************
      *
           PERFORM     F94CT THRU F94CT-FN.
       F50CA-FN. EXIT.
      *N50DA.    NOTE *CHECK IF ACCT NUMBER NOT FOUND     *.
       F50DA.    IF    IK = '1'                                         lv15
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
      *********************************
      ** IF ACCOUNT NUMBER NOT FOUND  *
      ** ON THE CONTRACT DATABASE     *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012011 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
      *N50DZ.    NOTE *RETURN TO CALLING MODULE           *.
       F50DZ.                                                           lv20
      *
      *********************************
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F50DZ-FN. EXIT.
       F50DA-FN. EXIT.
       F50BA-FN. EXIT.
      *N50EA.    NOTE *POSITION ESTABLISHED AT CT01       *.
       F50EA.                                                           lv10
      *
      *********************************
      ** POSITION WAS ESTABLISHED AT  *
      ** CT01 SEGMENT.                *
      *********************************
      *
      *N50FA.    NOTE *READ FIRST CT22 SEGMENT            *.
       F50FA.                                                           lv15
      *
      *********************************
      ** READ THE FIRST CT22 SEGMENT  *
      ** ON THE CONTRACT DATABASE     *
      *********************************
      *
           PERFORM     F94CN THRU F94CN-FN.
       F50FA-FN. EXIT.
      *N50GA.    NOTE *LOOP WHILE MORE CT22 SEGMENTS      *.
       F50GA.    IF    IK = '0'                                         lv15
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F50GA-FN.
      *
      *********************************
      ** KEEP ON LOOPING WHILE MORE   *
      ** CT22 SEGMENTS.               *
      *********************************
      *
      *N50HA.    NOTE *ADD 1 TO NUMBER OF CT22 SEGMENTS   *.
       F50HA.                                                           lv20
      *
      *********************************
      ** BUMP UP THE CT22 TABLE       *
      ** INDEX LOAD BY 1              *
      *********************************
      *
           ADD         1 TO AW34-QITEM.
       F50HA-FN. EXIT.
      *N50IA.    NOTE *CHECK IF TABLE OVERFLOW            *.
       F50IA.    IF    AW34-QITEM > AW34-XIMAX                          lv20
                 NEXT SENTENCE ELSE GO TO     F50IA-FN.
      *
      *********************************
      ** STOP READING CT22 SEGMENTS   *
      ** WHEN THERE IS A TABLE        *
      ** OVERFLOW                     *
      *********************************
      *
           MOVE        AW34-XIMAX TO AW34-QITEM
           MOVE        '1' TO IK
               GO TO     F50GA-900.
       F50IA-FN. EXIT.
      *N50JA.    NOTE *MOVE CT22 SEGMENT TO TABLE         *.
       F50JA.                                                           lv20
      *
      *********************************
      ** MOVE CT22 SEGMENT TO THE     *
      ** TABLE IN AW34.               *
      *********************************
      *
           MOVE        CT22 TO AW34-CT22 (AW34-QITEM).
       F50JA-FN. EXIT.
      *N50KA.    NOTE *READ THE NEXT CT22 SEGMENT         *.
       F50KA.                                                           lv20
      *
      *********************************
      ** READ THE NEXT CT22 SEGMENT   *
      ** ON THE CONTRACT DATABASE     *
      *********************************
      *
           PERFORM     F94CN THRU F94CN-FN.
       F50KA-FN. EXIT.
       F50GA-900. GO TO F50GA.
       F50GA-FN. EXIT.
       F50EA-FN. EXIT.
       F50-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *ACCOUNT NUMBER ACCESSED            *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *
      *********************************
      ** THE ACCOUNT NUMBER PASSED    *
      ** WAS NOT IN ANY OF THE SAVE   *
      ** AREAS, SO THE CONTRACT       *
      ** DATABASE WAS ACCESSED TO GET *
      ** THE DATA.  NOW SHUFFLE THE   *
      ** SAVE AREAS AROUND.  THE FIRST*
      ** SAVE AREA WILL BE MOVED TO   *
      ** SECOND SAVE AREA AND THE     *
      ** NEW ACCOUNT NUMBER DATA WILL *
      ** BE MOVED TO THE FIRST SAVE   *
      ** AREA.                        *
      *********************************
      *
      *N60BA.    NOTE *MOVE FIRST SAVE AREA               *.
       F60BA.                                                           lv10
      *
      *********************************
      ** MOVE THE FIRST SAVE AREA TO  *
      ** THE SECOND SAVE AREA.        *
      *********************************
      *
           MOVE        S134 TO S234.
       F60BA-FN. EXIT.
      *N60CA.    NOTE *MOVE AC14 TO FIRST SAVE AREA       *.
       F60CA.                                                           lv10
      *
      *********************************
      ** MOVE AC14 TO THE FIRST SAVE  *
      ** AREA.                        *
      *********************************
      *
           MOVE        AW34 TO S134.
       F60CA-FN. EXIT.
       F60-FN.   EXIT.
      *N79.      NOTE *************************************.            ADU002
      *               *                                   *             ADU002
      *               *RETURN TO CALLING MODULE           *             ADU002
      *               *                                   *             ADU002
      *               *************************************.            ADU002
       F79.                                                             lv05
      *                                                                 ADU002
      *********************************                                 ADU002
      ** RETURN TO CALLING MODULE     *                                 ADU002
      *********************************                                 ADU002
      *                                                                 ADU002
           MOVE                     ALL '1' TO FT GO TO F20.            ADU002
       F79-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N93.      NOTE *************************************.            ADU029
      *               *                                   *             ADU029
      *               *COMMON DL1 ERROR ROUTINES          *             ADU029
      *               *                                   *             ADU029
      *               *************************************.            ADU029
       F93.           EXIT.                                             lv05
      *N93EA.    NOTE *DATABASE I/O ERROR PROCESSING      *.            ADU029
       F93EA.                                                           lv10
                 IF    XW05-XRC = '  '                                  DOT
           MOVE        ZERO TO IK                                       ADU029
                 ELSE                                                   ADU029
           MOVE        '1' TO IK                                        ADU029
           DE10-IPBIK                                                   ADU029
           INITIALIZE        DE10-DU03                                  ADU029
           PERFORM     F93UI THRU F93UI-FN                              ADU029
           PERFORM     F93PC THRU F93PC-FN.                             ADU029
       F93EA-FN. EXIT.
      *N93PC.    NOTE *CHECK DL1 PCB - CALL CI0009P       *.            ADU029
       F93PC.                                                           lv10
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU029
           MOVE        XW05-XRC TO DE10-XRC                             ADU029
           MOVE        XW05-XSEGNM TO DE10-MSEG                         ADU029
           MOVE        XW05-XCOKEY TO DE10-XCOKEY                       ADU029
           MOVE        XW05-XDBDNM TO DE10-XDBDNM                       ADU029
      *                                                                 ADU029
           MOVE        'CI0009P ' TO W-PASS-XPROGR                      ADU029
           CALL        W-PASS-XPROGR                                    ADU029
           USING DFHEIBLK                                               ADU029
           DFHCOMMAREA                                                  ADU029
           DE10                                                         ADU029
           MS03.                                                        ADU029
       F93PC-FN. EXIT.
      *N93UI.    NOTE *CHECK DL1 USER INTRFACE BLK(UIB)   *.            ADU029
       F93UI.                                                           lv10
      *>>> CALL CI0008P                                                 ADU029
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU029
           MOVE        UIBFCTR TO DE10-CUIBR                            ADU029
           MOVE        UIBDLTR TO DE10-CUIBA                            ADU029
      *                                                                 ADU029
           MOVE        'CI0008P ' TO W-PASS-XPROGR                      ADU029
           CALL        W-PASS-XPROGR                                    ADU029
           USING DFHEIBLK                                               ADU029
           DFHCOMMAREA                                                  ADU029
           DE10                                                         ADU029
           MS03.                                                        ADU029
       F93UI-FN. EXIT.
       F93-FN.   EXIT.
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *DL/I CALLS                         *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CN.    NOTE *CALL GN ON CT22                    *.            ADU026
       F94CN.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT22' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CT22                                                    ADU026
           S-CTU01-SSA S-CT22-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CN-FN. EXIT.
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
       F94-FN.   EXIT.
      *N98.      NOTE *************************************.            ADU002
      *               *                                   *             ADU002
      *               *COMMON PERFORMED ROUTINES          *             ADU002
      *               *                                   *             ADU002
      *               *************************************.            ADU002
       F98.           EXIT.                                             lv05
      *N98ET.    NOTE *ERROR MESSAGE PROCESS              *.            ADU002
       F98ET.                                                           lv10
      *********************************                                 ADU002
      *    SET MESSAGE SEVERITY                                         ADU002
      *********************************                                 ADU002
           MOVE        11 TO MS03-CMESB                                 ADU002
      *********************************                                 ADU002
      *    GET MESSAGE TEXT                                             ADU002
      *********************************                                 ADU002
           PERFORM     F98GM THRU F98GM-FN.                             ADU002
       F98ET-FN. EXIT.
      *N98GM.    NOTE *GET MESSAGE TEXT                   *.            ADU002
       F98GM.                                                           lv10
      *********************************                                 ADU002
      *CALL DATA UTILITY MESSAGE MODULE                                 ADU002
      *********************************                                 ADU002
           MOVE        'AP' TO MS03-CMSSF                               ADU002
           CALL        CI0002 USING                                     ADU002
           DFHEIBLK                                                     ADU002
           DFHCOMMAREA                                                  ADU002
           MS03.                                                        ADU002
       F98GM-FN. EXIT.
      *N98IC.    NOTE *INFO MESSAGE PROCESS               *.            ADU002
       F98IC.                                                           lv10
      *********************************                                 ADU002
      *    SET MESSAGE SEVERITY                                         ADU002
      *********************************                                 ADU002
           MOVE        10 TO MS03-CMESB                                 ADU002
      *********************************                                 ADU002
      *    GET MESSAGE TEXT                                             ADU002
      *********************************                                 ADU002
           PERFORM     F98GM THRU F98GM-FN.                             ADU002
       F98IC-FN. EXIT.
       F98-FN.   EXIT.
