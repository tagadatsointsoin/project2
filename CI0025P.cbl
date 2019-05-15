       IDENTIFICATION DIVISION.                                         CI0025
       PROGRAM-ID.  CI0025P.                                            CI0025
      *AUTHOR.         M\M - GET BANK NAME AND CL18.                    CI0025
      *DATE-COMPILED.   09/08/14.                                       CI0025
       ENVIRONMENT DIVISION.                                            CI0025
       CONFIGURATION SECTION.                                           CI0025
       SOURCE-COMPUTER. IBM-370.                                        CI0025
       OBJECT-COMPUTER. IBM-370.                                        CI0025
       DATA DIVISION.                                                   CI0025
       WORKING-STORAGE SECTION.                                         CI0025
       01                 CL00.                                         CI0025
            02            CL01.                                         CI0025
            10            CL01-CL01K.                                   CI0025
            11            CL01-C199.                                    CI0025
            12            CL01-CLID.                                    CI0025
            13            CL01-CLIDO  PICTURE  9(3).                    CI0025
            13            CL01-CLIDN.                                   CI0025
            14            CL01-CLIDNP PICTURE  X(12).                   CI0025
            14            CL01-CLIDND PICTURE  9(8).                    CI0025
            10            CL01-GECKD  PICTURE  9.                       CI0025
            10            CL01-GEMDA  PICTURE  9(8).                    CI0025
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0025
                          BINARY.                                       CI0025
            10            CL01-GECUC  PICTURE  99.                      CI0025
            10            CL01-CLDOR  PICTURE  9(8).                    CI0025
            10            CL01-CLLNG  PICTURE  XX.                      CI0025
            10            CL01-GESLC  PICTURE  99.                      CI0025
            10            CL01-CLTYP  PICTURE  X.                       CI0025
            10            CL01-CLCLS  PICTURE  9(3).                    CI0025
            10            CL01-CLTWRC PICTURE  99.                      CI0025
            10            CL01-CLPVC  PICTURE  99.                      CI0025
            10            CL01-CLIND  PICTURE  9(3).                    CI0025
            10            CL01-CLTRC  PICTURE  99.                      CI0025
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0025
                          COMPUTATIONAL-3.                              CI0025
            10            CL01-AYSIDA PICTURE  9(3).                    CI0025
            10            CL01-AYSID  PICTURE  9(5).                    CI0025
            10            CL01-CLSTR  PICTURE  9(2).                    CI0025
            10            CL01-CLC11  PICTURE  X.                       CI0025
            10            CL01-CLTIN  PICTURE  9(12).                   CI0025
            10            CL01-CLTND  PICTURE  9(8).                    CI0025
            10            CL01-CLTINC PICTURE  9.                       CI0025
            10            CL01-CCDWA  PICTURE  9.                       CI0025
            10            CL01-CICES  PICTURE  X.                       CI0025
            10            CL01-CLTRA  PICTURE  9(2).                    CI0025
            10            CL01-DIRSY  PICTURE  9(4)                     CI0025
                          COMPUTATIONAL-3.                              CI0025
            10            CL01-CFEDS  PICTURE  X.                       CI0025
            10            CL01-FILLER PICTURE  X(06).                   CI0025
            02            CL12.                                         CI0025
            10            CL12-GEDLA  PICTURE  9(8).                    CI0025
            10            CL12-CLBCD  PICTURE  9(3).                    CI0025
            10            CL12-CLFDW  PICTURE  X.                       CI0025
            10            CL12-CLOSD  PICTURE  9(8).                    CI0025
            10            CL12-CLOED  PICTURE  9(8).                    CI0025
            10            CL12-CLOEI  PICTURE  X.                       CI0025
            10            CL12-CLIBN  PICTURE  X(20).                   CI0025
            10            CL12-CLINT  PICTURE  9(3).                    CI0025
            10            CL12-CLONE  PICTURE  9(9).                    CI0025
            10            CL12-CLORC  PICTURE  99.                      CI0025
            10            CL12-CLORN  PICTURE  X(45).                   CI0025
            10            CL12-CLORP  PICTURE  X(25).                   CI0025
            10            CL12-GEPHNB PICTURE  X(14).                   CI0025
            10            CL12-GEPHNX PICTURE  9(4).                    CI0025
            10            CL12-GEPHNA PICTURE  X(14).                   CI0025
            10            CL12-GEFYE  PICTURE  9(4).                    CI0025
            10            CL12-AYCDE  PICTURE  9(3).                    CI0025
            10            CL12-AYID   PICTURE  9(5).                    CI0025
            10            CL12-CFOBO  PICTURE  99.                      CI0025
            10            CL12-CLINRG                                   CI0025
                          OCCURS       003     TIMES.                   CI0025
            11            CL12-CLIRT  PICTURE  99.                      CI0025
            11            CL12-CLINR  PICTURE  X(3).                    CI0025
            11            CL12-CLIRD  PICTURE  9(8).                    CI0025
            10            CL12-IOTXE  PICTURE  X.                       CI0025
            10            CL12-IO501  PICTURE  X.                       CI0025
            10            CL12-IOFOG  PICTURE  X.                       CI0025
            10            CL12-IOPRA  PICTURE  X.                       CI0025
            10            CL12-IOSCS  PICTURE  X.                       CI0025
            10            CL12-IACHA  PICTURE  X.                       CI0025
            10            CL12-IFORG  PICTURE  X.                       CI0025
            10            CL12-IFIND  PICTURE  X.                       CI0025
            10            CL12-CFCNT3 PICTURE  X(2).                    CI0025
            10            CL12-FILLER PICTURE  X(06).                   CI0025
            02            CL18.                                         CI0025
            10            CL18-CL18K.                                   CI0025
            11            CL18-NRTSQ  PICTURE  S9(3)                    CI0025
                          COMPUTATIONAL-3.                              CI0025
            10            CL18-NTR    PICTURE  9(8).                    CI0025
            10            CL18-GECKD  PICTURE  9.                       CI0025
            10            CL18-GEEND  PICTURE  9(8).                    CI0025
            10            CL18-NPDIN  PICTURE  X(4).                    CI0025
            10            CL18-IRTNA  PICTURE  X.                       CI0025
            10            CL18-IRTNP  PICTURE  X.                       CI0025
            10            CL18-IRTNW  PICTURE  X.                       CI0025
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
      ******************************************************************ADU029
      ***  STORAGE AREAS FOR DL1                                        ADU029
      ******************************************************************ADU029
      ***  DL1 FUNCTIONS                                                ADU029
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU029
       01                 XW05.                                         CI0025
            10            XW05-XW06.                                    CI0025
            11            XW05-XDBPCB.                                  CI0025
            12            XW05-XDBDNM PICTURE  X(08)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-XSEGLV PICTURE  X(02)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-XRC    PICTURE  X(02)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-XPROPT PICTURE  X(04)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-FILLER PICTURE  S9(5)                    CI0025
                          VALUE                ZERO                     CI0025
                          BINARY.                                       CI0025
            12            XW05-XSEGNM PICTURE  X(08)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0025
                          VALUE                ZERO                     CI0025
                          BINARY.                                       CI0025
            12            XW05-XSEGNB PICTURE  9(05)                    CI0025
                          VALUE                ZERO                     CI0025
                          BINARY.                                       CI0025
            12            XW05-XCOKEY PICTURE  X(70)                    CI0025
                          VALUE                SPACE.                   CI0025
            10            XW05-XW07.                                    CI0025
            11            XW05-XIOPCB.                                  CI0025
            12            XW05-XTERMI PICTURE  X(08)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-FILLER PICTURE  XX                       CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-XRC1   PICTURE  X(02)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-FILLER PICTURE  X(12)                    CI0025
                          VALUE                SPACE.                   CI0025
            12            XW05-XMODNM PICTURE  X(8)                     CI0025
                          VALUE                SPACE.                   CI0025
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0025
                          VALUE                ZERO.                    CI0025
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0025
                          VALUE                ZERO.                    CI0025
            10            XW05-XGU    PICTURE  X(4)                     CI0025
                          VALUE                'GU  '.                  CI0025
            10            XW05-XGHU   PICTURE  X(4)                     CI0025
                          VALUE                'GHU '.                  CI0025
            10            XW05-XGN    PICTURE  X(4)                     CI0025
                          VALUE                'GN  '.                  CI0025
            10            XW05-XGHN   PICTURE  X(4)                     CI0025
                          VALUE                'GHN '.                  CI0025
            10            XW05-XGNP   PICTURE  X(4)                     CI0025
                          VALUE                'GNP '.                  CI0025
            10            XW05-XGHNP  PICTURE  X(4)                     CI0025
                          VALUE                'GHNP'.                  CI0025
            10            XW05-XREPL  PICTURE  XXXX                     CI0025
                          VALUE                'REPL'.                  CI0025
            10            XW05-XISRT  PICTURE  X(4)                     CI0025
                          VALUE                'ISRT'.                  CI0025
            10            XW05-XDLET  PICTURE  X(4)                     CI0025
                          VALUE                'DLET'.                  CI0025
            10            XW05-XOPEN  PICTURE  X(4)                     CI0025
                          VALUE                'OPEN'.                  CI0025
            10            XW05-XCLSE  PICTURE  X(4)                     CI0025
                          VALUE                'CLSE'.                  CI0025
            10            XW05-XCHKP  PICTURE  X(4)                     CI0025
                          VALUE                'CHKP'.                  CI0025
            10            XW05-XXRST  PICTURE  X(4)                     CI0025
                          VALUE                'XRST'.                  CI0025
            10            XW05-XTERM  PICTURE  X(4)                     CI0025
                          VALUE                'TERM'.                  CI0025
            10            XW05-XNFPAC PICTURE  X(13)                    CI0025
                          VALUE                SPACE.                   CI0025
      *!WI pl=DL200                                                     ADU029
       01  DL01-KFPCB                                                   ADU029
                        PICTURE X(04)                                   CI0025
              VALUE 'PCB '.                                             ADU029
      ***  SAVE AREA FOR DL1 FUNCTION VALUE - USED FOR ERROR PROCESSING ADU029
       01  SV01-FUNC     PIC X(4).                                      ADU029
      ******************************************************************ADU029
      *** USED WHEN DYNAMICALLY CALLING PCB & UIB ERROR CHECKING MODULESADU029
      ***    (CI0008P - UIB ERROR CHECK    CI0009P - PCB ERROR CHECK)   ADU029
      ******************************************************************ADU029
      *!WI pl=DN100                                                     ADU029
       01  W-PASS-XPROGR                                                ADU029
                        PICTURE X(8).                                   CI0025
       01   DEBUT-WSS.                                                  CI0025
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0025
            05   IK     PICTURE X.                                      CI0025
       01  CONSTANTES-PAC.                                              CI0025
           05  FILLER  PICTURE X(87)   VALUE                            CI0025
                     '6015 CAT09/08/14CI0025ADMIN   14:34:18CI0025P AMERCI0025
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0025
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0025
           05  NUGNA   PICTURE X(5).                                    CI0025
           05  APPLI   PICTURE X(3).                                    CI0025
           05  DATGN   PICTURE X(8).                                    CI0025
           05  PROGR   PICTURE X(6).                                    CI0025
           05  CODUTI  PICTURE X(8).                                    CI0025
           05  TIMGN   PICTURE X(8).                                    CI0025
           05  PROGE   PICTURE X(8).                                    CI0025
           05  COBASE  PICTURE X(4).                                    CI0025
           05  DATGNC  PICTURE X(10).                                   CI0025
           05  RELEAS  PICTURE X(7).                                    CI0025
           05  DATGE   PICTURE X(10).                                   CI0025
           05  DATSQ   PICTURE X(10).                                   CI0025
       01  DATCE.                                                       CI0025
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0025
         05  DATOR.                                                     CI0025
           10  DATOA  PICTURE XX.                                       CI0025
           10  DATOM  PICTURE XX.                                       CI0025
           10  DATOJ  PICTURE XX.                                       CI0025
       01   VARIABLES-CONDITIONNELLES.                                  CI0025
            05                  FT      PICTURE X VALUE '0'.            CI0025
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0025
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0025
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0025
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0025
       01               S-CL01-SSA.                                     CI0025
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0025
                                      VALUE 'CL01    '.                 CI0025
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0025
            10          S-CL01-CCOD   PICTURE X(5)                      CI0025
                                      VALUE '-----'.                    CI0025
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0025
       01            S-CLU01-SSA.                                       CI0025
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0025
                                      VALUE 'CL01    '.                 CI0025
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0025
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0025
                                      VALUE '-----'.                    CI0025
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0025
                                      VALUE '(CL01K'.                   CI0025
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0025
            10       S-CLU01-CL01K.                                     CI0025
            11       S-CLU01-C199.                                      CI0025
            12       S-CLU01-CLID.                                      CI0025
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0025
            13       S-CLU01-CLIDN.                                     CI0025
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0025
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0025
            10  FILLER   PICTURE X    VALUE ')'.                        CI0025
       01               S-CL12-SSA.                                     CI0025
            10         S1-CL12-SEGNAM PICTURE X(8)                      CI0025
                                      VALUE 'CL12    '.                 CI0025
            10         S1-CL12-CCOM   PICTURE X VALUE '*'.              CI0025
            10          S-CL12-CCOD   PICTURE X(5)                      CI0025
                                      VALUE '-----'.                    CI0025
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0025
       01               S-CL18-SSA.                                     CI0025
            10         S1-CL18-SEGNAM PICTURE X(8)                      CI0025
                                      VALUE 'CL18    '.                 CI0025
            10         S1-CL18-CCOM   PICTURE X VALUE '*'.              CI0025
            10          S-CL18-CCOD   PICTURE X(5)                      CI0025
                                      VALUE '-----'.                    CI0025
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0025
       01            S-CLU18-SSA.                                       CI0025
            10      S1-CLU18-SEGNAM PICTURE X(8)                        CI0025
                                      VALUE 'CL18    '.                 CI0025
            10      S1-CLU18-CCOM   PICTURE X VALUE '*'.                CI0025
            10       S-CLU18-CCOD   PICTURE X(5)                        CI0025
                                      VALUE '-----'.                    CI0025
            10      S1-CLU18-FLDNAM PICTURE X(9)                        CI0025
                                      VALUE '(CL18K'.                   CI0025
            10       S-CLU18-OPER  PICTURE XX VALUE ' ='.               CI0025
            10       S-CLU18-CL18K.                                     CI0025
            11       S-CLU18-NRTSQ    PICTURE  S9(3)                    CI0025
                          COMPUTATIONAL-3.                              CI0025
            10  FILLER   PICTURE X    VALUE ')'.                        CI0025
       01            S-CL118-SSA.                                       CI0025
            10      S1-CL118-SEGNAM PICTURE X(8)                        CI0025
                                      VALUE 'CL18    '.                 CI0025
            10      S1-CL118-CCOM   PICTURE X VALUE '*'.                CI0025
            10       S-CL118-CCOD   PICTURE X(5)                        CI0025
                                      VALUE '-----'.                    CI0025
            10      S1-CL118-FLDNAM PICTURE X(9)                        CI0025
                                      VALUE '(XNTR'.                    CI0025
            10       S-CL118-OPER  PICTURE XX VALUE ' ='.               CI0025
            10       S-CL118-NTR      PICTURE  9(8).                    CI0025
            10  FILLER   PICTURE X    VALUE ')'.                        CI0025
       01            S-CL218-SSA.                                       CI0025
            10      S1-CL218-SEGNAM PICTURE X(8)                        CI0025
                                      VALUE 'CL18    '.                 CI0025
            10      S1-CL218-CCOM   PICTURE X VALUE '*'.                CI0025
            10       S-CL218-CCOD   PICTURE X(5)                        CI0025
                                      VALUE '-----'.                    CI0025
            10      S1-CL218-FLDNAM PICTURE X(9)                        CI0025
                                      VALUE '(XGEEND'.                  CI0025
            10       S-CL218-OPER  PICTURE XX VALUE ' ='.               CI0025
            10       S-CL218-GEEND    PICTURE  9(8).                    CI0025
            10  FILLER   PICTURE X    VALUE ')'.                        CI0025
       01            S-CL318-SSA.                                       CI0025
            10      S1-CL318-SEGNAM PICTURE X(8)                        CI0025
                                      VALUE 'CL18    '.                 CI0025
            10      S1-CL318-CCOM   PICTURE X VALUE '*'.                CI0025
            10       S-CL318-CCOD   PICTURE X(5)                        CI0025
                                      VALUE '-----'.                    CI0025
            10      S1-CL318-FLDNAM PICTURE X(9)                        CI0025
                                      VALUE '(XIRTNA'.                  CI0025
            10       S-CL318-OPER  PICTURE XX VALUE ' ='.               CI0025
            10       S-CL318-IRTNA    PICTURE  X.                       CI0025
            10  FILLER   PICTURE X    VALUE ')'.                        CI0025
       01            S-CL418-SSA.                                       CI0025
            10      S1-CL418-SEGNAM PICTURE X(8)                        CI0025
                                      VALUE 'CL18    '.                 CI0025
            10      S1-CL418-CCOM   PICTURE X VALUE '*'.                CI0025
            10       S-CL418-CCOD   PICTURE X(5)                        CI0025
                                      VALUE '-----'.                    CI0025
            10      S1-CL418-FLDNAM PICTURE X(9)                        CI0025
                                      VALUE '(XIRTNP'.                  CI0025
            10       S-CL418-OPER  PICTURE XX VALUE ' ='.               CI0025
            10       S-CL418-IRTNP    PICTURE  X.                       CI0025
            10  FILLER   PICTURE X    VALUE ')'.                        CI0025
       01   ZONES-UTILISATEUR PICTURE X.                                CI0025
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
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0025
          05              PA00-SUITE.                                   CI0025
            15       FILLER         PICTURE  X(00106).                  CI0025
       01                 PA06  REDEFINES      PA00.                    CI0025
            10            PA06-XDBPCB.                                  CI0025
            11            PA06-XDBDNM PICTURE  X(08).                   CI0025
            11            PA06-XSEGLV PICTURE  X(02).                   CI0025
            11            PA06-XRC    PICTURE  X(02).                   CI0025
            11            PA06-XPROPT PICTURE  X(04).                   CI0025
            11            PA06-FILLER PICTURE  S9(5)                    CI0025
                          BINARY.                                       CI0025
            11            PA06-XSEGNM PICTURE  X(08).                   CI0025
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0025
                          BINARY.                                       CI0025
            11            PA06-XSEGNB PICTURE  9(05)                    CI0025
                          BINARY.                                       CI0025
            11            PA06-XCOKEY PICTURE  X(70).                   CI0025
      *                                                                 AMDU30
      ******************************************************************AMDU30
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET A      *AMDU30
      **     BANK'S NAME AND RTN.                                      *AMDU30
      ******************************************************************AMDU30
      *                                                                 AMDU30
      *!WF DSP=BR DSL=DU SEL=30 FOR=I LEV=1                             AMDU30
       01                 BR00.                                         CI0025
          05              BR00-SUITE.                                   CI0025
            15       FILLER         PICTURE  X(00308).                  CI0025
       01                 BR30  REDEFINES      BR00.                    CI0025
            10            BR30-C199.                                    CI0025
            11            BR30-CLID.                                    CI0025
            12            BR30-CLIDO  PICTURE  9(3).                    CI0025
            12            BR30-CLIDN.                                   CI0025
            13            BR30-CLIDNP PICTURE  X(12).                   CI0025
            13            BR30-CLIDND PICTURE  9(8).                    CI0025
            10            BR30-CDEL1  PICTURE  9(3).                    CI0025
            10            BR30-DCACG  PICTURE  9(8).                    CI0025
            10            BR30-NRTSQ1 PICTURE  S9(3)                    CI0025
                          COMPUTATIONAL-3.                              CI0025
            10            BR30-FILLER PICTURE  X(100).                  CI0025
            10            BR30-CLORN  PICTURE  X(45).                   CI0025
            10            BR30-CL18.                                    CI0025
            11            BR30-CL18K.                                   CI0025
            12            BR30-NRTSQ  PICTURE  S9(3)                    CI0025
                          COMPUTATIONAL-3.                              CI0025
            11            BR30-NTR    PICTURE  9(8).                    CI0025
            11            BR30-GECKD  PICTURE  9.                       CI0025
            11            BR30-GEEND  PICTURE  9(8).                    CI0025
            11            BR30-NPDIN  PICTURE  X(4).                    CI0025
            11            BR30-IRTNA  PICTURE  X.                       CI0025
            11            BR30-IRTNP  PICTURE  X.                       CI0025
            11            BR30-IRTNW  PICTURE  X.                       CI0025
            10            BR30-IBNKI  PICTURE  X.                       CI0025
            10            BR30-FILLER PICTURE  X(100).                  CI0025
      *                                                                 AMDU30
      *                                                                 AMDU30
      *                                                                 AMDU30
      *                                                                 AMDU30
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0025
          05              DE00-SUITE.                                   CI0025
            15       FILLER         PICTURE  X(00653).                  CI0025
       01                 DE10  REDEFINES      DE00.                    CI0025
            10            DE10-DU11.                                    CI0025
            11            DE10-XFONC  PICTURE  X(4).                    CI0025
            11            DE10-MPSBN  PICTURE  X(8).                    CI0025
            11            DE10-XDBDNM PICTURE  X(08).                   CI0025
            11            DE10-XSEGNM PICTURE  X(08).                   CI0025
            11            DE10-XRC    PICTURE  X(02).                   CI0025
            11            DE10-MSEG   PICTURE  X(08).                   CI0025
            11            DE10-XCOKEY PICTURE  X(70).                   CI0025
            11            DE10-CUIBR  PICTURE  X(01).                   CI0025
            11            DE10-CUIBA  PICTURE  X(01).                   CI0025
            11            DE10-IPBIK  PICTURE  X(1).                    CI0025
            10            DE10-DU03.                                    CI0025
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0025
                          COMPUTATIONAL-3.                              CI0025
            11            DE10-CMSSF  PICTURE  XX.                      CI0025
            11            DE10-DU09.                                    CI0025
            12            DE10-CMESA  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            12            DE10-CMESB  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            12            DE10-CMSST  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            12            DE10-QELLAA PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            12            DE10-TMESS4 PICTURE  X(512).                  CI0025
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
       01                 MS00.                                         CI0025
          05              MS00-SUITE.                                   CI0025
            15       FILLER         PICTURE  X(00542).                  CI0025
       01                 MS03  REDEFINES      MS00.                    CI0025
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0025
                          COMPUTATIONAL-3.                              CI0025
            10            MS03-CMSSF  PICTURE  XX.                      CI0025
            10            MS03-DU09.                                    CI0025
            11            MS03-CMESA  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            11            MS03-CMESB  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            11            MS03-CMSST  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            11            MS03-QELLAA PICTURE  S9(9)                    CI0025
                          BINARY.                                       CI0025
            11            MS03-TMESS4 PICTURE  X(512).                  CI0025
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                BR30
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0025
      *               *                                   *             CI0025
      *               *INITIALISATIONS                    *             CI0025
      *               *                                   *             CI0025
      *               *************************************.            CI0025
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
      *N02CA.    NOTE *INITIALIZE BANK FOUND INDICATOR    *.
       F02CA.                                                           lv10
      *
      *********************************
      ** INITIALIZE THE BANK FOUND    *
      ** INDICATOR TO 'N'             *
      *********************************
      *
           MOVE        'N' TO BR30-IBNKI.
       F02CA-FN. EXIT.
      *N02DA.    NOTE *INITIALIZE CL18 SEGMENT            *.
       F02DA.                                                           lv10
      *
      *********************************
      ** INITIALIZE THE CL18 SEGMENT  *
      ** THAT WILL BE RETURNED TO     *
      ** CALLING MODULE.              *
      *********************************
      *
           INITIALIZE      BR30-CL18.
       F02DA-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *
      *********************************
      ** SET ADDRESSES FOR DATABASES  *
      *********************************
      *.
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0025
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0025
      *               *                                   *             CI0025
      *               *FIN DE TRAITEMENT                  *             CI0025
      *               *                                   *             CI0025
      *               *************************************.            CI0025
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0025
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
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *GET BANK NAME AND RTN              *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** READ THE CLIENT DATABASE TO  *
      ** GET THE BANK'S NAME AND RTN  *
      *********************************
      *
      *N50BA.    NOTE *SET UP SSA FOR CL01 READ           *.
       F50BA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CL01 READ     *
      ** USING CLIENT ID NUMBER       *
      ** FROM CX18 SEGMENT            *
      *********************************
      *
           MOVE        BR30-CLID TO S-CLU01-CLID.
       F50BA-FN. EXIT.
      *N50CA.    NOTE *READ CL01 SEGMENT                  *.
       F50CA.                                                           lv10
      *
      *********************************
      ** READ CL01 SEGMENT            *
      *********************************
      *
           PERFORM     F94CL THRU F94CL-FN.
       F50CA-FN. EXIT.
      *N50DA.    NOTE *CHECK IF CLIENT NUMBER NOT FOUND   *.
       F50DA.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
      *********************************
      ** IF CLIENT NUMBER NOT FOUND   *
      ** ON THE CLIENT DATABASE       *
      ** GET APPROPRIATE MESSAGE INFO *
      ** BY CALLING CI0002 AND RETURN *
      ** TO CALLING MODULE.           *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012012 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN.                             ADU019
      *N50EA.    NOTE *RETURN TO CALLING MODULE           *.
       F50EA.                                                           lv15
      *
      *********************************
      ** RETURN TO CALLING MODULE     *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F50EA-FN. EXIT.
       F50DA-FN. EXIT.
      *N50FA.    NOTE *CLIENT IS AN ORGANIZATION          *.
       F50FA.    IF    CL01-CLTYP = 'O'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50FA-FN.
      *
      *********************************
      ** READ CL12 SEGMENT FOR        *
      ** ORGANIZATION NAME            *
      *********************************
      *
      *N50GA.    NOTE *READ CL12 SEGMENT                  *.
       F50GA.                                                           lv15
      *
      *********************************
      ** READ CL12 SEGMENT FOR BANK   *
      ** NAME                         *
      *********************************
      *
           PERFORM     F94CO THRU F94CO-FN.
       F50GA-FN. EXIT.
      *N50HA.    NOTE *CHECK IF CL12 SEGMENT NOT FOUND    *.
       F50HA.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50HA-FN.
      *
      *********************************
      ** IF CL12 SEGMENT NOT FOUND,   *
      ** MOVE 'UNKNOWN' TO NAME       *
      *********************************
      *
           MOVE        'UNKNOWN' TO BR30-CLORN.
       F50HA-900. GO TO F50IA-FN.
       F50HA-FN. EXIT.
      *N50IA.    NOTE *CL12 SEGMENT WAS FOUND             *.
       F50IA.                                                           lv15
      *
      *********************************
      ** CL12 SEGMENT WAS FOUND, CHECK*
      ** TO SEE IF SHOULD SEARCH FOR  *
      ** FIRST ACTIVE CL18 OR HAS THE *
      ** SEQUENCE NUMBER BEEN SUPPLIED*
      *********************************
      *
           MOVE        CL12-CLORN TO BR30-CLORN.
      *N50JA.    NOTE *SEARCH FOR CL18 SEGMENT            *.
       F50JA.    IF    BR30-NRTSQ1 = ZEROS                              lv20
                 NEXT SENTENCE ELSE GO TO     F50JA-FN.
      *
      *********************************
      ** IF THE CL18 SEQUENCE NUMBER  *
      ** IS ZEROS, SEARCH FOR THE     *
      ** FIRST ACTIVE CL18.           *
      *********************************
      *
      *N50KA.    NOTE *READ FIRST CL18 SEGMENT            *.
       F50KA.                                                           lv25
      *
      *********************************
      ** READ FIRST CL18 SEGMENT UNDER*
      ** CL12 SEGMENT                 *
      *********************************
      *
           PERFORM     F94CR THRU F94CR-FN.
       F50KA-FN. EXIT.
      *N50LA.    NOTE *LOOP WHILE MORE CL18 SEGMENTS      *.
       F50LA.    IF    IK = '0'                                         lv25
                 AND   DE10-NMESS2 = ZEROS
                 AND   BR30-IBNKI = 'N'
                 NEXT SENTENCE ELSE GO TO     F50LA-FN.
      *AN ACTIVE CL18 SEGMENT NOT FOUND
      *********************************
      ** LOOP WHILE MORE CL18 SEGMENT *
      ** AND AN ACTIVE CL18 SEGMENT   *
      ** WAS NOT FOUND.               *
      *********************************
      *
      *N50MA.    NOTE *CHECK IF ACTIVE CL18 SEGMENT       *.
       F50MA.    IF    CL18-GEEND = ZEROS                               lv30
                 NEXT SENTENCE ELSE GO TO     F50MA-FN.
      *
      *********************************
      ** IF AN ACTIVE CL18 SEGMENT    *
      ** FOUND.                       *
      *********************************
      *
      *N50NA.    NOTE *CHECK WHICH TYPE OF RTN NEEDED     *.
       F50NA.    IF    (BR30-CDEL1 = 001 AND                            lv35
                       CL18-IRTNW = 'Y')
                 OR    (BR30-CDEL1 = 002 AND
                       CL18-IRTNP = 'Y')
                 OR    (BR30-CDEL1 = 003 AND
                       CL18-IRTNA = 'Y')
                 NEXT SENTENCE ELSE GO TO     F50NA-FN.
      *********************************
      ** CHECK WHAT TYPE OF RTN IS    *
      ** NEEDED(PAPER,WIRE OR ACH) AND*
      ** LOOK FOR THE APPROPRIATE     *
      ** CL18 SEGMENT.                *
      *********************************
      *
           MOVE        'Y' TO BR30-IBNKI
               GO TO     F50LA-900.
       F50NA-FN. EXIT.
       F50MA-FN. EXIT.
      *N50OA.    NOTE *READ NEXT CL18 SEGMENT             *.
       F50OA.                                                           lv30
      *
      *********************************
      ** READ NEXT CL18 SEGMENT UNDER *
      ** CL12 SEGMENT                 *
      *********************************
      *
           PERFORM     F94CR THRU F94CR-FN.
       F50OA-FN. EXIT.
       F50LA-900. GO TO F50LA.
       F50LA-FN. EXIT.
      *N50PA.    NOTE *CHECK IF CL18 SEGMENT WAS FOUND    *.
       F50PA.    IF    BR30-IBNKI = 'Y'                                 lv25
                 NEXT SENTENCE ELSE GO TO     F50PA-FN.
      *
      *********************************
      ** USE DEFAULT VALUES FOR NAME  *
      ** AND RTN                      *
      *********************************
      *
           MOVE        CL18 TO BR30-CL18.
       F50PA-FN. EXIT.
       F50JA-900. GO TO F50QA-FN.
       F50JA-FN. EXIT.
      *N50QA.    NOTE *SEQUENCE NUMBER WAS SUPPLIED       *.
       F50QA.                                                           lv20
      *
      *********************************
      ** THE CALLING MODULE WOULD LIKE*
      ** A SPECIFIC CL18 SEGMENT      *
      *********************************
      *
      *N50RA.    NOTE *BUILD CL18 SSA                     *.
       F50RA.                                                           lv25
      *
      *********************************
      ** BUILD THE SSA FOR CL18       *
      *********************************
      *
           MOVE        BR30-NRTSQ1 TO S-CLU18-NRTSQ.
       F50RA-FN. EXIT.
      *N50SA.    NOTE *READ THE CL18 SEGMENT              *.
       F50SA.                                                           lv25
      *
      *********************************
      ** READ THE CL18 SEGMENT        *
      *********************************
      *
           PERFORM     F94CB THRU F94CB-FN.
       F50SA-FN. EXIT.
      *N50TA.    NOTE *CHECK IF CL18 SEGMENT WAS FOUND    *.
       F50TA.    IF    IK = '0'                                         lv25
                 AND   DE10-NMESS2 = ZEROS
                 NEXT SENTENCE ELSE GO TO     F50TA-FN.
      *********************************
      ** MOVE THE CL18 SEGMENT        *
      *********************************
      *
           MOVE        CL18 TO BR30-CL18
           MOVE        'Y' TO BR30-IBNKI.
       F50TA-FN. EXIT.
       F50QA-FN. EXIT.
       F50IA-FN. EXIT.
       F50FA-900. GO TO F50ZA-FN.
       F50FA-FN. EXIT.
      *N50ZA.    NOTE *CLIENT IS A PERSON                 *.
       F50ZA.                                                           lv10
      *
      *********************************
      ** USE DEFAULT VALUES FOR NAME  *
      ** AND RTN                      *
      *********************************
      *
           MOVE        'UNKNOWN' TO BR30-CLORN.
       F50ZA-FN. EXIT.
       F50-FN.   EXIT.
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
      *               *DLI FUNCTIONS                      *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CB.    NOTE *CALL GU ON CL18                    *.            ADU026
       F94CB.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CL18                                                    ADU026
           S-CLU01-SSA S-CL12-SSA S-CLU18-SSA                           ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CB-FN. EXIT.
      *N94CL.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CL.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
      *N94CO.    NOTE *CALL GN ON CL12                    *.            ADU026
       F94CO.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CL12                                                    ADU026
           S-CLU01-SSA S-CL12-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CO-FN. EXIT.
      *N94CR.    NOTE *CALL GNP ON CL18                   *.            ADU026
       F94CR.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGNP                        ADU026
           PA06 CL18                                                    ADU026
           S-CLU01-SSA S-CL12-SSA S-CL18-SSA                            ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGNP TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CR-FN. EXIT.
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
