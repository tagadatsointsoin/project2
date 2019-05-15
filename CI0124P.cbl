       IDENTIFICATION DIVISION.                                         CI0124
       PROGRAM-ID.  CI0124P.                                            CI0124
      *AUTHOR.         GET ACCOUNT HOLD CODES.                          CI0124
      *DATE-COMPILED.   09/08/14.                                       CI0124
       ENVIRONMENT DIVISION.                                            CI0124
       CONFIGURATION SECTION.                                           CI0124
       SOURCE-COMPUTER. IBM-370.                                        CI0124
       OBJECT-COMPUTER. IBM-370.                                        CI0124
       DATA DIVISION.                                                   CI0124
       WORKING-STORAGE SECTION.                                         CI0124
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01                 CT00.                                         CI0124
            02            CT01.                                         CI0124
            10            CT01-CT01K.                                   CI0124
            11            CT01-C299.                                    CI0124
            12            CT01-CTID.                                    CI0124
            13            CT01-CTIDA  PICTURE  9(3).                    CI0124
            13            CT01-CTIDN.                                   CI0124
            14            CT01-CTIDNP PICTURE  X(13).                   CI0124
            14            CT01-CTIDND PICTURE  9(11).                   CI0124
            10            CT01-GECKD  PICTURE  9.                       CI0124
            10            CT01-GEMDA  PICTURE  9(8).                    CI0124
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0124
                          BINARY.                                       CI0124
            10            CT01-GECUC  PICTURE  99.                      CI0124
            10            CT01-CTAUL  PICTURE  9(3).                    CI0124
            10            CT01-DIRAC  PICTURE  9(4).                    CI0124
            10            CT01-CTCCI  PICTURE  X.                       CI0124
            10            CT01-CTCUS  PICTURE  999.                     CI0124
            10            CT01-CTEFD  PICTURE  9(8).                    CI0124
            10            CT01-CTIAD  PICTURE  9(8).                    CI0124
            10            CT01-CLCUS  PICTURE  99.                      CI0124
            10            CT01-CAMMB  PICTURE  X(3).                    CI0124
            10            CT01-CKPMM  PICTURE  X.                       CI0124
            10            CT01-CTLAD  PICTURE  9(8).                    CI0124
            10            CT01-IPERS  PICTURE  X.                       CI0124
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0124
                          COMPUTATIONAL-3.                              CI0124
            10            CT01-CTLAT  PICTURE  9(8).                    CI0124
            10            CT01-CTLATC PICTURE  9(6).                    CI0124
            10            CT01-IMEGA  PICTURE  X.                       CI0124
            10            CT01-DIRAB  PICTURE  9(8).                    CI0124
            10            CT01-COLRQ  PICTURE  X.                       CI0124
            10            CT01-ZDA04  PICTURE  X(4).                    CI0124
            10            CT01-CTLPD  PICTURE  9(8).                    CI0124
            10            CT01-CIRASP PICTURE  9.                       CI0124
            10            CT01-CIRATP PICTURE  99.                      CI0124
            10            CT01-DRTHC  PICTURE  9(8).                    CI0124
            10            CT01-CPPTC  PICTURE  X.                       CI0124
            10            CT01-ZDA06  PICTURE  X(6).                    CI0124
            10            CT01-CTACD  PICTURE  9(8).                    CI0124
            10            CT01-CTNLI  PICTURE  X.                       CI0124
            10            CT01-CTRHO  PICTURE  9(8).                    CI0124
            10            CT01-CTSGD  PICTURE  9(8).                    CI0124
            10            CT01-CPATP  PICTURE  X(1).                    CI0124
            10            CT01-IRSTA  PICTURE  X.                       CI0124
            10            CT01-CTSTA  PICTURE  99.                      CI0124
            10            CT01-CTSSC  PICTURE  99.                      CI0124
            10            CT01-PRLIN  PICTURE  9(3).                    CI0124
            10            CT01-PRCOD  PICTURE  9(5).                    CI0124
            10            CT01-PRSCD  PICTURE  X(9).                    CI0124
            10            CT01-CTLNI  PICTURE  X.                       CI0124
            10            CT01-AYSIDA PICTURE  9(3).                    CI0124
            10            CT01-AYSID  PICTURE  9(5).                    CI0124
            10            CT01-CTBMC  PICTURE  99.                      CI0124
            10            CT01-CINAR  PICTURE  99.                      CI0124
            10            CT01-CPHTR  PICTURE  X.                       CI0124
            10            CT01-CDSTR  PICTURE  XX.                      CI0124
            10            CT01-CQACT  PICTURE  999.                     CI0124
            10            CT01-CIRAS  PICTURE  999.                     CI0124
            10            CT01-CIRAT  PICTURE  999.                     CI0124
            10            CT01-CLRAY  PICTURE  9(5).                    CI0124
            10            CT01-CATTP  PICTURE  X.                       CI0124
            02            CT13.                                         CI0124
            10            CT13-CT13K.                                   CI0124
            11            CT13-GEHSD  PICTURE  9(8).                    CI0124
            11            CT13-GEHCD  PICTURE  9(3).                    CI0124
            11            CT13-GEHCSE PICTURE  X(12).                   CI0124
            11            CT13-GEHCSU PICTURE  9(5).                    CI0124
            10            CT13-GEHRD  PICTURE  9(8).                    CI0124
            10            CT13-GEHV   PICTURE  S9(7)                    CI0124
                          COMPUTATIONAL-3.                              CI0124
            10            CT13-GEDC   PICTURE  9(2).                    CI0124
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0124
            10            XW05-XW06.                                    CI0124
            11            XW05-XDBPCB.                                  CI0124
            12            XW05-XDBDNM PICTURE  X(08)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-XSEGLV PICTURE  X(02)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-XRC    PICTURE  X(02)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-XPROPT PICTURE  X(04)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-FILLER PICTURE  S9(5)                    CI0124
                          VALUE                ZERO                     CI0124
                          BINARY.                                       CI0124
            12            XW05-XSEGNM PICTURE  X(08)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0124
                          VALUE                ZERO                     CI0124
                          BINARY.                                       CI0124
            12            XW05-XSEGNB PICTURE  9(05)                    CI0124
                          VALUE                ZERO                     CI0124
                          BINARY.                                       CI0124
            12            XW05-XCOKEY PICTURE  X(70)                    CI0124
                          VALUE                SPACE.                   CI0124
            10            XW05-XW07.                                    CI0124
            11            XW05-XIOPCB.                                  CI0124
            12            XW05-XTERMI PICTURE  X(08)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-FILLER PICTURE  XX                       CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-XRC1   PICTURE  X(02)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-FILLER PICTURE  X(12)                    CI0124
                          VALUE                SPACE.                   CI0124
            12            XW05-XMODNM PICTURE  X(8)                     CI0124
                          VALUE                SPACE.                   CI0124
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0124
                          VALUE                ZERO.                    CI0124
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0124
                          VALUE                ZERO.                    CI0124
            10            XW05-XGU    PICTURE  X(4)                     CI0124
                          VALUE                'GU  '.                  CI0124
            10            XW05-XGHU   PICTURE  X(4)                     CI0124
                          VALUE                'GHU '.                  CI0124
            10            XW05-XGN    PICTURE  X(4)                     CI0124
                          VALUE                'GN  '.                  CI0124
            10            XW05-XGHN   PICTURE  X(4)                     CI0124
                          VALUE                'GHN '.                  CI0124
            10            XW05-XGNP   PICTURE  X(4)                     CI0124
                          VALUE                'GNP '.                  CI0124
            10            XW05-XGHNP  PICTURE  X(4)                     CI0124
                          VALUE                'GHNP'.                  CI0124
            10            XW05-XREPL  PICTURE  XXXX                     CI0124
                          VALUE                'REPL'.                  CI0124
            10            XW05-XISRT  PICTURE  X(4)                     CI0124
                          VALUE                'ISRT'.                  CI0124
            10            XW05-XDLET  PICTURE  X(4)                     CI0124
                          VALUE                'DLET'.                  CI0124
            10            XW05-XOPEN  PICTURE  X(4)                     CI0124
                          VALUE                'OPEN'.                  CI0124
            10            XW05-XCLSE  PICTURE  X(4)                     CI0124
                          VALUE                'CLSE'.                  CI0124
            10            XW05-XCHKP  PICTURE  X(4)                     CI0124
                          VALUE                'CHKP'.                  CI0124
            10            XW05-XXRST  PICTURE  X(4)                     CI0124
                          VALUE                'XRST'.                  CI0124
            10            XW05-XTERM  PICTURE  X(4)                     CI0124
                          VALUE                'TERM'.                  CI0124
            10            XW05-XNFPAC PICTURE  X(13)                    CI0124
                          VALUE                SPACE.                   CI0124
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0124
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0124
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
      ******************************************************************ADUTAB
      **              TABLE TA33 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA33-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=33 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA33.                                                CI0124
           04    G-TA33-PARAM.                                          CI0124
             10  G-TA33-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0124
                        VALUE      +063.                                CI0124
             10  G-TA33-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0124
                        VALUE      +001.                                CI0124
             10  G-TA33-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0124
                        VALUE      +003.                                CI0124
             10  G-TA33-NUAPP  PICTURE 99                               CI0124
                        VALUE       0.                                  CI0124
             10  G-TA33-NUTAB  PICTURE X(6)                             CI0124
                        VALUE 'GEHCD '.                                 CI0124
             10  G-TA33-TABFO  PICTURE XX                 VALUE SPACE.  CI0124
             10  G-TA33-TABCR  PICTURE XX                 VALUE SPACE.  CI0124
             10  G-TA33-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0124
             10  G-TA33-NUSSC  PICTURE X  VALUE   ' '.                  CI0124
             10  G-TA33-NUSSY  PICTURE X                  VALUE SPACE.  CI0124
             10  G-TA33-TRANID PICTURE X(4)               VALUE SPACE.  CI0124
             10  G-TA33-FILSYS.                                         CI0124
             15  G-TA33-USERC  PICTURE X(6)               VALUE SPACE.  CI0124
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0124
           04             TA33.                                         CI0124
            10            TA33-GEHCD  PICTURE  9(3)                     CI0124
                          VALUE                ZERO.                    CI0124
            10            TA33-GEHCDD PICTURE  X(40)                    CI0124
                          VALUE                SPACE.                   CI0124
            10            TA33-THCDM  PICTURE  X(20)                    CI0124
                          VALUE                SPACE.                   CI0124
      **                                                                ADUTAB
      *-----> MISCELLANEOUS WORKING STORAGE ELEMENTS...

      *9'S COMPLIMENT WORKING STORAGE DATE FIELDS...
      *!WE
       01                 WS13-GEHSD
                        PICTURE 9(8).                                   CI0124
      *!WE
       01                 WS13-GEHRD
                        PICTURE 9(8).                                   CI0124
       01   DEBUT-WSS.                                                  CI0124
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0124
            05   IK     PICTURE X.                                      CI0124
       01  CONSTANTES-PAC.                                              CI0124
           05  FILLER  PICTURE X(87)   VALUE                            CI0124
                     '6015 CAT09/08/14CI0124ADMIN   14:34:56CI0124P AMERCI0124
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0124
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0124
           05  NUGNA   PICTURE X(5).                                    CI0124
           05  APPLI   PICTURE X(3).                                    CI0124
           05  DATGN   PICTURE X(8).                                    CI0124
           05  PROGR   PICTURE X(6).                                    CI0124
           05  CODUTI  PICTURE X(8).                                    CI0124
           05  TIMGN   PICTURE X(8).                                    CI0124
           05  PROGE   PICTURE X(8).                                    CI0124
           05  COBASE  PICTURE X(4).                                    CI0124
           05  DATGNC  PICTURE X(10).                                   CI0124
           05  RELEAS  PICTURE X(7).                                    CI0124
           05  DATGE   PICTURE X(10).                                   CI0124
           05  DATSQ   PICTURE X(10).                                   CI0124
       01  DATCE.                                                       CI0124
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0124
         05  DATOR.                                                     CI0124
           10  DATOA  PICTURE XX.                                       CI0124
           10  DATOM  PICTURE XX.                                       CI0124
           10  DATOJ  PICTURE XX.                                       CI0124
       01   VARIABLES-CONDITIONNELLES.                                  CI0124
            05                  FT      PICTURE X VALUE '0'.            CI0124
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0124
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0124
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
            05           J55CFR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0124
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0124
       01               S-CT01-SSA.                                     CI0124
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0124
                                      VALUE 'CT01    '.                 CI0124
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0124
            10          S-CT01-CCOD   PICTURE X(5)                      CI0124
                                      VALUE '-----'.                    CI0124
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0124
       01            S-CTU01-SSA.                                       CI0124
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0124
                                      VALUE 'CT01    '.                 CI0124
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0124
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0124
                                      VALUE '-----'.                    CI0124
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0124
                                      VALUE '(CT01K'.                   CI0124
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0124
            10       S-CTU01-CT01K.                                     CI0124
            11       S-CTU01-C299.                                      CI0124
            12       S-CTU01-CTID.                                      CI0124
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0124
            13       S-CTU01-CTIDN.                                     CI0124
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0124
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0124
            10  FILLER   PICTURE X    VALUE ')'.                        CI0124
       01               S-CT13-SSA.                                     CI0124
            10         S1-CT13-SEGNAM PICTURE X(8)                      CI0124
                                      VALUE 'CT13    '.                 CI0124
            10         S1-CT13-CCOM   PICTURE X VALUE '*'.              CI0124
            10          S-CT13-CCOD   PICTURE X(5)                      CI0124
                                      VALUE '-----'.                    CI0124
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0124
       01            S-CTU13-SSA.                                       CI0124
            10      S1-CTU13-SEGNAM PICTURE X(8)                        CI0124
                                      VALUE 'CT13    '.                 CI0124
            10      S1-CTU13-CCOM   PICTURE X VALUE '*'.                CI0124
            10       S-CTU13-CCOD   PICTURE X(5)                        CI0124
                                      VALUE '-----'.                    CI0124
            10      S1-CTU13-FLDNAM PICTURE X(9)                        CI0124
                                      VALUE '(CT13K'.                   CI0124
            10       S-CTU13-OPER  PICTURE XX VALUE ' ='.               CI0124
            10       S-CTU13-CT13K.                                     CI0124
            11       S-CTU13-GEHSD    PICTURE  9(8).                    CI0124
            11       S-CTU13-GEHCD    PICTURE  9(3).                    CI0124
            11       S-CTU13-GEHCSE   PICTURE  X(12).                   CI0124
            11       S-CTU13-GEHCSU   PICTURE  9(5).                    CI0124
            10  FILLER   PICTURE X    VALUE ')'.                        CI0124
       01   ZONES-UTILISATEUR PICTURE X.                                CI0124
       LINKAGE SECTION.                                                 ADU102
      *>>>>>>   DLIUIBII Copybook                                       ADU129
            COPY  DLIUIBII.                                             ADU129
      *                                                                 ADU129
      *>>>>>> Address list of PCB's                                     ADU129
      *                                                                 ADU129
       01   PCB-ADDRESS-LIST.                                           ADU129
      *                                                                 ADU129
      *NOTE: All PCB pointers must be added here using macro ADU015     ADU129
      *      once for each database used                                ADU129
      *                                                                 ADU129
      *NOTE: Following PCB pointers, include PCB masks using ADU015     ADU129
      *      once for each database used                                ADU129
      *                                                                 ADU129
      *-----------------------------------------------------------------ADU129
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0124
          05              PC00-SUITE.                                   CI0124
            15       FILLER         PICTURE  X(00106).                  CI0124
       01                 PC06  REDEFINES      PC00.                    CI0124
            10            PC06-XDBPCB.                                  CI0124
            11            PC06-XDBDNM PICTURE  X(08).                   CI0124
            11            PC06-XSEGLV PICTURE  X(02).                   CI0124
            11            PC06-XRC    PICTURE  X(02).                   CI0124
            11            PC06-XPROPT PICTURE  X(04).                   CI0124
            11            PC06-FILLER PICTURE  S9(5)                    CI0124
                          BINARY.                                       CI0124
            11            PC06-XSEGNM PICTURE  X(08).                   CI0124
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0124
                          BINARY.                                       CI0124
            11            PC06-XSEGNB PICTURE  9(05)                    CI0124
                          BINARY.                                       CI0124
            11            PC06-XCOKEY PICTURE  X(70).                   CI0124
      *-----> SEGMENT LINKAGE FOR INPUT AND OUTPUT
      *
      *!WF DSP=PJ DSL=PJ SEL=01 FOR=I DES=1 LEV=1 PLT=80
       01                 PJ01.                                         CI0124
            10            PJ01-CTID   PICTURE  X(27).                   CI0124
            10            PJ01-DASOF  PICTURE  9(8).                    CI0124
            10            PJ01-CT13                                     CI0124
                          OCCURS       003     TIMES.                   CI0124
            11            PJ01-CT13K.                                   CI0124
            12            PJ01-GEHSD  PICTURE  9(8).                    CI0124
            12            PJ01-GEHCD  PICTURE  9(3).                    CI0124
            12            PJ01-GEHCSE PICTURE  X(12).                   CI0124
            12            PJ01-GEHCSU PICTURE  9(5).                    CI0124
            11            PJ01-GEHRD  PICTURE  9(8).                    CI0124
            11            PJ01-GEHV   PICTURE  S9(7)                    CI0124
                          COMPUTATIONAL-3.                              CI0124
            11            PJ01-GEDC   PICTURE  9(2).                    CI0124
            10            PJ01-GEHCDD PICTURE  X(40)                    CI0124
                          OCCURS       003     TIMES.                   CI0124
            10            PJ01-THCDM  PICTURE  X(20)                    CI0124
                          OCCURS       003     TIMES.                   CI0124
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0124
          05              DE00-SUITE.                                   CI0124
            15       FILLER         PICTURE  X(00653).                  CI0124
       01                 DE10  REDEFINES      DE00.                    CI0124
            10            DE10-DU11.                                    CI0124
            11            DE10-XFONC  PICTURE  X(4).                    CI0124
            11            DE10-MPSBN  PICTURE  X(8).                    CI0124
            11            DE10-XDBDNM PICTURE  X(08).                   CI0124
            11            DE10-XSEGNM PICTURE  X(08).                   CI0124
            11            DE10-XRC    PICTURE  X(02).                   CI0124
            11            DE10-MSEG   PICTURE  X(08).                   CI0124
            11            DE10-XCOKEY PICTURE  X(70).                   CI0124
            11            DE10-CUIBR  PICTURE  X(01).                   CI0124
            11            DE10-CUIBA  PICTURE  X(01).                   CI0124
            11            DE10-IPBIK  PICTURE  X(1).                    CI0124
            10            DE10-DU03.                                    CI0124
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0124
                          COMPUTATIONAL-3.                              CI0124
            11            DE10-CMSSF  PICTURE  XX.                      CI0124
            11            DE10-DU09.                                    CI0124
            12            DE10-CMESA  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            12            DE10-CMESB  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            12            DE10-CMSST  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            12            DE10-QELLAA PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            12            DE10-TMESS4 PICTURE  X(512).                  CI0124
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0124
          05              MS00-SUITE.                                   CI0124
            15       FILLER         PICTURE  X(00542).                  CI0124
       01                 MS03  REDEFINES      MS00.                    CI0124
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0124
                          COMPUTATIONAL-3.                              CI0124
            10            MS03-CMSSF  PICTURE  XX.                      CI0124
            10            MS03-DU09.                                    CI0124
            11            MS03-CMESA  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            11            MS03-CMESB  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            11            MS03-CMSST  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            11            MS03-QELLAA PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
            11            MS03-TMESS4 PICTURE  X(512).                  CI0124
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0124
            10            MX11-QMSGS  PICTURE  9(03).                   CI0124
            10            MX11-PJ09                                     CI0124
                          OCCURS       025     TIMES.                   CI0124
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0124
                          COMPUTATIONAL-3.                              CI0124
            11            MX11-CMESB  PICTURE  S9(9)                    CI0124
                          BINARY.                                       CI0124
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ01
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0124
      *               *                                   *             CI0124
      *               *INITIALISATIONS                    *             CI0124
      *               *                                   *             CI0124
      *               *************************************.            CI0124
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
      *N02XA.    NOTE *SET ADDRESS FOR CT1P               *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0124
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0124
      *               *                                   *             CI0124
      *               *FIN DE TRAITEMENT                  *             CI0124
      *               *                                   *             CI0124
      *               *************************************.            CI0124
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0124
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *MAIN PROCESSING LOOP FOR MODULE    *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55BB.    NOTE *Access CT01 Contract Segment       *.
       F55BB.                                                           lv10
      *
           MOVE        PJ01-CTID TO S-CTU01-CTID
           PERFORM     F94CT THRU F94CT-FN.
       F55BB-FN. EXIT.
      *N55CC.    NOTE *Error on Access Failure            *.
       F55CC.    IF    IK = '1'                                         lv10
                 OR    DE10-NMESS2 NOT = ZERO
                 NEXT SENTENCE ELSE GO TO     F55CC-FN.
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012234 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
       F55CC-900. GO TO F55CD-FN.
       F55CC-FN. EXIT.
      *N55CD.    NOTE *Look for first CT13 Hold Segment   *.
       F55CD.                                                           lv10
           PERFORM     F94CP THRU F94CP-FN.
      *N55CF.    NOTE *Initialize Output Segment          *.
       F55CF.                                                           lv15
           MOVE        1                        TO J55CFR
                                    GO TO     F55CF-B.
       F55CF-A.
           ADD         1                        TO J55CFR.
       F55CF-B.
           IF          J55CFR                   >  3
                                    GO TO     F55CF-FN.
           INITIALIZE  PJ01-CT13 (J55CFR)
           INITIALIZE  PJ01-GEHCDD (J55CFR)
           INITIALIZE  PJ01-THCDM (J55CFR).
       F55CF-900. GO TO F55CF-A.
       F55CF-FN. EXIT.
      *N55CG.    NOTE *Initialize loop variables...       *.
       F55CG.                                                           lv15
           MOVE        ZERO TO J55CFR.
       F55CG-FN. EXIT.
      *N55CH.    NOTE *Process up to three CT13's         *.
       F55CH.    IF    IK = 0                                           lv15
                 AND   J55CFR < 3
                 NEXT SENTENCE ELSE GO TO     F55CH-FN.
      *Decode the Dates...
           COMPUTE     WS13-GEHSD = 999999999 -
           CT13-GEHSD.
      *N55CM.    NOTE *---> Select records where..        *.
       F55CM.    IF    WS13-GEHSD NOT > PJ01-DASOF                      lv20
                 AND   (CT13-GEHRD = ZERO
                 OR    CT13-GEHRD NOT <
                       PJ01-DASOF)
                 NEXT SENTENCE ELSE GO TO     F55CM-FN.
      *Start date is before selection
      *date and the end date is
      *either not set or is on or
      *after the selection date...
           ADD         1 TO J55CFR
           MOVE        CT13 TO PJ01-CT13 (J55CFR)
      *DECODE HOLD CODE DEFINITIONS...
           MOVE        CT13-GEHCD TO TA33-GEHCD
           PERFORM     F92HC THRU F92HC-FN
           MOVE        TA33-GEHCDD TO PJ01-GEHCDD (J55CFR)
           MOVE        TA33-THCDM TO PJ01-THCDM (J55CFR).
       F55CM-FN. EXIT.
      *N55CX.    NOTE *Read next CT13...                  *.
       F55CX.                                                           lv20
           PERFORM     F94CP THRU F94CP-FN.
       F55CX-FN. EXIT.
       F55CH-900. GO TO F55CH.
       F55CH-FN. EXIT.
       F55CD-FN. EXIT.
       F55-FN.   EXIT.
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
      *N92HC.    NOTE *RANDOM TABLE READ FOR TA33         *.            ADUTAB
       F92HC.                                                           lv10
           MOVE        'R1' TO G-TA33-TABFO                             ADUTAB
           COMPUTE     G-TA33-LTH = 60 + G-TA33-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA33-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA33)                                ADUTAB
                       LENGTH (G-TA33-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA33-TABCR NOT = '00'                          DOT
      *---  Error Process ---
           MOVE        'UNKOWN' TO TA33-GEHCDD
           TA33-THCDM.
      *---  ENDIF  ---                                                  DOT
       F92HC-FN. EXIT.
      *N93.      NOTE *************************************.            ADU129
      *               *                                   *             ADU129
      *               *---> Common DL/1 Error Checks      *             ADU129
      *               *                                   *             ADU129
      *               *************************************.            ADU129
       F93.           EXIT.                                             lv05
      *N93EA.    NOTE *---> DL/1 I/O Error Checks         *.            ADU129
       F93EA.                                                           lv10
                 IF    XW05-XRC = '  '                                  DOT
           MOVE        ZERO TO IK                                       ADU129
                 ELSE                                                   ADU129
           MOVE        '1' TO IK                                        ADU129
           DE10-IPBIK                                                   ADU129
           INITIALIZE  DE10-DU03                                        ADU129
           PERFORM     F93UI THRU F93UI-FN                              ADU129
           PERFORM     F93PC THRU F93PC-FN.                             ADU129
       F93EA-FN. EXIT.
      *N93PC.    NOTE *---> DL/1 PCB Check via CI0009P    *.            ADU129
       F93PC.                                                           lv10
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU129
           MOVE        XW05-XRC TO DE10-XRC                             ADU129
           MOVE        XW05-XSEGNM TO DE10-MSEG                         ADU129
           MOVE        XW05-XCOKEY TO DE10-XCOKEY                       ADU129
           MOVE        XW05-XDBDNM TO DE10-XDBDNM                       ADU129
      *                                                                 ADU129
           MOVE        'CI0009P ' TO W-PASS-XPROGR                      ADU129
           CALL        W-PASS-XPROGR                                    ADU129
           USING DFHEIBLK                                               ADU129
           DFHCOMMAREA                                                  ADU129
           DE10                                                         ADU129
           MS03.                                                        ADU129
      *N93PD.    NOTE *---> Exit on severe DL/1 Error     *.            ADU129
       F93PD.    IF    DE10-NMESS2 NOT = ZERO                           lv15
                 NEXT SENTENCE ELSE GO TO     F93PD-FN.                 ADU129
           MOVE                     ALL '1' TO FT GO TO F20.            ADU129
       F93PD-FN. EXIT.
       F93PC-FN. EXIT.
      *N93UI.    NOTE *---> DL/1 User Intrface BLK(UIB)   *.            ADU129
       F93UI.                                                           lv10
      *     via CI0008P                                                 ADU129
           MOVE        SV01-FUNC TO DE10-XFONC                          ADU129
           MOVE        UIBFCTR TO DE10-CUIBR                            ADU129
           MOVE        UIBDLTR TO DE10-CUIBA                            ADU129
      *                                                                 ADU129
           MOVE        'CI0008P ' TO W-PASS-XPROGR                      ADU129
           CALL        W-PASS-XPROGR                                    ADU129
           USING DFHEIBLK                                               ADU129
           DFHCOMMAREA                                                  ADU129
           DE10                                                         ADU129
           MS03.                                                        ADU129
       F93UI-FN. EXIT.
       F93-FN.   EXIT.
      *N94CP.    NOTE *CALL GNP ON CT13                   *.            ADU026
       F94CP.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGNP                        ADU026
           PC06 CT13                                                    ADU026
           S-CTU01-SSA S-CT13-SSA                                       ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGNP TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CP-FN. EXIT.
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
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
