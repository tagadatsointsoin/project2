       IDENTIFICATION DIVISION.                                         CI0069
       PROGRAM-ID.  CI0069P.                                            CI0069
      *AUTHOR.         M\M - DETERMINE PAYMENT DATES.                   CI0069
      *DATE-COMPILED.   09/08/14.                                       CI0069
       ENVIRONMENT DIVISION.                                            CI0069
       CONFIGURATION SECTION.                                           CI0069
       SOURCE-COMPUTER. IBM-370.                                        CI0069
       OBJECT-COMPUTER. IBM-370.                                        CI0069
       DATA DIVISION.                                                   CI0069
       WORKING-STORAGE SECTION.                                         CI0069
       01                 CM00.                                         CI0069
            02            CM01.                                         CI0069
            10            CM01-CEKCNT.                                  CI0069
            11            CM01-CEGCN.                                   CI0069
            12            CM01-CEPRE  PICTURE  9(4).                    CI0069
            12            CM01-CEBAS  PICTURE  9(8).                    CI0069
            10            CM01-CEMAD  PICTURE  9(8).                    CI0069
            10            CM01-CEAUD  PICTURE  9(8).                    CI0069
            10            CM01-CEBKD  PICTURE  9(8).                    CI0069
            10            CM01-CELAD  PICTURE  9(8).                    CI0069
            10            CM01-CECSD  PICTURE  9(8).                    CI0069
            10            CM01-CECOM  PICTURE  9(2).                    CI0069
            10            CM01-CEPDT  PICTURE  9(4).                    CI0069
            10            CM01-CECHK  PICTURE  9(1).                    CI0069
            10            CM01-CECTC  PICTURE  X(1).                    CI0069
            10            CM01-CEST   PICTURE  9(1).                    CI0069
            10            CM01-CETYP  PICTURE  9(2).                    CI0069
            10            CM01-CECLO  PICTURE  9(1).                    CI0069
            10            CM01-CEAV   PICTURE  9(1).                    CI0069
            10            CM01-CE30M  PICTURE  X(1).                    CI0069
            10            CM01-CEBCD  PICTURE  S9(3)                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CEUNT  PICTURE  S9(5)                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CEFAC  PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CERD   PICTURE  9(3).                    CI0069
            10            CM01-CEDST  PICTURE  9(2).                    CI0069
            10            CM01-CEIPN  PICTURE  9(1).                    CI0069
            10            CM01-CEMAL  PICTURE  X(1).                    CI0069
            10            CM01-CEMALC PICTURE  9(2).                    CI0069
            10            CM01-CEUNL  PICTURE  X(1).                    CI0069
            10            CM01-CEMLN  PICTURE  X(1).                    CI0069
            10            CM01-CEYEX  PICTURE  9(2).                    CI0069
            10            CM01-CELLO  PICTURE  9(1).                    CI0069
            10            CM01-CESLD  PICTURE  9(8).                    CI0069
            10            CM01-CELAT.                                   CI0069
            11            CM01-CETMJ  PICTURE  9(3).                    CI0069
            11            CM01-CETMN  PICTURE  9(3).                    CI0069
            10            CM01-CELATD PICTURE  9(8).                    CI0069
            10            CM01-CEYER  PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CEPYR  PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CELIN  PICTURE  X(1).                    CI0069
            10            CM01-CEANP  PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CELBD  PICTURE  9(8).                    CI0069
            10            CM01-CELMC  PICTURE  9(3).                    CI0069
            10            CM01-CEXNO  PICTURE  S9(9)                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CEORS  PICTURE  9(2).                    CI0069
            10            CM01-CESST  PICTURE  9(2).                    CI0069
            10            CM01-CESAG  PICTURE  9(2).                    CI0069
            10            CM01-CEOSD  PICTURE  9(3).                    CI0069
            10            CM01-CEORD  PICTURE  9(3).                    CI0069
            10            CM01-CECLS  PICTURE  9(2).                    CI0069
            10            CM01-CEIND  PICTURE  9(3).                    CI0069
            10            CM01-CEOCC  PICTURE  9(3).                    CI0069
            10            CM01-CEASI  PICTURE  X(1).                    CI0069
            10            CM01-CESUB  PICTURE  9(3)                     CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CERDC  PICTURE  9(1).                    CI0069
            10            CM01-CECEA  PICTURE  S9(13)                   CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CETEA  PICTURE  S9(13)                   CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-FILLER PICTURE  X(1).                    CI0069
            10            CM01-COPDC  PICTURE  9.                       CI0069
            10            CM01-FILLER PICTURE  9(1).                    CI0069
            10            CM01-CEOWN  PICTURE  9(2).                    CI0069
            10            CM01-CESSC  PICTURE  9(2).                    CI0069
            10            CM01-CESSI  PICTURE  X(1).                    CI0069
            10            CM01-CECID  PICTURE  9(8).                    CI0069
            10            CM01-CECO   PICTURE  9(2).                    CI0069
            10            CM01-DSLRT  PICTURE  9(8).                    CI0069
            10            CM01-CEYPR  PICTURE  9(2).                    CI0069
            10            CM01-CEIPCB PICTURE  S9(9)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CETTR  PICTURE  X(1).                    CI0069
            10            CM01-CESCM  PICTURE  9(2).                    CI0069
            10            CM01-CEBSA  PICTURE  X(1).                    CI0069
            10            CM01-CESX   PICTURE  9(1).                    CI0069
            10            CM01-CERZC  PICTURE  S9(9)                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CERBD  PICTURE  9(8).                    CI0069
            10            CM01-CECUS  PICTURE  9(2).                    CI0069
            10            CM01-CEDUW  PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CEDCB  PICTURE  S9(7)V99                 CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-CTIDA1 PICTURE  9(3).                    CI0069
            10            CM01-IBPR1  PICTURE  9.                       CI0069
            10            CM01-IBPR2  PICTURE  9.                       CI0069
            10            CM01-IBPR3  PICTURE  9.                       CI0069
            10            CM01-IBPR4  PICTURE  9.                       CI0069
            10            CM01-IBPR0  PICTURE  9.                       CI0069
            10            CM01-CEXP   PICTURE  9(2).                    CI0069
            10            CM01-FILLER PICTURE  S9(7)V99                 CI0069
                          OCCURS       004     TIMES                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            CM01-GRPLT  PICTURE  99.                      CI0069
            10            CM01-FILLER PICTURE  X(16).                   CI0069
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
      ******************************************************            AADA82
      ****      WORK AREAS FOR COMMON DATE UTILITY       ***            AADA82
      ******************************************************            AADA82
      **                                                                AADA82
       01  MWS100EX        PIC X(8) VALUE 'MWS100EX'.                   AADA82
      **                                                                AADA82
      **   SEGMENT DD30 - FUNCTION LAYOUT                               AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=30 FOR=I DES=2 LEV=1                       AADA82
       01                 DD30.                                         CI0069
            10            DD30-CDTFN  PICTURE  9(4)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD30-CDTSF  PICTURE  9(4)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD30-CDTSC  PICTURE  9(4)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD30-FILLER PICTURE  X(40)                    CI0069
                          VALUE                SPACE.                   CI0069
       01                 DD34.                                         CI0069
            10            DD34-CAINS  PICTURE  X(03)                    CI0069
                          VALUE                SPACE.                   CI0069
            10            DD34-CDTUC  PICTURE  9                        CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-NDTUN  PICTURE  S9(05)                   CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-FILLER PICTURE  X(162)                   CI0069
                          VALUE                SPACE.                   CI0069
            10            DD34-DTGRG.                                   CI0069
            11            DD34-DTGCY.                                   CI0069
            12            DD34-DTGCC  PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            12            DD34-DTGYY  PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTGMM  PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTGDD  PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-DTJUL.                                   CI0069
            11            DD34-DTJCY.                                   CI0069
            12            DD34-DTJCC  PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            12            DD34-DTJYY  PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTJDD  PICTURE  9(3)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFM  PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLM  PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFF  PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLF  PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFW  PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLW  PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CCDOWA PICTURE  9                        CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CCDRW  PICTURE  9                        CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-FILLER PICTURE  X(58)                    CI0069
                          VALUE                SPACE.                   CI0069
            10            DD34-DTGRGA.                                  CI0069
            11            DD34-DTGCYA.                                  CI0069
            12            DD34-DTGCCA PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            12            DD34-DTGYYA PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTGMMA PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTGDDA PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-DTJULA.                                  CI0069
            11            DD34-DTJCYA.                                  CI0069
            12            DD34-DTJCCA PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            12            DD34-DTJYYA PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTJDDA PICTURE  9(3)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFMA PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLMA PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFFA PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLFA PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFWA PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLWA PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CCDOWB PICTURE  9                        CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CCDRWA PICTURE  9                        CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-FILLER PICTURE  X(58)                    CI0069
                          VALUE                SPACE.                   CI0069
            10            DD34-DTGRGB.                                  CI0069
            11            DD34-DTGCYB.                                  CI0069
            12            DD34-DTGCCB PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            12            DD34-DTGYYB PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTGMMB PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTGDDB PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-DTJULB.                                  CI0069
            11            DD34-DTJCYB.                                  CI0069
            12            DD34-DTJCCB PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            12            DD34-DTJYYB PICTURE  9(2)                     CI0069
                          VALUE                ZERO.                    CI0069
            11            DD34-DTJDDB PICTURE  9(3)                     CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFMB PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLMB PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFFB PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLFB PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTFWB PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CDTLWB PICTURE  9(01)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CCDOWC PICTURE  9                        CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-CCDRWB PICTURE  9                        CI0069
                          VALUE                ZERO.                    CI0069
            10            DD34-FILLER PICTURE  X(58)                    CI0069
                          VALUE                SPACE.                   CI0069
            10            DD34-FILLER PICTURE  X(40)                    CI0069
                          VALUE                SPACE.                   CI0069
      **                                                                AADA82
      **   SEGMENT DD34 - CONVERT DATE LAYOUT                           AADA82
      **                                                                AADA82
      *!WF DSP=DD DSL=DD SEL=34 FOR=I DES=2 LEV=1                       AADA82
      **                                                                AADA82
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0069
            10            XW05-XW06.                                    CI0069
            11            XW05-XDBPCB.                                  CI0069
            12            XW05-XDBDNM PICTURE  X(08)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-XSEGLV PICTURE  X(02)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-XRC    PICTURE  X(02)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-XPROPT PICTURE  X(04)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-FILLER PICTURE  S9(5)                    CI0069
                          VALUE                ZERO                     CI0069
                          BINARY.                                       CI0069
            12            XW05-XSEGNM PICTURE  X(08)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0069
                          VALUE                ZERO                     CI0069
                          BINARY.                                       CI0069
            12            XW05-XSEGNB PICTURE  9(05)                    CI0069
                          VALUE                ZERO                     CI0069
                          BINARY.                                       CI0069
            12            XW05-XCOKEY PICTURE  X(70)                    CI0069
                          VALUE                SPACE.                   CI0069
            10            XW05-XW07.                                    CI0069
            11            XW05-XIOPCB.                                  CI0069
            12            XW05-XTERMI PICTURE  X(08)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-FILLER PICTURE  XX                       CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-XRC1   PICTURE  X(02)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-FILLER PICTURE  X(12)                    CI0069
                          VALUE                SPACE.                   CI0069
            12            XW05-XMODNM PICTURE  X(8)                     CI0069
                          VALUE                SPACE.                   CI0069
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0069
                          VALUE                ZERO.                    CI0069
            10            XW05-XGU    PICTURE  X(4)                     CI0069
                          VALUE                'GU  '.                  CI0069
            10            XW05-XGHU   PICTURE  X(4)                     CI0069
                          VALUE                'GHU '.                  CI0069
            10            XW05-XGN    PICTURE  X(4)                     CI0069
                          VALUE                'GN  '.                  CI0069
            10            XW05-XGHN   PICTURE  X(4)                     CI0069
                          VALUE                'GHN '.                  CI0069
            10            XW05-XGNP   PICTURE  X(4)                     CI0069
                          VALUE                'GNP '.                  CI0069
            10            XW05-XGHNP  PICTURE  X(4)                     CI0069
                          VALUE                'GHNP'.                  CI0069
            10            XW05-XREPL  PICTURE  XXXX                     CI0069
                          VALUE                'REPL'.                  CI0069
            10            XW05-XISRT  PICTURE  X(4)                     CI0069
                          VALUE                'ISRT'.                  CI0069
            10            XW05-XDLET  PICTURE  X(4)                     CI0069
                          VALUE                'DLET'.                  CI0069
            10            XW05-XOPEN  PICTURE  X(4)                     CI0069
                          VALUE                'OPEN'.                  CI0069
            10            XW05-XCLSE  PICTURE  X(4)                     CI0069
                          VALUE                'CLSE'.                  CI0069
            10            XW05-XCHKP  PICTURE  X(4)                     CI0069
                          VALUE                'CHKP'.                  CI0069
            10            XW05-XXRST  PICTURE  X(4)                     CI0069
                          VALUE                'XRST'.                  CI0069
            10            XW05-XTERM  PICTURE  X(4)                     CI0069
                          VALUE                'TERM'.                  CI0069
            10            XW05-XNFPAC PICTURE  X(13)                    CI0069
                          VALUE                SPACE.                   CI0069
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0069
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0069
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
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************
      *
       01  W-WORK-MISC.
           05  W-WORK-CTIDND.
      *!WI
               10  W-WORK-NPREF
                        PICTURE 9(4).                                   CI0069
      *!WI
               10  W-WORK-NBASE
                        PICTURE 9(7).                                   CI0069
           05  WS01-DAYS           PIC S9(03) VALUE -018.
           05  WS01-DATE           PIC 9(08).
           05  WS01-CESLD.
               10  WS01-CESLD-MM   PIC 9(02).
               10  WS01-CESLD-DD   PIC 9(02).
               10  WS01-CESLD-CCYY PIC 9(04).
       01   DEBUT-WSS.                                                  CI0069
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0069
            05   IK     PICTURE X.                                      CI0069
       01  CONSTANTES-PAC.                                              CI0069
           05  FILLER  PICTURE X(87)   VALUE                            CI0069
                     '6015 CAT09/08/14CI0069ADMIN   14:34:34CI0069P AMERCI0069
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0069
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0069
           05  NUGNA   PICTURE X(5).                                    CI0069
           05  APPLI   PICTURE X(3).                                    CI0069
           05  DATGN   PICTURE X(8).                                    CI0069
           05  PROGR   PICTURE X(6).                                    CI0069
           05  CODUTI  PICTURE X(8).                                    CI0069
           05  TIMGN   PICTURE X(8).                                    CI0069
           05  PROGE   PICTURE X(8).                                    CI0069
           05  COBASE  PICTURE X(4).                                    CI0069
           05  DATGNC  PICTURE X(10).                                   CI0069
           05  RELEAS  PICTURE X(7).                                    CI0069
           05  DATGE   PICTURE X(10).                                   CI0069
           05  DATSQ   PICTURE X(10).                                   CI0069
       01  DATCE.                                                       CI0069
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0069
         05  DATOR.                                                     CI0069
           10  DATOA  PICTURE XX.                                       CI0069
           10  DATOM  PICTURE XX.                                       CI0069
           10  DATOJ  PICTURE XX.                                       CI0069
       01   VARIABLES-CONDITIONNELLES.                                  CI0069
            05                  FT      PICTURE X VALUE '0'.            CI0069
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0069
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0069
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0069
            05       5-CM00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0069
       01               S-CM01-SSA.                                     CI0069
            10         S1-CM01-SEGNAM PICTURE X(8)                      CI0069
                                      VALUE 'CACPCNT'.                  CI0069
            10         S1-CM01-CCOM   PICTURE X VALUE '*'.              CI0069
            10          S-CM01-CCOD   PICTURE X(5)                      CI0069
                                      VALUE '-----'.                    CI0069
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0069
       01            S-CMA01-SSA.                                       CI0069
            10      S1-CMA01-SEGNAM PICTURE X(8)                        CI0069
                                      VALUE 'CACPCNT'.                  CI0069
            10      S1-CMA01-CCOM   PICTURE X VALUE '*'.                CI0069
            10       S-CMA01-CCOD   PICTURE X(5)                        CI0069
                                      VALUE '-----'.                    CI0069
            10      S1-CMA01-FLDNAM PICTURE X(9)                        CI0069
                                      VALUE '(CACPPRDK'.                CI0069
            10       S-CMA01-OPER  PICTURE XX VALUE ' ='.               CI0069
            10       S-CMA01-CEPDT    PICTURE  9(4).                    CI0069
            10  FILLER   PICTURE X    VALUE ')'.                        CI0069
       01            S-CMU01-SSA.                                       CI0069
            10      S1-CMU01-SEGNAM PICTURE X(8)                        CI0069
                                      VALUE 'CACPCNT'.                  CI0069
            10      S1-CMU01-CCOM   PICTURE X VALUE '*'.                CI0069
            10       S-CMU01-CCOD   PICTURE X(5)                        CI0069
                                      VALUE '-----'.                    CI0069
            10      S1-CMU01-FLDNAM PICTURE X(9)                        CI0069
                                      VALUE '(CACPCNTK'.                CI0069
            10       S-CMU01-OPER  PICTURE XX VALUE ' ='.               CI0069
            10       S-CMU01-CEKCNT.                                    CI0069
            11       S-CMU01-CEGCN.                                     CI0069
            12       S-CMU01-CEPRE    PICTURE  9(4).                    CI0069
            12       S-CMU01-CEBAS    PICTURE  9(8).                    CI0069
            10  FILLER   PICTURE X    VALUE ')'.                        CI0069
       01   ZONES-UTILISATEUR PICTURE X.                                CI0069
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
      ** PCB POINTER FOR CA1P                                           ADU015
            05 PCB-CA1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CA1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0069
          05              PA00-SUITE.                                   CI0069
            15       FILLER         PICTURE  X(00106).                  CI0069
       01                 PA06  REDEFINES      PA00.                    CI0069
            10            PA06-XDBPCB.                                  CI0069
            11            PA06-XDBDNM PICTURE  X(08).                   CI0069
            11            PA06-XSEGLV PICTURE  X(02).                   CI0069
            11            PA06-XRC    PICTURE  X(02).                   CI0069
            11            PA06-XPROPT PICTURE  X(04).                   CI0069
            11            PA06-FILLER PICTURE  S9(5)                    CI0069
                          BINARY.                                       CI0069
            11            PA06-XSEGNM PICTURE  X(08).                   CI0069
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0069
                          BINARY.                                       CI0069
            11            PA06-XSEGNB PICTURE  9(05)                    CI0069
                          BINARY.                                       CI0069
            11            PA06-XCOKEY PICTURE  X(70).                   CI0069
      *
      ******************************************************************
      ** THIS SEGMENT CONTAINS THE INPUT/OUTPUT PARAMETERS FOR CI0069  *
      ******************************************************************
      *
      *!WF DSP=DS DSL=DU SEL=69 FOR=I LEV=1 PLT=05
       01                 DS00.                                         CI0069
          05              DS00-SUITE.                                   CI0069
            15       FILLER         PICTURE  X(00255).                  CI0069
       01                 DS69  REDEFINES      DS00.                    CI0069
            10            DS69-C299.                                    CI0069
            11            DS69-CTID.                                    CI0069
            12            DS69-CTIDA  PICTURE  9(3).                    CI0069
            12            DS69-CTIDN.                                   CI0069
            13            DS69-CTIDNP PICTURE  X(13).                   CI0069
            13            DS69-CTIDND PICTURE  9(11).                   CI0069
            10            DS69-DCACG  PICTURE  9(8).                    CI0069
            10            DS69-CTSTA  PICTURE  99.                      CI0069
            10            DS69-FILLER PICTURE  X(100).                  CI0069
            10            DS69-DAUTB  PICTURE  9(8).                    CI0069
            10            DS69-CESLD  PICTURE  9(8).                    CI0069
            10            DS69-DOPDA  PICTURE  99.                      CI0069
            10            DS69-FILLER PICTURE  X(100).                  CI0069
      *
      *
      *
      *
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0069
          05              DE00-SUITE.                                   CI0069
            15       FILLER         PICTURE  X(00653).                  CI0069
       01                 DE10  REDEFINES      DE00.                    CI0069
            10            DE10-DU11.                                    CI0069
            11            DE10-XFONC  PICTURE  X(4).                    CI0069
            11            DE10-MPSBN  PICTURE  X(8).                    CI0069
            11            DE10-XDBDNM PICTURE  X(08).                   CI0069
            11            DE10-XSEGNM PICTURE  X(08).                   CI0069
            11            DE10-XRC    PICTURE  X(02).                   CI0069
            11            DE10-MSEG   PICTURE  X(08).                   CI0069
            11            DE10-XCOKEY PICTURE  X(70).                   CI0069
            11            DE10-CUIBR  PICTURE  X(01).                   CI0069
            11            DE10-CUIBA  PICTURE  X(01).                   CI0069
            11            DE10-IPBIK  PICTURE  X(1).                    CI0069
            10            DE10-DU03.                                    CI0069
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            11            DE10-CMSSF  PICTURE  XX.                      CI0069
            11            DE10-DU09.                                    CI0069
            12            DE10-CMESA  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            12            DE10-CMESB  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            12            DE10-CMSST  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            12            DE10-QELLAA PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            12            DE10-TMESS4 PICTURE  X(512).                  CI0069
      *                                                                 AMDU10
      *                                                                 AMDU10
      *                                                                 AMDU10
      *                                                                 AMDU10
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0069
          05              MS00-SUITE.                                   CI0069
            15       FILLER         PICTURE  X(00542).                  CI0069
       01                 MS03  REDEFINES      MS00.                    CI0069
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            10            MS03-CMSSF  PICTURE  XX.                      CI0069
            10            MS03-DU09.                                    CI0069
            11            MS03-CMESA  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            11            MS03-CMESB  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            11            MS03-CMSST  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            11            MS03-QELLAA PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
            11            MS03-TMESS4 PICTURE  X(512).                  CI0069
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0069
            10            MX11-QMSGS  PICTURE  9(03).                   CI0069
            10            MX11-PJ09                                     CI0069
                          OCCURS       025     TIMES.                   CI0069
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0069
                          COMPUTATIONAL-3.                              CI0069
            11            MX11-CMESB  PICTURE  S9(9)                    CI0069
                          BINARY.                                       CI0069
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                DS69
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0069
      *               *                                   *             CI0069
      *               *INITIALISATIONS                    *             CI0069
      *               *                                   *             CI0069
      *               *************************************.            CI0069
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
       F02XA.                                                           lv10
      *SET ADDRESS FOR CA1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CA1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0069
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0069
      *               *                                   *             CI0069
      *               *FIN DE TRAITEMENT                  *             CI0069
      *               *                                   *             CI0069
      *               *************************************.            CI0069
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0069
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               ***** INITIALIZE OUTPUT AREA ****   *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35BA.    NOTE ***** INITIALIZE OUTPUT AREA ****   *.
       F35BA.                                                           lv10
           MOVE        ZEROS TO DS69-DAUTB
           MOVE        ZEROS TO DS69-CESLD
           MOVE        ZEROS TO DS69-DOPDA.
       F35BA-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               ***** DETERMINE PAYMENT DATES ***   *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BA.    NOTE ****** IF ACCOUNT IS A FUND *****   *.
       F40BA.    IF    DS69-CTIDA = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40BA-FN.
           MOVE        DS69-DCACG TO DD34-DTGRGA
           MOVE        0 TO DD34-CDTUC
           MOVE        WS01-DAYS TO DD34-NDTUN
           MOVE        9 TO DD30-CDTSF
           PERFORM     F94BG THRU F94BG-FN.
      *N40BB.    NOTE ****** CHECK RETURNED DATE  *****   *.
       F40BB.    IF    DD30-CDTSC = 0                                   lv15
                 NEXT SENTENCE ELSE GO TO     F40BB-FN.
           MOVE        DD34-DTGRGB TO DS69-DAUTB
           MOVE        ZEROS TO DS69-DOPDA
           MOVE        ZEROS TO DS69-CESLD.
       F40BB-900. GO TO F40BC-FN.
       F40BB-FN. EXIT.
      *N40BC.    NOTE ****** CHECK RETURNED DATE  *****   *.
       F40BC.                                                           lv15
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013150 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F40BC-FN. EXIT.
       F40BA-FN. EXIT.
      *N40DA.    NOTE ****** IF ACCOUNT IS A CERT *****   *.
       F40DA.    IF    DS69-CTIDA = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
           MOVE        DS69-DCACG TO DS69-DAUTB.
      *N40GA.    NOTE ******* SET UP SSA FOR CM01 *****   *.
       F40GA.                                                           lv15
      *********************************
      ** SET UP SSA FOR CM01.  THE    *
      ** ACCOUNT NUMBER IS REFORMATTED*
      *********************************
      *
           MOVE        DS69-CTIDND TO W-WORK-CTIDND
           MOVE        W-WORK-NPREF TO S-CMU01-CEPRE
           MOVE        W-WORK-NBASE TO S-CMU01-CEBAS.
       F40GA-FN. EXIT.
      *N40HA.    NOTE ******* READ CM01 SEGMENT *******   *.
       F40HA.                                                           lv15
      *
           PERFORM     F94C1 THRU F94C1-FN.
      *N40IA.    NOTE ****** IF CM01 SEGMENT FOUND ****   *.
       F40IA.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40IA-FN.
      *********************************
      ** IF THE CM01 SEGMENT WAS N    *
      ** FOUND, RETURN CMO1 SEGMENT   *
      ** SALE DATE AND THE PAYMENT    *
      ** DAY - DATE TO THE CALLING    *
      ** PROGRAM.                     *
      *********************************
      **
           MOVE        CM01-CESLD TO DS69-CESLD
           MOVE        CM01-CESLD TO WS01-CESLD
           MOVE        WS01-CESLD-DD TO DS69-DOPDA.
       F40IA-900. GO TO F40JA-FN.
       F40IA-FN. EXIT.
      *N40JA.    NOTE *** ELSE CM01 SEGMENT NOT FOUND *   *.
       F40JA.                                                           lv20
      *********************************
      ** RETURN CM01 SEGMENT NOT      *
      ** FOUND ERROR.                 *
      *********************************
      *
                 IF    DS69-CTSTA NOT = '01'                            DOT
      ** IF FROM ACCOUNT STATUS NOT = *
      ** PENDING - SEND ERROR MESSAGE *
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012802 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
      **   END 99 LEVEL IF STATEMENT  *                                 DOT
       F40JA-FN. EXIT.
       F40HA-FN. EXIT.
       F40DA-FN. EXIT.
       F40-FN.   EXIT.
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
      *N94BG.    NOTE *CDU - DATE DIFF/CALCULATION        *.            AADA82
       F94BG.                                                           lv10
      ** * * * * * * * * * * * * * * *                                  AADA82
      *This code calls the common date                                  AADA82
      *utility MWS100EX to calculate                                    AADA82
      *the difference between 2 dates                                   AADA82
      *or calculate a new date (add/                                    AADA82
      *subtract days). It uses a                                        AADA82
      *dynamic call.                                                    AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Before the call set the subfunc                                  AADA82
      *request code DD30-CDTSF:                                         AADA82
      *  8 = date difference                                            AADA82
      *  9 = date add/subtract days                                     AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
      *Check return code DD30-CDTSC                                     AADA82
      *after the call.                                                  AADA82
      *    0 = Error Free                                               AADA82
      *    3 = Invalid Date                                             AADA82
      *    5 = Invalid Day                                              AADA82
      *    6 = Invalid Month                                            AADA82
      ** * * * * * * * * * * * * * * *                                  AADA82
           MOVE        4 TO DD30-CDTFN                                  AADA82
           CALL        MWS100EX USING DD30                              AADA82
           DD34.                                                        AADA82
       F94BG-FN. EXIT.
      *N94C1.    NOTE *CALL GU ON CM01                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'CA1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CM01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CM01                                                    ADU026
           S-CMU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
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
