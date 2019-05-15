       IDENTIFICATION DIVISION.                                         CI0062
       PROGRAM-ID.  CI0062P.                                            CI0062
      *AUTHOR.         GET ONE AEFA ACCOUNT FOR BANK.                   CI0062
      *DATE-COMPILED.   09/08/14.                                       CI0062
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CI0062
       CONFIGURATION SECTION.                                           CI0062
       SOURCE-COMPUTER. IBM-370.                                        CI0062
       OBJECT-COMPUTER. IBM-370.                                        CI0062
       DATA DIVISION.                                                   CI0062
       WORKING-STORAGE SECTION.                                         CI0062
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
       01                 CX01.                                         CI0062
            10            CX01-CX01K.                                   CI0062
            11            CX01-C199.                                    CI0062
            12            CX01-CLID.                                    CI0062
            13            CX01-CLIDO  PICTURE  9(3).                    CI0062
            13            CX01-CLIDN.                                   CI0062
            14            CX01-CLIDNP PICTURE  X(12).                   CI0062
            14            CX01-CLIDND PICTURE  9(8).                    CI0062
            10            CX01-GEMDA  PICTURE  9(8).                    CI0062
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0062
                          BINARY.                                       CI0062
            10            CX01-FILLER PICTURE  X(5).                    CI0062
       01                 CX03.                                         CI0062
            10            CX03-GELL   PICTURE  9(4)                     CI0062
                          BINARY.                                       CI0062
            10            CX03-CY00.                                    CI0062
            11            CX03-CX03K.                                   CI0062
            12            CX03-CARTY  PICTURE  99.                      CI0062
            12            CX03-NARRS  PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX03-CARST  PICTURE  99.                      CI0062
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX03-CPMTG  PICTURE  99.                      CI0062
            11            CX03-GRCRNG PICTURE  9(3).                    CI0062
            11            CX03-DEXDT  PICTURE  9(8).                    CI0062
            11            CX03-DASUP  PICTURE  9(8).                    CI0062
            11            CX03-CSTEC  PICTURE  X(3).                    CI0062
            11            CX03-FILLER PICTURE  X(17).                   CI0062
            11            CX03-CY50.                                    CI0062
            12            CX03-NARID  PICTURE  X(30).                   CI0062
            11            CX03-CY51                                     CI0062
                          REDEFINES            CX03-CY50.               CI0062
            12            CX03-NDIDN  PICTURE  9(12).                   CI0062
            12            CX03-FILLER PICTURE  X(18).                   CI0062
            11            CX03-CY52                                     CI0062
                          REDEFINES            CX03-CY50.               CI0062
            12            CX03-NAIDC  PICTURE  9(12).                   CI0062
            12            CX03-FILLER PICTURE  X(18).                   CI0062
            11            CX03-CY53                                     CI0062
                          REDEFINES            CX03-CY50.               CI0062
            12            CX03-NAMEXB PICTURE  9(15).                   CI0062
            12            CX03-FILLER PICTURE  X(15).                   CI0062
            10            CX03-CY99.                                    CI0062
            11            CX03-FILLER PICTURE  X(109).                  CI0062
            10            CX03-CY01                                     CI0062
                          REDEFINES            CX03-CY99.               CI0062
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX03-ICPCI  PICTURE  X.                       CI0062
            11            CX03-CLUPD  PICTURE  9(3).                    CI0062
            11            CX03-DLAUP  PICTURE  9(8).                    CI0062
            11            CX03-CWRC   PICTURE  99.                      CI0062
            11            CX03-CHCR   PICTURE  99.                      CI0062
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0062
            11            CX03-GEAUN  PICTURE  9(5).                    CI0062
            11            CX03-DPCHD  PICTURE  9(8).                    CI0062
            11            CX03-DLRCHK PICTURE  9(8).                    CI0062
            11            CX03-QTRCHK PICTURE  9(2).                    CI0062
            11            CX03-DNPMT  PICTURE  9(8).                    CI0062
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0062
                          COMPUTATIONAL-3.                              CI0062
            10            CX03-CY02                                     CI0062
                          REDEFINES            CX03-CY99.               CI0062
            11            CX03-QSIRQ  PICTURE  99.                      CI0062
            11            CX03-QDRMN  PICTURE  9(2)                     CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX03-DDPRE  PICTURE  9(8).                    CI0062
            11            CX03-DDSHP  PICTURE  9(8).                    CI0062
            11            CX03-NDRFTB PICTURE  9(5).                    CI0062
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0062
            11            CX03-DDSHPA PICTURE  9(8).                    CI0062
            11            CX03-NDRFTF PICTURE  9(5).                    CI0062
            11            CX03-QDIPBK PICTURE  9(3).                    CI0062
            11            CX03-CREOR  PICTURE  X(1).                    CI0062
            11            CX03-CREOR1 PICTURE  X(1).                    CI0062
            11            CX03-DDASC  PICTURE  9(8).                    CI0062
            11            CX03-FILLER PICTURE  X(7).                    CI0062
            10            CX03-CY03                                     CI0062
                          REDEFINES            CX03-CY99.               CI0062
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0062
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0062
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0062
            11            CX03-DOPDA  PICTURE  99.                      CI0062
            11            CX03-CPMTF  PICTURE  99.                      CI0062
            11            CX03-CIRMO  PICTURE  X(12).                   CI0062
            11            CX03-CPALL  PICTURE  X(1).                    CI0062
            11            CX03-CCOLM  PICTURE  9(2).                    CI0062
            11            CX03-CBLTP  PICTURE  X(1).                    CI0062
            11            CX03-CASUB  PICTURE  9(2).                    CI0062
            11            CX03-CBLFM  PICTURE  9(2).                    CI0062
            11            CX03-IBILS  PICTURE  X.                       CI0062
            11            CX03-IPAOS  PICTURE  X.                       CI0062
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0062
            11            CX03-DLBPD  PICTURE  9(8).                    CI0062
            11            CX03-DNBPD  PICTURE  9(8).                    CI0062
            11            CX03-DODBD  PICTURE  9(8).                    CI0062
            11            CX03-CPSRE  PICTURE  99.                      CI0062
            11            CX03-ISPHN  PICTURE  X.                       CI0062
            11            CX03-TCARR  PICTURE  X(6).                    CI0062
            11            CX03-CBKPT  PICTURE  9(2).                    CI0062
            11            CX03-IECNT  PICTURE  X.                       CI0062
            11            CX03-ICONV  PICTURE  X(1).                    CI0062
            11            CX03-FILLER PICTURE  X(4).                    CI0062
            10            CX03-CY04                                     CI0062
                          REDEFINES            CX03-CY99.               CI0062
            11            CX03-CCARD  PICTURE  X(02).                   CI0062
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0062
            11            CX03-IREMT  PICTURE  X(01).                   CI0062
            11            CX03-ISBILA PICTURE  X.                       CI0062
            11            CX03-DLBPDA PICTURE  9(8).                    CI0062
            11            CX03-DNBPDA.                                  CI0062
            12            CX03-DNCYM  PICTURE  9(6).                    CI0062
            12            CX03-CEDTD  PICTURE  9(2).                    CI0062
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX03-DREMT  PICTURE  9(8).                    CI0062
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0062
            11            CX03-CWRC2  PICTURE  99.                      CI0062
            11            CX03-CHCR2  PICTURE  99.                      CI0062
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0062
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0062
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0062
       01                 CX06.                                         CI0062
            10            CX06-CX06K.                                   CI0062
            11            CX06-C299.                                    CI0062
            12            CX06-CTID.                                    CI0062
            13            CX06-CTIDA  PICTURE  9(3).                    CI0062
            13            CX06-CTIDN.                                   CI0062
            14            CX06-CTIDNP PICTURE  X(13).                   CI0062
            14            CX06-CTIDND PICTURE  9(11).                   CI0062
            10            CX06-NPECK  PICTURE  9(02).                   CI0062
            10            CX06-FILLER PICTURE  X.                       CI0062
       01                 CX12.                                         CI0062
            10            CX12-CX12K.                                   CI0062
            11            CX12-CPMTC  PICTURE  99.                      CI0062
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            CX12-GESTD  PICTURE  9(8).                    CI0062
            10            CX12-GEEND  PICTURE  9(8).                    CI0062
            10            CX12-CIRMO  PICTURE  X(12).                   CI0062
            10            CX12-CDEST  PICTURE  99.                      CI0062
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0062
                          COMPUTATIONAL-3.                              CI0062
            10            CX12-DNPMT  PICTURE  9(8).                    CI0062
            10            CX12-NIRACM PICTURE  9(2).                    CI0062
            10            CX12-CPMTF  PICTURE  99.                      CI0062
            10            CX12-IPODM  PICTURE  X.                       CI0062
            10            CX12-CLUPD  PICTURE  9(3).                    CI0062
            10            CX12-DLAUP  PICTURE  9(8).                    CI0062
            10            CX12-CWRC   PICTURE  99.                      CI0062
            10            CX12-CHCR   PICTURE  99.                      CI0062
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0062
            10            CX12-GEAUN  PICTURE  9(5).                    CI0062
            10            CX12-DPCHD  PICTURE  9(8).                    CI0062
            10            CX12-DNEXE  PICTURE  9(8).                    CI0062
            10            CX12-CCSMQ  PICTURE  X.                       CI0062
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0062
            10            CX12-CORTY  PICTURE  X.                       CI0062
            10            CX12-CNAVR  PICTURE  X(1).                    CI0062
            10            CX12-DELOI3 PICTURE  9(6).                    CI0062
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0062
                          COMPUTATIONAL-3.                              CI0062
            10            CX12-FILLER PICTURE  X(5).                    CI0062
      ******************************************************************ADU029
      ***  STORAGE AREAS FOR DL1                                        ADU029
      ******************************************************************ADU029
      ***  DL1 FUNCTIONS                                                ADU029
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU029
       01                 XW05.                                         CI0062
            10            XW05-XW06.                                    CI0062
            11            XW05-XDBPCB.                                  CI0062
            12            XW05-XDBDNM PICTURE  X(08)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-XSEGLV PICTURE  X(02)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-XRC    PICTURE  X(02)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-XPROPT PICTURE  X(04)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-FILLER PICTURE  S9(5)                    CI0062
                          VALUE                ZERO                     CI0062
                          BINARY.                                       CI0062
            12            XW05-XSEGNM PICTURE  X(08)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0062
                          VALUE                ZERO                     CI0062
                          BINARY.                                       CI0062
            12            XW05-XSEGNB PICTURE  9(05)                    CI0062
                          VALUE                ZERO                     CI0062
                          BINARY.                                       CI0062
            12            XW05-XCOKEY PICTURE  X(70)                    CI0062
                          VALUE                SPACE.                   CI0062
            10            XW05-XW07.                                    CI0062
            11            XW05-XIOPCB.                                  CI0062
            12            XW05-XTERMI PICTURE  X(08)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-FILLER PICTURE  XX                       CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-XRC1   PICTURE  X(02)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-FILLER PICTURE  X(12)                    CI0062
                          VALUE                SPACE.                   CI0062
            12            XW05-XMODNM PICTURE  X(8)                     CI0062
                          VALUE                SPACE.                   CI0062
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0062
                          VALUE                ZERO.                    CI0062
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0062
                          VALUE                ZERO.                    CI0062
            10            XW05-XGU    PICTURE  X(4)                     CI0062
                          VALUE                'GU  '.                  CI0062
            10            XW05-XGHU   PICTURE  X(4)                     CI0062
                          VALUE                'GHU '.                  CI0062
            10            XW05-XGN    PICTURE  X(4)                     CI0062
                          VALUE                'GN  '.                  CI0062
            10            XW05-XGHN   PICTURE  X(4)                     CI0062
                          VALUE                'GHN '.                  CI0062
            10            XW05-XGNP   PICTURE  X(4)                     CI0062
                          VALUE                'GNP '.                  CI0062
            10            XW05-XGHNP  PICTURE  X(4)                     CI0062
                          VALUE                'GHNP'.                  CI0062
            10            XW05-XREPL  PICTURE  XXXX                     CI0062
                          VALUE                'REPL'.                  CI0062
            10            XW05-XISRT  PICTURE  X(4)                     CI0062
                          VALUE                'ISRT'.                  CI0062
            10            XW05-XDLET  PICTURE  X(4)                     CI0062
                          VALUE                'DLET'.                  CI0062
            10            XW05-XOPEN  PICTURE  X(4)                     CI0062
                          VALUE                'OPEN'.                  CI0062
            10            XW05-XCLSE  PICTURE  X(4)                     CI0062
                          VALUE                'CLSE'.                  CI0062
            10            XW05-XCHKP  PICTURE  X(4)                     CI0062
                          VALUE                'CHKP'.                  CI0062
            10            XW05-XXRST  PICTURE  X(4)                     CI0062
                          VALUE                'XRST'.                  CI0062
            10            XW05-XTERM  PICTURE  X(4)                     CI0062
                          VALUE                'TERM'.                  CI0062
            10            XW05-XNFPAC PICTURE  X(13)                    CI0062
                          VALUE                SPACE.                   CI0062
      *!WI pl=DL200                                                     ADU029
       01  DL01-KFPCB                                                   ADU029
                        PICTURE X(04)                                   CI0062
              VALUE 'PCB '.                                             ADU029
      ***  SAVE AREA FOR DL1 FUNCTION VALUE - USED FOR ERROR PROCESSING ADU029
       01  SV01-FUNC     PIC X(4).                                      ADU029
      ******************************************************************ADU029
      *** USED WHEN DYNAMICALLY CALLING PCB & UIB ERROR CHECKING MODULESADU029
      ***    (CI0008P - UIB ERROR CHECK    CI0009P - PCB ERROR CHECK)   ADU029
      ******************************************************************ADU029
      *!WI pl=DN100                                                     ADU029
       01  W-PASS-XPROGR                                                ADU029
                        PICTURE X(8).                                   CI0062
       01   DEBUT-WSS.                                                  CI0062
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0062
            05   IK     PICTURE X.                                      CI0062
       01  CONSTANTES-PAC.                                              CI0062
           05  FILLER  PICTURE X(87)   VALUE                            CI0062
                     '6015 CAT09/08/14CI0062ADMIN   14:34:23CI0062P AMERCI0062
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0062
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0062
           05  NUGNA   PICTURE X(5).                                    CI0062
           05  APPLI   PICTURE X(3).                                    CI0062
           05  DATGN   PICTURE X(8).                                    CI0062
           05  PROGR   PICTURE X(6).                                    CI0062
           05  CODUTI  PICTURE X(8).                                    CI0062
           05  TIMGN   PICTURE X(8).                                    CI0062
           05  PROGE   PICTURE X(8).                                    CI0062
           05  COBASE  PICTURE X(4).                                    CI0062
           05  DATGNC  PICTURE X(10).                                   CI0062
           05  RELEAS  PICTURE X(7).                                    CI0062
           05  DATGE   PICTURE X(10).                                   CI0062
           05  DATSQ   PICTURE X(10).                                   CI0062
       01  DATCE.                                                       CI0062
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0062
         05  DATOR.                                                     CI0062
           10  DATOA  PICTURE XX.                                       CI0062
           10  DATOM  PICTURE XX.                                       CI0062
           10  DATOJ  PICTURE XX.                                       CI0062
       01   VARIABLES-CONDITIONNELLES.                                  CI0062
            05                  FT      PICTURE X VALUE '0'.            CI0062
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0062
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0062
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0062
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0062
       01               S-CX01-SSA.                                     CI0062
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0062
                                      VALUE 'CX01    '.                 CI0062
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0062
            10          S-CX01-CCOD   PICTURE X(5)                      CI0062
                                      VALUE '-----'.                    CI0062
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0062
       01            S-CXU01-SSA.                                       CI0062
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX01    '.                 CI0062
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0062
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CX01K'.                   CI0062
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0062
            10       S-CXU01-CX01K.                                     CI0062
            11       S-CXU01-C199.                                      CI0062
            12       S-CXU01-CLID.                                      CI0062
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0062
            13       S-CXU01-CLIDN.                                     CI0062
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0062
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0062
            10  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01               S-CX03-SSA.                                     CI0062
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0062
                                      VALUE 'CX03    '.                 CI0062
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0062
            10          S-CX03-CCOD   PICTURE X(5)                      CI0062
                                      VALUE '-----'.                    CI0062
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0062
       01            S-CXA03-SSA.                                       CI0062
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0062
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CARTY'.                   CI0062
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0062
            12       S-CXA03-CARTY    PICTURE  99.                      CI0062
            12  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXB03-SSA.                                       CI0062
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0062
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(NARRS'.                   CI0062
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0062
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            12  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXC03-SSA.                                       CI0062
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0062
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CPMTG'.                   CI0062
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0062
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0062
            11  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXD03-SSA.                                       CI0062
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0062
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(GRCRNG'.                  CI0062
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0062
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0062
            11  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXE03-SSA.                                       CI0062
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0062
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(DEXDT'.                   CI0062
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0062
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0062
            11  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXF03-SSA.                                       CI0062
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0062
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CY50'.                    CI0062
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0062
            11       S-CXF03-CY50.                                      CI0062
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0062
            11  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXG03-SSA.                                       CI0062
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0062
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(NBASQ'.                   CI0062
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0062
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXH03-SSA.                                       CI0062
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0062
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(NARID'.                   CI0062
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0062
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0062
            12  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXU03-SSA.                                       CI0062
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX03    '.                 CI0062
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0062
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CX03K'.                   CI0062
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0062
            11       S-CXU03-CX03K.                                     CI0062
            12       S-CXU03-CARTY    PICTURE  99.                      CI0062
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01               S-CX06-SSA.                                     CI0062
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0062
                                      VALUE 'CX06    '.                 CI0062
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0062
            10          S-CX06-CCOD   PICTURE X(5)                      CI0062
                                      VALUE '-----'.                    CI0062
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0062
       01            S-CXU06-SSA.                                       CI0062
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX06    '.                 CI0062
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0062
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CX06K'.                   CI0062
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0062
            10       S-CXU06-CX06K.                                     CI0062
            11       S-CXU06-C299.                                      CI0062
            12       S-CXU06-CTID.                                      CI0062
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0062
            13       S-CXU06-CTIDN.                                     CI0062
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0062
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0062
            10  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01               S-CX12-SSA.                                     CI0062
            10         S1-CX12-SEGNAM PICTURE X(8)                      CI0062
                                      VALUE 'CX12    '.                 CI0062
            10         S1-CX12-CCOM   PICTURE X VALUE '*'.              CI0062
            10          S-CX12-CCOD   PICTURE X(5)                      CI0062
                                      VALUE '-----'.                    CI0062
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0062
       01            S-CXA12-SSA.                                       CI0062
            10      S1-CXA12-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX12    '.                 CI0062
            10      S1-CXA12-CCOM   PICTURE X VALUE '*'.                CI0062
            10       S-CXA12-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            10      S1-CXA12-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CDEST'.                   CI0062
            10       S-CXA12-OPER  PICTURE XX VALUE ' ='.               CI0062
            10       S-CXA12-CDEST    PICTURE  99.                      CI0062
            10  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXB12-SSA.                                       CI0062
            10      S1-CXB12-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX12    '.                 CI0062
            10      S1-CXB12-CCOM   PICTURE X VALUE '*'.                CI0062
            10       S-CXB12-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            10      S1-CXB12-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(DNPMT'.                   CI0062
            10       S-CXB12-OPER  PICTURE XX VALUE ' ='.               CI0062
            10       S-CXB12-DNPMT    PICTURE  9(8).                    CI0062
            10  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXC12-SSA.                                       CI0062
            11      S1-CXC12-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX12    '.                 CI0062
            11      S1-CXC12-CCOM   PICTURE X VALUE '*'.                CI0062
            11       S-CXC12-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            11      S1-CXC12-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(NAPDS'.                   CI0062
            11       S-CXC12-OPER  PICTURE XX VALUE ' ='.               CI0062
            11       S-CXC12-NAPDS    PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01            S-CXU12-SSA.                                       CI0062
            10      S1-CXU12-SEGNAM PICTURE X(8)                        CI0062
                                      VALUE 'CX12    '.                 CI0062
            10      S1-CXU12-CCOM   PICTURE X VALUE '*'.                CI0062
            10       S-CXU12-CCOD   PICTURE X(5)                        CI0062
                                      VALUE '-----'.                    CI0062
            10      S1-CXU12-FLDNAM PICTURE X(9)                        CI0062
                                      VALUE '(CX12K'.                   CI0062
            10       S-CXU12-OPER  PICTURE XX VALUE ' ='.               CI0062
            10       S-CXU12-CX12K.                                     CI0062
            11       S-CXU12-CPMTC    PICTURE  99.                      CI0062
            11       S-CXU12-NAPDS    PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11       S-CXU12-GESTD    PICTURE  9(8).                    CI0062
            10  FILLER   PICTURE X    VALUE ')'.                        CI0062
       01   ZONES-UTILISATEUR PICTURE X.                                CI0062
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
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0062
          05              PA00-SUITE.                                   CI0062
            15       FILLER         PICTURE  X(00106).                  CI0062
       01                 PA06  REDEFINES      PA00.                    CI0062
            10            PA06-XDBPCB.                                  CI0062
            11            PA06-XDBDNM PICTURE  X(08).                   CI0062
            11            PA06-XSEGLV PICTURE  X(02).                   CI0062
            11            PA06-XRC    PICTURE  X(02).                   CI0062
            11            PA06-XPROPT PICTURE  X(04).                   CI0062
            11            PA06-FILLER PICTURE  S9(5)                    CI0062
                          BINARY.                                       CI0062
            11            PA06-XSEGNM PICTURE  X(08).                   CI0062
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0062
                          BINARY.                                       CI0062
            11            PA06-XSEGNB PICTURE  9(05)                    CI0062
                          BINARY.                                       CI0062
            11            PA06-XCOKEY PICTURE  X(70).                   CI0062
      *                                                                 AMDU62
      ******************************************************************AMDU62
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET A      *AMDU62
      **     SPECIFIC CX12 SEGMENT FOR THE KEY SUPPLIED.               *AMDU62
      ******************************************************************AMDU62
      *                                                                 AMDU62
      *!WF DSP=WZ DSL=DU SEL=62 FOR=I DES=1 LEV=1                       AMDU62
       01                 WZ62.                                         CI0062
            10            WZ62-CFUNC  PICTURE  X(3).                    CI0062
            10            WZ62-CLID   PICTURE  X(23).                   CI0062
            10            WZ62-CARTYK PICTURE  99.                      CI0062
            10            WZ62-NARRSK PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            10            WZ62-CTID01 PICTURE  X(27).                   CI0062
            10            WZ62-CPMTCX PICTURE  XX.                      CI0062
            10            WZ62-NAPDSK PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            10            WZ62-GESTD1 PICTURE  9(8).                    CI0062
            10            WZ62-CX12.                                    CI0062
            11            WZ62-CX12K.                                   CI0062
            12            WZ62-CPMTC  PICTURE  99.                      CI0062
            12            WZ62-NAPDS  PICTURE  S9(3)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            12            WZ62-GESTD  PICTURE  9(8).                    CI0062
            11            WZ62-GEEND  PICTURE  9(8).                    CI0062
            11            WZ62-CIRMO  PICTURE  X(12).                   CI0062
            11            WZ62-CDEST  PICTURE  99.                      CI0062
            11            WZ62-APMTL  PICTURE  S9(9)V99                 CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            WZ62-DNPMT  PICTURE  9(8).                    CI0062
            11            WZ62-NIRACM PICTURE  9(2).                    CI0062
            11            WZ62-CPMTF  PICTURE  99.                      CI0062
            11            WZ62-IPODM  PICTURE  X.                       CI0062
            11            WZ62-CLUPD  PICTURE  9(3).                    CI0062
            11            WZ62-DLAUP  PICTURE  9(8).                    CI0062
            11            WZ62-CWRC   PICTURE  99.                      CI0062
            11            WZ62-CHCR   PICTURE  99.                      CI0062
            11            WZ62-GEOPD2 PICTURE  X(8).                    CI0062
            11            WZ62-GEAUN  PICTURE  9(5).                    CI0062
            11            WZ62-DPCHD  PICTURE  9(8).                    CI0062
            11            WZ62-DNEXE  PICTURE  9(8).                    CI0062
            11            WZ62-CCSMQ  PICTURE  X.                       CI0062
            11            WZ62-GCUSPZ PICTURE  X(12).                   CI0062
            11            WZ62-CORTY  PICTURE  X.                       CI0062
            11            WZ62-CNAVR  PICTURE  X(1).                    CI0062
            11            WZ62-DELOI3 PICTURE  9(6).                    CI0062
            11            WZ62-ALOIDD PICTURE  9(9)V99                  CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            WZ62-FILLER PICTURE  X(5).                    CI0062
      *                                                                 AMDU62
      *                                                                 AMDU62
      *                                                                 AMDU62
      *                                                                 AMDU62
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0062
          05              DE00-SUITE.                                   CI0062
            15       FILLER         PICTURE  X(00653).                  CI0062
       01                 DE10  REDEFINES      DE00.                    CI0062
            10            DE10-DU11.                                    CI0062
            11            DE10-XFONC  PICTURE  X(4).                    CI0062
            11            DE10-MPSBN  PICTURE  X(8).                    CI0062
            11            DE10-XDBDNM PICTURE  X(08).                   CI0062
            11            DE10-XSEGNM PICTURE  X(08).                   CI0062
            11            DE10-XRC    PICTURE  X(02).                   CI0062
            11            DE10-MSEG   PICTURE  X(08).                   CI0062
            11            DE10-XCOKEY PICTURE  X(70).                   CI0062
            11            DE10-CUIBR  PICTURE  X(01).                   CI0062
            11            DE10-CUIBA  PICTURE  X(01).                   CI0062
            11            DE10-IPBIK  PICTURE  X(1).                    CI0062
            10            DE10-DU03.                                    CI0062
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            11            DE10-CMSSF  PICTURE  XX.                      CI0062
            11            DE10-DU09.                                    CI0062
            12            DE10-CMESA  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            12            DE10-CMESB  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            12            DE10-CMSST  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            12            DE10-QELLAA PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            12            DE10-TMESS4 PICTURE  X(512).                  CI0062
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
       01                 MS00.                                         CI0062
          05              MS00-SUITE.                                   CI0062
            15       FILLER         PICTURE  X(00542).                  CI0062
       01                 MS03  REDEFINES      MS00.                    CI0062
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0062
                          COMPUTATIONAL-3.                              CI0062
            10            MS03-CMSSF  PICTURE  XX.                      CI0062
            10            MS03-DU09.                                    CI0062
            11            MS03-CMESA  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            11            MS03-CMESB  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            11            MS03-CMSST  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            11            MS03-QELLAA PICTURE  S9(9)                    CI0062
                          BINARY.                                       CI0062
            11            MS03-TMESS4 PICTURE  X(512).                  CI0062
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ62
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0062
      *               *                                   *             CI0062
      *               *INITIALISATIONS                    *             CI0062
      *               *                                   *             CI0062
      *               *************************************.            CI0062
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
      *N02CA.    NOTE *SET ADDRESS FOR ARRANGEMENT DB     *.
       F02CA.                                                           lv10
      **
      *********************************
      ** SET ADDRESS FOR DATABASE     *
      *********************************
      **.
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
       F02CA-FN. EXIT.
      *N02FA.    NOTE *INITIALIZE THE MESSAGE AREA        *.
       F02FA.                                                           lv10
      **
      *********************************
      ** INITIALIZE THE MS03 SEG      *
      ** WHICH WILL BE RETURNED TO THE*
      ** CALLING MODULE               *
      *********************************
      **
           INITIALIZE  MS03.
       F02FA-FN. EXIT.
      *N02HA.    NOTE *QUICK VALIDATION OF PSSD PARAMS    *.
       F02HA.    IF    WZ62-CFUNC = SPACE                               lv10
                 OR    WZ62-CLID = SPACE
                 OR    WZ62-CARTYK NOT NUMERIC
                 OR    WZ62-NARRSK NOT NUMERIC
                 OR    WZ62-CTID01 = SPACE
                 OR    WZ62-CPMTCX = SPACE
                 OR    WZ62-NAPDSK NOT NUMERIC
                 OR    WZ62-GESTD1 NOT NUMERIC
                 NEXT SENTENCE ELSE GO TO     F02HA-FN.
      *********************************
      ** DO AN INITIAL CHECK TO ENSURE*
      ** THAT ALL THE PARAMETERS ARE  *
      ** OF THE RIGHT TYPE.           *
      *********************************
      **
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012596 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
           MOVE                     ALL '1' TO FT GO TO F20.
       F02HA-FN. EXIT.
      *N02HC.    NOTE *VALIDATION OF FUNCTION PARAMETER   *.
       F02HC.    IF    WZ62-CFUNC NOT = 'GU'                            lv10
                 AND   WZ62-CFUNC NOT = 'GHU'
                 NEXT SENTENCE ELSE GO TO     F02HC-FN.
      *********************************
      ** VALIDATE THAT THE REQUESTED  *
      ** FUNCTION IS ONE THIS MODULE  *
      ** WORKS WITH.                  *
      *********************************
      **
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012595 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
           MOVE                     ALL '1' TO FT GO TO F20.
       F02HC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0062
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0062
      *               *                                   *             CI0062
      *               *FIN DE TRAITEMENT                  *             CI0062
      *               *                                   *             CI0062
      *               *************************************.            CI0062
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0062
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
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *READ FOR SPECIFIED CX12            *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *
      *********************************
      ** SET UP AND DO THE READ OF THE*
      ** CX12 SEGMENT.                *
      *********************************
      **
      *N35FA.    NOTE *SET UP SSA & READ CX01             *.
       F35FA.                                                           lv10
      *
           MOVE        'EQ' TO S-CXU01-OPER
           MOVE        WZ62-CLID TO S-CXU01-CLID
           MOVE        'EQ' TO S-CXU03-OPER
           MOVE        WZ62-CARTYK TO S-CXU03-CARTY
           MOVE        WZ62-NARRSK TO S-CXU03-NARRS
           MOVE        'EQ' TO S-CXU06-OPER
           MOVE        WZ62-CTID01 TO S-CXU06-CTID
           MOVE        'EQ' TO S-CXU12-OPER
           MOVE        WZ62-CPMTCX TO S-CXU12-CPMTC
           MOVE        WZ62-NAPDSK TO S-CXU12-NAPDS
           MOVE        WZ62-GESTD1 TO S-CXU12-GESTD.
       F35FA-FN. EXIT.
      *N35GA.    NOTE *IS REQUEST FOR READ ONLY OF CX12   *.
       F35GA.    IF    WZ62-CFUNC = 'GU'                                lv10
                 NEXT SENTENCE ELSE GO TO     F35GA-FN.
      **
      *********************************
      ** READ THE CX12 SEGMENT BASED  *
      ** ON THE FUNCTION REQUESTED.   *
      *********************************
      **
           PERFORM     F94C1 THRU F94C1-FN.
       F35GA-900. GO TO F35GM-FN.
       F35GA-FN. EXIT.
      *N35GM.    NOTE *ELSE.... READ AND HOLD CX12        *.
       F35GM.                                                           lv10
      *
      *********************************
      ** READ AND HOLD THE CX12 FOR   *
      ** UPDATE.                      *
      *********************************
           PERFORM     F94C2 THRU F94C2-FN.
       F35GM-FN. EXIT.
      *N35NA.    NOTE *IF CX12 SEGMENT NOT FOUND          *.
       F35NA.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F35NA-FN.
      *
      *********************************
      ** CX12 SEGMENT NOT FOUND.  GO  *
      ** BACK TO CALLING PROGRAM AFTER*
      ** SETTING UP ERROR MSG & COUNT *
      *********************************
      **
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012598 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
           MOVE                     ALL '1' TO FT GO TO F20.
       F35NA-900. GO TO F35OA-FN.
       F35NA-FN. EXIT.
      *N35OA.    NOTE *ELSE... LOAD UP CX12 SEG FOUND     *.
       F35OA.         EXIT.                                             lv10
      *N35OF.    NOTE *CLEANUP BAD CIRMO FIELDS           *.
       F35OF.                                                           lv15
           INSPECT     CX12-CIRMO
                REPLACING ALL LOW-VALUES
                       BY SPACES.
       F35OF-FN. EXIT.
      *N35PA.    NOTE *LOAD UP CX12 SEG FOUND             *.
       F35PA.                                                           lv15
           MOVE        CX12 TO WZ62-CX12.
       F35PA-FN. EXIT.
       F35OA-FN. EXIT.
       F35-FN.   EXIT.
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
           PERFORM     F93PC THRU F93PC-FN                              ADU029
           PERFORM     F93ER THRU F93ER-FN.
       F93EA-FN. EXIT.
      *N93ER.    NOTE *SEVERE DL/1 ERROR PROCESSING       *.
       F93ER.         EXIT.                                             lv10
      *N93ET.    NOTE *IF SEVERE ERROR; SET SEVERE ERR    *.
       F93ET.    IF    DE10-NMESS2 NOT = ZERO                           lv15
                 NEXT SENTENCE ELSE GO TO     F93ET-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F93ET-FN. EXIT.
       F93ER-FN. EXIT.
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
      *N94C1.    NOTE *CALL GU ON CX12                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CXU12-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GHU ON CX12                   *.            ADU026
       F94C2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA S-CXU06-SSA                          ADU026
           S-CXU12-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
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
