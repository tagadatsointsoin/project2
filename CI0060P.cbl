       IDENTIFICATION DIVISION.                                         CI0060
       PROGRAM-ID.  CI0060P.                                            CI0060
      *AUTHOR.         CLIENT BANK READ MODULE.                         CI0060
      *DATE-COMPILED.   09/08/14.                                       CI0060
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CI0060
       CONFIGURATION SECTION.                                           CI0060
       SOURCE-COMPUTER. IBM-370.                                        CI0060
       OBJECT-COMPUTER. IBM-370.                                        CI0060
       DATA DIVISION.                                                   CI0060
       WORKING-STORAGE SECTION.                                         CI0060
      ******************************************************************
      **  MISC FIELDS AND SWITCHES                                     *
      ******************************************************************
      **
       01  W-SEGMENT-SWITCHES.
           05  CX03-CF                 PIC X.
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
       01                 CX01.                                         CI0060
            10            CX01-CX01K.                                   CI0060
            11            CX01-C199.                                    CI0060
            12            CX01-CLID.                                    CI0060
            13            CX01-CLIDO  PICTURE  9(3).                    CI0060
            13            CX01-CLIDN.                                   CI0060
            14            CX01-CLIDNP PICTURE  X(12).                   CI0060
            14            CX01-CLIDND PICTURE  9(8).                    CI0060
            10            CX01-GEMDA  PICTURE  9(8).                    CI0060
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0060
                          BINARY.                                       CI0060
            10            CX01-FILLER PICTURE  X(5).                    CI0060
       01                 CX03.                                         CI0060
            10            CX03-GELL   PICTURE  9(4)                     CI0060
                          BINARY.                                       CI0060
            10            CX03-CY00.                                    CI0060
            11            CX03-CX03K.                                   CI0060
            12            CX03-CARTY  PICTURE  99.                      CI0060
            12            CX03-NARRS  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            CX03-CARST  PICTURE  99.                      CI0060
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            CX03-CPMTG  PICTURE  99.                      CI0060
            11            CX03-GRCRNG PICTURE  9(3).                    CI0060
            11            CX03-DEXDT  PICTURE  9(8).                    CI0060
            11            CX03-DASUP  PICTURE  9(8).                    CI0060
            11            CX03-CSTEC  PICTURE  X(3).                    CI0060
            11            CX03-FILLER PICTURE  X(17).                   CI0060
            11            CX03-CY50.                                    CI0060
            12            CX03-NARID  PICTURE  X(30).                   CI0060
            11            CX03-CY51                                     CI0060
                          REDEFINES            CX03-CY50.               CI0060
            12            CX03-NDIDN  PICTURE  9(12).                   CI0060
            12            CX03-FILLER PICTURE  X(18).                   CI0060
            11            CX03-CY52                                     CI0060
                          REDEFINES            CX03-CY50.               CI0060
            12            CX03-NAIDC  PICTURE  9(12).                   CI0060
            12            CX03-FILLER PICTURE  X(18).                   CI0060
            11            CX03-CY53                                     CI0060
                          REDEFINES            CX03-CY50.               CI0060
            12            CX03-NAMEXB PICTURE  9(15).                   CI0060
            12            CX03-FILLER PICTURE  X(15).                   CI0060
            10            CX03-CY99.                                    CI0060
            11            CX03-FILLER PICTURE  X(109).                  CI0060
            10            CX03-CY01                                     CI0060
                          REDEFINES            CX03-CY99.               CI0060
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            CX03-ICPCI  PICTURE  X.                       CI0060
            11            CX03-CLUPD  PICTURE  9(3).                    CI0060
            11            CX03-DLAUP  PICTURE  9(8).                    CI0060
            11            CX03-CWRC   PICTURE  99.                      CI0060
            11            CX03-CHCR   PICTURE  99.                      CI0060
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0060
            11            CX03-GEAUN  PICTURE  9(5).                    CI0060
            11            CX03-DPCHD  PICTURE  9(8).                    CI0060
            11            CX03-DLRCHK PICTURE  9(8).                    CI0060
            11            CX03-QTRCHK PICTURE  9(2).                    CI0060
            11            CX03-DNPMT  PICTURE  9(8).                    CI0060
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0060
                          COMPUTATIONAL-3.                              CI0060
            10            CX03-CY02                                     CI0060
                          REDEFINES            CX03-CY99.               CI0060
            11            CX03-QSIRQ  PICTURE  99.                      CI0060
            11            CX03-QDRMN  PICTURE  9(2)                     CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            CX03-DDPRE  PICTURE  9(8).                    CI0060
            11            CX03-DDSHP  PICTURE  9(8).                    CI0060
            11            CX03-NDRFTB PICTURE  9(5).                    CI0060
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0060
            11            CX03-DDSHPA PICTURE  9(8).                    CI0060
            11            CX03-NDRFTF PICTURE  9(5).                    CI0060
            11            CX03-QDIPBK PICTURE  9(3).                    CI0060
            11            CX03-CREOR  PICTURE  X(1).                    CI0060
            11            CX03-CREOR1 PICTURE  X(1).                    CI0060
            11            CX03-DDASC  PICTURE  9(8).                    CI0060
            11            CX03-FILLER PICTURE  X(7).                    CI0060
            10            CX03-CY03                                     CI0060
                          REDEFINES            CX03-CY99.               CI0060
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0060
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0060
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0060
            11            CX03-DOPDA  PICTURE  99.                      CI0060
            11            CX03-CPMTF  PICTURE  99.                      CI0060
            11            CX03-CIRMO  PICTURE  X(12).                   CI0060
            11            CX03-CPALL  PICTURE  X(1).                    CI0060
            11            CX03-CCOLM  PICTURE  9(2).                    CI0060
            11            CX03-CBLTP  PICTURE  X(1).                    CI0060
            11            CX03-CASUB  PICTURE  9(2).                    CI0060
            11            CX03-CBLFM  PICTURE  9(2).                    CI0060
            11            CX03-IBILS  PICTURE  X.                       CI0060
            11            CX03-IPAOS  PICTURE  X.                       CI0060
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0060
            11            CX03-DLBPD  PICTURE  9(8).                    CI0060
            11            CX03-DNBPD  PICTURE  9(8).                    CI0060
            11            CX03-DODBD  PICTURE  9(8).                    CI0060
            11            CX03-CPSRE  PICTURE  99.                      CI0060
            11            CX03-ISPHN  PICTURE  X.                       CI0060
            11            CX03-TCARR  PICTURE  X(6).                    CI0060
            11            CX03-CBKPT  PICTURE  9(2).                    CI0060
            11            CX03-IECNT  PICTURE  X.                       CI0060
            11            CX03-ICONV  PICTURE  X(1).                    CI0060
            11            CX03-FILLER PICTURE  X(4).                    CI0060
            10            CX03-CY04                                     CI0060
                          REDEFINES            CX03-CY99.               CI0060
            11            CX03-CCARD  PICTURE  X(02).                   CI0060
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0060
            11            CX03-IREMT  PICTURE  X(01).                   CI0060
            11            CX03-ISBILA PICTURE  X.                       CI0060
            11            CX03-DLBPDA PICTURE  9(8).                    CI0060
            11            CX03-DNBPDA.                                  CI0060
            12            CX03-DNCYM  PICTURE  9(6).                    CI0060
            12            CX03-CEDTD  PICTURE  9(2).                    CI0060
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            CX03-DREMT  PICTURE  9(8).                    CI0060
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0060
            11            CX03-CWRC2  PICTURE  99.                      CI0060
            11            CX03-CHCR2  PICTURE  99.                      CI0060
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0060
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0060
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0060
       01                 CX18.                                         CI0060
            10            CX18-CX18K.                                   CI0060
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            10            CX18-NPBN   PICTURE  X(20).                   CI0060
            10            CX18-CCBAT  PICTURE  99.                      CI0060
            10            CX18-DACHP  PICTURE  9(8).                    CI0060
            10            CX18-CSTPRE PICTURE  99.                      CI0060
            10            CX18-C199.                                    CI0060
            11            CX18-CLID.                                    CI0060
            12            CX18-CLIDO  PICTURE  9(3).                    CI0060
            12            CX18-CLIDN.                                   CI0060
            13            CX18-CLIDNP PICTURE  X(12).                   CI0060
            13            CX18-CLIDND PICTURE  9(8).                    CI0060
            10            CX18-MCSIG  PICTURE  X(30).                   CI0060
            10            CX18-CPBNU  PICTURE  X.                       CI0060
            10            CX18-CSPCR  PICTURE  99.                      CI0060
            10            CX18-DAPCR  PICTURE  9(8).                    CI0060
            10            CX18-FILLER PICTURE  XX.                      CI0060
      ******************************************************************ADU029
      ***  STORAGE AREAS FOR DL1                                        ADU029
      ******************************************************************ADU029
      ***  DL1 FUNCTIONS                                                ADU029
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU029
       01                 XW05.                                         CI0060
            10            XW05-XW06.                                    CI0060
            11            XW05-XDBPCB.                                  CI0060
            12            XW05-XDBDNM PICTURE  X(08)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-XSEGLV PICTURE  X(02)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-XRC    PICTURE  X(02)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-XPROPT PICTURE  X(04)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-FILLER PICTURE  S9(5)                    CI0060
                          VALUE                ZERO                     CI0060
                          BINARY.                                       CI0060
            12            XW05-XSEGNM PICTURE  X(08)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0060
                          VALUE                ZERO                     CI0060
                          BINARY.                                       CI0060
            12            XW05-XSEGNB PICTURE  9(05)                    CI0060
                          VALUE                ZERO                     CI0060
                          BINARY.                                       CI0060
            12            XW05-XCOKEY PICTURE  X(70)                    CI0060
                          VALUE                SPACE.                   CI0060
            10            XW05-XW07.                                    CI0060
            11            XW05-XIOPCB.                                  CI0060
            12            XW05-XTERMI PICTURE  X(08)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-FILLER PICTURE  XX                       CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-XRC1   PICTURE  X(02)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-FILLER PICTURE  X(12)                    CI0060
                          VALUE                SPACE.                   CI0060
            12            XW05-XMODNM PICTURE  X(8)                     CI0060
                          VALUE                SPACE.                   CI0060
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0060
                          VALUE                ZERO.                    CI0060
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0060
                          VALUE                ZERO.                    CI0060
            10            XW05-XGU    PICTURE  X(4)                     CI0060
                          VALUE                'GU  '.                  CI0060
            10            XW05-XGHU   PICTURE  X(4)                     CI0060
                          VALUE                'GHU '.                  CI0060
            10            XW05-XGN    PICTURE  X(4)                     CI0060
                          VALUE                'GN  '.                  CI0060
            10            XW05-XGHN   PICTURE  X(4)                     CI0060
                          VALUE                'GHN '.                  CI0060
            10            XW05-XGNP   PICTURE  X(4)                     CI0060
                          VALUE                'GNP '.                  CI0060
            10            XW05-XGHNP  PICTURE  X(4)                     CI0060
                          VALUE                'GHNP'.                  CI0060
            10            XW05-XREPL  PICTURE  XXXX                     CI0060
                          VALUE                'REPL'.                  CI0060
            10            XW05-XISRT  PICTURE  X(4)                     CI0060
                          VALUE                'ISRT'.                  CI0060
            10            XW05-XDLET  PICTURE  X(4)                     CI0060
                          VALUE                'DLET'.                  CI0060
            10            XW05-XOPEN  PICTURE  X(4)                     CI0060
                          VALUE                'OPEN'.                  CI0060
            10            XW05-XCLSE  PICTURE  X(4)                     CI0060
                          VALUE                'CLSE'.                  CI0060
            10            XW05-XCHKP  PICTURE  X(4)                     CI0060
                          VALUE                'CHKP'.                  CI0060
            10            XW05-XXRST  PICTURE  X(4)                     CI0060
                          VALUE                'XRST'.                  CI0060
            10            XW05-XTERM  PICTURE  X(4)                     CI0060
                          VALUE                'TERM'.                  CI0060
            10            XW05-XNFPAC PICTURE  X(13)                    CI0060
                          VALUE                SPACE.                   CI0060
      *!WI pl=DL200                                                     ADU029
       01  DL01-KFPCB                                                   ADU029
                        PICTURE X(04)                                   CI0060
              VALUE 'PCB '.                                             ADU029
      ***  SAVE AREA FOR DL1 FUNCTION VALUE - USED FOR ERROR PROCESSING ADU029
       01  SV01-FUNC     PIC X(4).                                      ADU029
      ******************************************************************ADU029
      *** USED WHEN DYNAMICALLY CALLING PCB & UIB ERROR CHECKING MODULESADU029
      ***    (CI0008P - UIB ERROR CHECK    CI0009P - PCB ERROR CHECK)   ADU029
      ******************************************************************ADU029
      *!WI pl=DN100                                                     ADU029
       01  W-PASS-XPROGR                                                ADU029
                        PICTURE X(8).                                   CI0060
      *                   DU9D   <--- INDEX FOR DU9D IN WZ60 (KEEP SYNC)
       01   DEBUT-WSS.                                                  CI0060
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0060
            05   IK     PICTURE X.                                      CI0060
       01  CONSTANTES-PAC.                                              CI0060
           05  FILLER  PICTURE X(87)   VALUE                            CI0060
                     '6015 CAT09/08/14CI0060ADMIN   14:34:22CI0060P AMERCI0060
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0060
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0060
           05  NUGNA   PICTURE X(5).                                    CI0060
           05  APPLI   PICTURE X(3).                                    CI0060
           05  DATGN   PICTURE X(8).                                    CI0060
           05  PROGR   PICTURE X(6).                                    CI0060
           05  CODUTI  PICTURE X(8).                                    CI0060
           05  TIMGN   PICTURE X(8).                                    CI0060
           05  PROGE   PICTURE X(8).                                    CI0060
           05  COBASE  PICTURE X(4).                                    CI0060
           05  DATGNC  PICTURE X(10).                                   CI0060
           05  RELEAS  PICTURE X(7).                                    CI0060
           05  DATGE   PICTURE X(10).                                   CI0060
           05  DATSQ   PICTURE X(10).                                   CI0060
       01  DATCE.                                                       CI0060
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0060
         05  DATOR.                                                     CI0060
           10  DATOA  PICTURE XX.                                       CI0060
           10  DATOM  PICTURE XX.                                       CI0060
           10  DATOJ  PICTURE XX.                                       CI0060
       01   VARIABLES-CONDITIONNELLES.                                  CI0060
            05                  FT      PICTURE X VALUE '0'.            CI0060
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0060
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0060
            05           IDU9DL PICTURE S9(4) VALUE  ZERO.
            05           IDU9DR PICTURE S9(4) VALUE  ZERO.
            05           IDU9DM PICTURE S9(4) VALUE +0020.
            05           J02DDR PICTURE S9(4) VALUE  ZERO.
            05           J50NAR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0060
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0060
       01               S-CX01-SSA.                                     CI0060
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0060
                                      VALUE 'CX01    '.                 CI0060
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0060
            10          S-CX01-CCOD   PICTURE X(5)                      CI0060
                                      VALUE '-----'.                    CI0060
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0060
       01            S-CXU01-SSA.                                       CI0060
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX01    '.                 CI0060
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0060
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CX01K'.                   CI0060
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0060
            10       S-CXU01-CX01K.                                     CI0060
            11       S-CXU01-C199.                                      CI0060
            12       S-CXU01-CLID.                                      CI0060
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0060
            13       S-CXU01-CLIDN.                                     CI0060
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0060
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0060
            10  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01               S-CX03-SSA.                                     CI0060
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0060
                                      VALUE 'CX03    '.                 CI0060
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0060
            10          S-CX03-CCOD   PICTURE X(5)                      CI0060
                                      VALUE '-----'.                    CI0060
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0060
       01            S-CXA03-SSA.                                       CI0060
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0060
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CARTY'.                   CI0060
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0060
            12       S-CXA03-CARTY    PICTURE  99.                      CI0060
            12  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXB03-SSA.                                       CI0060
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0060
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(NARRS'.                   CI0060
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0060
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            12  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXC03-SSA.                                       CI0060
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0060
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CPMTG'.                   CI0060
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0060
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0060
            11  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXD03-SSA.                                       CI0060
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0060
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(GRCRNG'.                  CI0060
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0060
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0060
            11  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXE03-SSA.                                       CI0060
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0060
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(DEXDT'.                   CI0060
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0060
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0060
            11  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXF03-SSA.                                       CI0060
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0060
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CY50'.                    CI0060
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0060
            11       S-CXF03-CY50.                                      CI0060
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0060
            11  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXG03-SSA.                                       CI0060
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0060
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(NBASQ'.                   CI0060
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0060
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            11  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXH03-SSA.                                       CI0060
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0060
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(NARID'.                   CI0060
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0060
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0060
            12  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXU03-SSA.                                       CI0060
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX03    '.                 CI0060
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0060
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CX03K'.                   CI0060
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0060
            11       S-CXU03-CX03K.                                     CI0060
            12       S-CXU03-CARTY    PICTURE  99.                      CI0060
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            11  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01               S-CX18-SSA.                                     CI0060
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0060
                                      VALUE 'CX18    '.                 CI0060
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0060
            10          S-CX18-CCOD   PICTURE X(5)                      CI0060
                                      VALUE '-----'.                    CI0060
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0060
       01            S-CXA18-SSA.                                       CI0060
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX18    '.                 CI0060
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0060
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CSTPRE'.                  CI0060
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0060
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0060
            10  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXB18-SSA.                                       CI0060
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX18    '.                 CI0060
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0060
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CSPCR'.                   CI0060
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0060
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0060
            10  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01            S-CXU18-SSA.                                       CI0060
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0060
                                      VALUE 'CX18    '.                 CI0060
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0060
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0060
                                      VALUE '-----'.                    CI0060
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0060
                                      VALUE '(CX18K'.                   CI0060
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0060
            10       S-CXU18-CX18K.                                     CI0060
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            10  FILLER   PICTURE X    VALUE ')'.                        CI0060
       01   ZONES-UTILISATEUR PICTURE X.                                CI0060
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
       01                 PA00.                                         CI0060
          05              PA00-SUITE.                                   CI0060
            15       FILLER         PICTURE  X(00106).                  CI0060
       01                 PA06  REDEFINES      PA00.                    CI0060
            10            PA06-XDBPCB.                                  CI0060
            11            PA06-XDBDNM PICTURE  X(08).                   CI0060
            11            PA06-XSEGLV PICTURE  X(02).                   CI0060
            11            PA06-XRC    PICTURE  X(02).                   CI0060
            11            PA06-XPROPT PICTURE  X(04).                   CI0060
            11            PA06-FILLER PICTURE  S9(5)                    CI0060
                          BINARY.                                       CI0060
            11            PA06-XSEGNM PICTURE  X(08).                   CI0060
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0060
                          BINARY.                                       CI0060
            11            PA06-XSEGNB PICTURE  9(05)                    CI0060
                          BINARY.                                       CI0060
            11            PA06-XCOKEY PICTURE  X(70).                   CI0060
      *                                                                 AMDU60
      ******************************************************************AMDU60
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET A      *AMDU60
      **     CLIENTS ARRANGEMENTS & ASSOC BANK FOR A SPECIFIED ARR TYPE*AMDU60
      ******************************************************************AMDU60
      *                                                                 AMDU60
      *!WF DSP=WZ DSL=DU SEL=60 FOR=I LEV=1                             AMDU60
       01                 WZ00.                                         CI0060
          05              WZ00-SUITE.                                   CI0060
            15       FILLER         PICTURE  X(05884).                  CI0060
       01                 WZ60  REDEFINES      WZ00.                    CI0060
            10            WZ60-CLID01 PICTURE  X(23).                   CI0060
            10            WZ60-CARTYK PICTURE  99.                      CI0060
            10            WZ60-CFUNC  PICTURE  X(3).                    CI0060
            10            WZ60-NARRSK PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            10            WZ60-GECTR  PICTURE  99.                      CI0060
            10            WZ60-NARRSB PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            10            WZ60-NSEQ4B PICTURE  9(8)                     CI0060
                          BINARY.                                       CI0060
            10            WZ60-FILLER PICTURE  X(46).                   CI0060
            10            WZ60-DU9D                                     CI0060
                          OCCURS       020     TIMES.                   CI0060
            11            WZ60-CX03.                                    CI0060
            12            WZ60-GELL   PICTURE  9(4)                     CI0060
                          BINARY.                                       CI0060
            12            WZ60-CY00.                                    CI0060
            13            WZ60-CX03K.                                   CI0060
            14            WZ60-CARTY  PICTURE  99.                      CI0060
            14            WZ60-NARRS  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            13            WZ60-CARST  PICTURE  99.                      CI0060
            13            WZ60-GECSQ  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            13            WZ60-CPMTG  PICTURE  99.                      CI0060
            13            WZ60-GRCRNG PICTURE  9(3).                    CI0060
            13            WZ60-DEXDT  PICTURE  9(8).                    CI0060
            13            WZ60-DASUP  PICTURE  9(8).                    CI0060
            13            WZ60-CSTEC  PICTURE  X(3).                    CI0060
            13            WZ60-FILLER PICTURE  X(17).                   CI0060
            13            WZ60-CY50.                                    CI0060
            14            WZ60-NARID  PICTURE  X(30).                   CI0060
            13            WZ60-CY51                                     CI0060
                          REDEFINES            WZ60-CY50.               CI0060
            14            WZ60-NDIDN  PICTURE  9(12).                   CI0060
            14            WZ60-FILLER PICTURE  X(18).                   CI0060
            13            WZ60-CY52                                     CI0060
                          REDEFINES            WZ60-CY50.               CI0060
            14            WZ60-NAIDC  PICTURE  9(12).                   CI0060
            14            WZ60-FILLER PICTURE  X(18).                   CI0060
            13            WZ60-CY53                                     CI0060
                          REDEFINES            WZ60-CY50.               CI0060
            14            WZ60-NAMEXB PICTURE  9(15).                   CI0060
            14            WZ60-FILLER PICTURE  X(15).                   CI0060
            12            WZ60-CY99.                                    CI0060
            13            WZ60-FILLER PICTURE  X(109).                  CI0060
            12            WZ60-CY01                                     CI0060
                          REDEFINES            WZ60-CY99.               CI0060
            13            WZ60-NBASQ  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            13            WZ60-ICPCI  PICTURE  X.                       CI0060
            13            WZ60-CLUPD  PICTURE  9(3).                    CI0060
            13            WZ60-DLAUP  PICTURE  9(8).                    CI0060
            13            WZ60-CWRC   PICTURE  99.                      CI0060
            13            WZ60-CHCR   PICTURE  99.                      CI0060
            13            WZ60-GEOPD2 PICTURE  X(8).                    CI0060
            13            WZ60-GEAUN  PICTURE  9(5).                    CI0060
            13            WZ60-DPCHD  PICTURE  9(8).                    CI0060
            13            WZ60-DLRCHK PICTURE  9(8).                    CI0060
            13            WZ60-QTRCHK PICTURE  9(2).                    CI0060
            13            WZ60-DNPMT  PICTURE  9(8).                    CI0060
            13            WZ60-APMTLA PICTURE  S9(9)V99                 CI0060
                          COMPUTATIONAL-3.                              CI0060
            12            WZ60-CY02                                     CI0060
                          REDEFINES            WZ60-CY99.               CI0060
            13            WZ60-QSIRQ  PICTURE  99.                      CI0060
            13            WZ60-QDRMN  PICTURE  9(2)                     CI0060
                          COMPUTATIONAL-3.                              CI0060
            13            WZ60-DDPRE  PICTURE  9(8).                    CI0060
            13            WZ60-DDSHP  PICTURE  9(8).                    CI0060
            13            WZ60-NDRFTB PICTURE  9(5).                    CI0060
            13            WZ60-QDIPBJ PICTURE  9(3).                    CI0060
            13            WZ60-DDSHPA PICTURE  9(8).                    CI0060
            13            WZ60-NDRFTF PICTURE  9(5).                    CI0060
            13            WZ60-QDIPBK PICTURE  9(3).                    CI0060
            13            WZ60-CREOR  PICTURE  X(1).                    CI0060
            13            WZ60-CREOR1 PICTURE  X(1).                    CI0060
            13            WZ60-DDASC  PICTURE  9(8).                    CI0060
            13            WZ60-FILLER PICTURE  X(7).                    CI0060
            12            WZ60-CY03                                     CI0060
                          REDEFINES            WZ60-CY99.               CI0060
            13            WZ60-DLAUP1 PICTURE  9(8).                    CI0060
            13            WZ60-GEOPD3 PICTURE  X(8).                    CI0060
            13            WZ60-DNPMT1 PICTURE  9(8).                    CI0060
            13            WZ60-DOPDA  PICTURE  99.                      CI0060
            13            WZ60-CPMTF  PICTURE  99.                      CI0060
            13            WZ60-CIRMO  PICTURE  X(12).                   CI0060
            13            WZ60-CPALL  PICTURE  X(1).                    CI0060
            13            WZ60-CCOLM  PICTURE  9(2).                    CI0060
            13            WZ60-CBLTP  PICTURE  X(1).                    CI0060
            13            WZ60-CASUB  PICTURE  9(2).                    CI0060
            13            WZ60-CBLFM  PICTURE  9(2).                    CI0060
            13            WZ60-IBILS  PICTURE  X.                       CI0060
            13            WZ60-IPAOS  PICTURE  X.                       CI0060
            13            WZ60-CBLSQ  PICTURE  X(4).                    CI0060
            13            WZ60-DLBPD  PICTURE  9(8).                    CI0060
            13            WZ60-DNBPD  PICTURE  9(8).                    CI0060
            13            WZ60-DODBD  PICTURE  9(8).                    CI0060
            13            WZ60-CPSRE  PICTURE  99.                      CI0060
            13            WZ60-ISPHN  PICTURE  X.                       CI0060
            13            WZ60-TCARR  PICTURE  X(6).                    CI0060
            13            WZ60-CBKPT  PICTURE  9(2).                    CI0060
            13            WZ60-IECNT  PICTURE  X.                       CI0060
            13            WZ60-ICONV  PICTURE  X(1).                    CI0060
            13            WZ60-FILLER PICTURE  X(4).                    CI0060
            12            WZ60-CY04                                     CI0060
                          REDEFINES            WZ60-CY99.               CI0060
            13            WZ60-CCARD  PICTURE  X(02).                   CI0060
            13            WZ60-MCSIG4 PICTURE  X(20).                   CI0060
            13            WZ60-IREMT  PICTURE  X(01).                   CI0060
            13            WZ60-ISBILA PICTURE  X.                       CI0060
            13            WZ60-DLBPDA PICTURE  9(8).                    CI0060
            13            WZ60-DNBPDA.                                  CI0060
            14            WZ60-DNCYM  PICTURE  9(6).                    CI0060
            14            WZ60-CEDTD  PICTURE  9(2).                    CI0060
            13            WZ60-AREMT  PICTURE  S9(7)V99                 CI0060
                          COMPUTATIONAL-3.                              CI0060
            13            WZ60-DREMT  PICTURE  9(8).                    CI0060
            13            WZ60-ADBRQ  PICTURE  S9(11)V99                CI0060
                          COMPUTATIONAL-3.                              CI0060
            13            WZ60-CLUPD1 PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            13            WZ60-DLAUP3 PICTURE  9(8).                    CI0060
            13            WZ60-CWRC2  PICTURE  99.                      CI0060
            13            WZ60-CHCR2  PICTURE  99.                      CI0060
            13            WZ60-GEOPD9 PICTURE  X(8).                    CI0060
            13            WZ60-GEAUN1 PICTURE  9(5).                    CI0060
            13            WZ60-DPCHD1 PICTURE  9(8).                    CI0060
            11            WZ60-CX18.                                    CI0060
            12            WZ60-CX18K.                                   CI0060
            13            WZ60-NBASQ  PICTURE  S9(3)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            12            WZ60-NPBN   PICTURE  X(20).                   CI0060
            12            WZ60-CCBAT  PICTURE  99.                      CI0060
            12            WZ60-DACHP  PICTURE  9(8).                    CI0060
            12            WZ60-CSTPRE PICTURE  99.                      CI0060
            12            WZ60-C199.                                    CI0060
            13            WZ60-CLID.                                    CI0060
            14            WZ60-CLIDO  PICTURE  9(3).                    CI0060
            14            WZ60-CLIDN.                                   CI0060
            15            WZ60-CLIDNP PICTURE  X(12).                   CI0060
            15            WZ60-CLIDND PICTURE  9(8).                    CI0060
            12            WZ60-MCSIG  PICTURE  X(30).                   CI0060
            12            WZ60-CPBNU  PICTURE  X.                       CI0060
            12            WZ60-CSPCR  PICTURE  99.                      CI0060
            12            WZ60-DAPCR  PICTURE  9(8).                    CI0060
            12            WZ60-FILLER PICTURE  XX.                      CI0060
      *                                                                 AMDU60
      *                                                                 AMDU60
      *                                                                 AMDU60
      *                                                                 AMDU60
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0060
          05              DE00-SUITE.                                   CI0060
            15       FILLER         PICTURE  X(00653).                  CI0060
       01                 DE10  REDEFINES      DE00.                    CI0060
            10            DE10-DU11.                                    CI0060
            11            DE10-XFONC  PICTURE  X(4).                    CI0060
            11            DE10-MPSBN  PICTURE  X(8).                    CI0060
            11            DE10-XDBDNM PICTURE  X(08).                   CI0060
            11            DE10-XSEGNM PICTURE  X(08).                   CI0060
            11            DE10-XRC    PICTURE  X(02).                   CI0060
            11            DE10-MSEG   PICTURE  X(08).                   CI0060
            11            DE10-XCOKEY PICTURE  X(70).                   CI0060
            11            DE10-CUIBR  PICTURE  X(01).                   CI0060
            11            DE10-CUIBA  PICTURE  X(01).                   CI0060
            11            DE10-IPBIK  PICTURE  X(1).                    CI0060
            10            DE10-DU03.                                    CI0060
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            11            DE10-CMSSF  PICTURE  XX.                      CI0060
            11            DE10-DU09.                                    CI0060
            12            DE10-CMESA  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            12            DE10-CMESB  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            12            DE10-CMSST  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            12            DE10-QELLAA PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            12            DE10-TMESS4 PICTURE  X(512).                  CI0060
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
       01                 MS00.                                         CI0060
          05              MS00-SUITE.                                   CI0060
            15       FILLER         PICTURE  X(00542).                  CI0060
       01                 MS03  REDEFINES      MS00.                    CI0060
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0060
                          COMPUTATIONAL-3.                              CI0060
            10            MS03-CMSSF  PICTURE  XX.                      CI0060
            10            MS03-DU09.                                    CI0060
            11            MS03-CMESA  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            11            MS03-CMESB  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            11            MS03-CMSST  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            11            MS03-QELLAA PICTURE  S9(9)                    CI0060
                          BINARY.                                       CI0060
            11            MS03-TMESS4 PICTURE  X(512).                  CI0060
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ60
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0060
      *               *                                   *             CI0060
      *               *INITIALISATIONS                    *             CI0060
      *               *                                   *             CI0060
      *               *************************************.            CI0060
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
      *N02DA.    NOTE *INIT PASS TBL, INDEX & MISC FLDS   *.
       F02DA.                                                           lv10
      **
      *********************************
      ** INITIALIZE THE DU9D SEG (TBL)*
      ** WHICH WILL BE RETURNED TO THE*
      ** CALLING MODULE.  ALSO, INIT  *
      ** THE TABLE INDEXES & MISC FLDS*
      *********************************
      **
           INITIALIZE  IDU9DR
           MOVE        1 TO IDU9DL
           MOVE        ZERO TO WZ60-GECTR
           MOVE        ZERO TO WZ60-NARRSB
           MOVE        ZERO TO WZ60-NSEQ4B.
      *N02DD.    NOTE *LOOP TO INIT DU9D PART OF WZ60     *.
       F02DD.                                                           lv15
           MOVE        1                        TO J02DDR
                                    GO TO     F02DD-B.
       F02DD-A.
           ADD         1                        TO J02DDR.
       F02DD-B.
           IF          J02DDR                   >  IDU9DM
                                    GO TO     F02DD-FN.
           INITIALIZE  WZ60-DU9D (J02DDR).
       F02DD-900. GO TO F02DD-A.
       F02DD-FN. EXIT.
       F02DA-FN. EXIT.
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
       F02HA.    IF    WZ60-CLID01 = SPACE                              lv10
                 OR    WZ60-CARTYK NOT NUMERIC
                 OR    WZ60-CFUNC = SPACE
                 OR    WZ60-NARRSK NOT NUMERIC
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
       F02HC.    IF    WZ60-CFUNC NOT = 'GU'                            lv10
                 AND   WZ60-CFUNC NOT = 'GHU'
                 AND   WZ60-CFUNC NOT = 'GN'
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
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0060
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0060
      *               *                                   *             CI0060
      *               *FIN DE TRAITEMENT                  *             CI0060
      *               *                                   *             CI0060
      *               *************************************.            CI0060
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0060
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
      *               *COMMON CX01 PROCESSING             *
      *               *                                   *
      *               *************************************.
       F35.                                                             lv05
      *
      *********************************
      ** DO INITIAL READ ON CX01 AND  *
      ** DETERMINE IF PROCESSING CAN  *
      ** CONTINUE.                    *
      *********************************
      **
      *N35FA.    NOTE *SET UP SSA & READ CX01             *.
       F35FA.                                                           lv10
      *
           MOVE        WZ60-CLID01 TO S-CXU01-CLID
      *
      *********************************
      ** READ THE CX01 SEGMENT FOR THE*
      ** CLID PASSED IN FROM CALLING  *
      ** PROGRAM.                     *
      *********************************
      **
           PERFORM     F94C1 THRU F94C1-FN.
       F35FA-FN. EXIT.
      *N35HA.    NOTE *IF CX01 SEGMENT NOT FOUND          *.
       F35HA.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F35HA-FN.
      *
      *********************************
      ** CX01 SEGMENT NOT FOUND.  GO  *
      ** BACK TO CALLING PROGRAM AFTER*
      ** SETTING UP ERROR MSG & COUNT *
      *********************************
      **
           MOVE        ZEROS TO WZ60-GECTR
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012006 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
           MOVE                     ALL '1' TO FT GO TO F20.
       F35HA-900. GO TO F35JA-FN.
       F35HA-FN. EXIT.
      *N35JA.    NOTE *ELSE..... STORE SEQ NUM            *.
       F35JA.                                                           lv10
           MOVE        CX01-NSEQ4B TO WZ60-NSEQ4B.
       F35JA-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *IS REQUEST FOR UNIQUE DATA         *
      *               *                                   *
      *               *************************************.
       F40.      IF    WZ60-CFUNC = 'GU'                                lv05
                 OR    = 'GHU'
                 NEXT SENTENCE ELSE GO TO     F40-FN.
      *********************************
      ** CALLING RPC IS RESPONSIBLE   *
      ** FOR PASSING ARRANGEMENT TYPE *
      ** (CARTY) AND ARRANGEMENT SEQ  *
      ** NUMBER (NAARS) IF USING GU   *
      ** OR GHU.                      *
      ** ONCE A CX03 IS FOUND THE     *
      ** CX18 WILL BE ACCESSED USING  *
      ** THE RELATED BANK FROM THE    *
      ** CX03                         *
      *********************************
      *
      *N40CA.    NOTE *DO A GU ON CX03                    *.
       F40CA.                                                           lv10
      **
           MOVE        'EQ' TO S-CXU03-OPER
           MOVE        WZ60-CARTYK TO S-CXU03-CARTY
           MOVE        WZ60-NARRSK TO S-CXU03-NARRS
      *
           PERFORM     F94C3 THRU F94C3-FN.
       F40CA-FN. EXIT.
      *N40DA.    NOTE *IF CX03 NOT FOUND; ERROR SET       *.
       F40DA.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *
      *********************************
      ** CX03 SEGMENT NOT FOUND.  GO  *
      ** BACK TO CALLING PROGRAM AFTER*
      ** SETTING UP ERROR MSG & COUNT *
      *********************************
      **
           MOVE        ZEROS TO WZ60-GECTR
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012007 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
           MOVE                     ALL '1' TO FT GO TO F20.
       F40DA-900. GO TO F40EA-FN.
       F40DA-FN. EXIT.
      *N40EA.    NOTE *ELSE..CX03 FOUND;STORE; CONTINUE   *.
       F40EA.                                                           lv10
           MOVE        1 TO WZ60-GECTR
           MOVE        +1 TO IDU9DL
           MOVE        CX03 TO WZ60-CX03 (IDU9DL).
      *N40FA.    NOTE *SET UP CX18 SSA WITH CX03 DATA     *.
       F40FA.                                                           lv15
      **
           MOVE        'EQ' TO S-CXU18-OPER
           MOVE        CX03-NBASQ TO S-CXU18-NBASQ.
       F40FA-FN. EXIT.
      *N40GA.    NOTE *IS REQUEST FOR READ ONLY           *.
       F40GA.    IF    WZ60-CFUNC = 'GU'                                lv15
                 NEXT SENTENCE ELSE GO TO     F40GA-FN.
      *
           PERFORM     F94C4 THRU F94C4-FN.
       F40GA-900. GO TO F40GM-FN.
       F40GA-FN. EXIT.
      *N40GM.    NOTE *ELSE.... READ AND HOLD             *.
       F40GM.                                                           lv15
      *
      *********************************
      ** IN THEORY, THE CX03 SHOULD BE*
      ** HELD TOO, BUT IMS DOES NOT   *
      ** ALLOW LOCKING ON TWO SEGMENTS*
      ** SO A LOCK IS DONE ON CX18    *
      ** TO SATISFY ONE LOCK          *
      *********************************
           PERFORM     F94C5 THRU F94C5-FN.
       F40GM-FN. EXIT.
      *N40HA.    NOTE *IF CX18 NOT FOUND; SEVERE ERROR    *.
       F40HA.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40HA-FN.
      *
      *********************************
      ** NO CX18 FOUND, WHEN A GU OR  *
      ** GHU IS DONE.. SOMETHING IS   *
      ** WAY OUT OF WACK.. GET OUT    *
      *********************************
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012027 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN.                             ADU019
       F40HA-900. GO TO F40IA-FN.
       F40HA-FN. EXIT.
      *N40IA.    NOTE *ELSE.... STORE CX18 SEGMENT        *.
       F40IA.                                                           lv15
      *
           MOVE        CX18 TO WZ60-CX18 (IDU9DL).
       F40IA-FN. EXIT.
       F40EA-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *IS REQUEST FOR ALL ARR OF TYPE     *
      *               *                                   *
      *               *************************************.
       F50.      IF    WZ60-CFUNC = 'GN'                                lv05
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *N50CA.    NOTE *READ(GN) FIRST/REPOSIT ON CX03     *.
       F50CA.                                                           lv10
      *********************************
      ** PROCESS ARRANGEMENTS OF TYPE *
      ** WZ60-CARTYK FOR THE CLID01.  *
      ** IF A NARRSK IS SENT THIS READ*
      ** WILL DO REPOSITIONING.       *
      *********************************
           MOVE        'GE' TO S-CXU03-OPER
           MOVE        WZ60-CARTYK TO S-CXU03-CARTY
           MOVE        WZ60-NARRSK TO S-CXU03-NARRS
           PERFORM     F94C3 THRU F94C3-FN.
       F50CA-FN. EXIT.
      *N50DA.    NOTE *IF CX03 SEGMENT NOT FOUND          *.
       F50DA.    IF    IK = '1'                                         lv10
                 OR    (IK = '0'
                 AND   WZ60-NARRSK > ZERO
                 AND   WZ60-CARTYK NOT =
                       CX03-CARTY)
                 NEXT SENTENCE ELSE GO TO     F50DA-FN.
      *********************************
      ** CX01 SEGMENT NOT FOUND.  GO  *
      ** BACK TO CALLING PROGRAM AFTER*
      ** SETTING UP ERROR MSG & COUNT *
      *********************************
      **
           MOVE        ZEROS TO WZ60-GECTR.
      *N50DE.    NOTE *IF PROBLEM REPOSITIONING IN CX03   *.
       F50DE.    IF    WZ60-NARRSK > ZERO                               lv15
                 NEXT SENTENCE ELSE GO TO     F50DE-FN.
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012593 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN.                             ADU019
       F50DE-900. GO TO F50DL-FN.
       F50DE-FN. EXIT.
      *N50DL.    NOTE *ELSE... NO CX03 FOR THE CLIENT     *.
       F50DL.                                                           lv15
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012007 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN.                             ADU019
       F50DL-FN. EXIT.
      *N50DR.    NOTE *RETURN TO CALLING PROGRAM          *.
       F50DR.                                                           lv15
      *
      *********************************
      **  RETURN TO CALLING PROGRAM   *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F50DR-FN. EXIT.
       F50DA-900. GO TO F50FA-FN.
       F50DA-FN. EXIT.
      *N50FA.    NOTE *ELSE... CX03 WAS FOUND             *.
       F50FA.         EXIT.                                             lv10
      *N50HA.    NOTE *CX03 LOOP - FOR SPECIFIED CARTYK   *.
       F50HA.    IF    IK = '0'                                         lv15
                 AND   CX03-CARTY = WZ60-CARTYK
                 AND   IDU9DL NOT > IDU9DM
                 NEXT SENTENCE ELSE GO TO     F50HA-FN.
      *N50IA.    NOTE *STORE CX03 DATA TO WDUD(I) TABLE   *.
       F50IA.                                                           lv20
           MOVE        CX03 TO WZ60-CX03 (IDU9DL).
       F50IA-FN. EXIT.
      *N50JA.    NOTE *INCREMENT DU9D INDEX ON WZ60 TBL   *.
       F50JA.                                                           lv20
           COMPUTE     IDU9DL = IDU9DL + 1.
       F50JA-FN. EXIT.
      *N50KA.    NOTE *READ (GN) NEXT CX03 FOR CLIENT     *.
       F50KA.                                                           lv20
           PERFORM     F94C2 THRU F94C2-FN.
       F50KA-FN. EXIT.
       F50HA-900. GO TO F50HA.
       F50HA-FN. EXIT.
      *N50LA.    NOTE *SET FIELDS FOLLOWING CX03 LOOP     *.
       F50LA.                                                           lv15
      *
      *********************************
      ** SET NBR OF ROWS IN DU9D TABLE*
      ** IN THE WZ60 SEGMENT & ADJUST *
      ** THE INDEX BACK TO THE ACTUAL *
      ** NBR OF CX03S FOUND.          *
      *********************************
      *
           COMPUTE     IDU9DL = IDU9DL - 1
           MOVE        IDU9DL TO WZ60-GECTR.
       F50LA-FN. EXIT.
      *N50MA.    NOTE *IF INDEX MAXED - SAVE CX03 READ    *.
       F50MA.    IF    IDU9DL = IDU9DM                                  lv15
                 AND   CX03-CARTY = WZ60-CARTYK
                 NEXT SENTENCE ELSE GO TO     F50MA-FN.
      *********************************
      ** IF THE CLIENT HAS MORE CX03S *
      ** OF THE SPECIFIED CARTYK THAN *
      ** THE DU9D TABLE MAX - STORE   *
      ** THE 'NEXT' CX03 TO NARRSB    *
      ** FOR REPOSITIONING            *
      *********************************
      *
           MOVE        CX03-NARRS TO WZ60-NARRSB.
       F50MA-FN. EXIT.
      *N50NA.    NOTE *READ(GU) CX18 FOR CX03S IN DU9D    *.
       F50NA.                                                           lv15
           MOVE        1                        TO J50NAR
                                    GO TO     F50NA-B.
       F50NA-A.
           ADD         1                        TO J50NAR.
       F50NA-B.
           IF          J50NAR                   >  IDU9DL
                                    GO TO     F50NA-FN.
      *
      *********************************
      ** READ THE CX18 SEGMENT FOR    *
      ** EACH CX03 STORED IN THE DU9D *
      ** TABLE.  A CX18 MAY NOT EXIST *
      ** FOR THE CX03 - JUST LEAVE ITS*
      ** CX18 BLANK AND MOVE ON TO THE*
      ** NEXT CX03.                   *
      *********************************
      *
           MOVE        'EQ' TO S-CXU18-OPER
           MOVE        WZ60-CX03 (J50NAR) TO CX03
           MOVE        CX03-NBASQ TO S-CXU18-NBASQ
           PERFORM     F94C4 THRU F94C4-FN.
      *N50OA.    NOTE *IF CX18 FOUND - WRITE TO DU9D(I)   *.
       F50OA.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50OA-FN.
      *
           MOVE        CX18 TO WZ60-CX18 (J50NAR).
       F50OA-FN. EXIT.
       F50NA-900. GO TO F50NA-A.
       F50NA-FN. EXIT.
       F50FA-FN. EXIT.
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
           PERFORM     F93PC THRU F93PC-FN                              ADU029
           PERFORM     F93ER THRU F93ER-FN.
       F93EA-FN. EXIT.
      *N93ER.    NOTE *SEVERE DL/1 ERROR PROCESSING       *.
       F93ER.         EXIT.                                             lv10
      *N93ET.    NOTE *IF SEVERE ERROR; SET SEVERE ERR    *.
       F93ET.    IF    DE10-NMESS2 NOT = ZERO                           lv15
                 NEXT SENTENCE ELSE GO TO     F93ET-FN.
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012474 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
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
      *N94C1.    NOTE *CALL GU ON CX01                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GN ON CX03                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CX03-SSA                                       ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94C3.    NOTE *CALL GN ON CX03                    *.            ADU026
       F94C3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
      *N94C4.    NOTE *CALL GU ON CX18                    *.            ADU026
       F94C4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C4-FN. EXIT.
      *N94C5.    NOTE *CALL GHU ON CX18                   *.            ADU026
       F94C5.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 CX18                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C5-FN. EXIT.
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
