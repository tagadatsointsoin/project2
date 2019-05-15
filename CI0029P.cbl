       IDENTIFICATION DIVISION.                                         CI0029
       PROGRAM-ID.  CI0029P.                                            CI0029
      *AUTHOR.         M\M - WRITE PAYOUT CONFIRM MOD.                  CI0029
      *DATE-COMPILED.   09/08/14.                                       CI0029
       ENVIRONMENT DIVISION.                                            CI0029
       CONFIGURATION SECTION.                                           CI0029
       SOURCE-COMPUTER. IBM-370.                                        CI0029
       OBJECT-COMPUTER. IBM-370.                                        CI0029
       DATA DIVISION.                                                   CI0029
       WORKING-STORAGE SECTION.                                         CI0029
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
      ******************************************************************ADU029
      ***  STORAGE AREAS FOR DL1                                        ADU029
      ******************************************************************ADU029
      ***  DL1 FUNCTIONS                                                ADU029
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU029
       01                 XW05.                                         CI0029
            10            XW05-XW06.                                    CI0029
            11            XW05-XDBPCB.                                  CI0029
            12            XW05-XDBDNM PICTURE  X(08)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-XSEGLV PICTURE  X(02)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-XRC    PICTURE  X(02)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-XPROPT PICTURE  X(04)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-FILLER PICTURE  S9(5)                    CI0029
                          VALUE                ZERO                     CI0029
                          BINARY.                                       CI0029
            12            XW05-XSEGNM PICTURE  X(08)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0029
                          VALUE                ZERO                     CI0029
                          BINARY.                                       CI0029
            12            XW05-XSEGNB PICTURE  9(05)                    CI0029
                          VALUE                ZERO                     CI0029
                          BINARY.                                       CI0029
            12            XW05-XCOKEY PICTURE  X(70)                    CI0029
                          VALUE                SPACE.                   CI0029
            10            XW05-XW07.                                    CI0029
            11            XW05-XIOPCB.                                  CI0029
            12            XW05-XTERMI PICTURE  X(08)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-FILLER PICTURE  XX                       CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-XRC1   PICTURE  X(02)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-FILLER PICTURE  X(12)                    CI0029
                          VALUE                SPACE.                   CI0029
            12            XW05-XMODNM PICTURE  X(8)                     CI0029
                          VALUE                SPACE.                   CI0029
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0029
                          VALUE                ZERO.                    CI0029
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0029
                          VALUE                ZERO.                    CI0029
            10            XW05-XGU    PICTURE  X(4)                     CI0029
                          VALUE                'GU  '.                  CI0029
            10            XW05-XGHU   PICTURE  X(4)                     CI0029
                          VALUE                'GHU '.                  CI0029
            10            XW05-XGN    PICTURE  X(4)                     CI0029
                          VALUE                'GN  '.                  CI0029
            10            XW05-XGHN   PICTURE  X(4)                     CI0029
                          VALUE                'GHN '.                  CI0029
            10            XW05-XGNP   PICTURE  X(4)                     CI0029
                          VALUE                'GNP '.                  CI0029
            10            XW05-XGHNP  PICTURE  X(4)                     CI0029
                          VALUE                'GHNP'.                  CI0029
            10            XW05-XREPL  PICTURE  XXXX                     CI0029
                          VALUE                'REPL'.                  CI0029
            10            XW05-XISRT  PICTURE  X(4)                     CI0029
                          VALUE                'ISRT'.                  CI0029
            10            XW05-XDLET  PICTURE  X(4)                     CI0029
                          VALUE                'DLET'.                  CI0029
            10            XW05-XOPEN  PICTURE  X(4)                     CI0029
                          VALUE                'OPEN'.                  CI0029
            10            XW05-XCLSE  PICTURE  X(4)                     CI0029
                          VALUE                'CLSE'.                  CI0029
            10            XW05-XCHKP  PICTURE  X(4)                     CI0029
                          VALUE                'CHKP'.                  CI0029
            10            XW05-XXRST  PICTURE  X(4)                     CI0029
                          VALUE                'XRST'.                  CI0029
            10            XW05-XTERM  PICTURE  X(4)                     CI0029
                          VALUE                'TERM'.                  CI0029
            10            XW05-XNFPAC PICTURE  X(13)                    CI0029
                          VALUE                SPACE.                   CI0029
      *!WI pl=DL200                                                     ADU029
       01  DL01-KFPCB                                                   ADU029
                        PICTURE X(04)                                   CI0029
              VALUE 'PCB '.                                             ADU029
      ***  SAVE AREA FOR DL1 FUNCTION VALUE - USED FOR ERROR PROCESSING ADU029
       01  SV01-FUNC     PIC X(4).                                      ADU029
      ******************************************************************ADU029
      *** USED WHEN DYNAMICALLY CALLING PCB & UIB ERROR CHECKING MODULESADU029
      ***    (CI0008P - UIB ERROR CHECK    CI0009P - PCB ERROR CHECK)   ADU029
      ******************************************************************ADU029
      *!WI pl=DN100                                                     ADU029
       01  W-PASS-XPROGR                                                ADU029
                        PICTURE X(8).                                   CI0029
       01                 GQ00.                                         CI0029
            02            GQ01.                                         CI0029
            10            GQ01-GELL   PICTURE  9(4)                     CI0029
                          BINARY.                                       CI0029
            10            GQ01-GMISC.                                   CI0029
            11            GQ01-GS00.                                    CI0029
            12            GQ01-GT01.                                    CI0029
            13            GQ01-GQ01K.                                   CI0029
            14            GQ01-CANUMB PICTURE  X(27).                   CI0029
            14            GQ01-CAMCTR PICTURE  9(5).                    CI0029
            14            GQ01-GESQ2  PICTURE  99.                      CI0029
            12            GQ01-GT02                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-C199.                                    CI0029
            14            GQ01-CLID.                                    CI0029
            15            GQ01-CLIDO  PICTURE  9(3).                    CI0029
            15            GQ01-CLIDN.                                   CI0029
            16            GQ01-CLIDNP PICTURE  X(12).                   CI0029
            16            GQ01-CLIDND PICTURE  9(8).                    CI0029
            12            GQ01-GT03                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-C299.                                    CI0029
            14            GQ01-CTID.                                    CI0029
            15            GQ01-CTIDA  PICTURE  9(3).                    CI0029
            15            GQ01-CTIDN.                                   CI0029
            16            GQ01-CTIDNP PICTURE  X(13).                   CI0029
            16            GQ01-CTIDND PICTURE  9(11).                   CI0029
            12            GQ01-GT04                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-NPBN   PICTURE  X(20).                   CI0029
            12            GQ01-GT05                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-GR98.                                    CI0029
            14            GQ01-GRID.                                    CI0029
            15            GQ01-GRIDC  PICTURE  9(3).                    CI0029
            15            GQ01-GRIDN.                                   CI0029
            16            GQ01-GRIDNP PICTURE  99.                      CI0029
            16            GQ01-GRIDND PICTURE  9(8).                    CI0029
            12            GQ01-GT06                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-NTR    PICTURE  9(8).                    CI0029
            12            GQ01-GT07                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-NTRAC  PICTURE  9(14).                   CI0029
            12            GQ01-GT08                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-NSRAB  PICTURE  9(7).                    CI0029
            13            GQ01-GECKD  PICTURE  9.                       CI0029
            13            GQ01-NBLCK  PICTURE  9(5).                    CI0029
            13            GQ01-CTRID  PICTURE  X(4).                    CI0029
            12            GQ01-GT19                                     CI0029
                          REDEFINES            GQ01-GT01.               CI0029
            13            GQ01-GEOPD2 PICTURE  X(8).                    CI0029
            12            GQ01-CACKD  PICTURE  9.                       CI0029
            12            GQ01-CENTT  PICTURE  X.                       CI0029
            12            GQ01-CADATE PICTURE  X(8).                    CI0029
            12            GQ01-GETIM  PICTURE  S9(7)                    CI0029
                          COMPUTATIONAL-3.                              CI0029
            12            GQ01-GEOPID PICTURE  X(6).                    CI0029
            12            GQ01-CAUNIT PICTURE  X(4).                    CI0029
            12            GQ01-XTERMI PICTURE  X(08).                   CI0029
            12            GQ01-CAPPL  PICTURE  X(8).                    CI0029
            12            GQ01-CSYS   PICTURE  X(4).                    CI0029
            12            GQ01-NTRSU  PICTURE  999.                     CI0029
            12            GQ01-FILLER PICTURE  X(20).                   CI0029
            11            GQ01-XMISL  PICTURE  X(599).                  CI0029
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS VARIABLE PART OF MISC TRAN 00051    *
      **       - USED AS A SAVE AREA FOR WHAT WAS PASSED TO THIS MODULE*
      ******************************************************************
      *
      *!WF DSP=MT DSL=GS SEL=51 FOR=I LEV=1 PLT=GS
       01                 MT00.                                         CI0029
          05              MT00-00.                                      CI0029
            10            MT00-GT01.                                    CI0029
            11            MT00-GQ01K.                                   CI0029
            12            MT00-CANUMB PICTURE  X(27).                   CI0029
            12            MT00-CAMCTR PICTURE  9(5).                    CI0029
            12            MT00-GESQ2  PICTURE  99.                      CI0029
            10            MT00-GT02                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-C199.                                    CI0029
            12            MT00-CLID.                                    CI0029
            13            MT00-CLIDO  PICTURE  9(3).                    CI0029
            13            MT00-CLIDN.                                   CI0029
            14            MT00-CLIDNP PICTURE  X(12).                   CI0029
            14            MT00-CLIDND PICTURE  9(8).                    CI0029
            10            MT00-GT03                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-C299.                                    CI0029
            12            MT00-CTID.                                    CI0029
            13            MT00-CTIDA  PICTURE  9(3).                    CI0029
            13            MT00-CTIDN.                                   CI0029
            14            MT00-CTIDNP PICTURE  X(13).                   CI0029
            14            MT00-CTIDND PICTURE  9(11).                   CI0029
            10            MT00-GT04                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-NPBN   PICTURE  X(20).                   CI0029
            10            MT00-GT05                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-GR98.                                    CI0029
            12            MT00-GRID.                                    CI0029
            13            MT00-GRIDC  PICTURE  9(3).                    CI0029
            13            MT00-GRIDN.                                   CI0029
            14            MT00-GRIDNP PICTURE  99.                      CI0029
            14            MT00-GRIDND PICTURE  9(8).                    CI0029
            10            MT00-GT06                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-NTR    PICTURE  9(8).                    CI0029
            10            MT00-GT07                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-NTRAC  PICTURE  9(14).                   CI0029
            10            MT00-GT08                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-NSRAB  PICTURE  9(7).                    CI0029
            11            MT00-GECKD  PICTURE  9.                       CI0029
            11            MT00-NBLCK  PICTURE  9(5).                    CI0029
            11            MT00-CTRID  PICTURE  X(4).                    CI0029
            10            MT00-GT19                                     CI0029
                          REDEFINES            MT00-GT01.               CI0029
            11            MT00-GEOPD2 PICTURE  X(8).                    CI0029
            10            MT00-CACKD  PICTURE  9.                       CI0029
            10            MT00-CENTT  PICTURE  X.                       CI0029
            10            MT00-CADATE PICTURE  X(8).                    CI0029
            10            MT00-GETIM  PICTURE  S9(7)                    CI0029
                          COMPUTATIONAL-3.                              CI0029
            10            MT00-GEOPID PICTURE  X(6).                    CI0029
            10            MT00-CAUNIT PICTURE  X(4).                    CI0029
            10            MT00-XTERMI PICTURE  X(08).                   CI0029
            10            MT00-CAPPL  PICTURE  X(8).                    CI0029
            10            MT00-CSYS   PICTURE  X(4).                    CI0029
            10            MT00-NTRSU  PICTURE  999.                     CI0029
            10            MT00-FILLER PICTURE  X(20).                   CI0029
          05              MT00-SUITE.                                   CI0029
            15       FILLER         PICTURE  X(00011).                  CI0029
       01                 MT51  REDEFINES      MT00.                    CI0029
            10       FILLER         PICTURE  X(00101).                  CI0029
            10            MT51-CRETP  PICTURE  X.                       CI0029
            10            MT51-ICSWD  PICTURE  X(1).                    CI0029
            10            MT51-FILLER PICTURE  X(09).                   CI0029
      *
      *
      ******************************************************************
      **     SEGMENT THAT CONTAINS VARIABLE PART OF MISC TRAN 00051    *
      **       - USED AS A SAVE AREA FOR WHAT WAS FOUND ON DATABASE    *
      ******************************************************************
      *
      *!WF DSP=GS DSL=GS SEL=51 FOR=I LEV=1 PLT=GS
       01                 GS00.                                         CI0029
          05              GS00-00.                                      CI0029
            10            GS00-GT01.                                    CI0029
            11            GS00-GQ01K.                                   CI0029
            12            GS00-CANUMB PICTURE  X(27).                   CI0029
            12            GS00-CAMCTR PICTURE  9(5).                    CI0029
            12            GS00-GESQ2  PICTURE  99.                      CI0029
            10            GS00-GT02                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-C199.                                    CI0029
            12            GS00-CLID.                                    CI0029
            13            GS00-CLIDO  PICTURE  9(3).                    CI0029
            13            GS00-CLIDN.                                   CI0029
            14            GS00-CLIDNP PICTURE  X(12).                   CI0029
            14            GS00-CLIDND PICTURE  9(8).                    CI0029
            10            GS00-GT03                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-C299.                                    CI0029
            12            GS00-CTID.                                    CI0029
            13            GS00-CTIDA  PICTURE  9(3).                    CI0029
            13            GS00-CTIDN.                                   CI0029
            14            GS00-CTIDNP PICTURE  X(13).                   CI0029
            14            GS00-CTIDND PICTURE  9(11).                   CI0029
            10            GS00-GT04                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-NPBN   PICTURE  X(20).                   CI0029
            10            GS00-GT05                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-GR98.                                    CI0029
            12            GS00-GRID.                                    CI0029
            13            GS00-GRIDC  PICTURE  9(3).                    CI0029
            13            GS00-GRIDN.                                   CI0029
            14            GS00-GRIDNP PICTURE  99.                      CI0029
            14            GS00-GRIDND PICTURE  9(8).                    CI0029
            10            GS00-GT06                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-NTR    PICTURE  9(8).                    CI0029
            10            GS00-GT07                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-NTRAC  PICTURE  9(14).                   CI0029
            10            GS00-GT08                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-NSRAB  PICTURE  9(7).                    CI0029
            11            GS00-GECKD  PICTURE  9.                       CI0029
            11            GS00-NBLCK  PICTURE  9(5).                    CI0029
            11            GS00-CTRID  PICTURE  X(4).                    CI0029
            10            GS00-GT19                                     CI0029
                          REDEFINES            GS00-GT01.               CI0029
            11            GS00-GEOPD2 PICTURE  X(8).                    CI0029
            10            GS00-CACKD  PICTURE  9.                       CI0029
            10            GS00-CENTT  PICTURE  X.                       CI0029
            10            GS00-CADATE PICTURE  X(8).                    CI0029
            10            GS00-GETIM  PICTURE  S9(7)                    CI0029
                          COMPUTATIONAL-3.                              CI0029
            10            GS00-GEOPID PICTURE  X(6).                    CI0029
            10            GS00-CAUNIT PICTURE  X(4).                    CI0029
            10            GS00-XTERMI PICTURE  X(08).                   CI0029
            10            GS00-CAPPL  PICTURE  X(8).                    CI0029
            10            GS00-CSYS   PICTURE  X(4).                    CI0029
            10            GS00-NTRSU  PICTURE  999.                     CI0029
            10            GS00-FILLER PICTURE  X(20).                   CI0029
          05              GS00-SUITE.                                   CI0029
            15       FILLER         PICTURE  X(00011).                  CI0029
       01                 GS51  REDEFINES      GS00.                    CI0029
            10       FILLER         PICTURE  X(00101).                  CI0029
            10            GS51-CRETP  PICTURE  X.                       CI0029
            10            GS51-ICSWD  PICTURE  X(1).                    CI0029
            10            GS51-FILLER PICTURE  X(09).                   CI0029
      *
      *
      *
      *
       01   DEBUT-WSS.                                                  CI0029
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0029
            05   IK     PICTURE X.                                      CI0029
       01  CONSTANTES-PAC.                                              CI0029
           05  FILLER  PICTURE X(87)   VALUE                            CI0029
                     '6015 CAT09/08/14CI0029ADMIN   14:34:21CI0029P AMERCI0029
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0029
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0029
           05  NUGNA   PICTURE X(5).                                    CI0029
           05  APPLI   PICTURE X(3).                                    CI0029
           05  DATGN   PICTURE X(8).                                    CI0029
           05  PROGR   PICTURE X(6).                                    CI0029
           05  CODUTI  PICTURE X(8).                                    CI0029
           05  TIMGN   PICTURE X(8).                                    CI0029
           05  PROGE   PICTURE X(8).                                    CI0029
           05  COBASE  PICTURE X(4).                                    CI0029
           05  DATGNC  PICTURE X(10).                                   CI0029
           05  RELEAS  PICTURE X(7).                                    CI0029
           05  DATGE   PICTURE X(10).                                   CI0029
           05  DATSQ   PICTURE X(10).                                   CI0029
       01  DATCE.                                                       CI0029
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0029
         05  DATOR.                                                     CI0029
           10  DATOA  PICTURE XX.                                       CI0029
           10  DATOM  PICTURE XX.                                       CI0029
           10  DATOJ  PICTURE XX.                                       CI0029
       01   VARIABLES-CONDITIONNELLES.                                  CI0029
            05                  FT      PICTURE X VALUE '0'.            CI0029
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0029
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0029
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0029
            05       5-GQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0029
       01               S-GQ01-SSA.                                     CI0029
            10         S1-GQ01-SEGNAM PICTURE X(8)                      CI0029
                                      VALUE 'GQ01    '.                 CI0029
            10         S1-GQ01-CCOM   PICTURE X VALUE '*'.              CI0029
            10          S-GQ01-CCOD   PICTURE X(5)                      CI0029
                                      VALUE '-----'.                    CI0029
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0029
       01            S-GQU01-SSA.                                       CI0029
            13      S1-GQU01-SEGNAM PICTURE X(8)                        CI0029
                                      VALUE 'GQ01    '.                 CI0029
            13      S1-GQU01-CCOM   PICTURE X VALUE '*'.                CI0029
            13       S-GQU01-CCOD   PICTURE X(5)                        CI0029
                                      VALUE '-----'.                    CI0029
            13      S1-GQU01-FLDNAM PICTURE X(9)                        CI0029
                                      VALUE '(GQ01K'.                   CI0029
            13       S-GQU01-OPER  PICTURE XX VALUE ' ='.               CI0029
            13       S-GQU01-GQ01K.                                     CI0029
            14       S-GQU01-CANUMB   PICTURE  X(27).                   CI0029
            14       S-GQU01-CAMCTR   PICTURE  9(5).                    CI0029
            14       S-GQU01-GESQ2    PICTURE  99.                      CI0029
            13  FILLER   PICTURE X    VALUE ')'.                        CI0029
       01            S-GQ701-SSA.                                       CI0029
            14      S1-GQ701-SEGNAM PICTURE X(8)                        CI0029
                                      VALUE 'GQ01    '.                 CI0029
            14      S1-GQ701-CCOM   PICTURE X VALUE '*'.                CI0029
            14       S-GQ701-CCOD   PICTURE X(5)                        CI0029
                                      VALUE '-----'.                    CI0029
            14      S1-GQ701-FLDNAM PICTURE X(9)                        CI0029
                                      VALUE '(XCANUMB'.                 CI0029
            14       S-GQ701-OPER  PICTURE XX VALUE ' ='.               CI0029
            14       S-GQ701-CANUMB   PICTURE  X(27).                   CI0029
            14  FILLER   PICTURE X    VALUE ')'.                        CI0029
       01   ZONES-UTILISATEUR PICTURE X.                                CI0029
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
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0029
          05              PA00-SUITE.                                   CI0029
            15       FILLER         PICTURE  X(00106).                  CI0029
       01                 PA06  REDEFINES      PA00.                    CI0029
            10            PA06-XDBPCB.                                  CI0029
            11            PA06-XDBDNM PICTURE  X(08).                   CI0029
            11            PA06-XSEGLV PICTURE  X(02).                   CI0029
            11            PA06-XRC    PICTURE  X(02).                   CI0029
            11            PA06-XPROPT PICTURE  X(04).                   CI0029
            11            PA06-FILLER PICTURE  S9(5)                    CI0029
                          BINARY.                                       CI0029
            11            PA06-XSEGNM PICTURE  X(08).                   CI0029
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0029
                          BINARY.                                       CI0029
            11            PA06-XSEGNB PICTURE  9(05)                    CI0029
                          BINARY.                                       CI0029
            11            PA06-XCOKEY PICTURE  X(70).                   CI0029
      *                                                                 AMGQ01
      ******************************************************************AMGQ01
      **     SEGMENT THAT CONTAINS THE MISC TRAN INFORMATION PASSED    *AMGQ01
      **       TO AND FROM THE DATA UTILITY MISC TRAN PROCESS MODULES  *AMGQ01
      ******************************************************************AMGQ01
      *                                                                 AMGQ01
      *!WF DSP=TR DSL=GQ SEL=01 FOR=I LEV=1                             AMGQ01
       01                 TR00.                                         CI0029
          05              TR00-SUITE.                                   CI0029
            15       FILLER         PICTURE  X(00702).                  CI0029
       01                 TR01  REDEFINES      TR00.                    CI0029
            10            TR01-GELL   PICTURE  9(4)                     CI0029
                          BINARY.                                       CI0029
            10            TR01-GMISC.                                   CI0029
            11            TR01-GS00.                                    CI0029
            12            TR01-GT01.                                    CI0029
            13            TR01-GQ01K.                                   CI0029
            14            TR01-CANUMB PICTURE  X(27).                   CI0029
            14            TR01-CAMCTR PICTURE  9(5).                    CI0029
            14            TR01-GESQ2  PICTURE  99.                      CI0029
            12            TR01-GT02                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-C199.                                    CI0029
            14            TR01-CLID.                                    CI0029
            15            TR01-CLIDO  PICTURE  9(3).                    CI0029
            15            TR01-CLIDN.                                   CI0029
            16            TR01-CLIDNP PICTURE  X(12).                   CI0029
            16            TR01-CLIDND PICTURE  9(8).                    CI0029
            12            TR01-GT03                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-C299.                                    CI0029
            14            TR01-CTID.                                    CI0029
            15            TR01-CTIDA  PICTURE  9(3).                    CI0029
            15            TR01-CTIDN.                                   CI0029
            16            TR01-CTIDNP PICTURE  X(13).                   CI0029
            16            TR01-CTIDND PICTURE  9(11).                   CI0029
            12            TR01-GT04                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-NPBN   PICTURE  X(20).                   CI0029
            12            TR01-GT05                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-GR98.                                    CI0029
            14            TR01-GRID.                                    CI0029
            15            TR01-GRIDC  PICTURE  9(3).                    CI0029
            15            TR01-GRIDN.                                   CI0029
            16            TR01-GRIDNP PICTURE  99.                      CI0029
            16            TR01-GRIDND PICTURE  9(8).                    CI0029
            12            TR01-GT06                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-NTR    PICTURE  9(8).                    CI0029
            12            TR01-GT07                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-NTRAC  PICTURE  9(14).                   CI0029
            12            TR01-GT08                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-NSRAB  PICTURE  9(7).                    CI0029
            13            TR01-GECKD  PICTURE  9.                       CI0029
            13            TR01-NBLCK  PICTURE  9(5).                    CI0029
            13            TR01-CTRID  PICTURE  X(4).                    CI0029
            12            TR01-GT19                                     CI0029
                          REDEFINES            TR01-GT01.               CI0029
            13            TR01-GEOPD2 PICTURE  X(8).                    CI0029
            12            TR01-CACKD  PICTURE  9.                       CI0029
            12            TR01-CENTT  PICTURE  X.                       CI0029
            12            TR01-CADATE PICTURE  X(8).                    CI0029
            12            TR01-GETIM  PICTURE  S9(7)                    CI0029
                          COMPUTATIONAL-3.                              CI0029
            12            TR01-GEOPID PICTURE  X(6).                    CI0029
            12            TR01-CAUNIT PICTURE  X(4).                    CI0029
            12            TR01-XTERMI PICTURE  X(08).                   CI0029
            12            TR01-CAPPL  PICTURE  X(8).                    CI0029
            12            TR01-CSYS   PICTURE  X(4).                    CI0029
            12            TR01-NTRSU  PICTURE  999.                     CI0029
            12            TR01-FILLER PICTURE  X(20).                   CI0029
            11            TR01-XMISL  PICTURE  X(599).                  CI0029
      *                                                                 AMGQ01
      *                                                                 AMGQ01
      *                                                                 AMGQ01
      *                                                                 AMGQ01
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0029
          05              DE00-SUITE.                                   CI0029
            15       FILLER         PICTURE  X(00653).                  CI0029
       01                 DE10  REDEFINES      DE00.                    CI0029
            10            DE10-DU11.                                    CI0029
            11            DE10-XFONC  PICTURE  X(4).                    CI0029
            11            DE10-MPSBN  PICTURE  X(8).                    CI0029
            11            DE10-XDBDNM PICTURE  X(08).                   CI0029
            11            DE10-XSEGNM PICTURE  X(08).                   CI0029
            11            DE10-XRC    PICTURE  X(02).                   CI0029
            11            DE10-MSEG   PICTURE  X(08).                   CI0029
            11            DE10-XCOKEY PICTURE  X(70).                   CI0029
            11            DE10-CUIBR  PICTURE  X(01).                   CI0029
            11            DE10-CUIBA  PICTURE  X(01).                   CI0029
            11            DE10-IPBIK  PICTURE  X(1).                    CI0029
            10            DE10-DU03.                                    CI0029
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0029
                          COMPUTATIONAL-3.                              CI0029
            11            DE10-CMSSF  PICTURE  XX.                      CI0029
            11            DE10-DU09.                                    CI0029
            12            DE10-CMESA  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            12            DE10-CMESB  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            12            DE10-CMSST  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            12            DE10-QELLAA PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            12            DE10-TMESS4 PICTURE  X(512).                  CI0029
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
       01                 MS00.                                         CI0029
          05              MS00-SUITE.                                   CI0029
            15       FILLER         PICTURE  X(00542).                  CI0029
       01                 MS03  REDEFINES      MS00.                    CI0029
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0029
                          COMPUTATIONAL-3.                              CI0029
            10            MS03-CMSSF  PICTURE  XX.                      CI0029
            10            MS03-DU09.                                    CI0029
            11            MS03-CMESA  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            11            MS03-CMESB  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            11            MS03-CMSST  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            11            MS03-QELLAA PICTURE  S9(9)                    CI0029
                          BINARY.                                       CI0029
            11            MS03-TMESS4 PICTURE  X(512).                  CI0029
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                TR01
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0029
      *               *                                   *             CI0029
      *               *INITIALISATIONS                    *             CI0029
      *               *                                   *             CI0029
      *               *************************************.            CI0029
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
      *N02GS.    NOTE *SAVE VARIABLE PART OF MISC TRAN    *.
       F02GS.                                                           lv10
      *
      *********************************
      ** NEED CRETP FOR COMPARISON    *
      ** LOGIC LATER IN PROGRAM       *
      *********************************
      *
           MOVE        TR01-XMISL TO MT51.
       F02GS-FN. EXIT.
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *
      *********************************
      ** SET ADDRESSES FOR DATABASES  *
      *********************************
      *.
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0029
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0029
      *               *                                   *             CI0029
      *               *FIN DE TRAITEMENT                  *             CI0029
      *               *                                   *             CI0029
      *               *************************************.            CI0029
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0029
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
      *               *FORMAT AND WRITE MISC TRAN         *
      *               *                                   *
      *               *************************************.
       F40.           EXIT.                                             lv05
      *N40BG.    NOTE *POPULATE MISC TRAN                 *.
       F40BG.                                                           lv10
      *
      *********************************
      ** USE FORMATTED INFO (TR01)    *
      ** FROM CALLING MODULE.         *
      *********************************
      *
           MOVE        TR01 TO GQ01.
       F40BG-FN. EXIT.
      *N40CG.    NOTE *WRITE MISC TRAN TO DATABASE        *.
       F40CG.                                                           lv10
           PERFORM     F94TI THRU F94TI-FN.
       F40CG-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *PROCESS BASED ON RETURN CODE       *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      *
      *********************************
      *ALL CASES RETURN IMMEDIATELY TO
      *THE CALLING PROGRAM EXCEPT 'II'.
      *********************************
      *IF A MISC TRAN (WITH THIS KEY)
      *ALREADY EXISTS, THIS PROGRAM MAY
      *REPLACE IT WITH THE NEW MISC
      *TRAN INFO JUST RECEIVED, OR IT
      *MAY DO NOTHING TO THE ORIGINAL
      *MISC TRAN.
      *********************************
      *
      *N45BG.    NOTE *MISC TRAN ALREADY EXISTS           *.
       F45BG.    IF    XW05-XRC = 'II'                                  lv10
                 NEXT SENTENCE ELSE GO TO     F45BG-FN.
      *N45CG.    NOTE *NEW TRIGGER = SETUP OR REQUEST     *.
       F45CG.    IF    MT51-CRETP NOT = 'C'                             lv15
                 NEXT SENTENCE ELSE GO TO     F45CG-FN.
      * (NEW TRIGGER NOT = "CHANGE")
      *
      *N45DG.    NOTE *GET MISC TRAN FOR UPDATE           *.
       F45DG.                                                           lv20
      *
           MOVE        TR01-GQ01K TO S-GQU01-GQ01K
           PERFORM     F94TG THRU F94TG-FN.
       F45DG-FN. EXIT.
      *N45EG.    NOTE *FOUND MISC TRAN                    *.
       F45EG.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F45EG-FN.
      *
      *********************************
      ** MOVE VARIABLE PART TO WORK   *
      ** AREA                         *
      *********************************
      *
           MOVE        GQ01-XMISL TO GS51.
      *N45FG.    NOTE *REPLACE TRIGGER WITH PASS INFO     *.
       F45FG.         EXIT.                                             lv25
      *N45GG.    NOTE *........REPLACE AS "SETUP"         *.
       F45GG.    IF    ((GS51-CRETP = 'C') AND                          lv30
                       (MT51-CRETP = 'S'))
                 NEXT SENTENCE ELSE GO TO     F45GG-FN.
      *********************************
      *     DB = "CHANGE" AND
      *    NEW = "SETUP"
      *********************************
      *
           MOVE        TR01 TO GQ01
           PERFORM     F94TR THRU F94TR-FN.
       F45GG-FN. EXIT.
      *N45HG.    NOTE *........REPLACE AS "REQUEST"       *.
       F45HG.    IF    ((GS51-CRETP NOT = 'R')                          lv30
                       AND (MT51-CRETP = 'R'))
                 NEXT SENTENCE ELSE GO TO     F45HG-FN.
      *********************************
      * DB NOT = "REQUEST" AND
      *    NEW = "REQUEST"
      *********************************
      *
           MOVE        TR01 TO GQ01
           PERFORM     F94TR THRU F94TR-FN.
       F45HG-FN. EXIT.
       F45FG-FN. EXIT.
       F45EG-FN. EXIT.
       F45CG-FN. EXIT.
       F45BG-FN. EXIT.
       F45-FN.   EXIT.
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
      *N94TG.    NOTE *CALL GHU ON GQ01                   *.            ADU026
       F94TG.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PA06 GQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94TG-FN. EXIT.
      *N94TI.    NOTE *CALL ISRT ON GQ01                  *.            ADU026
       F94TI.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PA06 GQ01                                                    ADU026
           S-GQ01-SSA                                                   ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94TI-FN. EXIT.
      *N94TR.    NOTE *CALL REPL ON GQ01                  *.            ADU026
       F94TR.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PA06 GQ01                                                    ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94TR-FN. EXIT.
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
