       IDENTIFICATION DIVISION.                                         CI0061
       PROGRAM-ID.  CI0061P.                                            CI0061
      *AUTHOR.         CLIENT ARR DETAIL READ MODULE.                   CI0061
      *DATE-COMPILED.   09/08/14.                                       CI0061
       ENVIRONMENT DIVISION.                                            CI0061
       CONFIGURATION SECTION.                                           CI0061
       SOURCE-COMPUTER. IBM-370.                                        CI0061
       OBJECT-COMPUTER. IBM-370.                                        CI0061
       DATA DIVISION.                                                   CI0061
       WORKING-STORAGE SECTION.                                         CI0061
      ******************************************************************
      **  MISC FIELDS AND SWITCHES                                     *
      ******************************************************************
      **
       01  W-SWITCHES.
           05  CX03-CF         PIC X(01).
           05  CX06-CF         PIC X(01).
           05  CX12-CF         PIC X(01).
           05  MORE-CX06S      PIC X(01).
               88  NO-MORE-CX06S   VALUE 'N'.
           05  MORE-CX12S      PIC X(01).
               88  NO-MORE-CX12S   VALUE 'N'.
           05  FIRST-TIME-SW   PIC X(01).
       01  7-CT01-DIRAC.
           05  7-CT01-DIRACM   PIC 99.
           05  7-CT01-DIRACD   PIC 99.
      *!WI
       01  7-CT01-PRSCD
                        PICTURE X(9).                                   CI0061
      ******************************************************************
      **  PASS AREA WORK FIELDS.                                       *
      ******************************************************************
      **
       01  MISCELLANEOUS-FIELDS.
           05  SUB            PIC S9(03)  COMP-3.
           05  SUB-MAX        PIC S9(03)  VALUE +100.
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
       01                 CT01.                                         CI0061
            10            CT01-CT01K.                                   CI0061
            11            CT01-C299.                                    CI0061
            12            CT01-CTID.                                    CI0061
            13            CT01-CTIDA  PICTURE  9(3).                    CI0061
            13            CT01-CTIDN.                                   CI0061
            14            CT01-CTIDNP PICTURE  X(13).                   CI0061
            14            CT01-CTIDND PICTURE  9(11).                   CI0061
            10            CT01-GECKD  PICTURE  9.                       CI0061
            10            CT01-GEMDA  PICTURE  9(8).                    CI0061
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0061
                          BINARY.                                       CI0061
            10            CT01-GECUC  PICTURE  99.                      CI0061
            10            CT01-CTAUL  PICTURE  9(3).                    CI0061
            10            CT01-DIRAC  PICTURE  9(4).                    CI0061
            10            CT01-CTCCI  PICTURE  X.                       CI0061
            10            CT01-CTCUS  PICTURE  999.                     CI0061
            10            CT01-CTEFD  PICTURE  9(8).                    CI0061
            10            CT01-CTIAD  PICTURE  9(8).                    CI0061
            10            CT01-CLCUS  PICTURE  99.                      CI0061
            10            CT01-CAMMB  PICTURE  X(3).                    CI0061
            10            CT01-CKPMM  PICTURE  X.                       CI0061
            10            CT01-CTLAD  PICTURE  9(8).                    CI0061
            10            CT01-IPERS  PICTURE  X.                       CI0061
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0061
                          COMPUTATIONAL-3.                              CI0061
            10            CT01-CTLAT  PICTURE  9(8).                    CI0061
            10            CT01-CTLATC PICTURE  9(6).                    CI0061
            10            CT01-IMEGA  PICTURE  X.                       CI0061
            10            CT01-DIRAB  PICTURE  9(8).                    CI0061
            10            CT01-COLRQ  PICTURE  X.                       CI0061
            10            CT01-ZDA04  PICTURE  X(4).                    CI0061
            10            CT01-CTLPD  PICTURE  9(8).                    CI0061
            10            CT01-CIRASP PICTURE  9.                       CI0061
            10            CT01-CIRATP PICTURE  99.                      CI0061
            10            CT01-DRTHC  PICTURE  9(8).                    CI0061
            10            CT01-CPPTC  PICTURE  X.                       CI0061
            10            CT01-ZDA06  PICTURE  X(6).                    CI0061
            10            CT01-CTACD  PICTURE  9(8).                    CI0061
            10            CT01-CTNLI  PICTURE  X.                       CI0061
            10            CT01-CTRHO  PICTURE  9(8).                    CI0061
            10            CT01-CTSGD  PICTURE  9(8).                    CI0061
            10            CT01-CPATP  PICTURE  X(1).                    CI0061
            10            CT01-IRSTA  PICTURE  X.                       CI0061
            10            CT01-CTSTA  PICTURE  99.                      CI0061
            10            CT01-CTSSC  PICTURE  99.                      CI0061
            10            CT01-PRLIN  PICTURE  9(3).                    CI0061
            10            CT01-PRCOD  PICTURE  9(5).                    CI0061
            10            CT01-PRSCD  PICTURE  X(9).                    CI0061
            10            CT01-CTLNI  PICTURE  X.                       CI0061
            10            CT01-AYSIDA PICTURE  9(3).                    CI0061
            10            CT01-AYSID  PICTURE  9(5).                    CI0061
            10            CT01-CTBMC  PICTURE  99.                      CI0061
            10            CT01-CINAR  PICTURE  99.                      CI0061
            10            CT01-CPHTR  PICTURE  X.                       CI0061
            10            CT01-CDSTR  PICTURE  XX.                      CI0061
            10            CT01-CQACT  PICTURE  999.                     CI0061
            10            CT01-CIRAS  PICTURE  999.                     CI0061
            10            CT01-CIRAT  PICTURE  999.                     CI0061
            10            CT01-CLRAY  PICTURE  9(5).                    CI0061
            10            CT01-CATTP  PICTURE  X.                       CI0061
       01                 CX01.                                         CI0061
            10            CX01-CX01K.                                   CI0061
            11            CX01-C199.                                    CI0061
            12            CX01-CLID.                                    CI0061
            13            CX01-CLIDO  PICTURE  9(3).                    CI0061
            13            CX01-CLIDN.                                   CI0061
            14            CX01-CLIDNP PICTURE  X(12).                   CI0061
            14            CX01-CLIDND PICTURE  9(8).                    CI0061
            10            CX01-GEMDA  PICTURE  9(8).                    CI0061
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0061
                          BINARY.                                       CI0061
            10            CX01-FILLER PICTURE  X(5).                    CI0061
       01                 CX03.                                         CI0061
            10            CX03-GELL   PICTURE  9(4)                     CI0061
                          BINARY.                                       CI0061
            10            CX03-CY00.                                    CI0061
            11            CX03-CX03K.                                   CI0061
            12            CX03-CARTY  PICTURE  99.                      CI0061
            12            CX03-NARRS  PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX03-CARST  PICTURE  99.                      CI0061
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX03-CPMTG  PICTURE  99.                      CI0061
            11            CX03-GRCRNG PICTURE  9(3).                    CI0061
            11            CX03-DEXDT  PICTURE  9(8).                    CI0061
            11            CX03-DASUP  PICTURE  9(8).                    CI0061
            11            CX03-CSTEC  PICTURE  X(3).                    CI0061
            11            CX03-FILLER PICTURE  X(17).                   CI0061
            11            CX03-CY50.                                    CI0061
            12            CX03-NARID  PICTURE  X(30).                   CI0061
            11            CX03-CY51                                     CI0061
                          REDEFINES            CX03-CY50.               CI0061
            12            CX03-NDIDN  PICTURE  9(12).                   CI0061
            12            CX03-FILLER PICTURE  X(18).                   CI0061
            11            CX03-CY52                                     CI0061
                          REDEFINES            CX03-CY50.               CI0061
            12            CX03-NAIDC  PICTURE  9(12).                   CI0061
            12            CX03-FILLER PICTURE  X(18).                   CI0061
            11            CX03-CY53                                     CI0061
                          REDEFINES            CX03-CY50.               CI0061
            12            CX03-NAMEXB PICTURE  9(15).                   CI0061
            12            CX03-FILLER PICTURE  X(15).                   CI0061
            10            CX03-CY99.                                    CI0061
            11            CX03-FILLER PICTURE  X(109).                  CI0061
            10            CX03-CY01                                     CI0061
                          REDEFINES            CX03-CY99.               CI0061
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX03-ICPCI  PICTURE  X.                       CI0061
            11            CX03-CLUPD  PICTURE  9(3).                    CI0061
            11            CX03-DLAUP  PICTURE  9(8).                    CI0061
            11            CX03-CWRC   PICTURE  99.                      CI0061
            11            CX03-CHCR   PICTURE  99.                      CI0061
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0061
            11            CX03-GEAUN  PICTURE  9(5).                    CI0061
            11            CX03-DPCHD  PICTURE  9(8).                    CI0061
            11            CX03-DLRCHK PICTURE  9(8).                    CI0061
            11            CX03-QTRCHK PICTURE  9(2).                    CI0061
            11            CX03-DNPMT  PICTURE  9(8).                    CI0061
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0061
                          COMPUTATIONAL-3.                              CI0061
            10            CX03-CY02                                     CI0061
                          REDEFINES            CX03-CY99.               CI0061
            11            CX03-QSIRQ  PICTURE  99.                      CI0061
            11            CX03-QDRMN  PICTURE  9(2)                     CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX03-DDPRE  PICTURE  9(8).                    CI0061
            11            CX03-DDSHP  PICTURE  9(8).                    CI0061
            11            CX03-NDRFTB PICTURE  9(5).                    CI0061
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0061
            11            CX03-DDSHPA PICTURE  9(8).                    CI0061
            11            CX03-NDRFTF PICTURE  9(5).                    CI0061
            11            CX03-QDIPBK PICTURE  9(3).                    CI0061
            11            CX03-CREOR  PICTURE  X(1).                    CI0061
            11            CX03-CREOR1 PICTURE  X(1).                    CI0061
            11            CX03-DDASC  PICTURE  9(8).                    CI0061
            11            CX03-FILLER PICTURE  X(7).                    CI0061
            10            CX03-CY03                                     CI0061
                          REDEFINES            CX03-CY99.               CI0061
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0061
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0061
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0061
            11            CX03-DOPDA  PICTURE  99.                      CI0061
            11            CX03-CPMTF  PICTURE  99.                      CI0061
            11            CX03-CIRMO  PICTURE  X(12).                   CI0061
            11            CX03-CPALL  PICTURE  X(1).                    CI0061
            11            CX03-CCOLM  PICTURE  9(2).                    CI0061
            11            CX03-CBLTP  PICTURE  X(1).                    CI0061
            11            CX03-CASUB  PICTURE  9(2).                    CI0061
            11            CX03-CBLFM  PICTURE  9(2).                    CI0061
            11            CX03-IBILS  PICTURE  X.                       CI0061
            11            CX03-IPAOS  PICTURE  X.                       CI0061
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0061
            11            CX03-DLBPD  PICTURE  9(8).                    CI0061
            11            CX03-DNBPD  PICTURE  9(8).                    CI0061
            11            CX03-DODBD  PICTURE  9(8).                    CI0061
            11            CX03-CPSRE  PICTURE  99.                      CI0061
            11            CX03-ISPHN  PICTURE  X.                       CI0061
            11            CX03-TCARR  PICTURE  X(6).                    CI0061
            11            CX03-CBKPT  PICTURE  9(2).                    CI0061
            11            CX03-IECNT  PICTURE  X.                       CI0061
            11            CX03-ICONV  PICTURE  X(1).                    CI0061
            11            CX03-FILLER PICTURE  X(4).                    CI0061
            10            CX03-CY04                                     CI0061
                          REDEFINES            CX03-CY99.               CI0061
            11            CX03-CCARD  PICTURE  X(02).                   CI0061
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0061
            11            CX03-IREMT  PICTURE  X(01).                   CI0061
            11            CX03-ISBILA PICTURE  X.                       CI0061
            11            CX03-DLBPDA PICTURE  9(8).                    CI0061
            11            CX03-DNBPDA.                                  CI0061
            12            CX03-DNCYM  PICTURE  9(6).                    CI0061
            12            CX03-CEDTD  PICTURE  9(2).                    CI0061
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX03-DREMT  PICTURE  9(8).                    CI0061
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0061
            11            CX03-CWRC2  PICTURE  99.                      CI0061
            11            CX03-CHCR2  PICTURE  99.                      CI0061
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0061
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0061
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0061
       01                 CX06.                                         CI0061
            10            CX06-CX06K.                                   CI0061
            11            CX06-C299.                                    CI0061
            12            CX06-CTID.                                    CI0061
            13            CX06-CTIDA  PICTURE  9(3).                    CI0061
            13            CX06-CTIDN.                                   CI0061
            14            CX06-CTIDNP PICTURE  X(13).                   CI0061
            14            CX06-CTIDND PICTURE  9(11).                   CI0061
            10            CX06-NPECK  PICTURE  9(02).                   CI0061
            10            CX06-FILLER PICTURE  X.                       CI0061
       01                 CX12.                                         CI0061
            10            CX12-CX12K.                                   CI0061
            11            CX12-CPMTC  PICTURE  99.                      CI0061
            11            CX12-NAPDS  PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            CX12-GESTD  PICTURE  9(8).                    CI0061
            10            CX12-GEEND  PICTURE  9(8).                    CI0061
            10            CX12-CIRMO  PICTURE  X(12).                   CI0061
            10            CX12-CDEST  PICTURE  99.                      CI0061
            10            CX12-APMTL  PICTURE  S9(9)V99                 CI0061
                          COMPUTATIONAL-3.                              CI0061
            10            CX12-DNPMT  PICTURE  9(8).                    CI0061
            10            CX12-NIRACM PICTURE  9(2).                    CI0061
            10            CX12-CPMTF  PICTURE  99.                      CI0061
            10            CX12-IPODM  PICTURE  X.                       CI0061
            10            CX12-CLUPD  PICTURE  9(3).                    CI0061
            10            CX12-DLAUP  PICTURE  9(8).                    CI0061
            10            CX12-CWRC   PICTURE  99.                      CI0061
            10            CX12-CHCR   PICTURE  99.                      CI0061
            10            CX12-GEOPD2 PICTURE  X(8).                    CI0061
            10            CX12-GEAUN  PICTURE  9(5).                    CI0061
            10            CX12-DPCHD  PICTURE  9(8).                    CI0061
            10            CX12-DNEXE  PICTURE  9(8).                    CI0061
            10            CX12-CCSMQ  PICTURE  X.                       CI0061
            10            CX12-GCUSPZ PICTURE  X(12).                   CI0061
            10            CX12-CORTY  PICTURE  X.                       CI0061
            10            CX12-CNAVR  PICTURE  X(1).                    CI0061
            10            CX12-DELOI3 PICTURE  9(6).                    CI0061
            10            CX12-ALOIDD PICTURE  9(9)V99                  CI0061
                          COMPUTATIONAL-3.                              CI0061
            10            CX12-FILLER PICTURE  X(5).                    CI0061
      ******************************************************************ADU029
      ***  STORAGE AREAS FOR DL1                                        ADU029
      ******************************************************************ADU029
      ***  DL1 FUNCTIONS                                                ADU029
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU029
       01                 XW05.                                         CI0061
            10            XW05-XW06.                                    CI0061
            11            XW05-XDBPCB.                                  CI0061
            12            XW05-XDBDNM PICTURE  X(08)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-XSEGLV PICTURE  X(02)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-XRC    PICTURE  X(02)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-XPROPT PICTURE  X(04)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-FILLER PICTURE  S9(5)                    CI0061
                          VALUE                ZERO                     CI0061
                          BINARY.                                       CI0061
            12            XW05-XSEGNM PICTURE  X(08)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0061
                          VALUE                ZERO                     CI0061
                          BINARY.                                       CI0061
            12            XW05-XSEGNB PICTURE  9(05)                    CI0061
                          VALUE                ZERO                     CI0061
                          BINARY.                                       CI0061
            12            XW05-XCOKEY PICTURE  X(70)                    CI0061
                          VALUE                SPACE.                   CI0061
            10            XW05-XW07.                                    CI0061
            11            XW05-XIOPCB.                                  CI0061
            12            XW05-XTERMI PICTURE  X(08)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-FILLER PICTURE  XX                       CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-XRC1   PICTURE  X(02)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-FILLER PICTURE  X(12)                    CI0061
                          VALUE                SPACE.                   CI0061
            12            XW05-XMODNM PICTURE  X(8)                     CI0061
                          VALUE                SPACE.                   CI0061
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0061
                          VALUE                ZERO.                    CI0061
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0061
                          VALUE                ZERO.                    CI0061
            10            XW05-XGU    PICTURE  X(4)                     CI0061
                          VALUE                'GU  '.                  CI0061
            10            XW05-XGHU   PICTURE  X(4)                     CI0061
                          VALUE                'GHU '.                  CI0061
            10            XW05-XGN    PICTURE  X(4)                     CI0061
                          VALUE                'GN  '.                  CI0061
            10            XW05-XGHN   PICTURE  X(4)                     CI0061
                          VALUE                'GHN '.                  CI0061
            10            XW05-XGNP   PICTURE  X(4)                     CI0061
                          VALUE                'GNP '.                  CI0061
            10            XW05-XGHNP  PICTURE  X(4)                     CI0061
                          VALUE                'GHNP'.                  CI0061
            10            XW05-XREPL  PICTURE  XXXX                     CI0061
                          VALUE                'REPL'.                  CI0061
            10            XW05-XISRT  PICTURE  X(4)                     CI0061
                          VALUE                'ISRT'.                  CI0061
            10            XW05-XDLET  PICTURE  X(4)                     CI0061
                          VALUE                'DLET'.                  CI0061
            10            XW05-XOPEN  PICTURE  X(4)                     CI0061
                          VALUE                'OPEN'.                  CI0061
            10            XW05-XCLSE  PICTURE  X(4)                     CI0061
                          VALUE                'CLSE'.                  CI0061
            10            XW05-XCHKP  PICTURE  X(4)                     CI0061
                          VALUE                'CHKP'.                  CI0061
            10            XW05-XXRST  PICTURE  X(4)                     CI0061
                          VALUE                'XRST'.                  CI0061
            10            XW05-XTERM  PICTURE  X(4)                     CI0061
                          VALUE                'TERM'.                  CI0061
            10            XW05-XNFPAC PICTURE  X(13)                    CI0061
                          VALUE                SPACE.                   CI0061
      *!WI pl=DL200                                                     ADU029
       01  DL01-KFPCB                                                   ADU029
                        PICTURE X(04)                                   CI0061
              VALUE 'PCB '.                                             ADU029
      ***  SAVE AREA FOR DL1 FUNCTION VALUE - USED FOR ERROR PROCESSING ADU029
       01  SV01-FUNC     PIC X(4).                                      ADU029
      ******************************************************************ADU029
      *** USED WHEN DYNAMICALLY CALLING PCB & UIB ERROR CHECKING MODULESADU029
      ***    (CI0008P - UIB ERROR CHECK    CI0009P - PCB ERROR CHECK)   ADU029
      ******************************************************************ADU029
      *!WI pl=DN100                                                     ADU029
       01  W-PASS-XPROGR                                                ADU029
                        PICTURE X(8).                                   CI0061
      ******************************************************************ADUTAB
      **              TABLE TG17 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TG17-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TG DSL=TK SEL=17 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TG17.                                                CI0061
           04    G-TG17-PARAM.                                          CI0061
             10  G-TG17-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0061
                        VALUE      +055.                                CI0061
             10  G-TG17-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0061
                        VALUE      +001.                                CI0061
             10  G-TG17-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0061
                        VALUE      +009.                                CI0061
             10  G-TG17-NUAPP  PICTURE 99                               CI0061
                        VALUE       0.                                  CI0061
             10  G-TG17-NUTAB  PICTURE X(6)                             CI0061
                        VALUE 'TK0017'.                                 CI0061
             10  G-TG17-TABFO  PICTURE XX                 VALUE SPACE.  CI0061
             10  G-TG17-TABCR  PICTURE XX                 VALUE SPACE.  CI0061
             10  G-TG17-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0061
             10  G-TG17-NUSSC  PICTURE X  VALUE   ' '.                  CI0061
             10  G-TG17-NUSSY  PICTURE X                  VALUE SPACE.  CI0061
             10  G-TG17-TRANID PICTURE X(4)               VALUE SPACE.  CI0061
             10  G-TG17-FILSYS.                                         CI0061
             15  G-TG17-USERC  PICTURE X(6)               VALUE SPACE.  CI0061
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0061
           04             TG17.                                         CI0061
            10            TG17-PRSCD  PICTURE  X(9)                     CI0061
                          VALUE                SPACE.                   CI0061
            10            TG17-MSP03A PICTURE  X(1)                     CI0061
                          VALUE                SPACE.                   CI0061
            10            TG17-ZDA45  PICTURE  X(45)                    CI0061
                          VALUE                SPACE.                   CI0061
      **                                                                ADUTAB
       01        G-TK17.                                                CI0061
           04    G-TK17-PARAM.                                          CI0061
             10  G-TK17-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0061
                        VALUE      +055.                                CI0061
             10  G-TK17-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0061
                        VALUE      +001.                                CI0061
             10  G-TK17-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0061
                        VALUE      +009.                                CI0061
             10  G-TK17-NUAPP  PICTURE 99                               CI0061
                        VALUE       0.                                  CI0061
             10  G-TK17-NUTAB  PICTURE X(6)                             CI0061
                        VALUE 'TK0017'.                                 CI0061
             10  G-TK17-TABFO  PICTURE XX                 VALUE SPACE.  CI0061
             10  G-TK17-TABCR  PICTURE XX                 VALUE SPACE.  CI0061
             10  G-TK17-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0061
             10  G-TK17-NUSSC  PICTURE X  VALUE   ' '.                  CI0061
             10  G-TK17-NUSSY  PICTURE X                  VALUE SPACE.  CI0061
             10  G-TK17-TRANID PICTURE X(4)               VALUE SPACE.  CI0061
             10  G-TK17-FILSYS.                                         CI0061
             15  G-TK17-USERC  PICTURE X(6)               VALUE SPACE.  CI0061
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0061
           04             TK17.                                         CI0061
            10            TK17-PRSCD  PICTURE  X(9)                     CI0061
                          VALUE                SPACE.                   CI0061
            10            TK17-MSP03A PICTURE  X(1)                     CI0061
                          VALUE                SPACE.                   CI0061
            10            TK17-ZDA45  PICTURE  X(45)                    CI0061
                          VALUE                SPACE.                   CI0061
       01   DEBUT-WSS.                                                  CI0061
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0061
            05   IK     PICTURE X.                                      CI0061
       01  CONSTANTES-PAC.                                              CI0061
           05  FILLER  PICTURE X(87)   VALUE                            CI0061
                     '6015 CAT09/08/14CI0061ADMIN   14:34:23CI0061P AMERCI0061
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0061
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0061
           05  NUGNA   PICTURE X(5).                                    CI0061
           05  APPLI   PICTURE X(3).                                    CI0061
           05  DATGN   PICTURE X(8).                                    CI0061
           05  PROGR   PICTURE X(6).                                    CI0061
           05  CODUTI  PICTURE X(8).                                    CI0061
           05  TIMGN   PICTURE X(8).                                    CI0061
           05  PROGE   PICTURE X(8).                                    CI0061
           05  COBASE  PICTURE X(4).                                    CI0061
           05  DATGNC  PICTURE X(10).                                   CI0061
           05  RELEAS  PICTURE X(7).                                    CI0061
           05  DATGE   PICTURE X(10).                                   CI0061
           05  DATSQ   PICTURE X(10).                                   CI0061
       01  DATCE.                                                       CI0061
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0061
         05  DATOR.                                                     CI0061
           10  DATOA  PICTURE XX.                                       CI0061
           10  DATOM  PICTURE XX.                                       CI0061
           10  DATOJ  PICTURE XX.                                       CI0061
       01   VARIABLES-CONDITIONNELLES.                                  CI0061
            05                  FT      PICTURE X VALUE '0'.            CI0061
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0061
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0061
            05           J02DDR PICTURE S9(4) VALUE  ZERO.
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0061
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0061
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0061
            05       5-TK00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0061
       01               S-CT01-SSA.                                     CI0061
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0061
                                      VALUE 'CT01    '.                 CI0061
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0061
            10          S-CT01-CCOD   PICTURE X(5)                      CI0061
                                      VALUE '-----'.                    CI0061
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0061
       01            S-CTU01-SSA.                                       CI0061
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CT01    '.                 CI0061
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0061
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CT01K'.                   CI0061
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0061
            10       S-CTU01-CT01K.                                     CI0061
            11       S-CTU01-C299.                                      CI0061
            12       S-CTU01-CTID.                                      CI0061
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0061
            13       S-CTU01-CTIDN.                                     CI0061
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0061
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0061
            10  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01               S-CX01-SSA.                                     CI0061
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0061
                                      VALUE 'CX01    '.                 CI0061
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0061
            10          S-CX01-CCOD   PICTURE X(5)                      CI0061
                                      VALUE '-----'.                    CI0061
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0061
       01            S-CXU01-SSA.                                       CI0061
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX01    '.                 CI0061
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0061
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CX01K'.                   CI0061
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0061
            10       S-CXU01-CX01K.                                     CI0061
            11       S-CXU01-C199.                                      CI0061
            12       S-CXU01-CLID.                                      CI0061
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0061
            13       S-CXU01-CLIDN.                                     CI0061
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0061
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0061
            10  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01               S-CX03-SSA.                                     CI0061
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0061
                                      VALUE 'CX03    '.                 CI0061
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0061
            10          S-CX03-CCOD   PICTURE X(5)                      CI0061
                                      VALUE '-----'.                    CI0061
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0061
       01            S-CXA03-SSA.                                       CI0061
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0061
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CARTY'.                   CI0061
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0061
            12       S-CXA03-CARTY    PICTURE  99.                      CI0061
            12  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXB03-SSA.                                       CI0061
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0061
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(NARRS'.                   CI0061
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0061
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            12  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXC03-SSA.                                       CI0061
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0061
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CPMTG'.                   CI0061
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0061
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0061
            11  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXD03-SSA.                                       CI0061
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0061
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(GRCRNG'.                  CI0061
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0061
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0061
            11  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXE03-SSA.                                       CI0061
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0061
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(DEXDT'.                   CI0061
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0061
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0061
            11  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXF03-SSA.                                       CI0061
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0061
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CY50'.                    CI0061
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0061
            11       S-CXF03-CY50.                                      CI0061
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0061
            11  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXG03-SSA.                                       CI0061
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0061
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(NBASQ'.                   CI0061
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0061
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXH03-SSA.                                       CI0061
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0061
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(NARID'.                   CI0061
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0061
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0061
            12  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXU03-SSA.                                       CI0061
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX03    '.                 CI0061
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0061
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CX03K'.                   CI0061
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0061
            11       S-CXU03-CX03K.                                     CI0061
            12       S-CXU03-CARTY    PICTURE  99.                      CI0061
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01               S-CX06-SSA.                                     CI0061
            10         S1-CX06-SEGNAM PICTURE X(8)                      CI0061
                                      VALUE 'CX06    '.                 CI0061
            10         S1-CX06-CCOM   PICTURE X VALUE '*'.              CI0061
            10          S-CX06-CCOD   PICTURE X(5)                      CI0061
                                      VALUE '-----'.                    CI0061
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0061
       01            S-CXU06-SSA.                                       CI0061
            10      S1-CXU06-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX06    '.                 CI0061
            10      S1-CXU06-CCOM   PICTURE X VALUE '*'.                CI0061
            10       S-CXU06-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            10      S1-CXU06-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CX06K'.                   CI0061
            10       S-CXU06-OPER  PICTURE XX VALUE ' ='.               CI0061
            10       S-CXU06-CX06K.                                     CI0061
            11       S-CXU06-C299.                                      CI0061
            12       S-CXU06-CTID.                                      CI0061
            13       S-CXU06-CTIDA    PICTURE  9(3).                    CI0061
            13       S-CXU06-CTIDN.                                     CI0061
            14       S-CXU06-CTIDNP   PICTURE  X(13).                   CI0061
            14       S-CXU06-CTIDND   PICTURE  9(11).                   CI0061
            10  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01               S-CX12-SSA.                                     CI0061
            10         S1-CX12-SEGNAM PICTURE X(8)                      CI0061
                                      VALUE 'CX12    '.                 CI0061
            10         S1-CX12-CCOM   PICTURE X VALUE '*'.              CI0061
            10          S-CX12-CCOD   PICTURE X(5)                      CI0061
                                      VALUE '-----'.                    CI0061
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0061
       01            S-CXA12-SSA.                                       CI0061
            10      S1-CXA12-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX12    '.                 CI0061
            10      S1-CXA12-CCOM   PICTURE X VALUE '*'.                CI0061
            10       S-CXA12-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            10      S1-CXA12-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CDEST'.                   CI0061
            10       S-CXA12-OPER  PICTURE XX VALUE ' ='.               CI0061
            10       S-CXA12-CDEST    PICTURE  99.                      CI0061
            10  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXB12-SSA.                                       CI0061
            10      S1-CXB12-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX12    '.                 CI0061
            10      S1-CXB12-CCOM   PICTURE X VALUE '*'.                CI0061
            10       S-CXB12-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            10      S1-CXB12-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(DNPMT'.                   CI0061
            10       S-CXB12-OPER  PICTURE XX VALUE ' ='.               CI0061
            10       S-CXB12-DNPMT    PICTURE  9(8).                    CI0061
            10  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXC12-SSA.                                       CI0061
            11      S1-CXC12-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX12    '.                 CI0061
            11      S1-CXC12-CCOM   PICTURE X VALUE '*'.                CI0061
            11       S-CXC12-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            11      S1-CXC12-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(NAPDS'.                   CI0061
            11       S-CXC12-OPER  PICTURE XX VALUE ' ='.               CI0061
            11       S-CXC12-NAPDS    PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01            S-CXU12-SSA.                                       CI0061
            10      S1-CXU12-SEGNAM PICTURE X(8)                        CI0061
                                      VALUE 'CX12    '.                 CI0061
            10      S1-CXU12-CCOM   PICTURE X VALUE '*'.                CI0061
            10       S-CXU12-CCOD   PICTURE X(5)                        CI0061
                                      VALUE '-----'.                    CI0061
            10      S1-CXU12-FLDNAM PICTURE X(9)                        CI0061
                                      VALUE '(CX12K'.                   CI0061
            10       S-CXU12-OPER  PICTURE XX VALUE ' ='.               CI0061
            10       S-CXU12-CX12K.                                     CI0061
            11       S-CXU12-CPMTC    PICTURE  99.                      CI0061
            11       S-CXU12-NAPDS    PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11       S-CXU12-GESTD    PICTURE  9(8).                    CI0061
            10  FILLER   PICTURE X    VALUE ')'.                        CI0061
       01   ZONES-UTILISATEUR PICTURE X.                                CI0061
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
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0061
          05              PA00-SUITE.                                   CI0061
            15       FILLER         PICTURE  X(00106).                  CI0061
       01                 PA06  REDEFINES      PA00.                    CI0061
            10            PA06-XDBPCB.                                  CI0061
            11            PA06-XDBDNM PICTURE  X(08).                   CI0061
            11            PA06-XSEGLV PICTURE  X(02).                   CI0061
            11            PA06-XRC    PICTURE  X(02).                   CI0061
            11            PA06-XPROPT PICTURE  X(04).                   CI0061
            11            PA06-FILLER PICTURE  S9(5)                    CI0061
                          BINARY.                                       CI0061
            11            PA06-XSEGNM PICTURE  X(08).                   CI0061
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0061
                          BINARY.                                       CI0061
            11            PA06-XSEGNB PICTURE  9(05)                    CI0061
                          BINARY.                                       CI0061
            11            PA06-XCOKEY PICTURE  X(70).                   CI0061
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0061
          05              PC00-SUITE.                                   CI0061
            15       FILLER         PICTURE  X(00106).                  CI0061
       01                 PC06  REDEFINES      PC00.                    CI0061
            10            PC06-XDBPCB.                                  CI0061
            11            PC06-XDBDNM PICTURE  X(08).                   CI0061
            11            PC06-XSEGLV PICTURE  X(02).                   CI0061
            11            PC06-XRC    PICTURE  X(02).                   CI0061
            11            PC06-XPROPT PICTURE  X(04).                   CI0061
            11            PC06-FILLER PICTURE  S9(5)                    CI0061
                          BINARY.                                       CI0061
            11            PC06-XSEGNM PICTURE  X(08).                   CI0061
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0061
                          BINARY.                                       CI0061
            11            PC06-XSEGNB PICTURE  9(05)                    CI0061
                          BINARY.                                       CI0061
            11            PC06-XCOKEY PICTURE  X(70).                   CI0061
      *                                                                 AMDU61
      ******************************************************************AMDU61
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET A      *AMDU61
      **     CLIENTS' ARRANGEMENT DETAILS FOR A SPECIFIED ARR TYPE.    *AMDU61
      ******************************************************************AMDU61
      *                                                                 AMDU61
      *!WF DSP=WZ DSL=DU SEL=61 FOR=I LEV=1                             AMDU61
       01                 WZ00.                                         CI0061
          05              WZ00-SUITE.                                   CI0061
            15       FILLER         PICTURE  X(29071).                  CI0061
       01                 WZ61  REDEFINES      WZ00.                    CI0061
            10            WZ61-CLID   PICTURE  X(23).                   CI0061
            10            WZ61-CARTY  PICTURE  99.                      CI0061
            10            WZ61-NARRS  PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            10            WZ61-MIPPS  PICTURE  X(4).                    CI0061
            10            WZ61-CTID3  PICTURE  X(27).                   CI0061
            10            WZ61-CPMTCX PICTURE  XX.                      CI0061
            10            WZ61-NAPDSK PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            10            WZ61-GESTD1 PICTURE  9(8).                    CI0061
            10            WZ61-FILLER PICTURE  X(100).                  CI0061
            10            WZ61-IENDP  PICTURE  X.                       CI0061
            10            WZ61-DU9E                                     CI0061
                          OCCURS       100     TIMES.                   CI0061
            11            WZ61-CX06.                                    CI0061
            12            WZ61-CX06K.                                   CI0061
            13            WZ61-C299.                                    CI0061
            14            WZ61-CTID.                                    CI0061
            15            WZ61-CTIDA  PICTURE  9(3).                    CI0061
            15            WZ61-CTIDN.                                   CI0061
            16            WZ61-CTIDNP PICTURE  X(13).                   CI0061
            16            WZ61-CTIDND PICTURE  9(11).                   CI0061
            12            WZ61-NPECK  PICTURE  9(02).                   CI0061
            12            WZ61-FILLER PICTURE  X.                       CI0061
            11            WZ61-CX12.                                    CI0061
            12            WZ61-CX12K.                                   CI0061
            13            WZ61-CPMTC  PICTURE  99.                      CI0061
            13            WZ61-NAPDS  PICTURE  S9(3)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            13            WZ61-GESTD  PICTURE  9(8).                    CI0061
            12            WZ61-GEEND  PICTURE  9(8).                    CI0061
            12            WZ61-CIRMO  PICTURE  X(12).                   CI0061
            12            WZ61-CDEST  PICTURE  99.                      CI0061
            12            WZ61-APMTL  PICTURE  S9(9)V99                 CI0061
                          COMPUTATIONAL-3.                              CI0061
            12            WZ61-DNPMT  PICTURE  9(8).                    CI0061
            12            WZ61-NIRACM PICTURE  9(2).                    CI0061
            12            WZ61-CPMTF  PICTURE  99.                      CI0061
            12            WZ61-IPODM  PICTURE  X.                       CI0061
            12            WZ61-CLUPD  PICTURE  9(3).                    CI0061
            12            WZ61-DLAUP  PICTURE  9(8).                    CI0061
            12            WZ61-CWRC   PICTURE  99.                      CI0061
            12            WZ61-CHCR   PICTURE  99.                      CI0061
            12            WZ61-GEOPD2 PICTURE  X(8).                    CI0061
            12            WZ61-GEAUN  PICTURE  9(5).                    CI0061
            12            WZ61-DPCHD  PICTURE  9(8).                    CI0061
            12            WZ61-DNEXE  PICTURE  9(8).                    CI0061
            12            WZ61-CCSMQ  PICTURE  X.                       CI0061
            12            WZ61-GCUSPZ PICTURE  X(12).                   CI0061
            12            WZ61-CORTY  PICTURE  X.                       CI0061
            12            WZ61-CNAVR  PICTURE  X(1).                    CI0061
            12            WZ61-DELOI3 PICTURE  9(6).                    CI0061
            12            WZ61-ALOIDD PICTURE  9(9)V99                  CI0061
                          COMPUTATIONAL-3.                              CI0061
            12            WZ61-FILLER PICTURE  X(5).                    CI0061
            11            WZ61-MSP03A PICTURE  X(1).                    CI0061
            11            WZ61-FILLER PICTURE  X(129).                  CI0061
      *                                                                 AMDU61
      *                                                                 AMDU61
      *                                                                 AMDU61
      *                                                                 AMDU61
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0061
          05              DE00-SUITE.                                   CI0061
            15       FILLER         PICTURE  X(00653).                  CI0061
       01                 DE10  REDEFINES      DE00.                    CI0061
            10            DE10-DU11.                                    CI0061
            11            DE10-XFONC  PICTURE  X(4).                    CI0061
            11            DE10-MPSBN  PICTURE  X(8).                    CI0061
            11            DE10-XDBDNM PICTURE  X(08).                   CI0061
            11            DE10-XSEGNM PICTURE  X(08).                   CI0061
            11            DE10-XRC    PICTURE  X(02).                   CI0061
            11            DE10-MSEG   PICTURE  X(08).                   CI0061
            11            DE10-XCOKEY PICTURE  X(70).                   CI0061
            11            DE10-CUIBR  PICTURE  X(01).                   CI0061
            11            DE10-CUIBA  PICTURE  X(01).                   CI0061
            11            DE10-IPBIK  PICTURE  X(1).                    CI0061
            10            DE10-DU03.                                    CI0061
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            11            DE10-CMSSF  PICTURE  XX.                      CI0061
            11            DE10-DU09.                                    CI0061
            12            DE10-CMESA  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            12            DE10-CMESB  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            12            DE10-CMSST  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            12            DE10-QELLAA PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            12            DE10-TMESS4 PICTURE  X(512).                  CI0061
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
       01                 MS00.                                         CI0061
          05              MS00-SUITE.                                   CI0061
            15       FILLER         PICTURE  X(00542).                  CI0061
       01                 MS03  REDEFINES      MS00.                    CI0061
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0061
                          COMPUTATIONAL-3.                              CI0061
            10            MS03-CMSSF  PICTURE  XX.                      CI0061
            10            MS03-DU09.                                    CI0061
            11            MS03-CMESA  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            11            MS03-CMESB  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            11            MS03-CMSST  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            11            MS03-QELLAA PICTURE  S9(9)                    CI0061
                          BINARY.                                       CI0061
            11            MS03-TMESS4 PICTURE  X(512).                  CI0061
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ61
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0061
      *               *                                   *             CI0061
      *               *INITIALISATIONS                    *             CI0061
      *               *                                   *             CI0061
      *               *************************************.            CI0061
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
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
       F02CA-FN. EXIT.
      *N02DA.    NOTE *INITIALIZATION                     *.
       F02DA.                                                           lv10
      **
      *********************************
      ** INITIALIZE THE DU9E SEGMENT  *
      ** WHICH IS PART OF THE WZ61    *
      ** SEGMENT, THE PASS AREA BACK  *
      ** TO THE CALLING MODULE.       *
      *********************************
      **
      *N02DD.    NOTE *LOOP TO INIT DU9E PART OF WZ61     *.
       F02DD.                                                           lv15
           MOVE        1                        TO J02DDR
                                    GO TO     F02DD-B.
       F02DD-A.
           ADD         1                        TO J02DDR.
       F02DD-B.
           IF          J02DDR                   >  SUB-MAX
                                    GO TO     F02DD-FN.
           INITIALIZE  WZ61-DU9E (J02DDR).
       F02DD-900. GO TO F02DD-A.
       F02DD-FN. EXIT.
       F02DA-FN. EXIT.
      *N02FA.    NOTE *INITIALIZE THE MESSAGE AREA        *.
       F02FA.                                                           lv10
      **
      *********************************
      ** INITIALIZE THE MS03 SEGMENT  *
      ** WHICH WILL BE RETURNED TO THE*
      ** CALLING MODULE               *
      *********************************
      **
           INITIALIZE  MS03.
       F02FA-FN. EXIT.
      *N02FH.    NOTE *INITIALIZE IRA MONTH               *.
       F02FH.                                                           lv10
      **
      *********************************
      ** INIT THE WORKING STORAGE IRA *
      ** MONTH.  HOLDS THE CONTRACT   *
      ** IRA MTH - PUT THIS ON EACH   *
      ** CX12 SENT BACK.              *
      *********************************
      **
           INITIALIZE  7-CT01-DIRAC.
       F02FH-FN. EXIT.
      *N02HA.    NOTE *QUICK VALIDATION OF PASSED PARMS   *.
       F02HA.    IF    WZ61-CLID = SPACES                               lv10
                 OR    WZ61-CARTY NOT NUMERIC
                 OR    WZ61-NARRS NOT NUMERIC
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
           MOVE        012309 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98IC THRU F98IC-FN                              ADU019
           MOVE                     ALL '1' TO FT GO TO F20.
       F02HA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0061
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0061
      *               *                                   *             CI0061
      *               *FIN DE TRAITEMENT                  *             CI0061
      *               *                                   *             CI0061
      *               *************************************.            CI0061
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0061
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
      *               *GET CX03 SEGMENT                   *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      ** CALLING RPC IS RESPONSIBLE   *
      ** FOR PASSING ARRANGEMENT TYPE *
      ** (CARTY) AND ARRANGEMENT SEQ  *
      ** NUMBER (NAARS), CX03 KEYS,   *
      ** AND CLID, THE CX01 KEY.      *
      ** PERFORM A GET UNIQUE CALL    *
      ** FOR THE CX03 SEGMENT, ON     *
      ** CONTINUATION PROCESSING.     *
      ** SEE F45BB.                   *
      *********************************
      *
      *N40BB.    NOTE *SET UP SSAS TO GET CX06            *.
       F40BB.                                                           lv10
      *
           MOVE        WZ61-CLID TO S-CXU01-CLID
           MOVE        WZ61-CARTY TO S-CXU03-CARTY
           MOVE        WZ61-NARRS TO S-CXU03-NARRS
           MOVE        'EQ' TO S-CXU03-OPER.
       F40BB-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *IF THIS IS A CONTINUATION          *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      *
      *********************************
      ** IF THIS IS NOT THE FIRST TIME*
      ** THE MODULE HAS BEEN CALLED   *
      ** FOR THIS CLIENT/ARRANGEMENT, *
      ** THE END-OF-PROCESSING INDIC- *
      ** ATOR WILL BE 'N'.  IF SO,    *
      ** PERFORM A GET UNIQUE ON THE  *
      ** CX03 AND THEN ESTABLISH      *
      ** POSITION ON THE CX06/CX12    *
      ** BEFORE LOOPING THROUGH THE   *
      ** REMAINING SEGMENTS.          *
      *********************************
      *
      *N45BB.    NOTE *IF CONTINUATION PROCESSING         *.
       F45BB.    IF    WZ61-IENDP = 'N'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45BB-FN.
      *GET UNIQUE ON THE CX03 SEGMENT
      *AND THEN THE CX06.
      *
           PERFORM     F94C1 THRU F94C1-FN.
      *N45BF.    NOTE *IF CX03 NOT FOUND, SEND ERROR      *.
       F45BF.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F45BF-FN.
      *
      *GO BACK TO CALLING PROGRAM AFTER
      *SETTING UP ERROR MESSAGE.
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
       F45BF-900. GO TO F45CB-FN.
       F45BF-FN. EXIT.
      *N45CB.    NOTE *IF CX03 FOUND                      *.
       F45CB.                                                           lv15
      *
      *SET UP CX06 SSA TO DO A GNP
      *
           MOVE        '0' TO CX06-CF
           MOVE        WZ61-CTID3 TO S-CXU06-CX06K
           PERFORM     F94C2 THRU F94C2-FN.
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO CX06-CF.
      *N45CR.    NOTE *IF CX06 FOUND                      *.
       F45CR.    IF    CX06-CF = '1'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F45CR-FN.
      *N45DB.    NOTE *IF CX12 REQ, GET CT01 & CX12       *.
       F45DB.    IF    WZ61-MIPPS = 'CX12'                              lv25
                 NEXT SENTENCE ELSE GO TO     F45DB-FN.
      *
      *N45DE.    NOTE *GET CT01 FOR THE CX06              *.
       F45DE.                                                           lv30
      *GO GET THE CONTRACT SEGMENT
           PERFORM     F85 THRU F85-FN.
       F45DE-FN. EXIT.
      *N45DH.    NOTE *GET CX12 REQUESTED                 *.
       F45DH.                                                           lv30
           MOVE        '0' TO CX12-CF
           MOVE        WZ61-CPMTCX TO S-CXU12-CPMTC
           MOVE        WZ61-NAPDSK TO S-CXU12-NAPDS
           MOVE        WZ61-GESTD1 TO S-CXU12-GESTD
           PERFORM     F94C3 THRU F94C3-FN.
                 IF    IK = '0'                                         DOT
           MOVE        '1' TO CX12-CF.
      *N45EB.    NOTE *IF CX12 REQUESTED BUT NOT FOUND    *.
       F45EB.    IF    CX12-CF = '0'                                    lv35
                 NEXT SENTENCE ELSE GO TO     F45EB-FN.
      *SEND ERROR MESSAGE
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012610 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F45EB-FN. EXIT.
       F45DH-FN. EXIT.
       F45DB-FN. EXIT.
       F45CR-FN. EXIT.
      *N45FB.    NOTE *IF CX06 NOT FOUND                  *.
       F45FB.    IF    CX06-CF = '0'                                    lv20
                 NEXT SENTENCE ELSE GO TO     F45FB-FN.
      *SEND ERROR MESSAGE
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012610 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F45FB-FN. EXIT.
       F45CB-FN. EXIT.
       F45BB-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *LOOP THROUGH CX06 SEGMENTS         *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
           MOVE        'Y' TO MORE-CX06S
           MOVE        +1 TO SUB
           MOVE        'Y' TO FIRST-TIME-SW.
      *N50BB.    NOTE *GET CX06 SEGMENTS UNTIL THERE      *.
       F50BB.                       GO TO     F50BB-B.                  lv10
       F50BB-A.
                 IF    NO-MORE-CX06S
                 OR    SUB > SUB-MAX
                                    GO TO     F50BB-FN.
       F50BB-B.
      *ARE NO MORE, OR UNTIL PASS AREA
      *TABLE IS FULL
      *N50CB.    NOTE *IF FIRST TIME IN LOOP AND THIS     *.
       F50CB.    IF    FIRST-TIME-SW = 'Y'                              lv15
                 AND   WZ61-IENDP = 'N'
                 NEXT SENTENCE ELSE GO TO     F50CB-FN.
      *IS A CONTINUATION PROCESS
      *(STARTING FROM WHERE A PREVIOUS
      *CALL TO THIS PROGRAM ENDED)
      *
           MOVE        'N' TO FIRST-TIME-SW.
       F50CB-900. GO TO F50DB-FN.
       F50CB-FN. EXIT.
      *N50DB.    NOTE *IF NOT FIRST TIME AND A CONTINU-   *.
       F50DB.                                                           lv15
      *ATION, GET NEXT ON CX06
      *
           MOVE        '1' TO CX06-CF
           PERFORM     F94C4 THRU F94C4-FN.
                 IF    IK = '1'                                         DOT
           MOVE        '0' TO CX06-CF.
       F50DB-FN. EXIT.
      *N50EB.    NOTE *IF CX06 IS FOUND AND DETAILS ARE   *.
       F50EB.    IF    CX06-CF = '1'                                    lv15
                 AND   WZ61-MIPPS NOT = SPACES
                 NEXT SENTENCE ELSE GO TO     F50EB-FN.
      *WANTED, SET UP TO LOOP THROUGH
      *THE DETAIL SEGMENTS.
      *
      *N50EF.    NOTE *IF BA DETAILS ARE WANTED (CX12S)   *.
       F50EF.    IF    WZ61-MIPPS = 'CX12'                              lv20
                 NEXT SENTENCE ELSE GO TO     F50EF-FN.
      *GET THE CT01 FOR THE CX06
      *SET THE SEGMENT SWITCH AND MOVE
      *THE CX06 KEY TO THE SSA.
      *
           PERFORM     F85 THRU F85-FN
      *
           MOVE        'Y' TO MORE-CX12S
           MOVE        CX06-CX06K TO S-CXU06-CX06K.
      *N50FB.    NOTE *GET CX12 SEGMENTS UNTIL NO MORE    *.
       F50FB.                       GO TO     F50FB-B.                  lv25
       F50FB-A.
                 IF    NO-MORE-CX12S
                 OR    SUB > SUB-MAX
                                    GO TO     F50FB-FN.
       F50FB-B.
      *ARE FOUND, OR PASS AREA TABLE
      *IS FILLED UP.
      *
           PERFORM     F94C5 THRU F94C5-FN.
      *N50GB.    NOTE *IF CX12 FOUND, MOVE TO PASS AREA   *.
       F50GB.    IF    IK = '0'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F50GB-FN.
      *N50GD.    NOTE ***** GET MUTUAL FUND CLASS ****    *.
       F50GD.    IF    CT01-CTIDA = 002                                 lv35
                 NEXT SENTENCE ELSE GO TO     F50GD-FN.
      *GET THE MUTUAL FUND CLASS FROM
      *THE FUND CLASS TABLE TK17
           MOVE        7-CT01-PRSCD TO TG17-PRSCD
           PERFORM     F92TK THRU F92TK-FN.
       F50GD-FN. EXIT.
      *N50GF.    NOTE *CLEANUP BAD CIRMO FIELDS           *.
       F50GF.                                                           lv35
           INSPECT     CX12-CIRMO
                REPLACING ALL LOW-VALUES
                       BY SPACES.
       F50GF-FN. EXIT.
      *N50GT.    NOTE *MOVE TO PASS AREA                  *.
       F50GT.                                                           lv35
      *
           MOVE        CX06 TO WZ61-CX06 (SUB)
      *
      *MOVE CONTRACT IRA MTH TO CX12
           MOVE        7-CT01-DIRACM TO CX12-NIRACM
      *
           MOVE        CX12 TO WZ61-CX12 (SUB)
           ADD         +1 TO SUB.
       F50GT-FN. EXIT.
       F50GB-900. GO TO F50HB-FN.
       F50GB-FN. EXIT.
      *N50HB.    NOTE *IF CX12 NOT FOUND, SET SWITCH      *.
       F50HB.                                                           lv30
      *
           MOVE        'N' TO MORE-CX12S.
       F50HB-FN. EXIT.
       F50FB-900. GO TO F50FB-A.
       F50FB-FN. EXIT.
       F50EF-FN. EXIT.
       F50EB-FN. EXIT.
      *N50IB.    NOTE *IF CX06 IS FOUND BUT NO DETAILS    *.
       F50IB.    IF    CX06-CF = '1'                                    lv15
                 AND   WZ61-MIPPS = SPACES
                 NEXT SENTENCE ELSE GO TO     F50IB-FN.
      *ARE WANTED, MOVE THE CX06
      *TO THE PASS AREA
      *
           MOVE        CX06 TO WZ61-CX06 (SUB)
           ADD         +1 TO SUB.
       F50IB-FN. EXIT.
      *N50JB.    NOTE *IF CX06 NOT FOUND, SET SWITCH      *.
       F50JB.    IF    CX06-CF = '0'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F50JB-FN.
      *
           MOVE        'N' TO MORE-CX06S.
       F50JB-FN. EXIT.
       F50BB-900. GO TO F50BB-A.
       F50BB-FN. EXIT.
      *N50KB.    NOTE *IF PASS AREA IS FILLED UP,         *.
       F50KB.    IF    SUB > SUB-MAX                                    lv10
                 NEXT SENTENCE ELSE GO TO     F50KB-FN.
      *MOVE 'N' TO END-OF-PROCESSING
      *INDICATOR, MEANING THAT WE ARE
      *NOT THROUGH PROCESSING YET.
      *THE CALLING PROGRAM WILL DECIDE
      *WHETHER TO DO ANYTHING ELSE, AS
      *FAR AS CALLING THIS MODULE AGAIN
      *OR NOT.
      *
           MOVE        'N' TO WZ61-IENDP.
       F50KB-900. GO TO F50LB-FN.
       F50KB-FN. EXIT.
      *N50LB.    NOTE *IF PASS AREA IS NOT FILLED UP,     *.
       F50LB.                                                           lv10
      *MOVE 'Y' TO END-OF-PROCESSING
      *INDICATOR, MEANING THAT WE ARE
      *THROUGH PROCESSING.
      *
           MOVE        'Y' TO WZ61-IENDP.
       F50LB-FN. EXIT.
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
      *N85.      NOTE *************************************.
      *               *                                   *
      *               *GET THE CT01 SEG FOR THE CX06      *
      *               *                                   *
      *               *************************************.
       F85.                                                             lv05
      *
      *********************************
      ** GET THE IRA MONTH FROM THE   *
      ** CONTRACT.  THIS IS PERFORMED *
      ** FOR EACH CX06 READ.  THE CT01*
      ** HOLDS THE IRA MONTH THAT     *
      ** SHOULD REPLACE WHAT IS IN THE*
      ** CX12-NIRACM.  THE CX12 IRA   *
      ** MONTH IS NOT ACCURATE AS IT  *
      ** IS ON THE DB.                *
      *********************************
      *
      *N85EE.    NOTE *GET THE CONTRACT FOR THE CX06      *.
       F85EE.                                                           lv10
      *GET THE CONTRACT IRA MONTH
      *
           MOVE        CX06-CTID TO S-CTU01-CTID
      *
           PERFORM     F94C7 THRU F94C7-FN.
      *N85EH.    NOTE *IF THE CONTRACT ISN'T FOUND ERR    *.
       F85EH.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F85EH-FN.
      *
      *                                                                 ADU019
      *********************************                                 ADU019
      **    MOVE MESSAGE NUMBER       *                                 ADU019
      *********************************                                 ADU019
           MOVE        012011 TO MS03-NMESS2                            ADU019
      *********************************                                 ADU019
      **       SEND MESSAGE           *                                 ADU019
      *********************************                                 ADU019
           PERFORM     F98ET THRU F98ET-FN                              ADU019
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F85EH-FN. EXIT.
      *N85EK.    NOTE *CONTRACT WAS FOUND STORE IRA MTH   *.
       F85EK.                                                           lv15
      *
      *STORE THE CONTRACT IRA MONTH TO
      *REPLACE WHAT'S IN THE CX12.  THE
      *CX12 NIRACM ISN'T ACCURATE.
      *
           MOVE        CT01-DIRAC TO 7-CT01-DIRAC
      *STORE THE PRODUCT SUB-CODE
           MOVE        CT01-PRSCD TO 7-CT01-PRSCD.
      *N85EP.    NOTE *CHECK WHETHER MUTUAL FUND          *.
       F85EP.    IF    CT01-PRSCD = SPACES                              lv20
                 AND   CT01-CTIDA = 002
                 NEXT SENTENCE ELSE GO TO     F85EP-FN.
      *ACCOUNT HAS PRODUCT SUB-CODE
           MOVE        SPACES TO 7-CT01-PRSCD
      *DEFAULT THE PRODUCT SUB-CODE
           MOVE        '000000001' TO 7-CT01-PRSCD.
       F85EP-FN. EXIT.
       F85EK-FN. EXIT.
       F85EE-FN. EXIT.
       F85-FN.   EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N92EF.    NOTE *TABLE READ ERROR PROCESS (TA1W)    *.
       F92EF.                                                           lv10
           MOVE        SPACES TO TG17-MSP03A.
       F92EF-FN. EXIT.
      *N92TK.    NOTE *RANDOM TABLE READ FOR TG17         *.            ADUTAB
       F92TK.                                                           lv10
           MOVE        'R1' TO G-TG17-TABFO                             ADUTAB
           COMPUTE     G-TG17-LTH = 60 + G-TG17-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TG17-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TG17)                                ADUTAB
                       LENGTH (G-TG17-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TG17-TABCR NOT = '00'                          DOT
           PERFORM     F92EF THRU F92EF-FN.                             ADUTAB
           MOVE        TG17-MSP03A TO WZ61-MSP03A (SUB).                DOT
       F92TK-FN. EXIT.
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
      *N94C1.    NOTE *CALL GU ON CX03                    *.            ADU026
       F94C1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PA06 CX03                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL GN ON CX06                    *.            ADU026
       F94C2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94C3.    NOTE *CALL GN ON CX12                    *.            ADU026
       F94C3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CXU12-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
      *N94C4.    NOTE *CALL GN ON CX06                    *.            ADU026
       F94C4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX06' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX06                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CX06-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C4-FN. EXIT.
      *N94C5.    NOTE *CALL GN ON CX12                    *.            ADU026
       F94C5.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PA06 CX12                                                    ADU026
           S-CXU01-SSA S-CXU03-SSA                                      ADU026
           S-CXU06-SSA S-CX12-SSA
           MOVE        PA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C5-FN. EXIT.
      *N94C7.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94C7.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PC06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C7-FN. EXIT.
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
