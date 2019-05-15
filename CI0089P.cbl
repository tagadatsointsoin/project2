       IDENTIFICATION DIVISION.                                         CI0089
       PROGRAM-ID.  CI0089P.                                            CI0089
      *AUTHOR.         ADD BANK.                                        CI0089
      *DATE-COMPILED.   09/08/14.                                       CI0089
       ENVIRONMENT DIVISION.                                            CI0089
       CONFIGURATION SECTION.                                           CI0089
       SOURCE-COMPUTER. IBM-370.                                        CI0089
       OBJECT-COMPUTER. IBM-370.                                        CI0089
       DATA DIVISION.                                                   CI0089
       WORKING-STORAGE SECTION.                                         CI0089
      *                                                                 ADU031
      ******************************************************************ADU031
      **        ACF2 COMMON AREA FOR CALLING ACF2 MODULES              *ADU031
      ******************************************************************ADU031
      *                                                                 ADU031
         COPY ACFUAREA.                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *                                                                 ADU031
      *>>>>>>> Audit Log Work Area                                      ADU165
                                                                        ADU165
       01               AL00-ADDR.                                      ADU165
              05        AL00-NPNTR     USAGE IS POINTER.                ADU165
                                                                        ADU165
      *!WI pl=AL005                                                     ADU165
       01               AL00-NSEQ2P    VALUE ZERO                       ADU165
                        PICTURE S9(3)                                   CI0089
                          COMPUTATIONAL-3.                              CI0089
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0089
            10            DH10-GERTC  PICTURE  X                        CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0089
                          VALUE                ZERO                     CI0089
                          BINARY.                                       CI0089
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0089
                          VALUE                ZERO                     CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            DH10-CAUL   PICTURE  X                        CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-MAUSB  PICTURE  X(8)                     CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-NAUSK  PICTURE  X(50)                    CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-CSYS   PICTURE  X(4)                     CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-CAPPL  PICTURE  X(8)                     CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-CAUSR  PICTURE  X                        CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0089
                          VALUE                ZERO                     CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0089
                          VALUE                ZERO                     CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            DH10-GEOPID PICTURE  X(6)                     CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-CAUNIT PICTURE  X(4)                     CI0089
                          VALUE                SPACE.                   CI0089
            10            DH10-GAUVR  PICTURE  X(400)                   CI0089
                          VALUE                SPACE.                   CI0089
      *                                                                 AMDU05
      ******************************************************************AMDU05
      **     SEGMENT THAT CONTAINS THE REQUESTED CLIENT'S ADDRESS      *AMDU05
      ******************************************************************AMDU05
      *                                                                 AMDU05
      *!WF DSP=CA DSL=DU SEL=05 FOR=I LEV=1                             AMDU05
       01                 CA00.                                         CI0089
          05              CA00-SUITE.                                   CI0089
            15       FILLER         PICTURE  X(00435).                  CI0089
       01                 CA05  REDEFINES      CA00.                    CI0089
            10            CA05-C199.                                    CI0089
            11            CA05-CLID.                                    CI0089
            12            CA05-CLIDO  PICTURE  9(3).                    CI0089
            12            CA05-CLIDN.                                   CI0089
            13            CA05-CLIDNP PICTURE  X(12).                   CI0089
            13            CA05-CLIDND PICTURE  9(8).                    CI0089
            10            CA05-GECSQ1 PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            CA05-DCACG  PICTURE  9(8).                    CI0089
            10            CA05-FILLER PICTURE  X(100).                  CI0089
            10            CA05-CL24.                                    CI0089
            11            CA05-GELL   PICTURE  9(4)                     CI0089
                          BINARY.                                       CI0089
            11            CA05-CL24K.                                   CI0089
            12            CA05-GECSQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CA05-GECSD  PICTURE  9(8).                    CI0089
            11            CA05-GECED  PICTURE  9(8).                    CI0089
            11            CA05-CREQ2  PICTURE  X.                       CI0089
            11            CA05-FILLER PICTURE  X(4).                    CI0089
            11            CA05-GECTA  PICTURE  X.                       CI0089
            11            CA05-GELCD  PICTURE  9(8).                    CI0089
            11            CA05-GEADS  PICTURE  9.                       CI0089
            11            CA05-GECIT  PICTURE  X(25).                   CI0089
            11            CA05-GECTRY PICTURE  X(20).                   CI0089
            11            CA05-GECTY  PICTURE  9(3).                    CI0089
            11            CA05-GEPCD  PICTURE  X(12).                   CI0089
            11            CA05-GEST   PICTURE  X(8).                    CI0089
            11            CA05-IRESA  PICTURE  X.                       CI0089
            11            CA05-FILLER PICTURE  X(8).                    CI0089
            11            CA05-GESAD  PICTURE  X(30)                    CI0089
                          OCCURS       003     TIMES.                   CI0089
            10            CA05-FILLER PICTURE  X(100).                  CI0089
      *                                                                 AMDU05
      *                                                                 AMDU05
      *                                                                 AMDU05
      *                                                                 AMDU05
       01                 CL01.                                         CI0089
            10            CL01-CL01K.                                   CI0089
            11            CL01-C199.                                    CI0089
            12            CL01-CLID.                                    CI0089
            13            CL01-CLIDO  PICTURE  9(3).                    CI0089
            13            CL01-CLIDN.                                   CI0089
            14            CL01-CLIDNP PICTURE  X(12).                   CI0089
            14            CL01-CLIDND PICTURE  9(8).                    CI0089
            10            CL01-GECKD  PICTURE  9.                       CI0089
            10            CL01-GEMDA  PICTURE  9(8).                    CI0089
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0089
                          BINARY.                                       CI0089
            10            CL01-GECUC  PICTURE  99.                      CI0089
            10            CL01-CLDOR  PICTURE  9(8).                    CI0089
            10            CL01-CLLNG  PICTURE  XX.                      CI0089
            10            CL01-GESLC  PICTURE  99.                      CI0089
            10            CL01-CLTYP  PICTURE  X.                       CI0089
            10            CL01-CLCLS  PICTURE  9(3).                    CI0089
            10            CL01-CLTWRC PICTURE  99.                      CI0089
            10            CL01-CLPVC  PICTURE  99.                      CI0089
            10            CL01-CLIND  PICTURE  9(3).                    CI0089
            10            CL01-CLTRC  PICTURE  99.                      CI0089
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            CL01-AYSIDA PICTURE  9(3).                    CI0089
            10            CL01-AYSID  PICTURE  9(5).                    CI0089
            10            CL01-CLSTR  PICTURE  9(2).                    CI0089
            10            CL01-CLC11  PICTURE  X.                       CI0089
            10            CL01-CLTIN  PICTURE  9(12).                   CI0089
            10            CL01-CLTND  PICTURE  9(8).                    CI0089
            10            CL01-CLTINC PICTURE  9.                       CI0089
            10            CL01-CCDWA  PICTURE  9.                       CI0089
            10            CL01-CICES  PICTURE  X.                       CI0089
            10            CL01-CLTRA  PICTURE  9(2).                    CI0089
            10            CL01-DIRSY  PICTURE  9(4)                     CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            CL01-CFEDS  PICTURE  X.                       CI0089
            10            CL01-FILLER PICTURE  X(06).                   CI0089
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0006           PIC X(8) VALUE 'CI0006P '.                  AM0006
       01                 CX01.                                         CI0089
            10            CX01-CX01K.                                   CI0089
            11            CX01-C199.                                    CI0089
            12            CX01-CLID.                                    CI0089
            13            CX01-CLIDO  PICTURE  9(3).                    CI0089
            13            CX01-CLIDN.                                   CI0089
            14            CX01-CLIDNP PICTURE  X(12).                   CI0089
            14            CX01-CLIDND PICTURE  9(8).                    CI0089
            10            CX01-GEMDA  PICTURE  9(8).                    CI0089
            10            CX01-NSEQ4B PICTURE  9(8)                     CI0089
                          BINARY.                                       CI0089
            10            CX01-FILLER PICTURE  X(5).                    CI0089
       01                 CX03.                                         CI0089
            10            CX03-GELL   PICTURE  9(4)                     CI0089
                          BINARY.                                       CI0089
            10            CX03-CY00.                                    CI0089
            11            CX03-CX03K.                                   CI0089
            12            CX03-CARTY  PICTURE  99.                      CI0089
            12            CX03-NARRS  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CX03-CARST  PICTURE  99.                      CI0089
            11            CX03-GECSQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CX03-CPMTG  PICTURE  99.                      CI0089
            11            CX03-GRCRNG PICTURE  9(3).                    CI0089
            11            CX03-DEXDT  PICTURE  9(8).                    CI0089
            11            CX03-DASUP  PICTURE  9(8).                    CI0089
            11            CX03-CSTEC  PICTURE  X(3).                    CI0089
            11            CX03-FILLER PICTURE  X(17).                   CI0089
            11            CX03-CY50.                                    CI0089
            12            CX03-NARID  PICTURE  X(30).                   CI0089
            11            CX03-CY51                                     CI0089
                          REDEFINES            CX03-CY50.               CI0089
            12            CX03-NDIDN  PICTURE  9(12).                   CI0089
            12            CX03-FILLER PICTURE  X(18).                   CI0089
            11            CX03-CY52                                     CI0089
                          REDEFINES            CX03-CY50.               CI0089
            12            CX03-NAIDC  PICTURE  9(12).                   CI0089
            12            CX03-FILLER PICTURE  X(18).                   CI0089
            11            CX03-CY53                                     CI0089
                          REDEFINES            CX03-CY50.               CI0089
            12            CX03-NAMEXB PICTURE  9(15).                   CI0089
            12            CX03-FILLER PICTURE  X(15).                   CI0089
            10            CX03-CY99.                                    CI0089
            11            CX03-FILLER PICTURE  X(109).                  CI0089
            10            CX03-CY01                                     CI0089
                          REDEFINES            CX03-CY99.               CI0089
            11            CX03-NBASQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CX03-ICPCI  PICTURE  X.                       CI0089
            11            CX03-CLUPD  PICTURE  9(3).                    CI0089
            11            CX03-DLAUP  PICTURE  9(8).                    CI0089
            11            CX03-CWRC   PICTURE  99.                      CI0089
            11            CX03-CHCR   PICTURE  99.                      CI0089
            11            CX03-GEOPD2 PICTURE  X(8).                    CI0089
            11            CX03-GEAUN  PICTURE  9(5).                    CI0089
            11            CX03-DPCHD  PICTURE  9(8).                    CI0089
            11            CX03-DLRCHK PICTURE  9(8).                    CI0089
            11            CX03-QTRCHK PICTURE  9(2).                    CI0089
            11            CX03-DNPMT  PICTURE  9(8).                    CI0089
            11            CX03-APMTLA PICTURE  S9(9)V99                 CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            CX03-CY02                                     CI0089
                          REDEFINES            CX03-CY99.               CI0089
            11            CX03-QSIRQ  PICTURE  99.                      CI0089
            11            CX03-QDRMN  PICTURE  9(2)                     CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CX03-DDPRE  PICTURE  9(8).                    CI0089
            11            CX03-DDSHP  PICTURE  9(8).                    CI0089
            11            CX03-NDRFTB PICTURE  9(5).                    CI0089
            11            CX03-QDIPBJ PICTURE  9(3).                    CI0089
            11            CX03-DDSHPA PICTURE  9(8).                    CI0089
            11            CX03-NDRFTF PICTURE  9(5).                    CI0089
            11            CX03-QDIPBK PICTURE  9(3).                    CI0089
            11            CX03-CREOR  PICTURE  X(1).                    CI0089
            11            CX03-CREOR1 PICTURE  X(1).                    CI0089
            11            CX03-DDASC  PICTURE  9(8).                    CI0089
            11            CX03-FILLER PICTURE  X(7).                    CI0089
            10            CX03-CY03                                     CI0089
                          REDEFINES            CX03-CY99.               CI0089
            11            CX03-DLAUP1 PICTURE  9(8).                    CI0089
            11            CX03-GEOPD3 PICTURE  X(8).                    CI0089
            11            CX03-DNPMT1 PICTURE  9(8).                    CI0089
            11            CX03-DOPDA  PICTURE  99.                      CI0089
            11            CX03-CPMTF  PICTURE  99.                      CI0089
            11            CX03-CIRMO  PICTURE  X(12).                   CI0089
            11            CX03-CPALL  PICTURE  X(1).                    CI0089
            11            CX03-CCOLM  PICTURE  9(2).                    CI0089
            11            CX03-CBLTP  PICTURE  X(1).                    CI0089
            11            CX03-CASUB  PICTURE  9(2).                    CI0089
            11            CX03-CBLFM  PICTURE  9(2).                    CI0089
            11            CX03-IBILS  PICTURE  X.                       CI0089
            11            CX03-IPAOS  PICTURE  X.                       CI0089
            11            CX03-CBLSQ  PICTURE  X(4).                    CI0089
            11            CX03-DLBPD  PICTURE  9(8).                    CI0089
            11            CX03-DNBPD  PICTURE  9(8).                    CI0089
            11            CX03-DODBD  PICTURE  9(8).                    CI0089
            11            CX03-CPSRE  PICTURE  99.                      CI0089
            11            CX03-ISPHN  PICTURE  X.                       CI0089
            11            CX03-TCARR  PICTURE  X(6).                    CI0089
            11            CX03-CBKPT  PICTURE  9(2).                    CI0089
            11            CX03-IECNT  PICTURE  X.                       CI0089
            11            CX03-ICONV  PICTURE  X(1).                    CI0089
            11            CX03-FILLER PICTURE  X(4).                    CI0089
            10            CX03-CY04                                     CI0089
                          REDEFINES            CX03-CY99.               CI0089
            11            CX03-CCARD  PICTURE  X(02).                   CI0089
            11            CX03-MCSIG4 PICTURE  X(20).                   CI0089
            11            CX03-IREMT  PICTURE  X(01).                   CI0089
            11            CX03-ISBILA PICTURE  X.                       CI0089
            11            CX03-DLBPDA PICTURE  9(8).                    CI0089
            11            CX03-DNBPDA.                                  CI0089
            12            CX03-DNCYM  PICTURE  9(6).                    CI0089
            12            CX03-CEDTD  PICTURE  9(2).                    CI0089
            11            CX03-AREMT  PICTURE  S9(7)V99                 CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CX03-DREMT  PICTURE  9(8).                    CI0089
            11            CX03-ADBRQ  PICTURE  S9(11)V99                CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CX03-CLUPD1 PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            CX03-DLAUP3 PICTURE  9(8).                    CI0089
            11            CX03-CWRC2  PICTURE  99.                      CI0089
            11            CX03-CHCR2  PICTURE  99.                      CI0089
            11            CX03-GEOPD9 PICTURE  X(8).                    CI0089
            11            CX03-GEAUN1 PICTURE  9(5).                    CI0089
            11            CX03-DPCHD1 PICTURE  9(8).                    CI0089
       01                 CX18.                                         CI0089
            10            CX18-CX18K.                                   CI0089
            11            CX18-NBASQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            CX18-NPBN   PICTURE  X(20).                   CI0089
            10            CX18-CCBAT  PICTURE  99.                      CI0089
            10            CX18-DACHP  PICTURE  9(8).                    CI0089
            10            CX18-CSTPRE PICTURE  99.                      CI0089
            10            CX18-C199.                                    CI0089
            11            CX18-CLID.                                    CI0089
            12            CX18-CLIDO  PICTURE  9(3).                    CI0089
            12            CX18-CLIDN.                                   CI0089
            13            CX18-CLIDNP PICTURE  X(12).                   CI0089
            13            CX18-CLIDND PICTURE  9(8).                    CI0089
            10            CX18-MCSIG  PICTURE  X(30).                   CI0089
            10            CX18-CPBNU  PICTURE  X.                       CI0089
            10            CX18-CSPCR  PICTURE  99.                      CI0089
            10            CX18-DAPCR  PICTURE  9(8).                    CI0089
            10            CX18-FILLER PICTURE  XX.                      CI0089
       01                 CX19.                                         CI0089
            10            CX19-DAUDT  PICTURE  9(8).                    CI0089
            10            CX19-FILLER PICTURE  X(20).                   CI0089
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0089
            10            XW05-XW06.                                    CI0089
            11            XW05-XDBPCB.                                  CI0089
            12            XW05-XDBDNM PICTURE  X(08)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-XSEGLV PICTURE  X(02)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-XRC    PICTURE  X(02)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-XPROPT PICTURE  X(04)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-FILLER PICTURE  S9(5)                    CI0089
                          VALUE                ZERO                     CI0089
                          BINARY.                                       CI0089
            12            XW05-XSEGNM PICTURE  X(08)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0089
                          VALUE                ZERO                     CI0089
                          BINARY.                                       CI0089
            12            XW05-XSEGNB PICTURE  9(05)                    CI0089
                          VALUE                ZERO                     CI0089
                          BINARY.                                       CI0089
            12            XW05-XCOKEY PICTURE  X(70)                    CI0089
                          VALUE                SPACE.                   CI0089
            10            XW05-XW07.                                    CI0089
            11            XW05-XIOPCB.                                  CI0089
            12            XW05-XTERMI PICTURE  X(08)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-FILLER PICTURE  XX                       CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-XRC1   PICTURE  X(02)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-FILLER PICTURE  X(12)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            XW05-XMODNM PICTURE  X(8)                     CI0089
                          VALUE                SPACE.                   CI0089
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0089
                          VALUE                ZERO.                    CI0089
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0089
                          VALUE                ZERO.                    CI0089
            10            XW05-XGU    PICTURE  X(4)                     CI0089
                          VALUE                'GU  '.                  CI0089
            10            XW05-XGHU   PICTURE  X(4)                     CI0089
                          VALUE                'GHU '.                  CI0089
            10            XW05-XGN    PICTURE  X(4)                     CI0089
                          VALUE                'GN  '.                  CI0089
            10            XW05-XGHN   PICTURE  X(4)                     CI0089
                          VALUE                'GHN '.                  CI0089
            10            XW05-XGNP   PICTURE  X(4)                     CI0089
                          VALUE                'GNP '.                  CI0089
            10            XW05-XGHNP  PICTURE  X(4)                     CI0089
                          VALUE                'GHNP'.                  CI0089
            10            XW05-XREPL  PICTURE  XXXX                     CI0089
                          VALUE                'REPL'.                  CI0089
            10            XW05-XISRT  PICTURE  X(4)                     CI0089
                          VALUE                'ISRT'.                  CI0089
            10            XW05-XDLET  PICTURE  X(4)                     CI0089
                          VALUE                'DLET'.                  CI0089
            10            XW05-XOPEN  PICTURE  X(4)                     CI0089
                          VALUE                'OPEN'.                  CI0089
            10            XW05-XCLSE  PICTURE  X(4)                     CI0089
                          VALUE                'CLSE'.                  CI0089
            10            XW05-XCHKP  PICTURE  X(4)                     CI0089
                          VALUE                'CHKP'.                  CI0089
            10            XW05-XXRST  PICTURE  X(4)                     CI0089
                          VALUE                'XRST'.                  CI0089
            10            XW05-XTERM  PICTURE  X(4)                     CI0089
                          VALUE                'TERM'.                  CI0089
            10            XW05-XNFPAC PICTURE  X(13)                    CI0089
                          VALUE                SPACE.                   CI0089
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0089
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0089
      *!WI pl=GU020                                                     AACC20
       01  7-GU01-CLUPD                                                 AACC20
                        PICTURE 9(3).                                   CI0089
      *!WI pl=GU030                                                     AACC20
       01  7-GU01-CACOD                                                 AACC20
                        PICTURE 9.                                      CI0089
      *!WI pl=GU050                                                     AACC20
       01  7-GU01-XCORET                                                AACC20
                        PICTURE XX.                                     CI0089
      *!WF DSP=WX DSL=GU SEL=01 FOR=I LEV=1                             AACC20
       01                 WX00.                                         CI0089
          05              WX00-SUITE.                                   CI0089
            15       FILLER         PICTURE  X(00400).                  CI0089
       01                 WX01  REDEFINES      WX00.                    CI0089
            10            WX01-GELL   PICTURE  9(4)                     CI0089
                          BINARY.                                       CI0089
            10            WX01-GV00.                                    CI0089
            11            WX01-GU01K.                                   CI0089
            12            WX01-CANUMB PICTURE  X(27).                   CI0089
            12            WX01-CREQT  PICTURE  9(04).                   CI0089
            12            WX01-NSEQ2P PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            WX01-DCACG  PICTURE  9(8).                    CI0089
            11            WX01-IDTYP  PICTURE  9.                       CI0089
            11            WX01-CRSTA  PICTURE  X(01).                   CI0089
            11            WX01-CCOPY  PICTURE  9(02).                   CI0089
            11            WX01-MRPID  PICTURE  X(06).                   CI0089
            10            WX01-GV99.                                    CI0089
            11            WX01-FILLER PICTURE  X(347).                  CI0089
            10            WX01-GV01                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-GR98.                                    CI0089
            12            WX01-GRID.                                    CI0089
            13            WX01-GRIDC  PICTURE  9(3).                    CI0089
            13            WX01-GRIDN.                                   CI0089
            14            WX01-GRIDNP PICTURE  99.                      CI0089
            14            WX01-GRIDND PICTURE  9(8).                    CI0089
            11            WX01-AYSID  PICTURE  9(5).                    CI0089
            11            WX01-ATID   PICTURE  9(7).                    CI0089
            11            WX01-GEAEN  PICTURE  X(12).                   CI0089
            11            WX01-GEAUN  PICTURE  9(5).                    CI0089
            11            WX01-CBLDG  PICTURE  X.                       CI0089
            11            WX01-CENTT  PICTURE  X.                       CI0089
            10            WX01-GV02                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-NARRS  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            WX01-CARTY  PICTURE  99.                      CI0089
            11            WX01-NLPGH  PICTURE  99.                      CI0089
            10            WX01-GV04                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-CLNMX2 PICTURE  X(60).                   CI0089
            11            WX01-IUGMA  PICTURE  X.                       CI0089
            10            WX01-GV05                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-CANUMV PICTURE  X(27).                   CI0089
            11            WX01-CLNMX3 PICTURE  X(60).                   CI0089
            11            WX01-CMINR  PICTURE  9.                       CI0089
            10            WX01-GV06                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-C299.                                    CI0089
            12            WX01-CTID.                                    CI0089
            13            WX01-CTIDA  PICTURE  9(3).                    CI0089
            13            WX01-CTIDN.                                   CI0089
            14            WX01-CTIDNP PICTURE  X(13).                   CI0089
            14            WX01-CTIDND PICTURE  9(11).                   CI0089
            11            WX01-CAGRID PICTURE  9(13).                   CI0089
            10            WX01-GV07                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-CACLID PICTURE  X(23).                   CI0089
            11            WX01-NSORG  PICTURE  9(3).                    CI0089
            11            WX01-NSM    PICTURE  9(10).                   CI0089
            10            WX01-GV08                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-GRIDA.                                   CI0089
            12            WX01-GRIDCB PICTURE  9(3).                    CI0089
            12            WX01-GRIDNB.                                  CI0089
            13            WX01-GRIDNG PICTURE  99.                      CI0089
            13            WX01-GRIDNA PICTURE  9(8).                    CI0089
            11            WX01-AYIDD  PICTURE  9(5).                    CI0089
            11            WX01-ATID1  PICTURE  9(7).                    CI0089
            11            WX01-GEAEN2 PICTURE  X(12).                   CI0089
            11            WX01-GEAUN2 PICTURE  9(5).                    CI0089
            11            WX01-CBLDGA PICTURE  X.                       CI0089
            11            WX01-CVLID1 PICTURE  X.                       CI0089
            11            WX01-TGMSG  PICTURE  X(10).                   CI0089
            11            WX01-CGCRD  PICTURE  X(2).                    CI0089
            10            WX01-GV10                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-NPTRN  PICTURE  9(06).                   CI0089
            11            WX01-CPFMT  PICTURE  9(02).                   CI0089
            11            WX01-NTPN   PICTURE  9(02).                   CI0089
            11            WX01-NLPN   PICTURE  9(02).                   CI0089
            11            WX01-NLIN   PICTURE  9(07).                   CI0089
            11            WX01-CAIF   PICTURE  9(02).                   CI0089
            11            WX01-XZ23.                                    CI0089
            12            WX01-GECSQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            12            WX01-CKPMM  PICTURE  X.                       CI0089
            12            WX01-CTTLC  PICTURE  X.                       CI0089
            12            WX01-XZ19   PICTURE  X(19).                   CI0089
            11            WX01-DPSUM  PICTURE  X(18).                   CI0089
            11            WX01-IPIRA  PICTURE  X(1).                    CI0089
            11            WX01-CPVER  PICTURE  9(1).                    CI0089
            11            WX01-QPMXL  PICTURE  9(03).                   CI0089
            11            WX01-PRCOD3 PICTURE  9(5).                    CI0089
            11            WX01-PRSCD  PICTURE  X(9).                    CI0089
            11            WX01-CFING  PICTURE  X.                       CI0089
            11            WX01-IQACU  PICTURE  X.                       CI0089
            11            WX01-IADIA  PICTURE  X(01).                   CI0089
            11            WX01-IADIB  PICTURE  X(01).                   CI0089
            11            WX01-IADIC  PICTURE  X(01).                   CI0089
            11            WX01-IADID  PICTURE  X(01).                   CI0089
            11            WX01-IADIE  PICTURE  X(01).                   CI0089
            11            WX01-IADIF  PICTURE  X(01).                   CI0089
            11            WX01-FILLER PICTURE  X(258).                  CI0089
            10            WX01-GV11                                     CI0089
                          REDEFINES            WX01-GV99.               CI0089
            11            WX01-GEAEN1 PICTURE  X(12).                   CI0089
            11            WX01-GEAUN1 PICTURE  9(5).                    CI0089
            11            WX01-MEMPL  PICTURE  X(20).                   CI0089
            11            WX01-CSHRC  PICTURE  9(03).                   CI0089
            11            WX01-ISHVR  PICTURE  X(1).                    CI0089
            11            WX01-CSHTC  PICTURE  X(2).                    CI0089
       01  7-GU01-RTN-CD       PIC X.                                   AACC30
       01                 GU01.                                         CI0089
            10            GU01-GELL   PICTURE  9(4)                     CI0089
                          BINARY.                                       CI0089
            10            GU01-GV00.                                    CI0089
            11            GU01-GU01K.                                   CI0089
            12            GU01-CANUMB PICTURE  X(27).                   CI0089
            12            GU01-CREQT  PICTURE  9(04).                   CI0089
            12            GU01-NSEQ2P PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            GU01-DCACG  PICTURE  9(8).                    CI0089
            11            GU01-IDTYP  PICTURE  9.                       CI0089
            11            GU01-CRSTA  PICTURE  X(01).                   CI0089
            11            GU01-CCOPY  PICTURE  9(02).                   CI0089
            11            GU01-MRPID  PICTURE  X(06).                   CI0089
            10            GU01-GV99.                                    CI0089
            11            GU01-FILLER PICTURE  X(347).                  CI0089
            10            GU01-GV01                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-GR98.                                    CI0089
            12            GU01-GRID.                                    CI0089
            13            GU01-GRIDC  PICTURE  9(3).                    CI0089
            13            GU01-GRIDN.                                   CI0089
            14            GU01-GRIDNP PICTURE  99.                      CI0089
            14            GU01-GRIDND PICTURE  9(8).                    CI0089
            11            GU01-AYSID  PICTURE  9(5).                    CI0089
            11            GU01-ATID   PICTURE  9(7).                    CI0089
            11            GU01-GEAEN  PICTURE  X(12).                   CI0089
            11            GU01-GEAUN  PICTURE  9(5).                    CI0089
            11            GU01-CBLDG  PICTURE  X.                       CI0089
            11            GU01-CENTT  PICTURE  X.                       CI0089
            10            GU01-GV02                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-NARRS  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            GU01-CARTY  PICTURE  99.                      CI0089
            11            GU01-NLPGH  PICTURE  99.                      CI0089
            10            GU01-GV04                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-CLNMX2 PICTURE  X(60).                   CI0089
            11            GU01-IUGMA  PICTURE  X.                       CI0089
            10            GU01-GV05                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-CANUMV PICTURE  X(27).                   CI0089
            11            GU01-CLNMX3 PICTURE  X(60).                   CI0089
            11            GU01-CMINR  PICTURE  9.                       CI0089
            10            GU01-GV06                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-C299.                                    CI0089
            12            GU01-CTID.                                    CI0089
            13            GU01-CTIDA  PICTURE  9(3).                    CI0089
            13            GU01-CTIDN.                                   CI0089
            14            GU01-CTIDNP PICTURE  X(13).                   CI0089
            14            GU01-CTIDND PICTURE  9(11).                   CI0089
            11            GU01-CAGRID PICTURE  9(13).                   CI0089
            10            GU01-GV07                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-CACLID PICTURE  X(23).                   CI0089
            11            GU01-NSORG  PICTURE  9(3).                    CI0089
            11            GU01-NSM    PICTURE  9(10).                   CI0089
            10            GU01-GV08                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-GRIDA.                                   CI0089
            12            GU01-GRIDCB PICTURE  9(3).                    CI0089
            12            GU01-GRIDNB.                                  CI0089
            13            GU01-GRIDNG PICTURE  99.                      CI0089
            13            GU01-GRIDNA PICTURE  9(8).                    CI0089
            11            GU01-AYIDD  PICTURE  9(5).                    CI0089
            11            GU01-ATID1  PICTURE  9(7).                    CI0089
            11            GU01-GEAEN2 PICTURE  X(12).                   CI0089
            11            GU01-GEAUN2 PICTURE  9(5).                    CI0089
            11            GU01-CBLDGA PICTURE  X.                       CI0089
            11            GU01-CVLID1 PICTURE  X.                       CI0089
            11            GU01-TGMSG  PICTURE  X(10).                   CI0089
            11            GU01-CGCRD  PICTURE  X(2).                    CI0089
            10            GU01-GV10                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-NPTRN  PICTURE  9(06).                   CI0089
            11            GU01-CPFMT  PICTURE  9(02).                   CI0089
            11            GU01-NTPN   PICTURE  9(02).                   CI0089
            11            GU01-NLPN   PICTURE  9(02).                   CI0089
            11            GU01-NLIN   PICTURE  9(07).                   CI0089
            11            GU01-CAIF   PICTURE  9(02).                   CI0089
            11            GU01-XZ23.                                    CI0089
            12            GU01-GECSQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            12            GU01-CKPMM  PICTURE  X.                       CI0089
            12            GU01-CTTLC  PICTURE  X.                       CI0089
            12            GU01-XZ19   PICTURE  X(19).                   CI0089
            11            GU01-DPSUM  PICTURE  X(18).                   CI0089
            11            GU01-IPIRA  PICTURE  X(1).                    CI0089
            11            GU01-CPVER  PICTURE  9(1).                    CI0089
            11            GU01-QPMXL  PICTURE  9(03).                   CI0089
            11            GU01-PRCOD3 PICTURE  9(5).                    CI0089
            11            GU01-PRSCD  PICTURE  X(9).                    CI0089
            11            GU01-CFING  PICTURE  X.                       CI0089
            11            GU01-IQACU  PICTURE  X.                       CI0089
            11            GU01-IADIA  PICTURE  X(01).                   CI0089
            11            GU01-IADIB  PICTURE  X(01).                   CI0089
            11            GU01-IADIC  PICTURE  X(01).                   CI0089
            11            GU01-IADID  PICTURE  X(01).                   CI0089
            11            GU01-IADIE  PICTURE  X(01).                   CI0089
            11            GU01-IADIF  PICTURE  X(01).                   CI0089
            11            GU01-FILLER PICTURE  X(258).                  CI0089
            10            GU01-GV11                                     CI0089
                          REDEFINES            GU01-GV99.               CI0089
            11            GU01-GEAEN1 PICTURE  X(12).                   CI0089
            11            GU01-GEAUN1 PICTURE  9(5).                    CI0089
            11            GU01-MEMPL  PICTURE  X(20).                   CI0089
            11            GU01-CSHRC  PICTURE  9(03).                   CI0089
            11            GU01-ISHVR  PICTURE  X(1).                    CI0089
            11            GU01-CSHTC  PICTURE  X(2).                    CI0089
      *GENERATE INDEX FOR MESSAGE TEXT
      *                   MS03
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
      *================================================================
      *= USED BY NAME PACKER - CMU016DY                               =
      *================================================================
       01               NM00.
      *!WI
         05             NM00-GERTC
                        PICTURE X.                                      CI0089
      *!WI
         05             NM00-CLNAM
                        PICTURE X(70).                                  CI0089
         05             NM00-CLNM67.
           10           NM00-CLNM30   PIC X(30).
           10           NM00-CLNM37   PIC X(37).
                                                                        AM0006
      ******************************************************************AM0006
      **     PCB ADDRESS LIST FOR CI0006.  MODULE CI0003 WILL NEED     *AM0006
      **     PCB'S FOR:                                                *AM0006
      **                CLIENT DATABASE(CL1P)                          *AM0006
      ******************************************************************AM0006
                                                                        AM0006
       01  CI0006-PCB-ADDRESS-LIST.                                     AM0006
           05  CI0006-PCB-CL1P-PTR1      POINTER.                       AM0006
      ******************************************************************AM0055
      *        PCB ADDRESS LIST FOR CI0055.  PCB'S ARE NEEDED FOR      *AM0055
      *        CAMS CLIENT DATABASE(CL1P) FOR CL01, CL41, AND CL42     *AM0055
      ******************************************************************AM0055
                                                                        AM0055
       01  CI0055-PCB-ADDRESS-LIST.                                     AM0055
           05  CI0055-PCB-CL1P-PTR1        POINTER.                     AM0055
      ******************************************************************
      **  SEGMENTS FOR FORMATTING AUDIT LOG                            *
      ******************************************************************
      *!WF DSP=VA DSL=VA SEL=0103 FOR=I DES=1 LEV=1
      * PLT=VA
       01                 VA01.                                         CI0089
            10            VA01-NARRS  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            VA01-CARTY  PICTURE  99.                      CI0089
            10            VA01-MCSIG  PICTURE  X(30).                   CI0089
            10            VA01-NTR    PICTURE  9(8).                    CI0089
            10            VA01-GECKD1 PICTURE  9.                       CI0089
            10            VA01-NPBN   PICTURE  X(20).                   CI0089
            10            VA01-CCBAT  PICTURE  99.                      CI0089
            10            VA01-ICPCI  PICTURE  X.                       CI0089
            10            VA01-CWRC   PICTURE  99.                      CI0089
            10            VA01-CHCR   PICTURE  99.                      CI0089
            10            VA01-CARST  PICTURE  99.                      CI0089
            10            VA01-CSTPRE PICTURE  99.                      CI0089
            10            VA01-DACHP  PICTURE  9(8).                    CI0089
            10            VA01-CLUPD  PICTURE  9(3).                    CI0089
            10            VA01-DLAUP  PICTURE  9(8).                    CI0089
            10            VA01-DPCHD  PICTURE  9(8).                    CI0089
       01                 VA03.                                         CI0089
            10            VA03-NPDIN  PICTURE  X(4).                    CI0089
            10            VA03-IRTNA  PICTURE  X.                       CI0089
            10            VA03-CLORN  PICTURE  X(45).                   CI0089
            10            VA03-CACSZ  PICTURE  X(30).                   CI0089
            10            VA03-NTR    PICTURE  9(8).                    CI0089
            10            VA03-GECKD1 PICTURE  9.                       CI0089
            10            VA03-IRTNP  PICTURE  X.                       CI0089
            10            VA03-IRTNW  PICTURE  X.                       CI0089
            10            VA03-GEEND  PICTURE  9(8).                    CI0089
            10            VA03-IACHA  PICTURE  X.                       CI0089
            10            VA03-XPBNF  PICTURE  X(20).                   CI0089
      *================================================================
      *= USED TO DETERMINE IF AN AUDIT LOG RECORD IS REQUIRED         =
      *================================================================
       01               WX00-GBACH.
         05             WX01-ISRT          PIC X.
         05             WX03-ISRT          PIC X.
         05             WX18-ISRT          PIC X.
      ******************************************************************AM0055
      *        CI0055 I/O FIELDS                                       *AM0055
      ******************************************************************AM0055
                                                                        AM0055
      *!WF DSP=WZ DSL=CP SEL=03 FOR=I DES=2 LEV=1                       AM0055
       01                 WZ03.                                         CI0089
            10            WZ03-C199.                                    CI0089
            11            WZ03-CLID.                                    CI0089
            12            WZ03-CLIDO  PICTURE  9(3)                     CI0089
                          VALUE                ZERO.                    CI0089
            12            WZ03-CLIDN.                                   CI0089
            13            WZ03-CLIDNP PICTURE  X(12)                    CI0089
                          VALUE                SPACE.                   CI0089
            13            WZ03-CLIDND PICTURE  9(8)                     CI0089
                          VALUE                ZERO.                    CI0089
            10            WZ03-CLTYP  PICTURE  X                        CI0089
                          VALUE                SPACE.                   CI0089
            10            WZ03-CLORN  PICTURE  X(45)                    CI0089
                          VALUE                SPACE.                   CI0089
            10            WZ03-C198.                                    CI0089
            11            WZ03-CLNAM.                                   CI0089
            12            WZ03-CLNAMH PICTURE  X(6)                     CI0089
                          VALUE                SPACE.                   CI0089
            12            WZ03-CLNAMF PICTURE  X(20)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            WZ03-CLNAMM.                                  CI0089
            13            WZ03-CLNAMI PICTURE  X                        CI0089
                          VALUE                SPACE.                   CI0089
            13            WZ03-CLNAMR PICTURE  X(14)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            WZ03-CLNAML PICTURE  X(25)                    CI0089
                          VALUE                SPACE.                   CI0089
            12            WZ03-CLNAMS PICTURE  X(4)                     CI0089
                          VALUE                SPACE.                   CI0089
                                                                        AM0055
       01 CI0055                    PIC X(8) VALUE 'CI0055P'.           AM0055
       01   DEBUT-WSS.                                                  CI0089
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0089
            05   IK     PICTURE X.                                      CI0089
       01  CONSTANTES-PAC.                                              CI0089
           05  FILLER  PICTURE X(87)   VALUE                            CI0089
                     '6015 CAT09/08/14CI0089ADMIN   14:34:42CI0089P AMERCI0089
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0089
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0089
           05  NUGNA   PICTURE X(5).                                    CI0089
           05  APPLI   PICTURE X(3).                                    CI0089
           05  DATGN   PICTURE X(8).                                    CI0089
           05  PROGR   PICTURE X(6).                                    CI0089
           05  CODUTI  PICTURE X(8).                                    CI0089
           05  TIMGN   PICTURE X(8).                                    CI0089
           05  PROGE   PICTURE X(8).                                    CI0089
           05  COBASE  PICTURE X(4).                                    CI0089
           05  DATGNC  PICTURE X(10).                                   CI0089
           05  RELEAS  PICTURE X(7).                                    CI0089
           05  DATGE   PICTURE X(10).                                   CI0089
           05  DATSQ   PICTURE X(10).                                   CI0089
       01  DATCE.                                                       CI0089
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0089
         05  DATOR.                                                     CI0089
           10  DATOA  PICTURE XX.                                       CI0089
           10  DATOM  PICTURE XX.                                       CI0089
           10  DATOJ  PICTURE XX.                                       CI0089
       01   VARIABLES-CONDITIONNELLES.                                  CI0089
            05                  FT      PICTURE X VALUE '0'.            CI0089
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0089
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0089
            05           IMS03L PICTURE S9(4) VALUE  ZERO.
            05           IMS03R PICTURE S9(4) VALUE  ZERO.
            05           IMS03M PICTURE S9(4) VALUE +0512.
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0089
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0089
            05       5-CX00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0089
            05       5-GU00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0089
       01               S-CL01-SSA.                                     CI0089
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0089
                                      VALUE 'CL01    '.                 CI0089
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0089
            10          S-CL01-CCOD   PICTURE X(5)                      CI0089
                                      VALUE '-----'.                    CI0089
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0089
       01            S-CLU01-SSA.                                       CI0089
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CL01    '.                 CI0089
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0089
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CL01K'.                   CI0089
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0089
            10       S-CLU01-CL01K.                                     CI0089
            11       S-CLU01-C199.                                      CI0089
            12       S-CLU01-CLID.                                      CI0089
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0089
            13       S-CLU01-CLIDN.                                     CI0089
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0089
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0089
            10  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01               S-CX01-SSA.                                     CI0089
            10         S1-CX01-SEGNAM PICTURE X(8)                      CI0089
                                      VALUE 'CX01    '.                 CI0089
            10         S1-CX01-CCOM   PICTURE X VALUE '*'.              CI0089
            10          S-CX01-CCOD   PICTURE X(5)                      CI0089
                                      VALUE '-----'.                    CI0089
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0089
       01            S-CXU01-SSA.                                       CI0089
            10      S1-CXU01-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX01    '.                 CI0089
            10      S1-CXU01-CCOM   PICTURE X VALUE '*'.                CI0089
            10       S-CXU01-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            10      S1-CXU01-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CX01K'.                   CI0089
            10       S-CXU01-OPER  PICTURE XX VALUE ' ='.               CI0089
            10       S-CXU01-CX01K.                                     CI0089
            11       S-CXU01-C199.                                      CI0089
            12       S-CXU01-CLID.                                      CI0089
            13       S-CXU01-CLIDO    PICTURE  9(3).                    CI0089
            13       S-CXU01-CLIDN.                                     CI0089
            14       S-CXU01-CLIDNP   PICTURE  X(12).                   CI0089
            14       S-CXU01-CLIDND   PICTURE  9(8).                    CI0089
            10  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01               S-CX03-SSA.                                     CI0089
            10         S1-CX03-SEGNAM PICTURE X(8)                      CI0089
                                      VALUE 'CX03    '.                 CI0089
            10         S1-CX03-CCOM   PICTURE X VALUE '*'.              CI0089
            10          S-CX03-CCOD   PICTURE X(5)                      CI0089
                                      VALUE '-----'.                    CI0089
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0089
       01            S-CXA03-SSA.                                       CI0089
            12      S1-CXA03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            12      S1-CXA03-CCOM   PICTURE X VALUE '*'.                CI0089
            12       S-CXA03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            12      S1-CXA03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CARTY'.                   CI0089
            12       S-CXA03-OPER  PICTURE XX VALUE ' ='.               CI0089
            12       S-CXA03-CARTY    PICTURE  99.                      CI0089
            12  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXB03-SSA.                                       CI0089
            12      S1-CXB03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            12      S1-CXB03-CCOM   PICTURE X VALUE '*'.                CI0089
            12       S-CXB03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            12      S1-CXB03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(NARRS'.                   CI0089
            12       S-CXB03-OPER  PICTURE XX VALUE ' ='.               CI0089
            12       S-CXB03-NARRS    PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            12  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXC03-SSA.                                       CI0089
            11      S1-CXC03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            11      S1-CXC03-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-CXC03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-CXC03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CPMTG'.                   CI0089
            11       S-CXC03-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-CXC03-CPMTG    PICTURE  99.                      CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXD03-SSA.                                       CI0089
            11      S1-CXD03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            11      S1-CXD03-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-CXD03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-CXD03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(GRCRNG'.                  CI0089
            11       S-CXD03-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-CXD03-GRCRNG   PICTURE  9(3).                    CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXE03-SSA.                                       CI0089
            11      S1-CXE03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            11      S1-CXE03-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-CXE03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-CXE03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(DEXDT'.                   CI0089
            11       S-CXE03-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-CXE03-DEXDT    PICTURE  9(8).                    CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXF03-SSA.                                       CI0089
            11      S1-CXF03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            11      S1-CXF03-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-CXF03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-CXF03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CY50'.                    CI0089
            11       S-CXF03-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-CXF03-CY50.                                      CI0089
            12       S-CXF03-NARID    PICTURE  X(30).                   CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXG03-SSA.                                       CI0089
            11      S1-CXG03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            11      S1-CXG03-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-CXG03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-CXG03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(NBASQ'.                   CI0089
            11       S-CXG03-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-CXG03-NBASQ    PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXH03-SSA.                                       CI0089
            12      S1-CXH03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            12      S1-CXH03-CCOM   PICTURE X VALUE '*'.                CI0089
            12       S-CXH03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            12      S1-CXH03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(NARID'.                   CI0089
            12       S-CXH03-OPER  PICTURE XX VALUE ' ='.               CI0089
            12       S-CXH03-NARID    PICTURE  X(30).                   CI0089
            12  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXU03-SSA.                                       CI0089
            11      S1-CXU03-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX03    '.                 CI0089
            11      S1-CXU03-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-CXU03-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-CXU03-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CX03K'.                   CI0089
            11       S-CXU03-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-CXU03-CX03K.                                     CI0089
            12       S-CXU03-CARTY    PICTURE  99.                      CI0089
            12       S-CXU03-NARRS    PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01               S-CX18-SSA.                                     CI0089
            10         S1-CX18-SEGNAM PICTURE X(8)                      CI0089
                                      VALUE 'CX18    '.                 CI0089
            10         S1-CX18-CCOM   PICTURE X VALUE '*'.              CI0089
            10          S-CX18-CCOD   PICTURE X(5)                      CI0089
                                      VALUE '-----'.                    CI0089
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0089
       01            S-CXA18-SSA.                                       CI0089
            10      S1-CXA18-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX18    '.                 CI0089
            10      S1-CXA18-CCOM   PICTURE X VALUE '*'.                CI0089
            10       S-CXA18-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            10      S1-CXA18-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CSTPRE'.                  CI0089
            10       S-CXA18-OPER  PICTURE XX VALUE ' ='.               CI0089
            10       S-CXA18-CSTPRE   PICTURE  99.                      CI0089
            10  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXB18-SSA.                                       CI0089
            10      S1-CXB18-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX18    '.                 CI0089
            10      S1-CXB18-CCOM   PICTURE X VALUE '*'.                CI0089
            10       S-CXB18-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            10      S1-CXB18-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CSPCR'.                   CI0089
            10       S-CXB18-OPER  PICTURE XX VALUE ' ='.               CI0089
            10       S-CXB18-CSPCR    PICTURE  99.                      CI0089
            10  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-CXU18-SSA.                                       CI0089
            10      S1-CXU18-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'CX18    '.                 CI0089
            10      S1-CXU18-CCOM   PICTURE X VALUE '*'.                CI0089
            10       S-CXU18-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            10      S1-CXU18-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CX18K'.                   CI0089
            10       S-CXU18-OPER  PICTURE XX VALUE ' ='.               CI0089
            10       S-CXU18-CX18K.                                     CI0089
            11       S-CXU18-NBASQ    PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            10  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01               S-CX19-SSA.                                     CI0089
            10         S1-CX19-SEGNAM PICTURE X(8)                      CI0089
                                      VALUE 'CX19    '.                 CI0089
            10         S1-CX19-CCOM   PICTURE X VALUE '*'.              CI0089
            10          S-CX19-CCOD   PICTURE X(5)                      CI0089
                                      VALUE '-----'.                    CI0089
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0089
       01               S-GU01-SSA.                                     CI0089
            10         S1-GU01-SEGNAM PICTURE X(8)                      CI0089
                                      VALUE 'GU01    '.                 CI0089
            10         S1-GU01-CCOM   PICTURE X VALUE '*'.              CI0089
            10          S-GU01-CCOD   PICTURE X(5)                      CI0089
                                      VALUE '-----'.                    CI0089
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0089
       01            S-GUG01-SSA.                                       CI0089
            12      S1-GUG01-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'GU01    '.                 CI0089
            12      S1-GUG01-CCOM   PICTURE X VALUE '*'.                CI0089
            12       S-GUG01-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            12      S1-GUG01-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(CANUMB'.                  CI0089
            12       S-GUG01-OPER  PICTURE XX VALUE ' ='.               CI0089
            12       S-GUG01-CANUMB   PICTURE  X(27).                   CI0089
            12  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-GUH01-SSA.                                       CI0089
            11      S1-GUH01-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'GU01    '.                 CI0089
            11      S1-GUH01-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-GUH01-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-GUH01-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(IDTYP'.                   CI0089
            11       S-GUH01-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-GUH01-IDTYP    PICTURE  9.                       CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01            S-GUU01-SSA.                                       CI0089
            11      S1-GUU01-SEGNAM PICTURE X(8)                        CI0089
                                      VALUE 'GU01    '.                 CI0089
            11      S1-GUU01-CCOM   PICTURE X VALUE '*'.                CI0089
            11       S-GUU01-CCOD   PICTURE X(5)                        CI0089
                                      VALUE '-----'.                    CI0089
            11      S1-GUU01-FLDNAM PICTURE X(9)                        CI0089
                                      VALUE '(GU01K'.                   CI0089
            11       S-GUU01-OPER  PICTURE XX VALUE ' ='.               CI0089
            11       S-GUU01-GU01K.                                     CI0089
            12       S-GUU01-CANUMB   PICTURE  X(27).                   CI0089
            12       S-GUU01-CREQT    PICTURE  9(04).                   CI0089
            12       S-GUU01-NSEQ2P   PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11  FILLER   PICTURE X    VALUE ')'.                        CI0089
       01   ZONES-UTILISATEUR PICTURE X.                                CI0089
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
      ** PCB POINTER FOR AR1P                                           ADU015
            05 PCB-AR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR AGCP                                           ADU015
            05 PCB-AGCP-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR AR1P                                             ADU015
      *!WF DSP=XA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XA00.                                         CI0089
          05              XA00-SUITE.                                   CI0089
            15       FILLER         PICTURE  X(00106).                  CI0089
       01                 XA06  REDEFINES      XA00.                    CI0089
            10            XA06-XDBPCB.                                  CI0089
            11            XA06-XDBDNM PICTURE  X(08).                   CI0089
            11            XA06-XSEGLV PICTURE  X(02).                   CI0089
            11            XA06-XRC    PICTURE  X(02).                   CI0089
            11            XA06-XPROPT PICTURE  X(04).                   CI0089
            11            XA06-FILLER PICTURE  S9(5)                    CI0089
                          BINARY.                                       CI0089
            11            XA06-XSEGNM PICTURE  X(08).                   CI0089
            11            XA06-XKEYLN PICTURE  S9(05)                   CI0089
                          BINARY.                                       CI0089
            11            XA06-XSEGNB PICTURE  9(05)                    CI0089
                          BINARY.                                       CI0089
            11            XA06-XCOKEY PICTURE  X(70).                   CI0089
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=XB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XB00.                                         CI0089
          05              XB00-SUITE.                                   CI0089
            15       FILLER         PICTURE  X(00106).                  CI0089
       01                 XB06  REDEFINES      XB00.                    CI0089
            10            XB06-XDBPCB.                                  CI0089
            11            XB06-XDBDNM PICTURE  X(08).                   CI0089
            11            XB06-XSEGLV PICTURE  X(02).                   CI0089
            11            XB06-XRC    PICTURE  X(02).                   CI0089
            11            XB06-XPROPT PICTURE  X(04).                   CI0089
            11            XB06-FILLER PICTURE  S9(5)                    CI0089
                          BINARY.                                       CI0089
            11            XB06-XSEGNM PICTURE  X(08).                   CI0089
            11            XB06-XKEYLN PICTURE  S9(05)                   CI0089
                          BINARY.                                       CI0089
            11            XB06-XSEGNB PICTURE  9(05)                    CI0089
                          BINARY.                                       CI0089
            11            XB06-XCOKEY PICTURE  X(70).                   CI0089
      *** PCB MASK FOR AGCP                                             ADU015
      *!WF DSP=XC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 XC00.                                         CI0089
          05              XC00-SUITE.                                   CI0089
            15       FILLER         PICTURE  X(00106).                  CI0089
       01                 XC06  REDEFINES      XC00.                    CI0089
            10            XC06-XDBPCB.                                  CI0089
            11            XC06-XDBDNM PICTURE  X(08).                   CI0089
            11            XC06-XSEGLV PICTURE  X(02).                   CI0089
            11            XC06-XRC    PICTURE  X(02).                   CI0089
            11            XC06-XPROPT PICTURE  X(04).                   CI0089
            11            XC06-FILLER PICTURE  S9(5)                    CI0089
                          BINARY.                                       CI0089
            11            XC06-XSEGNM PICTURE  X(08).                   CI0089
            11            XC06-XKEYLN PICTURE  S9(05)                   CI0089
                          BINARY.                                       CI0089
            11            XC06-XSEGNB PICTURE  9(05)                    CI0089
                          BINARY.                                       CI0089
            11            XC06-XCOKEY PICTURE  X(70).                   CI0089

      *PASS AREA TO/FROM CI0089
      *!WF DSP=QT DSL=QT SEL=89 FOR=I DES=1 LEV=1 PLT=10
       01                 QT89.                                         CI0089
            10            QT89-INPUT.                                   CI0089
            11            QT89-DCACG  PICTURE  9(8).                    CI0089
            11            QT89-CLID   PICTURE  X(23).                   CI0089
            11            QT89-CLID4  PICTURE  X(23).                   CI0089
            11            QT89-CCBAT  PICTURE  99.                      CI0089
            11            QT89-NPBN   PICTURE  X(20).                   CI0089
            11            QT89-MCSIG  PICTURE  X(30).                   CI0089
            11            QT89-CWRC   PICTURE  99.                      CI0089
            11            QT89-CHCR   PICTURE  99.                      CI0089
            11            QT89-GECSQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            QT89-CSTEC  PICTURE  X(3).                    CI0089
            11            QT89-NTR    PICTURE  9(8).                    CI0089
            11            QT89-GECKD  PICTURE  9.                       CI0089
            10            QT89-FILLER PICTURE  X(26).                   CI0089
            10            QT89-OUTPUT.                                  CI0089
            11            QT89-GEMDA  PICTURE  9(8).                    CI0089
            11            QT89-NSEQ4B PICTURE  9(8)                     CI0089
                          BINARY.                                       CI0089
            11            QT89-NBASQ  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            QT89-NARRS  PICTURE  S9(3)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            QT89-FILLER PICTURE  X(28).                   CI0089
      *-----> ERROR SEGMENT TO RETURN DL/1 ERRORS...
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1 PLT=85
       01                 DE00.                                         CI0089
          05              DE00-SUITE.                                   CI0089
            15       FILLER         PICTURE  X(00653).                  CI0089
       01                 DE10  REDEFINES      DE00.                    CI0089
            10            DE10-DU11.                                    CI0089
            11            DE10-XFONC  PICTURE  X(4).                    CI0089
            11            DE10-MPSBN  PICTURE  X(8).                    CI0089
            11            DE10-XDBDNM PICTURE  X(08).                   CI0089
            11            DE10-XSEGNM PICTURE  X(08).                   CI0089
            11            DE10-XRC    PICTURE  X(02).                   CI0089
            11            DE10-MSEG   PICTURE  X(08).                   CI0089
            11            DE10-XCOKEY PICTURE  X(70).                   CI0089
            11            DE10-CUIBR  PICTURE  X(01).                   CI0089
            11            DE10-CUIBA  PICTURE  X(01).                   CI0089
            11            DE10-IPBIK  PICTURE  X(1).                    CI0089
            10            DE10-DU03.                                    CI0089
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            DE10-CMSSF  PICTURE  XX.                      CI0089
            11            DE10-DU09.                                    CI0089
            12            DE10-CMESA  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            12            DE10-CMESB  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            12            DE10-CMSST  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            12            DE10-QELLAA PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            12            DE10-TMESS4 PICTURE  X(512).                  CI0089
      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0089
          05              MS00-SUITE.                                   CI0089
            15       FILLER         PICTURE  X(00542).                  CI0089
       01                 MS03  REDEFINES      MS00.                    CI0089
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            10            MS03-CMSSF  PICTURE  XX.                      CI0089
            10            MS03-DU09.                                    CI0089
            11            MS03-CMESA  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            11            MS03-CMESB  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            11            MS03-CMSST  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            11            MS03-QELLAA PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
            11            MS03-TMESS4 PICTURE  X(512).                  CI0089
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0089
            10            MX11-QMSGS  PICTURE  9(03).                   CI0089
            10            MX11-PJ09                                     CI0089
                          OCCURS       025     TIMES.                   CI0089
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0089
                          COMPUTATIONAL-3.                              CI0089
            11            MX11-CMESB  PICTURE  S9(9)                    CI0089
                          BINARY.                                       CI0089
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                QT89
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N0TSC.    NOTE *SET ADDRESSES FOR PCB LINKAGE      *.
       F0TSC.                                                           lv10
      *SET ADDRESS FOR AR1P                                             DOT
           SET ADDRESS OF XA06 TO                                       ADU015
                PCB-AR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF XB06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR AGCP                                             DOT
           SET ADDRESS OF XC06 TO                                       ADU015
                PCB-AGCP-PTR1.                                          ADU015
       F0TSC-FN. EXIT.
      *N01.      NOTE *************************************.            CI0089
      *               *                                   *             CI0089
      *               *INITIALISATIONS                    *             CI0089
      *               *                                   *             CI0089
      *               *************************************.            CI0089
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
      *N02CX.    NOTE *---> INITIALIZE ISRT INDICATORS    *.
       F02CX.                                                           lv10
           MOVE ALL    'N' TO WX00-GBACH.
       F02CX-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0089
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0089
      *               *                                   *             CI0089
      *               *FIN DE TRAITEMENT                  *             CI0089
      *               *                                   *             CI0089
      *               *************************************.            CI0089
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0089
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N25.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE INPUT                     *
      *               *                                   *
      *               *************************************.
       F25.           EXIT.                                             lv05
      *N25CB.    NOTE *VALIDATE CLID                      *.
       F25CB.    IF    QT89-CLID NOT > 0                                lv10
                 NEXT SENTENCE ELSE GO TO     F25CB-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012002 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25CB-FN. EXIT.
      *N25CE.    NOTE *VALIDATE CLID4                     *.
       F25CE.    IF    QT89-CLID4 NOT > 0                               lv10
                 NEXT SENTENCE ELSE GO TO     F25CE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012612 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25CE-FN. EXIT.
      *N25CH.    NOTE *VALIDATE CCBAT                     *.
       F25CH.    IF    QT89-CCBAT NOT = '01'                            lv10
                 AND   QT89-CCBAT NOT = '02'
                 NEXT SENTENCE ELSE GO TO     F25CH-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012158 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25CH-FN. EXIT.
      *N25CK.    NOTE *VALIDATE NPBN                      *.
       F25CK.    IF    QT89-NPBN NOT > SPACES                           lv10
                 NEXT SENTENCE ELSE GO TO     F25CK-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012605 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25CK-FN. EXIT.
      *N25CQ.    NOTE *VALIDATE CWRC                      *.
       F25CQ.    IF    QT89-CWRC = 0                                    lv10
                 NEXT SENTENCE ELSE GO TO     F25CQ-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012045 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25CQ-FN. EXIT.
      *N25CT.    NOTE *VALIDATE CHCR                      *.
       F25CT.    IF    QT89-CHCR = 0                                    lv10
                 NEXT SENTENCE ELSE GO TO     F25CT-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012053 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25CT-FN. EXIT.
      *N25CW.    NOTE *VALIDATE DCACG                     *.
       F25CW.    IF    QT89-DCACG = 0                                   lv10
                 NEXT SENTENCE ELSE GO TO     F25CW-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012786 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F25CW-FN. EXIT.
       F25-FN.   EXIT.
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *VALIDATE EXISTENCE OF SEGS         *
      *               *                                   *
      *               *************************************.
       F30.           EXIT.                                             lv05
      *N30CB.    NOTE *CLIENT KEY CHECK                   *.
       F30CB.                                                           lv10
           MOVE        QT89-CLID TO S-CLU01-CLID
           PERFORM     F94CL THRU F94CL-FN.
      *N30CE.    NOTE *CLIENT NOT FOUND                   *.
       F30CE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30CE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30CE-FN. EXIT.
       F30CB-FN. EXIT.
      *N30DB.    NOTE *BANK CLIENT KEY CHECK              *.
       F30DB.                                                           lv10
           MOVE        QT89-CLID4 TO S-CLU01-CLID
           PERFORM     F94CL THRU F94CL-FN.
      *N30DE.    NOTE *BANK CLIENT NOT FOUND              *.
       F30DE.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F30DE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F30DE-FN. EXIT.
       F30DB-FN. EXIT.
      *N30EC.    NOTE *GET/VALIDATE CLIENT ADDR SEQ NBR   *.
       F30EC.                                                           lv10
           PERFORM     F91CA THRU F91CA-FN
           MOVE        CA05-GECSQ TO QT89-GECSQ.
       F30EC-FN. EXIT.
       F30-FN.   EXIT.
      *N40CA.    NOTE *CALL ACF EXIT MODULE               *.            ADU031
       F40CA.                                                           lv10
           EXEC CICS   LINK PROGRAM (ACF-PROG)                          ADU031
                       COMMAREA (ACF-USER-AREA)                         ADU031
                       LENGTH (ACF-AREA-LEN)                 END-EXEC.  ADU031
       F40CA-FN. EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *DO CREATE LOGIC                    *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50CA.    NOTE *ENSURE CX01 EXISTS                 *.
       F50CA.                                                           lv10
           MOVE        QT89-CLID TO S-CXU01-CLID
           PERFORM     F94CX THRU F94CX-FN.
      *N50DC.    NOTE *IF CX01 FOUND - NO PROBLEM         *.
       F50DC.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F50DC-FN.
       F50DC-900. GO TO F50DE-FN.
       F50DC-FN. EXIT.
      *N50DE.    NOTE *ELSE... INSERT CX01                *.
       F50DE.                                                           lv15
           INITIALIZE  CX01
           MOVE        QT89-CLID TO CX01-CLID
           PERFORM     F94C1 THRU F94C1-FN
           MOVE        'Y' TO WX01-ISRT.
      *N50DG.    NOTE *IMS ERROR                          *.
       F50DG.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F50DG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012156 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50DG-FN. EXIT.
       F50DE-FN. EXIT.
       F50CA-FN. EXIT.
      *N50EC.    NOTE *GET FIRST CX18 FOR CLIENT          *.
       F50EC.                                                           lv10
           INITIALIZE  CX18
           MOVE        QT89-CLID TO S-CXU01-CLID
           PERFORM     F94CO THRU F94CO-FN.
       F50EC-FN. EXIT.
      *N50EE.    NOTE *FIND MATCHING CX18                 *.
       F50EE.    IF    IK = '0'                                         lv10
                 AND   (CX18-CLID NOT = QT89-CLID4
                 OR    CX18-NPBN NOT = QT89-NPBN
                 OR    CX18-CCBAT NOT = QT89-CCBAT)
                 NEXT SENTENCE ELSE GO TO     F50EE-FN.
      *MUST MATCH BANK CLID
      *PERSONAL BANK ACCT NUMBER
      **AND* ACCOUNT TYPE (CHK OR SAV)
      *N50EG.    NOTE *GET NEXT CX18 FOR CLIENT           *.
       F50EG.                                                           lv15
           PERFORM     F94CO THRU F94CO-FN.
       F50EG-FN. EXIT.
       F50EE-900. GO TO F50EE.
       F50EE-FN. EXIT.
      *N50EI.    NOTE *NEW CX18 NEEDED                    *.
       F50EI.    IF    CX18-CLID NOT = QT89-CLID4                       lv10
                 OR    CX18-NPBN NOT = QT89-NPBN
                 OR    CX18-CCBAT NOT = QT89-CCBAT
                 NEXT SENTENCE ELSE GO TO     F50EI-FN.
      *N50IC.    NOTE *GET MCSIG, IF NOT SUPPLIED         *.
       F50IC.    IF    QT89-MCSIG = SPACES                              lv15
                 NEXT SENTENCE ELSE GO TO     F50IC-FN.
           PERFORM     F91CL THRU F91CL-FN.
      *N50IE.    NOTE *PACK THE CLIENT NAME               *.
       F50IE.                                                           lv20
           PERFORM     F91NM THRU F91NM-FN
           MOVE        NM00-CLNM30 TO QT89-MCSIG.
       F50IE-FN. EXIT.
       F50IC-FN. EXIT.
      *N50IG.    NOTE *CREATE CX18                        *.
       F50IG.                                                           lv15
           INITIALIZE  CX18
           MOVE        +1 TO CX18-NBASQ
           MOVE        QT89-NPBN TO CX18-NPBN
           MOVE        QT89-CCBAT TO CX18-CCBAT
           MOVE        QT89-DCACG TO CX18-DACHP
           MOVE        05 TO CX18-CSTPRE
           MOVE        QT89-CLID4 TO CX18-CLID
           MOVE        QT89-MCSIG TO CX18-MCSIG
           MOVE        SPACE TO CX18-CPBNU
           MOVE        03 TO CX18-CSPCR
           MOVE        ZERO TO CX18-DAPCR.
       F50IG-FN. EXIT.
      *N50II.    NOTE *INSERT CX18 SEGMENT                *.
       F50II.                       GO TO     F50II-B.                  lv15
       F50II-A.
                 IF    IK = '0'
                                    GO TO     F50II-FN.
       F50II-B.
           PERFORM     F94C2 THRU F94C2-FN.
      *N50IK.    NOTE *CASE OF RETURN CODE                *.
       F50IK.         EXIT.                                             lv20
      *N50IM.    NOTE *INSERT SUCCESSFUL                  *.
       F50IM.    IF    XW05-XRC =                                       lv25
                       '  '
                 NEXT SENTENCE ELSE GO TO     F50IM-FN.
           MOVE        'Y' TO WX18-ISRT.
       F50IM-900. GO TO F50IK-FN.
       F50IM-FN. EXIT.
      *N50IO.    NOTE *DUPLICATE KEY                      *.
       F50IO.    IF    XW05-XRC =                                       lv25
                       'II'
                 NEXT SENTENCE ELSE GO TO     F50IO-FN.
           ADD         +1 TO CX18-NBASQ.
       F50IO-900. GO TO F50IK-FN.
       F50IO-FN. EXIT.
      *N50IQ.    NOTE *IMS ERROR                          *.
       F50IQ.                                                           lv25
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012609 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50IQ-FN. EXIT.
       F50IK-FN. EXIT.
       F50II-900. GO TO F50II-A.
       F50II-FN. EXIT.
      *N50IS.    NOTE *INSERT CX19 SEGMENT                *.
       F50IS.                                                           lv15
           MOVE        CX18-NBASQ TO S-CXU18-CX18K
           INITIALIZE  CX19
           PERFORM     F94C6 THRU F94C6-FN.
       F50IS-FN. EXIT.
       F50EI-FN. EXIT.
      *N50LA.    NOTE *REPOSITION ON CX01                 *.
       F50LA.                                                           lv10
           MOVE        QT89-CLID TO S-CXU01-CLID
           PERFORM     F94CX THRU F94CX-FN.
       F50LA-FN. EXIT.
      *N50LC.    NOTE *GET FIRST CX03 FOR CLIENT          *.
       F50LC.                                                           lv10
           INITIALIZE  CX03 CX03-CY01 CX03-CY50
           MOVE        QT89-CLID TO S-CXU01-CLID
           PERFORM     F94CQ THRU F94CQ-FN.
       F50LC-FN. EXIT.
      *N50LE.    NOTE *FIND MATCHING CX03                 *.
       F50LE.    IF    IK = '0'                                         lv10
                 AND   (CX03-CARTY NOT = 01
                 OR    CX03-NBASQ NOT =
                       CX18-NBASQ)
                 NEXT SENTENCE ELSE GO TO     F50LE-FN.
      *N50LG.    NOTE *GET NEXT CX03 FOR CLIENT           *.
       F50LG.                                                           lv15
           PERFORM     F94CQ THRU F94CQ-FN.
       F50LG-FN. EXIT.
       F50LE-900. GO TO F50LE.
       F50LE-FN. EXIT.
      *N50MC.    NOTE *NEW CX03 NEEDED                    *.
       F50MC.    IF    CX03-CARTY NOT = 01                              lv10
                 OR    CX03-NBASQ NOT = CX18-NBASQ
                 NEXT SENTENCE ELSE GO TO     F50MC-FN.
      *N50ME.    NOTE *FORMAT CX03                        *.
       F50ME.                                                           lv15
           INITIALIZE  CX03
           MOVE        +1 TO CX03-NARRS
           MOVE        +144 TO CX03-GELL
           MOVE        01 TO CX03-CARST
           MOVE        QT89-GECSQ TO CX03-GECSQ
           MOVE        01 TO CX03-CARTY
           MOVE        ZERO TO CX03-CPMTG
           MOVE        ZERO TO CX03-GRCRNG
           MOVE        ZERO TO CX03-DEXDT
           MOVE        ZERO TO CX03-APMTLA
           MOVE        QT89-DCACG TO CX03-DASUP
           MOVE        CX18-NBASQ TO CX03-NBASQ
           MOVE        'N' TO CX03-ICPCI
           MOVE        001 TO CX03-CLUPD
           MOVE        QT89-CWRC TO CX03-CWRC
           MOVE        QT89-CHCR TO CX03-CHCR
           MOVE        ACF-USER-ID TO CX03-GEOPD2
           MOVE        ACF-USER-UNIT TO CX03-GEAUN
           MOVE        ZERO TO CX03-DPCHD
           MOVE        ZEROS TO CX03-DNPMT
           MOVE        QT89-DCACG TO CX03-DLAUP
           MOVE        QT89-CSTEC TO CX03-CSTEC
           MOVE        ZEROS TO CX03-DLRCHK
           MOVE        ZEROS TO CX03-QTRCHK.
       F50ME-FN. EXIT.
      *N50MG.    NOTE *INSERT CX03 SEGMENT                *.
       F50MG.                       GO TO     F50MG-B.                  lv15
       F50MG-A.
                 IF    IK = '0'
                                    GO TO     F50MG-FN.
       F50MG-B.
           PERFORM     F94C3 THRU F94C3-FN.
      *N50MK.    NOTE *CASE OF RETURN CODE                *.
       F50MK.         EXIT.                                             lv20
      *N50MM.    NOTE *INSERT SUCCESSFUL                  *.
       F50MM.    IF    XW05-XRC =                                       lv25
                       '  '
                 NEXT SENTENCE ELSE GO TO     F50MM-FN.
           MOVE        'Y' TO WX03-ISRT.
       F50MM-900. GO TO F50MK-FN.
       F50MM-FN. EXIT.
      *N50MO.    NOTE *DUPLICATE KEY                      *.
       F50MO.    IF    XW05-XRC =                                       lv25
                       'II'
                 NEXT SENTENCE ELSE GO TO     F50MO-FN.
           ADD         +1 TO CX03-NARRS.
       F50MO-900. GO TO F50MK-FN.
       F50MO-FN. EXIT.
      *N50MQ.    NOTE *IMS ERROR                          *.
       F50MQ.                                                           lv25
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012074 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50MQ-FN. EXIT.
       F50MK-FN. EXIT.
       F50MG-900. GO TO F50MG-A.
       F50MG-FN. EXIT.
       F50MC-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *RETURN VALUES                      *
      *               *                                   *
      *               *************************************.
       F55.           EXIT.                                             lv05
      *N55CA.    NOTE *RETURN SEQUENCE NUMBERS            *.
       F55CA.                                                           lv10
           MOVE        CX18-NBASQ TO QT89-NBASQ
           MOVE        CX03-NARRS TO QT89-NARRS.
       F55CA-FN. EXIT.
       F55-FN.   EXIT.
      *N75.      NOTE *************************************.
      *               *                                   *
      *               *MISCELLANEOUS FINAL UPDATES        *
      *               *                                   *
      *               *************************************.
       F75.      IF    WX01-ISRT = 'Y'                                  lv05
                 OR    WX03-ISRT = 'Y'
                 OR    WX18-ISRT = 'Y'
                 NEXT SENTENCE ELSE GO TO     F75-FN.
      *N75CA.    NOTE *GET CX01 SEQUENCE NUMBER           *.
       F75CA.                                                           lv10
           MOVE        QT89-CLID TO S-CXU01-CLID
           PERFORM     F94C4 THRU F94C4-FN.
       F75CA-FN. EXIT.
      *N75DC.    NOTE *IF SEQUENCE BELOW LIMIT            *.
       F75DC.    IF    CX01-NSEQ4B < 90000000                           lv10
                 NEXT SENTENCE ELSE GO TO     F75DC-FN.
           ADD         +1 TO CX01-NSEQ4B.
      *N75DE.    NOTE *ELSE... START AT ONE AGAIN         *.
       F75DE.                                                           lv20
           MOVE        +1 TO CX01-NSEQ4B.
       F75DE-FN. EXIT.
      *N75DJ.    NOTE *REPLACE SEGMENT                    *.
       F75DJ.                                                           lv20
           MOVE        QT89-DCACG TO CX01-GEMDA
           PERFORM     F94C5 THRU F94C5-FN.
       F75DJ-FN. EXIT.
       F75DC-FN. EXIT.
      *N75FA.    NOTE *FORMAT AUDIT LOG - BANK UPDATE     *.
       F75FA.                                                           lv10
           PERFORM     F83EA THRU F83EA-FN.
       F75FA-FN. EXIT.
      *N75GA.    NOTE *COMMUNICATION DB UPDATE            *.
       F75GA.                                                           lv10
           MOVE        0059 TO GU01-GELL
           MOVE        QT89-CLID TO GU01-CANUMB
           MOVE        '1' TO GU01-IDTYP
           MOVE        SPACES TO GU01-CRSTA
           MOVE        QT89-DCACG TO GU01-DCACG
           MOVE        PROGR TO GU01-MRPID
           MOVE        03 TO GU01-CCOPY
           MOVE        CX03-CARTY TO GU01-CARTY
           MOVE        CX03-NARRS TO GU01-NARRS
           MOVE        ZERO TO GU01-NSEQ2P
           MOVE        0200 TO GU01-CREQT
           MOVE        '001' TO 7-GU01-CLUPD
           MOVE        ZERO TO 7-GU01-CACOD.
       F75GA-FN. EXIT.
      *N75GD.    NOTE *EXECUTE CCDS MAILBOX MACROS        *.
       F75GD.                                                           lv10
           PERFORM     F81BA THRU F81BA-FN.
       F75GD-FN. EXIT.
      *N75GT.    NOTE *IF RECORD ALREADY EXIST            *.
       F75GT.    IF    7-GU01-XCORET = 'II'                             lv10
                 NEXT SENTENCE ELSE GO TO     F75GT-FN.
           PERFORM     F82EA THRU F82EA-FN.
       F75GT-FN. EXIT.
      *N75QT.    NOTE *RETURN CX01 KEYS TO CALLER         *.
       F75QT.                                                           lv10
           MOVE        CX01-GEMDA TO QT89-GEMDA
           MOVE        CX01-NSEQ4B TO QT89-NSEQ4B.
       F75QT-FN. EXIT.
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
      *N81BA.    NOTE *DETERMINE IPDT PARAGRAPH           *.            AACC20
       F81BA.         EXIT.                                             lv10
      *N81BB.    NOTE *ESTABLISH LEVEL 15                 *.            AACC20
       F81BB.                                                           lv15
      *                                                                 AACC20
      *SET THE VARIABLE SEGMENT LENGTH                                  AACC20
           MOVE        0059 TO GU01-GELL.                               AACC20
      *N81BD.    NOTE *ALL PURPOSE CODE = 1               *.            AACC20
       F81BD.    IF    7-GU01-CACOD = 1                                 lv20
                 NEXT SENTENCE ELSE GO TO     F81BD-FN.                 AACC20
      *IF INSURANCE PMT WENT FROM                                       AACC20
      *PENDING TO ACTIVE, SET TABLE KEY                                 AACC20
           MOVE        03 TO GU01-NLPGH                                 AACC20
      *INSERT INTO 'MAILBOX'                                            AACC20
               GO TO     F81BB-FN.                                      AACC20
       F81BD-FN. EXIT.
      *N81BE.    NOTE *ALL PURPOSE CODE = 2               *.            AACC20
       F81BE.    IF    7-GU01-CACOD = 2                                 lv20
                 NEXT SENTENCE ELSE GO TO     F81BE-FN.                 AACC20
      *IF A SETUP OCCURED WITH AS-OF                                    AACC20
      *PAYMENTS, SET TABLE KEY                                          AACC20
                 IF    7-GU01-CLUPD = 001                               DOT
           MOVE        05 TO GU01-NLPGH                                 AACC20
      *INSERT INTO CLIENT COMM DB                                       AACC20
               GO TO     F81BB-FN.                                      AACC20
       F81BE-FN. EXIT.
      *N81BF.    NOTE *SUB-FUNC BF-BT FOR FUTURE USE      *.            AACC20
       F81BF.         EXIT.                                             lv20
       F81BF-FN. EXIT.
      *N81BU.    NOTE *CHECK LAST UPDATE CODE             *.            AACC20
       F81BU.    IF    7-GU01-CACOD = 0                                 lv20
                 NEXT SENTENCE ELSE GO TO     F81BU-FN.                 AACC20
                 IF    7-GU01-CLUPD = 001                               DOT
      *IF LAST UPDATE IS SETUP                                          AACC20
           MOVE        02 TO GU01-NLPGH                                 AACC20
               GO TO     F81BB-FN.                                      AACC20
                 IF    7-GU01-CLUPD = 016                               DOT
      *IF LAST UPDATE IS ON-DEMAND                                      AACC20
           MOVE        01 TO GU01-NLPGH                                 AACC20
               GO TO     F81BB-FN.                                      AACC20
      *LAST UPDATE WAS A CHANGE                                         AACC20
           MOVE        04 TO GU01-NLPGH.                                AACC20
       F81BU-FN. EXIT.
       F81BB-FN. EXIT.
      *N81BW.    NOTE *SAVE TRANSACTION INFO              *.            AACC20
       F81BW.                                                           lv15
           MOVE        GU01 TO WX01.                                    AACC20
       F81BW-FN. EXIT.
      *N81BY.    NOTE *PROCESS REQUESTED TRANSACTION      *.            AACC20
       F81BY.                                                           lv15
           ADD         1 TO WX01-NSEQ2P GU01-NSEQ2P                     AACC20
      *INSERT TRANSACTION INTO MAILBOX                                  AACC20
           PERFORM     F81I1 THRU F81I1-FN                              AACC20
           MOVE        XW05-XRC TO 7-GU01-XCORET.
                 IF    7-GU01-XCORET NOT = 'II'                         DOT
      *IF INSERT WAS SUCCESSFUL OR
      *INSERT FAILED FOR REASON OTHER                                   AACC20
      *THAN SEGMENT ALREADY EXISTS,                                     AACC20
      *END SELECTION PROCESS                                            AACC20
               GO TO     F81BA-FN.                                      AACC20
      *GET-HOLD-UNIQUE EXISTING TRAN                                    AACC20
           MOVE        GU01-CANUMB TO S-GUU01-CANUMB                    AACC20
           MOVE        GU01-CREQT TO S-GUU01-CREQT                      AACC20
           MOVE        GU01-NSEQ2P TO S-GUU01-NSEQ2P                    AACC20
           PERFORM     F81H1 THRU F81H1-FN.                             AACC20
                 IF    IK NOT = 0                                       DOT
      *CHECK FOR GOOD GET-HOLD-UNIQUE                                   AACC20
               GO TO     F81BA-FN.                                      AACC20
                 IF    GU01-NARRS NOT = WX01-NARRS                      DOT
                 OR    GU01-CARTY NOT = WX01-CARTY                      AACC20
      *INSERT FAILED - COMPARE THE                                      AACC20
      *BUMP UP THE SEQUENCE KEY FIELD                                   AACC20
      *IF THEY ARE NOT THE SAME THEN                                    AACC20
      *RE-INSERT THE REQUESTED TRAN                                     AACC20
      *MOVE IN THE REQUESTED TRAN DATA                                  AACC20
           MOVE        WX01 TO GU01                                     AACC20
      *RE-START INSERT PROCESS                                          AACC20
           GO TO  F81BY.                                                AACC20
       F81BY-FN. EXIT.
       F81BA-FN. EXIT.
      *N81H1.    NOTE *CALL GHU ON GU01                   *.            ADU026
       F81H1.                                                           lv10
           MOVE        'AGCP' TO DE10-XDBDNM                            ADU026
           MOVE        'GU01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XC06 GU01                                                    ADU026
           S-GUU01-SSA                                                  ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F81H1-FN. EXIT.
      *N81I1.    NOTE *CALL ISRT ON GU01                  *.            ADU026
       F81I1.                                                           lv10
           MOVE        'AGCP' TO DE10-XDBDNM                            ADU026
           MOVE        'GU01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XC06 GU01                                                    ADU026
           S-GU01-SSA                                                   ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F81I1-FN. EXIT.
      *N82BA.    NOTE *CALL REPL ON GU01                  *.            ADU026
       F82BA.                                                           lv10
           MOVE        'AGCP' TO DE10-XDBDNM                            ADU026
           MOVE        'GU01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XC06 GU01                                                    ADU026
           MOVE        XC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F82BA-FN. EXIT.
      *N82EA.    NOTE *REQUEST TYPE DECISION PROCESS      *.            AACC30
       F82EA.         EXIT.                                             lv10
      *N82ED.    NOTE *ESTABLISH LEVEL 15                 *.            AACC30
       F82ED.                                                           lv15
      *WX01-NLPGH WILL CONTAIN THE                                      AACC30
      *PARAGRAPH KEY OF THE REQUESTED                                   AACC30
      *TRANSACTION                                                      AACC30
      *                                                                 AACC30
      *GU01-NLPGH WILL CONTAIN THE                                      AACC30
      *PARAGRAPH KEY OF THE EXISTING                                    AACC30
      *TRANSACTION                                                      AACC30
           MOVE        SPACES TO 7-GU01-RTN-CD.                         AACC30
      *N82EF.    NOTE *COMPARE PARAGRAPHS                 *.            AACC30
       F82EF.    IF    WX01-NLPGH = 01                                  lv20
                 NEXT SENTENCE ELSE GO TO     F82EF-FN.                 AACC30
      *CHECK NEW SEQ FOR 'ON DEMAND'                                    AACC30
      *SUPPRESS NEW PARAGRAPH                                           AACC30
               GO TO     F82EA-FN.                                      AACC30
       F82EF-FN. EXIT.
      *N82EG.    NOTE *CHECK NEW SEQ FOR 'SETUP' AND      *.            AACC30
       F82EG.    IF    WX01-NLPGH = 02                                  lv20
                 OR    WX01-NLPGH = 05                                  AACC30
                 NEXT SENTENCE ELSE GO TO     F82EG-FN.                 AACC30
      *'AS-OF SETUP'                                                    AACC30
      *INVALID PARAGRAPH MATCH                                          AACC30
           MOVE        'N' TO 7-GU01-RTN-CD                             AACC30
               GO TO     F82EA-FN.                                      AACC30
       F82EG-FN. EXIT.
      *N82EH.    NOTE *CHECK NEW SEQ FOR 'INSPNT'         *.            AACC30
       F82EH.    IF    WX01-NLPGH = 03                                  lv20
                 NEXT SENTENCE ELSE GO TO     F82EH-FN.                 AACC30
                 IF    GU01-NLPGH = 01                                  DOT
      *CHECK OLD SEQ FOR 'ON DEMAND'                                    AACC30
      *REPLACE OLD TRAN WITH NEW REQ                                    AACC30
               GO TO     F82ED-FN.                                      AACC30
                 IF    GU01-NLPGH = 03                                  DOT
                 OR    GU01-NLPGH = 06                                  AACC30
      *CHECK OLD SEQ FOR 'INS PMT'                                      AACC30
      *SUPPRESS NEW PARAGRAPH                                           AACC30
               GO TO     F82EA-FN.                                      AACC30
                 IF    GU01-NLPGH = 04                                  DOT
      *CHECK OLD SEQ FOR 'CHANGES'                                      AACC30
      *REPLACE TRAN WITH DIFF PARAGPH                                   AACC30
           MOVE        06 TO WX01-NLPGH                                 AACC30
               GO TO     F82ED-FN.                                      AACC30
                 IF    GU01-NLPGH = 02                                  DOT
      *CHECK FOR OLD SEQ FOR 'SETUP'                                    AACC30
      *AND SUPPRESS 'NEW' PARAGRAPH                                     AACC30
               GO TO     F82EA-FN.                                      AACC30
      *INVALID PARAGRAPH MATCH                                          AACC30
           MOVE        'N' TO 7-GU01-RTN-CD                             AACC30
               GO TO     F82EA-FN.                                      AACC30
       F82EH-FN. EXIT.
      *N82EJ.    NOTE *CHECK NEW SEQ FOR 'CHANGES'        *.            AACC30
       F82EJ.    IF    WX01-NLPGH = 04                                  lv20
                 NEXT SENTENCE ELSE GO TO     F82EJ-FN.                 AACC30
                 IF    GU01-NLPGH = 01                                  DOT
      *CHECK OLD SEQ FOR 'ON DEMAND'                                    AACC30
      *REPLACE OLD TRAN WITH NEW REQ                                    AACC30
               GO TO     F82ED-FN.                                      AACC30
                 IF    GU01-NLPGH = 02                                  DOT
                 OR    GU01-NLPGH = 04                                  AACC30
                 OR    GU01-NLPGH = 05                                  AACC30
                 OR    GU01-NLPGH = 06                                  AACC30
      *CHECK OLD SEQ FOR 'SETUP',                                       AACC30
      *'INS PMT', 'SETUP AS-OF'                                         AACC30
      *AND 'CHGS INS PMT'                                               AACC30
      *SUPPRESS NEW PARAGRAPH                                           AACC30
               GO TO     F82EA-FN.                                      AACC30
                 IF    GU01-NLPGH = 03                                  DOT
      *REPLACE TRAN WITH DIFF PARAGRPH                                  AACC30
           MOVE        06 TO WX01-NLPGH                                 AACC30
               GO TO     F82ED-FN.                                      AACC30
      *INVALID PARAGRAPH MATCH                                          AACC30
           MOVE        'N' TO 7-GU01-RTN-CD                             AACC30
               GO TO     F82EA-FN.                                      AACC30
       F82EJ-FN. EXIT.
       F82ED-FN. EXIT.
      *N8202.    NOTE *REPLACE OLD TRAN WITH NEW          *.            AACC30
       F8202.                                                           lv15
      *MOVE SAVED REQUESTED TRANSACTION                                 AACC30
      *TO 'MAILBOX' RECORD                                              AACC30
           MOVE        WX01 TO GU01                                     AACC30
      *WRITE TO 'MAILBOX'                                               AACC30
           PERFORM     F82BA THRU F82BA-FN.                             AACC30
       F8202-FN. EXIT.
       F82EA-FN. EXIT.
      *N83EA.    NOTE *LOAD AUDIT LOG AFTER IMAGE         *.
       F83EA.                                                           lv10
           INITIALIZE  DH10
           MOVE        '00001' TO DH10-CAUAC
           MOVE        '70002' TO DH10-CAUFR
           MOVE        CX03-NARRS TO VA01-NARRS
           MOVE        CX03-CARTY TO VA01-CARTY
           MOVE        CX03-ICPCI TO VA01-ICPCI
           MOVE        CX03-CWRC TO VA01-CWRC
           MOVE        CX03-CHCR TO VA01-CHCR
           MOVE        CX03-CARST TO VA01-CARST
           MOVE        CX03-CLUPD TO VA01-CLUPD
           MOVE        CX03-DLAUP TO VA01-DLAUP
           MOVE        CX03-DPCHD TO VA01-DPCHD
           MOVE        CX18-CSTPRE TO VA01-CSTPRE
           MOVE        CX18-MCSIG TO VA01-MCSIG
           MOVE        CX18-NPBN TO VA01-NPBN
           MOVE        CX18-CCBAT TO VA01-CCBAT
           MOVE        CX18-DACHP TO VA01-DACHP
           MOVE        QT89-NTR TO VA01-NTR
           MOVE        QT89-GECKD TO VA01-GECKD1
           MOVE        VA01 TO DH10-GAUVR
           PERFORM     F96AL THRU F96AL-FN.
       F83EA-FN. EXIT.
       F9099-ITER-FN.  GO TO F05.
      *N91.      NOTE *************************************.
      *               *                                   *
      *               *CALLED MODULES                     *
      *               *                                   *
      *               *************************************.
       F91.           EXIT.                                             lv05
      *N91CA.    NOTE *CALL CI0006 - CLIENT ADDRESS       *.            AM0006
       F91CA.                                                           lv10
      *                                                                 AM0006
      *********************************                                 AM0006
      ** THIS MODULE WILL READ THE    *                                 AM0006
      ** CLIENT DATABASE TO GET THE   *                                 AM0006
      ** REQUESTED CLIENT'S ADDRESS.  *                                 AM0006
      ** IF CA05-GECSQ IS NOT EQUAL TO*                                 AM0006
      ** ZEROS, THAN THE MODULE WILL  *                                 AM0006
      ** RETURN THE SPECIFIED ADDRESS.*                                 AM0006
      ** IF CA05-GECSQ IS EQUAL TO    *                                 AM0006
      ** ZEROS, THAN THE MODULE WILL  *                                 AM0006
      ** RETURN THE FIRST ACTIVE      *                                 AM0006
      ** ADDRESS.                     *                                 AM0006
      *********************************                                 AM0006
      *                                                                 AM0006
           INITIALIZE      CA05                                         AM0006
           INITIALIZE  DE10-DU03
           INITIALIZE  MS03
           MOVE        QT89-CLID TO CA05-CLID                           AM0006
           MOVE        ZEROS TO CA05-GECSQ1                             AM0006
           MOVE        QT89-DCACG TO CA05-DCACG                         AM0006
           SET CI0006-PCB-CL1P-PTR1 TO                                  AM0006
                       PCB-CL1P-PTR1                                    AM0006
           INITIALIZE DE10-DU03                                         AM0006
           CALL        CI0006 USING                                     AM0006
           DFHEIBLK                                                     AM0006
           DFHCOMMAREA                                                  AM0006
           DLIUIBII                                                     AM0006
           CI0006-PCB-ADDRESS-LIST                                      AM0006
           CA05                                                         AM0006
           DE10                                                         AM0006
           MS03.                                                        AM0006
           MOVE        CI0006 TO W-PASS-XPROGR
           PERFORM     F92ET THRU F92ET-FN
           PERFORM     F92MS THRU F92MS-FN.
       F91CA-FN. EXIT.
      *N91CL.    NOTE *CALL CI0055 - GROUPS FOR CLIENT    *.            AM0055
       F91CL.                                                           lv10
      *                                                                 AM0055
      *********************************                                 AM0055
      ** THIS MODULE WILL READ THE    *                                 AM0055
      ** CAMS CLIENT CL01, CL03, &    *                                 AM0055
      ** CL12 SEGMENTS FOR THE CLID   *                                 AM0055
      ** PASSED.                      *                                 AM0055
      *********************************                                 AM0055
      *                                                                 AM0055
           INITIALIZE  DE10-DU03
           INITIALIZE  MS03
           INITIALIZE  WZ03                                             AM0055
           INITIALIZE  MX11                                             AM0055
           MOVE        QT89-CLID TO                                     AM0055
           WZ03-CLID                                                    AM0055
            SET CI0055-PCB-CL1P-PTR1 TO                                 AM0055
                         PCB-CL1P-PTR1                                  AM0055
           INITIALIZE  DE10-DU03                                        AM0055
           CALL        CI0055 USING                                     AM0055
           DFHEIBLK                                                     AM0055
           DFHCOMMAREA                                                  AM0055
           DLIUIBII                                                     AM0055
           CI0055-PCB-ADDRESS-LIST                                      AM0055
           WZ03                                                         AM0055
           DE10                                                         AM0055
           MS03                                                         AM0055
           MX11.                                                        AM0055
           MOVE        CI0055 TO W-PASS-XPROGR
           PERFORM     F92ET THRU F92ET-FN
           PERFORM     F92MS THRU F92MS-FN.
       F91CL-FN. EXIT.
      *N91NM.    NOTE *REFORMAT CLIENT NAME               *.
       F91NM.                                                           lv10
           MOVE        WZ03-CLNAM TO NM00-CLNAM
           MOVE        SPACES TO NM00-CLNM67
           NM00-GERTC
           CALL        'CMU016DY' USING NM00.
       F91NM-FN. EXIT.
       F91-FN.   EXIT.
      *N92ET.    NOTE *DL1 ERROR                          *.
       F92ET.    IF    DE10-NMESS2 > ZERO                               lv10
                 NEXT SENTENCE ELSE GO TO     F92ET-FN.
           COMPUTE     IMS03R = DE10-QELLAA + 2
           MOVE        W-PASS-XPROGR TO
           DE10-TMESS4 (IMS03R:6)
           ADD         +7 TO DE10-QELLAA
           MOVE                     ALL '1' TO FT GO TO F20.
       F92ET-FN. EXIT.
      *N92MS.    NOTE *NON DL1 ERROR                      *.
       F92MS.    IF    MS03-NMESS2 > ZERO                               lv10
                 NEXT SENTENCE ELSE GO TO     F92MS-FN.
           COMPUTE     IMS03R = MS03-QELLAA + 2
           MOVE        W-PASS-XPROGR TO
           MS03-TMESS4 (IMS03R : 6)
           ADD         +7 TO MS03-QELLAA
           MOVE                     ALL '1' TO FT GO TO F20.
       F92MS-FN. EXIT.
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
      *N94CL.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CL.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XB06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        XB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
      *N94CO.    NOTE *CALL GN ON CX18                    *.            ADU026
       F94CO.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CX18                                                    ADU026
           S-CXU01-SSA S-CX18-SSA                                       ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CO-FN. EXIT.
      *N94CQ.    NOTE *CALL GN ON CX03                    *.            ADU026
       F94CQ.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           XA06 CX03                                                    ADU026
           S-CXU01-SSA S-CX03-SSA                                       ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CQ-FN. EXIT.
      *N94CX.    NOTE *CALL GU ON CX01                    *.            ADU026
       F94CX.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           XA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CX-FN. EXIT.
      *N94C1.    NOTE *CALL ISRT ON CX01                  *.            ADU026
       F94C1.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 CX01                                                    ADU026
           S-CX01-SSA                                                   ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C1-FN. EXIT.
      *N94C2.    NOTE *CALL ISRT ON CX18                  *.            ADU026
       F94C2.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX18' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 CX18                                                    ADU026
           S-CXU01-SSA S-CX18-SSA                                       ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C2-FN. EXIT.
      *N94C3.    NOTE *CALL ISRT ON CX03                  *.            ADU026
       F94C3.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX03' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 CX03                                                    ADU026
           S-CXU01-SSA S-CX03-SSA                                       ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C3-FN. EXIT.
      *N94C4.    NOTE *CALL GHU ON CX01                   *.            ADU026
       F94C4.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           XA06 CX01                                                    ADU026
           S-CXU01-SSA                                                  ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C4-FN. EXIT.
      *N94C5.    NOTE *CALL REPL ON CX01                  *.            ADU026
       F94C5.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           XA06 CX01                                                    ADU026
           S-CX01-SSA                                                   ADU026
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C5-FN. EXIT.
      *N94C6.    NOTE *CALL ISRT ON CX19                  *.            ADU026
       F94C6.                                                           lv10
           MOVE        'AR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CX19' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           XA06 CX19                                                    ADU026
           S-CXU01-SSA S-CXU18-SSA                                      ADU026
           S-CX19-SSA
           MOVE        XA06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94C6-FN. EXIT.
      *N96AL.    NOTE *---> Audit Log Process             *.            ADU165
       F96AL.         EXIT.                                             lv10
      *N96AN.    NOTE *---> Format Audit Log Data         *.            ADU165
       F96AN.                                                           lv15
           SET AL00-NPNTR                                               ADU165
           TO ADDRESS OF DLIUIBII                                       ADU165
           MOVE        AL00-ADDR TO DH10-XUIBP                          ADU165
           MOVE        AL00-NSEQ2P TO DH10-NSEQ2P                       ADU165
           MOVE        'E' TO DH10-CAUL                                 ADU165
           MOVE        'CLIENT' TO DH10-MAUSB                           ADU165
           MOVE        QT89-CLID TO DH10-NAUSK                          ADU165
           MOVE        'CATS' TO DH10-CSYS                              ADU165
           MOVE        EIBTRNID TO DH10-CAPPL                           ADU165
           MOVE        'C' TO DH10-CAUSR                                ADU165
           MOVE        ACF-USER-ID TO DH10-GEOPID                       ADU165
           MOVE        ACF-USER-UNIT TO DH10-CAUNIT                     ADU165
      *                                                                 ADU165
      *---> Execute Audit Log Write                                     ADU165
           EXEC CICS   LINK PROGRAM ('DBI110P')                         ADU165
                       LENGTH (495)                                     ADU165
                       COMMAREA (DH10)                       END-EXEC.  ADU165
       F96AN-FN. EXIT.
      *N96AP.    NOTE *---> Audit Log failed              *.            ADU165
       F96AP.    IF    DH10-GERTC NOT = 'Y'                             lv15
                 NEXT SENTENCE ELSE GO TO     F96AP-FN.                 ADU165
      *     Use macro ADU119 to                                         ADU165
      *     send error 012038                                           ADU165
      *     and  Terminate...                                           ADU165
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012038 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F96AP-900. GO TO F96AX-FN.
       F96AP-FN. EXIT.
      *N96AX.    NOTE *---> Audit Logs Created OK         *.            ADU165
       F96AX.                                                           lv15
      *     Increment Sequence No                                       ADU165
      *     and initialize log segment                                  ADU165
           ADD         1 TO AL00-NSEQ2P                                 ADU165
           INITIALIZE  DH10.                                            ADU165
       F96AX-FN. EXIT.
       F96AL-FN. EXIT.
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
