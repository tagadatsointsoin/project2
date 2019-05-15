       IDENTIFICATION DIVISION.                                         CI0070
       PROGRAM-ID.  CI0070P.                                            CI0070
      *AUTHOR.         RETRIEVE W/H INSTRUCTIONS.                       CI0070
      *DATE-COMPILED.   09/08/14.                                       CI0070
       ENVIRONMENT DIVISION.                                            CI0070
       CONFIGURATION SECTION.                                           CI0070
       SOURCE-COMPUTER. IBM-370.                                        CI0070
       OBJECT-COMPUTER. IBM-370.                                        CI0070
       DATA DIVISION.                                                   CI0070
       WORKING-STORAGE SECTION.                                         CI0070
      *                                                                 AMDU14
      ******************************************************************AMDU14
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU14
      **     REQUESTED TYPE OF CLIENTS FOR THE ACCOUNT NUMBER PASSED.  *AMDU14
      ******************************************************************AMDU14
      *                                                                 AMDU14
      *!WF DSP=AC DSL=DU SEL=14 FOR=I LEV=1                             AMDU14
       01                 AC00.                                         CI0070
          05              AC00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00917).                  CI0070
       01                 AC14  REDEFINES      AC00.                    CI0070
            10            AC14-C299.                                    CI0070
            11            AC14-CTID.                                    CI0070
            12            AC14-CTIDA  PICTURE  9(3).                    CI0070
            12            AC14-CTIDN.                                   CI0070
            13            AC14-CTIDNP PICTURE  X(13).                   CI0070
            13            AC14-CTIDND PICTURE  9(11).                   CI0070
            10            AC14-DCACG  PICTURE  9(8).                    CI0070
            10            AC14-IPOCH  PICTURE  X.                       CI0070
            10            AC14-FILLER PICTURE  X(100).                  CI0070
            10            AC14-CLID01.                                  CI0070
            11            AC14-CLIDO1 PICTURE  X(3).                    CI0070
            11            AC14-NCLID1.                                  CI0070
            12            AC14-CLIDP1 PICTURE  X(12).                   CI0070
            12            AC14-CLIDNA PICTURE  9(8).                    CI0070
            10            AC14-CLCTR  PICTURE  9(3).                    CI0070
            10            AC14-DU21                                     CI0070
                          OCCURS       025     TIMES.                   CI0070
            11            AC14-C199.                                    CI0070
            12            AC14-CLID.                                    CI0070
            13            AC14-CLIDO  PICTURE  9(3).                    CI0070
            13            AC14-CLIDN.                                   CI0070
            14            AC14-CLIDNP PICTURE  X(12).                   CI0070
            14            AC14-CLIDND PICTURE  9(8).                    CI0070
            11            AC14-CLCTRC PICTURE  9(3).                    CI0070
            10            AC14-QITEM  PICTURE  9(3).                    CI0070
            10            AC14-XIMAX  PICTURE  S9(4)                    CI0070
                          BINARY.                                       CI0070
            10            AC14-CRROL  PICTURE  X.                       CI0070
            10            AC14-FILLER PICTURE  X(099).                  CI0070
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=AW DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 AW00.                                         CI0070
          05              AW00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00541).                  CI0070
       01                 AW34  REDEFINES      AW00.                    CI0070
            10            AW34-C299.                                    CI0070
            11            AW34-CTID.                                    CI0070
            12            AW34-CTIDA  PICTURE  9(3).                    CI0070
            12            AW34-CTIDN.                                   CI0070
            13            AW34-CTIDNP PICTURE  X(13).                   CI0070
            13            AW34-CTIDND PICTURE  9(11).                   CI0070
            10            AW34-DCACG  PICTURE  9(8).                    CI0070
            10            AW34-IPOCH  PICTURE  X.                       CI0070
            10            AW34-FILLER PICTURE  X(100).                  CI0070
            10            AW34-CT22                                     CI0070
                          OCCURS       010     TIMES.                   CI0070
            11            AW34-CT22K.                                   CI0070
            12            AW34-CGVEN  PICTURE  X(2).                    CI0070
            12            AW34-CTWHC  PICTURE  9(2).                    CI0070
            11            AW34-CFCNTY PICTURE  X(2).                    CI0070
            11            AW34-DLAUP  PICTURE  9(8).                    CI0070
            11            AW34-CTWTC  PICTURE  9(2).                    CI0070
            11            AW34-CTWHAT PICTURE  S9(7)V99                 CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            AW34-CTWHP  PICTURE  9(3)V99                  CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            AW34-FILLER PICTURE  X(06).                   CI0070
            10            AW34-QITEM  PICTURE  9(3).                    CI0070
            10            AW34-XIMAX  PICTURE  S9(4)                    CI0070
                          BINARY.                                       CI0070
            10            AW34-CTXMT  PICTURE  9(2).                    CI0070
            10            AW34-CTCUS  PICTURE  999.                     CI0070
            10            AW34-FILLER PICTURE  X(95).                   CI0070
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
       01                 CL00.                                         CI0070
            02            CL01.                                         CI0070
            10            CL01-CL01K.                                   CI0070
            11            CL01-C199.                                    CI0070
            12            CL01-CLID.                                    CI0070
            13            CL01-CLIDO  PICTURE  9(3).                    CI0070
            13            CL01-CLIDN.                                   CI0070
            14            CL01-CLIDNP PICTURE  X(12).                   CI0070
            14            CL01-CLIDND PICTURE  9(8).                    CI0070
            10            CL01-GECKD  PICTURE  9.                       CI0070
            10            CL01-GEMDA  PICTURE  9(8).                    CI0070
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0070
                          BINARY.                                       CI0070
            10            CL01-GECUC  PICTURE  99.                      CI0070
            10            CL01-CLDOR  PICTURE  9(8).                    CI0070
            10            CL01-CLLNG  PICTURE  XX.                      CI0070
            10            CL01-GESLC  PICTURE  99.                      CI0070
            10            CL01-CLTYP  PICTURE  X.                       CI0070
            10            CL01-CLCLS  PICTURE  9(3).                    CI0070
            10            CL01-CLTWRC PICTURE  99.                      CI0070
            10            CL01-CLPVC  PICTURE  99.                      CI0070
            10            CL01-CLIND  PICTURE  9(3).                    CI0070
            10            CL01-CLTRC  PICTURE  99.                      CI0070
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            CL01-AYSIDA PICTURE  9(3).                    CI0070
            10            CL01-AYSID  PICTURE  9(5).                    CI0070
            10            CL01-CLSTR  PICTURE  9(2).                    CI0070
            10            CL01-CLC11  PICTURE  X.                       CI0070
            10            CL01-CLTIN  PICTURE  9(12).                   CI0070
            10            CL01-CLTND  PICTURE  9(8).                    CI0070
            10            CL01-CLTINC PICTURE  9.                       CI0070
            10            CL01-CCDWA  PICTURE  9.                       CI0070
            10            CL01-CICES  PICTURE  X.                       CI0070
            10            CL01-CLTRA  PICTURE  9(2).                    CI0070
            10            CL01-DIRSY  PICTURE  9(4)                     CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            CL01-CFEDS  PICTURE  X.                       CI0070
            10            CL01-FILLER PICTURE  X(06).                   CI0070
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0026           PIC X(8) VALUE 'CI0026P '.                  AM0026
      ******************************************************************
      *
      *WORK AREA NEEDED FOR MACRO AADA55
      *
      *!WF DSP=DD DSL=DD SEL=01 FOR=I LEV=1 PLT=DD
       01                 DD00.                                         CI0070
          05              DD00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00093).                  CI0070
       01                 DD01  REDEFINES      DD00.                    CI0070
            10            DD01-XDAT8.                                   CI0070
            11            DD01-XDATC  PICTURE  XX.                      CI0070
            11            DD01-XDATY  PICTURE  XX.                      CI0070
            11            DD01-XDATM  PICTURE  XX.                      CI0070
            11            DD01-XDATD  PICTURE  XX.                      CI0070
            10            DD01-XDAT8D                                   CI0070
                          REDEFINES            DD01-XDAT8               CI0070
               PICTURE    9(8).                                         CI0070
            10            DD01-XDAT81.                                  CI0070
            11            DD01-XDATM1 PICTURE  XX.                      CI0070
            11            DD01-XDATD1 PICTURE  XX.                      CI0070
            11            DD01-XDATC1 PICTURE  XX.                      CI0070
            11            DD01-XDATY1 PICTURE  XX.                      CI0070
            10            DD01-XDAT80                                   CI0070
                          REDEFINES            DD01-XDAT81              CI0070
               PICTURE    9(8).                                         CI0070
            10            DD01-XDAT62.                                  CI0070
            11            DD01-XDATM2 PICTURE  XX.                      CI0070
            11            DD01-XDATD2 PICTURE  XX.                      CI0070
            11            DD01-XDATY2 PICTURE  XX.                      CI0070
            10            DD01-XDAT69                                   CI0070
                          REDEFINES            DD01-XDAT62              CI0070
               PICTURE    9(6).                                         CI0070
            10            DD01-XDATCU.                                  CI0070
            11            DD01-XDATC9 PICTURE  99.                      CI0070
            11            DD01-XDAYMD.                                  CI0070
            12            DD01-XDATY9 PICTURE  99.                      CI0070
            12            DD01-XDAMD.                                   CI0070
            13            DD01-XDATM9 PICTURE  99.                      CI0070
            13            DD01-XDATD9 PICTURE  99.                      CI0070
            10            DD01-XDAT89 PICTURE  9(8).                    CI0070
            10            DD01-XDAJC  PICTURE  9(7).                    CI0070
            10            DD01-XDAJC1.                                  CI0070
            11            DD01-XDAJC9 PICTURE  99.                      CI0070
            11            DD01-XDAJY  PICTURE  99.                      CI0070
            11            DD01-XDAJN  PICTURE  999.                     CI0070
            10            DD01-XDAB   PICTURE  9(5).                    CI0070
            10            DD01-DD05.                                    CI0070
            11            DD01-XDACT  PICTURE  S9(3)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            DD01-XDACV  PICTURE  S9                       CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            DD01-XDAGP  PICTURE  S9(9)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            DD01-XDAJP  PICTURE  S9(7)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            DD01-XDACV1 PICTURE  S9                       CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            DD01-XDAGP1 PICTURE  S9(9)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            DD01-XDAJP1 PICTURE  S9(7)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            DD01-XW03.                                    CI0070
            11            DD01-XDATG.                                   CI0070
            12            DD01-XDAT1.                                   CI0070
            13            DD01-XDAT19 PICTURE  99.                      CI0070
            12            DD01-XDAT2.                                   CI0070
            13            DD01-XDAT29 PICTURE  99.                      CI0070
            12            DD01-XDAT3.                                   CI0070
            13            DD01-XDAT39 PICTURE  99.                      CI0070
            12            DD01-XDAT4.                                   CI0070
            13            DD01-XDAT49 PICTURE  99.                      CI0070
            11            DD01-XLEAPY PICTURE  99.                      CI0070
            11            DD01-DTGCY  PICTURE  9(4).                    CI0070
            11            DD01-FILLER                                   CI0070
                          REDEFINES            DD01-DTGCY.              CI0070
            12            DD01-DTGCC  PICTURE  9(2).                    CI0070
            12            DD01-DTGYY  PICTURE  9(2).                    CI0070
      *
      ******************************************************************
      *
       01  7-WORK-AREAS.
      ** - FIELDS NEED FOR DATE CONVERSION MACRO
      *
           05  7-WORK-XRC                   PIC 9(02).
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0070
            10            XW05-XW06.                                    CI0070
            11            XW05-XDBPCB.                                  CI0070
            12            XW05-XDBDNM PICTURE  X(08)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-XSEGLV PICTURE  X(02)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-XRC    PICTURE  X(02)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-XPROPT PICTURE  X(04)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-FILLER PICTURE  S9(5)                    CI0070
                          VALUE                ZERO                     CI0070
                          BINARY.                                       CI0070
            12            XW05-XSEGNM PICTURE  X(08)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0070
                          VALUE                ZERO                     CI0070
                          BINARY.                                       CI0070
            12            XW05-XSEGNB PICTURE  9(05)                    CI0070
                          VALUE                ZERO                     CI0070
                          BINARY.                                       CI0070
            12            XW05-XCOKEY PICTURE  X(70)                    CI0070
                          VALUE                SPACE.                   CI0070
            10            XW05-XW07.                                    CI0070
            11            XW05-XIOPCB.                                  CI0070
            12            XW05-XTERMI PICTURE  X(08)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-FILLER PICTURE  XX                       CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-XRC1   PICTURE  X(02)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-FILLER PICTURE  X(12)                    CI0070
                          VALUE                SPACE.                   CI0070
            12            XW05-XMODNM PICTURE  X(8)                     CI0070
                          VALUE                SPACE.                   CI0070
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0070
                          VALUE                ZERO.                    CI0070
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0070
                          VALUE                ZERO.                    CI0070
            10            XW05-XGU    PICTURE  X(4)                     CI0070
                          VALUE                'GU  '.                  CI0070
            10            XW05-XGHU   PICTURE  X(4)                     CI0070
                          VALUE                'GHU '.                  CI0070
            10            XW05-XGN    PICTURE  X(4)                     CI0070
                          VALUE                'GN  '.                  CI0070
            10            XW05-XGHN   PICTURE  X(4)                     CI0070
                          VALUE                'GHN '.                  CI0070
            10            XW05-XGNP   PICTURE  X(4)                     CI0070
                          VALUE                'GNP '.                  CI0070
            10            XW05-XGHNP  PICTURE  X(4)                     CI0070
                          VALUE                'GHNP'.                  CI0070
            10            XW05-XREPL  PICTURE  XXXX                     CI0070
                          VALUE                'REPL'.                  CI0070
            10            XW05-XISRT  PICTURE  X(4)                     CI0070
                          VALUE                'ISRT'.                  CI0070
            10            XW05-XDLET  PICTURE  X(4)                     CI0070
                          VALUE                'DLET'.                  CI0070
            10            XW05-XOPEN  PICTURE  X(4)                     CI0070
                          VALUE                'OPEN'.                  CI0070
            10            XW05-XCLSE  PICTURE  X(4)                     CI0070
                          VALUE                'CLSE'.                  CI0070
            10            XW05-XCHKP  PICTURE  X(4)                     CI0070
                          VALUE                'CHKP'.                  CI0070
            10            XW05-XXRST  PICTURE  X(4)                     CI0070
                          VALUE                'XRST'.                  CI0070
            10            XW05-XTERM  PICTURE  X(4)                     CI0070
                          VALUE                'TERM'.                  CI0070
            10            XW05-XNFPAC PICTURE  X(13)                    CI0070
                          VALUE                SPACE.                   CI0070
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0070
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0070
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU070
      *                   MS03                                          ADU070
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
      *                                                                 AM0020
      ******************************************************************AM0020
      **     SEGMENT THAT CONTAINS THE CAMS ACCOUNTING DATES           *AM0020
      ******************************************************************AM0020
      *                                                                 AM0020
      *!WF DSP=NS DSL=NS SEL=20 FOR=I LEV=1                             AM0020
       01                 NS00.                                         CI0070
          05              NS00-00.                                      CI0070
            10            NS00-NS00K.                                   CI0070
            11            NS00-PRCSTK PICTURE  XX.                      CI0070
          05              NS00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00078).                  CI0070
       01                 NS20  REDEFINES      NS00.                    CI0070
            10       FILLER         PICTURE  X(00002).                  CI0070
            10            NS20-DCACG  PICTURE  9(8).                    CI0070
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            NS20-CCDAT  PICTURE  X(8).                    CI0070
            10            NS20-DCALP  PICTURE  X(12).                   CI0070
            10            NS20-DNACG  PICTURE  9(8).                    CI0070
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            NS20-CNDAT  PICTURE  X(8).                    CI0070
            10            NS20-DNALP  PICTURE  X(12).                   CI0070
            10            NS20-DCACD  PICTURE  X(10).                   CI0070
            10            NS20-FILLER PICTURE  X(4).                    CI0070
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
                                                                        AM0026
      ******************************************************************AM0026
      **     PCB ADDRESS LIST FOR CI0026.  MODULE CI0026 WILL NEED     *AM0026
      **     PCB'S FOR:                                                *AM0026
      **                CONTRACT DATABASE(CT1P)                        *AM0026
      **                CLIENT DATABASE(CL1P)                          *AM0026
      **                GROUP DATABASE(GR1P)                           *AM0026
      ******************************************************************AM0026
                                                                        AM0026
       01  CI0026A-PCB-ADDRESS-LIST.                                    AM0026
           05  CI0026A-PCB-CT1P-PTR1      POINTER.                      AM0026
           05  CI0026A-PCB-CL1P-PTR1      POINTER.                      AM0026
           05  CI0026A-PCB-GR1P-PTR1      POINTER.                      AM0026
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018C-PCB-ADDRESS-LIST.                                    AM0018
           05  CI0018C-PCB-CT1P-PTR1      POINTER.                      AM0018
      ******************************************************************ADUTAB
      **              TABLE TA1W ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA1W-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=1W FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA1W.                                                CI0070
           04    G-TA1W-PARAM.                                          CI0070
             10  G-TA1W-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0070
                        VALUE      +042.                                CI0070
             10  G-TA1W-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0070
                        VALUE      +001.                                CI0070
             10  G-TA1W-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0070
                        VALUE      +002.                                CI0070
             10  G-TA1W-NUAPP  PICTURE 99                               CI0070
                        VALUE       0.                                  CI0070
             10  G-TA1W-NUTAB  PICTURE X(6)                             CI0070
                        VALUE 'TA001W'.                                 CI0070
             10  G-TA1W-TABFO  PICTURE XX                 VALUE SPACE.  CI0070
             10  G-TA1W-TABCR  PICTURE XX                 VALUE SPACE.  CI0070
             10  G-TA1W-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0070
             10  G-TA1W-NUSSC  PICTURE X  VALUE   ' '.                  CI0070
             10  G-TA1W-NUSSY  PICTURE X                  VALUE SPACE.  CI0070
             10  G-TA1W-TRANID PICTURE X(4)               VALUE SPACE.  CI0070
             10  G-TA1W-FILSYS.                                         CI0070
             15  G-TA1W-USERC  PICTURE X(6)               VALUE SPACE.  CI0070
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0070
           04             TA1W.                                         CI0070
            10            TA1W-CFCNTY PICTURE  X(2)                     CI0070
                          VALUE                SPACE.                   CI0070
            10            TA1W-MCTYL  PICTURE  X(40)                    CI0070
                          VALUE                SPACE.                   CI0070
      **                                                                ADUTAB
      *                                                                 AMDU35
      ******************************************************************AMDU35
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU35
      **     TAX MARKET SEGMENT FOR AN ACCOUNT NUMBER PASSED.          *AMDU35
      ******************************************************************AMDU35
      *                                                                 AMDU35
      *!WF DSP=TM DSL=DU SEL=35 FOR=I LEV=1                             AMDU35
       01                 TM00.                                         CI0070
          05              TM00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00237).                  CI0070
       01                 TM35  REDEFINES      TM00.                    CI0070
            10            TM35-C299.                                    CI0070
            11            TM35-CTID.                                    CI0070
            12            TM35-CTIDA  PICTURE  9(3).                    CI0070
            12            TM35-CTIDN.                                   CI0070
            13            TM35-CTIDNP PICTURE  X(13).                   CI0070
            13            TM35-CTIDND PICTURE  9(11).                   CI0070
            10            TM35-DCACG  PICTURE  9(8).                    CI0070
            10            TM35-FILLER PICTURE  X(100).                  CI0070
            10            TM35-CTXMT  PICTURE  9(2).                    CI0070
            10            TM35-CTCUS  PICTURE  999.                     CI0070
            10            TM35-CGRMF  PICTURE  X.                       CI0070
            10            TM35-GRPLC  PICTURE  99.                      CI0070
            10            TM35-GRPLT  PICTURE  99.                      CI0070
            10            TM35-FILLER PICTURE  X(092).                  CI0070
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMWHIN
      ******************************************************************AMWHIN
      **     WITHHOLDING INSTRUCTION WORKING STORAGE FIELDS            *AMWHIN
      ******************************************************************AMWHIN
      *                                                                 AMWHIN
       01  W-WITH-MISC.                                                 AMWHIN
      *!WI pl=WH120                                                     AMWHIN
           05  W-AW34-QITEM                                             AMWHIN
                        PICTURE 9(3).                                   CI0070
      *!WI pl=WH140                                                     AMWHIN
           05  W-WORK-CTWTC                                             AMWHIN
                        PICTURE 9(2).                                   CI0070
      *!WI pl=WH160                                                     AMWHIN
           05  W-WORK-AWITH                                             AMWHIN
                        PICTURE S9(11)V99                               CI0070
                          COMPUTATIONAL-3.                              CI0070
      *!WI pl=WH200                                                     AMWHIN
           05  W-PRDC-CTWTC                                             AMWHIN
                        PICTURE 9(2).                                   CI0070
      *!WI pl=WH220                                                     AMWHIN
           05  W-PRDC-AWITH                                             AMWHIN
                        PICTURE S9(11)V99                               CI0070
                          COMPUTATIONAL-3.                              CI0070
      *!WI pl=WH300                                                     AMWHIN
           05  W-NPFD-CTWTC                                             AMWHIN
                        PICTURE 9(2).                                   CI0070
      *!WI pl=WH320                                                     AMWHIN
           05  W-NPFD-AWITH                                             AMWHIN
                        PICTURE S9(11)V99                               CI0070
                          COMPUTATIONAL-3.                              CI0070
      *!WI pl=WH400                                                     AMWHIN
           05  W-NPST-CTWTC                                             AMWHIN
                        PICTURE 9(2).                                   CI0070
      *!WI pl=WH420                                                     AMWHIN
           05  W-NPST-AWITH                                             AMWHIN
                        PICTURE S9(11)V99                               CI0070
                          COMPUTATIONAL-3.                              CI0070
      *!WI pl=WH500                                                     AMWHIN
           05  W-DIVD-CTWTC                                             AMWHIN
                        PICTURE 9(2).                                   CI0070
      *!WI pl=WH520                                                     AMWHIN
           05  W-DIVD-AWITH                                             AMWHIN
                        PICTURE S9(11)V99                               CI0070
                          COMPUTATIONAL-3.                              CI0070
      *!WI pl=WH600                                                     AMWHIN
           05  W-FRGN-CTWTC                                             AMWHIN
                        PICTURE 9(2).                                   CI0070
      *!WI pl=WH620                                                     AMWHIN
           05  W-FRGN-AWITH                                             AMWHIN
                        PICTURE S9(11)V99                               CI0070
                          COMPUTATIONAL-3.                              CI0070
      *!WI pl=WH640                                                     AMWHIN
           05  W-FRGN-MCTYL                                             AMWHIN
                        PICTURE X(40).                                  CI0070
      *!WI pl=WH700                                                     AMWHIN
           05  W-BKUP-CTWTC                                             AMWHIN
                        PICTURE 9(2).                                   CI0070
      *!WI pl=WH720                                                     AMWHIN
           05  W-BKUP-AWITH                                             AMWHIN
                        PICTURE S9(11)V99                               CI0070
                          COMPUTATIONAL-3.                              CI0070
      *                                                                 AMWHIN
      *                                                                 AMWHIN
      *                                                                 AMWHIN
      *                                                                 AMWHIN
       01  W-WS00-AREAS.
           05  W-CT22-FOUND                             PIC X.
      *
      *
      *
      *
       01   DEBUT-WSS.                                                  CI0070
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0070
            05   IK     PICTURE X.                                      CI0070
       01  CONSTANTES-PAC.                                              CI0070
           05  FILLER  PICTURE X(87)   VALUE                            CI0070
                     '6015 CAT09/08/14CI0070ADMIN   14:34:34CI0070P AMERCI0070
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0070
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0070
           05  NUGNA   PICTURE X(5).                                    CI0070
           05  APPLI   PICTURE X(3).                                    CI0070
           05  DATGN   PICTURE X(8).                                    CI0070
           05  PROGR   PICTURE X(6).                                    CI0070
           05  CODUTI  PICTURE X(8).                                    CI0070
           05  TIMGN   PICTURE X(8).                                    CI0070
           05  PROGE   PICTURE X(8).                                    CI0070
           05  COBASE  PICTURE X(4).                                    CI0070
           05  DATGNC  PICTURE X(10).                                   CI0070
           05  RELEAS  PICTURE X(7).                                    CI0070
           05  DATGE   PICTURE X(10).                                   CI0070
           05  DATSQ   PICTURE X(10).                                   CI0070
       01  DATCE.                                                       CI0070
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0070
         05  DATOR.                                                     CI0070
           10  DATOA  PICTURE XX.                                       CI0070
           10  DATOM  PICTURE XX.                                       CI0070
           10  DATOJ  PICTURE XX.                                       CI0070
       01   VARIABLES-CONDITIONNELLES.                                  CI0070
            05                  FT      PICTURE X VALUE '0'.            CI0070
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0070
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0070
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0070
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0070
       01               S-CL01-SSA.                                     CI0070
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0070
                                      VALUE 'CL01    '.                 CI0070
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0070
            10          S-CL01-CCOD   PICTURE X(5)                      CI0070
                                      VALUE '-----'.                    CI0070
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0070
       01            S-CLU01-SSA.                                       CI0070
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0070
                                      VALUE 'CL01    '.                 CI0070
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0070
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0070
                                      VALUE '-----'.                    CI0070
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0070
                                      VALUE '(CL01K'.                   CI0070
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0070
            10       S-CLU01-CL01K.                                     CI0070
            11       S-CLU01-C199.                                      CI0070
            12       S-CLU01-CLID.                                      CI0070
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0070
            13       S-CLU01-CLIDN.                                     CI0070
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0070
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0070
            10  FILLER   PICTURE X    VALUE ')'.                        CI0070
       01   ZONES-UTILISATEUR PICTURE X.                                CI0070
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
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0070
          05              PA00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00106).                  CI0070
       01                 PA06  REDEFINES      PA00.                    CI0070
            10            PA06-XDBPCB.                                  CI0070
            11            PA06-XDBDNM PICTURE  X(08).                   CI0070
            11            PA06-XSEGLV PICTURE  X(02).                   CI0070
            11            PA06-XRC    PICTURE  X(02).                   CI0070
            11            PA06-XPROPT PICTURE  X(04).                   CI0070
            11            PA06-FILLER PICTURE  S9(5)                    CI0070
                          BINARY.                                       CI0070
            11            PA06-XSEGNM PICTURE  X(08).                   CI0070
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0070
                          BINARY.                                       CI0070
            11            PA06-XSEGNB PICTURE  9(05)                    CI0070
                          BINARY.                                       CI0070
            11            PA06-XCOKEY PICTURE  X(70).                   CI0070
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0070
          05              PB00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00106).                  CI0070
       01                 PB06  REDEFINES      PB00.                    CI0070
            10            PB06-XDBPCB.                                  CI0070
            11            PB06-XDBDNM PICTURE  X(08).                   CI0070
            11            PB06-XSEGLV PICTURE  X(02).                   CI0070
            11            PB06-XRC    PICTURE  X(02).                   CI0070
            11            PB06-XPROPT PICTURE  X(04).                   CI0070
            11            PB06-FILLER PICTURE  S9(5)                    CI0070
                          BINARY.                                       CI0070
            11            PB06-XSEGNM PICTURE  X(08).                   CI0070
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0070
                          BINARY.                                       CI0070
            11            PB06-XSEGNB PICTURE  9(05)                    CI0070
                          BINARY.                                       CI0070
            11            PB06-XCOKEY PICTURE  X(70).                   CI0070
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0070
          05              PC00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00106).                  CI0070
       01                 PC06  REDEFINES      PC00.                    CI0070
            10            PC06-XDBPCB.                                  CI0070
            11            PC06-XDBDNM PICTURE  X(08).                   CI0070
            11            PC06-XSEGLV PICTURE  X(02).                   CI0070
            11            PC06-XRC    PICTURE  X(02).                   CI0070
            11            PC06-XPROPT PICTURE  X(04).                   CI0070
            11            PC06-FILLER PICTURE  S9(5)                    CI0070
                          BINARY.                                       CI0070
            11            PC06-XSEGNM PICTURE  X(08).                   CI0070
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0070
                          BINARY.                                       CI0070
            11            PC06-XSEGNB PICTURE  9(05)                    CI0070
                          BINARY.                                       CI0070
            11            PC06-XCOKEY PICTURE  X(70).                   CI0070
      *                                                                 AMDU7A
      ******************************************************************AMDU7A
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET A      *AMDU7A
      **     CLIENTS ARRANGEMENTS & ASSOC BANK FOR A SPECIFIED ARR TYPE*AMDU7A
      ******************************************************************AMDU7A
      *                                                                 AMDU7A
      *!WF DSP=WZ DSL=DU SEL=7A FOR=I DES=1 LEV=1                       AMDU7A
       01                 WZ7A.                                         CI0070
            10            WZ7A-C299.                                    CI0070
            11            WZ7A-CTID.                                    CI0070
            12            WZ7A-CTIDA  PICTURE  9(3).                    CI0070
            12            WZ7A-CTIDN.                                   CI0070
            13            WZ7A-CTIDNP PICTURE  X(13).                   CI0070
            13            WZ7A-CTIDND PICTURE  9(11).                   CI0070
            10            WZ7A-CTWTC  PICTURE  9(2).                    CI0070
            10            WZ7A-AWITH  PICTURE  S9(11)V99                CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            WZ7A-CTWTC1 PICTURE  9(2).                    CI0070
            10            WZ7A-AWITH1 PICTURE  S9(11)V99                CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            WZ7A-CTWTC2 PICTURE  9(2).                    CI0070
            10            WZ7A-AWITH2 PICTURE  S9(11)V99                CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            WZ7A-CTWTC3 PICTURE  9(2).                    CI0070
            10            WZ7A-AWITH3 PICTURE  S9(11)V99                CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            WZ7A-CTWTC4 PICTURE  9(2).                    CI0070
            10            WZ7A-AWITH4 PICTURE  S9(11)V99                CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            WZ7A-MCTYL  PICTURE  X(40).                   CI0070
            10            WZ7A-CTWTC5 PICTURE  9(2).                    CI0070
            10            WZ7A-AWITH5 PICTURE  S9(11)V99                CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            WZ7A-ICUST  PICTURE  X.                       CI0070
            10            WZ7A-IWTHH  PICTURE  X.                       CI0070
      *                                                                 AMDU7A
      *                                                                 AMDU7A
      *                                                                 AMDU7A
      *                                                                 AMDU7A
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0070
          05              DE00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00653).                  CI0070
       01                 DE10  REDEFINES      DE00.                    CI0070
            10            DE10-DU11.                                    CI0070
            11            DE10-XFONC  PICTURE  X(4).                    CI0070
            11            DE10-MPSBN  PICTURE  X(8).                    CI0070
            11            DE10-XDBDNM PICTURE  X(08).                   CI0070
            11            DE10-XSEGNM PICTURE  X(08).                   CI0070
            11            DE10-XRC    PICTURE  X(02).                   CI0070
            11            DE10-MSEG   PICTURE  X(08).                   CI0070
            11            DE10-XCOKEY PICTURE  X(70).                   CI0070
            11            DE10-CUIBR  PICTURE  X(01).                   CI0070
            11            DE10-CUIBA  PICTURE  X(01).                   CI0070
            11            DE10-IPBIK  PICTURE  X(1).                    CI0070
            10            DE10-DU03.                                    CI0070
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            DE10-CMSSF  PICTURE  XX.                      CI0070
            11            DE10-DU09.                                    CI0070
            12            DE10-CMESA  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            12            DE10-CMESB  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            12            DE10-CMSST  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            12            DE10-QELLAA PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            12            DE10-TMESS4 PICTURE  X(512).                  CI0070
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
       01                 MS00.                                         CI0070
          05              MS00-SUITE.                                   CI0070
            15       FILLER         PICTURE  X(00542).                  CI0070
       01                 MS03  REDEFINES      MS00.                    CI0070
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            10            MS03-CMSSF  PICTURE  XX.                      CI0070
            10            MS03-DU09.                                    CI0070
            11            MS03-CMESA  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            11            MS03-CMESB  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            11            MS03-CMSST  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            11            MS03-QELLAA PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
            11            MS03-TMESS4 PICTURE  X(512).                  CI0070
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0070
            10            MX11-QMSGS  PICTURE  9(03).                   CI0070
            10            MX11-PJ09                                     CI0070
                          OCCURS       025     TIMES.                   CI0070
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0070
                          COMPUTATIONAL-3.                              CI0070
            11            MX11-CMESB  PICTURE  S9(9)                    CI0070
                          BINARY.                                       CI0070
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ7A
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0070
      *               *                                   *             CI0070
      *               *INITIALISATIONS                    *             CI0070
      *               *                                   *             CI0070
      *               *************************************.            CI0070
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
           MOVE        MX11-QMSGS TO IMX11L                             ADU102
           INITIALIZE       DE10.
       F02BA-FN. EXIT.
      *N02SC.    NOTE *ALIGN WITH PCBS PASSED             *.
       F02SC.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
       F02SC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0070
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0070
      *               *                                   *             CI0070
      *               *FIN DE TRAITEMENT                  *             CI0070
      *               *                                   *             CI0070
      *               *************************************.            CI0070
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0070
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N35.      NOTE *************************************.
      *               *                                   *
      *               *EDIT INCOMING PARAMETERS           *
      *               *                                   *
      *               *************************************.
       F35.           EXIT.                                             lv05
      *N35BA.    NOTE *VERIFY CONTRACT ID NUMBER          *.
       F35BA.    IF    WZ7A-CTID NOT NUMERIC                            lv10
                 NEXT SENTENCE ELSE GO TO     F35BA-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BA-FN. EXIT.
       F35-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *MAINLINE INITIALIZATIONS           *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      ** INITIALIZE:                  *
      **  - GET CURRENT ACTG DATE     *
      *********************************
      *N40EA.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F40EA.                                                           lv10
      *                                                                 AM0020
      *********************************                                 AM0020
      ** THIS MODULE WILL READ THE    *                                 AM0020
      ** CAMS ACCOUNTING DATE VSAM    *                                 AM0020
      ** FILE(CAMNMSTK) AND THE CAMS  *                                 AM0020
      ** DATE RECORD.                 *                                 AM0020
      *********************************                                 AM0020
      *                                                                 AM0020
           CALL        CI0020 USING                                     AM0020
           DFHEIBLK                                                     AM0020
           DFHCOMMAREA                                                  AM0020
           NS20                                                         AM0020
           MS03.                                                        AM0020
      *N40EC.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F40EC.    IF    MS03-NMESS2 > ZERO                               lv15
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F40EC-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F40EC-900. GO TO F40EE-FN.
       F40EC-FN. EXIT.
      *N40EE.    NOTE *NO ERRORS                          *.            ADU070
       F40EE.                                                           lv15
           INITIALIZE  MS03.                                            ADU070
       F40EE-FN. EXIT.
       F40EA-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *GET TAXPAYER CLIENT ID             *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      *
      *********************************
      ** CALL MODULE CI0018 TO GET    *
      ** THE ACCOUNT'S TAXPAYER CLIENT*
      ** ID TO CHECK TO SEE IF THE    *
      ** CLIENT IS AN ORGANIZATION.   *
      ** IF IT IS AN ORGANIZATION,    *
      ** JUST DESCRIBLE THE COLUMNS   *
      ** AND RETURN.                  *
      *********************************
      *
      *N45BA.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F45BA.                                                           lv10
      *                                                                 AM0018
      *********************************                                 AM0018
      ** THIS MODULE WILL READ THE    *                                 AM0018
      ** CONTRACT DATABASE TO GET THE *                                 AM0018
      ** TAXPAYER CLIENT ID AND OWNER *                                 AM0018
      ** CLIENT ID'S ASSOCIATED WITH  *                                 AM0018
      ** THE ACCOUNT NUMBER.          *                                 AM0018
      *********************************                                 AM0018
      *                                                                 AM0018
           INITIALIZE      AC14                                         AM0018
           MOVE        WZ7A-CTID TO AC14-CTID                           AM0018
           MOVE        NS20-DCACG TO AC14-DCACG                         AM0018
           MOVE        25 TO AC14-XIMAX                                 AM0018
           MOVE        'Y' TO AC14-IPOCH                                AM0018
           SET CI0018C-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018C-PCB-ADDRESS-LIST                                     AM0018
           AC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
      *N45BC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F45BC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F45BC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0018 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0018 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F45BC-900. GO TO F45BE-FN.
       F45BC-FN. EXIT.
      *N45BE.    NOTE *NO ERRORS                          *.            ADU071
       F45BE.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F45BE-FN. EXIT.
      *N45DA.    NOTE *F45DA                              *.
       F45DA.                                                           lv15
      *********************************
      ** IF THERE WERE ANY ERRORS     *
      ** WHILE GETTING THE DEFAULT    *
      ** CLIENT ID, SEND MESSAGE BACK *
      ** TO THE WORKSTATION.  IF THE  *
      ** MESSAGE IS CRITICAL, READ THE*
      ** NEXT CX13 SEGMENT.  IF THE   *
      ** MESSAGE IS INFORMATIONAL,    *
      ** SEND THE ROW TO THE WS.      *
      *********************************
       F45DA-FN. EXIT.
       F45BA-FN. EXIT.
      *N45FA.    NOTE *SET UP SSA FOR CL01 SEGMENT        *.
       F45FA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CL01 SEGMENT  *
      *********************************
      *
           MOVE        AC14-CLID01 TO S-CLU01-CLID.
       F45FA-FN. EXIT.
      *N45GA.    NOTE *READ CL01 SEGMENT FOR TAXPAYER     *.
       F45GA.                                                           lv10
      *
      *********************************
      ** READ THE CL01 SEGMENT        *
      *********************************
      *
           PERFORM     F94CL THRU F94CL-FN.
       F45GA-FN. EXIT.
      *N45HA.    NOTE *CHECK IF CL01 SEGMENT NOT FOUND    *.
       F45HA.    IF    IK = '1'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F45HA-FN.
      *
      *********************************
      ** SEND AN ERROR MESSAGE IF CL01*
      ** SEGMENT NOT FOUND.           *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45HA-FN. EXIT.
      *N45IA.    NOTE *CHECK IF ORGANIZATION              *.
       F45IA.    IF    CL01-CLTYP = 'O'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F45IA-FN.
      *
      *********************************
      ** IF IT IS AN ORGANIZATION,    *
      ** JUST DESCRIBE THE COLUMNS    *
      ** AND RETURN.                  *
      *********************************
      *
           MOVE        'N' TO WZ7A-IWTHH
           MOVE                     ALL '1' TO FT GO TO F20.
       F45IA-FN. EXIT.
       F45-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *GET WITHHOLDING INSTRUCTIONS       *
      *               *                                   *
      *               *************************************.
       F50.                                                             lv05
      *
      *********************************
      ** CALL MODULE CI0026 TO GET    *
      ** THE ACCOUNT'S WITHHOLDING    *
      ** INSTRUCTIONS.                *
      *********************************
      *
      *N50BA.    NOTE *CALL CI0026 - ACCT W/H INSTRUCTS   *.            AM0026
       F50BA.                                                           lv10
      *                                                                 AM0026
      *********************************                                 AM0026
      ** THIS MODULE WILL GET THE     *                                 AM0026
      ** ACCOUNT NUMBER'S WITHHOLDING *                                 AM0026
      ** INSTRUCTIONS.                *                                 AM0026
      *********************************                                 AM0026
      *                                                                 AM0026
           INITIALIZE      AW34                                         AM0026
           MOVE        WZ7A-CTID TO AW34-CTID                           AM0026
           MOVE        NS20-DCACG TO AW34-DCACG                         AM0026
           MOVE        10 TO AW34-XIMAX                                 AM0026
           MOVE        'Y' TO AW34-IPOCH                                AM0026
           SET CI0026A-PCB-CT1P-PTR1 TO                                 AM0026
                       PCB-CT1P-PTR1                                    AM0026
           SET CI0026A-PCB-CL1P-PTR1 TO                                 AM0026
                       PCB-CL1P-PTR1                                    AM0026
           SET CI0026A-PCB-GR1P-PTR1 TO                                 AM0026
                       PCB-GR1P-PTR1                                    AM0026
           INITIALIZE      DE10-DU03                                    AM0026
           CALL        CI0026 USING                                     AM0026
           DFHEIBLK                                                     AM0026
           DFHCOMMAREA                                                  AM0026
           DLIUIBII                                                     AM0026
           CI0026A-PCB-ADDRESS-LIST                                     AM0026
           AW34                                                         AM0026
           AC14                                                         AM0026
           DE10                                                         AM0026
           MS03                                                         AM0026
           MX11.                                                        AM0026
      *N50BC.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50BC.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50BC-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0026 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0026 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F50BC-900. GO TO F50BE-FN.
       F50BC-FN. EXIT.
      *N50BE.    NOTE *NO ERRORS                          *.            ADU071
       F50BE.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F50BE-FN. EXIT.
       F50BA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *EXTRACT W/H INSTRUCTIONS TYPE      *
      *               *                                   *
      *               *************************************.
       F55.                                                             lv05
      *
      *********************************
      ** LOOP THRU THE CT22 TABLE AND *
      ** EXTRACT THE DIFFERENT W/H    *
      ** INSTRUCTIONS TYPE.           *
      *********************************
      *
      *N55BA.    NOTE *INITIALIZE                         *.            AMWHIN
       F55BA.                                                           lv10
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** INITIALIZE THE CT22 TABLE    *                                 AMWHIN
      ** READ INDEX.                  *                                 AMWHIN
      ** INITIALIZE THE WORKING       *                                 AMWHIN
      ** STORAGE FIELDS.              *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           INITIALIZE      W-WITH-MISC                                  AMWHIN
           MOVE        +1 TO W-AW34-QITEM                               AMWHIN
           INITIALIZE      TA1W
           MOVE        'N' TO W-CT22-FOUND.
       F55BA-FN. EXIT.
      *N55CA.    NOTE *DO WHILE MORE CT22 TABLE ENTRIES   *.            AMWHIN
       F55CA.    IF    W-AW34-QITEM NOT >                               lv10
                       AW34-QITEM                                       AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55CA-FN.                 AMWHIN
      *********************************                                 AMWHIN
      ** LOOP WHILE MORE CT22 TABLE   *                                 AMWHIN
      ** ENTRIES TO PROCESS.          *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
      *N55DA.    NOTE *MOVE IN TYPE OF AMOUNT             *.            AMWHIN
       F55DA.                                                           lv15
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** MOVE THE TYPE OF WITHHOLDING *                                 AMWHIN
      ** AMOUNT TO A WORK FIELD.      *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        AW34-CTWTC (W-AW34-QITEM) TO                     AMWHIN
           W-WORK-CTWTC.                                                AMWHIN
       F55DA-FN. EXIT.
      *N55EA.    NOTE *CASE OF STRUCT FOR TYPE OF AMT     *.            AMWHIN
       F55EA.                                                           lv15
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** CASE OF STRUCTURE FOR TYPE   *                                 AMWHIN
      ** OF AMOUNT.                   *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
      *N55FA.    NOTE *CHECK IF DOLLAR AMOUNT             *.            AMWHIN
       F55FA.    IF    W-WORK-CTWTC =                                   lv20
                       01                                               AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55FA-FN.                 AMWHIN
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** IF WITHHOLDING IS IN DOLLAR  *                                 AMWHIN
      ** AMOUNT, MOVE THE AMOUNT.     *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        AW34-CTWHAT (W-AW34-QITEM) TO                    AMWHIN
           W-WORK-AWITH.                                                AMWHIN
       F55FA-900. GO TO F55EA-FN.
       F55FA-FN. EXIT.
      *N55GA.    NOTE *CHECK IF PERCENTAGE AMOUNT         *.            AMWHIN
       F55GA.    IF    W-WORK-CTWTC =                                   lv20
                       02                                               AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.                 AMWHIN
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** IF WITHHOLDING IS PERCENTAGE *                                 AMWHIN
      ** AMOUNT, MOVE THE AMOUNT.     *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        AW34-CTWHP (W-AW34-QITEM) TO                     AMWHIN
           W-WORK-AWITH.                                                AMWHIN
       F55GA-900. GO TO F55EA-FN.
       F55GA-FN. EXIT.
      *N55HA.    NOTE *DEFAULT                            *.            AMWHIN
       F55HA.                                                           lv20
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** THE DEFAULT IS ZERO AMOUNT   *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        ZEROS TO W-WORK-AWITH.                           AMWHIN
       F55HA-FN. EXIT.
       F55EA-FN. EXIT.
      *N55IA.    NOTE *CASE OF STRUCT TYPE OF W/H         *.            AMWHIN
       F55IA.                                                           lv15
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** CASE OF STRUCTURE FOR TYPE   *                                 AMWHIN
      ** OF WITHHOLDING INSTRUCTION   *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
      *N55JA.    NOTE *CHECK IF FOREIGN WITHHOLDING       *.            AMWHIN
       F55JA.    IF    AW34-CT22K (W-AW34-QITEM) =                      lv20
                       'US01'                                           AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55JA-FN.                 AMWHIN
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** FOREIGN WITHHOLDING          *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        W-WORK-CTWTC TO W-FRGN-CTWTC                     AMWHIN
           MOVE        W-WORK-AWITH TO W-FRGN-AWITH.                    AMWHIN
      *N55KA.    NOTE *RANDOM TABLE READ FOR TA1W         *.            ADUTAB
       F55KA.                                                           lv25
           MOVE        AW34-CFCNTY (W-AW34-QITEM) TO
           TA1W-CFCNTY
           MOVE        'R1' TO G-TA1W-TABFO                             ADUTAB
           COMPUTE     G-TA1W-LTH = 60 + G-TA1W-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA1W-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA1W)                                ADUTAB
                       LENGTH (G-TA1W-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA1W-TABCR NOT = '00'                          DOT
           MOVE        'UNKNOWN' TO TA1W-MCTYL.
       F55KA-FN. EXIT.
       F55JA-900. GO TO F55IA-FN.
       F55JA-FN. EXIT.
      *N55LA.    NOTE *CHECK IF PERIODIC WITHHOLDING      *.            AMWHIN
       F55LA.    IF    AW34-CT22K (W-AW34-QITEM) =                      lv20
                       'US02'                                           AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55LA-FN.                 AMWHIN
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** PERIODIC WITHHOLDING         *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        W-WORK-CTWTC TO W-PRDC-CTWTC                     AMWHIN
           MOVE        W-WORK-AWITH TO W-PRDC-AWITH.                    AMWHIN
                 IF    W-WORK-CTWTC NOT = ZEROS                         DOT
           MOVE        'Y' TO W-CT22-FOUND.
       F55LA-900. GO TO F55IA-FN.
       F55LA-FN. EXIT.
      *N55MA.    NOTE *CHECK IF NONPERIODIC WITHHOLDING   *.            AMWHIN
       F55MA.    IF    AW34-CT22K (W-AW34-QITEM) =                      lv20
                       'US03'                                           AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.                 AMWHIN
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** PERIODIC WITHHOLDING         *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        W-WORK-CTWTC TO W-NPFD-CTWTC                     AMWHIN
           MOVE        W-WORK-AWITH TO W-NPFD-AWITH.                    AMWHIN
       F55MA-900. GO TO F55IA-FN.
       F55MA-FN. EXIT.
      *N55NA.    NOTE *CHECK IF DIVIDEND WITHHOLDING      *.            AMWHIN
       F55NA.    IF    AW34-CT22K (W-AW34-QITEM) =                      lv20
                       'US04'                                           AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55NA-FN.                 AMWHIN
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** DIVIDEND WITHHOLDING         *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        W-WORK-CTWTC TO W-DIVD-CTWTC                     AMWHIN
           MOVE        W-WORK-AWITH TO W-DIVD-AWITH.                    AMWHIN
       F55NA-900. GO TO F55IA-FN.
       F55NA-FN. EXIT.
      *N55OA.    NOTE *CHECK IF BACKUP WITHHOLDING        *.            AMWHIN
       F55OA.    IF    AW34-CT22K (W-AW34-QITEM) =                      lv20
                       'US05'                                           AMWHIN
                 NEXT SENTENCE ELSE GO TO     F55OA-FN.                 AMWHIN
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** BACKUP WITHHOLDING           *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           MOVE        W-WORK-CTWTC TO W-BKUP-CTWTC                     AMWHIN
           MOVE        W-WORK-AWITH TO W-BKUP-AWITH.                    AMWHIN
       F55OA-900. GO TO F55IA-FN.
       F55OA-FN. EXIT.
      *N55PA.    NOTE *END OF CASE OF STRUCTURE           *.            AMWHIN
       F55PA.                                                           lv20
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** END OF CASE OF STRUCTURE     *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
       F55PA-FN. EXIT.
       F55IA-FN. EXIT.
      *N55QA.    NOTE *ADD 1 TO CT22 TABLE READ INDEX     *.            AMWHIN
       F55QA.                                                           lv15
      *                                                                 AMWHIN
      *********************************                                 AMWHIN
      ** ADD 1 TO THE CT22 TABLE READ *                                 AMWHIN
      ** INDEX.                       *                                 AMWHIN
      *********************************                                 AMWHIN
      *                                                                 AMWHIN
           ADD         +1 TO W-AW34-QITEM.                              AMWHIN
       F55QA-FN. EXIT.
       F55CA-900. GO TO F55CA.
       F55CA-FN. EXIT.
       F55-FN.   EXIT.
      *N60.      NOTE *************************************.
      *               *                                   *
      *               *BUILD THE WZ7A PASS AREA           *
      *               *                                   *
      *               *************************************.
       F60.                                                             lv05
      *
      *********************************
      ** BUILD THE PASS AREA TO BE    *
      ** SENT DOWN TO THE WORKSTAITON *
      *********************************
      *
      *N60BA.    NOTE *BUILD PASS AREA.                   *.
       F60BA.                                                           lv10
      *
      *********************************
      ** BUILD PASS AREA THAT CORP    *
      ** INTERFACE WILL USE TO SEND   *
      ** THE ROW TO THE WORKSTATION   *
      *********************************
      *
           MOVE        W-PRDC-CTWTC TO WZ7A-CTWTC
           MOVE        W-PRDC-AWITH TO WZ7A-AWITH
           MOVE        W-NPFD-CTWTC TO WZ7A-CTWTC1
           MOVE        W-NPFD-AWITH TO WZ7A-AWITH1
           MOVE        W-NPST-CTWTC TO WZ7A-CTWTC2
           MOVE        W-NPST-AWITH TO WZ7A-AWITH2
           MOVE        W-DIVD-CTWTC TO WZ7A-CTWTC3
           MOVE        W-DIVD-AWITH TO WZ7A-AWITH3
           MOVE        W-FRGN-CTWTC TO WZ7A-CTWTC4
           MOVE        W-FRGN-AWITH TO WZ7A-AWITH4
           MOVE        TA1W-MCTYL TO WZ7A-MCTYL
           MOVE        W-BKUP-CTWTC TO WZ7A-CTWTC5
           MOVE        W-BKUP-AWITH TO WZ7A-AWITH5.
      *N60CA.    NOTE *IF CUSTODIAL ACCOUNT               *.
       F60CA.    IF    AW34-CTXMT = 05 OR 07 OR 09                      lv15
                 NEXT SENTENCE ELSE GO TO     F60CA-FN.
           MOVE        'Y' TO WZ7A-ICUST.
       F60CA-900. GO TO F60CM-FN.
       F60CA-FN. EXIT.
      *N60CM.    NOTE *ELSE.. NON CUSTODIAL               *.
       F60CM.                                                           lv15
           MOVE        'N' TO WZ7A-ICUST.
       F60CM-FN. EXIT.
       F60BA-FN. EXIT.
       F60-FN.   EXIT.
      *N65.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF PERIODIC W/H NEEDED       *
      *               *                                   *
      *               *************************************.
       F65.           EXIT.                                             lv05
      *N65BA.    NOTE *IF IDS IS CUSTODIAN                *.
       F65BA.    IF    WZ7A-ICUST = 'Y'                                 lv10
                 AND   W-CT22-FOUND = 'N'
                 NEXT SENTENCE ELSE GO TO     F65BA-FN.
      *********************************
      ** IF IDS IS THE CUSTODIAN AND  *
      ** THERE IS NO PERIODIC W/H     *
      ** INSTRUCTIONS, SEND AN        *
      ** INFORMATIONAL MESSAGE TO THE *
      ** WORKSTATION.                 *
      *********************************
           MOVE        'Y' TO WZ7A-IWTHH
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012160 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
       F65BA-FN. EXIT.
       F65-FN.   EXIT.
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
           PERFORM     F93PC THRU F93PC-FN                              ADU129
           PERFORM     F93ER THRU F93ER-FN.
       F93EA-FN. EXIT.
      *N93ER.    NOTE *SEVERE DL/1 ERROR PROCESSING       *.
       F93ER.         EXIT.                                             lv10
      *N93ET.    NOTE *IF SEVERE ERROR; GET OUT           *.
       F93ET.    IF    DE10-NMESS2 NOT = ZERO                           lv15
                 NEXT SENTENCE ELSE GO TO     F93ET-FN.
           MOVE                     ALL '1' TO FT GO TO F20.
       F93ET-FN. EXIT.
       F93ER-FN. EXIT.
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
           PC06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PC06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
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
