       IDENTIFICATION DIVISION.                                         CI0028
       PROGRAM-ID.  CI0028P.                                            CI0028
      *AUTHOR.         M\M - GET TAX MARKET SEGMENT.                    CI0028
      *DATE-COMPILED.   09/08/14.                                       CI0028
       ENVIRONMENT DIVISION.                                            CI0028
       CONFIGURATION SECTION.                                           CI0028
       SOURCE-COMPUTER. IBM-370.                                        CI0028
       OBJECT-COMPUTER. IBM-370.                                        CI0028
       DATA DIVISION.                                                   CI0028
       WORKING-STORAGE SECTION.                                         CI0028
      *                                                                 AMDU14
      ******************************************************************AMDU14
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU14
      **     REQUESTED TYPE OF CLIENTS FOR THE ACCOUNT NUMBER PASSED.  *AMDU14
      ******************************************************************AMDU14
      *                                                                 AMDU14
      *!WF DSP=AC DSL=DU SEL=14 FOR=I LEV=1                             AMDU14
       01                 AC00.                                         CI0028
          05              AC00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00917).                  CI0028
       01                 AC14  REDEFINES      AC00.                    CI0028
            10            AC14-C299.                                    CI0028
            11            AC14-CTID.                                    CI0028
            12            AC14-CTIDA  PICTURE  9(3).                    CI0028
            12            AC14-CTIDN.                                   CI0028
            13            AC14-CTIDNP PICTURE  X(13).                   CI0028
            13            AC14-CTIDND PICTURE  9(11).                   CI0028
            10            AC14-DCACG  PICTURE  9(8).                    CI0028
            10            AC14-IPOCH  PICTURE  X.                       CI0028
            10            AC14-FILLER PICTURE  X(100).                  CI0028
            10            AC14-CLID01.                                  CI0028
            11            AC14-CLIDO1 PICTURE  X(3).                    CI0028
            11            AC14-NCLID1.                                  CI0028
            12            AC14-CLIDP1 PICTURE  X(12).                   CI0028
            12            AC14-CLIDNA PICTURE  9(8).                    CI0028
            10            AC14-CLCTR  PICTURE  9(3).                    CI0028
            10            AC14-DU21                                     CI0028
                          OCCURS       025     TIMES.                   CI0028
            11            AC14-C199.                                    CI0028
            12            AC14-CLID.                                    CI0028
            13            AC14-CLIDO  PICTURE  9(3).                    CI0028
            13            AC14-CLIDN.                                   CI0028
            14            AC14-CLIDNP PICTURE  X(12).                   CI0028
            14            AC14-CLIDND PICTURE  9(8).                    CI0028
            11            AC14-CLCTRC PICTURE  9(3).                    CI0028
            10            AC14-QITEM  PICTURE  9(3).                    CI0028
            10            AC14-XIMAX  PICTURE  S9(4)                    CI0028
                          BINARY.                                       CI0028
            10            AC14-CRROL  PICTURE  X.                       CI0028
            10            AC14-FILLER PICTURE  X(099).                  CI0028
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU14
      *                                                                 AMDU15
      ******************************************************************AMDU15
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU15
      **     GROUPS FOR THE ACCOUNT NUMBER PASSED.                     *AMDU15
      ******************************************************************AMDU15
      *                                                                 AMDU15
      *!WF DSP=AG DSL=DU SEL=15 FOR=I LEV=1                             AMDU15
       01                 AG00.                                         CI0028
          05              AG00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(04181).                  CI0028
       01                 AG15  REDEFINES      AG00.                    CI0028
            10            AG15-C299.                                    CI0028
            11            AG15-CTID.                                    CI0028
            12            AG15-CTIDA  PICTURE  9(3).                    CI0028
            12            AG15-CTIDN.                                   CI0028
            13            AG15-CTIDNP PICTURE  X(13).                   CI0028
            13            AG15-CTIDND PICTURE  9(11).                   CI0028
            10            AG15-DCACG  PICTURE  9(8).                    CI0028
            10            AG15-IPOCH  PICTURE  X.                       CI0028
            10            AG15-FILLER PICTURE  X(100).                  CI0028
            10            AG15-DU18                                     CI0028
                          OCCURS       010     TIMES.                   CI0028
            11            AG15-CT10.                                    CI0028
            12            AG15-CT10K.                                   CI0028
            13            AG15-GR98.                                    CI0028
            14            AG15-GRID.                                    CI0028
            15            AG15-GRIDC  PICTURE  9(3).                    CI0028
            15            AG15-GRIDN.                                   CI0028
            16            AG15-GRIDNP PICTURE  99.                      CI0028
            16            AG15-GRIDND PICTURE  9(8).                    CI0028
            12            AG15-GR97                                     CI0028
                          REDEFINES            AG15-CT10K.              CI0028
            13            AG15-GRIDCB PICTURE  9(3).                    CI0028
            13            AG15-FILLER PICTURE  X(10).                   CI0028
            12            AG15-GERSD  PICTURE  9(8).                    CI0028
            12            AG15-GERED  PICTURE  9(8).                    CI0028
            12            AG15-GRCSI  PICTURE  X.                       CI0028
            11            AG15-GR01.                                    CI0028
            12            AG15-GR01K.                                   CI0028
            13            AG15-GR98.                                    CI0028
            14            AG15-GRID.                                    CI0028
            15            AG15-GRIDC  PICTURE  9(3).                    CI0028
            15            AG15-GRIDN.                                   CI0028
            16            AG15-GRIDNP PICTURE  99.                      CI0028
            16            AG15-GRIDND PICTURE  9(8).                    CI0028
            12            AG15-GECKD  PICTURE  9.                       CI0028
            12            AG15-GEMDA  PICTURE  9(8).                    CI0028
            12            AG15-NSEQ4B PICTURE  9(8)                     CI0028
                          BINARY.                                       CI0028
            12            AG15-GRDOR  PICTURE  9(8).                    CI0028
            12            AG15-GRIAD  PICTURE  9(8).                    CI0028
            12            AG15-GECUC  PICTURE  99.                      CI0028
            12            AG15-GRLNG  PICTURE  99.                      CI0028
            12            AG15-GESLC  PICTURE  99.                      CI0028
            12            AG15-AYSIDA PICTURE  9(3).                    CI0028
            12            AG15-AYSID  PICTURE  9(5).                    CI0028
            12            AG15-GRCSD  PICTURE  9(8).                    CI0028
            12            AG15-GRCFD  PICTURE  9(8).                    CI0028
            12            AG15-GRNCL  PICTURE  S9(5)                    CI0028
                          COMPUTATIONAL-3.                              CI0028
            12            AG15-GRNCT  PICTURE  S9(5)                    CI0028
                          COMPUTATIONAL-3.                              CI0028
            12            AG15-GRSFC  PICTURE  99.                      CI0028
            12            AG15-GRCRN  PICTURE  9(3).                    CI0028
            12            AG15-GRCSS  PICTURE  X.                       CI0028
            12            AG15-MKSRC  PICTURE  99                       CI0028
                          OCCURS       010     TIMES.                   CI0028
            12            AG15-NEFPS  PICTURE  X(5).                    CI0028
            12            AG15-DEFPS  PICTURE  9(8).                    CI0028
            12            AG15-DLSRV  PICTURE  9(8).                    CI0028
            12            AG15-CTLNI  PICTURE  X.                       CI0028
            12            AG15-CGRLI  PICTURE  X.                       CI0028
            12            AG15-CAMGR  PICTURE  9(5)                     CI0028
                          COMPUTATIONAL-3.                              CI0028
            12            AG15-CAMGS  PICTURE  9(5)                     CI0028
                          COMPUTATIONAL-3.                              CI0028
            12            AG15-CAMGN  PICTURE  9(3)                     CI0028
                          COMPUTATIONAL-3.                              CI0028
            12            AG15-CGRMF  PICTURE  X.                       CI0028
            12            AG15-FILLER PICTURE  X(08).                   CI0028
            11            AG15-GR07.                                    CI0028
            12            AG15-GEDLA  PICTURE  9(8).                    CI0028
            12            AG15-GRAID  PICTURE  X(12).                   CI0028
            12            AG15-GRPAP  PICTURE  X(14).                   CI0028
            12            AG15-GEPHNX PICTURE  9(4).                    CI0028
            12            AG15-DPLEF  PICTURE  9(8).                    CI0028
            12            AG15-DPLAM  PICTURE  9(8).                    CI0028
            12            AG15-NCPFN  PICTURE  9(6).                    CI0028
            12            AG15-GEFYE  PICTURE  9(4).                    CI0028
            12            AG15-FILLER PICTURE  X(06).                   CI0028
            12            AG15-GRPAN  PICTURE  X(45).                   CI0028
            12            AG15-CGRPA  PICTURE  99.                      CI0028
            12            AG15-IPRTT7 PICTURE  X.                       CI0028
            12            AG15-GRPED  PICTURE  9(8).                    CI0028
            12            AG15-FILLER PICTURE  X(05).                   CI0028
            12            AG15-GRPLC  PICTURE  99.                      CI0028
            12            AG15-GRPLT  PICTURE  99.                      CI0028
            12            AG15-FILLER PICTURE  X(04).                   CI0028
            12            AG15-GEADI  PICTURE  X.                       CI0028
            12            AG15-GRCFA  PICTURE  S9(11)V99                CI0028
                          COMPUTATIONAL-3.                              CI0028
            12            AG15-GECFY  PICTURE  9(4).                    CI0028
            12            AG15-GECFC  PICTURE  99.                      CI0028
            12            AG15-MEMPL  PICTURE  X(20).                   CI0028
            12            AG15-CAUNIT PICTURE  X(4).                    CI0028
            12            AG15-FILLER PICTURE  X(21).                   CI0028
            12            AG15-GRPPP  PICTURE  999.                     CI0028
            12            AG15-CCORT  PICTURE  9(3).                    CI0028
            12            AG15-CIDRP  PICTURE  99.                      CI0028
            12            AG15-CCDWA  PICTURE  9.                       CI0028
            12            AG15-IERSA  PICTURE  X.                       CI0028
            12            AG15-DERSA  PICTURE  9(8).                    CI0028
            12            AG15-FILLER PICTURE  X(04).                   CI0028
            10            AG15-QITEM  PICTURE  9(3).                    CI0028
            10            AG15-XIMAX  PICTURE  S9(4)                    CI0028
                          BINARY.                                       CI0028
            10            AG15-FILLER PICTURE  X(100).                  CI0028
      *                                                                 AMDU15
      *                                                                 AMDU15
      *                                                                 AMDU15
      *                                                                 AMDU15
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=AW DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 AW00.                                         CI0028
          05              AW00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00541).                  CI0028
       01                 AW34  REDEFINES      AW00.                    CI0028
            10            AW34-C299.                                    CI0028
            11            AW34-CTID.                                    CI0028
            12            AW34-CTIDA  PICTURE  9(3).                    CI0028
            12            AW34-CTIDN.                                   CI0028
            13            AW34-CTIDNP PICTURE  X(13).                   CI0028
            13            AW34-CTIDND PICTURE  9(11).                   CI0028
            10            AW34-DCACG  PICTURE  9(8).                    CI0028
            10            AW34-IPOCH  PICTURE  X.                       CI0028
            10            AW34-FILLER PICTURE  X(100).                  CI0028
            10            AW34-CT22                                     CI0028
                          OCCURS       010     TIMES.                   CI0028
            11            AW34-CT22K.                                   CI0028
            12            AW34-CGVEN  PICTURE  X(2).                    CI0028
            12            AW34-CTWHC  PICTURE  9(2).                    CI0028
            11            AW34-CFCNTY PICTURE  X(2).                    CI0028
            11            AW34-DLAUP  PICTURE  9(8).                    CI0028
            11            AW34-CTWTC  PICTURE  9(2).                    CI0028
            11            AW34-CTWHAT PICTURE  S9(7)V99                 CI0028
                          COMPUTATIONAL-3.                              CI0028
            11            AW34-CTWHP  PICTURE  9(3)V99                  CI0028
                          COMPUTATIONAL-3.                              CI0028
            11            AW34-FILLER PICTURE  X(06).                   CI0028
            10            AW34-QITEM  PICTURE  9(3).                    CI0028
            10            AW34-XIMAX  PICTURE  S9(4)                    CI0028
                          BINARY.                                       CI0028
            10            AW34-CTXMT  PICTURE  9(2).                    CI0028
            10            AW34-CTCUS  PICTURE  999.                     CI0028
            10            AW34-FILLER PICTURE  X(95).                   CI0028
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
       01                 CL01.                                         CI0028
            10            CL01-CL01K.                                   CI0028
            11            CL01-C199.                                    CI0028
            12            CL01-CLID.                                    CI0028
            13            CL01-CLIDO  PICTURE  9(3).                    CI0028
            13            CL01-CLIDN.                                   CI0028
            14            CL01-CLIDNP PICTURE  X(12).                   CI0028
            14            CL01-CLIDND PICTURE  9(8).                    CI0028
            10            CL01-GECKD  PICTURE  9.                       CI0028
            10            CL01-GEMDA  PICTURE  9(8).                    CI0028
            10            CL01-NSEQ4B PICTURE  9(8)                     CI0028
                          BINARY.                                       CI0028
            10            CL01-GECUC  PICTURE  99.                      CI0028
            10            CL01-CLDOR  PICTURE  9(8).                    CI0028
            10            CL01-CLLNG  PICTURE  XX.                      CI0028
            10            CL01-GESLC  PICTURE  99.                      CI0028
            10            CL01-CLTYP  PICTURE  X.                       CI0028
            10            CL01-CLCLS  PICTURE  9(3).                    CI0028
            10            CL01-CLTWRC PICTURE  99.                      CI0028
            10            CL01-CLPVC  PICTURE  99.                      CI0028
            10            CL01-CLIND  PICTURE  9(3).                    CI0028
            10            CL01-CLTRC  PICTURE  99.                      CI0028
            10            CL01-CLIASS PICTURE  S9(7)V99                 CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            CL01-AYSIDA PICTURE  9(3).                    CI0028
            10            CL01-AYSID  PICTURE  9(5).                    CI0028
            10            CL01-CLSTR  PICTURE  9(2).                    CI0028
            10            CL01-CLC11  PICTURE  X.                       CI0028
            10            CL01-CLTIN  PICTURE  9(12).                   CI0028
            10            CL01-CLTND  PICTURE  9(8).                    CI0028
            10            CL01-CLTINC PICTURE  9.                       CI0028
            10            CL01-CCDWA  PICTURE  9.                       CI0028
            10            CL01-CICES  PICTURE  X.                       CI0028
            10            CL01-CLTRA  PICTURE  9(2).                    CI0028
            10            CL01-DIRSY  PICTURE  9(4)                     CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            CL01-CFEDS  PICTURE  X.                       CI0028
            10            CL01-FILLER PICTURE  X(06).                   CI0028
       01                 CL12.                                         CI0028
            10            CL12-GEDLA  PICTURE  9(8).                    CI0028
            10            CL12-CLBCD  PICTURE  9(3).                    CI0028
            10            CL12-CLFDW  PICTURE  X.                       CI0028
            10            CL12-CLOSD  PICTURE  9(8).                    CI0028
            10            CL12-CLOED  PICTURE  9(8).                    CI0028
            10            CL12-CLOEI  PICTURE  X.                       CI0028
            10            CL12-CLIBN  PICTURE  X(20).                   CI0028
            10            CL12-CLINT  PICTURE  9(3).                    CI0028
            10            CL12-CLONE  PICTURE  9(9).                    CI0028
            10            CL12-CLORC  PICTURE  99.                      CI0028
            10            CL12-CLORN  PICTURE  X(45).                   CI0028
            10            CL12-CLORP  PICTURE  X(25).                   CI0028
            10            CL12-GEPHNB PICTURE  X(14).                   CI0028
            10            CL12-GEPHNX PICTURE  9(4).                    CI0028
            10            CL12-GEPHNA PICTURE  X(14).                   CI0028
            10            CL12-GEFYE  PICTURE  9(4).                    CI0028
            10            CL12-AYCDE  PICTURE  9(3).                    CI0028
            10            CL12-AYID   PICTURE  9(5).                    CI0028
            10            CL12-CFOBO  PICTURE  99.                      CI0028
            10            CL12-CLINRG                                   CI0028
                          OCCURS       003     TIMES.                   CI0028
            11            CL12-CLIRT  PICTURE  99.                      CI0028
            11            CL12-CLINR  PICTURE  X(3).                    CI0028
            11            CL12-CLIRD  PICTURE  9(8).                    CI0028
            10            CL12-IOTXE  PICTURE  X.                       CI0028
            10            CL12-IO501  PICTURE  X.                       CI0028
            10            CL12-IOFOG  PICTURE  X.                       CI0028
            10            CL12-IOPRA  PICTURE  X.                       CI0028
            10            CL12-IOSCS  PICTURE  X.                       CI0028
            10            CL12-IACHA  PICTURE  X.                       CI0028
            10            CL12-IFORG  PICTURE  X.                       CI0028
            10            CL12-IFIND  PICTURE  X.                       CI0028
            10            CL12-CFCNT3 PICTURE  X(2).                    CI0028
            10            CL12-FILLER PICTURE  X(06).                   CI0028
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU002
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0019           PIC X(8) VALUE 'CI0019P '.                  AM0019
       01  CI0027           PIC X(8) VALUE 'CI0027P '.                  AM0027
       01                 CT01.                                         CI0028
            10            CT01-CT01K.                                   CI0028
            11            CT01-C299.                                    CI0028
            12            CT01-CTID.                                    CI0028
            13            CT01-CTIDA  PICTURE  9(3).                    CI0028
            13            CT01-CTIDN.                                   CI0028
            14            CT01-CTIDNP PICTURE  X(13).                   CI0028
            14            CT01-CTIDND PICTURE  9(11).                   CI0028
            10            CT01-GECKD  PICTURE  9.                       CI0028
            10            CT01-GEMDA  PICTURE  9(8).                    CI0028
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0028
                          BINARY.                                       CI0028
            10            CT01-GECUC  PICTURE  99.                      CI0028
            10            CT01-CTAUL  PICTURE  9(3).                    CI0028
            10            CT01-DIRAC  PICTURE  9(4).                    CI0028
            10            CT01-CTCCI  PICTURE  X.                       CI0028
            10            CT01-CTCUS  PICTURE  999.                     CI0028
            10            CT01-CTEFD  PICTURE  9(8).                    CI0028
            10            CT01-CTIAD  PICTURE  9(8).                    CI0028
            10            CT01-CLCUS  PICTURE  99.                      CI0028
            10            CT01-CAMMB  PICTURE  X(3).                    CI0028
            10            CT01-CKPMM  PICTURE  X.                       CI0028
            10            CT01-CTLAD  PICTURE  9(8).                    CI0028
            10            CT01-IPERS  PICTURE  X.                       CI0028
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            CT01-CTLAT  PICTURE  9(8).                    CI0028
            10            CT01-CTLATC PICTURE  9(6).                    CI0028
            10            CT01-IMEGA  PICTURE  X.                       CI0028
            10            CT01-DIRAB  PICTURE  9(8).                    CI0028
            10            CT01-COLRQ  PICTURE  X.                       CI0028
            10            CT01-ZDA04  PICTURE  X(4).                    CI0028
            10            CT01-CTLPD  PICTURE  9(8).                    CI0028
            10            CT01-CIRASP PICTURE  9.                       CI0028
            10            CT01-CIRATP PICTURE  99.                      CI0028
            10            CT01-DRTHC  PICTURE  9(8).                    CI0028
            10            CT01-CPPTC  PICTURE  X.                       CI0028
            10            CT01-ZDA06  PICTURE  X(6).                    CI0028
            10            CT01-CTACD  PICTURE  9(8).                    CI0028
            10            CT01-CTNLI  PICTURE  X.                       CI0028
            10            CT01-CTRHO  PICTURE  9(8).                    CI0028
            10            CT01-CTSGD  PICTURE  9(8).                    CI0028
            10            CT01-CPATP  PICTURE  X(1).                    CI0028
            10            CT01-IRSTA  PICTURE  X.                       CI0028
            10            CT01-CTSTA  PICTURE  99.                      CI0028
            10            CT01-CTSSC  PICTURE  99.                      CI0028
            10            CT01-PRLIN  PICTURE  9(3).                    CI0028
            10            CT01-PRCOD  PICTURE  9(5).                    CI0028
            10            CT01-PRSCD  PICTURE  X(9).                    CI0028
            10            CT01-CTLNI  PICTURE  X.                       CI0028
            10            CT01-AYSIDA PICTURE  9(3).                    CI0028
            10            CT01-AYSID  PICTURE  9(5).                    CI0028
            10            CT01-CTBMC  PICTURE  99.                      CI0028
            10            CT01-CINAR  PICTURE  99.                      CI0028
            10            CT01-CPHTR  PICTURE  X.                       CI0028
            10            CT01-CDSTR  PICTURE  XX.                      CI0028
            10            CT01-CQACT  PICTURE  999.                     CI0028
            10            CT01-CIRAS  PICTURE  999.                     CI0028
            10            CT01-CIRAT  PICTURE  999.                     CI0028
            10            CT01-CLRAY  PICTURE  9(5).                    CI0028
            10            CT01-CATTP  PICTURE  X.                       CI0028
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0028
            10            XW05-XW06.                                    CI0028
            11            XW05-XDBPCB.                                  CI0028
            12            XW05-XDBDNM PICTURE  X(08)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-XSEGLV PICTURE  X(02)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-XRC    PICTURE  X(02)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-XPROPT PICTURE  X(04)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-FILLER PICTURE  S9(5)                    CI0028
                          VALUE                ZERO                     CI0028
                          BINARY.                                       CI0028
            12            XW05-XSEGNM PICTURE  X(08)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0028
                          VALUE                ZERO                     CI0028
                          BINARY.                                       CI0028
            12            XW05-XSEGNB PICTURE  9(05)                    CI0028
                          VALUE                ZERO                     CI0028
                          BINARY.                                       CI0028
            12            XW05-XCOKEY PICTURE  X(70)                    CI0028
                          VALUE                SPACE.                   CI0028
            10            XW05-XW07.                                    CI0028
            11            XW05-XIOPCB.                                  CI0028
            12            XW05-XTERMI PICTURE  X(08)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-FILLER PICTURE  XX                       CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-XRC1   PICTURE  X(02)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-FILLER PICTURE  X(12)                    CI0028
                          VALUE                SPACE.                   CI0028
            12            XW05-XMODNM PICTURE  X(8)                     CI0028
                          VALUE                SPACE.                   CI0028
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0028
                          VALUE                ZERO.                    CI0028
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0028
                          VALUE                ZERO.                    CI0028
            10            XW05-XGU    PICTURE  X(4)                     CI0028
                          VALUE                'GU  '.                  CI0028
            10            XW05-XGHU   PICTURE  X(4)                     CI0028
                          VALUE                'GHU '.                  CI0028
            10            XW05-XGN    PICTURE  X(4)                     CI0028
                          VALUE                'GN  '.                  CI0028
            10            XW05-XGHN   PICTURE  X(4)                     CI0028
                          VALUE                'GHN '.                  CI0028
            10            XW05-XGNP   PICTURE  X(4)                     CI0028
                          VALUE                'GNP '.                  CI0028
            10            XW05-XGHNP  PICTURE  X(4)                     CI0028
                          VALUE                'GHNP'.                  CI0028
            10            XW05-XREPL  PICTURE  XXXX                     CI0028
                          VALUE                'REPL'.                  CI0028
            10            XW05-XISRT  PICTURE  X(4)                     CI0028
                          VALUE                'ISRT'.                  CI0028
            10            XW05-XDLET  PICTURE  X(4)                     CI0028
                          VALUE                'DLET'.                  CI0028
            10            XW05-XOPEN  PICTURE  X(4)                     CI0028
                          VALUE                'OPEN'.                  CI0028
            10            XW05-XCLSE  PICTURE  X(4)                     CI0028
                          VALUE                'CLSE'.                  CI0028
            10            XW05-XCHKP  PICTURE  X(4)                     CI0028
                          VALUE                'CHKP'.                  CI0028
            10            XW05-XXRST  PICTURE  X(4)                     CI0028
                          VALUE                'XRST'.                  CI0028
            10            XW05-XTERM  PICTURE  X(4)                     CI0028
                          VALUE                'TERM'.                  CI0028
            10            XW05-XNFPAC PICTURE  X(13)                    CI0028
                          VALUE                SPACE.                   CI0028
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0028
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0028

      *WORK AREA FOR GRIDND FOR PENSION GROUPS
       01  7-GR01-GRIDND.
           05  7-GR01-GRID-610 PIC 9(3).
           05  FILLER          PIC 9(5).
       01                 GR01.                                         CI0028
            10            GR01-GR01K.                                   CI0028
            11            GR01-GR98.                                    CI0028
            12            GR01-GRID.                                    CI0028
            13            GR01-GRIDC  PICTURE  9(3).                    CI0028
            13            GR01-GRIDN.                                   CI0028
            14            GR01-GRIDNP PICTURE  99.                      CI0028
            14            GR01-GRIDND PICTURE  9(8).                    CI0028
            10            GR01-GECKD  PICTURE  9.                       CI0028
            10            GR01-GEMDA  PICTURE  9(8).                    CI0028
            10            GR01-NSEQ4B PICTURE  9(8)                     CI0028
                          BINARY.                                       CI0028
            10            GR01-GRDOR  PICTURE  9(8).                    CI0028
            10            GR01-GRIAD  PICTURE  9(8).                    CI0028
            10            GR01-GECUC  PICTURE  99.                      CI0028
            10            GR01-GRLNG  PICTURE  99.                      CI0028
            10            GR01-GESLC  PICTURE  99.                      CI0028
            10            GR01-AYSIDA PICTURE  9(3).                    CI0028
            10            GR01-AYSID  PICTURE  9(5).                    CI0028
            10            GR01-GRCSD  PICTURE  9(8).                    CI0028
            10            GR01-GRCFD  PICTURE  9(8).                    CI0028
            10            GR01-GRNCL  PICTURE  S9(5)                    CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            GR01-GRNCT  PICTURE  S9(5)                    CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            GR01-GRSFC  PICTURE  99.                      CI0028
            10            GR01-GRCRN  PICTURE  9(3).                    CI0028
            10            GR01-GRCSS  PICTURE  X.                       CI0028
            10            GR01-MKSRC  PICTURE  99                       CI0028
                          OCCURS       010     TIMES.                   CI0028
            10            GR01-NEFPS  PICTURE  X(5).                    CI0028
            10            GR01-DEFPS  PICTURE  9(8).                    CI0028
            10            GR01-DLSRV  PICTURE  9(8).                    CI0028
            10            GR01-CTLNI  PICTURE  X.                       CI0028
            10            GR01-CGRLI  PICTURE  X.                       CI0028
            10            GR01-CAMGR  PICTURE  9(5)                     CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            GR01-CAMGS  PICTURE  9(5)                     CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            GR01-CAMGN  PICTURE  9(3)                     CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            GR01-CGRMF  PICTURE  X.                       CI0028
            10            GR01-FILLER PICTURE  X(08).                   CI0028
       01                 GR07.                                         CI0028
            10            GR07-GEDLA  PICTURE  9(8).                    CI0028
            10            GR07-GRAID  PICTURE  X(12).                   CI0028
            10            GR07-GRPAP  PICTURE  X(14).                   CI0028
            10            GR07-GEPHNX PICTURE  9(4).                    CI0028
            10            GR07-DPLEF  PICTURE  9(8).                    CI0028
            10            GR07-DPLAM  PICTURE  9(8).                    CI0028
            10            GR07-NCPFN  PICTURE  9(6).                    CI0028
            10            GR07-GEFYE  PICTURE  9(4).                    CI0028
            10            GR07-FILLER PICTURE  X(06).                   CI0028
            10            GR07-GRPAN  PICTURE  X(45).                   CI0028
            10            GR07-CGRPA  PICTURE  99.                      CI0028
            10            GR07-IPRTT7 PICTURE  X.                       CI0028
            10            GR07-GRPED  PICTURE  9(8).                    CI0028
            10            GR07-FILLER PICTURE  X(05).                   CI0028
            10            GR07-GRPLC  PICTURE  99.                      CI0028
            10            GR07-GRPLT  PICTURE  99.                      CI0028
            10            GR07-FILLER PICTURE  X(04).                   CI0028
            10            GR07-GEADI  PICTURE  X.                       CI0028
            10            GR07-GRCFA  PICTURE  S9(11)V99                CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            GR07-GECFY  PICTURE  9(4).                    CI0028
            10            GR07-GECFC  PICTURE  99.                      CI0028
            10            GR07-MEMPL  PICTURE  X(20).                   CI0028
            10            GR07-CAUNIT PICTURE  X(4).                    CI0028
            10            GR07-FILLER PICTURE  X(21).                   CI0028
            10            GR07-GRPPP  PICTURE  999.                     CI0028
            10            GR07-CCORT  PICTURE  9(3).                    CI0028
            10            GR07-CIDRP  PICTURE  99.                      CI0028
            10            GR07-CCDWA  PICTURE  9.                       CI0028
            10            GR07-IERSA  PICTURE  X.                       CI0028
            10            GR07-DERSA  PICTURE  9(8).                    CI0028
            10            GR07-FILLER PICTURE  X(04).                   CI0028
      *
      ******************************************************************
      **     CT22 TABLE INDICES FOR TAX MARKET DETERMINATION           *
      ******************************************************************
      *
      *                   AW34
      *
      *
      *
      *
      *GENERATE INDEX FOR MESSAGE TEXT (ONLY IF 'OCCURS' SPECIFIED)     ADU071
      *                   MS03                                          ADU071
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018A-PCB-ADDRESS-LIST.                                    AM0018
           05  CI0018A-PCB-CT1P-PTR1      POINTER.                      AM0018
                                                                        AM0019
      ******************************************************************AM0019
      **     PCB ADDRESS LIST FOR CI0019.  MODULE CI0019 WILL NEED     *AM0019
      **     PCB'S FOR:                                                *AM0019
      **                CONTRACT DATABASE(CT1P)                        *AM0019
      **                GROUP DATABASE(GR1P)                           *AM0019
      ******************************************************************AM0019
                                                                        AM0019
       01  CI0019B-PCB-ADDRESS-LIST.                                    AM0019
           05  CI0019B-PCB-CT1P-PTR1      POINTER.                      AM0019
           05  CI0019B-PCB-GR1P-PTR1      POINTER.                      AM0019
                                                                        AM0027
      ******************************************************************AM0027
      **     PCB ADDRESS LIST FOR CI0027.  MODULE CI0027 WILL NEED     *AM0027
      **     PCB'S FOR:                                                *AM0027
      **                CONTRACT DATABASE(CT1P)                        *AM0027
      ******************************************************************AM0027
                                                                        AM0027
       01  CI0027C-PCB-ADDRESS-LIST.                                    AM0027
           05  CI0027C-PCB-CT1P-PTR1      POINTER.                      AM0027
       01   DEBUT-WSS.                                                  CI0028
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0028
            05   IK     PICTURE X.                                      CI0028
       01  CONSTANTES-PAC.                                              CI0028
           05  FILLER  PICTURE X(87)   VALUE                            CI0028
                     '6015 CAT09/08/14CI0028ADMIN   14:34:20CI0028P AMERCI0028
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0028
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0028
           05  NUGNA   PICTURE X(5).                                    CI0028
           05  APPLI   PICTURE X(3).                                    CI0028
           05  DATGN   PICTURE X(8).                                    CI0028
           05  PROGR   PICTURE X(6).                                    CI0028
           05  CODUTI  PICTURE X(8).                                    CI0028
           05  TIMGN   PICTURE X(8).                                    CI0028
           05  PROGE   PICTURE X(8).                                    CI0028
           05  COBASE  PICTURE X(4).                                    CI0028
           05  DATGNC  PICTURE X(10).                                   CI0028
           05  RELEAS  PICTURE X(7).                                    CI0028
           05  DATGE   PICTURE X(10).                                   CI0028
           05  DATSQ   PICTURE X(10).                                   CI0028
       01  DATCE.                                                       CI0028
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0028
         05  DATOR.                                                     CI0028
           10  DATOA  PICTURE XX.                                       CI0028
           10  DATOM  PICTURE XX.                                       CI0028
           10  DATOJ  PICTURE XX.                                       CI0028
       01   VARIABLES-CONDITIONNELLES.                                  CI0028
            05                  FT      PICTURE X VALUE '0'.            CI0028
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0028
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0028
            05           IAW34L PICTURE S9(4) VALUE  ZERO.
            05           IAW34R PICTURE S9(4) VALUE  ZERO.
            05           IAW34M PICTURE S9(4) VALUE +0010.
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU071
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU071
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0028
            05       5-CL00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0028
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0028
            05       5-GR00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0028
       01               S-CL01-SSA.                                     CI0028
            10         S1-CL01-SEGNAM PICTURE X(8)                      CI0028
                                      VALUE 'CL01    '.                 CI0028
            10         S1-CL01-CCOM   PICTURE X VALUE '*'.              CI0028
            10          S-CL01-CCOD   PICTURE X(5)                      CI0028
                                      VALUE '-----'.                    CI0028
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0028
       01            S-CLU01-SSA.                                       CI0028
            10      S1-CLU01-SEGNAM PICTURE X(8)                        CI0028
                                      VALUE 'CL01    '.                 CI0028
            10      S1-CLU01-CCOM   PICTURE X VALUE '*'.                CI0028
            10       S-CLU01-CCOD   PICTURE X(5)                        CI0028
                                      VALUE '-----'.                    CI0028
            10      S1-CLU01-FLDNAM PICTURE X(9)                        CI0028
                                      VALUE '(CL01K'.                   CI0028
            10       S-CLU01-OPER  PICTURE XX VALUE ' ='.               CI0028
            10       S-CLU01-CL01K.                                     CI0028
            11       S-CLU01-C199.                                      CI0028
            12       S-CLU01-CLID.                                      CI0028
            13       S-CLU01-CLIDO    PICTURE  9(3).                    CI0028
            13       S-CLU01-CLIDN.                                     CI0028
            14       S-CLU01-CLIDNP   PICTURE  X(12).                   CI0028
            14       S-CLU01-CLIDND   PICTURE  9(8).                    CI0028
            10  FILLER   PICTURE X    VALUE ')'.                        CI0028
       01               S-CL12-SSA.                                     CI0028
            10         S1-CL12-SEGNAM PICTURE X(8)                      CI0028
                                      VALUE 'CL12    '.                 CI0028
            10         S1-CL12-CCOM   PICTURE X VALUE '*'.              CI0028
            10          S-CL12-CCOD   PICTURE X(5)                      CI0028
                                      VALUE '-----'.                    CI0028
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0028
       01               S-CT01-SSA.                                     CI0028
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0028
                                      VALUE 'CT01    '.                 CI0028
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0028
            10          S-CT01-CCOD   PICTURE X(5)                      CI0028
                                      VALUE '-----'.                    CI0028
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0028
       01            S-CTU01-SSA.                                       CI0028
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0028
                                      VALUE 'CT01    '.                 CI0028
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0028
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0028
                                      VALUE '-----'.                    CI0028
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0028
                                      VALUE '(CT01K'.                   CI0028
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0028
            10       S-CTU01-CT01K.                                     CI0028
            11       S-CTU01-C299.                                      CI0028
            12       S-CTU01-CTID.                                      CI0028
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0028
            13       S-CTU01-CTIDN.                                     CI0028
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0028
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0028
            10  FILLER   PICTURE X    VALUE ')'.                        CI0028
       01               S-GR01-SSA.                                     CI0028
            10         S1-GR01-SEGNAM PICTURE X(8)                      CI0028
                                      VALUE 'GR01    '.                 CI0028
            10         S1-GR01-CCOM   PICTURE X VALUE '*'.              CI0028
            10          S-GR01-CCOD   PICTURE X(5)                      CI0028
                                      VALUE '-----'.                    CI0028
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0028
       01            S-GRU01-SSA.                                       CI0028
            10      S1-GRU01-SEGNAM PICTURE X(8)                        CI0028
                                      VALUE 'GR01    '.                 CI0028
            10      S1-GRU01-CCOM   PICTURE X VALUE '*'.                CI0028
            10       S-GRU01-CCOD   PICTURE X(5)                        CI0028
                                      VALUE '-----'.                    CI0028
            10      S1-GRU01-FLDNAM PICTURE X(9)                        CI0028
                                      VALUE '(GR01K'.                   CI0028
            10       S-GRU01-OPER  PICTURE XX VALUE ' ='.               CI0028
            10       S-GRU01-GR01K.                                     CI0028
            11       S-GRU01-GR98.                                      CI0028
            12       S-GRU01-GRID.                                      CI0028
            13       S-GRU01-GRIDC    PICTURE  9(3).                    CI0028
            13       S-GRU01-GRIDN.                                     CI0028
            14       S-GRU01-GRIDNP   PICTURE  99.                      CI0028
            14       S-GRU01-GRIDND   PICTURE  9(8).                    CI0028
            10  FILLER   PICTURE X    VALUE ')'.                        CI0028
       01               S-GR07-SSA.                                     CI0028
            10         S1-GR07-SEGNAM PICTURE X(8)                      CI0028
                                      VALUE 'GR07    '.                 CI0028
            10         S1-GR07-CCOM   PICTURE X VALUE '*'.              CI0028
            10          S-GR07-CCOD   PICTURE X(5)                      CI0028
                                      VALUE '-----'.                    CI0028
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0028
       01            S-GRA07-SSA.                                       CI0028
            10      S1-GRA07-SEGNAM PICTURE X(8)                        CI0028
                                      VALUE 'GR07    '.                 CI0028
            10      S1-GRA07-CCOM   PICTURE X VALUE '*'.                CI0028
            10       S-GRA07-CCOD   PICTURE X(5)                        CI0028
                                      VALUE '-----'.                    CI0028
            10      S1-GRA07-FLDNAM PICTURE X(9)                        CI0028
                                      VALUE '(NCPFN'.                   CI0028
            10       S-GRA07-OPER  PICTURE XX VALUE ' ='.               CI0028
            10       S-GRA07-NCPFN    PICTURE  9(6).                    CI0028
            10  FILLER   PICTURE X    VALUE ')'.                        CI0028
       01   ZONES-UTILISATEUR PICTURE X.                                CI0028
      *                                                                 AMDU35
      ******************************************************************AMDU35
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU35
      **     TAX MARKET SEGMENT FOR AN ACCOUNT NUMBER PASSED.          *AMDU35
      ******************************************************************AMDU35
      *                                                                 AMDU35
      *!WF DSP=S1 DSL=DU SEL=35 FOR=I LEV=1                             AMDU35
       01                 S100.                                         CI0028
          05              S100-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00237).                  CI0028
       01                 S135  REDEFINES      S100.                    CI0028
            10            S135-C299.                                    CI0028
            11            S135-CTID.                                    CI0028
            12            S135-CTIDA  PICTURE  9(3).                    CI0028
            12            S135-CTIDN.                                   CI0028
            13            S135-CTIDNP PICTURE  X(13).                   CI0028
            13            S135-CTIDND PICTURE  9(11).                   CI0028
            10            S135-DCACG  PICTURE  9(8).                    CI0028
            10            S135-FILLER PICTURE  X(100).                  CI0028
            10            S135-CTXMT  PICTURE  9(2).                    CI0028
            10            S135-CTCUS  PICTURE  999.                     CI0028
            10            S135-CGRMF  PICTURE  X.                       CI0028
            10            S135-GRPLC  PICTURE  99.                      CI0028
            10            S135-GRPLT  PICTURE  99.                      CI0028
            10            S135-FILLER PICTURE  X(092).                  CI0028
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      ******************************************************************AMDU35
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU35
      **     TAX MARKET SEGMENT FOR AN ACCOUNT NUMBER PASSED.          *AMDU35
      ******************************************************************AMDU35
      *                                                                 AMDU35
      *!WF DSP=S2 DSL=DU SEL=35 FOR=I LEV=1                             AMDU35
       01                 S200.                                         CI0028
          05              S200-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00237).                  CI0028
       01                 S235  REDEFINES      S200.                    CI0028
            10            S235-C299.                                    CI0028
            11            S235-CTID.                                    CI0028
            12            S235-CTIDA  PICTURE  9(3).                    CI0028
            12            S235-CTIDN.                                   CI0028
            13            S235-CTIDNP PICTURE  X(13).                   CI0028
            13            S235-CTIDND PICTURE  9(11).                   CI0028
            10            S235-DCACG  PICTURE  9(8).                    CI0028
            10            S235-FILLER PICTURE  X(100).                  CI0028
            10            S235-CTXMT  PICTURE  9(2).                    CI0028
            10            S235-CTCUS  PICTURE  999.                     CI0028
            10            S235-CGRMF  PICTURE  X.                       CI0028
            10            S235-GRPLC  PICTURE  99.                      CI0028
            10            S235-GRPLT  PICTURE  99.                      CI0028
            10            S235-FILLER PICTURE  X(092).                  CI0028
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
       LINKAGE SECTION.                                                 ADU002
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
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR GR1P                                           ADU015
            05 PCB-GR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0028
          05              PA00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00106).                  CI0028
       01                 PA06  REDEFINES      PA00.                    CI0028
            10            PA06-XDBPCB.                                  CI0028
            11            PA06-XDBDNM PICTURE  X(08).                   CI0028
            11            PA06-XSEGLV PICTURE  X(02).                   CI0028
            11            PA06-XRC    PICTURE  X(02).                   CI0028
            11            PA06-XPROPT PICTURE  X(04).                   CI0028
            11            PA06-FILLER PICTURE  S9(5)                    CI0028
                          BINARY.                                       CI0028
            11            PA06-XSEGNM PICTURE  X(08).                   CI0028
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0028
                          BINARY.                                       CI0028
            11            PA06-XSEGNB PICTURE  9(05)                    CI0028
                          BINARY.                                       CI0028
            11            PA06-XCOKEY PICTURE  X(70).                   CI0028
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0028
          05              PB00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00106).                  CI0028
       01                 PB06  REDEFINES      PB00.                    CI0028
            10            PB06-XDBPCB.                                  CI0028
            11            PB06-XDBDNM PICTURE  X(08).                   CI0028
            11            PB06-XSEGLV PICTURE  X(02).                   CI0028
            11            PB06-XRC    PICTURE  X(02).                   CI0028
            11            PB06-XPROPT PICTURE  X(04).                   CI0028
            11            PB06-FILLER PICTURE  S9(5)                    CI0028
                          BINARY.                                       CI0028
            11            PB06-XSEGNM PICTURE  X(08).                   CI0028
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0028
                          BINARY.                                       CI0028
            11            PB06-XSEGNB PICTURE  9(05)                    CI0028
                          BINARY.                                       CI0028
            11            PB06-XCOKEY PICTURE  X(70).                   CI0028
      *** PCB MASK FOR GR1P                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0028
          05              PC00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00106).                  CI0028
       01                 PC06  REDEFINES      PC00.                    CI0028
            10            PC06-XDBPCB.                                  CI0028
            11            PC06-XDBDNM PICTURE  X(08).                   CI0028
            11            PC06-XSEGLV PICTURE  X(02).                   CI0028
            11            PC06-XRC    PICTURE  X(02).                   CI0028
            11            PC06-XPROPT PICTURE  X(04).                   CI0028
            11            PC06-FILLER PICTURE  S9(5)                    CI0028
                          BINARY.                                       CI0028
            11            PC06-XSEGNM PICTURE  X(08).                   CI0028
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0028
                          BINARY.                                       CI0028
            11            PC06-XSEGNB PICTURE  9(05)                    CI0028
                          BINARY.                                       CI0028
            11            PC06-XCOKEY PICTURE  X(70).                   CI0028
      *                                                                 AMDU35
      ******************************************************************AMDU35
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU35
      **     TAX MARKET SEGMENT FOR AN ACCOUNT NUMBER PASSED.          *AMDU35
      ******************************************************************AMDU35
      *                                                                 AMDU35
      *!WF DSP=TM DSL=DU SEL=35 FOR=I LEV=1                             AMDU35
       01                 TM00.                                         CI0028
          05              TM00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00237).                  CI0028
       01                 TM35  REDEFINES      TM00.                    CI0028
            10            TM35-C299.                                    CI0028
            11            TM35-CTID.                                    CI0028
            12            TM35-CTIDA  PICTURE  9(3).                    CI0028
            12            TM35-CTIDN.                                   CI0028
            13            TM35-CTIDNP PICTURE  X(13).                   CI0028
            13            TM35-CTIDND PICTURE  9(11).                   CI0028
            10            TM35-DCACG  PICTURE  9(8).                    CI0028
            10            TM35-FILLER PICTURE  X(100).                  CI0028
            10            TM35-CTXMT  PICTURE  9(2).                    CI0028
            10            TM35-CTCUS  PICTURE  999.                     CI0028
            10            TM35-CGRMF  PICTURE  X.                       CI0028
            10            TM35-GRPLC  PICTURE  99.                      CI0028
            10            TM35-GRPLT  PICTURE  99.                      CI0028
            10            TM35-FILLER PICTURE  X(092).                  CI0028
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU35
      *                                                                 AMDU10
      ******************************************************************AMDU10
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *AMDU10
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *AMDU10
      ******************************************************************AMDU10
      *                                                                 AMDU10
      *!WF DSP=DE DSL=DU SEL=10 FOR=I LEV=1                             AMDU10
       01                 DE00.                                         CI0028
          05              DE00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00653).                  CI0028
       01                 DE10  REDEFINES      DE00.                    CI0028
            10            DE10-DU11.                                    CI0028
            11            DE10-XFONC  PICTURE  X(4).                    CI0028
            11            DE10-MPSBN  PICTURE  X(8).                    CI0028
            11            DE10-XDBDNM PICTURE  X(08).                   CI0028
            11            DE10-XSEGNM PICTURE  X(08).                   CI0028
            11            DE10-XRC    PICTURE  X(02).                   CI0028
            11            DE10-MSEG   PICTURE  X(08).                   CI0028
            11            DE10-XCOKEY PICTURE  X(70).                   CI0028
            11            DE10-CUIBR  PICTURE  X(01).                   CI0028
            11            DE10-CUIBA  PICTURE  X(01).                   CI0028
            11            DE10-IPBIK  PICTURE  X(1).                    CI0028
            10            DE10-DU03.                                    CI0028
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0028
                          COMPUTATIONAL-3.                              CI0028
            11            DE10-CMSSF  PICTURE  XX.                      CI0028
            11            DE10-DU09.                                    CI0028
            12            DE10-CMESA  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            12            DE10-CMESB  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            12            DE10-CMSST  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            12            DE10-QELLAA PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            12            DE10-TMESS4 PICTURE  X(512).                  CI0028
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
       01                 MS00.                                         CI0028
          05              MS00-SUITE.                                   CI0028
            15       FILLER         PICTURE  X(00542).                  CI0028
       01                 MS03  REDEFINES      MS00.                    CI0028
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0028
                          COMPUTATIONAL-3.                              CI0028
            10            MS03-CMSSF  PICTURE  XX.                      CI0028
            10            MS03-DU09.                                    CI0028
            11            MS03-CMESA  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            11            MS03-CMESB  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            11            MS03-CMSST  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            11            MS03-QELLAA PICTURE  S9(9)                    CI0028
                          BINARY.                                       CI0028
            11            MS03-TMESS4 PICTURE  X(512).                  CI0028
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
      *                                                                 ADU002
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                TM35
                                DE10
                                MS03.                                   ADU002
      *N01.      NOTE *************************************.            CI0028
      *               *                                   *             CI0028
      *               *INITIALISATIONS                    *             CI0028
      *               *                                   *             CI0028
      *               *************************************.            CI0028
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
      *N02XA.    NOTE *SET ADDRESSES FOR DATABASES        *.
       F02XA.                                                           lv10
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR GR1P                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-GR1P-PTR1.                                          ADU015
       F02XA-FN. EXIT.
      *N02YA.    NOTE *INIT WORK AREAS                    *.
       F02YA.                                                           lv10
      *INIT DB SEGMENTS
           INITIALIZE  CT01
           CL01
           CL12.
       F02YA-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0028
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0028
      *               *                                   *             CI0028
      *               *FIN DE TRAITEMENT                  *             CI0028
      *               *                                   *             CI0028
      *               *************************************.            CI0028
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0028
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
      *N30.      NOTE *************************************.
      *               *                                   *
      *               *CHECK IF ACCT IS ONE OF BUFFERS    *
      *               *                                   *
      *               *************************************.
       F30.                                                             lv05
      *
      *********************************
      ** FOR EFFICIENCY, THE LAST TWO *
      ** ACCOUNT NUMBERS THAT WERE    *
      ** ACCESSED WILL BE STORED IN   *
      ** SAVE AREAS.                  *
      *********************************
      *
      *N30BA.    NOTE *CHECK FIRST SAVE AREA              *.
       F30BA.    IF    S135-CTID = TM35-CTID                            lv10
                 NEXT SENTENCE ELSE GO TO     F30BA-FN.
      *
      *********************************
      ** CHECK IF THE ACCOUNT NUMBERS *
      ** MATCH BETWEEN WHAT THE       *
      ** CALLING MODULE WANTS AND     *
      ** WHAT IS IN THE FIRST SAVE    *
      ** AREA.                        *
      *********************************
      *
      *N30CA.    NOTE *ACCOUNT NUMBERS MATCH              *.
       F30CA.                                                           lv15
      *
      *********************************
      ** THE ACCOUNT NUMBER ARE THE   *
      ** SAME, PASS THE FIRST SAVED   *
      ** AREA BACK TO THE CALLING     *
      ** MODULE.                      *
      *********************************
      *
           MOVE        S135 TO TM35.
       F30CA-FN. EXIT.
      *N30DA.    NOTE *RETURN TO CALLING MODULE           *.
       F30DA.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F30DA-FN. EXIT.
       F30BA-FN. EXIT.
      *N30EA.    NOTE *CHECK SECOND SAVE AREA             *.
       F30EA.    IF    S235-CTID = TM35-CTID                            lv10
                 NEXT SENTENCE ELSE GO TO     F30EA-FN.
      *
      *********************************
      ** CHECK IF THE ACCOUNT NUMBERS *
      ** MATCH BETWEEN WHAT THE       *
      ** CALLING MODULE WANTS AND     *
      ** WHAT IS IN THE SECOND SAVE   *
      ** AREA.                        *
      *********************************
      *
      *N30FA.    NOTE *ACCOUNT NUMBERS MATCH              *.
       F30FA.                                                           lv15
      *
      *********************************
      ** THE ACCOUNT NUMBER ARE THE   *
      ** SAME, PASS THE SECOND SAVED  *
      ** AREA BACK TO THE CALLING     *
      ** MODULE.                      *
      *********************************
      *
           MOVE        S235 TO TM35.
       F30FA-FN. EXIT.
      *N30GA.    NOTE *RETURN TO CALLING MODULE           *.
       F30GA.                                                           lv15
           MOVE                     ALL '1' TO FT GO TO F20.
       F30GA-FN. EXIT.
       F30EA-FN. EXIT.
       F30-FN.   EXIT.
      *N40.      NOTE *************************************.
      *               *                                   *
      *               *GET DATA FOR TAX MARKET DETERMIN   *
      *               *                                   *
      *               *************************************.
       F40.                                                             lv05
      *
      *********************************
      ** READ CONTRACT, CLIENT AND    *
      ** GROUP DATABASES TO GET THE   *
      ** DATA NEEDED TO DETERMINE THE *
      ** TAX MARKET FOR AN ACCOUNT.   *
      *********************************
      *
      *N40BA.    NOTE *SET UP SSA FOR CT01 READ           *.
       F40BA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CT01 READ     *
      ** USING ACCOUNT ID NUMBER      *
      ** PASSED FROM CALLING MODULE   *
      *********************************
      *
           MOVE        TM35-CTID TO S-CTU01-CTID.
       F40BA-FN. EXIT.
      *N40CA.    NOTE *READ CT01 SEGMENT                  *.
       F40CA.                                                           lv10
           PERFORM     F94CT THRU F94CT-FN.
      *N40DA.    NOTE *CT01 NOT FOUND                     *.
       F40DA.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40DA-FN.
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
      *N40DZ.    NOTE *RETURN TO CALLING MODULE           *.
       F40DZ.                                                           lv20
           MOVE                     ALL '1' TO FT GO TO F20.
       F40DZ-FN. EXIT.
       F40DA-FN. EXIT.
       F40CA-FN. EXIT.
      *N40EA.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F40EA.                                                           lv10
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
           MOVE        TM35-CTID TO AC14-CTID                           AM0018
           MOVE        TM35-DCACG TO AC14-DCACG                         AM0018
           MOVE        25 TO AC14-XIMAX                                 AM0018
           MOVE        'N' TO AC14-IPOCH                                AM0018
           SET CI0018A-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018A-PCB-ADDRESS-LIST                                     AM0018
           AC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
      *N40EB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40EB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > ZERO)                               ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40EB-FN.                 ADU071
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
       F40EB-900. GO TO F40EC-FN.
       F40EB-FN. EXIT.
      *N40EC.    NOTE *NO ERRORS                          *.            ADU071
       F40EC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F40EC-FN. EXIT.
       F40EA-FN. EXIT.
      *N40GA.    NOTE *CALL CI0019 - ACCOUNT GROUPS       *.            AM0019
       F40GA.                                                           lv10
      *                                                                 AM0019
      *********************************                                 AM0019
      ** THIS MODULE WILL READ THE    *                                 AM0019
      ** CONTRACT DATABASE TO GET ALL *                                 AM0019
      ** THE GROUPS FOR THE ACCOUNT   *                                 AM0019
      ** NUMBER.                      *                                 AM0019
      *********************************                                 AM0019
      *                                                                 AM0019
           INITIALIZE      AG15                                         AM0019
           MOVE        TM35-CTID TO AG15-CTID                           AM0019
           MOVE        TM35-DCACG TO AG15-DCACG                         AM0019
           MOVE        10 TO AG15-XIMAX                                 AM0019
           MOVE        'N' TO AG15-IPOCH                                AM0019
           SET CI0019B-PCB-CT1P-PTR1 TO                                 AM0019
                       PCB-CT1P-PTR1                                    AM0019
           SET CI0019B-PCB-GR1P-PTR1 TO                                 AM0019
                       PCB-GR1P-PTR1                                    AM0019
           INITIALIZE      DE10-DU03                                    AM0019
           CALL        CI0019 USING                                     AM0019
           DFHEIBLK                                                     AM0019
           DFHCOMMAREA                                                  AM0019
           DLIUIBII                                                     AM0019
           CI0019B-PCB-ADDRESS-LIST                                     AM0019
           AG15                                                         AM0019
           DE10                                                         AM0019
           MS03.                                                        AM0019
      *N40GB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40GB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > ZERO)                               ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40GB-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0019 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0019 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40GB-900. GO TO F40GC-FN.
       F40GB-FN. EXIT.
      *N40GC.    NOTE *NO ERRORS                          *.            ADU071
       F40GC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F40GC-FN. EXIT.
       F40GA-FN. EXIT.
      *N40IA.    NOTE *CALL CI0027 - ACCT CT22 SEGMENTS   *.            AM0027
       F40IA.                                                           lv10
      *                                                                 AM0027
      *********************************                                 AM0027
      ** THIS MODULE WILL READ THE    *                                 AM0027
      ** CONTRACT DATABASE TO GET THE *                                 AM0027
      ** CT22 SEGMENTS FOR THE ACCOUNT*                                 AM0027
      ** NUMBER.                      *                                 AM0027
      *********************************                                 AM0027
      *                                                                 AM0027
           INITIALIZE      AW34                                         AM0027
           MOVE        TM35-CTID TO AW34-CTID                           AM0027
           MOVE        TM35-DCACG TO AW34-DCACG                         AM0027
           MOVE        10 TO AW34-XIMAX                                 AM0027
           MOVE        'N' TO AW34-IPOCH                                AM0027
           SET CI0027C-PCB-CT1P-PTR1 TO                                 AM0027
                       PCB-CT1P-PTR1                                    AM0027
           INITIALIZE      DE10-DU03                                    AM0027
           CALL        CI0027 USING                                     AM0027
           DFHEIBLK                                                     AM0027
           DFHCOMMAREA                                                  AM0027
           DLIUIBII                                                     AM0027
           CI0027C-PCB-ADDRESS-LIST                                     AM0027
           AW34                                                         AM0027
           DE10                                                         AM0027
           MS03.                                                        AM0027
      *N40IB.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F40IB.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > ZERO)                               ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F40IB-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0027 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0027 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F40IB-900. GO TO F40IC-FN.
       F40IB-FN. EXIT.
      *N40IC.    NOTE *NO ERRORS                          *.            ADU071
       F40IC.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F40IC-FN. EXIT.
       F40IA-FN. EXIT.
      *N40KA.    NOTE *SET UP SSA FOR CL01 READ           *.
       F40KA.                                                           lv10
      *
      *********************************
      ** SET UP SSA FOR CL01 READ     *
      ** USING TAXPAYER CLIENT ID     *
      ** GOTTEN FROM MODULE CI0018    *
      *********************************
      *
           MOVE        AC14-CLID01 TO S-CLU01-CLID.
       F40KA-FN. EXIT.
      *N40LA.    NOTE *READ CL01 SEGMENT                  *.
       F40LA.                                                           lv10
           PERFORM     F94CL THRU F94CL-FN.
      *N40MA.    NOTE *CL01 NOT FOUND                     *.
       F40MA.    IF    IK = '1'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F40MA-FN.
      *---> Send INFORMATIONAL Message                                  ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012012 TO MS03-NMESS2                            ADU119
           PERFORM     F98IC THRU F98IC-FN.                             ADU119
       F40MA-FN. EXIT.
       F40LA-FN. EXIT.
      *N40NA.    NOTE *CLIENT IS AN ORGANIZATION          *.
       F40NA.    IF    CL01-CLTYP = 'O'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F40NA-FN.
      *N40OA.    NOTE *READ CL12 SEGMENT                  *.
       F40OA.                                                           lv15
           PERFORM     F94CO THRU F94CO-FN.
      *N40PA.    NOTE *CL12 NOT FOUND                     *.
       F40PA.    IF    IK = '1'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F40PA-FN.
      *SET FORM OF BUS. OWN. TO ZERO
           MOVE        ZEROS TO CL12-CFOBO.
       F40PA-FN. EXIT.
       F40OA-FN. EXIT.
       F40NA-FN. EXIT.
       F40-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *DETERMINE TAX MARKET FOR ACCOUNT   *
      *               *                                   *
      *               *************************************.
       F50.           EXIT.                                             lv05
      *N50BA.    NOTE *MOVE GROUP SEGMENTS                *.
       F50BA.                                                           lv10
           MOVE        AG15-GR01 (1) TO GR01
           MOVE        AG15-GR07 (1) TO GR07.
       F50BA-FN. EXIT.
      *N50BG.    NOTE *MOVE FIELDS NEEDED FOR SEL FND     *.
       F50BG.                                                           lv10
                 IF    GR01-GRIDC = 001                                 DOT
      *CGRMF ONLY SET ON HH GROUPS
           MOVE        GR01-CGRMF TO TM35-CGRMF
                 ELSE
           MOVE        'N' TO TM35-CGRMF.
                 IF    GR01-GRIDC = 002                                 DOT
      *GRPLC & GRPLT ONLY ON PENS GRPS
           MOVE        GR07-GRPLC TO TM35-GRPLC
           MOVE        GR07-GRPLT TO TM35-GRPLT
                 ELSE
           MOVE        ZEROS TO TM35-GRPLC
           TM35-GRPLT.
       F50BG-FN. EXIT.
      *N50CA.    NOTE *MOVE CT22 LENGTH INDEX             *.
       F50CA.                                                           lv10
           MOVE        AW34-QITEM TO IAW34L.
      *N50MA.    NOTE *DETERMINE TAX MARKET               *.            AABTMD
       F50MA.                                                           lv20
           MOVE        ZEROS TO TM35-CTXMT.                             AABTMD
      *N50MD.    NOTE *SEARCH FOR FOREIGN INSTRUCTIONS    *.            AABTMD
       F50MD.                                                           lv22
           MOVE 1 TO     IAW34R.
       F50MD-100. IF     IAW34R NOT >    IAW34L
           AND           AW34-CT22K     (IAW34R)
           NOT =           'US01'
           ADD 1 TO      IAW34R    GO TO F50MD-100.
      *N50ME.    NOTE *FOREIGN FOUND                      *.
       F50ME.    IF    IAW34R NOT > IAW34L                              lv25
                 NEXT SENTENCE ELSE GO TO     F50ME-FN.
           MOVE        2 TO TM35-CTXMT.                                 AABTMD
       F50ME-900. GO TO F50MG-FN.
       F50ME-FN. EXIT.
      *N50MG.    NOTE *NON-FOREIGNS                       *.            AABTMD
       F50MG.         EXIT.                                             lv25
      *N50MJ.    NOTE *HOUSEHOLD GROUP                    *.            AABTMD
       F50MJ.    IF    GR01-GRIDC = 1                                   lv30
                 NEXT SENTENCE ELSE GO TO     F50MJ-FN.                 AABTMD
      *N50MM.    NOTE *CHECK CT01-CQACT                   *.            AABTMD
       F50MM.         EXIT.                                             lv35
      *N50MP.    NOTE *PRIVATE DEFERRED COMPENSATION      *.            AABTMD
       F50MP.    IF    CT01-CQACT =                                     lv40
                       5                                                AABTMD
                 NEXT SENTENCE ELSE GO TO     F50MP-FN.                 AABTMD
           MOVE        3 TO TM35-CTXMT.                                 AABTMD
       F50MP-900. GO TO F50MM-FN.
       F50MP-FN. EXIT.
      *N50MS.    NOTE *TSA 403(B)                         *.            AABTMD
       F50MS.    IF    CT01-CQACT =                                     lv40
                       (2 OR 3)                                         AABTMD
                 NEXT SENTENCE ELSE GO TO     F50MS-FN.                 AABTMD
           MOVE        10 TO TM35-CTXMT.                                AABTMD
       F50MS-900. GO TO F50MM-FN.
       F50MS-FN. EXIT.
      *N50MV.    NOTE *TSCA 403(B)                        *.            AABTMD
       F50MV.    IF    CT01-CQACT =                                     lv40
                       4                                                AABTMD
                 NEXT SENTENCE ELSE GO TO     F50MV-FN.                 AABTMD
                 IF    CT01-CTCCI = '1'                                 DOT
           MOVE        9 TO TM35-CTXMT.                                 AABTMD
       F50MV-900. GO TO F50MM-FN.
       F50MV-FN. EXIT.
      *N50MY.    NOTE *IRAS                               *.            AABTMD
       F50MY.    IF    CT01-CQACT =                                     lv40
                       1                                                AABTMD
                 NEXT SENTENCE ELSE GO TO     F50MY-FN.                 AABTMD
      *N50NA.    NOTE *IDS CUSTODIAN                      *.            AABTMD
       F50NA.    IF    CT01-CTCCI = '1'                                 lv45
                 NEXT SENTENCE ELSE GO TO     F50NA-FN.                 AABTMD
           MOVE        7 TO TM35-CTXMT.                                 AABTMD
      *AN  CT01-CIRAS = (1 OR 2)                                        AABTMD
      *AN  CT01-CIRAT = (1 OR 2 OR 3                                    AABTMD
      *    OR 0)                                                        AABTMD
       F50NA-FN. EXIT.
      *N50ND.    NOTE *IDS NOT CUSTODIAN                  *.            AABTMD
       F50ND.    IF    CT01-CTCCI NOT = '1'                             lv45
                 NEXT SENTENCE ELSE GO TO     F50ND-FN.                 AABTMD
           MOVE        8 TO TM35-CTXMT.                                 AABTMD
      *AN  CT01-CIRAS = (1 OR 2)                                        AABTMD
      *AN  CT01-CIRAT = (1 OR 2 OR 3                                    AABTMD
      *    OR 0)                                                        AABTMD
       F50ND-FN. EXIT.
       F50MY-900. GO TO F50MM-FN.
       F50MY-FN. EXIT.
      *N50NE.    NOTE *END OF CASE STRUCTURE              *.            AABTMD
       F50NE.         EXIT.                                             lv40
      *N50NF.    NOTE *CORPORATE ACCOUNT                  *.            AABTMD
       F50NF.    IF    CL01-CLTYP = 'O'                                 lv45
                 AND   (CL12-CFOBO = 01                                 AABTMD
                 OR    CL12-CFOBO = 04                                  AABTMD
                 OR    CL12-CFOBO = 05                                  AABTMD
                 OR    CL12-CFOBO = 06                                  AABTMD
                 OR    CL12-CFOBO = 10)                                 AABTMD
                 NEXT SENTENCE ELSE GO TO     F50NF-FN.                 AABTMD
           MOVE        13 TO TM35-CTXMT.                                AABTMD
       F50NF-900. GO TO F50NG-FN.
       F50NF-FN. EXIT.
      *N50NG.    NOTE *PERSONAL ACCOUNT                   *.            AABTMD
       F50NG.                                                           lv45
           MOVE        1 TO TM35-CTXMT.                                 AABTMD
       F50NG-FN. EXIT.
       F50NE-FN. EXIT.
       F50MM-FN. EXIT.
       F50MJ-FN. EXIT.
      *N50NK.    NOTE *PENSION GROUP                      *.            AABTMD
       F50NK.    IF    GR01-GRIDC = 2                                   lv30
                 NEXT SENTENCE ELSE GO TO     F50NK-FN.                 AABTMD
      *N50NN.    NOTE *GOVERNMENT DEFERRED COMPENSATION   *.            AABTMD
       F50NN.    IF    GR07-GRPLT = 7                                   lv40
                 NEXT SENTENCE ELSE GO TO     F50NN-FN.                 AABTMD
           MOVE        4 TO TM35-CTXMT.                                 AABTMD
       F50NN-FN. EXIT.
      *N50NQ.    NOTE *CUSTODIAL PLANS                    *.            AABTMD
       F50NQ.    IF    GR07-GRPLC = 1                                   lv40
                 NEXT SENTENCE ELSE GO TO     F50NQ-FN.                 AABTMD
      *N50NT.    NOTE *IDS CUSTODIAL PLAN (KEOGH)         *.            AABTMD
       F50NT.    IF    GR07-CIDRP = 1                                   lv45
                 AND   GR07-GRPLT = (4 OR 5 OR 6)                       AABTMD
                 NEXT SENTENCE ELSE GO TO     F50NT-FN.                 AABTMD
           MOVE        5 TO TM35-CTXMT.                                 AABTMD
       F50NT-FN. EXIT.
      *N50NW.    NOTE *NON-IDS CUSTODIAL PLAN (KEOGH)     *.            AABTMD
       F50NW.    IF    GR07-CIDRP NOT = 1                               lv45
                 AND   GR07-GRPLT = (4 OR 5 OR 6                        AABTMD
                       OR 10)                                           AABTMD
                 NEXT SENTENCE ELSE GO TO     F50NW-FN.                 AABTMD
           MOVE        6 TO TM35-CTXMT.                                 AABTMD
       F50NW-FN. EXIT.
       F50NQ-FN. EXIT.
      *N50OA.    NOTE *TRUSTEED PLANS                     *.            AABTMD
       F50OA.    IF    GR07-GRPLC = 2                                   lv40
                 NEXT SENTENCE ELSE GO TO     F50OA-FN.                 AABTMD
      *N50OD.    NOTE *TRUSTEED 401K PLAN                 *.            AABTMD
       F50OD.    IF    GR07-GRPLT = 2 OR 11                             lv45
                 NEXT SENTENCE ELSE GO TO     F50OD-FN.                 AABTMD
           MOVE        11 TO TM35-CTXMT.                                AABTMD
       F50OD-FN. EXIT.
      *N50OG.    NOTE *TRUSTEED BENEFIT PLAN              *.            AABTMD
       F50OG.    IF    GR07-GRPLT = (1 OR 3 OR 4 OR                     lv45
                       5 OR 6 OR 10)                                    AABTMD
                 NEXT SENTENCE ELSE GO TO     F50OG-FN.                 AABTMD
           MOVE        12 TO TM35-CTXMT.                                AABTMD
       F50OG-FN. EXIT.
       F50OA-FN. EXIT.
       F50NK-FN. EXIT.
       F50MG-FN. EXIT.
       F50MD-FN. EXIT.
       F50MA-FN. EXIT.
       F50CA-FN. EXIT.
       F50-FN.   EXIT.
      *N55.      NOTE *************************************.
      *               *                                   *
      *               *DERIVE CUSTODIAL CODE              *
      *               *                                   *
      *               *************************************.
       F55.                                                             lv05
      *********************************
      ** RULES ARE TAKEN FROM THE     *
      ** ONLINE PROGRAM UDKD70.       *
      *********************************
      *N55CA.    NOTE *INITIALIZE CUSTODIAL CODE          *.
       F55CA.                                                           lv10
           MOVE        ZERO TO TM35-CTCUS.
       F55CA-FN. EXIT.
      *N55DA.    NOTE *IF GROUP IS HOUSEHOLD              *.
       F55DA.    IF    GR01-GRIDC = 001                                 lv10
                 NEXT SENTENCE ELSE GO TO     F55DA-FN.
      *N55EA.    NOTE *IF IDS IS CUSTODIAN                *.
       F55EA.    IF    CT01-CTCCI = 1                                   lv15
                 NEXT SENTENCE ELSE GO TO     F55EA-FN.
      *N55FA.    NOTE *IF QUAL ACCOUNT TYPE = IRA         *.
       F55FA.    IF    CT01-CQACT = 1                                   lv20
                 NEXT SENTENCE ELSE GO TO     F55FA-FN.
      *N55GA.    NOTE *IF IRA STATUS = ACTIVE             *.
       F55GA.    IF    CT01-CIRAS = 1                                   lv25
                 NEXT SENTENCE ELSE GO TO     F55GA-FN.
      *N55HA.    NOTE *IF IRA TYPE = SEP                  *.
       F55HA.    IF    CT01-CIRAT = 3                                   lv30
                 NEXT SENTENCE ELSE GO TO     F55HA-FN.
           MOVE        8 TO TM35-CTCUS.
       F55HA-FN. EXIT.
      *N55IA.    NOTE *IF IRA TYPE = INDIVIDUAL           *.
       F55IA.    IF    CT01-CIRAT = 1                                   lv30
                 NEXT SENTENCE ELSE GO TO     F55IA-FN.
           MOVE        6 TO TM35-CTCUS.
       F55IA-FN. EXIT.
      *N55IH.    NOTE *IF IRA TYPE = SRA                  *.
       F55IH.    IF    CT01-CIRAT = 4                                   lv30
                 NEXT SENTENCE ELSE GO TO     F55IH-FN.
           MOVE        10 TO TM35-CTCUS.
       F55IH-FN. EXIT.
      *N55IM.    NOTE *IF IRA TYPE = ROTH IRA CONTRIB     *.
       F55IM.    IF    CT01-CIRAT = 5                                   lv30
                 NEXT SENTENCE ELSE GO TO     F55IM-FN.
           MOVE        11 TO TM35-CTCUS.
       F55IM-FN. EXIT.
      *N55IT.    NOTE *IF IRA TYPE = ROTH IRA CONV 1998   *.
       F55IT.    IF    CT01-CIRAT = 6                                   lv30
                 NEXT SENTENCE ELSE GO TO     F55IT-FN.
           MOVE        13 TO TM35-CTCUS.
       F55IT-FN. EXIT.
      *N55IV.    NOTE *IF EDUCATIONAL IRA                 *.
       F55IV.    IF    CT01-CIRAT = 7                                   lv30
                 NEXT SENTENCE ELSE GO TO     F55IV-FN.
           MOVE        15 TO TM35-CTCUS.
       F55IV-FN. EXIT.
       F55GA-FN. EXIT.
      *N55JA.    NOTE *IF IRA STATUS = ROLLOVER           *.
       F55JA.    IF    CT01-CIRAS = 2                                   lv25
                 NEXT SENTENCE ELSE GO TO     F55JA-FN.
           MOVE        2 TO TM35-CTCUS.
       F55JA-FN. EXIT.
      *N55KA.    NOTE *IF IRA STATUS = BENEFICIAL         *.
       F55KA.    IF    CT01-CIRAS = 3                                   lv25
                 NEXT SENTENCE ELSE GO TO     F55KA-FN.
           MOVE        9 TO TM35-CTCUS.
      *N55LA.    NOTE *IF ROTH IRA CONTRIBUTORY BENE      *.
       F55LA.    IF    CT01-CIRAT = 005                                 lv30
                 NEXT SENTENCE ELSE GO TO     F55LA-FN.
           MOVE        12 TO TM35-CTCUS.
       F55LA-FN. EXIT.
      *N55LM.    NOTE *IF ROTH IRA CONV BENEFICIAL 1998   *.
       F55LM.    IF    CT01-CIRAT = 006                                 lv30
                 NEXT SENTENCE ELSE GO TO     F55LM-FN.
           MOVE        14 TO TM35-CTCUS.
       F55LM-FN. EXIT.
       F55KA-FN. EXIT.
       F55FA-FN. EXIT.
      *N55MA.    NOTE *IF QUAL ACCT TYPE = 403(B) TSCA    *.
       F55MA.    IF    CT01-CQACT = 4                                   lv20
                 NEXT SENTENCE ELSE GO TO     F55MA-FN.
           MOVE        3 TO TM35-CTCUS.
       F55MA-FN. EXIT.
       F55EA-FN. EXIT.
       F55DA-FN. EXIT.
      *N55QA.    NOTE *IF GROUP IS PENSION                *.
       F55QA.    IF    GR01-GRIDC = 002                                 lv10
                 NEXT SENTENCE ELSE GO TO     F55QA-FN.
           MOVE        GR01-GRIDND TO 7-GR01-GRIDND.
      *N55RA.    NOTE *IF IDS IS CUSTODIAN                *.
       F55RA.    IF    GR07-CIDRP = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F55RA-FN.
      *N55SA.    NOTE *BASED ON PLAN TYPE                 *.
       F55SA.         EXIT.                                             lv20
      *N55TA.    NOTE *IF PLAN TYPE = PROFIT SHARING      *.
       F55TA.    IF    GR07-GRPLT =                                     lv25
                       05
                 NEXT SENTENCE ELSE GO TO     F55TA-FN.
      *N55TD.    NOTE *IF MINNESOTA MEDICAL PLAN          *.
       F55TD.    IF    7-GR01-GRID-610 = 061                            lv30
                 NEXT SENTENCE ELSE GO TO     F55TD-FN.
           MOVE        1 TO TM35-CTCUS.
       F55TD-900. GO TO F55TL-FN.
       F55TD-FN. EXIT.
      *N55TL.    NOTE *ELSE... KEOGH PLAN 4               *.
       F55TL.                                                           lv30
           MOVE        4 TO TM35-CTCUS.
       F55TL-FN. EXIT.
       F55TA-900. GO TO F55SA-FN.
       F55TA-FN. EXIT.
      *N55UA.    NOTE *IF PLAN TYPE = ASD/TGT BEN         *.
       F55UA.    IF    GR07-GRPLT =                                     lv25
                       04
                 NEXT SENTENCE ELSE GO TO     F55UA-FN.
           MOVE        5 TO TM35-CTCUS.
       F55UA-900. GO TO F55SA-FN.
       F55UA-FN. EXIT.
      *N55VA.    NOTE *IF PLAN TYPE = MONEY PURCHASE      *.
       F55VA.    IF    GR07-GRPLT =                                     lv25
                       06
                 NEXT SENTENCE ELSE GO TO     F55VA-FN.
           MOVE        7 TO TM35-CTCUS.
       F55VA-900. GO TO F55SA-FN.
       F55VA-FN. EXIT.
       F55SA-FN. EXIT.
       F55RA-FN. EXIT.
       F55QA-FN. EXIT.
       F55-FN.   EXIT.
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
      ** AREAS, SO THE DATABASES      *
      ** WERE ACCESSED TO GET         *
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
           MOVE        S135 TO S235.
       F60BA-FN. EXIT.
      *N60CA.    NOTE *MOVE TM35 TO FIRST SAVE AREA       *.
       F60CA.                                                           lv10
      *
      *********************************
      ** MOVE TM35 TO THE FIRST SAVE  *
      ** AREA.                        *
      *********************************
      *
           MOVE        TM35 TO S135.
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
      *N94.      NOTE *************************************.
      *               *                                   *
      *               *DL/I CALLS                         *
      *               *                                   *
      *               *************************************.
       F94.           EXIT.                                             lv05
      *N94CL.    NOTE *CALL GU ON CL01                    *.            ADU026
       F94CL.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CL01                                                    ADU026
           S-CLU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CL-FN. EXIT.
      *N94CO.    NOTE *CALL GU ON CL12                    *.            ADU026
       F94CO.                                                           lv10
           MOVE        'CL1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CL12' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CL12                                                    ADU026
           S-CLU01-SSA S-CL12-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CO-FN. EXIT.
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
