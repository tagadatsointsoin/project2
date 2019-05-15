       IDENTIFICATION DIVISION.                                         CI0071
       PROGRAM-ID.  CI0071P.                                            CI0071
      *AUTHOR.         M\M - UPDATE WITHHOLDING.                        CI0071
      *DATE-COMPILED.   09/08/14.                                       CI0071
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1997                          *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.  ALL RIGHTS RESERVED.           *ACOPYP
      *     THE CATS   SYSTEM AND ALL INFORMATION RELATING THERETO,    *ACOPYP
      *     WHETHER IN THE FORM OF A COMPUTER PRINTOUT OR IN MACHINE   *ACOPYP
      *     READABLE FORM, AND ALL MATERIAL AND DOCUMENTATION RELATING *ACOPYP
      *     THERETO, IS AND CONTAINS CONFIDENTIAL INFORMATION AND      *ACOPYP
      *     TRADE SECRETS OF AMERIPRISE FINANCIAL, INC. OR ONE         *ACOPYP
      *     OF ITS SUBSIDIARIES.  THE CATS   SYSTEM AND ALL            *ACOPYP
      *     INFORMATION, MATERIAL AND DOCUMENTATION RELATING THERETO   *ACOPYP
      *     MAY BE USED OR DISCLOSED ONLY IN ACCORDANCE WITH           *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.'S POLICY ON PROPRIETARY         *ACOPYP
      *     INFORMATION AND TRADE SECRETS THAT APPEARS IN THE          *ACOPYP
      *     AMERIPRISE FINANCIAL CODE OF CONDUCT OR, AS SPECIFIED IN A *ACOPYP
      *     WRITTEN NON-DISCLOSURE AGREEMENT. NEITHER THE CATS         *ACOPYP
      *     SYSTEM NOR ANY MATERIAL OR DOCUMENTATION RELATING THERETO  *ACOPYP
      *     MAY BE REPRODUCED OR COPIED WITHOUT THE WRITTEN APPROVAL   *ACOPYP
      *     OF:                                                        *ACOPYP
      *     COPR. 1997                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
      ******************************************************************$2000
      ** YEAR 2000 COMPLIANT - YES                                      $2000
      ** (THIS IS NOT CERTIFICATION FOR YEAR 2000)                      $2000
      ******************************************************************$2000
       ENVIRONMENT DIVISION.                                            CI0071
       CONFIGURATION SECTION.                                           CI0071
       SOURCE-COMPUTER. IBM-370.                                        CI0071
       OBJECT-COMPUTER. IBM-370.                                        CI0071
       DATA DIVISION.                                                   CI0071
       WORKING-STORAGE SECTION.                                         CI0071
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
                        PICTURE S9(3)                                   CI0071
                          COMPUTATIONAL-3.                              CI0071
                                                                        ADU165
      *>>>>>>> Linkage Area for Logger Program DBI110                   ADU165
      *!WF DSP=DH DSL=DH SEL=10 FOR=I DES=2 LEV=1                       ADU165
       01                 DH10.                                         CI0071
            10            DH10-GERTC  PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-XUIBP  PICTURE  S9(8)                    CI0071
                          VALUE                ZERO                     CI0071
                          BINARY.                                       CI0071
            10            DH10-NSEQ2P PICTURE  S9(3)                    CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            DH10-CAUL   PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-MAUSB  PICTURE  X(8)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-NAUSK  PICTURE  X(50)                    CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-CSYS   PICTURE  X(4)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-CAPPL  PICTURE  X(8)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-CAUSR  PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-CAUFR  PICTURE  S9(5)                    CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            DH10-CAUAC  PICTURE  S9(5)                    CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            DH10-GEOPID PICTURE  X(6)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-CAUNIT PICTURE  X(4)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            DH10-GAUVR  PICTURE  X(400)                   CI0071
                          VALUE                SPACE.                   CI0071
      ******************************************************************
      ** ****     AUDIT LOG VARIABLE AREA SEGMENTS                   ***
      ******************************************************************
      *!WF DSP=AL DSL=DK SEL=25 FOR=I DES=2 LEV=1 PLT=AL
       01                 AL25.                                         CI0071
            10            AL25-CGVEN  PICTURE  X(2)                     CI0071
                          OCCURS       002     TIMES                    CI0071
                          VALUE                SPACE.                   CI0071
            10            AL25-CTWHC  PICTURE  9(2)                     CI0071
                          OCCURS       002     TIMES                    CI0071
                          VALUE                ZERO.                    CI0071
            10            AL25-CFCNTY PICTURE  X(2)                     CI0071
                          OCCURS       002     TIMES                    CI0071
                          VALUE                SPACE.                   CI0071
            10            AL25-DLAUP  PICTURE  9(8)                     CI0071
                          OCCURS       002     TIMES                    CI0071
                          VALUE                ZERO.                    CI0071
            10            AL25-CTWTC  PICTURE  9(2)                     CI0071
                          OCCURS       002     TIMES                    CI0071
                          VALUE                ZERO.                    CI0071
            10            AL25-CTWHAT PICTURE  S9(7)V99                 CI0071
                          OCCURS       002     TIMES                    CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            AL25-CTWHP  PICTURE  9(3)V99                  CI0071
                          OCCURS       002     TIMES                    CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
      *                                                                 AMDU34
      ******************************************************************AMDU34
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO GET THE    *AMDU34
      **     CT22 SEGMENTS FOR AN ACCOUNT NUMBER PASSED.               *AMDU34
      ******************************************************************AMDU34
      *                                                                 AMDU34
      *!WF DSP=AW DSL=DU SEL=34 FOR=I LEV=1                             AMDU34
       01                 AW00.                                         CI0071
          05              AW00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00541).                  CI0071
       01                 AW34  REDEFINES      AW00.                    CI0071
            10            AW34-C299.                                    CI0071
            11            AW34-CTID.                                    CI0071
            12            AW34-CTIDA  PICTURE  9(3).                    CI0071
            12            AW34-CTIDN.                                   CI0071
            13            AW34-CTIDNP PICTURE  X(13).                   CI0071
            13            AW34-CTIDND PICTURE  9(11).                   CI0071
            10            AW34-DCACG  PICTURE  9(8).                    CI0071
            10            AW34-IPOCH  PICTURE  X.                       CI0071
            10            AW34-FILLER PICTURE  X(100).                  CI0071
            10            AW34-CT22                                     CI0071
                          OCCURS       010     TIMES.                   CI0071
            11            AW34-CT22K.                                   CI0071
            12            AW34-CGVEN  PICTURE  X(2).                    CI0071
            12            AW34-CTWHC  PICTURE  9(2).                    CI0071
            11            AW34-CFCNTY PICTURE  X(2).                    CI0071
            11            AW34-DLAUP  PICTURE  9(8).                    CI0071
            11            AW34-CTWTC  PICTURE  9(2).                    CI0071
            11            AW34-CTWHAT PICTURE  S9(7)V99                 CI0071
                          COMPUTATIONAL-3.                              CI0071
            11            AW34-CTWHP  PICTURE  9(3)V99                  CI0071
                          COMPUTATIONAL-3.                              CI0071
            11            AW34-FILLER PICTURE  X(06).                   CI0071
            10            AW34-QITEM  PICTURE  9(3).                    CI0071
            10            AW34-XIMAX  PICTURE  S9(4)                    CI0071
                          BINARY.                                       CI0071
            10            AW34-CTXMT  PICTURE  9(2).                    CI0071
            10            AW34-CTCUS  PICTURE  999.                     CI0071
            10            AW34-FILLER PICTURE  X(95).                   CI0071
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
      *                                                                 AMDU34
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01                 CT00.                                         CI0071
            02            CT01.                                         CI0071
            10            CT01-CT01K.                                   CI0071
            11            CT01-C299.                                    CI0071
            12            CT01-CTID.                                    CI0071
            13            CT01-CTIDA  PICTURE  9(3).                    CI0071
            13            CT01-CTIDN.                                   CI0071
            14            CT01-CTIDNP PICTURE  X(13).                   CI0071
            14            CT01-CTIDND PICTURE  9(11).                   CI0071
            10            CT01-GECKD  PICTURE  9.                       CI0071
            10            CT01-GEMDA  PICTURE  9(8).                    CI0071
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0071
                          BINARY.                                       CI0071
            10            CT01-GECUC  PICTURE  99.                      CI0071
            10            CT01-CTAUL  PICTURE  9(3).                    CI0071
            10            CT01-DIRAC  PICTURE  9(4).                    CI0071
            10            CT01-CTCCI  PICTURE  X.                       CI0071
            10            CT01-CTCUS  PICTURE  999.                     CI0071
            10            CT01-CTEFD  PICTURE  9(8).                    CI0071
            10            CT01-CTIAD  PICTURE  9(8).                    CI0071
            10            CT01-CLCUS  PICTURE  99.                      CI0071
            10            CT01-CAMMB  PICTURE  X(3).                    CI0071
            10            CT01-CKPMM  PICTURE  X.                       CI0071
            10            CT01-CTLAD  PICTURE  9(8).                    CI0071
            10            CT01-IPERS  PICTURE  X.                       CI0071
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            CT01-CTLAT  PICTURE  9(8).                    CI0071
            10            CT01-CTLATC PICTURE  9(6).                    CI0071
            10            CT01-IMEGA  PICTURE  X.                       CI0071
            10            CT01-DIRAB  PICTURE  9(8).                    CI0071
            10            CT01-COLRQ  PICTURE  X.                       CI0071
            10            CT01-ZDA04  PICTURE  X(4).                    CI0071
            10            CT01-CTLPD  PICTURE  9(8).                    CI0071
            10            CT01-CIRASP PICTURE  9.                       CI0071
            10            CT01-CIRATP PICTURE  99.                      CI0071
            10            CT01-DRTHC  PICTURE  9(8).                    CI0071
            10            CT01-CPPTC  PICTURE  X.                       CI0071
            10            CT01-ZDA06  PICTURE  X(6).                    CI0071
            10            CT01-CTACD  PICTURE  9(8).                    CI0071
            10            CT01-CTNLI  PICTURE  X.                       CI0071
            10            CT01-CTRHO  PICTURE  9(8).                    CI0071
            10            CT01-CTSGD  PICTURE  9(8).                    CI0071
            10            CT01-CPATP  PICTURE  X(1).                    CI0071
            10            CT01-IRSTA  PICTURE  X.                       CI0071
            10            CT01-CTSTA  PICTURE  99.                      CI0071
            10            CT01-CTSSC  PICTURE  99.                      CI0071
            10            CT01-PRLIN  PICTURE  9(3).                    CI0071
            10            CT01-PRCOD  PICTURE  9(5).                    CI0071
            10            CT01-PRSCD  PICTURE  X(9).                    CI0071
            10            CT01-CTLNI  PICTURE  X.                       CI0071
            10            CT01-AYSIDA PICTURE  9(3).                    CI0071
            10            CT01-AYSID  PICTURE  9(5).                    CI0071
            10            CT01-CTBMC  PICTURE  99.                      CI0071
            10            CT01-CINAR  PICTURE  99.                      CI0071
            10            CT01-CPHTR  PICTURE  X.                       CI0071
            10            CT01-CDSTR  PICTURE  XX.                      CI0071
            10            CT01-CQACT  PICTURE  999.                     CI0071
            10            CT01-CIRAS  PICTURE  999.                     CI0071
            10            CT01-CIRAT  PICTURE  999.                     CI0071
            10            CT01-CLRAY  PICTURE  9(5).                    CI0071
            10            CT01-CATTP  PICTURE  X.                       CI0071
            02            CT22.                                         CI0071
            10            CT22-CT22K.                                   CI0071
            11            CT22-CGVEN  PICTURE  X(2).                    CI0071
            11            CT22-CTWHC  PICTURE  9(2).                    CI0071
            10            CT22-CFCNTY PICTURE  X(2).                    CI0071
            10            CT22-DLAUP  PICTURE  9(8).                    CI0071
            10            CT22-CTWTC  PICTURE  9(2).                    CI0071
            10            CT22-CTWHAT PICTURE  S9(7)V99                 CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            CT22-CTWHP  PICTURE  9(3)V99                  CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            CT22-FILLER PICTURE  X(06).                   CI0071
      *                                                                 ADU155
      ******************************************************************ADU155
      ** WORK AREA NEEDED FOR MACRO ADU155                             *ADU155
      **        DATE COMMON AREA FOR EXECUTING CICS ASKTIME/FORMATTIME *ADU155
      ******************************************************************ADU155
      *                                                                 ADU155
      *!WI pl=DD100                                                     ADU155
       01  DD01-XMSTS                                                   ADU155
                        PICTURE S9(15)                                  CI0071
                          COMPUTATIONAL-3.                              CI0071
       01  DD01-F2CCYY             PIC S9(08) COMP.                     ADU155
      *!WI pl=DD200                                                     ADU155
       01  DD01-XDAT69                                                  ADU155
                        PICTURE 9(6).                                   CI0071
       01  DD01-UDATE.                                                  ADU155
           05  DD01-YEAR           PIC  9(04).                          ADU155
           05  DD01-MMDD           PIC  9(04).                          ADU155
      *!WI pl=DD280                                                     ADU155
       01  DD01-XDATCU REDEFINES DD01-UDATE                             ADU155
                        PICTURE X(8).                                   CI0071
      *                                                                 ADU155
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0071
            10            XW05-XW06.                                    CI0071
            11            XW05-XDBPCB.                                  CI0071
            12            XW05-XDBDNM PICTURE  X(08)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-XSEGLV PICTURE  X(02)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-XRC    PICTURE  X(02)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-XPROPT PICTURE  X(04)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-FILLER PICTURE  S9(5)                    CI0071
                          VALUE                ZERO                     CI0071
                          BINARY.                                       CI0071
            12            XW05-XSEGNM PICTURE  X(08)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0071
                          VALUE                ZERO                     CI0071
                          BINARY.                                       CI0071
            12            XW05-XSEGNB PICTURE  9(05)                    CI0071
                          VALUE                ZERO                     CI0071
                          BINARY.                                       CI0071
            12            XW05-XCOKEY PICTURE  X(70)                    CI0071
                          VALUE                SPACE.                   CI0071
            10            XW05-XW07.                                    CI0071
            11            XW05-XIOPCB.                                  CI0071
            12            XW05-XTERMI PICTURE  X(08)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-FILLER PICTURE  XX                       CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-XRC1   PICTURE  X(02)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-FILLER PICTURE  X(12)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            XW05-XMODNM PICTURE  X(8)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0071
                          VALUE                ZERO.                    CI0071
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0071
                          VALUE                ZERO.                    CI0071
            10            XW05-XGU    PICTURE  X(4)                     CI0071
                          VALUE                'GU  '.                  CI0071
            10            XW05-XGHU   PICTURE  X(4)                     CI0071
                          VALUE                'GHU '.                  CI0071
            10            XW05-XGN    PICTURE  X(4)                     CI0071
                          VALUE                'GN  '.                  CI0071
            10            XW05-XGHN   PICTURE  X(4)                     CI0071
                          VALUE                'GHN '.                  CI0071
            10            XW05-XGNP   PICTURE  X(4)                     CI0071
                          VALUE                'GNP '.                  CI0071
            10            XW05-XGHNP  PICTURE  X(4)                     CI0071
                          VALUE                'GHNP'.                  CI0071
            10            XW05-XREPL  PICTURE  XXXX                     CI0071
                          VALUE                'REPL'.                  CI0071
            10            XW05-XISRT  PICTURE  X(4)                     CI0071
                          VALUE                'ISRT'.                  CI0071
            10            XW05-XDLET  PICTURE  X(4)                     CI0071
                          VALUE                'DLET'.                  CI0071
            10            XW05-XOPEN  PICTURE  X(4)                     CI0071
                          VALUE                'OPEN'.                  CI0071
            10            XW05-XCLSE  PICTURE  X(4)                     CI0071
                          VALUE                'CLSE'.                  CI0071
            10            XW05-XCHKP  PICTURE  X(4)                     CI0071
                          VALUE                'CHKP'.                  CI0071
            10            XW05-XXRST  PICTURE  X(4)                     CI0071
                          VALUE                'XRST'.                  CI0071
            10            XW05-XTERM  PICTURE  X(4)                     CI0071
                          VALUE                'TERM'.                  CI0071
            10            XW05-XNFPAC PICTURE  X(13)                    CI0071
                          VALUE                SPACE.                   CI0071
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0071
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0071
       01                 GQ01.                                         CI0071
            10            GQ01-GELL   PICTURE  9(4)                     CI0071
                          BINARY.                                       CI0071
            10            GQ01-GMISC.                                   CI0071
            11            GQ01-GS00.                                    CI0071
            12            GQ01-GT01.                                    CI0071
            13            GQ01-GQ01K.                                   CI0071
            14            GQ01-CANUMB PICTURE  X(27).                   CI0071
            14            GQ01-CAMCTR PICTURE  9(5).                    CI0071
            14            GQ01-GESQ2  PICTURE  99.                      CI0071
            12            GQ01-GT02                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-C199.                                    CI0071
            14            GQ01-CLID.                                    CI0071
            15            GQ01-CLIDO  PICTURE  9(3).                    CI0071
            15            GQ01-CLIDN.                                   CI0071
            16            GQ01-CLIDNP PICTURE  X(12).                   CI0071
            16            GQ01-CLIDND PICTURE  9(8).                    CI0071
            12            GQ01-GT03                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-C299.                                    CI0071
            14            GQ01-CTID.                                    CI0071
            15            GQ01-CTIDA  PICTURE  9(3).                    CI0071
            15            GQ01-CTIDN.                                   CI0071
            16            GQ01-CTIDNP PICTURE  X(13).                   CI0071
            16            GQ01-CTIDND PICTURE  9(11).                   CI0071
            12            GQ01-GT04                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-NPBN   PICTURE  X(20).                   CI0071
            12            GQ01-GT05                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-GR98.                                    CI0071
            14            GQ01-GRID.                                    CI0071
            15            GQ01-GRIDC  PICTURE  9(3).                    CI0071
            15            GQ01-GRIDN.                                   CI0071
            16            GQ01-GRIDNP PICTURE  99.                      CI0071
            16            GQ01-GRIDND PICTURE  9(8).                    CI0071
            12            GQ01-GT06                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-NTR    PICTURE  9(8).                    CI0071
            12            GQ01-GT07                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-NTRAC  PICTURE  9(14).                   CI0071
            12            GQ01-GT08                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-NSRAB  PICTURE  9(7).                    CI0071
            13            GQ01-GECKD  PICTURE  9.                       CI0071
            13            GQ01-NBLCK  PICTURE  9(5).                    CI0071
            13            GQ01-CTRID  PICTURE  X(4).                    CI0071
            12            GQ01-GT19                                     CI0071
                          REDEFINES            GQ01-GT01.               CI0071
            13            GQ01-GEOPD2 PICTURE  X(8).                    CI0071
            12            GQ01-CACKD  PICTURE  9.                       CI0071
            12            GQ01-CENTT  PICTURE  X.                       CI0071
            12            GQ01-CADATE PICTURE  X(8).                    CI0071
            12            GQ01-GETIM  PICTURE  S9(7)                    CI0071
                          COMPUTATIONAL-3.                              CI0071
            12            GQ01-GEOPID PICTURE  X(6).                    CI0071
            12            GQ01-CAUNIT PICTURE  X(4).                    CI0071
            12            GQ01-XTERMI PICTURE  X(08).                   CI0071
            12            GQ01-CAPPL  PICTURE  X(8).                    CI0071
            12            GQ01-CSYS   PICTURE  X(4).                    CI0071
            12            GQ01-NTRSU  PICTURE  999.                     CI0071
            12            GQ01-FILLER PICTURE  X(20).                   CI0071
            11            GQ01-XMISL  PICTURE  X(599).                  CI0071
      ******************************************************************
      **           MISCELLANEOUS TRAN WORK AREAS                       *
      ******************************************************************
      ** MISC TRAN 00046 VARIABLE PORTION                              *
      ******************************************************************
      *!WF DSP=GS DSL=GS SEL=46 FOR=I DES=2 LEV=1                       ADU034
       01                 GS00.                                         CI0071
            10            GS00-GT01.                                    CI0071
            11            GS00-GQ01K.                                   CI0071
            12            GS00-CANUMB PICTURE  X(27)                    CI0071
                          VALUE                SPACE.                   CI0071
            12            GS00-CAMCTR PICTURE  9(5)                     CI0071
                          VALUE                ZERO.                    CI0071
            12            GS00-GESQ2  PICTURE  99                       CI0071
                          VALUE                ZERO.                    CI0071
            10            GS00-GT02                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-C199.                                    CI0071
            12            GS00-CLID.                                    CI0071
            13            GS00-CLIDO  PICTURE  9(3).                    CI0071
            13            GS00-CLIDN.                                   CI0071
            14            GS00-CLIDNP PICTURE  X(12).                   CI0071
            14            GS00-CLIDND PICTURE  9(8).                    CI0071
            10            GS00-GT03                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-C299.                                    CI0071
            12            GS00-CTID.                                    CI0071
            13            GS00-CTIDA  PICTURE  9(3).                    CI0071
            13            GS00-CTIDN.                                   CI0071
            14            GS00-CTIDNP PICTURE  X(13).                   CI0071
            14            GS00-CTIDND PICTURE  9(11).                   CI0071
            10            GS00-GT04                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-NPBN   PICTURE  X(20).                   CI0071
            10            GS00-GT05                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-GR98.                                    CI0071
            12            GS00-GRID.                                    CI0071
            13            GS00-GRIDC  PICTURE  9(3).                    CI0071
            13            GS00-GRIDN.                                   CI0071
            14            GS00-GRIDNP PICTURE  99.                      CI0071
            14            GS00-GRIDND PICTURE  9(8).                    CI0071
            10            GS00-GT06                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-NTR    PICTURE  9(8).                    CI0071
            10            GS00-GT07                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-NTRAC  PICTURE  9(14).                   CI0071
            10            GS00-GT08                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-NSRAB  PICTURE  9(7).                    CI0071
            11            GS00-GECKD  PICTURE  9.                       CI0071
            11            GS00-NBLCK  PICTURE  9(5).                    CI0071
            11            GS00-CTRID  PICTURE  X(4).                    CI0071
            10            GS00-GT19                                     CI0071
                          REDEFINES            GS00-GT01.               CI0071
            11            GS00-GEOPD2 PICTURE  X(8).                    CI0071
            10            GS00-CACKD  PICTURE  9                        CI0071
                          VALUE                ZERO.                    CI0071
            10            GS00-CENTT  PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            GS00-CADATE PICTURE  X(8)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            GS00-GETIM  PICTURE  S9(7)                    CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            GS00-GEOPID PICTURE  X(6)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            GS00-CAUNIT PICTURE  X(4)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            GS00-XTERMI PICTURE  X(08)                    CI0071
                          VALUE                SPACE.                   CI0071
            10            GS00-CAPPL  PICTURE  X(8)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            GS00-CSYS   PICTURE  X(4)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            GS00-NTRSU  PICTURE  999                      CI0071
                          VALUE                ZERO.                    CI0071
            10            GS00-FILLER PICTURE  X(20)                    CI0071
                          VALUE                SPACE.                   CI0071
       01                 GS46.                                         CI0071
            10            GS46-CGVEN  PICTURE  X(2)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            GS46-CTWHC  PICTURE  9(2)                     CI0071
                          VALUE                ZERO.                    CI0071
            10            GS46-CTWTC  PICTURE  9(2)                     CI0071
                          VALUE                ZERO.                    CI0071
            10            GS46-CFCNTY PICTURE  X(2)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            GS46-CTWHAT PICTURE  S9(7)V99                 CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            GS46-CTWHP  PICTURE  9(3)V99                  CI0071
                          VALUE                ZERO                     CI0071
                          COMPUTATIONAL-3.                              CI0071
      ******************************************************************
      ** WORK AREAS FOR MISC TRAN LOOP PROCESS                         *
      ******************************************************************
       01  WS-LOOP-FIELDS.
         05  WS-LOOP-END                   PIC X.
      *!WI
         05  WS-GQ01-GESQ2
                        PICTURE 99.                                     CI0071
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
       01                 NS00.                                         CI0071
          05              NS00-00.                                      CI0071
            10            NS00-NS00K.                                   CI0071
            11            NS00-PRCSTK PICTURE  XX.                      CI0071
          05              NS00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00078).                  CI0071
       01                 NS20  REDEFINES      NS00.                    CI0071
            10       FILLER         PICTURE  X(00002).                  CI0071
            10            NS20-DCACG  PICTURE  9(8).                    CI0071
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            NS20-CCDAT  PICTURE  X(8).                    CI0071
            10            NS20-DCALP  PICTURE  X(12).                   CI0071
            10            NS20-DNACG  PICTURE  9(8).                    CI0071
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            NS20-CNDAT  PICTURE  X(8).                    CI0071
            10            NS20-DNALP  PICTURE  X(12).                   CI0071
            10            NS20-DCACD  PICTURE  X(10).                   CI0071
            10            NS20-FILLER PICTURE  X(4).                    CI0071
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *!WI
       01               PZ01-MAPPN
                        PICTURE X(10).                                  CI0071
           88  CSOVRADV             VALUE 'CSOVRADV  '.
           88  CSOVRCLI             VALUE 'CSOVRCLI  '.
           88  OLS                  VALUE 'OLS       '.
           88  CATS-BA              VALUE 'BA        '.
           88  CATS-SD              VALUE 'SD        '.
           88  AUTOFILE             VALUE 'AUTOFILE  '.
           88  TITLETRANSFER        VALUE 'TITLETSF  '.
           88  FACTS-1099           VALUE 'FACTS1099 '.
           88  FACTS-ASAP           VALUE 'FACTSASAP '.
           88  EXPRESSLINK          VALUE 'EXPRSSLINK'.
           88  COLAPHASE3           VALUE 'COLAPHASE3'.
           88  ADVISORLINK-PACD     VALUE 'ADVLPACD  '.
           88  ADVISORLINK-FAS      VALUE 'ADVLFAS   '.
           88  CATS-UD              VALUE 'UD        '.
           88  CSO-WORKFLOW         VALUE 'CWKFL     '.
           88  ADVISORLINK-CDS      VALUE 'ACDS      '.
           88  STMTIV               VALUE 'TSTAT4    '.
           88  TRUST-DC             VALUE 'TDC       '.
           88  FORMS-AUTOMATION     VALUE 'TFAP      '.
           88  FAS91                VALUE 'IFAS91    '.
           88  MARS-PB              VALUE 'FMARPB    '.
      *
           88  VALID-APPLICATION   VALUES 'CSOVRADV  '
                        'UD        '      'CSOVRCLI  '
                        'CWKFL     '      'OLS       '
                        'ACDS      '      'BA        '
                        'TSTAT4    '      'SD        '
                        'TDC       '      'AUTOFILE  '
                        'TFAP      '      'TITLETSF  '
                        'IFAS91    '      'FACTS1099 '
                        'FMARPB    '      'FACTSASAP '
                                          'EXPRSSLINK'
                                          'COLAPHASE3'
                                          'ADVLPACD  '
                                          'ADVLFAS   '.
      *
      ******************************************************************
      **     CT22 SAVE SEGMENT FOR AUDIT LOG.                          *
      ******************************************************************
      *
      *!WF DSP=SV DSL=CT SEL=22 FOR=I LEV=1 PLT=SV
       01                 SV00.                                         CI0071
          05              SV00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00030).                  CI0071
       01                 SV22  REDEFINES      SV00.                    CI0071
            10            SV22-CT22K.                                   CI0071
            11            SV22-CGVEN  PICTURE  X(2).                    CI0071
            11            SV22-CTWHC  PICTURE  9(2).                    CI0071
            10            SV22-CFCNTY PICTURE  X(2).                    CI0071
            10            SV22-DLAUP  PICTURE  9(8).                    CI0071
            10            SV22-CTWTC  PICTURE  9(2).                    CI0071
            10            SV22-CTWHAT PICTURE  S9(7)V99                 CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            SV22-CTWHP  PICTURE  9(3)V99                  CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            SV22-FILLER PICTURE  X(06).                   CI0071
      *
      *
      *
      *
      ******************************************************************ADUTAB
      **              TABLE TA75 ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA75-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=75 FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA75.                                                CI0071
           04    G-TA75-PARAM.                                          CI0071
             10  G-TA75-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0071
                        VALUE      +060.                                CI0071
             10  G-TA75-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0071
                        VALUE      +001.                                CI0071
             10  G-TA75-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0071
                        VALUE      +005.                                CI0071
             10  G-TA75-NUAPP  PICTURE 99                               CI0071
                        VALUE       0.                                  CI0071
             10  G-TA75-NUTAB  PICTURE X(6)                             CI0071
                        VALUE 'CAMCTR'.                                 CI0071
             10  G-TA75-TABFO  PICTURE XX                 VALUE SPACE.  CI0071
             10  G-TA75-TABCR  PICTURE XX                 VALUE SPACE.  CI0071
             10  G-TA75-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0071
             10  G-TA75-NUSSC  PICTURE X  VALUE   ' '.                  CI0071
             10  G-TA75-NUSSY  PICTURE X                  VALUE SPACE.  CI0071
             10  G-TA75-TRANID PICTURE X(4)               VALUE SPACE.  CI0071
             10  G-TA75-FILSYS.                                         CI0071
             15  G-TA75-USERC  PICTURE X(6)               VALUE SPACE.  CI0071
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0071
           04             TA75.                                         CI0071
            10            TA75-CAMCTR PICTURE  9(5)                     CI0071
                          VALUE                ZERO.                    CI0071
            10            TA75-TTDES  PICTURE  X(36)                    CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-MSYSID PICTURE  X(8)                     CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-NDLEN  PICTURE  S9(4)                    CI0071
                          VALUE                ZERO.                    CI0071
            10            TA75-IMIND1 PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-IMIND2 PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-IMIND3 PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-IMIND5 PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-IMIND7 PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-IMIND8 PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
            10            TA75-IMINE  PICTURE  X                        CI0071
                          VALUE                SPACE.                   CI0071
      **                                                                ADUTAB
      *
      ******************************************************************
      **     WORKING STORAGE MISC FIELDS                               *
      ******************************************************************
      *
       01  WS-WORK-MISC.
      *!WI
           05  WS-CURR-DCACG
                        PICTURE 9(8).                                   CI0071
           05  WS-CT22-MODIFIED                     PIC X.
       01   DEBUT-WSS.                                                  CI0071
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0071
            05   IK     PICTURE X.                                      CI0071
       01  CONSTANTES-PAC.                                              CI0071
           05  FILLER  PICTURE X(87)   VALUE                            CI0071
                     '6015 CAT09/08/14CI0071ADMIN   14:34:35CI0071P AMERCI0071
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0071
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0071
           05  NUGNA   PICTURE X(5).                                    CI0071
           05  APPLI   PICTURE X(3).                                    CI0071
           05  DATGN   PICTURE X(8).                                    CI0071
           05  PROGR   PICTURE X(6).                                    CI0071
           05  CODUTI  PICTURE X(8).                                    CI0071
           05  TIMGN   PICTURE X(8).                                    CI0071
           05  PROGE   PICTURE X(8).                                    CI0071
           05  COBASE  PICTURE X(4).                                    CI0071
           05  DATGNC  PICTURE X(10).                                   CI0071
           05  RELEAS  PICTURE X(7).                                    CI0071
           05  DATGE   PICTURE X(10).                                   CI0071
           05  DATSQ   PICTURE X(10).                                   CI0071
       01  DATCE.                                                       CI0071
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0071
         05  DATOR.                                                     CI0071
           10  DATOA  PICTURE XX.                                       CI0071
           10  DATOM  PICTURE XX.                                       CI0071
           10  DATOJ  PICTURE XX.                                       CI0071
       01   VARIABLES-CONDITIONNELLES.                                  CI0071
            05                  FT      PICTURE X VALUE '0'.            CI0071
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0071
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0071
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0071
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0071
            05       5-GQ00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0071
       01               S-CT01-SSA.                                     CI0071
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0071
                                      VALUE 'CT01    '.                 CI0071
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0071
            10          S-CT01-CCOD   PICTURE X(5)                      CI0071
                                      VALUE '-----'.                    CI0071
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0071
       01            S-CTU01-SSA.                                       CI0071
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0071
                                      VALUE 'CT01    '.                 CI0071
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0071
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0071
                                      VALUE '-----'.                    CI0071
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0071
                                      VALUE '(CT01K'.                   CI0071
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0071
            10       S-CTU01-CT01K.                                     CI0071
            11       S-CTU01-C299.                                      CI0071
            12       S-CTU01-CTID.                                      CI0071
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0071
            13       S-CTU01-CTIDN.                                     CI0071
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0071
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0071
            10  FILLER   PICTURE X    VALUE ')'.                        CI0071
       01               S-CT22-SSA.                                     CI0071
            10         S1-CT22-SEGNAM PICTURE X(8)                      CI0071
                                      VALUE 'CT22    '.                 CI0071
            10         S1-CT22-CCOM   PICTURE X VALUE '*'.              CI0071
            10          S-CT22-CCOD   PICTURE X(5)                      CI0071
                                      VALUE '-----'.                    CI0071
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0071
       01            S-CTU22-SSA.                                       CI0071
            10      S1-CTU22-SEGNAM PICTURE X(8)                        CI0071
                                      VALUE 'CT22    '.                 CI0071
            10      S1-CTU22-CCOM   PICTURE X VALUE '*'.                CI0071
            10       S-CTU22-CCOD   PICTURE X(5)                        CI0071
                                      VALUE '-----'.                    CI0071
            10      S1-CTU22-FLDNAM PICTURE X(9)                        CI0071
                                      VALUE '(CT22K'.                   CI0071
            10       S-CTU22-OPER  PICTURE XX VALUE ' ='.               CI0071
            10       S-CTU22-CT22K.                                     CI0071
            11       S-CTU22-CGVEN    PICTURE  X(2).                    CI0071
            11       S-CTU22-CTWHC    PICTURE  9(2).                    CI0071
            10  FILLER   PICTURE X    VALUE ')'.                        CI0071
       01               S-GQ01-SSA.                                     CI0071
            10         S1-GQ01-SEGNAM PICTURE X(8)                      CI0071
                                      VALUE 'GQ01    '.                 CI0071
            10         S1-GQ01-CCOM   PICTURE X VALUE '*'.              CI0071
            10          S-GQ01-CCOD   PICTURE X(5)                      CI0071
                                      VALUE '-----'.                    CI0071
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0071
       01            S-GQU01-SSA.                                       CI0071
            13      S1-GQU01-SEGNAM PICTURE X(8)                        CI0071
                                      VALUE 'GQ01    '.                 CI0071
            13      S1-GQU01-CCOM   PICTURE X VALUE '*'.                CI0071
            13       S-GQU01-CCOD   PICTURE X(5)                        CI0071
                                      VALUE '-----'.                    CI0071
            13      S1-GQU01-FLDNAM PICTURE X(9)                        CI0071
                                      VALUE '(GQ01K'.                   CI0071
            13       S-GQU01-OPER  PICTURE XX VALUE ' ='.               CI0071
            13       S-GQU01-GQ01K.                                     CI0071
            14       S-GQU01-CANUMB   PICTURE  X(27).                   CI0071
            14       S-GQU01-CAMCTR   PICTURE  9(5).                    CI0071
            14       S-GQU01-GESQ2    PICTURE  99.                      CI0071
            13  FILLER   PICTURE X    VALUE ')'.                        CI0071
       01            S-GQ701-SSA.                                       CI0071
            14      S1-GQ701-SEGNAM PICTURE X(8)                        CI0071
                                      VALUE 'GQ01    '.                 CI0071
            14      S1-GQ701-CCOM   PICTURE X VALUE '*'.                CI0071
            14       S-GQ701-CCOD   PICTURE X(5)                        CI0071
                                      VALUE '-----'.                    CI0071
            14      S1-GQ701-FLDNAM PICTURE X(9)                        CI0071
                                      VALUE '(XCANUMB'.                 CI0071
            14       S-GQ701-OPER  PICTURE XX VALUE ' ='.               CI0071
            14       S-GQ701-CANUMB   PICTURE  X(27).                   CI0071
            14  FILLER   PICTURE X    VALUE ')'.                        CI0071
       01   ZONES-UTILISATEUR PICTURE X.                                CI0071
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
      ** PCB POINTER FOR DATP                                           ADU015
            05 PCB-DATP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR DATP                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0071
          05              PA00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00106).                  CI0071
       01                 PA06  REDEFINES      PA00.                    CI0071
            10            PA06-XDBPCB.                                  CI0071
            11            PA06-XDBDNM PICTURE  X(08).                   CI0071
            11            PA06-XSEGLV PICTURE  X(02).                   CI0071
            11            PA06-XRC    PICTURE  X(02).                   CI0071
            11            PA06-XPROPT PICTURE  X(04).                   CI0071
            11            PA06-FILLER PICTURE  S9(5)                    CI0071
                          BINARY.                                       CI0071
            11            PA06-XSEGNM PICTURE  X(08).                   CI0071
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0071
                          BINARY.                                       CI0071
            11            PA06-XSEGNB PICTURE  9(05)                    CI0071
                          BINARY.                                       CI0071
            11            PA06-XCOKEY PICTURE  X(70).                   CI0071
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0071
          05              PB00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00106).                  CI0071
       01                 PB06  REDEFINES      PB00.                    CI0071
            10            PB06-XDBPCB.                                  CI0071
            11            PB06-XDBDNM PICTURE  X(08).                   CI0071
            11            PB06-XSEGLV PICTURE  X(02).                   CI0071
            11            PB06-XRC    PICTURE  X(02).                   CI0071
            11            PB06-XPROPT PICTURE  X(04).                   CI0071
            11            PB06-FILLER PICTURE  S9(5)                    CI0071
                          BINARY.                                       CI0071
            11            PB06-XSEGNM PICTURE  X(08).                   CI0071
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0071
                          BINARY.                                       CI0071
            11            PB06-XSEGNB PICTURE  9(05)                    CI0071
                          BINARY.                                       CI0071
            11            PB06-XCOKEY PICTURE  X(70).                   CI0071
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0071
          05              PE00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00106).                  CI0071
       01                 PE06  REDEFINES      PE00.                    CI0071
            10            PE06-XDBPCB.                                  CI0071
            11            PE06-XDBDNM PICTURE  X(08).                   CI0071
            11            PE06-XSEGLV PICTURE  X(02).                   CI0071
            11            PE06-XRC    PICTURE  X(02).                   CI0071
            11            PE06-XPROPT PICTURE  X(04).                   CI0071
            11            PE06-FILLER PICTURE  S9(5)                    CI0071
                          BINARY.                                       CI0071
            11            PE06-XSEGNM PICTURE  X(08).                   CI0071
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0071
                          BINARY.                                       CI0071
            11            PE06-XSEGNB PICTURE  9(05)                    CI0071
                          BINARY.                                       CI0071
            11            PE06-XCOKEY PICTURE  X(70).                   CI0071
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED TO UPDATE THE *
      **     CLIENTS ARRANGEMENTS & ASSOC BANK FOR A SPECIFIED ARR TYPE*
      ******************************************************************
      *
      *!WF DSP=WZ DSL=DU SEL=7B FOR=I DES=1 LEV=1 PLT=15
       01                 WZ7B.                                         CI0071
            10            WZ7B-MAPPN  PICTURE  X(10).                   CI0071
            10            WZ7B-CTID   PICTURE  X(27).                   CI0071
            10            WZ7B-CT22.                                    CI0071
            11            WZ7B-CT22K.                                   CI0071
            12            WZ7B-CGVEN  PICTURE  X(2).                    CI0071
            12            WZ7B-CTWHC  PICTURE  9(2).                    CI0071
            11            WZ7B-CFCNTY PICTURE  X(2).                    CI0071
            11            WZ7B-DLAUP  PICTURE  9(8).                    CI0071
            11            WZ7B-CTWTC  PICTURE  9(2).                    CI0071
            11            WZ7B-CTWHAT PICTURE  S9(7)V99                 CI0071
                          COMPUTATIONAL-3.                              CI0071
            11            WZ7B-CTWHP  PICTURE  9(3)V99                  CI0071
                          COMPUTATIONAL-3.                              CI0071
            11            WZ7B-FILLER PICTURE  X(06).                   CI0071
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
       01                 DE00.                                         CI0071
          05              DE00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00653).                  CI0071
       01                 DE10  REDEFINES      DE00.                    CI0071
            10            DE10-DU11.                                    CI0071
            11            DE10-XFONC  PICTURE  X(4).                    CI0071
            11            DE10-MPSBN  PICTURE  X(8).                    CI0071
            11            DE10-XDBDNM PICTURE  X(08).                   CI0071
            11            DE10-XSEGNM PICTURE  X(08).                   CI0071
            11            DE10-XRC    PICTURE  X(02).                   CI0071
            11            DE10-MSEG   PICTURE  X(08).                   CI0071
            11            DE10-XCOKEY PICTURE  X(70).                   CI0071
            11            DE10-CUIBR  PICTURE  X(01).                   CI0071
            11            DE10-CUIBA  PICTURE  X(01).                   CI0071
            11            DE10-IPBIK  PICTURE  X(1).                    CI0071
            10            DE10-DU03.                                    CI0071
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0071
                          COMPUTATIONAL-3.                              CI0071
            11            DE10-CMSSF  PICTURE  XX.                      CI0071
            11            DE10-DU09.                                    CI0071
            12            DE10-CMESA  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            12            DE10-CMESB  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            12            DE10-CMSST  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            12            DE10-QELLAA PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            12            DE10-TMESS4 PICTURE  X(512).                  CI0071
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
       01                 MS00.                                         CI0071
          05              MS00-SUITE.                                   CI0071
            15       FILLER         PICTURE  X(00542).                  CI0071
       01                 MS03  REDEFINES      MS00.                    CI0071
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0071
                          COMPUTATIONAL-3.                              CI0071
            10            MS03-CMSSF  PICTURE  XX.                      CI0071
            10            MS03-DU09.                                    CI0071
            11            MS03-CMESA  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            11            MS03-CMESB  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            11            MS03-CMSST  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            11            MS03-QELLAA PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
            11            MS03-TMESS4 PICTURE  X(512).                  CI0071
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0071
            10            MX11-QMSGS  PICTURE  9(03).                   CI0071
            10            MX11-PJ09                                     CI0071
                          OCCURS       025     TIMES.                   CI0071
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0071
                          COMPUTATIONAL-3.                              CI0071
            11            MX11-CMESB  PICTURE  S9(9)                    CI0071
                          BINARY.                                       CI0071
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                WZ7B
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0071
      *               *                                   *             CI0071
      *               *INITIALISATIONS                    *             CI0071
      *               *                                   *             CI0071
      *               *************************************.            CI0071
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
      *N02SC.    NOTE *ALIGN WITH PCBS PASSED             *.
       F02SC.                                                           lv10
      *SET ADDRESS FOR DATP                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-DATP-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F02SC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0071
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0071
      *               *                                   *             CI0071
      *               *FIN DE TRAITEMENT                  *             CI0071
      *               *                                   *             CI0071
      *               *************************************.            CI0071
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0071
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
      *N35AB.    NOTE *MOVE FIELDS TO WORKING STORAGE     *.
       F35AB.                                                           lv10
           MOVE        WZ7B-MAPPN TO PZ01-MAPPN.
       F35AB-FN. EXIT.
      *N35BA.    NOTE *DOES PROGRAM RECOGNIZE SOURCE      *.
       F35BA.    IF    VALID-APPLICATION                                lv10
                 NEXT SENTENCE ELSE GO TO     F35BA-FN.
       F35BA-900. GO TO F35BB-FN.
       F35BA-FN. EXIT.
      *N35BB.    NOTE *ELSE ERROR                         *.
       F35BB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012734 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35BB-FN. EXIT.
      *N35CA.    NOTE *VERIFY CONTRACT ID NUMBER          *.
       F35CA.    IF    WZ7B-CTID NUMERIC                                lv10
                 AND   WZ7B-CTID > ZERO
                 NEXT SENTENCE ELSE GO TO     F35CA-FN.
       F35CA-900. GO TO F35CB-FN.
       F35CA-FN. EXIT.
      *N35CB.    NOTE *ELSE ERROR                         *.
       F35CB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012004 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35CB-FN. EXIT.
      *N35DA.    NOTE *VERIFY GOVERNING ENTITY CODE       *.
       F35DA.    IF    WZ7B-CGVEN = 'US'                                lv10
                 NEXT SENTENCE ELSE GO TO     F35DA-FN.
       F35DA-900. GO TO F35DB-FN.
       F35DA-FN. EXIT.
      *N35DB.    NOTE *ELSE ERROR                         *.
       F35DB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012870 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35DB-FN. EXIT.
      *N35EA.    NOTE *VERIFY W/H INSTRUCTION TYPE CODE   *.
       F35EA.    IF    WZ7B-CTWHC = 02                                  lv10
                 OR    WZ7B-CTWHC = 04
                 OR    WZ7B-CTWHC = 03
                 NEXT SENTENCE ELSE GO TO     F35EA-FN.
      *********************************
      ** ONLY ALLOW PERIODIC OR       *
      ** DIVIDEND OR NON-PERIODIC     *
      ** W/H CHANGES                  *
      *********************************
       F35EA-900. GO TO F35EB-FN.
       F35EA-FN. EXIT.
      *N35EB.    NOTE *ELSE ERROR                         *.
       F35EB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012870 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35EB-FN. EXIT.
      *N35FA.    NOTE *VERIFY FOREIGN COUNTRY CODE        *.
       F35FA.    IF    WZ7B-CFCNTY = SPACES                             lv10
                 NEXT SENTENCE ELSE GO TO     F35FA-FN.
       F35FA-900. GO TO F35FB-FN.
       F35FA-FN. EXIT.
      *N35FB.    NOTE *ELSE ERROR                         *.
       F35FB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012872 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35FB-FN. EXIT.
      *N35GA.    NOTE *VERIFY LAST UPDATE DATE            *.
       F35GA.    IF    WZ7B-DLAUP = ZEROES                              lv10
                 NEXT SENTENCE ELSE GO TO     F35GA-FN.
       F35GA-900. GO TO F35GB-FN.
       F35GA-FN. EXIT.
      *N35GB.    NOTE *ELSE ERROR                         *.
       F35GB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012632 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35GB-FN. EXIT.
      *N35HA.    NOTE *VERIFY W/H ELECTION CODE           *.
       F35HA.    IF    WZ7B-CTWTC = 01                                  lv10
                 OR    WZ7B-CTWTC = 02
                 OR    WZ7B-CTWTC = 03
                 NEXT SENTENCE ELSE GO TO     F35HA-FN.
      *********************************
      ** VALID ELECTION CODES FOR     *
      ** UPDATE ARE: 01 = W/H AMT     *
      **             02 = W/H PERCENT *
      **             03 = NO W/H      *
      *********************************
       F35HA-900. GO TO F35HB-FN.
       F35HA-FN. EXIT.
      *N35HB.    NOTE *ELSE ERROR                         *.
       F35HB.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012871 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35HB-FN. EXIT.
      *N35IB.    NOTE *CHECK IF WITHHOLDING AMOUNT        *.
       F35IB.    IF    WZ7B-CTWTC = 01                                  lv10
                 AND   WZ7B-CTWHAT < ZEROS
                 NEXT SENTENCE ELSE GO TO     F35IB-FN.
      *********************************
      ** IF WITHHOLDING AMOUNT AND    *
      ** AMOUNT IS LESS THAN ZERO,    *
      ** SEND AN ERROR MESSAGE TO THE *
      ** WORKSTATION AND RETURN.      *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012175 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35IB-FN. EXIT.
      *N35JB.    NOTE *CHECK IF WITHHOLDING PERCENT       *.
       F35JB.    IF    WZ7B-CTWTC = 02                                  lv10
                 AND   (WZ7B-CTWHP < ZEROS OR
                       WZ7B-CTWHP > 100.00)
                 NEXT SENTENCE ELSE GO TO     F35JB-FN.
      *********************************
      ** IF WITHHOLDING PERCENT AND   *
      ** PERCENT IS NOT BETWEEN       *
      ** ZERO AND 100 PERCENT,        *
      ** SEND AN ERROR MESSAGE TO THE *
      ** WORKSTATION AND RETURN.      *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012176 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35JB-FN. EXIT.
      *N35KB.    NOTE *CHECK IF NO W/H REQUESTED          *.
       F35KB.    IF    WZ7B-CTWTC = 03                                  lv10
                 AND   (WZ7B-CTWHAT NOT = ZEROS
                 OR    WZ7B-CTWHP NOT = ZEROS)
                 NEXT SENTENCE ELSE GO TO     F35KB-FN.
      *********************************
      ** IF WITHHOLDING AMOUNT AND    *
      ** WITHHOLDING PERCENT FIELDS   *
      ** ARE NOT ZEROES, SEND AN      *
      ** ERROR MESSAGE TO THE         *
      ** WORKSTATION AND RETURN.      *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012873 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F35KB-FN. EXIT.
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
      **  - GET CURRENT SYSTEM DATE   *
      **    AND CONVERT TO GREGORIAN  *
      **  - ACCESS ACF2 SECURITY      *
      **  - GET CAMS ACCOUNTING DATE  *
      **  - SET MODIFICATION SWITCH   *
      **  - LOOP THRU CT22 SEGMENTS   *
      *********************************
      *N40AB.    NOTE *GET CURRENT DATE IN GREGORIAN      *.
       F40AB.                                                           lv10
      *
      *********************************
      ** RETRIEVE THE CURRENT DATE    *
      ** VIA ADU155.                  *
      *********************************
      *
           PERFORM     F92DA THRU F92DA-FN
           MOVE        DD01-XDATCU TO WS-CURR-DCACG.
       F40AB-FN. EXIT.
      *N40AD.    NOTE *CALL ACF EXIT MODULE               *.            ADU031
       F40AD.                                                           lv10
           EXEC CICS   LINK PROGRAM (ACF-PROG)                          ADU031
                       COMMAREA (ACF-USER-AREA)                         ADU031
                       LENGTH (ACF-AREA-LEN)                 END-EXEC.  ADU031
       F40AD-FN. EXIT.
      *N40AF.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F40AF.                                                           lv10
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
       F40AF-FN. EXIT.
      *N40AK.    NOTE *IF ERROR WITH DATE; EXIT           *.
       F40AK.    IF    MS03-NMESS2 NOT = ZERO                           lv10
                 NEXT SENTENCE ELSE GO TO     F40AK-FN.
      *
      *********************************
      ** EXIT BACK TO CALLING PROGRAM *
      *********************************
      *
           MOVE                     ALL '1' TO FT GO TO F20.
       F40AK-FN. EXIT.
      *N40AN.    NOTE *INITIALIZE SWITCHES                *.
       F40AN.                                                           lv10
      *********************************
      ** INITIALIZATION OF SWITCHES   *
      *********************************
           MOVE        'N' TO WS-CT22-MODIFIED.
       F40AN-FN. EXIT.
       F40-FN.   EXIT.
      *N45.      NOTE *************************************.
      *               *                                   *
      *               *MAINLINE PROCESSING                *
      *               *                                   *
      *               *************************************.
       F45.                                                             lv05
      *
      *********************************
      ** MAINLINE PROCESSING ROUTINE  *
      *********************************
      *N45BA.    NOTE *BUILD SSA FOR CT22 SEGMENT         *.
       F45BA.                                                           lv10
      *
      *********************************
      ** BUILD SSA FOR CT22 GHU READ  *
      *********************************
      *
           MOVE        WZ7B-CTID TO S-CTU01-CTID
           MOVE        WZ7B-CT22K TO S-CTU22-CT22K.
       F45BA-FN. EXIT.
      *N45DA.    NOTE *READ CT22 SEGMENT                  *.
       F45DA.                                                           lv10
      *
      *********************************
      ** READ CT22 SEGMENT FOR UPDATE *
      *********************************
      *
           PERFORM     F94UB THRU F94UB-FN.
       F45DA-FN. EXIT.
      *N45EA.    NOTE *CHECK IF CT22 SEGMENT WAS FOUND    *.
       F45EA.    IF    IK = '0'                                         lv10
                 NEXT SENTENCE ELSE GO TO     F45EA-FN.
      *
      *********************************
      ** IF CT22 SEGMENT WAS FOUND    *
      ** BUILD THE NEW CT22 SEGMENT   *
      *********************************
      *
           MOVE        CT22 TO SV22
           MOVE        WZ7B-CT22 TO CT22
           MOVE        NS20-DCACG TO CT22-DLAUP.
      *N45ED.    NOTE *UPDATE CT22 IF CHANGES FOUND       *.
       F45ED.    IF    SV22-CTWTC NOT = CT22-CTWTC                      lv15
                 OR    SV22-CTWHAT NOT =
                       CT22-CTWHAT
                 OR    SV22-CTWHP NOT = CT22-CTWHP
                 NEXT SENTENCE ELSE GO TO     F45ED-FN.
      *********************************
      ** IF CT22 SEGMENT WAS CHANGED  *
      ** REPLACE THE CT22 SEGMENT.    *
      *********************************
      *
           PERFORM     F94RB THRU F94RB-FN.
      *N45EG.    NOTE *CT22 REPLACE WAS SUCCESSFUL        *.
       F45EG.    IF    IK = '0'                                         lv20
                 NEXT SENTENCE ELSE GO TO     F45EG-FN.
      *
      *********************************
      ** IF CT22 SEGMENT WAS REPLACED *
      ** SUCCESSFULLY, UPDATE THE     *
      ** AUDIT LOG.                   *
      *********************************
      *N45EJ.    NOTE *FORMAT THE AUDIT LOG RECORD        *.
       F45EJ.                                                           lv25
      *
      *********************************
      ** FORMAT THE AUDIT LOG RECORD  *
      ** FOR THE CT22 UPDATE.         *
      *********************************
           MOVE        +00225 TO DH10-CAUFR
           MOVE        +00040 TO DH10-CAUAC
           MOVE        SV22-CGVEN TO AL25-CGVEN (1)
           MOVE        CT22-CGVEN TO AL25-CGVEN (2)
           MOVE        SV22-CTWHC TO AL25-CTWHC (1)
           MOVE        CT22-CTWHC TO AL25-CTWHC (2)
           MOVE        SV22-CFCNTY TO AL25-CFCNTY (1)
           MOVE        CT22-CFCNTY TO AL25-CFCNTY (2)
           MOVE        SV22-DLAUP TO AL25-DLAUP (1)
           MOVE        CT22-DLAUP TO AL25-DLAUP (2)
           MOVE        SV22-CTWTC TO AL25-CTWTC (1)
           MOVE        CT22-CTWTC TO AL25-CTWTC (2)
           MOVE        SV22-CTWHAT TO AL25-CTWHAT (1)
           MOVE        CT22-CTWHAT TO AL25-CTWHAT (2)
           MOVE        SV22-CTWHP TO AL25-CTWHP (1)
           MOVE        CT22-CTWHP TO AL25-CTWHP (2)
           MOVE        AL25 TO DH10-GAUVR.
       F45EJ-FN. EXIT.
      *N45EL.    NOTE *INSERT AUDIT LOG - CT22 UPDATE     *.
       F45EL.                                                           lv25
      *
      *********************************
      ** INSERT AN AUDIT LOG RECORD   *
      ** FOR THE CT22 UPDATE.         *
      *********************************
      *
           PERFORM     F96AL THRU F96AL-FN.
       F45EL-FN. EXIT.
      *N45EM.    NOTE *SET CT22 MODIFY INDICATOR          *.
       F45EM.                                                           lv25
      *
      *********************************
      ** SET CT22 MODIFIED INDICATOR  *
      *********************************
      *
           MOVE        'Y' TO WS-CT22-MODIFIED.
       F45EM-FN. EXIT.
       F45EG-900. GO TO F45FA-FN.
       F45EG-FN. EXIT.
      *N45FA.    NOTE *CT22 REPLACE WAS NOT SUCCESSFUL    *.
       F45FA.                                                           lv20
      *
      *********************************
      ** CT22 REPLACE WAS NOT         *
      ** SUCCESSFUL, SEND AN ERROR    *
      ** MESSAGE AND TERMINATE.       *
      *********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012179 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45FA-FN. EXIT.
       F45ED-FN. EXIT.
       F45EA-900. GO TO F45MA-FN.
       F45EA-FN. EXIT.
      *N45MA.    NOTE *INSERT NEW CT22 SEGMENT            *.
       F45MA.                                                           lv10
      *
      *********************************
      ** IF THE CT22 SEGMENT WAS NOT  *
      ** FOUND, INSERT WITH THE NEW   *
      ** CT22 SEGMENT.                *
      *********************************
      *
           MOVE        WZ7B-CT22 TO CT22
           MOVE        NS20-DCACG TO CT22-DLAUP
           PERFORM     F94IB THRU F94IB-FN.
      *N45MG.    NOTE *CT22 INSERT WAS SUCCESSFUL         *.
       F45MG.    IF    IK = '0'                                         lv15
                 NEXT SENTENCE ELSE GO TO     F45MG-FN.
      *
      *********************************
      ** IF CT22 SEGMENT WAS INSERTED *
      ** SUCCESSFULLY, UPDATE THE     *
      ** AUDIT LOG.                   *
      *********************************
      *N45MJ.    NOTE *FORMAT THE AUDIT LOG RECORD        *.
       F45MJ.                                                           lv20
      *
      *********************************
      ** FORMAT THE AUDIT LOG RECORD  *
      ** FOR THE CT22 UPDATE.         *
      *********************************
           MOVE        +00225 TO DH10-CAUFR
           MOVE        +00040 TO DH10-CAUAC
           MOVE        SPACES TO AL25-CGVEN (1)
           MOVE        CT22-CGVEN TO AL25-CGVEN (2)
           MOVE        ZEROES TO AL25-CTWHC (1)
           MOVE        CT22-CTWHC TO AL25-CTWHC (2)
           MOVE        SPACES TO AL25-CFCNTY (1)
           MOVE        CT22-CFCNTY TO AL25-CFCNTY (2)
           MOVE        ZEROES TO AL25-DLAUP (1)
           MOVE        CT22-DLAUP TO AL25-DLAUP (2)
           MOVE        ZEROES TO AL25-CTWTC (1)
           MOVE        CT22-CTWTC TO AL25-CTWTC (2)
           MOVE        ZEROES TO AL25-CTWHAT (1)
           MOVE        CT22-CTWHAT TO AL25-CTWHAT (2)
           MOVE        ZEROES TO AL25-CTWHP (1)
           MOVE        CT22-CTWHP TO AL25-CTWHP (2)
           MOVE        AL25 TO DH10-GAUVR.
       F45MJ-FN. EXIT.
      *N45ML.    NOTE *INSERT AUDIT LOG - CT22 UPDATE     *.
       F45ML.                                                           lv20
      *
      *********************************
      ** INSERT AN AUDIT LOG RECORD   *
      ** FOR THE CT22 UPDATE.         *
      *********************************
      *
           PERFORM     F96AL THRU F96AL-FN.
       F45ML-FN. EXIT.
      *N45MM.    NOTE *SET CT22 MODIFY INDICATOR          *.
       F45MM.                                                           lv20
      *
      *********************************
      ** SET CT22 MODIFIED INDICATOR  *
      *********************************
      *
           MOVE        'Y' TO WS-CT22-MODIFIED.
       F45MM-FN. EXIT.
       F45MG-900. GO TO F45PA-FN.
       F45MG-FN. EXIT.
      *N45PA.    NOTE *CT22 INSERT WAS NOT SUCCESSFUL     *.
       F45PA.                                                           lv15
      *
      *********************************
      ** CT22 INSERT WAS NOT          *
      ** SUCCESSFUL, SEND AN ERROR    *
      ** MESSAGE AND TERMINATE.       *
      *********************************
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012180 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F45PA-FN. EXIT.
       F45MA-FN. EXIT.
       F45-FN.   EXIT.
      *N48.      NOTE *************************************.
      *               *                                   *
      *               *MODIFICATION PROCESSING            *
      *               *                                   *
      *               *************************************.
       F48.                                                             lv05
      *
      *********************************
      ** MODIFICATION PROCESSING      *
      *********************************
      *
      *N48BA.    NOTE *IF CT22 UPDATED, UPDATE CT01       *.
       F48BA.    IF    WS-CT22-MODIFIED = 'Y'                           lv10
                 NEXT SENTENCE ELSE GO TO     F48BA-FN.
      *
      *********************************
      ** IF CT22 WAS UPDATED, THEN    *
      ** UPDATE CT01 SEGMENT.         *
      *********************************
      *
      *N48DA.    NOTE *READ CT01 SEGMENT                  *.
       F48DA.                                                           lv15
      *
      *********************************
      ** READ CT01 SEGMENT FOR UPDATE *
      *********************************
      *
           PERFORM     F94UA THRU F94UA-FN.
       F48DA-FN. EXIT.
      *N48EA.    NOTE *CHECK IF CT01 SEGMENT NOT FOUND    *.
       F48EA.    IF    IK = '1'                                         lv15
                 OR    DE10-NMESS2 NOT = ZEROES
                 NEXT SENTENCE ELSE GO TO     F48EA-FN.
      *********************************
      ** IF CT01 SEGMENT NOT FOUND,   *
      ** SEND AN ERROR MESSAGE AND    *
      ** TERMINATE.                   *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012011 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F48EA-FN. EXIT.
      *N48FA.    NOTE *UPDATE TIMESTAMP ON CT01           *.
       F48FA.                                                           lv15
      *                                                                 DOT
      *********************************                                 AMSTMP
      ** UPDATE TIMESTAMP ON SEGMENT  *                                 AMSTMP
      ** CT01                                                           AMSTMP
      *********************************                                 AMSTMP
           ADD +1 TO CT01-NSEQ4B                                        AMSTMP
             SIZE ERROR                                                 AMSTMP
             MOVE +1 TO CT01-NSEQ4B.                                    AMSTMP
       F48FA-FN. EXIT.
      *N48GA.    NOTE *REPLACE CT01 SEGMENT               *.
       F48GA.                                                           lv15
      *
      *********************************
      ** REPLACE THE CT01 SEGMENT     *
      *********************************
      *
           PERFORM     F94RA THRU F94RA-FN.
       F48GA-FN. EXIT.
      *N48HA.    NOTE *CHECK IF REPLACE DID NOT WORK      *.
       F48HA.    IF    IK = '1'                                         lv15
                 OR    DE10-NMESS2 NOT = ZEROS
                 NEXT SENTENCE ELSE GO TO     F48HA-FN.
      *********************************
      ** IF THE CT01 SEGMENT WAS NOT  *
      ** REPLACED, SEND AN ERROR      *
      ** MESSAGE AND TERMINATE.       *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012177 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F48HA-FN. EXIT.
      *N48QA.    NOTE *MISCELLANEOUS TRAN                 *.
       F48QA.                                                           lv15
      *
      *********************************
      ** MISCELLANEOUS TRAN 00046     *
      ** ACCOUNT TAX WITHHOLDING      *
      *********************************
      *
       F48QA-FN. EXIT.
      *N48QC.    NOTE *PREP FOR GQ01 PROCESSING LOOP      *.
       F48QC.                                                           lv15
      *
      *********************************
      ** FORMAT GQ01 SSA              *
      ** SAVE GQ01 SEQUENCE NUMBER    *
      ** SET END OF LOOP SWITCH TO NO *
      *********************************
      *
           MOVE        WZ7B-CTID TO S-GQU01-CANUMB
           MOVE        00046 TO S-GQU01-CAMCTR
           MOVE        1 TO S-GQU01-GESQ2
           WS-GQ01-GESQ2
           MOVE        'N' TO WS-LOOP-END.
      *N48QG.    NOTE *GQ01 PROCESSING LOOP               *.
       F48QG.                       GO TO     F48QG-B.                  lv20
       F48QG-A.
                 IF    WS-LOOP-END = 'Y'
                                    GO TO     F48QG-FN.
       F48QG-B.
      *
      *********************************
      ** PROCESS GQ01'S FOR THIS ACCT *
      ** UNTIL NO MORE, OR UNTIL A    *
      ** GQ01 WITH MATCHING WITHHOLDING
      ** TYPE IS FOUND AND DELETED.   *
      *********************************
      *
      *GET GQ01 FOR UPDATE
      *
           PERFORM     F94QG THRU F94QG-FN.
      *N48QJ.    NOTE *NO (MORE) GQ01 FOUND               *.
       F48QJ.    IF    IK = '1'                                         lv25
                 NEXT SENTENCE ELSE GO TO     F48QJ-FN.
      *
      *********************************
      ** SAVE GQ01 SEQUENCE NUMBER    *
      ** AND END GQ01 LOOP.           *
      *********************************
      *
           MOVE        S-GQU01-GESQ2 TO WS-GQ01-GESQ2
           MOVE        'Y' TO WS-LOOP-END.
       F48QJ-900. GO TO F48QN-FN.
       F48QJ-FN. EXIT.
      *N48QN.    NOTE *GQ01 FOUND                         *.
       F48QN.                                                           lv25
      *
      *********************************
      ** SAVE GQ01 VARIABLE PORTION   *
      *********************************
      *
           MOVE        GQ01-XMISL TO GS46.
      *N48QR.    NOTE *MISC TRAN WITHHOLDING TYPE MATCH   *.
       F48QR.    IF    CT22-CTWHC = GS46-CTWHC                          lv30
                 NEXT SENTENCE ELSE GO TO     F48QR-FN.
      *
      *********************************
      ** SAVE GQ01 SEQUENCE NUMBER,   *
      ** SET GQ01 LOOP END SWITCH,    *
      ** AND DELETE EXISTING MISC TRAN*
      *********************************
      *
           MOVE        S-GQU01-GESQ2 TO WS-GQ01-GESQ2
           MOVE        'Y' TO WS-LOOP-END
           PERFORM     F94QD THRU F94QD-FN.
       F48QR-900. GO TO F48QU-FN.
       F48QR-FN. EXIT.
      *N48QU.    NOTE *DIFFERENT WITHHOLDING TYPE         *.
       F48QU.                                                           lv30
      *
      *********************************
      ** BUMP GQ01 SEQUENCE NUMBER,   *
      ** GO TO READ ANOTHER GQ01      *
      *********************************
      *
           ADD         1 TO S-GQU01-GESQ2.
       F48QU-FN. EXIT.
       F48QN-FN. EXIT.
       F48QG-900. GO TO F48QG-A.
       F48QG-FN. EXIT.
      *N48QW.    NOTE *FORMAT AND WRITE MISC TRAN         *.
       F48QW.         EXIT.                                             lv20
      *N48QY.    NOTE *FORMAT MISC TRAN COMMON PART       *.
       F48QY.                                                           lv25
      *
      *********************************
      ** TERMINATE IF MISC TRAN NOT   *
      ** FOUND ON PACBASE TABLE TA75. *
      *********************************
      *.
      *FORMAT "COMMON" MISC TRAN                                        DOT
           INITIALIZE GQ01                                              ADU033
           MOVE        00046 TO TA75-CAMCTR                             ADU033
           PERFORM     F92TA THRU F92TA-FN                              ADU033
           MOVE        TA75-NDLEN TO GQ01-GELL                          ADU033
           MOVE        WZ7B-CTID TO GQ01-CANUMB                         ADU033
           MOVE        00046 TO GQ01-CAMCTR                             ADU033
           MOVE        WS-GQ01-GESQ2 TO GQ01-GESQ2
           MOVE        CT01-GECKD TO GQ01-CACKD                         ADU033
           MOVE        'A' TO GQ01-CENTT                                ADU033
           MOVE        NS20-DCACG TO GQ01-CADATE                        ADU033
           MOVE        EIBTIME TO GQ01-GETIM                            ADU033
           MOVE        ACF-USER-ID TO GQ01-GEOPID                       ADU033
           MOVE        ACF-USER-UNIT TO GQ01-CAUNIT                     ADU033
           MOVE        EIBTRMID TO GQ01-XTERMI                          ADU033
           MOVE        PROGR TO GQ01-CAPPL                              ADU033
           MOVE        'COLA' TO GQ01-CSYS.                             ADU033
       F48QY-FN. EXIT.
      *N48RB.    NOTE *FORMAT MISC TRAN VARIABLE PART     *.
       F48RB.                                                           lv25
      *
      *********************************
      ** MISC TRAN VARIABLE = GS46    *
      *********************************
      *.
           INITIALIZE GS46                                              DOT
      *FORMAT GS46 FLDS (LINES 503-558)                                 ADU034
           MOVE        CT22-CGVEN TO GS46-CGVEN
           MOVE        CT22-CTWHC TO GS46-CTWHC
           MOVE        CT22-CTWTC TO GS46-CTWTC
           MOVE        CT22-CFCNTY TO GS46-CFCNTY
           MOVE        CT22-CTWHAT TO GS46-CTWHAT
           MOVE        CT22-CTWHP TO GS46-CTWHP
           MOVE        GS46 TO GQ01-XMISL.                              ADU034
       F48RB-FN. EXIT.
      *N48RF.    NOTE *WRITE MISC TRAN 00046              *.
       F48RF.                                                           lv25
      *
      *********************************
      ** INSERT GQ01                  *
      *********************************
      *
           PERFORM     F94QI THRU F94QI-FN.
      *N48RJ.    NOTE *GQ01 WRITE FAILED                  *.
       F48RJ.    IF    IK = '1'                                         lv30
                 NEXT SENTENCE ELSE GO TO     F48RJ-FN.
      *
      *********************************
      ** SEND MESSAGE THAT MISC TRAN  *
      ** FOR WITHHOLDING NOT CREATED, *
      ** THEN TERMINATE.              *
      *********************************
      *
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012208 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F48RJ-FN. EXIT.
       F48RF-FN. EXIT.
       F48QW-FN. EXIT.
       F48QC-FN. EXIT.
       F48BA-FN. EXIT.
       F48-FN.   EXIT.
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
      *N92DA.    NOTE *RETRIEVE CURRENT DATE              *.
       F92DA.                                                           lv10
           EXEC CICS   ASKTIME ABSTIME (DD01-XMSTS)          END-EXEC.  ADU155
           EXEC CICS   FORMATTIME ABSTIME (DD01-XMSTS)                  ADU155
                       YYMMDD (DD01-XDAT69)                             ADU155
                       YEAR (DD01-F2CCYY)                    END-EXEC.  ADU155
           COMPUTE     DD01-YEAR = DD01-F2CCYY                          ADU155
      ** MOVE DD01-UDATE TO YOUR FIELD                                  ADU155
           MOVE        DD01-XDAT69 (3:4) TO DD01-MMDD.                  ADU155
       F92DA-FN. EXIT.
      *N92EA.    NOTE *MISC TRAN NOT FOUND ON TA75        *.
       F92EA.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012207 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F92EA-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA75         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA75-TABFO                             ADUTAB
           COMPUTE     G-TA75-LTH = 60 + G-TA75-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA75-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA75)                                ADUTAB
                       LENGTH (G-TA75-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA75-TABCR NOT = '00'                          DOT
           PERFORM     F92EA THRU F92EA-FN.                             ADUTAB
       F92TA-FN. EXIT.
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
      *N94IB.    NOTE *CALL ISRT ON CT22                  *.            ADU026
       F94IB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT22' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PB06 CT22                                                    ADU026
           S-CTU01-SSA S-CT22-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94IB-FN. EXIT.
      *N94QD.    NOTE *CALL DLET ON GQ01                  *.            ADU026
       F94QD.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XDLET                       ADU026
           PE06 GQ01                                                    ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XDLET TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94QD-FN. EXIT.
      *N94QG.    NOTE *CALL GHU ON GQ01                   *.            ADU026
       F94QG.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PE06 GQ01                                                    ADU026
           S-GQU01-SSA                                                  ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94QG-FN. EXIT.
      *N94QI.    NOTE *CALL ISRT ON GQ01                  *.            ADU026
       F94QI.                                                           lv10
           MOVE        'TR1P' TO DE10-XDBDNM                            ADU026
           MOVE        'GQ01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XISRT                       ADU026
           PE06 GQ01                                                    ADU026
           S-GQ01-SSA                                                   ADU026
           MOVE        PE06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XISRT TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94QI-FN. EXIT.
      *N94RA.    NOTE *CALL REPL ON CT01                  *.            ADU026
       F94RA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PB06 CT01                                                    ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94RA-FN. EXIT.
      *N94RB.    NOTE *CALL REPL ON CT22                  *.            ADU026
       F94RB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT22' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XREPL                       ADU026
           PB06 CT22                                                    ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XREPL TO SV01-FUNC                          ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94RB-FN. EXIT.
      *N94UA.    NOTE *CALL GHU ON CT01                   *.            ADU026
       F94UA.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PB06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94UA-FN. EXIT.
      *N94UB.    NOTE *CALL GHU ON CT22                   *.            ADU026
       F94UB.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT22' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGHU                        ADU026
           PB06 CT22                                                    ADU026
           S-CTU01-SSA S-CTU22-SSA                                      ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGHU TO SV01-FUNC                           ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94UB-FN. EXIT.
      *N96AL.    NOTE *---> Audit Log Process             *.            ADU165
       F96AL.         EXIT.                                             lv10
      *N96AN.    NOTE *---> Format Audit Log Data         *.            ADU165
       F96AN.                                                           lv15
           SET AL00-NPNTR                                               ADU165
           TO ADDRESS OF DLIUIBII                                       ADU165
           MOVE        AL00-ADDR TO DH10-XUIBP                          ADU165
           MOVE        AL00-NSEQ2P TO DH10-NSEQ2P                       ADU165
           MOVE        'E' TO DH10-CAUL                                 ADU165
           MOVE        'CT22' TO DH10-MAUSB                             ADU165
           MOVE        WZ7B-CTID TO DH10-NAUSK                          ADU165
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
