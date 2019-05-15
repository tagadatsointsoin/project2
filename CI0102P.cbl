       IDENTIFICATION DIVISION.                                         CI0102
       PROGRAM-ID.  CI0102P.                                            CI0102
      *AUTHOR.         VALIDATE ADD/CHG OF UD.                          CI0102
      *DATE-COMPILED.   09/08/14.                                       CI0102
      ******************************************************************ACOPYP
      *     UNPUBLISHED WORK.  COPYRIGHT 1998                          *ACOPYP
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
      *     COPR. 1998                                                 *ACOPYP
      *     AMERIPRISE FINANCIAL, INC.                                 *ACOPYP
      *     70100 AMERIPRISE FINANCIAL CENTER, MINNEAPOLIS, MN¦ 55474  *ACOPYP
      ******************************************************************ACOPYP
       ENVIRONMENT DIVISION.                                            CI0102
       CONFIGURATION SECTION.                                           CI0102
       SOURCE-COMPUTER. IBM-370.                                        CI0102
       OBJECT-COMPUTER. IBM-370.                                        CI0102
       DATA DIVISION.                                                   CI0102
       WORKING-STORAGE SECTION.                                         CI0102
       01  CI0002           PIC X(8) VALUE 'CI0002P '.                  ADU102
       01  CI0003           PIC X(8) VALUE 'CI0003P '.                  AM0003
       01  CI0018           PIC X(8) VALUE 'CI0018P '.                  AM0018
       01  CI0020           PIC X(8) VALUE 'CI0020P '.                  AM0020
       01  CI0095           PIC X(8) VALUE 'CI0095P '.                  AM0095
       01  CI0100           PIC X(8) VALUE 'CI0100P '.                  AM0100
       01  CI0103           PIC X(8) VALUE 'CI0103P '.                  AM0103
       01                 CT01.                                         CI0102
            10            CT01-CT01K.                                   CI0102
            11            CT01-C299.                                    CI0102
            12            CT01-CTID.                                    CI0102
            13            CT01-CTIDA  PICTURE  9(3).                    CI0102
            13            CT01-CTIDN.                                   CI0102
            14            CT01-CTIDNP PICTURE  X(13).                   CI0102
            14            CT01-CTIDND PICTURE  9(11).                   CI0102
            10            CT01-GECKD  PICTURE  9.                       CI0102
            10            CT01-GEMDA  PICTURE  9(8).                    CI0102
            10            CT01-NSEQ4B PICTURE  9(8)                     CI0102
                          BINARY.                                       CI0102
            10            CT01-GECUC  PICTURE  99.                      CI0102
            10            CT01-CTAUL  PICTURE  9(3).                    CI0102
            10            CT01-DIRAC  PICTURE  9(4).                    CI0102
            10            CT01-CTCCI  PICTURE  X.                       CI0102
            10            CT01-CTCUS  PICTURE  999.                     CI0102
            10            CT01-CTEFD  PICTURE  9(8).                    CI0102
            10            CT01-CTIAD  PICTURE  9(8).                    CI0102
            10            CT01-CLCUS  PICTURE  99.                      CI0102
            10            CT01-CAMMB  PICTURE  X(3).                    CI0102
            10            CT01-CKPMM  PICTURE  X.                       CI0102
            10            CT01-CTLAD  PICTURE  9(8).                    CI0102
            10            CT01-IPERS  PICTURE  X.                       CI0102
            10            CT01-AUNCB  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            CT01-CTLAT  PICTURE  9(8).                    CI0102
            10            CT01-CTLATC PICTURE  9(6).                    CI0102
            10            CT01-IMEGA  PICTURE  X.                       CI0102
            10            CT01-DIRAB  PICTURE  9(8).                    CI0102
            10            CT01-COLRQ  PICTURE  X.                       CI0102
            10            CT01-ZDA04  PICTURE  X(4).                    CI0102
            10            CT01-CTLPD  PICTURE  9(8).                    CI0102
            10            CT01-CIRASP PICTURE  9.                       CI0102
            10            CT01-CIRATP PICTURE  99.                      CI0102
            10            CT01-DRTHC  PICTURE  9(8).                    CI0102
            10            CT01-CPPTC  PICTURE  X.                       CI0102
            10            CT01-ZDA06  PICTURE  X(6).                    CI0102
            10            CT01-CTACD  PICTURE  9(8).                    CI0102
            10            CT01-CTNLI  PICTURE  X.                       CI0102
            10            CT01-CTRHO  PICTURE  9(8).                    CI0102
            10            CT01-CTSGD  PICTURE  9(8).                    CI0102
            10            CT01-CPATP  PICTURE  X(1).                    CI0102
            10            CT01-IRSTA  PICTURE  X.                       CI0102
            10            CT01-CTSTA  PICTURE  99.                      CI0102
            10            CT01-CTSSC  PICTURE  99.                      CI0102
            10            CT01-PRLIN  PICTURE  9(3).                    CI0102
            10            CT01-PRCOD  PICTURE  9(5).                    CI0102
            10            CT01-PRSCD  PICTURE  X(9).                    CI0102
            10            CT01-CTLNI  PICTURE  X.                       CI0102
            10            CT01-AYSIDA PICTURE  9(3).                    CI0102
            10            CT01-AYSID  PICTURE  9(5).                    CI0102
            10            CT01-CTBMC  PICTURE  99.                      CI0102
            10            CT01-CINAR  PICTURE  99.                      CI0102
            10            CT01-CPHTR  PICTURE  X.                       CI0102
            10            CT01-CDSTR  PICTURE  XX.                      CI0102
            10            CT01-CQACT  PICTURE  999.                     CI0102
            10            CT01-CIRAS  PICTURE  999.                     CI0102
            10            CT01-CIRAT  PICTURE  999.                     CI0102
            10            CT01-CLRAY  PICTURE  9(5).                    CI0102
            10            CT01-CATTP  PICTURE  X.                       CI0102
       01                 CT13.                                         CI0102
            10            CT13-CT13K.                                   CI0102
            11            CT13-GEHSD  PICTURE  9(8).                    CI0102
            11            CT13-GEHCD  PICTURE  9(3).                    CI0102
            11            CT13-GEHCSE PICTURE  X(12).                   CI0102
            11            CT13-GEHCSU PICTURE  9(5).                    CI0102
            10            CT13-GEHRD  PICTURE  9(8).                    CI0102
            10            CT13-GEHV   PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            CT13-GEDC   PICTURE  9(2).                    CI0102
      *-----> PCB address list for calling module CI0095                AM0095
      *                                                                 AM0095
       01               CI0095-PCB-ADDRESS-LIST.                        AM0095
            05          CI0095-PCB-CL1P-PTR1        POINTER.            AM0095
      *-----------------------------------------------------------------ADU129
      **   Storage area for DL/1                                        ADU129
      *-----------------------------------------------------------------ADU129
      *                                                                 ADU129
      *!WF DSP=XW DSL=XW SEL=05 FOR=I DES=2 LEV=1                       ADU129
       01                 XW05.                                         CI0102
            10            XW05-XW06.                                    CI0102
            11            XW05-XDBPCB.                                  CI0102
            12            XW05-XDBDNM PICTURE  X(08)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-XSEGLV PICTURE  X(02)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-XRC    PICTURE  X(02)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-XPROPT PICTURE  X(04)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-FILLER PICTURE  S9(5)                    CI0102
                          VALUE                ZERO                     CI0102
                          BINARY.                                       CI0102
            12            XW05-XSEGNM PICTURE  X(08)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-XKEYLN PICTURE  S9(05)                   CI0102
                          VALUE                ZERO                     CI0102
                          BINARY.                                       CI0102
            12            XW05-XSEGNB PICTURE  9(05)                    CI0102
                          VALUE                ZERO                     CI0102
                          BINARY.                                       CI0102
            12            XW05-XCOKEY PICTURE  X(70)                    CI0102
                          VALUE                SPACE.                   CI0102
            10            XW05-XW07.                                    CI0102
            11            XW05-XIOPCB.                                  CI0102
            12            XW05-XTERMI PICTURE  X(08)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-FILLER PICTURE  XX                       CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-XRC1   PICTURE  X(02)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-FILLER PICTURE  X(12)                    CI0102
                          VALUE                SPACE.                   CI0102
            12            XW05-XMODNM PICTURE  X(8)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            XW05-XKEYL9 PICTURE  9(05)                    CI0102
                          VALUE                ZERO.                    CI0102
            10            XW05-XSEGN9 PICTURE  9(05)                    CI0102
                          VALUE                ZERO.                    CI0102
            10            XW05-XGU    PICTURE  X(4)                     CI0102
                          VALUE                'GU  '.                  CI0102
            10            XW05-XGHU   PICTURE  X(4)                     CI0102
                          VALUE                'GHU '.                  CI0102
            10            XW05-XGN    PICTURE  X(4)                     CI0102
                          VALUE                'GN  '.                  CI0102
            10            XW05-XGHN   PICTURE  X(4)                     CI0102
                          VALUE                'GHN '.                  CI0102
            10            XW05-XGNP   PICTURE  X(4)                     CI0102
                          VALUE                'GNP '.                  CI0102
            10            XW05-XGHNP  PICTURE  X(4)                     CI0102
                          VALUE                'GHNP'.                  CI0102
            10            XW05-XREPL  PICTURE  XXXX                     CI0102
                          VALUE                'REPL'.                  CI0102
            10            XW05-XISRT  PICTURE  X(4)                     CI0102
                          VALUE                'ISRT'.                  CI0102
            10            XW05-XDLET  PICTURE  X(4)                     CI0102
                          VALUE                'DLET'.                  CI0102
            10            XW05-XOPEN  PICTURE  X(4)                     CI0102
                          VALUE                'OPEN'.                  CI0102
            10            XW05-XCLSE  PICTURE  X(4)                     CI0102
                          VALUE                'CLSE'.                  CI0102
            10            XW05-XCHKP  PICTURE  X(4)                     CI0102
                          VALUE                'CHKP'.                  CI0102
            10            XW05-XXRST  PICTURE  X(4)                     CI0102
                          VALUE                'XRST'.                  CI0102
            10            XW05-XTERM  PICTURE  X(4)                     CI0102
                          VALUE                'TERM'.                  CI0102
            10            XW05-XNFPAC PICTURE  X(13)                    CI0102
                          VALUE                SPACE.                   CI0102
      *!WI pl=DL200                                                     ADU129
       01  DL01-KFPCB                                                   ADU129
                        PICTURE X(04)                                   CI0102
              VALUE 'PCB '.                                             ADU129
      *    Save area for DL1 function value - used for error processing ADU129
       01  SV01-FUNC     PIC X(4).                                      ADU129
      *-----------------------------------------------------------------ADU129
      *   Used when dynamically calling PCB & UIB error checking modulesADU129
      *      (CI0008P - UIB Error check    CI0009P - PCB Error check)   ADU129
      *-----------------------------------------------------------------ADU129
      *!WI pl=DN100                                                     ADU129
       01               W-PASS-XPROGR                                   ADU129
                        PICTURE X(8).                                   CI0102

      *PASS AREA TO/FROM CI0003
      *!WF DSP=FA DSL=DU SEL=04 FOR=I LEV=1 PLT=FA
       01                 FA00.                                         CI0102
          05              FA00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00407).                  CI0102
       01                 FA04  REDEFINES      FA00.                    CI0102
            10            FA04-C299.                                    CI0102
            11            FA04-CTID.                                    CI0102
            12            FA04-CTIDA  PICTURE  9(3).                    CI0102
            12            FA04-CTIDN.                                   CI0102
            13            FA04-CTIDNP PICTURE  X(13).                   CI0102
            13            FA04-CTIDND PICTURE  9(11).                   CI0102
            10            FA04-IPOCH  PICTURE  X.                       CI0102
            10            FA04-FILLER PICTURE  X(099).                  CI0102
            10            FA04-CTTLN1 PICTURE  X(30).                   CI0102
            10            FA04-CTTLN2 PICTURE  X(30).                   CI0102
            10            FA04-CTTLN3 PICTURE  X(30).                   CI0102
            10            FA04-CTTBO1 PICTURE  X(45).                   CI0102
            10            FA04-CTTBO2 PICTURE  X(45).                   CI0102
            10            FA04-CTOWN  PICTURE  9(3).                    CI0102
            10            FA04-IUGMA  PICTURE  X.                       CI0102
            10            FA04-FILLER PICTURE  X(096).                  CI0102
                                                                        AM0100
      ******************************************************************AM0100
      **     FIELDS USED IN THE PARAMETER LIST OF CI0100.  THESE WILL  *AM0100
      **     BE VALUED AND PASSED IN THE CALLING PROGRAM.              *AM0100
      ******************************************************************AM0100
                                                                        AM0100
       01  7-GC00-AREA.                                                 AM0100
      *!WI pl=GC140                                                     AM0100
           05  7-GC00-MAPPN                                             AM0100
                        PICTURE X(10)                                   CI0102
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC180                                                     AM0100
           05  7-GC00-CFUNC                                             AM0100
                        PICTURE X(3)                                    CI0102
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC220                                                     AM0100
           05  7-GC00-CASTC OCCURS 6                                    AM0100
                        PICTURE 99.                                     CI0102
      *!WI pl=GC240                                                     AM0100
           05  7-GC00-CAATY OCCURS 3                                    AM0100
                        PICTURE 9(3).                                   CI0102
           05  7-GC00-C299.                                             AM0100
      *!WI pl=GC280                                                     AM0100
               10  7-GC00-CTID                                          AM0100
                        PICTURE X(27)                                   CI0102
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC320                                                     AM0100
           05  7-GC00-DCACG9                                            AM0100
                        PICTURE 9(8)                                    CI0102
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC360                                                     AM0100
           05  7-GC00-NAASQ                                             AM0100
                        PICTURE S9(3)                                   CI0102
                          COMPUTATIONAL-3                               CI0102
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC400                                                     AM0100
           05  7-GC00-NPISQ                                             AM0100
                        PICTURE S9(3)                                   CI0102
                          COMPUTATIONAL-3                               CI0102
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC440                                                     AM0100
           05  7-GC00-CIRAP                                             AM0100
                        PICTURE XX                                      CI0102
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC480                                                     AM0100
           05  7-GC00-IPERT                                             AM0100
                        PICTURE X                                       CI0102
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC520                                                     AM0100
           05  7-GC00-NEIBT                                             AM0100
                        PICTURE X(7)                                    CI0102
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC560                                                     AM0100
           05  7-GC00-GESQ2C                                            AM0100
                        PICTURE S99                                     CI0102
                          COMPUTATIONAL-3                               CI0102
                                   VALUE ZERO.                          AM0100
      *!WI pl=GC600                                                     AM0100
           05  7-GC00-MIPPS                                             AM0100
                        PICTURE X(4)                                    CI0102
                                   VALUE SPACES.                        AM0100
      *!WI pl=GC640                                                     AM0100
           05  7-GC00-IENDP                                             AM0100
                        PICTURE X                                       CI0102
                                   VALUE SPACES.                        AM0100
                                                                        AM0100
      ******************************************************************AM0100
      **     PCB ADDRESS LIST FOR CI0100.  MODULE CI0100 WILL NEED     *AM0100
      **     PCB'S FOR:                                                *AM0100
      **             ARRANGEMENT DATABASE(ACAP)                        *AM0100
      ******************************************************************AM0100
                                                                        AM0100
       01  CI0100GC-PCB-ADDRESS-LIST.                                   AM0100
           05  CI0100GC-PCB-ACAP-PTR1      POINTER.                     AM0100
      *
      ******************************************************************
      **     CI0018 - SEGMENT OF PARAMETERS     NEEDED TO GET THE      *
      **     REQUESTED TYPE OF CLIENTS FOR THE ACCOUNT NUMBER PASSED.  *
      ******************************************************************
      *
      *!WF DSP=HC DSL=DU SEL=14 FOR=I LEV=1 PLT=HC
       01                 HC00.                                         CI0102
          05              HC00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00917).                  CI0102
       01                 HC14  REDEFINES      HC00.                    CI0102
            10            HC14-C299.                                    CI0102
            11            HC14-CTID.                                    CI0102
            12            HC14-CTIDA  PICTURE  9(3).                    CI0102
            12            HC14-CTIDN.                                   CI0102
            13            HC14-CTIDNP PICTURE  X(13).                   CI0102
            13            HC14-CTIDND PICTURE  9(11).                   CI0102
            10            HC14-DCACG  PICTURE  9(8).                    CI0102
            10            HC14-IPOCH  PICTURE  X.                       CI0102
            10            HC14-FILLER PICTURE  X(100).                  CI0102
            10            HC14-CLID01.                                  CI0102
            11            HC14-CLIDO1 PICTURE  X(3).                    CI0102
            11            HC14-NCLID1.                                  CI0102
            12            HC14-CLIDP1 PICTURE  X(12).                   CI0102
            12            HC14-CLIDNA PICTURE  9(8).                    CI0102
            10            HC14-CLCTR  PICTURE  9(3).                    CI0102
            10            HC14-DU21                                     CI0102
                          OCCURS       025     TIMES.                   CI0102
            11            HC14-C199.                                    CI0102
            12            HC14-CLID.                                    CI0102
            13            HC14-CLIDO  PICTURE  9(3).                    CI0102
            13            HC14-CLIDN.                                   CI0102
            14            HC14-CLIDNP PICTURE  X(12).                   CI0102
            14            HC14-CLIDND PICTURE  9(8).                    CI0102
            11            HC14-CLCTRC PICTURE  9(3).                    CI0102
            10            HC14-QITEM  PICTURE  9(3).                    CI0102
            10            HC14-XIMAX  PICTURE  S9(4)                    CI0102
                          BINARY.                                       CI0102
            10            HC14-CRROL  PICTURE  X.                       CI0102
            10            HC14-FILLER PICTURE  X(099).                  CI0102
      *
      *
      *
      ******************************************************************
      **     THIS SEGMENT CONTAINS THE PARAMETERS NEEDED FOR THE       *
      **                    CALL TO CI0095                             *
      ******************************************************************
      *
      *!WF DSP=HG DSL=PJ SEL=6061 FOR=I DES=1 LEV=1
      * PLT=HG
       01                 HG60.                                         CI0102
            10            HG60-MAPPN  PICTURE  X(10).                   CI0102
            10            HG60-C199.                                    CI0102
            11            HG60-CLID.                                    CI0102
            12            HG60-CLIDO  PICTURE  9(3).                    CI0102
            12            HG60-CLIDN.                                   CI0102
            13            HG60-CLIDNP PICTURE  X(12).                   CI0102
            13            HG60-CLIDND PICTURE  9(8).                    CI0102
            10            HG60-FILLER PICTURE  X(24).                   CI0102
       01                 HG61.                                         CI0102
            10            HG61-ICI95  PICTURE  X.                       CI0102
            10            HG61-CLNAMF PICTURE  X(20).                   CI0102
            10            HG61-CLNAML PICTURE  X(25).                   CI0102
            10            HG61-CLNAMI PICTURE  X.                       CI0102
            10            HG61-CLNAMR PICTURE  X(14).                   CI0102
            10            HG61-CLNAMH PICTURE  X(6).                    CI0102
            10            HG61-CLNAMS PICTURE  X(4).                    CI0102
            10            HG61-GEPHNH PICTURE  X(14).                   CI0102
            10            HG61-GEPHNB PICTURE  X(14).                   CI0102
            10            HG61-GEPHNX PICTURE  9(4).                    CI0102
            10            HG61-GEPHNA PICTURE  X(14).                   CI0102
            10            HG61-CLDOB  PICTURE  9(8).                    CI0102
            10            HG61-CLDOD  PICTURE  9(8).                    CI0102
            10            HG61-CLDTH  PICTURE  X.                       CI0102
            10            HG61-CLAIN  PICTURE  S9(11)                   CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HG61-CLMAR  PICTURE  X.                       CI0102
            10            HG61-IOCOB  PICTURE  X.                       CI0102
            10            HG61-CLSEX  PICTURE  X.                       CI0102
            10            HG61-ICUSC  PICTURE  X.                       CI0102
            10            HG61-CLAEN  PICTURE  X(12).                   CI0102
            10            HG61-MPRFS  PICTURE  X(4).                    CI0102
            10            HG61-CRACE  PICTURE  X.                       CI0102
            10            HG61-CNIRA  PICTURE  X.                       CI0102
            10            HG61-TCTZN  PICTURE  X(25).                   CI0102
            10            HG61-CLEMCL PICTURE  X(25).                   CI0102
            10            HG61-CLEMCS PICTURE  X(6).                    CI0102
            10            HG61-CLMARS PICTURE  X(10).                   CI0102
            10            HG61-CLOCCD PICTURE  X(40).                   CI0102
            10            HG61-CLOCCS PICTURE  X(20).                   CI0102
            10            HG61-CLOCCL PICTURE  X(40).                   CI0102
            10            HG61-GEDLA  PICTURE  9(8).                    CI0102
            10            HG61-QYEAR  PICTURE  9(3).                    CI0102
            10            HG61-QMTHAA PICTURE  9(2).                    CI0102
            10            HG61-MCTYC  PICTURE  X(20).                   CI0102
                                                                        AM0103
      ******************************************************************AM0103
      **     REQUEST SEGMENT FOR CI0103.                               *AM0103
      ******************************************************************AM0103
      *!WF DSP=HS DSL=PJ SEL=51 FOR=I DES=1 LEV=1                       AM0103
       01                 HS51.                                         CI0102
            10            HS51-MAPPN  PICTURE  X(10).                   CI0102
            10            HS51-C299.                                    CI0102
            11            HS51-CTID.                                    CI0102
            12            HS51-CTIDA  PICTURE  9(3).                    CI0102
            12            HS51-CTIDN.                                   CI0102
            13            HS51-CTIDNP PICTURE  X(13).                   CI0102
            13            HS51-CTIDND PICTURE  9(11).                   CI0102
            10            HS51-IGOTY  PICTURE  X.                       CI0102
            10            HS51-FILLER PICTURE  X(30).                   CI0102
       01                 HS52.                                         CI0102
            10            HS52-AACTV  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-ADDAC  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-AGRPV  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-QSHOW  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-AFAVP  PICTURE  S9(4)V9(3)               CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-DASOF  PICTURE  9(8).                    CI0102
            10            HS52-QDHGF  PICTURE  9(2).                    CI0102
            10            HS52-QSHIS  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-CPORT  PICTURE  X.                       CI0102
            10            HS52-AWDRTP PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-AWDRTC PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-APPAYA PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-APPAYN PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-IGOTYA PICTURE  X.                       CI0102
            10            HS52-QTYUD1 PICTURE  9(5).                    CI0102
            10            HS52-QTYUD2 PICTURE  9(5).                    CI0102
            10            HS52-AGOFD  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-AGOFD1 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-ANGOF  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-AATOTI PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-QSHOM  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            HS52-FILLER PICTURE  X(43).                   CI0102
      **                                                                AM0103
      ******************************************************************AM0103
      **     RESPONSE SEGMENT FOR CI0103.                              *AM0103
      ******************************************************************AM0103
      *!WF DSP=HS DSL=PJ SEL=52 FOR=I DES=1 LEV=1                       AM0103
      **                                                                AM0103
      ******************************************************************AM0103
      **     FIELDS USED IN THE PARAMETER LIST OF CI0103.  THESE WILL  *AM0103
      **     BE VALUED AND PASSED IN THE CALLING PROGRAM.              *AM0103
      ******************************************************************AM0103
      **                                                                AM0103
       01  7-HS00-AREA.                                                 AM0103
      *!WI pl=HS260                                                     AM0103
           05  7-HS00-MAPPN                                             AM0103
                        PICTURE X(10)                                   CI0102
                                   VALUE SPACES.                        AM0103
           05  7-HS00-C299.                                             AM0103
      *!WI pl=HS320                                                     AM0103
               10  7-HS00-CTID                                          AM0103
                        PICTURE X(27)                                   CI0102
                                   VALUE ZEROES.                        AM0103
      *!WI pl=HS360                                                     AM0103
           05  7-HS00-IGOTY                                             AM0103
                        PICTURE X                                       CI0102
                                   VALUE SPACES.                        AM0103
      ******************************************************************AM0103
      **     PCB ADDRESS LIST FOR CI0103.  MODULE CI0103 WILL NEED     *AM0103
      **     PCB'S FOR:                                                *AM0103
      **             SHARK ACCOUNT DATABASE(SA1P)                      *AM0103
      **             SHARK PRODUCT RULES DATABASE(SBUP)                *AM0103
      **             SHARK PRODUCT RULES DATABASE(SCOP)                *AM0103
      **             SHARK PRICE DATABASE(SPRP)                        *AM0103
      **             SHARK PRODUCT RULES DATABASE(SSPP)                *AM0103
      **             CONTRACT DATABASE(CT1P)                           *AM0103
      **             ARRANGEMENT DATABASE(ACAP)                        *AM0103
      **             MISC TRAN DATABASE(TR1P)                          *AM0103
      ******************************************************************AM0103
                                                                        AM0103
       01  CI0103HS-PCB-ADDRESS-LIST.                                   AM0103
           05  CI0103HS-PCB-SBUP-PTR1      POINTER.                     AM0103
           05  CI0103HS-PCB-SCOP-PTR1      POINTER.                     AM0103
           05  CI0103HS-PCB-SSPP-PTR1      POINTER.                     AM0103
           05  CI0103HS-PCB-CT1P-PTR1      POINTER.                     AM0103
           05  CI0103HS-PCB-ACAP-PTR1      POINTER.                     AM0103
           05  CI0103HS-PCB-TR1P-PTR1      POINTER.                     AM0103
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
       01                 NS00.                                         CI0102
          05              NS00-00.                                      CI0102
            10            NS00-NS00K.                                   CI0102
            11            NS00-PRCSTK PICTURE  XX.                      CI0102
          05              NS00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00078).                  CI0102
       01                 NS20  REDEFINES      NS00.                    CI0102
            10       FILLER         PICTURE  X(00002).                  CI0102
            10            NS20-DCACG  PICTURE  9(8).                    CI0102
            10            NS20-DCACJ  PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            NS20-CCDAT  PICTURE  X(8).                    CI0102
            10            NS20-DCALP  PICTURE  X(12).                   CI0102
            10            NS20-DNACG  PICTURE  9(8).                    CI0102
            10            NS20-DNACJ  PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            NS20-CNDAT  PICTURE  X(8).                    CI0102
            10            NS20-DNALP  PICTURE  X(12).                   CI0102
            10            NS20-DCACD  PICTURE  X(10).                   CI0102
            10            NS20-FILLER PICTURE  X(4).                    CI0102
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
      *                                                                 AM0020
                                                                        AM0003
      ******************************************************************AM0003
      **     PCB ADDRESS LIST FOR CI0003.  MODULE CI0003 WILL NEED     *AM0003
      **     PCB'S FOR:                                                *AM0003
      **                CONTRACT DATABASE(CT1P)                        *AM0003
      ******************************************************************AM0003
                                                                        AM0003
       01  CI0003A-PCB-ADDRESS-LIST.                                    AM0003
           05  CI0003A-PCB-CT1P-PTR1      POINTER.                      AM0003
                                                                        AM0018
      ******************************************************************AM0018
      **     PCB ADDRESS LIST FOR CI0018.  MODULE CI0018 WILL NEED     *AM0018
      **     PCB'S FOR:                                                *AM0018
      **                CONTRACT DATABASE(CT1P)                        *AM0018
      ******************************************************************AM0018
                                                                        AM0018
       01  CI0018B-PCB-ADDRESS-LIST.                                    AM0018
           05  CI0018B-PCB-CT1P-PTR1      POINTER.                      AM0018
      *-----------------------------------------------------------------
      *>>>>>> INPUT SEGMENT FOR MODULE CI0100.
      *-----------------------------------------------------------------
      *
      *!WF DSP=PK DSL=PJ SEL=40 FOR=I DES=1 LEV=1 PLT=PK
       01                 PK40.                                         CI0102
            10            PK40-MAPPN  PICTURE  X(10).                   CI0102
            10            PK40-CFUNC  PICTURE  X(3).                    CI0102
            10            PK40-CASTC  PICTURE  99                       CI0102
                          OCCURS       006     TIMES.                   CI0102
            10            PK40-CAATY  PICTURE  9(3)                     CI0102
                          OCCURS       003     TIMES.                   CI0102
            10            PK40-C299.                                    CI0102
            11            PK40-CTID.                                    CI0102
            12            PK40-CTIDA  PICTURE  9(3).                    CI0102
            12            PK40-CTIDN.                                   CI0102
            13            PK40-CTIDNP PICTURE  X(13).                   CI0102
            13            PK40-CTIDND PICTURE  9(11).                   CI0102
            10            PK40-DCACG9 PICTURE  9(8).                    CI0102
            10            PK40-NAASQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            PK40-NPISQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            PK40-CIRAP  PICTURE  XX.                      CI0102
            10            PK40-IPERT  PICTURE  X.                       CI0102
            10            PK40-NEIBT  PICTURE  X(7).                    CI0102
            10            PK40-GESQ2C PICTURE  S99                      CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            PK40-MIPPS  PICTURE  X(4).                    CI0102
            10            PK40-IENDP  PICTURE  X.                       CI0102
            10            PK40-FILLER PICTURE  X(20).                   CI0102
       01                 PK41.                                         CI0102
            10            PK41-IENDP  PICTURE  X.                       CI0102
            10            PK41-MIPPS  PICTURE  X(4).                    CI0102
            10            PK41-GC01.                                    CI0102
            11            PK41-GC01K.                                   CI0102
            12            PK41-C299.                                    CI0102
            13            PK41-CTID.                                    CI0102
            14            PK41-CTIDA  PICTURE  9(3).                    CI0102
            14            PK41-CTIDN.                                   CI0102
            15            PK41-CTIDNP PICTURE  X(13).                   CI0102
            15            PK41-CTIDND PICTURE  9(11).                   CI0102
            11            PK41-DCAG9L PICTURE  9(8).                    CI0102
            11            PK41-NAASQL PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            PK41-ICUST  PICTURE  X.                       CI0102
            11            PK41-NSEQ4B PICTURE  9(8)                     CI0102
                          BINARY.                                       CI0102
            11            PK41-PRCOD  PICTURE  9(5).                    CI0102
            11            PK41-PRSCD  PICTURE  X(9).                    CI0102
            11            PK41-FILLER PICTURE  X(8).                    CI0102
            10            PK41-IGC01  PICTURE  X(01).                   CI0102
            10            PK41-QDECT9 PICTURE  99.                      CI0102
            10            PK41-FILLER PICTURE  X(20).                   CI0102
            10            PK41-GAKEY                                    CI0102
                          OCCURS       010     TIMES.                   CI0102
            11            PK41-IGC03  PICTURE  X(01).                   CI0102
            11            PK41-IGC04  PICTURE  X(01).                   CI0102
            11            PK41-IGC06  PICTURE  X(01).                   CI0102
            11            PK41-IGC12  PICTURE  X(01).                   CI0102
            11            PK41-IGC21  PICTURE  X(01).                   CI0102
            11            PK41-GC03.                                    CI0102
            12            PK41-GELL   PICTURE  9(4)                     CI0102
                          BINARY.                                       CI0102
            12            PK41-GD00.                                    CI0102
            13            PK41-GC03K.                                   CI0102
            14            PK41-DCACG9 PICTURE  9(8).                    CI0102
            14            PK41-NAASQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CAATY  PICTURE  9(3).                    CI0102
            13            PK41-CVSYS  PICTURE  X(2).                    CI0102
            13            PK41-CACTO  PICTURE  9(3).                    CI0102
            13            PK41-CATRN.                                   CI0102
            14            PK41-CATRF  PICTURE  9(3).                    CI0102
            14            PK41-CATRS  PICTURE  9(3).                    CI0102
            13            PK41-CASTC  PICTURE  99.                      CI0102
            13            PK41-IPULL  PICTURE  X.                       CI0102
            13            PK41-GEAUN  PICTURE  9(5).                    CI0102
            13            PK41-GEOPD2 PICTURE  X(8).                    CI0102
            13            PK41-NBTCH  PICTURE  9(4).                    CI0102
            13            PK41-DEFFT  PICTURE  9(8).                    CI0102
            13            PK41-NSUNT  PICTURE  9(4).                    CI0102
            13            PK41-ITRAN  PICTURE  X.                       CI0102
            13            PK41-DLAUP1 PICTURE  9(8).                    CI0102
            13            PK41-ADRET  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-TTRMS  PICTURE  X(12).                   CI0102
            13            PK41-IDELT  PICTURE  X.                       CI0102
            13            PK41-GEOPDM PICTURE  X(8).                    CI0102
            13            PK41-FILLER PICTURE  X(07).                   CI0102
            12            PK41-GD09.                                    CI0102
            13            PK41-FILLER PICTURE  X(70).                   CI0102
            12            PK41-GD01                                     CI0102
                          REDEFINES            PK41-GD09.               CI0102
            13            PK41-ADBRQ  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CTRTP  PICTURE  X(2).                    CI0102
            13            PK41-CPORT  PICTURE  X.                       CI0102
            13            PK41-CSCRNU PICTURE  X(4).                    CI0102
            13            PK41-DLAUP  PICTURE  9(8).                    CI0102
            13            PK41-CTWHAT PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-PWHLD  PICTURE  S999V9(5)                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-IWTHH  PICTURE  X.                       CI0102
            13            PK41-NDRFT  PICTURE  9(5).                    CI0102
            13            PK41-IDPAP  PICTURE  X.                       CI0102
            13            PK41-GETIM  PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QNACT  PICTURE  9(3).                    CI0102
            13            PK41-AEDRQ  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-IPLIN  PICTURE  X.                       CI0102
            13            PK41-CLIDNB PICTURE  9(8).                    CI0102
            13            PK41-CSLCT  PICTURE  X.                       CI0102
            13            PK41-ITELE  PICTURE  X.                       CI0102
            13            PK41-FILLER PICTURE  X(06).                   CI0102
            12            PK41-GD02                                     CI0102
                          REDEFINES            PK41-GD09.               CI0102
            13            PK41-CSYST  PICTURE  99.                      CI0102
            13            PK41-FILLER PICTURE  X.                       CI0102
            13            PK41-ACASH  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-DTRAC  PICTURE  9(8).                    CI0102
            13            PK41-CTRSO  PICTURE  9(02).                   CI0102
            13            PK41-NTRCE  PICTURE  9(06).                   CI0102
            13            PK41-GECKD1 PICTURE  9.                       CI0102
            13            PK41-CCOLL  PICTURE  X(3).                    CI0102
            13            PK41-CLTDP  PICTURE  X(3).                    CI0102
            13            PK41-PSLLD  PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ISLOR  PICTURE  X.                       CI0102
            13            PK41-ITPAC  PICTURE  X.                       CI0102
            13            PK41-CPMTCA PICTURE  XXX.                     CI0102
            13            PK41-CSERV  PICTURE  X(3).                    CI0102
            13            PK41-ACOMO  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-IPLIN1 PICTURE  X.                       CI0102
            13            PK41-INQEX  PICTURE  X.                       CI0102
            13            PK41-CTKRAA PICTURE  X(12).                   CI0102
            13            PK41-CCSMQ  PICTURE  X.                       CI0102
            13            PK41-IVAEX1 PICTURE  X.                       CI0102
            13            PK41-IHPMT  PICTURE  X(1).                    CI0102
            13            PK41-GETIM3 PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-GD03                                     CI0102
                          REDEFINES            PK41-GD09.               CI0102
            13            PK41-CATRNC PICTURE  9(6).                    CI0102
            13            PK41-APRNT1 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QSHOWT PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACINVT PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACOMO7 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QSHOMW PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ATAXT3 PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CTSTR  PICTURE  9(2).                    CI0102
            13            PK41-ICIRA  PICTURE  X.                       CI0102
            13            PK41-GETIM2 PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CPMTCX PICTURE  XX.                      CI0102
            13            PK41-FILLER PICTURE  X(16).                   CI0102
            12            PK41-GD99.                                    CI0102
            13            PK41-FILLER PICTURE  X(248).                  CI0102
            12            PK41-GD10                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-MROTC  PICTURE  X(7).                    CI0102
            13            PK41-CEDSC  PICTURE  9(1).                    CI0102
            13            PK41-ILPOI  PICTURE  X(1).                    CI0102
            13            PK41-AWRCH  PICTURE  S9(3)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CHCOC1 PICTURE  9(2).                    CI0102
            13            PK41-CHCOC2 PICTURE  9(2).                    CI0102
            13            PK41-CHCOC3 PICTURE  9(2).                    CI0102
            13            PK41-CHCOC4 PICTURE  9(2).                    CI0102
            13            PK41-CMCOC1 PICTURE  9(3).                    CI0102
            13            PK41-CMCOC2 PICTURE  9(3).                    CI0102
            13            PK41-CMCOC3 PICTURE  9(3).                    CI0102
            13            PK41-GD11.                                    CI0102
            14            PK41-FILLER PICTURE  X(219).                  CI0102
            13            PK41-GD12                                     CI0102
                          REDEFINES            PK41-GD11.               CI0102
            14            PK41-CELLO  PICTURE  9(1).                    CI0102
            14            PK41-CECLO  PICTURE  9(1).                    CI0102
            14            PK41-AEXML  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CEPI   PICTURE  X(1).                    CI0102
            14            PK41-CEXTY  PICTURE  X.                       CI0102
            14            PK41-CROPC  PICTURE  9(1).                    CI0102
            14            PK41-CPUTY  PICTURE  9(1).                    CI0102
            14            PK41-IMCII  PICTURE  X(1).                    CI0102
            14            PK41-GEMISC                                   CI0102
                          OCCURS       010     TIMES.                   CI0102
            15            PK41-AMGLA  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            15            PK41-CMGLC  PICTURE  9(1).                    CI0102
            15            PK41-NMGLN  PICTURE  9(4).                    CI0102
            14            PK41-ACTRN  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-IWRBK  PICTURE  X.                       CI0102
            14            PK41-IFEDX  PICTURE  X.                       CI0102
            14            PK41-ICNTR  PICTURE  X.                       CI0102
            14            PK41-IOCKH  PICTURE  X.                       CI0102
            14            PK41-ICRCK  PICTURE  X.                       CI0102
            14            PK41-NHMPN  PICTURE  S9(10)                   CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ITELR1 PICTURE  X.                       CI0102
            13            PK41-GD13                                     CI0102
                          REDEFINES            PK41-GD11.               CI0102
            14            PK41-DREDO  PICTURE  9(8).                    CI0102
            14            PK41-CATRNR PICTURE  9(6).                    CI0102
            14            PK41-CEVN   PICTURE  9(9).                    CI0102
            14            PK41-ISUSP  PICTURE  X(1).                    CI0102
            13            PK41-GD15                                     CI0102
                          REDEFINES            PK41-GD11.               CI0102
            14            PK41-CPUTZ  PICTURE  9(1).                    CI0102
            14            PK41-CETLB  PICTURE  9(3).                    CI0102
            14            PK41-QTRMC  PICTURE  9(3).                    CI0102
            14            PK41-DEFFTE PICTURE  9(8).                    CI0102
            14            PK41-DEFFTF PICTURE  9(8).                    CI0102
            14            PK41-DEFFTG PICTURE  9(8).                    CI0102
            14            PK41-XZ1A   PICTURE  X.                       CI0102
            14            PK41-XZ1B   PICTURE  X.                       CI0102
            14            PK41-XZ1C   PICTURE  X.                       CI0102
            14            PK41-XZ1D   PICTURE  X.                       CI0102
            14            PK41-XZ1E   PICTURE  X.                       CI0102
            14            PK41-XZ1F   PICTURE  X.                       CI0102
            14            PK41-XZ1G   PICTURE  X.                       CI0102
            14            PK41-XZ1H   PICTURE  X.                       CI0102
            14            PK41-XZ1I   PICTURE  X.                       CI0102
            14            PK41-DEFFTH PICTURE  9(8).                    CI0102
            13            PK41-GD19                                     CI0102
                          REDEFINES            PK41-GD11.               CI0102
            14            PK41-GD11.                                    CI0102
            15            PK41-FILLER PICTURE  X(219).                  CI0102
            12            PK41-GD20                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-ADDACT PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ISIGV  PICTURE  X.                       CI0102
            13            PK41-IALLF  PICTURE  X.                       CI0102
            13            PK41-QSHOWQ PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CCDSCW PICTURE  9(2).                    CI0102
            13            PK41-IDWRL  PICTURE  X.                       CI0102
            13            PK41-ITELR  PICTURE  X.                       CI0102
            13            PK41-IABIN  PICTURE  X.                       CI0102
            13            PK41-PACT1  PICTURE  S999V999                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-IBFAF  PICTURE  X.                       CI0102
            13            PK41-IFRSA  PICTURE  X.                       CI0102
            13            PK41-ICRCAN PICTURE  X.                       CI0102
            13            PK41-ACACTV PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-AGFND  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QCSHOW PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QCSHIS PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-NDTRC  PICTURE  9(8).                    CI0102
            13            PK41-CAERU  PICTURE  X(4).                    CI0102
            13            PK41-IFDGO  PICTURE  X.                       CI0102
            13            PK41-PSLLD2 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ISLOR2 PICTURE  X.                       CI0102
            13            PK41-QSFIO  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QSFID  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CGDIN  PICTURE  X.                       CI0102
            13            PK41-DGDIN  PICTURE  9(8).                    CI0102
            12            PK41-GD30                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-ISKED  PICTURE  X.                       CI0102
            13            PK41-CENXC  PICTURE  9(2).                    CI0102
            13            PK41-GD31.                                    CI0102
            14            PK41-FILLER PICTURE  X(245).                  CI0102
            13            PK41-GD32                                     CI0102
                          REDEFINES            PK41-GD31.               CI0102
            14            PK41-IABIN1 PICTURE  X.                       CI0102
            14            PK41-CLDOD  PICTURE  9(8).                    CI0102
            14            PK41-NCLAM  PICTURE  9(5)                     CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ISURR  PICTURE  X.                       CI0102
            14            PK41-GEHCD  PICTURE  9(3).                    CI0102
            14            PK41-CRATC  PICTURE  9(4).                    CI0102
            14            PK41-AMAXD  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ASCHGA PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-APYOM  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-IWTHH1 PICTURE  X.                       CI0102
            14            PK41-CPAYCL PICTURE  X(2).                    CI0102
            14            PK41-CTSAO  PICTURE  X.                       CI0102
            14            PK41-NCONF  PICTURE  9(08).                   CI0102
            14            PK41-CLID   PICTURE  X(23).                   CI0102
            14            PK41-CARTY  PICTURE  99.                      CI0102
            14            PK41-NARRS  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CARTZ  PICTURE  99.                      CI0102
            14            PK41-NAPDS  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CPMTO  PICTURE  X.                       CI0102
            14            PK41-DNPMT  PICTURE  9(8).                    CI0102
            14            PK41-IPCTV  PICTURE  X.                       CI0102
            14            PK41-IMECH  PICTURE  X(01).                   CI0102
            14            PK41-IMVAO  PICTURE  X(1).                    CI0102
            14            PK41-AMVA1  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CACTS  PICTURE  X.                       CI0102
            14            PK41-CTSPP  PICTURE  X(1).                    CI0102
            14            PK41-CACT4  PICTURE  X(2).                    CI0102
            14            PK41-IVAEX  PICTURE  X.                       CI0102
            14            PK41-DFPMT  PICTURE  9(8).                    CI0102
            14            PK41-IDEMD  PICTURE  X.                       CI0102
            14            PK41-IOFST  PICTURE  X.                       CI0102
            14            PK41-AMXLB  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ACULB  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-DEIRNB PICTURE  9(8).                    CI0102
            14            PK41-DEFFE  PICTURE  9(8).                    CI0102
            14            PK41-DEFFR  PICTURE  9(8).                    CI0102
            14            PK41-ISPUP  PICTURE  X.                       CI0102
            14            PK41-CPNCG  PICTURE  X.                       CI0102
            14            PK41-IEXPU  PICTURE  X.                       CI0102
            14            PK41-IPPCF  PICTURE  X.                       CI0102
            14            PK41-NAAPT  PICTURE  9(2).                    CI0102
            14            PK41-PWHLDS PICTURE  S999V9(5)                CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ISWHO  PICTURE  X(1).                    CI0102
            13            PK41-GD33                                     CI0102
                          REDEFINES            PK41-GD31.               CI0102
            14            PK41-CPAYC  PICTURE  X(2).                    CI0102
            14            PK41-ADBRQX PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ADBRQV PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-APTXR  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CTRTPE PICTURE  X(2).                    CI0102
            14            PK41-NCLAMI PICTURE  S9(9)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CLIDO8 PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CLIDN  PICTURE  X(20).                   CI0102
            14            PK41-DSET01 PICTURE  S9(8)                    CI0102
                          BINARY.                                       CI0102
            14            PK41-CTSET1 PICTURE  S9(6)                    CI0102
                          BINARY.                                       CI0102
            14            PK41-DSET02 PICTURE  S9(8)                    CI0102
                          BINARY.                                       CI0102
            14            PK41-CTSET2 PICTURE  S9(6)                    CI0102
                          BINARY.                                       CI0102
            13            PK41-GD34                                     CI0102
                          REDEFINES            PK41-GD31.               CI0102
            14            PK41-QNOFM  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-CLTRM  PICTURE  99.                      CI0102
            14            PK41-AMXLN  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ALADJ  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ACHK   PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-APRMO  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-IMECH1 PICTURE  X(01).                   CI0102
            14            PK41-CACT41 PICTURE  X(2).                    CI0102
            14            PK41-ACDSCC PICTURE  S9(05)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-ACDSCD PICTURE  S9(05)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-GD39                                     CI0102
                          REDEFINES            PK41-GD31.               CI0102
            14            PK41-GD31.                                    CI0102
            15            PK41-FILLER PICTURE  X(245).                  CI0102
            12            PK41-GD40                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-NTR    PICTURE  9(8).                    CI0102
            13            PK41-NPBNC  PICTURE  X(24).                   CI0102
            13            PK41-CRREV  PICTURE  X(3).                    CI0102
            13            PK41-CSUSL  PICTURE  S9.                      CI0102
            13            PK41-NMGLN1 PICTURE  9(4).                    CI0102
            13            PK41-DCAC92 PICTURE  9(8).                    CI0102
            13            PK41-NAASQ3 PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-GD49.                                    CI0102
            14            PK41-FILLER PICTURE  X(198).                  CI0102
            13            PK41-GD41                                     CI0102
                          REDEFINES            PK41-GD49.               CI0102
            14            PK41-CRREF  PICTURE  9(2).                    CI0102
            14            PK41-CORIR  PICTURE  X(02).                   CI0102
            14            PK41-CIPDB  PICTURE  X(03).                   CI0102
            14            PK41-CPAYH  PICTURE  X(02).                   CI0102
            14            PK41-NAMEX  PICTURE  9(15)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-DCHAE  PICTURE  9(4).                    CI0102
            14            PK41-DRQST  PICTURE  S9(8)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-GD42                                     CI0102
                          REDEFINES            PK41-GD49.               CI0102
            14            PK41-CPMTCB PICTURE  X(3).                    CI0102
            12            PK41-GD50                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-ALOAD  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-PSLLD4 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CSUSL1 PICTURE  S9.                      CI0102
            13            PK41-CRREV1 PICTURE  X(3).                    CI0102
            13            PK41-ADDAC  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-DL13.                                    CI0102
            14            PK41-GEYR   PICTURE  9(4).                    CI0102
            14            PK41-GEMTH  PICTURE  99.                      CI0102
            14            PK41-NDAY   PICTURE  99.                      CI0102
            13            PK41-NSEQ3P PICTURE  S9(5)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-XZ6A   PICTURE  X(6).                    CI0102
            13            PK41-XZ7    PICTURE  X(7).                    CI0102
            13            PK41-XZ6B   PICTURE  X(6).                    CI0102
            13            PK41-XZ6    PICTURE  X(6).                    CI0102
            13            PK41-XZ6C   PICTURE  X(6).                    CI0102
            13            PK41-XZ20   PICTURE  X(20).                   CI0102
            13            PK41-CATRN1 PICTURE  9(6).                    CI0102
            13            PK41-ADDAC2 PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ATAXT2 PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACOMOT PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-XZ5    PICTURE  X(5).                    CI0102
            13            PK41-IREVD  PICTURE  X(1).                    CI0102
            13            PK41-ISUSP1 PICTURE  X(1).                    CI0102
            13            PK41-XZ6D   PICTURE  X(6).                    CI0102
            13            PK41-XZ13   PICTURE  X(13).                   CI0102
            13            PK41-CWHTP2 PICTURE  X(3).                    CI0102
            13            PK41-CWHTP3 PICTURE  X(3).                    CI0102
            13            PK41-DTREN  PICTURE  9(8).                    CI0102
            13            PK41-NAASQ1 PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-GD51                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-ADOMOT PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACGLT  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACGST  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CTXMT  PICTURE  9(2).                    CI0102
            13            PK41-ALOAD3 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-FILLER PICTURE  X(31).                   CI0102
            12            PK41-GD52                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-DEFFT5 PICTURE  9(8).                    CI0102
            13            PK41-PSLLD5 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CSUSL2 PICTURE  S9.                      CI0102
            13            PK41-ALOAD2 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-DL22.                                    CI0102
            14            PK41-NYEAR1 PICTURE  9(4).                    CI0102
            14            PK41-GEMTHA PICTURE  99.                      CI0102
            14            PK41-NDAY01 PICTURE  99.                      CI0102
            13            PK41-NSEQ3R PICTURE  S9(5)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CWHTP  PICTURE  X(3).                    CI0102
            13            PK41-CWHFR  PICTURE  X(3).                    CI0102
            13            PK41-CATRN7 PICTURE  9(6).                    CI0102
            13            PK41-ATAXT5 PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QSHOT  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACINT3 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CWHTP1 PICTURE  X(3).                    CI0102
            13            PK41-CWHFR1 PICTURE  X(3).                    CI0102
            13            PK41-ACOMO5 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QSHOMU PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACASH1 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-FILLER PICTURE  X(04).                   CI0102
            13            PK41-CATRN8 PICTURE  9(6).                    CI0102
            13            PK41-ALOAD1 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-PSLLD1 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QSHOT1 PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACINT4 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CSUSL4 PICTURE  S9.                      CI0102
            13            PK41-ACOMO4 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-GD60                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-GEOPDD PICTURE  X(8)                     CI0102
                          OCCURS       005     TIMES.                   CI0102
            13            PK41-DLAUP3 PICTURE  9(8)                     CI0102
                          OCCURS       005     TIMES.                   CI0102
            13            PK41-GEOPDB PICTURE  X(8).                    CI0102
            13            PK41-DLAUP4 PICTURE  9(8).                    CI0102
            13            PK41-ITELR2 PICTURE  X.                       CI0102
            13            PK41-IPMTA  PICTURE  X.                       CI0102
            13            PK41-CCSMG  PICTURE  X.                       CI0102
            13            PK41-CPLEC  PICTURE  XX.                      CI0102
            13            PK41-CORTYA PICTURE  X(3).                    CI0102
            13            PK41-CACTBC PICTURE  X(1).                    CI0102
            13            PK41-CGSPIA PICTURE  X.                       CI0102
            13            PK41-IPTRDA PICTURE  X(01).                   CI0102
            13            PK41-GCUSPY PICTURE  X(12).                   CI0102
            13            PK41-CPALLA PICTURE  X(1).                    CI0102
            13            PK41-QSHO5A PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-IFRSAB PICTURE  X.                       CI0102
            13            PK41-DELOI  PICTURE  9(8).                    CI0102
            13            PK41-IAROAA PICTURE  X.                       CI0102
            13            PK41-ACINVR PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ILTINA PICTURE  X.                       CI0102
            13            PK41-ALOIDA PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CFUNTA PICTURE  X(2).                    CI0102
            13            PK41-CLGND  PICTURE  X.                       CI0102
            13            PK41-CPH3U  PICTURE  X.                       CI0102
            13            PK41-GESTD  PICTURE  9(8).                    CI0102
            13            PK41-GEEND  PICTURE  9(8).                    CI0102
            13            PK41-CPMTF  PICTURE  99.                      CI0102
            13            PK41-CNAVR  PICTURE  X(1).                    CI0102
            12            PK41-GD70                                     CI0102
                          REDEFINES            PK41-GD99.               CI0102
            13            PK41-CMEMO  PICTURE  X(2).                    CI0102
            13            PK41-ALPLDT PICTURE  9(8).                    CI0102
            13            PK41-CTLPD  PICTURE  9(8).                    CI0102
            13            PK41-CPAYCM PICTURE  X(2).                    CI0102
            11            PK41-GC06.                                    CI0102
            12            PK41-GELL   PICTURE  9(4)                     CI0102
                          BINARY.                                       CI0102
            12            PK41-GE00.                                    CI0102
            13            PK41-GC06K.                                   CI0102
            14            PK41-NPISQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-ACOTD  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-PPOTD  PICTURE  S9(3)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-QPSTD  PICTURE  S9(7)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CPITC  PICTURE  99.                      CI0102
            13            PK41-ITRNB  PICTURE  X.                       CI0102
            13            PK41-FILLER PICTURE  X(14).                   CI0102
            12            PK41-GE98.                                    CI0102
            13            PK41-FILLER PICTURE  X(240).                  CI0102
            12            PK41-GE10                                     CI0102
                          REDEFINES            PK41-GE98.               CI0102
            13            PK41-CDELI  PICTURE  9(3).                    CI0102
            13            PK41-CPAYC  PICTURE  X(2).                    CI0102
            13            PK41-ICHKP  PICTURE  X.                       CI0102
            13            PK41-CLTIN  PICTURE  9(12).                   CI0102
            13            PK41-IFHAI  PICTURE  X.                       CI0102
            13            PK41-CDQUA  PICTURE  X(2).                    CI0102
            13            PK41-FILLER PICTURE  X(07).                   CI0102
            13            PK41-GE99.                                    CI0102
            14            PK41-FILLER PICTURE  X(212).                  CI0102
            13            PK41-GE01                                     CI0102
                          REDEFINES            PK41-GE99.               CI0102
            14            PK41-NTR    PICTURE  9(8).                    CI0102
            14            PK41-GECKD  PICTURE  9.                       CI0102
            14            PK41-NPBN   PICTURE  X(20).                   CI0102
            14            PK41-CCBAT  PICTURE  99.                      CI0102
            14            PK41-CLID4  PICTURE  X(23).                   CI0102
            14            PK41-GENAL1 PICTURE  X(30)                    CI0102
                          OCCURS       002     TIMES.                   CI0102
            14            PK41-GESAD1 PICTURE  X(30)                    CI0102
                          OCCURS       003     TIMES.                   CI0102
            13            PK41-GE02                                     CI0102
                          REDEFINES            PK41-GE99.               CI0102
            14            PK41-GENAL  PICTURE  X(30)                    CI0102
                          OCCURS       002     TIMES.                   CI0102
            14            PK41-GESAD  PICTURE  X(30)                    CI0102
                          OCCURS       003     TIMES.                   CI0102
            13            PK41-GE03                                     CI0102
                          REDEFINES            PK41-GE99.               CI0102
            14            PK41-NCHKN  PICTURE  9(11).                   CI0102
            13            PK41-GE04                                     CI0102
                          REDEFINES            PK41-GE99.               CI0102
            14            PK41-CTIDAP PICTURE  9(3).                    CI0102
            14            PK41-PRCOD  PICTURE  9(5).                    CI0102
            14            PK41-TDELI  PICTURE  X(30).                   CI0102
            14            PK41-CINCD  PICTURE  9(02).                   CI0102
            12            PK41-GE20                                     CI0102
                          REDEFINES            PK41-GE98.               CI0102
            13            PK41-C299.                                    CI0102
            14            PK41-CTID.                                    CI0102
            15            PK41-CTIDA  PICTURE  9(3).                    CI0102
            15            PK41-CTIDN.                                   CI0102
            16            PK41-CTIDNP PICTURE  X(13).                   CI0102
            16            PK41-CTIDND PICTURE  9(11).                   CI0102
            13            PK41-DCACG9 PICTURE  9(8).                    CI0102
            13            PK41-NAASQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            PK41-CIRAP  PICTURE  XX.                      CI0102
            13            PK41-CTYPE  PICTURE  X.                       CI0102
            13            PK41-INACT  PICTURE  X.                       CI0102
            13            PK41-FILLER PICTURE  X(01).                   CI0102
            13            PK41-ITPAC  PICTURE  X.                       CI0102
            13            PK41-ITAXI  PICTURE  X.                       CI0102
            13            PK41-IOWNC  PICTURE  X.                       CI0102
            13            PK41-CDVCD  PICTURE  X(2).                    CI0102
            13            PK41-CTCUS  PICTURE  999.                     CI0102
            13            PK41-CPMTCB PICTURE  X(3).                    CI0102
            13            PK41-CASTC1 PICTURE  99.                      CI0102
            13            PK41-PRCOD1 PICTURE  9(5).                    CI0102
            13            PK41-CPRSC1 PICTURE  X(9).                    CI0102
            13            PK41-CPRTB  PICTURE  X.                       CI0102
            13            PK41-CBRKD  PICTURE  9(4).                    CI0102
            13            PK41-FILLER PICTURE  X(12).                   CI0102
            12            PK41-GE30                                     CI0102
                          REDEFINES            PK41-GE98.               CI0102
            13            PK41-CFIDC  PICTURE  X(5).                    CI0102
            13            PK41-CPHSE  PICTURE  9(2).                    CI0102
            13            PK41-FILLER PICTURE  X(05).                   CI0102
            13            PK41-IABIN  PICTURE  X.                       CI0102
            13            PK41-PDFND  PICTURE  S999V9(3)                CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-GE40                                     CI0102
                          REDEFINES            PK41-GE98.               CI0102
            13            PK41-CACCT  PICTURE  X.                       CI0102
            13            PK41-CPAYR  PICTURE  X(2).                    CI0102
            13            PK41-CDELI1 PICTURE  9(3).                    CI0102
            13            PK41-CATRN.                                   CI0102
            14            PK41-CATRF  PICTURE  9(3).                    CI0102
            14            PK41-CATRS  PICTURE  9(3).                    CI0102
            13            PK41-DEFFT  PICTURE  9(8).                    CI0102
            13            PK41-CTYPC  PICTURE  X.                       CI0102
            13            PK41-CIRAPA PICTURE  XX.                      CI0102
            13            PK41-FILLER PICTURE  X(09).                   CI0102
            13            PK41-GE49.                                    CI0102
            14            PK41-FILLER PICTURE  X(208).                  CI0102
            13            PK41-GE41                                     CI0102
                          REDEFINES            PK41-GE49.               CI0102
            14            PK41-NCHKN1 PICTURE  9(6).                    CI0102
            13            PK41-GE42                                     CI0102
                          REDEFINES            PK41-GE49.               CI0102
            14            PK41-CTID1.                                   CI0102
            15            PK41-CTIDA1 PICTURE  9(3).                    CI0102
            15            PK41-CTIDP1 PICTURE  X(13).                   CI0102
            15            PK41-CTIDN1 PICTURE  9(11).                   CI0102
            13            PK41-GE43                                     CI0102
                          REDEFINES            PK41-GE49.               CI0102
            14            PK41-GENAL2 PICTURE  X(30)                    CI0102
                          OCCURS       002     TIMES.                   CI0102
            14            PK41-GESAD2 PICTURE  X(30)                    CI0102
                          OCCURS       003     TIMES.                   CI0102
            13            PK41-GE44                                     CI0102
                          REDEFINES            PK41-GE49.               CI0102
            14            PK41-CTID01.                                  CI0102
            15            PK41-CTIDA6 PICTURE  9(3).                    CI0102
            15            PK41-NTIDP2 PICTURE  X(13).                   CI0102
            15            PK41-CTIDN2 PICTURE  9(11).                   CI0102
            14            PK41-GECKD2 PICTURE  9.                       CI0102
            14            PK41-PACCT  PICTURE  S999V99                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-PLOAN  PICTURE  S999V99                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-PADPT  PICTURE  S999V99                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            14            PK41-IPCTL  PICTURE  X.                       CI0102
            14            PK41-IPCTP  PICTURE  X.                       CI0102
            14            PK41-CEUNT  PICTURE  S9(5)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-GE31                                     CI0102
                          REDEFINES            PK41-GE98.               CI0102
            13            PK41-GCUSPZ PICTURE  X(12).                   CI0102
            11            PK41-GC12                                     CI0102
                          REDEFINES            PK41-GC06.               CI0102
            12            PK41-GC12K.                                   CI0102
            13            PK41-CIRAP  PICTURE  XX.                      CI0102
            12            PK41-AIRCT  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-FILLER PICTURE  X.                       CI0102
            11            PK41-GC04.                                    CI0102
            12            PK41-CLCUS  PICTURE  99.                      CI0102
            12            PK41-CCACT  PICTURE  99.                      CI0102
            12            PK41-AFEET  PICTURE  S9(5)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-ITERF  PICTURE  X.                       CI0102
            12            PK41-ATERF  PICTURE  S9(5)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-CLDOB  PICTURE  9(8).                    CI0102
            12            PK41-CPLTYP PICTURE  X(14).                   CI0102
            12            PK41-IACFPD PICTURE  X(1).                    CI0102
            12            PK41-FILLER PICTURE  X(14).                   CI0102
            11            PK41-GC21                                     CI0102
                          REDEFINES            PK41-GC04.               CI0102
            12            PK41-C299.                                    CI0102
            13            PK41-CTID.                                    CI0102
            14            PK41-CTIDA  PICTURE  9(3).                    CI0102
            14            PK41-CTIDN.                                   CI0102
            15            PK41-CTIDNP PICTURE  X(13).                   CI0102
            15            PK41-CTIDND PICTURE  9(11).                   CI0102
            12            PK41-DCACG9 PICTURE  9(8).                    CI0102
            12            PK41-NAASQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            PK41-FILLER PICTURE  X.                       CI0102
      *
      *-----------------------------------------------------------------
      *>>>>>> OUTPUT SEGMENT FOR MODULE CI0100.
      *-----------------------------------------------------------------
      *
      *!WF DSP=PK DSL=PJ SEL=41 FOR=I DES=1 LEV=1 PLT=PK
      *
      ******************************************************************ADUTAB
      **              TABLE TA5B ACCESS FIELDS                         *ADUTAB
      ******************************************************************ADUTAB
      **                                                                ADUTAB
       01  G-TA5B-LTH        PIC S9(4) COMP.                            ADUTAB
      **                                                                ADUTAB
      *!WF DSP=TA DSL=TA SEL=5B FOR=I DES=2 LEV=1 ORG=G                 ADUTAB
       01        G-TA5B.                                                CI0102
           04    G-TA5B-PARAM.                                          CI0102
             10  G-TA5B-LOZTR  PICTURE S9(4) COMPUTATIONAL              CI0102
                        VALUE      +154.                                CI0102
             10  G-TA5B-ADRCLE PICTURE S9(4) COMPUTATIONAL              CI0102
                        VALUE      +001.                                CI0102
             10  G-TA5B-LOCLE  PICTURE S9(4) COMPUTATIONAL              CI0102
                        VALUE      +017.                                CI0102
             10  G-TA5B-NUAPP  PICTURE 99                               CI0102
                        VALUE       0.                                  CI0102
             10  G-TA5B-NUTAB  PICTURE X(6)                             CI0102
                        VALUE 'TA005B'.                                 CI0102
             10  G-TA5B-TABFO  PICTURE XX                 VALUE SPACE.  CI0102
             10  G-TA5B-TABCR  PICTURE XX                 VALUE SPACE.  CI0102
             10  G-TA5B-DAHTA  PICTURE X(6)               VALUE SPACE.  CI0102
             10  G-TA5B-NUSSC  PICTURE X  VALUE   ' '.                  CI0102
             10  G-TA5B-NUSSY  PICTURE X                  VALUE SPACE.  CI0102
             10  G-TA5B-TRANID PICTURE X(4)               VALUE SPACE.  CI0102
             10  G-TA5B-FILSYS.                                         CI0102
             15  G-TA5B-USERC  PICTURE X(6)               VALUE SPACE.  CI0102
             15  FILLER        PICTURE X(24)              VALUE SPACE.  CI0102
           04             TA5B.                                         CI0102
            10            TA5B-GAPSC.                                   CI0102
            11            TA5B-CTIDA  PICTURE  9(3)                     CI0102
                          VALUE                ZERO.                    CI0102
            11            TA5B-PRCOD  PICTURE  9(5)                     CI0102
                          VALUE                ZERO.                    CI0102
            11            TA5B-PRSCD  PICTURE  X(9)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-PRCODX PICTURE  9(5)                     CI0102
                          VALUE                ZERO.                    CI0102
            10            TA5B-PRCSUB PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-PRCAUT PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-PRCBAS PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-PRCSTK PICTURE  XX                       CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-PRCPRE PICTURE  X(4)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IBDUP  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IUSPR  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-CVSYS  PICTURE  X(2)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IDTOD  PICTURE  X(1)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-GRSFC  PICTURE  99                       CI0102
                          VALUE                ZERO.                    CI0102
            10            TA5B-ZDA18  PICTURE  X(18)                    CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-CMPCTB PICTURE  X(4)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ITERM  PICTURE  X(1)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-AMFAC  PICTURE  S9(7)                    CI0102
                          VALUE                ZERO.                    CI0102
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-CPRBK  PICTURE  X(3)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-CFXDM  PICTURE  99                       CI0102
                          VALUE                ZERO.                    CI0102
            10            TA5B-NGLCS  PICTURE  X(5)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-NDFCS  PICTURE  X(5)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ZDA20  PICTURE  X(20)                    CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-CTNLI  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-CBANK  PICTURE  X(03)                    CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ISYPO  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ISYPP  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ICOPT  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IANPY  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IDSAR  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ICIPT  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IANDS  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IKPMA  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-INMWT  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IVANT  PICTURE  X(1)                     CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ISDAV  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-IUDAV  PICTURE  X                        CI0102
                          VALUE                SPACE.                   CI0102
            10            TA5B-ZDA15  PICTURE  X(15)                    CI0102
                          VALUE                SPACE.                   CI0102
      **                                                                ADUTAB
      *
      ******************************************************************
      ** WORKING STORAGE AREA FOR CL0102                               *
      ******************************************************************
      *
      *!WI
       01  7-HC14-CLID
                        PICTURE X(23).                                  CI0102
       01  WORK-IK.
           05  TA5B-IK        PIC X.
           05  7-WS-QTYUDT    PIC 9(6).
       01   DEBUT-WSS.                                                  CI0102
            05   FILLER PICTURE X(7) VALUE 'WORKING'.                   CI0102
            05   IK     PICTURE X.                                      CI0102
       01  CONSTANTES-PAC.                                              CI0102
           05  FILLER  PICTURE X(87)   VALUE                            CI0102
                     '6015 CAT09/08/14CI0102ADMIN   14:34:45CI0102P AMERCI0102
      -    '09/08/20143.5 V1023/06/201425/02/2014'.                     CI0102
       01  PAC-CONSTANTES REDEFINES CONSTANTES-PAC.                     CI0102
           05  NUGNA   PICTURE X(5).                                    CI0102
           05  APPLI   PICTURE X(3).                                    CI0102
           05  DATGN   PICTURE X(8).                                    CI0102
           05  PROGR   PICTURE X(6).                                    CI0102
           05  CODUTI  PICTURE X(8).                                    CI0102
           05  TIMGN   PICTURE X(8).                                    CI0102
           05  PROGE   PICTURE X(8).                                    CI0102
           05  COBASE  PICTURE X(4).                                    CI0102
           05  DATGNC  PICTURE X(10).                                   CI0102
           05  RELEAS  PICTURE X(7).                                    CI0102
           05  DATGE   PICTURE X(10).                                   CI0102
           05  DATSQ   PICTURE X(10).                                   CI0102
       01  DATCE.                                                       CI0102
         05  CENTUR   PICTURE XX   VALUE '20'.                          CI0102
         05  DATOR.                                                     CI0102
           10  DATOA  PICTURE XX.                                       CI0102
           10  DATOM  PICTURE XX.                                       CI0102
           10  DATOJ  PICTURE XX.                                       CI0102
       01   VARIABLES-CONDITIONNELLES.                                  CI0102
            05                  FT      PICTURE X VALUE '0'.            CI0102
       01   INDICES  COMPUTATIONAL  SYNC.                               CI0102
            05          TALLI   PICTURE S9(4) VALUE  ZERO.              CI0102
            05           IMS03L PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03R PICTURE S9(4) VALUE  ZERO.              ADU070
            05           IMS03M PICTURE S9(4) VALUE +0512.              ADU070
            05           IMX11L PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11R PICTURE S9(4) VALUE  ZERO.              ADU102
            05           IMX11M PICTURE S9(4) VALUE +0025.              ADU102
       01   COMPTEURS-FICHIERS       COMPUTATIONAL-3.                   CI0102
            05       5-CT00-CPTENR PICTURE S9(9) VALUE ZERO.            CI0102
       01               S-CT01-SSA.                                     CI0102
            10         S1-CT01-SEGNAM PICTURE X(8)                      CI0102
                                      VALUE 'CT01    '.                 CI0102
            10         S1-CT01-CCOM   PICTURE X VALUE '*'.              CI0102
            10          S-CT01-CCOD   PICTURE X(5)                      CI0102
                                      VALUE '-----'.                    CI0102
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0102
       01            S-CTU01-SSA.                                       CI0102
            10      S1-CTU01-SEGNAM PICTURE X(8)                        CI0102
                                      VALUE 'CT01    '.                 CI0102
            10      S1-CTU01-CCOM   PICTURE X VALUE '*'.                CI0102
            10       S-CTU01-CCOD   PICTURE X(5)                        CI0102
                                      VALUE '-----'.                    CI0102
            10      S1-CTU01-FLDNAM PICTURE X(9)                        CI0102
                                      VALUE '(CT01K'.                   CI0102
            10       S-CTU01-OPER  PICTURE XX VALUE ' ='.               CI0102
            10       S-CTU01-CT01K.                                     CI0102
            11       S-CTU01-C299.                                      CI0102
            12       S-CTU01-CTID.                                      CI0102
            13       S-CTU01-CTIDA    PICTURE  9(3).                    CI0102
            13       S-CTU01-CTIDN.                                     CI0102
            14       S-CTU01-CTIDNP   PICTURE  X(13).                   CI0102
            14       S-CTU01-CTIDND   PICTURE  9(11).                   CI0102
            10  FILLER   PICTURE X    VALUE ')'.                        CI0102
       01               S-CT13-SSA.                                     CI0102
            10         S1-CT13-SEGNAM PICTURE X(8)                      CI0102
                                      VALUE 'CT13    '.                 CI0102
            10         S1-CT13-CCOM   PICTURE X VALUE '*'.              CI0102
            10          S-CT13-CCOD   PICTURE X(5)                      CI0102
                                      VALUE '-----'.                    CI0102
            10  FILLER   PICTURE X    VALUE SPACE.                      CI0102
       01            S-CTU13-SSA.                                       CI0102
            10      S1-CTU13-SEGNAM PICTURE X(8)                        CI0102
                                      VALUE 'CT13    '.                 CI0102
            10      S1-CTU13-CCOM   PICTURE X VALUE '*'.                CI0102
            10       S-CTU13-CCOD   PICTURE X(5)                        CI0102
                                      VALUE '-----'.                    CI0102
            10      S1-CTU13-FLDNAM PICTURE X(9)                        CI0102
                                      VALUE '(CT13K'.                   CI0102
            10       S-CTU13-OPER  PICTURE XX VALUE ' ='.               CI0102
            10       S-CTU13-CT13K.                                     CI0102
            11       S-CTU13-GEHSD    PICTURE  9(8).                    CI0102
            11       S-CTU13-GEHCD    PICTURE  9(3).                    CI0102
            11       S-CTU13-GEHCSE   PICTURE  X(12).                   CI0102
            11       S-CTU13-GEHCSU   PICTURE  9(5).                    CI0102
            10  FILLER   PICTURE X    VALUE ')'.                        CI0102
       01   ZONES-UTILISATEUR PICTURE X.                                CI0102
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
      ** PCB POINTER FOR CL1P                                           ADU015
            05 PCB-CL1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR CT1P                                           ADU015
            05 PCB-CT1P-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR ACAP                                           ADU015
            05 PCB-ACAP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SBUP                                           ADU015
            05 PCB-SBUP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SCOP                                           ADU015
            05 PCB-SCOP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR SSPP                                           ADU015
            05 PCB-SSPP-PTR1        POINTER.                            ADU015
      ** PCB POINTER FOR TR1P                                           ADU015
            05 PCB-TR1P-PTR1        POINTER.                            ADU015
      *** PCB MASK FOR CL1P                                             ADU015
      *!WF DSP=PA DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PA00.                                         CI0102
          05              PA00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00106).                  CI0102
       01                 PA06  REDEFINES      PA00.                    CI0102
            10            PA06-XDBPCB.                                  CI0102
            11            PA06-XDBDNM PICTURE  X(08).                   CI0102
            11            PA06-XSEGLV PICTURE  X(02).                   CI0102
            11            PA06-XRC    PICTURE  X(02).                   CI0102
            11            PA06-XPROPT PICTURE  X(04).                   CI0102
            11            PA06-FILLER PICTURE  S9(5)                    CI0102
                          BINARY.                                       CI0102
            11            PA06-XSEGNM PICTURE  X(08).                   CI0102
            11            PA06-XKEYLN PICTURE  S9(05)                   CI0102
                          BINARY.                                       CI0102
            11            PA06-XSEGNB PICTURE  9(05)                    CI0102
                          BINARY.                                       CI0102
            11            PA06-XCOKEY PICTURE  X(70).                   CI0102
      *** PCB MASK FOR CT1P                                             ADU015
      *!WF DSP=PB DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PB00.                                         CI0102
          05              PB00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00106).                  CI0102
       01                 PB06  REDEFINES      PB00.                    CI0102
            10            PB06-XDBPCB.                                  CI0102
            11            PB06-XDBDNM PICTURE  X(08).                   CI0102
            11            PB06-XSEGLV PICTURE  X(02).                   CI0102
            11            PB06-XRC    PICTURE  X(02).                   CI0102
            11            PB06-XPROPT PICTURE  X(04).                   CI0102
            11            PB06-FILLER PICTURE  S9(5)                    CI0102
                          BINARY.                                       CI0102
            11            PB06-XSEGNM PICTURE  X(08).                   CI0102
            11            PB06-XKEYLN PICTURE  S9(05)                   CI0102
                          BINARY.                                       CI0102
            11            PB06-XSEGNB PICTURE  9(05)                    CI0102
                          BINARY.                                       CI0102
            11            PB06-XCOKEY PICTURE  X(70).                   CI0102
      *** PCB MASK FOR ACAP                                             ADU015
      *!WF DSP=PC DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PC00.                                         CI0102
          05              PC00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00106).                  CI0102
       01                 PC06  REDEFINES      PC00.                    CI0102
            10            PC06-XDBPCB.                                  CI0102
            11            PC06-XDBDNM PICTURE  X(08).                   CI0102
            11            PC06-XSEGLV PICTURE  X(02).                   CI0102
            11            PC06-XRC    PICTURE  X(02).                   CI0102
            11            PC06-XPROPT PICTURE  X(04).                   CI0102
            11            PC06-FILLER PICTURE  S9(5)                    CI0102
                          BINARY.                                       CI0102
            11            PC06-XSEGNM PICTURE  X(08).                   CI0102
            11            PC06-XKEYLN PICTURE  S9(05)                   CI0102
                          BINARY.                                       CI0102
            11            PC06-XSEGNB PICTURE  9(05)                    CI0102
                          BINARY.                                       CI0102
            11            PC06-XCOKEY PICTURE  X(70).                   CI0102
      *** PCB MASK FOR SBUP                                             ADU015
      *!WF DSP=PE DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PE00.                                         CI0102
          05              PE00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00106).                  CI0102
       01                 PE06  REDEFINES      PE00.                    CI0102
            10            PE06-XDBPCB.                                  CI0102
            11            PE06-XDBDNM PICTURE  X(08).                   CI0102
            11            PE06-XSEGLV PICTURE  X(02).                   CI0102
            11            PE06-XRC    PICTURE  X(02).                   CI0102
            11            PE06-XPROPT PICTURE  X(04).                   CI0102
            11            PE06-FILLER PICTURE  S9(5)                    CI0102
                          BINARY.                                       CI0102
            11            PE06-XSEGNM PICTURE  X(08).                   CI0102
            11            PE06-XKEYLN PICTURE  S9(05)                   CI0102
                          BINARY.                                       CI0102
            11            PE06-XSEGNB PICTURE  9(05)                    CI0102
                          BINARY.                                       CI0102
            11            PE06-XCOKEY PICTURE  X(70).                   CI0102
      *** PCB MASK FOR SCOP                                             ADU015
      *!WF DSP=PF DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PF00.                                         CI0102
          05              PF00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00106).                  CI0102
       01                 PF06  REDEFINES      PF00.                    CI0102
            10            PF06-XDBPCB.                                  CI0102
            11            PF06-XDBDNM PICTURE  X(08).                   CI0102
            11            PF06-XSEGLV PICTURE  X(02).                   CI0102
            11            PF06-XRC    PICTURE  X(02).                   CI0102
            11            PF06-XPROPT PICTURE  X(04).                   CI0102
            11            PF06-FILLER PICTURE  S9(5)                    CI0102
                          BINARY.                                       CI0102
            11            PF06-XSEGNM PICTURE  X(08).                   CI0102
            11            PF06-XKEYLN PICTURE  S9(05)                   CI0102
                          BINARY.                                       CI0102
            11            PF06-XSEGNB PICTURE  9(05)                    CI0102
                          BINARY.                                       CI0102
            11            PF06-XCOKEY PICTURE  X(70).                   CI0102
      *** PCB MASK FOR SSPP                                             ADU015
      *!WF DSP=PH DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PH00.                                         CI0102
          05              PH00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00106).                  CI0102
       01                 PH06  REDEFINES      PH00.                    CI0102
            10            PH06-XDBPCB.                                  CI0102
            11            PH06-XDBDNM PICTURE  X(08).                   CI0102
            11            PH06-XSEGLV PICTURE  X(02).                   CI0102
            11            PH06-XRC    PICTURE  X(02).                   CI0102
            11            PH06-XPROPT PICTURE  X(04).                   CI0102
            11            PH06-FILLER PICTURE  S9(5)                    CI0102
                          BINARY.                                       CI0102
            11            PH06-XSEGNM PICTURE  X(08).                   CI0102
            11            PH06-XKEYLN PICTURE  S9(05)                   CI0102
                          BINARY.                                       CI0102
            11            PH06-XSEGNB PICTURE  9(05)                    CI0102
                          BINARY.                                       CI0102
            11            PH06-XCOKEY PICTURE  X(70).                   CI0102
      *** PCB MASK FOR TR1P                                             ADU015
      *!WF DSP=PI DSL=XW SEL=06 FOR=I LEV=1                             ADU015
       01                 PI00.                                         CI0102
          05              PI00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00106).                  CI0102
       01                 PI06  REDEFINES      PI00.                    CI0102
            10            PI06-XDBPCB.                                  CI0102
            11            PI06-XDBDNM PICTURE  X(08).                   CI0102
            11            PI06-XSEGLV PICTURE  X(02).                   CI0102
            11            PI06-XRC    PICTURE  X(02).                   CI0102
            11            PI06-XPROPT PICTURE  X(04).                   CI0102
            11            PI06-FILLER PICTURE  S9(5)                    CI0102
                          BINARY.                                       CI0102
            11            PI06-XSEGNM PICTURE  X(08).                   CI0102
            11            PI06-XKEYLN PICTURE  S9(05)                   CI0102
                          BINARY.                                       CI0102
            11            PI06-XSEGNB PICTURE  9(05)                    CI0102
                          BINARY.                                       CI0102
            11            PI06-XCOKEY PICTURE  X(70).                   CI0102

      *PASS AREA TO/FROM CL0102
      *!WF DSP=PJ DSL=PJ SEL=50 FOR=I DES=1 LEV=1 PLT=10
       01                 PJ50.                                         CI0102
            10            PJ50-MAPPN  PICTURE  X(10).                   CI0102
            10            PJ50-CHCR   PICTURE  99.                      CI0102
            10            PJ50-CACTS  PICTURE  X.                       CI0102
            10            PJ50-C299.                                    CI0102
            11            PJ50-CTID.                                    CI0102
            12            PJ50-CTIDA  PICTURE  9(3).                    CI0102
            12            PJ50-CTIDN.                                   CI0102
            13            PJ50-CTIDNP PICTURE  X(13).                   CI0102
            13            PJ50-CTIDND PICTURE  9(11).                   CI0102
            10            PJ50-DCACG9 PICTURE  9(8).                    CI0102
            10            PJ50-NAASQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            PJ50-NPISQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            PJ50-FILLER PICTURE  X(50).                   CI0102
            10            PJ50-AWDRTP PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            PJ50-APPAYA PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            PJ50-APPAYN PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102


      *ACTIVITY DB SEGMENTS PASSED BACK FROM CL0102
      *!WF DSP=GC DSL=GC SEL=01030406 FOR=I DES=1 LEV=1
      * PLT=30
       01                 GC01.                                         CI0102
            10            GC01-GC01K.                                   CI0102
            11            GC01-C299.                                    CI0102
            12            GC01-CTID.                                    CI0102
            13            GC01-CTIDA  PICTURE  9(3).                    CI0102
            13            GC01-CTIDN.                                   CI0102
            14            GC01-CTIDNP PICTURE  X(13).                   CI0102
            14            GC01-CTIDND PICTURE  9(11).                   CI0102
            10            GC01-DCAG9L PICTURE  9(8).                    CI0102
            10            GC01-NAASQL PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC01-ICUST  PICTURE  X.                       CI0102
            10            GC01-NSEQ4B PICTURE  9(8)                     CI0102
                          BINARY.                                       CI0102
            10            GC01-PRCOD  PICTURE  9(5).                    CI0102
            10            GC01-PRSCD  PICTURE  X(9).                    CI0102
            10            GC01-FILLER PICTURE  X(8).                    CI0102
       01                 GC03.                                         CI0102
            10            GC03-GELL   PICTURE  9(4)                     CI0102
                          BINARY.                                       CI0102
            10            GC03-GD00.                                    CI0102
            11            GC03-GC03K.                                   CI0102
            12            GC03-DCACG9 PICTURE  9(8).                    CI0102
            12            GC03-NAASQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CAATY  PICTURE  9(3).                    CI0102
            11            GC03-CVSYS  PICTURE  X(2).                    CI0102
            11            GC03-CACTO  PICTURE  9(3).                    CI0102
            11            GC03-CATRN.                                   CI0102
            12            GC03-CATRF  PICTURE  9(3).                    CI0102
            12            GC03-CATRS  PICTURE  9(3).                    CI0102
            11            GC03-CASTC  PICTURE  99.                      CI0102
            11            GC03-IPULL  PICTURE  X.                       CI0102
            11            GC03-GEAUN  PICTURE  9(5).                    CI0102
            11            GC03-GEOPD2 PICTURE  X(8).                    CI0102
            11            GC03-NBTCH  PICTURE  9(4).                    CI0102
            11            GC03-DEFFT  PICTURE  9(8).                    CI0102
            11            GC03-NSUNT  PICTURE  9(4).                    CI0102
            11            GC03-ITRAN  PICTURE  X.                       CI0102
            11            GC03-DLAUP1 PICTURE  9(8).                    CI0102
            11            GC03-ADRET  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-TTRMS  PICTURE  X(12).                   CI0102
            11            GC03-IDELT  PICTURE  X.                       CI0102
            11            GC03-GEOPDM PICTURE  X(8).                    CI0102
            11            GC03-FILLER PICTURE  X(07).                   CI0102
            10            GC03-GD09.                                    CI0102
            11            GC03-FILLER PICTURE  X(70).                   CI0102
            10            GC03-GD01                                     CI0102
                          REDEFINES            GC03-GD09.               CI0102
            11            GC03-ADBRQ  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CTRTP  PICTURE  X(2).                    CI0102
            11            GC03-CPORT  PICTURE  X.                       CI0102
            11            GC03-CSCRNU PICTURE  X(4).                    CI0102
            11            GC03-DLAUP  PICTURE  9(8).                    CI0102
            11            GC03-CTWHAT PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-PWHLD  PICTURE  S999V9(5)                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-IWTHH  PICTURE  X.                       CI0102
            11            GC03-NDRFT  PICTURE  9(5).                    CI0102
            11            GC03-IDPAP  PICTURE  X.                       CI0102
            11            GC03-GETIM  PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QNACT  PICTURE  9(3).                    CI0102
            11            GC03-AEDRQ  PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-IPLIN  PICTURE  X.                       CI0102
            11            GC03-CLIDNB PICTURE  9(8).                    CI0102
            11            GC03-CSLCT  PICTURE  X.                       CI0102
            11            GC03-ITELE  PICTURE  X.                       CI0102
            11            GC03-FILLER PICTURE  X(06).                   CI0102
            10            GC03-GD02                                     CI0102
                          REDEFINES            GC03-GD09.               CI0102
            11            GC03-CSYST  PICTURE  99.                      CI0102
            11            GC03-FILLER PICTURE  X.                       CI0102
            11            GC03-ACASH  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-DTRAC  PICTURE  9(8).                    CI0102
            11            GC03-CTRSO  PICTURE  9(02).                   CI0102
            11            GC03-NTRCE  PICTURE  9(06).                   CI0102
            11            GC03-GECKD1 PICTURE  9.                       CI0102
            11            GC03-CCOLL  PICTURE  X(3).                    CI0102
            11            GC03-CLTDP  PICTURE  X(3).                    CI0102
            11            GC03-PSLLD  PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ISLOR  PICTURE  X.                       CI0102
            11            GC03-ITPAC  PICTURE  X.                       CI0102
            11            GC03-CPMTCA PICTURE  XXX.                     CI0102
            11            GC03-CSERV  PICTURE  X(3).                    CI0102
            11            GC03-ACOMO  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-IPLIN1 PICTURE  X.                       CI0102
            11            GC03-INQEX  PICTURE  X.                       CI0102
            11            GC03-CTKRAA PICTURE  X(12).                   CI0102
            11            GC03-CCSMQ  PICTURE  X.                       CI0102
            11            GC03-IVAEX1 PICTURE  X.                       CI0102
            11            GC03-IHPMT  PICTURE  X(1).                    CI0102
            11            GC03-GETIM3 PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC03-GD03                                     CI0102
                          REDEFINES            GC03-GD09.               CI0102
            11            GC03-CATRNC PICTURE  9(6).                    CI0102
            11            GC03-APRNT1 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QSHOWT PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACINVT PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACOMO7 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QSHOMW PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ATAXT3 PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CTSTR  PICTURE  9(2).                    CI0102
            11            GC03-ICIRA  PICTURE  X.                       CI0102
            11            GC03-GETIM2 PICTURE  S9(7)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CPMTCX PICTURE  XX.                      CI0102
            11            GC03-FILLER PICTURE  X(16).                   CI0102
            10            GC03-GD99.                                    CI0102
            11            GC03-FILLER PICTURE  X(248).                  CI0102
            10            GC03-GD10                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-MROTC  PICTURE  X(7).                    CI0102
            11            GC03-CEDSC  PICTURE  9(1).                    CI0102
            11            GC03-ILPOI  PICTURE  X(1).                    CI0102
            11            GC03-AWRCH  PICTURE  S9(3)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CHCOC1 PICTURE  9(2).                    CI0102
            11            GC03-CHCOC2 PICTURE  9(2).                    CI0102
            11            GC03-CHCOC3 PICTURE  9(2).                    CI0102
            11            GC03-CHCOC4 PICTURE  9(2).                    CI0102
            11            GC03-CMCOC1 PICTURE  9(3).                    CI0102
            11            GC03-CMCOC2 PICTURE  9(3).                    CI0102
            11            GC03-CMCOC3 PICTURE  9(3).                    CI0102
            11            GC03-GD11.                                    CI0102
            12            GC03-FILLER PICTURE  X(219).                  CI0102
            11            GC03-GD12                                     CI0102
                          REDEFINES            GC03-GD11.               CI0102
            12            GC03-CELLO  PICTURE  9(1).                    CI0102
            12            GC03-CECLO  PICTURE  9(1).                    CI0102
            12            GC03-AEXML  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CEPI   PICTURE  X(1).                    CI0102
            12            GC03-CEXTY  PICTURE  X.                       CI0102
            12            GC03-CROPC  PICTURE  9(1).                    CI0102
            12            GC03-CPUTY  PICTURE  9(1).                    CI0102
            12            GC03-IMCII  PICTURE  X(1).                    CI0102
            12            GC03-GEMISC                                   CI0102
                          OCCURS       010     TIMES.                   CI0102
            13            GC03-AMGLA  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            13            GC03-CMGLC  PICTURE  9(1).                    CI0102
            13            GC03-NMGLN  PICTURE  9(4).                    CI0102
            12            GC03-ACTRN  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-IWRBK  PICTURE  X.                       CI0102
            12            GC03-IFEDX  PICTURE  X.                       CI0102
            12            GC03-ICNTR  PICTURE  X.                       CI0102
            12            GC03-IOCKH  PICTURE  X.                       CI0102
            12            GC03-ICRCK  PICTURE  X.                       CI0102
            12            GC03-NHMPN  PICTURE  S9(10)                   CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ITELR1 PICTURE  X.                       CI0102
            11            GC03-GD13                                     CI0102
                          REDEFINES            GC03-GD11.               CI0102
            12            GC03-DREDO  PICTURE  9(8).                    CI0102
            12            GC03-CATRNR PICTURE  9(6).                    CI0102
            12            GC03-CEVN   PICTURE  9(9).                    CI0102
            12            GC03-ISUSP  PICTURE  X(1).                    CI0102
            11            GC03-GD15                                     CI0102
                          REDEFINES            GC03-GD11.               CI0102
            12            GC03-CPUTZ  PICTURE  9(1).                    CI0102
            12            GC03-CETLB  PICTURE  9(3).                    CI0102
            12            GC03-QTRMC  PICTURE  9(3).                    CI0102
            12            GC03-DEFFTE PICTURE  9(8).                    CI0102
            12            GC03-DEFFTF PICTURE  9(8).                    CI0102
            12            GC03-DEFFTG PICTURE  9(8).                    CI0102
            12            GC03-XZ1A   PICTURE  X.                       CI0102
            12            GC03-XZ1B   PICTURE  X.                       CI0102
            12            GC03-XZ1C   PICTURE  X.                       CI0102
            12            GC03-XZ1D   PICTURE  X.                       CI0102
            12            GC03-XZ1E   PICTURE  X.                       CI0102
            12            GC03-XZ1F   PICTURE  X.                       CI0102
            12            GC03-XZ1G   PICTURE  X.                       CI0102
            12            GC03-XZ1H   PICTURE  X.                       CI0102
            12            GC03-XZ1I   PICTURE  X.                       CI0102
            12            GC03-DEFFTH PICTURE  9(8).                    CI0102
            11            GC03-GD19                                     CI0102
                          REDEFINES            GC03-GD11.               CI0102
            12            GC03-GD11.                                    CI0102
            13            GC03-FILLER PICTURE  X(219).                  CI0102
            10            GC03-GD20                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-ADDACT PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ISIGV  PICTURE  X.                       CI0102
            11            GC03-IALLF  PICTURE  X.                       CI0102
            11            GC03-QSHOWQ PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CCDSCW PICTURE  9(2).                    CI0102
            11            GC03-IDWRL  PICTURE  X.                       CI0102
            11            GC03-ITELR  PICTURE  X.                       CI0102
            11            GC03-IABIN  PICTURE  X.                       CI0102
            11            GC03-PACT1  PICTURE  S999V999                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-IBFAF  PICTURE  X.                       CI0102
            11            GC03-IFRSA  PICTURE  X.                       CI0102
            11            GC03-ICRCAN PICTURE  X.                       CI0102
            11            GC03-ACACTV PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-AGFND  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QCSHOW PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QCSHIS PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-NDTRC  PICTURE  9(8).                    CI0102
            11            GC03-CAERU  PICTURE  X(4).                    CI0102
            11            GC03-IFDGO  PICTURE  X.                       CI0102
            11            GC03-PSLLD2 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ISLOR2 PICTURE  X.                       CI0102
            11            GC03-QSFIO  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QSFID  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CGDIN  PICTURE  X.                       CI0102
            11            GC03-DGDIN  PICTURE  9(8).                    CI0102
            10            GC03-GD30                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-ISKED  PICTURE  X.                       CI0102
            11            GC03-CENXC  PICTURE  9(2).                    CI0102
            11            GC03-GD31.                                    CI0102
            12            GC03-FILLER PICTURE  X(245).                  CI0102
            11            GC03-GD32                                     CI0102
                          REDEFINES            GC03-GD31.               CI0102
            12            GC03-IABIN1 PICTURE  X.                       CI0102
            12            GC03-CLDOD  PICTURE  9(8).                    CI0102
            12            GC03-NCLAM  PICTURE  9(5)                     CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ISURR  PICTURE  X.                       CI0102
            12            GC03-GEHCD  PICTURE  9(3).                    CI0102
            12            GC03-CRATC  PICTURE  9(4).                    CI0102
            12            GC03-AMAXD  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ASCHGA PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-APYOM  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-IWTHH1 PICTURE  X.                       CI0102
            12            GC03-CPAYCL PICTURE  X(2).                    CI0102
            12            GC03-CTSAO  PICTURE  X.                       CI0102
            12            GC03-NCONF  PICTURE  9(08).                   CI0102
            12            GC03-CLID   PICTURE  X(23).                   CI0102
            12            GC03-CARTY  PICTURE  99.                      CI0102
            12            GC03-NARRS  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CARTZ  PICTURE  99.                      CI0102
            12            GC03-NAPDS  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CPMTO  PICTURE  X.                       CI0102
            12            GC03-DNPMT  PICTURE  9(8).                    CI0102
            12            GC03-IPCTV  PICTURE  X.                       CI0102
            12            GC03-IMECH  PICTURE  X(01).                   CI0102
            12            GC03-IMVAO  PICTURE  X(1).                    CI0102
            12            GC03-AMVA1  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CACTS  PICTURE  X.                       CI0102
            12            GC03-CTSPP  PICTURE  X(1).                    CI0102
            12            GC03-CACT4  PICTURE  X(2).                    CI0102
            12            GC03-IVAEX  PICTURE  X.                       CI0102
            12            GC03-DFPMT  PICTURE  9(8).                    CI0102
            12            GC03-IDEMD  PICTURE  X.                       CI0102
            12            GC03-IOFST  PICTURE  X.                       CI0102
            12            GC03-AMXLB  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ACULB  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-DEIRNB PICTURE  9(8).                    CI0102
            12            GC03-DEFFE  PICTURE  9(8).                    CI0102
            12            GC03-DEFFR  PICTURE  9(8).                    CI0102
            12            GC03-ISPUP  PICTURE  X.                       CI0102
            12            GC03-CPNCG  PICTURE  X.                       CI0102
            12            GC03-IEXPU  PICTURE  X.                       CI0102
            12            GC03-IPPCF  PICTURE  X.                       CI0102
            12            GC03-NAAPT  PICTURE  9(2).                    CI0102
            12            GC03-PWHLDS PICTURE  S999V9(5)                CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ISWHO  PICTURE  X(1).                    CI0102
            11            GC03-GD33                                     CI0102
                          REDEFINES            GC03-GD31.               CI0102
            12            GC03-CPAYC  PICTURE  X(2).                    CI0102
            12            GC03-ADBRQX PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ADBRQV PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-APTXR  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CTRTPE PICTURE  X(2).                    CI0102
            12            GC03-NCLAMI PICTURE  S9(9)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CLIDO8 PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CLIDN  PICTURE  X(20).                   CI0102
            12            GC03-DSET01 PICTURE  S9(8)                    CI0102
                          BINARY.                                       CI0102
            12            GC03-CTSET1 PICTURE  S9(6)                    CI0102
                          BINARY.                                       CI0102
            12            GC03-DSET02 PICTURE  S9(8)                    CI0102
                          BINARY.                                       CI0102
            12            GC03-CTSET2 PICTURE  S9(6)                    CI0102
                          BINARY.                                       CI0102
            11            GC03-GD34                                     CI0102
                          REDEFINES            GC03-GD31.               CI0102
            12            GC03-QNOFM  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-CLTRM  PICTURE  99.                      CI0102
            12            GC03-AMXLN  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ALADJ  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ACHK   PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-APRMO  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-IMECH1 PICTURE  X(01).                   CI0102
            12            GC03-CACT41 PICTURE  X(2).                    CI0102
            12            GC03-ACDSCC PICTURE  S9(05)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-ACDSCD PICTURE  S9(05)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-GD39                                     CI0102
                          REDEFINES            GC03-GD31.               CI0102
            12            GC03-GD31.                                    CI0102
            13            GC03-FILLER PICTURE  X(245).                  CI0102
            10            GC03-GD40                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-NTR    PICTURE  9(8).                    CI0102
            11            GC03-NPBNC  PICTURE  X(24).                   CI0102
            11            GC03-CRREV  PICTURE  X(3).                    CI0102
            11            GC03-CSUSL  PICTURE  S9.                      CI0102
            11            GC03-NMGLN1 PICTURE  9(4).                    CI0102
            11            GC03-DCAC92 PICTURE  9(8).                    CI0102
            11            GC03-NAASQ3 PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-GD49.                                    CI0102
            12            GC03-FILLER PICTURE  X(198).                  CI0102
            11            GC03-GD41                                     CI0102
                          REDEFINES            GC03-GD49.               CI0102
            12            GC03-CRREF  PICTURE  9(2).                    CI0102
            12            GC03-CORIR  PICTURE  X(02).                   CI0102
            12            GC03-CIPDB  PICTURE  X(03).                   CI0102
            12            GC03-CPAYH  PICTURE  X(02).                   CI0102
            12            GC03-NAMEX  PICTURE  9(15)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC03-DCHAE  PICTURE  9(4).                    CI0102
            12            GC03-DRQST  PICTURE  S9(8)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-GD42                                     CI0102
                          REDEFINES            GC03-GD49.               CI0102
            12            GC03-CPMTCB PICTURE  X(3).                    CI0102
            10            GC03-GD50                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-ALOAD  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-PSLLD4 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CSUSL1 PICTURE  S9.                      CI0102
            11            GC03-CRREV1 PICTURE  X(3).                    CI0102
            11            GC03-ADDAC  PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-DL13.                                    CI0102
            12            GC03-GEYR   PICTURE  9(4).                    CI0102
            12            GC03-GEMTH  PICTURE  99.                      CI0102
            12            GC03-NDAY   PICTURE  99.                      CI0102
            11            GC03-NSEQ3P PICTURE  S9(5)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-XZ6A   PICTURE  X(6).                    CI0102
            11            GC03-XZ7    PICTURE  X(7).                    CI0102
            11            GC03-XZ6B   PICTURE  X(6).                    CI0102
            11            GC03-XZ6    PICTURE  X(6).                    CI0102
            11            GC03-XZ6C   PICTURE  X(6).                    CI0102
            11            GC03-XZ20   PICTURE  X(20).                   CI0102
            11            GC03-CATRN1 PICTURE  9(6).                    CI0102
            11            GC03-ADDAC2 PICTURE  S9(7)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ATAXT2 PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACOMOT PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-XZ5    PICTURE  X(5).                    CI0102
            11            GC03-IREVD  PICTURE  X(1).                    CI0102
            11            GC03-ISUSP1 PICTURE  X(1).                    CI0102
            11            GC03-XZ6D   PICTURE  X(6).                    CI0102
            11            GC03-XZ13   PICTURE  X(13).                   CI0102
            11            GC03-CWHTP2 PICTURE  X(3).                    CI0102
            11            GC03-CWHTP3 PICTURE  X(3).                    CI0102
            11            GC03-DTREN  PICTURE  9(8).                    CI0102
            11            GC03-NAASQ1 PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC03-GD51                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-ADOMOT PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACGLT  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACGST  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CTXMT  PICTURE  9(2).                    CI0102
            11            GC03-ALOAD3 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-FILLER PICTURE  X(31).                   CI0102
            10            GC03-GD52                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-DEFFT5 PICTURE  9(8).                    CI0102
            11            GC03-PSLLD5 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CSUSL2 PICTURE  S9.                      CI0102
            11            GC03-ALOAD2 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-DL22.                                    CI0102
            12            GC03-NYEAR1 PICTURE  9(4).                    CI0102
            12            GC03-GEMTHA PICTURE  99.                      CI0102
            12            GC03-NDAY01 PICTURE  99.                      CI0102
            11            GC03-NSEQ3R PICTURE  S9(5)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CWHTP  PICTURE  X(3).                    CI0102
            11            GC03-CWHFR  PICTURE  X(3).                    CI0102
            11            GC03-CATRN7 PICTURE  9(6).                    CI0102
            11            GC03-ATAXT5 PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QSHOT  PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACINT3 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CWHTP1 PICTURE  X(3).                    CI0102
            11            GC03-CWHFR1 PICTURE  X(3).                    CI0102
            11            GC03-ACOMO5 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QSHOMU PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACASH1 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-FILLER PICTURE  X(04).                   CI0102
            11            GC03-CATRN8 PICTURE  9(6).                    CI0102
            11            GC03-ALOAD1 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-PSLLD1 PICTURE  S99V999                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-QSHOT1 PICTURE  S9(10)V999               CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ACINT4 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CSUSL4 PICTURE  S9.                      CI0102
            11            GC03-ACOMO4 PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC03-GD60                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-GEOPDD PICTURE  X(8)                     CI0102
                          OCCURS       005     TIMES.                   CI0102
            11            GC03-DLAUP3 PICTURE  9(8)                     CI0102
                          OCCURS       005     TIMES.                   CI0102
            11            GC03-GEOPDB PICTURE  X(8).                    CI0102
            11            GC03-DLAUP4 PICTURE  9(8).                    CI0102
            11            GC03-ITELR2 PICTURE  X.                       CI0102
            11            GC03-IPMTA  PICTURE  X.                       CI0102
            11            GC03-CCSMG  PICTURE  X.                       CI0102
            11            GC03-CPLEC  PICTURE  XX.                      CI0102
            11            GC03-CORTYA PICTURE  X(3).                    CI0102
            11            GC03-CACTBC PICTURE  X(1).                    CI0102
            11            GC03-CGSPIA PICTURE  X.                       CI0102
            11            GC03-IPTRDA PICTURE  X(01).                   CI0102
            11            GC03-GCUSPY PICTURE  X(12).                   CI0102
            11            GC03-CPALLA PICTURE  X(1).                    CI0102
            11            GC03-QSHO5A PICTURE  S9(9)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-IFRSAB PICTURE  X.                       CI0102
            11            GC03-DELOI  PICTURE  9(8).                    CI0102
            11            GC03-IAROAA PICTURE  X.                       CI0102
            11            GC03-ACINVR PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-ILTINA PICTURE  X.                       CI0102
            11            GC03-ALOIDA PICTURE  S9(11)V99                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC03-CFUNTA PICTURE  X(2).                    CI0102
            11            GC03-CLGND  PICTURE  X.                       CI0102
            11            GC03-CPH3U  PICTURE  X.                       CI0102
            11            GC03-GESTD  PICTURE  9(8).                    CI0102
            11            GC03-GEEND  PICTURE  9(8).                    CI0102
            11            GC03-CPMTF  PICTURE  99.                      CI0102
            11            GC03-CNAVR  PICTURE  X(1).                    CI0102
            10            GC03-GD70                                     CI0102
                          REDEFINES            GC03-GD99.               CI0102
            11            GC03-CMEMO  PICTURE  X(2).                    CI0102
            11            GC03-ALPLDT PICTURE  9(8).                    CI0102
            11            GC03-CTLPD  PICTURE  9(8).                    CI0102
            11            GC03-CPAYCM PICTURE  X(2).                    CI0102
       01                 GC04.                                         CI0102
            10            GC04-CLCUS  PICTURE  99.                      CI0102
            10            GC04-CCACT  PICTURE  99.                      CI0102
            10            GC04-AFEET  PICTURE  S9(5)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC04-ITERF  PICTURE  X.                       CI0102
            10            GC04-ATERF  PICTURE  S9(5)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC04-CLDOB  PICTURE  9(8).                    CI0102
            10            GC04-CPLTYP PICTURE  X(14).                   CI0102
            10            GC04-IACFPD PICTURE  X(1).                    CI0102
            10            GC04-FILLER PICTURE  X(14).                   CI0102
       01                 GC06.                                         CI0102
            10            GC06-GELL   PICTURE  9(4)                     CI0102
                          BINARY.                                       CI0102
            10            GC06-GE00.                                    CI0102
            11            GC06-GC06K.                                   CI0102
            12            GC06-NPISQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC06-ACOTD  PICTURE  S9(9)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC06-PPOTD  PICTURE  S9(3)V99                 CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC06-QPSTD  PICTURE  S9(7)V999                CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC06-CPITC  PICTURE  99.                      CI0102
            11            GC06-ITRNB  PICTURE  X.                       CI0102
            11            GC06-FILLER PICTURE  X(14).                   CI0102
            10            GC06-GE98.                                    CI0102
            11            GC06-FILLER PICTURE  X(240).                  CI0102
            10            GC06-GE10                                     CI0102
                          REDEFINES            GC06-GE98.               CI0102
            11            GC06-CDELI  PICTURE  9(3).                    CI0102
            11            GC06-CPAYC  PICTURE  X(2).                    CI0102
            11            GC06-ICHKP  PICTURE  X.                       CI0102
            11            GC06-CLTIN  PICTURE  9(12).                   CI0102
            11            GC06-IFHAI  PICTURE  X.                       CI0102
            11            GC06-CDQUA  PICTURE  X(2).                    CI0102
            11            GC06-FILLER PICTURE  X(07).                   CI0102
            11            GC06-GE99.                                    CI0102
            12            GC06-FILLER PICTURE  X(212).                  CI0102
            11            GC06-GE01                                     CI0102
                          REDEFINES            GC06-GE99.               CI0102
            12            GC06-NTR    PICTURE  9(8).                    CI0102
            12            GC06-GECKD  PICTURE  9.                       CI0102
            12            GC06-NPBN   PICTURE  X(20).                   CI0102
            12            GC06-CCBAT  PICTURE  99.                      CI0102
            12            GC06-CLID4  PICTURE  X(23).                   CI0102
            12            GC06-GENAL1 PICTURE  X(30)                    CI0102
                          OCCURS       002     TIMES.                   CI0102
            12            GC06-GESAD1 PICTURE  X(30)                    CI0102
                          OCCURS       003     TIMES.                   CI0102
            11            GC06-GE02                                     CI0102
                          REDEFINES            GC06-GE99.               CI0102
            12            GC06-GENAL  PICTURE  X(30)                    CI0102
                          OCCURS       002     TIMES.                   CI0102
            12            GC06-GESAD  PICTURE  X(30)                    CI0102
                          OCCURS       003     TIMES.                   CI0102
            11            GC06-GE03                                     CI0102
                          REDEFINES            GC06-GE99.               CI0102
            12            GC06-NCHKN  PICTURE  9(11).                   CI0102
            11            GC06-GE04                                     CI0102
                          REDEFINES            GC06-GE99.               CI0102
            12            GC06-CTIDAP PICTURE  9(3).                    CI0102
            12            GC06-PRCOD  PICTURE  9(5).                    CI0102
            12            GC06-TDELI  PICTURE  X(30).                   CI0102
            12            GC06-CINCD  PICTURE  9(02).                   CI0102
            10            GC06-GE20                                     CI0102
                          REDEFINES            GC06-GE98.               CI0102
            11            GC06-C299.                                    CI0102
            12            GC06-CTID.                                    CI0102
            13            GC06-CTIDA  PICTURE  9(3).                    CI0102
            13            GC06-CTIDN.                                   CI0102
            14            GC06-CTIDNP PICTURE  X(13).                   CI0102
            14            GC06-CTIDND PICTURE  9(11).                   CI0102
            11            GC06-DCACG9 PICTURE  9(8).                    CI0102
            11            GC06-NAASQ  PICTURE  S9(3)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            GC06-CIRAP  PICTURE  XX.                      CI0102
            11            GC06-CTYPE  PICTURE  X.                       CI0102
            11            GC06-INACT  PICTURE  X.                       CI0102
            11            GC06-FILLER PICTURE  X(01).                   CI0102
            11            GC06-ITPAC  PICTURE  X.                       CI0102
            11            GC06-ITAXI  PICTURE  X.                       CI0102
            11            GC06-IOWNC  PICTURE  X.                       CI0102
            11            GC06-CDVCD  PICTURE  X(2).                    CI0102
            11            GC06-CTCUS  PICTURE  999.                     CI0102
            11            GC06-CPMTCB PICTURE  X(3).                    CI0102
            11            GC06-CASTC1 PICTURE  99.                      CI0102
            11            GC06-PRCOD1 PICTURE  9(5).                    CI0102
            11            GC06-CPRSC1 PICTURE  X(9).                    CI0102
            11            GC06-CPRTB  PICTURE  X.                       CI0102
            11            GC06-CBRKD  PICTURE  9(4).                    CI0102
            11            GC06-FILLER PICTURE  X(12).                   CI0102
            10            GC06-GE30                                     CI0102
                          REDEFINES            GC06-GE98.               CI0102
            11            GC06-CFIDC  PICTURE  X(5).                    CI0102
            11            GC06-CPHSE  PICTURE  9(2).                    CI0102
            11            GC06-FILLER PICTURE  X(05).                   CI0102
            11            GC06-IABIN  PICTURE  X.                       CI0102
            11            GC06-PDFND  PICTURE  S999V9(3)                CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC06-GE40                                     CI0102
                          REDEFINES            GC06-GE98.               CI0102
            11            GC06-CACCT  PICTURE  X.                       CI0102
            11            GC06-CPAYR  PICTURE  X(2).                    CI0102
            11            GC06-CDELI1 PICTURE  9(3).                    CI0102
            11            GC06-CATRN.                                   CI0102
            12            GC06-CATRF  PICTURE  9(3).                    CI0102
            12            GC06-CATRS  PICTURE  9(3).                    CI0102
            11            GC06-DEFFT  PICTURE  9(8).                    CI0102
            11            GC06-CTYPC  PICTURE  X.                       CI0102
            11            GC06-CIRAPA PICTURE  XX.                      CI0102
            11            GC06-FILLER PICTURE  X(09).                   CI0102
            11            GC06-GE49.                                    CI0102
            12            GC06-FILLER PICTURE  X(208).                  CI0102
            11            GC06-GE41                                     CI0102
                          REDEFINES            GC06-GE49.               CI0102
            12            GC06-NCHKN1 PICTURE  9(6).                    CI0102
            11            GC06-GE42                                     CI0102
                          REDEFINES            GC06-GE49.               CI0102
            12            GC06-CTID1.                                   CI0102
            13            GC06-CTIDA1 PICTURE  9(3).                    CI0102
            13            GC06-CTIDP1 PICTURE  X(13).                   CI0102
            13            GC06-CTIDN1 PICTURE  9(11).                   CI0102
            11            GC06-GE43                                     CI0102
                          REDEFINES            GC06-GE49.               CI0102
            12            GC06-GENAL2 PICTURE  X(30)                    CI0102
                          OCCURS       002     TIMES.                   CI0102
            12            GC06-GESAD2 PICTURE  X(30)                    CI0102
                          OCCURS       003     TIMES.                   CI0102
            11            GC06-GE44                                     CI0102
                          REDEFINES            GC06-GE49.               CI0102
            12            GC06-CTID01.                                  CI0102
            13            GC06-CTIDA6 PICTURE  9(3).                    CI0102
            13            GC06-NTIDP2 PICTURE  X(13).                   CI0102
            13            GC06-CTIDN2 PICTURE  9(11).                   CI0102
            12            GC06-GECKD2 PICTURE  9.                       CI0102
            12            GC06-PACCT  PICTURE  S999V99                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC06-PLOAN  PICTURE  S999V99                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC06-PADPT  PICTURE  S999V99                  CI0102
                          COMPUTATIONAL-3.                              CI0102
            12            GC06-IPCTL  PICTURE  X.                       CI0102
            12            GC06-IPCTP  PICTURE  X.                       CI0102
            12            GC06-CEUNT  PICTURE  S9(5)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            GC06-GE31                                     CI0102
                          REDEFINES            GC06-GE98.               CI0102
            11            GC06-GCUSPZ PICTURE  X(12).                   CI0102


      ******************************************************************
      **     SEGMENT THAT CONTAINS THE DL1 ERROR INFORMATION PASSED    *
      **       TO AND FROM THE DATA UTILITY DL1 ERROR HANDLING MODULES *
      ******************************************************************
      *
      *!WF DSP=DE DSL=DU SEL=10 FOR=I DES=1 LEV=1 PLT=85
       01                 DE10.                                         CI0102
            10            DE10-DU11.                                    CI0102
            11            DE10-XFONC  PICTURE  X(4).                    CI0102
            11            DE10-MPSBN  PICTURE  X(8).                    CI0102
            11            DE10-XDBDNM PICTURE  X(08).                   CI0102
            11            DE10-XSEGNM PICTURE  X(08).                   CI0102
            11            DE10-XRC    PICTURE  X(02).                   CI0102
            11            DE10-MSEG   PICTURE  X(08).                   CI0102
            11            DE10-XCOKEY PICTURE  X(70).                   CI0102
            11            DE10-CUIBR  PICTURE  X(01).                   CI0102
            11            DE10-CUIBA  PICTURE  X(01).                   CI0102
            11            DE10-IPBIK  PICTURE  X(1).                    CI0102
            10            DE10-DU03.                                    CI0102
            11            DE10-NMESS2 PICTURE  S9(6)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            DE10-CMSSF  PICTURE  XX.                      CI0102
            11            DE10-DU09.                                    CI0102
            12            DE10-CMESA  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            12            DE10-NMESS3 PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            12            DE10-CMESB  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            12            DE10-CMSST  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            12            DE10-CSLNN  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            12            DE10-QELLAA PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            12            DE10-TMESS4 PICTURE  X(512).                  CI0102

      *                                                                 ADU102
      ******************************************************************ADU102
      **     SEGMENT THAT CONTAINS THE MESSAGE INFORMATION NEEDED      *ADU102
      **     TO SEND A MESSAGE BACK TO THE WORKSTATION                 *ADU102
      ******************************************************************ADU102
      *                                                                 ADU102
      *!WF DSP=MS DSL=DU SEL=03 FOR=I LEV=1                             ADU102
       01                 MS00.                                         CI0102
          05              MS00-SUITE.                                   CI0102
            15       FILLER         PICTURE  X(00542).                  CI0102
       01                 MS03  REDEFINES      MS00.                    CI0102
            10            MS03-NMESS2 PICTURE  S9(6)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            10            MS03-CMSSF  PICTURE  XX.                      CI0102
            10            MS03-DU09.                                    CI0102
            11            MS03-CMESA  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            11            MS03-NMESS3 PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            11            MS03-CMESB  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            11            MS03-CMSST  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            11            MS03-CSLNN  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            11            MS03-QELLAA PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
            11            MS03-TMESS4 PICTURE  X(512).                  CI0102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *                                                                 ADU102
      *-----------------------------------------------------------------ADU102
      *>>>>>> Multiple message linkage                                  ADU102
      *-----------------------------------------------------------------ADU102
      *                                                                 ADU102
      *!WF DSP=MX DSL=PJ SEL=11 FOR=I DES=1 LEV=1                       ADU102
       01                 MX11.                                         CI0102
            10            MX11-QMSGS  PICTURE  9(03).                   CI0102
            10            MX11-PJ09                                     CI0102
                          OCCURS       025     TIMES.                   CI0102
            11            MX11-NMESS2 PICTURE  S9(6)                    CI0102
                          COMPUTATIONAL-3.                              CI0102
            11            MX11-CMESB  PICTURE  S9(9)                    CI0102
                          BINARY.                                       CI0102
      *                                                                 ADU102
       PROCEDURE DIVISION USING
                                DFHEIBLK
                                DFHCOMMAREA
                                DLIUIBII
                                PCB-ADDRESS-LIST
                                PJ50
                                GC01
                                GC03
                                GC04
                                GC06
                                DE10
                                MS03                                    ADU102
                                MX11.                                   ADU102
      *N01.      NOTE *************************************.            CI0102
      *               *                                   *             CI0102
      *               *INITIALISATIONS                    *             CI0102
      *               *                                   *             CI0102
      *               *************************************.            CI0102
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
      *N02DC.    NOTE *SET PCB ADDRESSES                  *.
       F02DC.                                                           lv10
      *SET ADDRESS FOR CL1P                                             DOT
           SET ADDRESS OF PA06 TO                                       ADU015
                PCB-CL1P-PTR1.                                          ADU015
      *SET ADDRESS FOR CT1P                                             DOT
           SET ADDRESS OF PB06 TO                                       ADU015
                PCB-CT1P-PTR1.                                          ADU015
      *SET ADDRESS FOR ACAP                                             DOT
           SET ADDRESS OF PC06 TO                                       ADU015
                PCB-ACAP-PTR1.                                          ADU015
      *SET ADDRESS FOR SBUP                                             DOT
           SET ADDRESS OF PE06 TO                                       ADU015
                PCB-SBUP-PTR1.                                          ADU015
      *SET ADDRESS FOR SCOP                                             DOT
           SET ADDRESS OF PF06 TO                                       ADU015
                PCB-SCOP-PTR1.                                          ADU015
      *SET ADDRESS FOR SSPP                                             DOT
           SET ADDRESS OF PH06 TO                                       ADU015
                PCB-SSPP-PTR1.                                          ADU015
      *SET ADDRESS FOR TR1P                                             DOT
           SET ADDRESS OF PI06 TO                                       ADU015
                PCB-TR1P-PTR1.                                          ADU015
       F02DC-FN. EXIT.
       F02-FN.   EXIT.
      *          NOTE *  DEBUT ITERATION DU PROGRAMME     *.            CI0102
       F05.           EXIT.
      *N20.      NOTE *************************************.            CI0102
      *               *                                   *             CI0102
      *               *FIN DE TRAITEMENT                  *             CI0102
      *               *                                   *             CI0102
      *               *************************************.            CI0102
       F20.      IF FT =            ALL '1'
                 NEXT SENTENCE ELSE GO TO     F20-FN.                   CI0102
      *N2099.    NOTE *---> Go back to calling module     *.            ADU102
       F2099.                                                           lv10
      *                                                                 ADU102
           GOBACK.                                                      ADU102
       F2099-FN. EXIT.
       F20-FN.   EXIT.
      *N50.      NOTE *************************************.
      *               *                                   *
      *               *IF THIS IS AN ADD OR CHANGE        *
      *               *                                   *
      *               *************************************.
       F50.      IF    PJ50-CACTS = 'A'                                 lv05
                 OR    PJ50-CACTS = 'C'
                 NEXT SENTENCE ELSE GO TO     F50-FN.
      *********************************
      ** THE FOLLOWING EDITS ARE FOR  *
      ** ADDS AND CHANGE TRANSACTIONS *
      *********************************
      *N50CB.    NOTE *GU CT01                            *.
       F50CB.                                                           lv10
           MOVE        PJ50-CTID TO S-CTU01-CTID
           PERFORM     F94CT THRU F94CT-FN.
                 IF    IK = '1'                                         DOT
      *CT01 NOT FOUND
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012077 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
                 IF    CT01-CTIDA = 002                                 DOT
                 AND   CT01-PRSCD = SPACES
      *FUND ACCT W/ NO SUB-PROD
           MOVE        '000000001' TO CT01-PRSCD.
       F50CB-FN. EXIT.
      *N50CD.    NOTE *CHECK IF STATUS IS NOT ACTIVE      *.
       F50CD.         EXIT.                                             lv10
      *N50CF.    NOTE *..AND ACCOUNT IS INACTIVE...       *.
       F50CF.    IF    CT01-CTSTA = 03                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50CF-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013425 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50CF-FN. EXIT.
      *N50CH.    NOTE *..AND ACCOUNT IS PENDING...        *.
       F50CH.    IF    CT01-CTSTA = 01                                  lv15
                 NEXT SENTENCE ELSE GO TO     F50CH-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013493 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50CH-FN. EXIT.
       F50CD-FN. EXIT.
      *N50CJ.    NOTE *READ THE TA5B TABLE                *.
       F50CJ.                                                           lv10
      *
      *********************************
      ** READ THE TA5B TABLE FOR THE  *
      ** PRCAUT VALUE TO SEE IF THE   *
      ** 'FROM' ACCOUNT IS TERMINATED.*
      *********************************
           MOVE        '0' TO TA5B-IK
           MOVE        PJ50-CTIDA TO TA5B-CTIDA
           MOVE        CT01-PRCOD TO TA5B-PRCOD
           MOVE        CT01-PRSCD TO TA5B-PRSCD
           PERFORM     F92TA THRU F92TA-FN.
      *N50CL.    NOTE *IF TA5B - "PRODUCT NOT FOUND"      *.
       F50CL.    IF    TA5B-IK = '1'                                    lv15
                 NEXT SENTENCE ELSE GO TO     F50CL-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012405 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50CL-FN. EXIT.
      *N50CN.    NOTE *IF TA5B - "PRODUCT IS TERMINATED   *.
       F50CN.    IF    TA5B-PRCAUT = 'T'                                lv15
                 NEXT SENTENCE ELSE GO TO     F50CN-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012807 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50CN-FN. EXIT.
       F50CJ-FN. EXIT.
      *N50DC.    NOTE *NOT POSSIBLE ON EZTRANS            *.
       F50DC.    IF    CT01-CTIDA = 001                                 lv10
                 AND   CT01-PRCOD = 180
                 AND   CT01-PRSCD = 000000001
                 NEXT SENTENCE ELSE GO TO     F50DC-FN.
      *EDIT TO PREVENT DISBURSEMENTS
      *FROM SAI CERTIFICATES
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013624 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50DC-FN. EXIT.
      *N50FC.    NOTE *READ FOR A CT13 - ACCT ON HOLD     *.
       F50FC.                                                           lv10
      ** IF A CT13 IS FOUND THE ACCOUNT
      ** IS CONSIDERED "ON HOLD" NO
      ** PROCESSING IS ALLOWED
      *
           MOVE        PJ50-CTID TO S-CTU01-CTID
      *
           PERFORM     F94CU THRU F94CU-FN
      *
                 IF    IK = '0'                                         DOT
                 AND   CT13-GEHCD = 045
      **  AML HOLD CODE
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        015582 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
                 IF    IK = '0'                                         DOT
      **  ERROR IF "FOUND"
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012841 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F50FC-FN. EXIT.
      *N50HC.    NOTE *CALL CI0003 - ACCT OWNER/BENE      *.            AM0003
       F50HC.                                                           lv10
      *THE FOLLOWING FUNCTIONS (50HC
      *THRU 50NC) PURPOSE ARE TO FIND
      *OUT IF THE TAXPAYER CLIENT IS 18
      *OR OVER IF UGMA/UTMA APPLIES.
      *                                                                 AM0003
      *********************************                                 AM0003
      ** THIS MODULE WILL READ THE    *                                 AM0003
      ** CONTRACT DATABASE TO GET THE *                                 AM0003
      ** ACCOUNT OWNERSHIP AND        *                                 AM0003
      ** BENEFICIARY LINES FOR THE    *                                 AM0003
      ** REQUESTED ACCOUNT NUMBER.    *                                 AM0003
      *********************************                                 AM0003
      *                                                                 AM0003
           INITIALIZE      FA04                                         AM0003
           MOVE        PJ50-CTID TO FA04-CTID                           AM0003
           MOVE        'Y' TO FA04-IPOCH                                AM0003
           SET CI0003A-PCB-CT1P-PTR1 TO                                 AM0003
                       PCB-CT1P-PTR1                                    AM0003
           INITIALIZE      DE10-DU03                                    AM0003
           CALL        CI0003 USING                                     AM0003
           DFHEIBLK                                                     AM0003
           DFHCOMMAREA                                                  AM0003
           DLIUIBII                                                     AM0003
           CI0003A-PCB-ADDRESS-LIST                                     AM0003
           FA04                                                         AM0003
           DE10                                                         AM0003
           MS03.                                                        AM0003
      *N50HG.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50HG.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50HG-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0003 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0003 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F50HG-900. GO TO F50HI-FN.
       F50HG-FN. EXIT.
      *N50HI.    NOTE *NO ERRORS                          *.            ADU071
       F50HI.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F50HI-FN. EXIT.
       F50HC-FN. EXIT.
      *N50HM.    NOTE *DOES UGMA/UTMA APPLY?              *.
       F50HM.    IF    FA04-IUGMA = 'Y'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F50HM-FN.
      *N50IA.    NOTE *CALL CI0020 - CAMS ACCTG DATE      *.            AM0020
       F50IA.                                                           lv15
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
      *N50IC.    NOTE *NON-DL1 ERROR                      *.            ADU070
       F50IC.    IF    MS03-NMESS2 > ZERO                               lv20
                 AND   MS03-CMESB > 10                                  ADU070
                 NEXT SENTENCE ELSE GO TO     F50IC-FN.                 ADU070
      *OF A CERTAIN SEVERITY                                            ADU070
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU070
           MOVE        CI0020 TO MS03-TMESS4 (IMS03R : 6)               ADU070
           ADD         +7 TO MS03-QELLAA                                ADU070
           MOVE                     ALL '1' TO FT GO TO F20.            ADU070
       F50IC-900. GO TO F50IE-FN.
       F50IC-FN. EXIT.
      *N50IE.    NOTE *NO ERRORS                          *.            ADU070
       F50IE.                                                           lv20
           INITIALIZE  MS03.                                            ADU070
       F50IE-FN. EXIT.
       F50IA-FN. EXIT.
      *N50JC.    NOTE *CALL CI0018 - ACCT CLIENTS         *.            AM0018
       F50JC.                                                           lv15
      *                                                                 AM0018
      *********************************                                 AM0018
      ** THIS MODULE WILL READ THE    *                                 AM0018
      ** CONTRACT DATABASE TO GET THE *                                 AM0018
      ** TAXPAYER CLIENT ID AND OWNER *                                 AM0018
      ** CLIENT ID'S ASSOCIATED WITH  *                                 AM0018
      ** THE ACCOUNT NUMBER.          *                                 AM0018
      *********************************                                 AM0018
      *                                                                 AM0018
           INITIALIZE      HC14                                         AM0018
           MOVE        PJ50-CTID TO HC14-CTID                           AM0018
           MOVE        NS20-DCACG TO HC14-DCACG                         AM0018
           MOVE        25 TO HC14-XIMAX                                 AM0018
           MOVE        'Y' TO HC14-IPOCH                                AM0018
           SET CI0018B-PCB-CT1P-PTR1 TO                                 AM0018
                       PCB-CT1P-PTR1                                    AM0018
           INITIALIZE      DE10-DU03                                    AM0018
           CALL        CI0018 USING                                     AM0018
           DFHEIBLK                                                     AM0018
           DFHCOMMAREA                                                  AM0018
           DLIUIBII                                                     AM0018
           CI0018B-PCB-ADDRESS-LIST                                     AM0018
           HC14                                                         AM0018
           DE10                                                         AM0018
           MS03.                                                        AM0018
      *N50JF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50JF.    IF    (MS03-NMESS2 > ZERO                              lv20
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50JF-FN.                 ADU071
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
       F50JF-900. GO TO F50JH-FN.
       F50JF-FN. EXIT.
      *N50JH.    NOTE *NO ERRORS                          *.            ADU071
       F50JH.                                                           lv20
           INITIALIZE  MS03.                                            ADU071
       F50JH-FN. EXIT.
      *N50KC.    NOTE *MOVE TAXPAYER CLID TO WS           *.
       F50KC.                                                           lv20
           MOVE        HC14-CLID01 TO 7-HC14-CLID.
       F50KC-FN. EXIT.
       F50JC-FN. EXIT.
      *N50LC.    NOTE *Call module CI0095                 *.            AM0095
       F50LC.                                                           lv15
      *Get Client Person Data                                           AM0095
           INITIALIZE  HG60 HG61                                        AM0095
           DE10-DU03                                                    AM0095
      *Prime Input Linkage                                              AM0095
           MOVE        PJ50-MAPPN TO HG60-MAPPN                         AM0095
           MOVE        7-HC14-CLID TO HG60-CLID                         AM0095
           SET CI0095-PCB-CL1P-PTR1 TO                                  AM0095
                      PCB-CL1P-PTR1                                     AM0095
           CALL        CI0095 USING                                     AM0095
           DFHEIBLK                                                     AM0095
           DFHCOMMAREA                                                  AM0095
           DLIUIBII                                                     AM0095
           CI0095-PCB-ADDRESS-LIST                                      AM0095
           HG60                                                         AM0095
           HG61                                                         AM0095
           DE10                                                         AM0095
           MS03                                                         AM0095
           MX11.                                                        AM0095
      *N50LF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F50LF.    IF    (MS03-NMESS2 > ZERO                              lv20
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F50LF-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0095 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0095 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F50LF-900. GO TO F50LH-FN.
       F50LF-FN. EXIT.
      *N50LH.    NOTE *NO ERRORS                          *.            ADU071
       F50LH.                                                           lv20
           INITIALIZE  MS03.                                            ADU071
       F50LH-FN. EXIT.
      *N50NC.    NOTE *IF CLIENT OVER 18, ISSUE MSG       *.
       F50NC.    IF    HG61-QYEAR >= 18                                 lv20
                 NEXT SENTENCE ELSE GO TO     F50NC-FN.
      *---> Send INFO ERR Message                                       ADU119
      *      and CONTINUE                                               ADU119
           MOVE        013414 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F50NC-FN. EXIT.
       F50LC-FN. EXIT.
       F50HM-FN. EXIT.
       F50-FN.   EXIT.
      *N52.      NOTE *************************************.
      *               *                                   *
      *               *IF THIS IS A CHANGE                *
      *               *                                   *
      *               *************************************.
       F52.      IF    PJ50-CACTS = 'C'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F52-FN.
      *********************************
      ** THE FOLLOWING EDIT(S) ARE FOR*
      ** CHANGE TRANSACTIONS          *
      *********************************
      *N52DC.    NOTE *LOAD VARIABLES TO CALL CI0100      *.
       F52DC.                                                           lv10
           MOVE        PJ50-MAPPN TO 7-GC00-MAPPN
           MOVE        'GU' TO 7-GC00-CFUNC
           MOVE        PJ50-CTID TO 7-GC00-CTID
           MOVE        PJ50-DCACG9 TO 7-GC00-DCACG9
           MOVE        PJ50-NAASQ TO 7-GC00-NAASQ
           MOVE        PJ50-NPISQ TO 7-GC00-NPISQ
           MOVE        'N' TO 7-GC00-IPERT
           MOVE        ZERO TO 7-GC00-NEIBT.
       F52DC-FN. EXIT.
      *N52EC.    NOTE *CALL CI0100 - ACCESS ACTIVITY      *.            AM0100
       F52EC.                                                           lv10
      *********************************                                 AM0100
      ** THIS MODULE WILL ACCESS THE  *                                 AM0100
      ** ACTIVITY DATABASE AND        *                                 AM0100
      ** RETRIEVE 1 TO 10 ACTIVITIES  *                                 AM0100
      *********************************                                 AM0100
           INITIALIZE  PK40                                             AM0100
           MOVE        7-GC00-MAPPN TO PK40-MAPPN                       AM0100
           MOVE        7-GC00-CFUNC TO PK40-CFUNC                       AM0100
           MOVE        7-GC00-CASTC (1) TO PK40-CASTC (1)               AM0100
           MOVE        7-GC00-CASTC (2) TO PK40-CASTC (2)               AM0100
           MOVE        7-GC00-CASTC (3) TO PK40-CASTC (3)               AM0100
           MOVE        7-GC00-CASTC (4) TO PK40-CASTC (4)               AM0100
           MOVE        7-GC00-CASTC (5) TO PK40-CASTC (5)               AM0100
           MOVE        7-GC00-CASTC (6) TO PK40-CASTC (6)               AM0100
           MOVE        7-GC00-CAATY (1) TO PK40-CAATY (1)               AM0100
           MOVE        7-GC00-CAATY (2) TO PK40-CAATY (2)               AM0100
           MOVE        7-GC00-CAATY (3) TO PK40-CAATY (3)               AM0100
           MOVE        7-GC00-C299 TO PK40-C299                         AM0100
           MOVE        7-GC00-DCACG9 TO PK40-DCACG9                     AM0100
           MOVE        7-GC00-NAASQ TO PK40-NAASQ                       AM0100
           MOVE        7-GC00-NPISQ TO PK40-NPISQ                       AM0100
           MOVE        7-GC00-CIRAP TO PK40-CIRAP                       AM0100
           MOVE        7-GC00-IPERT TO PK40-IPERT                       AM0100
           MOVE        7-GC00-NEIBT TO PK40-NEIBT                       AM0100
           MOVE        7-GC00-GESQ2C TO PK40-GESQ2C                     AM0100
           MOVE        7-GC00-MIPPS TO PK40-MIPPS                       AM0100
           MOVE        7-GC00-IENDP TO PK40-IENDP                       AM0100
           SET CI0100GC-PCB-ACAP-PTR1 TO                                AM0100
                        PCB-ACAP-PTR1                                   AM0100
           INITIALIZE  DE10-DU03                                        AM0100
           CALL        CI0100 USING                                     AM0100
           DFHEIBLK                                                     AM0100
           DFHCOMMAREA                                                  AM0100
           DLIUIBII                                                     AM0100
           CI0100GC-PCB-ADDRESS-LIST                                    AM0100
           PK40                                                         AM0100
           PK41                                                         AM0100
           DE10                                                         AM0100
           MS03                                                         AM0100
           MX11.                                                        AM0100
      *N52EF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F52EF.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F52EF-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0100 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0100 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F52EF-900. GO TO F52EH-FN.
       F52EF-FN. EXIT.
      *N52EH.    NOTE *NO ERRORS                          *.            ADU071
       F52EH.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F52EH-FN. EXIT.
       F52EC-FN. EXIT.
      *N52FC.    NOTE *WAS A GC01 FOUND?? IF NOT...       *.
       F52FC.    IF    PK41-IGC01 = 'N'                                 lv10
                 NEXT SENTENCE ELSE GO TO     F52FC-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013420 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52FC-FN. EXIT.
      *N52FE.    NOTE *WAS A GC03 FOUND?? IF NOT...       *.
       F52FE.    IF    PK41-IGC03 (1) = 'N'                             lv10
                 NEXT SENTENCE ELSE GO TO     F52FE-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013421 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52FE-FN. EXIT.
      *N52FG.    NOTE *WAS A GC06 FOUND?? IF NOT...       *.
       F52FG.    IF    PK41-IGC06 (1) = 'N'                             lv10
                 NEXT SENTENCE ELSE GO TO     F52FG-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013402 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52FG-FN. EXIT.
      *N52GI.    NOTE *MOVE ACTIVITY FIELDS TO LINKAGE    *.
       F52GI.                                                           lv10
           MOVE        PK41-GC01 TO GC01
           MOVE        PK41-GC03 (1) TO GC03
           MOVE        PK41-GC04 (1) TO GC04
           MOVE        PK41-GC06 (1) TO GC06.
       F52GI-FN. EXIT.
      *N52HK.    NOTE *IF ACT'Y WAS CREATED BY CATS UD    *.
       F52HK.    IF    (GC03-CACTO = 001 OR                             lv10
                       GC03-CACTO = 020)
                 AND   GC03-CTRTP = 'S'
                 AND   GC03-CAATY = 001
                 AND   GC03-CVSYS = 01
                 AND   GC03-CASTC = 01
                 NEXT SENTENCE ELSE GO TO     F52HK-FN.
      * OR EZ-TRANS
      * AND IT'S A SURRENDER/REDEMPTION
      * AND IT'S A DISBURSEMENT
      * AND IT'S FUNDS
      * AND IT'S UNPROCESSED
      *   THEN CONTINUE...
       F52HK-900. GO TO F52HM-FN.
       F52HK-FN. EXIT.
      *N52HM.    NOTE *ELSE, CHANGE IS NOT ALLOWED        *.
       F52HM.                                                           lv10
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013415 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52HM-FN. EXIT.
      *N52JM.    NOTE *IF THIS IS A CONVERSION TRANS..    *.
       F52JM.    IF    GC06-CPITC = 02                                  lv10
                 AND   GC06-CTYPE = 'C'
                 NEXT SENTENCE ELSE GO TO     F52JM-FN.
      * CHANGE IS NOT ALLOWED EITHER
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013495 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52JM-FN. EXIT.
      *N52JP.    NOTE *CANNOT CHANGE LOI TRAN             *.
       F52JP.    IF    GC06-CPITC = 01                                  lv10
                 AND   GC06-CPAYC = 'LI'
                 NEXT SENTENCE ELSE GO TO     F52JP-FN.
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013887 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52JP-FN. EXIT.
      *N52JQ.    NOTE *STOP PORTABLITY TRANS CHANGE       *.
       F52JQ.    IF    GC06-CPITC = 2                                   lv10
                 AND   (GC06-CTYPE = 'P'
                 OR    GC06-CTYPE = 'X')
                 NEXT SENTENCE ELSE GO TO     F52JQ-FN.
      *BEYOND THIS POINT
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        012148 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52JQ-FN. EXIT.
      *N52KM.    NOTE *IF THIS IS NON-TAX REPORTABLE      *.
       F52KM.    IF    ((PK41-IGC04 (1) = 'Y')                          lv10
                 AND   (GC04-CLCUS = 99))
                 AND   GC06-CPAYC = 'P'
                 NEXT SENTENCE ELSE GO TO     F52KM-FN.
      * AND A SPECIAL PAYEE..
      * THEN CAN'T CHANGE....
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013494 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52KM-FN. EXIT.
      *N52LM.    NOTE *IF AN IDS ACCOUNT                  *.
       F52LM.    IF    GC06-CPITC = 02                                  lv10
                 NEXT SENTENCE ELSE GO TO     F52LM-FN.
      *N52LN.    NOTE *NOT POSSIBLE ON EZTRANS            *.
       F52LN.    IF    GC06-CTIDA = 001                                 lv15
                 AND   GC06-PRCOD1 = 180
                 AND   GC06-CPRSC1 = 000000001
                 NEXT SENTENCE ELSE GO TO     F52LN-FN.
      *EDIT TO PREVENT DELETION AND
      *MODIFICATION OF TRANS WITH
      *SAI CERTIFICATES
      *---> Send ERROR Message                                          ADU119
      *      and TERMINATE                                              ADU119
           MOVE        013625 TO MS03-NMESS2                            ADU119
           PERFORM     F98ET THRU F98ET-FN.                             ADU119
       F52LN-FN. EXIT.
       F52LM-FN. EXIT.
       F52-FN.   EXIT.
      *N53.      NOTE *************************************.
      *               *                                   *
      *               *CALL CI0103                        *
      *               *                                   *
      *               *************************************.
       F53.                                                             lv05
      *********************************
      **THIS MODULE IS CALLED TO      *
      **RETRIEVE PENDING AMOUNTS AND  *
      **ALSO FOR ASSISTANCE IN SOME   *
      **ADD EDITS IN F54              *
      *********************************
      *N53BC.    NOTE *LOAD VARIABLES TO CALL CI0103      *.
       F53BC.                                                           lv10
           MOVE        PJ50-MAPPN TO 7-HS00-MAPPN
           MOVE        PJ50-CTID TO 7-HS00-CTID
           MOVE        'Y' TO 7-HS00-IGOTY.
       F53BC-FN. EXIT.
      *N53CC.    NOTE *CALL CI0103 - ACCESS ACTIVITY      *.            AM0103
       F53CC.                                                           lv10
      *********************************                                 AM0103
      ** THIS MODULE WILL RETRIEVE    *                                 AM0103
      ** SPECIFIC ACCOUNT VALUES      *                                 AM0103
      ** INCLUDING GROSS VALUE,       *                                 AM0103
      ** PENDING WITHDRAWAL (DISPLAY  *                                 AM0103
      ** & CALCULATION), PENDING PAY- *                                 AM0103
      ** MENTS (AVAILABLE & NOT       *                                 AM0103
      ** AVAILABLE), GOOD FUNDS AMT,  *                                 AM0103
      ** AND ASSETS AVAILABLE AMT.    *                                 AM0103
      *********************************                                 AM0103
           INITIALIZE  HS51                                             AM0103
           MOVE        7-HS00-MAPPN TO HS51-MAPPN                       AM0103
           MOVE        7-HS00-CTID TO HS51-CTID                         AM0103
           MOVE        7-HS00-IGOTY TO HS51-IGOTY                       AM0103
           SET CI0103HS-PCB-SBUP-PTR1 TO                                AM0103
                        PCB-SBUP-PTR1                                   AM0103
           SET CI0103HS-PCB-SCOP-PTR1 TO                                AM0103
                        PCB-SCOP-PTR1                                   AM0103
           SET CI0103HS-PCB-SSPP-PTR1 TO                                AM0103
                        PCB-SSPP-PTR1                                   AM0103
           SET CI0103HS-PCB-CT1P-PTR1 TO                                AM0103
                        PCB-CT1P-PTR1                                   AM0103
           SET CI0103HS-PCB-ACAP-PTR1 TO                                AM0103
                        PCB-ACAP-PTR1                                   AM0103
           SET CI0103HS-PCB-TR1P-PTR1 TO                                AM0103
                        PCB-TR1P-PTR1                                   AM0103
           INITIALIZE  DE10-DU03                                        AM0103
           CALL        CI0103 USING                                     AM0103
           DFHEIBLK                                                     AM0103
           DFHCOMMAREA                                                  AM0103
           DLIUIBII                                                     AM0103
           CI0103HS-PCB-ADDRESS-LIST                                    AM0103
           HS51                                                         AM0103
           HS52                                                         AM0103
           DE10                                                         AM0103
           MS03                                                         AM0103
           MX11.                                                        AM0103
      *N53CD.    NOTE *NON-DL1 DST ERROR, TERMINATE THE   *.
       F53CD.    IF    (MS03-NMESS2 > 14515                             lv15
                 AND   MS03-NMESS2 < 14586)
                 NEXT SENTENCE ELSE GO TO     F53CD-FN.
      *PROGRAM
           MOVE                     ALL '1' TO FT GO TO F20.
       F53CD-FN. EXIT.
      *N53CF.    NOTE *NON-DL1 ERROR                      *.            ADU071
       F53CF.    IF    (MS03-NMESS2 > ZERO                              lv15
                 AND   MS03-CMESB > 10)                                 ADU071
                 OR    DE10-NMESS2 > ZERO                               ADU071
                 NEXT SENTENCE ELSE GO TO     F53CF-FN.                 ADU071
      *OF A CERTAIN SEVERITY                                            ADU071
      *OR DL1 ERROR                                                     ADU071
                 IF    MS03-NMESS2 > ZERO                               DOT
      *NON-DL1 ERROR                                                    ADU071
           COMPUTE     IMS03R = MS03-QELLAA + 2                         ADU071
           MOVE        CI0103 TO MS03-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO MS03-QELLAA.                               ADU071
                 IF    DE10-NMESS2 > ZERO                               DOT
      *DL1 ERROR                                                        ADU071
           COMPUTE     IMS03R = DE10-QELLAA + 2                         ADU071
           MOVE        CI0103 TO DE10-TMESS4 (IMS03R : 6)               ADU071
           ADD         +7 TO DE10-QELLAA.                               ADU071
           MOVE                     ALL '1' TO FT GO TO F20.            DOT
       F53CF-900. GO TO F53CH-FN.
       F53CF-FN. EXIT.
      *N53CH.    NOTE *NO ERRORS                          *.            ADU071
       F53CH.                                                           lv15
           INITIALIZE  MS03.                                            ADU071
       F53CH-FN. EXIT.
       F53CC-FN. EXIT.
      *N53EM.    NOTE *MOVE PENDING FIELDS TO OUTPUT      *.
       F53EM.                                                           lv10
           MOVE        HS52-AWDRTP TO PJ50-AWDRTP
           MOVE        HS52-APPAYA TO PJ50-APPAYA
           MOVE        HS52-APPAYN TO PJ50-APPAYN.
       F53EM-FN. EXIT.
       F53-FN.   EXIT.
      *N54.      NOTE *************************************.
      *               *                                   *
      *               *IF THIS IS AN ADD                  *
      *               *                                   *
      *               *************************************.
       F54.      IF    PJ50-CACTS = 'A'                                 lv05
                 NEXT SENTENCE ELSE GO TO     F54-FN.
      *********************************
      ** THE FOLLOWING EDIT(S) ARE FOR*
      ** ADD TRANSACTIONS             *
      *********************************
      *N54EC.    NOTE *CHECK FOR POSSIBLE DUP TRANS       *.
       F54EC.    IF    HS52-IGOTYA = 'Y'                                lv10
                 NEXT SENTENCE ELSE GO TO     F54EC-FN.
      *
      *---> Send INFO ERR Message                                       ADU119
      *      and CONTINUE                                               ADU119
           MOVE        013416 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F54EC-FN. EXIT.
      *N54GB.    NOTE *ADD COUNTERS FROM CI0103           *.
       F54GB.                                                           lv10
           COMPUTE     7-WS-QTYUDT =
           HS52-QTYUD1 + HS52-QTYUD2.
       F54GB-FN. EXIT.
      *N54GC.    NOTE *MORE THAN 7 ACTIVITIES IN          *.
       F54GC.    IF    7-WS-QTYUDT > 7                                  lv10
                 NEXT SENTENCE ELSE GO TO     F54GC-FN.
      * THE LAST 30 DAYS?
      *---> Send INFO ERR Message                                       ADU119
      *      and CONTINUE                                               ADU119
           MOVE        013417 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F54GC-FN. EXIT.
      *N54KC.    NOTE *PRX EDITING - 'ADD' INFO MSG       *.
       F54KC.    IF    CT01-CLCUS = 02                                  lv10
                 AND   CT01-CQACT = 001
                 NEXT SENTENCE ELSE GO TO     F54KC-FN.
      * O - WHERE PRX ALREADY APPLIES
      * O - ON AN IRA ACCOUNT
      *
      *---> Send INFO ERR Message                                       ADU119
      *      and CONTINUE                                               ADU119
           MOVE        012828 TO MS03-NMESS2                            ADU119
           PERFORM     F98MX THRU F98MX-FN.                             ADU119
       F54KC-FN. EXIT.
       F54-FN.   EXIT.
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
      *N92.      NOTE *************************************.
      *               *                                   *
      *               *MISCELLANEOUS ROUTINES             *
      *               *                                   *
      *               *************************************.
       F92.           EXIT.                                             lv05
      *N92EB.    NOTE *ERROR ON TABLE READ FOR TA5B       *.
       F92EB.                                                           lv10
           MOVE        '1' TO TA5B-IK.
       F92EB-FN. EXIT.
      *N92TA.    NOTE *RANDOM TABLE READ FOR TA5B         *.            ADUTAB
       F92TA.                                                           lv10
           MOVE        'R1' TO G-TA5B-TABFO                             ADUTAB
           COMPUTE     G-TA5B-LTH = 60 + G-TA5B-LOZTR                   ADUTAB
           MOVE        'PA01' TO G-TA5B-TRANID                          ADUTAB
           EXEC CICS   LINK PROGRAM ('PACTABLE')                        ADUTAB
                       COMMAREA (G-TA5B)                                ADUTAB
                       LENGTH (G-TA5B-LTH)                   END-EXEC.  ADUTAB
                 IF    G-TA5B-TABCR NOT = '00'                          DOT
           PERFORM     F92EB THRU F92EB-FN.                             ADUTAB
       F92TA-FN. EXIT.
       F92-FN.   EXIT.
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
      *N94CT.    NOTE *CALL GU ON CT01                    *.            ADU026
       F94CT.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT01' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGU                         ADU026
           PB06 CT01                                                    ADU026
           S-CTU01-SSA                                                  ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGU TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CT-FN. EXIT.
      *N94CU.    NOTE *CALL GN ON CT13                    *.            ADU026
       F94CU.                                                           lv10
           MOVE        'CT1P' TO DE10-XDBDNM                            ADU026
           MOVE        'CT13' TO DE10-XSEGNM                            ADU026
           CALL        'CBLTDLI' USING XW05-XGN                         ADU026
           PB06 CT13                                                    ADU026
           S-CTU01-SSA S-CT13-SSA                                       ADU026
           MOVE        PB06 TO XW05-XDBPCB                              ADU026
           MOVE        XW05-XGN TO SV01-FUNC                            ADU026
           PERFORM     F93EA THRU F93EA-FN.                             ADU026
       F94CU-FN. EXIT.
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
